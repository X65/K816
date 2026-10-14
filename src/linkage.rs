/*****************************************************************************/
/*                                                                           */
/*                                linkage.rs                                 */
/*                                                                           */
/*         Cross-unit name resolution and derived imports/exports            */
/*                                                                           */
/*                                                                           */
/* Copyright (c) 2026 Tomasz Sterna (smokku)                                 */
/*                                                                           */
/* Permission to use, copy, modify, and/or distribute this software for any  */
/* purpose with or without fee is hereby granted.                            */
/*                                                                           */
/* THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES  */
/* WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF          */
/* MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR   */
/* ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES    */
/* WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN     */
/* ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF   */
/* OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.            */
/*                                                                           */
/*****************************************************************************/

//! Linkage is derived where a body exists, and declared where one does not.
//!
//! K65 had no `import`, `export`, or `extern` syntax: the legacy linker
//! resolved every name globally across a project's units, so there was
//! nothing in the source to read, and K816 worked out what crosses a unit
//! boundary from the symbol tables themselves. That derivation stands
//! (`plans/feature-manifest.md` §3): a name one unit defines and another uses
//! becomes an `.export` in the first and an `.import` in the second, both
//! carrying the symbol's address size — ca65 validates addressing modes
//! against an imported symbol's address size, so getting that wrong shows up
//! as an assembly error rather than a wrong byte. Two units defining one
//! exported name is a frontend error, rather than an ambiguity handed to
//! ld65.
//!
//! Sub-plan K adds two things the derivation could not reach.
//!
//! # Which units a unit may see
//!
//! Not all of them. A unit sees itself and the units it `import`s
//! **directly**, and nothing else. The transitive closure is *loaded* — a
//! callee's own callees decide its exit contract, so classifying one needs
//! bodies the entry never names — but it is not *visible*, and that
//! separation is what makes a unit mean one thing: `b.k816` compiled alone
//! and `b.k816` reached through `a.k816` resolve the same names, because
//! what `a.k816` imports is not `b.k816`'s business.
//!
//! # Names no unit defines
//!
//! A name nothing defines is still an undeclared-symbol error; the frontend
//! does not invent an import to make an unresolved name someone else's
//! problem. What it accepts instead is a name someone *declared*: an
//! `extern` is a [`Definition`] with [`Origin::Declared`], carrying the kind,
//! the address size and — for a function — a contract that is trusted rather
//! than derived, because there is no body here to derive it from
//! (`plans/10-modules-and-ffi.md` decision 5). An `extern` and a real
//! definition of one name is an error, and so are two `extern`s that
//! disagree.

use std::collections::{BTreeMap, BTreeSet};

use crate::abi;
use crate::ast::{
    AstFile, Block, DataEntry, DataEntryKind, Item, ItemKind, Statement, StatementKind,
    WidthAnnotation, WidthAnnotationKind,
};
use crate::diagnostic::{Diagnostic, DiagnosticCode, Label};
use crate::isa::RegisterWidth;
use crate::parser;
use crate::semantic;
use crate::source::{FileId, Provenance, SourceMap, Span};
use crate::symbol::{LabelScope, SymbolId, SymbolKind};
use crate::width;

/// An `inline` declaration kept for the units that expand it.
///
/// `usable` is false when the parser had to recover inside the body. The
/// declaration is still kept, because a borrower needs its name and span to
/// say what is wrong; what it must not do is expand a body the parser guessed
/// at. The recovered AST is a *shorter* body, not a broken one, so copying it
/// would emit silently truncated code (`postmortem.md` §G.10.4).
#[derive(Debug)]
struct BorrowedInline {
    item: Item,
    usable: bool,
    /// The unit that declared it. A body is copied only into a unit that can
    /// *see* the declaration, so this is compared against the borrower's
    /// visible set rather than assumed to be reachable.
    file: FileId,
}

/// What `inline_expansions` found for one program.
#[derive(Debug, Default)]
pub struct BorrowedInlines {
    /// Declarations to append to the borrowing unit's program.
    pub items: Vec<Item>,
    /// Sections it expands that cannot be copied, by name and declaration
    /// span, for the one line that says so.
    pub unusable: Vec<(String, Span)>,
}

/// Where a workspace name came from.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Origin {
    /// A unit of this program defines it: there is a body, or storage, and
    /// bytes are emitted for it somewhere.
    Body,
    /// An `extern` declares it. No unit of this program emits anything for
    /// it; ld65 resolves it from an object K816 never saw.
    Declared,
}

/// One unit-level definition, and the unit that owns it.
#[derive(Clone, Debug)]
pub struct Definition {
    /// The unit that defines it, or — for a `Declared` one — the first unit
    /// that declared it. Every unit that declares or defines the name is in
    /// `declared_in`; this is the one an editor is sent to, and a body wins
    /// over a declaration so that "go to definition" lands on the truth.
    pub file: FileId,
    pub kind: SymbolKind,
    pub declaration: Span,
    /// For a code section: the widths it assumes on entry and what a caller
    /// may believe afterwards, classified across the whole workspace.
    ///
    /// `None` until the third resolution phase fills it, and for everything
    /// that is not a function. For a `Declared` definition it is filled in
    /// the *first* phase and from the declaration itself, because there is
    /// no body to classify.
    pub contract: Option<width::CalleeContract>,
    pub origin: Origin,
}

/// One `import` edge, resolved.
#[derive(Clone, Debug)]
struct ImportEdge {
    /// The string that named it, for the diagnostic when nothing is there.
    span: Span,
    /// The path it names, relative to the importing file.
    resolved: std::path::PathBuf,
    /// The unit it reaches. `None` where no file of that identity is in the
    /// map, which the loader could not read and this reports.
    target: Option<FileId>,
}

/// One `abstract var` declaration, carried across a unit boundary.
#[derive(Clone, Debug)]
pub struct LayoutDeclaration {
    /// Where it was written, for the diagnostic that points at it.
    pub declaration: Span,
    /// The layout and everything nested under it, with ids of its own.
    pub arena: Vec<crate::symbol::Layout>,
    pub root: crate::symbol::LayoutId,
}

/// What every unit in a workspace defines, and which foreign names each one
/// uses.
#[derive(Debug, Default)]
pub struct Linkage {
    definitions: BTreeMap<String, Definition>,
    /// The `abstract var` layouts each unit declares, per unit.
    ///
    /// Kept out of `definitions` on purpose, and it is not one: a layout
    /// emits nothing, ld65 never sees it, and two units may declare the same
    /// one without that being a duplicate — which is what a shared header
    /// needs and what `is_exportable` says. What it *is* is a compile-time
    /// fact with no address at all, exactly like a `const`'s value, so it
    /// crosses a direct `import` the way a `const` does.
    ///
    /// The data crosses rather than the id. A [`LayoutId`] indexes one unit's
    /// arena and the declaring unit's model is gone by the time an importer
    /// asks, so each entry carries an arena of its own
    /// ([`Layouts::extract`]), and the importer interns it
    /// ([`SymbolTable::adopt_layout`]).
    layouts: BTreeMap<FileId, BTreeMap<String, LayoutDeclaration>>,
    /// The field layout of every `var` declared with one, by name.
    ///
    /// The variable itself is a [`Definition`] and crosses as one; what its
    /// kind cannot carry is the layout, because a [`LayoutId`] indexes the
    /// declaring unit's arena. So the layout travels beside it as data, the
    /// way an `abstract var`'s does, and the importer interns it and puts the
    /// id it gets back on the kind (`plans/13-ca65-converter.md` decision 1).
    /// Keyed by name alone: a name has one definition, and two that collide
    /// are already `K816-E0102`.
    var_layouts: BTreeMap<String, LayoutDeclaration>,
    /// Every unit that defines *or* declares each name.
    ///
    /// Visibility is decided against this rather than against
    /// [`Definition::file`], because one name may be declared `extern` by
    /// several interface units and a third unit reaches it through whichever
    /// one it imported.
    declared_in: BTreeMap<String, BTreeSet<FileId>>,
    /// Which units each unit may see, itself included: `{u}` and the units
    /// `u` imports **directly**.
    ///
    /// Nothing else. Not what those import, and not the other files the
    /// caller happened to hand over — which is what every unit could see
    /// before sub-plan K, and what made a unit mean whatever the company it
    /// was compiled in made it mean.
    visible: BTreeMap<FileId, BTreeSet<FileId>>,
    /// Every `import` each unit wrote, resolved.
    imports: BTreeMap<FileId, Vec<ImportEdge>>,
    /// The assembled bytes of every data section the workspace defines, by
    /// name, from the pass that analyzed each unit alone.
    ///
    /// The original keeps every section of a project alive and answers
    /// `index` and `size` about any of them, measured — a section's bytes are
    /// a compile-time fact wherever they were written, unlike its address.
    /// A section whose own unit could not decide its bytes is here too, as
    /// `Unresolved`, so that a read of one gets the sentence that says so
    /// rather than "not declared".
    ///
    /// Taken from the first pass, so a section that itself reads a foreign
    /// section does not resolve and does not appear. That is a chain the
    /// original has too — it compiles a project's units in order — and it
    /// fails as a refusal rather than as a wrong byte.
    sections: BTreeMap<String, (FileId, crate::eval::SectionBytes)>,
    /// The declaration of every `inline` the workspace defines, by name.
    ///
    /// An `inline` is the one section kind that does not link: it has no
    /// address, so ld65 cannot resolve a use site in another unit the way it
    /// resolves a `jsr`. What crosses the boundary is therefore the *body*,
    /// copied into the using unit's program before a symbol is declared, so
    /// that everything downstream sees an ordinary local `inline`
    /// (`plans/06-compatibility.md` decision 24, `postmortem.md` §G.9.4).
    ///
    /// Taken from the defining unit's preprocessed program, so a directive
    /// inside the body is decided where the body is written and every unit
    /// that expands it gets the branch that unit chose
    /// (`doc/syntax-reference.md`, "`inline` bodies").
    inlines: BTreeMap<String, BorrowedInline>,
    /// Foreign names each unit refers to, keyed by the referring unit.
    references: BTreeMap<FileId, BTreeSet<String>>,
    /// Every complaint a unit makes about its *own* source, as a code and a
    /// position.
    ///
    /// A borrowed `inline` body is analyzed once in every unit that expands
    /// it, and a mistake inside the body is therefore found again in each of
    /// them. Sub-plan E's decision 12 keeps an `inline`'s analysis at its
    /// declaration so that the diagnostic count does not multiply with the
    /// *call* count; this is the same rule against the *unit* count. The
    /// defining unit reports what its own body says; a borrowing unit reports
    /// only what borrowing revealed — a name that resolves there and not here
    /// is exactly the case that has to survive.
    owned: BTreeSet<(FileId, &'static str, u32)>,
    /// What each unit is called, so a diagnostic about a borrowed body can
    /// name the source to go and read.
    units: BTreeMap<FileId, String>,
    /// Functions the workspace call graph put in a cycle, which have no exit
    /// contract to compute and are pinned to the language rule.
    recursive: BTreeSet<String>,
    /// What resolution itself found: a name two units define, an `extern`
    /// the program contradicts, two `extern`s that disagree, an `import`
    /// that reaches nothing.
    diagnostics: Vec<Diagnostic>,
    /// Names already reported as a clash between declarations, or between a
    /// declaration and a definition.
    ///
    /// One complaint per name, however many units join in. Without it the
    /// *count* depends on which unit was collected first — a body seen first
    /// makes every later declaration a separate report, and a declaration
    /// seen first makes the body one — and map order is the entry file's,
    /// so one program would report differently depending on which of its
    /// units was being compiled. That is exactly the property `import`
    /// exists to give (`plans/10-modules-and-ffi.md` decision 3).
    clashed: BTreeSet<String>,
    /// The state the machine is in when this build's entry point runs.
    ///
    /// It lives here because two passes need it and they must not be able to
    /// disagree: the workspace exit classification below walks every
    /// function's body from it, and so does `semantic::analyze_unit`, which
    /// already takes a `&Linkage` as its "everything outside this unit"
    /// parameter. Handing each of them an environment separately is the shape
    /// `memory/entry-abi-assumption-conflict.md` records — two places
    /// documenting one assumption, neither able to detect the other.
    environment: abi::EntryEnvironment,
}

impl Linkage {
    /// Resolves every unit in `map` against every other.
    ///
    /// Two passes, because the question is circular: a unit's references
    /// cannot resolve until the other units' definitions are known, and the
    /// export decision cannot be made until the references are. The first
    /// pass analyzes each unit alone and keeps only what it *defines*; the
    /// second re-analyzes each unit with those definitions visible and keeps
    /// what it *uses*.
    pub fn resolve(map: &SourceMap, environment: abi::EntryEnvironment) -> Self {
        let mut linkage = Self {
            environment,
            ..Self::default()
        };
        for source in map.files() {
            let parse = parser::parse_source(source, None);
            let (model, _) = semantic::analyze_named(&parse.file, source.name(), environment);
            linkage.units.insert(source.id(), source.name().to_owned());
            linkage.collect_imports(map, source, model.program(&parse.file));
            linkage.collect_definitions(source.id(), &model);
            // From the *first* pass, which analyzes each unit alone: a body
            // copied out of a unit that had already copied bodies in would
            // carry someone else's, and this pass is the one where no unit
            // has.
            linkage.collect_inlines(source.id(), model.program(&parse.file), &parse.diagnostics);
        }
        // Before the second pass, because that pass is the first one that
        // asks a unit what it may see.
        linkage.compute_visibility(map);
        for source in map.files() {
            let parse = parser::parse_source(source, None);
            let (model, diagnostics) = semantic::analyze_unit(&parse.file, source, &linkage);
            linkage.collect_references(source.id(), &model);
            linkage.collect_owned_diagnostics(source.id(), &diagnostics);
        }
        linkage.classify_exits(map);
        linkage
    }

    /// Records every `import` in `program`, and which unit each one reaches.
    ///
    /// The edge is resolved here and the file is opened by the loader
    /// (`crate::load_imports`); both ask [`crate::imports`] which file a
    /// string names, so an edge that reaches nothing means the loader could
    /// not read it — never that the two disagreed about what to look for.
    fn collect_imports(
        &mut self,
        map: &SourceMap,
        source: &crate::source::SourceFile,
        program: &AstFile,
    ) {
        let (file, importer) = (source.id(), source.path().to_path_buf());
        let edges: Vec<ImportEdge> = program
            .items
            .iter()
            .filter_map(|item| match &item.kind {
                ItemKind::Import(import) => Some(import),
                _ => None,
            })
            .map(|import| {
                let resolved = crate::imports::resolve(&importer, &import.file.path);
                ImportEdge {
                    span: import.file.span,
                    target: map.locate(&crate::imports::identity(&resolved)),
                    resolved,
                }
            })
            .collect();
        for edge in &edges {
            if edge.target.is_some() {
                continue;
            }
            self.diagnostics.push(
                Diagnostic::error(
                    DiagnosticCode::IMPORT_NOT_FOUND,
                    format!("`import` cannot find `{}`", map.span_text(edge.span)),
                    Label::new(edge.span, "no such unit"),
                )
                .with_note(format!(
                    "K816 looked for `{}`, relative to the directory of `{}`",
                    edge.resolved.display(),
                    map.file(file).name()
                ))
                .with_docs("doc/syntax-reference.md#imports"),
            );
        }
        self.imports.insert(file, edges);
    }

    /// Which units each unit may see.
    ///
    /// Itself and what it imports directly — never what *those* import, which
    /// is the rule that makes a unit mean one thing however it was reached
    /// (`plans/10-modules-and-ffi.md` decision 3).
    fn compute_visibility(&mut self, map: &SourceMap) {
        for source in map.files() {
            let mut visible = BTreeSet::from([source.id()]);
            if let Some(edges) = self.imports.get(&source.id()) {
                visible.extend(edges.iter().filter_map(|edge| edge.target));
            }
            self.visible.insert(source.id(), visible);
        }
    }

    /// Classifies every function's exit contract across the whole workspace.
    ///
    /// A summary taken before resolution is not merely imprecise, it is
    /// unsound. A `wrapper @a16` that calls a foreign `foreign` exports
    /// `Preserve` when analyzed alone, because the call falls back to the
    /// language rule; with the defining unit visible, `foreign` turns out to
    /// leave the accumulator 8-bit and `wrapper` really returns 8-bit. Every
    /// caller that consumed the first summary sizes its immediates 16-bit —
    /// wrong bytes, authored by K816 rather than by the user.
    ///
    /// So the graph is built over the whole workspace, condensed, and
    /// processed in reverse topological order, exactly as it is within one
    /// unit: one pass, no iteration count, and recursion — cross-unit
    /// recursion included — pinned to the language rule.
    fn classify_exits(&mut self, map: &SourceMap) {
        // Summarize: every unit analyzed with the others' definitions
        // visible, which is what makes a foreign callee a name rather than
        // nothing. The exits this run computes are discarded; only the call
        // edges and the entry contracts are kept, and neither depends on
        // them.
        let units: Vec<(FileId, parser::ParseResult, semantic::SemanticModel)> = map
            .files()
            .map(|source| {
                let parse = parser::parse_source(source, None);
                let (model, _) = semantic::analyze_unit(&parse.file, source, self);
                (source.id(), parse, model)
            })
            .collect();

        // Nodes are functions, named globally, and **one node per name**: the
        // graph, the observation fixpoint and the contract table are all keyed
        // by name, so a second node for one name overwrites the first
        // according to traversal order.
        //
        // Two units defining one exported name is already an error, so the
        // only way a name repeats is a *borrowed* `inline` — its declaration
        // is copied into every unit that expands it, and each copy is a
        // function of that unit's model. The copies are skipped here and every
        // edge to the name lands on the defining unit's node, which is the one
        // whose contract is the section's own. Without this a sibling that
        // borrows a body can decide what a *different* unit's function
        // advertises as its exit (`postmortem.md` §G.10.1).
        let mut names: Vec<String> = Vec::new();
        let mut sites: Vec<(usize, usize)> = Vec::new();
        let mut index_of: BTreeMap<String, usize> = BTreeMap::new();
        for (unit, (file, _, model)) in units.iter().enumerate() {
            for (function, info) in model.functions.iter().enumerate() {
                let name = model.symbols.name(info.symbol).to_owned();
                if self
                    .definitions
                    .get(&name)
                    .is_some_and(|definition| definition.file != *file)
                {
                    continue;
                }
                // The premise stated above, checked rather than trusted: an
                // overwrite here is silent, and what it costs is a contract
                // decided by traversal order.
                debug_assert!(
                    !index_of.contains_key(&name),
                    "two workspace nodes named `{name}`"
                );
                index_of.insert(name.clone(), names.len());
                sites.push((unit, function));
                names.push(name);
            }
        }
        let adjacency: Vec<Vec<usize>> = sites
            .iter()
            .map(|(unit, function)| {
                let model = &units[*unit].2;
                model.functions[*function]
                    .callees
                    .iter()
                    .filter_map(|callee| index_of.get(model.symbols.name(*callee)).copied())
                    .collect()
            })
            .collect();

        // What each function's own body observes, plus everything for a body
        // that calls a name no analyzed unit defines: the edge is not in the
        // graph, so the fixpoint below would otherwise not carry it.
        let seeds: Vec<semantic::WidthNeeds> = sites
            .iter()
            .zip(&adjacency)
            .map(|((unit, function), edges)| {
                let info = &units[*unit].2.functions[*function];
                if edges.len() == info.callees.len() {
                    info.needs
                } else {
                    info.needs.union(semantic::WidthNeeds::opaque())
                }
            })
            .collect();
        let observed = width::observation_fixpoint(&seeds, &adjacency);

        // Seeded with what the `extern` declarations promise, before any
        // body is classified. An extern is not a node of this graph — there
        // is nothing to walk — so the fixpoint above never reaches it and
        // the loop below never writes it; what a caller needs is the
        // declaration, and this is where the declaration is published to
        // the units that call it.
        let mut contracts: BTreeMap<String, width::CalleeContract> = self
            .definitions
            .iter()
            .filter(|(_, definition)| definition.origin == Origin::Declared)
            .filter_map(|(name, definition)| {
                definition.contract.map(|contract| (name.clone(), contract))
            })
            .collect();
        for component in width::strongly_connected_components(&adjacency) {
            let recursive = component.len() > 1 || adjacency[component[0]].contains(&component[0]);
            for node in component {
                let (unit, function) = sites[node];
                let (_, parse, model) = &units[unit];
                let info = &model.functions[function];
                // `info.item_index` indexes the program *analysis* walked,
                // which is what the preprocessor left — not what the parser
                // produced. A single top-level directive shifts every index,
                // and the shift is silent: the lookup misses, the callee gets
                // no exit contract, and its caller emits bytes for widths the
                // callee did not leave behind (`postmortem.md` §G.6.9).
                let Some(body) = function_body(model.program(&parse.file), info.item_index) else {
                    continue;
                };
                // The solver needs its callees keyed by *this unit's* symbol
                // ids, so the workspace answers are translated back per unit.
                let callees = local_contracts(model, &contracts);
                let (analysis, _) = width::analyze_function(
                    body,
                    self.environment.body_state(info.entry, info.contract),
                    &width::Context {
                        symbols: &model.symbols,
                        eval: &model.eval,
                        function: Some(info.symbol),
                        callees: &callees,
                        externally_reachable: &BTreeSet::new(),
                    },
                );
                let exit = if recursive {
                    crate::op::ExitContract::preserving()
                } else {
                    width::classify_exit(&analysis).exit
                };
                if recursive {
                    self.recursive.insert(names[node].clone());
                }
                contracts.insert(
                    names[node].clone(),
                    width::CalleeContract {
                        entry: info.contract,
                        exit,
                        observes: observed[node],
                    },
                );
            }
        }

        for (name, contract) in contracts {
            if let Some(definition) = self.definitions.get_mut(&name) {
                definition.contract = Some(contract);
            }
        }
    }

    fn collect_definitions(&mut self, file: FileId, model: &semantic::SemanticModel) {
        for (id, symbol) in model.symbols.iter() {
            if let Some(bytes) = model.eval.sections().get(&symbol.name)
                && symbol.is_unit_level()
                && !symbol.imported
            {
                self.sections
                    .insert(symbol.name.clone(), (file, bytes.clone()));
            }
            if !symbol.is_unit_level() {
                continue;
            }
            if !is_exportable(symbol.kind) {
                // A layout is the one non-exportable kind another unit may
                // still name. An `extern` cannot declare one, so a layout
                // here is always this unit's own.
                if let SymbolKind::AbstractVar { layout } = symbol.kind
                    && !symbol.imported
                {
                    let (arena, root) = model.symbols.layouts().extract(layout);
                    self.layouts.entry(file).or_default().insert(
                        symbol.name.clone(),
                        LayoutDeclaration {
                            declaration: symbol.declaration,
                            arena,
                            root,
                        },
                    );
                }
                continue;
            }
            if let SymbolKind::Var {
                layout: Some(layout),
                ..
            } = symbol.kind
                && !symbol.imported
            {
                let (arena, root) = model.symbols.layouts().extract(layout);
                self.var_layouts.insert(
                    symbol.name.clone(),
                    LayoutDeclaration {
                        declaration: symbol.declaration,
                        arena,
                        root,
                    },
                );
            }
            // In this pass, `imported` can only mean one thing: the unit
            // declared the name `extern`. `declare_imports` is the other way
            // a symbol gets the flag and it runs in the *second* pass, with
            // this table already built.
            let origin = if symbol.imported {
                Origin::Declared
            } else {
                Origin::Body
            };
            let definition = Definition {
                file,
                kind: symbol.kind,
                declaration: symbol.declaration,
                // A body's contract is filled by the third phase, once every
                // callee is known. A declaration's is the declaration.
                contract: model.externs.get(&id).copied(),
                origin,
            };
            match self.definitions.get(&symbol.name) {
                Some(existing) if existing.file == file => {}
                // A definition and a declaration of one name: the program
                // contradicts its own interface, and which of the two is
                // right is not the compiler's to guess.
                Some(existing) if existing.origin != origin => {
                    let (body, declared) = if origin == Origin::Body {
                        (&definition, existing)
                    } else {
                        (existing, &definition)
                    };
                    if self.clashed.insert(symbol.name.clone()) {
                        self.diagnostics.push(
                            Diagnostic::error(
                                DiagnosticCode::EXTERN_COLLIDES_WITH_DEFINITION,
                                format!(
                                    "`{}` is declared `extern` but this program defines it",
                                    symbol.name
                                ),
                                Label::new(declared.declaration, "declared here"),
                            )
                            .with_secondary(Label::new(body.declaration, "and defined here"))
                            .with_note(
                                "an `extern` describes a symbol another object provides; a \
                             definition in the program is the one ld65 will resolve",
                            )
                            .with_help("delete the `extern`, or the definition")
                            .with_docs("doc/syntax-reference.md#extern-declarations"),
                        );
                    }
                    // The body wins, so an editor sends a reader to the truth
                    // rather than to the file that disagrees with it.
                    if origin == Origin::Body {
                        self.definitions.insert(symbol.name.clone(), definition);
                    }
                }
                // Two declarations that disagree. A mismatched address size
                // would be no more than an ld65 *warning*, and a mismatched
                // contract would be a silently wrong byte, so the two are
                // required to be identical and this is where that is asked.
                Some(existing) if origin == Origin::Declared => {
                    if let Some(axis) = disagreement(existing, &definition)
                        && self.clashed.insert(symbol.name.clone())
                    {
                        self.diagnostics.push(
                            Diagnostic::error(
                                DiagnosticCode::EXTERN_DECLARATIONS_DISAGREE,
                                format!(
                                    "`{}` is declared `extern` twice, and the \
                                     declarations disagree",
                                    symbol.name
                                ),
                                Label::new(symbol.declaration, "declared here"),
                            )
                            .with_secondary(Label::new(existing.declaration, "and here"))
                            .with_note(format!("the declarations differ in {axis}"))
                            .with_help(
                                "make them identical, or import one interface unit \
                                 rather than writing the declaration twice",
                            )
                            .with_docs("doc/syntax-reference.md#extern-declarations"),
                        );
                    }
                }
                // One name, two units: an ambiguity ld65 would resolve
                // arbitrarily, so the frontend reports it instead.
                Some(existing) => self.diagnostics.push(
                    Diagnostic::error(
                        DiagnosticCode::DUPLICATE_SYMBOL,
                        format!("`{}` is defined by more than one unit", symbol.name),
                        Label::new(symbol.declaration, "defined here"),
                    )
                    .with_secondary(Label::new(existing.declaration, "and here"))
                    .with_help("rename one of the definitions"),
                ),
                None => {
                    self.definitions.insert(symbol.name.clone(), definition);
                }
            }
            self.declared_in
                .entry(symbol.name.clone())
                .or_default()
                .insert(file);
        }
    }

    /// Keeps the declaration of every `inline` this unit defines.
    ///
    /// The module's default widths are folded into the copy for any axis the
    /// `inline` does not state itself. An `inline`'s body is classified once,
    /// against the section's own contract (`doc/syntax-reference.md`, "Why
    /// aliases are inline-only"), and in its own unit that contract includes
    /// the file-top `@a8` above it — which does not travel with the body.
    /// Writing it onto the copy is what keeps one `inline` one contract
    /// however many units expand it.
    fn collect_inlines(
        &mut self,
        file: FileId,
        program: &AstFile,
        parse_diagnostics: &[Diagnostic],
    ) {
        let defaults = module_defaults(program);
        for item in &program.items {
            let ItemKind::Func(func) = &item.kind else {
                continue;
            };
            if func.kind != crate::ast::FunctionKind::Inline
                || func.name.provenance == Provenance::Synthetic
            {
                continue;
            }
            let mut copy = item.clone();
            let ItemKind::Func(function) = &mut copy.kind else {
                unreachable!("the item was matched as a function above")
            };
            // Discarded: a conflict between two annotations on one section is
            // reported where the section is written, and reporting it again
            // in every unit that expands the section would multiply one
            // mistake by the call graph.
            let mut ignored = Vec::new();
            let stated = width::fold_annotations(&function.width, &mut ignored);
            for (axis, default) in [(0, defaults.a), (1, defaults.i)] {
                let Some(width) = default else { continue };
                if (axis == 0 && stated.a.is_some()) || (axis == 1 && stated.i.is_some()) {
                    continue;
                }
                function.width.push(WidthAnnotation {
                    kind: match (axis, width) {
                        (0, RegisterWidth::W8) => WidthAnnotationKind::A8,
                        (0, _) => WidthAnnotationKind::A16,
                        (_, RegisterWidth::W8) => WidthAnnotationKind::I8,
                        (_, _) => WidthAnnotationKind::I16,
                    },
                    // The section's own name: the annotation was never
                    // written, and a span pointing at the file-top `@a8` of
                    // another unit would send a reader somewhere that does
                    // not explain what they are reading.
                    span: function.name.span,
                });
            }
            // A body the parser had to recover inside is kept but not
            // copied: what the recovery produced is a shorter body, and
            // expanding it would put the compiler's guess in the borrower's
            // object with nothing said.
            let usable = !parse_diagnostics.iter().any(|diagnostic| {
                let at = diagnostic.primary.span;
                at.file == item.span.file && item.span.contains(at.start)
            });
            self.inlines.insert(
                func.name.text.clone(),
                BorrowedInline {
                    file,
                    item: copy,
                    usable,
                },
            );
        }
    }

    /// The `inline` declarations `program` expands but does not contain.
    ///
    /// Returned in the order they are reached, and transitively: a body that
    /// is copied in may itself expand an `inline` from a third unit, and the
    /// copy is worth nothing without it.
    ///
    /// A name `program` already declares is never copied — that is what keeps
    /// the defining unit from importing its own body back — and neither is a
    /// name no unit declares as an `inline`, which stays an ordinary call for
    /// the symbol table to resolve or complain about.
    pub fn inline_expansions(&self, unit: FileId, program: &AstFile) -> BorrowedInlines {
        // "Already declares" is asked of the definitions table rather than of
        // the items, so it covers every kind a name can be: a unit with a
        // `data hop` that calls `hop` must not also be handed a foreign
        // `inline hop` to declare, which would add a duplicate of its own
        // making to a workspace error it already has.
        //
        // `unit` is passed rather than read off the first item's span: a
        // program with no items has no span to read, and one whose items were
        // all removed by a directive has someone else's.
        let defined_here = |name: &str| {
            self.definitions
                .get(name)
                .is_some_and(|definition| definition.file == unit)
        };
        let mut found = BorrowedInlines::default();
        let mut seen: BTreeSet<&str> = BTreeSet::new();
        let mut pending: std::collections::VecDeque<String> =
            called_names(&program.items).into_iter().collect();
        while let Some(name) = pending.pop_front() {
            if defined_here(&name) {
                continue;
            }
            let Some((name, borrowed)) = self.inlines.get_key_value(&name) else {
                continue;
            };
            // A body is copied only out of a unit this one can see. An
            // `inline` two imports away is as invisible as any other name the
            // unit did not ask for.
            if !self.sees(unit, borrowed.file) {
                continue;
            }
            if !seen.insert(name.as_str()) {
                continue;
            }
            let ItemKind::Func(func) = &borrowed.item.kind else {
                continue;
            };
            if !borrowed.usable {
                found.unusable.push((name.clone(), func.name.span));
                continue;
            }
            pending.extend(called_names(std::slice::from_ref(&borrowed.item)));
            found.items.push(borrowed.item.clone());
        }
        found
    }

    /// Records what `file` complains about in its own text.
    fn collect_owned_diagnostics(&mut self, file: FileId, diagnostics: &[Diagnostic]) {
        for diagnostic in diagnostics {
            if diagnostic.primary.span.file == file {
                self.owned
                    .insert((file, diagnostic.code.0, diagnostic.primary.span.start));
            }
        }
    }

    /// The name of the unit `file` is, for a message that has to say where
    /// to look.
    ///
    /// Recorded rather than derived: `analyze` has the unit's name but not the
    /// `SourceMap`, and the one thing it needs about another unit is what to
    /// call it.
    pub fn unit_of(&self, file: FileId) -> &str {
        self.units.get(&file).map_or("another unit", String::as_str)
    }

    /// Whether the unit that owns `diagnostic`'s primary span reports it too.
    ///
    /// Keyed by code and position rather than by the rendered text: the two
    /// runs analyze the same body against the same contract, so a complaint
    /// that is the same complaint lands on the same byte with the same code.
    pub fn reported_by_owner(&self, diagnostic: &Diagnostic) -> bool {
        self.owned.contains(&(
            diagnostic.primary.span.file,
            diagnostic.code.0,
            diagnostic.primary.span.start,
        ))
    }

    fn collect_references(&mut self, file: FileId, model: &semantic::SemanticModel) {
        // A `const` folds into its value wherever it is read, so reading a
        // foreign one links nothing and asks nothing of the defining unit.
        let used: BTreeSet<String> = model
            .referenced
            .iter()
            .map(|id| model.symbols.symbol(*id))
            .filter(|symbol| symbol.imported && !matches!(symbol.kind, SymbolKind::Const { .. }))
            .map(|symbol| symbol.name.clone())
            .collect();
        self.references.insert(file, used);
    }

    /// The unit-level definition of `name`, wherever it lives.
    pub fn definition(&self, name: &str) -> Option<&Definition> {
        self.definitions.get(name)
    }

    /// The workspace-classified contract of `name`, if it names a function.
    ///
    /// A name nothing in the workspace defines has none, and its callers
    /// fall under the documented `Unknown ⇒ Preserve` rule with a call-site
    /// annotation rather than a diagnostic.
    pub fn contract(&self, name: &str) -> Option<width::CalleeContract> {
        self.definitions.get(name).and_then(|d| d.contract)
    }

    /// True when the workspace call graph puts `name` in a cycle, which has
    /// no exit contract to compute and no answer better than the fallback.
    pub fn is_recursive(&self, name: &str) -> bool {
        self.recursive.contains(name)
    }

    /// Every definition in the workspace, in name order.
    /// The data sections the workspace defines, by name, with what each
    /// assembled to.
    /// The data sections `unit` may read, by name, with what each assembled
    /// to.
    pub fn sections(
        &self,
        unit: FileId,
    ) -> impl Iterator<Item = (&str, &crate::eval::SectionBytes)> {
        self.sections
            .iter()
            .filter(move |(_, (owner, _))| self.sees(unit, *owner))
            .map(|(name, (_, bytes))| (name.as_str(), bytes))
    }

    /// Every name the workspace holds, whoever may see it.
    ///
    /// For the callers that ask about a symbol a unit's own table already
    /// has — the width solver, the optimizer — where visibility was decided
    /// when the table was built. A caller deciding what a unit *may* name
    /// wants [`Linkage::visible_definitions`].
    pub fn definitions(&self) -> impl Iterator<Item = (&str, &Definition)> {
        self.definitions
            .iter()
            .map(|(name, definition)| (name.as_str(), definition))
    }

    /// The names `unit` may refer to: what it defines or declares itself,
    /// and what the units it imports directly define or declare.
    pub fn visible_definitions(&self, unit: FileId) -> impl Iterator<Item = (&str, &Definition)> {
        self.definitions
            .iter()
            .filter(move |(name, _)| {
                self.declared_in
                    .get(name.as_str())
                    .is_some_and(|files| files.iter().any(|owner| self.sees(unit, *owner)))
            })
            .map(|(name, definition)| (name.as_str(), definition))
    }

    /// The `abstract var` layouts `unit` may name: its own, and those of the
    /// units it imports directly.
    ///
    /// Separate from [`Linkage::visible_definitions`] because a layout is not
    /// a definition and does not travel as one — the caller interns the arena
    /// and builds the kind with the id it gets back.
    pub fn visible_layouts(
        &self,
        unit: FileId,
    ) -> impl Iterator<Item = (&str, &LayoutDeclaration)> {
        self.layouts
            .iter()
            .filter(move |(owner, _)| self.sees(unit, **owner))
            .flat_map(|(_, layouts)| layouts.iter())
            .map(|(name, declaration)| (name.as_str(), declaration))
    }

    /// The field layout the `var` named `name` was declared with, when it
    /// was declared with one.
    ///
    /// Carried as data for the same reason [`Linkage::visible_layouts`] is.
    /// Visibility is the variable's own: a caller asks about a name it found
    /// among [`Linkage::visible_definitions`].
    pub fn var_layout(&self, name: &str) -> Option<&LayoutDeclaration> {
        self.var_layouts.get(name)
    }

    /// Whether `unit` may see what `owner` declares.
    fn sees(&self, unit: FileId, owner: FileId) -> bool {
        unit == owner
            || self
                .visible
                .get(&unit)
                .is_some_and(|files| files.contains(&owner))
    }

    /// The units `unit` may see, itself included.
    ///
    /// Empty for a file this workspace does not hold. Public because the
    /// relation is the thing sub-plan K introduced and a reader of a program
    /// — a test, an editor asking why a name does not resolve — has no other
    /// way to ask what a unit was allowed to name.
    pub fn visible_from(&self, unit: FileId) -> BTreeSet<FileId> {
        self.visible.get(&unit).cloned().unwrap_or_default()
    }

    /// True when a unit other than `file` refers to `name`.
    ///
    /// No longer an export decision — sub-plan K made those unconditional —
    /// but still two other questions' answer: whether a global label may be
    /// jumped to from outside, and whether `main`'s exit is published.
    pub fn referenced_elsewhere(&self, name: &str, file: FileId) -> bool {
        self.references
            .iter()
            .any(|(referrer, names)| *referrer != file && names.contains(name))
    }

    /// The entry environment this workspace was resolved for.
    pub fn environment(&self) -> abi::EntryEnvironment {
        self.environment
    }

    /// What resolving the workspace found, which is reported before anything
    /// is emitted: a name two units define, an `import` that reaches
    /// nothing, and — once sub-plan K's task K6 lands — an `extern` the
    /// program contradicts.
    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }
}

/// The module's default widths, folded from the annotations above the first
/// section.
///
/// A syntactic reading of the same rule `semantic.rs` applies, because this
/// runs before any analysis of the unit that is about to borrow a body. An
/// annotation written *below* a section is an error there and is ignored
/// here; the copy takes what the defining unit's sections actually got.
fn module_defaults(program: &AstFile) -> crate::op::WidthContract {
    let mut annotations = Vec::new();
    for item in &program.items {
        match &item.kind {
            ItemKind::Func(_) => break,
            ItemKind::ModuleWidth(annotation) => annotations.push(annotation.clone()),
            _ => {}
        }
    }
    width::fold_annotations(&annotations, &mut Vec::new())
}

/// Every name written as a call anywhere in `items`, at any depth.
///
/// A *syntactic* walk, and it has to be: it decides which foreign bodies join
/// this unit's program, and that has to be settled before a symbol is
/// declared. The two call shapes are `call NAME` and the bare name; a `goto`
/// or an operand naming an `inline` is an error rather than an expansion
/// (`K816-E0115`), so neither is a reason to copy a body in.
fn called_names(items: &[Item]) -> BTreeSet<String> {
    let mut names = BTreeSet::new();
    for item in items {
        match &item.kind {
            ItemKind::Func(func) => called_in_block(&func.body, &mut names),
            ItemKind::Data(data) => called_in_data(&data.entries, &mut names),
            ItemKind::Var(_)
            | ItemKind::Const(_)
            | ItemKind::Segment(_)
            | ItemKind::EvalBlock(_)
            | ItemKind::EvalFunc(_)
            | ItemKind::Preprocessor(_)
            | ItemKind::Binary(_)
            | ItemKind::Reserved(_)
            | ItemKind::ModuleWidth(_)
            | ItemKind::Import(_)
            | ItemKind::Extern(_)
            | ItemKind::Error => {}
        }
    }
    names
}

fn called_in_block(block: &Block, names: &mut BTreeSet<String>) {
    for statement in &block.statements {
        called_in_statement(statement, names);
    }
}

fn called_in_statement(statement: &Statement, names: &mut BTreeSet<String>) {
    match &statement.kind {
        StatementKind::Call(call) => {
            names.insert(call.target.text.clone());
        }
        StatementKind::BareCall(call) => {
            names.insert(call.target.text.clone());
        }
        StatementKind::WidthBlock(block) => called_in_block(&block.body, names),
        StatementKind::Conditional(conditional) => {
            called_in_block(&conditional.body, names);
            if let Some(otherwise) = &conditional.otherwise {
                called_in_block(otherwise, names);
            }
        }
        StatementKind::ConditionalJump(jump) => called_in_statement(&jump.jump, names),
        StatementKind::Loop(loop_statement) => called_in_block(&loop_statement.body, names),
        StatementKind::Nocross(block) => called_in_block(block, names),
        StatementKind::Data(data) => called_in_data(&data.entries, names),
        StatementKind::Label(_)
        | StatementKind::Native(_)
        | StatementKind::Hla(_)
        | StatementKind::ModeSet(_)
        | StatementKind::Goto(_)
        | StatementKind::Break
        | StatementKind::Repeat
        | StatementKind::Align(_)
        | StatementKind::Address(_)
        | StatementKind::Reserved(_)
        | StatementKind::Preprocessor(_)
        | StatementKind::Return(_)
        | StatementKind::Error => {}
    }
}

fn called_in_data(entries: &[DataEntry], names: &mut BTreeSet<String>) {
    for entry in crate::ast::flat_data_entries(entries) {
        match &entry.kind {
            DataEntryKind::Code { body, .. } => called_in_block(body, names),
            DataEntryKind::Repeat { body, .. } => called_in_data(&body.entries, names),
            DataEntryKind::NocrossGroup(_)
            | DataEntryKind::Segment(_)
            | DataEntryKind::Align(_)
            | DataEntryKind::Address(_)
            | DataEntryKind::Nocross
            | DataEntryKind::Charset(_)
            | DataEntryKind::Label(_)
            | DataEntryKind::Typed { .. }
            | DataEntryKind::Value(_)
            | DataEntryKind::Evaluator(_)
            | DataEntryKind::ForEval { .. }
            | DataEntryKind::Preprocessor(_)
            | DataEntryKind::Binary(_)
            | DataEntryKind::Reserved(_)
            | DataEntryKind::Error => {}
        }
    }
}

/// The body of the code section at `item_index`.
fn function_body(file: &AstFile, item_index: usize) -> Option<&crate::ast::Block> {
    match &file.items.get(item_index)?.kind {
        ItemKind::Func(func) => Some(&func.body),
        _ => None,
    }
}

/// Translates workspace contracts, which are keyed by name, into the symbol
/// ids one unit's solver uses.
fn local_contracts(
    model: &semantic::SemanticModel,
    contracts: &BTreeMap<String, width::CalleeContract>,
) -> BTreeMap<SymbolId, width::CalleeContract> {
    model
        .symbols
        .iter()
        .filter_map(|(id, symbol)| contracts.get(&symbol.name).map(|contract| (id, *contract)))
        .collect()
}

/// True when a symbol kind can cross a unit boundary at all.
///
/// A local label cannot: it is scoped to one function by definition. An
/// `abstract var` cannot either, and for a stronger reason — it has no place
/// and no value, so there is nothing for ld65 to resolve, and its field
/// offsets belong to the arena of the unit that declared it. Two units may
/// therefore declare the same layout without that being a duplicate
/// definition, which is what a shared header needs. Every other unit-level
/// kind can cross, including a `const`, whose value another unit reaches
/// through an emitted equate rather than by folding.
fn is_exportable(kind: SymbolKind) -> bool {
    !matches!(
        kind,
        SymbolKind::Label {
            scope: LabelScope::Local,
            ..
        } | SymbolKind::AbstractVar { .. }
    )
}

/// What two `extern` declarations of one name disagree about, if anything.
///
/// Named rather than merely detected: "the declarations disagree" sends the
/// author to compare two lines character by character, and "they differ in
/// the calling convention" is the answer.
fn disagreement(left: &Definition, right: &Definition) -> Option<&'static str> {
    if left.kind == right.kind && left.contract == right.contract {
        return None;
    }
    if std::mem::discriminant(&left.kind) != std::mem::discriminant(&right.kind) {
        return Some("what kind of thing they declare");
    }
    if let (SymbolKind::Function { size: a }, SymbolKind::Function { size: b }) =
        (left.kind, right.kind)
        && a != b
    {
        return Some("the calling convention: one is `far func` and the other is not");
    }
    if left.kind.address_size() != right.kind.address_size() {
        return Some("the address size");
    }
    if left.kind != right.kind {
        return Some("the declared width");
    }
    Some("the width contract")
}

/// Whether a unit-level definition of this kind leaves the object file.
///
/// **Unconditionally, if at all.** Until sub-plan K a `const` and a
/// fixed-address `var` were exported only when another unit was seen to
/// reference them, on the reasoning that a value K816 folds needs no name in
/// the object file. That reasoning cannot survive per-file compilation: an
/// exporter cannot see who asks. What replaces it is what cc65 itself does —
/// `symtab.c::EmitExternals` exports every non-`static` definition and
/// nothing in a C source marks either direction — so the condition is
/// *removed* rather than replaced by a marker
/// (`plans/10-modules-and-ffi.md` decision 8).
///
/// What still does not leave: an alias, which is scoped to one `inline` and
/// vanishes at expansion; an `inline` itself, which has no address to hand
/// anyone because its body is expanded at each use site; a local label; and
/// an `abstract var`, which is a layout rather than a place or a value.
pub fn exported(kind: SymbolKind) -> bool {
    match kind {
        SymbolKind::Function { .. }
        | SymbolKind::DataLabel { .. }
        | SymbolKind::Var { .. }
        | SymbolKind::Const { .. } => true,
        SymbolKind::Label { scope, .. } => scope == LabelScope::Global,
        SymbolKind::AbstractVar { .. } | SymbolKind::Inline | SymbolKind::Alias { .. } => false,
        // `AbstractVar` never reaches here — `is_exportable` keeps it out of
        // the workspace — and the arm stays so that adding a kind is a
        // compile error rather than a silent default.
    }
}
