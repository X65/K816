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

//! Linkage is derived, not declared.
//!
//! K65 has no `import`, `export`, or `extern` syntax: the legacy linker
//! resolved every name globally across a project's units, so there is
//! nothing in the source to read. K816 therefore works out what crosses a
//! unit boundary from the symbol tables themselves
//! (`plans/feature-manifest.md` §3).
//!
//! A name one unit defines and another uses becomes an `.export` in the
//! first and an `.import` in the second, both carrying the symbol's address
//! size — ca65 validates addressing modes against an imported symbol's
//! address size, so getting that wrong shows up as an assembly error rather
//! than a wrong byte. A name *nothing* defines stays an undeclared-symbol
//! error: the frontend does not invent an import to make an unresolved name
//! someone else's problem. Two units defining one exported name is a
//! frontend error too, rather than an ambiguity handed to ld65.

use std::collections::{BTreeMap, BTreeSet};

use crate::diagnostic::{Diagnostic, DiagnosticCode, Label};
use crate::parser;
use crate::semantic;
use crate::source::{FileId, SourceMap, Span};
use crate::symbol::{LabelScope, SymbolId, SymbolKind};
use crate::width;

/// One unit-level definition, and the unit that owns it.
#[derive(Clone, Debug)]
pub struct Definition {
    pub file: FileId,
    pub kind: SymbolKind,
    pub declaration: Span,
    /// For a code section: the widths it assumes on entry and what a caller
    /// may believe afterwards, classified across the whole workspace.
    ///
    /// `None` until the third resolution phase fills it, and for everything
    /// that is not a function.
    pub contract: Option<width::CalleeContract>,
}

/// What every unit in a workspace defines, and which foreign names each one
/// uses.
#[derive(Debug, Default)]
pub struct Linkage {
    definitions: BTreeMap<String, Definition>,
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
    sections: BTreeMap<String, crate::eval::SectionBytes>,
    /// Foreign names each unit refers to, keyed by the referring unit.
    references: BTreeMap<FileId, BTreeSet<String>>,
    /// Functions the workspace call graph put in a cycle, which have no exit
    /// contract to compute and are pinned to the language rule.
    recursive: BTreeSet<String>,
    duplicates: Vec<Diagnostic>,
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
    pub fn resolve(map: &SourceMap) -> Self {
        let mut linkage = Self::default();
        for source in map.files() {
            let parse = parser::parse_source(source, None);
            let (model, _) = semantic::analyze_named(&parse.file, source.name());
            linkage.collect_definitions(source.id(), &model);
        }
        for source in map.files() {
            let parse = parser::parse_source(source, None);
            let (model, _) = semantic::analyze_unit(&parse.file, source.name(), &linkage);
            linkage.collect_references(source.id(), &model);
        }
        linkage.classify_exits(map);
        linkage
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
                let (model, _) = semantic::analyze_unit(&parse.file, source.name(), self);
                (source.id(), parse, model)
            })
            .collect();

        // Nodes are functions, named globally: a name is what crosses a unit
        // boundary, and it is unique across the workspace because two units
        // defining one exported name is already an error.
        let mut names: Vec<String> = Vec::new();
        let mut sites: Vec<(usize, usize)> = Vec::new();
        let mut index_of: BTreeMap<String, usize> = BTreeMap::new();
        for (unit, (_, _, model)) in units.iter().enumerate() {
            for (function, info) in model.functions.iter().enumerate() {
                let name = model.symbols.name(info.symbol).to_owned();
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

        let mut contracts: BTreeMap<String, width::CalleeContract> = BTreeMap::new();
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
                    crate::abi::entry_state(info.entry, info.contract),
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
        for (_, symbol) in model.symbols.iter() {
            if let Some(bytes) = model.eval.sections().get(&symbol.name)
                && symbol.is_unit_level()
                && !symbol.imported
            {
                self.sections.insert(symbol.name.clone(), bytes.clone());
            }
            if symbol.imported || !symbol.is_unit_level() || !is_exportable(symbol.kind) {
                continue;
            }
            let definition = Definition {
                file,
                kind: symbol.kind,
                declaration: symbol.declaration,
                // Filled by the third phase, once every callee is known.
                contract: None,
            };
            match self.definitions.get(&symbol.name) {
                // One name, two units: an ambiguity ld65 would resolve
                // arbitrarily, so the frontend reports it instead.
                Some(existing) if existing.file != file => self.duplicates.push(
                    Diagnostic::error(
                        DiagnosticCode::DUPLICATE_SYMBOL,
                        format!("`{}` is defined by more than one unit", symbol.name),
                        Label::new(symbol.declaration, "defined here"),
                    )
                    .with_secondary(Label::new(existing.declaration, "and here"))
                    .with_help("rename one of the definitions"),
                ),
                Some(_) => {}
                None => {
                    self.definitions.insert(symbol.name.clone(), definition);
                }
            }
        }
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
    pub fn sections(&self) -> impl Iterator<Item = (&str, &crate::eval::SectionBytes)> {
        self.sections
            .iter()
            .map(|(name, bytes)| (name.as_str(), bytes))
    }

    pub fn definitions(&self) -> impl Iterator<Item = (&str, &Definition)> {
        self.definitions
            .iter()
            .map(|(name, definition)| (name.as_str(), definition))
    }

    /// True when a unit other than `file` refers to `name`.
    pub fn referenced_elsewhere(&self, name: &str, file: FileId) -> bool {
        self.references
            .iter()
            .any(|(referrer, names)| *referrer != file && names.contains(name))
    }

    /// The cross-unit duplicate-definition errors, which are reported before
    /// anything is emitted.
    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.duplicates
    }
}

/// The body of the code section at `item_index`.
fn function_body(file: &crate::ast::AstFile, item_index: usize) -> Option<&crate::ast::Block> {
    match &file.items.get(item_index)?.kind {
        crate::ast::ItemKind::Func(func) => Some(&func.body),
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

/// Whether a definition is exported unconditionally, or only when another
/// unit actually asks for it.
///
/// Functions, global labels, and data-block labels name *places*, and a
/// place is the only thing another unit can be handed. A `const` or a
/// fixed-address `var` is a value K816 folds, so exporting one that nobody
/// reads would put a name in the object file for no reason
/// (`plans/feature-manifest.md` §3). An `abstract var` has no place and no
/// value: it is a layout, and there is nothing to hand anyone.
pub fn always_exported(kind: SymbolKind) -> bool {
    match kind {
        SymbolKind::Function { .. } | SymbolKind::DataLabel { .. } => true,
        SymbolKind::Label { scope, .. } => scope == LabelScope::Global,
        // An alias is scoped to one `inline` and vanishes at expansion, so
        // there is no place and no value for another unit to be handed.
        // An `inline` has no address to hand anyone: its body is expanded
        // at each use site, and a use site in another unit has no body to
        // expand. Sharing one is a source-level question, not a link-time
        // one.
        SymbolKind::Const { .. }
        | SymbolKind::Var { .. }
        | SymbolKind::AbstractVar { .. }
        | SymbolKind::Inline
        | SymbolKind::Alias { .. } => false,
        // `AbstractVar` never reaches here — `is_exportable` keeps it out of
        // the workspace — and the arm stays so that adding a kind is a
        // compile error rather than a silent default.
    }
}
