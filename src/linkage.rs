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
use crate::symbol::{LabelScope, SymbolKind};

/// One unit-level definition, and the unit that owns it.
#[derive(Clone, Debug)]
pub struct Definition {
    pub file: FileId,
    pub kind: SymbolKind,
    pub declaration: Span,
}

/// What every unit in a workspace defines, and which foreign names each one
/// uses.
#[derive(Debug, Default)]
pub struct Linkage {
    definitions: BTreeMap<String, Definition>,
    /// Foreign names each unit refers to, keyed by the referring unit.
    references: BTreeMap<FileId, BTreeSet<String>>,
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
            let (model, _) = semantic::analyze_file(&parse.file);
            linkage.collect_definitions(source.id(), &model);
        }
        for source in map.files() {
            let parse = parser::parse_source(source, None);
            let (model, _) = semantic::analyze_unit(&parse.file, &linkage);
            linkage.collect_references(source.id(), &model);
        }
        linkage
    }

    fn collect_definitions(&mut self, file: FileId, model: &semantic::SemanticModel) {
        for (_, symbol) in model.symbols.iter() {
            if symbol.imported || !symbol.is_unit_level() || !is_exportable(symbol.kind) {
                continue;
            }
            let definition = Definition {
                file,
                kind: symbol.kind,
                declaration: symbol.declaration,
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

    /// Every definition in the workspace, in name order.
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

/// True when a symbol kind can cross a unit boundary at all.
///
/// A local label cannot: it is scoped to one function by definition. Every
/// other unit-level kind can, including a `const`, whose value another unit
/// reaches through an emitted equate rather than by folding.
fn is_exportable(kind: SymbolKind) -> bool {
    !matches!(
        kind,
        SymbolKind::Label {
            scope: LabelScope::Local,
            ..
        }
    )
}

/// Whether a definition is exported unconditionally, or only when another
/// unit actually asks for it.
///
/// Functions, global labels, and data-block labels name *places*, and a
/// place is the only thing another unit can be handed. A `const` or a
/// fixed-address `var` is a value K816 folds, so exporting one that nobody
/// reads would put a name in the object file for no reason
/// (`plans/feature-manifest.md` §3).
pub fn always_exported(kind: SymbolKind) -> bool {
    match kind {
        SymbolKind::Function { .. } | SymbolKind::DataLabel { .. } => true,
        SymbolKind::Label { scope, .. } => scope == LabelScope::Global,
        SymbolKind::Const { .. } | SymbolKind::Var { .. } => false,
    }
}
