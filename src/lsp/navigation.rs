/*****************************************************************************/
/*                                                                           */
/*                               navigation.rs                               */
/*                                                                           */
/*      Definition, references, and the symbol table as an outline           */
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

//! Definition, references, and the symbol table rendered as an outline.
//!
//! Every answer here comes from the reference index the semantic pass
//! recorded while resolving names, and from `Symbol.declaration`. Nothing
//! searches text, and nothing re-resolves a name: an editor that disagreed
//! with the compiler about what a name means would be worse than one that
//! answered nothing.
//!
//! **A symbol's identity across units is its declaration span and its name.**
//! A `SymbolId` indexes one unit's table and means nothing in another, but an
//! *imported* symbol carries the declaration span of the unit that defines it
//! — so the span is the shared identity, and the name goes with it because a
//! span is not unique on its own: every evaluator variable one bracket
//! assigns is declared at that bracket.

use crate::source::{FileId, Span};
use crate::symbol::{LabelScope, SymbolId, SymbolKind};
use crate::{Analysis, AnalyzedUnit};

use super::context::at;

/// What identifies a symbol across the units of a workspace.
#[derive(Clone, Debug, PartialEq, Eq)]
struct Identity {
    declaration: Span,
    name: String,
}

fn identity(unit: &AnalyzedUnit, id: SymbolId) -> Identity {
    let symbol = unit.model.symbols.symbol(id);
    Identity {
        declaration: symbol.declaration,
        name: symbol.name.clone(),
    }
}

/// The declaration the position at `offset` points at.
///
/// A position on a declaring name answers with itself, which is what an
/// editor shows when a user asks a declaration where it is declared.
pub fn definition(analysis: &Analysis, file: FileId, offset: u32) -> Option<Span> {
    let context = at(analysis, file, offset)?;
    if let Some(id) = context.symbol() {
        return Some(context.unit.model.symbols.symbol(id).declaration);
    }
    let (layout, index) = context.field?;
    Some(
        context
            .unit
            .model
            .symbols
            .layout(layout)
            .fields
            .get(index)?
            .declaration,
    )
}

/// Every place the thing at `offset` is written.
///
/// Across every analyzed unit, because a symbol one unit defines is used by
/// the others that import it — and, with decision 2's program, "every
/// analyzed unit" is every document the editor has open.
pub fn references(
    analysis: &Analysis,
    file: FileId,
    offset: u32,
    include_declaration: bool,
) -> Vec<Span> {
    let Some(context) = at(analysis, file, offset) else {
        return Vec::new();
    };
    let mut found: Vec<Span> = Vec::new();

    if let Some(id) = context.symbol() {
        let wanted = identity(context.unit, id);
        if include_declaration {
            found.push(wanted.declaration);
        }
        for unit in &analysis.units {
            for (span, other) in &unit.model.references {
                if identity(unit, *other) == wanted {
                    found.push(*span);
                }
            }
        }
    } else if let Some((layout, index)) = context.field {
        let field = context.unit.model.symbols.layout(layout).fields.get(index);
        let Some((declaration, declaring_name)) =
            field.map(|field| (field.declaration, field.name_span))
        else {
            return Vec::new();
        };
        if include_declaration {
            found.push(declaring_name);
        }
        for unit in &analysis.units {
            for (span, (other_layout, other_index)) in &unit.model.field_references {
                let matches = unit
                    .model
                    .symbols
                    .layout(*other_layout)
                    .fields
                    .get(*other_index)
                    .is_some_and(|field| field.declaration == declaration);
                if matches {
                    found.push(*span);
                }
            }
        }
    }

    found.sort();
    found.dedup();
    found
}

/// What a symbol is, for an editor's outline.
///
/// K816's own kinds, mapped onto the protocol's by the layer that speaks it:
/// the protocol has one list for every language, and which of its entries a
/// `data` label belongs in is a rendering decision, not a fact about K816.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum OutlineKind {
    Function,
    Inline,
    Variable,
    Constant,
    Field,
    Label,
    Data,
}

/// One entry of an editor's outline.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Outline {
    pub name: String,
    pub kind: OutlineKind,
    /// The declaring name, which is what the editor selects.
    pub declaration: Span,
    /// The whole construct, which is what the editor highlights. Equal to
    /// `declaration` where the model holds nothing wider.
    pub full: Span,
    /// A short suffix the editor shows beside the name — an address, a
    /// value, a contract.
    pub detail: Option<String>,
    pub children: Vec<Outline>,
}

fn outline_kind(kind: &SymbolKind) -> OutlineKind {
    match kind {
        SymbolKind::Const { .. } => OutlineKind::Constant,
        SymbolKind::Var { .. } | SymbolKind::AbstractVar { .. } => OutlineKind::Variable,
        SymbolKind::Label { .. } => OutlineKind::Label,
        SymbolKind::DataLabel { .. } => OutlineKind::Data,
        SymbolKind::Inline => OutlineKind::Inline,
        SymbolKind::Function { .. } => OutlineKind::Function,
        // An `inline` alias stands for whatever the use site substitutes; it
        // is a name in the section's scope and nothing wider.
        SymbolKind::Alias { .. } => OutlineKind::Variable,
    }
}

/// The unit's own declarations, hierarchically.
///
/// Only what this unit declares: an imported symbol's declaration is in
/// another document, and listing it here would put a second copy of that
/// document's outline inside this one.
pub fn document_symbols(analysis: &Analysis, file: FileId) -> Vec<Outline> {
    let Some(unit) = analysis.unit(file) else {
        return Vec::new();
    };
    let symbols = &unit.model.symbols;

    // A function's full range is its item's span, which is the only wider
    // extent the model holds for any declaration.
    let mut extents: Vec<(SymbolId, Span)> = Vec::new();
    for info in &unit.model.functions {
        if let Some(item) = unit.program().items.get(info.item_index) {
            extents.push((info.symbol, item.span));
        }
    }

    let mut top: Vec<Outline> = Vec::new();
    for (id, symbol) in symbols.iter() {
        if symbol.imported || !symbol.is_unit_level() || symbol.declaration.file != file {
            continue;
        }
        let full = extents
            .iter()
            .find(|(owner, _)| *owner == id)
            .map(|(_, span)| *span)
            .unwrap_or(symbol.declaration);
        top.push(Outline {
            name: symbol.name.clone(),
            kind: outline_kind(&symbol.kind),
            declaration: symbol.declaration,
            full,
            detail: detail(&symbol.kind),
            children: fields(unit, &symbol.kind),
        });
    }

    // A local label belongs under the function it is scoped to.
    for (_, symbol) in symbols.iter() {
        let Some(owner) = symbol.scope else {
            continue;
        };
        if symbol.declaration.file != file {
            continue;
        }
        let owner_name = symbols.name(owner);
        let Some(parent) = top.iter_mut().find(|outline| {
            outline.name == owner_name && outline.full.contains(symbol.declaration.start)
        }) else {
            continue;
        };
        parent.children.push(Outline {
            name: symbol.name.clone(),
            kind: outline_kind(&symbol.kind),
            declaration: symbol.declaration,
            full: symbol.declaration,
            detail: None,
            children: Vec::new(),
        });
    }

    for outline in &mut top {
        outline.children.sort_by_key(|child| child.declaration);
    }
    top.sort_by_key(|outline| outline.declaration);
    top
}

/// A laid-out variable's fields, as children of its outline entry.
fn fields(unit: &AnalyzedUnit, kind: &SymbolKind) -> Vec<Outline> {
    let layout = match kind {
        SymbolKind::Var {
            layout: Some(layout),
            ..
        }
        | SymbolKind::AbstractVar { layout } => *layout,
        _ => return Vec::new(),
    };
    unit.model
        .symbols
        .layout(layout)
        .fields
        .iter()
        .map(|field| Outline {
            name: format!(".{}", field.name),
            kind: OutlineKind::Field,
            declaration: field.declaration,
            full: field.declaration,
            detail: Some(format!("+{}", field.offset)),
            children: Vec::new(),
        })
        .collect()
}

/// The one-line suffix an editor shows beside a name.
fn detail(kind: &SymbolKind) -> Option<String> {
    match kind {
        SymbolKind::Const { value: Some(value) } if (*value as i32) < 0 => {
            Some(format!("= {}", *value as i32))
        }
        SymbolKind::Const { value: Some(value) } => Some(format!("= ${value:04X}")),
        SymbolKind::Const { value: None } => Some("extern".to_owned()),
        SymbolKind::Var {
            address: Some(address),
            ..
        } => Some(format!("@ ${address:04X}")),
        _ => None,
    }
}

/// Every unit-level declaration in the workspace whose name contains `query`,
/// case-insensitively. An empty query matches everything.
pub fn workspace_symbols(analysis: &Analysis, query: &str) -> Vec<(FileId, Outline)> {
    let query = query.to_ascii_lowercase();
    let mut found = Vec::new();
    for unit in &analysis.units {
        for outline in document_symbols(analysis, unit.file) {
            if outline.name.to_ascii_lowercase().contains(&query) {
                found.push((unit.file, outline));
            }
        }
    }
    found
}

/// Whether `kind` is a label scoped to one function.
pub fn is_local_label(kind: &SymbolKind) -> bool {
    matches!(
        kind,
        SymbolKind::Label {
            scope: LabelScope::Local,
            ..
        }
    )
}
