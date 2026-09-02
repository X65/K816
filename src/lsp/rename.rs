/*****************************************************************************/
/*                                                                           */
/*                                 rename.rs                                 */
/*                                                                           */
/*        Semantic rename: the occurrences the compiler resolved, and         */
/*                    the refusals it would make itself                       */
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

//! Rename, from the reference index rather than from the text.
//!
//! The occurrences are exactly the ones the compiler resolved, so an
//! identifier that happens to spell the same thing in a comment, in a string,
//! or as a *different* function's local label is not touched. That is the
//! phase-11 criterion "rename uses semantic references rather than textual
//! replacement", and the fixture that makes it checkable puts all four in one
//! file.
//!
//! **The refusals are the compiler's own.** A new name that names a register
//! is refused with `compat::reserved_name`'s words, and a collision with the
//! duplicate-declaration wording — because a rename an editor accepts and the
//! next build rejects is worse than one the editor declined. Nothing is
//! applied here: the answer is a list of edits, the client applies them, and
//! the next `didChange` re-analyzes.

use std::collections::BTreeMap;

use crate::source::{FileId, Span};
use crate::symbol::{LayoutId, SymbolId, SymbolKind};
use crate::{Analysis, AnalyzedUnit};

use super::context::{Context, at};
use super::navigation::references;

/// What a rename would change: one replacement per occurrence.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Rename {
    /// Every span to replace, in source order, grouped by file.
    pub edits: BTreeMap<FileId, Vec<Span>>,
    /// The text each of them becomes. A local label keeps its leading `.`
    /// and a field keeps its own, so this is not always the typed name.
    pub replacement: String,
}

impl Rename {
    /// Every span this rename would replace, across every file.
    pub fn spans(&self) -> impl Iterator<Item = Span> + '_ {
        self.edits.values().flatten().copied()
    }
}

/// Why a rename cannot happen.
///
/// A sentence, not a code: it is answered to a `rename` request, which the
/// protocol renders as a message beside the editor's rename box.
pub type Refusal = String;

/// What `prepareRename` answers: the span whose text the editor should offer
/// for editing.
///
/// `None` for every position that is not a name this compiler resolved — a
/// mnemonic, a register or flag token, a keyword, whitespace, a name the
/// parser invented for a `MISSING` node, and a position inside a comment or
/// a string.
pub fn prepare(analysis: &Analysis, file: FileId, offset: u32) -> Option<Span> {
    let context = at(analysis, file, offset)?;
    renameable(&context)?;
    context.name_span()
}

/// What the position names, when it names something a rename could move.
///
/// The gate `prepare` and `rename` share, so the editor never offers a rename
/// box for something `rename` would then refuse.
fn renameable<'a>(context: &Context<'a>) -> Option<Target<'a>> {
    if !context.on_token() {
        return None;
    }
    // A register, a flag letter and a mnemonic are tokens of the language,
    // not names: the grammar gives them their own node kinds, so this asks
    // the grammar rather than matching on spelling.
    if matches!(
        context.node_kind(),
        "register" | "flag_name" | "mnemonic" | "index_register" | "width_annotation"
    ) {
        return None;
    }
    if let Some(id) = context.symbol() {
        let symbol = context.unit.model.symbols.symbol(id);
        // A name the parser invented for a `MISSING` node covers no source
        // text, so there is nothing to replace.
        if symbol.declaration.is_empty() {
            return None;
        }
        // An `inline` alias binds *positionally* against the use site's
        // arguments (`CallFlow.inputs` is in written order), so renaming one
        // is renaming the contract, and a use site inside an expanded body
        // carries the defining unit's spans. Refused rather than reasoned
        // about (`plans/07-lsp-tooling.md`'s risk note on borrowed bodies).
        if matches!(symbol.kind, SymbolKind::Alias { .. }) {
            return None;
        }
        return Some(Target::Symbol(id, context.unit));
    }
    let (layout, index) = context.field?;
    // A field of a layout this unit does not own is another unit's offsets;
    // a field list does not cross a unit boundary, so there is nothing here
    // to rename consistently.
    context
        .unit
        .model
        .symbols
        .layout(layout)
        .fields
        .get(index)
        .filter(|field| field.declaration.file == context.unit.file)?;
    Some(Target::Field(layout, context.unit))
}

/// What the position under the cursor names.
enum Target<'a> {
    Symbol(SymbolId, &'a AnalyzedUnit),
    /// A `.field` step or a field declaration, named by its layout and its
    /// position in it. Fields carry no `SymbolId`, so the occurrences come
    /// from the field index rather than the symbol table — and so does the
    /// collision check, because a field's scope is its own layout.
    Field(LayoutId, &'a AnalyzedUnit),
}

/// The edits that rename what is at `offset` to `new_name`.
///
/// Every occurrence the compiler resolved, across every open unit — so an
/// imported symbol is renamed at its definition and at each importer. The
/// leading `.` of a local label and of a field is kept: it is part of the
/// spelling at every occurrence, and typing it again in the rename box would
/// double it.
pub fn rename(
    analysis: &Analysis,
    file: FileId,
    offset: u32,
    new_name: &str,
) -> Result<Rename, Refusal> {
    let Some(context) = at(analysis, file, offset) else {
        return Err("this document is not open in the workspace".to_owned());
    };
    let Some(target) = renameable(&context) else {
        return Err("there is nothing here that K816 resolved to a name".to_owned());
    };

    let typed = new_name.trim();
    if typed.is_empty() {
        return Err("a name cannot be empty".to_owned());
    }
    // A local label and a field keep their leading `.` at every occurrence,
    // so it is part of the spelling rather than part of the name — and a user
    // who types it back into the rename box must not get `..name`.
    let current = current_text(&context);
    let dotted = current.starts_with('.');
    let bare = typed.trim_start_matches('.');
    let replacement = if dotted {
        format!(".{bare}")
    } else {
        bare.to_owned()
    };
    if replacement == current {
        return Err(format!("`{current}` is already its name"));
    }

    // An `inline` parameter may take a register or flag spelling, because it
    // is in no namespace to collide with one — so the editor must allow what
    // the compiler allows, here as much as the other way round.
    let alias = context.symbol().is_some_and(|id| {
        matches!(
            context.unit.model.symbols.kind(id),
            SymbolKind::Alias { .. }
        )
    });
    check_spelling(bare, &replacement, alias)?;

    let spans = references(analysis, file, offset, true);
    if spans.is_empty() {
        return Err("K816 resolved no occurrences of this name".to_owned());
    }
    match target {
        Target::Symbol(id, unit) => check_collision(analysis, unit, id, bare, &replacement)?,
        Target::Field(layout, unit) => check_field_collision(unit, layout, bare)?,
    }

    let mut edits: BTreeMap<FileId, Vec<Span>> = BTreeMap::new();
    for span in spans {
        edits.entry(span.file).or_default().push(span);
    }
    for spans in edits.values_mut() {
        spans.sort();
        spans.dedup();
    }
    Ok(Rename { edits, replacement })
}

/// The text of the name under the cursor, as written.
fn current_text(context: &Context<'_>) -> String {
    if let Some(id) = context.symbol() {
        return context.unit.model.symbols.name(id).to_owned();
    }
    match context.field {
        Some((layout, index)) => context
            .unit
            .model
            .symbols
            .layout(layout)
            .fields
            .get(index)
            .map(|field| format!(".{}", field.name))
            .unwrap_or_default(),
        None => String::new(),
    }
}

/// Refuses a spelling the compiler would refuse at a declaration.
///
/// The words are `compat::reserved_name`'s, not a second set here: a rename
/// an editor accepts and the next build rejects is worse than one the editor
/// declined, and the two agreeing has to be a fact rather than a habit.
///
/// `alias` is the compiler's one exception, and agreeing means keeping it: an
/// `inline` parameter is bound at the use site rather than declared into a
/// namespace, so no spelling is taken.
fn check_spelling(bare: &str, replacement: &str, alias: bool) -> Result<(), Refusal> {
    if !alias && let Some(refusal) = crate::compat::reserved_name_refusal(bare) {
        return Err(refusal);
    }
    let mut characters = replacement.trim_start_matches('.').chars();
    let starts_well = characters
        .next()
        .is_some_and(|first| first.is_ascii_alphabetic() || first == '_');
    let rest_is_well = characters.all(|c| c.is_ascii_alphanumeric() || c == '_');
    if !starts_well || !rest_is_well {
        return Err(format!(
            "`{replacement}` is not an identifier: a name starts with a letter or `_` and \
             continues with letters, digits or `_`"
        ));
    }
    Ok(())
}

/// Refuses a field name the layout already declares.
///
/// A field's scope is its own layout — `semantic.rs` reports a second entry
/// under one name as `K816-E0158`'s sibling, "declared twice in this field
/// list" — and the rename has to make the same refusal in the same words. It
/// was the one target that reached the edits without a collision check, so a
/// rename of `.state` to an existing `.other` was offered and then refused by
/// the next build.
fn check_field_collision(unit: &AnalyzedUnit, layout: LayoutId, bare: &str) -> Result<(), Refusal> {
    let taken = unit
        .model
        .symbols
        .layout(layout)
        .fields
        .iter()
        .any(|field| field.name == bare);
    if taken {
        return Err(format!(
            "`.{bare}` is declared twice in this field list — a field name selects one \
             offset, so a second entry under the same name could never be reached"
        ));
    }
    Ok(())
}

/// Refuses a new name already taken in a scope the rename would reach.
///
/// Two scopes, because the language has two: a unit-level name collides
/// across every unit the symbol reaches, and a local label collides only
/// inside the function that owns it.
fn check_collision(
    analysis: &Analysis,
    unit: &AnalyzedUnit,
    id: SymbolId,
    bare: &str,
    replacement: &str,
) -> Result<(), Refusal> {
    let symbol = unit.model.symbols.symbol(id);
    if let Some(owner) = symbol.scope {
        // A local label: only its own function's scope can collide.
        if unit
            .model
            .symbols
            .lookup_local(owner, replacement)
            .is_some()
        {
            return Err(format!(
                "{} — `{}` already has a local label of that name",
                crate::semantic::duplicate_symbol_title(replacement),
                unit.model.symbols.name(owner),
            ));
        }
        return Ok(());
    }
    for other in &analysis.units {
        if other.model.symbols.lookup(bare).is_some() {
            return Err(format!(
                "{} — a unit of this workspace already declares that name",
                crate::semantic::duplicate_symbol_title(bare),
            ));
        }
    }
    Ok(())
}
