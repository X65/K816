/*****************************************************************************/
/*                                                                           */
/*                                 tokens.rs                                 */
/*                                                                           */
/*        Semantic tokens, derived from the highlighting query itself        */
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

//! Semantic tokens, produced by running `highlights.scm` and refining what it
//! could not decide.
//!
//! **This is the second of two highlighting formats, and it is derived from
//! the first.** Editors that read Tree-sitter queries use the file; editors
//! that do not get these tokens, which come from running that same file over
//! the document's tree. Describing the highlighting twice — a TextMate
//! grammar beside a query — is what the amended `implementation.md` §3
//! decision 12 refuses, and "derived from" is what makes these tokens not a
//! second description (`plans/07-lsp-tooling.md` decisions 6 and 14).
//!
//! What the query cannot decide is what an *identifier* is. The grammar knows
//! `(const_entry name: (identifier))` is a constant, and it does not know what
//! a bare `SCREEN` in an operand is — only the symbol table does. So a
//! capture the query left as `@variable` is refined through the reference
//! index, and every refinement is a fact the compiler resolved rather than a
//! guess about the text.
//!
//! The modifiers are two and no more: `declaration` at a declaring name, and
//! `readonly` on a `const`. A modifier the model cannot fill honestly is one
//! an editor would render as a lie.

use lsp_types::{SemanticToken, SemanticTokenModifier, SemanticTokenType};

use crate::parser::{Capture, Query};
use crate::source::{FileId, SourceFile, Span};
use crate::symbol::SymbolKind;
use crate::{Analysis, AnalyzedUnit};

/// The highlighting query, compiled into the binary.
///
/// The server has to be able to run it wherever it is installed, and the file
/// lives in the grammar directory because that is where an editor reading it
/// directly looks. One source, two readers.
const HIGHLIGHTS: &str = include_str!("../../tree-sitter-k65/queries/highlights.scm");

/// Every capture name `highlights.scm` uses, and the token type it becomes.
///
/// A closed set in both directions: `tests/queries.rs` fails if the query
/// captures a name this table does not map, *and* if this table maps a name
/// the query never produces. That is what keeps the two formats one
/// description instead of two that agree today.
pub const LEGEND: &[(&str, SemanticTokenType)] = &[
    ("comment", SemanticTokenType::COMMENT),
    ("string", SemanticTokenType::STRING),
    ("number", SemanticTokenType::NUMBER),
    ("punctuation", SemanticTokenType::OPERATOR),
    ("punctuation.special", SemanticTokenType::OPERATOR),
    ("operator", SemanticTokenType::OPERATOR),
    ("keyword", SemanticTokenType::KEYWORD),
    ("keyword.function", SemanticTokenType::KEYWORD),
    ("keyword.return", SemanticTokenType::KEYWORD),
    ("keyword.control", SemanticTokenType::KEYWORD),
    ("keyword.directive", SemanticTokenType::MACRO),
    ("type", SemanticTokenType::TYPE),
    ("function", SemanticTokenType::FUNCTION),
    ("function.builtin", SemanticTokenType::FUNCTION),
    ("variable", SemanticTokenType::VARIABLE),
    ("variable.builtin", SemanticTokenType::VARIABLE),
    ("constant", SemanticTokenType::VARIABLE),
    ("constant.builtin", SemanticTokenType::VARIABLE),
    ("label", SemanticTokenType::NAMESPACE),
    ("namespace", SemanticTokenType::NAMESPACE),
    ("property", SemanticTokenType::PROPERTY),
];

/// The token types the server advertises, in legend order.
///
/// De-duplicated: several capture names map onto one protocol type — the
/// protocol's list is shorter than a highlighter's vocabulary — and the
/// client indexes into *this* list.
pub fn token_types() -> Vec<SemanticTokenType> {
    let mut types: Vec<SemanticTokenType> = Vec::new();
    for (_, token) in LEGEND {
        if !types.contains(token) {
            types.push(token.clone());
        }
    }
    types
}

/// The two modifiers, in the order their bits are numbered.
pub fn token_modifiers() -> Vec<SemanticTokenModifier> {
    vec![
        SemanticTokenModifier::DECLARATION,
        SemanticTokenModifier::READONLY,
    ]
}

const MODIFIER_DECLARATION: u32 = 1;
const MODIFIER_READONLY: u32 = 1 << 1;

/// One token, before it is encoded into the protocol's relative form.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Token {
    pub span: Span,
    /// Index into [`token_types`].
    pub token_type: u32,
    /// Bit set over [`token_modifiers`].
    pub modifiers: u32,
}

/// The tokens for one document, in source order and never overlapping.
pub fn tokens(analysis: &Analysis, file: FileId, source: &SourceFile) -> Vec<Token> {
    let Some(unit) = analysis.unit(file) else {
        return Vec::new();
    };
    let Ok(query) = Query::compile(HIGHLIGHTS) else {
        // `tests/queries.rs` compiles it under `cargo test`, so this is a
        // build that shipped a query the grammar rejects. Answering nothing
        // beats answering wrongly.
        return Vec::new();
    };

    // Later patterns win, which is what Tree-sitter's own highlighter does:
    // the general `(identifier) @variable` comes first and the specific
    // `(const_entry name: (identifier) @constant)` after it.
    let mut best: std::collections::BTreeMap<Span, Capture> = std::collections::BTreeMap::new();
    for capture in query.captures(&unit.parse.tree, file, source.text()) {
        match best.get(&capture.span) {
            Some(existing) if existing.pattern >= capture.pattern => {}
            _ => {
                best.insert(capture.span, capture);
            }
        }
    }

    let mut tokens: Vec<Token> = Vec::new();
    for capture in best.into_values() {
        let (token_type, modifiers) = classify(unit, &capture);
        let Some(token_type) = token_type else {
            continue;
        };
        // The protocol forbids overlapping tokens, and a query legitimately
        // captures a node and its child. The outer one is dropped, because
        // the inner one is the more specific statement about the same text.
        while tokens
            .last()
            .is_some_and(|last| last.span.end > capture.span.start)
        {
            tokens.pop();
        }
        for span in per_line(capture.span, source) {
            tokens.push(Token {
                span,
                token_type,
                modifiers,
            });
        }
    }
    tokens
}

/// The token type and modifiers of one capture.
///
/// Where the query said `@variable`, `@label`, `@function`, `@constant` or
/// `@property`, the symbol table gets the last word: it is what knows whether
/// a bare `SCREEN` is a variable or a section, and the grammar cannot.
fn classify(unit: &AnalyzedUnit, capture: &Capture) -> (Option<u32>, u32) {
    let refinable = matches!(
        capture.name.as_str(),
        "variable" | "label" | "function" | "constant" | "property"
    );
    let (name, modifiers) = if refinable {
        match resolved(unit, capture.span) {
            Some((kind, declaring)) => (
                match kind {
                    SymbolKind::Const { .. } => "constant",
                    SymbolKind::Function { .. } | SymbolKind::Inline => "function",
                    SymbolKind::Label { .. } | SymbolKind::DataLabel { .. } => "label",
                    SymbolKind::Var { .. } | SymbolKind::AbstractVar { .. } => "variable",
                    // An `inline` alias stands for whatever a use site
                    // substitutes; a variable is the nearest honest word.
                    SymbolKind::Alias { .. } => "variable",
                },
                if declaring { MODIFIER_DECLARATION } else { 0 }
                    | if matches!(kind, SymbolKind::Const { .. }) {
                        MODIFIER_READONLY
                    } else {
                        0
                    },
            ),
            // A field is not a symbol — it has no `SymbolId` — so a field's
            // *declaring* `.name` is neither a symbol declaration nor a
            // reference, and it was the one declaring occurrence in the
            // language that carried no `declaration` modifier.
            None if declares_field(unit, capture.span) => ("property", MODIFIER_DECLARATION),
            // Not a name the compiler resolved — an undeclared symbol, or a
            // capture on something that is not an identifier at all. The
            // query's own answer stands.
            None => (capture.name.as_str(), 0),
        }
    } else {
        (capture.name.as_str(), 0)
    };
    (index_of(name), modifiers)
}

/// What the model says the name at `span` is, and whether that occurrence
/// declares it.
fn resolved(unit: &AnalyzedUnit, span: Span) -> Option<(SymbolKind, bool)> {
    if let Some((_, symbol)) = unit
        .model
        .symbols
        .iter()
        .find(|(_, symbol)| symbol.declaration == span)
    {
        return Some((symbol.kind, true));
    }
    let id = unit.model.references.get(&span)?;
    Some((unit.model.symbols.kind(*id), false))
}

/// Whether `span` is the declaring `.name` of a layout field.
///
/// The layouts are walked rather than the AST for the same reason
/// `lsp::at` walks them: a layout is what a field reference names, and the
/// AST's field list would have to be matched back to one anyway.
fn declares_field(unit: &AnalyzedUnit, span: Span) -> bool {
    unit.model.symbols.iter().any(|(id, _)| {
        let layout = match unit.model.symbols.kind(id) {
            SymbolKind::Var {
                layout: Some(layout),
                ..
            }
            | SymbolKind::AbstractVar { layout } => layout,
            _ => return false,
        };
        unit.model
            .symbols
            .layout(layout)
            .fields
            .iter()
            .any(|field| field.name_span == span)
    })
}

/// The index a capture name has in [`token_types`].
fn index_of(name: &str) -> Option<u32> {
    let token = LEGEND
        .iter()
        .find(|(capture, _)| *capture == name)
        .map(|(_, token)| token)?;
    token_types()
        .iter()
        .position(|candidate| candidate == token)
        .map(|index| index as u32)
}

/// A span split at every newline it crosses.
///
/// The protocol's encoding is per line, and a block comment is one node
/// spanning several. Splitting keeps the comment highlighted instead of
/// dropping it.
///
/// Asked of [`SourceFile`]'s line index rather than of the text: `src/lsp/`
/// does not read source text for meaning, and "which line is this byte on"
/// is a question `line_col` already answers (H10's review item).
fn per_line(span: Span, source: &SourceFile) -> Vec<Span> {
    let first = source.line_col(span.start).line;
    let last = source.line_col(span.end).line;
    if first == last {
        return vec![span];
    }
    let mut spans = Vec::new();
    for line in first..=last {
        let start = source
            .offset(crate::source::LineCol { line, column: 0 })
            .max(span.start);
        let end = source
            .offset(crate::source::LineCol {
                line,
                column: source.line_text(line).len() as u32,
            })
            .min(span.end);
        if start < end {
            spans.push(Span::new(span.file, start, end));
        }
    }
    spans
}

/// The protocol's relative encoding: five integers per token, lengths and
/// columns in UTF-16 units.
pub fn encode(tokens: &[Token], source: &crate::source::SourceFile) -> Vec<SemanticToken> {
    let mut encoded = Vec::with_capacity(tokens.len());
    let mut previous_line = 0u32;
    let mut previous_start = 0u32;
    for token in tokens {
        let start = super::convert::position_in(source, token.span.start);
        let end = super::convert::position_in(source, token.span.end);
        let delta_line = start.line - previous_line;
        let delta_start = if delta_line == 0 {
            start.character - previous_start
        } else {
            start.character
        };
        encoded.push(SemanticToken {
            delta_line,
            delta_start,
            length: end.character.saturating_sub(start.character),
            token_type: token.token_type,
            token_modifiers_bitset: token.modifiers,
        });
        previous_line = start.line;
        previous_start = start.character;
    }
    encoded
}
