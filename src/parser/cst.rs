/*****************************************************************************/
/*                                                                           */
/*                                   cst.rs                                  */
/*                                                                           */
/*               Syntax tree wrapper and CST inspection helpers              */
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

//! Thin wrapper around the Tree-sitter tree plus CST inspection helpers.

use crate::source::{LineCol, SourceFile, Span};

/// One text replacement inside a source file: `span` becomes `replacement`.
///
/// The k816-typed description of an edit. It is deliberately *not* a
/// `tree_sitter::InputEdit`: that type needs six coordinates (three byte
/// offsets and three row/column points), which a span plus its replacement
/// text cannot supply on its own — see [`SyntaxTree::edit`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SourceEdit {
    pub span: Span,
    pub replacement: String,
}

impl SourceEdit {
    pub fn new(span: Span, replacement: impl Into<String>) -> Self {
        Self {
            span,
            replacement: replacement.into(),
        }
    }

    /// The text `before` would have after this edit.
    pub fn apply(&self, before: &SourceFile) -> String {
        debug_assert_eq!(self.span.file, before.id());
        let text = before.text();
        let mut edited = String::with_capacity(text.len() + self.replacement.len());
        edited.push_str(&text[..self.span.start as usize]);
        edited.push_str(&self.replacement);
        edited.push_str(&text[self.span.end as usize..]);
        edited
    }
}

/// An immutable parse tree for one source file.
///
/// Wraps the Tree-sitter tree so `tree_sitter` types stay inside `parser::`.
pub struct SyntaxTree {
    pub(super) tree: tree_sitter::Tree,
}

impl SyntaxTree {
    pub(super) fn root(&self) -> tree_sitter::Node<'_> {
        self.tree.root_node()
    }

    /// Renders the named CST as an indented dump with byte ranges, for exact
    /// snapshot tests. `ERROR` and `MISSING` nodes are marked explicitly.
    pub fn dump(&self) -> String {
        let mut out = String::new();
        dump_node(&mut out, self.root(), None, 0);
        out
    }

    /// Renders the CST as an S-expression in the Tree-sitter corpus format,
    /// with `field:` prefixes and explicit `(ERROR)` / `(MISSING "…")` nodes.
    pub fn to_sexp(&self) -> String {
        self.root().to_sexp()
    }

    /// Records `edit` in this tree so the next parse can reuse it.
    ///
    /// `before` is the source file *as it was when this tree was parsed*: the
    /// Tree-sitter edit protocol wants row/column points for the start and old
    /// end of the replaced range, and a `SyntaxTree` keeps no source text of
    /// its own. The new-end point is derived by walking `replacement`.
    ///
    /// After this call the tree is stale — pass it to
    /// [`parse_source`](super::parse_source) with the edited text to obtain a
    /// tree that matches the new source.
    pub fn edit(&mut self, before: &SourceFile, edit: &SourceEdit) {
        debug_assert_eq!(edit.span.file, before.id());
        let start = before.line_col(edit.span.start);
        let old_end = before.line_col(edit.span.end);
        self.tree.edit(&tree_sitter::InputEdit {
            start_byte: edit.span.start as usize,
            old_end_byte: edit.span.end as usize,
            new_end_byte: edit.span.start as usize + edit.replacement.len(),
            start_position: point(start),
            old_end_position: point(old_end),
            new_end_position: point(advance(start, &edit.replacement)),
        });
    }
}

fn point(location: LineCol) -> tree_sitter::Point {
    tree_sitter::Point {
        row: location.line as usize,
        column: location.column as usize,
    }
}

/// The location reached by writing `text` starting at `from`. Columns count
/// bytes, matching [`SourceFile::line_col`].
fn advance(from: LineCol, text: &str) -> LineCol {
    let mut location = from;
    for byte in text.bytes() {
        if byte == b'\n' {
            location.line += 1;
            location.column = 0;
        } else {
            location.column += 1;
        }
    }
    location
}

/// The byte span of `node` within `source`.
pub(super) fn node_span(node: tree_sitter::Node<'_>, source: &SourceFile) -> Span {
    Span::new(
        source.id(),
        node.start_byte() as u32,
        node.end_byte() as u32,
    )
}

fn dump_node(out: &mut String, node: tree_sitter::Node<'_>, field: Option<&str>, depth: usize) {
    if !node.is_named() && !node.is_missing() {
        return;
    }
    for _ in 0..depth {
        out.push_str("  ");
    }
    if let Some(field) = field {
        out.push_str(field);
        out.push_str(": ");
    }
    if node.is_missing() {
        out.push_str("MISSING ");
        if !node.is_named() {
            // A missing anonymous token, e.g. a `}` inserted by recovery.
            out.push_str(&format!(
                "\"{}\" {}..{}\n",
                node.kind(),
                node.start_byte(),
                node.end_byte()
            ));
            return;
        }
    }
    out.push_str(&format!(
        "{} {}..{}\n",
        node.kind(),
        node.start_byte(),
        node.end_byte()
    ));
    let mut cursor = node.walk();
    for (index, child) in node.children(&mut cursor).enumerate() {
        let field = node.field_name_for_child(index as u32);
        dump_node(out, child, field, depth + 1);
    }
}
