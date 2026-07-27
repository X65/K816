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

use crate::source::{SourceFile, Span};

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
