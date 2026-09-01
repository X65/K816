/*****************************************************************************/
/*                                                                           */
/*                                 format.rs                                 */
/*                                                                           */
/*        The formatter: it changes whitespace and nothing else              */
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

//! The formatter. **It changes whitespace and nothing else.**
//!
//! That is the whole specification, and the rules below are what it means
//! (`plans/07-lsp-tooling.md` decision 4):
//!
//! - **Indentation** is four spaces per brace-delimited construct the line's
//!   first token is inside. A closing delimiter goes back out to the
//!   enclosing level. A label sits where the corpus puts it — see
//!   [`label_column`].
//! - **A line's leading whitespace is replaced** by that indent.
//! - **Trailing whitespace goes**, and the file ends with exactly one
//!   newline.
//! - **Whitespace *inside* a line is left exactly as written.** Decision 4
//!   said a run between two tokens collapses to one space; the corpus says
//!   otherwise and the corpus is what the same-bytes gate is against.
//!   Fourteen goldens align `=` and `:word` in columns, and so do the
//!   imported fixtures — that is a convention, not fourteen mistakes
//!   (`postmortem.md` §H.10.1).
//!
//!   It also makes the risk note moot rather than merely handled: a
//!   formatter that never touches the text between two tokens *cannot* split
//!   `c-?{`, `>={`, `x++` or `a:`, whatever the grammar does with them.
//!
//! - **The line ending is the one the file uses.** A `\r\n` file stays
//!   `\r\n`; converting line endings is a whitespace change nobody asked
//!   for, and one file in the corpus is CRLF.
//!
//! **Never moved**: a line break, a comment, a token, or anything inside a
//! string or a multi-line literal — a token is emitted verbatim, interior and
//! all, so a formatter cannot reach inside one.
//!
//! **Never touched**: a file whose parse holds an `ERROR` or a `MISSING`
//! node. Formatting a program the grammar could not read is guessing, and the
//! diagnostics already say why.

use crate::source::SourceFile;

use super::cst::SyntaxTree;

/// Spaces per level.
///
/// Four, which is what the 39 goldens, the 21 legacy fixtures and
/// `examples/fullstack` already use. The K65 fixtures and the atari-xl
/// examples indent by two — the original compiler's style — and the
/// formatter re-indents them rather than preserving a second convention.
pub const INDENT: usize = 4;

/// The CST nodes whose contents are indented one level.
///
/// The same list `tree-sitter-k65/queries/indents.scm` carries, and
/// `tests/queries.rs` holds the two to each other line by line: the file an
/// editor reads and the formatter `k816 fmt` runs cannot drift apart.
pub const INDENTING: &[&str] = &[
    "block",
    "data_body",
    "code_subblock",
    "field_list",
    "eval_bracket",
    "eval_statement_block",
    "nocross_data_group",
];

/// Formats `source`, or `None` when its parse could not be read.
///
/// `tree` must be the parse of `source`.
pub fn format(source: &SourceFile, tree: &SyntaxTree) -> Option<String> {
    if tree.has_error() {
        return None;
    }
    let text = source.text();
    let tokens = leaves(tree);
    if tokens.is_empty() {
        // A file with nothing in it to place. Trailing whitespace still goes,
        // and what is left still ends with one newline — but an *empty* file
        // stays empty rather than growing a byte it never had. "The file ends
        // with exactly one newline" is a rule about a file with a line in it;
        // a formatter that wrote one into an empty file would be adding
        // content, which is the one thing this one does not do
        // (`plans/07-lsp-tooling.md` decision 4, corrected).
        let trimmed = text.trim_end();
        return Some(if trimmed.is_empty() {
            String::new()
        } else {
            format!("{trimmed}\n")
        });
    }

    let mut out = String::with_capacity(text.len());
    let mut previous_end = 0usize;
    for (index, token) in tokens.iter().enumerate() {
        let gap = &text[previous_end..token.start];
        let breaks = line_breaks(gap);

        if index == 0 || !breaks.is_empty() {
            out.push_str(&breaks);
            for _ in 0..indent_of(token) {
                out.push(' ');
            }
        } else {
            // A gap the source wrote inside a line, kept exactly: the corpus
            // aligns `=` and `:word` in columns, and a token pair the source
            // wrote adjacent stays adjacent.
            out.push_str(gap);
        }
        out.push_str(&text[token.start..token.end]);
        previous_end = token.end;
    }

    // Whatever followed the last token is trailing whitespace, and the file
    // ends with exactly one newline — the file's own kind of newline.
    let ending = if text.contains("\r\n") { "\r\n" } else { "\n" };
    while out.ends_with(char::is_whitespace) {
        out.pop();
    }
    out.push_str(ending);
    Some(out)
}

/// The line breaks in `gap`, as written.
///
/// A `\r\n` file stays `\r\n`. Everything else in the gap — the indentation
/// of the line the gap ends on — is dropped, because the indent replaces it.
fn line_breaks(gap: &str) -> String {
    let mut breaks = String::new();
    let bytes = gap.as_bytes();
    for (index, byte) in bytes.iter().enumerate() {
        if *byte == b'\n' {
            if index > 0 && bytes[index - 1] == b'\r' {
                breaks.push('\r');
            }
            breaks.push('\n');
        }
    }
    breaks
}

/// One token of the CST, with what the formatter needs to place it.
#[derive(Clone, Debug)]
struct Leaf {
    start: usize,
    end: usize,
    /// Brace-delimited constructs strictly enclosing it.
    depth: usize,
    /// True for the delimiter that opens or closes the construct it belongs
    /// to. Both sit at the *enclosing* level: `{` opens the indent and `}`
    /// ends it, so neither is inside it.
    closes: bool,
    /// `Some(unit_level)` for the name of a label statement.
    label: Option<bool>,
}

/// Every token of the tree, in source order.
fn leaves(tree: &SyntaxTree) -> Vec<Leaf> {
    let mut leaves = Vec::new();
    walk(tree.root(), 0, &mut leaves);
    leaves
}

fn walk(node: tree_sitter::Node<'_>, depth: usize, leaves: &mut Vec<Leaf>) {
    if node.child_count() == 0 {
        // Zero-width tokens contribute no text and would produce a line of
        // their own; a well-formed tree has none, but recovery can.
        if node.start_byte() == node.end_byte() {
            return;
        }
        leaves.push(Leaf {
            start: node.start_byte(),
            end: node.end_byte(),
            depth,
            closes: closes_its_parent(node),
            label: label_kind(node),
        });
        return;
    }
    let inner = if INDENTING.contains(&node.kind()) {
        depth + 1
    } else {
        depth
    };
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        walk(child, inner, leaves);
    }
}

/// Whether `node` is the delimiter that opens or closes the construct it
/// belongs to.
fn closes_its_parent(node: tree_sitter::Node<'_>) -> bool {
    if !matches!(node.kind(), "{" | "}" | "[" | "]" | "(" | ")") {
        return false;
    }
    node.parent()
        .is_some_and(|parent| INDENTING.contains(&parent.kind()))
}

/// `Some(true)` for a unit-level label's name, `Some(false)` for a local
/// one's, `None` for anything else.
fn label_kind(node: tree_sitter::Node<'_>) -> Option<bool> {
    let parent = node.parent()?;
    if parent.kind() != "label" {
        return None;
    }
    match node.kind() {
        "identifier" | "register" => Some(true),
        "local_identifier" => Some(false),
        _ => None,
    }
}

/// The column a line's first token starts at.
fn indent_of(token: &Leaf) -> usize {
    // A label sits at column 0, both kinds. Decision 4 said a unit-level one
    // at 0 and a local one at its block's depth; the corpus puts both at 0 —
    // `.spin:` in `golden/inline-expansion.k816`, in `examples/hello_uart.k65`
    // and in the imported fixtures — and a label left of the code it labels
    // is how assembly has been written since before either compiler
    // (`postmortem.md` §H.10.2).
    if token.label.is_some() {
        return 0;
    }
    let depth = if token.closes {
        token.depth.saturating_sub(1)
    } else {
        token.depth
    };
    depth * INDENT
}
