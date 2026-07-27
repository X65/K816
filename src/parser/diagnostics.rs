/*****************************************************************************/
/*                                                                           */
/*                               diagnostics.rs                              */
/*                                                                           */
/*              Syntax diagnostics from ERROR and MISSING nodes              */
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

//! Syntax-diagnostic adapter: maps Tree-sitter `ERROR`/`MISSING` nodes to
//! stable, actionable diagnostics and fixes.
//!
//! Tree-sitter supplies reliable ranges; K816 supplies the explanation and
//! remediation. Unknown shapes receive a conservative diagnostic over the
//! smallest useful CST range.

use crate::diagnostic::{Applicability, Diagnostic, DiagnosticCode, Fix, Label, TextEdit};
use crate::source::SourceFile;

use super::cst::node_span;

pub(super) fn collect_syntax_diagnostics(
    root: tree_sitter::Node<'_>,
    source: &SourceFile,
) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();
    if root.has_error() {
        walk(root, source, &mut diagnostics);
    }
    diagnostics
}

fn walk(node: tree_sitter::Node<'_>, source: &SourceFile, diagnostics: &mut Vec<Diagnostic>) {
    if node.is_error() {
        diagnostics.push(error_diagnostic(node, source));
        // Do not descend: one diagnostic per malformed region keeps the
        // output actionable; the AST still carries an `Error` node for it.
        return;
    }
    if node.is_missing() {
        diagnostics.push(missing_diagnostic(node, source));
        return;
    }
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if child.has_error() || child.is_missing() {
            walk(child, source, diagnostics);
        }
    }
}

/// A Tree-sitter `ERROR` node: genuinely malformed source.
fn error_diagnostic(node: tree_sitter::Node<'_>, source: &SourceFile) -> Diagnostic {
    let span = node_span(node, source);
    let expected = match node.parent().map(|parent| parent.kind()) {
        Some("block") => "a statement",
        Some("source_file") | None => "a `var` or `func` declaration",
        _ => "valid K816 syntax",
    };
    Diagnostic::error(
        DiagnosticCode::SYNTAX_ERROR,
        format!("expected {expected}"),
        Label::new(span, format!("cannot be parsed as {expected}")),
    )
    .with_docs("doc/syntax-reference.md")
}

/// A Tree-sitter `MISSING` node: incomplete source with an inferable token.
fn missing_diagnostic(node: tree_sitter::Node<'_>, source: &SourceFile) -> Diagnostic {
    let span = node_span(node, source);
    let token = node.kind();
    let mut diagnostic = Diagnostic::error(
        DiagnosticCode::SYNTAX_MISSING,
        format!("missing `{token}`"),
        Label::new(span, format!("`{token}` expected here")),
    );
    if !node.is_named() {
        // For a concrete missing token the remediation is machine-applicable.
        diagnostic = diagnostic
            .with_help(format!("insert `{token}`"))
            .with_fix(Fix {
                title: format!("insert `{token}`"),
                applicability: Applicability::MachineApplicable,
                edits: vec![TextEdit::insert(span, token)],
            });
    }
    diagnostic
}
