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
    // And the one ca65 idiom that parses: a `;` comment whose words are
    // calls. Reported here, where the tree is in hand, rather than at the
    // undeclared names it would otherwise produce one by one.
    diagnostics.extend(super::ca65::comments_read_as_calls(root, source));
    diagnostics
}

fn walk(node: tree_sitter::Node<'_>, source: &SourceFile, diagnostics: &mut Vec<Diagnostic>) {
    if node.is_error() {
        diagnostics.push(
            recognized(node, false, source).unwrap_or_else(|| error_diagnostic(node, source)),
        );
        // Do not descend: one diagnostic per malformed region keeps the
        // output actionable; the AST still carries an `Error` node for it.
        return;
    }
    if node.is_missing() {
        diagnostics.push(
            recognized(node, true, source).unwrap_or_else(|| missing_diagnostic(node, source)),
        );
        return;
    }
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if child.has_error() || child.is_missing() {
            walk(child, source, diagnostics);
        }
    }
}

/// The ca65 idiom at a recovery point, when the text holds one.
///
/// Asked before the generic diagnostics because its answer is the same
/// region explained better: "`;` begins a comment in ca65" where the grammar
/// could only say "missing `:`". A line that parses is never reached, so a
/// `;` used as K816's statement separator is not reported.
fn recognized(
    node: tree_sitter::Node<'_>,
    missing: bool,
    source: &SourceFile,
) -> Option<Diagnostic> {
    super::ca65::recognize(
        super::ca65::Recovery {
            span: node_span(node, source),
            missing,
            parent: node.parent().map(|parent| parent.kind()),
        },
        source,
    )
}

/// A Tree-sitter `ERROR` node: genuinely malformed source.
fn error_diagnostic(node: tree_sitter::Node<'_>, source: &SourceFile) -> Diagnostic {
    let span = node_span(node, source);
    let expected = expected_in(node.parent().map(|parent| parent.kind()));
    Diagnostic::error(
        DiagnosticCode::SYNTAX_ERROR,
        format!("expected {expected}"),
        Label::new(span, format!("cannot be parsed as {expected}")),
    )
    .with_docs("doc/syntax-reference.md")
}

/// A `MISSING` jump keyword that is the whole of an `if`'s jump: the parser's
/// guess at a statement the source does not have.
pub(super) fn invented_jump(node: tree_sitter::Node<'_>) -> bool {
    node.is_missing()
        && matches!(node.kind(), "repeat" | "break")
        && node
            .parent()
            .filter(|jump| matches!(jump.kind(), "repeat_statement" | "break_statement"))
            .and_then(|jump| jump.parent())
            .is_some_and(|parent| parent.kind() == "if_statement")
}

/// What the grammar was looking for where an `ERROR` node appeared, phrased
/// for the message "expected …".
///
/// Keyed by the *parent* node kind, which is what Tree-sitter reliably knows
/// at a recovery point. Anything not listed falls back to a phrasing that is
/// vague but never wrong.
fn expected_in(parent: Option<&str>) -> &'static str {
    match parent {
        Some("source_file") | None => "a declaration or a code section",
        Some("block") => "a statement",
        Some("data_body") => "a data entry",
        Some("data_block") | Some("data_repeat") => "a `{` opening the data block",
        Some("func_declaration") => "a function header or its body",
        Some("var_declaration") => "a variable name, type, or initializer",
        Some("const_declaration") | Some("const_entry") => "a constant name or its value",
        Some("segment_declaration") => "a segment name",
        // Three of sub-plan K's six arms are missing on purpose. Tree-sitter
        // closes `import_declaration`, `extern_const` and `extern_exit`
        // cleanly whatever follows them, so the ERROR node is parented to
        // `source_file` and the generic phrasing is what a reader gets — as
        // it is for every other item whose shape ends in a leaf. An arm that
        // cannot fire is a claim nothing checks.
        Some("extern_declaration") => "`func`, `var`, or `const` after `extern`",
        Some("extern_func") => "an extern function name, its widths, or `-> @a8`",
        Some("extern_var") => "an extern variable name or its type",
        Some("field_list") | Some("field_declaration") => "a `.field` entry",
        Some("call_contract") | Some("contract_inputs") | Some("contract_outputs") => {
            "a call contract parameter"
        }
        Some("native_instruction") | Some("block_move") => "an instruction operand",
        Some("immediate") => "an expression after `#`",
        Some("address") | Some("indirect") | Some("long_indirect") => "an address expression",
        Some("hla_assignment") => "an assignment operand",
        Some("hla_arithmetic") => "an operand for the arithmetic shorthand",
        Some("hla_shift") | Some("hla_step") => "a shift or step target",
        Some("conditional_block") | Some("postfix_loop") | Some("else_clause") => {
            "a `{ ... }` block"
        }
        Some("if_statement") => "a condition, then a `{ ... }` block or a jump",
        Some("conditional_jump") => "`goto`, `break`, or `repeat`",
        Some("goto_statement") => "a jump target",
        Some("width_block") | Some("nocross_block") | Some("code_subblock") => "a `{ ... }` block",
        Some("eval_bracket") | Some("eval_call") | Some("eval_parenthesized") => {
            "an evaluator expression"
        }
        Some("for_eval") => "a `for x=A..B eval [ ... ]` range",
        Some("preprocessor_directive") => "a condition or message",
        Some("binary_declaration") | Some("binary_entry") => "a quoted file name",
        Some("reserved_image_block") | Some("reserved_image_toplevel") => "an `image` argument",
        Some("parenthesized_expression") => "an expression inside the parentheses",
        Some("binary_expression") | Some("address_and_expression") => {
            "an expression after the operator"
        }
        Some("unary_expression") | Some("address_of_expression") => {
            "an expression after the operator"
        }
        _ => "valid K816 syntax",
    }
}

/// A Tree-sitter `MISSING` node: incomplete source with an inferable token.
fn missing_diagnostic(node: tree_sitter::Node<'_>, source: &SourceFile) -> Diagnostic {
    let span = node_span(node, source);
    let token = node.kind();
    // `if <` with nothing after it is a valid prefix of `if < repeat`, the
    // shortest jump, so that is the token Tree-sitter invents. Neither it nor
    // a fix inserting it is what the author meant: what is missing is the
    // whole of what the condition guards.
    if invented_jump(node) {
        return Diagnostic::error(
            DiagnosticCode::SYNTAX_MISSING,
            "missing a `{ ... }` block or a jump after the condition",
            Label::new(span, "a block, `goto`, `break` or `repeat` expected here"),
        )
        .with_docs("doc/syntax-reference.md");
    }
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
