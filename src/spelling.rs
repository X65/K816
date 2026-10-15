/*****************************************************************************/
/*                                                                           */
/*                                spelling.rs                                */
/*                                                                           */
/*      How a statement is written: the rules that turn on its spelling      */
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

//! Rules about how a statement is *written*, as opposed to what it means.
//!
//! Every rule here reads the source text or a spelling the AST keeps, and
//! none changes a byte: a statement this module accepts compiles exactly as
//! it would if the module did not exist (`plans/14-readable-hla.md`). Each
//! looks only at statements the unit's own file wrote — an `inline` body
//! borrowed from another unit is that unit's to answer for.
//!
//! - A pushed constant's written width (decision 9): `PEA` pushes two bytes
//!   whatever `M` says, so a constant written as one byte is warned about,
//!   and one that *is* one byte by construction is refused.

use crate::ast::{
    AstFile, Block, DataBlock, DataEntryKind, Expr, ExprKind, HlaStatement, ItemKind, Statement,
    StatementKind, TypeWidthKind, UnaryOp,
};
use crate::diagnostic::{Applicability, Diagnostic, DiagnosticCode, Fix, Label, TextEdit};
use crate::source::SourceFile;

/// Every spelling rule over one unit.
pub fn check(program: &AstFile, source: &SourceFile) -> Vec<Diagnostic> {
    let mut checker = Checker {
        source,
        diagnostics: Vec::new(),
    };
    for item in &program.items {
        match &item.kind {
            ItemKind::Func(function) => checker.block(&function.body),
            ItemKind::Data(data) => checker.data(data),
            _ => {}
        }
    }
    checker.diagnostics
}

struct Checker<'a> {
    source: &'a SourceFile,
    diagnostics: Vec<Diagnostic>,
}

impl Checker<'_> {
    fn block(&mut self, block: &Block) {
        for statement in &block.statements {
            self.statement(statement);
        }
    }

    fn data(&mut self, data: &DataBlock) {
        for entry in &data.entries {
            match &entry.kind {
                DataEntryKind::Code { body, .. } => self.block(body),
                DataEntryKind::Repeat { body, .. } => self.data(body),
                DataEntryKind::NocrossGroup(entries) => self.data(&DataBlock {
                    entries: entries.clone(),
                    ..data.clone()
                }),
                _ => {}
            }
        }
    }

    fn statement(&mut self, statement: &Statement) {
        if statement.span.file != self.source.id() {
            return;
        }
        match &statement.kind {
            StatementKind::Hla(HlaStatement::PushConstant(value)) => self.pushed_constant(value),
            StatementKind::Conditional(conditional) => {
                self.block(&conditional.body);
                if let Some(otherwise) = &conditional.otherwise {
                    self.block(otherwise);
                }
            }
            StatementKind::ConditionalJump(jump) => self.statement(&jump.jump),
            StatementKind::Loop(loop_statement) => self.block(&loop_statement.body),
            StatementKind::WidthBlock(width) => self.block(&width.body),
            StatementKind::Nocross(block) => self.block(block),
            StatementKind::Data(data) => self.data(data),
            _ => {}
        }
    }

    /// One constant of a `push` list. `PEA` pushes two bytes, so the
    /// constant has to be written as two: a literal written narrower is a
    /// warning whose fix writes four hex digits, and a constant that is one
    /// byte by construction — an address byte, a character, a `:byte` view —
    /// is an error whose fix says the high byte is zero with `:word`. A
    /// sizeless constant, a `const` or an evaluator name, says nothing about
    /// its width and is taken at its word.
    fn pushed_constant(&mut self, value: &Expr) {
        let text = self.source.span_text(value.span);
        if let Some(what) = one_byte(value, text) {
            let expanded = match &value.kind {
                // A `:byte` view already says how wide it is; the word is
                // the other view of the same expression.
                ExprKind::TypedView(view) => {
                    format!("{}:word", self.source.span_text(view.base.span))
                }
                _ if is_parenthesized(text) => format!("{text}:word"),
                _ => format!("({text}):word"),
            };
            self.diagnostics.push(
                Diagnostic::error(
                    DiagnosticCode::PUSH_BYTE_CONSTANT,
                    "a one-byte constant cannot be pushed",
                    Label::new(
                        value.span,
                        format!("{what} is one byte, and `push` pushes two"),
                    ),
                )
                .with_note(
                    "the 65816 has no instruction that pushes a constant byte; `push` writes \
                     `PEA`, which pushes a word whatever `M` says",
                )
                .with_help(format!(
                    "write `{expanded}` to push it with a zero high byte, or load it and \
                     `push a` with an 8-bit accumulator"
                ))
                .with_fix(Fix {
                    title: format!("push it as the word `{expanded}`"),
                    applicability: Applicability::MaybeIncorrect,
                    edits: vec![TextEdit::replace(value.span, expanded.clone())],
                })
                .with_docs("doc/syntax-reference.md#push-and-pull"),
            );
            return;
        }
        let ExprKind::Number(number) = value.kind else {
            return;
        };
        if number > 0xFFFF || !written_as_one_byte(literal(text), number) {
            return;
        }
        let word = format!("${number:04X}");
        self.diagnostics.push(
            Diagnostic::warning(
                DiagnosticCode::PUSH_NARROW_CONSTANT,
                "this constant is written as one byte, and `push` pushes two",
                Label::new(value.span, format!("`PEA` pushes the word `{word}`")),
            )
            .with_help(format!("write it as the word it pushes: `{word}`"))
            .with_fix(Fix {
                title: format!("write `{word}`"),
                applicability: Applicability::MachineApplicable,
                edits: vec![TextEdit::replace(value.span, word.clone())],
            })
            .with_docs("doc/syntax-reference.md#push-and-pull"),
        );
    }
}

/// What makes `value` one byte by construction, if anything does.
fn one_byte(value: &Expr, text: &str) -> Option<&'static str> {
    match &value.kind {
        ExprKind::Unary(unary) if unary.op == UnaryOp::LowByte => {
            Some("`&<` makes a low byte, which")
        }
        ExprKind::Unary(unary) if unary.op == UnaryOp::HighByte => {
            Some("`&>` makes a high byte, which")
        }
        ExprKind::TypedView(view) if view.width.kind == TypeWidthKind::Byte => {
            Some("a `:byte` view")
        }
        ExprKind::Number(_) if literal(text).starts_with('\'') => Some("a character"),
        _ => None,
    }
}

/// A literal's text without the parentheses around it.
fn literal(text: &str) -> &str {
    let mut text = text.trim();
    while let Some(inner) = text
        .strip_prefix('(')
        .and_then(|rest| rest.strip_suffix(')'))
    {
        text = inner.trim();
    }
    text
}

fn is_parenthesized(text: &str) -> bool {
    let text = text.trim();
    text.starts_with('(') && text.ends_with(')')
}

/// Whether a numeric literal is written at one byte's width: fewer than four
/// hex digits, eight binary digits or fewer, or a decimal below 256.
fn written_as_one_byte(literal: &str, value: u32) -> bool {
    let digits = |text: &str| text.chars().filter(char::is_ascii_alphanumeric).count();
    if let Some(hex) = literal
        .strip_prefix('$')
        .or_else(|| literal.strip_prefix("0x"))
        .or_else(|| literal.strip_prefix("0X"))
    {
        digits(hex) < 4
    } else if let Some(binary) = literal
        .strip_prefix('%')
        .or_else(|| literal.strip_prefix("0b"))
        .or_else(|| literal.strip_prefix("0B"))
    {
        digits(binary) <= 8
    } else {
        literal.chars().all(|c| c.is_ascii_digit()) && value < 256
    }
}
