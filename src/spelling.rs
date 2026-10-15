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
//! - K65's spellings in a K816 source (decision 11): the postfix loop, the
//!   signed flag test, the `<<=` / `>>=` relations and the `!!` / `??`
//!   stack are accepted in a `.k65` source and refused in a `.kk` or `.k816`
//!   one, each with a fix that writes the K816 form. The fix is the whole
//!   point: the two spellings compile to the same bytes, so applying it
//!   changes nothing but the spelling.
//! - Rule 3 (decision 10): in a K816 source's HLA, memory is reached only
//!   through a named variable, and a free-standing number is always a value
//!   — which `a = $10` already is, loading `#$10`. A number in any memory
//!   shape (`a = $10,x`, `$10 = a`, `$10++`, `a = ($10),y`) is refused, with
//!   the native mnemonic as the fix where the statement is one instruction:
//!   a mnemonic takes an address as written, and is the escape hatch. A
//!   number before `,s` is a displacement and no variable can name it, and
//!   one after `&` says it is an offset, so both stay.

use crate::ast::{
    ArithmeticOp, AstFile, Block, BranchCondition, BranchConditionKind, ComparisonKind, DataBlock,
    DataEntryKind, Expr, ExprKind, FlagTestSpelling, HlaOperand, HlaOperandKind, HlaStack,
    HlaStatement, IndexRegisterKind, ItemKind, LoopCondition, LoopSpelling, LoopStatement,
    RegisterKind, ShiftOp, StackOp, StackSpelling, Statement, StatementKind, StepOp, TypeWidthKind,
    UnaryOp,
};
use crate::diagnostic::{Applicability, Diagnostic, DiagnosticCode, Fix, Label, TextEdit};
use crate::source::{Dialect, SourceFile, Span};

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
        let k816 = self.source.dialect() == Dialect::K816;
        match &statement.kind {
            StatementKind::Hla(HlaStatement::PushConstant { value, .. }) => {
                self.pushed_constant(value)
            }
            StatementKind::Hla(HlaStatement::Stack(stack)) if k816 => {
                self.stack_shorthand(stack, statement.span)
            }
            StatementKind::Hla(hla) if k816 => self.numbered_places(hla, statement.span),
            StatementKind::Conditional(conditional) => {
                if k816 {
                    self.condition(&conditional.condition);
                }
                self.block(&conditional.body);
                if let Some(otherwise) = &conditional.otherwise {
                    self.block(otherwise);
                }
            }
            StatementKind::ConditionalJump(jump) => {
                if k816 {
                    self.condition(&jump.condition);
                }
                self.statement(&jump.jump);
            }
            StatementKind::Loop(loop_statement) => {
                if k816 {
                    self.postfix_loop(loop_statement);
                    if let LoopCondition::Branch(condition) = &loop_statement.condition {
                        self.condition(condition);
                    }
                }
                self.block(&loop_statement.body);
            }
            StatementKind::WidthBlock(width) => self.block(&width.body),
            StatementKind::Nocross(block) => self.block(block),
            StatementKind::Data(data) => self.data(data),
            _ => {}
        }
    }

    /// `{ x-- } !=`, `{ ... } always`, `{ ... } never`, with or without
    /// `nocross`: the loop K816 writes as `loop { x-- } while !=`. The fix
    /// inserts the keyword before the brace and `while` before a branch
    /// condition, which is the whole difference between the two.
    fn postfix_loop(&mut self, loop_statement: &LoopStatement) {
        if loop_statement.spelling != LoopSpelling::Postfix {
            return;
        }
        let brace = loop_statement.body.span;
        let brace = Span::new(brace.file, brace.start, brace.start + 1);
        let mut edits = vec![TextEdit::insert(brace, "loop ")];
        let after = match loop_statement.condition {
            LoopCondition::Branch(condition) => {
                edits.push(TextEdit::insert(condition.span, "while "));
                format!("while {}", self.source.span_text(condition.span))
            }
            LoopCondition::Always => "always".to_owned(),
            LoopCondition::Never => "never".to_owned(),
        };
        let written = format!("loop {{ ... }} {after}");
        self.diagnostics.push(
            Diagnostic::error(
                DiagnosticCode::K65_POSTFIX_LOOP,
                "a K816 source writes a loop with `loop`",
                Label::new(
                    brace,
                    "this block is a loop only because of what follows its `}`",
                ),
            )
            .with_note(
                "K65's loop is a block with its condition after it; K816's says where it \
                 begins, and both compile to the same bytes",
            )
            .with_help(format!(
                "write `{written}`, or name the file `.k65` to keep K65's spelling"
            ))
            .with_fix(Fix {
                title: format!("write `{written}`"),
                applicability: Applicability::MachineApplicable,
                edits,
            })
            .with_docs("doc/syntax-reference.md#loops"),
        );
    }

    /// `c+?` / `c-?` and the overflow relations `<<=` / `>>=`: the flag tests
    /// K816 writes `c?` / `!c?` and `v?` / `!v?`.
    fn condition(&mut self, condition: &BranchCondition) {
        let text = self.source.span_text(condition.span);
        let (code, title, written) = match condition.kind {
            BranchConditionKind::Flag {
                set,
                spelling: FlagTestSpelling::Signed,
                ..
            } => {
                let letter = text.get(..1).unwrap_or("c");
                (
                    DiagnosticCode::K65_SIGNED_FLAG_TEST,
                    "a K816 source writes a flag test without its sign",
                    if set {
                        format!("{letter}?")
                    } else {
                        format!("!{letter}?")
                    },
                )
            }
            BranchConditionKind::Comparison(ComparisonKind::OverflowSet) => (
                DiagnosticCode::K65_OVERFLOW_RELATION,
                "a K816 source tests the overflow flag by its name",
                "v?".to_owned(),
            ),
            BranchConditionKind::Comparison(ComparisonKind::OverflowClear) => (
                DiagnosticCode::K65_OVERFLOW_RELATION,
                "a K816 source tests the overflow flag by its name",
                "!v?".to_owned(),
            ),
            _ => return,
        };
        let note = if code == DiagnosticCode::K65_OVERFLOW_RELATION {
            "`<<=` and `>>=` read as relations and are the overflow flag, and `<<=` is one \
             character from the comparison `<=`"
        } else {
            "`c?` is `c+?` and `!c?` is `c-?`: the same branch"
        };
        self.diagnostics.push(
            Diagnostic::error(
                code,
                title,
                Label::new(condition.span, format!("K816 writes this test `{written}`")),
            )
            .with_note(note)
            .with_help(format!(
                "write `{written}`, or name the file `.k65` to keep K65's spelling"
            ))
            .with_fix(Fix {
                title: format!("write `{written}`"),
                applicability: Applicability::MachineApplicable,
                edits: vec![TextEdit::replace(condition.span, written.clone())],
            })
            .with_docs("doc/syntax-reference.md#branch-and-flow-control"),
        );
    }

    /// `a!!` / `a??`: the push and pull K816 writes `push a` / `pull a`.
    ///
    /// `b` is left to its own refusal, which names the register it meant.
    fn stack_shorthand(&mut self, stack: &HlaStack, span: Span) {
        if stack.spelling != StackSpelling::Shorthand || stack.register.kind == RegisterKind::B {
            return;
        }
        let register = self.source.span_text(stack.register.span);
        let written = match stack.op {
            StackOp::Push => format!("push {register}"),
            StackOp::Pull => format!("pull {register}"),
        };
        self.diagnostics.push(
            Diagnostic::error(
                DiagnosticCode::K65_STACK_SHORTHAND,
                "a K816 source pushes and pulls with `push` and `pull`",
                Label::new(stack.op_span, format!("K816 writes this `{written}`")),
            )
            .with_note(
                "`push` and `pull` take a list in the order written, and `push` takes \
                 constants too",
            )
            .with_help(format!(
                "write `{written}`, or name the file `.k65` to keep K65's spelling"
            ))
            .with_fix(Fix {
                title: format!("write `{written}`"),
                applicability: Applicability::MachineApplicable,
                edits: vec![TextEdit::replace(span, written.clone())],
            })
            .with_docs("doc/syntax-reference.md#push-and-pull"),
        );
    }

    /// Rule 3 over one HLA statement: every memory operand it has must name
    /// a place. A chain that names no register is the zero store's, which
    /// refuses a numeric target itself (`k816-zero-store-target`).
    ///
    /// A *source* with no index and no prefix is a value however it is
    /// written — `a = $10` loads `#$10`, which is the rule itself — so only a
    /// target, an indexed or prefixed source, and an indirect operand can be
    /// a number used as a place (`Resolver::hla_value` reads them the same
    /// way).
    fn numbered_places(&mut self, hla: &HlaStatement, span: Span) {
        // Each operand, and whether it is read as a source.
        let operands: Vec<(&HlaOperand, bool)> = match hla {
            HlaStatement::Assign(assign) => {
                let all: Vec<(&HlaOperand, bool)> = assign
                    .targets
                    .iter()
                    .map(|target| (target, false))
                    .chain(std::iter::once((&assign.source, true)))
                    .collect();
                if !all
                    .iter()
                    .any(|(operand, _)| matches!(operand.kind, HlaOperandKind::Register(_)))
                {
                    return;
                }
                all
            }
            HlaStatement::Arithmetic(arithmetic) => vec![(&arithmetic.operand, true)],
            HlaStatement::Shift(shift) => vec![(&shift.target, false)],
            HlaStatement::Step(step) => vec![(&step.target, false)],
            _ => return,
        };
        for (operand, source) in operands {
            let value = match &operand.kind {
                HlaOperandKind::Memory(address) => {
                    if address.offset_marker.is_some()
                        || address
                            .index
                            .is_some_and(|index| index.kind == IndexRegisterKind::Stack)
                        || (source && address.index.is_none() && address.encoding.is_none())
                    {
                        continue;
                    }
                    &address.value
                }
                HlaOperandKind::Indirect(indirect) => {
                    if indirect
                        .inner_index
                        .is_some_and(|index| index.kind == IndexRegisterKind::Stack)
                    {
                        continue;
                    }
                    &indirect.value
                }
                _ => continue,
            };
            if names_a_place(value) {
                continue;
            }
            let number = self.source.span_text(value.span);
            // A bracket in HLA is the evaluator's and in a mnemonic an
            // indirect-long operand, so the operand cannot be copied across.
            let native = (!holds_a_bracket(value))
                .then(|| native_instruction(hla, self.source))
                .flatten();
            let mut diagnostic = Diagnostic::error(
                DiagnosticCode::NUMBER_AS_PLACE,
                "a K816 source's HLA reaches memory through a named variable",
                Label::new(
                    value.span,
                    format!("`{number}` is a value here, not a place"),
                ),
            )
            .with_note(
                "in HLA a number is always a value — `a = $10` loads `#$10` — so a number \
                 cannot also be an address; a native mnemonic takes one as written",
            )
            .with_docs("doc/syntax-reference.md#a-number-is-a-value");
            diagnostic = match native {
                Some(native) => diagnostic
                    .with_help(format!(
                        "name the place — `var NAME = {number}` — and write `NAME`, or write \
                         the instruction: `{native}`"
                    ))
                    .with_fix(Fix {
                        title: format!("write `{native}`"),
                        applicability: Applicability::MachineApplicable,
                        edits: vec![TextEdit::replace(span, native.clone())],
                    }),
                None => diagnostic.with_help(format!(
                    "name the place — `var NAME = {number}` — and write `NAME`, or write the \
                     statement with native mnemonics"
                )),
            };
            self.diagnostics.push(diagnostic);
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

/// Whether `value` names a place: a variable, a label, a field or an element
/// of one. A `:sizeof` is a number however it is spelled, and a `const` is
/// the semantic layer's to refuse (`K816-E0110`), so either counts as a name
/// here only by being a name.
fn names_a_place(value: &Expr) -> bool {
    match &value.kind {
        ExprKind::Symbol(_)
        | ExprKind::LocalSymbol(_)
        | ExprKind::Field(_)
        | ExprKind::Subscript(_) => true,
        ExprKind::Unary(unary) => names_a_place(&unary.operand),
        ExprKind::Binary(binary) => names_a_place(&binary.left) || names_a_place(&binary.right),
        ExprKind::TypedView(view) => names_a_place(&view.base),
        ExprKind::Number(_) | ExprKind::Metadata(_) | ExprKind::Eval(_) | ExprKind::Error => false,
    }
}

fn holds_a_bracket(value: &Expr) -> bool {
    match &value.kind {
        ExprKind::Eval(_) => true,
        ExprKind::Unary(unary) => holds_a_bracket(&unary.operand),
        ExprKind::Binary(binary) => holds_a_bracket(&binary.left) || holds_a_bracket(&binary.right),
        ExprKind::TypedView(view) => holds_a_bracket(&view.base),
        _ => false,
    }
}

/// The one native instruction an HLA statement is, when it is one: a load,
/// a store, an ALU operation, a step or a shift with its operand as written.
fn native_instruction(hla: &HlaStatement, source: &SourceFile) -> Option<String> {
    let text = |operand: &HlaOperand| source.span_text(operand.span).trim().to_owned();
    let register = |operand: &HlaOperand| match &operand.kind {
        HlaOperandKind::Register(register) => Some(register.kind),
        _ => None,
    };
    let (mnemonic, operand) = match hla {
        HlaStatement::Assign(assign) if assign.targets.len() == 1 => {
            let target = &assign.targets[0];
            match (register(target), register(&assign.source)) {
                (Some(loaded), None) => (
                    match loaded {
                        RegisterKind::A => "lda",
                        RegisterKind::X => "ldx",
                        RegisterKind::Y => "ldy",
                        _ => return None,
                    },
                    text(&assign.source),
                ),
                (None, Some(stored)) => (
                    match stored {
                        RegisterKind::A => "sta",
                        RegisterKind::X => "stx",
                        RegisterKind::Y => "sty",
                        _ => return None,
                    },
                    text(target),
                ),
                _ => return None,
            }
        }
        HlaStatement::Arithmetic(arithmetic) => (
            match (arithmetic.register.kind, arithmetic.op) {
                (RegisterKind::A, ArithmeticOp::Add) => "adc",
                (RegisterKind::A, ArithmeticOp::Subtract) => "sbc",
                (RegisterKind::A, ArithmeticOp::And) => "and",
                (RegisterKind::A, ArithmeticOp::Or) => "ora",
                (RegisterKind::A, ArithmeticOp::Xor) => "eor",
                (RegisterKind::A, ArithmeticOp::Compare) => "cmp",
                (RegisterKind::A, ArithmeticOp::BitTest) => "bit",
                (RegisterKind::X, ArithmeticOp::Compare) => "cpx",
                (RegisterKind::Y, ArithmeticOp::Compare) => "cpy",
                _ => return None,
            },
            text(&arithmetic.operand),
        ),
        HlaStatement::Step(step) if register(&step.target).is_none() => (
            match step.op {
                StepOp::Increment => "inc",
                StepOp::Decrement => "dec",
            },
            text(&step.target),
        ),
        HlaStatement::Shift(shift) if register(&shift.target).is_none() => (
            match shift.op {
                ShiftOp::Left => "asl",
                ShiftOp::Right => "lsr",
                ShiftOp::RotateLeft => "rol",
                ShiftOp::RotateRight => "ror",
            },
            text(&shift.target),
        ),
        _ => return None,
    };
    Some(format!("{mnemonic} {operand}"))
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
