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
//! - Sub-plan Q's `if` (`plans/16-if-statement.md`): a K816 source writes a
//!   condition in front of a block or a jump after `if`, so the bare form is
//!   refused there with a fix inserting it; a `.k65` source takes none of
//!   K816's respellings, so an `if` or a compare in a `while` is refused
//!   there with a fix back to K65's spelling. Either way the compare slot
//!   holds a compare, which is not a dialect rule.
//! - And sub-plan O's: a `.k65` source accepts what K816 *adds* and nothing
//!   it *respells* (`plans/16-if-statement.md` decision 11), so a `loop`
//!   statement, a flag test without its sign and a `push` / `pull` list are
//!   refused there too, each with the K65 spelling it stands for.
//! - Rule 3 (decision 10): in a K816 source's HLA, memory is reached only
//!   through a named variable, and a free-standing number is always a value
//!   — which `a = $10` already is, loading `#$10`. A number in any memory
//!   shape (`a = $10,x`, `$10 = a`, `$10++`, `a = ($10),y`) is refused, with
//!   the native mnemonic as the fix where the statement is one instruction:
//!   a mnemonic takes an address as written, and is the escape hatch. A
//!   number before `,s` is a displacement and no variable can name it, and
//!   one after `&` says it is an offset, so both stay.

use crate::ast::{
    ArithmeticOp, AstFile, Block, BranchCondition, BranchConditionKind, ComparisonKind,
    ConditionChain, DataBlock, DataEntryKind, Expr, ExprKind, FlagTestSpelling, HlaOperand,
    HlaOperandKind, HlaStack, HlaStatement, IndexRegisterKind, ItemKind, LoopCondition,
    LoopSpelling, LoopStatement, RegisterKind, ShiftOp, StackOp, StackSpelling, Statement,
    StatementKind, StepOp, TestableFlag, TypeWidthKind, UnaryOp,
};
use crate::diagnostic::{Applicability, Diagnostic, DiagnosticCode, Fix, Label, TextEdit};
use crate::source::{Dialect, SourceFile, Span};

/// Where `push`/`pull` starts: `gap` is the text from the previous
/// statement's end to `item`, and `kept` how much of it comes before the
/// keyword.
fn previous_start(item: Span, gap: &str, kept: usize) -> u32 {
    item.start - (gap.len() - kept) as u32
}

/// A flag test's K816 spelling — the letter, with `!` for a clear flag —
/// for every condition K816 writes as a flag test; `None` for a relation.
fn letter(condition: BranchConditionKind) -> Option<String> {
    let (letter, set) = match condition {
        BranchConditionKind::Flag { flag, set, .. } => (
            match flag {
                TestableFlag::Carry => "c",
                TestableFlag::Zero => "z",
                TestableFlag::Negative => "n",
                TestableFlag::Overflow => "v",
            },
            set,
        ),
        BranchConditionKind::Comparison(ComparisonKind::OverflowSet) => ("v", true),
        BranchConditionKind::Comparison(ComparisonKind::OverflowClear) => ("v", false),
        BranchConditionKind::Comparison(_) => return None,
    };
    Some(if set {
        letter.to_owned()
    } else {
        format!("!{letter}")
    })
}

/// Every spelling rule over one unit.
pub fn check(program: &AstFile, source: &SourceFile) -> Vec<Diagnostic> {
    let mut checker = Checker {
        source,
        diagnostics: Vec::new(),
        chained: std::collections::BTreeSet::new(),
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
    /// The `if` keywords of `else if` chains a `.k65` source wrote, already
    /// refused with the chain's own fix by the conditional they hang from.
    chained: std::collections::BTreeSet<u32>,
}

impl Checker<'_> {
    fn block(&mut self, block: &Block) {
        for (index, statement) in block.statements.iter().enumerate() {
            // A compare written in an `if` is the statement the builder put
            // before the conditional (`ConditionalBlock::compare`).
            let written = match &statement.kind {
                StatementKind::Conditional(conditional) => conditional.compare,
                StatementKind::ConditionalJump(jump) => jump.compare,
                _ => None,
            };
            if written.is_some()
                && let Some(compare) = index.checked_sub(1).map(|i| &block.statements[i])
            {
                self.compare_slot(compare, "an `if`");
            }
            self.statement(statement);
        }
        if self.source.dialect() == Dialect::K65 {
            self.k65_stack_words(block);
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
            // A `when` arm is what the builder made of the `when`: no
            // spelling in it was written, so neither dialect has one to
            // refuse, and `when` itself is an addition both accept.
            StatementKind::Conditional(conditional) if conditional.arm.is_some() => {
                self.block(&conditional.body);
                if let Some(otherwise) = &conditional.otherwise {
                    self.block(otherwise);
                }
            }
            StatementKind::Conditional(conditional) => {
                if k816 {
                    self.chain(&conditional.condition, conditional.keyword.is_some());
                    if conditional.keyword.is_none() {
                        self.bare_condition(&conditional.condition, "block");
                    }
                } else {
                    self.k65_chain_conditions(&conditional.condition);
                    if let Some(keyword) = conditional.keyword {
                        self.k65_if(keyword, conditional.compare, &conditional.condition);
                    }
                    if conditional.chained
                        && let Some(otherwise) = &conditional.otherwise
                    {
                        self.k65_chain(otherwise);
                    }
                }
                self.slots(&conditional.condition, "an `if`");
                self.block(&conditional.body);
                if let Some(otherwise) = &conditional.otherwise {
                    self.block(otherwise);
                }
            }
            StatementKind::ConditionalJump(jump) => {
                if k816 {
                    self.chain(&jump.condition, jump.keyword.is_some());
                    if jump.keyword.is_none() {
                        self.bare_condition(&jump.condition, "jump");
                    }
                } else {
                    self.k65_chain_conditions(&jump.condition);
                    if let Some(keyword) = jump.keyword {
                        self.k65_if(keyword, jump.compare, &jump.condition);
                    }
                }
                self.slots(&jump.condition, "an `if`");
                self.statement(&jump.jump);
            }
            StatementKind::Loop(loop_statement) => {
                if k816 {
                    self.postfix_loop(loop_statement);
                    // A loop's condition is after `while`, or is once
                    // `K816-K65-0007`'s fix has written it.
                    if let LoopCondition::Branch(chain) = &loop_statement.condition {
                        self.chain(chain, true);
                    }
                } else {
                    self.k65_loop(statement.span, loop_statement);
                    if let LoopCondition::Branch(chain) = &loop_statement.condition {
                        self.k65_chain_conditions(chain);
                    }
                    if let Some(compare) = loop_statement.test {
                        self.k65_while_compare(loop_statement, compare);
                    }
                }
                if loop_statement.test.is_some()
                    && let Some(compare) = loop_statement.body.statements.last()
                {
                    self.compare_slot(compare, "a `while`");
                }
                if let LoopCondition::Branch(chain) = &loop_statement.condition {
                    self.slots(chain, "a `while`");
                }
                self.block(&loop_statement.body);
            }
            StatementKind::WidthBlock(width) => self.block(&width.body),
            StatementKind::Nocross(block) => self.block(block),
            StatementKind::Data(data) => self.data(data),
            _ => {}
        }
    }

    /// `>= { … }` or `< goto done` in a K816 source: the conditional K816
    /// writes `if >= { … }`. The fix inserts the keyword, which is the whole
    /// difference — the parser builds the same statements from both.
    ///
    /// A flag test K816 spells otherwise — K65's `c+?`, sub-plan O's `c?`,
    /// the overflow relations — is written as its letter in the same edit,
    /// so that this one fix leaves a K816 condition (`plans/17-flag-tests.md`
    /// decision 2); the refusals of those spellings offer no fix of their own
    /// here, where theirs and this one would overlap.
    fn bare_condition(&mut self, chain: &ConditionChain, guarded: &str) {
        // A bare condition predates chains, so it is one element.
        let condition = chain.first();
        let text = letter(condition.kind)
            .unwrap_or_else(|| self.source.span_text(condition.span).to_owned());
        let what = if guarded == "block" {
            format!("if {text} {{ ... }}")
        } else {
            format!("if {text} ...")
        };
        self.diagnostics.push(
            Diagnostic::error(
                DiagnosticCode::K65_BARE_CONDITION,
                "a K816 source writes `if` before a condition",
                Label::new(condition.span, format!("K816 writes this `if {text}`")),
            )
            .with_note(format!(
                "K65 writes the condition alone in front of the {guarded} it guards; K816 says \
                 where the test begins, and both compile to the same bytes"
            ))
            .with_help(format!(
                "write `{what}`, or name the file `.k65` to keep K65's spelling"
            ))
            .with_fix(Fix {
                title: format!("write `if {text}`"),
                applicability: Applicability::MachineApplicable,
                edits: vec![TextEdit::replace(condition.span, format!("if {text}"))],
            })
            .with_docs("doc/syntax-reference.md#branch-and-flow-control"),
        );
    }

    /// The `if` of a `.k65` source, which writes K65's conditional: the
    /// condition alone, and a compare as the statement before it — which a
    /// `.k65` source may write on the same line, so deleting `if ` is the
    /// whole fix (`plans/16-if-statement.md` decision 10).
    fn k65_if(&mut self, keyword: Span, compare: Option<Span>, chain: &ConditionChain) {
        if self.chained.contains(&keyword.start) {
            return;
        }
        let next = compare.map_or(chain.span.start, |compare| compare.start);
        let words = Span::new(keyword.file, keyword.start, next);
        self.k65_spelling(
            keyword,
            "a `.k65` source writes a condition without `if`",
            "`if` is K816's spelling",
            "K65 writes the condition alone in front of the block or the jump, and a compare \
             as a statement before it; both compile to the same bytes",
            Fix {
                title: "delete `if`".to_owned(),
                applicability: Applicability::MachineApplicable,
                edits: vec![TextEdit::delete(words)],
            },
        );
    }

    /// `} else if == { … }` in a `.k65` source: K65 nests the conditional in
    /// the `else` block, so the fix gives the chain the braces K816 left out.
    fn k65_chain(&mut self, otherwise: &Block) {
        let Some(Statement {
            kind: StatementKind::Conditional(inner),
            ..
        }) = otherwise.statements.last()
        else {
            return;
        };
        let Some(keyword) = inner.keyword else {
            return;
        };
        self.chained.insert(keyword.start);
        let next = inner
            .compare
            .map_or(inner.condition.span.start, |compare| compare.start);
        let words = Span::new(keyword.file, keyword.start, next);
        let end = Span::empty_at(otherwise.span.file, otherwise.span.end);
        self.k65_spelling(
            keyword,
            "a `.k65` source writes `else if` as a conditional inside `else`",
            "`else if` is K816's spelling",
            "K65 writes `else { == { ... } }`, the conditional nested in the `else` block; \
             both compile to the same bytes",
            Fix {
                title: "write `else { ... }`".to_owned(),
                applicability: Applicability::MachineApplicable,
                edits: vec![TextEdit::replace(words, "{ "), TextEdit::replace(end, " }")],
            },
        );
    }

    /// `while x?10 <` in a `.k65` source, which writes the compare as the
    /// loop body's last statement — where it runs either way.
    fn k65_while_compare(&mut self, loop_statement: &LoopStatement, compare: Span) {
        let LoopCondition::Branch(chain) = &loop_statement.condition else {
            return;
        };
        let condition = chain.first();
        let text = self.source.span_text(compare).to_owned();
        let brace = Span::empty_at(
            loop_statement.body.span.file,
            loop_statement.body.span.end.saturating_sub(1),
        );
        let words = Span::new(compare.file, compare.start, condition.span.start);
        self.k65_spelling(
            compare,
            "a `.k65` source writes the loop's compare in its body",
            "a compare in a `while` is K816's spelling",
            "K65 writes the compare as the last statement of the body, which is where it runs \
             in both: a `repeat` skips it, and `nocross` checks it",
            Fix {
                title: format!("move `{text}` to the end of the body"),
                applicability: Applicability::MachineApplicable,
                edits: vec![
                    TextEdit::replace(brace, format!("{text} ")),
                    TextEdit::delete(words),
                ],
            },
        );
    }

    /// `loop { … } while COND` in a `.k65` source, which writes K65's
    /// postfix loop: the same block with its condition after it.
    fn k65_loop(&mut self, span: Span, loop_statement: &LoopStatement) {
        if loop_statement.spelling != LoopSpelling::Keyword {
            return;
        }
        let file = span.file;
        let head = Span::new(file, span.start, loop_statement.body.span.start);
        let mut edits = vec![TextEdit::replace(
            head,
            if loop_statement.nocross {
                "nocross "
            } else {
                ""
            },
        )];
        let after = match &loop_statement.condition {
            LoopCondition::Branch(chain) => {
                // `while`, from the brace to the compare or the condition.
                let next = loop_statement
                    .test
                    .map_or(chain.span.start, |compare| compare.start);
                edits.push(TextEdit::replace(
                    Span::new(file, loop_statement.body.span.end, next),
                    " ",
                ));
                self.source.span_text(chain.span).to_owned()
            }
            LoopCondition::Always => "always".to_owned(),
            LoopCondition::Never => "never".to_owned(),
        };
        let keyword = Span::new(file, span.start, loop_statement.body.span.start);
        self.k65_spelling(
            keyword,
            "a `.k65` source writes a loop as a block with its condition after it",
            "`loop` is K816's spelling",
            "K65's loop is the block and the condition after it; K816 says where it begins, \
             and both compile to the same bytes",
            Fix {
                title: format!(
                    "write `{}{{ ... }} {after}`",
                    if loop_statement.nocross {
                        "nocross "
                    } else {
                        ""
                    }
                ),
                applicability: Applicability::MachineApplicable,
                edits,
            },
        );
    }

    /// `c?` / `!c?` in a `.k65` source, which writes the flag with its sign:
    /// `c+?` / `c-?`, and the overflow flag as the relation K65 has for it.
    fn k65_condition(&mut self, condition: &BranchCondition) {
        let BranchConditionKind::Flag {
            flag,
            set,
            spelling: FlagTestSpelling::Bare | FlagTestSpelling::Letter,
        } = condition.kind
        else {
            return;
        };
        let text = self.source.span_text(condition.span);
        let letter = text.trim_start_matches('!').trim_end_matches('?').trim();
        let written = match (flag, set) {
            (TestableFlag::Overflow, true) => "<<=".to_owned(),
            (TestableFlag::Overflow, false) => ">>=".to_owned(),
            (_, true) => format!("{letter}+?"),
            (_, false) => format!("{letter}-?"),
        };
        self.k65_spelling(
            condition.span,
            "a `.k65` source writes a flag test with its sign",
            "a flag test without its sign is K816's spelling",
            "K65 writes `c+?` for a set flag and `c-?` for a clear one, and the overflow \
             flag's tests as `<<=` and `>>=`; both compile to the same branch",
            Fix {
                title: format!("write `{written}`"),
                applicability: Applicability::MachineApplicable,
                edits: vec![TextEdit::replace(condition.span, written.clone())],
            },
        );
    }

    /// `push a, $0012, x` / `pull x, a` in a `.k65` source, which writes one
    /// `a!!` or `a??` per register and a pushed constant as the `pea` it is.
    ///
    /// The builder makes one statement of each item, spanning only the item,
    /// so a list is found again in the text: an item whose text before it
    /// ends in `push` or `pull` begins one, and an item after a `,` goes on.
    fn k65_stack_words(&mut self, block: &Block) {
        let text = self.source.text();
        let mut lists: Vec<(Span, StackOp, Vec<&Statement>)> = Vec::new();
        let mut previous_end = block.span.start;
        for statement in &block.statements {
            let span = statement.span;
            let op = match &statement.kind {
                StatementKind::Hla(HlaStatement::Stack(stack))
                    if stack.spelling == StackSpelling::Word =>
                {
                    Some(stack.op)
                }
                StatementKind::Hla(HlaStatement::PushConstant { .. }) => Some(StackOp::Push),
                _ => None,
            };
            let gap = text
                .get(previous_end as usize..span.start as usize)
                .unwrap_or_default();
            previous_end = span.end;
            if span.file != self.source.id() {
                continue;
            }
            let Some(op) = op else { continue };
            let before = gap.trim_end();
            let word = if op == StackOp::Push { "push" } else { "pull" };
            if before.ends_with(',')
                && let Some((_, _, items)) = lists.last_mut()
            {
                items.push(statement);
            } else if let Some(stripped) = before.strip_suffix(word) {
                let start = previous_start(span, gap, stripped.len());
                lists.push((Span::new(span.file, start, span.start), op, vec![statement]));
            }
        }
        for (keyword, op, items) in lists {
            let line_start = text[..keyword.start as usize]
                .rfind('\n')
                .map_or(0, |at| at + 1);
            let indent: String = text[line_start..keyword.start as usize]
                .chars()
                .take_while(|c| c.is_whitespace())
                .collect();
            let suffix = if op == StackOp::Push { "!!" } else { "??" };
            let spelled: Vec<String> = items
                .iter()
                .map(|item| {
                    let written = self.source.span_text(item.span);
                    match &item.kind {
                        StatementKind::Hla(HlaStatement::PushConstant { marked: true, .. }) => {
                            format!("pea {written}")
                        }
                        StatementKind::Hla(HlaStatement::PushConstant { .. }) => {
                            format!("pea #{written}")
                        }
                        _ => format!("{written}{suffix}"),
                    }
                })
                .collect();
            let last = items.last().map_or(keyword.end, |item| item.span.end);
            let word = if op == StackOp::Push { "push" } else { "pull" };
            self.k65_spelling(
                Span::new(keyword.file, keyword.start, keyword.start + 4),
                &format!("a `.k65` source writes `{word}` one register at a time"),
                &format!("`{word}` is K816's spelling"),
                "K65 pushes with `a!!` and pulls with `a??`, one register each, and pushes a \
                 constant with the `pea` it compiles to; the same instructions in the same order",
                Fix {
                    title: format!("write `{}`", spelled.join(" ")),
                    applicability: Applicability::MachineApplicable,
                    edits: vec![TextEdit::replace(
                        Span::new(keyword.file, keyword.start, last),
                        spelled.join(&format!("\n{indent}")),
                    )],
                },
            );
        }
    }

    fn k65_spelling(&mut self, at: Span, title: &str, label: &str, note: &str, fix: Fix) {
        self.diagnostics.push(
            Diagnostic::error(
                DiagnosticCode::K816_SPELLING_IN_K65,
                title,
                Label::new(at, label),
            )
            .with_note(note)
            .with_help(format!(
                "{}, or name the file `.kk` to write K816",
                fix.title
            ))
            .with_fix(fix)
            .with_docs("doc/k65-compat.md#two-dialects-one-grammar"),
        );
    }

    /// The statement in an `if`'s or a `while`'s compare slot: one of the
    /// four compares, whatever the dialect (decision 5).
    fn compare_slot(&mut self, statement: &Statement, slot: &str) {
        // A step — `x--`, `mem++` — stands there too (decision 3).
        let StatementKind::Hla(HlaStatement::Arithmetic(arithmetic)) = &statement.kind else {
            return;
        };
        if matches!(arithmetic.op, ArithmeticOp::Compare | ArithmeticOp::BitTest) {
            return;
        }
        let text = self.source.span_text(statement.span);
        self.diagnostics.push(
            Diagnostic::error(
                DiagnosticCode::CONDITION_COMPARE,
                format!("only a compare can be written in {slot}"),
                Label::new(statement.span, "this is not a compare"),
            )
            .with_note(
                "the slot takes `a?`, `x?`, `y?` and `a&?`, which set flags and change nothing \
                 else; anything else there reads as a test of what it computes",
            )
            .with_help(format!(
                "write `{text}` as a statement of its own, before the condition"
            ))
            .with_docs("doc/syntax-reference.md#branch-and-flow-control"),
        );
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
        let after = match &loop_statement.condition {
            LoopCondition::Branch(chain) => {
                let condition = chain.first();
                edits.push(TextEdit::insert(condition.span, "while "));
                // A flag test is written as its letter there, by its own
                // refusal's fix (`plans/17-flag-tests.md`).
                let condition = letter(condition.kind)
                    .unwrap_or_else(|| self.source.span_text(condition.span).to_owned());
                format!("while {condition}")
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

    /// `c+?` / `c-?`, the overflow relations `<<=` / `>>=`, and sub-plan O's
    /// `c?` / `!c?`: the flag tests K816 writes as the flag's letter after
    /// `if` or `while` (`plans/17-flag-tests.md`).
    ///
    /// `guarded` says the condition already stands after `if` or `while`.
    /// Where it does not, the condition's `K816-K65-0011` writes `if` and the
    /// letter in one edit, and this refusal carries no fix of its own.
    /// [`Self::condition`] over every element of a chain, and the slot rule
    /// over every element's slot.
    fn chain(&mut self, chain: &ConditionChain, guarded: bool) {
        for element in &chain.elements {
            if !element.assumed {
                self.condition(&element.condition, guarded);
            }
        }
    }

    /// The slots of a chain's later elements: each holds a compare or a step
    /// (decision 3), and its statements are this unit's to check.
    fn slots(&mut self, chain: &ConditionChain, slot_name: &str) {
        for slot in chain.slots() {
            if let Some(statement) = slot.statements.last() {
                self.compare_slot(statement, slot_name);
            }
            self.block(slot);
        }
    }

    /// [`Self::k65_condition`] over every element of a chain.
    fn k65_chain_conditions(&mut self, chain: &ConditionChain) {
        for element in &chain.elements {
            if !element.assumed {
                self.k65_condition(&element.condition);
            }
        }
    }

    fn condition(&mut self, condition: &BranchCondition, guarded: bool) {
        let Some(written) = letter(condition.kind) else {
            return;
        };
        let (code, title, note) = match condition.kind {
            // Already K816's.
            BranchConditionKind::Flag {
                spelling: FlagTestSpelling::Letter,
                ..
            } => return,
            BranchConditionKind::Flag {
                spelling: FlagTestSpelling::Signed,
                ..
            } => (
                DiagnosticCode::K65_SIGNED_FLAG_TEST,
                "a K816 source writes a flag test as its letter",
                "`c` after `if` or `while` is `c+?`, and `!c` is `c-?`: the same branch",
            ),
            BranchConditionKind::Flag {
                spelling: FlagTestSpelling::Bare,
                ..
            } => {
                // Sub-plan O's `c?` alone in front of a block is the bare
                // condition's refusal, which writes the letter itself.
                if !guarded {
                    return;
                }
                (
                    DiagnosticCode::FLAG_TEST_MARK,
                    "a K816 source writes a flag test as its letter",
                    "after `if` or `while` the position says a name is a test, so the `?` that \
                     marked one says nothing; `c` and `c?` are the same branch",
                )
            }
            _ => (
                DiagnosticCode::K65_OVERFLOW_RELATION,
                "a K816 source tests the overflow flag by its name",
                "`<<=` and `>>=` read as relations and are the overflow flag, and `<<=` is one \
                 character from the comparison `<=`",
            ),
        };
        let mut diagnostic = Diagnostic::error(
            code,
            title,
            Label::new(condition.span, format!("K816 writes this test `{written}`")),
        )
        .with_note(note)
        .with_help(format!(
            "write `{}{written}`, or name the file `.k65` to keep K65's spelling",
            if guarded { "" } else { "if " }
        ))
        .with_docs("doc/syntax-reference.md#branch-and-flow-control");
        if guarded {
            diagnostic = diagnostic.with_fix(Fix {
                title: format!("write `{written}`"),
                applicability: Applicability::MachineApplicable,
                edits: vec![TextEdit::replace(condition.span, written.clone())],
            });
        }
        self.diagnostics.push(diagnostic);
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
            self.diagnostics
                .push(byte_constant_pushed(value.span, what, &expanded));
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

/// `push` of a constant that is one byte by construction (`K816-E0175`):
/// `what` says which construction, and `expanded` is the word to write.
///
/// Shared with lowering, which is where a `const` declared `:byte` is known
/// to be one — this pass reads spellings and has no symbol table.
pub(crate) fn byte_constant_pushed(span: Span, what: &str, expanded: &str) -> Diagnostic {
    Diagnostic::error(
        DiagnosticCode::PUSH_BYTE_CONSTANT,
        "a one-byte constant cannot be pushed",
        Label::new(span, format!("{what} is one byte, and `push` pushes two")),
    )
    .with_note(
        "the 65816 has no instruction that pushes a constant byte; `push` writes \
         `PEA`, which pushes a word whatever `M` says",
    )
    .with_help(format!(
        "write `{expanded}` to push it as a word, or load it and `push a` with an 8-bit \
         accumulator"
    ))
    .with_fix(Fix {
        title: format!("push it as the word `{expanded}`"),
        applicability: Applicability::MaybeIncorrect,
        edits: vec![TextEdit::replace(span, expanded.to_owned())],
    })
    .with_docs("doc/syntax-reference.md#push-and-pull")
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
