/*****************************************************************************/
/*                                                                           */
/*                               preprocess.rs                               */
/*                                                                           */
/*             Which parts of the program the directives leave               */
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

//! The preprocessor: `#if`, `#elif`, `#else`, `#endif`, `#error`, `#warn`.
//!
//! K816 has no text preprocessor and does not acquire one. This is a pass
//! over the AST (`plans/06-compatibility.md` contract 6): the parser has
//! already recognized every directive as a node, and what happens here is
//! that the *flat* stream of directives at each level is paired into nested
//! regions, each region is decided live or dead, and the program that
//! remains — dead elements and every directive removed — is what the rest of
//! the compiler walks. Nothing downstream ever sees a directive, which is
//! why `semantic.rs`, `resolve.rs` and `lower.rs` need no rule for one.
//!
//! # The three levels
//!
//! The original nests regions at exactly three places, and so does K816:
//! top-level items (`compiler.inc:7-10`), the statements of a code section
//! (`:142-145`), and the entries of a data block (`:98-101`). A region is
//! structural, so it may not cross a `}`: the `#if` and its `#endif` belong
//! to one element list or the program is malformed.
//!
//! # Why the pass runs before analysis, and what that costs
//!
//! Liveness has to be decided before any symbol is declared. A `func` in a
//! region that is not taken must not exist — otherwise the ordinary
//!
//! ```text
//! #if FAST
//! func plot { ... }
//! #else
//! func plot { ... }
//! #endif
//! ```
//!
//! is a duplicate definition. But a condition may read a `const` and a name
//! an evaluator bracket assigned — both measured against the original,
//! `tests/compat/k65/preprocessor-condition-reads/` — and those only exist
//! *while* `semantic::declare_items` walks the file.
//!
//! The circle is broken by giving this pass a compile-time environment of
//! its own: it walks the file in source order, folds each `const` it passes
//! with a throwaway [`SymbolTable`], and runs each evaluator bracket through
//! a **silent** interpreter whose diagnostics, `print` notes and step budget
//! are discarded. That evaluation exists only to decide conditions;
//! `semantic.rs` runs every bracket again, for real, and it is that second
//! run which reports. So a bracket in a program that uses the preprocessor
//! is evaluated twice — and a program that contains no directive at all
//! skips this pass entirely ([`apply`] returns the file it was given), which
//! is every source in the corpus but two.
//!
//! That environment is built *as the walk passes each declaration*, so it is
//! not quite the one analysis builds: analysis declares every function and
//! data label up front, which is what makes a forward reference work, and
//! here a name is only visible below its declaration. A condition needs a
//! number, and none of the forward-declarable kinds is one, so the difference
//! shows up as a diagnostic that says "no compile-time value" rather than
//! "a data label" — never as a branch decided the wrong way.
//!
//! # Where K816 differs from the original, deliberately
//!
//! A condition inside a region that is not taken is **not** evaluated, and
//! neither is an `#elif` whose chain has already taken a branch. The
//! original evaluates both — its `PPIf` and `PPIfNumber` actions are the
//! only ones in the grammar with no `ST` guard, so `#if 0` around
//! `#if UNDEFINED` still reports "Undeclared eval variable". K816 takes the
//! rule every C programmer already has: `#if 0` comments out what is inside
//! it, whatever that is. `tests/compat/k65/preprocessor-dead-conditions/`
//! measures the original and the manifest row `pp-dead-conditions` records
//! the change.
//!
//! Three shapes abort the original outright rather than diagnosing anything
//! — `#elif` at statement or data level, `#error`/`#warn` anywhere but the
//! top level, and a region whose `#endif` is across a `}`. K816 supports the
//! first two at all three levels and reports the third, which is a strict
//! superset and costs nothing (decision 0).

use std::borrow::Cow;

use crate::ast::{
    AstFile, Block, ConstDeclaration, DataBlock, DataEntry, DataEntryKind, EvalExpr, Item,
    ItemKind, PreprocessorDirective, PreprocessorKind, Statement, StatementKind,
};
use crate::diagnostic::{Diagnostic, DiagnosticCode, Label};
use crate::eval;
use crate::resolve::Resolver;
use crate::source::{Provenance, Span};
use crate::symbol::{SymbolKind, SymbolTable};

/// Applies the preprocessor to `file`.
///
/// Returns the program that remains — every element of a region that was not
/// taken removed, and every directive removed with them — together with the
/// diagnostics the directives themselves produced: an `#error`, a `#warn`, a
/// condition that could not be evaluated, and the unbalanced shapes.
///
/// A file with no directive anywhere is returned untouched and unevaluated,
/// so the cost of the pass is paid only by sources that use it.
pub fn apply(file: &AstFile) -> (Cow<'_, AstFile>, Vec<Diagnostic>) {
    if !file.items.iter().any(item_has_directive) {
        return (Cow::Borrowed(file), Vec::new());
    }
    let mut pass = Preprocess::default();
    let items = pass.items(&file.items, end_of(&file.items));
    let mut file = AstFile { items };
    // The statement identities the builder assigned are keyed by source
    // order over the *parsed* file. Removing statements would leave them
    // sparse, and every stage after this one keys per-statement facts by
    // them, so the surviving statements are renumbered over the program that
    // actually exists.
    crate::parser::assign_statement_ids(&mut file);
    (Cow::Owned(file), pass.diagnostics)
}

/// A zero-width span just past the last item, for a top-level `#if` that the
/// file ends without closing. There is no `}` to point at up here.
fn end_of(items: &[Item]) -> Option<Span> {
    items
        .last()
        .map(|item| Span::empty_at(item.span.file, item.span.end))
}

// ---------------------------------------------------------------------------
// Does this subtree contain a directive at all?
//
// The question is asked once per file, and answering it "no" is what makes
// the pass free for every source that does not use the preprocessor. It is a
// separate walk from the rewrite rather than a flag the rewrite sets,
// because the rewrite has to run the evaluator and there is nothing to run
// it for when the answer is no.
// ---------------------------------------------------------------------------

fn item_has_directive(item: &Item) -> bool {
    match &item.kind {
        ItemKind::Preprocessor(_) => true,
        ItemKind::Func(func) => block_has_directive(&func.body),
        ItemKind::Data(data) => data_has_directive(data),
        ItemKind::Var(_)
        | ItemKind::Const(_)
        | ItemKind::Segment(_)
        | ItemKind::EvalBlock(_)
        | ItemKind::EvalFunc(_)
        | ItemKind::Binary(_)
        | ItemKind::Reserved(_)
        | ItemKind::ModuleWidth(_)
        | ItemKind::Error => false,
    }
}

fn block_has_directive(block: &Block) -> bool {
    block.statements.iter().any(statement_has_directive)
}

fn statement_has_directive(statement: &Statement) -> bool {
    match &statement.kind {
        StatementKind::Preprocessor(_) => true,
        StatementKind::WidthBlock(width) => block_has_directive(&width.body),
        StatementKind::Nocross(body) => block_has_directive(body),
        StatementKind::Loop(statement) => block_has_directive(&statement.body),
        StatementKind::Conditional(conditional) => {
            block_has_directive(&conditional.body)
                || conditional
                    .otherwise
                    .as_ref()
                    .is_some_and(block_has_directive)
        }
        StatementKind::ConditionalJump(jump) => statement_has_directive(&jump.jump),
        StatementKind::Data(data) => data_has_directive(data),
        StatementKind::Label(_)
        | StatementKind::Native(_)
        | StatementKind::Hla(_)
        | StatementKind::ModeSet(_)
        | StatementKind::Call(_)
        | StatementKind::BareCall(_)
        | StatementKind::Goto(_)
        | StatementKind::Break
        | StatementKind::Repeat
        | StatementKind::Return(_)
        | StatementKind::Align(_)
        | StatementKind::Address(_)
        | StatementKind::Reserved(_)
        | StatementKind::Error => false,
    }
}

fn data_has_directive(data: &DataBlock) -> bool {
    data.entries.iter().any(|entry| match &entry.kind {
        DataEntryKind::Preprocessor(_) => true,
        DataEntryKind::Code { body, .. } => block_has_directive(body),
        DataEntryKind::Repeat { body, .. } => data_has_directive(body),
        DataEntryKind::Segment(_)
        | DataEntryKind::Align(_)
        | DataEntryKind::Address(_)
        | DataEntryKind::Nocross
        | DataEntryKind::Charset(_)
        | DataEntryKind::Label(_)
        | DataEntryKind::Typed { .. }
        | DataEntryKind::Value(_)
        | DataEntryKind::ForEval { .. }
        | DataEntryKind::Binary(_)
        | DataEntryKind::Reserved(_)
        | DataEntryKind::Error => false,
    })
}

// ---------------------------------------------------------------------------
// The gate: one element list's regions, as a stack
// ---------------------------------------------------------------------------

/// How far a `#if` / `#elif` / `#else` chain has got.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Chain {
    /// No branch of this chain has been taken yet; the next condition still
    /// decides something.
    Open,
    /// A branch has been taken, so every later branch is dead and no later
    /// condition is evaluated.
    Settled,
}

struct Frame {
    /// The `#if` that opened this region, for the report if it is never
    /// closed and for the "matching `#if`" secondary label.
    open: Span,
    chain: Chain,
    /// Whether the elements at this depth are live, ignoring enclosing
    /// frames.
    live: bool,
    /// The `#else` already seen in this chain, if any: a second one, or an
    /// `#elif` after one, is the fourth unbalanced shape.
    otherwise: Option<Span>,
}

/// The walk, and everything the conditions it meets can read.
///
/// The region stack is *not* here: it is a local of each level's loop,
/// because a region may not cross from one element list into another and a
/// shared stack would let it.
#[derive(Default)]
struct Preprocess {
    diagnostics: Vec<Diagnostic>,
    /// The evaluator variables assigned so far, in source order.
    env: eval::Environment,
    /// Which bracket is running, so the evaluator's write-once rule reads
    /// the same here as it does in analysis.
    block: eval::BlockId,
    /// The `evalfunc` definitions passed so far, so a condition may call one.
    functions: eval::Functions,
    /// The `const` values and data-section labels passed so far — the second
    /// namespace a condition reads. Built here rather than borrowed because
    /// the real one does not exist until liveness is already decided.
    symbols: SymbolTable,
    /// What each bracket this pass evaluated produced, keyed by its span.
    ///
    /// It exists because folding a `const` needs a [`Resolver`] and a
    /// resolver looks a bracket's value up here — which is what makes
    /// `const N = [2 * 3]` fold, and therefore what makes `#if N` below it
    /// decide anything. Analysis keeps its own, from its own run.
    outcome: eval::Outcome,
}

impl Preprocess {
    // -----------------------------------------------------------------
    // The three levels
    // -----------------------------------------------------------------

    fn items(&mut self, items: &[Item], end: Option<Span>) -> Vec<Item> {
        let mut stack = Vec::new();
        let mut kept = Vec::new();
        for item in items {
            if let ItemKind::Preprocessor(directive) = &item.kind {
                self.directive(&mut stack, directive, item.span);
                continue;
            }
            if !live(&stack) {
                continue;
            }
            // Rewritten first, then observed: a bracket inside a region this
            // item's *own* directives do not take must not run, and after
            // the rewrite it is no longer there to run.
            let item = Item {
                kind: self.item_kind(&item.kind, item.span),
                span: item.span,
            };
            self.observe(&item);
            kept.push(item);
        }
        self.close(stack, end);
        kept
    }

    fn block(&mut self, block: &Block) -> Block {
        let mut stack = Vec::new();
        let mut kept = Vec::new();
        for statement in &block.statements {
            if let StatementKind::Preprocessor(directive) = &statement.kind {
                self.directive(&mut stack, directive, statement.span);
                continue;
            }
            if !live(&stack) {
                continue;
            }
            let mut statement = statement.clone();
            statement.kind = self.statement_kind(&statement.kind, statement.span);
            kept.push(statement);
        }
        self.close(stack, Some(Span::empty_at(block.span.file, block.span.end)));
        Block {
            statements: kept,
            span: block.span,
        }
    }

    fn data(&mut self, data: &DataBlock, end: Option<Span>) -> DataBlock {
        let mut stack = Vec::new();
        let mut kept = Vec::new();
        for entry in &data.entries {
            if let DataEntryKind::Preprocessor(directive) = &entry.kind {
                self.directive(&mut stack, directive, entry.span);
                continue;
            }
            if !live(&stack) {
                continue;
            }
            let kind = match &entry.kind {
                DataEntryKind::Code { nocross, body } => DataEntryKind::Code {
                    nocross: *nocross,
                    body: self.block(body),
                },
                DataEntryKind::Repeat { count, body } => DataEntryKind::Repeat {
                    count: count.clone(),
                    body: self.data(body, Some(Span::empty_at(entry.span.file, entry.span.end))),
                },
                // Exhaustive for the reason `item_kind`'s tail gives.
                DataEntryKind::Segment(_)
                | DataEntryKind::Align(_)
                | DataEntryKind::Address(_)
                | DataEntryKind::Nocross
                | DataEntryKind::Charset(_)
                | DataEntryKind::Label(_)
                | DataEntryKind::Typed { .. }
                | DataEntryKind::Value(_)
                | DataEntryKind::ForEval { .. }
                | DataEntryKind::Preprocessor(_)
                | DataEntryKind::Binary(_)
                | DataEntryKind::Reserved(_)
                | DataEntryKind::Error => entry.kind.clone(),
            };
            kept.push(DataEntry {
                kind,
                span: entry.span,
            });
        }
        self.close(stack, end);
        DataBlock {
            name: data.name.clone(),
            nocross: data.nocross,
            entries: kept,
        }
    }

    /// Rewrites the levels nested inside one item, leaving the item itself
    /// alone.
    fn item_kind(&mut self, kind: &ItemKind, span: Span) -> ItemKind {
        match kind {
            ItemKind::Func(func) => {
                let mut func = func.clone();
                func.body = self.block(&func.body);
                ItemKind::Func(func)
            }
            ItemKind::Data(data) => ItemKind::Data(self.data(data, closing(span))),
            // Exhaustive on purpose. A construct added later that carries an
            // element list has to be listed above or this match stops
            // compiling; a `_` arm would clone it past the filter and leave
            // a directive inside it alive, which is the failure
            // `no_directive_survives_anywhere_in_the_tree` exists to catch
            // and cannot catch for a shape nobody wrote a case for.
            ItemKind::Var(_)
            | ItemKind::Const(_)
            | ItemKind::Segment(_)
            | ItemKind::EvalBlock(_)
            | ItemKind::EvalFunc(_)
            | ItemKind::Preprocessor(_)
            | ItemKind::Binary(_)
            | ItemKind::Reserved(_)
            | ItemKind::ModuleWidth(_)
            | ItemKind::Error => kind.clone(),
        }
    }

    /// Rewrites the levels nested inside one statement.
    fn statement_kind(&mut self, kind: &StatementKind, span: Span) -> StatementKind {
        match kind {
            StatementKind::WidthBlock(width) => {
                let mut width = width.clone();
                width.body = self.block(&width.body);
                StatementKind::WidthBlock(width)
            }
            StatementKind::Nocross(body) => StatementKind::Nocross(self.block(body)),
            StatementKind::Loop(statement) => {
                let mut statement = statement.clone();
                statement.body = self.block(&statement.body);
                StatementKind::Loop(statement)
            }
            StatementKind::Conditional(conditional) => {
                let mut conditional = conditional.clone();
                conditional.body = self.block(&conditional.body);
                conditional.otherwise = conditional
                    .otherwise
                    .as_ref()
                    .map(|otherwise| self.block(otherwise));
                StatementKind::Conditional(conditional)
            }
            StatementKind::ConditionalJump(jump) => {
                let mut jump = jump.clone();
                let inner = jump.jump.span;
                jump.jump.kind = self.statement_kind(&jump.jump.kind, inner);
                StatementKind::ConditionalJump(jump)
            }
            StatementKind::Data(data) => StatementKind::Data(self.data(data, closing(span))),
            // Exhaustive for the reason `item_kind`'s tail gives.
            StatementKind::Label(_)
            | StatementKind::Native(_)
            | StatementKind::Hla(_)
            | StatementKind::ModeSet(_)
            | StatementKind::Call(_)
            | StatementKind::BareCall(_)
            | StatementKind::Goto(_)
            | StatementKind::Break
            | StatementKind::Repeat
            | StatementKind::Preprocessor(_)
            | StatementKind::Return(_)
            | StatementKind::Align(_)
            | StatementKind::Address(_)
            | StatementKind::Reserved(_)
            | StatementKind::Error => kind.clone(),
        }
    }

    // -----------------------------------------------------------------
    // The directives
    // -----------------------------------------------------------------

    fn directive(&mut self, stack: &mut Vec<Frame>, directive: &PreprocessorDirective, span: Span) {
        let keyword = directive.keyword_span;
        match directive.kind {
            PreprocessorKind::If => {
                // Not evaluated at all when the region around it is not
                // taken: `#if 0` comments out what is inside it, whatever
                // that is. See the module header for how the original
                // differs.
                let (chain, live) = if live(stack) {
                    let taken = self.condition(directive.condition.as_ref(), span);
                    (if taken { Chain::Settled } else { Chain::Open }, taken)
                } else {
                    (Chain::Settled, false)
                };
                stack.push(Frame {
                    open: keyword,
                    chain,
                    live,
                    otherwise: None,
                });
            }
            PreprocessorKind::ElseIf | PreprocessorKind::Else => {
                let is_else = directive.kind == PreprocessorKind::Else;
                let outer = live(&stack[..stack.len().saturating_sub(1)]);
                let Some(frame) = stack.last_mut() else {
                    self.unmatched(keyword, if is_else { "#else" } else { "#elif" });
                    return;
                };
                if let Some(first) = frame.otherwise {
                    let word = if is_else { "#else" } else { "#elif" };
                    self.diagnostics.push(
                        Diagnostic::error(
                            DiagnosticCode::PREPROCESSOR_UNBALANCED,
                            format!("`{word}` after the `#else` of the same `#if`"),
                            Label::new(keyword, "this branch can never be reached"),
                        )
                        .with_secondary(Label::new(first, "`#else` already ended the chain here"))
                        .with_secondary(Label::new(frame.open, "the `#if` it belongs to"))
                        .with_docs("doc/syntax-reference.md#preprocessor-directives"),
                    );
                    frame.live = false;
                    frame.chain = Chain::Settled;
                    return;
                }
                if is_else {
                    frame.otherwise = Some(keyword);
                }
                // A branch the chain has already settled is dead whatever
                // its condition says, so the condition is not evaluated —
                // the same rule `#if` follows inside a dead region.
                if !outer || frame.chain == Chain::Settled {
                    frame.live = false;
                    frame.chain = Chain::Settled;
                    return;
                }
                if is_else {
                    frame.live = true;
                    frame.chain = Chain::Settled;
                    return;
                }
                let taken = self.condition(directive.condition.as_ref(), span);
                let frame = stack.last_mut().expect("checked above");
                frame.live = taken;
                if taken {
                    frame.chain = Chain::Settled;
                }
            }
            PreprocessorKind::EndIf => {
                if stack.pop().is_none() {
                    self.unmatched(keyword, "#endif");
                }
            }
            PreprocessorKind::Error | PreprocessorKind::Warn => {
                if !live(stack) {
                    return;
                }
                let message = directive.message.clone().unwrap_or_default();
                let warn = directive.kind == PreprocessorKind::Warn;
                let (code, title) = if warn {
                    (DiagnosticCode::PREPROCESSOR_WARNING, "`#warn`")
                } else {
                    (DiagnosticCode::PREPROCESSOR_ERROR, "`#error`")
                };
                let label = Label::new(span, format!("{title} here"));
                let diagnostic = if warn {
                    Diagnostic::warning(code, message, label)
                } else {
                    Diagnostic::error(code, message, label)
                };
                self.diagnostics
                    .push(diagnostic.with_docs("doc/syntax-reference.md#preprocessor-directives"));
            }
        }
    }

    /// Reports every `#if` an element list ended without closing.
    ///
    /// This is also how a region that crosses a `}` is caught: the inner
    /// level ends with the `#if` still open and says so here, and the
    /// `#endif` on the other side finds no frame of its own and reports
    /// itself.
    fn close(&mut self, stack: Vec<Frame>, end: Option<Span>) {
        for frame in stack {
            let mut diagnostic = Diagnostic::error(
                DiagnosticCode::PREPROCESSOR_UNBALANCED,
                "`#if` is never closed",
                Label::new(frame.open, "this region has no `#endif`"),
            );
            if let Some(end) = end {
                diagnostic = diagnostic.with_secondary(Label::new(end, "the region ends here"));
            }
            self.diagnostics.push(
                diagnostic
                    .with_note(
                        "a preprocessor region is structural: it opens and closes inside one \
                         block, one data block, or the file's top level",
                    )
                    .with_docs("doc/syntax-reference.md#preprocessor-directives"),
            );
        }
    }

    fn unmatched(&mut self, keyword: Span, word: &str) {
        self.diagnostics.push(
            Diagnostic::error(
                DiagnosticCode::PREPROCESSOR_UNBALANCED,
                format!("`{word}` with no `#if` above it"),
                Label::new(keyword, "nothing here opened a region"),
            )
            .with_note(
                "a preprocessor region is structural: it opens and closes inside one block, \
                 one data block, or the file's top level",
            )
            .with_docs("doc/syntax-reference.md#preprocessor-directives"),
        );
    }

    // -----------------------------------------------------------------
    // Evaluation
    // -----------------------------------------------------------------

    /// Evaluates one condition and says whether its branch is taken.
    ///
    /// True when the value is a number that does not round to zero — the
    /// original's `num($2) != 0` over a `doeval` result, which has already
    /// been through the integer boundary rule, so `#if [0.4]` is false and
    /// `#if [0.6]` is true (`tests/compat/k65/preprocessor-condition-reads/`).
    /// A condition that cannot be evaluated reports and its branch is not
    /// taken.
    fn condition(&mut self, condition: Option<&EvalExpr>, span: Span) -> bool {
        let Some(condition) = condition else {
            // The parser recovered: a syntax error already stands, and
            // inventing a branch on top of it would report the same mistake
            // twice.
            return false;
        };
        if eval::is_recovered(condition) {
            return false;
        }
        match self.evaluate(condition) {
            Ok(value) => match value.number() {
                Some(number) => eval::to_integer(number) != 0,
                None => {
                    self.diagnostics.push(
                        Diagnostic::error(
                            DiagnosticCode::EVAL_NOT_A_NUMBER,
                            format!(
                                "a preprocessor condition must be a number, and this is {}",
                                value.describe()
                            ),
                            Label::new(span, "this decides whether a region is compiled"),
                        )
                        .with_docs("doc/syntax-reference.md#preprocessor-directives"),
                    );
                    false
                }
            },
            Err(error) => {
                if let Some(diagnostic) = crate::resolve::eval_diagnostic(error) {
                    self.diagnostics.push(diagnostic);
                }
                false
            }
        }
    }

    /// Runs one evaluator expression against this pass's environment.
    ///
    /// The `print` notes are dropped: the bracket that produced them runs
    /// again in `semantic.rs`, and that run is the one that speaks. A
    /// condition has no second run, so [`Self::condition`] reports its own
    /// errors; a bracket's errors are dropped for the same reason its notes
    /// are.
    fn evaluate(&mut self, body: &EvalExpr) -> Result<eval::Value, eval::Error> {
        self.block += 1;
        let mut env = std::mem::take(&mut self.env);
        let value = eval::Interpreter::new(&mut env, &self.functions, &self.symbols, self.block)
            .evaluate(body);
        // A name a bracket assigned is a compiler constant from there down,
        // and a condition below it may read it by either route.
        for (name, assigned) in env.take_new_names() {
            if let Some(number) = assigned.number() {
                let _ = self.symbols.declare(
                    &name,
                    SymbolKind::Const {
                        value: eval::to_integer(number) as u32,
                    },
                    body.span,
                );
            }
        }
        self.env = env;
        if let Ok(value) = &value {
            self.outcome.record(body.span, value.clone());
        }
        value
    }

    /// Records what one live item contributes to the environment a later
    /// condition reads: the brackets it contains, the `const` values it
    /// declares, the data sections it names, and the `evalfunc` it defines.
    ///
    /// Diagnostics are not produced here. Everything this touches is walked
    /// again by `semantic.rs`, which reports; a second voice would say
    /// everything twice.
    fn observe(&mut self, item: &Item) {
        match &item.kind {
            ItemKind::EvalFunc(function) => {
                let _ = self.functions.declare(
                    &function.name.text,
                    eval::UserFunction {
                        parameters: function
                            .parameters
                            .iter()
                            .map(|parameter| parameter.text.clone())
                            .collect(),
                        body: function.body.clone(),
                        declaration: function.name.span,
                    },
                );
            }
            // A `const` group is a sequence of declarations and a later
            // entry may read an earlier one, so each entry's brackets run
            // immediately before that entry folds — the order
            // `semantic::declare_const_group` keeps for the same reason.
            ItemKind::Const(declaration) => self.consts(declaration),
            ItemKind::Data(data) => {
                self.labels(data);
                self.brackets(item);
            }
            // A `var` has no compile-time value, and declaring it is what
            // makes the pass say so in the same words analysis would: without
            // the name in the table a condition reading it is "undeclared",
            // which is both wrong and less useful than "a variable".
            ItemKind::Var(var) => {
                if var.name.provenance != Provenance::Synthetic {
                    let _ = self.symbols.declare(
                        &var.name.text,
                        SymbolKind::Var {
                            address: None,
                            size: crate::symbol::AddressSize::Absolute,
                            width: None,
                            layout: None,
                            element: 1,
                            repeat: None,
                        },
                        var.name.span,
                    );
                }
                self.brackets(item);
            }
            // Likewise a code section: it has an address and no value, and
            // the pass has to know the name exists in order to say so.
            ItemKind::Func(func) => {
                if func.name.provenance != Provenance::Synthetic {
                    let kind = match func.kind {
                        crate::ast::FunctionKind::Inline => SymbolKind::Inline,
                        _ => SymbolKind::function(func.is_far),
                    };
                    let _ = self.symbols.declare(&func.name.text, kind, func.name.span);
                }
                self.brackets(item);
            }
            ItemKind::Segment(_)
            | ItemKind::EvalBlock(_)
            | ItemKind::Preprocessor(_)
            | ItemKind::Binary(_)
            | ItemKind::Reserved(_)
            | ItemKind::ModuleWidth(_)
            | ItemKind::Error => self.brackets(item),
        }
    }

    fn brackets(&mut self, item: &Item) {
        let mut sites = Vec::new();
        eval::sites_of_item(item, &mut sites);
        for site in sites {
            if let eval::Site::Bracket(body) = site
                && !eval::is_recovered(body)
            {
                let _ = self.evaluate(body);
            }
        }
    }

    fn consts(&mut self, declaration: &ConstDeclaration) {
        let mut next = Some(0u32);
        for entry in &declaration.entries {
            if let Some(expression) = &entry.value {
                let mut sites = Vec::new();
                eval::sites_of_expr(expression, &mut sites);
                for site in sites {
                    if let eval::Site::Bracket(body) = site
                        && !eval::is_recovered(body)
                    {
                        let _ = self.evaluate(body);
                    }
                }
            }
            let value = match &entry.value {
                Some(expression) => {
                    let mut resolver = Resolver::new(&self.symbols, &self.outcome);
                    let value = resolver.constant(expression, "`const` values");
                    // Discarded on purpose: `declare_const_group` folds the
                    // same expression against the real symbol table and
                    // reports what it finds. A name this pass has not
                    // reached yet is missing here and present there.
                    let _ = resolver.finish();
                    value
                }
                None => next,
            };
            next = value.map(|value| value.wrapping_add(1));
            if entry.name.provenance == Provenance::Synthetic {
                continue;
            }
            if let Some(value) = value {
                let _ = self.symbols.declare(
                    &entry.name.text,
                    SymbolKind::Const { value },
                    entry.name.span,
                );
            }
        }
    }

    /// Declares a data block's labels, which is all a condition can do with
    /// one: `getevalvar` reads a label as the *section* it names, so the
    /// value exists to make `index(tab, 2)` mean something rather than to be
    /// a number.
    fn labels(&mut self, data: &DataBlock) {
        let mut declare = |name: &crate::ast::Name| {
            if name.provenance != Provenance::Synthetic {
                let _ = self.symbols.declare(
                    &name.text,
                    SymbolKind::DataLabel {
                        size: crate::symbol::AddressSize::Absolute,
                    },
                    name.span,
                );
            }
        };
        if let Some(name) = &data.name {
            declare(name);
        }
        for entry in &data.entries {
            if let DataEntryKind::Label(name) = &entry.kind {
                declare(name);
            }
        }
    }
}

/// The zero-width point at which a construct's span ends — the `}` an
/// unclosed `#if` inside it never reached.
fn closing(span: Span) -> Option<Span> {
    Some(Span::empty_at(span.file, span.end))
}

/// Whether the elements at the bottom of `stack` are compiled.
fn live(stack: &[Frame]) -> bool {
    stack.iter().all(|frame| frame.live)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse_source;
    use crate::source::SourceMap;

    /// Preprocesses `source`, returning what survived and the codes reported.
    fn run(source: &str) -> (AstFile, Vec<String>) {
        let mut map = SourceMap::new();
        let id = map.add_file("pp.k65", source);
        let parsed = parse_source(map.file(id), None);
        assert_eq!(parsed.diagnostics, vec![], "`{source}` did not parse");
        let (program, diagnostics) = apply(&parsed.file);
        let codes = diagnostics
            .iter()
            .map(|diagnostic| diagnostic.code.as_str().to_owned())
            .collect();
        (program.into_owned(), codes)
    }

    /// The names the surviving top-level `const` and `func` items declare, in
    /// order — enough to say which branch of every region was taken without
    /// asserting on a whole tree.
    fn survivors(file: &AstFile) -> Vec<String> {
        let mut names = Vec::new();
        for item in &file.items {
            match &item.kind {
                ItemKind::Const(declaration) => names.extend(
                    declaration
                        .entries
                        .iter()
                        .map(|entry| entry.name.text.clone()),
                ),
                ItemKind::Func(func) => names.push(func.name.text.clone()),
                ItemKind::Var(var) => names.push(var.name.text.clone()),
                _ => {}
            }
        }
        names
    }

    /// A file with no directive is handed back untouched, and — the part that
    /// matters — *unevaluated*. The pass runs a second, silent copy of the
    /// evaluator, and this is what keeps every source that does not use the
    /// preprocessor from paying for it.
    #[test]
    fn a_file_with_no_directive_is_not_copied() {
        let mut map = SourceMap::new();
        let id = map.add_file("pp.k65", "[ N = 1 ]\nfunc main @a8 @i8 { return }\n");
        let parsed = parse_source(map.file(id), None);
        let (program, diagnostics) = apply(&parsed.file);
        assert!(matches!(program, Cow::Borrowed(_)));
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn the_taken_branch_is_the_only_one_that_survives() {
        let (file, codes) = run("#if 1\nconst TAKEN = 1\n#else\nconst DROPPED = 2\n#endif\n\
             #if 0\nconst ALSO_DROPPED = 3\n#else\nconst ELSE_TAKEN = 4\n#endif\n");
        assert_eq!(codes, Vec::<String>::new());
        assert_eq!(survivors(&file), ["TAKEN", "ELSE_TAKEN"]);
    }

    /// The `#elif` chain is C's: the first true branch wins, and a later true
    /// one does not reopen the chain.
    #[test]
    fn an_elif_chain_takes_its_first_true_branch_only() {
        let (file, _) = run(
            "#if 0\nconst A = 1\n#elif 1\nconst B = 2\n#elif 1\nconst C = 3\n\
             #else\nconst D = 4\n#endif\n",
        );
        assert_eq!(survivors(&file), ["B"]);
    }

    #[test]
    fn an_elif_chain_with_no_true_branch_falls_through_to_else() {
        let (file, _) =
            run("#if 0\nconst A = 1\n#elif 0\nconst B = 2\n#else\nconst C = 3\n#endif\n");
        assert_eq!(survivors(&file), ["C"]);
    }

    /// A region inside a region that is not taken is dead however its own
    /// condition reads — and its condition is never evaluated, so a name
    /// nothing declares is not an error there. This is the one rule K816
    /// takes from C rather than from the original.
    #[test]
    fn a_dead_region_swallows_the_regions_inside_it() {
        let (file, codes) = run(
            "#if 0\n#if 1\nconst INNER = 1\n#endif\n#if NOTHING_DECLARES_THIS\n\
             const ALSO = 2\n#endif\n#endif\nconst AFTER = 3\n",
        );
        assert_eq!(codes, Vec::<String>::new());
        assert_eq!(survivors(&file), ["AFTER"]);
    }

    /// Nor is the condition of an `#elif` whose chain has already decided.
    #[test]
    fn a_settled_chain_does_not_evaluate_its_remaining_conditions() {
        let (_, codes) =
            run("#if 1\nconst A = 1\n#elif NOTHING_DECLARES_THIS\nconst B = 2\n#endif\n");
        assert_eq!(codes, Vec::<String>::new());
    }

    /// The two namespaces a condition reads, which is why the pass carries a
    /// symbol table and an evaluator environment of its own.
    #[test]
    fn a_condition_reads_a_const_and_an_evaluator_name() {
        let (file, _) = run("[ ASSIGNED = 1 ]\nconst DECLARED = 1\n\
             #if DECLARED\nconst FROM_CONST = 1\n#endif\n\
             #if ASSIGNED\nconst FROM_BRACKET = 2\n#endif\n\
             #if [ DECLARED + ASSIGNED == 2 ]\nconst FROM_BOTH = 3\n#endif\n");
        assert_eq!(
            survivors(&file),
            ["DECLARED", "FROM_CONST", "FROM_BRACKET", "FROM_BOTH"]
        );
    }

    /// The condition goes through the integer boundary rule before it decides,
    /// which is `num($2) != 0` over a `doeval` result.
    #[test]
    fn a_fractional_condition_rounds_before_it_decides() {
        let (file, _) = run("#if [0.6]\nconst UP = 1\n#endif\n#if [0.4]\nconst DOWN = 2\n#endif\n");
        assert_eq!(survivors(&file), ["UP"]);
    }

    #[test]
    fn a_name_nothing_declares_is_an_error_rather_than_a_silent_zero() {
        let (file, codes) = run("#if NOTHING_DECLARES_THIS\nconst TAKEN = 1\n#endif\n");
        assert_eq!(codes, ["K816-E0101"]);
        // The branch is not taken, so recovery does not compile a region
        // whose condition nobody could evaluate.
        assert_eq!(survivors(&file), Vec::<String>::new());
    }

    #[test]
    fn a_condition_that_is_not_a_number_says_so() {
        let (_, codes) = run("#if [ \"text\" ]\nconst TAKEN = 1\n#endif\n");
        assert_eq!(codes, ["K816-E0148"]);
    }

    /// A `var` and a data label have no compile-time value, and the pass has
    /// to know they exist in order to say so in the words analysis would
    /// rather than calling them undeclared.
    #[test]
    fn a_condition_naming_something_with_no_value_says_which_kind() {
        let (_, codes) = run("var slot = $0400\n#if slot\nconst A = 1\n#endif\n");
        assert_eq!(codes, ["K816-E0148"]);
        let (_, codes) = run("data tab { 1 2 }\n#if tab\nconst A = 1\n#endif\n");
        assert_eq!(codes, ["K816-E0148"]);
        let (_, codes) = run("func f @a8 @i8 {\nreturn\n}\n#if f\nconst A = 1\n#endif\n");
        assert_eq!(codes, ["K816-E0148"]);
    }

    #[test]
    fn the_four_unbalanced_shapes_each_report_once() {
        for source in [
            "#endif\n",
            "#else\n",
            "#elif 1\n",
            "#if 1\nconst A = 1\n",
            "#if 1\n#else\n#else\n#endif\n",
            "#if 1\n#else\n#elif 1\n#endif\n",
        ] {
            let (_, codes) = run(source);
            assert_eq!(codes, ["K816-E0152"], "{source:?}");
        }
    }

    /// A region that crosses a `}` is two reports, one per level, because
    /// liveness is decided per element list and an element list stops at its
    /// closing brace.
    #[test]
    fn a_region_that_crosses_a_block_boundary_is_reported_from_both_sides() {
        let (_, codes) = run("func main @a8 @i8 {\n#if 1\nreturn\n}\n#endif\n");
        assert_eq!(codes, ["K816-E0152", "K816-E0152"]);
    }

    #[test]
    fn a_message_directive_carries_its_message_and_a_dead_one_says_nothing() {
        let (_, codes) = run("#error \"boom\"\n#warn \"careful\"\n");
        assert_eq!(codes, ["K816-E0150", "K816-W0151"]);
        let (_, codes) = run("#if 0\n#error \"boom\"\n#warn \"careful\"\n#endif\n");
        assert_eq!(codes, Vec::<String>::new());
    }

    /// Every element list the AST has, in one file, each with a region in it.
    ///
    /// The claim `preprocess.rs` makes is that *nothing* downstream sees a
    /// directive, and a walk over an AST is a claim that it visits everything
    /// with nothing checking the claim — the shape three of sub-plan G4's
    /// eight review findings shared. So this asserts the claim directly:
    /// after the pass, the exhaustive "does this subtree contain a directive"
    /// walk answers no, anywhere in the tree.
    #[test]
    fn no_directive_survives_anywhere_in_the_tree() {
        let (file, codes) = run(EVERY_LEVEL);
        assert_eq!(codes, Vec::<String>::new());
        assert!(
            !file.items.iter().any(item_has_directive),
            "a directive survived the pass"
        );
        // The same file with every condition false. Counting what is left is
        // what turns "no directive survived" into "every region was
        // *decided*": a level the walk never reached would keep its `nop`.
        let (dead, codes) = run(&EVERY_LEVEL.replace("#if 1", "#if 0"));
        assert_eq!(codes, Vec::<String>::new());
        // The blocks themselves stay — the regions were *inside* them — so
        // what has to be gone is every instruction and every value.
        assert_eq!(surviving_instructions(&file), 7);
        assert_eq!(surviving_instructions(&dead), 0);
        assert_eq!(surviving_data_values(&file), 3);
        assert_eq!(surviving_data_values(&dead), 0);
        assert!(survivors(&dead).iter().all(|name| name != "slot"));
    }

    /// Every element list the AST has, in one file, each with a region in it.
    const EVERY_LEVEL: &str = "#if 1\n\
             var slot = $0400\n\
             #endif\n\
             data table {\n\
             \x20 #if 1\n\
             \x20 1\n\
             \x20 #endif\n\
             \x20 repeat 2 {\n\
             \x20   #if 1\n\
             \x20   2\n\
             \x20   #endif\n\
             \x20 }\n\
             \x20 code {\n\
             \x20   #if 1\n\
             \x20   nop\n\
             \x20   #endif\n\
             \x20 }\n\
             }\n\
             func main @a8 @i8 {\n\
             \x20 #if 1\n\
             \x20 nop\n\
             \x20 #endif\n\
             \x20 @a16 {\n\
             \x20   #if 1\n\
             \x20   nop\n\
             \x20   #endif\n\
             \x20 }\n\
             \x20 nocross {\n\
             \x20   #if 1\n\
             \x20   nop\n\
             \x20   #endif\n\
             \x20 }\n\
             \x20 { #if 1\n\
             \x20   nop\n\
             \x20   #endif\n\
             \x20 } never\n\
             \x20 >={\n\
             \x20   #if 1\n\
             \x20   nop\n\
             \x20   #endif\n\
             \x20 } else {\n\
             \x20   #if 1\n\
             \x20   nop\n\
             \x20   #endif\n\
             \x20 }\n\
             \x20 data {\n\
             \x20   #if 1\n\
             \x20   3\n\
             \x20   #endif\n\
             \x20 }\n\
             \x20 return\n\
             }\n";

    /// How many native instructions the whole file has left, at every depth.
    fn surviving_instructions(file: &AstFile) -> usize {
        fn block(block: &Block) -> usize {
            block.statements.iter().map(one).sum()
        }
        fn one(statement: &Statement) -> usize {
            usize::from(matches!(statement.kind, StatementKind::Native(_)))
                + match &statement.kind {
                    StatementKind::WidthBlock(width) => block(&width.body),
                    StatementKind::Nocross(body) => block(body),
                    StatementKind::Loop(loop_statement) => block(&loop_statement.body),
                    StatementKind::Conditional(conditional) => {
                        block(&conditional.body) + conditional.otherwise.as_ref().map_or(0, block)
                    }
                    StatementKind::ConditionalJump(jump) => one(&jump.jump),
                    StatementKind::Data(data) => data_instructions(data),
                    _ => 0,
                }
        }
        file.items
            .iter()
            .map(|item| match &item.kind {
                ItemKind::Func(func) => block(&func.body),
                ItemKind::Data(data) => data_instructions(data),
                _ => 0,
            })
            .sum()
    }

    fn data_instructions(data: &DataBlock) -> usize {
        data.entries
            .iter()
            .map(|entry| match &entry.kind {
                DataEntryKind::Code { body, .. } => body
                    .statements
                    .iter()
                    .filter(|statement| matches!(statement.kind, StatementKind::Native(_)))
                    .count(),
                DataEntryKind::Repeat { body, .. } => data_instructions(body),
                _ => 0,
            })
            .sum()
    }

    /// How many literal values the file's data blocks have left.
    fn surviving_data_values(file: &AstFile) -> usize {
        fn count(data: &DataBlock) -> usize {
            data.entries
                .iter()
                .map(|entry| match &entry.kind {
                    DataEntryKind::Value(_) => 1,
                    DataEntryKind::Repeat { body, .. } => count(body),
                    _ => 0,
                })
                .sum()
        }
        file.items
            .iter()
            .map(|item| match &item.kind {
                ItemKind::Data(data) => count(data),
                ItemKind::Func(func) => func
                    .body
                    .statements
                    .iter()
                    .map(|statement| match &statement.kind {
                        StatementKind::Data(data) => count(data),
                        _ => 0,
                    })
                    .sum(),
                _ => 0,
            })
            .sum()
    }

    /// Statement identities key every per-statement fact analysis and lowering
    /// share, so the survivors are renumbered over the program that exists
    /// rather than left sparse where a region was removed.
    #[test]
    fn the_surviving_statements_are_renumbered_densely() {
        let (file, _) = run("func main @a8 @i8 {\n#if 0\nnop\nnop\n#endif\nnop\nnop\nreturn\n}\n");
        let ItemKind::Func(func) = &file.items[0].kind else {
            panic!("expected a function");
        };
        let ids: Vec<u32> = func
            .body
            .statements
            .iter()
            .map(|statement| statement.id.index())
            .collect();
        assert_eq!(ids, [0, 1, 2]);
    }
}
