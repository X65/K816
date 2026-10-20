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
//! with a throwaway [`SymbolTable`] — in which each `var` has the shape
//! analysis will give it, computed by the same [`layout::var_shape`], so
//! that `T:sizeof` is the layout's size here too — and runs each evaluator
//! bracket through a **silent** interpreter whose diagnostics, `print` notes
//! and step budget are discarded. That evaluation exists only to decide
//! conditions; `semantic.rs` runs every bracket again, for real, and it is
//! that second run which reports. So a bracket in a program that uses the
//! preprocessor is evaluated twice — and a program that contains no
//! directive at all skips this pass entirely ([`apply`] returns the file it
//! was given), which is every source in the corpus but two.
//!
//! That environment is built *as the walk passes each declaration*, so it is
//! not quite the one analysis builds: analysis declares every function and
//! data label up front, which is what makes a forward reference work, and
//! here a name is only visible below its declaration. A condition needs a
//! number, and none of the forward-declarable kinds is one, so the difference
//! shows up as a diagnostic that says "no compile-time value" rather than
//! "a data label" — never as a branch decided the wrong way.
//!
//! What it must never be is *different*: a name folded here to one number
//! and by analysis to another is a unit built from one value and exporting
//! the other. Every value this pass folds is handed on ([`Applied::folded`])
//! and analysis refuses a unit where the two disagree ([`disagreements`]).
//! A name this pass cannot fold truthfully — a `const` or a layout folded
//! from a section's bytes — it leaves without a value, so a reader of it is
//! refused and told why (`SymbolTable::unanswered`).
//!
//! What the units this one imports make available is available here too,
//! seeded by the same [`crate::linkage::Linkage`] calls analysis makes: an
//! `import` makes available what typing the imported declarations would.
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
use crate::layout;
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
///
/// `workspace` is the unit's place in a program, when there is one: what the
/// units it imports make available at compile time is available to a
/// condition, an `unroll` count and a bracket here, exactly as it is to
/// analysis — an `import` makes available what typing the imported
/// declarations here would.
pub fn apply<'a>(
    file: &'a AstFile,
    workspace: Option<(&crate::linkage::Linkage, crate::source::FileId)>,
) -> Applied<'a> {
    if !file.items.iter().any(item_has_directive) {
        return Applied {
            program: Cow::Borrowed(file),
            diagnostics: Vec::new(),
            effects: Vec::new(),
            folded: Vec::new(),
        };
    }
    let mut pass = Preprocess {
        sections: eval::SectionTable::before_analysis(),
        ..Preprocess::default()
    };
    if let Some((linkage, unit)) = workspace {
        linkage.seed_sections(unit, &mut pass.sections);
        linkage.seed_sections(unit, pass.outcome.sections_mut());
        linkage.seed_functions(unit, &mut pass.functions);
        linkage.seed_symbols(unit, &mut pass.symbols);
    }
    let items = pass.items(&file.items, end_of(&file.items));
    let mut program = AstFile { items };
    // The statement identities the builder assigned are keyed by source
    // order over the *parsed* file. Removing statements would leave them
    // sparse, and every stage after this one keys per-statement facts by
    // them, so the surviving statements are renumbered over the program that
    // actually exists.
    crate::parser::assign_statement_ids(&mut program);
    Applied {
        program: Cow::Owned(program),
        diagnostics: pass.diagnostics,
        effects: pass.effects,
        folded: pass.folded,
    }
}

/// What [`apply`] leaves for the rest of the compiler.
pub struct Applied<'a> {
    /// The program that remains. Borrowed, and identical to what was passed
    /// in, when the file holds no directive at all.
    pub program: Cow<'a, AstFile>,
    pub diagnostics: Vec<Diagnostic>,
    /// The names live `#if` conditions assigned, in source order.
    pub effects: Vec<Effect>,
    /// Every name this pass gave a compile-time value, with that value.
    ///
    /// The directives it decided and the `unroll`s it expanded read these,
    /// and nothing after this pass reads them again, so analysis checks that
    /// it folds each to the same number: a name with two values in one unit
    /// is a miscompile, and this is where one would be caught.
    pub folded: Vec<Folded>,
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
        | ItemKind::Import(_)
        | ItemKind::Extern(_)
        | ItemKind::Error => false,
    }
}

/// Whether a loop test's later slots hold a directive.
fn chain_has_directive(condition: &crate::ast::LoopCondition) -> bool {
    match condition {
        crate::ast::LoopCondition::Branch(chain) => chain.slots().any(block_has_directive),
        _ => false,
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
        // Not a directive, but expanded by this pass all the same.
        StatementKind::Unroll(_) => true,
        StatementKind::Loop(statement) => {
            block_has_directive(&statement.body) || chain_has_directive(&statement.condition)
        }
        StatementKind::Conditional(conditional) => {
            block_has_directive(&conditional.body)
                || conditional
                    .otherwise
                    .as_ref()
                    .is_some_and(block_has_directive)
                || conditional.condition.slots().any(block_has_directive)
        }
        StatementKind::ConditionalJump(jump) => {
            statement_has_directive(&jump.jump) || jump.condition.slots().any(block_has_directive)
        }
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
        | StatementKind::Continue
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
        DataEntryKind::NocrossGroup(entries) => entries.iter().any(|entry| {
            data_has_directive(&DataBlock {
                name: None,
                nocross: false,
                entries: vec![entry.clone()],
            })
        }),
        DataEntryKind::Evaluator(_)
        | DataEntryKind::Segment(_)
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
    /// namespace a condition reads — and the layouts and pinned addresses of
    /// the units this one imports, as foreign facts. Built here rather than
    /// borrowed because the real one does not exist until liveness is
    /// already decided.
    symbols: SymbolTable,
    /// Empty of this unit's sections, permanently, and it says so: a
    /// section's bytes are the product of resolving it, and this pass runs
    /// before any section is resolved — because a `data` block inside a
    /// region that is not taken must not be. A condition that reads one is
    /// refused here rather than answered wrong. What the units this one
    /// imports assembled is here, with their `const` values, because those
    /// were resolved where they were written.
    sections: eval::SectionTable,
    /// Where in the surviving item list a condition's effects belong, so
    /// that a name one assigns becomes visible to analysis at the point the
    /// source assigns it rather than at the top of the file.
    ///
    /// Exact for a top-level condition. For one nested inside an item it
    /// names that item, which is earlier than the truth by at most one item —
    /// and reading such a name *above* the directive that assigns it aborts
    /// the original outright, so there is no compatible program the
    /// difference can reach.
    anchor: usize,
    /// What live conditions assigned, for analysis to install. See
    /// [`Preprocess::condition`].
    effects: Vec<Effect>,
    /// What the evaluation that just finished produced, for its caller to
    /// keep or drop. See [`Preprocess::evaluate`].
    notes: Vec<eval::Note>,
    assigned: Vec<(String, eval::Value)>,
    /// What each bracket this pass evaluated produced, keyed by its span.
    ///
    /// It exists because folding a `const` needs a [`Resolver`] and a
    /// resolver looks a bracket's value up here — which is what makes
    /// `const N = [2 * 3]` fold, and therefore what makes `#if N` below it
    /// decide anything. Analysis keeps its own, from its own run.
    outcome: eval::Outcome,
    /// See [`Applied::folded`].
    folded: Vec<Folded>,
}

impl Preprocess {
    // -----------------------------------------------------------------
    // The three levels
    // -----------------------------------------------------------------

    fn items(&mut self, items: &[Item], end: Option<Span>) -> Vec<Item> {
        let mut stack = Vec::new();
        let mut kept = Vec::new();
        for item in items {
            // Where a name assigned by a condition inside this item becomes
            // visible to analysis. See `Preprocess::anchor`.
            self.anchor = kept.len();
            if let ItemKind::Preprocessor(directive) = &item.kind {
                self.directive(&mut stack, directive, item.span);
                continue;
            }
            if !live(&stack) {
                continue;
            }
            kept.push(self.item(item));
        }
        self.anchor = kept.len();
        self.close(stack, end);
        kept
    }

    /// Walks one live item: declares what it declares, evaluates what it
    /// evaluates, and rewrites the levels nested inside it — **in source
    /// order**, which is the whole contract.
    ///
    /// Nothing here may run out of order with the levels below it. A data
    /// block's `[ FLAG = 1 ]` has to run before the `#if FLAG` three lines
    /// under it, and the two live at different depths of this walk, so the
    /// walk is one walk: an element's own brackets run as the element is
    /// reached, and a nested element list is entered at the point it appears.
    fn item(&mut self, item: &Item) -> Item {
        let span = item.span;
        let kind = match &item.kind {
            ItemKind::Func(func) => {
                self.declare_section(&func.name, func.kind, func.is_far);
                let mut func = func.clone();
                func.body = self.block(&func.body);
                ItemKind::Func(func)
            }
            ItemKind::Data(data) => ItemKind::Data(self.data(data, closing(span))),
            // A `const` group is a sequence of declarations and a later entry
            // may read an earlier one, so each entry's brackets run
            // immediately before that entry folds — the order
            // `semantic::declare_const_group` keeps for the same reason.
            ItemKind::Const(declaration) => {
                self.consts(declaration);
                item.kind.clone()
            }
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
                item.kind.clone()
            }
            // A `var` has no compile-time value, but its layout does: a
            // condition reading `T:sizeof` reads a number. Its brackets run
            // first, as analysis runs them, because a field count may be one.
            ItemKind::Var(var) => {
                self.brackets_of_item(item);
                self.declare_var(var);
                item.kind.clone()
            }
            // An `extern const` is declared, for the reason a `var` is
            // above: without the name in the table a condition reading it is
            // "undeclared", which is wrong and less useful than "resolved by
            // ld65". The other extern forms have no compile-time value
            // either, and the table already refuses a `var` and a section by
            // kind, so declaring them would change nothing.
            ItemKind::Extern(declaration) => {
                if let crate::ast::ExternKind::Const(constant) = &declaration.kind {
                    self.declare(
                        &constant.name.text,
                        // No size: a condition reads values, never sizes.
                        SymbolKind::Const {
                            value: None,
                            size: None,
                        },
                        constant.name.span,
                    );
                }
                self.brackets_of_item(item);
                item.kind.clone()
            }
            ItemKind::Segment(_)
            | ItemKind::EvalBlock(_)
            | ItemKind::Preprocessor(_)
            | ItemKind::Binary(_)
            | ItemKind::Reserved(_)
            | ItemKind::ModuleWidth(_)
            | ItemKind::Import(_)
            | ItemKind::Error => {
                self.brackets_of_item(item);
                item.kind.clone()
            }
        };
        Item { kind, span }
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
            let mut kept_statement = statement.clone();
            kept_statement.kind = self.statement_kind(statement);
            kept.push(kept_statement);
        }
        self.close(stack, Some(Span::empty_at(block.span.file, block.span.end)));
        Block {
            statements: kept,
            span: block.span,
        }
    }

    fn data(&mut self, data: &DataBlock, end: Option<Span>) -> DataBlock {
        // The block's own name first, so a bracket inside it may read the
        // section it is in. Labels are declared as the walk reaches them,
        // which is what keeps one inside a region that is not taken from
        // existing.
        if let Some(name) = &data.name {
            self.declare_data_label(name);
        }
        DataBlock {
            name: data.name.clone(),
            nocross: data.nocross,
            entries: self.data_entries(&data.entries, end),
        }
    }

    /// One run of data entries — a section's body, or a `nocross { … }`
    /// group's — with its own directive stack, because a region opened inside
    /// a group has to close inside it.
    fn data_entries(&mut self, entries: &[DataEntry], end: Option<Span>) -> Vec<DataEntry> {
        let mut stack = Vec::new();
        let mut kept = Vec::new();
        for entry in entries {
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
                DataEntryKind::Repeat { count, body } => {
                    // The count is the entry's own expression and is written
                    // before the block, so it runs before the block — an arm
                    // that descends without running what sits above the
                    // descent drops those brackets from the environment every
                    // directive below is decided against.
                    let mut sites = Vec::new();
                    eval::sites_of_expr(count, &mut sites);
                    self.run(sites);
                    DataEntryKind::Repeat {
                        count: count.clone(),
                        body: self
                            .data(body, Some(Span::empty_at(entry.span.file, entry.span.end))),
                    }
                }
                // Exhaustive on purpose. A construct added later that carries
                // an element list has to be listed above or this match stops
                // compiling; a `_` arm would clone it past the filter and
                // leave a directive inside it alive, which is the failure
                // `no_directive_survives_anywhere_in_the_tree` exists to catch
                // and cannot catch for a shape nobody wrote a case for.
                DataEntryKind::Label(name) => {
                    self.declare_data_label(name);
                    entry.kind.clone()
                }
                // The group's entries are the section's, one brace deeper:
                // a directive inside it pairs inside it, and a bracket inside
                // it runs where it stands.
                DataEntryKind::NocrossGroup(entries) => {
                    DataEntryKind::NocrossGroup(self.data_entries(
                        entries,
                        Some(Span::empty_at(entry.span.file, entry.span.end)),
                    ))
                }
                DataEntryKind::Segment(_)
                | DataEntryKind::Align(_)
                | DataEntryKind::Address(_)
                | DataEntryKind::Evaluator(_)
                | DataEntryKind::Nocross
                | DataEntryKind::Charset(_)
                | DataEntryKind::Typed { .. }
                | DataEntryKind::Value(_)
                | DataEntryKind::ForEval { .. }
                | DataEntryKind::Preprocessor(_)
                | DataEntryKind::Binary(_)
                | DataEntryKind::Reserved(_)
                | DataEntryKind::Error => {
                    self.brackets_of_entry(entry);
                    entry.kind.clone()
                }
            };
            kept.push(DataEntry {
                kind,
                span: entry.span,
            });
        }
        self.close(stack, end);
        kept
    }

    /// Walks one live statement, in source order, for the reason
    /// [`Preprocess::item`] gives.
    fn statement_kind(&mut self, statement: &Statement) -> StatementKind {
        let span = statement.span;
        match &statement.kind {
            StatementKind::WidthBlock(width) => {
                let mut width = width.clone();
                width.body = self.block(&width.body);
                StatementKind::WidthBlock(width)
            }
            StatementKind::Nocross(body) => StatementKind::Nocross(self.block(body)),
            StatementKind::Unroll(unroll) => StatementKind::Unroll(self.unroll(unroll)),
            StatementKind::Loop(statement) => {
                let mut statement = statement.clone();
                statement.body = self.block(&statement.body);
                if let crate::ast::LoopCondition::Branch(chain) = &mut statement.condition {
                    for slot in chain.slots_mut() {
                        *slot = self.block(slot);
                    }
                }
                StatementKind::Loop(statement)
            }
            StatementKind::Conditional(conditional) => {
                let mut conditional = conditional.clone();
                for slot in conditional.condition.slots_mut() {
                    *slot = self.block(slot);
                }
                conditional.body = self.block(&conditional.body);
                conditional.otherwise = conditional
                    .otherwise
                    .as_ref()
                    .map(|otherwise| self.block(otherwise));
                StatementKind::Conditional(conditional)
            }
            StatementKind::ConditionalJump(jump) => {
                let mut jump = jump.clone();
                for slot in jump.condition.slots_mut() {
                    *slot = self.block(slot);
                }
                jump.jump.kind = self.statement_kind(&jump.jump.clone());
                StatementKind::ConditionalJump(jump)
            }
            StatementKind::Data(data) => StatementKind::Data(self.data(data, closing(span))),
            // Exhaustive for the reason `data`'s tail gives. Every arm below
            // carries no element list of its own, so walking its sites here
            // cannot reach past this statement.
            StatementKind::Label(_)
            | StatementKind::Native(_)
            | StatementKind::Hla(_)
            | StatementKind::ModeSet(_)
            | StatementKind::Call(_)
            | StatementKind::BareCall(_)
            | StatementKind::Goto(_)
            | StatementKind::Break
            | StatementKind::Repeat
            | StatementKind::Continue
            | StatementKind::Preprocessor(_)
            | StatementKind::Return(_)
            | StatementKind::Align(_)
            | StatementKind::Address(_)
            | StatementKind::Reserved(_)
            | StatementKind::Error => {
                self.brackets_of_statement(statement);
                statement.kind.clone()
            }
        }
    }

    /// `unroll N { … }`: the block, its own directives decided and any
    /// `unroll` inside it expanded first, written out N times
    /// (`plans/18-conditions.md` decision 9). The count is folded here
    /// because nothing after this pass sees the single copy; it reads what a
    /// condition reads — a number, a `const` above it, a bracket.
    ///
    /// A count that does not fold, or folds below one, leaves the one copy
    /// so the body is still analyzed; the program is refused either way.
    fn unroll(&mut self, unroll: &crate::ast::Unroll) -> crate::ast::Unroll {
        let mut sites = Vec::new();
        eval::sites_of_expr(&unroll.count, &mut sites);
        self.run(sites);
        let mut resolver = Resolver::new(&self.symbols, &self.outcome);
        let count = resolver.constant(&unroll.count, "`unroll` counts");
        self.diagnostics.extend(resolver.take_diagnostics());
        let body = self.block(&unroll.body);
        let copies = match count {
            Some(0) => {
                self.diagnostics.push(
                    Diagnostic::error(
                        DiagnosticCode::UNROLL_COUNT,
                        "an `unroll` writes its block at least once",
                        Label::new(unroll.count.span, "this count is 0"),
                    )
                    .with_help("leave the block out with `#if` instead")
                    .with_docs("doc/syntax-reference.md#unroll"),
                );
                1
            }
            // More copies than a bank has bytes cannot be placed, and a
            // negative count folds to one of these.
            Some(count) if count > 0x1_0000 => {
                self.diagnostics.push(
                    Diagnostic::error(
                        DiagnosticCode::UNROLL_COUNT,
                        "an `unroll` writes more copies than a bank holds bytes",
                        Label::new(unroll.count.span, format!("this count folds to ${count:X}")),
                    )
                    .with_note("a count above $10000 cannot fit in a 64 KiB bank; a negative count folds to one")
                    .with_help("use a loop")
                    .with_docs("doc/syntax-reference.md#unroll"),
                );
                1
            }
            Some(count) => count,
            None => 1,
        };
        let mut labels = Vec::new();
        crate::ast::visit_statements(&body, &mut |statement| match &statement.kind {
            StatementKind::Label(label) => labels.push(label.name.span),
            StatementKind::Data(data) => {
                labels.extend(data.name.iter().map(|name| name.span));
                for entry in crate::ast::flat_data_entries(&data.entries) {
                    if let DataEntryKind::Label(name) = &entry.kind {
                        labels.push(name.span);
                    }
                }
            }
            _ => {}
        });
        // One copy, so the refusal is not followed by every copy's
        // duplicate definition.
        let copies = if labels.is_empty() { copies } else { 1 };
        for label in labels {
            self.diagnostics.push(
                Diagnostic::error(
                    DiagnosticCode::UNROLL_LABEL,
                    "a label inside an `unroll` would be defined once per copy",
                    Label::new(label, "this label"),
                )
                .with_secondary(Label::new(unroll.keyword, "the block written out here"))
                .with_note(
                    "a jump out of the block, and `break`, `repeat` and `continue` to a loop \
                     around it, work in every copy; a branch inside it is spelled with `if` or \
                     a loop, whose labels are each copy's own",
                )
                .with_docs("doc/syntax-reference.md#unroll"),
            );
        }
        let statements = (0..copies)
            .flat_map(|_| body.statements.iter().cloned())
            .collect();
        crate::ast::Unroll {
            keyword: unroll.keyword,
            count: unroll.count.clone(),
            body: Block {
                statements,
                span: body.span,
            },
            copies,
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
                // The parser rejected the message — it was on the next line,
                // or recovery lost it — and that complaint already stands.
                let Some(message) = directive.message.clone() else {
                    return;
                };
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
    // The namespace a condition reads
    // -----------------------------------------------------------------

    /// Declares `name` as analysis would, and says whether it did.
    ///
    /// A register, flag or instruction spelling is refused there
    /// ([`crate::compat::reserved_name`]) and an evaluator name taking one
    /// stays a local of its bracket, so it is refused here too: declaring
    /// it would give a condition or an `unroll` count a name analysis does
    /// not have.
    fn declare(&mut self, name: &str, kind: SymbolKind, span: Span) -> bool {
        crate::compat::reserved_name_refusal(name).is_none()
            && self.symbols.declare(name, kind, span).is_ok()
    }

    /// Declares a code section, which has an address and no value: the pass
    /// has to know the name exists in order to say which kind it is.
    fn declare_section(
        &mut self,
        name: &crate::ast::Name,
        kind: crate::ast::FunctionKind,
        far: bool,
    ) {
        if name.provenance == Provenance::Synthetic {
            return;
        }
        let kind = match kind {
            crate::ast::FunctionKind::Inline => SymbolKind::Inline,
            _ => SymbolKind::function(far),
        };
        self.declare(&name.text, kind, name.span);
    }

    /// Declares a `var` or an `abstract var` with the shape analysis will
    /// give it.
    ///
    /// Its address is the linker's, so it has no compile-time value, and
    /// declaring it is what makes the pass say so rather than "undeclared".
    /// Its *shape* is the source's, and `T:sizeof` and `T.f:offsetof` read
    /// it, so the shape is [`layout::var_shape`] — the computation analysis
    /// runs, and not a placeholder: a placeholder answers `:sizeof` with
    /// its own size, which is a wrong number and not a missing one, and
    /// nothing downstream re-checks an `#if` this pass decided or an
    /// `unroll` it expanded.
    ///
    /// A shape that reads something this pass cannot fold — a section's
    /// bytes, which are assembled after it — leaves the name without a
    /// value, so a condition reading it is refused rather than given the
    /// size of a guess.
    fn declare_var(&mut self, var: &crate::ast::VarDeclaration) {
        if var.name.provenance == Provenance::Synthetic {
            return;
        }
        let mut shaping = Shaping {
            symbols: &mut self.symbols,
            outcome: &self.outcome,
            exact: true,
        };
        let kind = if var.is_abstract {
            let default = var.width.map(|width| layout::data_width(width.kind));
            SymbolKind::AbstractVar {
                layout: layout::build_layout(&mut shaping, &var.fields, default),
            }
        } else {
            let shape = layout::var_shape(&mut shaping, var);
            SymbolKind::Var {
                address: None,
                size: crate::symbol::AddressSize::Absolute,
                width: shape.width,
                layout: shape.layout,
                element: shape.allocation.element,
                repeat: shape.repeat,
            }
        };
        if shaping.exact {
            self.declare(&var.name.text, kind, var.name.span);
        } else {
            self.symbols
                .record_unanswered(&var.name.text, var.name.span);
        }
    }

    /// A data label is the section it names, which is what makes
    /// `index(tab, 2)` mean anything at all.
    fn declare_data_label(&mut self, name: &crate::ast::Name) {
        if name.provenance == Provenance::Synthetic {
            return;
        }
        self.declare(
            &name.text,
            SymbolKind::DataLabel {
                size: crate::symbol::AddressSize::Absolute,
            },
            name.span,
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
    ///
    /// Unlike a bracket, this evaluation is the **only** one: the directive
    /// is gone before analysis begins, so nothing re-runs it. That is why it
    /// keeps what [`Self::evaluate`] discards — the `print` notes it made and
    /// the names it assigned, which [`apply`] hands back for analysis to
    /// install. `#if [ FLAG = 1 ]` assigning a name the code below reads is
    /// measured on the original (`k65/preprocessor-condition-effects`).
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
        let value = self.evaluate(condition);
        // A condition's effects are its own, and nothing will produce them a
        // second time.
        for note in std::mem::take(&mut self.notes) {
            self.diagnostics.push(crate::resolve::eval_note(note));
        }
        let anchor = self.anchor;
        let assigned = std::mem::take(&mut self.assigned);
        self.effects
            .extend(assigned.into_iter().map(|(name, value)| Effect {
                anchor,
                name,
                value,
                span,
            }));
        match value {
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
    /// The `print` notes it made and the names it assigned are left in
    /// [`Preprocess::notes`] and [`Preprocess::assigned`] for the caller,
    /// which is where a bracket and a condition part company. A bracket's are
    /// dropped, because `semantic.rs` runs the same bracket again and that
    /// run is the one that speaks; a condition has no second run, so
    /// [`Self::condition`] keeps both. Either way they belong to the
    /// evaluation that just finished, so they are cleared here rather than
    /// marked and sliced.
    fn evaluate(&mut self, body: &EvalExpr) -> Result<eval::Value, eval::Error> {
        self.notes.clear();
        self.assigned.clear();
        self.block += 1;
        let mut env = std::mem::take(&mut self.env);
        // No section has been resolved where this pass runs, so a condition
        // that reads one — `#if [ size(t) ]` — reports rather than guessing;
        // analysis says the same thing again where it can be believed.
        let mut interpreter = eval::Interpreter::new(
            &mut env,
            &self.functions,
            &self.symbols,
            &self.sections,
            self.block,
        );
        let value = interpreter.evaluate(body);
        self.notes.extend(interpreter.take_notes());
        // A name an evaluation assigned is a compiler constant from there
        // down, and a condition below it may read it by either route.
        for (name, assigned) in env.take_new_names() {
            if let Some(number) = assigned.number() {
                self.declare_value(&name, eval::to_integer(number) as u32, body.span);
            }
            self.assigned.push((name, assigned));
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
    /// Every bracket one item holds, run in the order it is written.
    ///
    /// Only for an item with no element list nested in it: for the two that
    /// have one, the walk enters the level itself so that a directive in it
    /// meets the environment the elements above it left.
    fn brackets_of_item(&mut self, item: &Item) {
        let mut sites = Vec::new();
        eval::sites_of_item(item, &mut sites);
        self.run(sites);
    }

    fn brackets_of_statement(&mut self, statement: &Statement) {
        let mut sites = Vec::new();
        eval::sites_of_statement(statement, &mut sites);
        self.run(sites);
    }

    fn brackets_of_entry(&mut self, entry: &DataEntry) {
        let mut sites = Vec::new();
        eval::sites_of_data_entry(entry, &mut sites);
        self.run(sites);
    }

    fn run(&mut self, sites: Vec<eval::Site<'_>>) {
        for site in sites {
            // A `for … eval` body is not run here for the reason analysis
            // does not run it either: its bounds are K65 expressions nobody
            // has folded yet.
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
            match value {
                Some(value) => self.declare_value(&entry.name.text, value, entry.name.span),
                // Folded from something this pass cannot read — or from a
                // mistake analysis reports where it is written — so a
                // condition reading it says it has no value *here*, not that
                // it is undeclared.
                None => self
                    .symbols
                    .record_unanswered(&entry.name.text, entry.name.span),
            }
        }
    }

    /// Declares a name with a compile-time value, and remembers the value
    /// for analysis to check (`Applied::folded`).
    fn declare_value(&mut self, name: &str, value: u32, span: Span) {
        let declared = self.declare(
            name,
            // No size: a condition reads values, and analysis declares the
            // real one.
            SymbolKind::Const {
                value: Some(value),
                size: None,
            },
            span,
        );
        if declared {
            self.folded.push(Folded {
                name: name.to_owned(),
                value,
                span,
            });
        }
    }
}

/// A name this pass gave a compile-time value, and the value.
#[derive(Clone, Debug)]
pub struct Folded {
    pub name: String,
    pub value: u32,
    pub span: Span,
}

/// Every name the preprocessor folded to another number than analysis did.
///
/// The preprocessor decides `#if`s and expands `unroll`s against values it
/// folds itself, before analysis exists, and nothing re-checks what it
/// decided. A name with two values is therefore code built from one and
/// exported with the other — a miscompile no test of the output would
/// notice. That is a compiler bug and not the author's, so it is internal;
/// it is an error rather than an assertion because emitting the code is the
/// one thing that must not happen.
///
/// A name analysis has no value for is not compared: it refused the
/// declaration and said so, and the program does not compile.
pub fn disagreements(folded: &[Folded], symbols: &SymbolTable) -> Vec<Diagnostic> {
    folded
        .iter()
        .filter_map(|early| {
            let id = symbols.lookup(&early.name)?;
            let SymbolKind::Const {
                value: Some(late), ..
            } = symbols.kind(id)
            else {
                return None;
            };
            (late != early.value).then(|| {
                Diagnostic::error(
                    DiagnosticCode::INTERNAL,
                    format!("internal: `{}` has two values in this unit", early.name),
                    Label::new(
                        early.span,
                        format!("the preprocessor folded this to ${:X}", early.value),
                    ),
                )
                .with_secondary(Label::new(
                    symbols.symbol(id).declaration,
                    format!("analysis folded it to ${late:X}"),
                ))
                .with_note(
                    "an `#if`, an `unroll` count or an evaluator bracket was decided with the \
                     first value, and the code would be built from it",
                )
            })
        })
        .collect()
}

/// A declaration's shape as this pass folds it: against the table built so
/// far, silently, because analysis folds the same expressions again and that
/// run is the one that reports.
struct Shaping<'a> {
    symbols: &'a mut SymbolTable,
    outcome: &'a eval::Outcome,
    /// Cleared by a count or extent that did not fold here. The shape then
    /// differs from the one analysis computes, and the name is left without
    /// a value.
    exact: bool,
}

impl layout::Shaper for Shaping<'_> {
    fn layouts(&self) -> &crate::symbol::Layouts {
        self.symbols.layouts()
    }

    fn intern(&mut self, layout: crate::symbol::Layout) -> crate::symbol::LayoutId {
        self.symbols.add_layout(layout)
    }

    fn constant(&mut self, expr: &crate::ast::Expr, position: &str) -> Option<u32> {
        let mut resolver = Resolver::new(self.symbols, self.outcome);
        let value = resolver.constant(expr, position);
        let _ = resolver.finish();
        self.exact &= value.is_some();
        value
    }

    // Both shapes leave the zero count and the repeated field out, so they
    // still agree; analysis is the one that reports them.
    fn zero_count(&mut self, _: Span) {}

    fn duplicate_field(&mut self, _: &crate::ast::FieldDeclaration, _: Span) {}
}

/// A name a live `#if` condition assigned, and where analysis should install
/// it.
///
/// The preprocessor evaluates a condition and then deletes the directive, so
/// this is the only record that the assignment happened. `semantic.rs`
/// declares the name as a compiler constant and binds it in the evaluator
/// environment before it processes the item at `anchor`.
#[derive(Clone, Debug)]
pub struct Effect {
    pub anchor: usize,
    pub name: String,
    pub value: eval::Value,
    /// The directive whose condition assigned it, for the constant's
    /// declaration site.
    pub span: Span,
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
        let applied = apply(&parsed.file, None);
        let codes = applied
            .diagnostics
            .iter()
            .map(|diagnostic| diagnostic.code.as_str().to_owned())
            .collect();
        (applied.program.into_owned(), codes)
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

    /// The values this pass folded, by name.
    fn folded(source: &str) -> Vec<(String, u32)> {
        let mut map = SourceMap::new();
        let id = map.add_file("pp.kk", source);
        let parsed = parse_source(map.file(id), None);
        assert_eq!(parsed.diagnostics, vec![], "`{source}` did not parse");
        apply(&parsed.file, None)
            .folded
            .into_iter()
            .map(|folded| (folded.name, folded.value))
            .collect()
    }

    /// A layout query folds to the layout's size and offset here, not to the
    /// size of a placeholder `var`, and the names it reaches are recorded for
    /// analysis to check.
    #[test]
    fn a_layout_query_folds_to_the_layout_and_is_recorded() {
        let source = "abstract var T[ .a :word .b :byte [62] ]\n\
                      const TSZ = T:sizeof, OFS = T.b:offsetof\n\
                      [ SH = 0  while TSZ << SH < $100 { SH = SH + 1 } ]\n\
                      #if 1\n#endif\n";
        assert_eq!(
            folded(source),
            [
                ("TSZ".to_owned(), 64),
                ("OFS".to_owned(), 2),
                ("SH".to_owned(), 2)
            ]
        );
    }

    /// A name folded to another number than analysis has is refused, and the
    /// same number, or a name analysis gave no value, is not.
    #[test]
    fn a_name_with_two_values_is_an_internal_error() {
        let mut map = SourceMap::new();
        let file = map.add_file("pp.kk", "");
        let span = Span::empty_at(file, 0);
        let mut symbols = SymbolTable::default();
        for (name, value) in [("SAME", Some(64)), ("OTHER", Some(64)), ("NONE", None)] {
            symbols
                .declare(name, SymbolKind::Const { value, size: None }, span)
                .expect("declares");
        }
        let early = |name: &str, value| Folded {
            name: name.to_owned(),
            value,
            span,
        };
        let found = disagreements(
            &[
                early("SAME", 64),
                early("OTHER", 1),
                early("NONE", 1),
                early("ABSENT", 1),
            ],
            &symbols,
        );
        let titles: Vec<_> = found
            .iter()
            .map(|diagnostic| diagnostic.title.as_str())
            .collect();
        assert_eq!(titles, ["internal: `OTHER` has two values in this unit"]);
        assert_eq!(found[0].code, DiagnosticCode::INTERNAL);
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
        let applied = apply(&parsed.file, None);
        assert!(matches!(applied.program, Cow::Borrowed(_)));
        assert!(applied.diagnostics.is_empty());
        assert!(applied.effects.is_empty());
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

    /// A bracket above a directive runs before it, at every level.
    ///
    /// Liveness and evaluation are one source-order walk or they are wrong.
    /// They were two: the pass used to rewrite a whole item — descending into
    /// its statements and entries, deciding every directive in them — and
    /// only then run the item's brackets, so a `[ FLAG = 1 ]` three lines
    /// above a `#if FLAG` in the same data block had not happened yet.
    /// Measured on the original in `k65/preprocessor-condition-effects`.
    #[test]
    fn a_bracket_above_a_directive_runs_before_it() {
        // Top level.
        let (file, codes) = run("[ FLAG = 1 ]\n#if FLAG\nconst TAKEN = 1\n#endif\n");
        assert_eq!(codes, Vec::<String>::new());
        assert_eq!(survivors(&file), ["TAKEN"]);

        // Data-entry level, where the bracket is an entry of the same block.
        let (file, codes) =
            run("data tab {\n  [ FLAG = 1 ]\n  #if FLAG\n  7\n  #else\n  8\n  #endif\n}\n");
        assert_eq!(codes, Vec::<String>::new());
        assert_eq!(surviving_data_values(&file), 2, "the bracket and the `7`");

        // And one level further in, where the bracket is in an outer block
        // and the directive in a `repeat` nested inside it.
        let (file, codes) = run(
            "data tab {\n  [ FLAG = 1 ]\n  repeat 2 {\n    #if FLAG\n    7\n    #endif\n  }\n}\n",
        );
        assert_eq!(codes, Vec::<String>::new());
        assert_eq!(surviving_data_values(&file), 2);
    }

    /// What a live condition does survives it, because nothing runs it again.
    ///
    /// The directive is deleted before analysis begins, so a name the
    /// condition assigned has to be carried across as an [`Effect`] — and a
    /// `print` it made has to be reported here, since there is no second run
    /// to report it.
    #[test]
    fn a_live_condition_keeps_what_it_assigned_and_what_it_printed() {
        let mut map = SourceMap::new();
        let id = map.add_file(
            "pp.k65",
            "#if [ FLAG = 1 print(\"deciding\") 1 ]\nconst TAKEN = 1\n#endif\n",
        );
        let parsed = parse_source(map.file(id), None);
        assert_eq!(parsed.diagnostics, vec![]);
        let applied = apply(&parsed.file, None);
        let codes: Vec<&str> = applied
            .diagnostics
            .iter()
            .map(|diagnostic| diagnostic.code.as_str())
            .collect();
        assert_eq!(codes, ["K816-N0145"], "the `print` speaks exactly once");
        let names: Vec<&str> = applied
            .effects
            .iter()
            .map(|effect| effect.name.as_str())
            .collect();
        assert_eq!(names, ["FLAG"]);
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
                    StatementKind::Unroll(unroll) => block(&unroll.body),
                    StatementKind::Loop(loop_statement) => {
                        block(&loop_statement.body)
                            + match &loop_statement.condition {
                                crate::ast::LoopCondition::Branch(chain) => {
                                    chain.slots().map(block).sum()
                                }
                                _ => 0,
                            }
                    }
                    StatementKind::Conditional(conditional) => {
                        block(&conditional.body)
                            + conditional.otherwise.as_ref().map_or(0, block)
                            + conditional.condition.slots().map(block).sum::<usize>()
                    }
                    StatementKind::ConditionalJump(jump) => {
                        one(&jump.jump) + jump.condition.slots().map(block).sum::<usize>()
                    }
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

    /// The pass evaluates every bracket the program that survives it holds.
    ///
    /// The walk is now split across `item`, `statement_kind` and `data` —
    /// each arm either descends into a nested element list or runs the
    /// element's own sites — and an arm that does neither drops the brackets
    /// it holds silently. Analysis would still evaluate them, so nothing
    /// visible breaks; what breaks is the *environment* a directive below
    /// them is decided against, which is the whole reason the pass runs an
    /// evaluator at all.
    ///
    /// Checked against `eval::sites_of_item`, which is the same exhaustive
    /// walk analysis uses, so the two cannot drift apart in silence.
    #[test]
    fn every_bracket_of_the_surviving_program_was_evaluated() {
        let source = "#if 1\n\
                      [ TOP = 1 ]\n\
                      const SIZE = [ 2 ]\n\
                      var slot[ [1 + 1] ]\n\
                      data tab {\n\
                      \x20 [ IN_DATA = 3 ]\n\
                      \x20 repeat [ 1 + 1 ] {\n\
                      \x20   [ IN_REPEAT = 4 ]\n\
                      \x20 }\n\
                      \x20 code {\n\
                      \x20   a = #[ 5 ]\n\
                      \x20 }\n\
                      }\n\
                      func main @a8 @i8 {\n\
                      \x20 a = #[ 6 ]\n\
                      \x20 nocross {\n\
                      \x20   a = #[ 7 ]\n\
                      \x20 }\n\
                      \x20 return\n\
                      }\n\
                      #endif\n";
        let mut map = SourceMap::new();
        let id = map.add_file("pp.k65", source);
        let parsed = parse_source(map.file(id), None);
        assert_eq!(parsed.diagnostics, vec![], "the probe did not parse");

        let mut pass = Preprocess {
            sections: eval::SectionTable::before_analysis(),
            ..Preprocess::default()
        };
        let items = pass.items(&parsed.file.items, end_of(&parsed.file.items));
        let mut missed = Vec::new();
        for item in &items {
            let mut sites = Vec::new();
            eval::sites_of_item(item, &mut sites);
            for site in sites {
                if let eval::Site::Bracket(body) = site
                    && pass.outcome.value(body.span).is_none()
                {
                    missed.push(body.span);
                }
            }
        }
        assert!(
            missed.is_empty(),
            "brackets the pass never evaluated: {missed:?}"
        );
        // And the probe is only worth something if it has brackets in it.
        // Eight, not nine: `sites_of_item` deliberately skips a `const`
        // group, whose brackets `Preprocess::consts` interleaves with its
        // declarations instead — which the environment check below covers.
        let total: usize = items
            .iter()
            .map(|item| {
                let mut sites = Vec::new();
                eval::sites_of_item(item, &mut sites);
                sites.len()
            })
            .sum();
        assert_eq!(
            total, 8,
            "the probe no longer covers what it was written for"
        );
        for name in ["TOP", "IN_DATA", "IN_REPEAT"] {
            assert!(pass.env.get(name).is_some(), "`{name}` was never assigned");
        }
        assert!(
            pass.symbols.lookup("SIZE").is_some(),
            "a `const` group's bracket did not fold"
        );
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
