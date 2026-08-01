/*****************************************************************************/
/*                                                                           */
/*                                 width.rs                                  */
/*                                                                           */
/*        The register-width dataflow shared by analysis and lowering        */
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

//! One width dataflow, consumed by analysis and by lowering.
//!
//! `doc/register-width-aware-syntax.md` is the authority. The model answers
//! one question — *what are the register widths here* — and answers it once:
//! `semantic.rs` validates immediates against the answer and `lower.rs` emits
//! `REP`/`SEP` and [`crate::op::KOpKind::ModeAssumption`] from the same one.
//! Two independent width answers is the failure mode this module exists to
//! prevent, and it is not hypothetical: before it, `semantic.rs` validated a
//! `lda #$1234` against the *function's entry contract* while `emit_ca65.rs`
//! sized it against a `rep #$20` two lines above, so the same program was
//! both rejected and correct.
//!
//! The solver is a worklist dataflow over the function's control-flow graph,
//! not a single traversal. A backward branch is an edge from later in the
//! stream to earlier, so a loop entered at `@a8` and closed from `@a16` must
//! converge to unknown at its head, and only iteration finds that. A forward
//! walk also claims a width at a label a branch skipped over — which is how
//! a two-byte immediate becomes a three-byte one and every following byte
//! shifts.
//!
//! Emulation mode is tracked as a third axis: `doc/w65c816s.md` says "the M
//! and X flags are always equal to one in Emulation mode", so after an `XCE`
//! whose result is unknown a `rep #$20` establishes nothing. A narrowing
//! `SEP` is still believed — 8-bit is the answer in both modes — and only the
//! widening direction is refused.

use std::collections::{BTreeMap, BTreeSet, HashMap};

use crate::ast::{
    self, Block, DataEntryKind, LoopCondition, Statement, StatementKind, StmtId, WidthAnnotation,
    WidthAnnotationKind,
};
use crate::diagnostic::{Applicability, Diagnostic, DiagnosticCode, Fix, Label, TextEdit};
use crate::effects::{ControlEffect, WidthEffect};
use crate::isa::{ImmediateWidthClass, Mnemonic, RegisterWidth};
use crate::op::{
    AxisExit, AxisState, EmulationState, ExitContract, ModeState, Operand, WidthContract,
};
use crate::resolve::Resolver;
use crate::semantic::WidthNeeds;
use crate::source::Span;
use crate::symbol::{LabelScope, SymbolId, SymbolKind, SymbolTable};
use crate::value::KExpr;

/// The `SEP`/`REP` mask selecting the accumulator width flag (M).
pub const P_MASK_M: u32 = 0x20;
/// The `SEP`/`REP` mask selecting the index register width flag (X).
pub const P_MASK_X: u32 = 0x10;

/// What the solver concluded about one function.
///
/// Keyed by [`StmtId`] rather than by position: analysis runs before lowering
/// and the two walk the AST independently, so a result keyed by position
/// would be re-derived by each of them — the two-answers failure in a new
/// costume.
#[derive(Clone, Debug, Default)]
pub struct WidthAnalysis {
    /// The state on entry: module defaults merged with the declared contract.
    pub entry: ModeState,
    /// The state *before* each statement.
    before: BTreeMap<StmtId, ModeState>,
    /// The state *after* each statement, which is what a call's callee exit
    /// and a label's bridge have already been applied to.
    after: BTreeMap<StmtId, ModeState>,
    /// The mode operations the model itself contributes, per statement.
    bridges: BTreeMap<StmtId, Vec<Bridge>>,
    /// What callers may believe about their widths afterwards.
    pub exit: ExitContract,
    /// True when `exit` fell back to the language's `Unknown ⇒ Preserve`
    /// rule rather than being derived from the body.
    pub exit_is_assumed: bool,
    pub needs: WidthNeeds,
    pub callees: Vec<SymbolId>,
    /// Every reachable point where control leaves the function.
    ///
    /// The solver knows where they are and what the widths are there;
    /// *classifying* them into an [`ExitContract`] is a separate step, so
    /// that the one run of the solver whose states lowering reads is also
    /// the run the classification was made from.
    pub returns: Vec<ReturnSite>,
}

/// One reachable point where control leaves the function: an `rts`/`rtl`, a
/// `return`, or falling off the end of the body.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ReturnSite {
    pub state: ModeState,
    /// Whether each axis may differ from its value at function entry, which
    /// is what separates `Preserve` from `Unknown`.
    pub touched: Touched,
    pub span: Span,
}

impl WidthAnalysis {
    /// The width state before `statement`.
    ///
    /// `None` means this analysis does not describe that statement, which is
    /// a compiler bug rather than a statement whose state happens to be
    /// missing: silently substituting the entry contract here is exactly the
    /// mistake the keying exists to prevent.
    pub fn before(&self, statement: StmtId) -> Option<ModeState> {
        self.before.get(&statement).copied()
    }

    /// The width state after `statement`.
    pub fn after(&self, statement: StmtId) -> Option<ModeState> {
        self.after.get(&statement).copied()
    }

    /// The mode operations to emit around `statement`.
    pub fn bridges(&self, statement: StmtId) -> &[Bridge] {
        self.bridges.get(&statement).map_or(&[], Vec::as_slice)
    }
}

/// One `REP`/`SEP` the width model itself contributes.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Bridge {
    /// The width being established: 8-bit emits `SEP`, 16-bit emits `REP`.
    pub width: RegisterWidth,
    /// The `P` mask, one flag per bridge. Folding masks together is the
    /// optimizer's job, so the model emits the unfolded stream.
    pub mask: u32,
    pub anchor: BridgeAnchor,
    pub reason: BridgeReason,
}

/// Where a bridge sits relative to the statement it is keyed on.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BridgeAnchor {
    /// Immediately before the statement's own operations.
    Before,
    /// Immediately after the label operation, so that arrivals by branch get
    /// it too (`doc/register-width-aware-syntax.md`, "Label-site bridging").
    /// The adjacency is load-bearing: it is how the optimizer recognizes a
    /// fixed label bridge it must not eliminate.
    AfterLabel,
    /// At the end of the statement's own operations — a width block's
    /// restore.
    After,
}

/// Why the model emitted a bridge, for the annotation on the emitted line.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BridgeReason {
    /// `main` has no caller, so its contract is established at entry.
    EntryPoint,
    /// A bare `@a16` statement.
    ModeSet,
    /// Entering a scoped width block.
    BlockEnter,
    /// Restoring the width a scoped block changed.
    BlockRestore,
    /// Establishing an annotated width at a label, for every arrival.
    LabelBridge,
}

/// The per-axis "may differ from the value at function entry" bit that
/// separates `Preserve` from `Unknown` in exit classification.
///
/// Without it a function with no declared contract whose body never touches
/// the accumulator would exit `Unknown` — its entry state is unknown, so its
/// return state is too — when the honest answer is `Preserve`.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Touched {
    pub a: bool,
    pub i: bool,
}

impl Touched {
    fn join(self, other: Self) -> Self {
        Self {
            a: self.a || other.a,
            i: self.i || other.i,
        }
    }
}

/// The complete lattice element the solver carries.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
struct Fact {
    mode: ModeState,
    touched: Touched,
}

impl Fact {
    fn bottom() -> Self {
        Self::default()
    }

    fn join(self, other: Self) -> Self {
        if self.mode.is_bottom() {
            return other;
        }
        if other.mode.is_bottom() {
            return self;
        }
        Self {
            mode: self.mode.join(other.mode),
            touched: self.touched.join(other.touched),
        }
    }

    fn is_bottom(self) -> bool {
        self.mode.is_bottom()
    }
}

/// The state a target K816 cannot enumerate arrives in: nothing is known,
/// emulation included.
fn opaque_arrival() -> Fact {
    Fact {
        mode: ModeState {
            a: AxisState::Unknown,
            i: AxisState::Unknown,
            e: EmulationState::Unknown,
        },
        touched: Touched { a: true, i: true },
    }
}

/// What a node does to the tracked widths.
#[derive(Clone, Debug)]
enum Transfer {
    /// Nothing at all.
    Identity,
    /// An executed instruction's own width effect, interpreted through the
    /// tracked emulation state.
    Instruction {
        effect: WidthEffect,
        /// `XCE` is the only instruction that makes the emulation flag
        /// unknowable, and K65 has no syntax to make it known again.
        exchanges_emulation: bool,
    },
    /// Establish the annotated widths.
    Set(WidthContract),
    /// Restore the axes a width block changed to the fact at its entry node,
    /// the touched bit included: a block that widens and restores leaves the
    /// axis exactly as it found it.
    Restore { enter: usize, axes: WidthContract },
    /// A call, whose callee's exit contract advances the tracker.
    Call { callee: Option<SymbolId> },
}

/// Where a transfer's control goes next.
#[derive(Clone, Debug, Default)]
struct Control {
    /// Control may continue with the following node.
    fall_through: bool,
    /// An explicit transfer target.
    target: Option<Target>,
    /// Control may leave the function here.
    returns: bool,
}

impl Control {
    fn fall_through() -> Self {
        Self {
            fall_through: true,
            ..Self::default()
        }
    }
}

#[derive(Clone, Copy, Debug)]
enum Target {
    /// A label of this function, resolved after the whole body is flattened
    /// so that a forward branch works.
    Label(SymbolId),
    Node(usize),
    /// A target K816 cannot enumerate: an indirect jump, or a name that is
    /// not a label of this function.
    Unknown,
}

/// One node of the flattened control-flow graph.
#[derive(Clone, Debug)]
struct Node {
    /// The statement this node belongs to; several nodes may share one.
    statement: StmtId,
    span: Span,
    transfer: Transfer,
    control: Control,
    /// Set when this node *is* the label definition, so the solver knows
    /// where a label bridge goes and which merges to claim.
    label: Option<SymbolId>,
    /// True when this node's label is visible outside the function, and so
    /// may be branched to from code K816 is not looking at.
    label_is_global: bool,
    /// True when the node is the last one of its statement, so the "state
    /// after this statement" is its out-state.
    last_of_statement: bool,
}

/// A scoped width block's node range, for the leaking-exit check.
#[derive(Clone, Copy, Debug)]
struct BlockRange {
    enter: usize,
    exit: usize,
    span: Span,
    axes: WidthContract,
}

/// Folds a run of width annotations into a contract.
///
/// Two annotations constraining one axis differently are an error rather than
/// "last one wins": `func f @a8 @a16` states two incompatible requirements
/// and picking one silently would make the other a lie. Repeating the *same*
/// annotation is accepted.
pub fn fold_annotations(
    annotations: &[WidthAnnotation],
    diagnostics: &mut Vec<Diagnostic>,
) -> WidthContract {
    let mut contract = WidthContract::default();
    let mut origin: [Option<Span>; 2] = [None, None];
    for annotation in annotations {
        let (axis, width) = match annotation.kind {
            WidthAnnotationKind::A8 => (0, RegisterWidth::W8),
            WidthAnnotationKind::A16 => (0, RegisterWidth::W16),
            WidthAnnotationKind::I8 => (1, RegisterWidth::W8),
            WidthAnnotationKind::I16 => (1, RegisterWidth::W16),
        };
        let current = if axis == 0 { contract.a } else { contract.i };
        match current {
            Some(existing) if existing != width => {
                let first = origin[axis].expect("a set axis was set by an annotation");
                diagnostics.push(conflicting_width(annotation, first, axis));
                continue;
            }
            Some(_) => continue,
            None => {}
        }
        if axis == 0 {
            contract.a = Some(width);
        } else {
            contract.i = Some(width);
        }
        origin[axis] = Some(annotation.span);
    }
    contract
}

fn conflicting_width(annotation: &WidthAnnotation, first: Span, axis: usize) -> Diagnostic {
    let name = if axis == 0 {
        "accumulator"
    } else {
        "index register"
    };
    Diagnostic::error(
        DiagnosticCode::CONFLICTING_WIDTH,
        format!("two annotations give the {name} different widths"),
        Label::new(annotation.span, "this width contradicts the earlier one"),
    )
    .with_secondary(Label::new(first, "the width declared first"))
    .with_note("repeating the same annotation is fine; contradicting it is not")
    .with_fix(Fix {
        title: "drop the contradicting annotation".to_owned(),
        applicability: Applicability::MaybeIncorrect,
        edits: vec![TextEdit::delete(annotation.span)],
    })
    .with_docs("doc/register-width-aware-syntax.md#mode-annotations")
}

/// Merges module defaults into a declared contract, per axis.
///
/// A function-level annotation takes precedence; an axis the function leaves
/// unspecified inherits the module's.
pub fn merge_defaults(defaults: WidthContract, declared: WidthContract) -> WidthContract {
    WidthContract {
        a: declared.a.or(defaults.a),
        i: declared.i.or(defaults.i),
    }
}

/// What the solver needs to know beyond the body it is walking.
pub struct Context<'a> {
    pub symbols: &'a SymbolTable,
    /// The function whose scope `.name` labels resolve in.
    pub function: Option<SymbolId>,
    /// The classified exit contract of each callee. A callee absent from it
    /// takes the language's `Unknown ⇒ Preserve` rule.
    pub exits: &'a BTreeMap<SymbolId, ExitContract>,
    /// Global labels another unit in the workspace refers to.
    ///
    /// Such a label may be branched to from code K816 is not looking at, so
    /// it is reachable at a width nothing here establishes. A global label
    /// nobody else names is an ordinary merge point: treating every one of
    /// them as opaque would make a plain forward label inside a function
    /// destroy the widths around it.
    pub externally_reachable: &'a BTreeSet<SymbolId>,
}

/// Runs the width dataflow over one function body.
pub fn analyze_function(
    body: &Block,
    entry: ModeState,
    context: &Context<'_>,
) -> (WidthAnalysis, Vec<Diagnostic>) {
    let Context {
        symbols,
        function,
        exits,
        externally_reachable,
    } = *context;
    let mut flattener = Flattener {
        symbols,
        function,
        externally_reachable,
        nodes: Vec::new(),
        labels: HashMap::new(),
        blocks: Vec::new(),
        loops: Vec::new(),
        break_fixups: Vec::new(),
        diagnostics: Vec::new(),
        needs: WidthNeeds::default(),
        callees: Vec::new(),
    };
    flattener.block(body);
    let graph = flattener.finish();
    solve(graph, entry, exits)
}

struct Flattener<'a> {
    symbols: &'a SymbolTable,
    function: Option<SymbolId>,
    externally_reachable: &'a BTreeSet<SymbolId>,
    nodes: Vec<Node>,
    labels: HashMap<SymbolId, usize>,
    blocks: Vec<BlockRange>,
    /// Open loops, as `(head node, break fixup slot)`.
    loops: Vec<usize>,
    /// `break` nodes waiting for their loop's exit node index.
    break_fixups: Vec<(usize, usize)>,
    diagnostics: Vec<Diagnostic>,
    needs: WidthNeeds,
    callees: Vec<SymbolId>,
}

/// The flattened graph plus everything the solver needs beside it.
struct Graph {
    nodes: Vec<Node>,
    blocks: Vec<BlockRange>,
    /// True when some transfer in the function goes somewhere K816 cannot
    /// name, which makes every label reachable from an unknown width.
    opaque_transfer: bool,
    diagnostics: Vec<Diagnostic>,
    needs: WidthNeeds,
    callees: Vec<SymbolId>,
}

impl Flattener<'_> {
    fn finish(mut self) -> Graph {
        // Label targets resolve now that the whole body has been seen, which
        // is what makes a forward branch an edge rather than an unknown.
        for index in 0..self.nodes.len() {
            if let Some(Target::Label(symbol)) = self.nodes[index].control.target {
                self.nodes[index].control.target = match self.labels.get(&symbol) {
                    Some(target) => Some(Target::Node(*target)),
                    None => Some(Target::Unknown),
                };
            }
        }
        let opaque_transfer = self
            .nodes
            .iter()
            .any(|node| matches!(node.control.target, Some(Target::Unknown)));
        // A statement's last node is simply its highest-numbered one, which a
        // pass over the finished list gets right even where the model moved a
        // statement's transition past an intervening one.
        let mut last: HashMap<StmtId, usize> = HashMap::new();
        for (index, node) in self.nodes.iter().enumerate() {
            last.insert(node.statement, index);
        }
        for index in last.into_values() {
            self.nodes[index].last_of_statement = true;
        }
        self.report_block_leaks();
        Graph {
            nodes: self.nodes,
            blocks: self.blocks,
            opaque_transfer,
            diagnostics: self.diagnostics,
            needs: self.needs,
            callees: self.callees,
        }
    }

    /// Reports every transfer that leaves a scoped width block.
    ///
    /// No restore is emitted on such an edge — one placed after the branch is
    /// dead code, and one placed before it would also run on the paths that
    /// stay in the block — so the block's width arrives at the target. The
    /// dataflow carries that truth, so nothing is ever mis-sized; what is
    /// wrong is the surprise, and that is what the warning names.
    fn report_block_leaks(&mut self) {
        let mut reports = Vec::new();
        for (index, node) in self.nodes.iter().enumerate() {
            let Some(Target::Node(target)) = node.control.target else {
                continue;
            };
            for block in &self.blocks {
                let inside = |position: usize| position > block.enter && position < block.exit;
                if inside(index) && !inside(target) {
                    reports.push((node.span, *block));
                }
            }
        }
        for (span, block) in reports {
            self.diagnostics.push(block_leak(span, block));
        }
    }

    fn push(
        &mut self,
        statement: StmtId,
        span: Span,
        transfer: Transfer,
        control: Control,
    ) -> usize {
        self.nodes.push(Node {
            statement,
            span,
            transfer,
            control,
            label: None,
            label_is_global: false,
            last_of_statement: false,
        });
        self.nodes.len() - 1
    }

    fn block(&mut self, block: &Block) {
        let mut statements = block.statements.iter().peekable();
        while let Some(statement) = statements.next() {
            // Decision 6: an annotated mode pending in front of a label is
            // established *after* the label, so arrivals by branch get it too.
            if let StatementKind::ModeSet(annotations) = &statement.kind
                && let Some(next) = statements.peek()
                && matches!(next.kind, StatementKind::Label(_))
            {
                let axes = fold_annotations(annotations, &mut self.diagnostics);
                self.push(
                    statement.id,
                    statement.span,
                    Transfer::Identity,
                    Control::fall_through(),
                );
                let label = statements.next().expect("peeked");
                self.label_statement(label, Some((statement.id, axes)));
                continue;
            }
            self.statement(statement);
        }
    }

    fn statement(&mut self, statement: &Statement) {
        let id = statement.id;
        let span = statement.span;
        match &statement.kind {
            StatementKind::Label(_) => self.label_statement(statement, None),
            StatementKind::Native(native) => self.native(statement, native),
            StatementKind::ModeSet(annotations) => {
                let axes = fold_annotations(annotations, &mut self.diagnostics);
                self.push(id, span, Transfer::Set(axes), Control::fall_through());
            }
            StatementKind::WidthBlock(width) => {
                let axes = fold_annotations(&width.width, &mut self.diagnostics);
                let enter = self.push(id, span, Transfer::Set(axes), Control::fall_through());
                self.block(&width.body);
                let exit = self.push(
                    id,
                    span,
                    Transfer::Restore { enter, axes },
                    Control::fall_through(),
                );
                self.blocks.push(BlockRange {
                    enter,
                    exit,
                    span,
                    axes,
                });
            }
            // A `nocross` wrapper is a placement request, not control flow.
            StatementKind::Nocross(body) => {
                self.push(id, span, Transfer::Identity, Control::fall_through());
                self.block(body);
            }
            StatementKind::Call(call) => {
                let callee = self.lookup_callee(&call.target.text);
                self.call(id, span, callee);
            }
            StatementKind::BareCall(call) => {
                let callee = self.lookup_callee(&call.target.text);
                self.call(id, span, callee);
            }
            StatementKind::Return(_) => {
                self.push(
                    id,
                    span,
                    Transfer::Identity,
                    Control {
                        fall_through: false,
                        target: None,
                        returns: true,
                    },
                );
            }
            StatementKind::Goto(goto) => {
                let target = self.goto_target(goto);
                self.push(
                    id,
                    span,
                    Transfer::Identity,
                    Control {
                        fall_through: false,
                        target: Some(target),
                        returns: false,
                    },
                );
            }
            StatementKind::Break => {
                let node = self.push(
                    id,
                    span,
                    Transfer::Identity,
                    Control {
                        fall_through: false,
                        target: Some(Target::Unknown),
                        returns: false,
                    },
                );
                self.pending_break(node);
            }
            StatementKind::Repeat => {
                let target = self
                    .loops
                    .last()
                    .map_or(Target::Unknown, |head| Target::Node(*head));
                self.push(
                    id,
                    span,
                    Transfer::Identity,
                    Control {
                        fall_through: false,
                        target: Some(target),
                        returns: false,
                    },
                );
            }
            StatementKind::Conditional(conditional) => {
                // The condition either enters the body or skips it; both
                // paths meet at a join node the solver treats as a merge.
                let branch = self.push(
                    id,
                    span,
                    Transfer::Identity,
                    Control {
                        fall_through: true,
                        target: Some(Target::Unknown),
                        returns: false,
                    },
                );
                self.block(&conditional.body);
                let skip = match &conditional.otherwise {
                    Some(otherwise) => {
                        let jump = self.push(
                            id,
                            span,
                            Transfer::Identity,
                            Control {
                                fall_through: false,
                                target: Some(Target::Unknown),
                                returns: false,
                            },
                        );
                        let alternative = self.nodes.len();
                        self.block(otherwise);
                        let join = self.push(id, span, Transfer::Identity, Control::fall_through());
                        self.nodes[jump].control.target = Some(Target::Node(join));
                        (alternative, join)
                    }
                    None => {
                        let join = self.push(id, span, Transfer::Identity, Control::fall_through());
                        (join, join)
                    }
                };
                self.nodes[branch].control.target = Some(Target::Node(skip.0));
            }
            StatementKind::ConditionalJump(jump) => {
                // The transfer becomes conditional: control may take it, or
                // may continue with the next statement. The inner statement
                // keeps its own identity, so both are addressable.
                self.push(id, span, Transfer::Identity, Control::fall_through());
                let first = self.nodes.len();
                self.statement(&jump.jump);
                for node in &mut self.nodes[first..] {
                    node.control.fall_through = true;
                }
            }
            StatementKind::Loop(loop_statement) => {
                let head = self.push(id, span, Transfer::Identity, Control::fall_through());
                self.loops.push(head);
                self.block(&loop_statement.body);
                let control = match loop_statement.condition {
                    // `{ ... } !=` closes the loop conditionally, so the back
                    // edge and the fall-through both exist.
                    LoopCondition::Branch(_) => Control {
                        fall_through: true,
                        target: Some(Target::Node(head)),
                        returns: false,
                    },
                    LoopCondition::Always => Control {
                        fall_through: false,
                        target: Some(Target::Node(head)),
                        returns: false,
                    },
                    LoopCondition::Never => Control::fall_through(),
                };
                let exit = self.push(id, span, Transfer::Identity, control);
                self.loops.pop();
                self.resolve_breaks(exit);
            }
            StatementKind::Data(data) => {
                self.push(id, span, Transfer::Identity, Control::fall_through());
                // Statements nested in a `code` entry are numbered, so they
                // are flattened too rather than silently left stateless.
                for entry in &data.entries {
                    if let DataEntryKind::Code { body, .. } = &entry.kind {
                        self.block(body);
                    }
                }
            }
            StatementKind::Hla(hla) => {
                if let ast::HlaStatement::Assign(assign) = hla
                    && matches!(assign.source.kind, ast::HlaOperandKind::Immediate(_))
                    && assign.targets.iter().any(|target| {
                        matches!(&target.kind, ast::HlaOperandKind::Register(register)
                            if register.kind == ast::RegisterKind::A)
                    })
                {
                    self.needs.m = true;
                }
                self.push(id, span, Transfer::Identity, Control::fall_through());
            }
            StatementKind::Preprocessor(_) | StatementKind::Error => {
                self.push(id, span, Transfer::Identity, Control::fall_through());
            }
        }
    }

    /// A label definition, optionally carrying the bridge a preceding mode
    /// set handed it (decision 6).
    fn label_statement(&mut self, statement: &Statement, pending: Option<(StmtId, WidthContract)>) {
        let StatementKind::Label(label) = &statement.kind else {
            unreachable!("only called for a label statement");
        };
        let symbol = match label.scope {
            ast::LabelScope::Global => self.symbols.lookup(&label.name.text),
            ast::LabelScope::Local => self
                .function
                .and_then(|owner| self.symbols.lookup_local(owner, &label.name.text)),
        };
        let node = self.push(
            statement.id,
            statement.span,
            Transfer::Identity,
            Control::fall_through(),
        );
        self.nodes[node].label = symbol;
        self.nodes[node].label_is_global = symbol.is_some_and(|symbol| {
            self.externally_reachable.contains(&symbol)
                && matches!(
                    self.symbols.kind(symbol),
                    SymbolKind::Label {
                        scope: LabelScope::Global,
                        ..
                    }
                )
        });
        if let Some(symbol) = symbol {
            self.labels.insert(symbol, node);
        }
        if let Some((_, axes)) = pending {
            self.push(
                statement.id,
                statement.span,
                Transfer::Set(axes),
                Control::fall_through(),
            );
        }
    }

    fn native(&mut self, statement: &Statement, native: &ast::NativeInstruction) {
        let id = statement.id;
        let span = statement.span;
        // Classification is C's, and running it again here rather than
        // re-deriving the mode keeps one answer about which mode an operand
        // selects. This run's diagnostics are dropped: the analysis walk
        // reports them for real.
        let mut resolver = Resolver::new(self.symbols).in_function(self.function);
        let classified = resolver.classify(native);
        let Some(classified) = classified else {
            // The instruction is already rejected, so it will never be
            // emitted; claiming it destroys the widths would only add noise
            // to a program that has already failed.
            self.push(id, span, Transfer::Identity, Control::fall_through());
            return;
        };
        let instruction = classified.instruction();
        if let Some(class) = instruction.spec().and_then(|spec| spec.immediate_width()) {
            match class {
                ImmediateWidthClass::MDependent => self.needs.m = true,
                ImmediateWidthClass::XDependent => self.needs.x = true,
                ImmediateWidthClass::Fixed8 | ImmediateWidthClass::Fixed16 => {}
            }
        }
        let effects = instruction.effects();
        let target = instruction.operand.as_ref().and_then(operand_target);
        let symbol = target.and_then(|expr| match expr {
            KExpr::Symbol(symbol) => Some(*symbol),
            _ => None,
        });
        match effects.control {
            ControlEffect::Call => {
                let callee = symbol.filter(|symbol| {
                    matches!(self.symbols.kind(*symbol), SymbolKind::Function { .. })
                });
                self.call(id, span, callee);
            }
            ControlEffect::Branch | ControlEffect::Jump => {
                let target = match symbol {
                    Some(symbol)
                        if matches!(self.symbols.kind(symbol), SymbolKind::Label { .. }) =>
                    {
                        Target::Label(symbol)
                    }
                    _ => Target::Unknown,
                };
                self.push(
                    id,
                    span,
                    Transfer::Instruction {
                        effect: effects.width,
                        exchanges_emulation: instruction.mnemonic == Mnemonic::Xce,
                    },
                    Control {
                        fall_through: effects.control == ControlEffect::Branch,
                        target: Some(target),
                        returns: false,
                    },
                );
            }
            ControlEffect::Return => {
                self.push(
                    id,
                    span,
                    Transfer::Instruction {
                        effect: effects.width,
                        exchanges_emulation: instruction.mnemonic == Mnemonic::Xce,
                    },
                    Control {
                        fall_through: false,
                        target: None,
                        returns: true,
                    },
                );
            }
            // A trap returns through `RTI`, which restores `P` and with it
            // the widths, and `WAI` resumes at the next instruction. `STP`
            // shares `Halt` with `WAI`, so falling through is the answer that
            // never claims a width for code the CPU might still reach.
            ControlEffect::FallThrough
            | ControlEffect::Trap
            | ControlEffect::Halt
            | ControlEffect::Unknown => {
                self.push(
                    id,
                    span,
                    Transfer::Instruction {
                        effect: effects.width,
                        exchanges_emulation: instruction.mnemonic == Mnemonic::Xce,
                    },
                    Control::fall_through(),
                );
            }
        }
    }

    fn call(&mut self, statement: StmtId, span: Span, callee: Option<SymbolId>) {
        if let Some(callee) = callee
            && !self.callees.contains(&callee)
        {
            self.callees.push(callee);
        }
        self.push(
            statement,
            span,
            Transfer::Call { callee },
            Control::fall_through(),
        );
    }

    fn lookup_callee(&self, name: &str) -> Option<SymbolId> {
        self.symbols
            .lookup(name)
            .filter(|symbol| matches!(self.symbols.kind(*symbol), SymbolKind::Function { .. }))
    }

    fn goto_target(&self, goto: &ast::GotoStatement) -> Target {
        if goto.indirect {
            return Target::Unknown;
        }
        let ast::ExprKind::Symbol(name) = &goto.target.kind else {
            let ast::ExprKind::LocalSymbol(name) = &goto.target.kind else {
                return Target::Unknown;
            };
            return self
                .function
                .and_then(|owner| self.symbols.lookup_local(owner, &name.text))
                .map_or(Target::Unknown, Target::Label);
        };
        self.symbols
            .lookup(&name.text)
            .filter(|symbol| matches!(self.symbols.kind(*symbol), SymbolKind::Label { .. }))
            .map_or(Target::Unknown, Target::Label)
    }

    fn pending_break(&mut self, node: usize) {
        let depth = self.loops.len();
        self.break_fixups.push((node, depth));
    }

    fn resolve_breaks(&mut self, exit: usize) {
        let depth = self.loops.len() + 1;
        let mut remaining = Vec::new();
        for (node, break_depth) in std::mem::take(&mut self.break_fixups) {
            if break_depth == depth {
                self.nodes[node].control.target = Some(Target::Node(exit));
            } else {
                remaining.push((node, break_depth));
            }
        }
        self.break_fixups = remaining;
    }
}

/// The label or jump target an operand names, if it names one.
fn operand_target(operand: &Operand) -> Option<&KExpr> {
    match operand {
        Operand::Label(expr) => Some(expr),
        Operand::Address {
            expr, index: None, ..
        } => Some(expr),
        Operand::Address { .. }
        | Operand::Immediate(_)
        | Operand::Indirect { .. }
        | Operand::StackRelative(_)
        | Operand::BlockMove { .. } => None,
    }
}

fn block_leak(span: Span, block: BlockRange) -> Diagnostic {
    let axes = describe_axes(block.axes);
    Diagnostic::warning(
        DiagnosticCode::WIDTH_BLOCK_LEAK,
        "this transfer leaves a scoped width block",
        Label::new(span, format!("{axes} escapes to the target")),
    )
    .with_secondary(Label::new(
        block.span,
        "the block whose width is not restored",
    ))
    .with_note(
        "a restore after the transfer would be dead code, and one before it would \
         also run on the paths that stay in the block, so none is emitted",
    )
    .with_help("move the target inside the block, or annotate it with the width it should have")
    .with_docs("doc/register-width-aware-syntax.md#block-level-mode-scoping")
}

fn describe_axes(axes: WidthContract) -> String {
    let mut parts = Vec::new();
    if let Some(width) = axes.a {
        parts.push(match width {
            RegisterWidth::W8 => "@a8",
            RegisterWidth::W16 => "@a16",
        });
    }
    if let Some(width) = axes.i {
        parts.push(match width {
            RegisterWidth::W8 => "@i8",
            RegisterWidth::W16 => "@i16",
        });
    }
    if parts.is_empty() {
        "the block's width".to_owned()
    } else {
        format!("the block's {}", parts.join(" "))
    }
}

/// The worklist solver.
fn solve(
    graph: Graph,
    entry: ModeState,
    exits: &BTreeMap<SymbolId, ExitContract>,
) -> (WidthAnalysis, Vec<Diagnostic>) {
    let nodes = &graph.nodes;
    let count = nodes.len();
    let mut predecessors: Vec<Vec<usize>> = vec![Vec::new(); count];
    for (index, node) in nodes.iter().enumerate() {
        if node.control.fall_through && index + 1 < count {
            predecessors[index + 1].push(index);
        }
        if let Some(Target::Node(target)) = node.control.target {
            predecessors[target].push(index);
        }
    }

    // A `Restore` reads the fact at its block's entry node, so a change there
    // has to re-reach it even though there is no edge between the two.
    let mut restore_readers: HashMap<usize, Vec<usize>> = HashMap::new();
    for (index, node) in nodes.iter().enumerate() {
        if let Transfer::Restore { enter, .. } = node.transfer {
            restore_readers.entry(enter).or_default().push(index);
        }
    }

    let entry_fact = Fact {
        mode: entry,
        touched: Touched::default(),
    };
    let mut r#in: Vec<Fact> = vec![Fact::bottom(); count];
    let mut out: Vec<Fact> = vec![Fact::bottom(); count];

    // Every label is reachable from an unknown width when the function
    // contains a transfer K816 cannot name, and a *global* label is reachable
    // from outside the function by definition. Conservative and loud beats
    // plausible and silent.
    let opaque_labels: Vec<bool> = nodes
        .iter()
        .map(|node| node.label.is_some() && (graph.opaque_transfer || node.label_is_global))
        .collect();

    let mut worklist: Vec<usize> = (0..count).rev().collect();
    while let Some(index) = worklist.pop() {
        // The entry block starts at the declared contract; every other block
        // starts at `Bottom`, the join identity, so that a block no edge has
        // reached yet stays distinguishable from one whose merge is unknown.
        let mut incoming = if index == 0 {
            entry_fact
        } else {
            Fact::bottom()
        };
        for predecessor in &predecessors[index] {
            incoming = incoming.join(out[*predecessor]);
        }
        if opaque_labels[index] {
            incoming = incoming.join(opaque_arrival());
        }
        let produced = apply(&nodes[index].transfer, incoming, &r#in, exits);
        if incoming == r#in[index] && produced == out[index] {
            continue;
        }
        r#in[index] = incoming;
        out[index] = produced;
        if nodes[index].control.fall_through && index + 1 < count {
            worklist.push(index + 1);
        }
        if let Some(Target::Node(target)) = nodes[index].control.target {
            worklist.push(target);
        }
        if let Some(readers) = restore_readers.get(&index) {
            worklist.extend(readers);
        }
    }

    let mut analysis = WidthAnalysis {
        entry,
        exit: ExitContract::preserving(),
        exit_is_assumed: true,
        needs: graph.needs,
        callees: graph.callees,
        ..WidthAnalysis::default()
    };
    for (index, node) in nodes.iter().enumerate() {
        analysis
            .before
            .entry(node.statement)
            .or_insert(r#in[index].mode);
        if node.last_of_statement {
            analysis.after.insert(node.statement, out[index].mode);
        }
    }
    for (index, node) in nodes.iter().enumerate() {
        if out[index].is_bottom() {
            continue; // Unreachable: not a path any caller returns through.
        }
        // Falling off the end of the body is a return too — that is where
        // lowering's implicit `rts` goes.
        let leaves = node.control.returns
            || (node.control.fall_through && index + 1 == count && node.control.target.is_none());
        if leaves {
            analysis.returns.push(ReturnSite {
                state: out[index].mode,
                touched: out[index].touched,
                span: node.span,
            });
        }
    }
    record_bridges(&mut analysis, nodes, &r#in, &graph.blocks);
    (analysis, graph.diagnostics)
}

/// One node's transfer function.
fn apply(
    transfer: &Transfer,
    incoming: Fact,
    r#in: &[Fact],
    exits: &BTreeMap<SymbolId, ExitContract>,
) -> Fact {
    if incoming.is_bottom() {
        // No path reaches this node, so it produces nothing rather than
        // manufacturing a state out of its transfer.
        return Fact::bottom();
    }
    match *transfer {
        Transfer::Identity => incoming,
        Transfer::Instruction {
            effect,
            exchanges_emulation,
        } => apply_effect(incoming, effect, exchanges_emulation),
        Transfer::Set(axes) => {
            let mut fact = incoming;
            if let Some(width) = axes.a {
                fact.mode.a = AxisState::Known(width);
                fact.touched.a = true;
            }
            if let Some(width) = axes.i {
                fact.mode.i = AxisState::Known(width);
                fact.touched.i = true;
            }
            fact
        }
        Transfer::Restore { enter, axes } => {
            let saved = r#in[enter];
            let mut fact = incoming;
            // A block that never ran restores nothing; the saved fact is
            // `Bottom` exactly then.
            if saved.is_bottom() {
                return fact;
            }
            if axes.a.is_some() {
                fact.mode.a = saved.mode.a;
                fact.touched.a = saved.touched.a;
            }
            if axes.i.is_some() {
                fact.mode.i = saved.mode.i;
                fact.touched.i = saved.touched.i;
            }
            fact
        }
        Transfer::Call { callee } => {
            let exit = callee
                .and_then(|callee| exits.get(&callee).copied())
                .unwrap_or_else(ExitContract::preserving);
            let mut fact = incoming;
            fact.mode = exit.apply(incoming.mode);
            fact.touched.a |= matches!(exit.a, AxisExit::Fixed(_));
            fact.touched.i |= matches!(exit.i, AxisExit::Fixed(_));
            fact
        }
    }
}

/// An executed instruction's width transition, interpreted through the
/// tracked emulation state.
///
/// Under `Native` the effect table's answer applies as written. Under an
/// unknown emulation flag a *narrowing* transition still yields 8-bit — that
/// is the answer in both modes — while a *widening* one yields `Unknown`,
/// because `REP` cannot widen a register the CPU is forcing to 8 bits. The
/// asymmetry is the whole point: `SEP` is safe to believe, `REP` is not.
fn apply_effect(incoming: Fact, effect: WidthEffect, exchanges_emulation: bool) -> Fact {
    let mut fact = incoming;
    match effect {
        WidthEffect::Unchanged => {}
        WidthEffect::Known { a, i } => {
            if let Some(width) = a {
                fact.mode.a = believe(width, fact.mode.e);
                fact.touched.a = true;
            }
            if let Some(width) = i {
                fact.mode.i = believe(width, fact.mode.e);
                fact.touched.i = true;
            }
        }
        WidthEffect::Unknown => {
            fact.mode.a = AxisState::Unknown;
            fact.mode.i = AxisState::Unknown;
            fact.touched = Touched { a: true, i: true };
        }
    }
    if exchanges_emulation {
        fact.mode.e = EmulationState::Unknown;
    }
    fact
}

fn believe(width: RegisterWidth, emulation: EmulationState) -> AxisState {
    match (width, emulation) {
        (RegisterWidth::W8, _) | (RegisterWidth::W16, EmulationState::Native) => {
            AxisState::Known(width)
        }
        (RegisterWidth::W16, EmulationState::Unknown) => AxisState::Unknown,
    }
}

/// Records the `REP`/`SEP` the model itself contributes, once the states are
/// final: a bridge exists exactly where the tracked state differs from the
/// width being established, so an already-satisfied annotation emits nothing.
fn record_bridges(
    analysis: &mut WidthAnalysis,
    nodes: &[Node],
    r#in: &[Fact],
    blocks: &[BlockRange],
) {
    for (index, node) in nodes.iter().enumerate() {
        if r#in[index].is_bottom() {
            continue; // Unreachable: the model makes no claim here.
        }
        let (axes, anchor, reason) = match &node.transfer {
            Transfer::Set(axes) => {
                let is_block_enter = blocks.iter().any(|block| block.enter == index);
                let after_label = index > 0 && nodes[index - 1].label.is_some();
                let reason = if is_block_enter {
                    BridgeReason::BlockEnter
                } else if after_label {
                    BridgeReason::LabelBridge
                } else {
                    BridgeReason::ModeSet
                };
                let anchor = if after_label {
                    BridgeAnchor::AfterLabel
                } else {
                    BridgeAnchor::Before
                };
                (
                    WidthContract {
                        a: axes.a,
                        i: axes.i,
                    },
                    anchor,
                    reason,
                )
            }
            Transfer::Restore { enter, axes } => {
                let saved = r#in[*enter];
                if saved.is_bottom() {
                    continue;
                }
                (
                    WidthContract {
                        a: axes.a.and(saved.mode.a.width()),
                        i: axes.i.and(saved.mode.i.width()),
                    },
                    BridgeAnchor::After,
                    BridgeReason::BlockRestore,
                )
            }
            Transfer::Identity | Transfer::Instruction { .. } | Transfer::Call { .. } => continue,
        };
        let state = r#in[index].mode;
        let mut bridges = Vec::new();
        for (width, mask, current) in [(axes.a, P_MASK_M, state.a), (axes.i, P_MASK_X, state.i)] {
            let Some(width) = width else { continue };
            if current == AxisState::Known(width) {
                continue; // Already established; there is nothing to emit.
            }
            bridges.push(Bridge {
                width,
                mask,
                anchor,
                reason,
            });
        }
        if !bridges.is_empty() {
            analysis
                .bridges
                .entry(node.statement)
                .or_default()
                .extend(bridges);
        }
    }
}

/// One width axis, named for the diagnostics that talk about it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Axis {
    Accumulator,
    Index,
}

impl Axis {
    pub fn name(self) -> &'static str {
        match self {
            Self::Accumulator => "accumulator",
            Self::Index => "index register",
        }
    }

    /// The annotation that declares this axis at `width`.
    pub fn annotation(self, width: RegisterWidth) -> &'static str {
        match (self, width) {
            (Self::Accumulator, RegisterWidth::W8) => "@a8",
            (Self::Accumulator, RegisterWidth::W16) => "@a16",
            (Self::Index, RegisterWidth::W8) => "@i8",
            (Self::Index, RegisterWidth::W16) => "@i16",
        }
    }

    fn state(self, site: &ReturnSite) -> AxisState {
        match self {
            Self::Accumulator => site.state.a,
            Self::Index => site.state.i,
        }
    }

    fn touched(self, site: &ReturnSite) -> bool {
        match self {
            Self::Accumulator => site.touched.a,
            Self::Index => site.touched.i,
        }
    }
}

/// What classifying one function's exit found.
#[derive(Clone, Debug)]
pub struct ExitClassification {
    pub exit: ExitContract,
    /// True when some axis fell back to the language's `Unknown ⇒ Preserve`
    /// rule rather than being derived.
    pub assumed: bool,
    /// Axes whose reachable returns exit at different fixed widths, which is
    /// a hard error at the callee rather than something a caller can absorb.
    pub divergent: Vec<Divergence>,
}

/// Two reachable returns of one function that leave an axis at different
/// fixed widths.
#[derive(Clone, Copy, Debug)]
pub struct Divergence {
    pub axis: Axis,
    pub first: Span,
    pub first_width: RegisterWidth,
    pub second: Span,
    pub second_width: RegisterWidth,
}

/// Classifies one function's exit contract from the return sites the solver
/// found, given callee exits that are already final.
///
/// The join is per axis over the *reachable* returns — `rts`/`rtl`, `return`,
/// and falling off the end of the body:
///
/// | Return sites agree on | Exit |
/// | --- | --- |
/// | the axis is untouched on every path | `Preserve` |
/// | the same width on every path, at least one touched | `Fixed(w)` |
/// | different widths on different paths | a divergence, reported |
/// | anything else | `Unknown`, and callers fall back to preserving |
///
/// `Preserve` comes from an *untouched* axis rather than from comparing
/// values. Deriving it by comparison would make every uncoloured function
/// exit `Unknown` — its entry state is unknown, so its return state is too —
/// which then warns about the most ordinary shape in the language and pushes
/// its callers into needless bridges.
pub fn classify_exit(analysis: &WidthAnalysis) -> ExitClassification {
    let mut exit = ExitContract::default();
    let mut divergent = Vec::new();
    let mut assumed = false;
    for axis in [Axis::Accumulator, Axis::Index] {
        let classified = classify_axis(axis, &analysis.returns, &mut divergent);
        assumed |= classified == AxisExit::Unknown;
        match axis {
            Axis::Accumulator => exit.a = classified,
            Axis::Index => exit.i = classified,
        }
    }
    ExitClassification {
        exit,
        assumed,
        divergent,
    }
}

fn classify_axis(axis: Axis, sites: &[ReturnSite], divergent: &mut Vec<Divergence>) -> AxisExit {
    // A function with no reachable return — one that ends in `stp`, or loops
    // forever — has no exit for a caller to observe, so the language rule is
    // both the honest answer and a vacuous one.
    if sites.is_empty() {
        return AxisExit::Preserve;
    }
    if sites.iter().all(|site| !axis.touched(site)) {
        return AxisExit::Preserve;
    }
    let mut fixed: Option<(RegisterWidth, Span)> = None;
    for site in sites {
        let AxisState::Known(width) = axis.state(site) else {
            // Some path leaves the axis unknowable, or unreached; either way
            // there is no width to promise.
            return AxisExit::Unknown;
        };
        match fixed {
            Some((first, first_span)) if first != width => {
                divergent.push(Divergence {
                    axis,
                    first: first_span,
                    first_width: first,
                    second: site.span,
                    second_width: width,
                });
                return AxisExit::Unknown;
            }
            Some(_) => {}
            None => fixed = Some((width, site.span)),
        }
    }
    // At least one path touched the axis, so an untouched one alongside a
    // fixed one is not `Fixed` — the width a caller sees would depend on
    // which path ran.
    if sites.iter().any(|site| !axis.touched(site)) {
        return AxisExit::Unknown;
    }
    fixed.map_or(AxisExit::Unknown, |(width, _)| AxisExit::Fixed(width))
}

/// The span of the first return site whose axis is not a known width, which
/// is the evidence `W0124` points at.
pub fn unclassifiable_return(analysis: &WidthAnalysis, axis: Axis) -> Option<Span> {
    analysis
        .returns
        .iter()
        .find(|site| !matches!(axis.state(site), AxisState::Known(_)))
        .map(|site| site.span)
}
