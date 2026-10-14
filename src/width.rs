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
use crate::effects::{ControlEffect, Effects, FlagSet, WidthEffect};
use crate::isa::{Mnemonic, RegisterWidth};
use crate::op::{
    AxisExit, AxisState, EmulationExit, EmulationState, ExitContract, ModeState, Operand, Reason,
    WidthContract,
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
    /// Call statements whose callee K816 cannot see, and which therefore
    /// take the language's `Unknown ⇒ Preserve` rule.
    ///
    /// They are annotated at the call site rather than warned about: there
    /// is nothing at the callee to point a label at, and nothing the author
    /// of *this* unit could do about it, so a warning would be noise on
    /// every cross-boundary call while saying nothing new.
    pub assumed_calls: std::collections::BTreeSet<StmtId>,
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
    /// What the model emitted it for, which the emitted line and the IR dump
    /// both state rather than restating the instruction.
    pub reason: Reason,
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
    /// On the *taken edge* of the statement's transfer, which is a place no
    /// operation stream has until lowering makes one.
    ///
    /// A branch leaving a scoped width block cannot restore it before the
    /// branch — that would also run on the paths that stay — and cannot
    /// restore it after, where the code is dead. So the restore goes on the
    /// edge, and lowering splits the edge to give it somewhere to be
    /// (sub-plan E, decision 9).
    TakenEdge,
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
    /// Some path executed an `XCE`.
    ///
    /// What separates a callee that leaves the emulation flag alone — the
    /// overwhelming majority, and the ones a caller's own belief survives —
    /// from one whose exit has to say something about it.
    pub e: bool,
}

impl Touched {
    fn join(self, other: Self) -> Self {
        Self {
            a: self.a || other.a,
            i: self.i || other.i,
            e: self.e || other.e,
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
        touched: Touched {
            a: true,
            i: true,
            e: true,
        },
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
    Call {
        callee: Option<SymbolId>,
        /// True for the managed forms — `call NAME` and the bare call —
        /// which establish the callee's entry contract before transferring.
        /// A native `jsr` is the escape hatch and is transparent: K816 emits
        /// what the user wrote, which is the whole premise of raw native
        /// code.
        bridged: bool,
    },
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
    /// A transfer that leaves this function for a place K816 *can* name: the
    /// documented `goto other_function` tail jump, and its far form.
    ///
    /// Distinct from [`Target::Unknown`] because the two say different things
    /// about *this* function's labels. An unknown target could be any of
    /// them; a named function's entry is none of them. Collapsing the two
    /// would make one tail jump degrade every label in the body to an
    /// unknown arrival, which is the shape `doc/syntax-reference.md`'s
    /// `naked` example is written in.
    External,
    /// A target K816 cannot enumerate: an indirect jump, or a name that is
    /// not a label of this function.
    Unknown,
}

/// One node of the flattened control-flow graph.
#[derive(Clone, Debug)]
struct Node {
    /// True for a node lowering created by splitting an edge: its restore
    /// belongs on that edge rather than after the statement.
    edge_split: bool,
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

/// The transfer an `RTI` return makes.
///
/// `RTI` pops `P`, and with it both width flags, so control leaves at exactly
/// the state that was pushed — the interrupted code's. From that code's point
/// of view the handler *preserved* its widths, whatever the body did in
/// between, and an interrupt handler that saves and restores its registers is
/// the idiom rather than a hazard.
///
/// It is spelled as the restore a scoped width block already uses, against
/// node 0, because node 0's incoming fact is the function's entry state and
/// the restore carries the touched bits with it. The widths named in `axes`
/// are irrelevant — only *which axes* are restored is read — so they say
/// "both".
fn restores_the_interrupted_state() -> Transfer {
    Transfer::Restore {
        enter: 0,
        axes: WidthContract {
            a: Some(RegisterWidth::W8),
            i: Some(RegisterWidth::W8),
        },
    }
}

/// A scoped width block's node range, for the leaking-exit check.
#[derive(Clone, Copy, Debug)]
struct BlockRange {
    enter: usize,
    exit: usize,
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

/// What a call site needs to know about its callee.
///
/// Comparable because sub-plan K asks two `extern` declarations of one name
/// whether they promise the same thing, and "the same thing" is this whole
/// value: the entry a caller bridges to, the exit it may believe, and what
/// calling it observes.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct CalleeContract {
    /// The widths the callee's body assumes on entry, which a managed call
    /// bridges to.
    pub entry: WidthContract,
    /// What the caller may believe about its widths afterwards.
    pub exit: ExitContract,
    /// Which width axes calling this function observes, transitively.
    ///
    /// Computed by [`observation_fixpoint`] over the whole workspace call
    /// graph and read by the optimizer, which asks it one question: is there
    /// anything reachable from this call that would behave differently if the
    /// caller stopped establishing the axis. The width solver never reads it
    /// — a width is what a claim says, and an axis nobody observes still has
    /// the width the model concluded.
    pub observes: WidthNeeds,
}

/// The transitive observation of `plans/05-optimizer.md` decision 4.
///
/// `seeds[i]` is what function `i`'s own body observes — [`WidthNeeds`],
/// which already folds in a transfer to code K816 cannot see — and
/// `adjacency[i]` names the functions it calls. An axis is observed at `i`
/// when `i`'s body observes it or some callee's answer does.
///
/// A worklist rather than the reverse-topological single pass the exit
/// classification uses, and the difference is deliberate: observation is a
/// union over a two-element lattice, so it is monotone and a fixpoint is
/// exactly right — including around a cycle, where every member observes
/// whatever any member observes. `AxisExit` has no such order, which is why
/// `postmortem.md` §D.5 forbids iterating *that* one.
pub fn observation_fixpoint(seeds: &[WidthNeeds], adjacency: &[Vec<usize>]) -> Vec<WidthNeeds> {
    let mut observed = seeds.to_vec();
    // Callers of `i`, because a change at `i` is what its callers must see.
    let mut callers: Vec<Vec<usize>> = vec![Vec::new(); seeds.len()];
    for (caller, callees) in adjacency.iter().enumerate() {
        for callee in callees {
            callers[*callee].push(caller);
        }
    }
    let mut worklist: Vec<usize> = (0..seeds.len()).collect();
    while let Some(node) = worklist.pop() {
        let mut answer = observed[node];
        for callee in &adjacency[node] {
            answer = answer.union(observed[*callee]);
        }
        if answer == observed[node] {
            continue;
        }
        observed[node] = answer;
        worklist.extend(callers[node].iter().copied());
    }
    observed
}

/// What the solver needs to know beyond the body it is walking.
pub struct Context<'a> {
    pub symbols: &'a SymbolTable,
    /// The evaluator's answers, so that an operand written as a bracket
    /// classifies here exactly as it does in analysis. Without it a bracket
    /// would fold to nothing in this walk and the statement would look
    /// rejected.
    pub eval: &'a crate::eval::Outcome,
    /// The function whose scope `.name` labels resolve in.
    pub function: Option<SymbolId>,
    /// The contract of each callee K816 can see. A callee absent from it is
    /// invisible — an unresolved indirect call, or a foreign name no
    /// analyzed unit defines — and takes the language's `Unknown ⇒ Preserve`
    /// rule with a call-site annotation rather than a diagnostic.
    pub callees: &'a BTreeMap<SymbolId, CalleeContract>,
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
        eval,
        function,
        callees,
        externally_reachable,
    } = *context;
    let mut flattener = Flattener {
        symbols,
        eval,
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
        target_uses: HashMap::new(),
    };
    flattener.block(body);
    let mut graph = flattener.finish();
    graph.confined = confined_labels(body, symbols, function, &graph.target_uses);
    solve(graph, entry, callees)
}

/// The local labels a transfer K816 cannot name still cannot reach: those
/// whose every use is as the named target of a branch or a jump.
///
/// An indirect jump lands on an address, so it can reach a label only if
/// the label's address went somewhere — an immediate, a data entry, a
/// memory operand. A local label is scoped to its section, so every use of
/// it is in this body, and [`crate::names`] walks every name a body holds:
/// a label used more often than this body's transfers named it has had its
/// address taken, and stays reachable from an unknown width.
fn confined_labels(
    body: &Block,
    symbols: &SymbolTable,
    function: Option<SymbolId>,
    target_uses: &HashMap<SymbolId, usize>,
) -> BTreeSet<SymbolId> {
    let Some(owner) = function else {
        return BTreeSet::new();
    };
    let mut uses: HashMap<SymbolId, usize> = HashMap::new();
    crate::names::visit_block(body, &mut |name, role| {
        if role == crate::names::NameRole::Symbol
            && name.text.starts_with('.')
            && let Some(symbol) = symbols.lookup_local(owner, &name.text)
        {
            *uses.entry(symbol).or_default() += 1;
        }
    });
    uses.into_iter()
        .filter(|(symbol, count)| target_uses.get(symbol) == Some(count))
        .map(|(symbol, _)| symbol)
        .collect()
}

struct Flattener<'a> {
    symbols: &'a SymbolTable,
    eval: &'a crate::eval::Outcome,
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
    /// How often each label was the named target of a branch or a jump.
    target_uses: HashMap<SymbolId, usize>,
}

/// The flattened graph plus everything the solver needs beside it.
struct Graph {
    nodes: Vec<Node>,
    blocks: Vec<BlockRange>,
    /// True when some transfer in the function goes somewhere K816 cannot
    /// name, which makes every label reachable from an unknown width — but
    /// one in `confined`.
    opaque_transfer: bool,
    /// How often each label was the named target of a branch or a jump.
    target_uses: HashMap<SymbolId, usize>,
    /// Local labels no unnamed transfer can reach ([`confined_labels`]).
    confined: BTreeSet<SymbolId>,
    diagnostics: Vec<Diagnostic>,
    needs: WidthNeeds,
    callees: Vec<SymbolId>,
}

impl Flattener<'_> {
    /// Records the axes one instruction observes.
    ///
    /// An axis is observed when the instruction *reads* its flag, which is
    /// every load, store, ALU operation, compare, push, pull, index step, and
    /// width-dependent transfer — not only the ones carrying a
    /// width-dependent immediate. See [`WidthNeeds`] for why the narrower
    /// reading the width document gives is unsound.
    fn observe(&mut self, effects: Effects) {
        if effects.reads.flags.intersects(FlagSet::M) {
            self.needs.m = true;
        }
        if effects.reads.flags.intersects(FlagSet::X) {
            self.needs.x = true;
        }
    }

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
        // A transfer K816 cannot name reaches code that may read anything, so
        // both axes are observed through it. Same answer as an invisible
        // callee's, reached the same way.
        if opaque_transfer {
            self.needs = self.needs.union(WidthNeeds::opaque());
        }
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
        self.split_block_exits();
        Graph {
            nodes: self.nodes,
            blocks: self.blocks,
            opaque_transfer,
            target_uses: self.target_uses,
            confined: BTreeSet::new(),
            diagnostics: self.diagnostics,
            needs: self.needs,
            callees: self.callees,
        }
    }

    /// Splits every edge that leaves a scoped width block.
    ///
    /// The restore needs a node of its own: placed before the branch it would
    /// also run on the paths that stay in the block, and placed after it the
    /// code is dead. So the edge gets a node, the dataflow carries the
    /// restored width to the target, and lowering emits the landing pad that
    /// gives the node somewhere to be (sub-plan E, decision 9;
    /// `postmortem.md` §D.6.2).
    ///
    /// The new nodes go on the end. Nothing falls through to them — every one
    /// names its target — and appending keeps the positions the block ranges
    /// are expressed in valid.
    fn split_block_exits(&mut self) {
        let mut splits: Vec<(usize, usize, Vec<BlockRange>)> = Vec::new();
        for (index, node) in self.nodes.iter().enumerate() {
            let Some(Target::Node(target)) = node.control.target else {
                continue;
            };
            // A branch may leave several nested blocks at once, and each one
            // has its own axes to put back. Innermost first: a later `enter`
            // is a deeper block.
            let mut left: Vec<BlockRange> = self
                .blocks
                .iter()
                .filter(|block| {
                    let inside = |position: usize| position > block.enter && position < block.exit;
                    inside(index) && !inside(target)
                })
                .copied()
                .collect();
            if left.is_empty() {
                continue;
            }
            left.sort_by_key(|block| std::cmp::Reverse(block.enter));
            splits.push((index, target, left));
        }
        for (index, target, blocks) in splits {
            let mut next = target;
            // Built from the target backwards, so the first pad the branch
            // reaches is the innermost block's.
            for block in blocks.iter().rev() {
                let pad = self.nodes.len();
                self.nodes.push(Node {
                    edge_split: true,
                    statement: self.nodes[index].statement,
                    span: self.nodes[index].span,
                    transfer: Transfer::Restore {
                        enter: block.enter,
                        axes: block.axes,
                    },
                    control: Control {
                        fall_through: false,
                        target: Some(Target::Node(next)),
                        returns: false,
                    },
                    label: None,
                    label_is_global: false,
                    last_of_statement: false,
                });
                next = pad;
            }
            self.nodes[index].control.target = Some(Target::Node(next));
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
            edge_split: false,
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
                self.blocks.push(BlockRange { enter, exit, axes });
            }
            // A `nocross` wrapper is a placement request, not control flow.
            StatementKind::Nocross(body) => {
                self.push(id, span, Transfer::Identity, Control::fall_through());
                self.block(body);
            }
            StatementKind::Call(call) => {
                let callee = self.lookup_callee(&call.target.text);
                self.call(id, span, callee, true);
            }
            StatementKind::BareCall(call) => {
                let callee = self.lookup_callee(&call.target.text);
                self.call(id, span, callee, true);
            }
            // `return` leaves the widths alone; `return_i` is `rti`, which
            // pulls `P` off the stack and with it both width bits. The
            // difference is not cosmetic: a caller reads the state at the
            // return site as the function's exit, so a `return_i` that
            // claimed the tracked widths would promise an interrupt handler's
            // callers something the CPU does not do.
            StatementKind::Return(kind) => {
                let transfer = match kind {
                    ast::ReturnKind::Subroutine => Transfer::Instruction {
                        effect: crate::effects::instruction_effects(
                            Mnemonic::Rts,
                            crate::isa::AddressingMode::Stack,
                            None,
                        )
                        .width,
                        exchanges_emulation: false,
                    },
                    // `return_i` is `RTI`, which restores what it interrupted.
                    ast::ReturnKind::Interrupt => restores_the_interrupted_state(),
                };
                self.push(
                    id,
                    span,
                    transfer,
                    Control {
                        fall_through: false,
                        target: None,
                        returns: true,
                    },
                );
            }
            StatementKind::Goto(goto) => {
                let target = self.goto_target(goto);
                self.count_target(target);
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
                // are flattened too rather than silently left stateless —
                // including one inside a `nocross { … }` group, which is a
                // brace around ordinary entries and not an entry itself.
                for entry in crate::ast::flat_data_entries(&data.entries) {
                    if let DataEntryKind::Code { body, .. } = &entry.kind {
                        self.block(body);
                    }
                }
            }
            StatementKind::Hla(hla) => self.hla(statement, hla),
            // A placement directive moves where the bytes land and touches
            // no register; a reserved illegal-opcode form is refused before
            // anything is emitted. Both fall through carrying the widths
            // they were handed, like the two above them.
            StatementKind::Align(_)
            | StatementKind::Address(_)
            | StatementKind::Reserved(_)
            | StatementKind::Preprocessor(_)
            | StatementKind::Error => {
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

    /// What an HLA statement does to the widths is what its expansion does.
    ///
    /// Running the expansion here — rather than re-deriving a rule per
    /// statement family — is what keeps `p??` destroying the widths because
    /// `PLP` destroys them, without this file knowing anything about either.
    /// Every family sub-plans E2 and E3 opened falls through; the branching
    /// ones are statement kinds of their own, already flattened above.
    fn hla(&mut self, statement: &Statement, hla: &ast::HlaStatement) {
        let id = statement.id;
        let span = statement.span;
        // As in `native` below: this run's diagnostics are dropped, because
        // the analysis walk reports them for real.
        let mut resolver = Resolver::new(self.symbols, self.eval).in_function(self.function);
        let Some(steps) = crate::hla::statement(&mut resolver, hla, span) else {
            // Already rejected, so it will never be emitted; claiming it
            // destroys the widths would only add noise to a program that has
            // already failed.
            self.push(id, span, Transfer::Identity, Control::fall_through());
            return;
        };
        let mut effect = WidthEffect::Unchanged;
        for step in &steps {
            let instruction = step.instruction();
            let effects = instruction.effects();
            self.observe(effects);
            match effects.width {
                WidthEffect::Unchanged => {}
                changed => effect = changed,
            }
        }
        self.push(
            id,
            span,
            Transfer::Instruction {
                effect,
                // No HLA spelling reaches `XCE`; leaving emulation is written
                // with the mnemonic.
                exchanges_emulation: false,
            },
            Control::fall_through(),
        );
    }

    fn native(&mut self, statement: &Statement, native: &ast::NativeInstruction) {
        let id = statement.id;
        let span = statement.span;
        // Classification is C's, and running it again here rather than
        // re-deriving the mode keeps one answer about which mode an operand
        // selects. This run's diagnostics are dropped: the analysis walk
        // reports them for real.
        let mut resolver = Resolver::new(self.symbols, self.eval).in_function(self.function);
        let classified = resolver.classify(native);
        let Some(classified) = classified else {
            // The instruction is already rejected, so it will never be
            // emitted; claiming it destroys the widths would only add noise
            // to a program that has already failed.
            self.push(id, span, Transfer::Identity, Control::fall_through());
            return;
        };
        let instruction = classified.instruction();
        let effects = instruction.effects();
        self.observe(effects);
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
                self.call(id, span, callee, false);
            }
            // A `jmp`/`jml` whose operand names a *function* leaves this
            // function through that one, so it is a return path carrying that
            // function's exit rather than an edge in this graph.
            ControlEffect::Jump
                if symbol.is_some_and(|symbol| {
                    matches!(self.symbols.kind(symbol), SymbolKind::Function { .. })
                }) =>
            {
                if let Some(callee) = symbol
                    && !self.callees.contains(&callee)
                {
                    self.callees.push(callee);
                }
                self.push(
                    id,
                    span,
                    Transfer::Call {
                        callee: symbol,
                        bridged: false,
                    },
                    Control {
                        fall_through: false,
                        target: None,
                        returns: true,
                    },
                );
            }
            ControlEffect::Branch | ControlEffect::Jump => {
                let target = symbol.map_or(Target::Unknown, |symbol| self.named_target(symbol));
                self.count_target(target);
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
                // `RTI` pops `P`, and with it the widths — so a return
                // through it hands back exactly the state the interrupted
                // code had, whatever the body did in between. That is
                // `Preserve` and not "cannot say": the instruction's whole
                // job is the restore, and an interrupt handler that saves
                // and restores its registers is the idiom, not a hazard.
                // Node 0's incoming fact is the entry state, so the block
                // restore already written for scoped width blocks says this
                // exactly.
                let transfer = if instruction.mnemonic == Mnemonic::Rti {
                    restores_the_interrupted_state()
                } else {
                    Transfer::Instruction {
                        effect: effects.width,
                        exchanges_emulation: instruction.mnemonic == Mnemonic::Xce,
                    }
                };
                self.push(
                    id,
                    span,
                    transfer,
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

    fn call(&mut self, statement: StmtId, span: Span, callee: Option<SymbolId>, bridged: bool) {
        match callee {
            Some(callee) => {
                if !self.callees.contains(&callee) {
                    self.callees.push(callee);
                }
            }
            // A call K816 cannot name — through a variable holding an
            // address, or an unresolved indirect target — reaches code that
            // may read either width flag. Recording it as an observation of
            // both axes is what keeps the optimizer's transitive answer
            // sound for a body whose call edges do not list it.
            None => self.needs = self.needs.union(WidthNeeds::opaque()),
        }
        self.push(
            statement,
            span,
            Transfer::Call { callee, bridged },
            Control::fall_through(),
        );
    }

    /// The section a use site names, `func` or `inline` alike.
    ///
    /// An `inline` is modelled as a call: the use site bridges to its entry
    /// contract and adopts its exit, exactly as for a `func`. That is what
    /// makes "an `inline` body is analyzed once, at its declaration" sound —
    /// the widths inside the expansion are the ones its own contract
    /// establishes, and the bridge is what gets the use site there.
    fn lookup_callee(&self, name: &str) -> Option<SymbolId> {
        self.symbols.lookup(name).filter(|symbol| {
            matches!(
                self.symbols.kind(*symbol),
                SymbolKind::Function { .. } | SymbolKind::Inline
            )
        })
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
            .map_or(Target::Unknown, |symbol| self.named_target(symbol))
    }

    /// What a transfer to `symbol` is, from this function's point of view.
    fn named_target(&self, symbol: SymbolId) -> Target {
        match self.symbols.kind(symbol) {
            SymbolKind::Label { .. } => Target::Label(symbol),
            SymbolKind::Function { .. } => Target::External,
            _ => Target::Unknown,
        }
    }

    /// Counts a transfer naming a label, for [`confined_labels`].
    fn count_target(&mut self, target: Target) {
        if let Target::Label(symbol) = target {
            *self.target_uses.entry(symbol).or_default() += 1;
        }
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
        | Operand::BlockMove { .. }
        // A generated target names no symbol; the model's own structure is
        // where its edges come from.
        | Operand::Anchor(_) => None,
    }
}

/// The worklist solver.
fn solve(
    graph: Graph,
    entry: ModeState,
    callees: &BTreeMap<SymbolId, CalleeContract>,
) -> (WidthAnalysis, Vec<Diagnostic>) {
    let nodes = &graph.nodes;
    let count = nodes.len();
    // The body ends where the split edges begin. Those nodes go on the end
    // and only their branches reach them, so the body's last node falls off
    // the end of the function — its implicit return — and not into a pad.
    let body_end = nodes
        .iter()
        .position(|node| node.edge_split)
        .unwrap_or(count);
    let mut predecessors: Vec<Vec<usize>> = vec![Vec::new(); count];
    for (index, node) in nodes.iter().enumerate() {
        if node.control.fall_through && index + 1 < body_end {
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
    // plausible and silent — except for a local label whose address never
    // left the branches that name it, which no unnamed transfer can reach.
    let opaque_labels: Vec<bool> = nodes
        .iter()
        .map(|node| {
            node.label.is_some()
                && (node.label_is_global
                    || graph.opaque_transfer
                        && node
                            .label
                            .is_none_or(|label| !graph.confined.contains(&label)))
        })
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
        let produced = apply(&nodes[index].transfer, incoming, &r#in, callees);
        if incoming == r#in[index] && produced == out[index] {
            continue;
        }
        r#in[index] = incoming;
        out[index] = produced;
        if nodes[index].control.fall_through && index + 1 < body_end {
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
            || (node.control.fall_through
                && index + 1 == body_end
                && node.control.target.is_none());
        if leaves {
            analysis.returns.push(ReturnSite {
                state: out[index].mode,
                touched: out[index].touched,
                span: node.span,
            });
        }
    }
    let mut diagnostics = graph.diagnostics;
    record_bridges(
        &mut analysis,
        nodes,
        &r#in,
        &graph.blocks,
        callees,
        &mut diagnostics,
    );
    report_unbridged_calls(nodes, &r#in, callees, &mut diagnostics);
    (analysis, diagnostics)
}

/// One node's transfer function.
fn apply(
    transfer: &Transfer,
    incoming: Fact,
    r#in: &[Fact],
    callees: &BTreeMap<SymbolId, CalleeContract>,
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
            // An annotation is a user assertion, so it establishes its width
            // outright rather than through the emulation state — that is the
            // documented escape hatch for the `xce`-then-widen pattern.
            //
            // A *16-bit* assertion says something about emulation mode as
            // well: `doc/w65c816s.md` has the M and X flags forced to one in
            // emulation mode, so a 16-bit register can only exist in native
            // mode, and asserting one asserts that. Without this the escape
            // hatch opens and closes again immediately — the annotation is
            // believed and the very next `rep` is not.
            //
            // An 8-bit assertion says nothing: 8-bit is what emulation mode
            // forces, so `@a8` is equally true either way and leaves the
            // emulation state exactly as unknown as it found it.
            //
            // And the escape hatch only opens where the flag is *unknown*.
            // Under a known `Emulation` the build has stated what machine
            // this is, and an annotation inside a body does not get to
            // overrule the command line — `record_bridges` refuses the
            // widening with `K816-E0163`. The state is left alone here so
            // that the refusal is not followed by a second, contradictory
            // reading of every `rep` below it.
            if fact.mode.e == EmulationState::Unknown
                && (axes.a == Some(RegisterWidth::W16) || axes.i == Some(RegisterWidth::W16))
            {
                fact.mode.e = EmulationState::Native;
            }
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
        Transfer::Call { callee, bridged } => {
            let contract = callee
                .and_then(|callee| callees.get(&callee).copied())
                .unwrap_or_else(|| CalleeContract {
                    entry: WidthContract::default(),
                    exit: ExitContract::preserving(),
                    // A callee K816 cannot see may read either width flag.
                    observes: WidthNeeds::opaque(),
                });
            let mut fact = incoming;
            // A managed call establishes the callee's entry contract first;
            // the exit then applies to the state the callee actually started
            // from.
            if bridged {
                if let Some(width) = contract.entry.a {
                    fact.mode.a = AxisState::Known(width);
                    fact.touched.a = true;
                }
                if let Some(width) = contract.entry.i {
                    fact.mode.i = AxisState::Known(width);
                    fact.touched.i = true;
                }
            }
            // The exit contract describes the emulation flag too, so a
            // callee that runs `clc; xce` no longer leaves the caller
            // believing a machine it is not on. A callee nobody classified
            // carries `EmulationExit::Assumed`, which is where the older
            // rule lives: `Native` survives an unseen call and `Emulation`
            // does not.
            fact.mode = contract.exit.apply(fact.mode);
            fact.touched.a |= matches!(contract.exit.a, AxisExit::Fixed(_));
            fact.touched.i |= matches!(contract.exit.i, AxisExit::Fixed(_));
            fact.touched.e |= contract.exit.e != EmulationExit::Preserve;
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
///
/// Under `Emulation` the answer is *known* in every case, which is what makes
/// that state stronger than `Unknown` rather than a synonym for it: a
/// widening transition establishes 8 bits (`believe`), and so does a
/// transition the effect table cannot describe at all — `RTI` and `PLP`
/// restore a status byte, and no status byte can widen a register in
/// emulation mode.
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
        // `RTI` and `PLP` restore a status byte the model cannot read, and
        // `XCE` decides the emulation flag from the carry. In *emulation*
        // mode the first two cannot widen anything: M and X are forced to
        // one, so whatever byte comes back off the stack, both registers are
        // still eight bits wide. `XCE` is the one that really is unknown
        // afterwards, and it says so through `exchanges_emulation` below —
        // which is read *before* this, so the exchange does not get to
        // benefit from the state it is leaving.
        WidthEffect::Unknown
            if fact.mode.e == EmulationState::Emulation && !exchanges_emulation =>
        {
            fact.mode.a = AxisState::Known(RegisterWidth::W8);
            fact.mode.i = AxisState::Known(RegisterWidth::W8);
            fact.touched = Touched {
                a: true,
                i: true,
                e: fact.touched.e,
            };
        }
        WidthEffect::Unknown => {
            fact.mode.a = AxisState::Unknown;
            fact.mode.i = AxisState::Unknown;
            fact.touched = Touched {
                a: true,
                i: true,
                e: fact.touched.e,
            };
        }
    }
    if exchanges_emulation {
        fact.mode.e = EmulationState::Unknown;
        fact.touched.e = true;
    }
    fact
}

/// What an instruction's *stated* width effect establishes, read through the
/// emulation state.
///
/// Eight bits is eight bits everywhere. Sixteen depends on what the CPU will
/// do with the `REP`: in native mode it widens, in emulation mode M and X are
/// forced to one so the register stays eight bits wide
/// (`doc/w65c816s.md`), and after an `XCE` K816 knows neither.
///
/// The emulation answer is the exact one rather than `Unknown` on purpose:
/// under `--entry emulation` a program that never leaves emulation mode is
/// fully sized, and a stray `rep` in it is not an error but a no-op — which is
/// what the machine does with it.
///
/// `emit_ca65` calls this too rather than reading the effect table directly,
/// so that what ca65 is told and what the model believes are one answer. They
/// disagreed for exactly as long as `Emulation` existed and this was private:
/// the model said the accumulator was still 8-bit after a `rep` and emission
/// wrote `.a16`.
pub(crate) fn believe(width: RegisterWidth, emulation: EmulationState) -> AxisState {
    match (width, emulation) {
        (RegisterWidth::W8, _) | (RegisterWidth::W16, EmulationState::Native) => {
            AxisState::Known(width)
        }
        (RegisterWidth::W16, EmulationState::Emulation) => AxisState::Known(RegisterWidth::W8),
        (RegisterWidth::W16, EmulationState::Unknown) => AxisState::Unknown,
    }
}

/// Records the `REP`/`SEP` the model itself contributes, once the states are
/// final: a bridge exists exactly where the tracked state differs from the
/// width being established, so an already-satisfied annotation emits nothing.
/// A native `jsr`/`jsl` is transparent — K816 emits what the user wrote —
/// but a callee whose declared entry contract the tracked width does not
/// satisfy will run with the wrong widths, so the mismatch is named.
///
/// The managed `call NAME` bridges instead; this is the escape hatch, and
/// the fix it offers is to stop using it.
fn report_unbridged_calls(
    nodes: &[Node],
    r#in: &[Fact],
    callees: &BTreeMap<SymbolId, CalleeContract>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    for (index, node) in nodes.iter().enumerate() {
        let Transfer::Call {
            callee: Some(callee),
            bridged: false,
        } = node.transfer
        else {
            continue;
        };
        if r#in[index].is_bottom() {
            continue;
        }
        let Some(contract) = callees.get(&callee) else {
            continue;
        };
        let state = r#in[index].mode;
        for (axis, required, tracked) in [
            (Axis::Accumulator, contract.entry.a, state.a),
            (Axis::Index, contract.entry.i, state.i),
        ] {
            let Some(required) = required else { continue };
            if tracked == AxisState::Known(required) {
                continue;
            }
            diagnostics.push(unbridged_native_call(node.span, axis, required));
        }
    }
}

fn unbridged_native_call(span: Span, axis: Axis, required: RegisterWidth) -> Diagnostic {
    Diagnostic::warning(
        DiagnosticCode::UNBRIDGED_NATIVE_CALL,
        format!(
            "this call reaches a function that expects {}, and the tracked width is not that",
            axis.annotation(required)
        ),
        Label::new(span, "a native call is not bridged"),
    )
    .with_note(
        "`jsr` is the escape hatch: K816 emits raw native code as written, so it \
         establishes nothing before transferring",
    )
    .with_help(format!(
        "write `call` instead, which bridges, or establish {} before this line",
        axis.annotation(required)
    ))
    .with_docs("doc/register-width-aware-syntax.md#call-site-bridging")
}

/// Sixteen bits asked for where the machine is known to be in emulation mode.
///
/// The distinction from `K816-E0104` is the whole message: that one says the
/// width could not be determined, this one says it *was* determined and the
/// answer is eight. A `REP` here assembles as an instruction and executes as
/// nothing.
fn emulation_width(span: Span, axis: Axis, reason: Reason) -> Diagnostic {
    let wide = axis.annotation(RegisterWidth::W16);
    let narrow = axis.annotation(RegisterWidth::W8);
    let (what, help) = match reason {
        Reason::CallBridge(_) => (
            "this call reaches a function that expects it",
            format!(
                "give the callee a `{narrow}` contract, or leave emulation mode with \
                 `clc; xce` before the call \u{2014} an annotation there is the escape hatch \
                 once the flag is no longer known"
            ),
        ),
        _ => (
            "this asks for a 16-bit register",
            format!(
                "drop to `{narrow}`, or leave emulation mode with `clc; xce` first \u{2014} \
                 after an `xce` the flag is no longer known and `{wide}` is the documented \
                 assertion for exactly that"
            ),
        ),
    };
    Diagnostic::error(
        DiagnosticCode::EMULATION_WIDTH,
        format!("`{wide}` cannot be established while the machine is in emulation mode"),
        Label::new(span, what),
    )
    .with_note(
        "the M and X flags are always set in emulation mode, so the `rep` K816 would emit \
         here executes and changes nothing, leaving the code below assembled 16 bits wide \
         and running 8 bits wide",
    )
    .with_help(help)
    .with_docs("doc/register-width-aware-syntax.md#the-entry-point")
}

fn record_bridges(
    analysis: &mut WidthAnalysis,
    nodes: &[Node],
    r#in: &[Fact],
    blocks: &[BlockRange],
    callees: &BTreeMap<SymbolId, CalleeContract>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    for (index, node) in nodes.iter().enumerate() {
        if r#in[index].is_bottom() {
            continue; // Unreachable: the model makes no claim here.
        }
        let (axes, anchor, reason) = match &node.transfer {
            Transfer::Set(axes) => {
                // A width established right after a label *by the label* —
                // decision 6's pending annotation, which `label_statement`
                // files under the label's own statement — is the label
                // bridge, and its adjacency to the label operation is what
                // marks it fixed for the optimizer.
                //
                // A mode statement the author wrote *after* the label is not
                // that. It is filed under its own statement, and lowering
                // emits an after-label bridge only while lowering a label,
                // so anchoring it there dropped it: `entry: @a16` lost its
                // `rep` while the immediates after it were still sized
                // sixteen bits wide.
                if index > 0
                    && nodes[index - 1].label.is_some()
                    && nodes[index - 1].statement == node.statement
                {
                    (*axes, BridgeAnchor::AfterLabel, Reason::LabelBridge)
                } else if blocks.iter().any(|block| block.enter == index) {
                    (*axes, BridgeAnchor::Before, Reason::BlockEnter)
                } else {
                    (*axes, BridgeAnchor::Before, Reason::ModeSet)
                }
            }
            Transfer::Restore { enter, axes } => {
                let saved = r#in[*enter];
                if saved.is_bottom() {
                    continue;
                }
                let anchor = if node.edge_split {
                    BridgeAnchor::TakenEdge
                } else {
                    BridgeAnchor::After
                };
                (
                    WidthContract {
                        a: axes.a.and(saved.mode.a.width()),
                        i: axes.i.and(saved.mode.i.width()),
                    },
                    anchor,
                    Reason::BlockRestore,
                )
            }
            // A managed call establishes the callee's entry contract before
            // transferring: no bridge for an axis the callee leaves
            // unspecified, none for one the tracker already agrees with, one
            // where it differs or is unknown.
            Transfer::Call { callee, bridged } => {
                let contract = callee.and_then(|callee| callees.get(&callee));
                if contract.is_none() {
                    // An invisible callee is annotated at the call site
                    // rather than warned about.
                    analysis.assumed_calls.insert(node.statement);
                }
                if !bridged {
                    continue;
                }
                let entry = contract.map(|contract| contract.entry).unwrap_or_default();
                let Some(callee) = *callee else { continue };
                (entry, BridgeAnchor::Before, Reason::CallBridge(callee))
            }
            Transfer::Identity | Transfer::Instruction { .. } => continue,
        };
        let state = r#in[index].mode;
        let mut bridges = Vec::new();
        for (axis, width, mask, current) in [
            (Axis::Accumulator, axes.a, P_MASK_M, state.a),
            (Axis::Index, axes.i, P_MASK_X, state.i),
        ] {
            let Some(width) = width else { continue };
            if current == AxisState::Known(width) {
                continue; // Already established; there is nothing to emit.
            }
            // This is the one place K816 *authors* a `REP`/`SEP` of its own,
            // and in a known emulation state a widening one cannot work: the
            // CPU forces M and X to one, so the instruction runs and changes
            // nothing while everything below it is assembled sixteen bits
            // wide. Every route to a widening bridge passes here — a scoped
            // `@a16` block, a bare annotation, a label bridge, a block
            // restore, and a managed `call` to a wide callee — so one check
            // covers them instead of one per `Transfer` arm.
            if width == RegisterWidth::W16 && state.e == EmulationState::Emulation {
                diagnostics.push(emulation_width(node.span, axis, reason));
                continue;
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
    exit.e = classify_emulation(&analysis.returns);
    ExitClassification {
        exit,
        assumed,
        divergent,
    }
}

/// What the reachable returns say about the emulation flag.
///
/// The same shape as [`classify_axis`] and for the same reasons, with one
/// difference worth stating: there is no divergence to report. A width that
/// differs between two returns is a mistake an author can usually see and fix,
/// and `K816-W0121` names both sites; an emulation flag that differs is an
/// `XCE` on one path and not another, which is `xce`'s ordinary meaning and
/// not something to warn about (`plans/06-compatibility.md` decision 37).
fn classify_emulation(sites: &[ReturnSite]) -> EmulationExit {
    // No reachable return — a body that ends in `stp` or loops forever — has
    // no exit for a caller to observe.
    if sites.is_empty() {
        return EmulationExit::Preserve;
    }
    // Nothing exchanged the flag on any path, so it comes back as the caller
    // had it. This is the case almost every function is in, and the one that
    // keeps the change from costing anything.
    if sites.iter().all(|site| !site.touched.e) {
        return EmulationExit::Preserve;
    }
    // Some path exchanged it, so an untouched path alongside is not `Fixed`:
    // what the caller sees would depend on which one ran.
    if sites.iter().any(|site| !site.touched.e) {
        return EmulationExit::Unknown;
    }
    let mut fixed: Option<EmulationState> = None;
    for site in sites {
        // `Fixed(Unknown)` would be a promise of nothing, spelled as a
        // promise.
        if site.state.e == EmulationState::Unknown {
            return EmulationExit::Unknown;
        }
        match fixed {
            Some(first) if first != site.state.e => return EmulationExit::Unknown,
            Some(_) => {}
            None => fixed = Some(site.state.e),
        }
    }
    fixed.map_or(EmulationExit::Unknown, EmulationExit::Fixed)
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

/// Condenses a directed graph into its strongly connected components, in
/// reverse topological order — every component's successors come before it.
///
/// Tarjan's algorithm, which emits a component only once everything it can
/// reach has been emitted, so the order is the one exit classification needs
/// without a separate topological sort. Exit classification depends on the
/// order rather than on convergence: `AxisExit` has no order that would make
/// an iterate-until-stable scheme monotone, so an iterating implementation
/// would converge by luck on the inputs it was tested on.
pub fn strongly_connected_components(adjacency: &[Vec<usize>]) -> Vec<Vec<usize>> {
    struct Tarjan<'a> {
        adjacency: &'a [Vec<usize>],
        /// Discovery index per node, `None` until visited.
        order: Vec<Option<usize>>,
        /// The lowest discovery index reachable from the node's subtree.
        low: Vec<usize>,
        on_stack: Vec<bool>,
        stack: Vec<usize>,
        next: usize,
        components: Vec<Vec<usize>>,
    }

    impl Tarjan<'_> {
        fn visit(&mut self, node: usize) {
            self.order[node] = Some(self.next);
            self.low[node] = self.next;
            self.next += 1;
            self.stack.push(node);
            self.on_stack[node] = true;

            for edge in 0..self.adjacency[node].len() {
                let target = self.adjacency[node][edge];
                match self.order[target] {
                    None => {
                        self.visit(target);
                        self.low[node] = self.low[node].min(self.low[target]);
                    }
                    Some(discovered) if self.on_stack[target] => {
                        self.low[node] = self.low[node].min(discovered);
                    }
                    Some(_) => {}
                }
            }

            if self.low[node] == self.order[node].expect("visited") {
                let mut component = Vec::new();
                while let Some(member) = self.stack.pop() {
                    self.on_stack[member] = false;
                    component.push(member);
                    if member == node {
                        break;
                    }
                }
                self.components.push(component);
            }
        }
    }

    let mut tarjan = Tarjan {
        adjacency,
        order: vec![None; adjacency.len()],
        low: vec![0; adjacency.len()],
        on_stack: vec![false; adjacency.len()],
        stack: Vec::new(),
        next: 0,
        components: Vec::new(),
    };
    for node in 0..adjacency.len() {
        if tarjan.order[node].is_none() {
            tarjan.visit(node);
        }
    }
    tarjan.components
}
