/*****************************************************************************/
/*                                                                           */
/*                                optimize.rs                                */
/*                                                                           */
/*              Effect-justified optimization of KOp streams                 */
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

//! The effect-aware optimizer.
//!
//! Every rewrite is justified by explicit CPU effects from `effects.rs`;
//! unknown effects inhibit optimization. Native and HLA-generated operations
//! receive identical treatment — passes see only [`KOp`]s.
//!
//! # The fixed pass sequence
//!
//! `plans/implementation.md` §6 phase 9 fixes both the list and its order, and
//! the order is the runtime order. There is no fixpoint over the sequence, no
//! pass ordering computed at run time, and no per-pass flag: the only switch
//! is `--no-opt`. Where a pass creates an opportunity for an earlier one, that
//! pass takes it itself.
//!
//! # What the optimizer may not become
//!
//! `src/width.rs` is the single width authority. F rewrites operations; it
//! never re-derives a width. Every width fact a pass needs is already in the
//! stream as a [`KOpKind::ModeAssumption`], on the function as a
//! [`crate::op::WidthContract`] or [`ExitContract`], or in the workspace as a
//! [`crate::width::CalleeContract`]. A pass that computes "the accumulator is
//! 16 bits here" from anything else has become the second width authority,
//! which is the failure `width.rs` exists to prevent.

use std::collections::{BTreeMap, BTreeSet, HashMap};

use crate::effects::{ControlEffect, CpuState, Effects, FlagSet, MemoryEffect, WidthEffect};
use crate::isa::{Mnemonic, RegisterWidth};
use crate::linkage::Linkage;
use crate::op::{
    AnchorId, AxisExit, AxisState, ExitContract, KOp, KOpKind, KProgram, ModeState, Operand, Reason,
};
use crate::semantic::{SemanticModel, WidthNeeds};
use crate::source::FileId;
use crate::symbol::{SymbolId, SymbolTable};
use crate::value::KExpr;
use crate::width::{Axis, CalleeContract};

/// One pass of the fixed sequence.
///
/// The discriminant order is the runtime order, and [`Pass::name`] is the
/// spelling `emit-ir --after-opt --explain` prints and the tests match on.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Pass {
    /// 1 — clear dead M/X bits from a `REP`/`SEP` mask.
    EliminateDeadModes,
    /// 2 — merge an adjacent run of `REP`/`SEP` into the fewest instructions
    /// with the same composed status transform.
    FoldModes,
    /// 3 — delete an instruction whose whole `writes` set is dead.
    RemoveOverwritten,
    /// 4 — delete a load of a value the register provably already holds.
    CoalesceLoads,
    /// 5 — delete a flag operation whose flag provably already holds that
    /// value.
    CollapseFlags,
    /// 6 — `sta m` → `stz m` where A is known zero.
    ZeroStores,
    /// 8 — delete compiler-generated operations no edge reaches.
    ///
    /// Item 7 of phase 9's list, "rewrite safe tail calls", has no variant
    /// because the safe set K816 can identify is empty; see [`optimize`].
    Unreachable,
    /// Shrink a long-form branch E emitted conservatively, once the
    /// byte-removing passes have run and the distance is known.
    ShrinkBranches,
}

impl Pass {
    /// The name in the log and in `--explain`.
    pub fn name(self) -> &'static str {
        match self {
            Self::EliminateDeadModes => "eliminate-dead-modes",
            Self::FoldModes => "fold-modes",
            Self::RemoveOverwritten => "remove-overwritten",
            Self::CoalesceLoads => "coalesce-loads",
            Self::CollapseFlags => "collapse-flags",
            Self::ZeroStores => "zero-stores",
            Self::Unreachable => "unreachable",
            Self::ShrinkBranches => "shrink-branches",
        }
    }
}

/// Where a change happened: the function, and the operation's index in that
/// function's stream as the pass found it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Site {
    pub function: SymbolId,
    /// Index within the function's stream at the start of the pass that made
    /// the change — a position a reader can find in `emit-ir` output taken
    /// before that pass, which is the only stream a position can name.
    pub index: usize,
}

/// One rewrite, with what justified it.
///
/// A log of *changes* rather than of removals: phase 9's first success
/// criterion is that every rewrite test asserts the justification, and a
/// record that can only express deletion cannot carry a mask-bit strip, a
/// fold, or a shrunk branch.
#[derive(Clone, Debug)]
pub struct Change {
    pub pass: Pass,
    pub site: Site,
    /// The operations as they were, in stream order.
    pub before: Vec<KOp>,
    /// What replaced them. Empty when they were removed.
    pub after: Vec<KOp>,
    /// The operation whose own effect completed the justification, where one
    /// operation is what makes the change legal — the write that overwrote a
    /// dead one, the load a coalesced one duplicates.
    pub witness: Option<KOp>,
    /// The justification, in the words `--explain` prints.
    pub justification: String,
}

impl Change {
    /// The effects of each instruction in [`Self::before`], in order.
    ///
    /// Derived rather than stored, exactly as `effects.rs` derives them from
    /// the `(mnemonic, mode)` pair: an effect copied into a record is an
    /// effect that can go stale against the table it came from.
    pub fn effects(&self) -> Vec<Effects> {
        self.before.iter().filter_map(effects_of).collect()
    }
}

/// What the optimizer did, for inspection and tests.
#[derive(Debug, Default)]
pub struct OptimizationLog {
    pub changes: Vec<Change>,
}

impl OptimizationLog {
    pub fn is_empty(&self) -> bool {
        self.changes.is_empty()
    }

    /// The changes one pass made, in the order it made them.
    pub fn by_pass(&self, pass: Pass) -> impl Iterator<Item = &Change> {
        self.changes
            .iter()
            .filter(move |change| change.pass == pass)
    }

    /// The `emit-ir --after-opt --explain` rendering: one line per change with
    /// its position, one indented line with the recorded justification.
    pub fn explain(&self, symbols: &SymbolTable) -> String {
        if self.changes.is_empty() {
            return String::from("optimizer: no changes\n");
        }
        let mut out = String::from("optimizer:\n");
        for change in &self.changes {
            let position = format!(
                "{}+{}",
                symbols.name(change.site.function),
                change.site.index
            );
            let before = change
                .before
                .iter()
                .map(|op| crate::op::dump_op(op, symbols))
                .collect::<Vec<_>>()
                .join(", ");
            let after = if change.after.is_empty() {
                String::from("removed")
            } else {
                format!(
                    "-> {}",
                    change
                        .after
                        .iter()
                        .map(|op| crate::op::dump_op(op, symbols))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            };
            out.push_str(&format!(
                "  {:<21} {:<8} {:<19} {}\n",
                change.pass.name(),
                position,
                before,
                after
            ));
            out.push_str(&format!("      {}\n", change.justification));
        }
        out
    }
}

/// What the optimizer needs to know beyond the operation streams it rewrites.
///
/// Every field is a fact some earlier sub-plan already computed. F adds no
/// analysis to `semantic.rs` or `linkage.rs`; it reads what they concluded.
pub struct Context<'a> {
    pub model: &'a SemanticModel,
    pub linkage: &'a Linkage,
    /// The unit being optimized. Which names cross a boundary is a question
    /// about *this* file, so `Linkage::referenced_elsewhere` needs it.
    pub file: FileId,
}

/// Runs the fixed pass sequence over `program`.
///
/// The sequence is `plans/implementation.md` §6 phase 9's, in its order, with
/// one departure recorded rather than taken quietly: **item 7, "rewrite safe
/// tail calls", rewrites nothing.** `jsr f` + `rts` → `jmp f` leaves `f`
/// running with the stack two bytes shallower and with a live slot holding the
/// *caller's* return address, and licensing it needs a fact K816 cannot have —
/// that `f` never reads that slot. Opcode checks close only the S-derived
/// routes, since the stack is ordinary bank-zero memory that `lda $01FE`
/// reaches without naming `S`; alias analysis would have to refuse every
/// callee that reads memory through anything but a compile-time-known
/// non-stack address. So the safe set is empty, and
/// `tests/golden/opt-barriers.k816` carries a `jsr`/`rts` pair asserting that
/// nothing happens to it. `doc/design.md` §8 records the departure beside the
/// pass list; `plans/05-optimizer.md` decision 17 names the three language
/// decisions that would unlock it, all of them sub-plan G's.
pub fn optimize(program: &mut KProgram, context: &Context<'_>) -> OptimizationLog {
    let mut log = OptimizationLog::default();
    let symbols = &context.model.symbols;
    let callees = context.callee_contracts();
    let observed_nowhere = context.observed_nowhere();
    let info: BTreeMap<SymbolId, &crate::semantic::FunctionInfo> = context
        .model
        .functions
        .iter()
        .map(|info| (info.symbol, info))
        .collect();
    for function in program.functions_mut() {
        let unit = Unit {
            function: function.symbol,
            external_labels: &context.model.externally_reachable,
            symbols,
            callees: &callees,
            exit_is_published: info
                .get(&function.symbol)
                .is_none_or(|info| info.entry.is_none()),
            observed_nowhere,
        };
        eliminate_dead_mode_changes(&unit, &mut function.ops, &mut log);
        remove_overwritten_register_values(&unit, &mut function.ops, &mut log);
    }
    log
}

impl Context<'_> {
    /// Every callee this unit can see, keyed by this unit's symbol ids.
    ///
    /// The workspace answer wherever the linkage has one — it saw the callers
    /// and callees this unit cannot — and this unit's own otherwise, which is
    /// what a file compiled with no `--unit` siblings has.
    fn callee_contracts(&self) -> BTreeMap<SymbolId, CalleeContract> {
        let mut contracts: BTreeMap<SymbolId, CalleeContract> = self
            .model
            .functions
            .iter()
            .map(|info| {
                (
                    info.symbol,
                    CalleeContract {
                        entry: info.contract,
                        exit: info.exit,
                        observes: info.observes,
                    },
                )
            })
            .collect();
        for (id, symbol) in self.model.symbols.iter() {
            if let Some(contract) = self.linkage.contract(&symbol.name) {
                contracts.insert(id, contract);
            }
        }
        contracts
    }

    /// The axes no function anywhere in the workspace observes.
    ///
    /// The union runs over the linkage's classified contracts *and* this
    /// unit's own functions, because a unit compiled alone has the second and
    /// not always the first. A callee K816 cannot see contributes through the
    /// seed that recorded it, which is `WidthNeeds::opaque`.
    fn observed_nowhere(&self) -> WidthNeeds {
        let observed = self
            .model
            .functions
            .iter()
            .map(|info| info.observes)
            .chain(
                self.linkage
                    .definitions()
                    .filter_map(|(_, definition)| definition.contract)
                    .map(|contract| contract.observes),
            )
            .fold(WidthNeeds::default(), WidthNeeds::union);
        WidthNeeds {
            m: !observed.m,
            x: !observed.x,
        }
    }
}

/// What one function's passes need to know about the unit around it.
///
/// Every field is a fact the width model, the semantic model, or the linkage
/// already computed. F reads them; it derives no width of its own.
struct Unit<'a> {
    /// The function being rewritten, for the change log's site.
    function: SymbolId,
    /// Global labels another unit in the workspace refers to, so a block led
    /// by one is reachable from an edge K816 cannot see.
    external_labels: &'a BTreeSet<SymbolId>,
    symbols: &'a SymbolTable,
    /// What each callee this unit can see observes and promises.
    callees: &'a BTreeMap<SymbolId, CalleeContract>,
    /// True when this function's exit contract constrains someone.
    ///
    /// `main` cannot be called, and neither can an interrupt handler: their
    /// caller is the startup environment or the hardware, and no analyzed unit
    /// names them. An inferred exit that nothing consumes constrains nothing,
    /// so the operations that establish it are removable like any other dead
    /// mode operation (`plans/05-optimizer.md` decision 6). Every other
    /// function's exit is a published promise.
    exit_is_published: bool,
    /// Axes no function anywhere in the workspace observes.
    ///
    /// An axis observed nowhere is one no instruction in the program depends
    /// on, so the contracts that declare it serve nothing and every operation
    /// establishing it goes — entry setup, call bridges, block ends alike.
    /// This is the width document's "clear dead contracts" step, expressed as
    /// a property of the operation stream rather than as a mutation of a
    /// contract F has no business rewriting (decision 4).
    observed_nowhere: WidthNeeds,
}

impl Unit<'_> {
    /// What calling `callee` observes, and what it promises about the widths
    /// afterwards. A callee no analyzed unit defines observes both axes and
    /// promises nothing.
    fn callee(&self, callee: Option<SymbolId>) -> CalleeContract {
        callee
            .and_then(|callee| self.callees.get(&callee).copied())
            .unwrap_or_else(|| CalleeContract {
                entry: Default::default(),
                exit: ExitContract::preserving(),
                observes: WidthNeeds::opaque(),
            })
    }

    /// True when the axis is observed at every return from this function.
    ///
    /// A published exit contract is a promise F may not break: whatever the
    /// liveness says, the last operation establishing the axis on each path to
    /// a return has to survive, or every caller sizes its immediates against a
    /// promise the callee stopped keeping. `Fixed(w)` and `Preserve` differ in
    /// what they promise and not in whether they promise, so both seed the
    /// dataflow the same way (decision 3).
    ///
    /// **[`Self::observed_nowhere`] deliberately does not defeat this seed**,
    /// though decision 4 reads as though it should. "Observed nowhere" is a
    /// claim about the whole *program*, and what K816 can measure is the
    /// workspace it was handed. `tests/golden/contract-unit-a.k816` is one
    /// function whose entire body is the `sep #$20` that makes its declared
    /// `-> @a8` true; compiled without its sibling, no instruction in sight
    /// reads M, and the exemption applied here deletes the contract — which is
    /// exactly probe 4's miscompile, arrived at from the other direction. The
    /// exemption stays where the risk is local: a call bridge, whose removal
    /// changes this unit's call site and nothing anyone else compiled against.
    fn observed_at_exit(&self, axis: Axis) -> bool {
        let _ = axis;
        self.exit_is_published
    }
}

/// The effects of an operation, for the operations that have any.
fn effects_of(op: &KOp) -> Option<Effects> {
    match &op.kind {
        KOpKind::Instruction(instruction) => Some(instruction.effects()),
        KOpKind::Label(_)
        | KOpKind::Segment { .. }
        | KOpKind::Data(_)
        | KOpKind::Align(_)
        | KOpKind::Export(_)
        | KOpKind::Import(_)
        | KOpKind::NocrossGuard { .. }
        | KOpKind::SkipData
        | KOpKind::SkipTarget
        | KOpKind::Anchor(_)
        | KOpKind::ModeAssumption(_) => None,
    }
}

/// True when this operation is one only passes 1 and 2 may touch.
///
/// The `SEP`/`REP` read set is `ALL_P` minus the bits the folded mask writes
/// (`effects.rs`), which is what makes dead mode elimination possible at all —
/// and, on its own, what would let passes 3 and 5 delete a mode instruction,
/// since both already accept one as a removal candidate. They would do it with
/// none of the guards a mode change needs: a published exit contract, a fixed
/// label bridge, a live truncation, a mask carrying a non-width bit.
///
/// So the exclusion is stated once, here, and every pass but 1 and 2 consults
/// it. It is the other half of the effect-table correction and landed in the
/// same change, because between the two the compiler is unsound.
fn is_mode_operation(op: &KOp) -> bool {
    effects_of(op).is_some_and(|effects| effects.width != WidthEffect::Unchanged)
}

// ---------------------------------------------------------------------------
// The block view
// ---------------------------------------------------------------------------

/// One basic block: a half-open range of a function's operation stream.
#[derive(Clone, Debug)]
struct Block {
    start: usize,
    end: usize,
    successors: Vec<usize>,
}

/// Where control may go from one operation, resolved against this function.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Transfer {
    control: ControlEffect,
    /// The operation this one names, when it names one this function defines.
    target: Option<usize>,
    /// The operation names a target this function does not define — an
    /// indirect `jmp (vec)`, a computed target, a tail transfer to another
    /// function. Control goes somewhere K816 is not looking at.
    opaque: bool,
}

/// The per-function view every pass shares: basic blocks with a complete
/// successor relation, the liveness it induces, and which blocks an edge
/// reaches.
///
/// One shared view and one dataflow, not a pass framework: `doc/design.md`
/// §14 rule 9 wants two concrete users before an abstraction, and the master
/// plan forbids generic pass frameworks outright.
///
/// **Err toward more edges.** An extra successor can only make liveness larger
/// and reachability wider, which costs a missed optimization and never a wrong
/// program. A *missing* edge does the opposite, and
/// [`ControlEffect::Call`] is where that mistake is easiest to make: a call is
/// not `FallThrough`, so a splitter written from the leader rule alone leaves
/// everything after the first `jsr` looking unreachable, and the unreachable
/// pass then deletes it.
struct Flow {
    blocks: Vec<Block>,
    /// The block each operation belongs to.
    block_of: Vec<usize>,
    /// The CPU state live immediately after each operation.
    live_after: Vec<CpuState>,
    /// Blocks some edge reaches.
    reachable: Vec<bool>,
    /// Operations inside a `nocross`-guarded run, which no pass may change.
    frozen: Vec<bool>,
}

impl Flow {
    fn build(ops: &[KOp], unit: &Unit<'_>) -> Self {
        let transfers = resolve_transfers(ops);
        let blocks = split_blocks(ops, &transfers);
        let mut block_of = vec![0; ops.len()];
        for (index, block) in blocks.iter().enumerate() {
            for slot in block_of.iter_mut().take(block.end).skip(block.start) {
                *slot = index;
            }
        }
        let reachable = reachability(ops, &blocks, &transfers, unit);
        let live_after = liveness(ops, &blocks, &transfers);
        let frozen = frozen_operations(ops);
        Self {
            blocks,
            block_of,
            live_after,
            reachable,
            frozen,
        }
    }

    /// The block containing `index`.
    fn block(&self, index: usize) -> &Block {
        &self.blocks[self.block_of[index]]
    }
}

/// Resolves every operation's control transfer against this function's own
/// labels and anchors.
fn resolve_transfers(ops: &[KOp]) -> Vec<Transfer> {
    let mut label_at: HashMap<SymbolId, usize> = HashMap::new();
    let mut anchor_at: HashMap<AnchorId, usize> = HashMap::new();
    // ca65 counts every unnamed label it has seen, and the jump around inline
    // data names the next one below it — anchors and skip targets alike. The
    // block graph resolves it the same way emission spells it.
    let mut unnamed: Vec<usize> = Vec::new();
    for (index, op) in ops.iter().enumerate() {
        match &op.kind {
            KOpKind::Label(symbol) => {
                label_at.insert(*symbol, index);
            }
            KOpKind::Anchor(anchor) => {
                anchor_at.insert(*anchor, index);
                unnamed.push(index);
            }
            KOpKind::SkipTarget => unnamed.push(index),
            _ => {}
        }
    }

    ops.iter()
        .enumerate()
        .map(|(index, op)| {
            if op.kind == KOpKind::SkipData {
                let target = unnamed.iter().find(|&&site| site > index).copied();
                return Transfer {
                    control: ControlEffect::Jump,
                    target,
                    opaque: target.is_none(),
                };
            }
            let Some(effects) = effects_of(op) else {
                return Transfer {
                    control: ControlEffect::FallThrough,
                    target: None,
                    opaque: false,
                };
            };
            let KOpKind::Instruction(instruction) = &op.kind else {
                unreachable!("only an instruction has effects");
            };
            if !matches!(effects.control, ControlEffect::Branch | ControlEffect::Jump) {
                return Transfer {
                    control: effects.control,
                    target: None,
                    opaque: false,
                };
            }
            let target = match &instruction.operand {
                Some(Operand::Anchor(anchor)) => anchor_at.get(anchor).copied(),
                Some(Operand::Label(KExpr::Symbol(symbol))) => label_at.get(symbol).copied(),
                Some(Operand::Address {
                    expr: KExpr::Symbol(symbol),
                    index: None,
                    ..
                }) => label_at.get(symbol).copied(),
                _ => None,
            };
            Transfer {
                control: effects.control,
                target,
                opaque: target.is_none(),
            }
        })
        .collect()
}

/// Splits the stream into basic blocks.
///
/// Leaders are the first operation, every label-like operation control can
/// arrive at, and the operation after every transfer that is not a plain
/// fall-through.
fn split_blocks(ops: &[KOp], transfers: &[Transfer]) -> Vec<Block> {
    if ops.is_empty() {
        return Vec::new();
    }
    let mut leader = vec![false; ops.len()];
    leader[0] = true;
    for (index, op) in ops.iter().enumerate() {
        if matches!(
            op.kind,
            KOpKind::Label(_) | KOpKind::Anchor(_) | KOpKind::SkipTarget
        ) {
            leader[index] = true;
        }
        if transfers[index].control != ControlEffect::FallThrough && index + 1 < ops.len() {
            leader[index + 1] = true;
        }
    }

    let starts: Vec<usize> = (0..ops.len()).filter(|index| leader[*index]).collect();
    let mut blocks: Vec<Block> = starts
        .iter()
        .enumerate()
        .map(|(position, start)| Block {
            start: *start,
            end: starts.get(position + 1).copied().unwrap_or(ops.len()),
            successors: Vec::new(),
        })
        .collect();

    // Every label-led block, for the transfers K816 cannot name: control may
    // arrive at any of them from a target it could not resolve, which is the
    // same conservative answer the width solver gives, reached the same way.
    let label_blocks: Vec<usize> = blocks
        .iter()
        .enumerate()
        .filter(|(_, block)| matches!(ops[block.start].kind, KOpKind::Label(_)))
        .map(|(index, _)| index)
        .collect();
    let block_of_op = |op: usize| -> usize {
        starts
            .partition_point(|start| *start <= op)
            .saturating_sub(1)
    };

    for index in 0..blocks.len() {
        let last = blocks[index].end - 1;
        let next = (index + 1 < blocks.len()).then_some(index + 1);
        let transfer = transfers[last];
        let mut successors: Vec<usize> = Vec::new();
        let target = transfer.target.map(block_of_op);
        match transfer.control {
            // A call returns: `JSR` is not a leaf of this graph. It remains a
            // clobber and a dataflow barrier, which is the liveness transfer
            // function's business rather than the edge set's.
            ControlEffect::FallThrough | ControlEffect::Call => successors.extend(next),
            // `BRK`/`COP` vector to a handler that returns past the signature
            // byte; `WAI` resumes on the next instruction, and telling it from
            // `STP` is not worth an edge.
            ControlEffect::Trap | ControlEffect::Halt => successors.extend(next),
            ControlEffect::Branch => {
                successors.extend(next);
                match target {
                    Some(target) => successors.push(target),
                    None => successors.extend(label_blocks.iter().copied()),
                }
            }
            ControlEffect::Jump => match target {
                Some(target) => successors.push(target),
                None => successors.extend(label_blocks.iter().copied()),
            },
            ControlEffect::Return => {}
            ControlEffect::Unknown => {
                successors.extend(next);
                successors.extend(label_blocks.iter().copied());
            }
        }
        successors.sort_unstable();
        successors.dedup();
        blocks[index].successors = successors;
    }
    blocks
}

/// Which blocks an edge reaches.
fn reachability(
    ops: &[KOp],
    blocks: &[Block],
    transfers: &[Transfer],
    unit: &Unit<'_>,
) -> Vec<bool> {
    let mut reachable = vec![false; blocks.len()];
    if blocks.is_empty() {
        return reachable;
    }
    // A transfer K816 cannot name may land on any label in the function.
    let opaque_transfer = transfers.iter().any(|transfer| transfer.opaque);
    let mut worklist = vec![0usize];
    for (index, block) in blocks.iter().enumerate() {
        let KOpKind::Label(symbol) = &ops[block.start].kind else {
            continue;
        };
        if opaque_transfer || unit.external_labels.contains(symbol) {
            worklist.push(index);
        }
    }
    while let Some(index) = worklist.pop() {
        if reachable[index] {
            continue;
        }
        reachable[index] = true;
        worklist.extend(blocks[index].successors.iter().copied());
    }
    reachable
}

/// The CPU state live immediately after each operation.
///
/// Backward, unioned over successors, over a lattice whose join is set union
/// and whose transfer function is monotone — so a worklist converges, which is
/// the opposite of the exit classification `postmortem.md` §D.5 forbids
/// iterating.
fn liveness(ops: &[KOp], blocks: &[Block], transfers: &[Transfer]) -> Vec<CpuState> {
    let mut live_after = vec![CpuState::NONE; ops.len()];
    let mut live_in = vec![CpuState::NONE; blocks.len()];
    let mut changed = true;
    while changed {
        changed = false;
        for index in (0..blocks.len()).rev() {
            let block = &blocks[index];
            // An exit block has no successors, and its seed is everything:
            // decision 12, and the difference between an optimizer and one
            // that deletes return values.
            let mut live = if block.successors.is_empty() {
                CpuState::ALL
            } else {
                block
                    .successors
                    .iter()
                    .fold(CpuState::NONE, |state, successor| {
                        state.union(live_in[*successor])
                    })
            };
            for op in (block.start..block.end).rev() {
                live_after[op] = live;
                live = live_before(&ops[op], transfers[op], live);
            }
            if live != live_in[index] {
                live_in[index] = live;
                changed = true;
            }
        }
    }
    live_after
}

/// The state live before one operation, given what is live after it.
fn live_before(op: &KOp, transfer: Transfer, live_after: CpuState) -> CpuState {
    let Some(effects) = effects_of(op) else {
        // A label, a width claim, a directive: no bytes, no CPU state.
        return live_after;
    };
    if is_liveness_barrier(effects, transfer) {
        return CpuState::ALL;
    }
    live_after.without(effects.writes).union(effects.reads)
}

/// True when everything is live before this operation.
///
/// A call is the important row: the callee may read any register and any flag,
/// including ones no contract names, so a call clobbers the liveness rather
/// than passing it through. A return, a trap, a halt, an unclassifiable
/// operation, an unknown memory effect, and an unknown width transition are
/// the same answer for the same reason — and so is a branch or jump whose
/// target this function does not define, which leaves for code K816 is not
/// looking at.
///
/// `WidthEffect::Unknown` earns its place separately: `PLP` and `RTI` are
/// recorded as writing X and Y, because narrowing the index registers zeroes
/// their high bytes — but whether they narrow depends on a status byte nothing
/// tracked, so that must-write set cannot be subtracted from a live set.
fn is_liveness_barrier(effects: Effects, transfer: Transfer) -> bool {
    if transfer.opaque {
        return true;
    }
    matches!(
        effects.control,
        ControlEffect::Call
            | ControlEffect::Return
            | ControlEffect::Trap
            | ControlEffect::Halt
            | ControlEffect::Unknown
    ) || matches!(
        effects.memory,
        MemoryEffect::Unknown | MemoryEffect::Volatile
    ) || effects.width.is_unknown()
}

/// The operations inside a `nocross`-guarded run, which no pass may change.
///
/// A guard is a link-time assertion over the `length` bytes just emitted, and
/// `length` was measured at lowering. Removing an operation from inside the
/// run would leave the assertion covering bytes *before* the run's start —
/// a page-crossing error about code the `nocross` never claimed. The run has
/// no recorded start, so it is recovered here by walking back from the guard
/// until the byte lengths add up, using lowering's own measurement so that the
/// two agree by construction.
fn frozen_operations(ops: &[KOp]) -> Vec<bool> {
    let mut frozen = vec![false; ops.len()];
    for (guard, op) in ops.iter().enumerate() {
        let KOpKind::NocrossGuard { length } = op.kind else {
            continue;
        };
        let mut total = 0;
        let mut start = guard;
        while start > 0 && total < length {
            start -= 1;
            total += crate::lower::byte_length(&ops[start]);
        }
        for slot in frozen.iter_mut().take(guard).skip(start) {
            *slot = true;
        }
    }
    frozen
}

// ---------------------------------------------------------------------------
// Pass 3 — remove overwritten register values
// ---------------------------------------------------------------------------

/// Removes instructions nothing reachable reads.
///
/// An instruction is dead when nothing it writes is live on any path from it,
/// and it has no other effect to leave behind: no memory effect, plain
/// fall-through control, and not a mode instruction — those belong to passes 1
/// and 2 alone ([`is_mode_operation`]).
///
/// The pass iterates to its own fixpoint. Removing a `tax` can make the
/// `lda #1` above it dead in turn, and decision 18's rule is that each pass
/// closes what it opens rather than being re-run by a loop over the sequence.
fn remove_overwritten_register_values(
    unit: &Unit<'_>,
    ops: &mut Vec<KOp>,
    log: &mut OptimizationLog,
) {
    loop {
        let flow = Flow::build(ops, unit);
        let Some(index) = (0..ops.len()).find(|index| dead(ops, &flow, *index)) else {
            return;
        };
        let removed = ops.remove(index);
        let effects = effects_of(&removed).expect("an instruction");
        let witness = overwriter(ops, &flow, index, effects.writes);
        let justification = match &witness {
            Some(witness) => format!(
                "writes {}, all overwritten by `{}` before any read",
                describe_state(&effects.writes),
                crate::op::dump_op(witness, unit.symbols),
            ),
            None => format!(
                "writes {}, none of it live on any path from here",
                describe_state(&effects.writes),
            ),
        };
        log.changes.push(Change {
            pass: Pass::RemoveOverwritten,
            site: Site {
                function: unit.function,
                index,
            },
            before: vec![removed],
            after: Vec::new(),
            witness,
            justification,
        });
    }
}

/// True when the operation at `index` writes nothing anything reads.
fn dead(ops: &[KOp], flow: &Flow, index: usize) -> bool {
    if flow.frozen[index] || is_mode_operation(&ops[index]) {
        return false;
    }
    let Some(effects) = effects_of(&ops[index]) else {
        return false;
    };
    if !effects.is_removal_candidate() || effects.writes.is_empty() {
        return false;
    }
    // An operation no edge reaches is the unreachable pass's, which removes
    // only what the compiler generated. Leaving it here keeps a user's
    // unreachable instruction out of this pass's reach.
    if !flow.reachable[flow.block_of[index]] {
        return false;
    }
    !effects.writes.intersects(flow.live_after[index])
}

/// The operation that completed the overwrite, when one operation did.
///
/// The proof is the liveness; this is the *explanation*, and it exists because
/// "overwritten by `lda #$01`" tells a reader more than "not live". A straight
/// scan inside the block finds it where there is one, and there is none when
/// the value simply died at a merge or at a live-out.
fn overwriter(
    ops: &[KOp],
    flow: &Flow,
    index: usize,
    writes: crate::effects::CpuState,
) -> Option<KOp> {
    // `index` counted positions before the removal, so the operation that
    // followed the removed one now sits at `index`.
    let end = flow.block(index.min(ops.len().saturating_sub(1))).end - 1;
    let mut pending = writes;
    for op in ops.iter().take(end.max(index)).skip(index) {
        let effects = effects_of(op)?;
        pending = pending.without(effects.writes);
        if pending.is_empty() {
            return Some(op.clone());
        }
    }
    None
}

/// A register/flag set in the words a justification uses.
fn describe_state(state: &crate::effects::CpuState) -> String {
    let mut parts = Vec::new();
    if !state.registers.is_empty() {
        parts.push(state.registers.to_string());
    }
    if !state.flags.is_empty() {
        parts.push(state.flags.to_string());
    }
    if parts.is_empty() {
        return String::from("nothing");
    }
    parts.join(" ")
}

// ---------------------------------------------------------------------------
// Pass 1 — eliminate dead mode changes
// ---------------------------------------------------------------------------

/// Whether each axis is observed immediately after each operation.
///
/// "Observed" is an *effect* question, not an immediate question, and this is
/// where `doc/register-width-aware-syntax.md` "Dead mode elimination" step 1 is
/// overruled: it counts `ImmediateM`/`ImmediateX` instructions, and a
/// `rep #$20` before `lda addr` / `sta addr` carries no immediate at all.
/// Deleting it turns a 16-bit load and store into 8-bit ones — not a lost
/// optimization, a different program (`plans/05-optimizer.md` decision 2).
///
/// An axis is observed at a point when some operation reachable from it,
/// *before the next operation that establishes that axis*, reads the
/// corresponding flag, or is a call whose callee observes the axis, or is a
/// transfer K816 cannot resolve.
struct Observation {
    /// `[axis][operation]`: true when the axis is observed after it.
    after: [Vec<bool>; 2],
}

impl Observation {
    fn after(&self, index: usize, axis: Axis) -> bool {
        self.after[axis_slot(axis)][index]
    }

    fn build(ops: &[KOp], flow: &Flow, unit: &Unit<'_>) -> Self {
        let after = [
            observe_axis(ops, flow, unit, Axis::Accumulator),
            observe_axis(ops, flow, unit, Axis::Index),
        ];
        Self { after }
    }
}

fn axis_slot(axis: Axis) -> usize {
    match axis {
        Axis::Accumulator => 0,
        Axis::Index => 1,
    }
}

fn axis_flag(axis: Axis) -> FlagSet {
    match axis {
        Axis::Accumulator => FlagSet::M,
        Axis::Index => FlagSet::X,
    }
}

/// The backward dataflow for one axis.
fn observe_axis(ops: &[KOp], flow: &Flow, unit: &Unit<'_>, axis: Axis) -> Vec<bool> {
    let exit_seed = unit.observed_at_exit(axis);
    let mut after = vec![false; ops.len()];
    let mut at_entry = vec![false; flow.blocks.len()];
    let mut changed = true;
    while changed {
        changed = false;
        for index in (0..flow.blocks.len()).rev() {
            let block = &flow.blocks[index];
            let mut observed = if block.successors.is_empty() {
                exit_seed
            } else {
                block
                    .successors
                    .iter()
                    .any(|successor| at_entry[*successor])
            };
            for op in (block.start..block.end).rev() {
                after[op] = observed;
                observed = observes_before(&ops[op], unit, axis, observed);
            }
            if observed != at_entry[index] {
                at_entry[index] = observed;
                changed = true;
            }
        }
    }
    after
}

/// Whether the axis is observed *before* one operation.
fn observes_before(op: &KOp, unit: &Unit<'_>, axis: Axis, after: bool) -> bool {
    let Some(effects) = effects_of(op) else {
        // A label, a width claim, a directive: it emits no bytes, reads no
        // flag, and establishes nothing.
        return after;
    };
    // A width transition K816 cannot describe leaves the axis in a state
    // nothing here established and nothing here can reason about: `PLP` and
    // `RTI` pull a status byte, `XCE` may force both registers to eight bits,
    // and an unfolded mask may do anything its operand says. Keeping the
    // operation above them is the answer that never changes a program.
    if effects.width.is_unknown() {
        return true;
    }
    if effects.reads.flags.intersects(axis_flag(axis)) {
        return true;
    }
    if effects.control == ControlEffect::Call {
        let contract = unit.callee(call_target(op));
        // The callee re-establishes the axis on the way out, so nothing below
        // the call can observe what the caller set before it.
        let survives_the_call = !matches!(axis_exit(contract.exit, axis), AxisExit::Fixed(_));
        return contract.observes.axis(axis) || (survives_the_call && after);
    }
    // An instruction that establishes the axis ends the question: nothing
    // below it can observe what was set above it.
    if establishes(effects.width, axis).is_some() {
        return false;
    }
    after
}

/// The symbol a call names, when it names one this unit can resolve.
fn call_target(op: &KOp) -> Option<SymbolId> {
    let KOpKind::Instruction(instruction) = &op.kind else {
        return None;
    };
    match &instruction.operand {
        Some(Operand::Label(KExpr::Symbol(symbol)))
        | Some(Operand::Address {
            expr: KExpr::Symbol(symbol),
            index: None,
            ..
        }) => Some(*symbol),
        _ => None,
    }
}

fn axis_exit(exit: ExitContract, axis: Axis) -> AxisExit {
    match axis {
        Axis::Accumulator => exit.a,
        Axis::Index => exit.i,
    }
}

/// The width one instruction establishes on `axis`, if it establishes one.
fn establishes(width: WidthEffect, axis: Axis) -> Option<RegisterWidth> {
    match axis {
        Axis::Accumulator => width.accumulator(),
        Axis::Index => width.index(),
    }
}

/// The `SEP`/`REP` mask an operation carries, when it carries a folded one.
fn mode_mask(op: &KOp) -> Option<(Mnemonic, u8)> {
    let KOpKind::Instruction(instruction) = &op.kind else {
        return None;
    };
    if !matches!(instruction.mnemonic, Mnemonic::Sep | Mnemonic::Rep) {
        return None;
    }
    let value = instruction.operand.as_ref()?.immediate_value()?;
    Some((instruction.mnemonic, u8::try_from(value.get() & 0xFF).ok()?))
}

/// The mask bit one axis lives in.
fn axis_bit(axis: Axis) -> u8 {
    match axis {
        Axis::Accumulator => 0x20,
        Axis::Index => 0x10,
    }
}

/// Clears the dead M and X bits from every `REP`/`SEP` mask, and removes the
/// instruction only when the mask empties.
///
/// **A mask is not a width operation.** It is a write to an arbitrary set of P
/// bits, and only two of the eight are widths: `rep #$21` clears M *and* the
/// carry, so deleting it because M is unobserved breaks the `bcc` below it.
/// The pass therefore strips bits and leaves every other bit exactly where the
/// author put it (decision 8). Bits outside M and X are ordinary flag writes,
/// governed by the passes that already know how to prove a flag write dead.
fn eliminate_dead_mode_changes(unit: &Unit<'_>, ops: &mut Vec<KOp>, log: &mut OptimizationLog) {
    loop {
        let flow = Flow::build(ops, unit);
        let observation = Observation::build(ops, &flow, unit);
        let Some((index, kept, dead)) = (0..ops.len())
            .filter_map(|index| strip(ops, &flow, &observation, unit, index))
            .next()
        else {
            return;
        };

        let before = ops[index].clone();
        let justification = dead_mask_justification(unit, &before, dead, kept);
        let after = if kept == 0 {
            ops.remove(index);
            Vec::new()
        } else {
            set_mask(&mut ops[index], kept);
            vec![ops[index].clone()]
        };
        // Every claim the changed operation justified takes, on the affected
        // axis, the value the preceding claim carried (decision 10).
        for axis in [Axis::Accumulator, Axis::Index] {
            if dead & axis_bit(axis) != 0 {
                substitute_claims(unit, ops, index, axis);
            }
        }
        log.changes.push(Change {
            pass: Pass::EliminateDeadModes,
            site: Site {
                function: unit.function,
                index,
            },
            before: vec![before],
            after,
            witness: None,
            justification,
        });
    }
}

/// The bits of the mask at `index` that survive, and the ones that do not.
///
/// `None` when the operation is not a candidate or every bit is live.
fn strip(
    ops: &[KOp],
    flow: &Flow,
    observation: &Observation,
    unit: &Unit<'_>,
    index: usize,
) -> Option<(usize, u8, u8)> {
    let op = &ops[index];
    let (_, mask) = mode_mask(op)?;
    if flow.frozen[index] || !flow.reachable[flow.block_of[index]] {
        return None;
    }
    // A label-anchored bridge is fixed and is never eliminated: the arrivals
    // it exists for are branches from states F is not looking at, including
    // from another unit, and liveness below the label says nothing about them
    // (decision 7). Recognized by its recorded reason rather than by its
    // position — the adjacency is asserted separately.
    if op.reason == Some(Reason::LabelBridge) {
        return None;
    }
    let effects = effects_of(op)?;

    let mut dead = 0u8;
    for axis in [Axis::Accumulator, Axis::Index] {
        let bit = axis_bit(axis);
        if mask & bit == 0 || establishes(effects.width, axis).is_none() {
            continue;
        }
        // A bridge satisfies a callee's *published* entry contract, which the
        // whole program was compiled against. It goes only when the axis is
        // observed nowhere at all, in which case the contract that declares it
        // serves nothing (decision 3, third guard).
        if matches!(op.reason, Some(Reason::CallBridge(_))) && !unit.observed_nowhere.axis(axis) {
            continue;
        }
        if observation.after(index, axis) {
            continue;
        }
        // A *narrowing* bit carries a second obligation. `SEP` on the index
        // axis writes the X and Y registers, because narrowing zeroes their
        // high bytes, and `txs` reads the X register and no flag — so it sees
        // the difference without ever reading a width. A mode bit is dead only
        // when the axis is unobserved *and* the register write the bit
        // performs is dead (decision 2).
        if truncates(effects.width, axis)
            && flow.live_after[index].registers.intersects(truncated())
        {
            continue;
        }
        dead |= bit;
    }
    (dead != 0).then_some((index, mask & !dead, dead))
}

/// The registers a narrowing index-width transition truncates.
fn truncated() -> crate::effects::RegisterSet {
    crate::effects::RegisterSet::X.union(crate::effects::RegisterSet::Y)
}

/// True when this transition narrows the registers `axis` sizes.
fn truncates(width: WidthEffect, axis: Axis) -> bool {
    axis == Axis::Index && establishes(width, axis) == Some(RegisterWidth::W8)
}

/// Rewrites the mask of the `SEP`/`REP` at `index`.
fn set_mask(op: &mut KOp, mask: u8) {
    let KOpKind::Instruction(instruction) = &mut op.kind else {
        unreachable!("only an instruction carries a mask");
    };
    instruction.operand = Some(Operand::Immediate(KExpr::number(u32::from(mask))));
}

/// The claims a changed mode operation justified take, on the affected axis,
/// the value the *preceding* claim carried.
///
/// The rewrite stops at the end of the block, and that is sound rather than
/// lazy: F changed the operation only because the axis is unobserved on every
/// path forward, so a claim further on is inert — no instruction reads it, and
/// analysis has already refused every width-dependent immediate on an unknown
/// axis. A stale `.a16` in the emitted assembly can therefore mis-size
/// nothing: the only instructions ca65 sizes from it are the ones the dataflow
/// proved absent.
///
/// It stops early at two more places. A claim whose reason is
/// `CalleeExit(sym)` stands when that callee's exit *fixes* the axis, because
/// the callee re-establishes what F removed; and an instruction that
/// establishes the axis itself ends the substitution, because the claims below
/// it were never this operation's to justify.
///
/// F never writes an axis value it did not read off another claim, and never
/// turns `Unknown` into `Known`.
fn substitute_claims(unit: &Unit<'_>, ops: &mut [KOp], index: usize, axis: Axis) {
    let Some(carried) = preceding_claim(ops, index, axis) else {
        return;
    };
    let mut position = index;
    while position < ops.len() {
        match &ops[position].kind {
            // A leader: the end of this block.
            KOpKind::Label(_) | KOpKind::Anchor(_) | KOpKind::SkipTarget => return,
            KOpKind::ModeAssumption(state) => {
                let current = axis_state(*state, axis);
                // The callee puts the axis back, so its claim is not a
                // restatement of what F removed.
                if let Some(Reason::CalleeExit(callee)) = ops[position].reason
                    && matches!(
                        axis_exit(unit.callee(Some(callee)).exit, axis),
                        AxisExit::Fixed(_)
                    )
                {
                    return;
                }
                if current == AxisState::Unknown && carried != AxisState::Unknown {
                    return;
                }
                let KOpKind::ModeAssumption(state) = &mut ops[position].kind else {
                    unreachable!("just matched");
                };
                set_axis_state(state, axis, carried);
            }
            KOpKind::Instruction(_) => {
                let Some(effects) = effects_of(&ops[position]) else {
                    unreachable!("an instruction has effects");
                };
                if effects.width.is_unknown() || establishes(effects.width, axis).is_some() {
                    return;
                }
            }
            _ => {}
        }
        position += 1;
    }
}

/// The axis value of the nearest claim before `index`, when F may read one.
///
/// `None` when an operation between the two would have moved the axis, because
/// then the claim does not describe the state at `index` and F has no other
/// place to read a width from. Deriving one by applying a `WidthEffect` to a
/// tracked state is what would make the optimizer the second width authority,
/// which is the failure `src/width.rs` exists to prevent (decision 1).
///
/// In practice the answer is always the claim at the top of the same block:
/// the width model emits one at every function entry, every reachable label,
/// and after every call, which is exactly where a block begins.
fn preceding_claim(ops: &[KOp], index: usize, axis: Axis) -> Option<AxisState> {
    for op in ops[..index].iter().rev() {
        match &op.kind {
            KOpKind::ModeAssumption(state) => return Some(axis_state(*state, axis)),
            // A merge point: a claim above it describes arrivals this one does
            // not.
            KOpKind::Label(_) | KOpKind::Anchor(_) | KOpKind::SkipTarget => return None,
            KOpKind::Instruction(_) => {
                let effects = effects_of(op)?;
                if effects.width.is_unknown() || establishes(effects.width, axis).is_some() {
                    return None;
                }
            }
            _ => {}
        }
    }
    None
}

fn axis_state(state: ModeState, axis: Axis) -> AxisState {
    match axis {
        Axis::Accumulator => state.a,
        Axis::Index => state.i,
    }
}

fn set_axis_state(state: &mut ModeState, axis: Axis, value: AxisState) {
    match axis {
        Axis::Accumulator => state.a = value,
        Axis::Index => state.i = value,
    }
}

/// What the change log records for a stripped or removed mask.
fn dead_mask_justification(unit: &Unit<'_>, op: &KOp, dead: u8, kept: u8) -> String {
    let axes: Vec<&str> = [Axis::Accumulator, Axis::Index]
        .into_iter()
        .filter(|axis| dead & axis_bit(*axis) != 0)
        .map(|axis| match axis {
            Axis::Accumulator => "M",
            Axis::Index => "X",
        })
        .collect();
    let nowhere = [Axis::Accumulator, Axis::Index]
        .into_iter()
        .filter(|axis| dead & axis_bit(*axis) != 0)
        .all(|axis| unit.observed_nowhere.axis(axis));
    let scope = if nowhere {
        "no operation in the program observes it"
    } else {
        "no reachable operation reads it before the next operation that establishes it"
    };
    let tail = if kept == 0 {
        String::from("the mask is empty, so the instruction goes")
    } else {
        format!("${kept:02X} is not this pass's to touch, so the instruction stays")
    };
    let _ = op;
    format!("{} is unobserved: {scope}; {tail}", axes.join(" and "))
}
