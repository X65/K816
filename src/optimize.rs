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

use crate::effects::{
    ControlEffect, CpuState, Effects, FlagSet, MemoryEffect, RegisterSet, WidthEffect,
};
use crate::isa::{Mnemonic, RegisterWidth};
use crate::linkage::Linkage;
use crate::op::{
    AnchorId, AxisExit, AxisState, ExitContract, KOp, KOpKind, KProgram, ModeState, Operand,
    Origin, Reason,
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
    /// 9 — remove a branch whose target is the next label, which control
    /// reaches by falling through.
    RemoveBranchToNextLabel,
    /// 10 — remove a return that another return, immediately above it,
    /// already made.
    CollapseDuplicateReturns,
    /// 11 — shrink a long-form branch E emitted conservatively, once the
    /// byte-removing passes have run and the distance is known.
    ///
    /// Last, because it measures distances and every pass above it removes
    /// bytes — which is a constraint on the sequence rather than a position
    /// in it: a pass added later that removes bytes goes *above* this one.
    /// `plans/implementation.md` phase 9 numbers it 11 and says so;
    /// `plans/06-compatibility.md` §9 had put passes 9 and 10 "after F's
    /// eight", which taken literally is after this one (`postmortem.md`
    /// §G.13.1).
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
            Self::RemoveBranchToNextLabel => "remove-branch-to-next-label",
            Self::CollapseDuplicateReturns => "collapse-duplicate-returns",
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
    let called = context.called_in_the_workspace();
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
                .is_none_or(|info| info.entry.is_none() || called.contains(&function.symbol)),
            observed_nowhere,
        };
        eliminate_dead_mode_changes(&unit, &mut function.ops, &mut log);
        fold_mode_operations(&unit, &mut function.ops, &mut log);
        remove_overwritten_register_values(&unit, &mut function.ops, &mut log);
        coalesce_known_loads(&unit, &mut function.ops, &mut log);
        collapse_redundant_flags(&unit, &mut function.ops, &mut log);
        rewrite_zero_stores(&unit, &mut function.ops, &mut log);
        remove_unreachable(&unit, &mut function.ops, &mut log);
        remove_branches_to_the_next_label(&unit, &mut function.ops, &mut log);
        collapse_duplicate_returns(&unit, &mut function.ops, &mut log);
        shrink_branches(&unit, &mut function.ops, &mut log);
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

    /// Every function some analyzed unit transfers to by name.
    ///
    /// The question decision 6 asks about an entry point: `main`'s inferred
    /// exit constrains nothing *because nothing calls it*, and the moment
    /// something does it is a published promise like any other. Both call
    /// spellings and a tail `jmp` to a function are in `callees` — the width
    /// model records them all — and a name another unit refers to is the
    /// linkage's answer to the same question one boundary out.
    ///
    /// This is D's workspace optimism, not a second one: a function every
    /// analyzed unit ignores is treated as having no outside caller, which is
    /// what `--unit` exists to correct.
    fn called_in_the_workspace(&self) -> BTreeSet<SymbolId> {
        let mut called: BTreeSet<SymbolId> = self
            .model
            .functions
            .iter()
            .flat_map(|info| info.callees.iter().copied())
            .collect();
        for info in &self.model.functions {
            let name = self.model.symbols.name(info.symbol);
            if self.linkage.referenced_elsewhere(name, self.file) {
                called.insert(info.symbol);
            }
        }
        called
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
    /// An entry point is *usually* nobody's callee: `main` is entered by the
    /// startup code and an interrupt handler by the hardware, so an inferred
    /// exit that nothing consumes constrains nothing and the operations
    /// establishing it are removable like any other dead mode operation.
    ///
    /// **But only when no analyzed unit names it**, which is the condition
    /// decision 6 states and the easiest half of it to drop. K65 does not
    /// refuse a call to `main`, and once one exists the exit contract is
    /// consumed exactly like any other function's — the caller was compiled
    /// against it and sizes its immediates by it. Dropping the condition
    /// deletes the `rep` that makes `-> @a16` true and leaves the caller
    /// emitting a three-byte immediate at eight bits wide, in assembly that
    /// still assembles.
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
/// `SEP`/`REP` read no flag (`effects.rs`), which is what makes dead mode
/// elimination possible at all — and, on its own, what would let passes 3 and
/// 5 delete a mode instruction, since both already accept one as a removal
/// candidate. They would do it with none of the guards a mode change needs: a
/// published exit contract, a fixed label bridge, a live truncation, a mask
/// carrying a non-width bit.
///
/// So the exclusion is stated once, here, and every pass but 1 and 2 consults
/// it. It is the other half of the effect-table correction and landed in the
/// same change, because between the two the compiler is unsound.
fn is_mode_operation(op: &KOp) -> bool {
    // Both halves matter. The `WidthEffect` test is the plan's wording and is
    // what a `PLP` or an unfolded mask answers to; the mnemonic test catches
    // `sep #$01`, which carries no width bit and is still an instruction whose
    // mask is decided by the composed-transform rule rather than by liveness.
    match &op.kind {
        KOpKind::Instruction(instruction)
            if matches!(instruction.mnemonic, Mnemonic::Sep | Mnemonic::Rep) =>
        {
            true
        }
        _ => effects_of(op).is_some_and(|effects| effects.width != WidthEffect::Unchanged),
    }
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
    /// Where control may go from each operation, resolved against this
    /// function.
    transfers: Vec<Transfer>,
    /// Instructions the dead-write pass will remove.
    ///
    /// Computed as an *analysis* rather than discovered one removal at a time,
    /// because two passes need the answer and they run in a fixed order that
    /// puts them the wrong way round. Dead mode elimination runs first; the
    /// dead-write pass runs third and can make a mode bit dead by deleting the
    /// only instruction that read the flag. Decision 18's rule is that each
    /// pass closes what it opens, and there is no loop over the sequence to
    /// close this one — so the liveness both passes read is computed with the
    /// doomed instructions already treated as absent, and the mode pass sees
    /// through them the first time.
    doomed: Vec<bool>,
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
        let frozen = frozen_operations(ops);
        let (live_after, doomed) = liveness_and_doomed(
            ops,
            &blocks,
            &transfers,
            &Removable {
                frozen: &frozen,
                reachable: &reachable,
                block_of: &block_of,
            },
        );
        Self {
            blocks,
            block_of,
            live_after,
            reachable,
            frozen,
            doomed,
            transfers,
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

/// What makes an operation a candidate for removal at all, beyond its own
/// effects.
struct Removable<'a> {
    frozen: &'a [bool],
    reachable: &'a [bool],
    block_of: &'a [usize],
}

/// The CPU state live immediately after each operation, and the instructions
/// nothing reads.
///
/// The two are one fixpoint because each decides the other: an instruction is
/// dead when nothing it writes is live, and a dead instruction's *reads* keep
/// nothing alive. Computing liveness once and then deleting what it condemns
/// finds `tax` and misses the `lda #1` above it — the cascade only appears
/// when the liveness is recomputed without the condemned operation in it.
///
/// Monotone in both directions: the doomed set only grows, and the live set
/// only shrinks as it does, so the loop terminates. This is an ordinary
/// dataflow fixpoint over set union, not the exit classification
/// `postmortem.md` §D.5 forbids iterating.
fn liveness_and_doomed(
    ops: &[KOp],
    blocks: &[Block],
    transfers: &[Transfer],
    removable: &Removable<'_>,
) -> (Vec<CpuState>, Vec<bool>) {
    let mut doomed = vec![false; ops.len()];
    let mut live_after;
    loop {
        live_after = liveness(ops, blocks, transfers, &doomed);
        let condemned: Vec<usize> = (0..ops.len())
            .filter(|index| {
                !doomed[*index] && is_dead(ops, transfers, removable, &live_after, *index)
            })
            .collect();
        let grew = !condemned.is_empty();
        for index in condemned {
            doomed[index] = true;
        }
        if !grew {
            return (live_after, doomed);
        }
    }
}

/// The CPU state live immediately after each operation, with `doomed`
/// operations treated as absent.
///
/// Backward, unioned over successors, over a lattice whose join is set union
/// and whose transfer function is monotone — so a worklist converges.
fn liveness(
    ops: &[KOp],
    blocks: &[Block],
    transfers: &[Transfer],
    doomed: &[bool],
) -> Vec<CpuState> {
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
                if !doomed[op] {
                    live = live_before(&ops[op], transfers[op], live);
                }
            }
            if live != live_in[index] {
                live_in[index] = live;
                changed = true;
            }
        }
    }
    live_after
}

/// True when the operation at `index` writes nothing anything reads, and has
/// nothing else to leave behind.
fn is_dead(
    ops: &[KOp],
    transfers: &[Transfer],
    removable: &Removable<'_>,
    live_after: &[CpuState],
    index: usize,
) -> bool {
    if removable.frozen[index] || is_mode_operation(&ops[index]) {
        return false;
    }
    // An operation no edge reaches is the unreachable pass's, which removes
    // only what the compiler generated. Leaving it here keeps a user's
    // unreachable instruction out of this pass's reach.
    if !removable.reachable[removable.block_of[index]] {
        return false;
    }
    let Some(effects) = effects_of(&ops[index]) else {
        return false;
    };
    if transfers[index].opaque || !effects.is_removal_candidate() || effects.writes.is_empty() {
        return false;
    }
    !effects.writes.intersects(live_after[index])
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
    let live = live_after.without(effects.writes).union(effects.reads);
    // **A width transition redefines what a register write covers.**
    // `RegisterSet` has one bit for the accumulator and the effect table calls
    // an `lda` a write to it, but at 8 bits an `lda` writes the low byte and
    // leaves the hidden `B` byte exactly where the 16-bit load above it put
    // it — so the 16-bit load is not overwritten and is not dead. The same
    // holds on the index axis in the other direction: a narrowing `SEP` zeroes
    // the high bytes of X and Y and preserves the low ones, which is a partial
    // write recorded as a whole one.
    //
    // Both are must-write over-approximations, and `postmortem.md` §C.5 says
    // what those cost: the optimizer subtracts `writes` from a live set, so
    // over-approximating declares a live value dead. Making the affected
    // registers live at every width transition is the conservative answer, and
    // it costs a load before a mode change rather than a program.
    match effects.width {
        WidthEffect::Known { a, i } => {
            let mut live = live;
            if a.is_some() {
                live = live.union(CpuState::new(RegisterSet::A, FlagSet::NONE));
            }
            if i.is_some() {
                live = live.union(CpuState::new(truncated(), FlagSet::NONE));
            }
            live
        }
        WidthEffect::Unchanged | WidthEffect::Unknown => live,
    }
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
/// The analysis is [`Flow::doomed`], which already ran the liveness and the
/// deadness to their joint fixpoint — so this is a single sweep, every site
/// index names a position in the stream the pass was handed, and the cascade
/// (`lda #1` dying because the `tax` that read it did) is already in it.
///
/// A candidate has no memory effect, plain fall-through control, and is not a
/// mode instruction — those belong to passes 1 and 2 alone.
fn remove_overwritten_register_values(
    unit: &Unit<'_>,
    ops: &mut Vec<KOp>,
    log: &mut OptimizationLog,
) {
    let flow = Flow::build(ops, unit);
    let doomed: Vec<usize> = (0..ops.len()).filter(|index| flow.doomed[*index]).collect();
    for index in &doomed {
        let removed = ops[*index].clone();
        let effects = effects_of(&removed).expect("an instruction");
        let witness = overwriter(ops, &flow, *index, effects.writes);
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
                index: *index,
            },
            before: vec![removed],
            after: Vec::new(),
            witness,
            justification,
        });
    }
    for index in doomed.into_iter().rev() {
        ops.remove(index);
    }
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
    let end = flow.block(index).end;
    let mut pending = writes;
    for op in ops.iter().take(end).skip(index + 1) {
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

    fn build(ops: &[KOp], flow: &Flow, transfers: &[Transfer], unit: &Unit<'_>) -> Self {
        let after = [
            observe_axis(ops, flow, transfers, unit, Axis::Accumulator),
            observe_axis(ops, flow, transfers, unit, Axis::Index),
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
fn observe_axis(
    ops: &[KOp],
    flow: &Flow,
    transfers: &[Transfer],
    unit: &Unit<'_>,
    axis: Axis,
) -> Vec<bool> {
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
                // An instruction the dead-write pass will delete observes
                // nothing: it is not going to be there. Seeing through it here
                // is what keeps a second `optimize()` finding no work, since
                // that pass runs third and this one runs first.
                if !flow.doomed[op] {
                    observed = observes_before(&ops[op], transfers[op], unit, axis, observed);
                }
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
fn observes_before(op: &KOp, transfer: Transfer, unit: &Unit<'_>, axis: Axis, after: bool) -> bool {
    // A branch or jump this function cannot name reaches code K816 is not
    // looking at, which may read either flag. The block graph answers the
    // *reachability* question by making every label a successor; that says
    // nothing about a target outside the function, which is where an indirect
    // `jmp (vec)` most often goes.
    if transfer.opaque {
        return true;
    }
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
        let observation = Observation::build(ops, &flow, &flow.transfers, unit);
        let Some((index, kept, dead)) = (0..ops.len())
            .filter_map(|index| strip(ops, &flow, &observation, unit, index))
            .next()
        else {
            return;
        };

        let before = ops[index].clone();
        let justification = dead_mask_justification(unit, dead, kept);
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
fn truncated() -> RegisterSet {
    RegisterSet::X.union(RegisterSet::Y)
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
fn dead_mask_justification(unit: &Unit<'_>, dead: u8, kept: u8) -> String {
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
    format!("{} is unobserved: {scope}; {tail}", axes.join(" and "))
}

// ---------------------------------------------------------------------------
// Pass 2 — fold adjacent REP/SEP
// ---------------------------------------------------------------------------

/// The status transform an adjacent run of `REP`/`SEP` composes to.
///
/// **Not effect-set equality.** `Effects` records *which* state an instruction
/// touches, never what value it leaves there: `rep #$01` and `sep #$01` have
/// identical effects — same read set, same `C` write, no memory,
/// fall-through — and set the carry to opposite values. Any rule phrased as
/// "equal effects" licenses folding one into the other, so the rule is not
/// that (`plans/05-optimizer.md` decision 11).
///
/// The run composes bit by bit instead: each `SEP` sets the bits of its mask,
/// each `REP` clears them, and a bit no operation in the run touches is
/// unchanged. What comes out is one *set* mask and one *clear* mask — disjoint
/// by construction, last writer per bit winning — and a boolean for whether
/// the run narrowed the index registers on the way through. A replacement is
/// legal exactly when it reproduces all three.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
struct Composed {
    set: u8,
    clear: u8,
    /// Some operation in the run narrowed the index registers, which zeroes
    /// the high bytes of X and Y whether or not the composed result still
    /// leaves them narrow.
    truncated: bool,
}

impl Composed {
    fn apply(&mut self, mnemonic: Mnemonic, mask: u8) {
        match mnemonic {
            Mnemonic::Sep => {
                self.set |= mask;
                self.clear &= !mask;
                self.truncated |= mask & axis_bit(Axis::Index) != 0;
            }
            _ => {
                self.clear |= mask;
                self.set &= !mask;
            }
        }
    }

    /// True when the replacement still narrows the index registers.
    fn still_truncates(self) -> bool {
        self.set & axis_bit(Axis::Index) != 0
    }
}

/// Merges every adjacent run of `REP`/`SEP` into the fewest instructions with
/// the same composed status transform.
fn fold_mode_operations(unit: &Unit<'_>, ops: &mut Vec<KOp>, log: &mut OptimizationLog) {
    loop {
        let flow = Flow::build(ops, unit);
        let Some((start, end, replacement)) = (0..ops.len())
            .filter_map(|start| foldable(ops, &flow, start))
            .next()
        else {
            return;
        };
        let before: Vec<KOp> = ops[start..end].to_vec();
        let justification = fold_justification(&before, &replacement);
        ops.splice(start..end, replacement.iter().cloned());
        log.changes.push(Change {
            pass: Pass::FoldModes,
            site: Site {
                function: unit.function,
                index: start,
            },
            before,
            after: replacement,
            witness: None,
            justification,
        });
    }
}

/// The run starting at `start`, and what replaces it, when a replacement is
/// both legal and an improvement.
fn foldable(ops: &[KOp], flow: &Flow, start: usize) -> Option<(usize, usize, Vec<KOp>)> {
    let (first, first_mask) = mode_mask(&ops[start])?;
    if flow.frozen[start] || !flow.reachable[flow.block_of[start]] {
        return None;
    }
    // Equal *reason* and equal *origin*. The reason is the annotation the
    // merged line will carry, and the origin is who wrote it: merging a user's
    // `sep` with an expansion's would report one of them as the other, and
    // `doc/design.md` §8's identical-treatment rule is about how a pass
    // reasons, not about erasing which notation an operation came from.
    let (reason, origin) = (ops[start].reason, ops[start].origin);
    let mut composed = Composed::default();
    composed.apply(first, first_mask);

    // Any operation between two mode changes ends the run — a claim, a label,
    // an instruction — which is the document's own "on any non-mode op, the
    // pending bits are flushed". So does an unequal reason: a merged "bridge
    // for `narrow`" and "entering the width block" would have to claim one and
    // lie about the other, and the annotation on the emitted line has to stay
    // true.
    let mut end = start + 1;
    while end < ops.len()
        && !flow.frozen[end]
        && ops[end].reason == reason
        && ops[end].origin == origin
    {
        let Some((mnemonic, mask)) = mode_mask(&ops[end]) else {
            break;
        };
        composed.apply(mnemonic, mask);
        end += 1;
    }
    if end - start < 2 {
        return None;
    }

    // A narrowing the composed result no longer performs zeroed the high bytes
    // of X and Y on the way through, and that is an ordinary register write:
    // `sep #$10` + `rep #$10` composes to `rep #$10` only when X and Y are
    // dead (decision 11, the truncation rule in its second costume).
    if composed.truncated
        && !composed.still_truncates()
        && flow.live_after[end - 1].registers.intersects(truncated())
    {
        return None;
    }

    let mut replacement = Vec::new();
    let mut push = |mnemonic: Mnemonic, mask: u8| {
        if mask == 0 {
            return;
        }
        let mut op = ops[start].clone();
        op.kind = KOpKind::Instruction(crate::op::Instruction::new(
            mnemonic,
            crate::isa::AddressingMode::Immediate,
            Some(Operand::Immediate(KExpr::number(u32::from(mask)))),
        ));
        replacement.push(op);
    };
    // The order of the run's first instruction, which keeps the merged stream
    // reading like the one it replaced. Set and clear masks are disjoint, so
    // the composed state is the same either way, and so is the truncation.
    match first {
        Mnemonic::Sep => {
            push(Mnemonic::Sep, composed.set);
            push(Mnemonic::Rep, composed.clear);
        }
        _ => {
            push(Mnemonic::Rep, composed.clear);
            push(Mnemonic::Sep, composed.set);
        }
    }
    // Compared by *kind*: the replacement carries the first operation's span
    // and reason, so a run that composes to itself would otherwise look like a
    // change and rewrite every span in it for nothing.
    let unchanged = replacement.len() == end - start
        && replacement
            .iter()
            .zip(&ops[start..end])
            .all(|(new, old)| new.kind == old.kind);
    if replacement.len() > end - start || unchanged {
        return None;
    }
    Some((start, end, replacement))
}

/// What the change log records for a fold.
fn fold_justification(before: &[KOp], after: &[KOp]) -> String {
    let mut composed = Composed::default();
    for op in before {
        let (mnemonic, mask) = mode_mask(op).expect("a folded mask");
        composed.apply(mnemonic, mask);
    }
    let truncation = match (composed.truncated, composed.still_truncates()) {
        (false, _) => "no index truncation to reproduce",
        (true, true) => "the index truncation is reproduced",
        (true, false) => "the index truncation is dropped, and X and Y are dead",
    };
    format!(
        "identical composed transform: P bits set ${:02X}, cleared ${:02X}; {truncation}; \
         {} instructions become {}",
        composed.set,
        composed.clear,
        before.len(),
        after.len(),
    )
}

// ---------------------------------------------------------------------------
// The value tracker (passes 4, 5, and 6)
// ---------------------------------------------------------------------------

/// Which bits of a register a tracked value covers.
///
/// **A value domain of `register → value` alone is unsound on this CPU.**
/// `lda #0` in an 8-bit accumulator establishes the low byte and says nothing
/// about the hidden `B` byte; a following `tax` with 16-bit index registers
/// moves all sixteen bits, so X is *not* known to be zero
/// (`plans/05-optimizer.md` decision 15).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Lanes {
    /// The low byte only.
    Low,
    /// All sixteen bits.
    Full,
}

impl Lanes {
    fn of(width: RegisterWidth) -> Self {
        match width {
            RegisterWidth::W8 => Self::Low,
            RegisterWidth::W16 => Self::Full,
        }
    }

    /// True when a value known in `self` lanes covers everything `needed`
    /// asks for.
    fn covers(self, needed: Self) -> bool {
        self == Self::Full || needed == Self::Low
    }
}

/// One register's tracked value.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Known {
    value: u32,
    lanes: Lanes,
}

/// What the tracker knows at one point.
///
/// It dies at every width transition, which is what keeps decision 1 intact: F
/// reads a width from one place only — a `ModeAssumption` the width model
/// wrote — and never advances it across an instruction, because the first
/// instruction that would advance it also empties the tracker. F therefore
/// never *computes* a width; it consumes one and stops trusting it at exactly
/// the point the model would have written a new claim.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
struct Tracked {
    a: Option<Known>,
    x: Option<Known>,
    y: Option<Known>,
    /// The value N and Z were last set from, which is what lets a load whose
    /// register already holds its value go without changing the flags.
    nz: Option<Known>,
    carry: Option<bool>,
    decimal: Option<bool>,
    interrupt: Option<bool>,
    overflow: Option<bool>,
    /// The width claim in effect, or `None` once an instruction moved a width.
    claim: Option<ModeState>,
}

impl Tracked {
    fn register(&self, register: RegisterSet) -> Option<Known> {
        match register {
            RegisterSet::A => self.a,
            RegisterSet::X => self.x,
            RegisterSet::Y => self.y,
            _ => None,
        }
    }

    fn set_register(&mut self, register: RegisterSet, value: Option<Known>) {
        match register {
            RegisterSet::A => self.a = value,
            RegisterSet::X => self.x = value,
            RegisterSet::Y => self.y = value,
            _ => {}
        }
    }

    fn flag(&self, flag: FlagSet) -> Option<bool> {
        match flag {
            FlagSet::C => self.carry,
            FlagSet::D => self.decimal,
            FlagSet::I => self.interrupt,
            FlagSet::V => self.overflow,
            _ => None,
        }
    }

    fn set_flag(&mut self, flag: FlagSet, value: Option<bool>) {
        match flag {
            FlagSet::C => self.carry = value,
            FlagSet::D => self.decimal = value,
            FlagSet::I => self.interrupt = value,
            FlagSet::V => self.overflow = value,
            _ => {}
        }
    }

    /// The width in effect on `axis`, when a claim says so.
    fn width(&self, axis: Axis) -> Option<RegisterWidth> {
        let claim = self.claim?;
        match axis {
            Axis::Accumulator => claim.a.width(),
            Axis::Index => claim.i.width(),
        }
    }

    /// Everything the operation's effects invalidate.
    fn clobber(&mut self, effects: Effects) {
        for register in [RegisterSet::A, RegisterSet::X, RegisterSet::Y] {
            if effects.writes.registers.intersects(register) {
                self.set_register(register, None);
            }
        }
        if effects
            .writes
            .flags
            .intersects(FlagSet::N.union(FlagSet::Z))
        {
            self.nz = None;
        }
        for flag in [FlagSet::C, FlagSet::D, FlagSet::I, FlagSet::V] {
            if effects.writes.flags.intersects(flag) {
                self.set_flag(flag, None);
            }
        }
    }
}

/// What the tracker knows *before* each operation.
///
/// Forward, per block, starting from nothing at every block boundary. There is
/// no merge: a value that reaches a label by two paths is a question this pass
/// does not need to answer, and answering it would need a lattice join over a
/// domain whose whole point is that it dies easily.
fn track_values(ops: &[KOp], flow: &Flow) -> Vec<Tracked> {
    let mut before = vec![Tracked::default(); ops.len()];
    for block in &flow.blocks {
        let mut state = Tracked::default();
        for index in block.start..block.end {
            before[index] = state;
            state = advance(state, &ops[index]);
        }
    }
    before
}

/// The tracker after one operation.
fn advance(state: Tracked, op: &KOp) -> Tracked {
    if let KOpKind::ModeAssumption(claim) = &op.kind {
        let mut state = state;
        state.claim = Some(*claim);
        return state;
    }
    let Some(effects) = effects_of(op) else {
        return state;
    };
    let KOpKind::Instruction(instruction) = &op.kind else {
        unreachable!("only an instruction has effects");
    };

    // A width transition, a call, a return, an unknown memory effect, an
    // unfolded mask, `PLP`, `RTI`, and `XCE` all empty the tracker.
    if effects.width != WidthEffect::Unchanged
        || effects.control != ControlEffect::FallThrough
        || effects.memory == MemoryEffect::Unknown
        || effects.memory == MemoryEffect::Volatile
    {
        return Tracked::default();
    }

    let mut state = state;
    match instruction.mnemonic {
        // A mask that touches no width flag is an ordinary set or clear of the
        // bits it names, and the tracker can follow it exactly.
        Mnemonic::Sep | Mnemonic::Rep => {
            let Some((mnemonic, mask)) = mode_mask(op) else {
                return Tracked::default();
            };
            let value = mnemonic == Mnemonic::Sep;
            for flag in [FlagSet::C, FlagSet::D, FlagSet::I, FlagSet::V] {
                if FlagSet::from_p_mask(mask).intersects(flag) {
                    state.set_flag(flag, Some(value));
                }
            }
            if FlagSet::from_p_mask(mask).intersects(FlagSet::N.union(FlagSet::Z)) {
                state.nz = None;
            }
            return state;
        }
        Mnemonic::Clc => return set_flag(state, FlagSet::C, false),
        Mnemonic::Sec => return set_flag(state, FlagSet::C, true),
        Mnemonic::Cld => return set_flag(state, FlagSet::D, false),
        Mnemonic::Sed => return set_flag(state, FlagSet::D, true),
        Mnemonic::Cli => return set_flag(state, FlagSet::I, false),
        Mnemonic::Sei => return set_flag(state, FlagSet::I, true),
        Mnemonic::Clv => return set_flag(state, FlagSet::V, false),
        _ => {}
    }

    // The loads and the transfers are the only two shapes that establish a
    // value. **A load whose source is memory is never one of them**: a
    // fixed-address `var` is how K65 spells a hardware register, and
    // `examples/k65/atari-xl/landscape/main.k65` reads the POKEY
    // random-number register in two consecutive statements (decision 14).
    let established = match instruction.mnemonic {
        Mnemonic::Lda | Mnemonic::Ldx | Mnemonic::Ldy => {
            immediate_load(&state, instruction).map(|value| Established {
                register: destination(instruction),
                value,
                // A load reports on exactly what it loaded, at the width it
                // loaded it.
                nz: Some(value),
            })
        }
        Mnemonic::Tax => transferred(&state, RegisterSet::A, RegisterSet::X, Axis::Index),
        Mnemonic::Tay => transferred(&state, RegisterSet::A, RegisterSet::Y, Axis::Index),
        Mnemonic::Txa => transferred(&state, RegisterSet::X, RegisterSet::A, Axis::Accumulator),
        Mnemonic::Tya => transferred(&state, RegisterSet::Y, RegisterSet::A, Axis::Accumulator),
        Mnemonic::Txy => transferred(&state, RegisterSet::X, RegisterSet::Y, Axis::Index),
        Mnemonic::Tyx => transferred(&state, RegisterSet::Y, RegisterSet::X, Axis::Index),
        // `XBA` moves all sixteen bits regardless of width, so it needs a
        // full-width source and gives one — **and it reports on eight**. N and
        // Z come from the new low byte whatever `M` says (`doc/w65c816s.md`),
        // so the flags it leaves are not the flags a load of the same value
        // would leave: `lda #$0001` then `xba` gives A = $0100 with Z *set*,
        // where `lda #$0100` at sixteen bits leaves Z clear. Tracking the
        // value and the flag provenance as one thing loses that, and the
        // program takes the other branch.
        Mnemonic::Xba => state
            .a
            .filter(|known| known.lanes == Lanes::Full)
            .map(|known| Established {
                register: RegisterSet::A,
                value: Known {
                    value: ((known.value >> 8) & 0xFF) | ((known.value & 0xFF) << 8),
                    lanes: Lanes::Full,
                },
                nz: None,
            }),
        _ => None,
    };

    state.clobber(effects);
    if let Some(established) = established {
        state.set_register(established.register, Some(established.value));
        if effects.writes.flags.contains(FlagSet::N.union(FlagSet::Z)) {
            state.nz = established.nz;
        }
    }
    state
}

/// A value one operation establishes, and what it leaves N and Z reporting.
///
/// The two are separate because they can disagree about *width*. Every load
/// and every width-dependent transfer reports on exactly what it moved, so
/// `nz` is the value again; `XBA` moves sixteen bits and reports on eight, so
/// it has none.
#[derive(Clone, Copy, Debug)]
struct Established {
    register: RegisterSet,
    value: Known,
    nz: Option<Known>,
}

fn set_flag(mut state: Tracked, flag: FlagSet, value: bool) -> Tracked {
    state.set_flag(flag, Some(value));
    state
}

/// The register a load writes.
fn destination(instruction: &crate::op::Instruction) -> RegisterSet {
    match instruction.mnemonic {
        Mnemonic::Ldx => RegisterSet::X,
        Mnemonic::Ldy => RegisterSet::Y,
        _ => RegisterSet::A,
    }
}

/// The value an immediate load establishes, in the lanes the claim in effect
/// says its immediate encodes.
///
/// With no claim in effect, nothing is tracked: F reads a width off a claim the
/// width model wrote or it reads none at all.
fn immediate_load(state: &Tracked, instruction: &crate::op::Instruction) -> Option<Known> {
    let value = instruction.operand.as_ref()?.immediate_value()?.get();
    let class = instruction.spec()?.immediate_width()?;
    let lanes = match class {
        crate::isa::ImmediateWidthClass::Fixed8 => Lanes::Low,
        crate::isa::ImmediateWidthClass::Fixed16 => Lanes::Full,
        crate::isa::ImmediateWidthClass::MDependent => Lanes::of(state.width(Axis::Accumulator)?),
        crate::isa::ImmediateWidthClass::XDependent => Lanes::of(state.width(Axis::Index)?),
    };
    Some(Known { value, lanes })
}

/// The value a width-dependent transfer moves.
///
/// It moves as many bits as the *destination* width, which must itself be
/// known, and it propagates only when the source's known lanes cover all of
/// them.
fn transferred(
    state: &Tracked,
    from: RegisterSet,
    to: RegisterSet,
    destination_axis: Axis,
) -> Option<Established> {
    let width = state.width(destination_axis)?;
    let needed = Lanes::of(width);
    let source = state.register(from)?;
    if !source.lanes.covers(needed) {
        return None;
    }
    let value = Known {
        value: match width {
            RegisterWidth::W8 => source.value & 0xFF,
            RegisterWidth::W16 => source.value & 0xFFFF,
        },
        lanes: needed,
    };
    Some(Established {
        register: to,
        value,
        // These report on exactly what they moved, at the width they moved it.
        nz: Some(value),
    })
}

// ---------------------------------------------------------------------------
// Pass 4 — coalesce known register loads
// ---------------------------------------------------------------------------

/// Deletes a load of a value the register provably already holds.
fn coalesce_known_loads(unit: &Unit<'_>, ops: &mut Vec<KOp>, log: &mut OptimizationLog) {
    let flow = Flow::build(ops, unit);
    let values = track_values(ops, &flow);
    let redundant: Vec<usize> = (0..ops.len())
        .filter(|index| redundant_load(ops, &flow, &values, *index))
        .collect();
    for index in &redundant {
        let removed = ops[*index].clone();
        log.changes.push(Change {
            pass: Pass::CoalesceLoads,
            site: Site {
                function: unit.function,
                index: *index,
            },
            justification: format!(
                "the register already holds {}, in every lane this load writes",
                describe_value(&values[*index], &removed),
            ),
            before: vec![removed],
            after: Vec::new(),
            witness: None,
        });
    }
    for index in redundant.into_iter().rev() {
        ops.remove(index);
    }
}

/// True when the load at `index` establishes what is already there.
fn redundant_load(ops: &[KOp], flow: &Flow, values: &[Tracked], index: usize) -> bool {
    if flow.frozen[index] || flow.doomed[index] || !flow.reachable[flow.block_of[index]] {
        return false;
    }
    let KOpKind::Instruction(instruction) = &ops[index].kind else {
        return false;
    };
    if !matches!(
        instruction.mnemonic,
        Mnemonic::Lda | Mnemonic::Ldx | Mnemonic::Ldy
    ) {
        return false;
    }
    let state = &values[index];
    let Some(loaded) = immediate_load(state, instruction) else {
        return false;
    };
    let register = destination(instruction);
    if state.register(register) != Some(loaded) {
        return false;
    }
    // The load also reports on N and Z. It is a complete no-op only when the
    // flags already say the same thing, or when nothing reads them.
    let nz = FlagSet::N.union(FlagSet::Z);
    state.nz == Some(loaded) || !flow.live_after[index].flags.intersects(nz)
}

/// The tracked value a justification names.
fn describe_value(state: &Tracked, op: &KOp) -> String {
    let KOpKind::Instruction(instruction) = &op.kind else {
        return String::from("that value");
    };
    match state.register(destination(instruction)) {
        Some(known) => format!(
            "${:0width$X}",
            known.value,
            width = match known.lanes {
                Lanes::Low => 2,
                Lanes::Full => 4,
            }
        ),
        None => String::from("that value"),
    }
}

// ---------------------------------------------------------------------------
// Pass 5 — collapse redundant flag operations
// ---------------------------------------------------------------------------

/// The flag one operation sets, and to what.
fn flag_operation(op: &KOp) -> Option<(FlagSet, bool)> {
    let KOpKind::Instruction(instruction) = &op.kind else {
        return None;
    };
    Some(match instruction.mnemonic {
        Mnemonic::Clc => (FlagSet::C, false),
        Mnemonic::Sec => (FlagSet::C, true),
        Mnemonic::Cld => (FlagSet::D, false),
        Mnemonic::Sed => (FlagSet::D, true),
        Mnemonic::Cli => (FlagSet::I, false),
        Mnemonic::Sei => (FlagSet::I, true),
        Mnemonic::Clv => (FlagSet::V, false),
        _ => return None,
    })
}

/// Deletes a flag operation whose flag provably already holds that value.
///
/// This one needs no liveness at all: the instruction is deleted because it
/// would change nothing, not because nothing reads what it changes — which is
/// what keeps it away from the flags that are live at every exit.
fn collapse_redundant_flags(unit: &Unit<'_>, ops: &mut Vec<KOp>, log: &mut OptimizationLog) {
    let flow = Flow::build(ops, unit);
    let values = track_values(ops, &flow);
    let redundant: Vec<usize> = (0..ops.len())
        .filter(|index| {
            if flow.frozen[*index] || flow.doomed[*index] || is_mode_operation(&ops[*index]) {
                return false;
            }
            if !flow.reachable[flow.block_of[*index]] {
                return false;
            }
            flag_operation(&ops[*index])
                .is_some_and(|(flag, value)| values[*index].flag(flag) == Some(value))
        })
        .collect();
    for index in &redundant {
        let removed = ops[*index].clone();
        let (flag, value) = flag_operation(&removed).expect("a flag operation");
        log.changes.push(Change {
            pass: Pass::CollapseFlags,
            site: Site {
                function: unit.function,
                index: *index,
            },
            justification: format!(
                "{flag} already holds {}, so this changes nothing",
                u8::from(value)
            ),
            before: vec![removed],
            after: Vec::new(),
            witness: None,
        });
    }
    for index in redundant.into_iter().rev() {
        ops.remove(index);
    }
}

// ---------------------------------------------------------------------------
// Pass 6 — rewrite safe zero stores
// ---------------------------------------------------------------------------

/// Rewrites `sta m` to `stz m` where A is known zero, and then drops the
/// `lda #0` the rewrite orphaned.
///
/// `doc/syntax-reference.md` "Zero-Store Shortcut" specifies it exactly: "the
/// compiler's peephole rewrites each supported `STA` to `STZ` and drops the
/// leading `LDA #0` when `A` is dead afterwards". So the rewrite is *per
/// store* — `STZ` exists in four modes and `STA` in fourteen, and a chain may
/// mix them — and the load dies separately, if it dies at all.
///
/// The load's liveness is checked here rather than left to the dead-write
/// pass, because that pass runs earlier in the fixed order and will not see
/// the opportunity this one just created. That is decision 18's rule applied
/// to its one real instance.
fn rewrite_zero_stores(unit: &Unit<'_>, ops: &mut Vec<KOp>, log: &mut OptimizationLog) {
    let flow = Flow::build(ops, unit);
    let values = track_values(ops, &flow);
    let rewritten: Vec<usize> = (0..ops.len())
        .filter(|index| zero_store(ops, &flow, &values, *index).is_some())
        .collect();
    for index in &rewritten {
        let before = ops[*index].clone();
        let after = zero_store(ops, &flow, &values, *index).expect("just checked");
        ops[*index] = after.clone();
        log.changes.push(Change {
            pass: Pass::ZeroStores,
            site: Site {
                function: unit.function,
                index: *index,
            },
            before: vec![before],
            after: vec![after],
            witness: None,
            justification: String::from(
                "the accumulator is known zero in every lane this store writes, and `STZ` \
                 has this mode: same length, same stored bytes, and it stops reading A",
            ),
        });
    }
    if rewritten.is_empty() {
        return;
    }

    // The rewrite is what may have made the load dead, so the question is
    // asked again over the stream the rewrite left behind.
    let flow = Flow::build(ops, unit);
    let orphaned: Vec<usize> = (0..ops.len())
        .filter(|index| flow.doomed[*index] && loads_zero(&ops[*index]))
        .collect();
    for index in &orphaned {
        let removed = ops[*index].clone();
        log.changes.push(Change {
            pass: Pass::ZeroStores,
            site: Site {
                function: unit.function,
                index: *index,
            },
            before: vec![removed],
            after: Vec::new(),
            witness: None,
            justification: String::from(
                "nothing reads the accumulator or the N and Z it wrote, now that every \
                 store it fed is an `STZ`",
            ),
        });
    }
    for index in orphaned.into_iter().rev() {
        ops.remove(index);
    }
}

/// The `stz` an `sta` becomes, when it may.
fn zero_store(ops: &[KOp], flow: &Flow, values: &[Tracked], index: usize) -> Option<KOp> {
    if flow.frozen[index] || flow.doomed[index] || !flow.reachable[flow.block_of[index]] {
        return None;
    }
    let KOpKind::Instruction(instruction) = &ops[index].kind else {
        return None;
    };
    if instruction.mnemonic != Mnemonic::Sta {
        return None;
    }
    // The ISA table decides which modes exist, rather than a list repeated
    // here: `STZ` has direct, direct indexed by X, absolute, and absolute
    // indexed by X, and nothing else — no long, no indexed-Y, no indirect, no
    // stack-relative.
    crate::isa::spec(Mnemonic::Stz, instruction.mode)?;

    // Known zero in every lane the store writes. The store's width is the
    // accumulator's, and F reads that off the claim in effect or not at all.
    let state = &values[index];
    let width = state.width(Axis::Accumulator)?;
    let known = state.a?;
    if known.value != 0 || !known.lanes.covers(Lanes::of(width)) {
        return None;
    }

    let mut rewritten = ops[index].clone();
    rewritten.kind = KOpKind::Instruction(crate::op::Instruction::new(
        Mnemonic::Stz,
        instruction.mode,
        instruction.operand.clone(),
    ));
    Some(rewritten)
}

/// True when this operation is a `lda #0`.
fn loads_zero(op: &KOp) -> bool {
    let KOpKind::Instruction(instruction) = &op.kind else {
        return false;
    };
    instruction.mnemonic == Mnemonic::Lda
        && instruction
            .operand
            .as_ref()
            .and_then(Operand::immediate_value)
            .is_some_and(|value| value.get() == 0)
}

// ---------------------------------------------------------------------------
// Pass 8 — remove unreachable compiler-generated operations
// ---------------------------------------------------------------------------

/// Deletes instructions in blocks no edge reaches.
///
/// **Compiler-generated ones only**, which is the master plan's own wording
/// and not a hedge. A user's unreachable `rep` stays exactly where they wrote
/// it — `width-branch.k816::skipped` has one, and it is there to be seen in
/// the output. So does an HLA statement's expansion: the user wrote that too,
/// in a different notation, and `doc/design.md` §8's identical-treatment rule
/// is about how a pass *reasons*, not about who gets to keep unreachable code.
///
/// Instructions only. The data between a `SkipData` and its target is
/// unreachable as *code* by construction — that is what the jump around it is
/// for — and it is the bytes the program came for.
fn remove_unreachable(unit: &Unit<'_>, ops: &mut Vec<KOp>, log: &mut OptimizationLog) {
    let flow = Flow::build(ops, unit);
    let unreachable: Vec<usize> = (0..ops.len())
        .filter(|index| {
            !flow.frozen[*index]
                && !flow.reachable[flow.block_of[*index]]
                && ops[*index].origin == Origin::CompilerGenerated
                && matches!(ops[*index].kind, KOpKind::Instruction(_))
        })
        .collect();
    for index in &unreachable {
        let removed = ops[*index].clone();
        log.changes.push(Change {
            pass: Pass::Unreachable,
            site: Site {
                function: unit.function,
                index: *index,
            },
            before: vec![removed],
            after: Vec::new(),
            witness: None,
            justification: String::from(
                "no edge reaches this block, and the compiler generated the operation",
            ),
        });
    }
    for index in unreachable.into_iter().rev() {
        ops.remove(index);
    }
}

// ---------------------------------------------------------------------------
// Branches to the next label
// ---------------------------------------------------------------------------

/// True when the operation is a landing site and nothing else: it emits no
/// bytes, and control arriving at it is control arriving at everything below
/// it until the next operation that does.
fn is_landing_site(op: &KOp) -> bool {
    matches!(
        op.kind,
        KOpKind::Label(_) | KOpKind::Anchor(_) | KOpKind::SkipTarget
    )
}

/// Removes a forward branch whose target control reaches by falling through.
///
/// The legacy peephole's seventh rule (`remove_branch_to_next_label`), ported
/// onto the block view. Two things are deliberately not carried over.
///
/// **The legacy restricted the *target*** to labels its own lowering
/// synthesized (`is_synthetic_label`), which in that compiler was the same
/// set as "labels a compiler-emitted branch can name". It is not the same set
/// here: `> goto done` expands to `bcs done`, naming a label the *user*
/// wrote, and that is the case worth having. So the restriction moves to the
/// branch's own [`Origin`]: everything the compiler generated or expanded may
/// go, and a hand-written `jmp next` stays, because deleting an instruction
/// the author typed and can see in the listing is what
/// [`remove_unreachable`] already declines to do.
///
/// **It iterates.** One removal can expose another — the guard of a `>`
/// expansion is separated from its landing site only by the branch this pass
/// just took away — and a single scan would find the outer one and stop.
fn remove_branches_to_the_next_label(
    unit: &Unit<'_>,
    ops: &mut Vec<KOp>,
    log: &mut OptimizationLog,
) {
    loop {
        let flow = Flow::build(ops, unit);
        let Some(index) = (0..ops.len()).find(|index| falls_through_to_target(ops, &flow, *index))
        else {
            return;
        };
        let removed = ops.remove(index);
        log.changes.push(Change {
            pass: Pass::RemoveBranchToNextLabel,
            site: Site {
                function: unit.function,
                index,
            },
            before: vec![removed],
            after: Vec::new(),
            witness: None,
            justification: String::from(
                "the target is the next label, which control reaches by falling through",
            ),
        });
    }
}

/// True when the branch at `index` may be removed because falling through
/// arrives where it was going.
fn falls_through_to_target(ops: &[KOp], flow: &Flow, index: usize) -> bool {
    if flow.frozen[index] || ops[index].origin == Origin::User {
        return false;
    }
    let transfer = flow.transfers[index];
    if !matches!(
        transfer.control,
        ControlEffect::Branch | ControlEffect::Jump
    ) {
        return false;
    }
    let Some(target) = transfer.target else {
        return false;
    };
    // Forward only. A back edge to a label above is a loop, and every
    // operation between the two is one the loop runs again.
    target > index && ops[index + 1..target].iter().all(is_landing_site)
}

// ---------------------------------------------------------------------------
// Duplicate returns
// ---------------------------------------------------------------------------

/// The mnemonic when the operation is an operandless return.
fn return_mnemonic(op: &KOp) -> Option<Mnemonic> {
    let KOpKind::Instruction(instruction) = &op.kind else {
        return None;
    };
    if instruction.operand.is_some() {
        return None;
    }
    matches!(
        instruction.mnemonic,
        Mnemonic::Rts | Mnemonic::Rtl | Mnemonic::Rti
    )
    .then_some(instruction.mnemonic)
}

/// Removes a return the return above it already made.
///
/// The legacy peephole's eighth rule, and **the one pass that removes a
/// user-written operation**. The justification is structural rather than an
/// origin check: a return has no successors, so the only way to reach the
/// second one is to arrive at it from outside, and the only thing that lets
/// an edge in is a label. Any operation at all between the two — a label, an
/// anchor, a mode assumption, a byte of data — ends the run, which is the
/// legacy's rule and is what makes the argument hold without consulting the
/// block graph at all.
///
/// The mnemonics must match. `rts` after `rtl` is not a duplicate: it is a
/// second return that unwinds a different amount of stack, and reaching it is
/// a question about the caller rather than about this function.
fn collapse_duplicate_returns(unit: &Unit<'_>, ops: &mut Vec<KOp>, log: &mut OptimizationLog) {
    let frozen = frozen_operations(ops);
    let mut duplicates: Vec<usize> = Vec::new();
    let mut above: Option<Mnemonic> = None;
    for index in 0..ops.len() {
        let Some(mnemonic) = return_mnemonic(&ops[index]) else {
            above = None;
            continue;
        };
        if above == Some(mnemonic) && !frozen[index] {
            duplicates.push(index);
            continue;
        }
        above = Some(mnemonic);
    }
    for index in &duplicates {
        log.changes.push(Change {
            pass: Pass::CollapseDuplicateReturns,
            site: Site {
                function: unit.function,
                index: *index,
            },
            before: vec![ops[*index].clone()],
            after: Vec::new(),
            witness: Some(ops[index - 1].clone()),
            justification: String::from(
                "the return above it already left the function, and no label between them lets an edge in",
            ),
        });
    }
    for index in duplicates.into_iter().rev() {
        ops.remove(index);
    }
}

// ---------------------------------------------------------------------------
// Branch shrinking
// ---------------------------------------------------------------------------

/// An upper bound on the bytes one operation encodes, or a refusal.
///
/// E emits the inverted-branch-plus-`jmp` form wherever its conservative
/// estimate cannot prove a short branch reaches; once the byte-removing passes
/// have run, some of those now reach. But a `KOp` stream between a branch and
/// its target is not only instructions, and instruction lengths alone do not
/// give a distance (`plans/05-optimizer.md` decision 19).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Bound {
    Bytes(u32),
    /// The answer depends on something only ld65 knows.
    Unbounded,
}

/// The upper bound on one operation's encoded size.
fn size_bound(op: &KOp) -> Bound {
    match &op.kind {
        // The target moves to a different segment, and ld65 owns where.
        KOpKind::Segment { .. } => Bound::Unbounded,
        // `.align n` emits up to `n - 1` bytes of padding — an upper bound
        // only, which is all this pass needs.
        KOpKind::Align(expr) => match expr.value() {
            Some(value) => Bound::Bytes(value.get().saturating_sub(1)),
            None => Bound::Unbounded,
        },
        KOpKind::Data(crate::op::DataOp::Reserve(expr)) => match expr.value() {
            Some(value) => Bound::Bytes(value.get()),
            None => Bound::Unbounded,
        },
        // Everything else is what lowering measures, which is where the
        // instruction half of the answer lives.
        _ => Bound::Bytes(crate::lower::byte_length(op)),
    }
}

/// Puts a long-form branch back into its short form once the distance fits.
///
/// The pass computes an **upper** bound, shrinks only when the bound fits,
/// refuses across any unbounded operation or any segment change, and iterates
/// to its own fixpoint because each shrink brings other branches closer. It
/// never grows a branch: growing is E's decision at lowering, where the width
/// states are known.
///
/// Every shrunk branch is gated on ca65 accepting the output, because a wrong
/// bound is a `Range error` and not a silent one — which is the one mercy in
/// this pass.
fn shrink_branches(unit: &Unit<'_>, ops: &mut Vec<KOp>, log: &mut OptimizationLog) {
    loop {
        let flow = Flow::build(ops, unit);
        let Some((index, shrunk)) = first_shrinkable(ops, &flow) else {
            return;
        };
        let before = ops[index..index + 3].to_vec();
        ops.splice(index..index + 3, [shrunk.clone()]);
        log.changes.push(Change {
            pass: Pass::ShrinkBranches,
            site: Site {
                function: unit.function,
                index,
            },
            before,
            after: vec![shrunk],
            witness: None,
            justification: String::from(
                "the target is within a relative branch's reach now, by an upper bound over \
                 every operation between them",
            ),
        });
    }
}

/// The first long-form branch whose short form reaches, and what replaces it.
fn first_shrinkable(ops: &[KOp], flow: &Flow) -> Option<(usize, KOp)> {
    (0..ops.len().saturating_sub(2)).find_map(|index| shrinkable(ops, flow, index))
}

/// The three-operation idiom at `index`, collapsed, when it may be.
fn shrinkable(ops: &[KOp], flow: &Flow, index: usize) -> Option<(usize, KOp)> {
    // A `nocross` guard records the byte length of the run it closes, and
    // shrinking a branch inside one would leave that link-time assertion
    // covering bytes the `nocross` never claimed.
    if ops[index..index + 3]
        .iter()
        .enumerate()
        .any(|(offset, _)| flow.frozen[index + offset])
    {
        return None;
    }
    // The shape lowering emits: an inverted conditional branch over a jump,
    // and the anchor it lands on.
    let KOpKind::Instruction(guard) = &ops[index].kind else {
        return None;
    };
    if guard.mode != crate::isa::AddressingMode::Relative {
        return None;
    }
    let Some(Operand::Anchor(past)) = &guard.operand else {
        return None;
    };
    let KOpKind::Instruction(jump) = &ops[index + 1].kind else {
        return None;
    };
    if jump.mnemonic != Mnemonic::Jmp || ops[index + 1].reason != Some(Reason::LongBranch) {
        return None;
    }
    if ops[index + 2].kind != KOpKind::Anchor(*past) {
        return None;
    }
    // The landing pad exists only for this idiom. If anything else names it,
    // collapsing the idiom deletes a target.
    let named_elsewhere = ops.iter().enumerate().any(|(at, op)| {
        at != index
            && matches!(&op.kind, KOpKind::Instruction(instruction)
                if instruction.operand == Some(Operand::Anchor(*past)))
    });
    if named_elsewhere {
        return None;
    }
    let short = crate::lower::inverse_branch(guard.mnemonic)?;

    // The branch takes its target as a displacement ca65 computes, which is
    // the spelling the jump had to give up.
    let operand = match jump.operand.clone()? {
        Operand::Address {
            expr, index: None, ..
        } => Operand::Label(expr),
        anchor @ Operand::Anchor(_) => anchor,
        _ => return None,
    };
    let mut shrunk = ops[index].clone();
    shrunk.kind = KOpKind::Instruction(crate::op::Instruction::new(
        short,
        crate::isa::AddressingMode::Relative,
        Some(operand.clone()),
    ));

    // Measured on the stream the shrink would leave behind: each shrink brings
    // every branch that spans it three bytes closer, and checking here is what
    // makes the fixpoint terminate on distances rather than on a counter.
    let mut candidate: Vec<KOp> = ops.to_vec();
    candidate.splice(index..index + 3, [shrunk.clone()]);
    let target = match &operand {
        Operand::Anchor(anchor) => candidate
            .iter()
            .position(|op| op.kind == KOpKind::Anchor(*anchor))?,
        Operand::Label(KExpr::Symbol(symbol)) => candidate
            .iter()
            .position(|op| op.kind == KOpKind::Label(*symbol))?,
        _ => return None,
    };
    // A relative branch counts from the instruction after it, so the span is
    // the operations *between* the two — and the bound is summed over exactly
    // those. Measuring from the start of the function instead would let one
    // unbounded operation anywhere above the branch refuse a shrink that has
    // nothing to do with it.
    let span = if target > index {
        index + 1..target
    } else {
        target..index + 1
    };
    let mut distance: i64 = 0;
    for op in &candidate[span] {
        match size_bound(op) {
            Bound::Bytes(size) => distance += i64::from(size),
            Bound::Unbounded => return None,
        }
    }
    if target <= index {
        distance = -distance;
    }
    (-128..=127).contains(&distance).then_some((index, shrunk))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::op::DataOp;
    use crate::source::{FileId, SourceMap, Span};

    fn span() -> Span {
        let mut map = SourceMap::new();
        let id: FileId = map.add_file("t.k816", "func f { }");
        Span::new(id, 0, 4)
    }

    fn op(kind: KOpKind) -> KOp {
        KOp::new(kind, span(), Origin::CompilerGenerated)
    }

    /// Decision 19's table, asserted directly.
    ///
    /// Three of its rows name operations the language cannot yet put inside a
    /// function body — there is no `align`, no link-time `.res`, and no
    /// `segment` in a K65 statement position — so there is no fixture that can
    /// reach them. The rule is still the rule, and the day a sub-plan makes
    /// one of them expressible, this is what says what the bound has to be.
    #[test]
    fn the_size_bound_refuses_what_only_ld65_can_answer() {
        // A segment change: the target moves somewhere ld65 places, so there
        // is no distance to measure at all.
        assert_eq!(
            size_bound(&op(KOpKind::Segment {
                name: crate::op::SegmentName::new("OTHER"),
                addressing: None,
            })),
            Bound::Unbounded
        );

        // `.align n` emits up to `n - 1` bytes of padding — an upper bound,
        // which is what this pass needs and all it needs.
        assert_eq!(
            size_bound(&op(KOpKind::Align(KExpr::number(64)))),
            Bound::Bytes(63)
        );
        assert_eq!(
            size_bound(&op(KOpKind::Align(KExpr::Symbol(
                crate::symbol::SymbolId::from_index(0)
            )))),
            Bound::Unbounded
        );

        // A reservation is its folded constant, and unbounded when the count
        // is link-time.
        assert_eq!(
            size_bound(&op(KOpKind::Data(DataOp::Reserve(KExpr::number(12))))),
            Bound::Bytes(12)
        );
        assert_eq!(
            size_bound(&op(KOpKind::Data(DataOp::Reserve(KExpr::Symbol(
                crate::symbol::SymbolId::from_index(0)
            ))))),
            Bound::Unbounded
        );

        // The rows that emit nothing, and the one that emits a jump.
        assert_eq!(size_bound(&op(KOpKind::SkipTarget)), Bound::Bytes(0));
        assert_eq!(size_bound(&op(KOpKind::SkipData)), Bound::Bytes(3));
        assert_eq!(
            size_bound(&op(KOpKind::ModeAssumption(ModeState::unknown()))),
            Bound::Bytes(0)
        );
        assert_eq!(
            size_bound(&op(KOpKind::NocrossGuard { length: 40 })),
            Bound::Bytes(0)
        );

        // And a data entry is its values times their width.
        assert_eq!(
            size_bound(&op(KOpKind::Data(DataOp::Word(vec![
                KExpr::number(1),
                KExpr::number(2),
            ])))),
            Bound::Bytes(4)
        );
    }

    /// A branch measures the operations *between* it and its target, so one
    /// unbounded operation refuses the shrink and one outside the span does
    /// not.
    #[test]
    fn the_span_a_branch_measures_is_what_lies_between_it_and_its_target() {
        let unbounded = op(KOpKind::Segment {
            name: crate::op::SegmentName::new("OTHER"),
            addressing: None,
        });
        assert_eq!(size_bound(&unbounded), Bound::Unbounded);
        assert_eq!(
            size_bound(&op(KOpKind::Data(DataOp::Byte(vec![KExpr::number(0)])))),
            Bound::Bytes(1)
        );
    }
}
