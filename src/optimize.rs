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

use crate::effects::{ControlEffect, Effects, MemoryEffect, WidthEffect};
use crate::linkage::Linkage;
use crate::op::{KOp, KOpKind, KProgram};
use crate::semantic::SemanticModel;
use crate::source::FileId;
use crate::symbol::{SymbolId, SymbolTable};

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
    for function in program.functions_mut() {
        let symbol = function.symbol;
        remove_overwritten_register_values(symbol, &mut function.ops, symbols, &mut log);
    }
    log
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

/// Removes instructions whose entire written register/flag state is
/// overwritten before any operation reads it.
///
/// Candidates must have no memory effect and fall through — observable
/// memory, stack, and control effects are never removed — and must not be
/// mode instructions ([`is_mode_operation`]). The forward scan is
/// conservative: labels and non-fallthrough control are barriers (the state
/// may be observed elsewhere), reads are checked before writes so
/// read-modify-write operations keep their sources alive, and unknown
/// memory effects stop the scan.
fn remove_overwritten_register_values(
    function: SymbolId,
    ops: &mut Vec<KOp>,
    symbols: &SymbolTable,
    log: &mut OptimizationLog,
) {
    let mut index = 0;
    while index < ops.len() {
        match removable(ops, index) {
            Some(overwriter) => {
                let removed = ops.remove(index);
                // `overwriter` counted positions before the removal.
                let witness = ops[overwriter - 1].clone();
                let justification = format!(
                    "writes {}, all overwritten by `{}` before any read",
                    describe_state(&effects_of(&removed).expect("an instruction").writes),
                    crate::op::dump_op(&witness, symbols),
                );
                log.changes.push(Change {
                    pass: Pass::RemoveOverwritten,
                    site: Site { function, index },
                    before: vec![removed],
                    after: Vec::new(),
                    witness: Some(witness),
                    justification,
                });
                // Do not advance: the next op shifted into `index`.
            }
            None => index += 1,
        }
    }
}

/// If the op at `index` is provably dead, returns the index of the operation
/// that completed the overwrite of its written state.
fn removable(ops: &[KOp], index: usize) -> Option<usize> {
    let KOpKind::Instruction(candidate) = &ops[index].kind else {
        return None;
    };
    if is_mode_operation(&ops[index]) {
        return None;
    }
    let effects = candidate.effects();
    if !effects.is_removal_candidate() || effects.writes.is_empty() {
        return None;
    }

    let mut pending = effects.writes;
    for (offset, op) in ops.iter().enumerate().skip(index + 1) {
        let KOpKind::Instruction(next) = &op.kind else {
            // A label (control may enter here and observe the state), a
            // directive whose placement effect this pass cannot reason
            // about, or a mode assumption — which states what the width
            // model concluded, so removing an op across one could leave the
            // claim false. Every non-instruction op is a barrier until
            // sub-plan F's block view replaces this scan.
            return None;
        };
        let next_effects = next.effects();
        // Reads are checked before writes: a read-modify-write op keeps its
        // own sources alive.
        if next_effects.reads.intersects(pending) {
            return None;
        }
        pending = pending.without(next_effects.writes);
        if pending.is_empty() {
            return Some(offset);
        }
        // Unknown effects inhibit optimization; other control flow may
        // observe the remaining state. An unknown width transition is the
        // third stop: `PLP`, `RTI`, `XCE`, and an unfolded `SEP`/`REP` mask
        // may narrow the index registers and truncate them, which their
        // must-write set cannot express.
        if next_effects.memory == MemoryEffect::Unknown
            || next_effects.control != ControlEffect::FallThrough
            || next_effects.width.is_unknown()
        {
            return None;
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
