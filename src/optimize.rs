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
//! receive identical treatment — passes see only [`KOp`]s. The slice ships
//! one pass, removal of overwritten register values; the full fixed pass
//! sequence is sub-plan F.

use crate::effects::{ControlEffect, Effects, MemoryEffect};
use crate::op::{KOp, KOpKind, KProgram};

/// Why one operation was removed: the record a test or `emit-ir` reader can
/// audit. `effects` are the removed op's own effects — the justification is
/// that everything it writes is overwritten before any read.
#[derive(Clone, Debug)]
pub struct Removal {
    pub removed: KOp,
    /// Index of the removed op within its function's pre-pass stream.
    pub index: usize,
    /// The op whose write completed the overwrite of the removed op's state.
    pub overwritten_by: KOp,
    pub effects: Effects,
}

/// What the optimizer did, for inspection and tests.
#[derive(Debug, Default)]
pub struct OptimizationLog {
    pub removals: Vec<Removal>,
}

/// Runs the (slice-sized) fixed pass sequence over `program`.
pub fn optimize(program: &mut KProgram) -> OptimizationLog {
    let mut log = OptimizationLog::default();
    for function in program.functions_mut() {
        remove_overwritten_register_values(&mut function.ops, &mut log);
    }
    log
}

/// Removes instructions whose entire written register/flag state is
/// overwritten before any operation reads it.
///
/// Candidates must have no memory effect and fall through — observable
/// memory, stack, and control effects are never removed. The forward scan is
/// conservative: labels and non-fallthrough control are barriers (the state
/// may be observed elsewhere), reads are checked before writes so
/// read-modify-write operations keep their sources alive, and unknown
/// memory effects stop the scan.
fn remove_overwritten_register_values(ops: &mut Vec<KOp>, log: &mut OptimizationLog) {
    let mut index = 0;
    while index < ops.len() {
        match removable(ops, index) {
            Some(overwriter) => {
                let removed = ops.remove(index);
                let effects = match &removed.kind {
                    KOpKind::Instruction(instruction) => instruction.effects(),
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
                    | KOpKind::ModeAssumption(_) => {
                        unreachable!("only instructions are removal candidates")
                    }
                };
                log.removals.push(Removal {
                    removed,
                    index,
                    // `overwriter` counted positions before the removal.
                    overwritten_by: ops[overwriter - 1].clone(),
                    effects,
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
            // sub-plan F says otherwise.
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
