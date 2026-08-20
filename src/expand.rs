/*****************************************************************************/
/*                                                                           */
/*                                 expand.rs                                 */
/*                                                                           */
/*                  `inline` expansion at the use site                       */
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

//! `inline` expansion: turning one section's operations into a copy that
//! belongs to the body it was written into.
//!
//! An `inline` is lowered once, against its own contract — that is what
//! sub-plan E's decision 12 means by "analyzed once, at the declaration", and
//! it is why the same body does not have to be re-checked at every use site.
//! What is left is a *rewrite* of the operations it produced, and this module
//! is that rewrite. Two things have to change on the way in:
//!
//! **Names that belong to the `inline` cannot survive.** A label defined in
//! an expanded body would be defined again by the next expansion, so every
//! label the section declares becomes a fresh anchor of the enclosing
//! function — a ca65 unnamed label, the same answer sub-plan E already gives
//! for a generated branch target, and for the same reason: a synthesized name
//! either pollutes the object file or collides in the cheap-local scope.
//!
//! **Anchors have to move.** The body's own generated branch targets were
//! numbered from zero by the lowering that produced them, and the enclosing
//! function has its own numbering to keep.
//!
//! Aliases are *not* substituted here. They are bound in the resolver before
//! the body is lowered, so the one operand classifier sizes an alias-bearing
//! operand exactly as it sizes every other — against the widths the section's
//! contract establishes — and an argument that does not fit is reported by
//! the machinery that reports every other one. Substituting afterwards would
//! mean a second answer to a question the classifier already answers, which
//! is the failure `resolve.rs` exists to prevent.

use std::collections::BTreeMap;

use crate::op::{AnchorId, Instruction, KOp, KOpKind, Operand};
use crate::symbol::SymbolId;
use crate::value::KExpr;

/// Rewrites one lowered `inline` body into operations of the enclosing
/// function.
///
/// `anchors` is the enclosing function's anchor counter, read for the base
/// and advanced past everything this expansion claims — the body's own
/// generated targets first, then one for each label it declares.
pub fn splice(ops: Vec<KOp>, anchors: &mut u32) -> Vec<KOp> {
    let base = *anchors;
    // The body's anchors were numbered from zero by the lowering that
    // produced them, so shifting by the base is enough to make them the
    // enclosing function's — as long as the shift covers every one it used,
    // which is the highest it mentions plus one.
    let used = highest_anchor(&ops).map_or(0, |highest| highest + 1);
    let mut next = base + used;
    let mut labels: BTreeMap<SymbolId, AnchorId> = BTreeMap::new();
    for op in &ops {
        if let KOpKind::Label(symbol) = op.kind {
            labels.entry(symbol).or_insert_with(|| {
                let anchor = AnchorId::new(next);
                next += 1;
                anchor
            });
        }
    }
    *anchors = next;

    let shift = |anchor: AnchorId| AnchorId::new(base + anchor.index());
    ops.into_iter()
        .map(|mut op| {
            op.kind = match op.kind {
                KOpKind::Label(symbol) => match labels.get(&symbol) {
                    Some(anchor) => KOpKind::Anchor(*anchor),
                    // A label the body defines is always in the map; this is
                    // the shape the match takes rather than a reachable case.
                    None => KOpKind::Label(symbol),
                },
                KOpKind::Anchor(anchor) => KOpKind::Anchor(shift(anchor)),
                KOpKind::Instruction(instruction) => KOpKind::Instruction(Instruction {
                    operand: instruction
                        .operand
                        .map(|operand| rewrite_operand(operand, &labels, &shift)),
                    ..instruction
                }),
                other => other,
            };
            op
        })
        .collect()
}

/// The largest anchor number any operation mentions, defining or using.
fn highest_anchor(ops: &[KOp]) -> Option<u32> {
    ops.iter()
        .filter_map(|op| match &op.kind {
            KOpKind::Anchor(anchor) => Some(anchor.index()),
            KOpKind::Instruction(Instruction {
                operand: Some(Operand::Anchor(anchor)),
                ..
            }) => Some(anchor.index()),
            _ => None,
        })
        .max()
}

/// One operand, with the section's targets replaced by the enclosing
/// function's.
fn rewrite_operand(
    operand: Operand,
    labels: &BTreeMap<SymbolId, AnchorId>,
    shift: &impl Fn(AnchorId) -> AnchorId,
) -> Operand {
    if let Some(anchor) = branch_target(&operand).and_then(|symbol| labels.get(&symbol)) {
        return Operand::Anchor(*anchor);
    }
    match operand {
        Operand::Anchor(anchor) => Operand::Anchor(shift(anchor)),
        other => other,
    }
}

/// The symbol a transfer names, when the operand is a bare symbolic target.
///
/// Only a whole-symbol operand qualifies: `bcc .loop` and `jmp .loop` name a
/// label, while `lda .loop+1` names a *byte* of one, which stays the
/// expression it was and is ld65's to resolve — except that ld65 will never
/// see it, because a label of an expanded body has no name to resolve. Such a
/// reference is why `E0129` exists: it can only be written from outside the
/// section, and from outside there is nothing to point at.
fn branch_target(operand: &Operand) -> Option<SymbolId> {
    match operand {
        Operand::Label(KExpr::Symbol(symbol)) => Some(*symbol),
        Operand::Address {
            expr: KExpr::Symbol(symbol),
            index: None,
            ..
        } => Some(*symbol),
        _ => None,
    }
}
