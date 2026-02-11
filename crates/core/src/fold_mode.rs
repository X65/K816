use crate::hir::{Op, Program};
use crate::span::Spanned;

/// Fold consecutive REP/SEP pseudo-ops in the HIR stream into minimal instructions.
///
/// Adjacent mode changes are merged: e.g. `@a16 @i16` becomes a single `REP #$30`
/// instead of two separate instructions. Redundant toggles cancel out.
pub fn fold_mode_ops(program: &Program) -> Program {
    let mut out: Vec<Spanned<Op>> = Vec::with_capacity(program.ops.len());
    let mut want_rep: u8 = 0;
    let mut want_sep: u8 = 0;
    let mut last_mode_span = None;

    for op in &program.ops {
        match &op.node {
            Op::Rep(mask) => {
                want_rep |= mask;
                want_sep &= !mask;
                last_mode_span = Some(op.span);
            }
            Op::Sep(mask) => {
                want_sep |= mask;
                want_rep &= !mask;
                last_mode_span = Some(op.span);
            }
            _ => {
                flush_mode_ops(&mut out, &mut want_rep, &mut want_sep, last_mode_span);
                last_mode_span = None;
                out.push(op.clone());
            }
        }
    }

    // Flush any trailing mode ops
    flush_mode_ops(&mut out, &mut want_rep, &mut want_sep, last_mode_span);

    Program { ops: out }
}

fn flush_mode_ops(
    out: &mut Vec<Spanned<Op>>,
    want_rep: &mut u8,
    want_sep: &mut u8,
    span: Option<crate::span::Span>,
) {
    let Some(span) = span else {
        return;
    };

    if *want_rep != 0 {
        out.push(Spanned::new(Op::Rep(*want_rep), span));
    }
    if *want_sep != 0 {
        out.push(Spanned::new(Op::Sep(*want_sep), span));
    }
    *want_rep = 0;
    *want_sep = 0;
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hir::InstructionOp;
    use crate::span::{SourceId, Span};

    fn test_span() -> Span {
        Span::new(SourceId(0), 0, 1)
    }

    fn nop_op() -> Spanned<Op> {
        Spanned::new(
            Op::Instruction(InstructionOp {
                mnemonic: "nop".to_string(),
                operand: None,
            }),
            test_span(),
        )
    }

    #[test]
    fn folds_adjacent_rep_ops() {
        let program = Program {
            ops: vec![
                Spanned::new(Op::Rep(0x20), test_span()),
                Spanned::new(Op::Rep(0x10), test_span()),
                nop_op(),
            ],
        };
        let folded = fold_mode_ops(&program);
        assert_eq!(folded.ops.len(), 2); // REP #$30 + NOP
        assert!(matches!(folded.ops[0].node, Op::Rep(0x30)));
    }

    #[test]
    fn cancels_opposite_mode_ops() {
        let program = Program {
            ops: vec![
                Spanned::new(Op::Rep(0x20), test_span()),
                Spanned::new(Op::Sep(0x20), test_span()),
                nop_op(),
            ],
        };
        let folded = fold_mode_ops(&program);
        // Rep(0x20) then Sep(0x20) => want_rep=0, want_sep=0x20 => emit Sep(0x20) + NOP
        assert_eq!(folded.ops.len(), 2);
        assert!(matches!(folded.ops[0].node, Op::Sep(0x20)));
    }

    #[test]
    fn does_not_fold_across_non_mode_ops() {
        let program = Program {
            ops: vec![
                Spanned::new(Op::Rep(0x20), test_span()),
                nop_op(),
                Spanned::new(Op::Rep(0x10), test_span()),
                nop_op(),
            ],
        };
        let folded = fold_mode_ops(&program);
        assert_eq!(folded.ops.len(), 4); // REP #$20 + NOP + REP #$10 + NOP
        assert!(matches!(folded.ops[0].node, Op::Rep(0x20)));
        assert!(matches!(folded.ops[2].node, Op::Rep(0x10)));
    }

    #[test]
    fn mixed_rep_sep_folds_to_both() {
        let program = Program {
            ops: vec![
                Spanned::new(Op::Rep(0x20), test_span()), // A to 16-bit
                Spanned::new(Op::Sep(0x10), test_span()), // Index to 8-bit
                nop_op(),
            ],
        };
        let folded = fold_mode_ops(&program);
        assert_eq!(folded.ops.len(), 3); // REP #$20 + SEP #$10 + NOP
        assert!(matches!(folded.ops[0].node, Op::Rep(0x20)));
        assert!(matches!(folded.ops[1].node, Op::Sep(0x10)));
    }
}
