use crate::hir::{InstructionOp, Op, Program};
use crate::span::Spanned;

/// Returns `true` if the mnemonic is an unconditional return instruction.
fn is_return_mnemonic(m: &str) -> bool {
    matches!(m, "rts" | "rtl" | "rti")
}

/// Peephole optimization pass: collapse consecutive identical return instructions.
///
/// When two adjacent `rts`, `rtl`, or `rti` instructions appear with no
/// intervening labels, function boundaries, or other ops, the duplicate is
/// dropped.  A label between two returns resets tracking because the second
/// may be a branch target.
pub fn peephole_optimize(program: &Program) -> Program {
    let mut out: Vec<Spanned<Op>> = Vec::with_capacity(program.ops.len());
    let mut last_return: Option<&str> = None;

    for op in &program.ops {
        match &op.node {
            Op::Instruction(InstructionOp {
                mnemonic,
                operand: None,
            }) if is_return_mnemonic(mnemonic) => {
                if last_return == Some(mnemonic.as_str()) {
                    // Duplicate return — skip it.
                    continue;
                }
                last_return = Some(match mnemonic.as_str() {
                    "rts" => "rts",
                    "rtl" => "rtl",
                    "rti" => "rti",
                    _ => unreachable!(),
                });
                out.push(op.clone());
            }
            // Any other op resets tracking.
            _ => {
                last_return = None;
                out.push(op.clone());
            }
        }
    }

    Program { ops: out }
}
