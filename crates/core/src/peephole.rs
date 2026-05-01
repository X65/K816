use k816_isa65816::{RegSet, mnemonic_effects};

use crate::hir::{
    AddressOperandMode, AddressSizeHint, IndexRegister, InstructionOp, Op, OperandOp, Program,
};
use crate::span::Spanned;

/// Returns `true` if the mnemonic is an unconditional return instruction.
fn is_return_mnemonic(m: &str) -> bool {
    matches!(m, "rts" | "rtl" | "rti")
}

/// Peephole optimization pass.
///
/// Runs the collected peephole rewrites over the whole program in order:
/// 1. `stz_rewrite` — replaces `LDA #0; STA mem[,X]` runs with `STZ`.
/// 2. `collapse_duplicate_returns` — drops a trailing `rts`/`rtl`/`rti`
///    that is preceded by an identical return.
pub fn peephole_optimize(program: &Program) -> Program {
    let ops = stz_rewrite(&program.ops);
    let ops = collapse_duplicate_returns(&ops);
    Program { ops }
}

fn collapse_duplicate_returns(ops: &[Spanned<Op>]) -> Vec<Spanned<Op>> {
    let mut out: Vec<Spanned<Op>> = Vec::with_capacity(ops.len());
    let mut last_return: Option<&str> = None;

    for op in ops {
        match &op.node {
            Op::Instruction(InstructionOp {
                mnemonic,
                operand: None,
            }) if is_return_mnemonic(mnemonic) => {
                if last_return == Some(mnemonic.as_str()) {
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
            _ => {
                last_return = None;
                out.push(op.clone());
            }
        }
    }

    out
}

/// Rewrite `LDA #0` followed by one or more `STA mem[,X]` into `STZ` stores.
///
/// STZ supports direct-page, direct-page,X, absolute, and absolute,X addressing
/// modes (but not absolute-long or any indirect/Y form). When every `STA` in
/// the run can be rewritten and the remaining `A=0` value is dead immediately
/// afterwards, the leading `LDA #0` is dropped.
fn stz_rewrite(ops: &[Spanned<Op>]) -> Vec<Spanned<Op>> {
    let mut out: Vec<Spanned<Op>> = Vec::with_capacity(ops.len());
    let mut i = 0;
    while i < ops.len() {
        if !is_lda_zero(&ops[i].node) {
            out.push(ops[i].clone());
            i += 1;
            continue;
        }

        // Collect the contiguous run of rewrite-eligible `STA` ops.
        let mut run_end = i + 1;
        let mut any_rewrite = false;
        while run_end < ops.len() {
            if let Op::Instruction(inst) = &ops[run_end].node
                && inst.mnemonic == "sta"
                && stz_compatible(inst.operand.as_ref())
            {
                run_end += 1;
                any_rewrite = true;
                continue;
            }
            break;
        }

        if !any_rewrite {
            // No STA-to-STZ rewrite possible; emit the LDA unchanged.
            out.push(ops[i].clone());
            i += 1;
            continue;
        }

        // Determine whether A is dead immediately after the run: the next op
        // must be an instruction that overwrites A without reading it.
        let a_dead_after = ops
            .get(run_end)
            .map(|next| a_dead_at(&next.node))
            .unwrap_or(false);

        if !a_dead_after {
            out.push(ops[i].clone());
        }
        for op in &ops[i + 1..run_end] {
            let mut stz_op = op.clone();
            if let Op::Instruction(inst) = &mut stz_op.node {
                inst.mnemonic = "stz".to_string();
            }
            out.push(stz_op);
        }
        i = run_end;
    }
    out
}

fn is_lda_zero(op: &Op) -> bool {
    matches!(
        op,
        Op::Instruction(InstructionOp {
            mnemonic,
            operand: Some(OperandOp::Immediate(0)),
        }) if mnemonic == "lda"
    )
}

fn stz_compatible(operand: Option<&OperandOp>) -> bool {
    let Some(OperandOp::Address {
        size_hint, mode, ..
    }) = operand
    else {
        return false;
    };
    if matches!(size_hint, AddressSizeHint::ForceAbsoluteLong) {
        return false;
    }
    matches!(
        mode,
        AddressOperandMode::Direct {
            index: None | Some(IndexRegister::X),
        }
    )
}

/// Returns `true` if `op` definitely overwrites A without reading it.
fn a_dead_at(op: &Op) -> bool {
    let Op::Instruction(inst) = op else {
        return false;
    };
    let on_accumulator = inst.operand.is_none();
    let effects = mnemonic_effects(&inst.mnemonic, on_accumulator);
    effects.modifies.contains(RegSet::A) && !effects.reads.contains(RegSet::A)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hir::AddressValue;
    use crate::span::{SourceId, Span, Spanned};

    fn op(node: Op) -> Spanned<Op> {
        Spanned::new(node, Span::new(SourceId(0), 0, 0))
    }

    fn instr(mnemonic: &str, operand: Option<OperandOp>) -> Spanned<Op> {
        op(Op::Instruction(InstructionOp {
            mnemonic: mnemonic.to_string(),
            operand,
        }))
    }

    fn sta_dp(addr: u32) -> Spanned<Op> {
        instr(
            "sta",
            Some(OperandOp::Address {
                value: AddressValue::Literal(addr),
                size_hint: AddressSizeHint::Auto,
                mode: AddressOperandMode::Direct { index: None },
            }),
        )
    }

    fn mnemonics(program: &Program) -> Vec<&str> {
        program
            .ops
            .iter()
            .filter_map(|sp| match &sp.node {
                Op::Instruction(inst) => Some(inst.mnemonic.as_str()),
                _ => None,
            })
            .collect()
    }

    /// `tdc` overwrites A without reading it (per `mnemonic_effects`), so the
    /// preceding `lda #0` is dead and gets dropped, leaving just the `stz`.
    #[test]
    fn drops_lda_zero_before_tdc() {
        let program = Program {
            ops: vec![
                instr("lda", Some(OperandOp::Immediate(0))),
                sta_dp(0x10),
                instr("tdc", None),
            ],
        };
        let out = peephole_optimize(&program);
        assert_eq!(mnemonics(&out), vec!["stz", "tdc"]);
    }

    /// `xba` reads *and* modifies A (it byte-swaps), so it does NOT make A
    /// dead. The leading `lda #0` must be preserved.
    #[test]
    fn keeps_lda_zero_before_xba() {
        let program = Program {
            ops: vec![
                instr("lda", Some(OperandOp::Immediate(0))),
                sta_dp(0x10),
                instr("xba", None),
            ],
        };
        let out = peephole_optimize(&program);
        assert_eq!(mnemonics(&out), vec!["lda", "stz", "xba"]);
    }
}
