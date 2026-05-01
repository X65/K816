use k816_isa65816::{RegSet, mnemonic_effects};

use crate::hir::{
    AddressOperandMode, AddressSizeHint, IndexRegister, InstructionOp, Op, OperandOp, Program,
};
use crate::span::Spanned;

/// Returns the canonical mnemonic for an unconditional return instruction.
fn return_mnemonic(m: &str) -> Option<&'static str> {
    if m.eq_ignore_ascii_case("rts") {
        Some("rts")
    } else if m.eq_ignore_ascii_case("rtl") {
        Some("rtl")
    } else if m.eq_ignore_ascii_case("rti") {
        Some("rti")
    } else {
        None
    }
}

/// Peephole optimization pass.
///
/// Runs the collected peephole rewrites over the whole program in order:
/// 1. `stz_rewrite` — replaces dead `LDA #0; STA mem[,X]` runs with `STZ`.
/// 2. `collapse_duplicate_returns` — drops an adjacent `rts`/`rtl`/`rti`
///    that is preceded by an identical return.
pub fn peephole_optimize(program: &Program) -> Program {
    let ops = stz_rewrite(&program.ops);
    let ops = collapse_duplicate_returns(&ops);
    Program { ops }
}

fn collapse_duplicate_returns(ops: &[Spanned<Op>]) -> Vec<Spanned<Op>> {
    let mut out: Vec<Spanned<Op>> = Vec::with_capacity(ops.len());
    let mut last_return: Option<&'static str> = None;

    for op in ops {
        match &op.node {
            Op::Instruction(InstructionOp {
                mnemonic,
                operand: None,
            }) => {
                let Some(return_mnemonic) = return_mnemonic(mnemonic) else {
                    last_return = None;
                    out.push(op.clone());
                    continue;
                };
                if last_return == Some(return_mnemonic) {
                    continue;
                }
                last_return = Some(return_mnemonic);
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

/// Rewrite dead `LDA #0` followed by one or more `STA mem[,X]` into `STZ`.
///
/// STZ supports direct-page, direct-page,X, absolute, and absolute,X addressing
/// modes (but not absolute-long or any indirect/Y form). This pass only
/// introduces STZ when it can also remove the corresponding accumulator load
/// and the status flags set by that load cannot be observed.
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
                && inst.mnemonic.eq_ignore_ascii_case("sta")
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

        // Determine whether the zero load is dead immediately after the run.
        // Dropping `lda #0` is safe only if the next instruction overwrites A
        // without reading it and refreshes the N/Z flags that `lda #0` would
        // have set. Otherwise a following branch or flag shorthand could
        // observe different processor state.
        let zero_load_dead_after = ops
            .get(run_end)
            .map(|next| a_zero_load_dead_at(&next.node))
            .unwrap_or(false);

        if zero_load_dead_after {
            // STZ is a shorter zero-store, but it no longer shows the store as
            // coming from A. Only perform that substitution when the original
            // zero load into A is dead and can be removed at the same time.
            // When A remains live, keeping the original STA sequence preserves
            // the programmer's expressed data flow in generated listings and
            // avoids turning a non-size-changing rewrite into a cosmetic one.
            for op in &ops[i + 1..run_end] {
                let mut stz_op = op.clone();
                if let Op::Instruction(inst) = &mut stz_op.node {
                    inst.mnemonic = "stz".to_string();
                }
                out.push(stz_op);
            }
        } else {
            out.extend(ops[i..run_end].iter().cloned());
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
        }) if mnemonic.eq_ignore_ascii_case("lda")
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

/// Returns `true` if `op` definitely kills both `A = 0` and the N/Z flags from
/// the removed `LDA #0`.
fn a_zero_load_dead_at(op: &Op) -> bool {
    let Op::Instruction(inst) = op else {
        return false;
    };
    let on_accumulator = inst.operand.is_none();
    let effects = mnemonic_effects(&inst.mnemonic, on_accumulator);
    let overwrites_a = effects.modifies.contains(RegSet::A) && !effects.reads.contains(RegSet::A);
    overwrites_a && refreshes_nz_after_a_overwrite(&inst.mnemonic)
}

fn refreshes_nz_after_a_overwrite(mnemonic: &str) -> bool {
    matches!(
        mnemonic.to_ascii_lowercase().as_str(),
        "lda" | "pla" | "txa" | "tya" | "tsc" | "tdc"
    )
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

    fn sta_absolute_long(addr: u32) -> Spanned<Op> {
        instr(
            "sta",
            Some(OperandOp::Address {
                value: AddressValue::Literal(addr),
                size_hint: AddressSizeHint::ForceAbsoluteLong,
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

    /// `tdc` overwrites A without reading it and refreshes N/Z, so the
    /// preceding `lda #0` is dead and gets dropped, leaving just the `stz`.
    #[test]
    fn drops_lda_zero_before_tdc_flag_refresh() {
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

    #[test]
    fn drops_lda_zero_before_tdc_even_with_later_branch() {
        let program = Program {
            ops: vec![
                instr("lda", Some(OperandOp::Immediate(0))),
                sta_dp(0x10),
                instr("tdc", None),
                instr("bne", None),
            ],
        };
        let out = peephole_optimize(&program);
        assert_eq!(mnemonics(&out), vec!["stz", "tdc", "bne"]);
    }

    /// `xba` reads *and* modifies A (it byte-swaps), so it does NOT make A
    /// dead. The leading `lda #0` must be preserved, including its flags.
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
        assert_eq!(mnemonics(&out), vec!["lda", "sta", "xba"]);
    }

    #[test]
    fn keeps_lda_zero_before_direct_flag_branch() {
        let program = Program {
            ops: vec![
                instr("lda", Some(OperandOp::Immediate(0))),
                sta_dp(0x10),
                instr("beq", None),
            ],
        };
        let out = peephole_optimize(&program);
        assert_eq!(mnemonics(&out), vec!["lda", "sta", "beq"]);
    }

    #[test]
    fn uppercase_lda_zero_sta_rewrites_when_a_is_dead() {
        let program = Program {
            ops: vec![
                instr("LDA", Some(OperandOp::Immediate(0))),
                instr(
                    "STA",
                    Some(OperandOp::Address {
                        value: AddressValue::Literal(0x10),
                        size_hint: AddressSizeHint::Auto,
                        mode: AddressOperandMode::Direct { index: None },
                    }),
                ),
                instr("LDA", Some(OperandOp::Immediate(1))),
            ],
        };
        let out = peephole_optimize(&program);
        assert_eq!(mnemonics(&out), vec!["stz", "LDA"]);
    }

    #[test]
    fn incompatible_sta_preserves_original_sequence() {
        let program = Program {
            ops: vec![
                instr("lda", Some(OperandOp::Immediate(0))),
                sta_absolute_long(0x12_3456),
                instr("lda", Some(OperandOp::Immediate(1))),
            ],
        };
        let out = peephole_optimize(&program);
        assert_eq!(mnemonics(&out), vec!["lda", "sta", "lda"]);
    }

    #[test]
    fn collapses_adjacent_duplicate_returns() {
        let program = Program {
            ops: vec![
                instr("rts", None),
                instr("RTS", None),
                instr("rtl", None),
                instr("RTL", None),
                instr("rti", None),
                instr("RTI", None),
            ],
        };
        let out = peephole_optimize(&program);
        assert_eq!(mnemonics(&out), vec!["rts", "rtl", "rti"]);
    }

    #[test]
    fn keeps_mixed_adjacent_returns() {
        let program = Program {
            ops: vec![instr("rts", None), instr("rtl", None), instr("rti", None)],
        };
        let out = peephole_optimize(&program);
        assert_eq!(mnemonics(&out), vec!["rts", "rtl", "rti"]);
    }
}
