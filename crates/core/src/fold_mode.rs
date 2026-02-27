use rustc_hash::FxHashMap;

use crate::ast::ModeContract;
use crate::hir::{AddressValue, InstructionOp, Op, OperandOp, Program};
use crate::span::Spanned;

/// Returns (needs_m, needs_x) for an instruction.
///
/// This models semantic width dependencies, not just immediate operand sizing.
fn instruction_mode_needs(instruction: &InstructionOp) -> (bool, bool) {
    let mnemonic = instruction.mnemonic.to_ascii_lowercase();
    match mnemonic.as_str() {
        // Accumulator-width sensitive (M flag).
        "adc" | "and" | "bit" | "cmp" | "eor" | "lda" | "ora" | "pha" | "pla" | "sbc" | "sta"
        | "stz" | "trb" | "tsb" => (true, false),
        // Index-width sensitive (X flag, applies to both X and Y registers).
        "cpx" | "cpy" | "dex" | "dey" | "inx" | "iny" | "ldx" | "ldy" | "phx" | "phy" | "plx"
        | "ply" | "stx" | "sty" | "tsx" | "txs" | "txy" | "tyx" => (false, true),
        // Transfer ops that cross A and index registers depend on both dimensions.
        "tax" | "tay" | "txa" | "tya" => (true, true),
        // Shift/rotate/inc/dec are width-sensitive only in accumulator form.
        "asl" | "dec" | "inc" | "lsr" | "rol" | "ror" if instruction.operand.is_none() => {
            (true, false)
        }
        _ => (false, false),
    }
}

/// Extract the label target from a JSR/JSL instruction.
fn call_target(instruction: &InstructionOp) -> Option<&str> {
    if instruction.mnemonic != "jsr" && instruction.mnemonic != "jsl" {
        return None;
    }
    match &instruction.operand {
        Some(OperandOp::Address {
            value: AddressValue::Label(name),
            ..
        }) => Some(name.as_str()),
        _ => None,
    }
}

/// Phase 1: compute which functions truly need accumulator (M) and index (X) width.
fn compute_effective_needs(program: &Program) -> FxHashMap<String, (bool, bool)> {
    let mut needs: FxHashMap<String, (bool, bool)> = FxHashMap::default();

    // Direct scan: mark functions that use width-sensitive instructions.
    let mut current_function: Option<String> = None;
    let mut cur_m = false;
    let mut cur_x = false;

    for op in &program.ops {
        match &op.node {
            Op::FunctionStart { name, .. } => {
                current_function = Some(name.clone());
                cur_m = false;
                cur_x = false;
            }
            Op::FunctionEnd => {
                if let Some(name) = current_function.take() {
                    needs.insert(name, (cur_m, cur_x));
                }
            }
            Op::Instruction(instruction) if current_function.is_some() => {
                let (m, x) = instruction_mode_needs(instruction);
                if m {
                    cur_m = true;
                }
                if x {
                    cur_x = true;
                }
            }
            _ => {}
        }
    }

    // Propagation: if F calls G and G needs M/X, F also needs M/X.
    loop {
        let mut changed = false;
        current_function = None;

        for op in &program.ops {
            match &op.node {
                Op::FunctionStart { name, .. } => {
                    current_function = Some(name.clone());
                }
                Op::FunctionEnd => {
                    current_function = None;
                }
                Op::Instruction(instruction) if current_function.is_some() => {
                    if let Some(target) = call_target(instruction)
                        && let Some(&(target_m, target_x)) = needs.get(target) {
                            let func_name = current_function.as_ref().unwrap();
                            let &(cur_m, cur_x) =
                                needs.get(func_name.as_str()).unwrap_or(&(false, false));
                            let new_m = cur_m || target_m;
                            let new_x = cur_x || target_x;
                            if new_m != cur_m || new_x != cur_x {
                                needs.insert(func_name.clone(), (new_m, new_x));
                                changed = true;
                            }
                        }
                }
                _ => {}
            }
        }

        if !changed {
            break;
        }
    }

    needs
}

/// Eliminate dead REP/SEP ops that have no effect on subsequent instructions.
///
/// Scans each function body backwards: a Rep/Sep is only kept when a
/// width-sensitive instruction or a call to a function that needs the
/// corresponding width follows before the next Rep/Sep on the same axis.
/// Function mode contracts are trimmed to match.
pub fn eliminate_dead_mode_ops(program: &Program) -> Program {
    let effective_needs = compute_effective_needs(program);
    let mut out: Vec<Spanned<Op>> = Vec::with_capacity(program.ops.len());
    let mut i = 0;

    while i < program.ops.len() {
        if matches!(&program.ops[i].node, Op::FunctionStart { .. }) {
            // Find matching FunctionEnd.
            let func_start = i;
            let mut func_end = i + 1;
            while func_end < program.ops.len() {
                if matches!(&program.ops[func_end].node, Op::FunctionEnd) {
                    break;
                }
                func_end += 1;
            }

            let function_ops = &program.ops[func_start..=func_end];
            let processed = process_function(function_ops, &effective_needs);
            out.extend(processed);
            i = func_end + 1;
        } else {
            out.push(program.ops[i].clone());
            i += 1;
        }
    }

    Program { ops: out }
}

/// Backward-scan a single function and strip unnecessary Rep/Sep ops.
fn process_function(
    ops: &[Spanned<Op>],
    effective_needs: &FxHashMap<String, (bool, bool)>,
) -> Vec<Spanned<Op>> {
    let mut need_m = false;
    let mut need_x = false;
    let mut keep = vec![true; ops.len()];
    // Adjusted masks for Rep/Sep (may have bits stripped).
    let mut masks: Vec<u8> = ops
        .iter()
        .map(|op| match &op.node {
            Op::Rep(mask) | Op::Sep(mask) => *mask,
            _ => 0,
        })
        .collect();

    // Backward scan (skip FunctionStart at 0 and FunctionEnd at last).
    for idx in (1..ops.len().saturating_sub(1)).rev() {
        match &ops[idx].node {
            Op::Instruction(instruction) => {
                let (m, x) = instruction_mode_needs(instruction);
                if m {
                    need_m = true;
                }
                if x {
                    need_x = true;
                }
                // Calls to functions that need specific widths.
                if let Some(target) = call_target(instruction)
                    && let Some(&(target_m, target_x)) = effective_needs.get(target) {
                        if target_m {
                            need_m = true;
                        }
                        if target_x {
                            need_x = true;
                        }
                    }
            }
            Op::FixedRep(mask) | Op::FixedSep(mask) => {
                if mask & 0x20 != 0 {
                    need_m = false;
                }
                if mask & 0x10 != 0 {
                    need_x = false;
                }
            }
            Op::Rep(mask) | Op::Sep(mask) => {
                let mut new_mask = 0u8;
                if mask & 0x20 != 0 {
                    if need_m {
                        new_mask |= 0x20;
                    }
                    need_m = false;
                }
                if mask & 0x10 != 0 {
                    if need_x {
                        new_mask |= 0x10;
                    }
                    need_x = false;
                }
                if new_mask == 0 {
                    keep[idx] = false;
                } else {
                    masks[idx] = new_mask;
                }
            }
            _ => {}
        }
    }

    // Rebuild output with updated FunctionStart contract and pruned ops.
    let mut result = Vec::with_capacity(ops.len());
    for (idx, op) in ops.iter().enumerate() {
        if !keep[idx] {
            continue;
        }
        match &op.node {
            Op::FunctionStart {
                name,
                mode_contract,
                is_entry,
                is_far,
            } => {
                let (fn_needs_m, fn_needs_x) = effective_needs
                    .get(name.as_str())
                    .copied()
                    .unwrap_or((false, false));
                let new_contract = ModeContract {
                    a_width: if fn_needs_m {
                        mode_contract.a_width
                    } else {
                        None
                    },
                    i_width: if fn_needs_x {
                        mode_contract.i_width
                    } else {
                        None
                    },
                };
                result.push(Spanned::new(
                    Op::FunctionStart {
                        name: name.clone(),
                        mode_contract: new_contract,
                        is_entry: *is_entry,
                        is_far: *is_far,
                    },
                    op.span,
                ));
            }
            Op::Rep(_) => {
                result.push(Spanned::new(Op::Rep(masks[idx]), op.span));
            }
            Op::Sep(_) => {
                result.push(Spanned::new(Op::Sep(masks[idx]), op.span));
            }
            Op::FixedRep(_) | Op::FixedSep(_) => {
                result.push(op.clone());
            }
            _ => {
                result.push(op.clone());
            }
        }
    }

    result
}

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
            Op::FixedRep(_) | Op::FixedSep(_) => {
                flush_mode_ops(&mut out, &mut want_rep, &mut want_sep, last_mode_span);
                last_mode_span = None;
                out.push(op.clone());
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
    use crate::ast::ModeContract;
    use crate::hir::{AddressOperandMode, AddressValue, InstructionOp, OperandOp};
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

    fn abs_op(mnemonic: &str, literal: u32) -> Spanned<Op> {
        Spanned::new(
            Op::Instruction(InstructionOp {
                mnemonic: mnemonic.to_string(),
                operand: Some(OperandOp::Address {
                    value: AddressValue::Literal(literal),
                    force_far: false,
                    mode: AddressOperandMode::Direct { index: None },
                }),
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

    #[test]
    fn keeps_mode_ops_for_width_sensitive_memory_instructions() {
        let span = test_span();
        let program = Program {
            ops: vec![
                Spanned::new(
                    Op::FunctionStart {
                        name: "main".to_string(),
                        mode_contract: ModeContract::default(),
                        is_entry: true,
                        is_far: false,
                    },
                    span,
                ),
                Spanned::new(Op::Rep(0x20), span),
                abs_op("lda", 0x2000),
                Spanned::new(Op::Sep(0x20), span),
                abs_op("sta", 0x2000),
                Spanned::new(Op::FunctionEnd, span),
            ],
        };

        let pruned = eliminate_dead_mode_ops(&program);
        assert!(pruned.ops.iter().any(|op| matches!(op.node, Op::Rep(0x20))));
        assert!(pruned.ops.iter().any(|op| matches!(op.node, Op::Sep(0x20))));
    }
}
