use rustc_hash::FxHashMap;

use crate::hir::{AddressValue, InstructionOp, Op, OperandOp, Program};
use crate::span::Spanned;

#[cfg(test)]
use crate::hir::AddressSizeHint;

/// Returns (needs_m, needs_x) for an instruction.
///
/// Thin wrapper over `k816_isa65816::mnemonic_width_sensitivity` that maps the
/// HIR's `operand: None` convention to the ISA helper's `on_accumulator` flag.
pub(crate) fn instruction_mode_needs(instruction: &InstructionOp) -> (bool, bool) {
    k816_isa65816::mnemonic_width_sensitivity(&instruction.mnemonic, instruction.operand.is_none())
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
            Op::FunctionStart {
                name,
                mode_contract,
                ..
            } => {
                current_function = Some(name.clone());
                // A declared `@a*`/`@i*` contract IS a need: the function's
                // ABI says callers must establish that width, and the entry
                // prologue (for entry points) or callers (for non-entry)
                // must keep that REP/SEP alive. Seeding from the contract
                // here also lets propagation lift transitive callers.
                cur_m = mode_contract.a_width.is_some();
                cur_x = mode_contract.i_width.is_some();
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
                        && let Some(&(target_m, target_x)) = needs.get(target)
                    {
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
    // Entry-point functions establish their declared mode in the prologue
    // REP/SEP emitted by lowering. Seeding the backward scan from the
    // declared contract keeps that prologue from being stripped just because
    // the body delegates all width-sensitive work to (possibly extern)
    // callees the local scan cannot see into.
    if let Some(Op::FunctionStart {
        mode_contract,
        is_entry: true,
        ..
    }) = ops.first().map(|op| &op.node)
    {
        need_m = mode_contract.a_width.is_some();
        need_x = mode_contract.i_width.is_some();
    }
    let mut keep = vec![true; ops.len()];
    // Adjusted masks for Rep/Sep (may have bits stripped).
    let mut masks: Vec<u8> = ops
        .iter()
        .map(|op| match &op.node {
            Op::Rep { mask, .. } | Op::Sep { mask, .. } => *mask,
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
                    && let Some(&(target_m, target_x)) = effective_needs.get(target)
                {
                    if target_m {
                        need_m = true;
                    }
                    if target_x {
                        need_x = true;
                    }
                }
            }
            Op::Rep { mask, fixed: true } | Op::Sep { mask, fixed: true } => {
                if mask & 0x20 != 0 {
                    need_m = false;
                }
                if mask & 0x10 != 0 {
                    need_x = false;
                }
            }
            Op::Rep { mask, fixed: false } | Op::Sep { mask, fixed: false } => {
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
            Op::FunctionStart { .. } => {
                // The declared contract is the function's published ABI; the
                // optimizer must not rewrite it. Pass through verbatim so the
                // linker sees the user's `@a*`/`@i*` declaration intact.
                result.push(op.clone());
            }
            Op::Rep { fixed: false, .. } => {
                result.push(Spanned::new(
                    Op::Rep {
                        mask: masks[idx],
                        fixed: false,
                    },
                    op.span,
                ));
            }
            Op::Sep { fixed: false, .. } => {
                result.push(Spanned::new(
                    Op::Sep {
                        mask: masks[idx],
                        fixed: false,
                    },
                    op.span,
                ));
            }
            Op::Rep { fixed: true, .. } | Op::Sep { fixed: true, .. } => {
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
            Op::Rep { mask, fixed: false } => {
                want_rep |= mask;
                want_sep &= !mask;
                last_mode_span = Some(op.span);
            }
            Op::Sep { mask, fixed: false } => {
                want_sep |= mask;
                want_rep &= !mask;
                last_mode_span = Some(op.span);
            }
            Op::Rep { fixed: true, .. } | Op::Sep { fixed: true, .. } => {
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
        out.push(Spanned::new(
            Op::Rep {
                mask: *want_rep,
                fixed: false,
            },
            span,
        ));
    }
    if *want_sep != 0 {
        out.push(Spanned::new(
            Op::Sep {
                mask: *want_sep,
                fixed: false,
            },
            span,
        ));
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
                    size_hint: AddressSizeHint::Auto,
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
                Spanned::new(
                    Op::Rep {
                        mask: 0x20,
                        fixed: false,
                    },
                    test_span(),
                ),
                Spanned::new(
                    Op::Rep {
                        mask: 0x10,
                        fixed: false,
                    },
                    test_span(),
                ),
                nop_op(),
            ],
        };
        let folded = fold_mode_ops(&program);
        assert_eq!(folded.ops.len(), 2); // REP #$30 + NOP
        assert!(matches!(folded.ops[0].node, Op::Rep { mask: 0x30, .. }));
    }

    #[test]
    fn cancels_opposite_mode_ops() {
        let program = Program {
            ops: vec![
                Spanned::new(
                    Op::Rep {
                        mask: 0x20,
                        fixed: false,
                    },
                    test_span(),
                ),
                Spanned::new(
                    Op::Sep {
                        mask: 0x20,
                        fixed: false,
                    },
                    test_span(),
                ),
                nop_op(),
            ],
        };
        let folded = fold_mode_ops(&program);
        // Rep(0x20) then Sep(0x20) => want_rep=0, want_sep=0x20 => emit Sep(0x20) + NOP
        assert_eq!(folded.ops.len(), 2);
        assert!(matches!(folded.ops[0].node, Op::Sep { mask: 0x20, .. }));
    }

    #[test]
    fn does_not_fold_across_non_mode_ops() {
        let program = Program {
            ops: vec![
                Spanned::new(
                    Op::Rep {
                        mask: 0x20,
                        fixed: false,
                    },
                    test_span(),
                ),
                nop_op(),
                Spanned::new(
                    Op::Rep {
                        mask: 0x10,
                        fixed: false,
                    },
                    test_span(),
                ),
                nop_op(),
            ],
        };
        let folded = fold_mode_ops(&program);
        assert_eq!(folded.ops.len(), 4); // REP #$20 + NOP + REP #$10 + NOP
        assert!(matches!(folded.ops[0].node, Op::Rep { mask: 0x20, .. }));
        assert!(matches!(folded.ops[2].node, Op::Rep { mask: 0x10, .. }));
    }

    #[test]
    fn mixed_rep_sep_folds_to_both() {
        let program = Program {
            ops: vec![
                Spanned::new(
                    Op::Rep {
                        mask: 0x20,
                        fixed: false,
                    },
                    test_span(),
                ), // A to 16-bit
                Spanned::new(
                    Op::Sep {
                        mask: 0x10,
                        fixed: false,
                    },
                    test_span(),
                ), // Index to 8-bit
                nop_op(),
            ],
        };
        let folded = fold_mode_ops(&program);
        assert_eq!(folded.ops.len(), 3); // REP #$20 + SEP #$10 + NOP
        assert!(matches!(folded.ops[0].node, Op::Rep { mask: 0x20, .. }));
        assert!(matches!(folded.ops[1].node, Op::Sep { mask: 0x10, .. }));
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
                Spanned::new(
                    Op::Rep {
                        mask: 0x20,
                        fixed: false,
                    },
                    span,
                ),
                abs_op("lda", 0x2000),
                Spanned::new(
                    Op::Sep {
                        mask: 0x20,
                        fixed: false,
                    },
                    span,
                ),
                abs_op("sta", 0x2000),
                Spanned::new(Op::FunctionEnd, span),
            ],
        };

        let pruned = eliminate_dead_mode_ops(&program);
        assert!(
            pruned
                .ops
                .iter()
                .any(|op| matches!(op.node, Op::Rep { mask: 0x20, .. }))
        );
        assert!(
            pruned
                .ops
                .iter()
                .any(|op| matches!(op.node, Op::Sep { mask: 0x20, .. }))
        );
    }

    fn function_start_with_contract(
        name: &str,
        contract: ModeContract,
        is_entry: bool,
    ) -> Spanned<Op> {
        Spanned::new(
            Op::FunctionStart {
                name: name.to_string(),
                mode_contract: contract,
                is_entry,
                is_far: false,
            },
            test_span(),
        )
    }

    fn jsr_to(target: &str) -> Spanned<Op> {
        Spanned::new(
            Op::Instruction(InstructionOp {
                mnemonic: "jsr".to_string(),
                operand: Some(OperandOp::Address {
                    value: AddressValue::Label(target.to_string()),
                    size_hint: AddressSizeHint::Auto,
                    mode: AddressOperandMode::Direct { index: None },
                }),
            }),
            test_span(),
        )
    }

    /// Locks in Change 2: the strip pass must never rewrite the user's
    /// declared `mode_contract`. The contract is the function's published
    /// ABI, used by the linker to validate every cross-unit caller.
    #[test]
    fn preserves_declared_contract_through_strip_pass() {
        use crate::ast::RegWidth;
        let contract = ModeContract {
            a_width: Some(RegWidth::W16),
            i_width: Some(RegWidth::W16),
        };
        let program = Program {
            ops: vec![
                function_start_with_contract("main", contract, false),
                nop_op(),
                Spanned::new(Op::FunctionEnd, test_span()),
            ],
        };

        let pruned = eliminate_dead_mode_ops(&program);
        let start = pruned
            .ops
            .iter()
            .find_map(|op| match &op.node {
                Op::FunctionStart { mode_contract, .. } => Some(mode_contract.clone()),
                _ => None,
            })
            .expect("FunctionStart in result");
        assert_eq!(start.a_width, Some(RegWidth::W16));
        assert_eq!(start.i_width, Some(RegWidth::W16));
    }

    /// Locks in Change 3: an entry-point function with a declared
    /// `@a16 @i16` contract whose body has no width-sensitive opcodes
    /// (because all width-sensitive work is delegated to extern callees)
    /// must keep its prologue REP/SEP. Otherwise the runtime tracker
    /// stays at the contract's initial state with no instruction to bring
    /// the CPU into agreement.
    #[test]
    fn keeps_entry_prologue_rep_for_declared_contract_with_empty_body() {
        use crate::ast::RegWidth;
        let contract = ModeContract {
            a_width: Some(RegWidth::W16),
            i_width: Some(RegWidth::W16),
        };
        let program = Program {
            ops: vec![
                function_start_with_contract("main", contract, true),
                Spanned::new(
                    Op::Rep {
                        mask: 0x20,
                        fixed: false,
                    },
                    test_span(),
                ),
                Spanned::new(
                    Op::Rep {
                        mask: 0x10,
                        fixed: false,
                    },
                    test_span(),
                ),
                jsr_to("extern_callee"),
                Spanned::new(Op::FunctionEnd, test_span()),
            ],
        };

        let pruned = eliminate_dead_mode_ops(&program);
        let kept_m = pruned
            .ops
            .iter()
            .any(|op| matches!(&op.node, Op::Rep { mask: 0x20, .. }));
        let kept_x = pruned
            .ops
            .iter()
            .any(|op| matches!(&op.node, Op::Rep { mask: 0x10, .. }));
        assert!(kept_m, "M-axis prologue REP must survive");
        assert!(kept_x, "X-axis prologue REP must survive");
    }

    /// Locks in Change 1: a function declaring `@i16` propagates that need
    /// to its callers via `compute_effective_needs`, even if the callee's
    /// body has no X-sensitive opcode. Without this, leaf-shaped
    /// dispatchers calling such a callee would see `(false, false)` and
    /// strip prologue REPs / corrupt their own contract.
    #[test]
    fn propagates_declared_contract_through_compute_effective_needs() {
        use crate::ast::RegWidth;
        let caller_contract = ModeContract::default();
        let callee_contract = ModeContract {
            a_width: None,
            i_width: Some(RegWidth::W16),
        };
        let program = Program {
            ops: vec![
                function_start_with_contract("caller", caller_contract, false),
                jsr_to("callee"),
                Spanned::new(Op::FunctionEnd, test_span()),
                function_start_with_contract("callee", callee_contract, false),
                nop_op(),
                Spanned::new(Op::FunctionEnd, test_span()),
            ],
        };

        let needs = compute_effective_needs(&program);
        let caller_needs = needs.get("caller").copied().unwrap_or((false, false));
        let callee_needs = needs.get("callee").copied().unwrap_or((false, false));
        assert!(callee_needs.1, "callee must need X (declared @i16)");
        assert!(
            caller_needs.1,
            "caller must inherit X-need by propagation through JSR callee",
        );
    }
}
