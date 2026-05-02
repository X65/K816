//! Lowering of HLA-flavoured statements (`a = …`, `mem = 0`, do/while, etc.).
//!
//! Split out of `lower.rs` to keep that file under its own weight; the dispatcher
//! and its two helpers consume only types/functions defined in the parent module
//! and `crate::ast`/`crate::hir`/`crate::sema`/`crate::span`.

use crate::ast::{
    Expr, HlaAluOp, HlaCompareOp, HlaCondition, HlaCpuRegister, HlaFlag, HlaIncDecOp,
    HlaIncDecTarget, HlaOperandExpr, HlaRegister, HlaRhs, HlaShiftOp, HlaShiftTarget,
    HlaStackTarget, HlaStmt, NumFmt, Operand, OperandAddrMode,
};
use crate::diag::Diagnostic;
use crate::hir::Op;
use crate::sema::SemanticModel;
use crate::span::{Span, Spanned};

use super::{
    LowerContext, emit_branch_to_label, eval_to_number, format_hla_cpu_register, fresh_local_label,
    invalid_transfer_hint, load_mnemonic_for_register, lower_assignment_chain,
    lower_hla_operand_to_operand, lower_instruction_stmt, make_instruction, resolve_break_labels,
    resolve_transfer, store_mnemonic_for_register, stz_compatible_dest_mode,
};

pub(super) fn lower_hla_stmt(
    stmt: &HlaStmt,
    span: Span,
    scope: Option<&str>,
    sema: &SemanticModel,
    ctx: &mut LowerContext,
    diagnostics: &mut Vec<Diagnostic>,
    ops: &mut Vec<Spanned<Op>>,
) {
    match stmt {
        HlaStmt::RegisterAssign { register, rhs } => {
            let Some(mnemonic) = load_mnemonic_for_register(*register) else {
                match register {
                    HlaCpuRegister::C => diagnostics.push(Diagnostic::error(
                        span,
                        "C is the 16-bit accumulator; use a=expr for loads",
                    )),
                    _ => diagnostics.push(Diagnostic::error(
                        span,
                        format!(
                            "cannot load register '{}'",
                            format_hla_cpu_register(*register)
                        ),
                    )),
                }
                return;
            };
            let instruction = make_instruction(mnemonic, Some(lower_hla_operand_to_operand(rhs)));
            lower_instruction_stmt(&instruction, scope, sema, span, ctx, diagnostics, ops);
        }
        HlaStmt::RegisterStore { dest, src } => {
            let Some(mnemonic) = store_mnemonic_for_register(*src) else {
                diagnostics.push(Diagnostic::error(
                    span,
                    format!(
                        "cannot store register '{}' with assignment syntax",
                        format_hla_cpu_register(*src)
                    ),
                ));
                return;
            };
            let instruction = make_instruction(
                mnemonic,
                Some(Operand::Value {
                    expr: dest.expr.clone(),
                    force_far: false,
                    index: dest.index,
                    addr_mode: dest.addr_mode,
                }),
            );
            lower_instruction_stmt(&instruction, scope, sema, span, ctx, diagnostics, ops);
        }
        HlaStmt::MemStoreZero { dest } => {
            if !stz_compatible_dest_mode(dest) {
                diagnostics.push(Diagnostic::error(
                    span,
                    "'mem = 0' requires zp, zp,X, abs, or abs,X addressing — \
                     use 'mem = a = 0' for other modes",
                ));
                return;
            }
            let instruction = make_instruction(
                "stz",
                Some(Operand::Value {
                    expr: dest.expr.clone(),
                    force_far: false,
                    index: dest.index,
                    addr_mode: dest.addr_mode,
                }),
            );
            lower_instruction_stmt(&instruction, scope, sema, span, ctx, diagnostics, ops);
        }
        HlaStmt::RegisterTransfer { dest, src } => {
            let Some(mnemonic) = resolve_transfer(*dest, *src) else {
                let msg = format!(
                    "transfer '{}' to '{}' is not directly supported",
                    format_hla_cpu_register(*src).to_ascii_uppercase(),
                    format_hla_cpu_register(*dest).to_ascii_uppercase(),
                );
                diagnostics.push(
                    Diagnostic::error(span, msg)
                        .with_optional_help(invalid_transfer_hint(*dest, *src)),
                );
                return;
            };
            let instruction = make_instruction(mnemonic, None);
            lower_instruction_stmt(&instruction, scope, sema, span, ctx, diagnostics, ops);
        }
        HlaStmt::AssignmentChain { idents, tail_expr } => {
            lower_assignment_chain(
                idents,
                tail_expr.as_ref(),
                scope,
                sema,
                span,
                ctx,
                diagnostics,
                ops,
            );
        }
        HlaStmt::AccumulatorAlu { op, rhs } => {
            let mnemonic = match op {
                HlaAluOp::Add => "adc",
                HlaAluOp::Sub => "sbc",
                HlaAluOp::And => "and",
                HlaAluOp::Or => "ora",
                HlaAluOp::Xor => "eor",
            };
            let instruction = make_instruction(mnemonic, Some(lower_hla_operand_to_operand(rhs)));
            lower_instruction_stmt(&instruction, scope, sema, span, ctx, diagnostics, ops);
        }
        HlaStmt::AccumulatorBitTest { rhs } => {
            let instruction = make_instruction("bit", Some(lower_hla_operand_to_operand(rhs)));
            lower_instruction_stmt(&instruction, scope, sema, span, ctx, diagnostics, ops);
        }
        HlaStmt::IndexCompare { register, rhs } => {
            let mnemonic = match register {
                crate::ast::IndexRegister::X => "cpx",
                crate::ast::IndexRegister::Y => "cpy",
                crate::ast::IndexRegister::S => {
                    diagnostics.push(Diagnostic::error(span, "cannot compare S register"));
                    return;
                }
            };
            let instruction = make_instruction(mnemonic, Some(lower_hla_operand_to_operand(rhs)));
            lower_instruction_stmt(&instruction, scope, sema, span, ctx, diagnostics, ops);
        }
        HlaStmt::IncDec { op, target } => {
            let mnemonic = match op {
                HlaIncDecOp::Inc => "inc",
                HlaIncDecOp::Dec => "dec",
            };
            match target {
                HlaIncDecTarget::Register(register) => {
                    let mnemonic = match (op, register) {
                        (HlaIncDecOp::Inc, crate::ast::IndexRegister::X) => "inx",
                        (HlaIncDecOp::Inc, crate::ast::IndexRegister::Y) => "iny",
                        (HlaIncDecOp::Dec, crate::ast::IndexRegister::X) => "dex",
                        (HlaIncDecOp::Dec, crate::ast::IndexRegister::Y) => "dey",
                        (_, crate::ast::IndexRegister::S) => {
                            diagnostics.push(Diagnostic::error(
                                span,
                                "cannot increment/decrement S register",
                            ));
                            return;
                        }
                    };
                    let instruction = make_instruction(mnemonic, None);
                    lower_instruction_stmt(&instruction, scope, sema, span, ctx, diagnostics, ops);
                }
                HlaIncDecTarget::Address(address) => {
                    let instruction = make_instruction(
                        mnemonic,
                        Some(Operand::Value {
                            expr: address.expr.clone(),
                            force_far: false,
                            index: address.index,
                            addr_mode: address.addr_mode,
                        }),
                    );
                    lower_instruction_stmt(&instruction, scope, sema, span, ctx, diagnostics, ops);
                }
            }
        }
        HlaStmt::ShiftRotate { op, target } => {
            let mnemonic = match op {
                HlaShiftOp::Asl => "asl",
                HlaShiftOp::Lsr => "lsr",
                HlaShiftOp::Rol => "rol",
                HlaShiftOp::Ror => "ror",
            };
            let instruction = match target {
                HlaShiftTarget::Accumulator => make_instruction(mnemonic, None),
                HlaShiftTarget::Address(address) => make_instruction(
                    mnemonic,
                    Some(Operand::Value {
                        expr: address.expr.clone(),
                        force_far: false,
                        index: address.index,
                        addr_mode: address.addr_mode,
                    }),
                ),
            };
            lower_instruction_stmt(&instruction, scope, sema, span, ctx, diagnostics, ops);
        }
        HlaStmt::FlagSet { flag, set } => {
            let mnemonic = match (flag, set) {
                (HlaFlag::Carry, true) => Some("sec"),
                (HlaFlag::Carry, false) => Some("clc"),
                (HlaFlag::Decimal, true) => Some("sed"),
                (HlaFlag::Decimal, false) => Some("cld"),
                (HlaFlag::Interrupt, true) => Some("sei"),
                (HlaFlag::Interrupt, false) => Some("cli"),
                (HlaFlag::Overflow, false) => Some("clv"),
                (HlaFlag::Overflow, true) => None,
            };
            let Some(mnemonic) = mnemonic else {
                diagnostics.push(Diagnostic::error(
                    span,
                    "overflow flag cannot be explicitly set with shorthand",
                ));
                return;
            };
            let instruction = make_instruction(mnemonic, None);
            lower_instruction_stmt(&instruction, scope, sema, span, ctx, diagnostics, ops);
        }
        HlaStmt::StackOp { target, push } => {
            let mnemonic = match (target, push) {
                (HlaStackTarget::A, true) => "pha",
                (HlaStackTarget::A, false) => "pla",
                (HlaStackTarget::X, true) => "phx",
                (HlaStackTarget::X, false) => "plx",
                (HlaStackTarget::Y, true) => "phy",
                (HlaStackTarget::Y, false) => "ply",
                (HlaStackTarget::B, true) => "phb",
                (HlaStackTarget::B, false) => "plb",
                (HlaStackTarget::D, true) => "phd",
                (HlaStackTarget::D, false) => "pld",
                (HlaStackTarget::P, true) => "php",
                (HlaStackTarget::P, false) => "plp",
            };
            let instruction = make_instruction(mnemonic, None);
            lower_instruction_stmt(&instruction, scope, sema, span, ctx, diagnostics, ops);
        }
        HlaStmt::Goto {
            target,
            indirect,
            far,
        } => {
            let instruction = make_instruction(
                if *far { "jml" } else { "jmp" },
                Some(Operand::Value {
                    expr: target.clone(),
                    force_far: *far,
                    index: None,
                    addr_mode: if *indirect {
                        OperandAddrMode::Indirect
                    } else {
                        OperandAddrMode::Direct
                    },
                }),
            );
            lower_instruction_stmt(&instruction, scope, sema, span, ctx, diagnostics, ops);
        }
        HlaStmt::BranchGoto {
            mnemonic, target, ..
        } => {
            let instruction = make_instruction(
                mnemonic,
                Some(Operand::Value {
                    expr: target.clone(),
                    force_far: false,
                    index: None,
                    addr_mode: OperandAddrMode::Direct,
                }),
            );
            lower_instruction_stmt(&instruction, scope, sema, span, ctx, diagnostics, ops);
        }
        HlaStmt::Return { interrupt } => {
            let mnemonic = if *interrupt {
                "rti"
            } else if ctx.is_far {
                "rtl"
            } else {
                "rts"
            };
            let instruction = make_instruction(mnemonic, None);
            lower_instruction_stmt(&instruction, scope, sema, span, ctx, diagnostics, ops);
        }
        HlaStmt::XAssignImmediate { rhs } => {
            let instruction = make_instruction(
                "ldx",
                Some(Operand::Immediate {
                    expr: rhs.clone(),
                    explicit_hash: false,
                }),
            );
            lower_instruction_stmt(&instruction, scope, sema, span, ctx, diagnostics, ops);
        }
        HlaStmt::XIncrement => {
            let instruction = make_instruction("inx", None);
            lower_instruction_stmt(&instruction, scope, sema, span, ctx, diagnostics, ops);
        }
        HlaStmt::StoreFromA {
            dests,
            rhs,
            load_start,
            store_end,
        } => {
            let lda_instruction = make_instruction(
                "lda",
                Some(match rhs {
                    HlaRhs::Immediate(expr) => Operand::Immediate {
                        expr: expr.clone(),
                        explicit_hash: false,
                    },
                    HlaRhs::Value {
                        expr,
                        index,
                        addr_mode,
                    } => lower_hla_operand_to_operand(&HlaOperandExpr {
                        expr: expr.clone(),
                        index: *index,
                        addr_mode: *addr_mode,
                    }),
                }),
            );
            let lda_span = load_start
                .map(|start| Span::new(span.source_id, start, span.end))
                .unwrap_or(span);
            let ops_len_before_lda = ops.len();
            lower_instruction_stmt(
                &lda_instruction,
                scope,
                sema,
                lda_span,
                ctx,
                diagnostics,
                ops,
            );
            if ops.len() == ops_len_before_lda {
                return;
            }

            // Store to each destination in reverse order (innermost first)
            let sta_span = store_end
                .map(|end| Span::new(span.source_id, span.start, end))
                .unwrap_or(span);
            for dest in dests.iter().rev() {
                let sta_instruction = make_instruction(
                    "sta",
                    Some(Operand::Value {
                        expr: Expr::Ident(dest.clone()),
                        force_far: false,
                        index: None,
                        addr_mode: OperandAddrMode::Direct,
                    }),
                );
                lower_instruction_stmt(
                    &sta_instruction,
                    scope,
                    sema,
                    sta_span,
                    ctx,
                    diagnostics,
                    ops,
                );
            }
        }
        HlaStmt::WaitLoopWhileNFlagClear { symbol } => {
            let Some(wait_label) = fresh_local_label("wait", ctx, scope, span, diagnostics) else {
                return;
            };
            ops.push(Spanned::new(Op::Label(wait_label.clone()), span));

            let bit_instruction = make_instruction(
                "bit",
                Some(Operand::Value {
                    expr: Expr::Ident(symbol.clone()),
                    force_far: false,
                    index: None,
                    addr_mode: OperandAddrMode::Direct,
                }),
            );
            let ops_len_before_bit = ops.len();
            lower_instruction_stmt(&bit_instruction, scope, sema, span, ctx, diagnostics, ops);
            if ops.len() == ops_len_before_bit {
                return;
            }

            emit_branch_to_label("bpl", &wait_label, scope, sema, span, ctx, diagnostics, ops);
        }
        HlaStmt::ConditionSeed { .. } => {
            if let HlaStmt::ConditionSeed { rhs, .. } = stmt {
                let instruction = make_instruction("cmp", Some(lower_hla_operand_to_operand(rhs)));
                lower_instruction_stmt(&instruction, scope, sema, span, ctx, diagnostics, ops);
            }
        }
        HlaStmt::DoOpen => {
            let Some(loop_label) = fresh_local_label("loop", ctx, scope, span, diagnostics) else {
                return;
            };
            let break_depth = ctx.break_targets.len();
            ctx.do_loop_targets.push((loop_label.clone(), break_depth));
            ops.push(Spanned::new(Op::Label(loop_label), span));
        }
        HlaStmt::DoCloseNFlagClear => {
            let Some((loop_target, break_depth)) = ctx.do_loop_targets.pop() else {
                diagnostics.push(
                    Diagnostic::error(span, "HLA do/while close without matching '{'")
                        .with_help("open loop with a standalone '{' line before the condition"),
                );
                return;
            };
            emit_branch_to_label(
                "bpl",
                &loop_target,
                scope,
                sema,
                span,
                ctx,
                diagnostics,
                ops,
            );
            resolve_break_labels(break_depth, ctx, span, ops);
        }
        HlaStmt::DoCloseNFlagSet => {
            let Some((loop_target, break_depth)) = ctx.do_loop_targets.pop() else {
                diagnostics.push(
                    Diagnostic::error(span, "HLA do/while close without matching '{'")
                        .with_help("open loop with a standalone '{' line before the condition"),
                );
                return;
            };
            emit_branch_to_label(
                "bmi",
                &loop_target,
                scope,
                sema,
                span,
                ctx,
                diagnostics,
                ops,
            );
            resolve_break_labels(break_depth, ctx, span, ops);
        }
        HlaStmt::DoCloseWithOp { op } => {
            let Some((loop_target, break_depth)) = ctx.do_loop_targets.pop() else {
                diagnostics.push(
                    Diagnostic::error(span, "HLA do/while close without matching '{'")
                        .with_help("open loop with a standalone '{' line before the condition"),
                );
                return;
            };
            lower_hla_postfix_close_op_branch(
                *op,
                &loop_target,
                span,
                scope,
                sema,
                ctx,
                diagnostics,
                ops,
            );
            resolve_break_labels(break_depth, ctx, span, ops);
        }
        HlaStmt::DoClose { condition } => {
            let Some((loop_target, break_depth)) = ctx.do_loop_targets.pop() else {
                diagnostics.push(
                    Diagnostic::error(span, "HLA do/while close without matching '{'")
                        .with_help("open loop with a standalone '{' line before the condition"),
                );
                return;
            };
            lower_hla_condition_branch(
                condition,
                &loop_target,
                span,
                scope,
                sema,
                ctx,
                diagnostics,
                ops,
            );
            resolve_break_labels(break_depth, ctx, span, ops);
        }
        HlaStmt::DoCloseAlways => {
            let Some((loop_target, break_depth)) = ctx.do_loop_targets.pop() else {
                diagnostics.push(
                    Diagnostic::error(span, "HLA do/while close without matching '{'")
                        .with_help("open loop with a standalone '{' line before the condition"),
                );
                return;
            };
            emit_branch_to_label(
                "bra",
                &loop_target,
                scope,
                sema,
                span,
                ctx,
                diagnostics,
                ops,
            );
            resolve_break_labels(break_depth, ctx, span, ops);
        }
        HlaStmt::DoCloseNever => {
            let Some((_loop_target, break_depth)) = ctx.do_loop_targets.pop() else {
                diagnostics.push(
                    Diagnostic::error(span, "HLA do/while close without matching '{'")
                        .with_help("open loop with a standalone '{' line before the condition"),
                );
                return;
            };
            resolve_break_labels(break_depth, ctx, span, ops);
        }
        HlaStmt::DoCloseBranch { mnemonic } => {
            let Some((loop_target, break_depth)) = ctx.do_loop_targets.pop() else {
                diagnostics.push(
                    Diagnostic::error(span, "HLA do/while close without matching '{'")
                        .with_help("open loop with a standalone '{' line before the condition"),
                );
                return;
            };
            emit_branch_to_label(
                mnemonic,
                &loop_target,
                scope,
                sema,
                span,
                ctx,
                diagnostics,
                ops,
            );
            resolve_break_labels(break_depth, ctx, span, ops);
        }
        HlaStmt::LoopBreak { mnemonic } => {
            if ctx.do_loop_targets.is_empty() {
                diagnostics.push(
                    Diagnostic::error(span, "'break' outside loop")
                        .with_help("'break' can only be used inside a '{' ... '}' loop"),
                );
                return;
            }
            let Some(break_label) = fresh_local_label("break", ctx, scope, span, diagnostics)
            else {
                return;
            };
            ctx.break_targets.push(break_label.clone());
            emit_branch_to_label(
                mnemonic,
                &break_label,
                scope,
                sema,
                span,
                ctx,
                diagnostics,
                ops,
            );
        }
        HlaStmt::LoopRepeat { mnemonic } => {
            let Some((loop_target, _)) = ctx.do_loop_targets.last() else {
                diagnostics.push(
                    Diagnostic::error(span, "'repeat' outside loop")
                        .with_help("'repeat' can only be used inside a '{' ... '}' loop"),
                );
                return;
            };
            let loop_target = loop_target.clone();
            emit_branch_to_label(
                mnemonic,
                &loop_target,
                scope,
                sema,
                span,
                ctx,
                diagnostics,
                ops,
            );
        }
        HlaStmt::RepeatInstruction { mnemonic, count } => {
            let Some(n) = eval_to_number(count, scope, sema, span, diagnostics) else {
                return;
            };
            if n < 0 {
                diagnostics.push(Diagnostic::error(
                    span,
                    format!("'* N' repeat count must be non-negative, got {n}"),
                ));
                return;
            }
            let inst = make_instruction(mnemonic, None);
            for _ in 0..n as usize {
                lower_instruction_stmt(&inst, scope, sema, span, ctx, diagnostics, ops);
            }
        }
        HlaStmt::NeverBlock { .. } | HlaStmt::PrefixConditional { .. } => {
            // Handled in lower_stmt directly (needs fs and current_segment parameters)
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn lower_hla_postfix_close_op_branch(
    op: HlaCompareOp,
    loop_target: &str,
    span: Span,
    scope: Option<&str>,
    sema: &SemanticModel,
    ctx: &mut LowerContext,
    diagnostics: &mut Vec<Diagnostic>,
    ops: &mut Vec<Spanned<Op>>,
) {
    match op {
        HlaCompareOp::Eq => {
            emit_branch_to_label("beq", loop_target, scope, sema, span, ctx, diagnostics, ops);
        }
        HlaCompareOp::Ne => {
            emit_branch_to_label("bne", loop_target, scope, sema, span, ctx, diagnostics, ops);
        }
        HlaCompareOp::Lt => {
            emit_branch_to_label("bcc", loop_target, scope, sema, span, ctx, diagnostics, ops);
        }
        HlaCompareOp::Ge => {
            emit_branch_to_label("bcs", loop_target, scope, sema, span, ctx, diagnostics, ops);
        }
        HlaCompareOp::Le => {
            emit_branch_to_label("bcc", loop_target, scope, sema, span, ctx, diagnostics, ops);
            emit_branch_to_label("beq", loop_target, scope, sema, span, ctx, diagnostics, ops);
        }
        HlaCompareOp::Gt => {
            let Some(skip_label) =
                fresh_local_label("postfix_gt_skip", ctx, scope, span, diagnostics)
            else {
                return;
            };
            emit_branch_to_label("beq", &skip_label, scope, sema, span, ctx, diagnostics, ops);
            emit_branch_to_label("bcc", &skip_label, scope, sema, span, ctx, diagnostics, ops);
            emit_branch_to_label("bra", loop_target, scope, sema, span, ctx, diagnostics, ops);
            ops.push(Spanned::new(Op::Label(skip_label), span));
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn lower_hla_condition_branch(
    condition: &HlaCondition,
    loop_target: &str,
    span: Span,
    scope: Option<&str>,
    sema: &SemanticModel,
    ctx: &mut LowerContext,
    diagnostics: &mut Vec<Diagnostic>,
    ops: &mut Vec<Spanned<Op>>,
) {
    if condition.lhs != HlaRegister::A {
        diagnostics.push(Diagnostic::error(
            span,
            "only accumulator 'a' is currently supported in HLA loop conditions",
        ));
        return;
    }

    let rhs = condition
        .rhs
        .as_ref()
        .unwrap_or(&Expr::Number(0, NumFmt::Dec));
    let Some(rhs_number) = eval_to_number(rhs, scope, sema, span, diagnostics) else {
        return;
    };

    let cmp_span = condition.seed_span.unwrap_or(span);
    let compare_instruction = make_instruction(
        "cmp",
        Some(Operand::Immediate {
            expr: Expr::Number(rhs_number, NumFmt::Dec),
            explicit_hash: false,
        }),
    );
    let ops_len_before_cmp = ops.len();
    lower_instruction_stmt(
        &compare_instruction,
        scope,
        sema,
        cmp_span,
        ctx,
        diagnostics,
        ops,
    );
    if ops.len() == ops_len_before_cmp {
        return;
    }

    match condition.op {
        HlaCompareOp::Eq => {
            emit_branch_to_label("beq", loop_target, scope, sema, span, ctx, diagnostics, ops);
        }
        HlaCompareOp::Ne => {
            emit_branch_to_label("bne", loop_target, scope, sema, span, ctx, diagnostics, ops);
        }
        HlaCompareOp::Ge => {
            let branch = if rhs_number == 0 { "bpl" } else { "bcs" };
            emit_branch_to_label(
                branch,
                loop_target,
                scope,
                sema,
                span,
                ctx,
                diagnostics,
                ops,
            );
        }
        HlaCompareOp::Lt => {
            let branch = if rhs_number == 0 { "bmi" } else { "bcc" };
            emit_branch_to_label(
                branch,
                loop_target,
                scope,
                sema,
                span,
                ctx,
                diagnostics,
                ops,
            );
        }
        HlaCompareOp::Le => {
            if rhs_number == 0 {
                emit_branch_to_label("bmi", loop_target, scope, sema, span, ctx, diagnostics, ops);
                emit_branch_to_label("beq", loop_target, scope, sema, span, ctx, diagnostics, ops);
            } else {
                emit_branch_to_label("bcc", loop_target, scope, sema, span, ctx, diagnostics, ops);
                emit_branch_to_label("beq", loop_target, scope, sema, span, ctx, diagnostics, ops);
            }
        }
        HlaCompareOp::Gt => {
            let Some(skip_label) = fresh_local_label("cond_skip", ctx, scope, span, diagnostics)
            else {
                return;
            };

            emit_branch_to_label("beq", &skip_label, scope, sema, span, ctx, diagnostics, ops);
            let branch = if rhs_number == 0 { "bpl" } else { "bcs" };
            emit_branch_to_label(
                branch,
                loop_target,
                scope,
                sema,
                span,
                ctx,
                diagnostics,
                ops,
            );
            ops.push(Spanned::new(Op::Label(skip_label), span));
        }
    }
}
