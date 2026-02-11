use k816_assets::AssetFS;

use crate::ast::{
    BlockKind, DataBlock, DataCommand, Expr, ExprBinaryOp, ExprUnaryOp, File, HlaCompareOp,
    HlaCondition, HlaRegister, HlaRhs, HlaStmt, Instruction, Item, NamedDataBlock, NamedDataEntry,
    Operand, OperandAddrMode, Stmt,
};
use crate::data_blocks::lower_data_block;
use crate::diag::Diagnostic;
use crate::hir::{
    AddressOperandMode, AddressValue, ByteRelocation, ByteRelocationKind, IndexRegister,
    InstructionOp, Op, OperandOp, Program,
};
use crate::sema::SemanticModel;
use crate::span::{Span, Spanned};

#[derive(Debug, Default)]
struct LowerContext {
    next_label: usize,
    do_loop_targets: Vec<String>,
}

struct EvaluatedBytes {
    bytes: Vec<u8>,
    relocations: Vec<ByteRelocation>,
}

impl LowerContext {
    fn next_label_id(&mut self) -> usize {
        let id = self.next_label;
        self.next_label += 1;
        id
    }
}

pub fn lower(
    file: &File,
    sema: &SemanticModel,
    fs: &dyn AssetFS,
) -> Result<Program, Vec<Diagnostic>> {
    let mut diagnostics = Vec::new();
    let mut ops = Vec::new();
    let mut top_level_ctx = LowerContext::default();
    let mut current_segment = "default".to_string();

    for item in &file.items {
        match &item.node {
            Item::Segment(segment) => {
                ops.push(Spanned::new(
                    Op::SelectSegment(segment.name.clone()),
                    item.span,
                ));
                current_segment = segment.name.clone();
            }
            Item::DataBlock(block) => match lower_data_block(block, fs) {
                Ok(mut lowered) => ops.append(&mut lowered),
                Err(mut errs) => diagnostics.append(&mut errs),
            },
            Item::NamedDataBlock(block) => {
                lower_named_data_block(
                    block,
                    sema,
                    fs,
                    &current_segment,
                    &mut diagnostics,
                    &mut ops,
                );
            }
            Item::CodeBlock(block) => {
                let mut block_ctx = LowerContext::default();
                let scope = block.name.clone();
                let label_span = block.name_span.unwrap_or(item.span);
                let mut block_segment = current_segment.clone();
                // Allow `segment ...` at the top of a code block to control where the
                // function/main entry label is emitted.
                let mut body_start = 0usize;
                while body_start < block.body.len() {
                    let stmt = &block.body[body_start];
                    match &stmt.node {
                        Stmt::Segment(segment) => {
                            ops.push(Spanned::new(
                                Op::SelectSegment(segment.name.clone()),
                                stmt.span,
                            ));
                            block_segment = segment.name.clone();
                            body_start += 1;
                        }
                        _ => break,
                    }
                }

                ops.push(Spanned::new(Op::FunctionStart(scope.clone()), label_span));
                ops.push(Spanned::new(Op::Label(scope.clone()), label_span));
                for stmt in block.body.iter().skip(body_start) {
                    lower_stmt(
                        &stmt.node,
                        stmt.span,
                        Some(scope.as_str()),
                        sema,
                        fs,
                        &mut block_segment,
                        &mut block_ctx,
                        &mut diagnostics,
                        &mut ops,
                    );
                }

                if !block_ctx.do_loop_targets.is_empty() {
                    diagnostics.push(
                        Diagnostic::error(item.span, "unterminated HLA do/while loop")
                            .with_help("close every '{' HLA loop with a '} a?...' condition"),
                    );
                }

                if !block.is_naked {
                    let mnemonic = if block.is_far { "rtl" } else { "rts" };
                    ops.push(Spanned::new(
                        Op::Instruction(InstructionOp {
                            mnemonic: mnemonic.to_string(),
                            operand: None,
                        }),
                        item.span,
                    ));
                }
                ops.push(Spanned::new(Op::FunctionEnd, item.span));
                if block_segment != current_segment {
                    ops.push(Spanned::new(
                        Op::SelectSegment(current_segment.clone()),
                        item.span,
                    ));
                }

                if block.kind == BlockKind::Main && block.is_far {
                    diagnostics.push(
                        Diagnostic::error(item.span, "main block cannot be declared far")
                            .with_help("remove 'far' from main or use 'func'"),
                    );
                }
            }
            Item::Statement(stmt) => {
                lower_stmt(
                    stmt,
                    item.span,
                    None,
                    sema,
                    fs,
                    &mut current_segment,
                    &mut top_level_ctx,
                    &mut diagnostics,
                    &mut ops,
                );
            }
            Item::Var(_) => {}
        }
    }

    if diagnostics.is_empty() {
        Ok(Program { ops })
    } else {
        Err(diagnostics)
    }
}

fn lower_named_data_block(
    block: &NamedDataBlock,
    sema: &SemanticModel,
    fs: &dyn AssetFS,
    outer_segment: &str,
    diagnostics: &mut Vec<Diagnostic>,
    ops: &mut Vec<Spanned<Op>>,
) {
    let Some(name) = resolve_symbol(&block.name, None, block.name_span, diagnostics) else {
        return;
    };
    let mut label_emitted = false;
    let mut block_segment = outer_segment.to_string();

    for entry in &block.entries {
        if !label_emitted
            && matches!(
                entry.node,
                NamedDataEntry::Segment(_)
                    | NamedDataEntry::Address(_)
                    | NamedDataEntry::Align(_)
                    | NamedDataEntry::Nocross(_)
            )
        {
            lower_named_data_entry(
                &entry.node,
                entry.span,
                sema,
                fs,
                &mut block_segment,
                diagnostics,
                ops,
            );
            continue;
        }

        if !label_emitted {
            ops.push(Spanned::new(Op::Label(name.clone()), block.name_span));
            label_emitted = true;
        }

        lower_named_data_entry(
            &entry.node,
            entry.span,
            sema,
            fs,
            &mut block_segment,
            diagnostics,
            ops,
        );
    }

    if !label_emitted {
        ops.push(Spanned::new(Op::Label(name), block.name_span));
    }
    if block_segment != outer_segment {
        ops.push(Spanned::new(
            Op::SelectSegment(outer_segment.to_string()),
            block.name_span,
        ));
    }
}

fn lower_named_data_entry(
    entry: &NamedDataEntry,
    span: Span,
    sema: &SemanticModel,
    fs: &dyn AssetFS,
    current_segment: &mut String,
    diagnostics: &mut Vec<Diagnostic>,
    ops: &mut Vec<Spanned<Op>>,
) {
    match entry {
        NamedDataEntry::Segment(segment) => {
            ops.push(Spanned::new(Op::SelectSegment(segment.name.clone()), span));
            *current_segment = segment.name.clone();
        }
        NamedDataEntry::Address(value) => {
            ops.push(Spanned::new(Op::Address(*value), span));
        }
        NamedDataEntry::Align(value) => {
            ops.push(Spanned::new(Op::Align(*value), span));
        }
        NamedDataEntry::Nocross(value) => {
            ops.push(Spanned::new(Op::Nocross(*value), span));
        }
        NamedDataEntry::Bytes(values) => {
            if let Some(evaluated) = evaluate_byte_exprs(values, None, sema, span, diagnostics) {
                let op = if evaluated.relocations.is_empty() {
                    Op::EmitBytes(evaluated.bytes)
                } else {
                    Op::EmitRelocBytes {
                        bytes: evaluated.bytes,
                        relocations: evaluated.relocations,
                    }
                };
                ops.push(Spanned::new(op, span));
            }
        }
        NamedDataEntry::String(value) => {
            ops.push(Spanned::new(Op::EmitBytes(value.as_bytes().to_vec()), span));
        }
        NamedDataEntry::Convert { kind, args } => {
            let data_block = DataBlock {
                commands: vec![Spanned::new(
                    DataCommand::Convert {
                        kind: kind.clone(),
                        args: args.clone(),
                    },
                    span,
                )],
            };
            match lower_data_block(&data_block, fs) {
                Ok(mut lowered) => ops.append(&mut lowered),
                Err(mut errs) => diagnostics.append(&mut errs),
            }
        }
        NamedDataEntry::Ignored => {}
    }
}

fn lower_stmt(
    stmt: &Stmt,
    span: Span,
    scope: Option<&str>,
    sema: &SemanticModel,
    fs: &dyn AssetFS,
    current_segment: &mut String,
    ctx: &mut LowerContext,
    diagnostics: &mut Vec<Diagnostic>,
    ops: &mut Vec<Spanned<Op>>,
) {
    match stmt {
        Stmt::Segment(segment) => {
            ops.push(Spanned::new(Op::SelectSegment(segment.name.clone()), span));
            *current_segment = segment.name.clone();
        }
        Stmt::Label(label) => {
            if let Some(resolved) = resolve_symbol(&label.name, scope, span, diagnostics) {
                ops.push(Spanned::new(Op::Label(resolved), span));
            }
        }
        Stmt::Instruction(instruction) => {
            if instruction.operand.is_none() {
                if let Some(meta) = sema.functions.get(&instruction.mnemonic) {
                    let Some(target) =
                        resolve_symbol(&instruction.mnemonic, scope, span, diagnostics)
                    else {
                        return;
                    };
                    let mnemonic = if meta.is_far { "jsl" } else { "jsr" };
                    ops.push(Spanned::new(
                        Op::Instruction(InstructionOp {
                            mnemonic: mnemonic.to_string(),
                            operand: Some(OperandOp::Address {
                                value: AddressValue::Label(target),
                                force_far: meta.is_far,
                                mode: AddressOperandMode::Direct { index: None },
                            }),
                        }),
                        span,
                    ));
                    return;
                }
            }
            lower_instruction_and_push(instruction, scope, sema, span, diagnostics, ops);
        }
        Stmt::Call(call) => {
            let Some(meta) = sema.functions.get(&call.target) else {
                diagnostics.push(
                    Diagnostic::error(span, format!("unknown function '{}'", call.target))
                        .with_help("declare the function before calling it"),
                );
                return;
            };

            let Some(target) = resolve_symbol(&call.target, scope, span, diagnostics) else {
                return;
            };

            let mnemonic = if meta.is_far { "jsl" } else { "jsr" };
            ops.push(Spanned::new(
                Op::Instruction(InstructionOp {
                    mnemonic: mnemonic.to_string(),
                    operand: Some(OperandOp::Address {
                        value: AddressValue::Label(target),
                        force_far: meta.is_far,
                        mode: AddressOperandMode::Direct { index: None },
                    }),
                }),
                span,
            ));
        }
        Stmt::Bytes(values) => {
            if let Some(evaluated) = evaluate_byte_exprs(values, scope, sema, span, diagnostics) {
                let op = if evaluated.relocations.is_empty() {
                    Op::EmitBytes(evaluated.bytes)
                } else {
                    Op::EmitRelocBytes {
                        bytes: evaluated.bytes,
                        relocations: evaluated.relocations,
                    }
                };
                ops.push(Spanned::new(op, span));
            }
        }
        Stmt::DataBlock(block) => match lower_data_block(block, fs) {
            Ok(mut lowered) => ops.append(&mut lowered),
            Err(mut errs) => diagnostics.append(&mut errs),
        },
        Stmt::Address(address) => {
            ops.push(Spanned::new(Op::Address(*address), span));
        }
        Stmt::Align(align) => {
            ops.push(Spanned::new(Op::Align(*align), span));
        }
        Stmt::Nocross(nocross) => {
            ops.push(Spanned::new(Op::Nocross(*nocross), span));
        }
        Stmt::Hla(HlaStmt::PrefixConditional {
            skip_mnemonic,
            body,
        }) => {
            let Some(skip_label) =
                fresh_local_label("prefix_skip", ctx, scope, span, diagnostics)
            else {
                return;
            };
            emit_branch_to_label(skip_mnemonic, &skip_label, scope, sema, span, diagnostics, ops);
            for s in body {
                lower_stmt(
                    &s.node,
                    s.span,
                    scope,
                    sema,
                    fs,
                    current_segment,
                    ctx,
                    diagnostics,
                    ops,
                );
            }
            ops.push(Spanned::new(Op::Label(skip_label), span));
        }
        Stmt::Hla(stmt) => {
            lower_hla_stmt(stmt, span, scope, sema, ctx, diagnostics, ops);
        }
        Stmt::Var(_) | Stmt::Empty => {}
    }
}

fn lower_hla_stmt(
    stmt: &HlaStmt,
    span: Span,
    scope: Option<&str>,
    sema: &SemanticModel,
    ctx: &mut LowerContext,
    diagnostics: &mut Vec<Diagnostic>,
    ops: &mut Vec<Spanned<Op>>,
) {
    match stmt {
        HlaStmt::XAssignImmediate { rhs } => {
            let instruction = Instruction {
                mnemonic: "ldx".to_string(),
                operand: Some(Operand::Immediate(rhs.clone())),
            };
            lower_instruction_and_push(&instruction, scope, sema, span, diagnostics, ops);
        }
        HlaStmt::XIncrement => {
            let instruction = Instruction {
                mnemonic: "inx".to_string(),
                operand: None,
            };
            lower_instruction_and_push(&instruction, scope, sema, span, diagnostics, ops);
        }
        HlaStmt::StoreFromA { dest, rhs } => {
            let lhs_instruction = Instruction {
                mnemonic: "lda".to_string(),
                operand: Some(match rhs {
                    HlaRhs::Immediate(expr) => Operand::Immediate(expr.clone()),
                    HlaRhs::Value {
                        expr,
                        index,
                        addr_mode,
                    } => Operand::Value {
                        expr: expr.clone(),
                        force_far: false,
                        index: *index,
                        addr_mode: *addr_mode,
                    },
                }),
            };
            if !lower_instruction_and_push(&lhs_instruction, scope, sema, span, diagnostics, ops) {
                return;
            }

            let rhs_instruction = Instruction {
                mnemonic: "sta".to_string(),
                operand: Some(Operand::Value {
                    expr: Expr::Ident(dest.clone()),
                    force_far: false,
                    index: None,
                    addr_mode: OperandAddrMode::Direct,
                }),
            };
            lower_instruction_and_push(&rhs_instruction, scope, sema, span, diagnostics, ops);
        }
        HlaStmt::WaitLoopWhileNFlagClear { symbol } => {
            let Some(wait_label) = fresh_local_label("wait", ctx, scope, span, diagnostics) else {
                return;
            };
            ops.push(Spanned::new(Op::Label(wait_label.clone()), span));

            let bit_instruction = Instruction {
                mnemonic: "bit".to_string(),
                operand: Some(Operand::Value {
                    expr: Expr::Ident(symbol.clone()),
                    force_far: false,
                    index: None,
                    addr_mode: OperandAddrMode::Direct,
                }),
            };
            if !lower_instruction_and_push(&bit_instruction, scope, sema, span, diagnostics, ops) {
                return;
            }

            emit_branch_to_label("bpl", &wait_label, scope, sema, span, diagnostics, ops);
        }
        HlaStmt::ConditionSeed { .. } => {
            if let HlaStmt::ConditionSeed { rhs, .. } = stmt {
                let instruction = Instruction {
                    mnemonic: "cmp".to_string(),
                    operand: Some(Operand::Immediate(rhs.clone())),
                };
                let _ =
                    lower_instruction_and_push(&instruction, scope, sema, span, diagnostics, ops);
            }
        }
        HlaStmt::DoOpen => {
            let Some(loop_label) = fresh_local_label("loop", ctx, scope, span, diagnostics) else {
                return;
            };
            ctx.do_loop_targets.push(loop_label.clone());
            ops.push(Spanned::new(Op::Label(loop_label), span));
        }
        HlaStmt::DoCloseNFlagClear => {
            let Some(loop_target) = ctx.do_loop_targets.pop() else {
                diagnostics.push(
                    Diagnostic::error(span, "HLA do/while close without matching '{'")
                        .with_help("open loop with a standalone '{' line before the condition"),
                );
                return;
            };
            emit_branch_to_label("bpl", &loop_target, scope, sema, span, diagnostics, ops);
        }
        HlaStmt::DoCloseNFlagSet => {
            let Some(loop_target) = ctx.do_loop_targets.pop() else {
                diagnostics.push(
                    Diagnostic::error(span, "HLA do/while close without matching '{'")
                        .with_help("open loop with a standalone '{' line before the condition"),
                );
                return;
            };
            emit_branch_to_label("bmi", &loop_target, scope, sema, span, diagnostics, ops);
        }
        HlaStmt::DoCloseWithOp { op } => {
            let Some(loop_target) = ctx.do_loop_targets.pop() else {
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
        }
        HlaStmt::DoClose { condition } => {
            let Some(loop_target) = ctx.do_loop_targets.pop() else {
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
        }
        HlaStmt::DoCloseAlways => {
            let Some(loop_target) = ctx.do_loop_targets.pop() else {
                diagnostics.push(
                    Diagnostic::error(span, "HLA do/while close without matching '{'")
                        .with_help("open loop with a standalone '{' line before the condition"),
                );
                return;
            };
            emit_branch_to_label("bra", &loop_target, scope, sema, span, diagnostics, ops);
        }
        HlaStmt::DoCloseNever => {
            let Some(_loop_target) = ctx.do_loop_targets.pop() else {
                diagnostics.push(
                    Diagnostic::error(span, "HLA do/while close without matching '{'")
                        .with_help("open loop with a standalone '{' line before the condition"),
                );
                return;
            };
        }
        HlaStmt::DoCloseBranch { mnemonic } => {
            let Some(loop_target) = ctx.do_loop_targets.pop() else {
                diagnostics.push(
                    Diagnostic::error(span, "HLA do/while close without matching '{'")
                        .with_help("open loop with a standalone '{' line before the condition"),
                );
                return;
            };
            emit_branch_to_label(mnemonic, &loop_target, scope, sema, span, diagnostics, ops);
        }
        HlaStmt::RepeatNop(count) => {
            let nop = Instruction {
                mnemonic: "nop".to_string(),
                operand: None,
            };
            for _ in 0..*count {
                lower_instruction_and_push(&nop, scope, sema, span, diagnostics, ops);
            }
        }
        HlaStmt::PrefixConditional { .. } => {
            // Handled in lower_stmt directly (needs fs and current_segment parameters)
        }
    }
}

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
            emit_branch_to_label("beq", loop_target, scope, sema, span, diagnostics, ops);
        }
        HlaCompareOp::Ne => {
            emit_branch_to_label("bne", loop_target, scope, sema, span, diagnostics, ops);
        }
        HlaCompareOp::Lt => {
            emit_branch_to_label("bcc", loop_target, scope, sema, span, diagnostics, ops);
        }
        HlaCompareOp::Ge => {
            emit_branch_to_label("bcs", loop_target, scope, sema, span, diagnostics, ops);
        }
        HlaCompareOp::Le => {
            emit_branch_to_label("bcc", loop_target, scope, sema, span, diagnostics, ops);
            emit_branch_to_label("beq", loop_target, scope, sema, span, diagnostics, ops);
        }
        HlaCompareOp::Gt => {
            let Some(skip_label) =
                fresh_local_label("postfix_gt_skip", ctx, scope, span, diagnostics)
            else {
                return;
            };
            emit_branch_to_label("beq", &skip_label, scope, sema, span, diagnostics, ops);
            emit_branch_to_label("bcc", &skip_label, scope, sema, span, diagnostics, ops);
            emit_branch_to_label("bra", loop_target, scope, sema, span, diagnostics, ops);
            ops.push(Spanned::new(Op::Label(skip_label), span));
        }
    }
}

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

    let rhs = condition.rhs.as_ref().unwrap_or(&Expr::Number(0));
    let Some(rhs_number) = eval_to_number(rhs, scope, sema, span, diagnostics) else {
        return;
    };

    let compare_instruction = Instruction {
        mnemonic: "cmp".to_string(),
        operand: Some(Operand::Immediate(Expr::Number(rhs_number))),
    };
    if !lower_instruction_and_push(&compare_instruction, scope, sema, span, diagnostics, ops) {
        return;
    }

    match condition.op {
        HlaCompareOp::Eq => {
            emit_branch_to_label("beq", loop_target, scope, sema, span, diagnostics, ops);
        }
        HlaCompareOp::Ne => {
            emit_branch_to_label("bne", loop_target, scope, sema, span, diagnostics, ops);
        }
        HlaCompareOp::Ge => {
            let branch = if rhs_number == 0 { "bpl" } else { "bcs" };
            emit_branch_to_label(branch, loop_target, scope, sema, span, diagnostics, ops);
        }
        HlaCompareOp::Lt => {
            let branch = if rhs_number == 0 { "bmi" } else { "bcc" };
            emit_branch_to_label(branch, loop_target, scope, sema, span, diagnostics, ops);
        }
        HlaCompareOp::Le => {
            if rhs_number == 0 {
                emit_branch_to_label("bmi", loop_target, scope, sema, span, diagnostics, ops);
                emit_branch_to_label("beq", loop_target, scope, sema, span, diagnostics, ops);
            } else {
                emit_branch_to_label("bcc", loop_target, scope, sema, span, diagnostics, ops);
                emit_branch_to_label("beq", loop_target, scope, sema, span, diagnostics, ops);
            }
        }
        HlaCompareOp::Gt => {
            let Some(skip_label) = fresh_local_label("cond_skip", ctx, scope, span, diagnostics)
            else {
                return;
            };

            emit_branch_to_label("beq", &skip_label, scope, sema, span, diagnostics, ops);
            let branch = if rhs_number == 0 { "bpl" } else { "bcs" };
            emit_branch_to_label(branch, loop_target, scope, sema, span, diagnostics, ops);
            ops.push(Spanned::new(Op::Label(skip_label), span));
        }
    }
}

fn emit_branch_to_label(
    mnemonic: &str,
    target: &str,
    scope: Option<&str>,
    sema: &SemanticModel,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
    ops: &mut Vec<Spanned<Op>>,
) {
    let instruction = Instruction {
        mnemonic: mnemonic.to_string(),
        operand: Some(Operand::Value {
            expr: Expr::Ident(target.to_string()),
            force_far: false,
            index: None,
            addr_mode: OperandAddrMode::Direct,
        }),
    };
    let _ = lower_instruction_and_push(&instruction, scope, sema, span, diagnostics, ops);
}

fn fresh_local_label(
    family: &str,
    ctx: &mut LowerContext,
    scope: Option<&str>,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<String> {
    let raw = format!(".__k816_{family}_{}", ctx.next_label_id());
    resolve_symbol(&raw, scope, span, diagnostics)
}

fn evaluate_byte_exprs(
    values: &[Expr],
    scope: Option<&str>,
    sema: &SemanticModel,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<EvaluatedBytes> {
    let mut bytes = Vec::with_capacity(values.len());
    let mut relocations = Vec::new();

    for (index, value) in values.iter().enumerate() {
        if let Some(number) = eval_to_number_strict(value, sema, span, diagnostics) {
            match u8::try_from(number) {
                Ok(byte) => bytes.push(byte),
                Err(_) => {
                    diagnostics.push(Diagnostic::error(
                        span,
                        format!("byte literal out of range: {number}"),
                    ));
                    return None;
                }
            }
            continue;
        }

        if matches!(
            value,
            Expr::Unary {
                op: ExprUnaryOp::LowByte | ExprUnaryOp::HighByte,
                ..
            }
        ) {
            if let Some((kind, label)) =
                resolve_symbolic_byte_relocation(value, scope, sema, span, diagnostics)
            {
                bytes.push(0);
                relocations.push(ByteRelocation {
                    offset: u32::try_from(index).expect("byte expression index should fit in u32"),
                    kind,
                    label,
                });
                continue;
            }

            diagnostics.push(
                Diagnostic::error(
                    span,
                    "low/high-byte expression must be a constant or plain symbol reference",
                )
                .with_help("supported symbolic forms are '&<label' and '&>label'"),
            );
            return None;
        }

        let Some(number) = eval_to_number(value, scope, sema, span, diagnostics) else {
            return None;
        };
        match u8::try_from(number) {
            Ok(byte) => bytes.push(byte),
            Err(_) => {
                diagnostics.push(Diagnostic::error(
                    span,
                    format!("byte literal out of range: {number}"),
                ));
                return None;
            }
        }
    }

    Some(EvaluatedBytes { bytes, relocations })
}

fn eval_to_number_strict(
    expr: &Expr,
    sema: &SemanticModel,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<i64> {
    match expr {
        Expr::Number(value) => Some(*value),
        Expr::Ident(name) => sema.vars.get(name).map(|var| i64::from(var.address)),
        Expr::EvalText(_) => {
            diagnostics.push(Diagnostic::error(
                span,
                "internal error: eval text should be expanded before lowering",
            ));
            None
        }
        Expr::Binary { op, lhs, rhs } => {
            let lhs = eval_to_number_strict(lhs, sema, span, diagnostics)?;
            let rhs = eval_to_number_strict(rhs, sema, span, diagnostics)?;
            match op {
                ExprBinaryOp::Add => lhs.checked_add(rhs),
                ExprBinaryOp::Sub => lhs.checked_sub(rhs),
            }
            .or_else(|| {
                diagnostics.push(Diagnostic::error(span, "arithmetic overflow"));
                None
            })
        }
        Expr::Unary { op, expr } => {
            let value = eval_to_number_strict(expr, sema, span, diagnostics)?;
            match op {
                ExprUnaryOp::LowByte => Some(value & 0xFF),
                ExprUnaryOp::HighByte => Some((value >> 8) & 0xFF),
            }
        }
    }
}

fn resolve_symbolic_byte_relocation(
    expr: &Expr,
    scope: Option<&str>,
    sema: &SemanticModel,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<(ByteRelocationKind, String)> {
    let Expr::Unary { op, expr } = expr else {
        return None;
    };

    let kind = match op {
        ExprUnaryOp::LowByte => ByteRelocationKind::LowByte,
        ExprUnaryOp::HighByte => ByteRelocationKind::HighByte,
    };

    let Expr::Ident(symbol) = expr.as_ref() else {
        return None;
    };

    if sema.vars.contains_key(symbol) || looks_like_constant_ident(symbol) {
        return None;
    }

    let resolved = resolve_symbol(symbol, scope, span, diagnostics)?;
    Some((kind, resolved))
}

fn looks_like_constant_ident(value: &str) -> bool {
    let mut has_alpha = false;
    for ch in value.chars() {
        if ch.is_ascii_alphabetic() {
            has_alpha = true;
            if !ch.is_ascii_uppercase() {
                return false;
            }
        } else if !(ch.is_ascii_digit() || ch == '_') {
            return false;
        }
    }
    has_alpha
}

fn lower_instruction_and_push(
    instruction: &Instruction,
    scope: Option<&str>,
    sema: &SemanticModel,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
    ops: &mut Vec<Spanned<Op>>,
) -> bool {
    if let Some(op) = lower_instruction(instruction, scope, sema, span, diagnostics) {
        ops.push(Spanned::new(Op::Instruction(op), span));
        true
    } else {
        false
    }
}

fn lower_instruction(
    instruction: &Instruction,
    scope: Option<&str>,
    sema: &SemanticModel,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<InstructionOp> {
    let operand = match &instruction.operand {
        None => None,
        Some(Operand::Immediate(expr)) => {
            let value = eval_to_number(expr, scope, sema, span, diagnostics)?;
            Some(OperandOp::Immediate(value))
        }
        Some(Operand::Value {
            expr,
            force_far,
            index,
            addr_mode,
        }) => {
            let mode = lower_operand_mode(*addr_mode, *index);
            match expr {
                Expr::Number(value) => {
                    let address = u32::try_from(*value).map_err(|_| {
                        Diagnostic::error(span, format!("address cannot be negative: {value}"))
                    });
                    match address {
                        Ok(address) => Some(OperandOp::Address {
                            value: AddressValue::Literal(address),
                            force_far: *force_far,
                            mode,
                        }),
                        Err(diag) => {
                            diagnostics.push(diag);
                            return None;
                        }
                    }
                }
                Expr::Ident(name) => Some(resolve_operand_ident(
                    name,
                    scope,
                    sema,
                    span,
                    diagnostics,
                    *force_far,
                    mode,
                )?),
                Expr::Binary { .. } | Expr::Unary { .. } => {
                    let Some(value) = eval_to_number(expr, scope, sema, span, diagnostics) else {
                        return None;
                    };
                    let Ok(address) = u32::try_from(value) else {
                        diagnostics.push(Diagnostic::error(
                            span,
                            format!("address cannot be negative: {value}"),
                        ));
                        return None;
                    };
                    Some(OperandOp::Address {
                        value: AddressValue::Literal(address),
                        force_far: *force_far,
                        mode,
                    })
                }
                Expr::EvalText(_) => {
                    diagnostics.push(Diagnostic::error(
                        span,
                        "internal error: eval text should be expanded before lowering",
                    ));
                    return None;
                }
            }
        }
    };

    Some(InstructionOp {
        mnemonic: instruction.mnemonic.clone(),
        operand,
    })
}

fn eval_to_number(
    expr: &Expr,
    scope: Option<&str>,
    sema: &SemanticModel,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<i64> {
    match expr {
        Expr::Number(value) => Some(*value),
        Expr::Ident(name) => {
            if let Some(var) = sema.vars.get(name) {
                return Some(i64::from(var.address));
            }

            // Constants that are not materialized in semantic tables yet;
            // keep lowering permissive by resolving unknown constants to 0.
            let _ = resolve_symbol(name, scope, span, diagnostics)?;
            Some(0)
        }
        Expr::EvalText(_) => {
            diagnostics.push(Diagnostic::error(
                span,
                "internal error: eval text should be expanded before lowering",
            ));
            None
        }
        Expr::Binary { op, lhs, rhs } => {
            let lhs = eval_to_number(lhs, scope, sema, span, diagnostics)?;
            let rhs = eval_to_number(rhs, scope, sema, span, diagnostics)?;
            match op {
                ExprBinaryOp::Add => lhs.checked_add(rhs),
                ExprBinaryOp::Sub => lhs.checked_sub(rhs),
            }
            .or_else(|| {
                diagnostics.push(Diagnostic::error(span, "arithmetic overflow"));
                None
            })
        }
        Expr::Unary { op, expr } => {
            let value = eval_to_number(expr, scope, sema, span, diagnostics)?;
            match op {
                ExprUnaryOp::LowByte => Some(value & 0xFF),
                ExprUnaryOp::HighByte => Some((value >> 8) & 0xFF),
            }
        }
    }
}

fn lower_operand_mode(
    addr_mode: OperandAddrMode,
    index: Option<crate::ast::IndexRegister>,
) -> AddressOperandMode {
    match addr_mode {
        OperandAddrMode::Direct => AddressOperandMode::Direct {
            index: index.map(lower_index_register),
        },
        OperandAddrMode::Indirect => AddressOperandMode::Indirect,
        OperandAddrMode::IndexedIndirectX => AddressOperandMode::IndexedIndirectX,
        OperandAddrMode::IndirectIndexedY => AddressOperandMode::IndirectIndexedY,
    }
}

fn lower_index_register(index: crate::ast::IndexRegister) -> IndexRegister {
    match index {
        crate::ast::IndexRegister::X => IndexRegister::X,
        crate::ast::IndexRegister::Y => IndexRegister::Y,
    }
}

fn resolve_operand_ident(
    name: &str,
    scope: Option<&str>,
    sema: &SemanticModel,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
    force_far: bool,
    mode: AddressOperandMode,
) -> Option<OperandOp> {
    if name.starts_with('.') {
        let resolved = resolve_symbol(name, scope, span, diagnostics)?;
        return Some(OperandOp::Address {
            value: AddressValue::Label(resolved),
            force_far,
            mode,
        });
    }

    if let Some(var) = sema.vars.get(name) {
        return Some(OperandOp::Address {
            value: AddressValue::Literal(var.address),
            force_far,
            mode,
        });
    }

    if sema.functions.contains_key(name) {
        return Some(OperandOp::Address {
            value: AddressValue::Label(name.to_string()),
            force_far,
            mode,
        });
    }

    Some(OperandOp::Address {
        value: AddressValue::Label(name.to_string()),
        force_far,
        mode,
    })
}

fn resolve_symbol(
    symbol: &str,
    scope: Option<&str>,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<String> {
    if let Some(local) = symbol.strip_prefix('.') {
        let Some(scope) = scope else {
            diagnostics.push(
                Diagnostic::error(span, "local labels require function/main scope")
                    .with_label(span, format!("label '.{local}' cannot be resolved here")),
            );
            return None;
        };
        return Some(format!("{scope}::.{}", local));
    }
    Some(symbol.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hir::{AddressOperandMode, AddressValue, OperandOp};
    use crate::parser::parse;
    use crate::sema::analyze;
    use crate::span::SourceId;
    use k816_assets::StdAssetFS;

    #[test]
    fn resolves_var_operand_to_literal_address() {
        let source = "var target = 0x1234\nmain {\n  lda target\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs).expect("lower");

        let operand = program
            .ops
            .iter()
            .find_map(|op| match &op.node {
                Op::Instruction(instruction) if instruction.mnemonic == "lda" => {
                    instruction.operand.as_ref()
                }
                _ => None,
            })
            .expect("lda operand");

        match operand {
            OperandOp::Address {
                value,
                force_far,
                mode,
            } => {
                assert!(!force_far);
                assert_eq!(*mode, AddressOperandMode::Direct { index: None });
                assert!(matches!(value, AddressValue::Literal(0x1234)));
            }
            _ => panic!("expected address operand"),
        }
    }

    #[test]
    fn keeps_unresolved_identifier_as_label_operand() {
        let source = "main {\n  lda missing\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs).expect("lower");
        let operand = program
            .ops
            .iter()
            .find_map(|op| match &op.node {
                Op::Instruction(instruction) if instruction.mnemonic == "lda" => {
                    instruction.operand.as_ref()
                }
                _ => None,
            })
            .expect("lda operand");
        assert!(matches!(
            operand,
            OperandOp::Address {
                value: AddressValue::Label(name),
                ..
            } if name == "missing"
        ));
    }

    #[test]
    fn named_data_label_is_emitted_after_leading_segment_directive() {
        let source = "data info {\n  segment INFO\n  \"A\"\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs).expect("lower");

        let segment_index = program
            .ops
            .iter()
            .position(|op| matches!(&op.node, Op::SelectSegment(name) if name == "INFO"))
            .expect("segment select for INFO");
        let label_index = program
            .ops
            .iter()
            .position(|op| matches!(&op.node, Op::Label(name) if name == "info"))
            .expect("label for info");
        let emit_index = program
            .ops
            .iter()
            .position(|op| matches!(&op.node, Op::EmitBytes(bytes) if bytes == &[b'A']))
            .expect("emit for data payload");

        assert!(segment_index < label_index);
        assert!(label_index < emit_index);
    }

    #[test]
    fn named_data_label_is_emitted_after_leading_align_directive() {
        let source = "data bytes {\n  align 16\n  1\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs).expect("lower");

        let align_index = program
            .ops
            .iter()
            .position(|op| matches!(&op.node, Op::Align(16)))
            .expect("align directive");
        let label_index = program
            .ops
            .iter()
            .position(|op| matches!(&op.node, Op::Label(name) if name == "bytes"))
            .expect("label for bytes");
        let emit_index = program
            .ops
            .iter()
            .position(|op| matches!(&op.node, Op::EmitBytes(bytes) if bytes == &[1]))
            .expect("data byte emission");

        assert!(align_index < label_index);
        assert!(label_index < emit_index);
    }

    #[test]
    fn lowers_wait_loop_bit_pattern_to_bit_and_bpl() {
        let source = "var ready = 0x1234\nmain {\n  { a&?ready } n-?\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs).expect("lower");

        let mnemonics = program
            .ops
            .iter()
            .filter_map(|op| match &op.node {
                Op::Instruction(instruction) => Some(instruction.mnemonic.as_str()),
                _ => None,
            })
            .collect::<Vec<_>>();

        assert!(mnemonics.windows(2).any(|pair| pair == ["bit", "bpl"]));
        assert!(!mnemonics.contains(&"lda"));

        let bit_operand = program
            .ops
            .iter()
            .find_map(|op| match &op.node {
                Op::Instruction(instruction) if instruction.mnemonic == "bit" => {
                    instruction.operand.as_ref()
                }
                _ => None,
            })
            .expect("bit operand");

        assert!(matches!(
            bit_operand,
            OperandOp::Address {
                value: AddressValue::Literal(0x1234),
                force_far: false,
                mode: AddressOperandMode::Direct { index: None }
            }
        ));
    }

    #[test]
    fn lowers_postfix_n_flag_close_without_cmp() {
        let source = "var ready = 0x1234\nmain {\n  {\n    a=ready\n  } n-?\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs).expect("lower");

        let mnemonics = program
            .ops
            .iter()
            .filter_map(|op| match &op.node {
                Op::Instruction(instruction) => Some(instruction.mnemonic.as_str()),
                _ => None,
            })
            .collect::<Vec<_>>();

        assert!(mnemonics.windows(2).any(|pair| pair == ["lda", "bpl"]));
        assert!(!mnemonics.contains(&"cmp"));
    }

    #[test]
    fn lowers_postfix_le_to_two_direct_branches() {
        let source = "main {\n  {\n    x++\n  } <=\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs).expect("lower");

        let mnemonics = program
            .ops
            .iter()
            .filter_map(|op| match &op.node {
                Op::Instruction(instruction) => Some(instruction.mnemonic.as_str()),
                _ => None,
            })
            .collect::<Vec<_>>();

        assert!(mnemonics.windows(2).any(|pair| pair == ["bcc", "beq"]));
        assert!(!mnemonics.contains(&"cmp"));
    }

    #[test]
    fn lowers_postfix_gt_to_three_branch_sequence() {
        let source = "main {\n  {\n    x++\n  } >\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs).expect("lower");

        let mnemonics = program
            .ops
            .iter()
            .filter_map(|op| match &op.node {
                Op::Instruction(instruction) => Some(instruction.mnemonic.as_str()),
                _ => None,
            })
            .collect::<Vec<_>>();

        assert!(mnemonics.windows(3).any(|seq| seq == ["beq", "bcc", "bra"]));
        assert!(!mnemonics.contains(&"cmp"));
    }

    #[test]
    fn segment_inside_named_data_block_restores_outer_segment() {
        let source =
            "segment fixed_lo\ndata vectors {\n  segment fixed_hi\n  1\n}\ndata tail {\n  2\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs).expect("lower");

        let fixed_hi_index = program
            .ops
            .iter()
            .position(|op| matches!(&op.node, Op::SelectSegment(name) if name == "fixed_hi"))
            .expect("fixed_hi segment select");
        let fixed_lo_indices = program
            .ops
            .iter()
            .enumerate()
            .filter_map(|(idx, op)| {
                matches!(&op.node, Op::SelectSegment(name) if name == "fixed_lo").then_some(idx)
            })
            .collect::<Vec<_>>();
        assert!(
            fixed_lo_indices.len() >= 2,
            "expected top-level + restore selects"
        );
        assert!(fixed_lo_indices[1] > fixed_hi_index);

        let tail_label_index = program
            .ops
            .iter()
            .position(|op| matches!(&op.node, Op::Label(name) if name == "tail"))
            .expect("tail label");
        assert!(fixed_lo_indices[1] < tail_label_index);
    }

    #[test]
    fn segment_inside_code_block_restores_outer_segment() {
        let source = "segment fixed_lo\nmain {\n  segment fixed_hi\n  nop\n}\ndata tail {\n  2\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs).expect("lower");

        let fixed_hi_index = program
            .ops
            .iter()
            .position(|op| matches!(&op.node, Op::SelectSegment(name) if name == "fixed_hi"))
            .expect("fixed_hi segment select");
        let function_end_index = program
            .ops
            .iter()
            .position(|op| matches!(&op.node, Op::FunctionEnd))
            .expect("function end");
        let restore_index = program
            .ops
            .iter()
            .enumerate()
            .filter_map(|(idx, op)| {
                matches!(&op.node, Op::SelectSegment(name) if name == "fixed_lo").then_some(idx)
            })
            .find(|idx| *idx > function_end_index)
            .expect("restore segment fixed_lo after function");
        let tail_label_index = program
            .ops
            .iter()
            .position(|op| matches!(&op.node, Op::Label(name) if name == "tail"))
            .expect("tail label");

        assert!(fixed_hi_index < function_end_index);
        assert!(function_end_index < restore_index);
        assert!(restore_index < tail_label_index);
    }

    #[test]
    fn segment_inside_func_block_restores_outer_segment() {
        let source = "segment fixed_lo\nfunc worker {\n  segment fixed_hi\n  nop\n}\ndata tail {\n  2\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs).expect("lower");

        let fixed_hi_index = program
            .ops
            .iter()
            .position(|op| matches!(&op.node, Op::SelectSegment(name) if name == "fixed_hi"))
            .expect("fixed_hi segment select");
        let function_end_index = program
            .ops
            .iter()
            .position(|op| matches!(&op.node, Op::FunctionEnd))
            .expect("function end");
        let restore_index = program
            .ops
            .iter()
            .enumerate()
            .filter_map(|(idx, op)| {
                matches!(&op.node, Op::SelectSegment(name) if name == "fixed_lo").then_some(idx)
            })
            .find(|idx| *idx > function_end_index)
            .expect("restore segment fixed_lo after function");
        let tail_label_index = program
            .ops
            .iter()
            .position(|op| matches!(&op.node, Op::Label(name) if name == "tail"))
            .expect("tail label");

        assert!(fixed_hi_index < function_end_index);
        assert!(function_end_index < restore_index);
        assert!(restore_index < tail_label_index);
    }

    #[test]
    fn segment_inside_naked_block_restores_outer_segment() {
        let source = "segment fixed_lo\nnaked worker {\n  segment fixed_hi\n  nop\n}\ndata tail {\n  2\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs).expect("lower");

        let fixed_hi_index = program
            .ops
            .iter()
            .position(|op| matches!(&op.node, Op::SelectSegment(name) if name == "fixed_hi"))
            .expect("fixed_hi segment select");
        let function_end_index = program
            .ops
            .iter()
            .position(|op| matches!(&op.node, Op::FunctionEnd))
            .expect("function end");
        let restore_index = program
            .ops
            .iter()
            .enumerate()
            .filter_map(|(idx, op)| {
                matches!(&op.node, Op::SelectSegment(name) if name == "fixed_lo").then_some(idx)
            })
            .find(|idx| *idx > function_end_index)
            .expect("restore segment fixed_lo after naked block");
        let tail_label_index = program
            .ops
            .iter()
            .position(|op| matches!(&op.node, Op::Label(name) if name == "tail"))
            .expect("tail label");

        assert!(fixed_hi_index < function_end_index);
        assert!(function_end_index < restore_index);
        assert!(restore_index < tail_label_index);
    }
}
