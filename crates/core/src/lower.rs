use k816_assets::AssetFS;

use crate::ast::{
    BlockKind, DataBlock, DataCommand, Expr, ExprBinaryOp, ExprUnaryOp, File, HlaCompareOp,
    HlaCondition, HlaRegister, HlaRhs, HlaStmt, Instruction, Item, NamedDataBlock, NamedDataEntry,
    Operand, Stmt,
};
use crate::data_blocks::lower_data_block;
use crate::diag::Diagnostic;
use crate::hir::{AddressValue, InstructionOp, Op, OperandOp, Program};
use crate::sema::SemanticModel;
use crate::span::{Span, Spanned};

#[derive(Debug, Default)]
struct LowerContext {
    next_label: usize,
    do_loop_targets: Vec<String>,
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

    for item in &file.items {
        match &item.node {
            Item::Segment(segment) => {
                ops.push(Spanned::new(
                    Op::SelectSegment(segment.name.clone()),
                    item.span,
                ));
            }
            Item::DataBlock(block) => match lower_data_block(block, fs) {
                Ok(mut lowered) => ops.append(&mut lowered),
                Err(mut errs) => diagnostics.append(&mut errs),
            },
            Item::NamedDataBlock(block) => {
                lower_named_data_block(block, sema, fs, &mut diagnostics, &mut ops);
            }
            Item::CodeBlock(block) => {
                let mut block_ctx = LowerContext::default();
                let scope = block.name.clone();
                let label_span = block.name_span.unwrap_or(item.span);
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
    diagnostics: &mut Vec<Diagnostic>,
    ops: &mut Vec<Spanned<Op>>,
) {
    if let Some(name) = resolve_symbol(&block.name, None, block.name_span, diagnostics) {
        ops.push(Spanned::new(Op::Label(name), block.name_span));
    } else {
        return;
    }

    for entry in &block.entries {
        match &entry.node {
            NamedDataEntry::Segment(segment) => {
                ops.push(Spanned::new(
                    Op::SelectSegment(segment.name.clone()),
                    entry.span,
                ));
            }
            NamedDataEntry::Address(value) => {
                ops.push(Spanned::new(Op::Address(*value), entry.span));
            }
            NamedDataEntry::Align(value) => {
                ops.push(Spanned::new(Op::Align(*value), entry.span));
            }
            NamedDataEntry::Nocross(value) => {
                ops.push(Spanned::new(Op::Nocross(*value), entry.span));
            }
            NamedDataEntry::Bytes(values) | NamedDataEntry::LegacyBytes(values) => {
                if let Some(bytes) =
                    evaluate_byte_exprs(values, None, sema, entry.span, diagnostics)
                {
                    ops.push(Spanned::new(Op::EmitBytes(bytes), entry.span));
                }
            }
            NamedDataEntry::String(value) => {
                ops.push(Spanned::new(
                    Op::EmitBytes(value.as_bytes().to_vec()),
                    entry.span,
                ));
            }
            NamedDataEntry::Convert { kind, args } => {
                let data_block = DataBlock {
                    commands: vec![Spanned::new(
                        DataCommand::Convert {
                            kind: kind.clone(),
                            args: args.clone(),
                        },
                        entry.span,
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
}

fn lower_stmt(
    stmt: &Stmt,
    span: Span,
    scope: Option<&str>,
    sema: &SemanticModel,
    fs: &dyn AssetFS,
    ctx: &mut LowerContext,
    diagnostics: &mut Vec<Diagnostic>,
    ops: &mut Vec<Spanned<Op>>,
) {
    match stmt {
        Stmt::Segment(segment) => {
            ops.push(Spanned::new(Op::SelectSegment(segment.name.clone()), span));
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
                                index_x: false,
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
                        index_x: false,
                    }),
                }),
                span,
            ));
        }
        Stmt::Bytes(values) => {
            if let Some(bytes) = evaluate_byte_exprs(values, scope, sema, span, diagnostics) {
                ops.push(Spanned::new(Op::EmitBytes(bytes), span));
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
                    HlaRhs::Value { expr, index } => Operand::Value {
                        expr: expr.clone(),
                        force_far: false,
                        index: *index,
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
                }),
            };
            lower_instruction_and_push(&rhs_instruction, scope, sema, span, diagnostics, ops);
        }
        HlaStmt::WaitLoopWhileNFlagClear { symbol } => {
            let Some(wait_label) = fresh_local_label("wait", ctx, scope, span, diagnostics) else {
                return;
            };
            ops.push(Spanned::new(Op::Label(wait_label.clone()), span));

            let load_instruction = Instruction {
                mnemonic: "lda".to_string(),
                operand: Some(Operand::Value {
                    expr: Expr::Ident(symbol.clone()),
                    force_far: false,
                    index: None,
                }),
            };
            if !lower_instruction_and_push(&load_instruction, scope, sema, span, diagnostics, ops) {
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
        HlaStmt::DoCloseWithOp { op } => {
            let Some(loop_target) = ctx.do_loop_targets.pop() else {
                diagnostics.push(
                    Diagnostic::error(span, "HLA do/while close without matching '{'")
                        .with_help("open loop with a standalone '{' line before the condition"),
                );
                return;
            };
            lower_hla_condition_branch(
                &HlaCondition {
                    lhs: HlaRegister::A,
                    op: *op,
                    rhs: None,
                },
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
) -> Option<Vec<u8>> {
    let mut bytes = Vec::with_capacity(values.len());
    for value in values {
        match eval_to_number(value, scope, sema, span, diagnostics) {
            Some(number) => match u8::try_from(number) {
                Ok(byte) => bytes.push(byte),
                Err(_) => diagnostics.push(Diagnostic::error(
                    span,
                    format!("byte literal out of range: {number}"),
                )),
            },
            None => return None,
        }
    }
    Some(bytes)
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
        }) => {
            let index_x = index.is_some();
            match expr {
                Expr::Number(value) => {
                    let address = u32::try_from(*value).map_err(|_| {
                        Diagnostic::error(span, format!("address cannot be negative: {value}"))
                    });
                    match address {
                        Ok(address) => Some(OperandOp::Address {
                            value: AddressValue::Literal(address),
                            force_far: *force_far,
                            index_x,
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
                    index_x,
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
                        index_x,
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

            // Legacy syntax often uses constants that are not materialized in semantic
            // tables yet; keep lowering permissive by resolving unknown constants to 0.
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

fn resolve_operand_ident(
    name: &str,
    scope: Option<&str>,
    sema: &SemanticModel,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
    force_far: bool,
    index_x: bool,
) -> Option<OperandOp> {
    if name.starts_with('.') {
        let resolved = resolve_symbol(name, scope, span, diagnostics)?;
        return Some(OperandOp::Address {
            value: AddressValue::Label(resolved),
            force_far,
            index_x,
        });
    }

    if let Some(var) = sema.vars.get(name) {
        return Some(OperandOp::Address {
            value: AddressValue::Literal(var.address),
            force_far,
            index_x,
        });
    }

    if sema.functions.contains_key(name) {
        return Some(OperandOp::Address {
            value: AddressValue::Label(name.to_string()),
            force_far,
            index_x,
        });
    }

    Some(OperandOp::Address {
        value: AddressValue::Label(name.to_string()),
        force_far,
        index_x,
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
    use crate::hir::OperandOp;
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
                index_x,
            } => {
                assert!(!force_far);
                assert!(!index_x);
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
}
