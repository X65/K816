use k816_assets::AssetFS;

use crate::ast::{BlockKind, Expr, File, Instruction, Item, Operand, Stmt};
use crate::data_blocks::lower_data_block;
use crate::diag::Diagnostic;
use crate::hir::{AddressValue, InstructionOp, Op, OperandOp, Program};
use crate::sema::SemanticModel;
use crate::span::{Span, Spanned};

pub fn lower(
    file: &File,
    sema: &SemanticModel,
    fs: &dyn AssetFS,
) -> Result<Program, Vec<Diagnostic>> {
    let mut diagnostics = Vec::new();
    let mut ops = Vec::new();

    for item in &file.items {
        match &item.node {
            Item::Bank(bank) => {
                ops.push(Spanned::new(Op::SelectBank(bank.name.clone()), item.span));
            }
            Item::DataBlock(block) => match lower_data_block(block, fs) {
                Ok(mut lowered) => ops.append(&mut lowered),
                Err(mut errs) => diagnostics.append(&mut errs),
            },
            Item::CodeBlock(block) => {
                let scope = block.name.clone();
                ops.push(Spanned::new(Op::Label(scope.clone()), item.span));
                for stmt in &block.body {
                    lower_stmt(
                        &stmt.node,
                        stmt.span,
                        Some(scope.as_str()),
                        sema,
                        fs,
                        &mut diagnostics,
                        &mut ops,
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

                if block.kind == BlockKind::Main && block.is_far {
                    diagnostics.push(
                        Diagnostic::error(item.span, "main block cannot be declared far")
                            .with_help("remove 'far' from main or use 'func'"),
                    );
                }
            }
            Item::Statement(stmt) => {
                lower_stmt(stmt, item.span, None, sema, fs, &mut diagnostics, &mut ops);
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

fn lower_stmt(
    stmt: &Stmt,
    span: Span,
    scope: Option<&str>,
    sema: &SemanticModel,
    fs: &dyn AssetFS,
    diagnostics: &mut Vec<Diagnostic>,
    ops: &mut Vec<Spanned<Op>>,
) {
    match stmt {
        Stmt::Label(label) => {
            if let Some(resolved) = resolve_symbol(&label.name, scope, span, diagnostics) {
                ops.push(Spanned::new(Op::Label(resolved), span));
            }
        }
        Stmt::Instruction(instruction) => {
            if let Some(op) = lower_instruction(instruction, scope, sema, span, diagnostics) {
                ops.push(Spanned::new(Op::Instruction(op), span));
            }
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
                    }),
                }),
                span,
            ));
        }
        Stmt::Bytes(values) => {
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
                    None => return,
                }
            }
            ops.push(Spanned::new(Op::EmitBytes(bytes), span));
        }
        Stmt::DataBlock(block) => match lower_data_block(block, fs) {
            Ok(mut lowered) => ops.append(&mut lowered),
            Err(mut errs) => diagnostics.append(&mut errs),
        },
        Stmt::Var(_) | Stmt::Empty => {}
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
        Some(Operand::Value { expr, force_far }) => match expr {
            Expr::Number(value) => {
                let address = u32::try_from(*value).map_err(|_| {
                    Diagnostic::error(span, format!("address cannot be negative: {value}"))
                });
                match address {
                    Ok(address) => Some(OperandOp::Address {
                        value: AddressValue::Literal(address),
                        force_far: *force_far,
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
            )?),
            Expr::EvalText(_) => {
                diagnostics.push(Diagnostic::error(
                    span,
                    "internal error: eval text should be expanded before lowering",
                ));
                return None;
            }
        },
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
            if sema.vars.contains_key(name) {
                diagnostics.push(Diagnostic::error(
                    span,
                    format!(
                        "address symbol '{name}' cannot be used in .byte; expected numeric literal"
                    ),
                ));
                return None;
            }

            let resolved = resolve_symbol(name, scope, span, diagnostics)?;
            diagnostics.push(Diagnostic::error(
                span,
                format!("identifier '{resolved}' is not a numeric literal in this context"),
            ));
            None
        }
        Expr::EvalText(_) => {
            diagnostics.push(Diagnostic::error(
                span,
                "internal error: eval text should be expanded before lowering",
            ));
            None
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
) -> Option<OperandOp> {
    if name.starts_with('.') {
        let resolved = resolve_symbol(name, scope, span, diagnostics)?;
        return Some(OperandOp::Address {
            value: AddressValue::Label(resolved),
            force_far,
        });
    }

    if let Some(var) = sema.vars.get(name) {
        return Some(OperandOp::Address {
            value: AddressValue::Literal(var.address),
            force_far,
        });
    }

    if sema.functions.contains_key(name) {
        return Some(OperandOp::Address {
            value: AddressValue::Label(name.to_string()),
            force_far,
        });
    }

    diagnostics.push(
        Diagnostic::error(span, format!("unknown symbol '{name}'"))
            .with_help("declare a var/function or use a local label inside a code block"),
    );
    None
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
            OperandOp::Address { value, force_far } => {
                assert!(!force_far);
                assert!(matches!(value, AddressValue::Literal(0x1234)));
            }
            _ => panic!("expected address operand"),
        }
    }

    #[test]
    fn reports_unknown_identifier_in_instruction_operand() {
        let source = "main {\n  lda missing\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let errors = lower(&file, &sema, &fs).expect_err("must fail");

        assert!(
            errors
                .iter()
                .any(|error| error.message.contains("unknown symbol 'missing'"))
        );
    }
}
