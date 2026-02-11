use indexmap::IndexMap;

use crate::ast::{
    CodeBlock, DataWidth, Expr, ExprBinaryOp, ExprUnaryOp, File, Item, ModeContract, Stmt,
    VarDecl,
};
use crate::diag::Diagnostic;
use crate::span::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FunctionMeta {
    pub is_far: bool,
    pub is_naked: bool,
    pub is_inline: bool,
    pub mode_contract: ModeContract,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VarMeta {
    pub address: u32,
    pub size: u32,
    pub data_width: Option<DataWidth>,
}

#[derive(Debug, Clone, Default)]
pub struct SemanticModel {
    pub functions: IndexMap<String, FunctionMeta>,
    pub vars: IndexMap<String, VarMeta>,
}

pub fn analyze(file: &File) -> Result<SemanticModel, Vec<Diagnostic>> {
    let mut model = SemanticModel::default();
    let mut diagnostics = Vec::new();
    let mut next_auto_addr = 0_u32;

    for item in &file.items {
        match &item.node {
            Item::Var(var) => collect_var(
                var,
                item.span,
                &mut next_auto_addr,
                &mut model,
                &mut diagnostics,
            ),
            Item::CodeBlock(block) => {
                collect_function(block, item.span, &mut model, &mut diagnostics);
                for stmt in &block.body {
                    if let Stmt::Var(var) = &stmt.node {
                        collect_var(
                            var,
                            stmt.span,
                            &mut next_auto_addr,
                            &mut model,
                            &mut diagnostics,
                        );
                    }
                }
            }
            Item::Statement(Stmt::Var(var)) => collect_var(
                var,
                item.span,
                &mut next_auto_addr,
                &mut model,
                &mut diagnostics,
            ),
            Item::NamedDataBlock(_) => {}
            _ => {}
        }
    }

    if diagnostics.is_empty() {
        Ok(model)
    } else {
        Err(diagnostics)
    }
}

fn collect_function(
    block: &CodeBlock,
    span: Span,
    model: &mut SemanticModel,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if !is_symbol_available(&block.name, model) {
        diagnostics.push(
            Diagnostic::error(span, format!("duplicate symbol '{}'", block.name))
                .with_help("rename one of the blocks to keep symbols unique"),
        );
        return;
    }

    model.functions.insert(
        block.name.clone(),
        FunctionMeta {
            is_far: block.is_far,
            is_naked: block.is_naked,
            is_inline: block.is_inline,
            mode_contract: block.mode_contract,
        },
    );
}

fn collect_var(
    var: &VarDecl,
    span: Span,
    next_auto_addr: &mut u32,
    model: &mut SemanticModel,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if !is_symbol_available(&var.name, model) {
        diagnostics.push(
            Diagnostic::error(span, format!("duplicate symbol '{}'", var.name))
                .with_help("rename one of the vars/functions to keep symbols unique"),
        );
        return;
    }

    let Some(address) = eval_var_address(var, *next_auto_addr, span, diagnostics) else {
        return;
    };
    let Some(size) = eval_var_size(var, span, diagnostics) else {
        return;
    };
    let Some(next_addr) = address.checked_add(size) else {
        diagnostics.push(Diagnostic::error(
            span,
            format!(
                "var allocation for '{}' overflows address space (start={address:#X}, size={size})",
                var.name
            ),
        ));
        return;
    };

    *next_auto_addr = next_addr;
    model.vars.insert(
        var.name.clone(),
        VarMeta {
            address,
            size,
            data_width: var.data_width,
        },
    );
}

fn eval_var_address(
    var: &VarDecl,
    next_auto_addr: u32,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<u32> {
    let Some(initializer) = &var.initializer else {
        return Some(next_auto_addr);
    };

    match eval_const_expr(initializer) {
        Ok(value) => match u32::try_from(value) {
            Ok(address) => Some(address),
            Err(_) => {
                diagnostics.push(Diagnostic::error(
                    span,
                    format!("var address cannot be negative: {value}"),
                ));
                None
            }
        },
        Err(ConstExprError::Ident(name)) => {
            diagnostics.push(Diagnostic::error(
                span,
                format!("var initializer '{name}' must be a numeric literal"),
            ));
            None
        }
        Err(ConstExprError::EvalText) => {
            diagnostics.push(Diagnostic::error(
                span,
                "internal error: eval text should be expanded before semantic analysis",
            ));
            None
        }
        Err(ConstExprError::Overflow) => {
            diagnostics.push(Diagnostic::error(
                span,
                "var initializer overflows numeric literal range",
            ));
            None
        }
    }
}

fn eval_var_size(var: &VarDecl, span: Span, diagnostics: &mut Vec<Diagnostic>) -> Option<u32> {
    let element_size: u32 = match var.data_width {
        Some(DataWidth::Word) => 2,
        Some(DataWidth::Byte) | None => 1,
    };

    let Some(array_len) = &var.array_len else {
        return Some(element_size);
    };

    match eval_const_expr(array_len) {
        Ok(value) => {
            if value <= 0 {
                diagnostics.push(Diagnostic::error(
                    span,
                    format!("var array length must be positive: {value}"),
                ));
                return None;
            }
            match u32::try_from(value) {
                Ok(count) => Some(count * element_size),
                Err(_) => {
                    diagnostics.push(Diagnostic::error(
                        span,
                        format!("var array length is out of range: {value}"),
                    ));
                    None
                }
            }
        }
        Err(ConstExprError::Ident(name)) => {
            diagnostics.push(Diagnostic::error(
                span,
                format!("var array length '{name}' must be a numeric literal"),
            ));
            None
        }
        Err(ConstExprError::EvalText) => {
            diagnostics.push(Diagnostic::error(
                span,
                "internal error: eval text should be expanded before semantic analysis",
            ));
            None
        }
        Err(ConstExprError::Overflow) => {
            diagnostics.push(Diagnostic::error(
                span,
                "var array length overflows numeric literal range",
            ));
            None
        }
    }
}

enum ConstExprError {
    Ident(String),
    EvalText,
    Overflow,
}

fn eval_const_expr(expr: &Expr) -> Result<i64, ConstExprError> {
    match expr {
        Expr::Number(value) => Ok(*value),
        Expr::Ident(name) => Err(ConstExprError::Ident(name.clone())),
        Expr::EvalText(_) => Err(ConstExprError::EvalText),
        Expr::Binary { op, lhs, rhs } => {
            let lhs = eval_const_expr(lhs)?;
            let rhs = eval_const_expr(rhs)?;
            match op {
                ExprBinaryOp::Add => lhs.checked_add(rhs).ok_or(ConstExprError::Overflow),
                ExprBinaryOp::Sub => lhs.checked_sub(rhs).ok_or(ConstExprError::Overflow),
            }
        }
        Expr::Unary { op, expr } => {
            let value = eval_const_expr(expr)?;
            match op {
                ExprUnaryOp::LowByte => Ok(value & 0xFF),
                ExprUnaryOp::HighByte => Ok((value >> 8) & 0xFF),
            }
        }
        Expr::TypedView { expr, .. } => eval_const_expr(expr),
    }
}

fn is_symbol_available(symbol: &str, model: &SemanticModel) -> bool {
    !model.functions.contains_key(symbol) && !model.vars.contains_key(symbol)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse;
    use crate::span::SourceId;

    #[test]
    fn allocates_vars_monotonically_in_source_order() {
        let source = "var top\nfunc f {\n  var in_a\n  var in_b\n}\nvar tail\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");

        assert_eq!(sema.vars.get("top").expect("top").address, 0);
        assert_eq!(sema.vars.get("top").expect("top").size, 1);
        assert_eq!(sema.vars.get("in_a").expect("in_a").address, 1);
        assert_eq!(sema.vars.get("in_a").expect("in_a").size, 1);
        assert_eq!(sema.vars.get("in_b").expect("in_b").address, 2);
        assert_eq!(sema.vars.get("in_b").expect("in_b").size, 1);
        assert_eq!(sema.vars.get("tail").expect("tail").address, 3);
        assert_eq!(sema.vars.get("tail").expect("tail").size, 1);
    }

    #[test]
    fn explicit_var_address_resets_auto_allocator() {
        let source = "var first\nvar reset = 0x100\nvar next\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");

        assert_eq!(sema.vars.get("first").expect("first").address, 0);
        assert_eq!(sema.vars.get("reset").expect("reset").address, 0x100);
        assert_eq!(sema.vars.get("next").expect("next").address, 0x101);
    }

    #[test]
    fn array_length_advances_allocator() {
        let source = "var header[4]\nvar next\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");

        assert_eq!(sema.vars.get("header").expect("header").address, 0);
        assert_eq!(sema.vars.get("header").expect("header").size, 4);
        assert_eq!(sema.vars.get("next").expect("next").address, 4);
        assert_eq!(sema.vars.get("next").expect("next").size, 1);
    }

    #[test]
    fn explicit_array_address_sets_next_auto_address_after_full_size() {
        let source = "var first\nvar tiles[3] = 0x100\nvar tail\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");

        assert_eq!(sema.vars.get("first").expect("first").address, 0);
        assert_eq!(sema.vars.get("tiles").expect("tiles").address, 0x100);
        assert_eq!(sema.vars.get("tiles").expect("tiles").size, 3);
        assert_eq!(sema.vars.get("tail").expect("tail").address, 0x103);
    }

    #[test]
    fn array_length_must_be_positive_literal() {
        let source = "var bad[0]\n";
        let file = parse(SourceId(0), source).expect("parse");
        let errors = analyze(&file).expect_err("must fail");

        assert!(
            errors
                .iter()
                .any(|error| error.message.contains("array length must be positive"))
        );
    }

    #[test]
    fn duplicate_symbols_between_var_and_function_are_rejected() {
        let source = "var dup\nfunc dup {\n  nop\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let errors = analyze(&file).expect_err("must fail");

        assert!(
            errors
                .iter()
                .any(|error| error.message.contains("duplicate symbol 'dup'"))
        );
    }
}
