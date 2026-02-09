use indexmap::IndexMap;

use crate::ast::{CodeBlock, Expr, File, Item, Stmt, VarDecl};
use crate::diag::Diagnostic;
use crate::span::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FunctionMeta {
    pub is_far: bool,
    pub is_naked: bool,
    pub is_inline: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VarMeta {
    pub address: u32,
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
                .with_hint("rename one of the blocks to keep symbols unique"),
        );
        return;
    }

    model.functions.insert(
        block.name.clone(),
        FunctionMeta {
            is_far: block.is_far,
            is_naked: block.is_naked,
            is_inline: block.is_inline,
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
                .with_hint("rename one of the vars/functions to keep symbols unique"),
        );
        return;
    }

    let Some(address) = eval_var_address(var, *next_auto_addr, span, diagnostics) else {
        return;
    };

    *next_auto_addr = address.saturating_add(1);
    model.vars.insert(var.name.clone(), VarMeta { address });
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

    match initializer {
        Expr::Number(value) => match u32::try_from(*value) {
            Ok(address) => Some(address),
            Err(_) => {
                diagnostics.push(Diagnostic::error(
                    span,
                    format!("var address cannot be negative: {value}"),
                ));
                None
            }
        },
        Expr::Ident(name) => {
            diagnostics.push(Diagnostic::error(
                span,
                format!("var initializer '{name}' must be a numeric literal"),
            ));
            None
        }
        Expr::EvalText(_) => {
            diagnostics.push(Diagnostic::error(
                span,
                "internal error: eval text should be expanded before semantic analysis",
            ));
            None
        }
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
        assert_eq!(sema.vars.get("in_a").expect("in_a").address, 1);
        assert_eq!(sema.vars.get("in_b").expect("in_b").address, 2);
        assert_eq!(sema.vars.get("tail").expect("tail").address, 3);
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
