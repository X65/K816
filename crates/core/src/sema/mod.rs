use indexmap::IndexMap;

use crate::ast::{
    CodeBlock, ConstDecl, DataWidth, Expr, ExprBinaryOp, ExprUnaryOp, File, Item, ModeContract,
    Stmt, SymbolicSubscriptFieldDecl, VarDecl,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SymbolicSubscriptFieldMeta {
    pub offset: u32,
    pub size: u32,
    pub data_width: DataWidth,
    pub count: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SymbolicSubscriptMeta {
    pub fields: IndexMap<String, SymbolicSubscriptFieldMeta>,
    pub total_size: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VarMeta {
    pub address: u32,
    pub size: u32,
    pub data_width: Option<DataWidth>,
    pub symbolic_subscript: Option<SymbolicSubscriptMeta>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ConstMeta {
    pub value: i64,
}

#[derive(Debug, Clone, Default)]
pub struct SemanticModel {
    pub functions: IndexMap<String, FunctionMeta>,
    pub vars: IndexMap<String, VarMeta>,
    pub consts: IndexMap<String, ConstMeta>,
}

pub fn analyze(file: &File) -> Result<SemanticModel, Vec<Diagnostic>> {
    let mut model = SemanticModel::default();
    let mut diagnostics = Vec::new();
    let mut next_auto_addr = 0_u32;

    for item in &file.items {
        match &item.node {
            Item::Const(const_decl) => {
                collect_const(const_decl, item.span, &mut model, &mut diagnostics)
            }
            Item::Var(var) => collect_var(
                var,
                item.span,
                &mut next_auto_addr,
                &mut model,
                &mut diagnostics,
            ),
            Item::CodeBlock(block) => {
                collect_function(
                    block,
                    item.span,
                    &file.mode_default,
                    &mut model,
                    &mut diagnostics,
                );
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
    module_default: &ModeContract,
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

    // Merge: function-level explicit annotations take priority, module default fills gaps.
    let merged_contract = ModeContract {
        a_width: block.mode_contract.a_width.or(module_default.a_width),
        i_width: block.mode_contract.i_width.or(module_default.i_width),
    };

    model.functions.insert(
        block.name.clone(),
        FunctionMeta {
            is_far: block.is_far,
            is_naked: block.is_naked,
            is_inline: block.is_inline,
            mode_contract: merged_contract,
        },
    );
}

fn collect_const(
    const_decl: &ConstDecl,
    span: Span,
    model: &mut SemanticModel,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if !is_symbol_available(&const_decl.name, model) {
        diagnostics.push(
            Diagnostic::error(span, format!("duplicate symbol '{}'", const_decl.name))
                .with_help("rename one of the consts/vars/functions to keep symbols unique"),
        );
        return;
    }

    let initializer_span = const_decl.initializer_span.unwrap_or(span);
    match eval_const_expr(&const_decl.initializer, &model.consts) {
        Ok(value) => {
            model
                .consts
                .insert(const_decl.name.clone(), ConstMeta { value });
        }
        Err(ConstExprError::Ident(name)) => {
            diagnostics.push(Diagnostic::error(
                initializer_span,
                format!("const initializer '{name}' must be a constant numeric expression"),
            ));
        }
        Err(ConstExprError::EvalText) => {
            diagnostics.push(Diagnostic::error(
                initializer_span,
                "internal error: eval text should be expanded before semantic analysis",
            ));
        }
        Err(ConstExprError::Overflow) => {
            diagnostics.push(Diagnostic::error(
                initializer_span,
                "const initializer overflows numeric literal range",
            ));
        }
    }
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

    let Some(address) = eval_var_address(var, *next_auto_addr, span, &model.consts, diagnostics)
    else {
        return;
    };
    let Some(layout) = eval_var_layout(var, span, &model.consts, diagnostics) else {
        return;
    };
    let Some(next_addr) = address.checked_add(layout.size) else {
        diagnostics.push(Diagnostic::error(
            span,
            format!(
                "var allocation for '{}' overflows address space (start={address:#X}, size={})",
                var.name, layout.size
            ),
        ));
        return;
    };

    *next_auto_addr = next_addr;
    model.vars.insert(
        var.name.clone(),
        VarMeta {
            address,
            size: layout.size,
            data_width: var.data_width,
            symbolic_subscript: layout.symbolic_subscript,
        },
    );
}

fn eval_var_address(
    var: &VarDecl,
    next_auto_addr: u32,
    span: Span,
    consts: &IndexMap<String, ConstMeta>,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<u32> {
    let Some(initializer) = &var.initializer else {
        if var.symbolic_subscript_fields.is_some() {
            diagnostics.push(
                Diagnostic::error(
                    span,
                    format!(
                        "symbolic subscript array '{}' is missing a base address assignment",
                        var.name
                    ),
                )
                .with_help("append '= <constant numeric expression>' after the field list (for example: '] = $6000')"),
            );
            return None;
        }
        return Some(next_auto_addr);
    };
    let initializer_span = var.initializer_span.unwrap_or(span);

    match eval_const_expr(initializer, consts) {
        Ok(value) => match u32::try_from(value) {
            Ok(address) => Some(address),
            Err(_) => {
                diagnostics.push(Diagnostic::error(
                    initializer_span,
                    format!("var address cannot be negative: {value}"),
                ));
                None
            }
        },
        Err(ConstExprError::Ident(name)) => {
            diagnostics.push(Diagnostic::error(
                initializer_span,
                format!("var initializer '{name}' must be a constant numeric expression"),
            ));
            None
        }
        Err(ConstExprError::EvalText) => {
            diagnostics.push(Diagnostic::error(
                initializer_span,
                "internal error: eval text should be expanded before semantic analysis",
            ));
            None
        }
        Err(ConstExprError::Overflow) => {
            diagnostics.push(Diagnostic::error(
                initializer_span,
                "var initializer overflows numeric literal range",
            ));
            None
        }
    }
}

struct VarLayout {
    size: u32,
    symbolic_subscript: Option<SymbolicSubscriptMeta>,
}

fn eval_var_layout(
    var: &VarDecl,
    span: Span,
    consts: &IndexMap<String, ConstMeta>,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<VarLayout> {
    if let Some(symbolic_subscript_fields) = &var.symbolic_subscript_fields {
        if var.array_len.is_some() {
            diagnostics.push(Diagnostic::error(
                span,
                format!(
                    "var '{}' cannot use both array length and symbolic subscript field list",
                    var.name
                ),
            ));
            return None;
        }

        let symbolic_subscript = eval_symbolic_subscript_layout(
            var,
            symbolic_subscript_fields,
            span,
            consts,
            diagnostics,
        )?;
        return Some(VarLayout {
            size: symbolic_subscript.total_size,
            symbolic_subscript: Some(symbolic_subscript),
        });
    }

    let element_size: u32 = match var.data_width {
        Some(DataWidth::Word) => 2,
        Some(DataWidth::Byte) | None => 1,
    };

    let Some(array_len) = &var.array_len else {
        return Some(VarLayout {
            size: element_size,
            symbolic_subscript: None,
        });
    };

    match eval_const_expr(array_len, consts) {
        Ok(value) => {
            if value <= 0 {
                diagnostics.push(Diagnostic::error(
                    span,
                    format!("var array length must be positive: {value}"),
                ));
                return None;
            }
            match u32::try_from(value) {
                Ok(count) => Some(VarLayout {
                    size: count * element_size,
                    symbolic_subscript: None,
                }),
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
                format!("var array length '{name}' must be a constant numeric expression"),
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

fn eval_symbolic_subscript_layout(
    var: &VarDecl,
    fields: &[SymbolicSubscriptFieldDecl],
    span: Span,
    consts: &IndexMap<String, ConstMeta>,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<SymbolicSubscriptMeta> {
    let mut offset = 0_u32;
    let mut resolved_fields = IndexMap::new();

    for field in fields {
        if resolved_fields.contains_key(&field.name) {
            diagnostics.push(Diagnostic::error(
                field.span,
                format!(
                    "duplicate symbolic subscript field '.{}' in '{}'",
                    field.name, var.name
                ),
            ));
            return None;
        }

        let data_width = field
            .data_width
            .or(var.data_width)
            .unwrap_or(DataWidth::Byte);

        let count = match &field.count {
            Some(count_expr) => match eval_const_expr(count_expr, consts) {
                Ok(value) => {
                    if value <= 0 {
                        let count_span = field.count_span.unwrap_or(field.span);
                        diagnostics.push(Diagnostic::error(
                            count_span,
                            format!(
                                "symbolic subscript field '.{}' count must be >= 1, found {value}",
                                field.name
                            ),
                        ));
                        return None;
                    }
                    match u32::try_from(value) {
                        Ok(count) => count,
                        Err(_) => {
                            let count_span = field.count_span.unwrap_or(field.span);
                            diagnostics.push(Diagnostic::error(
                                count_span,
                                format!(
                                    "symbolic subscript field '.{}' count is out of range: {value}",
                                    field.name
                                ),
                            ));
                            return None;
                        }
                    }
                }
                Err(ConstExprError::Ident(name)) => {
                    let count_span = field.count_span.unwrap_or(field.span);
                    diagnostics.push(Diagnostic::error(
                        count_span,
                        format!(
                            "symbolic subscript field '.{}' count expression '{name}' must be a constant numeric expression",
                            field.name
                        ),
                    ));
                    return None;
                }
                Err(ConstExprError::EvalText) => {
                    diagnostics.push(Diagnostic::error(
                        span,
                        "internal error: eval text should be expanded before semantic analysis",
                    ));
                    return None;
                }
                Err(ConstExprError::Overflow) => {
                    let count_span = field.count_span.unwrap_or(field.span);
                    diagnostics.push(Diagnostic::error(
                        count_span,
                        format!(
                            "symbolic subscript field '.{}' count expression overflows numeric literal range",
                            field.name
                        ),
                    ));
                    return None;
                }
            },
            None => 1,
        };

        let element_size = match data_width {
            DataWidth::Byte => 1_u32,
            DataWidth::Word => 2_u32,
        };
        let Some(size) = count.checked_mul(element_size) else {
            diagnostics.push(Diagnostic::error(
                field.span,
                format!(
                    "symbolic subscript field '.{}' in '{}' overflows layout size",
                    field.name, var.name
                ),
            ));
            return None;
        };

        resolved_fields.insert(
            field.name.clone(),
            SymbolicSubscriptFieldMeta {
                offset,
                size,
                data_width,
                count,
            },
        );

        let Some(next_offset) = offset.checked_add(size) else {
            diagnostics.push(Diagnostic::error(
                span,
                format!(
                    "symbolic subscript array '{}' total size overflows address space",
                    var.name
                ),
            ));
            return None;
        };
        offset = next_offset;
    }

    Some(SymbolicSubscriptMeta {
        fields: resolved_fields,
        total_size: offset,
    })
}

enum ConstExprError {
    Ident(String),
    EvalText,
    Overflow,
}

fn eval_const_expr(
    expr: &Expr,
    consts: &IndexMap<String, ConstMeta>,
) -> Result<i64, ConstExprError> {
    match expr {
        Expr::Number(value) => Ok(*value),
        Expr::Ident(name) => consts
            .get(name)
            .map(|constant| constant.value)
            .ok_or_else(|| ConstExprError::Ident(name.clone())),
        Expr::IdentSpanned { name, .. } => consts
            .get(name)
            .map(|constant| constant.value)
            .ok_or_else(|| ConstExprError::Ident(name.clone())),
        Expr::EvalText(_) => Err(ConstExprError::EvalText),
        Expr::Index { base, index } => {
            let base = eval_const_expr(base, consts)?;
            let index = eval_const_expr(index, consts)?;
            base.checked_add(index).ok_or(ConstExprError::Overflow)
        }
        Expr::Binary { op, lhs, rhs } => {
            let lhs = eval_const_expr(lhs, consts)?;
            let rhs = eval_const_expr(rhs, consts)?;
            match op {
                ExprBinaryOp::Add => lhs.checked_add(rhs).ok_or(ConstExprError::Overflow),
                ExprBinaryOp::Sub => lhs.checked_sub(rhs).ok_or(ConstExprError::Overflow),
            }
        }
        Expr::Unary { op, expr } => {
            let value = eval_const_expr(expr, consts)?;
            match op {
                ExprUnaryOp::LowByte => Ok(value & 0xFF),
                ExprUnaryOp::HighByte => Ok((value >> 8) & 0xFF),
            }
        }
        Expr::TypedView { expr, .. } => eval_const_expr(expr, consts),
    }
}

fn is_symbol_available(symbol: &str, model: &SemanticModel) -> bool {
    !model.functions.contains_key(symbol)
        && !model.vars.contains_key(symbol)
        && !model.consts.contains_key(symbol)
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
    fn consts_are_collected_and_used_in_var_initializers() {
        let source = "const BASE = 0x100\nconst NEXT = BASE + 3\nvar ptr = NEXT\nvar tail\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");

        assert_eq!(sema.consts.get("BASE").expect("BASE").value, 0x100);
        assert_eq!(sema.consts.get("NEXT").expect("NEXT").value, 0x103);
        assert_eq!(sema.vars.get("ptr").expect("ptr").address, 0x103);
        assert_eq!(sema.vars.get("tail").expect("tail").address, 0x104);
    }

    #[test]
    fn const_declarations_do_not_advance_var_allocator() {
        let source = "const C = 7\nvar first\nvar second\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");

        assert_eq!(sema.vars.get("first").expect("first").address, 0);
        assert_eq!(sema.vars.get("second").expect("second").address, 1);
    }

    #[test]
    fn duplicate_symbols_between_const_and_var_are_rejected() {
        let source = "const dup = 1\nvar dup\n";
        let file = parse(SourceId(0), source).expect("parse");
        let errors = analyze(&file).expect_err("must fail");

        assert!(
            errors
                .iter()
                .any(|error| error.message.contains("duplicate symbol 'dup'"))
        );
    }

    #[test]
    fn const_initializer_must_be_a_constant_expression() {
        let source = "const A = B\n";
        let file = parse(SourceId(0), source).expect("parse");
        let errors = analyze(&file).expect_err("must fail");

        assert!(errors.iter().any(|error| {
            error
                .message
                .contains("const initializer 'B' must be a constant numeric expression")
        }));
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

    #[test]
    fn computes_symbolic_subscript_field_offsets_and_total_size() {
        let source = "var foo[\n  .field_w:word\n  .field_w2:word\n  .idx:byte\n  .string[4]:byte\n] = 0x1234\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");

        let foo = sema.vars.get("foo").expect("foo");
        assert_eq!(foo.address, 0x1234);
        assert_eq!(foo.size, 9);
        let symbolic_subscript = foo.symbolic_subscript.as_ref().expect("symbolic subscript");
        let field_w = symbolic_subscript.fields.get("field_w").expect("field_w");
        let field_w2 = symbolic_subscript.fields.get("field_w2").expect("field_w2");
        let idx = symbolic_subscript.fields.get("idx").expect("idx");
        let string = symbolic_subscript.fields.get("string").expect("string");
        assert_eq!(field_w.offset, 0);
        assert_eq!(field_w2.offset, 2);
        assert_eq!(idx.offset, 4);
        assert_eq!(string.offset, 5);
    }

    #[test]
    fn symbolic_subscript_array_requires_explicit_base_address() {
        let source = "var VIA[\n  .orb:byte\n  .ora:byte\n]\n";
        let file = parse(SourceId(0), source).expect("parse");
        let errors = analyze(&file).expect_err("must fail");
        assert!(errors.iter().any(|error| {
            error
                .message
                .contains("is missing a base address assignment")
        }));
    }

    #[test]
    fn symbolic_subscript_rejects_duplicate_field_names() {
        let source = "var VIA[\n  .orb:byte\n  .orb:word\n] = 0x6000\n";
        let file = parse(SourceId(0), source).expect("parse");
        let errors = analyze(&file).expect_err("must fail");
        assert!(errors.iter().any(|error| {
            error
                .message
                .contains("duplicate symbolic subscript field '.orb'")
        }));
    }

    #[test]
    fn symbolic_subscript_base_expression_must_be_constant() {
        let source = "var VIA[\n  .orb:byte\n] = base_addr\n";
        let file = parse(SourceId(0), source).expect("parse");
        let errors = analyze(&file).expect_err("must fail");
        assert!(errors.iter().any(|error| {
            error
                .message
                .contains("must be a constant numeric expression")
        }));
    }

    #[test]
    fn symbolic_subscript_fields_use_default_var_width_when_type_is_omitted() {
        let source = "var baz:byte[\n  .a\n  .b\n  .len:word\n] = 0x2244\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");

        let baz = sema.vars.get("baz").expect("baz");
        assert_eq!(baz.size, 4);
        let symbolic_subscript = baz.symbolic_subscript.as_ref().expect("symbolic subscript");
        assert_eq!(symbolic_subscript.fields.get("a").expect("a").offset, 0);
        assert_eq!(symbolic_subscript.fields.get("b").expect("b").offset, 1);
        assert_eq!(symbolic_subscript.fields.get("len").expect("len").offset, 2);
    }

    #[test]
    fn symbolic_subscript_fields_default_to_byte_when_no_type_is_provided() {
        let source = "var foo[\n  .a\n  .b[2]\n  .w:word\n] = 0x1234\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");

        let foo = sema.vars.get("foo").expect("foo");
        assert_eq!(foo.size, 5);
        let symbolic_subscript = foo.symbolic_subscript.as_ref().expect("symbolic subscript");
        let a = symbolic_subscript.fields.get("a").expect("a");
        let b = symbolic_subscript.fields.get("b").expect("b");
        let w = symbolic_subscript.fields.get("w").expect("w");
        assert_eq!(a.offset, 0);
        assert_eq!(b.offset, 1);
        assert_eq!(w.offset, 3);
        assert_eq!(a.data_width, DataWidth::Byte);
        assert_eq!(b.data_width, DataWidth::Byte);
        assert_eq!(w.data_width, DataWidth::Word);
    }
}
