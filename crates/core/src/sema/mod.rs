use indexmap::IndexMap;

use crate::ast::{
    CodeBlock, DataWidth, Expr, ExprBinaryOp, ExprUnaryOp, File, Item, ModeContract,
    OverlayFieldDecl, Stmt, VarDecl,
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
pub struct OverlayFieldMeta {
    pub offset: u32,
    pub size: u32,
    pub data_width: DataWidth,
    pub count: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OverlayMeta {
    pub fields: IndexMap<String, OverlayFieldMeta>,
    pub total_size: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VarMeta {
    pub address: u32,
    pub size: u32,
    pub data_width: Option<DataWidth>,
    pub overlay: Option<OverlayMeta>,
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
    let Some(layout) = eval_var_layout(var, span, diagnostics) else {
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
            overlay: layout.overlay,
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
        if var.overlay_fields.is_some() {
            diagnostics.push(
                Diagnostic::error(
                    span,
                    format!("overlay var '{}' is missing a base address assignment", var.name),
                )
                .with_help("append '= <constant numeric expression>' after the field list (for example: '] = $6000')"),
            );
            return None;
        }
        return Some(next_auto_addr);
    };
    let initializer_span = var.initializer_span.unwrap_or(span);

    match eval_const_expr(initializer) {
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
    overlay: Option<OverlayMeta>,
}

fn eval_var_layout(
    var: &VarDecl,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<VarLayout> {
    if let Some(overlay_fields) = &var.overlay_fields {
        if var.array_len.is_some() {
            diagnostics.push(Diagnostic::error(
                span,
                format!(
                    "var '{}' cannot use both array length and overlay field list",
                    var.name
                ),
            ));
            return None;
        }

        let overlay = eval_overlay_layout(var, overlay_fields, span, diagnostics)?;
        return Some(VarLayout {
            size: overlay.total_size,
            overlay: Some(overlay),
        });
    }

    let element_size: u32 = match var.data_width {
        Some(DataWidth::Word) => 2,
        Some(DataWidth::Byte) | None => 1,
    };

    let Some(array_len) = &var.array_len else {
        return Some(VarLayout {
            size: element_size,
            overlay: None,
        });
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
                Ok(count) => Some(VarLayout {
                    size: count * element_size,
                    overlay: None,
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

fn eval_overlay_layout(
    var: &VarDecl,
    fields: &[OverlayFieldDecl],
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<OverlayMeta> {
    let mut offset = 0_u32;
    let mut resolved_fields = IndexMap::new();

    for field in fields {
        if resolved_fields.contains_key(&field.name) {
            diagnostics.push(Diagnostic::error(
                field.span,
                format!(
                    "duplicate overlay field '.{}' in '{}'",
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
            Some(count_expr) => match eval_const_expr(count_expr) {
                Ok(value) => {
                    if value <= 0 {
                        let count_span = field.count_span.unwrap_or(field.span);
                        diagnostics.push(Diagnostic::error(
                            count_span,
                            format!(
                                "overlay field '.{}' count must be >= 1, found {value}",
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
                                    "overlay field '.{}' count is out of range: {value}",
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
                            "overlay field '.{}' count expression '{name}' must be a constant numeric expression",
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
                            "overlay field '.{}' count expression overflows numeric literal range",
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
                    "overlay field '.{}' in '{}' overflows layout size",
                    field.name, var.name
                ),
            ));
            return None;
        };

        resolved_fields.insert(
            field.name.clone(),
            OverlayFieldMeta {
                offset,
                size,
                data_width,
                count,
            },
        );

        let Some(next_offset) = offset.checked_add(size) else {
            diagnostics.push(Diagnostic::error(
                span,
                format!("overlay '{}' total size overflows address space", var.name),
            ));
            return None;
        };
        offset = next_offset;
    }

    Some(OverlayMeta {
        fields: resolved_fields,
        total_size: offset,
    })
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
        Expr::Index { base, index } => {
            let base = eval_const_expr(base)?;
            let index = eval_const_expr(index)?;
            base.checked_add(index).ok_or(ConstExprError::Overflow)
        }
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

    #[test]
    fn computes_overlay_field_offsets_and_total_size() {
        let source = "var foo[\n  .field_w:word\n  .field_w2:word\n  .idx:byte\n  .string[4]:byte\n] = 0x1234\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");

        let foo = sema.vars.get("foo").expect("foo");
        assert_eq!(foo.address, 0x1234);
        assert_eq!(foo.size, 9);
        let overlay = foo.overlay.as_ref().expect("overlay");
        let field_w = overlay.fields.get("field_w").expect("field_w");
        let field_w2 = overlay.fields.get("field_w2").expect("field_w2");
        let idx = overlay.fields.get("idx").expect("idx");
        let string = overlay.fields.get("string").expect("string");
        assert_eq!(field_w.offset, 0);
        assert_eq!(field_w2.offset, 2);
        assert_eq!(idx.offset, 4);
        assert_eq!(string.offset, 5);
    }

    #[test]
    fn overlay_requires_explicit_base_address() {
        let source = "var VIA[\n  .orb:byte\n  .ora:byte\n]\n";
        let file = parse(SourceId(0), source).expect("parse");
        let errors = analyze(&file).expect_err("must fail");
        assert!(
            errors
                .iter()
                .any(|error| error.message.contains("is missing a base address assignment"))
        );
    }

    #[test]
    fn overlay_rejects_duplicate_field_names() {
        let source = "var VIA[\n  .orb:byte\n  .orb:word\n] = 0x6000\n";
        let file = parse(SourceId(0), source).expect("parse");
        let errors = analyze(&file).expect_err("must fail");
        assert!(
            errors
                .iter()
                .any(|error| error.message.contains("duplicate overlay field '.orb'"))
        );
    }

    #[test]
    fn overlay_base_expression_must_be_constant() {
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
    fn overlay_fields_use_default_var_width_when_type_is_omitted() {
        let source = "var baz:byte[\n  .a\n  .b\n  .len:word\n] = 0x2244\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");

        let baz = sema.vars.get("baz").expect("baz");
        assert_eq!(baz.size, 4);
        let overlay = baz.overlay.as_ref().expect("overlay");
        assert_eq!(overlay.fields.get("a").expect("a").offset, 0);
        assert_eq!(overlay.fields.get("b").expect("b").offset, 1);
        assert_eq!(overlay.fields.get("len").expect("len").offset, 2);
    }

    #[test]
    fn overlay_fields_default_to_byte_when_no_type_is_provided() {
        let source = "var foo[\n  .a\n  .b[2]\n  .w:word\n] = 0x1234\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");

        let foo = sema.vars.get("foo").expect("foo");
        assert_eq!(foo.size, 5);
        let overlay = foo.overlay.as_ref().expect("overlay");
        let a = overlay.fields.get("a").expect("a");
        let b = overlay.fields.get("b").expect("b");
        let w = overlay.fields.get("w").expect("w");
        assert_eq!(a.offset, 0);
        assert_eq!(b.offset, 1);
        assert_eq!(w.offset, 3);
        assert_eq!(a.data_width, DataWidth::Byte);
        assert_eq!(b.data_width, DataWidth::Byte);
        assert_eq!(w.data_width, DataWidth::Word);
    }
}
