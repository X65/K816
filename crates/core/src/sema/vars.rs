use super::*;

pub(super) fn collect_var(
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

    match eval_const_expr_to_int(initializer, consts) {
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
        Err(ConstExprError::NonInteger) => {
            diagnostics.push(
                Diagnostic::error(
                    initializer_span,
                    "var initializer must be an exact integer value",
                )
                .with_help("remove fractional parts before using this value as an address"),
            );
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
        Some(DataWidth::Far) => 3,
        Some(DataWidth::Word) => 2,
        Some(DataWidth::Byte) | None => 1,
    };

    let Some(array_len) = &var.array_len else {
        return Some(VarLayout {
            size: element_size,
            symbolic_subscript: None,
        });
    };

    match eval_const_expr_to_int(array_len, consts) {
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
        Err(ConstExprError::NonInteger) => {
            diagnostics.push(
                Diagnostic::error(span, "var array length must be an exact integer value")
                    .with_help(
                        "remove fractional parts or convert to an integer before using it as an array length",
                    ),
            );
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
    let mut resolved_fields = IndexMap::new();
    let total_size = eval_symbolic_subscript_fields(
        &var.name,
        var.data_width,
        fields,
        "",
        span,
        consts,
        &mut resolved_fields,
        diagnostics,
    )?;
    Some(SymbolicSubscriptMeta {
        fields: resolved_fields,
        total_size,
    })
}

/// Recursively evaluates symbolic subscript fields. Returns the total size of this
/// level, and inserts all (possibly dotted) field entries into `resolved_fields`.
fn eval_symbolic_subscript_fields(
    var_name: &str,
    default_width: Option<DataWidth>,
    fields: &[SymbolicSubscriptFieldDecl],
    prefix: &str,
    span: Span,
    consts: &IndexMap<String, ConstMeta>,
    resolved_fields: &mut IndexMap<String, SymbolicSubscriptFieldMeta>,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<u32> {
    let mut offset = 0_u32;

    for field in fields {
        let qualified_name = if prefix.is_empty() {
            field.name.clone()
        } else {
            format!("{prefix}.{}", field.name)
        };

        if resolved_fields.contains_key(&qualified_name) {
            diagnostics.push(Diagnostic::error(
                field.span,
                format!("duplicate symbolic subscript field '.{qualified_name}' in '{var_name}'",),
            ));
            return None;
        }

        // Nested fields: recurse and flatten children into the parent map
        if let Some(nested) = &field.nested_fields {
            let nested_size = eval_symbolic_subscript_fields(
                var_name,
                default_width,
                nested,
                &qualified_name,
                span,
                consts,
                resolved_fields,
                diagnostics,
            )?;

            let Some(next_offset) = offset.checked_add(nested_size) else {
                diagnostics.push(Diagnostic::error(
                    span,
                    format!(
                        "symbolic subscript array '{var_name}' total size overflows address space",
                    ),
                ));
                return None;
            };

            // Adjust child offsets: they were computed relative to 0, shift by current offset
            for (key, meta) in resolved_fields.iter_mut() {
                if key.starts_with(&qualified_name) {
                    meta.offset += offset;
                }
            }

            offset = next_offset;
            continue;
        }

        let data_width = field
            .data_width
            .or(default_width)
            .unwrap_or(DataWidth::Byte);

        let count = match &field.count {
            Some(count_expr) => match eval_const_expr_to_int(count_expr, consts) {
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
                Err(ConstExprError::NonInteger) => {
                    let count_span = field.count_span.unwrap_or(field.span);
                    diagnostics.push(
                        Diagnostic::error(
                            count_span,
                            format!(
                                "symbolic subscript field '.{}' count must be an exact integer value",
                                field.name
                            ),
                        )
                        .with_help(
                            "remove fractional parts before using this value as a field count",
                        ),
                    );
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
            DataWidth::Far => 3_u32,
        };
        let Some(size) = count.checked_mul(element_size) else {
            diagnostics.push(Diagnostic::error(
                field.span,
                format!(
                    "symbolic subscript field '.{}' in '{var_name}' overflows layout size",
                    field.name
                ),
            ));
            return None;
        };

        resolved_fields.insert(
            qualified_name,
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
                    "symbolic subscript array '{var_name}' total size overflows address space",
                ),
            ));
            return None;
        };
        offset = next_offset;
    }

    Some(offset)
}
