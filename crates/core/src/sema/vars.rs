use std::collections::{HashMap, HashSet};

use crate::ast::ForceAddrMode;

use super::*;

pub(super) fn collect_var(
    var: &VarDecl,
    span: Span,
    cursors: &mut HashMap<String, u32>,
    current_segment: &str,
    model: &mut SemanticModel,
    external_names: &HashSet<String>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if !is_symbol_available(&var.name, model) && !external_names.contains(&var.name) {
        diagnostics.push(
            Diagnostic::error(span, format!("duplicate symbol '{}'", var.name))
                .with_help("rename one of the vars/functions to keep symbols unique"),
        );
        return;
    }

    if var.is_abstract {
        if !validate_abstract_var(var, span, diagnostics) {
            return;
        }
        let Some(layout) = eval_var_layout(var, span, &model.consts, diagnostics) else {
            return;
        };
        model.vars.insert(
            var.name.clone(),
            VarMeta {
                placement: VarPlacement::Abstract,
                size: layout.size,
                element_size: layout.element_size,
                repeat_count: layout.repeat_count,
                data_width: var.data_width,
                addr_mode_default: None,
                symbolic_subscript: layout.symbolic_subscript,
            },
        );
        return;
    }

    let Some(layout) = eval_var_layout(var, span, &model.consts, diagnostics) else {
        return;
    };

    let placement = match eval_var_placement(
        var,
        current_segment,
        cursors,
        layout.size,
        span,
        &model.consts,
        diagnostics,
    ) {
        Some(p) => p,
        None => return,
    };

    model.vars.insert(
        var.name.clone(),
        VarMeta {
            placement,
            size: layout.size,
            element_size: layout.element_size,
            repeat_count: layout.repeat_count,
            data_width: var.data_width,
            addr_mode_default: var.addr_mode_default,
            symbolic_subscript: layout.symbolic_subscript,
        },
    );
}

fn validate_abstract_var(var: &VarDecl, span: Span, diagnostics: &mut Vec<Diagnostic>) -> bool {
    let mut valid = true;

    if var.symbolic_subscript_fields.is_none() && var.array_len.is_none() {
        diagnostics.push(
            Diagnostic::error(
                span,
                format!("abstract var '{}' must declare a symbolic field layout", var.name),
            )
            .with_primary_label("abstract var without fields")
            .with_help(format!(
                "write `abstract var {}[ .field:byte ]` for layout metadata, or remove `abstract` to declare real storage",
                var.name
            ))
            .with_note(
                "`abstract var` carries only `:sizeof` and `:offsetof` metadata. Without a symbolic field list there is no layout to describe, and no storage is allocated.",
            ),
        );
        valid = false;
    }

    if var.array_len.is_some() {
        diagnostics.push(
            Diagnostic::error(
                span,
                format!("abstract var '{}' cannot use an array length", var.name),
            )
            .with_primary_label("array length on abstract var")
            .with_help(format!(
                "use a symbolic field list such as `abstract var {}[ .item:byte ]`, or remove `abstract` to allocate an array",
                var.name
            ))
            .with_note(
                "`abstract var` is for named field offsets only. Array-only declarations like `abstract var buf[16]` would define size without any field names to query.",
            ),
        );
        valid = false;
    }

    if var.addr_mode_default.is_some() {
        diagnostics.push(
            Diagnostic::error(
                span,
                format!("abstract var '{}' cannot use a storage prefix", var.name),
            )
            .with_primary_label("storage prefix on abstract var")
            .with_help(format!(
                "remove the `dp`/`abs`/`far` prefix from '{}'; abstract vars have no address or storage class",
                var.name
            ))
            .with_note(
                "Storage prefixes select how a real variable is addressed or allocated. An abstract var emits no symbol and cannot be used as an instruction operand.",
            ),
        );
        valid = false;
    }

    if var.initializer.is_some() {
        diagnostics.push(
            Diagnostic::error(
                var.initializer_span.unwrap_or(span),
                format!("abstract var '{}' cannot have an address initializer", var.name),
            )
            .with_primary_label("initializer on abstract var")
            .with_help(format!(
                "remove `= ...` from '{}', or remove `abstract` if this should name a real memory location",
                var.name
            ))
            .with_note(
                "`var NAME[...] = EXPR` pins a concrete memory location. `abstract var` intentionally has no concrete location.",
            ),
        );
        valid = false;
    }

    if var.alloc_count.is_some() {
        diagnostics.push(
            Diagnostic::error(
                span,
                format!("abstract var '{}' cannot use an allocation count", var.name),
            )
            .with_primary_label("allocation count on abstract var")
            .with_help(format!(
                "remove `* count` from '{}'; use `{}:sizeof * count` in expressions when you need scaled layout math",
                var.name, var.name
            ))
            .with_note(
                "`* count` reserves repeated storage for real vars. Abstract vars reserve no storage, so repetition belongs at the use site as arithmetic.",
            ),
        );
        valid = false;
    }

    valid
}

fn eval_var_placement(
    var: &VarDecl,
    current_segment: &str,
    cursors: &mut HashMap<String, u32>,
    size: u32,
    span: Span,
    consts: &IndexMap<String, ConstMeta>,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<VarPlacement> {
    if let Some(initializer) = &var.initializer {
        let address = eval_var_fixed_address(var, initializer, span, consts, diagnostics)?;
        if matches!(var.addr_mode_default, Some(ForceAddrMode::DirectPage)) && address > 0xFF {
            diagnostics.push(
                Diagnostic::error(
                    var.initializer_span.unwrap_or(span),
                    format!(
                        "DP variable '{}' fixed offset {:#06X} exceeds the direct page (0x00..=0xFF)",
                        var.name, address
                    ),
                )
                .with_primary_label("DP offset out of range")
                .with_help(format!(
                    "use an offset in the range 0x00..=0xFF, or remove the `dp` prefix to declare '{}' as an absolute (16-bit) variable",
                    var.name
                ))
                .with_note(
                    "`var dp NAME = $X` pins the variable to a slot inside the 256-byte direct page. The slot is 1 byte wide because DP-mode addressing always uses a u8 offset relative to the runtime D register; 16-bit values cannot encode there.",
                ),
            );
            return None;
        }
        return Some(VarPlacement::Fixed { address });
    }

    // No initializer — the linker assigns the final address.
    match var.addr_mode_default {
        Some(ForceAddrMode::AbsoluteLong) => {
            diagnostics.push(
                Diagnostic::error(
                    span,
                    format!(
                        "FAR variable '{}' must have an explicit address",
                        var.name
                    ),
                )
                .with_primary_label("missing address for far variable")
                .with_help(format!(
                    "give '{}' an address (e.g. `var far {} = $7E0000`); the assembler does not auto-allocate FAR storage because it has no notion of which 64K bank to place it in",
                    var.name, var.name
                ))
                .with_note(
                    "FAR variables live in the 24-bit address space and are addressed with long-form opcodes. Auto-allocation is only available for DP (256-byte logical pool) and ABS (per-segment cursor) storage classes; banked FAR placement is the user's responsibility.",
                ),
            );
            None
        }
        Some(ForceAddrMode::DirectPage) => {
            if size > 256 {
                diagnostics.push(
                    Diagnostic::error(
                        span,
                        format!(
                            "DP variable '{}' size {} bytes exceeds the 256-byte direct page",
                            var.name, size
                        ),
                    )
                    .with_primary_label("oversized DP variable")
                    .with_help(format!(
                        "shrink '{}' or move it out of DP — declare it as `var {}` (absolute) so it lives in the segment instead",
                        var.name, var.name
                    ))
                    .with_note(
                        "The direct page is a single 256-byte window; a single variable cannot exceed that bound.",
                    ),
                );
                return None;
            }
            // Track unit-local DP usage with a synthetic cursor key so the
            // existing per-segment cursor map can host it without leaking the
            // name into segment-rule selection.
            let used = *cursors.get(crate::DP_CURSOR_KEY).unwrap_or(&0);
            let Some(next_used) = used.checked_add(size) else {
                diagnostics.push(Diagnostic::error(
                    span,
                    format!(
                        "DP variable '{}' allocation overflows: per-unit DP usage already at {:#X} bytes",
                        var.name, used
                    ),
                ));
                return None;
            };
            if next_used > 256 {
                diagnostics.push(
                    Diagnostic::error(
                        span,
                        format!(
                            "DP variable '{}' would push per-unit DP usage to {} bytes (limit: 256)",
                            var.name, next_used
                        ),
                    )
                    .with_primary_label("DP pool exhausted")
                    .with_help(format!(
                        "consolidate DP usage by moving non-hot vars to absolute storage (`var {}` instead of `var dp {}`), or pin selected DP vars to specific offsets to free contiguous runs",
                        var.name, var.name
                    ))
                    .with_note(
                        "DP is a 256-byte logical pool. The runtime decides where it physically lives via `tcd`/`pld`; the assembler only tracks 8-bit offsets within the page, so total DP allocation cannot exceed 256 bytes per compilation unit.",
                    ),
                );
                return None;
            }
            cursors.insert(crate::DP_CURSOR_KEY.to_string(), next_used);
            Some(VarPlacement::AllocatedDp)
        }
        _ => {
            // Absolute (default and `abs`-prefixed) — keep the existing
            // per-segment cursor.
            let offset = *cursors.get(current_segment).unwrap_or(&0);
            let Some(next_offset) = offset.checked_add(size) else {
                diagnostics.push(Diagnostic::error(
                    span,
                    format!(
                        "var allocation for '{}' overflows segment '{current_segment}' (start_offset={offset:#X}, size={size})",
                        var.name
                    ),
                ));
                return None;
            };
            cursors.insert(current_segment.to_string(), next_offset);
            Some(VarPlacement::AllocatedAbs {
                segment: current_segment.to_string(),
                offset,
            })
        }
    }
}

fn eval_var_fixed_address(
    var: &VarDecl,
    initializer: &Expr,
    span: Span,
    consts: &IndexMap<String, ConstMeta>,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<u32> {
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
            diagnostics.push(
                Diagnostic::error(
                    initializer_span,
                    format!("var initializer '{name}' must be a constant numeric expression"),
                )
                .with_primary_label(format!("non-constant identifier `{name}`"))
                .with_help(format!(
                    "`var` initializers fix the variable's address at compile time, so the right-hand side must reduce to a number; replace `{name}` with a literal address, an arithmetic expression over literals, or a `const` that resolves to a number"
                ))
                .with_note(
                    "K816 `var = expr` declares a memory-located variable at the address `expr` evaluates to. That address is baked into every reference at compile time — there is no runtime computation to fall back on, so identifiers without a compile-time value cannot appear here.",
                ),
            );
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
    element_size: u32,
    repeat_count: u32,
    symbolic_subscript: Option<SymbolicSubscriptMeta>,
}

fn eval_var_layout(
    var: &VarDecl,
    span: Span,
    consts: &IndexMap<String, ConstMeta>,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<VarLayout> {
    let mut layout = eval_var_base_layout(var, span, consts, diagnostics)?;

    if let Some(alloc_count_expr) = &var.alloc_count {
        match eval_const_expr_to_int(alloc_count_expr, consts) {
            Ok(value) => {
                if value <= 0 {
                    diagnostics.push(Diagnostic::error(
                        span,
                        format!("var allocation count must be positive: {value}"),
                    ));
                    return None;
                }
                match u32::try_from(value) {
                    Ok(count) => {
                        let Some(new_size) = layout.size.checked_mul(count) else {
                            diagnostics.push(Diagnostic::error(
                                span,
                                format!(
                                    "var allocation for '{}' overflows address space (base_size={}, count={count})",
                                    var.name, layout.size
                                ),
                            ));
                            return None;
                        };
                        layout.size = new_size;
                        layout.repeat_count = count;
                    }
                    Err(_) => {
                        diagnostics.push(Diagnostic::error(
                            span,
                            format!("var allocation count is out of range: {value}"),
                        ));
                        return None;
                    }
                }
            }
            Err(ConstExprError::Ident(name)) => {
                diagnostics.push(Diagnostic::error(
                    span,
                    format!("var allocation count '{name}' must be a constant numeric expression"),
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
                diagnostics.push(
                    Diagnostic::error(span, "var allocation count must be an exact integer value")
                        .with_help(
                            "remove fractional parts or convert to an integer before using it as an allocation count",
                        ),
                );
                return None;
            }
            Err(ConstExprError::Overflow) => {
                diagnostics.push(Diagnostic::error(
                    span,
                    "var allocation count overflows numeric literal range",
                ));
                return None;
            }
        }
    }

    Some(layout)
}

fn eval_var_base_layout(
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
            element_size: symbolic_subscript.total_size,
            repeat_count: 1,
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
            element_size,
            repeat_count: 1,
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
                Ok(count) => {
                    let size = count * element_size;
                    Some(VarLayout {
                        size,
                        element_size: size,
                        repeat_count: 1,
                        symbolic_subscript: None,
                    })
                }
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
#[allow(clippy::too_many_arguments)]
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
            diagnostics.push(
                Diagnostic::error(
                    field.span,
                    format!("duplicate symbolic subscript field '.{qualified_name}' in '{var_name}'"),
                )
                .with_primary_label("duplicate field")
                .with_help(format!(
                    "rename one of the `.{qualified_name}` entries; each field name in '{var_name}' must be unique within its enclosing struct"
                ))
                .with_note(
                    "Symbolic subscripts compile to a fixed offset from the var's base address; two fields with the same name would resolve to the same offset, so the parser refuses to choose between them.",
                ),
            );
            return None;
        }

        // Nested fields: recurse and flatten children into the parent map
        if let Some(nested) = &field.nested_fields {
            // Insert the parent composite field first so it appears before
            // children in IndexMap iteration order (hover/display).
            // Use a placeholder size; we'll update it after recursion.
            resolved_fields.insert(
                qualified_name.clone(),
                SymbolicSubscriptFieldMeta {
                    offset,
                    size: 0,
                    data_width: None,
                    count: 1,
                },
            );

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

            // Update the parent entry with the actual computed size.
            resolved_fields.get_mut(&qualified_name).unwrap().size = nested_size;

            let Some(next_offset) = offset.checked_add(nested_size) else {
                diagnostics.push(Diagnostic::error(
                    span,
                    format!(
                        "symbolic subscript array '{var_name}' total size overflows address space",
                    ),
                ));
                return None;
            };

            // Adjust child offsets: they were computed relative to 0, shift by current offset.
            // Match only true children (requires a dot boundary) so sibling fields
            // sharing a prefix (e.g. `.ab` vs `.a`) aren't incorrectly shifted.
            let qualified_prefix = format!("{qualified_name}.");
            for (key, meta) in resolved_fields.iter_mut() {
                if key.starts_with(&qualified_prefix) {
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
                        diagnostics.push(
                            Diagnostic::error(
                                count_span,
                                format!(
                                    "symbolic subscript field '.{}' count must be >= 1, found {value}",
                                    field.name
                                ),
                            )
                            .with_primary_label("non-positive count")
                            .with_help(format!(
                                "give `.{}` a count of 1 or more, or drop the `[...]` if it is a single value rather than an array",
                                field.name
                            ))
                            .with_note(
                                "A symbolic subscript field's count is the number of array elements reserved at that offset; values of 0 or below would mean no slot, which is what omitting the field already expresses.",
                            ),
                        );
                        return None;
                    }
                    match u32::try_from(value) {
                        Ok(count) => count,
                        Err(_) => {
                            let count_span = field.count_span.unwrap_or(field.span);
                            diagnostics.push(
                                Diagnostic::error(
                                    count_span,
                                    format!(
                                        "symbolic subscript field '.{}' count is out of range: {value}",
                                        field.name
                                    ),
                                )
                                .with_primary_label("count out of range")
                                .with_help(format!(
                                    "`.{}` count must fit in a 32-bit unsigned value (0..=0xFFFFFFFF); reduce the literal or split the field into smaller groups",
                                    field.name
                                )),
                        );
                            return None;
                        }
                    }
                }
                Err(ConstExprError::Ident(name)) => {
                    let count_span = field.count_span.unwrap_or(field.span);
                    diagnostics.push(
                        Diagnostic::error(
                            count_span,
                            format!(
                                "symbolic subscript field '.{}' count expression '{name}' must be a constant numeric expression",
                                field.name
                            ),
                        )
                        .with_primary_label(format!("non-constant identifier `{name}`"))
                        .with_help(format!(
                            "replace `{name}` with a literal, an arithmetic expression over literals, or a `const` declaration that resolves to a number at compile time"
                        ))
                        .with_note(
                            "Field counts and offsets are baked into the encoded layout; everything inside `[...]` must reduce to a number during semantic analysis, before any code runs.",
                        ),
                    );
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
                    diagnostics.push(
                        Diagnostic::error(
                            count_span,
                            format!(
                                "symbolic subscript field '.{}' count expression overflows numeric literal range",
                                field.name
                            ),
                        )
                        .with_primary_label("count expression overflows")
                        .with_help(format!(
                            "shrink the count for `.{}` to a value the evaluator can hold; the compile-time integer evaluator uses i64-range arithmetic",
                            field.name
                        )),
                    );
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
                data_width: Some(data_width),
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
