use super::*;

pub(super) fn collect_const(
    const_decl: &ConstDecl,
    span: Span,
    model: &mut SemanticModel,
    evaluator_context: &mut EvalContext,
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
            evaluator_context.set(const_decl.name.clone(), value);
        }
        Err(ConstExprError::Ident(name)) => {
            diagnostics.push(Diagnostic::error(
                initializer_span,
                format!("const initializer '{name}' must be a constant numeric expression"),
            ));
        }
        Err(ConstExprError::NonInteger) => {
            diagnostics.push(
                Diagnostic::error(
                    initializer_span,
                    "const initializer requires an exact integer value in this expression",
                )
                .with_help("remove floating-point components or avoid integer-only operators"),
            );
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

pub(super) fn collect_evaluator_block(
    block: &EvaluatorBlock,
    span: Span,
    model: &mut SemanticModel,
    evaluator_context: &mut EvalContext,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let mut block_context = evaluator_context.clone();
    let outcome = match k816_eval::evaluate_with_context(&block.text, &mut block_context) {
        Ok(outcome) => outcome,
        Err(error) => {
            diagnostics.push(map_evaluator_error(error, span));
            return;
        }
    };

    for (name, _) in &outcome.assigned {
        if model.consts.contains_key(name) {
            diagnostics.push(
                Diagnostic::error(
                    span,
                    format!("cannot reassign constant '{name}' in a different evaluator block"),
                )
                .with_help(
                    "mutate a constant only within the same top-level evaluator block, or choose a new name",
                ),
            );
            return;
        }

        if model.functions.contains_key(name) || model.vars.contains_key(name) {
            diagnostics.push(
                Diagnostic::error(span, format!("duplicate symbol '{}'", name))
                    .with_help("rename one of the consts/vars/functions to keep symbols unique"),
            );
            return;
        }
    }

    for (name, value) in outcome.assigned {
        model.consts.insert(name.clone(), ConstMeta { value });
    }
    *evaluator_context = block_context;
}

pub(super) fn collect_named_data_block_array(
    block: &NamedDataBlock,
    consts: &IndexMap<String, ConstMeta>,
    evaluator_context: &mut EvalContext,
) {
    let Some(values) = try_collect_named_data_block_values(block, consts, evaluator_context) else {
        return;
    };
    evaluator_context.set_array(block.name.clone(), values);
}

fn try_collect_named_data_block_values(
    block: &NamedDataBlock,
    consts: &IndexMap<String, ConstMeta>,
    evaluator_context: &EvalContext,
) -> Option<Vec<Number>> {
    let mut out = Vec::new();
    for entry in &block.entries {
        match &entry.node {
            NamedDataEntry::Segment(_)
            | NamedDataEntry::Label(_)
            | NamedDataEntry::Address(_)
            | NamedDataEntry::Align(_)
            | NamedDataEntry::Nocross(_) => {}
            NamedDataEntry::String(value) => {
                out.extend(value.bytes().map(|byte| Number::Int(i64::from(byte))));
            }
            NamedDataEntry::Bytes(values) => {
                for expr in values {
                    let value = eval_const_expr_to_int(expr, consts).ok()?;
                    let byte = u8::try_from(value).ok()?;
                    out.push(Number::Int(i64::from(byte)));
                }
            }
            NamedDataEntry::Words(values) => {
                for expr in values {
                    let value = eval_const_expr_to_int(expr, consts).ok()?;
                    let word = u16::try_from(value).ok()?;
                    out.push(Number::Int(i64::from(word)));
                }
            }
            NamedDataEntry::Fars(values) => {
                for expr in values {
                    let value = eval_const_expr_to_int(expr, consts).ok()?;
                    if !(0..=0xFFFFFF).contains(&value) {
                        return None;
                    }
                    out.push(Number::Int(value));
                }
            }
            NamedDataEntry::ForEvalRange(range) => {
                out.extend(try_collect_named_data_range_values(
                    range,
                    consts,
                    evaluator_context,
                )?);
            }
            NamedDataEntry::Repeat { count, body } => {
                let inner = NamedDataBlock {
                    name: String::new(),
                    name_span: entry.span,
                    entries: body.clone(),
                };
                let inner_values =
                    try_collect_named_data_block_values(&inner, consts, evaluator_context)?;
                for _ in 0..*count {
                    out.extend(inner_values.iter().cloned());
                }
            }
            NamedDataEntry::Code(_) | NamedDataEntry::Evaluator(_) | NamedDataEntry::Charset(_) => {
                // Code blocks, evaluator and charset directives don't contribute to static data values
            }
        }
    }
    Some(out)
}

fn try_collect_named_data_range_values(
    range: &NamedDataForEvalRange,
    consts: &IndexMap<String, ConstMeta>,
    evaluator_context: &EvalContext,
) -> Option<Vec<Number>> {
    let start = eval_const_expr_to_int(&range.start, consts).ok()?;
    let end = eval_const_expr_to_int(&range.end, consts).ok()?;

    let mut context = evaluator_context.clone();
    let mut out = Vec::new();
    let step = if start <= end { 1_i64 } else { -1_i64 };
    let mut current = start;

    loop {
        context.set(range.iterator.as_str(), Number::Int(current));
        let outcome = k816_eval::evaluate_with_context(&range.eval, &mut context).ok()?;
        let value = outcome.value.to_i64_exact()?;
        let byte = u8::try_from(value).ok()?;
        out.push(Number::Int(i64::from(byte)));

        if current == end {
            break;
        }
        current = current.checked_add(step)?;
    }

    Some(out)
}

fn map_evaluator_error(error: EvaluatorError, span: Span) -> Diagnostic {
    match error {
        EvaluatorError::UnknownIdentifier { name, start, end } => {
            let primary = evaluator_relative_span(span, start, end);
            Diagnostic::error(
                primary,
                format!("unknown identifier '{name}' in top-level evaluator block"),
            )
            .with_help(
                "define the identifier earlier in the file or assign it in the same evaluator block",
            )
        }
        EvaluatorError::UnknownFunction { name } => {
            Diagnostic::error(span, format!("unknown evaluator function '{name}'"))
        }
        EvaluatorError::DeferredFunction { name, reason } => Diagnostic::error(
            span,
            format!("evaluator function '{name}' is not supported in top-level blocks yet"),
        )
        .with_help(reason),
        EvaluatorError::BadArity {
            name,
            expected,
            got,
        } => Diagnostic::error(
            span,
            format!("function '{name}' expected {expected} arguments, got {got}"),
        ),
        EvaluatorError::InvalidAssignmentTarget => {
            Diagnostic::error(span, "invalid assignment target in evaluator block")
                .with_help("assign only to identifiers (for example: `NAME = expr`)")
        }
        EvaluatorError::IntegerRequired { op } => Diagnostic::error(
            span,
            format!("operator '{op}' requires exact integer operands"),
        ),
        EvaluatorError::DivisionByZero => Diagnostic::error(span, "division by zero"),
        EvaluatorError::Overflow => Diagnostic::error(span, "arithmetic overflow"),
        EvaluatorError::UnexpectedToken { column, token } => {
            let primary = evaluator_column_span(span, column);
            Diagnostic::error(
                primary,
                format!("unexpected token {token} in evaluator block at column {column}"),
            )
        }
        EvaluatorError::UnexpectedEof => {
            Diagnostic::error(span, "unexpected end of evaluator expression")
        }
        EvaluatorError::InvalidNumber { literal } => {
            Diagnostic::error(span, format!("invalid number literal '{literal}'"))
        }
        EvaluatorError::ArrayLiteralInNumericContext => Diagnostic::error(
            span,
            "array literal can only be assigned to an identifier or indexed",
        ),
    }
}

fn evaluator_relative_span(block_span: Span, start: usize, end: usize) -> Span {
    let content_start = block_span.start.saturating_add(1);
    let content_end = block_span.end.saturating_sub(1);
    let content_len = content_end.saturating_sub(content_start);
    let rel_start = start.min(content_len);
    let rel_end = end.min(content_len).max(rel_start.saturating_add(1));

    Span::new(
        block_span.source_id,
        content_start.saturating_add(rel_start),
        content_start.saturating_add(rel_end),
    )
}

fn evaluator_column_span(block_span: Span, column: usize) -> Span {
    let start = column.saturating_sub(1);
    evaluator_relative_span(block_span, start, start.saturating_add(1))
}

pub(super) enum ConstExprError {
    Ident(String),
    EvalText,
    NonInteger,
    Overflow,
}

fn eval_const_expr(
    expr: &Expr,
    consts: &IndexMap<String, ConstMeta>,
) -> Result<Number, ConstExprError> {
    match expr {
        Expr::Number(value, _) => Ok(Number::Int(*value)),
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
            let base = eval_const_expr_to_int(base, consts)?;
            let index = eval_const_expr_to_int(index, consts)?;
            base.checked_add(index)
                .map(Number::Int)
                .ok_or(ConstExprError::Overflow)
        }
        Expr::Binary { op, lhs, rhs } => {
            let lhs = eval_const_expr(lhs, consts)?;
            let rhs = eval_const_expr(rhs, consts)?;
            match op {
                ExprBinaryOp::Add => lhs.checked_add(rhs).map_err(|error| match error {
                    EvaluatorError::Overflow => ConstExprError::Overflow,
                    _ => ConstExprError::NonInteger,
                }),
                ExprBinaryOp::Sub => lhs.checked_sub(rhs).map_err(|error| match error {
                    EvaluatorError::Overflow => ConstExprError::Overflow,
                    _ => ConstExprError::NonInteger,
                }),
                ExprBinaryOp::Mul => lhs.checked_mul(rhs).map_err(|error| match error {
                    EvaluatorError::Overflow => ConstExprError::Overflow,
                    _ => ConstExprError::NonInteger,
                }),
            }
        }
        Expr::Unary { op, expr } => {
            let value = eval_const_expr_to_int(expr, consts)?;
            match op {
                ExprUnaryOp::LowByte => Ok(Number::Int(value & 0xFF)),
                ExprUnaryOp::HighByte => Ok(Number::Int((value >> 8) & 0xFF)),
                ExprUnaryOp::WordLittleEndian | ExprUnaryOp::FarLittleEndian => {
                    Ok(Number::Int(value))
                }
                ExprUnaryOp::EvalBracketed => Ok(Number::Int(value)),
            }
        }
        Expr::TypedView { expr, .. } => eval_const_expr(expr, consts),
    }
}

pub(super) fn eval_const_expr_to_int(
    expr: &Expr,
    consts: &IndexMap<String, ConstMeta>,
) -> Result<i64, ConstExprError> {
    let value = eval_const_expr(expr, consts)?;
    value.to_i64_exact().ok_or(ConstExprError::NonInteger)
}

pub(super) fn is_symbol_available(symbol: &str, model: &SemanticModel) -> bool {
    !model.functions.contains_key(symbol)
        && !model.vars.contains_key(symbol)
        && !model.consts.contains_key(symbol)
}
