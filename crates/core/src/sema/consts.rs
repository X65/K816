use std::collections::HashSet;

use super::*;

pub(super) fn collect_const(
    const_decl: &ConstDecl,
    span: Span,
    model: &mut SemanticModel,
    evaluator_context: &mut EvalContext,
    external_names: &HashSet<String>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if !is_symbol_available(&const_decl.name, model) && !external_names.contains(&const_decl.name) {
        diagnostics.push(
            Diagnostic::error(span, format!("duplicate symbol '{}'", const_decl.name))
                .with_help("rename one of the consts/vars/functions to keep symbols unique"),
        );
        return;
    }

    let initializer_span = const_decl.initializer_span.unwrap_or(span);
    let ctx = ConstEvalCtx::new(&model.consts, &model.vars);
    match eval_const_expr(&const_decl.initializer, &ctx) {
        Ok(value) => {
            model
                .consts
                .insert(const_decl.name.clone(), ConstMeta { value });
            evaluator_context.set(const_decl.name.clone(), value);
        }
        Err(ConstExprError::Ident(name)) => {
            diagnostics.push(bare_ident_diagnostic(
                initializer_span,
                "const initializer",
                &name,
                &ctx,
            ));
        }
        Err(ConstExprError::AddrOfNonFoldable { name, kind }) => {
            diagnostics.push(addr_of_nonfoldable_diagnostic(
                initializer_span,
                &name,
                kind,
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
    external_names: &HashSet<String>,
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
        if model.consts.contains_key(name) && !external_names.contains(name) {
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

pub(super) fn collect_data_block_array(
    block: &DataBlock,
    ctx: &ConstEvalCtx<'_>,
    evaluator_context: &mut EvalContext,
) {
    let Some(name) = block.name.clone() else {
        return;
    };
    let Some(values) = try_collect_data_block_values(block, ctx, evaluator_context) else {
        return;
    };
    evaluator_context.set_array(name, values);
}

pub(super) fn collect_data_block_labels(
    block: &DataBlock,
    block_span: Span,
    model: &mut SemanticModel,
    external_names: &HashSet<String>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if let Some(name) = &block.name {
        let name_span = block.name_span.unwrap_or(block_span);
        register_label(name, name_span, model, external_names, diagnostics);
    }
    for entry in &block.entries {
        if let DataEntry::Label(name) = &entry.node {
            register_label(name, entry.span, model, external_names, diagnostics);
        }
    }
}

fn register_label(
    name: &str,
    span: Span,
    model: &mut SemanticModel,
    external_names: &HashSet<String>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if model.labels.contains_key(name) {
        return;
    }
    if !is_symbol_available(name, model) && !external_names.contains(name) {
        diagnostics.push(
            Diagnostic::error(span, format!("duplicate symbol '{}'", name))
                .with_help("rename one of the consts/vars/functions to keep symbols unique"),
        );
        return;
    }
    model
        .labels
        .insert(name.to_string(), LabelMeta { defined_at: span });
}

fn try_collect_data_block_values(
    block: &DataBlock,
    ctx: &ConstEvalCtx<'_>,
    evaluator_context: &EvalContext,
) -> Option<Vec<Number>> {
    let mut out = Vec::new();
    for entry in &block.entries {
        match &entry.node {
            DataEntry::Segment(_)
            | DataEntry::Label(_)
            | DataEntry::Address(_)
            | DataEntry::Align(_)
            | DataEntry::Nocross(_) => {}
            DataEntry::String(value) => {
                out.extend(value.bytes().map(|byte| Number::Int(i64::from(byte))));
            }
            DataEntry::Values { width, values } => {
                for expr in values {
                    let value = eval_const_expr_to_int(expr, ctx).ok()?;
                    let normalized = match width {
                        DataWidth::Byte => i64::from(u8::try_from(value).ok()?),
                        DataWidth::Word => i64::from(u16::try_from(value).ok()?),
                        DataWidth::Far => {
                            if !(0..=0xFFFFFF).contains(&value) {
                                return None;
                            }
                            value
                        }
                    };
                    out.push(Number::Int(normalized));
                }
            }
            DataEntry::ForEvalRange(range) => {
                out.extend(try_collect_data_range_values(
                    range,
                    ctx,
                    evaluator_context,
                )?);
            }
            DataEntry::Repeat { count, body } => {
                let inner = DataBlock {
                    name: None,
                    name_span: None,
                    entries: body.clone(),
                };
                let inner_values = try_collect_data_block_values(&inner, ctx, evaluator_context)?;
                for _ in 0..*count {
                    out.extend(inner_values.iter().cloned());
                }
            }
            DataEntry::Code(_)
            | DataEntry::Evaluator(_)
            | DataEntry::Charset(_)
            | DataEntry::Convert { .. } => {
                // Code blocks, evaluator/charset directives, and converter
                // entries don't contribute to static const-array values.
            }
        }
    }
    Some(out)
}

fn try_collect_data_range_values(
    range: &DataForEvalRange,
    ctx: &ConstEvalCtx<'_>,
    evaluator_context: &EvalContext,
) -> Option<Vec<Number>> {
    let start = eval_const_expr_to_int(&range.start, ctx).ok()?;
    let end = eval_const_expr_to_int(&range.end, ctx).ok()?;

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

pub(super) struct ConstEvalCtx<'a> {
    pub consts: &'a IndexMap<String, ConstMeta>,
    pub vars: &'a IndexMap<String, VarMeta>,
}

impl<'a> ConstEvalCtx<'a> {
    pub fn new(
        consts: &'a IndexMap<String, ConstMeta>,
        vars: &'a IndexMap<String, VarMeta>,
    ) -> Self {
        Self { consts, vars }
    }
}

pub(super) enum ConstExprError {
    Ident(String),
    EvalText,
    NonInteger,
    Overflow,
    AddrOfNonFoldable {
        name: String,
        kind: NonFoldableVarKind,
    },
}

#[derive(Debug, Clone, Copy)]
pub(super) enum NonFoldableVarKind {
    AutoAbs,
    AutoDp,
    FixedDp,
    Abstract,
}

/// Peel `Expr::Ident` / `Expr::IdentSpanned` (optionally wrapped in a single
/// `Expr::TypedView`, e.g. `&&(NAME:abs)`) and return the inner identifier.
fn address_of_target_name(expr: &Expr) -> Option<&str> {
    match expr {
        Expr::Ident(name) => Some(name.as_str()),
        Expr::IdentSpanned { name, .. } => Some(name.as_str()),
        Expr::TypedView { expr, .. } => match expr.as_ref() {
            Expr::Ident(name) => Some(name.as_str()),
            Expr::IdentSpanned { name, .. } => Some(name.as_str()),
            _ => None,
        },
        _ => None,
    }
}

enum AddrOfFold {
    Folded(u32),
    NonFoldable(NonFoldableVarKind),
    Unknown,
}

fn try_fold_address_of(name: &str, ctx: &ConstEvalCtx<'_>) -> AddrOfFold {
    let Some(var) = ctx.vars.get(name) else {
        return AddrOfFold::Unknown;
    };
    if var.is_abstract() {
        return AddrOfFold::NonFoldable(NonFoldableVarKind::Abstract);
    }
    if let Some(addr) = var.compile_time_address() {
        return AddrOfFold::Folded(addr);
    }
    // No 16-bit absolute address. Classify why.
    let kind = match (&var.placement, var.is_direct_page()) {
        (VarPlacement::Fixed { .. }, true) => NonFoldableVarKind::FixedDp,
        (VarPlacement::AllocatedAbs { .. }, _) => NonFoldableVarKind::AutoAbs,
        (VarPlacement::AllocatedDp, _) => NonFoldableVarKind::AutoDp,
        // Defensive: covers any future variant. `Abstract` was handled above.
        _ => NonFoldableVarKind::AutoAbs,
    };
    AddrOfFold::NonFoldable(kind)
}

fn eval_const_expr(expr: &Expr, ctx: &ConstEvalCtx<'_>) -> Result<Number, ConstExprError> {
    match expr {
        Expr::Number(value, _) => Ok(Number::Int(*value)),
        Expr::Ident(name) => ctx
            .consts
            .get(name)
            .map(|constant| constant.value)
            .ok_or_else(|| ConstExprError::Ident(name.clone())),
        Expr::IdentSpanned { name, .. } => ctx
            .consts
            .get(name)
            .map(|constant| constant.value)
            .ok_or_else(|| ConstExprError::Ident(name.clone())),
        Expr::EvalText(_) => Err(ConstExprError::EvalText),
        Expr::Index { base, index } => {
            let base = eval_const_expr_to_int(base, ctx)?;
            let index = eval_const_expr_to_int(index, ctx)?;
            base.checked_add(index)
                .map(Number::Int)
                .ok_or(ConstExprError::Overflow)
        }
        Expr::Member { field, .. } => Err(ConstExprError::Ident(field.clone())),
        Expr::Binary { op, lhs, rhs } => {
            let lhs = eval_const_expr(lhs, ctx)?;
            let rhs = eval_const_expr(rhs, ctx)?;
            let map_err = |error| match error {
                EvaluatorError::Overflow => ConstExprError::Overflow,
                _ => ConstExprError::NonInteger,
            };
            match op {
                ExprBinaryOp::Add => lhs.checked_add(rhs).map_err(map_err),
                ExprBinaryOp::Sub => lhs.checked_sub(rhs).map_err(map_err),
                ExprBinaryOp::Mul => lhs.checked_mul(rhs).map_err(map_err),
                ExprBinaryOp::BitOr => lhs.bit_or(rhs).map_err(map_err),
                ExprBinaryOp::BitAnd => lhs.bit_and(rhs).map_err(map_err),
                ExprBinaryOp::BitXor => lhs.bit_xor(rhs).map_err(map_err),
                ExprBinaryOp::Shl => lhs.checked_shl(rhs).map_err(map_err),
                ExprBinaryOp::Shr => lhs.checked_shr(rhs).map_err(map_err),
            }
        }
        Expr::Unary { op, expr } => {
            // Address-of fold: `&&NAME` / `&&&NAME` resolves to a literal
            // when NAME is a fixed-absolute-address var. DP-fixed,
            // auto-allocated, and abstract vars produce a precise error.
            // Names that don't resolve as a var fall through to const
            // lookup, preserving `&&LITERAL_CONST_NAME` and `&&123`.
            if matches!(
                op,
                ExprUnaryOp::WordLittleEndian | ExprUnaryOp::FarLittleEndian
            ) && let Some(name) = address_of_target_name(expr)
            {
                match try_fold_address_of(name, ctx) {
                    AddrOfFold::Folded(addr) => return Ok(Number::Int(i64::from(addr))),
                    AddrOfFold::NonFoldable(kind) => {
                        return Err(ConstExprError::AddrOfNonFoldable {
                            name: name.to_string(),
                            kind,
                        });
                    }
                    AddrOfFold::Unknown => {}
                }
            }
            let value = eval_const_expr_to_int(expr, ctx)?;
            match op {
                ExprUnaryOp::LowByte => Ok(Number::Int(value & 0xFF)),
                ExprUnaryOp::HighByte => Ok(Number::Int((value >> 8) & 0xFF)),
                ExprUnaryOp::WordLittleEndian | ExprUnaryOp::FarLittleEndian => {
                    Ok(Number::Int(value))
                }
                ExprUnaryOp::EvalBracketed => Ok(Number::Int(value)),
                ExprUnaryOp::AddressPositioned => Ok(Number::Int(value)),
                ExprUnaryOp::Negate => value
                    .checked_neg()
                    .map(Number::Int)
                    .ok_or(ConstExprError::Overflow),
                ExprUnaryOp::BitNot => Ok(Number::Int(!value)),
            }
        }
        Expr::TypedView { expr, .. } => eval_const_expr(expr, ctx),
        Expr::MetadataQuery { expr, .. } => {
            // :sizeof / :offsetof require VarMeta which is not available during
            // const propagation.  Extract the identifier name from the inner
            // expression so the caller can report a meaningful error.
            let name = match expr.as_ref() {
                Expr::Ident(n) | Expr::IdentSpanned { name: n, .. } => n.clone(),
                _ => String::new(),
            };
            Err(ConstExprError::Ident(name))
        }
    }
}

pub(super) fn eval_const_expr_to_int(
    expr: &Expr,
    ctx: &ConstEvalCtx<'_>,
) -> Result<i64, ConstExprError> {
    let value = eval_const_expr(expr, ctx)?;
    value.to_i64_exact().ok_or(ConstExprError::NonInteger)
}

/// Diagnostic for `&&NAME` where NAME is a known var but not a fixed-absolute
/// address. Shared by every site that calls into the const-eval helpers.
pub(super) fn addr_of_nonfoldable_diagnostic(
    span: Span,
    name: &str,
    kind: NonFoldableVarKind,
) -> Diagnostic {
    let (help, note) = match kind {
        NonFoldableVarKind::AutoAbs => (
            format!(
                "'{name}' is auto-allocated; declare it with an explicit address (e.g. `var {name} = $XXXX`) so its address is known at compile time"
            ),
            "Auto-allocated vars get their addresses from the linker, so `&&NAME` cannot fold to a literal here.",
        ),
        NonFoldableVarKind::AutoDp => (
            format!(
                "'{name}' is a direct-page var with a linker-assigned slot; its 16-bit address depends on the runtime D register and is not statically known"
            ),
            "DP-allocated vars carry a 1-byte slot offset; the absolute address depends on the runtime D register, so `&&NAME` cannot fold to a literal.",
        ),
        NonFoldableVarKind::FixedDp => (
            format!(
                "'{name}' is a direct-page var; `&&` produces a 16-bit absolute address, which is not statically known for DP vars (the runtime D register supplies the high byte). Declare '{name}' as a non-DP fixed var, or compute the address from D at runtime."
            ),
            "DP-fixed vars carry a 1-byte slot offset, not a 16-bit address. The runtime `tcd`/`pld` value supplies the high byte at execution time.",
        ),
        NonFoldableVarKind::Abstract => (
            format!(
                "'{name}' is an abstract layout-only var with no storage; use `:sizeof` / `:offsetof` for metadata, or declare a real var to take its address"
            ),
            "`abstract var` declares packed field offsets only. It emits no bytes and has no address.",
        ),
    };
    Diagnostic::error(
        span,
        format!("address-of of '{name}' cannot be folded at compile time"),
    )
    .with_primary_label("non-foldable address-of")
    .with_help(help)
    .with_note(note)
}

/// Build the bare-ident "must be a constant numeric expression" diagnostic with
/// an extra `&&NAME` hint when NAME refers to a fixed-absolute-address var.
/// `context_label` is e.g. "const initializer" or "var initializer".
pub(super) fn bare_ident_diagnostic(
    span: Span,
    context_label: &str,
    name: &str,
    ctx: &ConstEvalCtx<'_>,
) -> Diagnostic {
    let mut diag = Diagnostic::error(
        span,
        format!("{context_label} '{name}' must be a constant numeric expression"),
    );
    if ctx
        .vars
        .get(name)
        .and_then(|var| var.compile_time_address())
        .is_some()
    {
        diag = diag.with_help(format!(
            "'{name}' is a fixed-address var; if you meant its address, write `&&{name}` (e.g. `&&{name} + offset`)"
        ));
    }
    diag
}

pub(super) fn is_symbol_available(symbol: &str, model: &SemanticModel) -> bool {
    !model.functions.contains_key(symbol)
        && !model.vars.contains_key(symbol)
        && !model.consts.contains_key(symbol)
        && !model.labels.contains_key(symbol)
}
