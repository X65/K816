use crate::ast::{AddressHint, Expr, ExprBinaryOp, ExprUnaryOp, NumFmt};
use crate::lexer::{NumLit, TokenKind};
use crate::span::SourceId;
use chumsky::{
    IterParser, Parser as _,
    error::Rich,
    input::ValueInput,
    prelude::{SimpleSpan, end, just},
    recursive::recursive,
};

use super::{
    ParseExtra, address_hint_parser, data_width_parser, line_sep_parser, parse_expression_fragment,
    spanned,
};

#[derive(Debug, Clone, Copy)]
enum ExprSuffix {
    TypedView(crate::ast::DataWidth),
    AddressHint(AddressHint),
}

pub(super) fn expr_parser<'src, I>() -> impl chumsky::Parser<'src, I, Expr, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    recursive(|expr| {
        let number_atom = chumsky::select! {
            TokenKind::Number(NumLit { value, fmt }) => (value, fmt)
        }
        .map(|(v, f)| Expr::Number(v, f));

        let ident_atom = spanned(
            chumsky::select! { TokenKind::Ident(value) => value },
            SourceId(0),
        )
        .map(|ident| {
            let name = ident.node;
            Expr::IdentSpanned {
                name,
                start: ident.span.start,
                end: ident.span.end,
            }
        });

        let eval_atom = spanned(
            chumsky::select! { TokenKind::Eval(value) => value },
            SourceId(0),
        )
        .map(|eval_text| {
            parse_eval_expr_token_with_token_bounds(
                &eval_text.node,
                eval_text.span.start,
                eval_text.span.end,
            )
        });

        let base_atom = number_atom
            .or(ident_atom)
            .or(eval_atom)
            .or(just(TokenKind::LParen)
                .ignore_then(expr.clone())
                .then_ignore(just(TokenKind::RParen)));

        let atom = base_atom
            .then(
                chumsky::select! { TokenKind::Eval(value) => value }
                    .repeated()
                    .collect::<Vec<_>>(),
            )
            .try_map(|(base, suffixes), span| {
                apply_eval_suffixes(base, suffixes).map_err(|message| Rich::custom(span, message))
            });

        let unary = just(TokenKind::Amp)
            .ignore_then(
                just(TokenKind::Lt)
                    .to(ExprUnaryOp::LowByte)
                    .or(just(TokenKind::Gt).to(ExprUnaryOp::HighByte)),
            )
            .repeated()
            .collect::<Vec<_>>()
            .then(atom)
            .map(|(ops, mut inner)| {
                for op in ops.into_iter().rev() {
                    inner = Expr::Unary {
                        op,
                        expr: Box::new(inner),
                    };
                }
                inner
            });

        let mul_expr = unary
            .clone()
            .then(
                just(TokenKind::Star)
                    .to(ExprBinaryOp::Mul)
                    .then(unary)
                    .repeated()
                    .collect::<Vec<_>>(),
            )
            .map(|(lhs, chain)| {
                chain.into_iter().fold(lhs, |lhs, (op, rhs)| Expr::Binary {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                })
            });

        mul_expr
            .clone()
            .then(
                just(TokenKind::Plus)
                    .to(ExprBinaryOp::Add)
                    .or(just(TokenKind::Minus).to(ExprBinaryOp::Sub))
                    .then(mul_expr)
                    .repeated()
                    .collect::<Vec<_>>(),
            )
            .map(|(lhs, chain)| {
                chain.into_iter().fold(lhs, |lhs, (op, rhs)| Expr::Binary {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                })
            })
            .then(
                data_width_parser()
                    .map(ExprSuffix::TypedView)
                    .or(address_hint_parser().map(ExprSuffix::AddressHint))
                    .repeated()
                    .collect::<Vec<_>>(),
            )
            .try_map(|(expr, suffixes), span| {
                let mut expr = expr;
                let mut seen_address_hint = false;

                for suffix in suffixes {
                    match suffix {
                        ExprSuffix::TypedView(width) => {
                            if seen_address_hint {
                                return Err(Rich::custom(
                                    span,
                                    "':abs' must appear after any typed view suffix",
                                ));
                            }
                            expr = Expr::TypedView {
                                expr: Box::new(expr),
                                width,
                            };
                        }
                        ExprSuffix::AddressHint(hint) => {
                            if seen_address_hint {
                                return Err(Rich::custom(span, "duplicate ':abs' suffix"));
                            }
                            seen_address_hint = true;
                            expr = Expr::AddressHint {
                                expr: Box::new(expr),
                                hint,
                            };
                        }
                    }
                }

                Ok(expr)
            })
    })
    .boxed()
}

pub(super) fn apply_eval_suffixes(mut base: Expr, suffixes: Vec<String>) -> Result<Expr, String> {
    for suffix in suffixes {
        let trimmed = suffix.trim();
        if trimmed.is_empty() {
            return Err("index expression cannot be empty".to_string());
        }

        if let Some(field_name) = parse_symbolic_field_subscript(trimmed) {
            let base_name = match base {
                Expr::Ident(base_name) => base_name,
                Expr::IdentSpanned { name, .. } => name,
                _ => {
                    return Err("symbolic field indexing requires an identifier base".to_string());
                }
            };
            base = Expr::Ident(format!("{base_name}.{field_name}"));
            continue;
        }

        let index_expr = parse_expression_fragment(SourceId(0), trimmed)
            .map(|expr| expr.node)
            .unwrap_or_else(|_| parse_eval_expr_token(trimmed));
        base = Expr::Index {
            base: Box::new(base),
            index: Box::new(index_expr),
        };
    }

    Ok(base)
}

pub(super) fn parse_symbolic_field_subscript(text: &str) -> Option<&str> {
    let field = text.strip_prefix('.')?;
    if field.is_empty() {
        return None;
    }

    let mut chars = field.chars();
    let first = chars.next()?;
    if !(first.is_ascii_alphabetic() || first == '_') {
        return None;
    }
    if chars.all(|ch| ch.is_ascii_alphanumeric() || ch == '_') {
        Some(field)
    } else {
        None
    }
}

pub(super) fn expression_fragment_parser<'src, I>()
-> impl chumsky::Parser<'src, I, (Expr, SimpleSpan), ParseExtra<'src>>
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    let expr = expr_parser().map_with(|expr, extra| (expr, extra.span()));

    line_sep_parser()
        .repeated()
        .ignore_then(expr)
        .then_ignore(line_sep_parser().repeated())
        .then_ignore(end())
}

pub(super) fn parse_eval_expr_token(value: &str) -> Expr {
    let trimmed = value.trim();
    if trimmed.is_empty() {
        return Expr::Number(0, NumFmt::Dec);
    }

    if is_ident_text(trimmed) {
        return Expr::Unary {
            op: ExprUnaryOp::EvalBracketed,
            expr: Box::new(Expr::Ident(trimmed.to_string())),
        };
    }

    Expr::EvalText(trimmed.to_string())
}

pub(super) fn parse_eval_expr_token_with_token_bounds(
    value: &str,
    token_start: usize,
    _token_end: usize,
) -> Expr {
    let trimmed = value.trim();
    if trimmed.is_empty() {
        return Expr::Number(0, NumFmt::Dec);
    }

    if is_ident_text(trimmed) {
        let leading_ws = value.len() - value.trim_start().len();
        let start = token_start + 1 + leading_ws;
        let end = start + trimmed.len();
        return Expr::Unary {
            op: ExprUnaryOp::EvalBracketed,
            expr: Box::new(Expr::IdentSpanned {
                name: trimmed.to_string(),
                start,
                end,
            }),
        };
    }

    parse_eval_expr_token(value)
}

pub(super) fn is_ident_text(text: &str) -> bool {
    let mut chars = text.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    if !(first.is_ascii_alphabetic() || first == '_' || first == '.') {
        return false;
    }
    chars.all(|ch| ch.is_ascii_alphanumeric() || ch == '_' || ch == '.')
}

pub(super) fn eval_static_expr(expr: &Expr) -> Option<i64> {
    match expr {
        Expr::Number(value, _) => Some(*value),
        Expr::Ident(_) => None,
        Expr::IdentSpanned { .. } => None,
        Expr::EvalText(value) => {
            if let Ok(expanded) = k816_eval::expand(value) {
                expanded.trim().parse::<i64>().ok()
            } else {
                None
            }
        }
        Expr::Index { base, index } => {
            let base = eval_static_expr(base)?;
            let index = eval_static_expr(index)?;
            base.checked_add(index)
        }
        Expr::Binary { op, lhs, rhs } => {
            let lhs = eval_static_expr(lhs)?;
            let rhs = eval_static_expr(rhs)?;
            match op {
                ExprBinaryOp::Add => lhs.checked_add(rhs),
                ExprBinaryOp::Sub => lhs.checked_sub(rhs),
                ExprBinaryOp::Mul => lhs.checked_mul(rhs),
            }
        }
        Expr::Unary { op, expr } => {
            let value = eval_static_expr(expr)?;
            match op {
                ExprUnaryOp::LowByte => Some(value & 0xFF),
                ExprUnaryOp::HighByte => Some((value >> 8) & 0xFF),
                ExprUnaryOp::WordLittleEndian | ExprUnaryOp::FarLittleEndian => Some(value),
                ExprUnaryOp::EvalBracketed => Some(value),
            }
        }
        Expr::TypedView { expr, .. } | Expr::AddressHint { expr, .. } => eval_static_expr(expr),
    }
}
