use crate::ast::{
    AddressHint, DataArg, DataBlock, DataEntry, DataForEvalRange, DataWidth, Expr, ExprUnaryOp,
    MetadataQuery, NumFmt, SegmentDecl,
};
use crate::lexer::{NumLit, TokenKind};
use crate::span::{SourceId, Span, Spanned};
use chumsky::{
    IterParser, Parser as _,
    error::Rich,
    input::ValueInput,
    prelude::{SimpleSpan, any, choice, just, skip_then_retry_until},
};
use std::collections::HashSet;
use std::sync::Arc;

use super::{
    ParseExtra, boundary_parser, eval_const_into, expr_parser, ident_parser, line_sep_parser,
    line_tail_parser, number_parser, parse_eval_expr_token, spanned, stmt_parser,
};

mod consts;
mod vars;

pub(super) use self::consts::const_decl_item_parser;
pub(super) use self::vars::{CommaTrailer, prefix_condition_parser, var_decl_parser};

pub(super) fn data_block_parser<'src, I>(
    source_id: SourceId,
) -> impl chumsky::Parser<'src, I, DataBlock, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    data_block_parser_inner(source_id, true)
}

/// Inline variant for use inside statement positions (e.g. inside a function
/// body). Identical to [`data_block_parser`] except `code { }` entries are
/// disallowed — they would create an infinite mutual recursion through
/// `stmt_parser` at parser construction time, and a code block inside a
/// statement-level data block is redundant since the surrounding context is
/// already executable.
pub(super) fn data_block_inline_parser<'src, I>(
    source_id: SourceId,
) -> impl chumsky::Parser<'src, I, DataBlock, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    data_block_parser_inner(source_id, false)
}

fn data_block_parser_inner<'src, I>(
    source_id: SourceId,
    allow_code: bool,
) -> impl chumsky::Parser<'src, I, DataBlock, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    let separators = line_sep_parser().repeated();
    let entry = spanned(data_entry_parser(source_id, allow_code), source_id);
    let recover_entry =
        entry.recover_with(skip_then_retry_until(any().ignored(), boundary_parser()));

    let optional_name = ident_parser()
        .map_with(|name, extra| (name, extra.span()))
        .or_not();

    optional_name
        .then(
            just(TokenKind::LBrace)
                .ignore_then(separators.clone())
                .ignore_then(
                    recover_entry
                        .then_ignore(separators.clone())
                        .repeated()
                        .collect::<Vec<_>>(),
                )
                .then_ignore(just(TokenKind::RBrace)),
        )
        .map(
            move |(name, entries): (Option<(String, SimpleSpan)>, Vec<Spanned<DataEntry>>)| {
                let (name, name_span) = match name {
                    Some((n, span)) => {
                        let range = span.into_range();
                        (Some(n), Some(Span::new(source_id, range.start, range.end)))
                    }
                    None => (None, None),
                };
                DataBlock {
                    name,
                    name_span,
                    entries,
                }
            },
        )
        .boxed()
}

fn data_entry_parser<'src, I>(
    source_id: SourceId,
    allow_code: bool,
) -> impl chumsky::Parser<'src, I, DataEntry, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    let segment_entry = just(TokenKind::Segment)
        .ignore_then(ident_parser())
        .map(|name| DataEntry::Segment(SegmentDecl { name }))
        .boxed();

    let ident_entry = ident_parser()
        .then(
            just(TokenKind::Colon)
                .to(true)
                .or(line_tail_parser().to(false)),
        )
        .validate(|(name, is_label), extra, emitter| {
            if is_label {
                DataEntry::Label(name)
            } else {
                emitter.emit(Rich::custom(extra.span(), format!("unexpected '{name}'")));
                DataEntry::Bytes(vec![])
            }
        })
        .boxed();

    let address_entry = just(TokenKind::Address)
        .ignore_then(expr_parser())
        .try_map(|value, span| {
            eval_const_into::<u32>(&value, span, "address value").map(DataEntry::Address)
        })
        .boxed();

    let align_entry = just(TokenKind::Align)
        .ignore_then(expr_parser())
        .try_map(|value, span| {
            eval_const_into::<u16>(&value, span, "align value").map(DataEntry::Align)
        })
        .boxed();

    let nocross_entry = just(TokenKind::Nocross)
        .ignore_then(expr_parser().or_not())
        .try_map(|value, span| match value {
            Some(value) => {
                eval_const_into::<u16>(&value, span, "nocross value").map(DataEntry::Nocross)
            }
            None => Ok(DataEntry::Nocross(256)),
        })
        .boxed();

    let string_entry = chumsky::select! { TokenKind::String(value) => value }
        .map(DataEntry::String)
        .boxed();

    let address_byte_expr = just(TokenKind::Amp)
        .ignore_then(choice((
            just(TokenKind::Lt).ignore_then(expr_parser()).map(|expr| {
                vec![Expr::Unary {
                    op: ExprUnaryOp::LowByte,
                    expr: Box::new(expr),
                }]
            }),
            just(TokenKind::Gt).ignore_then(expr_parser()).map(|expr| {
                vec![Expr::Unary {
                    op: ExprUnaryOp::HighByte,
                    expr: Box::new(expr),
                }]
            }),
            just(TokenKind::Amp).ignore_then(choice((
                just(TokenKind::Amp).ignore_then(expr_parser()).map(|expr| {
                    vec![Expr::Unary {
                        op: ExprUnaryOp::FarLittleEndian,
                        expr: Box::new(expr),
                    }]
                }),
                expr_parser().map(|expr| {
                    vec![Expr::Unary {
                        op: ExprUnaryOp::WordLittleEndian,
                        expr: Box::new(expr),
                    }]
                }),
            ))),
        )))
        .boxed();

    let undef_byte = just(TokenKind::Question).to(vec![Expr::Number(0, NumFmt::Dec)]);

    let far_value = choice((
        number_parser().map(|NumLit { value, fmt }| vec![Expr::Number(value, fmt)]),
        address_byte_expr.clone(),
        undef_byte.clone(),
        ident_parser().map(|name| vec![Expr::Ident(name)]),
    ));

    let far_entry = just(TokenKind::Far)
        .ignore_then(far_value.repeated().at_least(1).collect::<Vec<_>>())
        .map(|chunks| DataEntry::Fars(chunks.into_iter().flatten().collect::<Vec<_>>()))
        .boxed();

    let word_value = choice((
        number_parser().map(|NumLit { value, fmt }| vec![Expr::Number(value, fmt)]),
        address_byte_expr.clone(),
        undef_byte.clone(),
        ident_parser().map(|name| vec![Expr::Ident(name)]),
    ));

    let word_entry = chumsky::select! {
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("word") => ()
    }
    .ignore_then(word_value.repeated().at_least(1).collect::<Vec<_>>())
    .map(|chunks| DataEntry::Words(chunks.into_iter().flatten().collect::<Vec<_>>()))
    .boxed();

    let bytes_entry = choice((
        number_parser().map(|NumLit { value, fmt }| vec![Expr::Number(value, fmt)]),
        address_byte_expr,
        undef_byte,
    ))
    .repeated()
    .at_least(1)
    .collect::<Vec<_>>()
    .map(|chunks| DataEntry::Bytes(chunks.into_iter().flatten().collect::<Vec<_>>()))
    .boxed();

    let eval_bytes_entry =
        chumsky::select! { TokenKind::Eval(value) => parse_eval_expr_token(&value) }
            .repeated()
            .at_least(1)
            .collect::<Vec<_>>()
            .map(DataEntry::Bytes)
            .boxed();

    let for_eval_entry = chumsky::select! {
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("for") => ()
    }
    .ignore_then(ident_parser())
    .then_ignore(just(TokenKind::Eq))
    .then(expr_parser())
    .then_ignore(just(TokenKind::DotDot))
    .then(expr_parser())
    .then_ignore(chumsky::select! {
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("eval") => ()
    })
    .then(chumsky::select! { TokenKind::Eval(value) => value })
    .map(|(((iterator, start), end), eval)| {
        DataEntry::ForEvalRange(DataForEvalRange {
            iterator,
            start,
            end,
            eval,
        })
    })
    .boxed();

    let separators_data = line_sep_parser().repeated();

    let data_body = |src_id| {
        let inner_entry = spanned(data_entry_flat_parser(), src_id);
        let recover_inner =
            inner_entry.recover_with(skip_then_retry_until(any().ignored(), boundary_parser()));
        let seps = line_sep_parser().repeated();
        just(TokenKind::LBrace)
            .ignore_then(seps.clone())
            .ignore_then(
                recover_inner
                    .then_ignore(seps)
                    .repeated()
                    .collect::<Vec<_>>(),
            )
            .then_ignore(just(TokenKind::RBrace))
            .boxed()
    };

    let repeat_entry = chumsky::select! {
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("repeat") => ()
    }
    .ignore_then(expr_parser())
    .then(data_body(source_id))
    .try_map(|(count_expr, body), span| {
        let count = eval_const_into::<u16>(&count_expr, span, "repeat count")?;
        Ok(DataEntry::Repeat { count, body })
    })
    .boxed();

    let code_entry = if allow_code {
        let stmt = spanned(stmt_parser(source_id, Arc::new(HashSet::new())), source_id);
        let recover_stmt =
            stmt.recover_with(skip_then_retry_until(any().ignored(), boundary_parser()));
        let code_body = just(TokenKind::LBrace)
            .ignore_then(separators_data.clone())
            .ignore_then(
                recover_stmt
                    .then_ignore(separators_data.clone())
                    .repeated()
                    .collect::<Vec<_>>(),
            )
            .then_ignore(just(TokenKind::RBrace))
            .boxed();

        just(TokenKind::Nocross)
            .or_not()
            .ignore_then(chumsky::select! {
                TokenKind::Ident(value) if value.eq_ignore_ascii_case("code") => ()
            })
            .ignore_then(code_body)
            .map(DataEntry::Code)
            .boxed()
    } else {
        // Inline data blocks (inside function bodies) cannot contain `code { }`
        // entries — the surrounding scope is already executable, and allowing
        // them would create a parser-construction cycle through stmt_parser.
        // Emit a never-matching parser placeholder so the `choice` tuple stays
        // shape-compatible with the item-level form.
        any().filter(|_| false)
            .map(|_| DataEntry::Code(Vec::new()))
            .boxed()
    };

    let evaluator_entry = chumsky::select! {
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("evaluator") => ()
    }
    .ignore_then(chumsky::select! { TokenKind::Eval(value) => value })
    .map(DataEntry::Evaluator)
    .boxed();

    let charset_entry = chumsky::select! {
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("charset") => ()
    }
    .ignore_then(chumsky::select! { TokenKind::String(value) => value })
    .map(DataEntry::Charset)
    .boxed();

    let convert_args = data_arg_parser()
        .separated_by(just(TokenKind::Comma))
        .collect::<Vec<_>>()
        .or_not()
        .map(|args: Option<Vec<DataArg>>| args.unwrap_or_default());
    let convert_entry = ident_parser()
        .then(
            just(TokenKind::LParen)
                .ignore_then(convert_args)
                .then_ignore(just(TokenKind::RParen)),
        )
        .map(|(kind, args)| DataEntry::Convert { kind, args })
        .boxed();

    choice((
        string_entry,
        segment_entry,
        address_entry,
        align_entry,
        nocross_entry,
        for_eval_entry,
        repeat_entry,
        code_entry,
        evaluator_entry,
        charset_entry,
        far_entry,
        word_entry,
        convert_entry,
        bytes_entry,
        eval_bytes_entry,
        ident_entry,
    ))
    .boxed()
}

fn data_entry_flat_parser<'src, I>()
-> impl chumsky::Parser<'src, I, DataEntry, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    let segment_entry = just(TokenKind::Segment)
        .ignore_then(ident_parser())
        .map(|name| DataEntry::Segment(SegmentDecl { name }))
        .boxed();

    let ident_entry = ident_parser()
        .then(
            just(TokenKind::Colon)
                .to(true)
                .or(line_tail_parser().to(false)),
        )
        .validate(|(name, is_label), extra, emitter| {
            if is_label {
                DataEntry::Label(name)
            } else {
                emitter.emit(Rich::custom(extra.span(), format!("unexpected '{name}'")));
                DataEntry::Bytes(vec![])
            }
        })
        .boxed();

    let address_entry = just(TokenKind::Address)
        .ignore_then(expr_parser())
        .try_map(|value, span| {
            eval_const_into::<u32>(&value, span, "address value").map(DataEntry::Address)
        })
        .boxed();

    let align_entry = just(TokenKind::Align)
        .ignore_then(expr_parser())
        .try_map(|value, span| {
            eval_const_into::<u16>(&value, span, "align value").map(DataEntry::Align)
        })
        .boxed();

    let nocross_entry = just(TokenKind::Nocross)
        .ignore_then(expr_parser().or_not())
        .try_map(|value, span| match value {
            Some(value) => {
                eval_const_into::<u16>(&value, span, "nocross value").map(DataEntry::Nocross)
            }
            None => Ok(DataEntry::Nocross(256)),
        })
        .boxed();

    let string_entry = chumsky::select! { TokenKind::String(value) => value }
        .map(DataEntry::String)
        .boxed();

    let address_byte_expr = just(TokenKind::Amp)
        .ignore_then(choice((
            just(TokenKind::Lt).ignore_then(expr_parser()).map(|expr| {
                vec![Expr::Unary {
                    op: ExprUnaryOp::LowByte,
                    expr: Box::new(expr),
                }]
            }),
            just(TokenKind::Gt).ignore_then(expr_parser()).map(|expr| {
                vec![Expr::Unary {
                    op: ExprUnaryOp::HighByte,
                    expr: Box::new(expr),
                }]
            }),
            just(TokenKind::Amp).ignore_then(choice((
                just(TokenKind::Amp).ignore_then(expr_parser()).map(|expr| {
                    vec![Expr::Unary {
                        op: ExprUnaryOp::FarLittleEndian,
                        expr: Box::new(expr),
                    }]
                }),
                expr_parser().map(|expr| {
                    vec![Expr::Unary {
                        op: ExprUnaryOp::WordLittleEndian,
                        expr: Box::new(expr),
                    }]
                }),
            ))),
        )))
        .boxed();

    let undef_byte = just(TokenKind::Question).to(vec![Expr::Number(0, NumFmt::Dec)]);

    let far_value = choice((
        number_parser().map(|NumLit { value, fmt }| vec![Expr::Number(value, fmt)]),
        address_byte_expr.clone(),
        undef_byte.clone(),
        ident_parser().map(|name| vec![Expr::Ident(name)]),
    ));

    let far_entry = just(TokenKind::Far)
        .ignore_then(far_value.repeated().at_least(1).collect::<Vec<_>>())
        .map(|chunks| DataEntry::Fars(chunks.into_iter().flatten().collect::<Vec<_>>()))
        .boxed();

    let word_value = choice((
        number_parser().map(|NumLit { value, fmt }| vec![Expr::Number(value, fmt)]),
        address_byte_expr.clone(),
        undef_byte.clone(),
        ident_parser().map(|name| vec![Expr::Ident(name)]),
    ));

    let word_entry = chumsky::select! {
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("word") => ()
    }
    .ignore_then(word_value.repeated().at_least(1).collect::<Vec<_>>())
    .map(|chunks| DataEntry::Words(chunks.into_iter().flatten().collect::<Vec<_>>()))
    .boxed();

    let bytes_entry = choice((
        number_parser().map(|NumLit { value, fmt }| vec![Expr::Number(value, fmt)]),
        address_byte_expr,
        undef_byte,
    ))
    .repeated()
    .at_least(1)
    .collect::<Vec<_>>()
    .map(|chunks| DataEntry::Bytes(chunks.into_iter().flatten().collect::<Vec<_>>()))
    .boxed();

    let eval_bytes_entry =
        chumsky::select! { TokenKind::Eval(value) => parse_eval_expr_token(&value) }
            .repeated()
            .at_least(1)
            .collect::<Vec<_>>()
            .map(DataEntry::Bytes)
            .boxed();

    let charset_entry = chumsky::select! {
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("charset") => ()
    }
    .ignore_then(chumsky::select! { TokenKind::String(value) => value })
    .map(DataEntry::Charset)
    .boxed();

    let convert_args = data_arg_parser()
        .separated_by(just(TokenKind::Comma))
        .collect::<Vec<_>>()
        .or_not()
        .map(|args: Option<Vec<DataArg>>| args.unwrap_or_default());
    let convert_entry = ident_parser()
        .then(
            just(TokenKind::LParen)
                .ignore_then(convert_args)
                .then_ignore(just(TokenKind::RParen)),
        )
        .map(|(kind, args)| DataEntry::Convert { kind, args })
        .boxed();

    choice((
        string_entry,
        segment_entry,
        address_entry,
        align_entry,
        nocross_entry,
        charset_entry,
        far_entry,
        word_entry,
        convert_entry,
        bytes_entry,
        eval_bytes_entry,
        ident_entry,
    ))
    .boxed()
}

fn data_arg_parser<'src, I>() -> impl chumsky::Parser<'src, I, DataArg, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    chumsky::select! {
        TokenKind::Number(NumLit { value, .. }) => DataArg::Int(value),
        TokenKind::String(value) => DataArg::Str(value),
    }
    .boxed()
}

pub(super) fn data_width_parser<'src, I>()
-> impl chumsky::Parser<'src, I, DataWidth, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    just(TokenKind::Colon)
        .ignore_then(chumsky::select! {
            TokenKind::Ident(value) if value.eq_ignore_ascii_case("byte") => DataWidth::Byte,
            TokenKind::Ident(value) if value.eq_ignore_ascii_case("word") => DataWidth::Word,
            TokenKind::Far => DataWidth::Far,
        })
        .boxed()
}

pub(super) fn address_hint_parser<'src, I>()
-> impl chumsky::Parser<'src, I, AddressHint, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    just(TokenKind::Colon)
        .ignore_then(chumsky::select! {
            TokenKind::Ident(value) if value.eq_ignore_ascii_case("abs") => AddressHint::ForceAbsolute16,
        })
        .boxed()
}

pub(super) fn metadata_query_parser<'src, I>()
-> impl chumsky::Parser<'src, I, MetadataQuery, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    just(TokenKind::Colon)
        .ignore_then(chumsky::select! {
            TokenKind::Ident(value) if value.eq_ignore_ascii_case("sizeof") => MetadataQuery::SizeOf,
            TokenKind::Ident(value) if value.eq_ignore_ascii_case("offsetof") => MetadataQuery::OffsetOf,
        })
        .boxed()
}
