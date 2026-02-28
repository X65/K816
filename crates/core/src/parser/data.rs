use crate::ast::{
    AddressHint, DataArg, DataBlock, DataCommand, DataWidth, Expr, ExprUnaryOp, NamedDataBlock,
    NamedDataEntry, NamedDataForEvalRange, NumFmt, SegmentDecl,
};
use crate::lexer::{NumLit, TokenKind};
use crate::span::{SourceId, Span, Spanned};
use chumsky::{
    IterParser, Parser as _,
    error::Rich,
    input::ValueInput,
    prelude::{SimpleSpan, any, end, just, skip_then_retry_until},
};

use super::{
    ParseExtra, eval_static_expr, expr_parser, ident_parser, line_sep_parser, line_tail_parser,
    number_parser, parse_eval_expr_token, spanned, stmt_parser,
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
    let separators = line_sep_parser().repeated();
    let command = spanned(data_command_parser(), source_id);
    let command_boundary = line_sep_parser()
        .ignored()
        .or(just(TokenKind::RBrace).ignored())
        .or(end().ignored());
    let recover_command =
        command.recover_with(skip_then_retry_until(any().ignored(), command_boundary));

    just(TokenKind::LBrace)
        .ignore_then(separators.clone())
        .ignore_then(
            recover_command
                .then_ignore(separators.clone())
                .repeated()
                .collect::<Vec<_>>(),
        )
        .then_ignore(just(TokenKind::RBrace))
        .map(|commands| DataBlock { commands })
        .boxed()
}

pub(super) fn named_data_block_parser<'src, I>(
    source_id: SourceId,
) -> impl chumsky::Parser<'src, I, NamedDataBlock, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    let separators = line_sep_parser().repeated();
    let entry = spanned(named_data_entry_parser(source_id), source_id);
    let entry_boundary = line_sep_parser()
        .ignored()
        .or(just(TokenKind::RBrace).ignored())
        .or(end().ignored());
    let recover_entry = entry.recover_with(skip_then_retry_until(any().ignored(), entry_boundary));

    ident_parser()
        .map_with(|name, extra| (name, extra.span()))
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
            move |((name, name_span), entries): (
                (String, SimpleSpan),
                Vec<Spanned<NamedDataEntry>>,
            )| {
                let range = name_span.into_range();
                NamedDataBlock {
                    name,
                    name_span: Span::new(source_id, range.start, range.end),
                    entries,
                }
            },
        )
        .boxed()
}

fn named_data_entry_parser<'src, I>(
    source_id: SourceId,
) -> impl chumsky::Parser<'src, I, NamedDataEntry, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    let segment_entry = just(TokenKind::Segment)
        .ignore_then(ident_parser())
        .map(|name| NamedDataEntry::Segment(SegmentDecl { name }));

    let ident_entry = ident_parser()
        .then(
            just(TokenKind::Colon)
                .to(true)
                .or(line_tail_parser().to(false)),
        )
        .validate(|(name, is_label), extra, emitter| {
            if is_label {
                NamedDataEntry::Label(name)
            } else {
                emitter.emit(Rich::custom(extra.span(), format!("unexpected '{name}'")));
                NamedDataEntry::Bytes(vec![])
            }
        });

    let address_entry =
        just(TokenKind::Address)
            .ignore_then(expr_parser())
            .try_map(|value, span| {
                let value = eval_static_expr(&value).ok_or_else(|| {
                    Rich::custom(span, "address value must be a constant expression")
                })?;
                u32::try_from(value)
                    .map(NamedDataEntry::Address)
                    .map_err(|_| Rich::custom(span, "address value must fit in u32"))
            });

    let align_entry = just(TokenKind::Align)
        .ignore_then(expr_parser())
        .try_map(|value, span| {
            let value = eval_static_expr(&value)
                .ok_or_else(|| Rich::custom(span, "align value must be a constant expression"))?;
            u16::try_from(value)
                .map(NamedDataEntry::Align)
                .map_err(|_| Rich::custom(span, "align value must fit in u16"))
        });

    let nocross_entry = just(TokenKind::Nocross)
        .ignore_then(expr_parser().or_not())
        .try_map(|value, span| {
            let value = match value {
                Some(value) => eval_static_expr(&value).ok_or_else(|| {
                    Rich::custom(span, "nocross value must be a constant expression")
                })?,
                None => 256,
            };
            u16::try_from(value)
                .map(NamedDataEntry::Nocross)
                .map_err(|_| Rich::custom(span, "nocross value must fit in u16"))
        });

    let string_entry =
        chumsky::select! { TokenKind::String(value) => value }.map(NamedDataEntry::String);

    let address_byte_expr = just(TokenKind::Amp).ignore_then(
        just(TokenKind::Lt)
            .ignore_then(expr_parser())
            .map(|expr| {
                vec![Expr::Unary {
                    op: ExprUnaryOp::LowByte,
                    expr: Box::new(expr),
                }]
            })
            .or(just(TokenKind::Gt).ignore_then(expr_parser()).map(|expr| {
                vec![Expr::Unary {
                    op: ExprUnaryOp::HighByte,
                    expr: Box::new(expr),
                }]
            }))
            .or(just(TokenKind::Amp).ignore_then(
                just(TokenKind::Amp)
                    .ignore_then(expr_parser())
                    .map(|expr| {
                        vec![Expr::Unary {
                            op: ExprUnaryOp::FarLittleEndian,
                            expr: Box::new(expr),
                        }]
                    })
                    .or(expr_parser().map(|expr| {
                        vec![Expr::Unary {
                            op: ExprUnaryOp::WordLittleEndian,
                            expr: Box::new(expr),
                        }]
                    })),
            )),
    );

    let undef_byte = just(TokenKind::Question).to(vec![Expr::Number(0, NumFmt::Dec)]);

    let far_value = number_parser()
        .map(|NumLit { value, fmt }| vec![Expr::Number(value, fmt)])
        .or(address_byte_expr.clone())
        .or(undef_byte.clone())
        .or(ident_parser().map(|name| vec![Expr::Ident(name)]));

    let far_entry = just(TokenKind::Far)
        .ignore_then(far_value.repeated().at_least(1).collect::<Vec<_>>())
        .map(|chunks| NamedDataEntry::Fars(chunks.into_iter().flatten().collect::<Vec<_>>()));

    let word_value = number_parser()
        .map(|NumLit { value, fmt }| vec![Expr::Number(value, fmt)])
        .or(address_byte_expr.clone())
        .or(undef_byte.clone())
        .or(ident_parser().map(|name| vec![Expr::Ident(name)]));

    let word_entry = chumsky::select! {
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("word") => ()
    }
    .ignore_then(word_value.repeated().at_least(1).collect::<Vec<_>>())
    .map(|chunks| NamedDataEntry::Words(chunks.into_iter().flatten().collect::<Vec<_>>()));

    let bytes_entry = number_parser()
        .map(|NumLit { value, fmt }| vec![Expr::Number(value, fmt)])
        .or(address_byte_expr)
        .or(undef_byte)
        .repeated()
        .at_least(1)
        .collect::<Vec<_>>()
        .map(|chunks| NamedDataEntry::Bytes(chunks.into_iter().flatten().collect::<Vec<_>>()));

    let eval_bytes_entry =
        chumsky::select! { TokenKind::Eval(value) => parse_eval_expr_token(&value) }
            .repeated()
            .at_least(1)
            .collect::<Vec<_>>()
            .map(NamedDataEntry::Bytes);

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
        NamedDataEntry::ForEvalRange(NamedDataForEvalRange {
            iterator,
            start,
            end,
            eval,
        })
    });

    let separators_data = line_sep_parser().repeated();

    let data_body = |src_id| {
        let inner_entry = spanned(named_data_entry_flat_parser(), src_id);
        let inner_boundary = line_sep_parser()
            .ignored()
            .or(just(TokenKind::RBrace).ignored())
            .or(end().ignored());
        let recover_inner =
            inner_entry.recover_with(skip_then_retry_until(any().ignored(), inner_boundary));
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
    };

    let repeat_entry = chumsky::select! {
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("repeat") => ()
    }
    .ignore_then(expr_parser())
    .then(data_body(source_id))
    .try_map(|(count_expr, body), span| {
        let count = eval_static_expr(&count_expr)
            .ok_or_else(|| Rich::custom(span, "repeat count must be a constant expression"))?;
        let count =
            u16::try_from(count).map_err(|_| Rich::custom(span, "repeat count must fit in u16"))?;
        Ok(NamedDataEntry::Repeat { count, body })
    });

    let code_body = {
        let stmt = spanned(stmt_parser(source_id), source_id);
        let stmt_boundary = line_sep_parser()
            .ignored()
            .or(just(TokenKind::RBrace).ignored())
            .or(end().ignored());
        let recover_stmt = stmt.recover_with(skip_then_retry_until(any().ignored(), stmt_boundary));
        just(TokenKind::LBrace)
            .ignore_then(separators_data.clone())
            .ignore_then(
                recover_stmt
                    .then_ignore(separators_data.clone())
                    .repeated()
                    .collect::<Vec<_>>(),
            )
            .then_ignore(just(TokenKind::RBrace))
    };

    let code_entry = just(TokenKind::Nocross)
        .or_not()
        .ignore_then(chumsky::select! {
            TokenKind::Ident(value) if value.eq_ignore_ascii_case("code") => ()
        })
        .ignore_then(code_body)
        .map(NamedDataEntry::Code);

    let evaluator_entry = chumsky::select! {
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("evaluator") => ()
    }
    .ignore_then(chumsky::select! { TokenKind::Eval(value) => value })
    .map(NamedDataEntry::Evaluator);

    let charset_entry = chumsky::select! {
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("charset") => ()
    }
    .ignore_then(chumsky::select! { TokenKind::String(value) => value })
    .map(NamedDataEntry::Charset);

    string_entry
        .or(segment_entry)
        .or(address_entry)
        .or(align_entry)
        .or(nocross_entry)
        .or(for_eval_entry)
        .or(repeat_entry)
        .or(code_entry)
        .or(evaluator_entry)
        .or(charset_entry)
        .or(far_entry)
        .or(word_entry)
        .or(bytes_entry)
        .or(eval_bytes_entry)
        .or(ident_entry)
        .boxed()
}

fn named_data_entry_flat_parser<'src, I>()
-> impl chumsky::Parser<'src, I, NamedDataEntry, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    let segment_entry = just(TokenKind::Segment)
        .ignore_then(ident_parser())
        .map(|name| NamedDataEntry::Segment(SegmentDecl { name }));

    let ident_entry = ident_parser()
        .then(
            just(TokenKind::Colon)
                .to(true)
                .or(line_tail_parser().to(false)),
        )
        .validate(|(name, is_label), extra, emitter| {
            if is_label {
                NamedDataEntry::Label(name)
            } else {
                emitter.emit(Rich::custom(extra.span(), format!("unexpected '{name}'")));
                NamedDataEntry::Bytes(vec![])
            }
        });

    let address_entry =
        just(TokenKind::Address)
            .ignore_then(expr_parser())
            .try_map(|value, span| {
                let value = eval_static_expr(&value).ok_or_else(|| {
                    Rich::custom(span, "address value must be a constant expression")
                })?;
                u32::try_from(value)
                    .map(NamedDataEntry::Address)
                    .map_err(|_| Rich::custom(span, "address value must fit in u32"))
            });

    let align_entry = just(TokenKind::Align)
        .ignore_then(expr_parser())
        .try_map(|value, span| {
            let value = eval_static_expr(&value)
                .ok_or_else(|| Rich::custom(span, "align value must be a constant expression"))?;
            u16::try_from(value)
                .map(NamedDataEntry::Align)
                .map_err(|_| Rich::custom(span, "align value must fit in u16"))
        });

    let nocross_entry = just(TokenKind::Nocross)
        .ignore_then(expr_parser().or_not())
        .try_map(|value, span| {
            let value = match value {
                Some(value) => eval_static_expr(&value).ok_or_else(|| {
                    Rich::custom(span, "nocross value must be a constant expression")
                })?,
                None => 256,
            };
            u16::try_from(value)
                .map(NamedDataEntry::Nocross)
                .map_err(|_| Rich::custom(span, "nocross value must fit in u16"))
        });

    let string_entry =
        chumsky::select! { TokenKind::String(value) => value }.map(NamedDataEntry::String);

    let address_byte_expr = just(TokenKind::Amp).ignore_then(
        just(TokenKind::Lt)
            .ignore_then(expr_parser())
            .map(|expr| {
                vec![Expr::Unary {
                    op: ExprUnaryOp::LowByte,
                    expr: Box::new(expr),
                }]
            })
            .or(just(TokenKind::Gt).ignore_then(expr_parser()).map(|expr| {
                vec![Expr::Unary {
                    op: ExprUnaryOp::HighByte,
                    expr: Box::new(expr),
                }]
            }))
            .or(just(TokenKind::Amp).ignore_then(
                just(TokenKind::Amp)
                    .ignore_then(expr_parser())
                    .map(|expr| {
                        vec![Expr::Unary {
                            op: ExprUnaryOp::FarLittleEndian,
                            expr: Box::new(expr),
                        }]
                    })
                    .or(expr_parser().map(|expr| {
                        vec![Expr::Unary {
                            op: ExprUnaryOp::WordLittleEndian,
                            expr: Box::new(expr),
                        }]
                    })),
            )),
    );

    let undef_byte = just(TokenKind::Question).to(vec![Expr::Number(0, NumFmt::Dec)]);

    let far_value = number_parser()
        .map(|NumLit { value, fmt }| vec![Expr::Number(value, fmt)])
        .or(address_byte_expr.clone())
        .or(undef_byte.clone())
        .or(ident_parser().map(|name| vec![Expr::Ident(name)]));

    let far_entry = just(TokenKind::Far)
        .ignore_then(far_value.repeated().at_least(1).collect::<Vec<_>>())
        .map(|chunks| NamedDataEntry::Fars(chunks.into_iter().flatten().collect::<Vec<_>>()));

    let word_value = number_parser()
        .map(|NumLit { value, fmt }| vec![Expr::Number(value, fmt)])
        .or(address_byte_expr.clone())
        .or(undef_byte.clone())
        .or(ident_parser().map(|name| vec![Expr::Ident(name)]));

    let word_entry = chumsky::select! {
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("word") => ()
    }
    .ignore_then(word_value.repeated().at_least(1).collect::<Vec<_>>())
    .map(|chunks| NamedDataEntry::Words(chunks.into_iter().flatten().collect::<Vec<_>>()));

    let bytes_entry = number_parser()
        .map(|NumLit { value, fmt }| vec![Expr::Number(value, fmt)])
        .or(address_byte_expr)
        .or(undef_byte)
        .repeated()
        .at_least(1)
        .collect::<Vec<_>>()
        .map(|chunks| NamedDataEntry::Bytes(chunks.into_iter().flatten().collect::<Vec<_>>()));

    let eval_bytes_entry =
        chumsky::select! { TokenKind::Eval(value) => parse_eval_expr_token(&value) }
            .repeated()
            .at_least(1)
            .collect::<Vec<_>>()
            .map(NamedDataEntry::Bytes);

    let charset_entry = chumsky::select! {
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("charset") => ()
    }
    .ignore_then(chumsky::select! { TokenKind::String(value) => value })
    .map(NamedDataEntry::Charset);

    string_entry
        .or(segment_entry)
        .or(address_entry)
        .or(align_entry)
        .or(nocross_entry)
        .or(charset_entry)
        .or(far_entry)
        .or(word_entry)
        .or(bytes_entry)
        .or(eval_bytes_entry)
        .or(ident_entry)
        .boxed()
}

fn data_command_parser<'src, I>()
-> impl chumsky::Parser<'src, I, DataCommand, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    let align = just(TokenKind::Align)
        .ignore_then(expr_parser())
        .try_map(|value, span| {
            let value = eval_static_expr(&value)
                .ok_or_else(|| Rich::custom(span, "align value must be a constant expression"))?;
            u16::try_from(value)
                .map(DataCommand::Align)
                .map_err(|_| Rich::custom(span, "align value must fit in u16"))
        });

    let address = just(TokenKind::Address)
        .ignore_then(expr_parser())
        .try_map(|value, span| {
            let value = eval_static_expr(&value)
                .ok_or_else(|| Rich::custom(span, "address value must be a constant expression"))?;
            u32::try_from(value)
                .map(DataCommand::Address)
                .map_err(|_| Rich::custom(span, "address value must fit in u32"))
        });

    let nocross = just(TokenKind::Nocross)
        .ignore_then(expr_parser().or_not())
        .try_map(|value, span| {
            let value = match value {
                Some(value) => eval_static_expr(&value).ok_or_else(|| {
                    Rich::custom(span, "nocross value must be a constant expression")
                })?,
                None => 256,
            };
            u16::try_from(value)
                .map(DataCommand::Nocross)
                .map_err(|_| Rich::custom(span, "nocross value must fit in u16"))
        });

    let args = data_arg_parser()
        .separated_by(just(TokenKind::Comma))
        .collect::<Vec<_>>()
        .or_not()
        .map(|args: Option<Vec<DataArg>>| args.unwrap_or_default());

    let convert = ident_parser()
        .then(
            just(TokenKind::LParen)
                .ignore_then(args)
                .then_ignore(just(TokenKind::RParen)),
        )
        .map(|(kind, args)| DataCommand::Convert { kind, args });

    let bytes = number_parser()
        .map(|n| n.value)
        .or(just(TokenKind::Question).to(0i64))
        .repeated()
        .at_least(1)
        .collect::<Vec<_>>()
        .map(DataCommand::Bytes);

    align.or(address).or(nocross).or(convert).or(bytes).boxed()
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
    just(TokenKind::Colon).ignore_then(chumsky::select! {
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("byte") => DataWidth::Byte,
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("word") => DataWidth::Word,
        TokenKind::Far => DataWidth::Far,
    })
}

pub(super) fn address_hint_parser<'src, I>()
-> impl chumsky::Parser<'src, I, AddressHint, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    just(TokenKind::Colon).ignore_then(chumsky::select! {
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("abs") => AddressHint::ForceAbsolute16,
    })
}
