use crate::ast::{
    BlockKind, CallStmt, CodeBlock, DataArg, DataBlock, DataCommand, Expr, File, HlaCompareOp,
    HlaCondition, HlaRegister, HlaRhs, HlaStmt, IndexRegister, Instruction, Item, NamedDataBlock,
    NamedDataEntry, Operand, SegmentDecl, Stmt, VarDecl, LabelDecl,
};
use crate::diag::Diagnostic;
use crate::lexer::{TokenKind, lex};
use crate::span::{SourceId, Span, Spanned};
use chumsky::{
    IterParser, Parser as _,
    error::{RichPattern, RichReason},
    extra,
    input::{Input as _, Stream, ValueInput},
    prelude::{Rich, SimpleSpan, any, end, just, skip_then_retry_until},
};

type ParseError<'src> = Rich<'src, TokenKind>;
type ParseExtra<'src> = extra::Err<ParseError<'src>>;

pub fn parse(source_id: SourceId, source_text: &str) -> Result<File, Vec<Diagnostic>> {
    let tokens = lex(source_id, source_text)?;
    let end_offset = tokens.last().map(|token| token.span.end).unwrap_or(0);
    let token_stream = Stream::from_iter(tokens.into_iter().map(|token| {
        let span = (token.span.start..token.span.end).into();
        (token.kind, span)
    }))
    .map((end_offset..end_offset).into(), |(kind, span): (_, _)| {
        (kind, span)
    });

    let (output, errors) = file_parser(source_id)
        .parse(token_stream)
        .into_output_errors();
    let diagnostics = errors
        .into_iter()
        .map(|error| rich_error_to_diagnostic(source_id, error, "invalid syntax"))
        .collect::<Vec<_>>();

    if diagnostics.is_empty() {
        Ok(output.unwrap_or_default())
    } else {
        Err(diagnostics)
    }
}

pub fn parse_expression_fragment(
    source_id: SourceId,
    source_text: &str,
) -> Result<Spanned<Expr>, Diagnostic> {
    let tokens = match lex(source_id, source_text) {
        Ok(tokens) => tokens,
        Err(mut diagnostics) => {
            return Err(diagnostics
                .pop()
                .expect("lexer should produce at least one diagnostic"));
        }
    };

    let end_offset = tokens.last().map(|token| token.span.end).unwrap_or(0);
    let token_stream = Stream::from_iter(tokens.into_iter().map(|token| {
        let span = (token.span.start..token.span.end).into();
        (token.kind, span)
    }))
    .map((end_offset..end_offset).into(), |(kind, span): (_, _)| {
        (kind, span)
    });

    expression_fragment_parser()
        .parse(token_stream)
        .into_result()
        .map(|(expr, span)| {
            let range = span.into_range();
            Spanned::new(expr, Span::new(source_id, range.start, range.end))
        })
        .map_err(|mut errors| {
            let error = errors
                .pop()
                .expect("chumsky should return at least one parse error");
            rich_error_to_diagnostic(source_id, error, "invalid expression fragment")
        })
}

fn file_parser<'src, I>(
    source_id: SourceId,
) -> impl chumsky::Parser<'src, I, File, ParseExtra<'src>>
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    let separators = just(TokenKind::Newline).repeated();
    let item = spanned(item_parser(source_id), source_id);
    let boundary = just(TokenKind::Newline)
        .ignored()
        .or(just(TokenKind::RBrace).ignored())
        .or(end().ignored());
    let recover_item = item.recover_with(skip_then_retry_until(any().ignored(), boundary));

    separators
        .clone()
        .ignore_then(
            recover_item
                .then_ignore(separators.clone())
                .repeated()
                .collect::<Vec<_>>(),
        )
        .then_ignore(end())
        .map(|items| File { items })
}

fn item_parser<'src, I>(
    source_id: SourceId,
) -> impl chumsky::Parser<'src, I, Item, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    let segment_item = just(TokenKind::Segment)
        .or(just(TokenKind::Bank))
        .ignore_then(ident_parser())
        .map(|name| Item::Segment(SegmentDecl { name }));

    let var_item = var_decl_parser(source_id).map(Item::Var);

    let data_item = just(TokenKind::Data).ignore_then(
        named_data_block_parser(source_id)
            .map(Item::NamedDataBlock)
            .or(data_block_parser(source_id).map(Item::DataBlock)),
    );

    let code_block_item = code_block_parser(source_id).map(Item::CodeBlock);

    let stmt_item = stmt_parser(source_id).map(Item::Statement);

    segment_item
        .or(var_item)
        .or(data_item)
        .or(code_block_item)
        .or(stmt_item)
        .boxed()
}

fn code_block_parser<'src, I>(
    source_id: SourceId,
) -> impl chumsky::Parser<'src, I, CodeBlock, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    #[derive(Clone, Copy)]
    enum Modifier {
        Far,
        Naked,
        Inline,
    }

    let modifier = just(TokenKind::Far)
        .to(Modifier::Far)
        .or(just(TokenKind::Naked).to(Modifier::Naked))
        .or(just(TokenKind::Inline).to(Modifier::Inline));

    let modifiers = modifier.repeated().collect::<Vec<_>>();
    let stmt = spanned(stmt_parser(source_id), source_id);
    let stmt_boundary = just(TokenKind::Newline)
        .ignored()
        .or(just(TokenKind::RBrace).ignored())
        .or(end().ignored());
    let recover_stmt = stmt.recover_with(skip_then_retry_until(any().ignored(), stmt_boundary));
    let separators = just(TokenKind::Newline).repeated();
    let body = just(TokenKind::LBrace)
        .ignore_then(
            separators
                .clone()
                .ignore_then(recover_stmt)
                .then_ignore(separators.clone())
                .repeated()
                .collect::<Vec<_>>(),
        )
        .then_ignore(just(TokenKind::RBrace));

    let main = just(TokenKind::Main)
        .ignore_then(body.clone())
        .map(|body| CodeBlock {
            name: "main".to_string(),
            name_span: None,
            kind: BlockKind::Main,
            is_far: false,
            is_naked: false,
            is_inline: false,
            body,
        });

    let func = just(TokenKind::Func)
        .ignore_then(ident_parser().map_with(|name, extra| (name, extra.span())))
        .then(body)
        .map(
            move |((name, name_span), body): ((String, SimpleSpan), Vec<Spanned<Stmt>>)| {
                let range = name_span.into_range();
                CodeBlock {
                    name,
                    name_span: Some(Span::new(source_id, range.start, range.end)),
                    kind: BlockKind::Func,
                    is_far: false,
                    is_naked: false,
                    is_inline: false,
                    body,
                }
            },
        );

    modifiers
        .then(main.or(func))
        .map(|(mods, mut block)| {
            for modifier in mods {
                match modifier {
                    Modifier::Far => block.is_far = true,
                    Modifier::Naked => block.is_naked = true,
                    Modifier::Inline => block.is_inline = true,
                }
            }
            block
        })
        .boxed()
}

fn data_block_parser<'src, I>(
    source_id: SourceId,
) -> impl chumsky::Parser<'src, I, DataBlock, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    let separators = just(TokenKind::Newline).repeated();
    let command = spanned(data_command_parser(), source_id);
    let command_boundary = just(TokenKind::Newline)
        .ignored()
        .or(just(TokenKind::RBrace).ignored())
        .or(end().ignored());
    let recover_command =
        command.recover_with(skip_then_retry_until(any().ignored(), command_boundary));

    just(TokenKind::LBrace)
        .ignore_then(
            separators
                .clone()
                .ignore_then(recover_command)
                .then_ignore(separators.clone())
                .repeated()
                .collect::<Vec<_>>(),
        )
        .then_ignore(just(TokenKind::RBrace))
        .map(|commands| DataBlock { commands })
        .boxed()
}

fn named_data_block_parser<'src, I>(
    source_id: SourceId,
) -> impl chumsky::Parser<'src, I, NamedDataBlock, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    let separators = just(TokenKind::Newline).repeated();
    let entry = spanned(named_data_entry_parser(source_id), source_id);
    let entry_boundary = just(TokenKind::Newline)
        .ignored()
        .or(just(TokenKind::RBrace).ignored())
        .or(end().ignored());
    let recover_entry = entry.recover_with(skip_then_retry_until(any().ignored(), entry_boundary));

    ident_parser()
        .map_with(|name, extra| (name, extra.span()))
        .then(
            just(TokenKind::LBrace)
                .ignore_then(
                    separators
                        .clone()
                        .ignore_then(recover_entry)
                        .then_ignore(separators.clone())
                        .repeated()
                        .collect::<Vec<_>>(),
                )
                .then_ignore(just(TokenKind::RBrace)),
        )
        .map(move |((name, name_span), entries): ((String, SimpleSpan), Vec<Spanned<NamedDataEntry>>)| {
            let range = name_span.into_range();
            NamedDataBlock {
                name,
                name_span: Span::new(source_id, range.start, range.end),
                entries,
            }
        })
        .boxed()
}

fn named_data_entry_parser<'src, I>(
    _source_id: SourceId,
) -> impl chumsky::Parser<'src, I, NamedDataEntry, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    let segment_entry = just(TokenKind::Segment)
        .or(just(TokenKind::Bank))
        .ignore_then(ident_parser())
        .map(|name| NamedDataEntry::Segment(SegmentDecl { name }));

    let address_entry = just(TokenKind::Address)
        .ignore_then(number_parser())
        .try_map(|value, span| {
            u32::try_from(value)
                .map(NamedDataEntry::Address)
                .map_err(|_| Rich::custom(span, "address value must fit in u32"))
        });

    let align_entry = just(TokenKind::Align)
        .ignore_then(number_parser())
        .try_map(|value, span| {
            u16::try_from(value)
                .map(NamedDataEntry::Align)
                .map_err(|_| Rich::custom(span, "align value must fit in u16"))
        });

    let nocross_entry = just(TokenKind::Nocross)
        .ignore_then(number_parser())
        .try_map(|value, span| {
            u16::try_from(value)
                .map(NamedDataEntry::Nocross)
                .map_err(|_| Rich::custom(span, "nocross value must fit in u16"))
        });

    let byte_entry = just(TokenKind::Ident(".byte".to_string()))
        .ignore_then(
            expr_parser()
                .separated_by(just(TokenKind::Comma))
                .at_least(1)
                .collect::<Vec<_>>(),
        )
        .map(NamedDataEntry::Bytes);

    let string_entry = chumsky::select! { TokenKind::String(value) => value }
        .map(NamedDataEntry::String);

    let legacy_bytes_entry = number_parser()
        .repeated()
        .at_least(1)
        .collect::<Vec<_>>()
        .map(|values| {
            NamedDataEntry::LegacyBytes(values.into_iter().map(Expr::Number).collect::<Vec<_>>())
        });

    let args = data_arg_parser()
        .separated_by(just(TokenKind::Comma))
        .collect::<Vec<_>>()
        .or_not()
        .map(|args: Option<Vec<DataArg>>| args.unwrap_or_default());
    let convert_entry = ident_parser()
        .then(
            just(TokenKind::LParen)
                .ignore_then(args)
                .then_ignore(just(TokenKind::RParen)),
        )
        .map(|(kind, args)| NamedDataEntry::Convert { kind, args });

    string_entry
        .or(segment_entry)
        .or(address_entry)
        .or(align_entry)
        .or(nocross_entry)
        .or(byte_entry)
        .or(convert_entry)
        .or(legacy_bytes_entry)
        .boxed()
}

fn data_command_parser<'src, I>()
-> impl chumsky::Parser<'src, I, DataCommand, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    let align = just(TokenKind::Align)
        .ignore_then(number_parser())
        .try_map(|value, span| {
            u16::try_from(value)
                .map(DataCommand::Align)
                .map_err(|_| Rich::custom(span, "align value must fit in u16"))
        });

    let address = just(TokenKind::Address)
        .ignore_then(number_parser())
        .try_map(|value, span| {
            u32::try_from(value)
                .map(DataCommand::Address)
                .map_err(|_| Rich::custom(span, "address value must fit in u32"))
        });

    let nocross = just(TokenKind::Nocross)
        .ignore_then(number_parser())
        .try_map(|value, span| {
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

    align.or(address).or(nocross).or(convert).boxed()
}

fn data_arg_parser<'src, I>() -> impl chumsky::Parser<'src, I, DataArg, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    chumsky::select! {
        TokenKind::Number(value) => DataArg::Int(value),
        TokenKind::String(value) => DataArg::Str(value),
    }
    .boxed()
}

fn var_decl_parser<'src, I>(
    source_id: SourceId,
) -> impl chumsky::Parser<'src, I, VarDecl, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    just(TokenKind::Var)
        .ignore_then(ident_parser())
        .then(bracket_expr_parser(source_id).or_not())
        .then(just(TokenKind::Eq).ignore_then(expr_parser()).or_not())
        .map(|((name, array_len), initializer)| VarDecl {
            name,
            array_len,
            initializer,
        })
        .boxed()
}

fn bracket_expr_parser<'src, I>(
    source_id: SourceId,
) -> impl chumsky::Parser<'src, I, Expr, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    chumsky::select! { TokenKind::Eval(text) => text }
        .try_map_with(move |text, extra| {
            parse_expression_fragment(source_id, &text)
                .map(|expr| expr.node)
                .map_err(|error| {
                    Rich::custom(
                        extra.span(),
                        format!(
                            "invalid var array length expression: {}; inside brackets: [{}]",
                            error.message, text
                        ),
                    )
                })
        })
        .boxed()
}

fn stmt_parser<'src, I>(
    source_id: SourceId,
) -> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    let segment_stmt = just(TokenKind::Segment)
        .or(just(TokenKind::Bank))
        .ignore_then(ident_parser())
        .map(|name| Stmt::Segment(SegmentDecl { name }));

    let var_stmt = var_decl_parser(source_id).map(Stmt::Var);

    let data_stmt = just(TokenKind::Data)
        .ignore_then(data_block_parser(source_id))
        .map(Stmt::DataBlock);

    let address_stmt = just(TokenKind::Address)
        .ignore_then(number_parser())
        .try_map(|value, span| {
            u32::try_from(value)
                .map(Stmt::Address)
                .map_err(|_| Rich::custom(span, "address value must fit in u32"))
        });

    let align_stmt = just(TokenKind::Align)
        .ignore_then(number_parser())
        .try_map(|value, span| {
            u16::try_from(value)
                .map(Stmt::Align)
                .map_err(|_| Rich::custom(span, "align value must fit in u16"))
        });

    let nocross_stmt = just(TokenKind::Nocross)
        .ignore_then(number_parser())
        .try_map(|value, span| {
            u16::try_from(value)
                .map(Stmt::Nocross)
                .map_err(|_| Rich::custom(span, "nocross value must fit in u16"))
        });

    let call_stmt = just(TokenKind::Call)
        .ignore_then(ident_parser())
        .map(|target| Stmt::Call(CallStmt { target }));

    let label_stmt = ident_parser()
        .then_ignore(just(TokenKind::Colon))
        .map(|name| Stmt::Label(LabelDecl { name }));

    let byte_stmt = just(TokenKind::Ident(".byte".to_string()))
        .ignore_then(
            expr_parser()
                .separated_by(just(TokenKind::Comma))
                .at_least(1)
                .collect::<Vec<_>>(),
        )
        .map(Stmt::Bytes);

    let hla_wait_stmt = hla_wait_loop_stmt_parser();
    let hla_do_open_stmt = just(TokenKind::LBrace).to(Stmt::Hla(HlaStmt::DoOpen));
    let hla_do_close_stmt = just(TokenKind::RBrace).ignore_then(
        hla_condition_parser()
            .map(|condition| Stmt::Hla(HlaStmt::DoClose { condition }))
            .or(hla_compare_op_parser().map(|op| Stmt::Hla(HlaStmt::DoCloseWithOp { op }))),
    );
    let hla_condition_seed_stmt = hla_condition_seed_stmt_parser();
    let hla_x_assign_stmt = hla_x_assign_stmt_parser();
    let hla_x_increment_stmt = hla_x_increment_stmt_parser();
    let hla_store_from_a_stmt = hla_store_from_a_stmt_parser();

    let mnemonic = ident_parser().try_map(|mnemonic, span| {
        if mnemonic == ".byte" {
            Err(Rich::custom(
                span,
                "expected one or more expressions after '.byte'",
            ))
        } else {
            Ok(mnemonic)
        }
    });

    let operand_boundary = just(TokenKind::Newline)
        .ignored()
        .or(just(TokenKind::RBrace).ignored())
        .or(end().ignored())
        .rewind();

    let operand = operand_boundary
        .to(None)
        .or(just(TokenKind::Hash)
            .ignore_then(expr_parser())
            .map(|expr| Some(Operand::Immediate(expr))))
        .or(just(TokenKind::Far)
            .or_not()
            .then(
                expr_parser().then(
                    just(TokenKind::Comma)
                        .ignore_then(ident_parser())
                        .or_not(),
                ),
            )
            .try_map(|(force_far, (expr, index)), span| {
                let index = match index {
                    None => None,
                    Some(value) if value.eq_ignore_ascii_case("x") => Some(IndexRegister::X),
                    Some(value) => {
                        return Err(Rich::custom(
                            span,
                            format!("unsupported index register '{value}', expected 'x'"),
                        ));
                    }
                };
                Ok(Some(Operand::Value {
                    expr,
                    force_far: force_far.is_some(),
                    index,
                }))
            }));

    let instruction = mnemonic
        .then(operand)
        .map(|(mnemonic, operand)| Stmt::Instruction(Instruction { mnemonic, operand }));

    segment_stmt
        .or(var_stmt)
        .or(data_stmt)
        .or(address_stmt)
        .or(align_stmt)
        .or(nocross_stmt)
        .or(call_stmt)
        .or(label_stmt)
        .or(byte_stmt)
        .or(hla_wait_stmt)
        .or(hla_do_close_stmt)
        .or(hla_condition_seed_stmt)
        .or(hla_do_open_stmt)
        .or(hla_x_increment_stmt)
        .or(hla_x_assign_stmt)
        .or(hla_store_from_a_stmt)
        .or(instruction)
        .boxed()
}

fn hla_wait_loop_stmt_parser<'src, I>()
-> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    just(TokenKind::LBrace)
        .ignore_then(ident_parser())
        .then_ignore(just(TokenKind::Amp))
        .then_ignore(just(TokenKind::Question))
        .then(ident_parser())
        .then_ignore(just(TokenKind::RBrace))
        .then(ident_parser())
        .then_ignore(just(TokenKind::Minus))
        .then_ignore(just(TokenKind::Question))
        .try_map(|((lhs, symbol), n_reg), span| {
            if !lhs.eq_ignore_ascii_case("a") {
                return Err(Rich::custom(
                    span,
                    format!("expected 'a' in wait-loop, found '{lhs}'"),
                ));
            }
            if !n_reg.eq_ignore_ascii_case("n") {
                return Err(Rich::custom(
                    span,
                    format!("expected 'n' in wait-loop suffix, found '{n_reg}'"),
                ));
            }
            Ok(Stmt::Hla(HlaStmt::WaitLoopWhileNFlagClear { symbol }))
        })
}

fn hla_x_assign_stmt_parser<'src, I>() -> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    ident_parser()
        .then_ignore(just(TokenKind::Eq))
        .then_ignore(just(TokenKind::Hash))
        .then(expr_parser())
        .try_map(|(lhs, rhs), span| {
            if !lhs.eq_ignore_ascii_case("x") {
                return Err(Rich::custom(
                    span,
                    format!("expected 'x' assignment, found '{lhs}'"),
                ));
            }
            Ok(Stmt::Hla(HlaStmt::XAssignImmediate { rhs }))
        })
}

fn hla_x_increment_stmt_parser<'src, I>()
-> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    ident_parser()
        .then_ignore(just(TokenKind::PlusPlus))
        .try_map(|ident, span| {
            if !ident.eq_ignore_ascii_case("x") {
                return Err(Rich::custom(
                    span,
                    format!("expected 'x++', found '{ident}++'"),
                ));
            }
            Ok(Stmt::Hla(HlaStmt::XIncrement))
        })
}

fn hla_store_rhs_parser<'src, I>()
-> impl chumsky::Parser<'src, I, HlaRhs, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    let immediate = just(TokenKind::Hash)
        .ignore_then(expr_parser())
        .map(HlaRhs::Immediate);

    let value = expr_parser()
        .then(just(TokenKind::Comma).ignore_then(ident_parser()).or_not())
        .try_map(|(expr, index), span| {
            let index = match index {
                None => None,
                Some(value) if value.eq_ignore_ascii_case("x") => Some(IndexRegister::X),
                Some(value) => {
                    return Err(Rich::custom(
                        span,
                        format!("unsupported index register '{value}', expected 'x'"),
                    ));
                }
            };

            if index.is_none() && matches!(expr, Expr::EvalText(_)) {
                return Ok(HlaRhs::Immediate(expr));
            }

            Ok(HlaRhs::Value { expr, index })
        });

    immediate.or(value).boxed()
}

fn hla_store_from_a_stmt_parser<'src, I>()
-> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    ident_parser()
        .then_ignore(just(TokenKind::Eq))
        .then(ident_parser())
        .then_ignore(just(TokenKind::Eq))
        .then(hla_store_rhs_parser())
        .try_map(|((dest, middle), rhs), span| {
            if !middle.eq_ignore_ascii_case("a") {
                return Err(Rich::custom(
                    span,
                    format!("expected 'a' in store-from-a assignment, found '{middle}'"),
                ));
            }
            Ok(Stmt::Hla(HlaStmt::StoreFromA { dest, rhs }))
        })
}

fn hla_compare_op_parser<'src, I>()
-> impl chumsky::Parser<'src, I, HlaCompareOp, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    just(TokenKind::EqEq)
        .to(HlaCompareOp::Eq)
        .or(just(TokenKind::BangEq).to(HlaCompareOp::Ne))
        .or(just(TokenKind::LtEq).to(HlaCompareOp::Le))
        .or(just(TokenKind::GtEq).to(HlaCompareOp::Ge))
        .or(just(TokenKind::Lt).to(HlaCompareOp::Lt))
        .or(just(TokenKind::Gt).to(HlaCompareOp::Gt))
}

fn hla_condition_seed_stmt_parser<'src, I>()
-> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    ident_parser()
        .then_ignore(just(TokenKind::Question))
        .then(expr_parser())
        .try_map(|(ident, rhs), span| {
            if !ident.eq_ignore_ascii_case("a") {
                return Err(Rich::custom(
                    span,
                    format!("unsupported HLA condition lhs '{ident}', expected 'a'"),
                ));
            }
            Ok(Stmt::Hla(HlaStmt::ConditionSeed {
                lhs: HlaRegister::A,
                rhs,
            }))
        })
}

fn hla_condition_parser<'src, I>()
-> impl chumsky::Parser<'src, I, HlaCondition, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    let lhs = ident_parser().try_map(|ident, span| {
        if ident.eq_ignore_ascii_case("a") {
            Ok(HlaRegister::A)
        } else {
            Err(Rich::custom(
                span,
                format!("unsupported HLA condition lhs '{ident}', expected 'a'"),
            ))
        }
    });

    lhs.then_ignore(just(TokenKind::Question))
        .then(expr_parser().or_not())
        .then(hla_compare_op_parser())
        .map(|((lhs, rhs), op)| HlaCondition { lhs, op, rhs })
}

fn expr_parser<'src, I>() -> impl chumsky::Parser<'src, I, Expr, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    chumsky::select! {
        TokenKind::Number(value) => Expr::Number(value),
        TokenKind::Ident(value) => Expr::Ident(value),
        TokenKind::Eval(value) => Expr::EvalText(value),
    }
    .boxed()
}

fn expression_fragment_parser<'src, I>()
-> impl chumsky::Parser<'src, I, (Expr, SimpleSpan), ParseExtra<'src>>
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    let expr = expr_parser().map_with(|expr, extra| (expr, extra.span()));

    just(TokenKind::Newline)
        .repeated()
        .ignore_then(expr)
        .then_ignore(just(TokenKind::Newline).repeated())
        .then_ignore(end())
}

fn ident_parser<'src, I>() -> impl chumsky::Parser<'src, I, String, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    chumsky::select! { TokenKind::Ident(value) => value }.boxed()
}

fn number_parser<'src, I>() -> impl chumsky::Parser<'src, I, i64, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    chumsky::select! { TokenKind::Number(value) => value }.boxed()
}

fn spanned<'src, I, T, P>(
    parser: P,
    source_id: SourceId,
) -> impl chumsky::Parser<'src, I, Spanned<T>, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
    P: chumsky::Parser<'src, I, T, ParseExtra<'src>> + Clone,
{
    parser.map_with(move |node, extra| {
        let range = extra.span().into_range();
        Spanned::new(node, Span::new(source_id, range.start, range.end))
    })
}

fn rich_error_to_diagnostic(
    source_id: SourceId,
    error: Rich<'_, TokenKind>,
    context: &str,
) -> Diagnostic {
    let range = error.span().into_range();
    let span = Span::new(source_id, range.start, range.end);
    let message = match error.reason() {
        RichReason::Custom(custom) => format!("{context}: {custom}"),
        RichReason::ExpectedFound { expected, found } => {
            let expected = format_expected_patterns(expected);
            let found = found
                .as_deref()
                .map(token_kind_message)
                .unwrap_or_else(|| "end of input".to_string());
            format!("{context}: expected {expected}, found {found}")
        }
    };
    Diagnostic::error(span, message)
}

fn format_expected_patterns(expected: &[RichPattern<'_, TokenKind>]) -> String {
    let mut values = Vec::new();
    for pattern in expected {
        let text = rich_pattern_message(pattern);
        if !values.contains(&text) {
            values.push(text);
        }
    }

    match values.as_slice() {
        [] => "something else".to_string(),
        [single] => single.clone(),
        [a, b] => format!("{a} or {b}"),
        _ => {
            let head = values[..values.len() - 1].join(", ");
            let tail = values.last().expect("non-empty values");
            format!("{head}, or {tail}")
        }
    }
}

fn rich_pattern_message(pattern: &RichPattern<'_, TokenKind>) -> String {
    match pattern {
        RichPattern::Token(token) => token_kind_message(token),
        RichPattern::Label(label) => label.to_string(),
        RichPattern::Identifier(identifier) => format!("'{}'", identifier),
        RichPattern::Any => "any token".to_string(),
        RichPattern::SomethingElse => "something else".to_string(),
        RichPattern::EndOfInput => "end of input".to_string(),
        _ => "something else".to_string(),
    }
}

fn token_kind_message(token: &TokenKind) -> String {
    match token {
        TokenKind::Segment => "'segment'".to_string(),
        TokenKind::Bank => "'bank'".to_string(),
        TokenKind::Var => "'var'".to_string(),
        TokenKind::Func => "'func'".to_string(),
        TokenKind::Main => "'main'".to_string(),
        TokenKind::Naked => "'naked'".to_string(),
        TokenKind::Inline => "'inline'".to_string(),
        TokenKind::Far => "'far'".to_string(),
        TokenKind::Data => "'data'".to_string(),
        TokenKind::Align => "'align'".to_string(),
        TokenKind::Address => "'address'".to_string(),
        TokenKind::Nocross => "'nocross'".to_string(),
        TokenKind::Call => "'call'".to_string(),
        TokenKind::LBrace => "'{'".to_string(),
        TokenKind::RBrace => "'}'".to_string(),
        TokenKind::LParen => "'('".to_string(),
        TokenKind::RParen => "')'".to_string(),
        TokenKind::Comma => "','".to_string(),
        TokenKind::Colon => "':'".to_string(),
        TokenKind::PlusPlus => "'++'".to_string(),
        TokenKind::Plus => "'+'".to_string(),
        TokenKind::Minus => "'-'".to_string(),
        TokenKind::Amp => "'&'".to_string(),
        TokenKind::Question => "'?'".to_string(),
        TokenKind::EqEq => "'=='".to_string(),
        TokenKind::BangEq => "'!='".to_string(),
        TokenKind::LtEq => "'<='".to_string(),
        TokenKind::GtEq => "'>='".to_string(),
        TokenKind::Lt => "'<'".to_string(),
        TokenKind::Gt => "'>'".to_string(),
        TokenKind::Hash => "'#'".to_string(),
        TokenKind::Eq => "'='".to_string(),
        TokenKind::Newline => "newline".to_string(),
        TokenKind::Eval(_) => "eval fragment".to_string(),
        TokenKind::String(_) => "string literal".to_string(),
        TokenKind::Number(_) => "number literal".to_string(),
        TokenKind::Ident(value) => format!("identifier '{value}'"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_far_function_and_call() {
        let source = "far func target {\n nop\n}\nmain {\n call target\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        assert_eq!(file.items.len(), 2);
    }

    #[test]
    fn parses_segment_and_bank_alias() {
        let source = "segment code\nbank legacy\n";
        let file = parse(SourceId(0), source).expect("parse");
        assert_eq!(file.items.len(), 2);

        let Item::Segment(first) = &file.items[0].node else {
            panic!("expected segment item");
        };
        assert_eq!(first.name, "code");

        let Item::Segment(second) = &file.items[1].node else {
            panic!("expected segment item from bank alias");
        };
        assert_eq!(second.name, "legacy");
    }

    #[test]
    fn parses_expression_fragment() {
        let expr = parse_expression_fragment(SourceId(0), "0x10").expect("parse");
        assert!(matches!(expr.node, Expr::Number(16)));
    }

    #[test]
    fn parses_expression_fragment_with_newline_padding() {
        let expr = parse_expression_fragment(SourceId(0), "\n0x10\n").expect("parse");
        assert!(matches!(expr.node, Expr::Number(16)));
    }

    #[test]
    fn parses_var_array_length() {
        let source = "var tiles[16]\n";
        let file = parse(SourceId(0), source).expect("parse");
        assert_eq!(file.items.len(), 1);

        let Item::Var(var) = &file.items[0].node else {
            panic!("expected var item");
        };

        assert!(matches!(var.array_len, Some(Expr::Number(16))));
        assert!(var.initializer.is_none());
    }

    #[test]
    fn parses_byte_directive_values() {
        let source = "main {\n .byte 1, 0x02, symbol\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        assert_eq!(file.items.len(), 1);

        let Item::CodeBlock(block) = &file.items[0].node else {
            panic!("expected code block");
        };
        assert_eq!(block.body.len(), 1);

        let Stmt::Bytes(values) = &block.body[0].node else {
            panic!("expected .byte statement");
        };
        assert_eq!(
            values,
            &vec![
                Expr::Number(1),
                Expr::Number(2),
                Expr::Ident("symbol".to_string())
            ]
        );
    }

    #[test]
    fn parses_data_block_commands() {
        let source =
            "data {\n align 16\n address 0x1234\n nocross 0x100\n binary(\"tiles\", 3)\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        assert_eq!(file.items.len(), 1);

        let Item::DataBlock(block) = &file.items[0].node else {
            panic!("expected data block");
        };
        assert_eq!(block.commands.len(), 4);

        assert!(matches!(block.commands[0].node, DataCommand::Align(16)));
        assert!(matches!(
            block.commands[1].node,
            DataCommand::Address(0x1234)
        ));
        assert!(matches!(
            block.commands[2].node,
            DataCommand::Nocross(0x100)
        ));

        let DataCommand::Convert { kind, args } = &block.commands[3].node else {
            panic!("expected converter command");
        };
        assert_eq!(kind, "binary");
        assert_eq!(
            args,
            &vec![DataArg::Str("tiles".to_string()), DataArg::Int(3)]
        );
    }

    #[test]
    fn parses_address_statement_inside_code_block() {
        let source = "func f {\n address $4000\n nop\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        assert_eq!(file.items.len(), 1);

        let Item::CodeBlock(block) = &file.items[0].node else {
            panic!("expected code block");
        };
        assert_eq!(block.body.len(), 2);
        assert!(matches!(block.body[0].node, Stmt::Address(0x4000)));
    }

    #[test]
    fn parses_segment_statement_inside_code_block() {
        let source = "func f {\n segment fixed_hi\n nop\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        assert_eq!(file.items.len(), 1);

        let Item::CodeBlock(block) = &file.items[0].node else {
            panic!("expected code block");
        };
        assert_eq!(block.body.len(), 2);
        let Stmt::Segment(segment) = &block.body[0].node else {
            panic!("expected segment statement");
        };
        assert_eq!(segment.name, "fixed_hi");
    }

    #[test]
    fn parses_label_and_instruction_statements() {
        let source = "main {\n loop:\n nop\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        assert_eq!(file.items.len(), 1);

        let Item::CodeBlock(block) = &file.items[0].node else {
            panic!("expected code block");
        };
        assert_eq!(block.body.len(), 2);

        let Stmt::Label(label) = &block.body[0].node else {
            panic!("expected label");
        };
        assert_eq!(label.name, "loop");

        let Stmt::Instruction(instr) = &block.body[1].node else {
            panic!("expected instruction");
        };
        assert_eq!(instr.mnemonic, "nop");
        assert!(instr.operand.is_none());
    }

    #[test]
    fn parses_hla_statements_without_text_desugaring() {
        let source = "main {\n  x = #0\n  {\n    { a&?UART_READY } n-?\n    UART_DATA = a = text,x\n    x++\n  } a?0 !=\n  API_OP = a = [$FF]\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let Item::CodeBlock(block) = &file.items[0].node else {
            panic!("expected code block");
        };

        assert_eq!(block.body.len(), 7);
        assert!(matches!(
            block.body[0].node,
            Stmt::Hla(HlaStmt::XAssignImmediate { .. })
        ));
        assert!(matches!(block.body[1].node, Stmt::Hla(HlaStmt::DoOpen)));
        assert!(matches!(
            block.body[2].node,
            Stmt::Hla(HlaStmt::WaitLoopWhileNFlagClear { .. })
        ));
        assert!(matches!(
            block.body[3].node,
            Stmt::Hla(HlaStmt::StoreFromA { .. })
        ));
        assert!(matches!(block.body[4].node, Stmt::Hla(HlaStmt::XIncrement)));
        assert!(matches!(
            block.body[5].node,
            Stmt::Hla(HlaStmt::DoClose { .. })
        ));
        assert!(matches!(
            block.body[6].node,
            Stmt::Hla(HlaStmt::StoreFromA { .. })
        ));
    }

    #[test]
    fn parses_named_data_block_entries() {
        let source = "data text {\n  segment INFO\n  \"Hello\"\n  $0D $0A $00\n  .byte 1, 2, 3\n  bytes(4, 5)\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let Item::NamedDataBlock(block) = &file.items[0].node else {
            panic!("expected named data block");
        };
        assert_eq!(block.name, "text");
        assert_eq!(block.entries.len(), 5);
        assert!(matches!(block.entries[0].node, NamedDataEntry::Segment(_)));
        assert!(matches!(block.entries[1].node, NamedDataEntry::String(_)));
        assert!(matches!(block.entries[2].node, NamedDataEntry::LegacyBytes(_)));
        assert!(matches!(block.entries[3].node, NamedDataEntry::Bytes(_)));
        assert!(matches!(
            block.entries[4].node,
            NamedDataEntry::Convert { .. }
        ));
    }

    #[test]
    fn rejects_modifier_without_code_block() {
        let diagnostics = parse(SourceId(0), "far var x\n").expect_err("expected parse error");
        assert!(!diagnostics.is_empty());
    }

    #[test]
    fn collects_multiple_errors_across_items() {
        let source = "far var first\nnaked var second\n";
        let diagnostics = parse(SourceId(0), source).expect_err("expected parse errors");
        assert_eq!(diagnostics.len(), 2, "expected exactly two diagnostics");
        assert!(diagnostics[0].message.contains("found 'var'"));
        assert!(diagnostics[1].message.contains("found 'var'"));
        assert_eq!(diagnostics[0].primary.start, 4);
        assert_eq!(diagnostics[0].primary.end, 7);
        assert_eq!(diagnostics[1].primary.start, 20);
        assert_eq!(diagnostics[1].primary.end, 23);
    }

    #[test]
    fn recovers_at_block_boundary_and_continues_items() {
        let source = "main {\n call\n}\nfar var trailing\n";
        let diagnostics = parse(SourceId(0), source).expect_err("expected parse errors");
        assert_eq!(diagnostics.len(), 2, "expected exactly two diagnostics");
        assert!(diagnostics
            .iter()
            .any(|diag| diag.message == "invalid syntax: expected something else, found newline"));
    }

    #[test]
    fn parse_error_messages_are_human_readable() {
        let diagnostics = parse(SourceId(0), "main {\n call\n}\n").expect_err("expected errors");
        let message = &diagnostics[0].message;
        assert!(message.contains("expected"));
        assert!(!message.contains("TokenKind"));
        assert!(!message.contains("ExpectedFound"));
    }
}
