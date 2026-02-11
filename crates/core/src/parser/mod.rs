use crate::ast::{
    BlockKind, CallStmt, CodeBlock, DataArg, DataBlock, DataCommand, Expr, ExprBinaryOp,
    ExprUnaryOp, File, HlaCompareOp, HlaCondition, HlaRegister, HlaRhs, HlaStmt, IndexRegister,
    Instruction, Item, LabelDecl, NamedDataBlock, NamedDataEntry, Operand, OperandAddrMode,
    SegmentDecl, Stmt, VarDecl,
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
    recursive::recursive,
};

type ParseError<'src> = Rich<'src, TokenKind>;
type ParseExtra<'src> = extra::Err<ParseError<'src>>;

#[derive(Debug, Clone)]
struct ParsedOperandExpr {
    expr: Expr,
    index: Option<IndexRegister>,
    addr_mode: OperandAddrMode,
}

#[derive(Debug, Clone)]
pub struct ParseOutput {
    pub file: File,
    pub warnings: Vec<Diagnostic>,
}

pub fn parse(source_id: SourceId, source_text: &str) -> Result<File, Vec<Diagnostic>> {
    parse_with_warnings(source_id, source_text).map(|parsed| parsed.file)
}

pub fn parse_with_warnings(
    source_id: SourceId,
    source_text: &str,
) -> Result<ParseOutput, Vec<Diagnostic>> {
    let preprocessed = preprocess_source(source_text);
    let source_text = preprocessed.as_str();
    let tokens = lex(source_id, source_text)?;
    let bank_spans = tokens
        .iter()
        .filter_map(|token| {
            if matches!(token.kind, TokenKind::Bank) {
                Some(token.span)
            } else {
                None
            }
        })
        .collect::<Vec<_>>();
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
        let file = output.unwrap_or_default();
        let warnings = collect_parser_warnings(&file, &bank_spans);
        Ok(ParseOutput { file, warnings })
    } else {
        Err(diagnostics)
    }
}

fn preprocess_source(source_text: &str) -> String {
    let mut out = Vec::new();
    let mut skipping_eval_block = false;
    let mut data_block_depth = 0usize;
    let mut skipped_nested_block_depth = 0usize;

    for raw_line in source_text.lines() {
        let mut line = raw_line.to_string();
        let mut trimmed = line.trim();

        if skipping_eval_block {
            if trimmed == "]" {
                skipping_eval_block = false;
            }
            continue;
        }

        if skipped_nested_block_depth > 0 {
            let opens = trimmed.chars().filter(|ch| *ch == '{').count();
            let closes = trimmed.chars().filter(|ch| *ch == '}').count();
            if opens >= closes {
                skipped_nested_block_depth += opens - closes;
            } else {
                skipped_nested_block_depth =
                    skipped_nested_block_depth.saturating_sub(closes - opens);
            }
            continue;
        }

        if trimmed == "[" {
            skipping_eval_block = true;
            continue;
        }

        if trimmed.starts_with('[') && trimmed.ends_with(']') {
            continue;
        }

        if trimmed.starts_with("nocross data {") {
            let indent = raw_line
                .chars()
                .take_while(|ch| ch.is_ascii_whitespace())
                .collect::<String>();
            line = format!("{indent}data {{");
            trimmed = line.trim();
        } else if data_block_depth == 0 && trimmed == "nocross {" {
            let indent = raw_line
                .chars()
                .take_while(|ch| ch.is_ascii_whitespace())
                .collect::<String>();
            line = format!("{indent}{{");
            trimmed = line.trim();
        }

        if data_block_depth > 0
            && (trimmed.starts_with("code {")
                || trimmed.starts_with("nocross code {")
                || trimmed.starts_with("repeat ") && trimmed.ends_with('{')
                || trimmed == "nocross {")
        {
            skipped_nested_block_depth = 1;
            continue;
        }

        if data_block_depth > 0 && trimmed == "?" {
            continue;
        }

        if trimmed.starts_with("var ") {
            let indent = raw_line
                .chars()
                .take_while(|ch| ch.is_ascii_whitespace())
                .collect::<String>();
            let mut payload = trimmed.trim_start_matches("var ").trim().to_string();
            if payload.ends_with('?') {
                payload.pop();
                payload = payload.trim_end().to_string();
            }
            if payload.contains(',') {
                for part in payload.split(',') {
                    let part = part.trim();
                    if !part.is_empty() {
                        out.push(format!("{indent}var {part}"));
                    }
                }
                continue;
            }
        }

        if trimmed.starts_with("inline ") && trimmed.ends_with('{') && !trimmed.contains("func") {
            let indent = raw_line
                .chars()
                .take_while(|ch| ch.is_ascii_whitespace())
                .collect::<String>();
            let rest = trimmed.trim_start_matches("inline ").trim();
            out.push(format!("{indent}inline func {rest}"));
            continue;
        }

        if trimmed.starts_with("naked ") && trimmed.ends_with('{') && !trimmed.contains("func") {
            let indent = raw_line
                .chars()
                .take_while(|ch| ch.is_ascii_whitespace())
                .collect::<String>();
            let rest = trimmed.trim_start_matches("naked ").trim();
            out.push(format!("{indent}naked func {rest}"));
            continue;
        }

        if trimmed.starts_with("data ") && trimmed.ends_with('{') {
            data_block_depth += 1;
        } else if data_block_depth > 0 && trimmed == "}" {
            data_block_depth -= 1;
        }

        out.push(line);
    }

    let mut text = out.join("\n");
    if source_text.ends_with('\n') {
        text.push('\n');
    }
    text
}

fn collect_parser_warnings(file: &File, bank_spans: &[Span]) -> Vec<Diagnostic> {
    let mut warnings = Vec::new();

    for span in bank_spans {
        warnings.push(
            Diagnostic::warning(*span, "`bank` keyword is deprecated; use `segment` instead")
                .with_help("replace `bank <name>` with `segment <name>`"),
        );
    }

    for item in &file.items {
        let Item::CodeBlock(block) = &item.node else {
            continue;
        };

        collect_hla_postfix_efficiency_warnings(block, &mut warnings);

        if block.kind != BlockKind::Func || !block.body.is_empty() {
            continue;
        }

        let span = block.name_span.unwrap_or(item.span);
        warnings.push(
            Diagnostic::warning(span, format!("empty function body '{}'", block.name))
                .with_help("add at least one statement or remove the function"),
        );
    }

    warnings
}

fn collect_hla_postfix_efficiency_warnings(block: &CodeBlock, warnings: &mut Vec<Diagnostic>) {
    for (index, stmt) in block.body.iter().enumerate() {
        let Stmt::Hla(HlaStmt::DoCloseWithOp { op }) = &stmt.node else {
            continue;
        };

        // `a?rhs` + `} OP` is normalized into compare-based close semantics.
        // Warn only for pure postfix closes that branch on existing flags.
        let has_condition_seed = index > 0
            && matches!(
                block.body[index - 1].node,
                Stmt::Hla(HlaStmt::ConditionSeed { .. })
            );
        if has_condition_seed {
            continue;
        }

        match op {
            HlaCompareOp::Le => warnings.push(
                Diagnostic::warning(
                    stmt.span,
                    "postfix `} <=` expands to two branch instructions and may be inefficient",
                )
                .with_help("expansion is `BCC target` + `BEQ target`"),
            ),
            HlaCompareOp::Gt => warnings.push(
                Diagnostic::warning(
                    stmt.span,
                    "postfix `} >` expands to three branch instructions and may be inefficient",
                )
                .with_help("expansion is `BEQ skip` + `BCC skip` + `BRA target`"),
            ),
            _ => {}
        }
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
    let separators = line_sep_parser().repeated();
    let item = spanned(item_parser(source_id), source_id);
    let boundary = line_sep_parser()
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

    let preproc_item = just(TokenKind::Hash)
        .then(line_tail_parser())
        .to(Item::Statement(Stmt::Empty));

    let eval_block_item =
        chumsky::select! { TokenKind::Eval(_) => () }.to(Item::Statement(Stmt::Empty));

    let compat_const_item =
        chumsky::select! { TokenKind::Ident(value) if value.eq_ignore_ascii_case("const") => () }
            .then(line_tail_parser())
            .to(Item::Statement(Stmt::Empty));

    let compat_evalfunc_item =
        chumsky::select! { TokenKind::Ident(value) if value.eq_ignore_ascii_case("evalfunc") => () }
            .then(line_tail_parser())
            .to(Item::Statement(Stmt::Empty));

    let image_binary_var_item = chumsky::select! {
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("image") || value.eq_ignore_ascii_case("binary") => ()
    }
    .ignore_then(
        ident_parser()
            .or(chumsky::select! { TokenKind::String(value) => value })
            .then_ignore(just(TokenKind::Eq))
            .then_ignore(chumsky::select! { TokenKind::String(_value) => () })
            .or_not()
            .then_ignore(line_tail_parser()),
    )
    .map(|name| {
        if let Some(name) = name {
            Item::Var(VarDecl {
                name,
                array_len: None,
                initializer: Some(Expr::Number(0)),
            })
        } else {
            Item::Statement(Stmt::Empty)
        }
    });

    preproc_item
        .or(eval_block_item)
        .or(compat_const_item)
        .or(compat_evalfunc_item)
        .or(image_binary_var_item)
        .or(segment_item)
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

    let modifiers = modifier.clone().repeated().collect::<Vec<_>>();
    let required_modifiers = modifier.repeated().at_least(1).collect::<Vec<_>>();
    let stmt = spanned(stmt_parser(source_id), source_id);
    let stmt_boundary = line_sep_parser()
        .ignored()
        .or(just(TokenKind::RBrace).ignored())
        .or(end().ignored());
    let recover_stmt = stmt.recover_with(skip_then_retry_until(any().ignored(), stmt_boundary));
    let separators = line_sep_parser().repeated();
    let body = just(TokenKind::LBrace)
        .ignore_then(separators.clone())
        .ignore_then(
            recover_stmt
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
        .then(body.clone())
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

    let explicit_block = modifiers.then(main.or(func)).map(|(mods, mut block)| {
        for modifier in mods {
            match modifier {
                Modifier::Far => block.is_far = true,
                Modifier::Naked => block.is_naked = true,
                Modifier::Inline => block.is_inline = true,
            }
        }
        block
    });

    let implicit_func = required_modifiers
        .then(ident_parser().map_with(|name, extra| (name, extra.span())))
        .then(body)
        .map(
            move |((mods, (name, name_span)), body): (
                (Vec<Modifier>, (String, SimpleSpan)),
                Vec<Spanned<Stmt>>,
            )| {
                let range = name_span.into_range();
                let mut block = CodeBlock {
                    name,
                    name_span: Some(Span::new(source_id, range.start, range.end)),
                    kind: BlockKind::Func,
                    is_far: false,
                    is_naked: false,
                    is_inline: false,
                    body,
                };
                for modifier in mods {
                    match modifier {
                        Modifier::Far => block.is_far = true,
                        Modifier::Naked => block.is_naked = true,
                        Modifier::Inline => block.is_inline = true,
                    }
                }
                block
            },
        );

    explicit_block.or(implicit_func).boxed()
}

fn data_block_parser<'src, I>(
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

fn named_data_block_parser<'src, I>(
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
    _source_id: SourceId,
) -> impl chumsky::Parser<'src, I, NamedDataEntry, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    let segment_entry = just(TokenKind::Segment)
        .or(just(TokenKind::Bank))
        .ignore_then(ident_parser())
        .map(|name| NamedDataEntry::Segment(SegmentDecl { name }));

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

    let byte_entry = just(TokenKind::Ident(".byte".to_string()))
        .ignore_then(
            expr_parser()
                .separated_by(just(TokenKind::Comma))
                .at_least(1)
                .collect::<Vec<_>>(),
        )
        .map(NamedDataEntry::Bytes);

    let string_entry = chumsky::select! { TokenKind::String(value) => value }
        .then(expr_parser().repeated().collect::<Vec<_>>())
        .map(|(value, extra)| {
            if extra.is_empty() {
                NamedDataEntry::String(value)
            } else {
                NamedDataEntry::Ignored
            }
        });

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
            .or(just(TokenKind::Amp)
                .ignore_then(ident_parser())
                .map(|name| {
                    vec![
                        Expr::Unary {
                            op: ExprUnaryOp::LowByte,
                            expr: Box::new(Expr::Ident(name.clone())),
                        },
                        Expr::Unary {
                            op: ExprUnaryOp::HighByte,
                            expr: Box::new(Expr::Ident(name)),
                        },
                    ]
                })),
    );

    let bytes_entry = number_parser()
        .map(|value| vec![Expr::Number(value)])
        .or(address_byte_expr)
        .repeated()
        .at_least(1)
        .collect::<Vec<_>>()
        .map(|chunks| {
            NamedDataEntry::Bytes(chunks.into_iter().flatten().collect::<Vec<_>>())
        });

    let eval_bytes_entry =
        chumsky::select! { TokenKind::Eval(value) => parse_eval_expr_token(&value) }
            .repeated()
            .at_least(1)
            .collect::<Vec<_>>()
            .map(NamedDataEntry::Bytes);

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

    let label_ignored_entry = ident_parser()
        .then_ignore(just(TokenKind::Colon))
        .to(NamedDataEntry::Ignored);

    let directive_ignored_entry = chumsky::select! {
        TokenKind::Ident(value) if is_discard_keyword(&value)
            || value.eq_ignore_ascii_case("image")
            || value.eq_ignore_ascii_case("binary")
            || value.eq_ignore_ascii_case("repeat") => ()
    }
    .then(line_tail_parser())
    .to(NamedDataEntry::Ignored);

    string_entry
        .or(segment_entry)
        .or(address_entry)
        .or(align_entry)
        .or(nocross_entry)
        .or(byte_entry)
        .or(convert_entry)
        .or(bytes_entry)
        .or(eval_bytes_entry)
        .or(label_ignored_entry)
        .or(directive_ignored_entry)
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
        .repeated()
        .at_least(1)
        .collect::<Vec<_>>()
        .map(|values| DataCommand::Convert {
            kind: "bytes".to_string(),
            args: values.into_iter().map(DataArg::Int).collect::<Vec<_>>(),
        });

    let directive_ignored = chumsky::select! {
        TokenKind::Ident(value) if is_discard_keyword(&value)
            || value.eq_ignore_ascii_case("image")
            || value.eq_ignore_ascii_case("binary")
            || value.eq_ignore_ascii_case("repeat") => ()
    }
    .then(line_tail_parser())
    .to(DataCommand::Ignored);

    let preproc_ignored = just(TokenKind::Hash)
        .then(line_tail_parser())
        .to(DataCommand::Ignored);

    align
        .or(address)
        .or(nocross)
        .or(convert)
        .or(bytes)
        .or(directive_ignored)
        .or(preproc_ignored)
        .boxed()
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

    let address_stmt =
        just(TokenKind::Address)
            .ignore_then(expr_parser())
            .try_map(|value, span| {
                let value = eval_static_expr(&value).ok_or_else(|| {
                    Rich::custom(span, "address value must be a constant expression")
                })?;
                u32::try_from(value)
                    .map(Stmt::Address)
                    .map_err(|_| Rich::custom(span, "address value must fit in u32"))
            });

    let align_stmt = just(TokenKind::Align)
        .ignore_then(expr_parser())
        .try_map(|value, span| {
            let value = eval_static_expr(&value)
                .ok_or_else(|| Rich::custom(span, "align value must be a constant expression"))?;
            u16::try_from(value)
                .map(Stmt::Align)
                .map_err(|_| Rich::custom(span, "align value must fit in u16"))
        });

    let nocross_stmt = just(TokenKind::Nocross)
        .ignore_then(expr_parser().or_not())
        .try_map(|value, span| {
            let value = match value {
                Some(value) => eval_static_expr(&value).ok_or_else(|| {
                    Rich::custom(span, "nocross value must be a constant expression")
                })?,
                None => 256,
            };
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
    let hla_do_close_suffix = hla_condition_parser()
        .map(|condition| Stmt::Hla(HlaStmt::DoClose { condition }))
        .or(hla_n_flag_close_stmt_parser())
        .or(hla_compare_op_parser().map(|op| Stmt::Hla(HlaStmt::DoCloseWithOp { op })))
        .or(
            chumsky::select! { TokenKind::Ident(value) if value.eq_ignore_ascii_case("always") => () }
                .to(Stmt::Hla(HlaStmt::DoCloseAlways)),
        )
        .or(
            chumsky::select! { TokenKind::Ident(value) if value.eq_ignore_ascii_case("never") => () }
                .to(Stmt::Hla(HlaStmt::DoCloseNever)),
        );
    let hla_do_close_stmt = just(TokenKind::RBrace)
        .then(hla_do_close_suffix.clone())
        .rewind()
        .ignore_then(just(TokenKind::RBrace).ignore_then(hla_do_close_suffix));
    let hla_condition_seed_stmt = hla_condition_seed_stmt_parser();
    let hla_x_assign_stmt = hla_x_assign_stmt_parser();
    let hla_x_increment_stmt = hla_x_increment_stmt_parser();
    let hla_store_from_a_stmt = hla_store_from_a_stmt_parser();
    let assign_stmt = assign_stmt_parser();
    let store_stmt = store_stmt_parser();
    let alu_stmt = alu_stmt_parser();
    let incdec_stmt = incdec_stmt_parser();
    let shift_stmt = shift_stmt_parser();
    let flag_stmt = flag_stmt_parser();
    let stack_stmt = stack_stmt_parser();
    let flow_stmt = flow_stmt_parser();
    let nop_stmt = nop_stmt_parser();
    let chain_stmt = chain_stmt_parser();
    let bare_rbrace_stmt = bare_rbrace_stmt_parser();
    let discard_stmt = discard_stmt_parser();

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

    let operand_boundary = line_sep_parser()
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
            .then(expr_parser().then(just(TokenKind::Comma).ignore_then(ident_parser()).or_not()))
            .try_map(|(force_far, (expr, index)), span| {
                let index = parse_index_register(index, span)?;
                Ok(Some(Operand::Value {
                    expr,
                    force_far: force_far.is_some(),
                    index,
                    addr_mode: OperandAddrMode::Direct,
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
        .or(bare_rbrace_stmt)
        .or(hla_condition_seed_stmt)
        .or(hla_do_open_stmt)
        .or(hla_x_increment_stmt)
        .or(hla_x_assign_stmt)
        .or(hla_store_from_a_stmt)
        .or(chain_stmt)
        .or(assign_stmt)
        .or(store_stmt)
        .or(alu_stmt)
        .or(incdec_stmt)
        .or(shift_stmt)
        .or(flag_stmt)
        .or(stack_stmt)
        .or(flow_stmt)
        .or(nop_stmt)
        .or(discard_stmt)
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

fn hla_x_assign_stmt_parser<'src, I>()
-> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
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

fn hla_store_rhs_parser<'src, I>() -> impl chumsky::Parser<'src, I, HlaRhs, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    let immediate = just(TokenKind::Hash)
        .ignore_then(expr_parser())
        .map(HlaRhs::Immediate);

    let value = operand_expr_parser().map(|parsed| {
        if parsed.addr_mode == OperandAddrMode::Direct
            && parsed.index.is_none()
            && !matches!(parsed.expr, Expr::Ident(_))
        {
            return HlaRhs::Immediate(parsed.expr);
        }

        HlaRhs::Value {
            expr: parsed.expr,
            index: parsed.index,
            addr_mode: parsed.addr_mode,
        }
        });

    immediate.or(value).boxed()
}

fn hla_store_from_a_stmt_parser<'src, I>()
-> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    chumsky::select! { TokenKind::Ident(value) if !is_register_name(&value) => value }
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

fn chain_stmt_parser<'src, I>()
-> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    ident_parser()
        .then_ignore(just(TokenKind::Eq))
        .then(ident_parser())
        .then_ignore(just(TokenKind::Eq))
        .then(line_tail_parser())
        .to(Stmt::Empty)
}

fn assign_stmt_parser<'src, I>()
-> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    chumsky::select! {
        TokenKind::Ident(value) if is_register_name(&value) => value
    }
    .then_ignore(just(TokenKind::Eq))
    .then(operand_expr_parser())
    .map(|(lhs, parsed)| {
        let lhs = lhs.to_ascii_lowercase();
        let rhs_ident = match &parsed.expr {
            Expr::Ident(value) => Some(value.to_ascii_lowercase()),
            _ => None,
        };

        let transfer = match (
            lhs.as_str(),
            rhs_ident.as_deref(),
            parsed.addr_mode,
            parsed.index,
        ) {
            ("x", Some("a"), OperandAddrMode::Direct, None) => Some("tax"),
            ("y", Some("a"), OperandAddrMode::Direct, None) => Some("tay"),
            ("a", Some("x"), OperandAddrMode::Direct, None) => Some("txa"),
            ("a", Some("y"), OperandAddrMode::Direct, None) => Some("tya"),
            ("x", Some("s"), OperandAddrMode::Direct, None) => Some("tsx"),
            ("s", Some("x"), OperandAddrMode::Direct, None) => Some("txs"),
            _ => None,
        };

        if let Some(mnemonic) = transfer {
            return instruction_stmt(mnemonic, None);
        }

        let mnemonic = match lhs.as_str() {
            "a" => "lda",
            "x" => "ldx",
            "y" => "ldy",
            _ => return Stmt::Empty,
        };

        let is_value = parsed.addr_mode != OperandAddrMode::Direct
            || parsed.index.is_some()
            || expr_is_address_like(&parsed.expr);
        let operand = if is_value {
            Operand::Value {
                expr: parsed.expr,
                force_far: false,
                index: parsed.index,
                addr_mode: parsed.addr_mode,
            }
        } else {
            Operand::Immediate(parsed.expr)
        };

        instruction_stmt(mnemonic, Some(operand))
    })
}

fn store_stmt_parser<'src, I>()
-> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    operand_expr_parser()
        .then_ignore(just(TokenKind::Eq))
        .then(chumsky::select! {
            TokenKind::Ident(value) if value.eq_ignore_ascii_case("a")
                || value.eq_ignore_ascii_case("x")
                || value.eq_ignore_ascii_case("y") => value
        })
        .map(|(dest, rhs)| {
            let mnemonic = if rhs.eq_ignore_ascii_case("a") {
                "sta"
            } else if rhs.eq_ignore_ascii_case("x") {
                "stx"
            } else {
                "sty"
            };

            instruction_stmt(
                mnemonic,
                Some(Operand::Value {
                    expr: dest.expr,
                    force_far: false,
                    index: dest.index,
                    addr_mode: dest.addr_mode,
                }),
            )
        })
}

fn operand_expr_parser<'src, I>()
-> impl chumsky::Parser<'src, I, ParsedOperandExpr, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    let index = just(TokenKind::Comma)
        .ignore_then(ident_parser())
        .or_not()
        .try_map(parse_index_register);

    let plain = expr_parser()
        .then(index.clone())
        .map(|(expr, index)| ParsedOperandExpr {
            expr,
            index,
            addr_mode: OperandAddrMode::Direct,
        });

    let parenthesized = just(TokenKind::LParen)
        .ignore_then(expr_parser().then(index.clone()))
        .then_ignore(just(TokenKind::RParen))
        .then(index.clone())
        .try_map(|((expr, inner_index), outer_index), span| {
            let (index, addr_mode) = match (inner_index, outer_index) {
                (None, None) => (None, OperandAddrMode::Indirect),
                (Some(IndexRegister::X), None) => (None, OperandAddrMode::IndexedIndirectX),
                (None, Some(IndexRegister::Y)) => (None, OperandAddrMode::IndirectIndexedY),
                (Some(IndexRegister::Y), None) => {
                    return Err(Rich::custom(
                        span,
                        "unsupported indirect index register 'y', expected '(expr,x)'",
                    ));
                }
                (None, Some(IndexRegister::X)) => {
                    return Err(Rich::custom(
                        span,
                        "unsupported post-indirect index register 'x', expected '(expr),y'",
                    ));
                }
                (Some(_), Some(_)) => {
                    return Err(Rich::custom(
                        span,
                        "invalid indirect operand: choose either '(expr,x)' or '(expr),y'",
                    ));
                }
            };

            Ok(ParsedOperandExpr {
                expr,
                index,
                addr_mode,
            })
        });

    parenthesized.or(plain)
}

fn alu_stmt_parser<'src, I>() -> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    let a_bit = chumsky::select! {
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("a") => ()
    }
    .then_ignore(just(TokenKind::Amp))
    .then_ignore(just(TokenKind::Question))
    .ignore_then(operand_expr_parser())
    .map(|parsed| {
        let operand = parsed_operand_to_operand(parsed);
        instruction_stmt("bit", Some(operand))
    });

    let a_alu = chumsky::select! {
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("a") => ()
    }
    .ignore_then(
        just(TokenKind::Plus)
            .to("adc")
            .or(just(TokenKind::Minus).to("sbc"))
            .or(just(TokenKind::Amp).to("and"))
            .or(just(TokenKind::Pipe).to("ora"))
            .or(just(TokenKind::Caret).to("eor")),
    )
    .then(operand_expr_parser())
    .map(|(mnemonic, parsed)| {
        let operand = parsed_operand_to_operand(parsed);
        instruction_stmt(mnemonic, Some(operand))
    });

    let xy_cmp = chumsky::select! {
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("x") || value.eq_ignore_ascii_case("y") => value
    }
    .then_ignore(just(TokenKind::Question))
    .then(operand_expr_parser())
    .map(|(lhs, parsed)| {
        let mnemonic = if lhs.eq_ignore_ascii_case("x") {
            "cpx"
        } else {
            "cpy"
        };
        let operand = parsed_operand_to_operand(parsed);
        instruction_stmt(mnemonic, Some(operand))
    });

    a_bit.or(a_alu).or(xy_cmp)
}

fn incdec_stmt_parser<'src, I>()
-> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    let indexed_inc = ident_parser()
        .then_ignore(just(TokenKind::Comma))
        .then(ident_parser())
        .then_ignore(just(TokenKind::PlusPlus))
        .try_map(|(base, idx), span| {
            let index = parse_index_register(Some(idx), span)?;
            Ok(instruction_stmt(
                "inc",
                Some(Operand::Value {
                    expr: Expr::Ident(base),
                    force_far: false,
                    index,
                    addr_mode: OperandAddrMode::Direct,
                }),
            ))
        });

    let indexed_dec = ident_parser()
        .then_ignore(just(TokenKind::Comma))
        .then(ident_parser())
        .then_ignore(just(TokenKind::Minus))
        .then_ignore(just(TokenKind::Minus))
        .try_map(|(base, idx), span| {
            let index = parse_index_register(Some(idx), span)?;
            Ok(instruction_stmt(
                "dec",
                Some(Operand::Value {
                    expr: Expr::Ident(base),
                    force_far: false,
                    index,
                    addr_mode: OperandAddrMode::Direct,
                }),
            ))
        });

    let inc = ident_parser()
        .then_ignore(just(TokenKind::PlusPlus))
        .map(|ident| {
            if ident.eq_ignore_ascii_case("x") {
                return instruction_stmt("inx", None);
            }
            if ident.eq_ignore_ascii_case("y") {
                return instruction_stmt("iny", None);
            }
            instruction_stmt(
                "inc",
                Some(Operand::Value {
                    expr: Expr::Ident(ident),
                    force_far: false,
                    index: None,
                    addr_mode: OperandAddrMode::Direct,
                }),
            )
        });

    let dec = ident_parser()
        .then_ignore(just(TokenKind::Minus))
        .then_ignore(just(TokenKind::Minus))
        .map(|ident| {
            if ident.eq_ignore_ascii_case("x") {
                return instruction_stmt("dex", None);
            }
            if ident.eq_ignore_ascii_case("y") {
                return instruction_stmt("dey", None);
            }
            instruction_stmt(
                "dec",
                Some(Operand::Value {
                    expr: Expr::Ident(ident),
                    force_far: false,
                    index: None,
                    addr_mode: OperandAddrMode::Direct,
                }),
            )
        });

    indexed_inc.or(indexed_dec).or(inc).or(dec)
}

fn shift_stmt_parser<'src, I>()
-> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    let shift_op = just(TokenKind::Lt)
        .then_ignore(just(TokenKind::Lt))
        .then_ignore(just(TokenKind::Lt))
        .to("rol")
        .or(just(TokenKind::Gt)
            .then_ignore(just(TokenKind::Gt))
            .then_ignore(just(TokenKind::Gt))
            .to("ror"))
        .or(just(TokenKind::Lt)
            .then_ignore(just(TokenKind::Lt))
            .to("asl"))
        .or(just(TokenKind::Gt)
            .then_ignore(just(TokenKind::Gt))
            .to("lsr"));

    let indexed = ident_parser()
        .then_ignore(just(TokenKind::Comma))
        .then(ident_parser())
        .then(shift_op.clone())
        .try_map(|((base, idx), mnemonic), span| {
            let index = parse_index_register(Some(idx), span)?;
            Ok(instruction_stmt(
                mnemonic,
                Some(Operand::Value {
                    expr: Expr::Ident(base),
                    force_far: false,
                    index,
                    addr_mode: OperandAddrMode::Direct,
                }),
            ))
        });

    let plain = ident_parser().then(shift_op).map(|(target, mnemonic)| {
        if target.eq_ignore_ascii_case("a") {
            instruction_stmt(mnemonic, None)
        } else {
            instruction_stmt(
                mnemonic,
                Some(Operand::Value {
                    expr: Expr::Ident(target),
                    force_far: false,
                    index: None,
                    addr_mode: OperandAddrMode::Direct,
                }),
            )
        }
    });

    indexed.or(plain)
}

fn flag_stmt_parser<'src, I>()
-> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    ident_parser()
        .then(
            just(TokenKind::Plus)
                .to(true)
                .or(just(TokenKind::Minus).to(false)),
        )
        .map(
            |(flag, set)| match (flag.to_ascii_lowercase().as_str(), set) {
                ("c", true) => instruction_stmt("sec", None),
                ("c", false) => instruction_stmt("clc", None),
                ("d", true) => instruction_stmt("sed", None),
                ("d", false) => instruction_stmt("cld", None),
                ("i", true) => instruction_stmt("sei", None),
                ("i", false) => instruction_stmt("cli", None),
                ("o", false) => instruction_stmt("clv", None),
                _ => Stmt::Empty,
            },
        )
}

fn stack_stmt_parser<'src, I>()
-> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    ident_parser()
        .then(
            just(TokenKind::Bang)
                .then_ignore(just(TokenKind::Bang))
                .to(true)
                .or(just(TokenKind::Question)
                    .then_ignore(just(TokenKind::Question))
                    .to(false)),
        )
        .map(|(target, push)| {
            if target.eq_ignore_ascii_case("a") {
                return if push {
                    instruction_stmt("pha", None)
                } else {
                    instruction_stmt("pla", None)
                };
            }
            if push {
                instruction_stmt("php", None)
            } else {
                instruction_stmt("plp", None)
            }
        })
}

fn flow_stmt_parser<'src, I>()
-> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    let goto_kw = chumsky::select! {
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("goto") => ()
    };

    let goto_stmt = goto_kw
        .ignore_then(
            just(TokenKind::LParen)
                .ignore_then(expr_parser())
                .then_ignore(just(TokenKind::RParen))
                .or(expr_parser()),
        )
        .map(|target| {
            instruction_stmt(
                "jmp",
                Some(Operand::Value {
                    expr: target,
                    force_far: false,
                    index: None,
                    addr_mode: OperandAddrMode::Direct,
                }),
            )
        });

    let branch_goto_stmt = just(TokenKind::Lt)
        .to("bcc")
        .or(just(TokenKind::GtEq).to("bcs"))
        .or(just(TokenKind::EqEq).to("beq"))
        .or(just(TokenKind::BangEq).to("bne"))
        .then_ignore(goto_kw.clone())
        .then(expr_parser())
        .map(|(mnemonic, target)| {
            instruction_stmt(
                mnemonic,
                Some(Operand::Value {
                    expr: target,
                    force_far: false,
                    index: None,
                    addr_mode: OperandAddrMode::Direct,
                }),
            )
        });

    let return_stmt = chumsky::select! {
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("return") || value.eq_ignore_ascii_case("return_i") => value
    }
    .map(|keyword| {
        if keyword.eq_ignore_ascii_case("return_i") {
            instruction_stmt("rti", None)
        } else {
            instruction_stmt("rts", None)
        }
    });

    let far_call_stmt = just(TokenKind::Far)
        .ignore_then(ident_parser())
        .map(|target| Stmt::Call(CallStmt { target }));

    let break_repeat_stmt = chumsky::select! {
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("break")
            || value.eq_ignore_ascii_case("repeat") => ()
    }
    .to(Stmt::Empty);

    goto_stmt
        .or(branch_goto_stmt)
        .or(return_stmt)
        .or(far_call_stmt)
        .or(break_repeat_stmt)
}

fn nop_stmt_parser<'src, I>() -> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    just(TokenKind::Star)
        .then(number_parser().or_not())
        .to(instruction_stmt("nop", None))
        .or(just(TokenKind::Percent).to(instruction_stmt("nop", None)))
}

fn discard_stmt_parser<'src, I>()
-> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    let else_clause = just(TokenKind::RBrace)
        .then(
            chumsky::select! { TokenKind::Ident(value) if value.eq_ignore_ascii_case("else") => () },
        )
        .then(line_tail_parser())
        .to(Stmt::Empty);

    let generic = just(TokenKind::Hash)
        .to(())
        .or(just(TokenKind::EqEq).to(()))
        .or(just(TokenKind::BangEq).to(()))
        .or(just(TokenKind::LtEq).to(()))
        .or(just(TokenKind::GtEq).to(()))
        .or(just(TokenKind::Lt).to(()))
        .or(just(TokenKind::Gt).to(()))
        .or(chumsky::select! {
            TokenKind::Ident(value) if is_discard_keyword(&value) => ()
        })
        .or(just(TokenKind::Question).to(()))
        .then(line_tail_parser())
        .to(Stmt::Empty);

    else_clause.or(generic)
}

fn bare_rbrace_stmt_parser<'src, I>()
-> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    just(TokenKind::RBrace)
        .then_ignore(line_sep_parser().repeated())
        .then(just(TokenKind::RBrace))
        .rewind()
        .ignore_then(just(TokenKind::RBrace))
        .to(Stmt::Empty)
}

fn instruction_stmt(mnemonic: &str, operand: Option<Operand>) -> Stmt {
    Stmt::Instruction(Instruction {
        mnemonic: mnemonic.to_string(),
        operand,
    })
}

fn parsed_operand_to_operand(parsed: ParsedOperandExpr) -> Operand {
    let is_value = parsed.addr_mode != OperandAddrMode::Direct
        || parsed.index.is_some()
        || expr_is_address_like(&parsed.expr);
    if is_value {
        Operand::Value {
            expr: parsed.expr,
            force_far: false,
            index: parsed.index,
            addr_mode: parsed.addr_mode,
        }
    } else {
        Operand::Immediate(parsed.expr)
    }
}

fn parse_index_register<'src>(
    index: Option<String>,
    span: SimpleSpan,
) -> Result<Option<IndexRegister>, Rich<'src, TokenKind>> {
    match index {
        None => Ok(None),
        Some(value) if value.eq_ignore_ascii_case("x") => Ok(Some(IndexRegister::X)),
        Some(value) if value.eq_ignore_ascii_case("y") => Ok(Some(IndexRegister::Y)),
        Some(value) => Err(Rich::custom(
            span,
            format!("unsupported index register '{value}', expected 'x' or 'y'"),
        )),
    }
}

fn is_register_name(value: &str) -> bool {
    value.eq_ignore_ascii_case("a")
        || value.eq_ignore_ascii_case("x")
        || value.eq_ignore_ascii_case("y")
        || value.eq_ignore_ascii_case("s")
}

fn looks_like_constant_ident(value: &str) -> bool {
    let mut has_alpha = false;
    for ch in value.chars() {
        if ch.is_ascii_alphabetic() {
            has_alpha = true;
            if !ch.is_ascii_uppercase() {
                return false;
            }
        } else if !(ch.is_ascii_digit() || ch == '_') {
            return false;
        }
    }
    has_alpha
}

fn expr_is_address_like(expr: &Expr) -> bool {
    match expr {
        Expr::Ident(name) => !looks_like_constant_ident(name) && !is_register_name(name),
        Expr::Binary { lhs, rhs, .. } => expr_is_address_like(lhs) || expr_is_address_like(rhs),
        Expr::Unary { .. } => false,
        Expr::Number(_) | Expr::EvalText(_) => false,
    }
}

fn is_discard_keyword(value: &str) -> bool {
    value.eq_ignore_ascii_case("else")
        || value.eq_ignore_ascii_case("always")
        || value.eq_ignore_ascii_case("never")
        || value.eq_ignore_ascii_case("evaluator")
        || value.eq_ignore_ascii_case("charset")
        || value.eq_ignore_ascii_case("for")
        || value.eq_ignore_ascii_case("code")
        || value.eq_ignore_ascii_case("tiles")
        || value.eq_ignore_ascii_case("colormode")
        || value.eq_ignore_ascii_case("imgwave")
        || value.eq_ignore_ascii_case("inv")
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

fn hla_n_flag_close_stmt_parser<'src, I>()
-> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    chumsky::select! { TokenKind::Ident(value) if value.eq_ignore_ascii_case("n") => () }
        .ignore_then(
            just(TokenKind::Minus)
                .then_ignore(just(TokenKind::Question))
                .to(Stmt::Hla(HlaStmt::DoCloseNFlagClear))
                .or(just(TokenKind::Plus)
                    .then_ignore(just(TokenKind::Question))
                    .to(Stmt::Hla(HlaStmt::DoCloseNFlagSet))),
        )
}

fn hla_condition_seed_stmt_parser<'src, I>()
-> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    chumsky::select! { TokenKind::Ident(value) if value.eq_ignore_ascii_case("a") => value }
        .then_ignore(just(TokenKind::Question))
        .then(operand_expr_parser())
        .map(|(_ident, parsed)| {
            if parsed.index.is_some() || parsed.addr_mode != OperandAddrMode::Direct {
                instruction_stmt("cmp", Some(parsed_operand_to_operand(parsed)))
            } else {
                Stmt::Hla(HlaStmt::ConditionSeed {
                    lhs: HlaRegister::A,
                    rhs: parsed.expr,
                })
            }
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
    recursive(|expr| {
        let atom = chumsky::select! {
            TokenKind::Number(value) => Expr::Number(value),
            TokenKind::Ident(value) => Expr::Ident(value),
            TokenKind::Main => Expr::Ident("main".to_string()),
            TokenKind::Eval(value) => parse_eval_expr_token(&value),
        }
        .or(just(TokenKind::LParen)
            .ignore_then(expr.clone())
            .then_ignore(just(TokenKind::RParen)));

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

        unary
            .clone()
            .then(
                just(TokenKind::Plus)
                    .to(ExprBinaryOp::Add)
                    .or(just(TokenKind::Minus).to(ExprBinaryOp::Sub))
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
            })
    })
    .boxed()
}

fn expression_fragment_parser<'src, I>()
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

fn line_sep_parser<'src, I>() -> impl chumsky::Parser<'src, I, TokenKind, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    just(TokenKind::Newline).or(just(TokenKind::Semi))
}

fn line_tail_parser<'src, I>()
-> impl chumsky::Parser<'src, I, Vec<TokenKind>, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    any()
        .filter(|token: &TokenKind| !matches!(token, TokenKind::Newline | TokenKind::Semi))
        .repeated()
        .collect::<Vec<_>>()
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
        TokenKind::Semi => "';'".to_string(),
        TokenKind::PlusPlus => "'++'".to_string(),
        TokenKind::Plus => "'+'".to_string(),
        TokenKind::Minus => "'-'".to_string(),
        TokenKind::Star => "'*'".to_string(),
        TokenKind::Percent => "'%'".to_string(),
        TokenKind::Amp => "'&'".to_string(),
        TokenKind::Pipe => "'|'".to_string(),
        TokenKind::Caret => "'^'".to_string(),
        TokenKind::Bang => "'!'".to_string(),
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

fn parse_eval_expr_token(value: &str) -> Expr {
    let trimmed = value.trim();
    if trimmed.is_empty() {
        return Expr::Number(0);
    }

    if is_ident_text(trimmed) {
        return Expr::Ident(trimmed.to_string());
    }

    if let Ok(number) = parse_numeric_text(trimmed) {
        return Expr::Number(number);
    }

    if let Ok(expanded) = k816_eval::expand(trimmed) {
        if let Ok(number) = expanded.trim().parse::<i64>() {
            return Expr::Number(number);
        }
    }

    Expr::EvalText(trimmed.to_string())
}

fn is_ident_text(text: &str) -> bool {
    let mut chars = text.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    if !(first.is_ascii_alphabetic() || first == '_' || first == '.') {
        return false;
    }
    chars.all(|ch| ch.is_ascii_alphanumeric() || ch == '_' || ch == '.')
}

fn parse_numeric_text(text: &str) -> Result<i64, ()> {
    if let Some(bin) = text.strip_prefix("0b") {
        return i64::from_str_radix(bin, 2).map_err(|_| ());
    }
    if let Some(hex) = text.strip_prefix("0x") {
        return i64::from_str_radix(hex, 16).map_err(|_| ());
    }
    if let Some(hex) = text.strip_prefix('$') {
        return i64::from_str_radix(hex, 16).map_err(|_| ());
    }
    text.parse::<i64>().map_err(|_| ())
}

fn eval_static_expr(expr: &Expr) -> Option<i64> {
    match expr {
        Expr::Number(value) => Some(*value),
        Expr::Ident(_) => None,
        Expr::EvalText(value) => {
            if let Ok(expanded) = k816_eval::expand(value) {
                expanded.trim().parse::<i64>().ok()
            } else {
                None
            }
        }
        Expr::Binary { op, lhs, rhs } => {
            let lhs = eval_static_expr(lhs)?;
            let rhs = eval_static_expr(rhs)?;
            match op {
                ExprBinaryOp::Add => lhs.checked_add(rhs),
                ExprBinaryOp::Sub => lhs.checked_sub(rhs),
            }
        }
        Expr::Unary { op, expr } => {
            let value = eval_static_expr(expr)?;
            match op {
                ExprUnaryOp::LowByte => Some(value & 0xFF),
                ExprUnaryOp::HighByte => Some((value >> 8) & 0xFF),
            }
        }
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
    fn emits_warning_for_bank_keyword() {
        let source = "bank legacy\n";
        let parsed = parse_with_warnings(SourceId(0), source).expect("parse");
        assert_eq!(parsed.file.items.len(), 1);
        assert_eq!(parsed.warnings.len(), 1);
        assert_eq!(parsed.warnings[0].severity, crate::diag::Severity::Warning);
        assert!(parsed.warnings[0].message.contains("deprecated"));
    }

    #[test]
    fn emits_warning_for_empty_function_body() {
        let source = "func f {\n}\n";
        let parsed = parse_with_warnings(SourceId(0), source).expect("parse");
        assert_eq!(parsed.file.items.len(), 1);
        assert_eq!(parsed.warnings.len(), 1);
        assert_eq!(parsed.warnings[0].severity, crate::diag::Severity::Warning);
        assert!(parsed.warnings[0].message.contains("empty function body"));
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
    fn parses_hla_do_close_with_n_flag_suffix() {
        let source = "main {\n  {\n    a=READY\n  } n-?\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let Item::CodeBlock(block) = &file.items[0].node else {
            panic!("expected code block");
        };

        assert!(matches!(block.body[0].node, Stmt::Hla(HlaStmt::DoOpen)));
        assert!(matches!(block.body[1].node, Stmt::Instruction(_)));
        assert!(matches!(
            block.body[2].node,
            Stmt::Hla(HlaStmt::DoCloseNFlagClear)
        ));
    }

    #[test]
    fn parses_hla_do_close_with_n_plus_suffix() {
        let source = "main {\n  {\n    a=READY\n  } n+?\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let Item::CodeBlock(block) = &file.items[0].node else {
            panic!("expected code block");
        };

        assert!(matches!(block.body[0].node, Stmt::Hla(HlaStmt::DoOpen)));
        assert!(matches!(block.body[1].node, Stmt::Instruction(_)));
        assert!(matches!(
            block.body[2].node,
            Stmt::Hla(HlaStmt::DoCloseNFlagSet)
        ));
    }

    #[test]
    fn warns_for_inefficient_postfix_le_and_gt_without_condition_seed() {
        let source = "main {\n  {\n    x++\n  } <=\n  {\n    y++\n  } >\n}\n";
        let parsed = parse_with_warnings(SourceId(0), source).expect("parse");
        assert_eq!(parsed.warnings.len(), 2);
        assert!(parsed.warnings[0].message.contains("`} <=`"));
        assert!(parsed.warnings[1].message.contains("`} >`"));
    }

    #[test]
    fn does_not_warn_for_postfix_ops_with_condition_seed() {
        let source = "main {\n  {\n    a?0\n  } <=\n}\n";
        let parsed = parse_with_warnings(SourceId(0), source).expect("parse");
        assert!(parsed.warnings.is_empty());
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
        assert!(matches!(
            block.entries[2].node,
            NamedDataEntry::Bytes(_)
        ));
        assert!(matches!(block.entries[3].node, NamedDataEntry::Bytes(_)));
        assert!(matches!(
            block.entries[4].node,
            NamedDataEntry::Convert { .. }
        ));
    }

    #[test]
    fn parses_main_symbol_in_address_byte_entries() {
        let source = "data vectors {\n  &<main &>main\n}\nmain {\n  return\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        assert_eq!(file.items.len(), 2);

        let Item::NamedDataBlock(block) = &file.items[0].node else {
            panic!("expected named data block");
        };
        assert_eq!(block.entries.len(), 1);

        let NamedDataEntry::Bytes(values) = &block.entries[0].node else {
            panic!("expected bytes entry");
        };
        assert_eq!(values.len(), 2);
        assert!(matches!(
            &values[0],
            Expr::Unary {
                op: ExprUnaryOp::LowByte,
                expr
            } if matches!(expr.as_ref(), Expr::Ident(name) if name == "main")
        ));
        assert!(matches!(
            &values[1],
            Expr::Unary {
                op: ExprUnaryOp::HighByte,
                expr
            } if matches!(expr.as_ref(), Expr::Ident(name) if name == "main")
        ));
    }

    #[test]
    fn parses_operand_modes_with_y_and_indirect_forms() {
        let source = "var ptr = 0x20\nmain {\n  a=ptr,y\n  a=(ptr)\n  a=(ptr,x)\n  a=(ptr),y\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        assert_eq!(file.items.len(), 2);

        let Item::CodeBlock(block) = &file.items[1].node else {
            panic!("expected code block");
        };
        assert_eq!(block.body.len(), 4);

        let expect_mode = |stmt: &Stmt, mode: OperandAddrMode, index: Option<IndexRegister>| {
            let Stmt::Instruction(instruction) = stmt else {
                panic!("expected instruction");
            };
            assert_eq!(instruction.mnemonic, "lda");
            let Some(Operand::Value {
                expr,
                force_far,
                index: found_index,
                addr_mode,
            }) = instruction.operand.as_ref()
            else {
                panic!("expected value operand");
            };
            assert!(matches!(expr, Expr::Ident(name) if name == "ptr"));
            assert!(!force_far);
            assert_eq!(*found_index, index);
            assert_eq!(*addr_mode, mode);
        };

        expect_mode(
            &block.body[0].node,
            OperandAddrMode::Direct,
            Some(IndexRegister::Y),
        );
        expect_mode(&block.body[1].node, OperandAddrMode::Indirect, None);
        expect_mode(&block.body[2].node, OperandAddrMode::IndexedIndirectX, None);
        expect_mode(&block.body[3].node, OperandAddrMode::IndirectIndexedY, None);
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
        assert!(
            diagnostics.iter().any(
                |diag| diag.message == "invalid syntax: expected something else, found newline"
            )
        );
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
