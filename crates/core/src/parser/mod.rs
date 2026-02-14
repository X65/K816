use crate::ast::{
    CallStmt, CodeBlock, ConstDecl, DataArg, DataBlock, DataCommand, DataWidth,
    EvaluatorBlock, Expr, ExprBinaryOp, ExprUnaryOp, File, HlaCompareOp, HlaCondition, HlaRegister,
    HlaRhs, HlaStmt, IndexRegister, Instruction, Item, LabelDecl, ModeContract, NamedDataBlock,
    NamedDataEntry, NamedDataForEvalRange, Operand, OperandAddrMode, RegWidth, SegmentDecl, Stmt,
    SymbolicSubscriptFieldDecl, VarDecl,
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
        .map(|error| rich_error_to_diagnostic(source_id, source_text, error, "invalid syntax"))
        .collect::<Vec<_>>();

    match output {
        Some(file) => {
            // Parse produced output — partition diagnostics by severity.
            // Warnings (from [warn]-tagged .validate() emitters) are non-fatal.
            // Errors (from real parse failures or error-tagged .validate()) are fatal.
            let mut warnings = collect_parser_warnings(&file);
            let mut errors = Vec::new();
            for diag in diagnostics {
                match diag.severity {
                    crate::diag::Severity::Warning => warnings.push(diag),
                    crate::diag::Severity::Error => errors.push(diag),
                }
            }
            if errors.is_empty() {
                Ok(ParseOutput { file, warnings })
            } else {
                Err(errors)
            }
        }
        None => {
            // Parse truly failed — no output produced
            Err(diagnostics)
        }
    }
}

fn preprocess_source(source_text: &str) -> String {
    let mut out = Vec::new();
    let mut data_block_depth = 0usize;
    let mut skipped_nested_block_depth = 0usize;

    for raw_line in source_text.lines() {
        let mut line = raw_line.to_string();
        let mut trimmed = line.trim();

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

        if data_block_depth > 0 && trimmed.starts_with('[') && trimmed.ends_with(']') {
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
                line = format!("{indent}var {payload}");
                trimmed = line.trim();
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
        if trimmed.starts_with("const ") {
            let indent = raw_line
                .chars()
                .take_while(|ch| ch.is_ascii_whitespace())
                .collect::<String>();
            let payload = trimmed.trim_start_matches("const ").trim();
            if payload.contains(',') {
                for part in payload.split(',') {
                    let part = part.trim();
                    if !part.is_empty() {
                        out.push(format!("{indent}const {part}"));
                    }
                }
                continue;
            }
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

fn collect_parser_warnings(file: &File) -> Vec<Diagnostic> {
    let mut warnings = Vec::new();

    for item in &file.items {
        let Item::CodeBlock(block) = &item.node else {
            continue;
        };

        collect_hla_postfix_efficiency_warnings(block, &mut warnings);

        if block.name == "main" || !block.body.is_empty() {
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
            rich_error_to_diagnostic(source_id, source_text, error, "invalid expression fragment")
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

    let mode_token = just(TokenKind::ModeA8)
        .to((Some(RegWidth::W8), None))
        .or(just(TokenKind::ModeA16).to((Some(RegWidth::W16), None)))
        .or(just(TokenKind::ModeI8).to((None, Some(RegWidth::W8))))
        .or(just(TokenKind::ModeI16).to((None, Some(RegWidth::W16))));
    let module_mode = mode_token
        .then_ignore(line_sep_parser().repeated())
        .repeated()
        .collect::<Vec<_>>()
        .map(|annotations| {
            let mut contract = ModeContract::default();
            for (a, i) in annotations {
                if let Some(a) = a {
                    contract.a_width = Some(a);
                }
                if let Some(i) = i {
                    contract.i_width = Some(i);
                }
            }
            contract
        });

    separators
        .clone()
        .ignore_then(module_mode)
        .then(
            recover_item
                .then_ignore(separators.clone())
                .repeated()
                .collect::<Vec<_>>(),
        )
        .then_ignore(end())
        .map(|(mode_default, items)| File {
            mode_default,
            items,
        })
}

fn item_parser<'src, I>(
    source_id: SourceId,
) -> impl chumsky::Parser<'src, I, Item, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    let segment_item = just(TokenKind::Segment)
        .ignore_then(ident_parser())
        .map(|name| Item::Segment(SegmentDecl { name }));

    let const_item = const_decl_parser(source_id).map(Item::Const);

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
        .validate(|_, extra, emitter| {
            emitter.emit(Rich::custom(
                extra.span(),
                "[warn] preprocessor directive not processed",
            ));
            Item::Statement(Stmt::Empty)
        });

    let eval_block_item = chumsky::select! { TokenKind::Eval(value) => value }
        .map(|text| Item::EvaluatorBlock(EvaluatorBlock { text }));

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
    .validate(|name, extra, emitter| {
        if let Some(name) = name {
            Item::Var(VarDecl {
                name,
                data_width: None,
                array_len: None,
                symbolic_subscript_fields: None,
                initializer: Some(Expr::Number(0)),
                initializer_span: None,
            })
        } else {
            emitter.emit(Rich::custom(
                extra.span(),
                "expected name after 'image'/'binary'",
            ));
            Item::Statement(Stmt::Empty)
        }
    });

    preproc_item
        .or(eval_block_item)
        .or(image_binary_var_item)
        .or(segment_item)
        .or(const_item)
        .or(var_item)
        .or(data_item)
        .or(code_block_item)
        .or(stmt_item)
        .boxed()
}

fn mode_annotation_parser<'src, I>()
-> impl chumsky::Parser<'src, I, ModeContract, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    let mode_token = just(TokenKind::ModeA8)
        .to((Some(RegWidth::W8), None))
        .or(just(TokenKind::ModeA16).to((Some(RegWidth::W16), None)))
        .or(just(TokenKind::ModeI8).to((None, Some(RegWidth::W8))))
        .or(just(TokenKind::ModeI16).to((None, Some(RegWidth::W16))));

    mode_token
        .repeated()
        .collect::<Vec<_>>()
        .map(|annotations| {
            let mut contract = ModeContract::default();
            for (a, i) in annotations {
                if let Some(a) = a {
                    contract.a_width = Some(a);
                }
                if let Some(i) = i {
                    contract.i_width = Some(i);
                }
            }
            contract
        })
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

    let mode_annotations = mode_annotation_parser();

    let func = just(TokenKind::Func)
        .ignore_then(ident_parser().map_with(|name, extra| (name, extra.span())))
        .then(mode_annotations.clone())
        .then(body.clone())
        .map(
            move |(((name, name_span), mode_contract), body): (
                ((String, SimpleSpan), ModeContract),
                Vec<Spanned<Stmt>>,
            )| {
                let range = name_span.into_range();
                CodeBlock {
                    name,
                    name_span: Some(Span::new(source_id, range.start, range.end)),
                    is_far: false,
                    is_naked: false,
                    is_inline: false,
                    mode_contract,
                    body,
                }
            },
        );

    let explicit_block = modifiers.then(func).map(|(mods, mut block)| {
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
        .then(mode_annotations)
        .then(body)
        .map(
            move |(((mods, (name, name_span)), mode_contract), body): (
                ((Vec<Modifier>, (String, SimpleSpan)), ModeContract),
                Vec<Spanned<Stmt>>,
            )| {
                let range = name_span.into_range();
                let mut block = CodeBlock {
                    name,
                    name_span: Some(Span::new(source_id, range.start, range.end)),
                    is_far: false,
                    is_naked: false,
                    is_inline: false,
                    mode_contract,
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
        .or(for_eval_entry)
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

fn const_decl_parser<'src, I>(
    source_id: SourceId,
) -> impl chumsky::Parser<'src, I, ConstDecl, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    just(TokenKind::Const)
        .ignore_then(ident_parser())
        .then_ignore(just(TokenKind::Eq))
        .then(spanned(expr_parser(), source_id))
        .map(|(name, initializer)| ConstDecl {
            name,
            initializer: initializer.node,
            initializer_span: Some(initializer.span),
        })
}

fn data_width_parser<'src, I>() -> impl chumsky::Parser<'src, I, DataWidth, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    just(TokenKind::Colon).ignore_then(chumsky::select! {
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("byte") => DataWidth::Byte,
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("word") => DataWidth::Word,
    })
}

#[derive(Debug, Clone)]
enum VarBracketPayload {
    ArrayLen(Expr),
    SymbolicSubscriptFields(Vec<SymbolicSubscriptFieldDecl>),
}

#[derive(Debug, Clone)]
struct BracketPayloadParseError {
    message: String,
    span: Span,
}

impl BracketPayloadParseError {
    fn new(span: Span, message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            span,
        }
    }
}

fn var_decl_parser<'src, I>(
    source_id: SourceId,
) -> impl chumsky::Parser<'src, I, VarDecl, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    just(TokenKind::Var)
        .ignore_then(ident_parser())
        .then(data_width_parser().or_not())
        .then(bracket_payload_parser(source_id))
        .then(
            just(TokenKind::Eq)
                .ignore_then(spanned(expr_parser(), source_id))
                .or_not(),
        )
        .map(
            |(((name, data_width), bracket_payload), initializer_with_span)| {
                let (array_len, symbolic_subscript_fields) = match bracket_payload {
                    Some(VarBracketPayload::ArrayLen(array_len)) => (Some(array_len), None),
                    Some(VarBracketPayload::SymbolicSubscriptFields(symbolic_subscript_fields)) => {
                        (None, Some(symbolic_subscript_fields))
                    }
                    None => (None, None),
                };
                let (initializer, initializer_span) = match initializer_with_span {
                    Some(initializer) => (Some(initializer.node), Some(initializer.span)),
                    None => (None, None),
                };

                VarDecl {
                    name,
                    data_width,
                    array_len,
                    symbolic_subscript_fields,
                    initializer,
                    initializer_span,
                }
            },
        )
        .boxed()
}

fn bracket_payload_parser<'src, I>(
    source_id: SourceId,
) -> impl chumsky::Parser<'src, I, Option<VarBracketPayload>, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    spanned(
        chumsky::select! { TokenKind::Eval(text) => text },
        source_id,
    )
    .or_not()
    .validate(move |payload, _, emitter| match payload {
        Some(payload) => {
            let text = payload.node;
            let span = payload.span;
            match parse_var_bracket_payload(source_id, &text, span) {
                Ok(parsed) => Some(parsed),
                Err(error) => {
                    let payload_span: SimpleSpan = (error.span.start..error.span.end).into();
                    emitter.emit(Rich::custom(
                        payload_span,
                        format!("{}; inside brackets: [{}]", error.message, text),
                    ));
                    None
                }
            }
        }
        None => None,
    })
    .boxed()
}

fn parse_var_bracket_payload(
    source_id: SourceId,
    text: &str,
    eval_span: Span,
) -> Result<VarBracketPayload, BracketPayloadParseError> {
    if looks_like_symbolic_subscript_field_list(text) {
        return parse_symbolic_subscript_field_list(source_id, text, eval_span)
            .map(VarBracketPayload::SymbolicSubscriptFields);
    }

    parse_expression_fragment(source_id, text)
        .map(|expr| VarBracketPayload::ArrayLen(expr.node))
        .map_err(|error| {
            BracketPayloadParseError::new(
                eval_span,
                format!("invalid var array length expression: {}", error.message),
            )
        })
}

fn looks_like_symbolic_subscript_field_list(text: &str) -> bool {
    let candidate = trim_symbolic_subscript_payload_start(text);
    candidate.starts_with('.')
        && candidate
            .chars()
            .nth(1)
            .is_some_and(|ch| ch.is_ascii_alphabetic() || ch == '_')
}

fn trim_symbolic_subscript_payload_start(mut text: &str) -> &str {
    loop {
        let trimmed = text.trim_start();

        if let Some(rest) = trimmed.strip_prefix("//") {
            if let Some(newline) = rest.find('\n') {
                text = &rest[newline + 1..];
                continue;
            }
            return "";
        }

        if let Some(rest) = trimmed.strip_prefix("/*") {
            if let Some(close) = rest.find("*/") {
                text = &rest[close + 2..];
                continue;
            }
            return "";
        }

        return trimmed;
    }
}

fn parse_symbolic_subscript_field_list(
    source_id: SourceId,
    text: &str,
    eval_span: Span,
) -> Result<Vec<SymbolicSubscriptFieldDecl>, BracketPayloadParseError> {
    let mut entries: Vec<(String, Span)> = Vec::new();
    let mut current_start = 0usize;
    let mut bracket_depth = 0usize;
    let inner_start = eval_span.start + 1;

    for (offset, ch) in text.char_indices() {
        match ch {
            '[' => {
                bracket_depth += 1;
            }
            ']' => {
                if bracket_depth == 0 {
                    let close_span = Span::new(
                        source_id,
                        inner_start + offset,
                        inner_start + offset + ch.len_utf8(),
                    );
                    return Err(BracketPayloadParseError::new(
                        close_span,
                        "invalid symbolic subscript field list: unexpected ']'",
                    ));
                }
                bracket_depth -= 1;
            }
            ',' | '\n' if bracket_depth == 0 => {
                push_symbolic_subscript_field_entry(
                    source_id,
                    text,
                    current_start,
                    offset,
                    inner_start,
                    &mut entries,
                );
                current_start = offset + ch.len_utf8();
            }
            _ => {}
        }
    }

    if bracket_depth != 0 {
        return Err(BracketPayloadParseError::new(
            eval_span,
            "invalid symbolic subscript field list: missing closing ']' in field declaration",
        ));
    }

    push_symbolic_subscript_field_entry(
        source_id,
        text,
        current_start,
        text.len(),
        inner_start,
        &mut entries,
    );

    if entries.is_empty() {
        return Err(BracketPayloadParseError::new(
            eval_span,
            "invalid symbolic subscript field list: expected at least one field",
        ));
    }

    entries
        .into_iter()
        .map(|(entry, span)| parse_symbolic_subscript_field_entry(source_id, &entry, span))
        .collect()
}

fn push_symbolic_subscript_field_entry(
    source_id: SourceId,
    text: &str,
    start: usize,
    end: usize,
    inner_start: usize,
    entries: &mut Vec<(String, Span)>,
) {
    if start >= end {
        return;
    }

    let segment = &text[start..end];
    let segment = segment.find("//").map_or(segment, |idx| &segment[..idx]);
    let trimmed = segment.trim();
    if trimmed.is_empty() {
        return;
    }
    if trimmed.starts_with("/*")
        || trimmed.starts_with('*')
        || trimmed.starts_with("*/")
        || trimmed.ends_with("*/")
    {
        return;
    }

    let leading = segment.len() - segment.trim_start().len();
    let trailing = segment.len() - segment.trim_end().len();
    let span = Span::new(
        source_id,
        inner_start + start + leading,
        inner_start + end - trailing,
    );
    entries.push((trimmed.to_string(), span));
}

fn parse_symbolic_subscript_field_entry(
    source_id: SourceId,
    entry: &str,
    span: Span,
) -> Result<SymbolicSubscriptFieldDecl, BracketPayloadParseError> {
    let entry_span =
        |start: usize, end: usize| Span::new(source_id, span.start + start, span.start + end);

    let mut cursor = entry.trim();
    let mut consumed = 0usize;
    if !cursor.starts_with('.') {
        return Err(BracketPayloadParseError::new(
            span,
            format!("invalid symbolic subscript field declaration '{entry}': expected '.field'"),
        ));
    }
    cursor = &cursor[1..];
    consumed += 1;

    let mut chars = cursor.char_indices();
    let Some((_, first)) = chars.next() else {
        return Err(BracketPayloadParseError::new(
            entry_span(0, consumed),
            format!(
                "invalid symbolic subscript field declaration '{entry}': expected field name after '.'"
            ),
        ));
    };
    if !(first.is_ascii_alphabetic() || first == '_') {
        return Err(BracketPayloadParseError::new(
            entry_span(consumed, consumed + first.len_utf8()),
            format!(
                "invalid symbolic subscript field declaration '{entry}': expected field name after '.'"
            ),
        ));
    }
    let mut name_end = first.len_utf8();
    for (idx, ch) in chars {
        if ch.is_ascii_alphanumeric() || ch == '_' {
            name_end = idx + ch.len_utf8();
        } else {
            break;
        }
    }
    if name_end == 0 {
        return Err(BracketPayloadParseError::new(
            entry_span(consumed, consumed + 1),
            format!(
                "invalid symbolic subscript field declaration '{entry}': expected field name after '.'"
            ),
        ));
    }
    let name = cursor[..name_end].to_string();
    let after_name = &cursor[name_end..];
    let ws_after_name = after_name.len() - after_name.trim_start().len();
    cursor = after_name.trim_start();
    consumed += name_end + ws_after_name;

    let mut count = None;
    let mut count_span = None;
    if cursor.starts_with('[') {
        let mut depth = 0usize;
        let mut close_offset = None;
        for (idx, ch) in cursor.char_indices() {
            match ch {
                '[' => depth += 1,
                ']' => {
                    depth -= 1;
                    if depth == 0 {
                        close_offset = Some(idx);
                        break;
                    }
                }
                _ => {}
            }
        }
        let Some(close_offset) = close_offset else {
            return Err(BracketPayloadParseError::new(
                entry_span(consumed, entry.len()),
                format!(
                    "invalid symbolic subscript field declaration '{entry}': missing closing ']'"
                ),
            ));
        };
        let count_text_raw = &cursor[1..close_offset];
        let count_text = count_text_raw.trim();
        if count_text.is_empty() {
            return Err(BracketPayloadParseError::new(
                entry_span(0, consumed + close_offset + 1),
                format!(
                    "invalid symbolic subscript field declaration '{entry}': empty array count"
                ),
            ));
        }
        let leading_ws = count_text_raw.len() - count_text_raw.trim_start().len();
        let trailing_ws = count_text_raw.len() - count_text_raw.trim_end().len();
        let count_start = span.start + consumed + 1 + leading_ws;
        let count_end = span.start + consumed + close_offset - trailing_ws;
        count_span = Some(Span::new(source_id, count_start, count_end));
        let count_expr = parse_expression_fragment(source_id, count_text)
            .map(|expr| expr.node)
            .map_err(|error| {
                BracketPayloadParseError::new(
                    count_span.expect("count span must exist for non-empty count"),
                    format!(
                        "invalid symbolic subscript field declaration '{entry}': invalid array count expression: {}",
                        error.message
                    ),
                )
            })?;
        count = Some(count_expr);
        let after_count = &cursor[close_offset + 1..];
        let ws_after_count = after_count.len() - after_count.trim_start().len();
        cursor = after_count.trim_start();
        consumed += close_offset + 1 + ws_after_count;
    }

    let data_width = if cursor.is_empty() {
        None
    } else {
        let Some(type_name) = cursor.strip_prefix(':') else {
            return Err(BracketPayloadParseError::new(
                entry_span(consumed, entry.len()),
                format!(
                    "invalid symbolic subscript field declaration '{entry}': expected ':byte' or ':word'"
                ),
            ));
        };
        let leading_ws = type_name.len() - type_name.trim_start().len();
        let trailing_ws = type_name.len() - type_name.trim_end().len();
        let type_name = type_name.trim();
        let type_span = Span::new(
            source_id,
            span.start + consumed + 1 + leading_ws,
            span.start + consumed + cursor.len() - trailing_ws,
        );
        if type_name.eq_ignore_ascii_case("byte") {
            Some(DataWidth::Byte)
        } else if type_name.eq_ignore_ascii_case("word") {
            Some(DataWidth::Word)
        } else {
            return Err(BracketPayloadParseError::new(
                type_span,
                format!(
                    "invalid symbolic subscript field declaration '{entry}': unsupported field type '{type_name}'"
                ),
            ));
        }
    };

    Ok(SymbolicSubscriptFieldDecl {
        name,
        data_width,
        count,
        count_span,
        span,
    })
}

fn prefix_condition_parser<'src, I>()
-> impl chumsky::Parser<'src, I, &'static str, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    // Flag-based: c-? c+? z+? z-? n+? n-? → skip branch (inverted)
    let c_flag =
        chumsky::select! { TokenKind::Ident(value) if value.eq_ignore_ascii_case("c") => () }
            .ignore_then(
                just(TokenKind::Plus)
                    .then_ignore(just(TokenKind::Question))
                    .to("bcc") // execute when C=1, skip when C=0
                    .or(just(TokenKind::Minus)
                        .then_ignore(just(TokenKind::Question))
                        .to("bcs")), // execute when C=0, skip when C=1
            );

    let z_flag =
        chumsky::select! { TokenKind::Ident(value) if value.eq_ignore_ascii_case("z") => () }
            .ignore_then(
                just(TokenKind::Plus)
                    .then_ignore(just(TokenKind::Question))
                    .to("bne") // execute when Z=1, skip when Z=0
                    .or(just(TokenKind::Minus)
                        .then_ignore(just(TokenKind::Question))
                        .to("beq")), // execute when Z=0, skip when Z=1
            );

    let n_flag =
        chumsky::select! { TokenKind::Ident(value) if value.eq_ignore_ascii_case("n") => () }
            .ignore_then(
                just(TokenKind::Plus)
                    .then_ignore(just(TokenKind::Question))
                    .to("bpl") // execute when N=1, skip when N=0
                    .or(just(TokenKind::Minus)
                        .then_ignore(just(TokenKind::Question))
                        .to("bmi")), // execute when N=0, skip when N=1
            );

    // v+? v-? (with trailing ?)
    let v_flag =
        chumsky::select! { TokenKind::Ident(value) if value.eq_ignore_ascii_case("v") => () }
            .ignore_then(
                just(TokenKind::Plus)
                    .then_ignore(just(TokenKind::Question))
                    .to("bvc") // execute when V=1, skip when V=0
                    .or(just(TokenKind::Minus)
                        .then_ignore(just(TokenKind::Question))
                        .to("bvs")), // execute when V=0, skip when V=1
            );

    // Comparison-based: >= < == != <0 >=0 <<= >>=
    let cmp_based = just(TokenKind::GtEq)
        .ignore_then(just(TokenKind::Number(0)).or_not())
        .map(|zero| if zero.is_some() { "bmi" } else { "bcc" }) // >=0: exec N=0, skip N=1; >=: exec C=1, skip C=0
        .or(just(TokenKind::Lt)
            .ignore_then(just(TokenKind::Number(0)).or_not())
            .map(|zero| if zero.is_some() { "bpl" } else { "bcs" })) // <0: exec N=1, skip N=0; <: exec C=0, skip C=1
        .or(just(TokenKind::EqEq).to("bne")) // ==: exec Z=1, skip Z=0
        .or(just(TokenKind::BangEq).to("beq")) // !=: exec Z=0, skip Z=1
        .or(just(TokenKind::LtLtEq).to("bvc")) // <<=: exec V=1, skip V=0
        .or(just(TokenKind::GtGtEq).to("bvs")); // >>=: exec V=0, skip V=1

    c_flag.or(z_flag).or(n_flag).or(v_flag).or(cmp_based)
}

fn stmt_parser<'src, I>(
    source_id: SourceId,
) -> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    let segment_stmt = just(TokenKind::Segment)
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
        .ignore_then(just(TokenKind::Far).or_not())
        .then(ident_parser())
        .map(|(far, target)| Stmt::Call(CallStmt { target, is_far: far.is_some() }));

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
        .or(hla_flag_close_stmt_parser())
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
        .then(hla_do_close_suffix.clone().or_not())
        .rewind()
        .try_map(|(_, suffix), span| {
            suffix.ok_or_else(|| Rich::custom(span, "unexpected '}'"))
        })
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
    let invalid_flag_goto_stmt = invalid_flag_goto_stmt_parser();
    let nop_stmt = nop_stmt_parser();
    let chain_stmt = chain_stmt_parser();
    let discard_stmt = discard_stmt_parser();

    let mode_set_stmt = just(TokenKind::ModeA8)
        .to(Stmt::ModeSet {
            a_width: Some(RegWidth::W8),
            i_width: None,
        })
        .or(just(TokenKind::ModeA16).to(Stmt::ModeSet {
            a_width: Some(RegWidth::W16),
            i_width: None,
        }))
        .or(just(TokenKind::ModeI8).to(Stmt::ModeSet {
            a_width: None,
            i_width: Some(RegWidth::W8),
        }))
        .or(just(TokenKind::ModeI16).to(Stmt::ModeSet {
            a_width: None,
            i_width: Some(RegWidth::W16),
        }));

    let swap_ab_stmt = chumsky::select! {
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("b") || value.eq_ignore_ascii_case("a") => value
    }
    .then(just(TokenKind::SwapOp))
    .then(chumsky::select! {
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("a") || value.eq_ignore_ascii_case("b") => ()
    })
    .to(Stmt::SwapAB);

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
            .map(|expr| {
                Some(Operand::Immediate {
                    expr,
                    explicit_hash: true,
                })
            }))
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

    let separators_inner = line_sep_parser().repeated();

    let common_stmt = mode_set_stmt
        .or(swap_ab_stmt)
        .or(segment_stmt)
        .or(var_stmt)
        .or(data_stmt)
        .or(address_stmt)
        .or(align_stmt)
        .or(nocross_stmt)
        .or(call_stmt)
        .or(byte_stmt)
        .or(invalid_flag_goto_stmt)
        .or(hla_condition_seed_stmt)
        .or(hla_x_increment_stmt)
        .or(hla_x_assign_stmt)
        .or(hla_store_from_a_stmt)
        .or(chain_stmt)
        .or(assign_stmt)
        .or(store_stmt)
        .or(alu_stmt)
        .or(incdec_stmt)
        .or(shift_stmt)
        .or(flow_stmt)
        .or(flag_stmt)
        .or(stack_stmt)
        .or(nop_stmt)
        .or(discard_stmt)
        .or(label_stmt)
        .or(instruction)
        .boxed();

    // Flat statement parsers (no brace-delimited blocks)
    let flat_stmt_inner = common_stmt.clone();

    let base_stmt = common_stmt
        .or(hla_wait_stmt)
        .or(hla_do_close_stmt)
        .or(hla_do_open_stmt)
        .boxed();

    // Use recursive to allow mode_scoped_block and prefix_conditional
    // to nest inside each other's bodies
    let inner_stmt_recursive =
        chumsky::prelude::recursive::<_, Spanned<Stmt>, _, _, _>(|inner_stmt_ref| {
            let block_body = just(TokenKind::LBrace)
                .ignore_then(separators_inner.clone())
                .ignore_then(
                    inner_stmt_ref
                        .then_ignore(separators_inner.clone())
                        .repeated()
                        .collect::<Vec<_>>(),
                )
                .then_ignore(just(TokenKind::RBrace));

            let else_body_inner = chumsky::select! {
                TokenKind::Ident(value) if value.eq_ignore_ascii_case("else") => ()
            }
            .ignore_then(block_body.clone())
            .or_not();

            let prefix_conditional =
                prefix_condition_parser()
                    .then(block_body.clone())
                    .then(else_body_inner)
                    .map(|((skip_mnemonic, body), else_body)| {
                        Stmt::Hla(HlaStmt::PrefixConditional {
                            skip_mnemonic: skip_mnemonic.to_string(),
                            body,
                            else_body,
                        })
                    });

            let mode_scoped_block = mode_annotation_parser()
                .filter(|c: &ModeContract| c.a_width.is_some() || c.i_width.is_some())
                .then(block_body)
                .map(|(contract, body)| Stmt::ModeScopedBlock {
                    a_width: contract.a_width,
                    i_width: contract.i_width,
                    body,
                });

            let inner_stmt = mode_scoped_block.or(prefix_conditional).or(flat_stmt_inner);

            spanned(inner_stmt, source_id)
        });

    // Top-level statement: mode_scoped_block, prefix_conditional, or base_stmt
    let block_body_top = just(TokenKind::LBrace)
        .ignore_then(separators_inner.clone())
        .ignore_then(
            inner_stmt_recursive
                .then_ignore(separators_inner)
                .repeated()
                .collect::<Vec<_>>(),
        )
        .then_ignore(just(TokenKind::RBrace));

    let else_body_top = chumsky::select! {
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("else") => ()
    }
    .ignore_then(block_body_top.clone())
    .or_not();

    let prefix_conditional_top =
        prefix_condition_parser()
            .then(block_body_top.clone())
            .then(else_body_top)
            .map(|((skip_mnemonic, body), else_body)| {
                Stmt::Hla(HlaStmt::PrefixConditional {
                    skip_mnemonic: skip_mnemonic.to_string(),
                    body,
                    else_body,
                })
            });

    let mode_scoped_block_top = mode_annotation_parser()
        .filter(|c: &ModeContract| c.a_width.is_some() || c.i_width.is_some())
        .then(block_body_top)
        .map(|(contract, body)| Stmt::ModeScopedBlock {
            a_width: contract.a_width,
            i_width: contract.i_width,
            body,
        });

    mode_scoped_block_top
        .or(prefix_conditional_top)
        .or(base_stmt)
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
            && !matches!(parsed.expr, Expr::Ident(_) | Expr::IdentSpanned { .. })
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
    // Parse: dest1=dest2=...=a=rhs (one or more non-register store destinations)
    chumsky::select! { TokenKind::Ident(value) if !is_register_name(&value) => value }
        .then_ignore(just(TokenKind::Eq))
        .repeated()
        .at_least(1)
        .collect::<Vec<_>>()
        .then(ident_parser())
        .then_ignore(just(TokenKind::Eq))
        .then(hla_store_rhs_parser())
        .try_map(|((dests, middle), rhs), span| {
            if !middle.eq_ignore_ascii_case("a") {
                return Err(Rich::custom(
                    span,
                    format!("expected 'a' in store-from-a assignment, found '{middle}'"),
                ));
            }
            Ok(Stmt::Hla(HlaStmt::StoreFromA { dests, rhs }))
        })
}

fn chain_stmt_parser<'src, I>() -> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    let stmt_boundary = line_sep_parser()
        .ignored()
        .or(just(TokenKind::RBrace).ignored())
        .or(end().ignored())
        .rewind();

    // Three-register chain: reg = reg = reg (all token-level Ident detection)
    let three_reg = chumsky::select! {
        TokenKind::Ident(value) if is_register_name(&value) => value
    }
    .then_ignore(just(TokenKind::Eq))
    .then(chumsky::select! {
        TokenKind::Ident(value) if is_register_name(&value) => value
    })
    .then_ignore(just(TokenKind::Eq))
    .then(chumsky::select! {
        TokenKind::Ident(value) if is_register_name(&value) => value
    })
    .then_ignore(stmt_boundary.clone())
    .try_map(|((dest, mid), src), span| {
        let dest = dest.to_ascii_lowercase();
        let mid = mid.to_ascii_lowercase();
        let src = src.to_ascii_lowercase();
        let inner_mnemonic = resolve_transfer(&mid, &src).ok_or_else(|| {
            Rich::custom(
                span,
                format!("unsupported inner transfer '{mid}={src}' in chain"),
            )
        })?;
        let outer_mnemonic = resolve_transfer(&dest, &mid).ok_or_else(|| {
            Rich::custom(
                span,
                format!("unsupported outer transfer '{dest}={mid}' in chain"),
            )
        })?;
        Ok(Stmt::TransferChain(vec![
            Instruction {
                mnemonic: inner_mnemonic.to_string(),
                operand: None,
            },
            Instruction {
                mnemonic: outer_mnemonic.to_string(),
                operand: None,
            },
        ]))
    });

    // Load-and-transfer chain: reg = reg = expr
    let load_transfer = chumsky::select! {
        TokenKind::Ident(value) if is_register_name(&value) => value
    }
    .then_ignore(just(TokenKind::Eq))
    .then(chumsky::select! {
        TokenKind::Ident(value) if is_register_name(&value) => value
    })
    .then_ignore(just(TokenKind::Eq))
    .then(operand_expr_parser())
    .try_map(|((dest, mid), rhs_parsed), span| {
        let dest = dest.to_ascii_lowercase();
        let mid = mid.to_ascii_lowercase();
        let load_mnemonic = match mid.as_str() {
            "a" => "lda",
            "x" => "ldx",
            "y" => "ldy",
            _ => {
                return Err(Rich::custom(
                    span,
                    format!("register '{mid}' cannot be loaded from memory"),
                ))
            }
        };
        let inner = Instruction {
            mnemonic: load_mnemonic.to_string(),
            operand: Some(parsed_operand_to_operand(rhs_parsed)),
        };
        let outer_mnemonic = resolve_transfer(&dest, &mid).ok_or_else(|| {
            Rich::custom(
                span,
                format!("unsupported outer transfer '{dest}={mid}' in chain"),
            )
        })?;
        let outer = Instruction {
            mnemonic: outer_mnemonic.to_string(),
            operand: None,
        };
        Ok(Stmt::TransferChain(vec![inner, outer]))
    });

    three_reg.or(load_transfer)
}

fn assign_stmt_parser<'src, I>() -> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    let stmt_boundary = line_sep_parser()
        .ignored()
        .or(just(TokenKind::RBrace).ignored())
        .or(end().ignored())
        .rewind();

    // Register-to-register transfer: both sides are Ident tokens (not eval expressions).
    // The boundary check ensures `a=x,y` falls through to the load alternative.
    // Uses .validate() instead of .try_map() so the error is emitted but the parser
    // still succeeds (preventing backtracking to store_stmt for `s=y`).
    let register_transfer = chumsky::select! {
        TokenKind::Ident(value) if is_register_name(&value) => value
    }
    .then_ignore(just(TokenKind::Eq))
    .then(chumsky::select! {
        TokenKind::Ident(value) if is_register_name(&value) => value
    })
    .then_ignore(stmt_boundary)
    .validate(|(lhs, rhs), extra, emitter| {
        let lhs_lc = lhs.to_ascii_lowercase();
        let rhs_lc = rhs.to_ascii_lowercase();
        if let Some(mnemonic) = resolve_transfer(&lhs_lc, &rhs_lc) {
            return instruction_stmt(mnemonic, None);
        }
        let rhs_upper = rhs_lc.to_ascii_uppercase();
        let lhs_upper = lhs_lc.to_ascii_uppercase();
        let msg =
            format!("transfer '{rhs_upper}' to '{lhs_upper}' is not directly supported");
        emitter.emit(Rich::custom(
            extra.span(),
            match invalid_transfer_hint(&lhs_lc, &rhs_lc) {
                Some(hint) => format!("{msg}; hint: {hint}"),
                None => msg,
            },
        ));
        Stmt::Empty
    });

    // Register load from expression (LDA/LDX/LDY, error for unsupported registers).
    let register_load = chumsky::select! {
        TokenKind::Ident(value) if is_register_name(&value) => value
    }
    .then_ignore(just(TokenKind::Eq))
    .then(operand_expr_parser())
    .validate(|(lhs, parsed), extra, emitter| {
        let lhs = lhs.to_ascii_lowercase();
        let mnemonic = match lhs.as_str() {
            "a" => "lda",
            "x" => "ldx",
            "y" => "ldy",
            "c" => {
                emitter.emit(Rich::custom(
                    extra.span(),
                    "C is the 16-bit accumulator; hint: use a=expr for loads",
                ));
                return Stmt::Empty;
            }
            _ => {
                let reg = lhs.to_ascii_uppercase();
                emitter.emit(Rich::custom(
                    extra.span(),
                    format!("cannot load register '{reg}' with expression"),
                ));
                return Stmt::Empty;
            }
        };
        let operand = parsed_operand_to_operand(parsed);
        instruction_stmt(mnemonic, Some(operand))
    });

    register_transfer.or(register_load)
}

fn store_stmt_parser<'src, I>() -> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
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

fn incdec_stmt_parser<'src, I>() -> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
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

fn shift_stmt_parser<'src, I>() -> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
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

fn flag_stmt_parser<'src, I>() -> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    chumsky::select! { TokenKind::Ident(value) if value.chars().count() == 1 => value }
        .then(
            just(TokenKind::Plus)
                .to(true)
                .or(just(TokenKind::Minus).to(false)),
        )
        .try_map(|(flag, set), span| {
            let lower = flag.to_ascii_lowercase();
            let sign = if set { '+' } else { '-' };
            let shorthand = format!("{flag}{sign}");
            match (lower.as_str(), set) {
                ("c", true) => Ok(instruction_stmt("sec", None)),
                ("c", false) => Ok(instruction_stmt("clc", None)),
                ("d", true) => Ok(instruction_stmt("sed", None)),
                ("d", false) => Ok(instruction_stmt("cld", None)),
                ("i", true) => Ok(instruction_stmt("sei", None)),
                ("i", false) => Ok(instruction_stmt("cli", None)),
                ("v", false) => Ok(instruction_stmt("clv", None)),
                _ => Err(Rich::custom(
                    span,
                    format!("unsupported flag shorthand '{shorthand}'"),
                )),
            }
        })
}

fn stack_stmt_parser<'src, I>() -> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
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

fn flow_stmt_parser<'src, I>() -> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    let goto_kw = chumsky::select! {
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("goto") => ()
    };

    let goto_indirect = goto_kw
        .clone()
        .ignore_then(just(TokenKind::LParen))
        .ignore_then(expr_parser())
        .then_ignore(just(TokenKind::RParen))
        .map(|target| {
            instruction_stmt(
                "jmp",
                Some(Operand::Value {
                    expr: target,
                    force_far: false,
                    index: None,
                    addr_mode: OperandAddrMode::Indirect,
                }),
            )
        });

    let goto_direct = goto_kw.ignore_then(expr_parser()).map(|target| {
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

    let goto_stmt = goto_indirect.or(goto_direct);

    // Flag-based goto forms: flag goto label → branch instruction
    let question_flag_goto = chumsky::select! {
        TokenKind::Ident(value) if value.chars().count() == 1 => value
    }
    .then(
        just(TokenKind::Minus)
            .then_ignore(just(TokenKind::Question))
            .to(false)
            .or(just(TokenKind::Plus)
                .then_ignore(just(TokenKind::Question))
                .to(true)),
    )
    .then_ignore(goto_kw.clone())
    .then(expr_parser())
    .try_map(|((flag, plus), target), span| {
        let lower = flag.to_ascii_lowercase();
        let mnemonic = match (lower.as_str(), plus) {
            ("c", false) => "bcc",
            ("c", true) => "bcs",
            ("z", true) => "beq",
            ("z", false) => "bne",
            ("n", true) => "bmi",
            ("n", false) => "bpl",
            _ => {
                let sign = if plus { '+' } else { '-' };
                return Err(Rich::custom(
                    span,
                    format!("unsupported flag shorthand '{flag}{sign}?'"),
                ));
            }
        };
        Ok(instruction_stmt(
            mnemonic,
            Some(Operand::Value {
                expr: target,
                force_far: false,
                index: None,
                addr_mode: OperandAddrMode::Direct,
            }),
        ))
    });

    // v+ and v- have no trailing ?
    let v_flag_goto =
        chumsky::select! { TokenKind::Ident(value) if value.eq_ignore_ascii_case("v") => () }
            .ignore_then(
                just(TokenKind::Plus)
                    .to("bvs")
                    .or(just(TokenKind::Minus).to("bvc")),
            );

    // <0 goto → bmi, >=0 goto → bpl
    let signed_goto = just(TokenKind::Lt)
        .ignore_then(just(TokenKind::Number(0)))
        .to("bmi")
        .or(just(TokenKind::GtEq)
            .ignore_then(just(TokenKind::Number(0)))
            .to("bpl"));

    // <<= goto → bvs, >>= goto → bvc
    let overflow_goto = just(TokenKind::LtLtEq)
        .to("bvs")
        .or(just(TokenKind::GtGtEq).to("bvc"));

    let symbolic_branch_goto_stmt = v_flag_goto
        .or(signed_goto)
        .or(overflow_goto)
        .or(just(TokenKind::Lt).to("bcc"))
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

    let branch_goto_stmt = question_flag_goto.or(symbolic_branch_goto_stmt);

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
        .map(|target| Stmt::Call(CallStmt { target, is_far: true }));

    let break_kw =
        chumsky::select! { TokenKind::Ident(v) if v.eq_ignore_ascii_case("break") => () };
    let repeat_kw =
        chumsky::select! { TokenKind::Ident(v) if v.eq_ignore_ascii_case("repeat") => () };

    let branch_condition = just(TokenKind::Lt)
        .ignore_then(just(TokenKind::Number(0)).or_not())
        .map(|zero| if zero.is_some() { "bmi" } else { "bcc" })
        .or(just(TokenKind::GtEq)
            .ignore_then(just(TokenKind::Number(0)).or_not())
            .map(|zero| if zero.is_some() { "bpl" } else { "bcs" }))
        .or(just(TokenKind::EqEq).to("beq"))
        .or(just(TokenKind::BangEq).to("bne"))
        .or(just(TokenKind::LtLtEq).to("bvs"))
        .or(just(TokenKind::GtGtEq).to("bvc"));

    let conditional_break = branch_condition
        .clone()
        .then_ignore(break_kw.clone())
        .map(|m| Stmt::Hla(HlaStmt::LoopBreak { mnemonic: m.to_string() }));

    let conditional_repeat = branch_condition
        .then_ignore(repeat_kw.clone())
        .map(|m| Stmt::Hla(HlaStmt::LoopRepeat { mnemonic: m.to_string() }));

    let unconditional_break = break_kw.to(Stmt::Hla(HlaStmt::LoopBreak {
        mnemonic: "bra".to_string(),
    }));

    let unconditional_repeat = repeat_kw.to(Stmt::Hla(HlaStmt::LoopRepeat {
        mnemonic: "bra".to_string(),
    }));

    let break_repeat_stmt = conditional_break
        .or(conditional_repeat)
        .or(unconditional_break)
        .or(unconditional_repeat);

    goto_stmt
        .or(branch_goto_stmt)
        .or(return_stmt)
        .or(far_call_stmt)
        .or(break_repeat_stmt)
}

fn invalid_flag_goto_stmt_parser<'src, I>()
-> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    let goto_kw = chumsky::select! {
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("goto") => ()
    };

    chumsky::select! {
        TokenKind::Ident(value)
            if value.chars().count() == 1
                && !value.eq_ignore_ascii_case("c")
                && !value.eq_ignore_ascii_case("z")
                && !value.eq_ignore_ascii_case("n") => value
    }
    .then(
        just(TokenKind::Minus)
            .then_ignore(just(TokenKind::Question))
            .to('-')
            .or(just(TokenKind::Plus)
                .then_ignore(just(TokenKind::Question))
                .to('+')),
    )
    .then_ignore(goto_kw)
    .then(expr_parser())
    .try_map(|((flag, sign), _target), span| {
        Err(Rich::custom(
            span,
            format!("unsupported flag shorthand '{flag}{sign}?'"),
        ))
    })
}

fn nop_stmt_parser<'src, I>() -> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    just(TokenKind::Star)
        .ignore_then(number_parser().or_not())
        .map(|count| {
            let n = count.unwrap_or(1) as usize;
            if n == 1 {
                instruction_stmt("nop", None)
            } else {
                Stmt::Hla(HlaStmt::RepeatNop(n))
            }
        })
        .or(just(TokenKind::Percent).to(instruction_stmt("nop", None)))
}

fn discard_stmt_parser<'src, I>() -> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    let preprocessor = just(TokenKind::Hash)
        .then(line_tail_parser())
        .validate(|_, extra, emitter| {
            emitter.emit(Rich::custom(
                extra.span(),
                "[warn] preprocessor directive not processed",
            ));
            Stmt::Empty
        });

    let operator = just(TokenKind::EqEq)
        .to("==")
        .or(just(TokenKind::BangEq).to("!="))
        .or(just(TokenKind::LtLtEq).to("<<="))
        .or(just(TokenKind::GtGtEq).to(">>="))
        .or(just(TokenKind::LtEq).to("<="))
        .or(just(TokenKind::GtEq).to(">="))
        .or(just(TokenKind::Lt).to("<"))
        .or(just(TokenKind::Gt).to(">"));

    let keyword = chumsky::select! {
        TokenKind::Ident(value) if is_discard_keyword(&value) => value
    };

    let generic = operator
        .map(|s| s.to_string())
        .or(keyword)
        .or(just(TokenKind::Question).to("?".to_string()))
        .then(line_tail_parser())
        .validate(|(token, _), extra, emitter| {
            emitter.emit(Rich::custom(
                extra.span(),
                format!("unexpected '{token}'"),
            ));
            Stmt::Empty
        });

    preprocessor.or(generic)
}


fn instruction_stmt(mnemonic: &str, operand: Option<Operand>) -> Stmt {
    Stmt::Instruction(Instruction {
        mnemonic: mnemonic.to_string(),
        operand,
    })
}

fn parsed_operand_to_operand(parsed: ParsedOperandExpr) -> Operand {
    if parsed.addr_mode != OperandAddrMode::Direct || parsed.index.is_some() {
        return Operand::Value {
            expr: parsed.expr,
            force_far: false,
            index: parsed.index,
            addr_mode: parsed.addr_mode,
        };
    }
    match &parsed.expr {
        Expr::Index { .. } => Operand::Value {
            expr: parsed.expr,
            force_far: false,
            index: None,
            addr_mode: OperandAddrMode::Direct,
        },
        Expr::Unary { .. } => Operand::Immediate {
            expr: parsed.expr,
            explicit_hash: false,
        },
        _ => Operand::Auto { expr: parsed.expr },
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
        || value.eq_ignore_ascii_case("b")
        || value.eq_ignore_ascii_case("c")
        || value.eq_ignore_ascii_case("d")
        || value.eq_ignore_ascii_case("x")
        || value.eq_ignore_ascii_case("y")
        || value.eq_ignore_ascii_case("s")
}

fn resolve_transfer(dest: &str, src: &str) -> Option<&'static str> {
    match (dest, src) {
        ("x", "a") => Some("tax"),
        ("y", "a") => Some("tay"),
        ("a", "x") => Some("txa"),
        ("a", "y") => Some("tya"),
        ("x", "s") => Some("tsx"),
        ("s", "x") => Some("txs"),
        ("y", "x") => Some("txy"),
        ("x", "y") => Some("tyx"),
        // 16-bit accumulator C transfers (TCD, TCS, TDC, TSC)
        ("d", "c") => Some("tcd"),
        ("s", "c") => Some("tcs"),
        ("c", "d") => Some("tdc"),
        ("c", "s") => Some("tsc"),
        _ => None,
    }
}

fn invalid_transfer_hint(dest: &str, src: &str) -> Option<&'static str> {
    match (dest, src) {
        // A↔D/S: point to 16-bit accumulator C
        ("d", "a") => Some("use d=c"),
        ("s", "a") => Some("use s=c"),
        ("a", "d") => Some("use c=d"),
        ("a", "s") => Some("use c=s"),
        // D/S chains via C
        ("d", "s") => Some("use d=c=s"),
        ("s", "d") => Some("use s=c=d"),
        // D/S chains requiring two statements
        ("d", "x") => Some("use a=x then d=c"),
        ("d", "y") => Some("use a=y then d=c"),
        // Existing valid chains
        ("s", "y") => Some("use s=x=y"),
        ("y", "s") => Some("use y=x=s"),
        ("y", "d") => Some("use c=d then y=a"),
        ("x", "d") => Some("use c=d then x=a"),
        // B swap
        ("b", _) | (_, "b") => Some("use b><a to swap A and B"),
        // C is only for 16-bit transfers with D and S
        ("c", _) | (_, "c") => {
            Some("C is the 16-bit accumulator; only d=c, s=c, c=d, c=s are valid")
        }
        _ => None,
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

fn hla_flag_close_stmt_parser<'src, I>()
-> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    let do_close_branch = |mnemonic: &str| {
        Stmt::Hla(HlaStmt::DoCloseBranch {
            mnemonic: mnemonic.to_string(),
        })
    };

    // n-? → BPL, n+? → BMI
    let n_flag =
        chumsky::select! { TokenKind::Ident(value) if value.eq_ignore_ascii_case("n") => () }
            .ignore_then(
                just(TokenKind::Minus)
                    .then_ignore(just(TokenKind::Question))
                    .to(Stmt::Hla(HlaStmt::DoCloseNFlagClear))
                    .or(just(TokenKind::Plus)
                        .then_ignore(just(TokenKind::Question))
                        .to(Stmt::Hla(HlaStmt::DoCloseNFlagSet))),
            );

    // c-? → BCC, c+? → BCS
    let c_flag =
        chumsky::select! { TokenKind::Ident(value) if value.eq_ignore_ascii_case("c") => () }
            .ignore_then(
                just(TokenKind::Minus)
                    .then_ignore(just(TokenKind::Question))
                    .to(do_close_branch("bcc"))
                    .or(just(TokenKind::Plus)
                        .then_ignore(just(TokenKind::Question))
                        .to(do_close_branch("bcs"))),
            );

    // z+? → BEQ, z-? → BNE
    let z_flag =
        chumsky::select! { TokenKind::Ident(value) if value.eq_ignore_ascii_case("z") => () }
            .ignore_then(
                just(TokenKind::Plus)
                    .then_ignore(just(TokenKind::Question))
                    .to(do_close_branch("beq"))
                    .or(just(TokenKind::Minus)
                        .then_ignore(just(TokenKind::Question))
                        .to(do_close_branch("bne"))),
            );

    // v+ → BVS, v- → BVC (no trailing ?)
    let v_flag =
        chumsky::select! { TokenKind::Ident(value) if value.eq_ignore_ascii_case("v") => () }
            .ignore_then(
                just(TokenKind::Plus)
                    .to(do_close_branch("bvs"))
                    .or(just(TokenKind::Minus).to(do_close_branch("bvc"))),
            );

    // <0 → BMI, >=0 → BPL
    let signed_cmp = just(TokenKind::Lt)
        .ignore_then(just(TokenKind::Number(0)))
        .to(do_close_branch("bmi"))
        .or(just(TokenKind::GtEq)
            .ignore_then(just(TokenKind::Number(0)))
            .to(do_close_branch("bpl")));

    // <<= → BVS, >>= → BVC
    let overflow_cmp = just(TokenKind::LtLtEq)
        .to(do_close_branch("bvs"))
        .or(just(TokenKind::GtGtEq).to(do_close_branch("bvc")));

    n_flag
        .or(c_flag)
        .or(z_flag)
        .or(v_flag)
        .or(signed_cmp)
        .or(overflow_cmp)
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
        let number_atom = chumsky::select! {
            TokenKind::Number(value) => value
        }
        .map(Expr::Number);

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
            .then(data_width_parser().repeated().collect::<Vec<_>>())
            .map(|(expr, views)| {
                views.into_iter().fold(expr, |expr, width| Expr::TypedView {
                    expr: Box::new(expr),
                    width,
                })
            })
    })
    .boxed()
}

fn apply_eval_suffixes(mut base: Expr, suffixes: Vec<String>) -> Result<Expr, String> {
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

fn parse_symbolic_field_subscript(text: &str) -> Option<&str> {
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
    source_text: &str,
    error: Rich<'_, TokenKind>,
    context: &str,
) -> Diagnostic {
    let range = error.span().into_range();
    let mut span = Span::new(source_id, range.start, range.end);
    let mut primary_label = "here";
    let (is_warning, message, help) = match error.reason() {
        RichReason::Custom(custom) => {
            let custom = custom.to_string();
            // Detect [warn] prefix — these become Warning severity diagnostics
            let (is_warning, custom) = if let Some(rest) = custom.strip_prefix("[warn] ") {
                (true, rest.to_string())
            } else {
                (false, custom)
            };
            // Extract embedded hint from "; hint: " separator
            let (message_part, embedded_hint) = match custom.find("; hint: ") {
                Some(idx) => (
                    custom[..idx].to_string(),
                    Some(custom[idx + 8..].to_string()),
                ),
                None => (custom.clone(), None),
            };
            let help = embedded_hint.or_else(|| {
                custom
                    .starts_with("unsupported flag shorthand '")
                    .then_some("expected one of c+, c-, d+, d-, i+, i-, or v-".to_string())
            });
            (is_warning, format!("{context}: {message_part}"), help)
        }
        RichReason::ExpectedFound { expected, found } => {
            if found
                .as_deref()
                .is_some_and(|token| matches!(token, TokenKind::Newline))
            {
                // Newline token spans include the line break byte, which renders as a cross-line
                // highlight. Collapse to a zero-width end-of-line span for clearer diagnostics.
                span = Span::new(source_id, range.start, range.start);
                primary_label = "expected here";
            }
            if found
                .as_deref()
                .is_some_and(|token| matches!(token, TokenKind::Question))
            {
                if let Some(shorthand) =
                    detect_invalid_flag_goto_shorthand(source_text, range.start)
                {
                    let message = format!("{context}: unsupported flag shorthand '{shorthand}'");
                    let help = "expected one of c+, c-, d+, d-, i+, i-, or v-".to_string();
                    (false, message, Some(help))
                } else {
                    let found = found
                        .as_deref()
                        .map(token_kind_message)
                        .unwrap_or_else(|| "end of input".to_string());
                    let expected = format_expected_patterns(expected);
                    if expected.len() > 80 {
                        (false, format!("{context}: unexpected {found}"), None)
                    } else {
                        (
                            false,
                            format!("{context}: expected {expected}, found {found}"),
                            None,
                        )
                    }
                }
            } else {
                let found = found
                    .as_deref()
                    .map(token_kind_message)
                    .unwrap_or_else(|| "end of input".to_string());
                let expected = format_expected_patterns(expected);
                if expected.len() > 80 {
                    (false, format!("{context}: unexpected {found}"), None)
                } else {
                    (
                        false,
                        format!("{context}: expected {expected}, found {found}"),
                        None,
                    )
                }
            }
        }
    };
    let help = help.or_else(|| detect_var_width_after_array_hint(source_text, range.start));
    let make_diagnostic = if is_warning {
        Diagnostic::warning
    } else {
        Diagnostic::error
    };
    let diagnostic = make_diagnostic(span, message).with_primary_label(primary_label);
    match help {
        Some(help) => diagnostic.with_help(help),
        None => diagnostic,
    }
}

fn detect_invalid_flag_goto_shorthand(source_text: &str, question_offset: usize) -> Option<String> {
    let bytes = source_text.as_bytes();
    if bytes.get(question_offset).copied()? != b'?' {
        return None;
    }
    if question_offset < 2 {
        return None;
    }

    let sign = bytes[question_offset - 1] as char;
    if sign != '+' && sign != '-' {
        return None;
    }

    let flag = bytes[question_offset - 2] as char;
    if !flag.is_ascii_alphabetic() {
        return None;
    }

    if matches!(flag.to_ascii_lowercase(), 'c' | 'z' | 'n') {
        return None;
    }

    if question_offset >= 3 {
        let prev = bytes[question_offset - 3] as char;
        if prev.is_ascii_alphanumeric() || prev == '_' || prev == '.' {
            return None;
        }
    }

    let mut idx = question_offset + 1;
    while idx < bytes.len() && matches!(bytes[idx], b' ' | b'\t' | b'\r' | b'\n') {
        idx += 1;
    }

    let tail = source_text.get(idx..)?;
    if !tail.to_ascii_lowercase().starts_with("goto") {
        return None;
    }

    let after_goto = idx + 4;
    if let Some(next) = source_text
        .get(after_goto..)
        .and_then(|rest| rest.chars().next())
    {
        if next.is_ascii_alphanumeric() || next == '_' || next == '.' {
            return None;
        }
    }

    Some(format!("{flag}{sign}?"))
}

fn detect_var_width_after_array_hint(source_text: &str, error_offset: usize) -> Option<String> {
    if source_text.as_bytes().get(error_offset).copied()? != b':' {
        return None;
    }

    let line_start = source_text[..error_offset]
        .rfind('\n')
        .map_or(0, |index| index + 1);
    let line_end = source_text[error_offset..]
        .find('\n')
        .map_or(source_text.len(), |index| error_offset + index);
    let line = source_text.get(line_start..line_end)?.trim();
    let rest = line.strip_prefix("var ")?.trim();

    let bracket_open = rest.find('[')?;
    let bracket_close = rest.rfind(']')?;
    if bracket_close <= bracket_open {
        return None;
    }

    let name = rest[..bracket_open].trim();
    if !is_ident_text(name) {
        return None;
    }
    let count = rest[bracket_open + 1..bracket_close].trim();
    if count.is_empty() {
        return None;
    }
    let suffix = rest[bracket_close + 1..].trim_start();
    if !suffix.starts_with(':') {
        return None;
    }

    let width = suffix
        .trim_start_matches(':')
        .split(|ch: char| ch.is_ascii_whitespace())
        .next()
        .filter(|w| matches!(w.to_ascii_lowercase().as_str(), "byte" | "word"))
        .unwrap_or("word");
    let example = format!("var {name}:{width}[{count}]");
    Some(format!(
        "for typed arrays, place the type before the array length (e.g. `{example}`)"
    ))
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
        TokenKind::Const => "'const'".to_string(),
        TokenKind::Var => "'var'".to_string(),
        TokenKind::Func => "'func'".to_string(),
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
        TokenKind::DotDot => "'..'".to_string(),
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
        TokenKind::LtLtEq => "'<<='".to_string(),
        TokenKind::GtGtEq => "'>>='".to_string(),
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
        TokenKind::ModeA8 => "'@a8'".to_string(),
        TokenKind::ModeA16 => "'@a16'".to_string(),
        TokenKind::ModeI8 => "'@i8'".to_string(),
        TokenKind::ModeI16 => "'@i16'".to_string(),
        TokenKind::SwapOp => "'><'".to_string(),
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

fn parse_eval_expr_token_with_token_bounds(
    value: &str,
    token_start: usize,
    _token_end: usize,
) -> Expr {
    let trimmed = value.trim();
    if trimmed.is_empty() {
        return Expr::Number(0);
    }

    if is_ident_text(trimmed) {
        let leading_ws = value.len() - value.trim_start().len();
        let start = token_start + 1 + leading_ws;
        let end = start + trimmed.len();
        return Expr::IdentSpanned {
            name: trimmed.to_string(),
            start,
            end,
        };
    }

    parse_eval_expr_token(value)
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
            }
        }
        Expr::Unary { op, expr } => {
            let value = eval_static_expr(expr)?;
            match op {
                ExprUnaryOp::LowByte => Some(value & 0xFF),
                ExprUnaryOp::HighByte => Some((value >> 8) & 0xFF),
            }
        }
        Expr::TypedView { expr, .. } => eval_static_expr(expr),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn is_ident_named(expr: &Expr, expected: &str) -> bool {
        match expr {
            Expr::Ident(name) => name == expected,
            Expr::IdentSpanned { name, .. } => name == expected,
            _ => false,
        }
    }

    #[test]
    fn parses_far_function_and_call() {
        let source = "far func target {\n nop\n}\nfunc main {\n call target\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        assert_eq!(file.items.len(), 2);
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
        assert!(var.symbolic_subscript_fields.is_none());
        assert!(var.initializer.is_none());
    }

    #[test]
    fn parses_const_declaration() {
        let source = "const LIMIT = $10\n";
        let file = parse(SourceId(0), source).expect("parse");
        assert_eq!(file.items.len(), 1);

        let Item::Const(const_decl) = &file.items[0].node else {
            panic!("expected const item");
        };
        assert_eq!(const_decl.name, "LIMIT");
        assert!(matches!(const_decl.initializer, Expr::Number(16)));
        assert!(const_decl.initializer_span.is_some());
    }

    #[test]
    fn preprocesses_comma_separated_const_declarations() {
        let source = "const A = 1, B = 2\n";
        let file = parse(SourceId(0), source).expect("parse");
        assert_eq!(file.items.len(), 2);
        assert!(matches!(file.items[0].node, Item::Const(_)));
        assert!(matches!(file.items[1].node, Item::Const(_)));
    }

    #[test]
    fn parses_top_level_evaluator_block_item() {
        let source = "[ A = 1 ]\n";
        let file = parse(SourceId(0), source).expect("parse");
        assert_eq!(file.items.len(), 1);
        let Item::EvaluatorBlock(block) = &file.items[0].node else {
            panic!("expected evaluator block item");
        };
        assert!(block.text.contains("A = 1"));
    }

    #[test]
    fn parses_multiline_top_level_evaluator_block_item() {
        let source = "[\n  A = 1,\n  B = A + 2\n]\n";
        let file = parse(SourceId(0), source).expect("parse");
        assert_eq!(file.items.len(), 1);
        let Item::EvaluatorBlock(block) = &file.items[0].node else {
            panic!("expected evaluator block item");
        };
        assert!(block.text.contains("A = 1"));
        assert!(block.text.contains("B = A + 2"));
    }

    #[test]
    fn rejects_const_statement_inside_code_block() {
        let source = "func main {\n  const LIMIT = 1\n}\n";
        let diagnostics = parse(SourceId(0), source).expect_err("expected parse error");
        assert!(
            diagnostics
                .iter()
                .any(|diag| diag.message.contains("unexpected 'const'")),
            "unexpected diagnostics: {diagnostics:#?}"
        );
    }

    #[test]
    fn rejects_const_without_initializer() {
        let source = "const LIMIT\n";
        let diagnostics = parse(SourceId(0), source).expect_err("expected parse error");
        assert!(
            diagnostics
                .iter()
                .any(|diag| diag.message.contains("expected '='")),
            "unexpected diagnostics: {diagnostics:#?}"
        );
    }

    #[test]
    fn parses_symbolic_subscript_field_list_with_commas_and_trailing_separator() {
        let source = "var foo[\n  .field_w:word,\n  .idx:byte,\n  .string[20]:byte,\n] = 0x1234\n";
        let file = parse(SourceId(0), source).expect("parse");
        assert_eq!(file.items.len(), 1);

        let Item::Var(var) = &file.items[0].node else {
            panic!("expected var item");
        };
        assert!(var.array_len.is_none());
        assert!(matches!(var.initializer, Some(Expr::Number(0x1234))));
        let fields = var
            .symbolic_subscript_fields
            .as_ref()
            .expect("symbolic subscript field list");
        assert_eq!(fields.len(), 3);
        assert_eq!(fields[0].name, "field_w");
        assert_eq!(fields[1].name, "idx");
        assert_eq!(fields[2].name, "string");
        assert!(matches!(fields[0].data_width, Some(DataWidth::Word)));
        assert!(matches!(fields[1].data_width, Some(DataWidth::Byte)));
        assert!(matches!(fields[2].count, Some(Expr::Number(20))));
    }

    #[test]
    fn parses_symbolic_subscript_field_list_with_leading_comments() {
        let source = "var regs[\n  // control registers\n  .ctrl:word\n  /* status byte */\n  .status:byte\n] = 0x2100\n";
        let file = parse(SourceId(0), source).expect("parse");
        assert_eq!(file.items.len(), 1);

        let Item::Var(var) = &file.items[0].node else {
            panic!("expected var item");
        };
        let fields = var
            .symbolic_subscript_fields
            .as_ref()
            .expect("symbolic subscript field list");
        assert_eq!(fields.len(), 2);
        assert_eq!(fields[0].name, "ctrl");
        assert_eq!(fields[1].name, "status");
        assert!(matches!(fields[0].data_width, Some(DataWidth::Word)));
        assert!(matches!(fields[1].data_width, Some(DataWidth::Byte)));
    }

    #[test]
    fn parses_symbolic_subscript_forms() {
        let source = "func main {\n  a=foo[.idx]\n  a=foo.string[2]\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let Item::CodeBlock(block) = &file.items[0].node else {
            panic!("expected code block");
        };
        assert_eq!(block.body.len(), 2);

        let Stmt::Instruction(first) = &block.body[0].node else {
            panic!("expected first instruction");
        };
        let Some(Operand::Auto { expr }) = &first.operand else {
            panic!("expected first auto operand");
        };
        assert!(is_ident_named(expr, "foo.idx"));

        let Stmt::Instruction(second) = &block.body[1].node else {
            panic!("expected second instruction");
        };
        let Some(Operand::Value { expr, .. }) = &second.operand else {
            panic!("expected second value operand");
        };
        assert!(matches!(
            expr,
            Expr::Index { base, index }
                if is_ident_named(base.as_ref(), "foo.string")
                && matches!(index.as_ref(), Expr::Number(2))
        ));
    }

    #[test]
    fn parses_symbolic_subscript_fields_with_default_var_width() {
        let source = "var baz:byte[\n  .a\n  .b\n  .len:word\n] = 0x2244\n";
        let file = parse(SourceId(0), source).expect("parse");
        let Item::Var(var) = &file.items[0].node else {
            panic!("expected var item");
        };
        assert!(matches!(var.data_width, Some(DataWidth::Byte)));
        let fields = var
            .symbolic_subscript_fields
            .as_ref()
            .expect("symbolic subscript field list");
        assert_eq!(fields.len(), 3);
        assert!(fields[0].data_width.is_none());
        assert!(fields[1].data_width.is_none());
        assert!(matches!(fields[2].data_width, Some(DataWidth::Word)));
    }

    #[test]
    fn rejects_unsupported_symbolic_subscript_field_type_in_var_brackets() {
        let source = "var foo[\n  .a:dword\n] = 0x1234\n";
        let errors = parse(SourceId(0), source).expect_err("must fail");

        assert!(
            errors
                .iter()
                .any(|error| { error.message.contains("unsupported field type 'dword'") })
        );
        assert!(
            !errors
                .iter()
                .any(|error| error.message.contains("unexpected '='"))
        );
    }

    #[test]
    fn rejects_empty_symbolic_subscript_array_count_at_field_slice() {
        let source = "var foo[\n  .a[]:byte\n] = 0x1234\n";
        let errors = parse(SourceId(0), source).expect_err("must fail");
        let error = errors
            .iter()
            .find(|error| error.message.contains("empty array count"))
            .expect("empty-count diagnostic");

        assert_eq!(&source[error.primary.start..error.primary.end], ".a[]");
    }

    #[test]
    fn parses_byte_directive_values() {
        let source = "func main {\n .byte 1, 0x02, symbol\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        assert_eq!(file.items.len(), 1);

        let Item::CodeBlock(block) = &file.items[0].node else {
            panic!("expected code block");
        };
        assert_eq!(block.body.len(), 1);

        let Stmt::Bytes(values) = &block.body[0].node else {
            panic!("expected .byte statement");
        };
        assert_eq!(values.len(), 3);
        assert!(matches!(values[0], Expr::Number(1)));
        assert!(matches!(values[1], Expr::Number(2)));
        assert!(is_ident_named(&values[2], "symbol"));
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
        let source = "func main {\n loop:\n nop\n}\n";
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
        let source = "func main {\n  x = #0\n  {\n    { a&?UART_READY } n-?\n    UART_DATA = a = text,x\n    x++\n  } a?0 !=\n  API_OP = a = [$FF]\n}\n";
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
        let source = "func main {\n  {\n    a=READY\n  } n-?\n}\n";
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
        let source = "func main {\n  {\n    a=READY\n  } n+?\n}\n";
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
        let source = "func main {\n  {\n    x++\n  } <=\n  {\n    y++\n  } >\n}\n";
        let parsed = parse_with_warnings(SourceId(0), source).expect("parse");
        assert_eq!(parsed.warnings.len(), 2);
        assert!(parsed.warnings[0].message.contains("`} <=`"));
        assert!(parsed.warnings[1].message.contains("`} >`"));
    }

    #[test]
    fn does_not_warn_for_postfix_ops_with_condition_seed() {
        let source = "func main {\n  {\n    a?0\n  } <=\n}\n";
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
        assert!(matches!(block.entries[2].node, NamedDataEntry::Bytes(_)));
        assert!(matches!(block.entries[3].node, NamedDataEntry::Bytes(_)));
        assert!(matches!(
            block.entries[4].node,
            NamedDataEntry::Convert { .. }
        ));
    }

    #[test]
    fn parses_named_data_for_eval_range_entries() {
        let source = "data table {\n  for i=0..4 eval [ i * FACTOR ]\n  for j=4..0 eval [ j ]\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let Item::NamedDataBlock(block) = &file.items[0].node else {
            panic!("expected named data block");
        };
        assert_eq!(block.entries.len(), 2);

        let NamedDataEntry::ForEvalRange(forward) = &block.entries[0].node else {
            panic!("expected forward for-eval entry");
        };
        assert_eq!(forward.iterator, "i");
        assert!(matches!(forward.start, Expr::Number(0)));
        assert!(matches!(forward.end, Expr::Number(4)));
        assert_eq!(forward.eval.trim(), "i * FACTOR");

        let NamedDataEntry::ForEvalRange(reverse) = &block.entries[1].node else {
            panic!("expected reverse for-eval entry");
        };
        assert_eq!(reverse.iterator, "j");
        assert!(matches!(reverse.start, Expr::Number(4)));
        assert!(matches!(reverse.end, Expr::Number(0)));
        assert_eq!(reverse.eval.trim(), "j");
    }

    #[test]
    fn parses_main_symbol_in_address_byte_entries() {
        let source = "data vectors {\n  &<main &>main\n}\nfunc main {\n  return\n}\n";
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
            } if is_ident_named(expr.as_ref(), "main")
        ));
        assert!(matches!(
            &values[1],
            Expr::Unary {
                op: ExprUnaryOp::HighByte,
                expr
            } if is_ident_named(expr.as_ref(), "main")
        ));
    }

    #[test]
    fn parses_operand_modes_with_y_and_indirect_forms() {
        let source = "var ptr = 0x20\nfunc main {\n  a=ptr,y\n  a=(ptr)\n  a=(ptr,x)\n  a=(ptr),y\n}\n";
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
            assert!(is_ident_named(expr, "ptr"));
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
        let source = "func main {\n call\n}\nfar var trailing\n";
        let diagnostics = parse(SourceId(0), source).expect_err("expected parse errors");
        assert_eq!(diagnostics.len(), 2, "expected exactly two diagnostics");
        assert!(
            diagnostics.iter().any(|diag| diag
                .message
                .starts_with("invalid syntax: expected")),
            "unexpected diagnostics: {diagnostics:#?}"
        );
    }

    #[test]
    fn parse_error_messages_are_human_readable() {
        let diagnostics = parse(SourceId(0), "func main {\n call\n}\n").expect_err("expected errors");
        let message = &diagnostics[0].message;
        assert!(message.contains("expected"));
        assert!(!message.contains("TokenKind"));
        assert!(!message.contains("ExpectedFound"));
    }

    #[test]
    fn suggests_width_before_array_in_var_decl_errors() {
        let source = "var bar[20]:word = 0x2000\n";
        let diagnostics = parse(SourceId(0), source).expect_err("expected parse error");
        assert!(
            diagnostics.iter().any(|diag| {
                diag.message == "invalid syntax: unexpected ':'"
                    && diag.supplements.iter().any(|supplement| {
                        matches!(
                            supplement,
                            crate::diag::Supplemental::Help(help)
                                if help.contains("`var bar:word[20]`")
                        )
                    })
            }),
            "unexpected diagnostics: {diagnostics:#?}"
        );
    }

    #[test]
    fn reports_unsupported_flag_shorthand_for_invalid_flag_goto() {
        let source = "func main {\n  s-? goto .skip\n.skip:\n  return\n}\n";
        let diagnostics = parse(SourceId(0), source).expect_err("expected parse errors");
        assert!(
            diagnostics
                .iter()
                .any(|diag| diag.message == "invalid syntax: unsupported flag shorthand 's-?'"),
            "unexpected diagnostics: {diagnostics:#?}"
        );
        assert!(diagnostics.iter().any(|diag| {
            diag.supplements.iter().any(|supplement| {
                matches!(
                    supplement,
                    crate::diag::Supplemental::Help(help)
                        if help == "expected one of c+, c-, d+, d-, i+, i-, or v-"
                )
            })
        }));
    }

    #[test]
    fn invalid_flag_goto_parser_matches_signed_flag_form() {
        let source = "s-? goto .skip";
        let tokens = lex(SourceId(0), source).expect("lex");
        let end_offset = tokens.last().map(|token| token.span.end).unwrap_or(0);
        let token_stream = Stream::from_iter(tokens.into_iter().map(|token| {
            let span = (token.span.start..token.span.end).into();
            (token.kind, span)
        }))
        .map((end_offset..end_offset).into(), |(kind, span): (_, _)| {
            (kind, span)
        });

        let (_output, errors) = invalid_flag_goto_stmt_parser()
            .parse(token_stream)
            .into_output_errors();
        assert!(
            errors.iter().any(|error| matches!(
                error.reason(),
                RichReason::Custom(message)
                    if message == "unsupported flag shorthand 's-?'"
            )),
            "unexpected parser errors: {errors:#?}"
        );
    }
}
