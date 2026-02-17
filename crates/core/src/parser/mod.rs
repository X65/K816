use crate::ast::{
    CallStmt, CodeBlock, Comment, ConstDecl, DataArg, DataBlock, DataCommand, DataWidth,
    EvaluatorBlock, Expr, ExprBinaryOp, ExprUnaryOp, File, HlaAluOp, HlaBranchForm, HlaCompareOp,
    HlaCondition, HlaCpuRegister, HlaFlag, HlaIncDecOp, HlaIncDecTarget, HlaOperandExpr, HlaRegister, HlaRhs,
    HlaShiftOp, HlaShiftTarget, HlaStackTarget, HlaStmt, IndexRegister, Instruction, Item,
    LabelDecl, ModeContract, NamedDataBlock, NamedDataEntry, NamedDataForEvalRange, NumFmt,
    Operand, OperandAddrMode, RegWidth, SegmentDecl, Stmt, SymbolicSubscriptFieldDecl, VarDecl,
};
use crate::diag::Diagnostic;
use crate::lexer::{NumLit, TokenKind, lex, lex_lenient};
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
pub struct ParseOutput {
    pub file: File,
    pub warnings: Vec<Diagnostic>,
}

pub fn parse(source_id: SourceId, source_text: &str) -> Result<File, Vec<Diagnostic>> {
    parse_with_warnings(source_id, source_text).map(|parsed| parsed.file)
}

/// Coalesce `LBracket ... RBracket` sequences into single `Eval(text)` tokens,
/// except for var bracket payloads which remain as individual tokens so the
/// parser can handle them with proper comment stripping.
fn coalesce_non_var_brackets(
    tokens: Vec<crate::lexer::Token>,
    source: &str,
) -> Vec<crate::lexer::Token> {
    use crate::lexer::Token;

    let is_var_bracket = |idx: usize| -> bool {
        let mut j = idx;
        while j > 0 {
            j -= 1;
            match &tokens[j].kind {
                TokenKind::Var => return true,
                TokenKind::Eq
                | TokenKind::Newline
                | TokenKind::Semi
                | TokenKind::LBrace
                | TokenKind::RBrace => return false,
                _ => continue,
            }
        }
        false
    };

    let find_matching_rbracket = |start: usize| -> Option<usize> {
        let mut depth = 1usize;
        let mut j = start + 1;
        while j < tokens.len() {
            match &tokens[j].kind {
                TokenKind::LBracket => depth += 1,
                TokenKind::RBracket => {
                    depth -= 1;
                    if depth == 0 {
                        return Some(j);
                    }
                }
                _ => {}
            }
            j += 1;
        }
        None
    };

    let mut result = Vec::with_capacity(tokens.len());
    let mut i = 0;
    while i < tokens.len() {
        match &tokens[i].kind {
            TokenKind::LBracket => {
                if is_var_bracket(i) {
                    // Leave var bracket payload as individual tokens for the parser.
                    // Skip past the entire bracket range so inner brackets aren't coalesced.
                    if let Some(close) = find_matching_rbracket(i) {
                        for token in &tokens[i..=close] {
                            result.push(token.clone());
                        }
                        i = close + 1;
                    } else {
                        // Unclosed bracket — emit as-is, let parser report the error
                        result.push(tokens[i].clone());
                        i += 1;
                    }
                } else if let Some(close) = find_matching_rbracket(i) {
                    // Coalesce into Eval(text) using source text
                    let content_start = tokens[i].span.end;
                    let content_end = tokens[close].span.start;
                    let text = source[content_start..content_end].to_string();
                    let span = Span::new(
                        tokens[i].span.source_id,
                        tokens[i].span.start,
                        tokens[close].span.end,
                    );
                    result.push(Token {
                        kind: TokenKind::Eval(text),
                        span,
                        text: source[tokens[i].span.start..tokens[close].span.end].to_string(),
                    });
                    i = close + 1;
                } else {
                    // Unclosed bracket — emit as-is
                    result.push(tokens[i].clone());
                    i += 1;
                }
            }
            TokenKind::RBracket => {
                // Stray RBracket not consumed by coalescing — emit as-is
                result.push(tokens[i].clone());
                i += 1;
            }
            _ => {
                result.push(tokens[i].clone());
                i += 1;
            }
        }
    }

    result
}

/// Strip comment tokens from the token list, returning them separately
/// so they can be attached to the AST without affecting the parser.
fn strip_comments(
    tokens: Vec<crate::lexer::Token>,
) -> (Vec<crate::lexer::Token>, Vec<Comment>) {
    let mut filtered = Vec::with_capacity(tokens.len());
    let mut comments = Vec::new();
    for token in tokens {
        match &token.kind {
            TokenKind::LineComment(text) | TokenKind::BlockComment(text) => {
                comments.push(Comment {
                    text: text.clone(),
                    span: token.span,
                });
            }
            _ => filtered.push(token),
        }
    }
    (filtered, comments)
}

pub fn parse_with_warnings(
    source_id: SourceId,
    source_text: &str,
) -> Result<ParseOutput, Vec<Diagnostic>> {
    let preprocessed = preprocess_source(source_text);
    let source_text = preprocessed.as_str();
    let (tokens, lex_diagnostics) = lex_lenient(source_id, source_text);
    let tokens = coalesce_non_var_brackets(tokens, source_text);
    let (tokens, comments) = strip_comments(tokens);

    // Lexer errors inside coalesced Eval tokens are fine — the evaluator engine
    // re-parses that text with its own lexer that supports a wider syntax.
    let eval_ranges: Vec<(usize, usize)> = tokens
        .iter()
        .filter_map(|t| match &t.kind {
            TokenKind::Eval(_) => Some((t.span.start, t.span.end)),
            _ => None,
        })
        .collect();
    let lex_errors: Vec<Diagnostic> = lex_diagnostics
        .into_iter()
        .filter(|d| {
            !eval_ranges
                .iter()
                .any(|(start, end)| d.primary.start >= *start && d.primary.end <= *end)
        })
        .collect();
    if !lex_errors.is_empty() {
        return Err(lex_errors);
    }

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
        Some(mut file) => {
            file.comments = comments;
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

/// Like `parse_with_warnings` but returns a partial AST even when there are
/// parse errors. Used by the LSP to extract symbol definitions from files that
/// contain syntax errors.
pub fn parse_lenient(source_id: SourceId, source_text: &str) -> (Option<File>, Vec<Diagnostic>) {
    let preprocessed = preprocess_source(source_text);
    let source_text = preprocessed.as_str();
    let (tokens, lex_diagnostics) = lex_lenient(source_id, source_text);
    let tokens = coalesce_non_var_brackets(tokens, source_text);
    let (tokens, comments) = strip_comments(tokens);
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
    let mut diagnostics: Vec<Diagnostic> = lex_diagnostics;
    diagnostics.extend(
        errors
            .into_iter()
            .map(|error| rich_error_to_diagnostic(source_id, source_text, error, "invalid syntax")),
    );

    let output = output.map(|mut file| {
        file.comments = comments;
        file
    });

    if let Some(ref file) = output {
        let mut warnings = collect_parser_warnings(file);
        warnings.extend(diagnostics.drain(..));
        return (output, warnings);
    }

    (output, diagnostics)
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

        if data_block_depth > 0 && trimmed == "nocross {" {
            skipped_nested_block_depth = 1;
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

        // Direct `a?rhs` + `} OP` is normalized into compare-based close semantics.
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
            comments: Vec::new(),
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

    let const_item = const_decl_item_parser(source_id);

    let var_item = var_decl_parser(source_id).map(Item::Var);

    let data_item = just(TokenKind::Data).ignore_then(
        named_data_block_parser(source_id)
            .map(Item::NamedDataBlock)
            .or(data_block_parser(source_id).map(Item::DataBlock)),
    );

    let code_block_item = code_block_parser(source_id).map(Item::CodeBlock);

    let stmt_item = stmt_parser(source_id).map(Item::Statement);

    let preproc_item =
        just(TokenKind::Hash)
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
            .then_ignore(
                just(TokenKind::Eq)
                    .then_ignore(chumsky::select! { TokenKind::String(_value) => () })
                    .or_not(),
            )
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
                initializer: Some(Expr::Number(0, NumFmt::Dec)),
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
    source_id: SourceId,
) -> impl chumsky::Parser<'src, I, NamedDataEntry, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    let segment_entry = just(TokenKind::Segment)
        .ignore_then(ident_parser())
        .map(|name| NamedDataEntry::Segment(SegmentDecl { name }));

    // Ident-based entry: label (ident:) or unknown (ident ...).
    // Unknown idents consume the rest of the line and emit an error,
    // preventing the data block parser from breaking on unrecognized directives.
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

    // repeat N { ... } - repeats data entries N times
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

    // code { ... } - embed code instructions within data section
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

    // evaluator [ code ] - inline evaluator in data context
    let evaluator_entry = chumsky::select! {
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("evaluator") => ()
    }
    .ignore_then(chumsky::select! { TokenKind::Eval(value) => value })
    .map(NamedDataEntry::Evaluator);

    // charset "string" - character-to-index mapping for subsequent strings
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

/// Flat (non-recursive) named data entry parser for nested contexts like repeat bodies.
fn named_data_entry_flat_parser<'src, I>()
-> impl chumsky::Parser<'src, I, NamedDataEntry, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    let segment_entry = just(TokenKind::Segment)
        .ignore_then(ident_parser())
        .map(|name| NamedDataEntry::Segment(SegmentDecl { name }));

    // Ident-based entry: label (ident:) or unknown (ident ...).
    // Unknown idents consume the rest of the line and emit an error,
    // preventing the data block parser from breaking on unrecognized directives.
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

fn const_decl_binding_parser<'src, I>(
    source_id: SourceId,
) -> impl chumsky::Parser<'src, I, ConstDecl, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    ident_parser()
        .then_ignore(just(TokenKind::Eq))
        .then(spanned(expr_parser(), source_id))
        .map(|(name, initializer)| ConstDecl {
            name,
            initializer: initializer.node,
            initializer_span: Some(initializer.span),
        })
}

fn const_decl_item_parser<'src, I>(
    source_id: SourceId,
) -> impl chumsky::Parser<'src, I, Item, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    just(TokenKind::Const)
        .ignore_then(
            const_decl_binding_parser(source_id)
                .then(
                    just(TokenKind::Comma)
                        .ignore_then(const_decl_binding_parser(source_id))
                        .repeated()
                        .collect::<Vec<_>>(),
                ),
        )
        .map(|(head, tail)| {
            if tail.is_empty() {
                Item::Const(head)
            } else {
                let mut decls = Vec::with_capacity(tail.len() + 1);
                decls.push(head);
                decls.extend(tail);
                Item::ConstGroup(decls)
            }
        })
}

fn data_width_parser<'src, I>() -> impl chumsky::Parser<'src, I, DataWidth, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    just(TokenKind::Colon).ignore_then(chumsky::select! {
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("byte") => DataWidth::Byte,
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("word") => DataWidth::Word,
        TokenKind::Far => DataWidth::Far,
    })
}

#[derive(Debug, Clone)]
enum VarBracketPayload {
    ArrayLen(Expr),
    SymbolicSubscriptFields(Vec<SymbolicSubscriptFieldDecl>),
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
    let field_entry = symbolic_subscript_field_entry_parser(source_id);

    let seps = just(TokenKind::Comma)
        .or(just(TokenKind::Newline))
        .repeated();

    let field_list = field_entry
        .then_ignore(seps.clone())
        .repeated()
        .at_least(1)
        .collect::<Vec<_>>()
        .map(VarBracketPayload::SymbolicSubscriptFields);

    let array_len = expr_parser()
        .then_ignore(seps.clone())
        .map(VarBracketPayload::ArrayLen);

    just(TokenKind::LBracket)
        .ignore_then(seps)
        .ignore_then(field_list.or(array_len))
        .then_ignore(just(TokenKind::RBracket))
        .or_not()
        .boxed()
}

fn symbolic_subscript_field_entry_parser<'src, I>(
    source_id: SourceId,
) -> impl chumsky::Parser<'src, I, SymbolicSubscriptFieldDecl, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    spanned(
        chumsky::select! { TokenKind::Ident(name) if name.starts_with('.') => name },
        source_id,
    )
    .then(
        just(TokenKind::LBracket)
            .ignore_then(spanned(expr_parser(), source_id))
            .then_ignore(just(TokenKind::RBracket))
            .or_not(),
    )
    .then(data_width_parser().or_not())
    .map(|((name_spanned, count_spanned), data_width)| {
        let name = name_spanned
            .node
            .strip_prefix('.')
            .unwrap_or(&name_spanned.node)
            .to_string();
        SymbolicSubscriptFieldDecl {
            name,
            data_width,
            count: count_spanned.as_ref().map(|c| c.node.clone()),
            count_span: count_spanned.map(|c| c.span),
            span: name_spanned.span,
        }
    })
    .boxed()
}

fn prefix_condition_parser<'src, I>()
-> impl chumsky::Parser<'src, I, (&'static str, HlaBranchForm), ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    // Flag-based: c-? c+? z+? z-? n+? n-? → skip branch (inverted)
    let c_flag =
        chumsky::select! { TokenKind::Ident(value) if value.eq_ignore_ascii_case("c") => () }
            .ignore_then(
                just(TokenKind::Plus)
                    .then_ignore(just(TokenKind::Question))
                    .to(("bcc", HlaBranchForm::FlagQuestion)) // execute when C=1, skip when C=0
                    .or(just(TokenKind::Minus)
                        .then_ignore(just(TokenKind::Question))
                        .to(("bcs", HlaBranchForm::FlagQuestion))), // execute when C=0, skip when C=1
            );

    let z_flag =
        chumsky::select! { TokenKind::Ident(value) if value.eq_ignore_ascii_case("z") => () }
            .ignore_then(
                just(TokenKind::Plus)
                    .then_ignore(just(TokenKind::Question))
                    .to(("bne", HlaBranchForm::FlagQuestion)) // execute when Z=1, skip when Z=0
                    .or(just(TokenKind::Minus)
                        .then_ignore(just(TokenKind::Question))
                        .to(("beq", HlaBranchForm::FlagQuestion))), // execute when Z=0, skip when Z=1
            );

    let n_flag =
        chumsky::select! { TokenKind::Ident(value) if value.eq_ignore_ascii_case("n") => () }
            .ignore_then(
                just(TokenKind::Plus)
                    .then_ignore(just(TokenKind::Question))
                    .to(("bpl", HlaBranchForm::FlagQuestion)) // execute when N=1, skip when N=0
                    .or(just(TokenKind::Minus)
                        .then_ignore(just(TokenKind::Question))
                        .to(("bmi", HlaBranchForm::FlagQuestion))), // execute when N=0, skip when N=1
            );

    // v+? v-? (with trailing ?); also accepts o+?/o-? as alias
    let v_flag =
        chumsky::select! { TokenKind::Ident(value) if value.eq_ignore_ascii_case("v") || value.eq_ignore_ascii_case("o") => () }
            .ignore_then(
                just(TokenKind::Plus)
                    .then_ignore(just(TokenKind::Question))
                    .to(("bvc", HlaBranchForm::FlagQuestion)) // execute when V=1, skip when V=0
                    .or(just(TokenKind::Minus)
                        .then_ignore(just(TokenKind::Question))
                        .to(("bvs", HlaBranchForm::FlagQuestion))), // execute when V=0, skip when V=1
            );

    // Comparison-based: >= < == != <0 >=0 <<= >>=
    let cmp_based = just(TokenKind::GtEq)
        .ignore_then(zero_number_token().or_not())
        .map(|zero| if zero.is_some() { ("bmi", HlaBranchForm::Symbolic) } else { ("bcc", HlaBranchForm::Symbolic) }) // >=0: exec N=0, skip N=1; >=: exec C=1, skip C=0
        .or(just(TokenKind::Lt)
            .ignore_then(zero_number_token().or_not())
            .map(|zero| if zero.is_some() { ("bpl", HlaBranchForm::Symbolic) } else { ("bcs", HlaBranchForm::Symbolic) })) // <0: exec N=1, skip N=0; <: exec C=0, skip C=1
        .or(just(TokenKind::EqEq).to(("bne", HlaBranchForm::Symbolic))) // ==: exec Z=1, skip Z=0
        .or(just(TokenKind::BangEq).to(("beq", HlaBranchForm::Symbolic))) // !=: exec Z=0, skip Z=1
        .or(just(TokenKind::LtLtEq).to(("bvc", HlaBranchForm::Symbolic))) // <<=: exec V=1, skip V=0
        .or(just(TokenKind::GtGtEq).to(("bvs", HlaBranchForm::Symbolic))); // >>=: exec V=0, skip V=1

    c_flag.or(z_flag).or(n_flag).or(v_flag).or(cmp_based)
}

fn zero_number_token<'src, I>() -> impl chumsky::Parser<'src, I, (), ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    chumsky::select! { TokenKind::Number(NumLit { value: 0, .. }) => () }
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
        .then(just(TokenKind::Plus).ignore_then(expr_parser()).or_not())
        .try_map(|(value, offset), span| {
            let value = eval_static_expr(&value)
                .ok_or_else(|| Rich::custom(span, "align value must be a constant expression"))?;
            let boundary = u16::try_from(value)
                .map_err(|_| Rich::custom(span, "align value must fit in u16"))?;
            let offset = match offset {
                Some(offset_expr) => {
                    let offset_val = eval_static_expr(&offset_expr).ok_or_else(|| {
                        Rich::custom(span, "align offset must be a constant expression")
                    })?;
                    u16::try_from(offset_val)
                        .map_err(|_| Rich::custom(span, "align offset must fit in u16"))?
                }
                None => 0,
            };
            Ok(Stmt::Align { boundary, offset })
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
        .map(|(far, target)| {
            Stmt::Call(CallStmt {
                target,
                is_far: far.is_some(),
            })
        });

    let label_stmt = ident_parser()
        .then_ignore(just(TokenKind::Colon))
        .map(|name| Stmt::Label(LabelDecl { name }));

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
        .try_map(|(_, suffix), span| suffix.ok_or_else(|| Rich::custom(span, "unexpected '}'")))
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

    let mnemonic = ident_parser().try_map(|mnemonic, _span| Ok(mnemonic));

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

            let prefix_conditional = prefix_condition_parser()
                .then(block_body.clone())
                .then(else_body_inner)
                .map(|(((skip_mnemonic, form), body), else_body)| {
                    Stmt::Hla(HlaStmt::PrefixConditional {
                        skip_mnemonic: skip_mnemonic.to_string(),
                        form,
                        body,
                        else_body,
                    })
                });

            let mode_scoped_block = mode_annotation_parser()
                .filter(|c: &ModeContract| c.a_width.is_some() || c.i_width.is_some())
                .then(block_body.clone())
                .map(|(contract, body)| Stmt::ModeScopedBlock {
                    a_width: contract.a_width,
                    i_width: contract.i_width,
                    body,
                });

            let never_block = chumsky::select! {
                TokenKind::Ident(value) if value.eq_ignore_ascii_case("never") => ()
            }
            .ignore_then(block_body)
            .map(|body| Stmt::Hla(HlaStmt::NeverBlock { body }));

            let inner_stmt = mode_scoped_block
                .or(prefix_conditional)
                .or(never_block)
                .or(flat_stmt_inner);

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

    let prefix_conditional_top = prefix_condition_parser()
        .then(block_body_top.clone())
        .then(else_body_top)
        .map(|(((skip_mnemonic, form), body), else_body)| {
            Stmt::Hla(HlaStmt::PrefixConditional {
                skip_mnemonic: skip_mnemonic.to_string(),
                form,
                body,
                else_body,
            })
        });

    let mode_scoped_block_top = mode_annotation_parser()
        .filter(|c: &ModeContract| c.a_width.is_some() || c.i_width.is_some())
        .then(block_body_top.clone())
        .map(|(contract, body)| Stmt::ModeScopedBlock {
            a_width: contract.a_width,
            i_width: contract.i_width,
            body,
        });

    let never_block_top = chumsky::select! {
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("never") => ()
    }
    .ignore_then(block_body_top)
    .map(|body| Stmt::Hla(HlaStmt::NeverBlock { body }));

    mode_scoped_block_top
        .or(prefix_conditional_top)
        .or(never_block_top)
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
    // Dead parser — `x = expr` is handled by assign_stmt_parser.
    chumsky::select! { TokenKind::Ident(value) if false => value }
        .map(|_: String| Stmt::Empty)
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
        .then(spanned(ident_parser(), SourceId(0)))
        .then_ignore(just(TokenKind::Eq))
        .then(hla_store_rhs_parser())
        .try_map(|((dests, middle), rhs), span| {
            if !middle.node.eq_ignore_ascii_case("a") {
                return Err(Rich::custom(
                    span,
                    format!(
                        "expected 'a' in store-from-a assignment, found '{}'",
                        middle.node
                    ),
                ));
            }
            Ok(Stmt::Hla(HlaStmt::StoreFromA {
                dests,
                rhs,
                load_start: Some(middle.span.start),
                store_end: Some(middle.span.end),
            }))
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

    // All-ident chain: ident = ident = ... = ident (3+ operands at statement boundary)
    let all_ident = ident_parser()
        .then_ignore(just(TokenKind::Eq))
        .repeated()
        .at_least(2)
        .collect::<Vec<_>>()
        .then(ident_parser())
        .then_ignore(stmt_boundary)
        .map(|(prefix, last)| {
            let mut operands = prefix;
            operands.push(last);
            Stmt::Hla(HlaStmt::AssignmentChain {
                idents: operands,
                tail_expr: None,
            })
        });

    // Ident chain ending with expression: ident = ident = ... = expr (3+ operands)
    let with_expr = ident_parser()
        .then_ignore(just(TokenKind::Eq))
        .repeated()
        .at_least(2)
        .collect::<Vec<_>>()
        .then(operand_expr_parser())
        .map(|(idents, rhs)| {
            Stmt::Hla(HlaStmt::AssignmentChain {
                idents,
                tail_expr: Some(rhs),
            })
        });

    all_ident.or(with_expr)
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
        if resolve_transfer(&lhs_lc, &rhs_lc).is_some() {
            return Stmt::Hla(HlaStmt::RegisterTransfer {
                dest: parse_cpu_register(&lhs_lc).expect("validated register"),
                src: parse_cpu_register(&rhs_lc).expect("validated register"),
            });
        }
        let rhs_upper = rhs_lc.to_ascii_uppercase();
        let lhs_upper = lhs_lc.to_ascii_uppercase();
        let msg = format!("transfer '{rhs_upper}' to '{lhs_upper}' is not directly supported");
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
        let register = match lhs.as_str() {
            "a" => HlaCpuRegister::A,
            "x" => HlaCpuRegister::X,
            "y" => HlaCpuRegister::Y,
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
        Stmt::Hla(HlaStmt::RegisterAssign {
            register,
            rhs: parsed,
        })
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
            let src = if rhs.eq_ignore_ascii_case("a") {
                HlaCpuRegister::A
            } else if rhs.eq_ignore_ascii_case("x") {
                HlaCpuRegister::X
            } else {
                HlaCpuRegister::Y
            };

            Stmt::Hla(HlaStmt::RegisterStore { dest, src })
        })
}

fn operand_expr_parser<'src, I>()
-> impl chumsky::Parser<'src, I, HlaOperandExpr, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    let index = just(TokenKind::Comma)
        .ignore_then(ident_parser())
        .or_not()
        .try_map(parse_index_register);

    let plain = expr_parser()
        .then(index.clone())
        .map(|(expr, index)| HlaOperandExpr {
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

            Ok(HlaOperandExpr {
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
    .map(|rhs| Stmt::Hla(HlaStmt::AccumulatorBitTest { rhs }));

    let a_alu = chumsky::select! {
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("a") => ()
    }
    .ignore_then(
        just(TokenKind::Plus)
            .to(HlaAluOp::Add)
            .or(just(TokenKind::Minus).to(HlaAluOp::Sub))
            .or(just(TokenKind::Amp).to(HlaAluOp::And))
            .or(just(TokenKind::Pipe).to(HlaAluOp::Or))
            .or(just(TokenKind::Caret).to(HlaAluOp::Xor)),
    )
    .then(operand_expr_parser())
    .map(|(op, rhs)| Stmt::Hla(HlaStmt::AccumulatorAlu { op, rhs }));

    let xy_cmp = chumsky::select! {
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("x") || value.eq_ignore_ascii_case("y") => value
    }
    .then_ignore(just(TokenKind::Question))
    .then(operand_expr_parser())
    .map(|(lhs, rhs)| {
        let register = if lhs.eq_ignore_ascii_case("x") {
            IndexRegister::X
        } else {
            IndexRegister::Y
        };
        Stmt::Hla(HlaStmt::IndexCompare { register, rhs })
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
            Ok(Stmt::Hla(HlaStmt::IncDec {
                op: HlaIncDecOp::Inc,
                target: HlaIncDecTarget::Address(HlaOperandExpr {
                    expr: Expr::Ident(base),
                    index,
                    addr_mode: OperandAddrMode::Direct,
                }),
            }))
        });

    let indexed_dec = ident_parser()
        .then_ignore(just(TokenKind::Comma))
        .then(ident_parser())
        .then_ignore(just(TokenKind::Minus))
        .then_ignore(just(TokenKind::Minus))
        .try_map(|(base, idx), span| {
            let index = parse_index_register(Some(idx), span)?;
            Ok(Stmt::Hla(HlaStmt::IncDec {
                op: HlaIncDecOp::Dec,
                target: HlaIncDecTarget::Address(HlaOperandExpr {
                    expr: Expr::Ident(base),
                    index,
                    addr_mode: OperandAddrMode::Direct,
                }),
            }))
        });

    let inc = ident_parser()
        .then_ignore(just(TokenKind::PlusPlus))
        .map(|ident| {
            if ident.eq_ignore_ascii_case("x") {
                return Stmt::Hla(HlaStmt::IncDec {
                    op: HlaIncDecOp::Inc,
                    target: HlaIncDecTarget::Register(IndexRegister::X),
                });
            }
            if ident.eq_ignore_ascii_case("y") {
                return Stmt::Hla(HlaStmt::IncDec {
                    op: HlaIncDecOp::Inc,
                    target: HlaIncDecTarget::Register(IndexRegister::Y),
                });
            }
            Stmt::Hla(HlaStmt::IncDec {
                op: HlaIncDecOp::Inc,
                target: HlaIncDecTarget::Address(HlaOperandExpr {
                    expr: Expr::Ident(ident),
                    index: None,
                    addr_mode: OperandAddrMode::Direct,
                }),
            })
        });

    let dec = ident_parser()
        .then_ignore(just(TokenKind::Minus))
        .then_ignore(just(TokenKind::Minus))
        .map(|ident| {
            if ident.eq_ignore_ascii_case("x") {
                return Stmt::Hla(HlaStmt::IncDec {
                    op: HlaIncDecOp::Dec,
                    target: HlaIncDecTarget::Register(IndexRegister::X),
                });
            }
            if ident.eq_ignore_ascii_case("y") {
                return Stmt::Hla(HlaStmt::IncDec {
                    op: HlaIncDecOp::Dec,
                    target: HlaIncDecTarget::Register(IndexRegister::Y),
                });
            }
            Stmt::Hla(HlaStmt::IncDec {
                op: HlaIncDecOp::Dec,
                target: HlaIncDecTarget::Address(HlaOperandExpr {
                    expr: Expr::Ident(ident),
                    index: None,
                    addr_mode: OperandAddrMode::Direct,
                }),
            })
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
        .to(HlaShiftOp::Rol)
        .or(just(TokenKind::Gt)
            .then_ignore(just(TokenKind::Gt))
            .then_ignore(just(TokenKind::Gt))
            .to(HlaShiftOp::Ror))
        .or(just(TokenKind::Lt)
            .then_ignore(just(TokenKind::Lt))
            .to(HlaShiftOp::Asl))
        .or(just(TokenKind::Gt)
            .then_ignore(just(TokenKind::Gt))
            .to(HlaShiftOp::Lsr));

    let indexed = ident_parser()
        .then_ignore(just(TokenKind::Comma))
        .then(ident_parser())
        .then(shift_op.clone())
        .try_map(|((base, idx), op), span| {
            let index = parse_index_register(Some(idx), span)?;
            Ok(Stmt::Hla(HlaStmt::ShiftRotate {
                op,
                target: HlaShiftTarget::Address(HlaOperandExpr {
                    expr: Expr::Ident(base),
                    index,
                    addr_mode: OperandAddrMode::Direct,
                }),
            }))
        });

    let plain = ident_parser().then(shift_op).map(|(target, op)| {
        if target.eq_ignore_ascii_case("a") {
            Stmt::Hla(HlaStmt::ShiftRotate {
                op,
                target: HlaShiftTarget::Accumulator,
            })
        } else {
            Stmt::Hla(HlaStmt::ShiftRotate {
                op,
                target: HlaShiftTarget::Address(HlaOperandExpr {
                    expr: Expr::Ident(target),
                    index: None,
                    addr_mode: OperandAddrMode::Direct,
                }),
            })
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
                ("c", true) => Ok(Stmt::Hla(HlaStmt::FlagSet {
                    flag: HlaFlag::Carry,
                    set: true,
                })),
                ("c", false) => Ok(Stmt::Hla(HlaStmt::FlagSet {
                    flag: HlaFlag::Carry,
                    set: false,
                })),
                ("d", true) => Ok(Stmt::Hla(HlaStmt::FlagSet {
                    flag: HlaFlag::Decimal,
                    set: true,
                })),
                ("d", false) => Ok(Stmt::Hla(HlaStmt::FlagSet {
                    flag: HlaFlag::Decimal,
                    set: false,
                })),
                ("i", true) => Ok(Stmt::Hla(HlaStmt::FlagSet {
                    flag: HlaFlag::Interrupt,
                    set: true,
                })),
                ("i", false) => Ok(Stmt::Hla(HlaStmt::FlagSet {
                    flag: HlaFlag::Interrupt,
                    set: false,
                })),
                ("v" | "o", false) => Ok(Stmt::Hla(HlaStmt::FlagSet {
                    flag: HlaFlag::Overflow,
                    set: false,
                })),
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
        .validate(|(target_text, push), extra, emitter| {
            let Some(target) = parse_stack_target(&target_text) else {
                let suffix = if push { "!!" } else { "??" };
                emitter.emit(Rich::custom(
                    extra.span(),
                    format!("unsupported stack shorthand '{target_text}{suffix}'"),
                ));
                return Stmt::Empty;
            };
            Stmt::Hla(HlaStmt::StackOp { target, push })
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
            Stmt::Hla(HlaStmt::Goto {
                target,
                indirect: true,
                far: false,
            })
        });

    let goto_direct = goto_kw.ignore_then(expr_parser()).map(|target| {
        Stmt::Hla(HlaStmt::Goto {
            target,
            indirect: false,
            far: false,
        })
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
            ("v" | "o", true) => "bvs",
            ("v" | "o", false) => "bvc",
            _ => {
                let sign = if plus { '+' } else { '-' };
                return Err(Rich::custom(
                    span,
                    format!("unsupported flag shorthand '{flag}{sign}?'"),
                ));
            }
        };
        Ok(Stmt::Hla(HlaStmt::BranchGoto {
            mnemonic: mnemonic.to_string(),
            target,
            form: HlaBranchForm::FlagQuestion,
        }))
    });

    // v+ and v- have no trailing ?; also accepts o+/o- as alias
    let v_flag_goto =
        chumsky::select! { TokenKind::Ident(value) if value.eq_ignore_ascii_case("v") || value.eq_ignore_ascii_case("o") => () }
            .ignore_then(
                just(TokenKind::Plus)
                    .to(("bvs", HlaBranchForm::FlagPlain))
                    .or(just(TokenKind::Minus).to(("bvc", HlaBranchForm::FlagPlain))),
            );

    // <0 goto → bmi, >=0 goto → bpl
    let signed_goto = just(TokenKind::Lt)
        .ignore_then(zero_number_token())
        .to(("bmi", HlaBranchForm::Symbolic))
        .or(just(TokenKind::GtEq)
            .ignore_then(zero_number_token())
            .to(("bpl", HlaBranchForm::Symbolic)));

    // <<= goto → bvs, >>= goto → bvc
    let overflow_goto = just(TokenKind::LtLtEq)
        .to(("bvs", HlaBranchForm::Symbolic))
        .or(just(TokenKind::GtGtEq).to(("bvc", HlaBranchForm::Symbolic)));

    let symbolic_branch_goto_stmt = v_flag_goto
        .or(signed_goto)
        .or(overflow_goto)
        .or(just(TokenKind::Lt).to(("bcc", HlaBranchForm::Symbolic)))
        .or(just(TokenKind::GtEq).to(("bcs", HlaBranchForm::Symbolic)))
        .or(just(TokenKind::EqEq).to(("beq", HlaBranchForm::Symbolic)))
        .or(just(TokenKind::BangEq).to(("bne", HlaBranchForm::Symbolic)))
        .then_ignore(goto_kw.clone())
        .then(expr_parser())
        .map(|((mnemonic, form), target)| {
            Stmt::Hla(HlaStmt::BranchGoto {
                mnemonic: mnemonic.to_string(),
                target,
                form,
            })
        });

    let branch_goto_stmt = question_flag_goto.or(symbolic_branch_goto_stmt);

    let return_stmt = chumsky::select! {
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("return") || value.eq_ignore_ascii_case("return_i") => value
    }
    .map(|keyword| Stmt::Hla(HlaStmt::Return {
        interrupt: keyword.eq_ignore_ascii_case("return_i"),
    }));

    let far_goto_stmt = just(TokenKind::Far)
        .ignore_then(goto_kw.clone())
        .ignore_then(expr_parser())
        .map(|target| {
            Stmt::Hla(HlaStmt::Goto {
                target,
                indirect: false,
                far: true,
            })
        });

    let far_call_stmt = just(TokenKind::Far)
        .ignore_then(ident_parser())
        .map(|target| {
            Stmt::Call(CallStmt {
                target,
                is_far: true,
            })
        });

    let break_kw =
        chumsky::select! { TokenKind::Ident(v) if v.eq_ignore_ascii_case("break") => () };
    let repeat_kw =
        chumsky::select! { TokenKind::Ident(v) if v.eq_ignore_ascii_case("repeat") => () };

    let branch_condition = just(TokenKind::Lt)
        .ignore_then(zero_number_token().or_not())
        .map(|zero| if zero.is_some() { "bmi" } else { "bcc" })
        .or(just(TokenKind::GtEq)
            .ignore_then(zero_number_token().or_not())
            .map(|zero| if zero.is_some() { "bpl" } else { "bcs" }))
        .or(just(TokenKind::EqEq).to("beq"))
        .or(just(TokenKind::BangEq).to("bne"))
        .or(just(TokenKind::LtLtEq).to("bvs"))
        .or(just(TokenKind::GtGtEq).to("bvc"));

    let conditional_break = branch_condition
        .clone()
        .then_ignore(break_kw.clone())
        .map(|m| {
            Stmt::Hla(HlaStmt::LoopBreak {
                mnemonic: m.to_string(),
            })
        });

    let conditional_repeat = branch_condition.then_ignore(repeat_kw.clone()).map(|m| {
        Stmt::Hla(HlaStmt::LoopRepeat {
            mnemonic: m.to_string(),
        })
    });

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
        .or(far_goto_stmt)
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
        .ignore_then(number_parser().map(|n| n.value).or_not())
        .map(|count| Stmt::Hla(HlaStmt::RepeatNop(count.unwrap_or(1) as usize)))
        .or(just(TokenKind::Percent).to(Stmt::Hla(HlaStmt::RepeatNop(1))))
}

fn discard_stmt_parser<'src, I>() -> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    let preprocessor =
        just(TokenKind::Hash)
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

    let data_keyword = chumsky::select! {
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("charset")
            || value.eq_ignore_ascii_case("tiles")
            || value.eq_ignore_ascii_case("colormode")
            || value.eq_ignore_ascii_case("imgwave")
            || value.eq_ignore_ascii_case("inv") => value
    }
    .then(line_tail_parser())
    .validate(|(token, _), extra, emitter| {
        emitter.emit(Rich::custom(extra.span(), format!("unexpected '{token}'")));
        Stmt::Empty
    });

    let generic = operator
        .map(|s| s.to_string())
        .or(just(TokenKind::Question).to("?".to_string()))
        .then(line_tail_parser())
        .validate(|(token, _), extra, emitter| {
            emitter.emit(Rich::custom(extra.span(), format!("unexpected '{token}'")));
            Stmt::Empty
        });

    preprocessor.or(data_keyword).or(generic)
}

fn parse_cpu_register(value: &str) -> Option<HlaCpuRegister> {
    match value {
        "a" => Some(HlaCpuRegister::A),
        "b" => Some(HlaCpuRegister::B),
        "c" => Some(HlaCpuRegister::C),
        "d" => Some(HlaCpuRegister::D),
        "s" => Some(HlaCpuRegister::S),
        "x" => Some(HlaCpuRegister::X),
        "y" => Some(HlaCpuRegister::Y),
        _ => None,
    }
}

fn parse_stack_target(value: &str) -> Option<HlaStackTarget> {
    if value.eq_ignore_ascii_case("a") {
        return Some(HlaStackTarget::A);
    }
    if value.eq_ignore_ascii_case("p")
        || value.eq_ignore_ascii_case("flag")
        || value.eq_ignore_ascii_case("n")
        || value.eq_ignore_ascii_case("v")
        || value.eq_ignore_ascii_case("o")
        || value.eq_ignore_ascii_case("m")
        || value.eq_ignore_ascii_case("x")
        || value.eq_ignore_ascii_case("b")
        || value.eq_ignore_ascii_case("d")
        || value.eq_ignore_ascii_case("i")
        || value.eq_ignore_ascii_case("z")
        || value.eq_ignore_ascii_case("c")
    {
        return Some(HlaStackTarget::P);
    }
    None
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
    parse_cpu_register(&value.to_ascii_lowercase()).is_some()
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

    // v+ → BVS, v- → BVC (no trailing ?); also accepts o+/o- as alias
    let v_flag =
        chumsky::select! { TokenKind::Ident(value) if value.eq_ignore_ascii_case("v") || value.eq_ignore_ascii_case("o") => () }
            .ignore_then(
                just(TokenKind::Plus)
                    .to(do_close_branch("bvs"))
                    .or(just(TokenKind::Minus).to(do_close_branch("bvc"))),
            );

    // <0 → BMI, >=0 → BPL
    let signed_cmp = just(TokenKind::Lt)
        .ignore_then(zero_number_token())
        .to(do_close_branch("bmi"))
        .or(just(TokenKind::GtEq)
            .ignore_then(zero_number_token())
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
            Stmt::Hla(HlaStmt::ConditionSeed {
                lhs: HlaRegister::A,
                rhs: parsed,
            })
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
        .map(|((lhs, rhs), op)| HlaCondition { lhs, op, rhs, seed_span: None })
}

fn expr_parser<'src, I>() -> impl chumsky::Parser<'src, I, Expr, ParseExtra<'src>> + Clone
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

fn number_parser<'src, I>(
) -> impl chumsky::Parser<'src, I, NumLit, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    chumsky::select! { TokenKind::Number(n) => n }.boxed()
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

    if matches!(flag.to_ascii_lowercase(), 'c' | 'z' | 'n' | 'v' | 'o') {
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
        TokenKind::LBracket => "'['".to_string(),
        TokenKind::RBracket => "']'".to_string(),
        TokenKind::Eval(_) => "eval fragment".to_string(),
        TokenKind::String(_) => "string literal".to_string(),
        TokenKind::Number(_) => "number literal".to_string(),
        TokenKind::Ident(value) => format!("identifier '{value}'"),
        TokenKind::ModeA8 => "'@a8'".to_string(),
        TokenKind::ModeA16 => "'@a16'".to_string(),
        TokenKind::ModeI8 => "'@i8'".to_string(),
        TokenKind::ModeI16 => "'@i16'".to_string(),
        TokenKind::SwapOp => "'><'".to_string(),
        TokenKind::LineComment(_) => "line comment".to_string(),
        TokenKind::BlockComment(_) => "block comment".to_string(),
    }
}

fn parse_eval_expr_token(value: &str) -> Expr {
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

    // Keep everything else (numbers, expressions) as EvalText so the
    // formatter preserves the original bracket syntax `[...]`.
    Expr::EvalText(trimmed.to_string())
}

fn parse_eval_expr_token_with_token_bounds(
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

fn eval_static_expr(expr: &Expr) -> Option<i64> {
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
        assert!(matches!(expr.node, Expr::Number(16, _)));
    }

    #[test]
    fn parses_expression_fragment_with_newline_padding() {
        let expr = parse_expression_fragment(SourceId(0), "\n0x10\n").expect("parse");
        assert!(matches!(expr.node, Expr::Number(16, _)));
    }

    #[test]
    fn parses_var_array_length() {
        let source = "var tiles[16]\n";
        let file = parse(SourceId(0), source).expect("parse");
        assert_eq!(file.items.len(), 1);

        let Item::Var(var) = &file.items[0].node else {
            panic!("expected var item");
        };

        assert!(matches!(var.array_len, Some(Expr::Number(16, _))));
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
        assert!(matches!(const_decl.initializer, Expr::Number(16, _)));
        assert!(const_decl.initializer_span.is_some());
    }

    #[test]
    fn preprocesses_comma_separated_const_declarations() {
        let source = "const A = 1, B = 2\n";
        let file = parse(SourceId(0), source).expect("parse");
        assert_eq!(file.items.len(), 1);
        let Item::ConstGroup(consts) = &file.items[0].node else {
            panic!("expected const group");
        };
        assert_eq!(consts.len(), 2);
        assert_eq!(consts[0].name, "A");
        assert_eq!(consts[1].name, "B");
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
        assert!(matches!(var.initializer, Some(Expr::Number(0x1234, _))));
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
        assert!(matches!(fields[2].count, Some(Expr::Number(20, _))));
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

        let Stmt::Hla(HlaStmt::RegisterAssign { rhs, .. }) = &block.body[0].node else {
            panic!("expected first HLA register assignment");
        };
        assert!(is_ident_named(&rhs.expr, "foo.idx"));

        let Stmt::Hla(HlaStmt::RegisterAssign { rhs, .. }) = &block.body[1].node else {
            panic!("expected second HLA register assignment");
        };
        assert!(matches!(
            &rhs.expr,
            Expr::Index { base, index }
                if is_ident_named(base.as_ref(), "foo.string")
                && matches!(index.as_ref(), Expr::Number(2, _))
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

        // With token-based parsing, chumsky reports that the type name is
        // not one of the recognized widths (byte/word/far).
        assert!(
            !errors.is_empty(),
            "expected parse errors for unsupported field type"
        );
    }

    #[test]
    fn rejects_empty_symbolic_subscript_array_count_at_field_slice() {
        let source = "var foo[\n  .a[]:byte\n] = 0x1234\n";
        let errors = parse(SourceId(0), source).expect_err("must fail");

        // With token-based parsing, chumsky reports that the expression
        // inside the brackets is missing.
        assert!(
            !errors.is_empty(),
            "expected parse errors for empty array count"
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
        let source = "func main {\n  x = 0\n  {\n    { a&?UART_READY } n-?\n    UART_DATA = a = text,x\n    x++\n  } a?0 !=\n  API_OP = a = [$FF]\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let Item::CodeBlock(block) = &file.items[0].node else {
            panic!("expected code block");
        };

        assert_eq!(block.body.len(), 7);
        assert!(matches!(
            block.body[0].node,
            Stmt::Hla(HlaStmt::RegisterAssign {
                register: HlaCpuRegister::X,
                ..
            })
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
        assert!(matches!(
            block.body[1].node,
            Stmt::Hla(HlaStmt::RegisterAssign { .. })
        ));
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
        assert!(matches!(
            block.body[1].node,
            Stmt::Hla(HlaStmt::RegisterAssign { .. })
        ));
        assert!(matches!(
            block.body[2].node,
            Stmt::Hla(HlaStmt::DoCloseNFlagSet)
        ));
    }

    #[test]
    fn parses_stack_shorthand_as_hla_nodes() {
        let source = "func main {\n  a!!\n  p??\n  flag!!\n  z??\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let Item::CodeBlock(block) = &file.items[0].node else {
            panic!("expected code block");
        };
        assert_eq!(block.body.len(), 4);
        assert!(matches!(
            block.body[0].node,
            Stmt::Hla(HlaStmt::StackOp {
                target: HlaStackTarget::A,
                push: true
            })
        ));
        assert!(matches!(
            block.body[1].node,
            Stmt::Hla(HlaStmt::StackOp {
                target: HlaStackTarget::P,
                push: false
            })
        ));
        assert!(matches!(
            block.body[2].node,
            Stmt::Hla(HlaStmt::StackOp {
                target: HlaStackTarget::P,
                push: true
            })
        ));
        assert!(matches!(
            block.body[3].node,
            Stmt::Hla(HlaStmt::StackOp {
                target: HlaStackTarget::P,
                push: false
            })
        ));
    }

    #[test]
    fn rejects_unknown_stack_shorthand_target() {
        let source = "func main {\n  foo!!\n}\n";
        let errors = parse(SourceId(0), source).expect_err("must fail");
        assert!(
            errors
                .iter()
                .any(|error| error.message.contains("unsupported stack shorthand")),
            "expected unsupported stack shorthand error, got: {errors:?}"
        );
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
    fn parses_indexed_condition_seed_as_hla_node() {
        let source = "func main {\n  a?zp,x\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let Item::CodeBlock(block) = &file.items[0].node else {
            panic!("expected code block");
        };
        let Stmt::Hla(HlaStmt::ConditionSeed { lhs, rhs }) = &block.body[0].node else {
            panic!("expected HLA condition seed");
        };
        assert_eq!(*lhs, HlaRegister::A);
        assert_eq!(rhs.index, Some(IndexRegister::X));
        assert_eq!(rhs.addr_mode, OperandAddrMode::Direct);
    }

    #[test]
    fn does_not_warn_for_postfix_ops_with_indexed_condition_seed() {
        let source = "func main {\n  {\n    a?zp,x\n  } <=\n}\n";
        let parsed = parse_with_warnings(SourceId(0), source).expect("parse");
        assert!(parsed.warnings.is_empty());
    }

    #[test]
    fn parses_named_data_block_entries() {
        let source = "data text {\n  segment INFO\n  \"Hello\"\n  $0D 'A' $00\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let Item::NamedDataBlock(block) = &file.items[0].node else {
            panic!("expected named data block");
        };
        assert_eq!(block.name, "text");
        assert_eq!(block.entries.len(), 3);
        assert!(matches!(block.entries[0].node, NamedDataEntry::Segment(_)));
        assert!(matches!(block.entries[1].node, NamedDataEntry::String(_)));
        assert!(matches!(block.entries[2].node, NamedDataEntry::Bytes(_)));
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
        assert!(matches!(forward.start, Expr::Number(0, _)));
        assert!(matches!(forward.end, Expr::Number(4, _)));
        assert_eq!(forward.eval.trim(), "i * FACTOR");

        let NamedDataEntry::ForEvalRange(reverse) = &block.entries[1].node else {
            panic!("expected reverse for-eval entry");
        };
        assert_eq!(reverse.iterator, "j");
        assert!(matches!(reverse.start, Expr::Number(4, _)));
        assert!(matches!(reverse.end, Expr::Number(0, _)));
        assert_eq!(reverse.eval.trim(), "j");
    }

    #[test]
    fn data_block_recovers_from_unknown_entries() {
        let cases = [
            "data gfx {\n  foo\n}\nfunc main {\n  a=0\n}\n",
            "data gfx {\n  image sprites 0 0\n  tiles 8 0 4\n}\nfunc main {\n  a=0\n}\n",
            "data gfx {\n  image\n}\nfunc main {\n  a=0\n}\n",
        ];
        for source in cases {
            let (file, _diagnostics) = parse_lenient(SourceId(0), source);
            let file = file.expect("should produce AST despite errors");
            assert!(
                file.items
                    .iter()
                    .any(|i| matches!(&i.node, Item::NamedDataBlock(_))),
                "data block not found in AST for: {source:?}",
            );
        }
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
    fn parses_packed_address_operators_in_bytes_entries() {
        let source = "data bytes {\n  &&ptr\n  &&&ptr\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let Item::NamedDataBlock(block) = &file.items[0].node else {
            panic!("expected named data block");
        };
        assert_eq!(block.entries.len(), 2);

        let NamedDataEntry::Bytes(first) = &block.entries[0].node else {
            panic!("expected bytes entry");
        };
        assert_eq!(first.len(), 1);
        assert!(matches!(
            &first[0],
            Expr::Unary {
                op: ExprUnaryOp::WordLittleEndian,
                expr
            } if is_ident_named(expr.as_ref(), "ptr")
        ));

        let NamedDataEntry::Bytes(second) = &block.entries[1].node else {
            panic!("expected bytes entry");
        };
        assert_eq!(second.len(), 1);
        assert!(matches!(
            &second[0],
            Expr::Unary {
                op: ExprUnaryOp::FarLittleEndian,
                expr
            } if is_ident_named(expr.as_ref(), "ptr")
        ));
    }

    #[test]
    fn keeps_bracketed_eval_ident_in_data_entries() {
        let source = "data text_data {\n  evaluator [ SCALE = 2 ]\n  [SCALE]\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let Item::NamedDataBlock(block) = &file.items[0].node else {
            panic!("expected named data block");
        };
        assert_eq!(block.entries.len(), 2);

        let NamedDataEntry::Bytes(values) = &block.entries[1].node else {
            panic!("expected bytes entry");
        };
        assert_eq!(values.len(), 1);
        assert!(matches!(
            &values[0],
            Expr::Unary {
                op: ExprUnaryOp::EvalBracketed,
                expr
            } if is_ident_named(expr.as_ref(), "SCALE")
        ));
    }

    #[test]
    fn keeps_bracketed_eval_ident_in_register_assignments() {
        let source = "[ A = 20, B = 30 ]\nfunc main {\n  a=[A]\n  x=[B]\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let Item::CodeBlock(block) = &file.items[1].node else {
            panic!("expected code block");
        };
        assert_eq!(block.body.len(), 2);

        let expect_bracketed_assign = |stmt: &Stmt, register: HlaCpuRegister, name: &str| {
            let Stmt::Hla(HlaStmt::RegisterAssign {
                register: dst,
                rhs,
            }) = stmt
            else {
                panic!("expected HLA register assignment");
            };
            assert_eq!(*dst, register);
            assert!(matches!(
                &rhs.expr,
                Expr::Unary {
                    op: ExprUnaryOp::EvalBracketed,
                    expr
                } if is_ident_named(expr.as_ref(), name)
            ));
        };

        expect_bracketed_assign(&block.body[0].node, HlaCpuRegister::A, "A");
        expect_bracketed_assign(&block.body[1].node, HlaCpuRegister::X, "B");
    }

    #[test]
    fn parses_operand_modes_with_y_and_indirect_forms() {
        let source =
            "var ptr = 0x20\nfunc main {\n  a=ptr,y\n  a=(ptr)\n  a=(ptr,x)\n  a=(ptr),y\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        assert_eq!(file.items.len(), 2);

        let Item::CodeBlock(block) = &file.items[1].node else {
            panic!("expected code block");
        };
        assert_eq!(block.body.len(), 4);

        let expect_mode = |stmt: &Stmt, mode: OperandAddrMode, index: Option<IndexRegister>| {
            let Stmt::Hla(HlaStmt::RegisterAssign { register, rhs }) = stmt else {
                panic!("expected HLA register assignment");
            };
            assert_eq!(*register, HlaCpuRegister::A);
            assert!(is_ident_named(&rhs.expr, "ptr"));
            assert_eq!(rhs.index, index);
            assert_eq!(rhs.addr_mode, mode);
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
            diagnostics
                .iter()
                .any(|diag| diag.message.starts_with("invalid syntax: expected")),
            "unexpected diagnostics: {diagnostics:#?}"
        );
    }

    #[test]
    fn parse_error_messages_are_human_readable() {
        let diagnostics =
            parse(SourceId(0), "func main {\n call\n}\n").expect_err("expected errors");
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

    #[test]
    fn keeps_flag_and_symbolic_branch_goto_forms_distinct() {
        let source = "func main {\n  c-? goto target\n  < goto target\n  v+ goto target\n  <<= goto target\n}\n";
        let parsed = parse(SourceId(0), source).expect("parse");
        let Item::CodeBlock(block) = &parsed.items[0].node else {
            panic!("expected code block");
        };

        let first = &block.body[0].node;
        let second = &block.body[1].node;
        let third = &block.body[2].node;
        let fourth = &block.body[3].node;

        assert!(matches!(
            first,
            Stmt::Hla(HlaStmt::BranchGoto {
                mnemonic,
                form: HlaBranchForm::FlagQuestion,
                ..
            }) if mnemonic == "bcc"
        ));
        assert!(matches!(
            second,
            Stmt::Hla(HlaStmt::BranchGoto {
                mnemonic,
                form: HlaBranchForm::Symbolic,
                ..
            }) if mnemonic == "bcc"
        ));
        assert!(matches!(
            third,
            Stmt::Hla(HlaStmt::BranchGoto {
                mnemonic,
                form: HlaBranchForm::FlagPlain,
                ..
            }) if mnemonic == "bvs"
        ));
        assert!(matches!(
            fourth,
            Stmt::Hla(HlaStmt::BranchGoto {
                mnemonic,
                form: HlaBranchForm::Symbolic,
                ..
            }) if mnemonic == "bvs"
        ));
    }

    #[test]
    fn keeps_prefix_flag_and_symbolic_forms_distinct() {
        let source = "func main {\n  c-?{ a=1 }\n  <{ a=2 }\n  v+?{ a=3 }\n  >>={ a=4 }\n}\n";
        let parsed = parse(SourceId(0), source).expect("parse");
        let Item::CodeBlock(block) = &parsed.items[0].node else {
            panic!("expected code block");
        };

        let first = &block.body[0].node;
        let second = &block.body[1].node;
        let third = &block.body[2].node;
        let fourth = &block.body[3].node;

        assert!(matches!(
            first,
            Stmt::Hla(HlaStmt::PrefixConditional {
                skip_mnemonic,
                form: HlaBranchForm::FlagQuestion,
                ..
            }) if skip_mnemonic == "bcs"
        ));
        assert!(matches!(
            second,
            Stmt::Hla(HlaStmt::PrefixConditional {
                skip_mnemonic,
                form: HlaBranchForm::Symbolic,
                ..
            }) if skip_mnemonic == "bcs"
        ));
        assert!(matches!(
            third,
            Stmt::Hla(HlaStmt::PrefixConditional {
                skip_mnemonic,
                form: HlaBranchForm::FlagQuestion,
                ..
            }) if skip_mnemonic == "bvc"
        ));
        assert!(matches!(
            fourth,
            Stmt::Hla(HlaStmt::PrefixConditional {
                skip_mnemonic,
                form: HlaBranchForm::Symbolic,
                ..
            }) if skip_mnemonic == "bvs"
        ));
    }
}
