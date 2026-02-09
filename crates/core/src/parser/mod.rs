use crate::ast::{
    BlockKind, CallStmt, CodeBlock, DataArg, DataBlock, DataCommand, Expr, File, SegmentDecl,
    Instruction, Item, LabelDecl, Operand, Stmt, VarDecl,
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

    let data_item = just(TokenKind::Data)
        .ignore_then(data_block_parser(source_id))
        .map(Item::DataBlock);

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
        });

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
            .then(expr_parser())
            .map(|(force_far, expr)| {
                Some(Operand::Value {
                    expr,
                    force_far: force_far.is_some(),
                })
            }));

    let instruction = mnemonic
        .then(operand)
        .map(|(mnemonic, operand)| Stmt::Instruction(Instruction { mnemonic, operand }));

    segment_stmt
        .or(var_stmt)
        .or(data_stmt)
        .or(address_stmt)
        .or(call_stmt)
        .or(label_stmt)
        .or(byte_stmt)
        .or(instruction)
        .boxed()
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
        assert_eq!(diagnostics.len(), 1, "expected exactly one diagnostic");
        assert_eq!(
            diagnostics[0].message,
            "invalid syntax: expected something else, found newline"
        );
        assert_eq!(diagnostics[0].primary.start, 12);
        assert_eq!(diagnostics[0].primary.end, 13);
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
