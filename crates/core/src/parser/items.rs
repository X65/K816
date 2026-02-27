use crate::ast::{
    CodeBlock, EvaluatorBlock, Expr, File, Item, ModeContract, NumFmt, RegWidth, SegmentDecl, Stmt,
    VarDecl,
};
use crate::lexer::TokenKind;
use crate::span::{SourceId, Span, Spanned};
use chumsky::{
    IterParser, Parser as _,
    error::Rich,
    input::ValueInput,
    prelude::{SimpleSpan, any, end, just, skip_then_retry_until},
};

use super::{
    ParseExtra, const_decl_item_parser, data_block_parser, ident_parser, line_sep_parser,
    line_tail_parser, named_data_block_parser, spanned, stmt_parser, var_decl_parser,
};

pub(super) fn file_parser<'src, I>(
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

pub(super) fn mode_annotation_parser<'src, I>()
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

    type FunctionBody = Vec<Spanned<Stmt>>;
    type NamedFunctionHeader = ((String, SimpleSpan), ModeContract);
    type ImplicitFunctionHeader = ((Vec<Modifier>, (String, SimpleSpan)), ModeContract);

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
                .collect::<Vec<Spanned<Stmt>>>(),
        )
        .then_ignore(just(TokenKind::RBrace));

    let mode_annotations = mode_annotation_parser();

    let func = just(TokenKind::Func)
        .ignore_then(ident_parser().map_with(|name, extra| (name, extra.span())))
        .then(mode_annotations.clone())
        .then(body.clone())
        .map(
            move |(((name, name_span), mode_contract), body): (NamedFunctionHeader, FunctionBody)| {
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
                ImplicitFunctionHeader,
                FunctionBody,
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
