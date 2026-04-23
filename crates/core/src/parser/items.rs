use crate::ast::{
    CodeBlock, ContractParam, EvaluatorBlock, Expr, File, ImmediateParam, ImmediateParamType, Item,
    ModeContract, NumFmt, RegName, RegWidth, SegmentDecl, Stmt, VarDecl,
};
use crate::lexer::TokenKind;
use crate::span::{SourceId, Span, Spanned};
use chumsky::{
    IterParser, Parser as _,
    error::Rich,
    input::ValueInput,
    prelude::{SimpleSpan, any, end, just, skip_then_retry_until},
};
use std::collections::HashSet;
use std::sync::Arc;

use super::{
    ParseExtra, const_decl_item_parser, data_block_parser, ident_parser, line_sep_parser,
    line_tail_parser, named_data_block_parser, parse_contract_register, spanned, stmt_parser,
    var_decl_parser,
};

pub(super) fn file_parser<'src, I>(
    source_id: SourceId,
    known_functions: Arc<HashSet<String>>,
) -> impl chumsky::Parser<'src, I, File, ParseExtra<'src>>
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    let separators = line_sep_parser().repeated();
    let item = spanned(item_parser(source_id, known_functions), source_id);
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
    known_functions: Arc<HashSet<String>>,
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

    let code_block_item =
        code_block_parser(source_id, known_functions.clone()).map(Item::CodeBlock);

    let stmt_item = stmt_parser(source_id, known_functions).map(Item::Statement);

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
                addr_hint: None,
                array_len: None,
                symbolic_subscript_fields: None,
                alloc_count: None,
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
    known_functions: Arc<HashSet<String>>,
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
    type ContractClause = (bool, Vec<ContractParam>, Option<ModeContract>, Vec<RegName>);
    type ExplicitFuncHead = ((String, SimpleSpan), ModeContract);
    type ImplicitFuncHead = ((Vec<Modifier>, (String, SimpleSpan)), ModeContract);
    type ExplicitFuncParts = ((ExplicitFuncHead, ContractClause), FunctionBody);
    type ImplicitFuncParts = ((ImplicitFuncHead, ContractClause), FunctionBody);

    let modifier = just(TokenKind::Far)
        .to(Modifier::Far)
        .or(just(TokenKind::Naked).to(Modifier::Naked))
        .or(just(TokenKind::Inline).to(Modifier::Inline));

    let modifiers = modifier.clone().repeated().collect::<Vec<_>>();
    let required_modifiers = modifier.repeated().at_least(1).collect::<Vec<_>>();
    let stmt = spanned(stmt_parser(source_id, known_functions), source_id);
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
    let reg_name = ident_parser().try_map(|name, span| {
        parse_contract_register(&name).ok_or_else(|| {
            Rich::custom(
                span,
                format!("expected contract register 'a', 'x', or 'y', found '{name}'"),
            )
        })
    });
    let immediate_param_type = chumsky::select! {
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("byte") => ImmediateParamType::Byte,
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("word") => ImmediateParamType::Word,
    };
    let contract_param = just(TokenKind::Hash)
        .ignore_then(ident_parser())
        .then(
            just(TokenKind::Colon)
                .ignore_then(immediate_param_type)
                .or_not(),
        )
        .map(|(name, ty)| {
            ContractParam::Immediate(ImmediateParam {
                name,
                ty: ty.unwrap_or(ImmediateParamType::Inferred),
            })
        })
        .or(
            ident_parser().map(|name| match parse_contract_register(&name) {
                Some(reg) => ContractParam::Register(reg),
                None => ContractParam::Alias(name),
            }),
        );
    let params = just(TokenKind::LParen)
        .ignore_then(
            contract_param
                .separated_by(just(TokenKind::Comma))
                .collect::<Vec<_>>(),
        )
        .then_ignore(just(TokenKind::RParen));
    let arrow_clause = just(TokenKind::Arrow)
        .ignore_then(
            mode_annotation_parser().then(
                reg_name
                    .separated_by(just(TokenKind::Comma))
                    .at_least(1)
                    .collect::<Vec<_>>()
                    .or_not(),
            ),
        )
        .validate(|(exit_contract, outputs), extra, emitter| {
            if exit_contract == ModeContract::default() && outputs.is_none() {
                emitter.emit(Rich::custom(
                    extra.span(),
                    "expected exit mode or output registers after '->'",
                ));
            }
            (
                (exit_contract != ModeContract::default()).then_some(exit_contract),
                outputs.unwrap_or_default(),
            )
        });
    let contract_clause = params
        .then(arrow_clause.clone().or_not())
        .map(|(params, arrow)| {
            let (exit_contract, outputs) = arrow.unwrap_or((None, Vec::new()));
            (true, params, exit_contract, outputs)
        })
        .or(arrow_clause.map(|(exit_contract, outputs)| (true, Vec::new(), exit_contract, outputs)))
        .or_not()
        .map(|clause| clause.unwrap_or((false, Vec::new(), None, Vec::new())));

    let func = just(TokenKind::Func)
        .ignore_then(ident_parser().map_with(|name, extra| (name, extra.span())))
        .then(mode_annotations.clone())
        .then(contract_clause.clone())
        .then(body.clone())
        .map(
            move |(
                (
                    ((name, name_span), mode_contract),
                    (has_contract, params, exit_contract, outputs),
                ),
                body,
            ): ExplicitFuncParts| {
                let range = name_span.into_range();
                CodeBlock {
                    name,
                    name_span: Some(Span::new(source_id, range.start, range.end)),
                    is_far: false,
                    is_naked: false,
                    is_inline: false,
                    has_contract,
                    params,
                    outputs,
                    mode_contract,
                    exit_contract,
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
        .then(contract_clause)
        .then(body)
        .map(
            move |(
                (
                    ((mods, (name, name_span)), mode_contract),
                    (has_contract, params, exit_contract, outputs),
                ),
                body,
            ): ImplicitFuncParts| {
                let range = name_span.into_range();
                let mut block = CodeBlock {
                    name,
                    name_span: Some(Span::new(source_id, range.start, range.end)),
                    is_far: false,
                    is_naked: false,
                    is_inline: false,
                    has_contract,
                    params,
                    outputs,
                    mode_contract,
                    exit_contract,
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
