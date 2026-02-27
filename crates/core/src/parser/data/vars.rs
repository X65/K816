use crate::ast::{Expr, HlaBranchForm, IndexRegister, SymbolicSubscriptFieldDecl, VarDecl};
use crate::lexer::TokenKind;
use crate::span::{SourceId, Spanned};
use chumsky::{
    IterParser, Parser as _,
    input::ValueInput,
    prelude::{SimpleSpan, just},
    recursive::recursive,
};

use super::super::{ParseExtra, expr_parser, ident_parser, spanned, zero_number_token};
use super::data_width_parser;

#[derive(Debug, Clone)]
pub(in super::super) enum CommaTrailer {
    Index(IndexRegister),
    BlockMoveDst(Expr),
}

#[derive(Debug, Clone)]
enum VarBracketPayload {
    ArrayLen(Expr),
    SymbolicSubscriptFields(Vec<SymbolicSubscriptFieldDecl>),
}

pub(in super::super) fn var_decl_parser<'src, I>(
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
    let field_entry = recursive::<_, SymbolicSubscriptFieldDecl, _, _, _>(|field_entry_ref| {
        let seps = just(TokenKind::Comma)
            .or(just(TokenKind::Newline))
            .repeated();

        let nested_field_list = field_entry_ref
            .then_ignore(seps.clone())
            .repeated()
            .at_least(1)
            .collect::<Vec<_>>();

        let array_count = spanned(expr_parser(), source_id);

        let bracket_content = just(TokenKind::LBracket)
            .ignore_then(seps)
            .ignore_then(
                nested_field_list
                    .map(FieldBracketContent::NestedFields)
                    .or(array_count.map(FieldBracketContent::Count)),
            )
            .then_ignore(just(TokenKind::RBracket))
            .or_not();

        spanned(
            chumsky::select! { TokenKind::Ident(name) if name.starts_with('.') => name },
            source_id,
        )
        .then(bracket_content)
        .then(data_width_parser().or_not())
        .map(|((name_spanned, bracket), data_width)| {
            let name = name_spanned
                .node
                .strip_prefix('.')
                .unwrap_or(&name_spanned.node)
                .to_string();
            let (count, count_span, nested_fields) = match bracket {
                Some(FieldBracketContent::Count(c)) => (Some(c.node), Some(c.span), None),
                Some(FieldBracketContent::NestedFields(fields)) => (None, None, Some(fields)),
                None => (None, None, None),
            };
            SymbolicSubscriptFieldDecl {
                name,
                data_width,
                count,
                count_span,
                nested_fields,
                span: name_spanned.span,
            }
        })
        .boxed()
    });

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

#[derive(Debug, Clone)]
enum FieldBracketContent {
    Count(Spanned<Expr>),
    NestedFields(Vec<SymbolicSubscriptFieldDecl>),
}

pub(in super::super) fn prefix_condition_parser<'src, I>()
-> impl chumsky::Parser<'src, I, (&'static str, HlaBranchForm), ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    let c_flag =
        chumsky::select! { TokenKind::Ident(value) if value.eq_ignore_ascii_case("c") => () }
            .ignore_then(
                just(TokenKind::Plus)
                    .then_ignore(just(TokenKind::Question))
                    .to(("bcc", HlaBranchForm::FlagQuestion))
                    .or(just(TokenKind::Minus)
                        .then_ignore(just(TokenKind::Question))
                        .to(("bcs", HlaBranchForm::FlagQuestion))),
            );

    let z_flag =
        chumsky::select! { TokenKind::Ident(value) if value.eq_ignore_ascii_case("z") => () }
            .ignore_then(
                just(TokenKind::Plus)
                    .then_ignore(just(TokenKind::Question))
                    .to(("bne", HlaBranchForm::FlagQuestion))
                    .or(just(TokenKind::Minus)
                        .then_ignore(just(TokenKind::Question))
                        .to(("beq", HlaBranchForm::FlagQuestion))),
            );

    let n_flag =
        chumsky::select! { TokenKind::Ident(value) if value.eq_ignore_ascii_case("n") => () }
            .ignore_then(
                just(TokenKind::Plus)
                    .then_ignore(just(TokenKind::Question))
                    .to(("bpl", HlaBranchForm::FlagQuestion))
                    .or(just(TokenKind::Minus)
                        .then_ignore(just(TokenKind::Question))
                        .to(("bmi", HlaBranchForm::FlagQuestion))),
            );

    let v_flag =
        chumsky::select! { TokenKind::Ident(value) if value.eq_ignore_ascii_case("v") || value.eq_ignore_ascii_case("o") => () }
            .ignore_then(
                just(TokenKind::Plus)
                    .then_ignore(just(TokenKind::Question))
                    .to(("bvc", HlaBranchForm::FlagQuestion))
                    .or(just(TokenKind::Minus)
                        .then_ignore(just(TokenKind::Question))
                        .to(("bvs", HlaBranchForm::FlagQuestion))),
            );

    let cmp_based = just(TokenKind::GtEq)
        .ignore_then(zero_number_token().or_not())
        .map(|zero| {
            if zero.is_some() {
                ("bmi", HlaBranchForm::Symbolic)
            } else {
                ("bcc", HlaBranchForm::Symbolic)
            }
        })
        .or(just(TokenKind::Lt)
            .ignore_then(zero_number_token().or_not())
            .map(|zero| {
                if zero.is_some() {
                    ("bpl", HlaBranchForm::Symbolic)
                } else {
                    ("bcs", HlaBranchForm::Symbolic)
                }
            }))
        .or(just(TokenKind::EqEq).to(("bne", HlaBranchForm::Symbolic)))
        .or(just(TokenKind::BangEq).to(("beq", HlaBranchForm::Symbolic)))
        .or(just(TokenKind::LtLtEq).to(("bvc", HlaBranchForm::Symbolic)))
        .or(just(TokenKind::GtGtEq).to(("bvs", HlaBranchForm::Symbolic)));

    c_flag.or(z_flag).or(n_flag).or(v_flag).or(cmp_based)
}
