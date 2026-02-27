use crate::lexer::{NumLit, TokenKind};
use crate::span::{SourceId, Span, Spanned};
use chumsky::{
    IterParser, Parser as _,
    input::ValueInput,
    prelude::{SimpleSpan, any, just},
};

use super::ParseExtra;

pub(super) fn simple_span_to_ast_span(source_id: SourceId, span: SimpleSpan) -> Span {
    let range = span.into_range();
    Span::new(source_id, range.start, range.end)
}

pub(super) fn line_sep_parser<'src, I>()
-> impl chumsky::Parser<'src, I, TokenKind, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    just(TokenKind::Newline).or(just(TokenKind::Semi))
}

pub(super) fn line_tail_parser<'src, I>()
-> impl chumsky::Parser<'src, I, Vec<TokenKind>, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    any()
        .filter(|token: &TokenKind| !matches!(token, TokenKind::Newline | TokenKind::Semi))
        .repeated()
        .collect::<Vec<_>>()
}

pub(super) fn ident_parser<'src, I>()
-> impl chumsky::Parser<'src, I, String, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    chumsky::select! { TokenKind::Ident(value) => value }.boxed()
}

pub(super) fn number_parser<'src, I>()
-> impl chumsky::Parser<'src, I, NumLit, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    chumsky::select! { TokenKind::Number(n) => n }.boxed()
}

pub(super) fn spanned<'src, I, T, P>(
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
