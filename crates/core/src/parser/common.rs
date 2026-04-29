use crate::lexer::{NumLit, TokenKind};
use crate::span::{SourceId, Span, Spanned};
use chumsky::{
    IterParser, Parser as _,
    input::ValueInput,
    prelude::{SimpleSpan, any, choice, end, just},
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
    just(TokenKind::Newline).or(just(TokenKind::Semi)).boxed()
}

/// A statement/entry boundary: a line separator, a closing brace, or end-of-input.
///
/// Used both as a recovery sync point for `skip_then_retry_until` and (with
/// `.rewind()`) as a non-consuming lookahead at the end of a token-level statement.
pub(super) fn boundary_parser<'src, I>()
-> impl chumsky::Parser<'src, I, (), ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    choice((
        line_sep_parser().ignored(),
        just(TokenKind::RBrace).ignored(),
        end().ignored(),
    ))
    .boxed()
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
        .boxed()
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
