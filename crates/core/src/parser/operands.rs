use crate::ast::{HlaOperandExpr, IndexRegister, OperandAddrMode};
use crate::lexer::TokenKind;
use chumsky::{
    Parser as _,
    error::Rich,
    input::ValueInput,
    prelude::{SimpleSpan, just},
};

use super::{ParseExtra, expr_parser, ident_parser, parse_index_register};

pub(super) fn operand_expr_parser<'src, I>()
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
