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

    let long_indirect = just(TokenKind::LBracket)
        .ignore_then(expr_parser())
        .then_ignore(just(TokenKind::RBracket))
        .then(index.clone())
        .try_map(|(expr, outer_index), span| {
            let addr_mode = match outer_index {
                None => OperandAddrMode::IndirectLong,
                Some(IndexRegister::Y) => OperandAddrMode::IndirectLongIndexedY,
                Some(_) => {
                    return Err(Rich::custom(
                        span,
                        "unsupported post-indirect-long index register, expected '[expr],y'",
                    ));
                }
            };
            Ok(HlaOperandExpr {
                expr,
                index: None,
                addr_mode,
            })
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
                (Some(IndexRegister::S), Some(IndexRegister::Y)) => {
                    (None, OperandAddrMode::StackRelativeIndirectIndexedY)
                }
                (Some(IndexRegister::Y), None) => {
                    return Err(Rich::custom(
                        span,
                        "unsupported indirect index register, expected '(expr,x)' or '(expr,s),y'",
                    ));
                }
                (Some(IndexRegister::S), None) => {
                    return Err(Rich::custom(
                        span,
                        "stack-relative indirect requires trailing ',y': '(expr,s),y'",
                    ));
                }
                (None, Some(IndexRegister::X)) | (None, Some(IndexRegister::S)) => {
                    return Err(Rich::custom(
                        span,
                        "unsupported post-indirect index register, expected '(expr),y'",
                    ));
                }
                (Some(IndexRegister::X), Some(_)) | (Some(IndexRegister::S), Some(_)) => {
                    return Err(Rich::custom(
                        span,
                        "invalid indirect operand: choose '(expr,x)', '(expr),y', or '(expr,s),y'",
                    ));
                }
                (Some(IndexRegister::Y), Some(_)) => {
                    return Err(Rich::custom(
                        span,
                        "invalid indirect operand: choose '(expr,x)', '(expr),y', or '(expr,s),y'",
                    ));
                }
            };

            Ok(HlaOperandExpr {
                expr,
                index,
                addr_mode,
            })
        });

    long_indirect.or(parenthesized).or(plain)
}
