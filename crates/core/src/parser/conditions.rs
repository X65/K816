use crate::ast::{HlaCompareOp, HlaCondition, HlaRegister, HlaStmt, Stmt};
use crate::lexer::TokenKind;
use chumsky::{
    Parser as _,
    error::Rich,
    input::ValueInput,
    prelude::{SimpleSpan, just},
};

use super::{ParseExtra, expr_parser, ident_parser, operand_expr_parser, zero_number_token};

pub(super) fn hla_compare_op_parser<'src, I>()
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

pub(super) fn hla_flag_close_stmt_parser<'src, I>()
-> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    let do_close_branch = |mnemonic: &str| {
        Stmt::Hla(HlaStmt::DoCloseBranch {
            mnemonic: mnemonic.to_string(),
        })
    };

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

    let v_flag =
        chumsky::select! { TokenKind::Ident(value) if value.eq_ignore_ascii_case("v") || value.eq_ignore_ascii_case("o") => () }
            .ignore_then(
                just(TokenKind::Plus)
                    .to(do_close_branch("bvs"))
                    .or(just(TokenKind::Minus).to(do_close_branch("bvc"))),
            );

    let signed_cmp = just(TokenKind::Lt)
        .ignore_then(zero_number_token())
        .to(do_close_branch("bmi"))
        .or(just(TokenKind::GtEq)
            .ignore_then(zero_number_token())
            .to(do_close_branch("bpl")));

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

pub(super) fn hla_condition_seed_stmt_parser<'src, I>()
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

pub(super) fn hla_condition_parser<'src, I>()
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
        .map(|((lhs, rhs), op)| HlaCondition {
            lhs,
            op,
            rhs,
            seed_span: None,
        })
}
