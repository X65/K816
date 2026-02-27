use crate::ast::{CallStmt, HlaBranchForm, HlaStmt, Stmt};
use crate::lexer::TokenKind;
use chumsky::{
    Parser as _,
    error::Rich,
    input::ValueInput,
    prelude::{SimpleSpan, just},
};

use super::{
    ParseExtra, expr_parser, ident_parser, line_tail_parser, number_parser, zero_number_token,
};

pub(super) fn flow_stmt_parser<'src, I>()
-> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    let goto_kw = chumsky::select! {
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("goto") => ()
    };

    let goto_indirect = goto_kw
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
    .then_ignore(goto_kw)
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

    let v_flag_goto =
        chumsky::select! { TokenKind::Ident(value) if value.eq_ignore_ascii_case("v") || value.eq_ignore_ascii_case("o") => () }
            .ignore_then(
                just(TokenKind::Plus)
                    .to(("bvs", HlaBranchForm::FlagPlain))
                    .or(just(TokenKind::Minus).to(("bvc", HlaBranchForm::FlagPlain))),
            );

    let signed_goto = just(TokenKind::Lt)
        .ignore_then(zero_number_token())
        .to(("bmi", HlaBranchForm::Symbolic))
        .or(just(TokenKind::GtEq)
            .ignore_then(zero_number_token())
            .to(("bpl", HlaBranchForm::Symbolic)));

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
        .then_ignore(goto_kw)
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
        .ignore_then(goto_kw)
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
        .then_ignore(break_kw)
        .map(|m| {
            Stmt::Hla(HlaStmt::LoopBreak {
                mnemonic: m.to_string(),
            })
        });

    let conditional_repeat = branch_condition.then_ignore(repeat_kw).map(|m| {
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

pub(super) fn invalid_flag_goto_stmt_parser<'src, I>()
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

pub(super) fn nop_stmt_parser<'src, I>()
-> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    just(TokenKind::Star)
        .ignore_then(number_parser().map(|n| n.value).or_not())
        .map(|count| Stmt::Hla(HlaStmt::RepeatNop(count.unwrap_or(1) as usize)))
        .or(just(TokenKind::Percent).to(Stmt::Hla(HlaStmt::RepeatNop(1))))
}

pub(super) fn discard_stmt_parser<'src, I>()
-> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
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
