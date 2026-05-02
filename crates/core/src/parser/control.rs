use crate::ast::{CallStmt, Expr, HlaBranchForm, HlaStmt, Instruction, NumFmt, Operand, Stmt};
use crate::lexer::TokenKind;
use chumsky::{
    Parser as _,
    error::Rich,
    input::ValueInput,
    prelude::{SimpleSpan, choice, just},
};

use super::{
    ParseExtra, expr_parser, ident_parser, line_tail_parser, zero_number_token,
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

    let goto_stmt = goto_indirect.or(goto_direct).boxed();

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
    })
    .boxed();

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

    let symbolic_branch_goto_stmt = choice((
        v_flag_goto,
        signed_goto,
        overflow_goto,
        just(TokenKind::Lt).to(("bcc", HlaBranchForm::Symbolic)),
        just(TokenKind::GtEq).to(("bcs", HlaBranchForm::Symbolic)),
        just(TokenKind::EqEq).to(("beq", HlaBranchForm::Symbolic)),
        just(TokenKind::BangEq).to(("bne", HlaBranchForm::Symbolic)),
    ))
    .then_ignore(goto_kw)
    .then(expr_parser())
    .map(|((mnemonic, form), target)| {
        Stmt::Hla(HlaStmt::BranchGoto {
            mnemonic: mnemonic.to_string(),
            target,
            form,
        })
    })
    .boxed();

    let branch_goto_stmt = question_flag_goto.or(symbolic_branch_goto_stmt).boxed();

    let return_stmt = chumsky::select! {
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("return") || value.eq_ignore_ascii_case("return_i") => value
    }
    .map(|keyword| Stmt::Hla(HlaStmt::Return {
        interrupt: keyword.eq_ignore_ascii_case("return_i"),
    }))
    .boxed();

    let far_goto_stmt = just(TokenKind::Far)
        .ignore_then(goto_kw)
        .ignore_then(expr_parser())
        .map(|target| {
            Stmt::Hla(HlaStmt::Goto {
                target,
                indirect: false,
                far: true,
            })
        })
        .boxed();

    let far_call_stmt = just(TokenKind::Far)
        .ignore_then(ident_parser())
        .map(|target| {
            Stmt::Call(CallStmt {
                target,
                is_far: true,
                args: Vec::new(),
                outputs: Vec::new(),
                is_bare: false,
            })
        })
        .boxed();

    let break_kw =
        chumsky::select! { TokenKind::Ident(v) if v.eq_ignore_ascii_case("break") => () };
    let repeat_kw =
        chumsky::select! { TokenKind::Ident(v) if v.eq_ignore_ascii_case("repeat") => () };

    let branch_condition = choice((
        just(TokenKind::Lt)
            .ignore_then(zero_number_token().or_not())
            .map(|zero| if zero.is_some() { "bmi" } else { "bcc" }),
        just(TokenKind::GtEq)
            .ignore_then(zero_number_token().or_not())
            .map(|zero| if zero.is_some() { "bpl" } else { "bcs" }),
        just(TokenKind::EqEq).to("beq"),
        just(TokenKind::BangEq).to("bne"),
        just(TokenKind::LtLtEq).to("bvs"),
        just(TokenKind::GtGtEq).to("bvc"),
    ));

    let conditional_break = branch_condition.clone().then_ignore(break_kw).map(|m| {
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

    let break_repeat_stmt = choice((
        conditional_break,
        conditional_repeat,
        unconditional_break,
        unconditional_repeat,
    ))
    .boxed();

    choice((
        goto_stmt,
        branch_goto_stmt,
        return_stmt,
        far_goto_stmt,
        far_call_stmt,
        break_repeat_stmt,
    ))
    .boxed()
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
    .boxed()
}

pub(super) fn nop_stmt_parser<'src, I>()
-> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    just(TokenKind::Star)
        .ignore_then(expr_parser().or_not())
        .map(|count| {
            Stmt::Hla(HlaStmt::RepeatInstruction {
                mnemonic: "nop".to_string(),
                count: count.unwrap_or(Expr::Number(1, NumFmt::Dec)),
            })
        })
        .or(just(TokenKind::Percent).to(Stmt::Instruction(Instruction {
            mnemonic: "brk".to_string(),
            operand: Some(Operand::Immediate {
                expr: Expr::Number(0, NumFmt::Dec),
                explicit_hash: true,
            }),
        })))
        .boxed()
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

    let operator = choice((
        just(TokenKind::EqEq).to("=="),
        just(TokenKind::BangEq).to("!="),
        just(TokenKind::LtLtEq).to("<<="),
        just(TokenKind::GtGtEq).to(">>="),
        just(TokenKind::LtEq).to("<="),
        just(TokenKind::GtEq).to(">="),
        just(TokenKind::Lt).to("<"),
        just(TokenKind::Gt).to(">"),
    ));

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

    choice((preprocessor, data_keyword, generic)).boxed()
}
