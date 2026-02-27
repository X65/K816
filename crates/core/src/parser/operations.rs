use crate::ast::{
    Expr, HlaAluOp, HlaCpuRegister, HlaFlag, HlaIncDecOp, HlaIncDecTarget, HlaOperandExpr, HlaRhs,
    HlaShiftOp, HlaShiftTarget, HlaStmt, IndexRegister, OperandAddrMode, Stmt,
};
use crate::lexer::TokenKind;
use crate::span::SourceId;
use chumsky::{
    IterParser, Parser as _,
    error::Rich,
    input::ValueInput,
    prelude::{SimpleSpan, end, just},
};

use super::{
    ParseExtra, expr_parser, ident_parser, invalid_transfer_hint, is_register_name,
    line_sep_parser, operand_expr_parser, parse_cpu_register, parse_index_register,
    parse_stack_target, resolve_transfer, spanned,
};

pub(super) fn hla_wait_loop_stmt_parser<'src, I>()
-> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    just(TokenKind::LBrace)
        .ignore_then(ident_parser())
        .then_ignore(just(TokenKind::Amp))
        .then_ignore(just(TokenKind::Question))
        .then(ident_parser())
        .then_ignore(just(TokenKind::RBrace))
        .then(ident_parser())
        .then_ignore(just(TokenKind::Minus))
        .then_ignore(just(TokenKind::Question))
        .try_map(|((lhs, symbol), n_reg), span| {
            if !lhs.eq_ignore_ascii_case("a") {
                return Err(Rich::custom(
                    span,
                    format!("expected 'a' in wait-loop, found '{lhs}'"),
                ));
            }
            if !n_reg.eq_ignore_ascii_case("n") {
                return Err(Rich::custom(
                    span,
                    format!("expected 'n' in wait-loop suffix, found '{n_reg}'"),
                ));
            }
            Ok(Stmt::Hla(HlaStmt::WaitLoopWhileNFlagClear { symbol }))
        })
}

pub(super) fn hla_x_assign_stmt_parser<'src, I>()
-> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    chumsky::select! { TokenKind::Ident(value) if false => value }.map(|_: String| Stmt::Empty)
}

pub(super) fn hla_x_increment_stmt_parser<'src, I>()
-> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    ident_parser()
        .then_ignore(just(TokenKind::PlusPlus))
        .try_map(|ident, span| {
            if !ident.eq_ignore_ascii_case("x") {
                return Err(Rich::custom(
                    span,
                    format!("expected 'x++', found '{ident}++'"),
                ));
            }
            Ok(Stmt::Hla(HlaStmt::XIncrement))
        })
}

fn hla_store_rhs_parser<'src, I>() -> impl chumsky::Parser<'src, I, HlaRhs, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    let immediate = just(TokenKind::Hash)
        .ignore_then(expr_parser())
        .map(HlaRhs::Immediate);

    let value = operand_expr_parser().map(|parsed| {
        if parsed.addr_mode == OperandAddrMode::Direct
            && parsed.index.is_none()
            && !matches!(parsed.expr, Expr::Ident(_) | Expr::IdentSpanned { .. })
        {
            return HlaRhs::Immediate(parsed.expr);
        }

        HlaRhs::Value {
            expr: parsed.expr,
            index: parsed.index,
            addr_mode: parsed.addr_mode,
        }
    });

    immediate.or(value).boxed()
}

pub(super) fn hla_store_from_a_stmt_parser<'src, I>()
-> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    chumsky::select! { TokenKind::Ident(value) if !is_register_name(&value) => value }
        .then_ignore(just(TokenKind::Eq))
        .repeated()
        .at_least(1)
        .collect::<Vec<_>>()
        .then(spanned(ident_parser(), SourceId(0)))
        .then_ignore(just(TokenKind::Eq))
        .then(hla_store_rhs_parser())
        .try_map(|((dests, middle), rhs), span| {
            if !middle.node.eq_ignore_ascii_case("a") {
                return Err(Rich::custom(
                    span,
                    format!(
                        "expected 'a' in store-from-a assignment, found '{}'",
                        middle.node
                    ),
                ));
            }
            Ok(Stmt::Hla(HlaStmt::StoreFromA {
                dests,
                rhs,
                load_start: Some(middle.span.start),
                store_end: Some(middle.span.end),
            }))
        })
}

pub(super) fn chain_stmt_parser<'src, I>()
-> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    let stmt_boundary = line_sep_parser()
        .ignored()
        .or(just(TokenKind::RBrace).ignored())
        .or(end().ignored())
        .rewind();

    let all_ident = ident_parser()
        .then_ignore(just(TokenKind::Eq))
        .repeated()
        .at_least(2)
        .collect::<Vec<_>>()
        .then(ident_parser())
        .then_ignore(stmt_boundary)
        .map(|(prefix, last)| {
            let mut operands = prefix;
            operands.push(last);
            Stmt::Hla(HlaStmt::AssignmentChain {
                idents: operands,
                tail_expr: None,
            })
        });

    let with_expr = ident_parser()
        .then_ignore(just(TokenKind::Eq))
        .repeated()
        .at_least(2)
        .collect::<Vec<_>>()
        .then(operand_expr_parser())
        .map(|(idents, rhs)| {
            Stmt::Hla(HlaStmt::AssignmentChain {
                idents,
                tail_expr: Some(rhs),
            })
        });

    all_ident.or(with_expr)
}

pub(super) fn assign_stmt_parser<'src, I>()
-> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    let stmt_boundary = line_sep_parser()
        .ignored()
        .or(just(TokenKind::RBrace).ignored())
        .or(end().ignored())
        .rewind();

    let register_transfer = chumsky::select! {
        TokenKind::Ident(value) if is_register_name(&value) => value
    }
    .then_ignore(just(TokenKind::Eq))
    .then(chumsky::select! {
        TokenKind::Ident(value) if is_register_name(&value) => value
    })
    .then_ignore(stmt_boundary)
    .validate(|(lhs, rhs), extra, emitter| {
        let lhs_lc = lhs.to_ascii_lowercase();
        let rhs_lc = rhs.to_ascii_lowercase();
        if resolve_transfer(&lhs_lc, &rhs_lc).is_some() {
            return Stmt::Hla(HlaStmt::RegisterTransfer {
                dest: parse_cpu_register(&lhs_lc).expect("validated register"),
                src: parse_cpu_register(&rhs_lc).expect("validated register"),
            });
        }
        let rhs_upper = rhs_lc.to_ascii_uppercase();
        let lhs_upper = lhs_lc.to_ascii_uppercase();
        let msg = format!("transfer '{rhs_upper}' to '{lhs_upper}' is not directly supported");
        emitter.emit(Rich::custom(
            extra.span(),
            match invalid_transfer_hint(&lhs_lc, &rhs_lc) {
                Some(hint) => format!("{msg}; hint: {hint}"),
                None => msg,
            },
        ));
        Stmt::Empty
    });

    let register_load = chumsky::select! {
        TokenKind::Ident(value) if is_register_name(&value) => value
    }
    .then_ignore(just(TokenKind::Eq))
    .then(operand_expr_parser())
    .validate(|(lhs, parsed), extra, emitter| {
        let lhs = lhs.to_ascii_lowercase();
        let register = match lhs.as_str() {
            "a" => HlaCpuRegister::A,
            "x" => HlaCpuRegister::X,
            "y" => HlaCpuRegister::Y,
            "c" => {
                emitter.emit(Rich::custom(
                    extra.span(),
                    "C is the 16-bit accumulator; hint: use a=expr for loads",
                ));
                return Stmt::Empty;
            }
            _ => {
                let reg = lhs.to_ascii_uppercase();
                emitter.emit(Rich::custom(
                    extra.span(),
                    format!("cannot load register '{reg}' with expression"),
                ));
                return Stmt::Empty;
            }
        };
        Stmt::Hla(HlaStmt::RegisterAssign {
            register,
            rhs: parsed,
        })
    });

    register_transfer.or(register_load)
}

pub(super) fn store_stmt_parser<'src, I>()
-> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    operand_expr_parser()
        .then_ignore(just(TokenKind::Eq))
        .then(chumsky::select! {
            TokenKind::Ident(value) if value.eq_ignore_ascii_case("a")
                || value.eq_ignore_ascii_case("x")
                || value.eq_ignore_ascii_case("y") => value
        })
        .map(|(dest, rhs)| {
            let src = if rhs.eq_ignore_ascii_case("a") {
                HlaCpuRegister::A
            } else if rhs.eq_ignore_ascii_case("x") {
                HlaCpuRegister::X
            } else {
                HlaCpuRegister::Y
            };

            Stmt::Hla(HlaStmt::RegisterStore { dest, src })
        })
}

pub(super) fn alu_stmt_parser<'src, I>()
-> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    let a_bit = chumsky::select! {
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("a") => ()
    }
    .then_ignore(just(TokenKind::Amp))
    .then_ignore(just(TokenKind::Question))
    .ignore_then(operand_expr_parser())
    .map(|rhs| Stmt::Hla(HlaStmt::AccumulatorBitTest { rhs }));

    let a_alu = chumsky::select! {
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("a") => ()
    }
    .ignore_then(
        just(TokenKind::Plus)
            .to(HlaAluOp::Add)
            .or(just(TokenKind::Minus).to(HlaAluOp::Sub))
            .or(just(TokenKind::Amp).to(HlaAluOp::And))
            .or(just(TokenKind::Pipe).to(HlaAluOp::Or))
            .or(just(TokenKind::Caret).to(HlaAluOp::Xor)),
    )
    .then(operand_expr_parser())
    .map(|(op, rhs)| Stmt::Hla(HlaStmt::AccumulatorAlu { op, rhs }));

    let xy_cmp = chumsky::select! {
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("x") || value.eq_ignore_ascii_case("y") => value
    }
    .then_ignore(just(TokenKind::Question))
    .then(operand_expr_parser())
    .map(|(lhs, rhs)| {
        let register = if lhs.eq_ignore_ascii_case("x") {
            IndexRegister::X
        } else {
            IndexRegister::Y
        };
        Stmt::Hla(HlaStmt::IndexCompare { register, rhs })
    });

    a_bit.or(a_alu).or(xy_cmp)
}

pub(super) fn incdec_stmt_parser<'src, I>()
-> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    let indexed_inc = ident_parser()
        .then_ignore(just(TokenKind::Comma))
        .then(ident_parser())
        .then_ignore(just(TokenKind::PlusPlus))
        .try_map(|(base, idx), span| {
            let index = parse_index_register(Some(idx), span)?;
            Ok(Stmt::Hla(HlaStmt::IncDec {
                op: HlaIncDecOp::Inc,
                target: HlaIncDecTarget::Address(HlaOperandExpr {
                    expr: Expr::Ident(base),
                    index,
                    addr_mode: OperandAddrMode::Direct,
                }),
            }))
        });

    let indexed_dec = ident_parser()
        .then_ignore(just(TokenKind::Comma))
        .then(ident_parser())
        .then_ignore(just(TokenKind::Minus))
        .then_ignore(just(TokenKind::Minus))
        .try_map(|(base, idx), span| {
            let index = parse_index_register(Some(idx), span)?;
            Ok(Stmt::Hla(HlaStmt::IncDec {
                op: HlaIncDecOp::Dec,
                target: HlaIncDecTarget::Address(HlaOperandExpr {
                    expr: Expr::Ident(base),
                    index,
                    addr_mode: OperandAddrMode::Direct,
                }),
            }))
        });

    let inc = ident_parser()
        .then_ignore(just(TokenKind::PlusPlus))
        .map(|ident| {
            if ident.eq_ignore_ascii_case("x") {
                return Stmt::Hla(HlaStmt::IncDec {
                    op: HlaIncDecOp::Inc,
                    target: HlaIncDecTarget::Register(IndexRegister::X),
                });
            }
            if ident.eq_ignore_ascii_case("y") {
                return Stmt::Hla(HlaStmt::IncDec {
                    op: HlaIncDecOp::Inc,
                    target: HlaIncDecTarget::Register(IndexRegister::Y),
                });
            }
            Stmt::Hla(HlaStmt::IncDec {
                op: HlaIncDecOp::Inc,
                target: HlaIncDecTarget::Address(HlaOperandExpr {
                    expr: Expr::Ident(ident),
                    index: None,
                    addr_mode: OperandAddrMode::Direct,
                }),
            })
        });

    let dec = ident_parser()
        .then_ignore(just(TokenKind::Minus))
        .then_ignore(just(TokenKind::Minus))
        .map(|ident| {
            if ident.eq_ignore_ascii_case("x") {
                return Stmt::Hla(HlaStmt::IncDec {
                    op: HlaIncDecOp::Dec,
                    target: HlaIncDecTarget::Register(IndexRegister::X),
                });
            }
            if ident.eq_ignore_ascii_case("y") {
                return Stmt::Hla(HlaStmt::IncDec {
                    op: HlaIncDecOp::Dec,
                    target: HlaIncDecTarget::Register(IndexRegister::Y),
                });
            }
            Stmt::Hla(HlaStmt::IncDec {
                op: HlaIncDecOp::Dec,
                target: HlaIncDecTarget::Address(HlaOperandExpr {
                    expr: Expr::Ident(ident),
                    index: None,
                    addr_mode: OperandAddrMode::Direct,
                }),
            })
        });

    indexed_inc.or(indexed_dec).or(inc).or(dec)
}

pub(super) fn shift_stmt_parser<'src, I>()
-> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    let shift_op = just(TokenKind::Lt)
        .then_ignore(just(TokenKind::Lt))
        .then_ignore(just(TokenKind::Lt))
        .to(HlaShiftOp::Rol)
        .or(just(TokenKind::Gt)
            .then_ignore(just(TokenKind::Gt))
            .then_ignore(just(TokenKind::Gt))
            .to(HlaShiftOp::Ror))
        .or(just(TokenKind::Lt)
            .then_ignore(just(TokenKind::Lt))
            .to(HlaShiftOp::Asl))
        .or(just(TokenKind::Gt)
            .then_ignore(just(TokenKind::Gt))
            .to(HlaShiftOp::Lsr));

    let indexed = ident_parser()
        .then_ignore(just(TokenKind::Comma))
        .then(ident_parser())
        .then(shift_op.clone())
        .try_map(|((base, idx), op), span| {
            let index = parse_index_register(Some(idx), span)?;
            Ok(Stmt::Hla(HlaStmt::ShiftRotate {
                op,
                target: HlaShiftTarget::Address(HlaOperandExpr {
                    expr: Expr::Ident(base),
                    index,
                    addr_mode: OperandAddrMode::Direct,
                }),
            }))
        });

    let plain = ident_parser().then(shift_op).map(|(target, op)| {
        if target.eq_ignore_ascii_case("a") {
            Stmt::Hla(HlaStmt::ShiftRotate {
                op,
                target: HlaShiftTarget::Accumulator,
            })
        } else {
            Stmt::Hla(HlaStmt::ShiftRotate {
                op,
                target: HlaShiftTarget::Address(HlaOperandExpr {
                    expr: Expr::Ident(target),
                    index: None,
                    addr_mode: OperandAddrMode::Direct,
                }),
            })
        }
    });

    indexed.or(plain)
}

pub(super) fn flag_stmt_parser<'src, I>()
-> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    chumsky::select! { TokenKind::Ident(value) if value.chars().count() == 1 => value }
        .then(
            just(TokenKind::Plus)
                .to(true)
                .or(just(TokenKind::Minus).to(false)),
        )
        .try_map(|(flag, set), span| {
            let lower = flag.to_ascii_lowercase();
            let sign = if set { '+' } else { '-' };
            let shorthand = format!("{flag}{sign}");
            match (lower.as_str(), set) {
                ("c", true) => Ok(Stmt::Hla(HlaStmt::FlagSet {
                    flag: HlaFlag::Carry,
                    set: true,
                })),
                ("c", false) => Ok(Stmt::Hla(HlaStmt::FlagSet {
                    flag: HlaFlag::Carry,
                    set: false,
                })),
                ("d", true) => Ok(Stmt::Hla(HlaStmt::FlagSet {
                    flag: HlaFlag::Decimal,
                    set: true,
                })),
                ("d", false) => Ok(Stmt::Hla(HlaStmt::FlagSet {
                    flag: HlaFlag::Decimal,
                    set: false,
                })),
                ("i", true) => Ok(Stmt::Hla(HlaStmt::FlagSet {
                    flag: HlaFlag::Interrupt,
                    set: true,
                })),
                ("i", false) => Ok(Stmt::Hla(HlaStmt::FlagSet {
                    flag: HlaFlag::Interrupt,
                    set: false,
                })),
                ("v" | "o", false) => Ok(Stmt::Hla(HlaStmt::FlagSet {
                    flag: HlaFlag::Overflow,
                    set: false,
                })),
                _ => Err(Rich::custom(
                    span,
                    format!("unsupported flag shorthand '{shorthand}'"),
                )),
            }
        })
}

pub(super) fn stack_stmt_parser<'src, I>()
-> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    ident_parser()
        .then(
            just(TokenKind::Bang)
                .then_ignore(just(TokenKind::Bang))
                .to(true)
                .or(just(TokenKind::Question)
                    .then_ignore(just(TokenKind::Question))
                    .to(false)),
        )
        .validate(|(target_text, push), extra, emitter| {
            let Some(target) = parse_stack_target(&target_text) else {
                let suffix = if push { "!!" } else { "??" };
                emitter.emit(Rich::custom(
                    extra.span(),
                    format!("unsupported stack shorthand '{target_text}{suffix}'"),
                ));
                return Stmt::Empty;
            };
            Stmt::Hla(HlaStmt::StackOp { target, push })
        })
}
