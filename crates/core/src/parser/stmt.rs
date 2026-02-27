use crate::ast::{
    CallStmt, HlaStmt, Instruction, LabelDecl, ModeContract, Operand, OperandAddrMode, RegWidth,
    SegmentDecl, Stmt,
};
use crate::lexer::{NumLit, TokenKind};
use crate::span::{SourceId, Spanned};
use chumsky::{
    IterParser, Parser as _,
    error::Rich,
    input::ValueInput,
    prelude::{SimpleSpan, end, just},
};

use super::ParseExtra;
use super::common::{ident_parser, line_sep_parser, spanned};
use super::conditions::{
    hla_compare_op_parser, hla_condition_parser, hla_condition_seed_stmt_parser,
    hla_flag_close_stmt_parser,
};
use super::control::{
    discard_stmt_parser, flow_stmt_parser, invalid_flag_goto_stmt_parser, nop_stmt_parser,
};
use super::data::{CommaTrailer, data_block_parser, prefix_condition_parser, var_decl_parser};
use super::expr::{eval_static_expr, expr_parser};
use super::items::mode_annotation_parser;
use super::operations::{
    alu_stmt_parser, assign_stmt_parser, chain_stmt_parser, flag_stmt_parser,
    hla_store_from_a_stmt_parser, hla_wait_loop_stmt_parser, hla_x_assign_stmt_parser,
    hla_x_increment_stmt_parser, incdec_stmt_parser, shift_stmt_parser, stack_stmt_parser,
    store_stmt_parser,
};
use super::registers::parse_index_register;

pub(super) fn zero_number_token<'src, I>()
-> impl chumsky::Parser<'src, I, (), ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    chumsky::select! { TokenKind::Number(NumLit { value: 0, .. }) => () }
}

pub(super) fn stmt_parser<'src, I>(
    source_id: SourceId,
) -> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    let segment_stmt = just(TokenKind::Segment)
        .ignore_then(ident_parser())
        .map(|name| Stmt::Segment(SegmentDecl { name }));

    let var_stmt = var_decl_parser(source_id).map(Stmt::Var);

    let data_stmt = just(TokenKind::Data)
        .ignore_then(data_block_parser(source_id))
        .map(Stmt::DataBlock);

    let address_stmt =
        just(TokenKind::Address)
            .ignore_then(expr_parser())
            .try_map(|value, span| {
                let value = eval_static_expr(&value).ok_or_else(|| {
                    Rich::custom(span, "address value must be a constant expression")
                })?;
                u32::try_from(value)
                    .map(Stmt::Address)
                    .map_err(|_| Rich::custom(span, "address value must fit in u32"))
            });

    let align_stmt = just(TokenKind::Align)
        .ignore_then(expr_parser())
        .then(just(TokenKind::Plus).ignore_then(expr_parser()).or_not())
        .try_map(|(value, offset), span| {
            let value = eval_static_expr(&value)
                .ok_or_else(|| Rich::custom(span, "align value must be a constant expression"))?;
            let boundary = u16::try_from(value)
                .map_err(|_| Rich::custom(span, "align value must fit in u16"))?;
            let offset = match offset {
                Some(offset_expr) => {
                    let offset_val = eval_static_expr(&offset_expr).ok_or_else(|| {
                        Rich::custom(span, "align offset must be a constant expression")
                    })?;
                    u16::try_from(offset_val)
                        .map_err(|_| Rich::custom(span, "align offset must fit in u16"))?
                }
                None => 0,
            };
            Ok(Stmt::Align { boundary, offset })
        });

    let nocross_stmt = just(TokenKind::Nocross)
        .ignore_then(expr_parser().or_not())
        .try_map(|value, span| {
            let value = match value {
                Some(value) => eval_static_expr(&value).ok_or_else(|| {
                    Rich::custom(span, "nocross value must be a constant expression")
                })?,
                None => 256,
            };
            u16::try_from(value)
                .map(Stmt::Nocross)
                .map_err(|_| Rich::custom(span, "nocross value must fit in u16"))
        });

    let call_stmt = just(TokenKind::Call)
        .ignore_then(just(TokenKind::Far).or_not())
        .then(ident_parser())
        .map(|(far, target)| {
            Stmt::Call(CallStmt {
                target,
                is_far: far.is_some(),
            })
        });

    let label_stmt = ident_parser()
        .then_ignore(just(TokenKind::Colon))
        .map(|name| Stmt::Label(LabelDecl { name }));

    let hla_wait_stmt = hla_wait_loop_stmt_parser();
    let hla_do_open_stmt = just(TokenKind::LBrace).to(Stmt::Hla(HlaStmt::DoOpen));
    let hla_do_close_suffix = hla_condition_parser()
        .map(|condition| Stmt::Hla(HlaStmt::DoClose { condition }))
        .or(hla_flag_close_stmt_parser())
        .or(hla_compare_op_parser().map(|op| Stmt::Hla(HlaStmt::DoCloseWithOp { op })))
        .or(
            chumsky::select! { TokenKind::Ident(value) if value.eq_ignore_ascii_case("always") => () }
                .to(Stmt::Hla(HlaStmt::DoCloseAlways)),
        )
        .or(
            chumsky::select! { TokenKind::Ident(value) if value.eq_ignore_ascii_case("never") => () }
                .to(Stmt::Hla(HlaStmt::DoCloseNever)),
        );
    let hla_do_close_stmt = just(TokenKind::RBrace)
        .then(hla_do_close_suffix.clone().or_not())
        .rewind()
        .try_map(|(_, suffix), span| suffix.ok_or_else(|| Rich::custom(span, "unexpected '}'")))
        .ignore_then(just(TokenKind::RBrace).ignore_then(hla_do_close_suffix));
    let hla_condition_seed_stmt = hla_condition_seed_stmt_parser();
    let hla_x_assign_stmt = hla_x_assign_stmt_parser();
    let hla_x_increment_stmt = hla_x_increment_stmt_parser();
    let hla_store_from_a_stmt = hla_store_from_a_stmt_parser();
    let assign_stmt = assign_stmt_parser();
    let store_stmt = store_stmt_parser();
    let alu_stmt = alu_stmt_parser();
    let incdec_stmt = incdec_stmt_parser();
    let shift_stmt = shift_stmt_parser();
    let flag_stmt = flag_stmt_parser();
    let stack_stmt = stack_stmt_parser();
    let flow_stmt = flow_stmt_parser();
    let invalid_flag_goto_stmt = invalid_flag_goto_stmt_parser();
    let nop_stmt = nop_stmt_parser();
    let chain_stmt = chain_stmt_parser();
    let discard_stmt = discard_stmt_parser();

    let mode_set_stmt = just(TokenKind::ModeA8)
        .to(Stmt::ModeSet {
            a_width: Some(RegWidth::W8),
            i_width: None,
        })
        .or(just(TokenKind::ModeA16).to(Stmt::ModeSet {
            a_width: Some(RegWidth::W16),
            i_width: None,
        }))
        .or(just(TokenKind::ModeI8).to(Stmt::ModeSet {
            a_width: None,
            i_width: Some(RegWidth::W8),
        }))
        .or(just(TokenKind::ModeI16).to(Stmt::ModeSet {
            a_width: None,
            i_width: Some(RegWidth::W16),
        }));

    let swap_ab_stmt = chumsky::select! {
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("b") || value.eq_ignore_ascii_case("a") => value
    }
    .then(just(TokenKind::SwapOp))
    .then(chumsky::select! {
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("a") || value.eq_ignore_ascii_case("b") => ()
    })
    .to(Stmt::SwapAB);

    let mnemonic = ident_parser().try_map(|mnemonic, _span| Ok(mnemonic));

    let operand_boundary = line_sep_parser()
        .ignored()
        .or(just(TokenKind::RBrace).ignored())
        .or(end().ignored())
        .rewind();

    let operand = operand_boundary
        .to(None)
        .or(just(TokenKind::Hash)
            .ignore_then(expr_parser())
            .map(|expr| {
                Some(Operand::Immediate {
                    expr,
                    explicit_hash: true,
                })
            }))
        .or(just(TokenKind::Far)
            .or_not()
            .then(
                expr_parser().then(
                    just(TokenKind::Comma)
                        .ignore_then(
                            // Try index register (,x / ,y) first
                            ident_parser()
                                .try_map(|name, span| {
                                    parse_index_register(Some(name), span)
                                        .map(|reg| CommaTrailer::Index(reg.unwrap()))
                                })
                                // Otherwise parse a second expression for block move (src,dst)
                                .or(expr_parser().map(CommaTrailer::BlockMoveDst)),
                        )
                        .or_not(),
                ),
            )
            .map(|(force_far, (expr, trailer))| match trailer {
                Some(CommaTrailer::Index(index)) => Some(Operand::Value {
                    expr,
                    force_far: force_far.is_some(),
                    index: Some(index),
                    addr_mode: OperandAddrMode::Direct,
                }),
                Some(CommaTrailer::BlockMoveDst(dst)) => {
                    Some(Operand::BlockMove { src: expr, dst })
                }
                None => Some(Operand::Value {
                    expr,
                    force_far: force_far.is_some(),
                    index: None,
                    addr_mode: OperandAddrMode::Direct,
                }),
            }));

    let instruction = mnemonic
        .then(operand)
        .map(|(mnemonic, operand)| Stmt::Instruction(Instruction { mnemonic, operand }));

    let separators_inner = line_sep_parser().repeated();

    let common_stmt = mode_set_stmt
        .or(swap_ab_stmt)
        .or(segment_stmt)
        .or(var_stmt)
        .or(data_stmt)
        .or(address_stmt)
        .or(align_stmt)
        .or(nocross_stmt)
        .or(call_stmt)
        .or(invalid_flag_goto_stmt)
        .or(hla_condition_seed_stmt)
        .or(hla_x_increment_stmt)
        .or(hla_x_assign_stmt)
        .or(hla_store_from_a_stmt)
        .or(chain_stmt)
        .or(assign_stmt)
        .or(store_stmt)
        .or(alu_stmt)
        .or(incdec_stmt)
        .or(shift_stmt)
        .or(flow_stmt)
        .or(flag_stmt)
        .or(stack_stmt)
        .or(nop_stmt)
        .or(discard_stmt)
        .or(label_stmt)
        .or(instruction)
        .boxed();

    // Flat statement parsers (no brace-delimited blocks)
    let flat_stmt_inner = common_stmt.clone();

    let base_stmt = common_stmt
        .or(hla_wait_stmt)
        .or(hla_do_close_stmt)
        .or(hla_do_open_stmt)
        .boxed();

    // Use recursive to allow mode_scoped_block and prefix_conditional
    // to nest inside each other's bodies
    let inner_stmt_recursive =
        chumsky::prelude::recursive::<_, Spanned<Stmt>, _, _, _>(|inner_stmt_ref| {
            let block_body = just(TokenKind::LBrace)
                .ignore_then(separators_inner.clone())
                .ignore_then(
                    inner_stmt_ref
                        .then_ignore(separators_inner.clone())
                        .repeated()
                        .collect::<Vec<_>>(),
                )
                .then_ignore(just(TokenKind::RBrace));

            let else_body_inner = chumsky::select! {
                TokenKind::Ident(value) if value.eq_ignore_ascii_case("else") => ()
            }
            .ignore_then(block_body.clone())
            .or_not();

            let prefix_conditional = prefix_condition_parser()
                .then(block_body.clone())
                .then(else_body_inner)
                .map(|(((skip_mnemonic, form), body), else_body)| {
                    Stmt::Hla(HlaStmt::PrefixConditional {
                        skip_mnemonic: skip_mnemonic.to_string(),
                        form,
                        body,
                        else_body,
                    })
                });

            let mode_scoped_block = mode_annotation_parser()
                .filter(|c: &ModeContract| c.a_width.is_some() || c.i_width.is_some())
                .then(block_body.clone())
                .map(|(contract, body)| Stmt::ModeScopedBlock {
                    a_width: contract.a_width,
                    i_width: contract.i_width,
                    body,
                });

            let never_block = chumsky::select! {
                TokenKind::Ident(value) if value.eq_ignore_ascii_case("never") => ()
            }
            .ignore_then(block_body)
            .map(|body| Stmt::Hla(HlaStmt::NeverBlock { body }));

            let inner_stmt = mode_scoped_block
                .or(prefix_conditional)
                .or(never_block)
                .or(flat_stmt_inner);

            spanned(inner_stmt, source_id)
        });

    // Top-level statement: mode_scoped_block, prefix_conditional, or base_stmt
    let block_body_top = just(TokenKind::LBrace)
        .ignore_then(separators_inner.clone())
        .ignore_then(
            inner_stmt_recursive
                .then_ignore(separators_inner)
                .repeated()
                .collect::<Vec<_>>(),
        )
        .then_ignore(just(TokenKind::RBrace));

    let else_body_top = chumsky::select! {
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("else") => ()
    }
    .ignore_then(block_body_top.clone())
    .or_not();

    let prefix_conditional_top = prefix_condition_parser()
        .then(block_body_top.clone())
        .then(else_body_top)
        .map(|(((skip_mnemonic, form), body), else_body)| {
            Stmt::Hla(HlaStmt::PrefixConditional {
                skip_mnemonic: skip_mnemonic.to_string(),
                form,
                body,
                else_body,
            })
        });

    let mode_scoped_block_top = mode_annotation_parser()
        .filter(|c: &ModeContract| c.a_width.is_some() || c.i_width.is_some())
        .then(block_body_top.clone())
        .map(|(contract, body)| Stmt::ModeScopedBlock {
            a_width: contract.a_width,
            i_width: contract.i_width,
            body,
        });

    let never_block_top = chumsky::select! {
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("never") => ()
    }
    .ignore_then(block_body_top)
    .map(|body| Stmt::Hla(HlaStmt::NeverBlock { body }));

    mode_scoped_block_top
        .or(prefix_conditional_top)
        .or(never_block_top)
        .or(base_stmt)
        .boxed()
}
