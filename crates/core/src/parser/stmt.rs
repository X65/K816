use crate::ast::{
    CallArg, CallStmt, HlaStmt, Instruction, LabelDecl, ModeContract, Operand, OperandAddrMode,
    RegName, RegWidth, SegmentDecl, Stmt,
};
use crate::lexer::{NumLit, TokenKind};
use crate::span::{SourceId, Spanned};
use chumsky::{
    IterParser, Parser as _,
    error::Rich,
    input::ValueInput,
    prelude::{SimpleSpan, choice, end, just},
};
use std::collections::HashSet;
use std::sync::Arc;

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
use super::registers::{parse_contract_register, parse_index_register};

pub(super) fn zero_number_token<'src, I>()
-> impl chumsky::Parser<'src, I, (), ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    chumsky::select! { TokenKind::Number(NumLit { value: 0, .. }) => () }.boxed()
}

pub(super) fn stmt_parser<'src, I>(
    source_id: SourceId,
    known_functions: Arc<HashSet<String>>,
) -> impl chumsky::Parser<'src, I, Stmt, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    let segment_stmt = just(TokenKind::Segment)
        .ignore_then(ident_parser())
        .map(|name| Stmt::Segment(SegmentDecl { name }))
        .boxed();

    let var_stmt = var_decl_parser(source_id).map(Stmt::Var).boxed();

    let data_stmt = just(TokenKind::Data)
        .ignore_then(data_block_parser(source_id))
        .map(Stmt::DataBlock)
        .boxed();

    let address_stmt = just(TokenKind::Address)
        .ignore_then(expr_parser())
        .try_map(|value, span| {
            let value = eval_static_expr(&value)
                .ok_or_else(|| Rich::custom(span, "address value must be a constant expression"))?;
            u32::try_from(value)
                .map(Stmt::Address)
                .map_err(|_| Rich::custom(span, "address value must fit in u32"))
        })
        .boxed();

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
        })
        .boxed();

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
        })
        .boxed();

    let call_stmt = just(TokenKind::Call)
        .ignore_then(just(TokenKind::Far).or_not())
        .then(ident_parser())
        .map(|(far, target)| {
            Stmt::Call(CallStmt {
                target,
                is_far: far.is_some(),
                args: Vec::new(),
                outputs: Vec::new(),
                is_bare: false,
            })
        })
        .boxed();

    let label_stmt = ident_parser()
        .then_ignore(just(TokenKind::Colon))
        .map(|name| Stmt::Label(LabelDecl { name }))
        .boxed();

    let call_reg = ident_parser().try_map(|name, span| {
        parse_contract_register(&name).ok_or_else(|| {
            Rich::custom(
                span,
                format!("expected call register 'a', 'x', or 'y', found '{name}'"),
            )
        })
    });
    let call_arg = just(TokenKind::Hash)
        .ignore_then(expr_parser())
        .map(CallArg::Immediate)
        .or(
            ident_parser().map(|name| match parse_contract_register(&name) {
                Some(reg) => CallArg::Register(reg),
                None => CallArg::Alias(name),
            }),
        );
    let output_regs = call_reg
        .clone()
        .separated_by(just(TokenKind::Comma))
        .at_least(1)
        .collect::<Vec<RegName>>();
    let bare_call_outputs = just(TokenKind::Arrow).ignore_then(output_regs).or_not();
    let bare_call_args = just(TokenKind::LParen)
        .ignore_then(
            call_arg
                .clone()
                .separated_by(just(TokenKind::Comma))
                .collect::<Vec<_>>(),
        )
        .then_ignore(just(TokenKind::RParen))
        .or(call_arg
            .clone()
            .separated_by(just(TokenKind::Comma))
            .at_least(1)
            .collect::<Vec<_>>());
    let function_target = {
        ident_parser().try_map(move |target, span| {
            if known_functions.contains(&target) {
                Ok(target)
            } else {
                Err(Rich::custom(
                    span,
                    format!("'{target}' is not a declared function"),
                ))
            }
        })
    };
    let bare_call_stmt = function_target
        .then(bare_call_args.or_not())
        .then(bare_call_outputs)
        .map(|((target, args), outputs)| {
            Stmt::Call(CallStmt {
                target,
                is_far: false,
                args: args.unwrap_or_default(),
                outputs: outputs.unwrap_or_default(),
                is_bare: true,
            })
        })
        .boxed();

    let hla_wait_stmt = hla_wait_loop_stmt_parser();
    let hla_do_open_stmt = just(TokenKind::LBrace).to(Stmt::Hla(HlaStmt::DoOpen));
    let hla_do_close_suffix = choice((
        hla_condition_parser().map(|condition| Stmt::Hla(HlaStmt::DoClose { condition })),
        hla_flag_close_stmt_parser(),
        hla_compare_op_parser().map(|op| Stmt::Hla(HlaStmt::DoCloseWithOp { op })),
        chumsky::select! { TokenKind::Ident(value) if value.eq_ignore_ascii_case("always") => () }
            .to(Stmt::Hla(HlaStmt::DoCloseAlways)),
        chumsky::select! { TokenKind::Ident(value) if value.eq_ignore_ascii_case("never") => () }
            .to(Stmt::Hla(HlaStmt::DoCloseNever)),
    ))
    .boxed();
    let hla_do_close_stmt = just(TokenKind::RBrace)
        .then(hla_do_close_suffix.clone().or_not())
        .rewind()
        .try_map(|(_, suffix), span| suffix.ok_or_else(|| Rich::custom(span, "unexpected '}'")))
        .ignore_then(just(TokenKind::RBrace).ignore_then(hla_do_close_suffix))
        .boxed();
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

    let mode_set_stmt = choice((
        just(TokenKind::ModeA8).to(Stmt::ModeSet {
            a_width: Some(RegWidth::W8),
            i_width: None,
        }),
        just(TokenKind::ModeA16).to(Stmt::ModeSet {
            a_width: Some(RegWidth::W16),
            i_width: None,
        }),
        just(TokenKind::ModeI8).to(Stmt::ModeSet {
            a_width: None,
            i_width: Some(RegWidth::W8),
        }),
        just(TokenKind::ModeI16).to(Stmt::ModeSet {
            a_width: None,
            i_width: Some(RegWidth::W16),
        }),
    ))
    .boxed();

    let swap_ab_stmt = chumsky::select! {
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("b") || value.eq_ignore_ascii_case("a") => value
    }
    .then(just(TokenKind::SwapOp))
    .then(chumsky::select! {
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("a") || value.eq_ignore_ascii_case("b") => ()
    })
    .to(Stmt::SwapAB)
    .boxed();

    let mnemonic = ident_parser().try_map(|mnemonic, _span| Ok(mnemonic));

    let operand_boundary = choice((
        line_sep_parser().ignored(),
        just(TokenKind::RBrace).ignored(),
        end().ignored(),
    ))
    .rewind();

    let operand_index_trailer = just(TokenKind::Comma)
        .ignore_then(ident_parser())
        .try_map(|name, span| parse_index_register(Some(name), span).map(|reg| reg.unwrap()));

    let long_indirect_operand = just(TokenKind::LBracket)
        .ignore_then(expr_parser())
        .then_ignore(just(TokenKind::RBracket))
        .then(operand_index_trailer.clone().or_not())
        .try_map(|(expr, outer_index), span| {
            let addr_mode = match outer_index {
                None => OperandAddrMode::IndirectLong,
                Some(crate::ast::IndexRegister::Y) => OperandAddrMode::IndirectLongIndexedY,
                Some(_) => {
                    return Err(Rich::custom(
                        span,
                        "unsupported post-indirect-long index register, expected '[expr],y'",
                    ));
                }
            };
            Ok(Some(Operand::Value {
                expr,
                force_far: false,
                index: None,
                addr_mode,
            }))
        })
        .boxed();

    let parenthesized_operand = just(TokenKind::LParen)
        .ignore_then(expr_parser().then(operand_index_trailer.clone().or_not()))
        .then_ignore(just(TokenKind::RParen))
        .then(operand_index_trailer.clone().or_not())
        .try_map(|((expr, inner_index), outer_index), span| {
            let addr_mode = match (inner_index, outer_index) {
                (None, None) => OperandAddrMode::Indirect,
                (Some(crate::ast::IndexRegister::X), None) => OperandAddrMode::IndexedIndirectX,
                (None, Some(crate::ast::IndexRegister::Y)) => OperandAddrMode::IndirectIndexedY,
                (Some(crate::ast::IndexRegister::S), Some(crate::ast::IndexRegister::Y)) => {
                    OperandAddrMode::StackRelativeIndirectIndexedY
                }
                _ => {
                    return Err(Rich::custom(
                        span,
                        "invalid indirect operand: choose '(expr)', '(expr,x)', '(expr),y', or '(expr,s),y'",
                    ));
                }
            };
            Ok(Some(Operand::Value {
                expr,
                force_far: false,
                index: None,
                addr_mode,
            }))
        })
        .boxed();

    let direct_operand = just(TokenKind::Far)
        .or_not()
        .then(
            expr_parser().then(
                just(TokenKind::Comma)
                    .ignore_then(
                        // Try index register (,x / ,y / ,s) first
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
            Some(CommaTrailer::BlockMoveDst(dst)) => Some(Operand::BlockMove { src: expr, dst }),
            None => Some(Operand::Value {
                expr,
                force_far: force_far.is_some(),
                index: None,
                addr_mode: OperandAddrMode::Direct,
            }),
        })
        .boxed();
    let immediate_operand = just(TokenKind::Hash)
        .ignore_then(expr_parser())
        .map(|expr| {
            Some(Operand::Immediate {
                expr,
                explicit_hash: true,
            })
        })
        .boxed();
    let operand = choice((
        operand_boundary.to(None),
        immediate_operand,
        long_indirect_operand,
        parenthesized_operand,
        direct_operand,
    ));

    let instruction = mnemonic
        .then(operand)
        .map(|(mnemonic, operand)| Stmt::Instruction(Instruction { mnemonic, operand }))
        .boxed();

    let separators_inner = line_sep_parser().repeated();

    // Split into two `choice` groups so we stay under chumsky's 26-tuple
    // arity for `Choice`. The single `.or` between them is fine — the
    // expensive case is *long chains* of `Or`, not a single layer.
    let common_stmt_a = choice((
        mode_set_stmt,
        swap_ab_stmt,
        segment_stmt,
        var_stmt,
        data_stmt,
        address_stmt,
        align_stmt,
        nocross_stmt,
        call_stmt,
        invalid_flag_goto_stmt,
        hla_condition_seed_stmt,
        hla_x_increment_stmt,
        hla_x_assign_stmt,
        hla_store_from_a_stmt,
    ));
    let common_stmt_b = choice((
        chain_stmt,
        assign_stmt,
        store_stmt,
        alu_stmt,
        incdec_stmt,
        shift_stmt,
        flow_stmt,
        flag_stmt,
        stack_stmt,
        nop_stmt,
        discard_stmt,
        label_stmt,
        bare_call_stmt,
        instruction,
    ));
    let common_stmt = common_stmt_a.or(common_stmt_b).boxed();

    // Flat statement parsers (no brace-delimited blocks)
    let flat_stmt_inner = common_stmt.clone();

    let base_stmt = choice((
        common_stmt,
        hla_wait_stmt,
        hla_do_close_stmt,
        hla_do_open_stmt,
    ))
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
                .then_ignore(just(TokenKind::RBrace))
                .boxed();

            let else_body_inner = chumsky::select! {
                TokenKind::Ident(value) if value.eq_ignore_ascii_case("else") => ()
            }
            .ignore_then(block_body.clone())
            .or_not()
            .boxed();

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
                })
                .boxed();

            let mode_scoped_block = mode_annotation_parser()
                .filter(|c: &ModeContract| c.a_width.is_some() || c.i_width.is_some())
                .then(block_body.clone())
                .map(|(contract, body)| Stmt::ModeScopedBlock {
                    a_width: contract.a_width,
                    i_width: contract.i_width,
                    body,
                })
                .boxed();

            let never_block = chumsky::select! {
                TokenKind::Ident(value) if value.eq_ignore_ascii_case("never") => ()
            }
            .ignore_then(block_body)
            .map(|body| Stmt::Hla(HlaStmt::NeverBlock { body }))
            .boxed();

            let inner_stmt = choice((
                mode_scoped_block,
                prefix_conditional,
                never_block,
                flat_stmt_inner,
            ))
            .boxed();

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
        .then_ignore(just(TokenKind::RBrace))
        .boxed();

    let else_body_top = chumsky::select! {
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("else") => ()
    }
    .ignore_then(block_body_top.clone())
    .or_not()
    .boxed();

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
        })
        .boxed();

    let mode_scoped_block_top = mode_annotation_parser()
        .filter(|c: &ModeContract| c.a_width.is_some() || c.i_width.is_some())
        .then(block_body_top.clone())
        .map(|(contract, body)| Stmt::ModeScopedBlock {
            a_width: contract.a_width,
            i_width: contract.i_width,
            body,
        })
        .boxed();

    let never_block_top = chumsky::select! {
        TokenKind::Ident(value) if value.eq_ignore_ascii_case("never") => ()
    }
    .ignore_then(block_body_top)
    .map(|body| Stmt::Hla(HlaStmt::NeverBlock { body }))
    .boxed();

    choice((
        mode_scoped_block_top,
        prefix_conditional_top,
        never_block_top,
        base_stmt,
    ))
    .boxed()
}
