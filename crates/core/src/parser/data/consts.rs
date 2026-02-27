use crate::ast::{ConstDecl, Expr, ExprBinaryOp, Item, NumFmt};
use crate::lexer::TokenKind;
use crate::span::SourceId;
use chumsky::{IterParser, Parser as _, error::Rich, input::ValueInput, prelude::SimpleSpan};

use super::super::{ParseExtra, expr_parser, ident_parser, simple_span_to_ast_span};

#[derive(Debug, Clone)]
struct ConstDeclBinding {
    name: String,
    name_span: SimpleSpan,
    initializer: Option<(Expr, SimpleSpan)>,
}

fn const_decl_binding_parser<'src, I>()
-> impl chumsky::Parser<'src, I, ConstDeclBinding, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    ident_parser()
        .map_with(|name, extra| (name, extra.span()))
        .then(
            chumsky::primitive::just(TokenKind::Eq)
                .ignore_then(expr_parser().map_with(|expr, extra| (expr, extra.span())))
                .or_not(),
        )
        .map(|((name, name_span), initializer)| ConstDeclBinding {
            name,
            name_span,
            initializer,
        })
}

#[derive(Debug, Clone, Copy)]
enum ConstDesugarError {
    MissingInitializerForSingleDecl { span: SimpleSpan },
}

fn desugar_const_bindings(
    bindings: Vec<ConstDeclBinding>,
    source_id: SourceId,
) -> Result<Item, ConstDesugarError> {
    let is_group = bindings.len() > 1;
    let mut decls = Vec::with_capacity(bindings.len());
    let mut prev_name: Option<String> = None;

    for binding in bindings {
        let name = binding.name;
        let (initializer, initializer_span) = if let Some((expr, expr_span)) = binding.initializer {
            (expr, Some(simple_span_to_ast_span(source_id, expr_span)))
        } else if prev_name.is_none() {
            if !is_group {
                let name_range = binding.name_span.into_range();
                let missing_span: SimpleSpan = (name_range.end..name_range.end).into();
                return Err(ConstDesugarError::MissingInitializerForSingleDecl {
                    span: missing_span,
                });
            }
            (Expr::Number(0, NumFmt::Dec), None)
        } else {
            let previous = prev_name
                .clone()
                .expect("previous const name must exist for omitted initializer");
            (
                Expr::Binary {
                    op: ExprBinaryOp::Add,
                    lhs: Box::new(Expr::Ident(previous)),
                    rhs: Box::new(Expr::Number(1, NumFmt::Dec)),
                },
                None,
            )
        };

        prev_name = Some(name.clone());
        decls.push(ConstDecl {
            name,
            initializer,
            initializer_span,
        });
    }

    if is_group {
        Ok(Item::ConstGroup(decls))
    } else {
        let decl = decls
            .into_iter()
            .next()
            .expect("const declaration must contain one binding");
        Ok(Item::Const(decl))
    }
}

pub(in super::super) fn const_decl_item_parser<'src, I>(
    source_id: SourceId,
) -> impl chumsky::Parser<'src, I, Item, ParseExtra<'src>> + Clone
where
    I: ValueInput<'src, Token = TokenKind, Span = SimpleSpan>,
{
    let const_separator = chumsky::primitive::just(TokenKind::Comma)
        .then_ignore(chumsky::primitive::just(TokenKind::Newline).repeated());
    let bindings = const_decl_binding_parser()
        .then(
            const_separator
                .clone()
                .ignore_then(const_decl_binding_parser())
                .repeated()
                .collect::<Vec<_>>(),
        )
        .then_ignore(const_separator.or_not())
        .map(|(head, tail)| {
            let mut bindings = Vec::with_capacity(tail.len() + 1);
            bindings.push(head);
            bindings.extend(tail);
            bindings
        })
        .try_map(move |bindings, _span| {
            desugar_const_bindings(bindings, source_id).map_err(|error| match error {
                ConstDesugarError::MissingInitializerForSingleDecl { span } => Rich::custom(
                    span,
                    "const initializer can be omitted only in comma-separated groups",
                ),
            })
        });

    chumsky::primitive::just(TokenKind::Const).ignore_then(bindings)
}
