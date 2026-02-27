use crate::lexer::TokenKind;
use chumsky::{error::Rich, extra};

#[cfg(test)]
use crate::ast::{
    DataArg, DataCommand, DataWidth, Expr, ExprBinaryOp, ExprUnaryOp, HlaBranchForm,
    HlaCpuRegister, HlaRegister, HlaStackTarget, HlaStmt, IndexRegister, Item, NamedDataEntry,
    NumFmt, OperandAddrMode, Stmt,
};
#[cfg(test)]
use crate::lexer::lex;
#[cfg(test)]
use crate::span::SourceId;
#[cfg(test)]
use chumsky::Parser as _;
#[cfg(test)]
use chumsky::error::RichReason;
#[cfg(test)]
use chumsky::input::{Input as _, Stream};

mod api;
mod common;
mod conditions;
mod control;
mod data;
mod diagnostics;
mod expr;
mod items;
mod operands;
mod operations;
mod preprocess;
mod registers;
mod stmt;
#[cfg(test)]
mod tests;
mod warnings;

pub use self::api::{
    ParseOutput, parse, parse_expression_fragment, parse_lenient, parse_lenient_raw,
    parse_with_warnings,
};
use self::common::{
    ident_parser, line_sep_parser, line_tail_parser, number_parser, simple_span_to_ast_span,
    spanned,
};
#[cfg(test)]
use self::control::invalid_flag_goto_stmt_parser;
use self::data::{
    const_decl_item_parser, data_block_parser, data_width_parser, named_data_block_parser,
    var_decl_parser,
};
use self::diagnostics::rich_error_to_diagnostic;
use self::expr::{
    eval_static_expr, expr_parser, expression_fragment_parser, is_ident_text, parse_eval_expr_token,
};
use self::items::file_parser;
use self::operands::operand_expr_parser;
use self::preprocess::{coalesce_non_var_brackets, preprocess_source, strip_comments};
use self::registers::{
    invalid_transfer_hint, is_register_name, parse_cpu_register, parse_index_register,
    parse_stack_target, resolve_transfer,
};
use self::stmt::{stmt_parser, zero_number_token};
use self::warnings::collect_parser_warnings;

type ParseError<'src> = Rich<'src, TokenKind>;
type ParseExtra<'src> = extra::Err<ParseError<'src>>;
