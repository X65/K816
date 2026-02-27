use indexmap::IndexMap;
use k816_eval::{EvalContext, EvalError as EvaluatorError, Number};

use crate::ast::{
    CodeBlock, ConstDecl, DataWidth, EvaluatorBlock, Expr, ExprBinaryOp, ExprUnaryOp, File, Item,
    ModeContract, NamedDataBlock, NamedDataEntry, NamedDataForEvalRange, Stmt,
    SymbolicSubscriptFieldDecl, VarDecl,
};
use crate::diag::Diagnostic;
use crate::span::Span;

mod analysis;
mod consts;
mod functions;
mod model;

#[cfg(test)]
mod tests;

mod vars;

pub use self::analysis::analyze;
use self::consts::{
    ConstExprError, collect_const, collect_evaluator_block, collect_named_data_block_array,
    eval_const_expr_to_int, is_symbol_available,
};
use self::functions::collect_function;
pub use self::model::{
    ConstMeta, FunctionMeta, SemanticModel, SymbolicSubscriptFieldMeta, SymbolicSubscriptMeta,
    VarMeta,
};
use self::vars::collect_var;
