use indexmap::IndexMap;
use k816_eval::{EvalContext, EvalError as EvaluatorError, Number};

use crate::ast::{
    CodeBlock, ConstDecl, DataBlock, DataEntry, DataForEvalRange, DataWidth, EvaluatorBlock, Expr,
    ExprBinaryOp, ExprUnaryOp, File, Item, ModeContract, Stmt, SymbolicSubscriptFieldDecl, VarDecl,
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

pub(crate) use self::analysis::analyze_with_externals;
pub use self::analysis::{analyze, analyze_partial};
use self::consts::{
    ConstEvalCtx, ConstExprError, addr_of_nonfoldable_diagnostic, collect_const,
    collect_data_block_array, collect_data_block_labels, collect_evaluator_block,
    eval_const_expr_to_int, is_symbol_available,
};
use self::functions::collect_function;
pub use self::model::{
    AnalysisExternals, ConstMeta, ExternalVarClass, FunctionMeta, LabelMeta, SemanticModel,
    SymbolicSubscriptFieldMeta, SymbolicSubscriptMeta, VarMeta, VarPlacement,
};
use self::vars::collect_var;
