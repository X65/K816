pub mod ast;
pub mod data_blocks;
pub mod diag;
pub mod driver;
pub mod emit;
pub mod eval_expand;
pub mod hir;
pub mod isa65816;
pub mod lexer;
pub mod lower;
pub mod parser;
pub mod sema;
pub mod span;

pub use driver::{CompileError, CompileOutput, compile_source, compile_source_with_fs};
