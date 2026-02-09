pub mod ast;
pub mod data_blocks;
pub mod diag;
pub mod driver;
pub mod emit;
pub mod emit_object;
pub mod eval_expand;
pub mod hir;
pub mod isa65816;
pub mod lexer;
pub mod lower;
pub mod parser;
pub mod sema;
pub mod span;

pub use driver::{
    CompileError, CompileObjectOutput, CompileOutput, compile_source, compile_source_to_object,
    compile_source_to_object_with_fs, compile_source_with_fs,
};
