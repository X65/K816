pub mod ast;
pub mod data_blocks;
pub mod diag;
pub mod driver;
pub mod emit_object;
pub mod eval_expand;
pub mod fold_mode;
pub mod hir;
pub mod lexer;
pub mod lower;
pub mod normalize_hla;
pub mod parser;
pub mod peephole;
pub mod sema;
pub mod span;

pub use driver::{
    CompileError, CompileObjectOutput, CompileRenderOptions, LinkCompileInput, compile_source,
    compile_source_with_fs, compile_sources, compile_sources_all_or_nothing,
};
pub use emit_object::AddressableSite;
