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
    CompileError, CompileObjectOutput, CompileRenderOptions, LinkCompileInput,
    collect_all_declared_function_names, collect_external_consts_for_link_sources,
    collect_external_functions_for_link_sources, collect_external_inline_bodies_for_link_sources,
    compile_source, compile_source_with_fs, compile_sources, compile_sources_all_or_nothing,
};
pub use emit_object::AddressableSite;
