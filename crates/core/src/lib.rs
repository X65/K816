pub mod ast;
pub mod data_blocks;
pub mod diag;
pub mod driver;
pub mod emit;
pub mod emit_object;
pub mod eval_expand;
pub mod fold_mode;
pub mod hir;
pub mod lexer;
pub mod lower;
pub mod normalize_hla;
pub mod parser;
pub mod sema;
pub mod span;

pub use driver::{
    CompileError, CompileObjectOutput, CompileOutput, CompileRenderOptions, compile_source,
    compile_source_to_object, compile_source_to_object_for_link,
    compile_source_to_object_for_link_with_fs,
    compile_source_to_object_for_link_with_fs_and_options,
    compile_source_to_object_for_link_with_options, compile_source_to_object_with_fs,
    compile_source_to_object_with_fs_and_options, compile_source_to_object_with_options,
    compile_source_with_fs, compile_source_with_fs_and_options, compile_source_with_options,
};
