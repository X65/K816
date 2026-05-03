pub(crate) const DEFAULT_SEGMENT: &str = "default";

pub mod ast;
pub mod diag;
pub mod driver;
pub mod emit_object;
pub mod eval_expand;
pub mod fold_mode;
pub mod hir;
pub mod lexer;
pub mod link_diag;
pub mod lower;
pub mod normalize_hla;
pub mod parser;
pub mod peephole;
pub mod sema;
pub mod span;

pub use driver::{
    CompileError, CompileFrontendOutput, CompileObjectOutput, CompileRenderOptions,
    LinkCompileInput, WorkspaceExternals, collect_all_declared_function_names,
    collect_workspace_externals, compile_source, compile_source_with_fs, compile_sources,
    compile_sources_all_or_nothing, compile_sources_all_or_nothing_keeping_map,
    compile_sources_keeping_map, compile_sources_with_externals,
};
pub use emit_object::AddressableSite;
pub use link_diag::{link_diagnostic_to_diagnostic, render_link_errors};
