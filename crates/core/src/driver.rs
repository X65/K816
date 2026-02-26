use thiserror::Error;

use k816_assets::{AssetFS, StdAssetFS};
use k816_o65::O65Object;

use crate::diag::{Diagnostic, RenderOptions, render_diagnostics_with_options};
use crate::emit_object::{AddressableSite, emit_object};
use crate::eval_expand::expand_file;
use crate::fold_mode::{eliminate_dead_mode_ops, fold_mode_ops};
use crate::lower::lower;
use crate::peephole::peephole_optimize;
use crate::normalize_hla::normalize_file;
use crate::parser::parse_with_warnings;
use crate::sema::analyze;
use crate::span::SourceMap;

#[derive(Debug, Clone)]
pub struct CompileObjectOutput {
    pub object: O65Object,
    pub addressable_sites: Vec<AddressableSite>,
    pub warnings: Vec<Diagnostic>,
    pub rendered_warnings: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CompileRenderOptions {
    pub color: bool,
}

impl CompileRenderOptions {
    pub const fn plain() -> Self {
        Self { color: false }
    }

    pub const fn colored() -> Self {
        Self { color: true }
    }
}

#[derive(Debug, Error)]
#[error("compilation failed")]
pub struct CompileError {
    pub diagnostics: Vec<Diagnostic>,
    pub rendered: String,
}

pub fn compile_source_to_object(
    source_name: &str,
    source_text: &str,
) -> Result<CompileObjectOutput, CompileError> {
    compile_source_to_object_with_options(source_name, source_text, CompileRenderOptions::plain())
}

pub fn compile_source_to_object_with_options(
    source_name: &str,
    source_text: &str,
    options: CompileRenderOptions,
) -> Result<CompileObjectOutput, CompileError> {
    let fs = StdAssetFS;
    compile_source_to_object_with_fs_and_options(source_name, source_text, &fs, options)
}

/// Alias for `compile_source_to_object` (kept for backwards compatibility).
pub fn compile_source_to_object_for_link(
    source_name: &str,
    source_text: &str,
) -> Result<CompileObjectOutput, CompileError> {
    compile_source_to_object(source_name, source_text)
}

/// Alias for `compile_source_to_object_with_options` (kept for backwards compatibility).
pub fn compile_source_to_object_for_link_with_options(
    source_name: &str,
    source_text: &str,
    options: CompileRenderOptions,
) -> Result<CompileObjectOutput, CompileError> {
    compile_source_to_object_with_options(source_name, source_text, options)
}

pub fn compile_source_to_object_with_fs(
    source_name: &str,
    source_text: &str,
    fs: &dyn AssetFS,
) -> Result<CompileObjectOutput, CompileError> {
    compile_source_to_object_with_fs_and_options(
        source_name,
        source_text,
        fs,
        CompileRenderOptions::plain(),
    )
}

pub fn compile_source_to_object_with_fs_and_options(
    source_name: &str,
    source_text: &str,
    fs: &dyn AssetFS,
    options: CompileRenderOptions,
) -> Result<CompileObjectOutput, CompileError> {
    let mut source_map = SourceMap::default();
    let source_id = source_map.add_source(source_name, source_text);

    let parsed = parse_with_warnings(source_id, source_text)
        .map_err(|diagnostics| fail_with_rendered(&source_map, diagnostics, options))?;
    let ast = parsed.file;
    let warnings = parsed.warnings;
    let rendered_warnings = render_diagnostics_with_options(
        &source_map,
        &warnings,
        RenderOptions {
            color: options.color,
        },
    );

    let ast = expand_file(&ast, source_id)
        .map_err(|diagnostics| fail_with_rendered(&source_map, diagnostics, options))?;
    let ast = normalize_file(&ast)
        .map_err(|diagnostics| fail_with_rendered(&source_map, diagnostics, options))?;

    let sema = analyze(&ast)
        .map_err(|diagnostics| fail_with_rendered(&source_map, diagnostics, options))?;

    let hir = lower(&ast, &sema, fs)
        .map_err(|diagnostics| fail_with_rendered(&source_map, diagnostics, options))?;
    let hir = eliminate_dead_mode_ops(&hir);
    let hir = fold_mode_ops(&hir);
    let hir = peephole_optimize(&hir);

    let emit_output = emit_object(&hir, &source_map)
        .map_err(|diagnostics| fail_with_rendered(&source_map, diagnostics, options))?;

    Ok(CompileObjectOutput {
        object: emit_output.object,
        addressable_sites: emit_output.addressable_sites,
        warnings,
        rendered_warnings,
    })
}

/// Alias for `compile_source_to_object_with_fs` (kept for backwards compatibility).
pub fn compile_source_to_object_for_link_with_fs(
    source_name: &str,
    source_text: &str,
    fs: &dyn AssetFS,
) -> Result<CompileObjectOutput, CompileError> {
    compile_source_to_object_with_fs(source_name, source_text, fs)
}

/// Alias for `compile_source_to_object_with_fs_and_options` (kept for backwards compatibility).
pub fn compile_source_to_object_for_link_with_fs_and_options(
    source_name: &str,
    source_text: &str,
    fs: &dyn AssetFS,
    options: CompileRenderOptions,
) -> Result<CompileObjectOutput, CompileError> {
    compile_source_to_object_with_fs_and_options(source_name, source_text, fs, options)
}

fn fail_with_rendered(
    source_map: &SourceMap,
    diagnostics: Vec<Diagnostic>,
    options: CompileRenderOptions,
) -> CompileError {
    let rendered = render_diagnostics_with_options(
        source_map,
        &diagnostics,
        RenderOptions {
            color: options.color,
        },
    );
    CompileError {
        diagnostics,
        rendered,
    }
}

#[cfg(test)]
mod tests {
    use super::compile_source_to_object;

    #[test]
    fn keeps_diagnostic_spans_aligned_after_eval_block_preprocess() {
        let source = "[\n  A = 1,\n  B = 2,\n  C = (A + B) * 4\n]\n\nfunc main {\n  ldy #[J]\n}\n";
        let error = compile_source_to_object("test.k65", source).expect_err("must fail");
        let diagnostic = error
            .diagnostics
            .iter()
            .find(|diagnostic| diagnostic.message.contains("unknown identifier 'J'"))
            .expect("expected unknown J diagnostic");

        assert_eq!(
            &source[diagnostic.primary.start..diagnostic.primary.end],
            "J"
        );
    }
}
