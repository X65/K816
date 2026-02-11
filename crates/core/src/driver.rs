use indexmap::IndexMap;
use thiserror::Error;

use k816_assets::{AssetFS, StdAssetFS};
use k816_o65::O65Object;

use crate::diag::{Diagnostic, RenderOptions, render_diagnostics_with_options};
use crate::emit::emit;
use crate::emit_object::emit_object;
use crate::eval_expand::expand_file;
use crate::lower::lower;
use crate::normalize_hla::normalize_file;
use crate::parser::parse_with_warnings;
use crate::sema::analyze;
use crate::span::SourceMap;

#[derive(Debug, Clone)]
pub struct CompileOutput {
    pub segments: IndexMap<String, Vec<u8>>,
    pub listing: String,
    pub warnings: Vec<Diagnostic>,
    pub rendered_warnings: String,
}

#[derive(Debug, Clone)]
pub struct CompileObjectOutput {
    pub object: O65Object,
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

pub fn compile_source(source_name: &str, source_text: &str) -> Result<CompileOutput, CompileError> {
    compile_source_with_options(source_name, source_text, CompileRenderOptions::plain())
}

pub fn compile_source_with_options(
    source_name: &str,
    source_text: &str,
    options: CompileRenderOptions,
) -> Result<CompileOutput, CompileError> {
    let fs = StdAssetFS;
    compile_source_with_fs_and_options(source_name, source_text, &fs, options)
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

pub fn compile_source_with_fs(
    source_name: &str,
    source_text: &str,
    fs: &dyn AssetFS,
) -> Result<CompileOutput, CompileError> {
    compile_source_with_fs_and_options(source_name, source_text, fs, CompileRenderOptions::plain())
}

pub fn compile_source_with_fs_and_options(
    source_name: &str,
    source_text: &str,
    fs: &dyn AssetFS,
    options: CompileRenderOptions,
) -> Result<CompileOutput, CompileError> {
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

    let emit_output =
        emit(&hir).map_err(|diagnostics| fail_with_rendered(&source_map, diagnostics, options))?;

    Ok(CompileOutput {
        segments: emit_output.segments,
        listing: emit_output.listing,
        warnings,
        rendered_warnings,
    })
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

    let emit_output = emit_object(&hir, &source_map)
        .map_err(|diagnostics| fail_with_rendered(&source_map, diagnostics, options))?;

    Ok(CompileObjectOutput {
        object: emit_output.object,
        warnings,
        rendered_warnings,
    })
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
