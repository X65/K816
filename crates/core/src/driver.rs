use indexmap::IndexMap;
use thiserror::Error;

use k816_assets::{AssetFS, StdAssetFS};
use k816_o65::O65Object;

use crate::diag::{Diagnostic, render_diagnostics};
use crate::emit::emit;
use crate::emit_object::emit_object;
use crate::eval_expand::expand_file;
use crate::lower::lower;
use crate::parser::parse;
use crate::sema::analyze;
use crate::span::SourceMap;

#[derive(Debug, Clone)]
pub struct CompileOutput {
    pub banks: IndexMap<String, Vec<u8>>,
    pub listing: String,
}

#[derive(Debug, Clone)]
pub struct CompileObjectOutput {
    pub object: O65Object,
}

#[derive(Debug, Error)]
#[error("compilation failed")]
pub struct CompileError {
    pub diagnostics: Vec<Diagnostic>,
    pub rendered: String,
}

pub fn compile_source(source_name: &str, source_text: &str) -> Result<CompileOutput, CompileError> {
    let fs = StdAssetFS;
    compile_source_with_fs(source_name, source_text, &fs)
}

pub fn compile_source_to_object(
    source_name: &str,
    source_text: &str,
) -> Result<CompileObjectOutput, CompileError> {
    let fs = StdAssetFS;
    compile_source_to_object_with_fs(source_name, source_text, &fs)
}

pub fn compile_source_with_fs(
    source_name: &str,
    source_text: &str,
    fs: &dyn AssetFS,
) -> Result<CompileOutput, CompileError> {
    let mut source_map = SourceMap::default();
    let source_id = source_map.add_source(source_name, source_text);

    let ast = parse(source_id, source_text)
        .map_err(|diagnostics| fail_with_rendered(&source_map, diagnostics))?;

    let ast = expand_file(&ast, source_id)
        .map_err(|diagnostics| fail_with_rendered(&source_map, diagnostics))?;

    let sema = analyze(&ast).map_err(|diagnostics| fail_with_rendered(&source_map, diagnostics))?;

    let hir = lower(&ast, &sema, fs)
        .map_err(|diagnostics| fail_with_rendered(&source_map, diagnostics))?;

    let emit_output =
        emit(&hir).map_err(|diagnostics| fail_with_rendered(&source_map, diagnostics))?;

    Ok(CompileOutput {
        banks: emit_output.banks,
        listing: emit_output.listing,
    })
}

pub fn compile_source_to_object_with_fs(
    source_name: &str,
    source_text: &str,
    fs: &dyn AssetFS,
) -> Result<CompileObjectOutput, CompileError> {
    let mut source_map = SourceMap::default();
    let source_id = source_map.add_source(source_name, source_text);

    let ast = parse(source_id, source_text)
        .map_err(|diagnostics| fail_with_rendered(&source_map, diagnostics))?;

    let ast = expand_file(&ast, source_id)
        .map_err(|diagnostics| fail_with_rendered(&source_map, diagnostics))?;

    let sema = analyze(&ast).map_err(|diagnostics| fail_with_rendered(&source_map, diagnostics))?;

    let hir = lower(&ast, &sema, fs)
        .map_err(|diagnostics| fail_with_rendered(&source_map, diagnostics))?;

    let emit_output = emit_object(&hir, &source_map)
        .map_err(|diagnostics| fail_with_rendered(&source_map, diagnostics))?;

    Ok(CompileObjectOutput {
        object: emit_output.object,
    })
}

fn fail_with_rendered(source_map: &SourceMap, diagnostics: Vec<Diagnostic>) -> CompileError {
    let rendered = render_diagnostics(source_map, &diagnostics);
    CompileError {
        diagnostics,
        rendered,
    }
}
