use thiserror::Error;

use std::collections::HashSet;

use indexmap::IndexMap;
use k816_assets::{AssetFS, StdAssetFS};
use k816_o65::O65Object;

use crate::diag::{Diagnostic, RenderOptions, render_diagnostics_with_options};
use crate::emit_object::{AddressableSite, emit_object};
use crate::eval_expand::expand_file;
use crate::fold_mode::{eliminate_dead_mode_ops, fold_mode_ops};
use crate::lower::lower;
use crate::normalize_hla::normalize_file;
use crate::parser::parse_with_warnings;
use crate::peephole::peephole_optimize;
use crate::sema::{ConstMeta, analyze};
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

#[derive(Debug, Clone, Copy)]
pub struct LinkCompileInput<'a> {
    pub source_name: &'a str,
    pub source_text: &'a str,
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

/// Compile multiple sources for linking while sharing cross-unit constant values.
pub fn compile_sources_to_objects_for_link(
    sources: &[LinkCompileInput<'_>],
) -> Result<Vec<CompileObjectOutput>, CompileError> {
    compile_sources_to_objects_for_link_with_options(sources, CompileRenderOptions::plain())
}

/// Same as `compile_sources_to_objects_for_link` with configurable diagnostic
/// render options.
pub fn compile_sources_to_objects_for_link_with_options(
    sources: &[LinkCompileInput<'_>],
    options: CompileRenderOptions,
) -> Result<Vec<CompileObjectOutput>, CompileError> {
    let external_consts = collect_external_consts_for_link_sources(sources);
    let fs = StdAssetFS;

    sources
        .iter()
        .map(|source| {
            compile_source_to_object_with_fs_and_options_and_external_consts(
                source.source_name,
                source.source_text,
                &fs,
                options,
                Some(&external_consts),
            )
        })
        .collect()
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
    compile_source_to_object_with_fs_and_options_and_external_consts(
        source_name,
        source_text,
        fs,
        options,
        None,
    )
}

fn compile_source_to_object_with_fs_and_options_and_external_consts(
    source_name: &str,
    source_text: &str,
    fs: &dyn AssetFS,
    options: CompileRenderOptions,
    external_consts: Option<&IndexMap<String, ConstMeta>>,
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

    let mut sema = analyze(&ast)
        .map_err(|diagnostics| fail_with_rendered(&source_map, diagnostics, options))?;
    if let Some(external_consts) = external_consts {
        for (name, meta) in external_consts {
            sema.consts.entry(name.clone()).or_insert(*meta);
        }
    }

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

fn collect_external_consts_for_link_sources(
    sources: &[LinkCompileInput<'_>],
) -> IndexMap<String, ConstMeta> {
    let mut consts: IndexMap<String, ConstMeta> = IndexMap::new();
    let mut ambiguous = HashSet::new();

    for source in sources {
        let source_id = crate::span::SourceId(0);
        let Ok(parsed) = parse_with_warnings(source_id, source.source_text) else {
            continue;
        };
        let Ok(ast) = expand_file(&parsed.file, source_id) else {
            continue;
        };
        let Ok(ast) = normalize_file(&ast) else {
            continue;
        };
        let Ok(sema) = analyze(&ast) else {
            continue;
        };

        for (name, meta) in sema.consts {
            if ambiguous.contains(&name) {
                continue;
            }
            match consts.get(&name) {
                Some(existing) if existing.value != meta.value => {
                    consts.shift_remove(&name);
                    ambiguous.insert(name);
                }
                Some(_) => {}
                None => {
                    consts.insert(name, meta);
                }
            }
        }
    }

    consts
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
    use super::{LinkCompileInput, compile_source_to_object, compile_sources_to_objects_for_link};

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

    #[test]
    fn multi_source_link_compile_treats_cross_unit_const_as_immediate() {
        let sources = [
            LinkCompileInput {
                source_name: "main.k65",
                source_text: "func main @a16 {\n  lda BAR\n}\n",
            },
            LinkCompileInput {
                source_name: "consts.k65",
                source_text: "const BAR = 1337\n",
            },
        ];

        let outputs = compile_sources_to_objects_for_link(&sources).expect("compile");
        let main_object = &outputs[0].object;
        let section = main_object
            .sections
            .get("default")
            .expect("default section exists");
        let bytes = section
            .chunks
            .iter()
            .flat_map(|chunk| chunk.bytes.iter().copied())
            .collect::<Vec<_>>();

        assert!(bytes.windows(3).any(|window| window == [0xA9, 0x39, 0x05]));
    }
}
