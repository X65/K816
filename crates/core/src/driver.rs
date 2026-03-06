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
use crate::sema::{ConstMeta, analyze_partial, analyze_with_external_consts};
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

/// Compile a single source file.
pub fn compile_source(
    source_name: &str,
    source_text: &str,
    options: CompileRenderOptions,
) -> Result<CompileObjectOutput, CompileError> {
    compile_source_with_fs(source_name, source_text, &StdAssetFS, options)
}

/// Compile a single source file with a custom asset filesystem.
pub fn compile_source_with_fs(
    source_name: &str,
    source_text: &str,
    fs: &dyn AssetFS,
    options: CompileRenderOptions,
) -> Result<CompileObjectOutput, CompileError> {
    compile_source_inner(source_name, source_text, fs, options, None)
}

/// Compile multiple sources for linking, sharing cross-unit constant values.
/// Returns per-file results so each file gets its own diagnostics.
pub fn compile_sources(
    sources: &[LinkCompileInput<'_>],
    options: CompileRenderOptions,
) -> Vec<Result<CompileObjectOutput, CompileError>> {
    let external_consts = collect_external_consts_for_link_sources(sources);
    let fs = StdAssetFS;

    sources
        .iter()
        .map(|source| {
            compile_source_inner(
                source.source_name,
                source.source_text,
                &fs,
                options,
                Some(&external_consts),
            )
        })
        .collect()
}

/// Compile multiple sources, failing on the first error.
/// Use `compile_sources` when you need per-file error isolation (e.g. LSP).
pub fn compile_sources_all_or_nothing(
    sources: &[LinkCompileInput<'_>],
    options: CompileRenderOptions,
) -> Result<Vec<CompileObjectOutput>, CompileError> {
    compile_sources(sources, options).into_iter().collect()
}

// --- Internal implementation ---

fn compile_source_inner(
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

    let sema = analyze_with_external_consts(&ast, external_consts)
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

fn collect_external_consts_for_link_sources(
    sources: &[LinkCompileInput<'_>],
) -> IndexMap<String, ConstMeta> {
    use crate::ast::File;

    let mut consts: IndexMap<String, ConstMeta> = IndexMap::new();
    let mut ambiguous = HashSet::new();

    // Pre-parse all sources once (parsing/expanding/normalizing is independent of consts).
    let parsed_sources: Vec<Option<File>> = sources
        .iter()
        .map(|source| {
            let source_id = crate::span::SourceId(0);
            let parsed = parse_with_warnings(source_id, source.source_text).ok()?;
            let ast = expand_file(&parsed.file, source_id).ok()?;
            normalize_file(&ast).ok()
        })
        .collect();

    // Iterate to a fixed point: each round may resolve new consts that unblock others.
    loop {
        let prev_count = consts.len();

        for ast in &parsed_sources {
            let Some(ast) = ast else { continue };
            let (sema, _diagnostics) = analyze_partial(ast, Some(&consts));

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

        // Fixed point: no new consts were added this round.
        if consts.len() == prev_count {
            break;
        }
    }

    consts
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
    use super::{
        CompileRenderOptions, LinkCompileInput, compile_source, compile_sources_all_or_nothing,
    };

    #[test]
    fn keeps_diagnostic_spans_aligned_after_eval_block_preprocess() {
        let source = "[\n  A = 1,\n  B = 2,\n  C = (A + B) * 4\n]\n\nfunc main {\n  ldy #[J]\n}\n";
        let error = compile_source("test.k65", source, CompileRenderOptions::plain())
            .expect_err("must fail");
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

        let outputs =
            compile_sources_all_or_nothing(&sources, CompileRenderOptions::plain())
                .expect("compile");
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
