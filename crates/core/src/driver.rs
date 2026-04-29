use thiserror::Error;

use std::collections::HashSet;

use indexmap::IndexMap;
use k816_assets::{AssetFS, StdAssetFS};
use k816_o65::O65Object;

use crate::ast::{CodeBlock, File, Item};
use crate::diag::{Diagnostic, RenderOptions, render_diagnostics_with_options};
use crate::emit_object::{AddressableSite, emit_object};
use crate::eval_expand::expand_file;
use crate::fold_mode::{eliminate_dead_mode_ops, fold_mode_ops};
use crate::lower::lower_with_warnings;
use crate::normalize_hla::normalize_file;
use crate::parser::{parse_with_warnings_and_externals, scan_declared_function_names};
use crate::peephole::peephole_optimize;
use crate::sema::{ConstMeta, FunctionMeta, analyze_partial, analyze_with_external_consts};
use crate::span::{SourceId, SourceMap};

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

#[derive(Clone, Copy, Default)]
struct CompileExternals<'a> {
    consts: Option<&'a IndexMap<String, ConstMeta>>,
    functions: Option<&'a IndexMap<String, FunctionMeta>>,
    function_names: Option<&'a HashSet<String>>,
    inline_bodies: Option<&'a IndexMap<String, CodeBlock>>,
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
    let mut source_map = SourceMap::default();
    let source_id = source_map.add_source(source_name, source_text);
    compile_source_inner(
        &source_map,
        source_id,
        source_text,
        fs,
        options,
        CompileExternals::default(),
    )
}

/// Compile multiple sources for linking, sharing cross-unit constant values.
/// Returns per-file results so each file gets its own diagnostics.
pub fn compile_sources(
    sources: &[LinkCompileInput<'_>],
    options: CompileRenderOptions,
) -> Vec<Result<CompileObjectOutput, CompileError>> {
    let external_function_names = collect_all_declared_function_names(sources);

    // Build a shared SourceMap up front so every parsed source — including the
    // silent metadata pre-scans below — gets a distinct SourceId. This is what
    // makes the cross-file inline detection in `retargeted_body_if_foreign`
    // actually fire and lets emit-time diagnostics anchored to a foreign source
    // resolve through one consistent SourceMap.
    let mut source_map = SourceMap::default();
    let source_ids: Vec<SourceId> = sources
        .iter()
        .map(|source| source_map.add_source(source.source_name, source.source_text))
        .collect();

    let parsed_sources: Vec<Option<File>> = sources
        .iter()
        .zip(source_ids.iter())
        .map(|(source, &source_id)| {
            parse_expand_normalize_source(
                source_id,
                source.source_text,
                Some(&external_function_names),
            )
        })
        .collect();
    let external_consts = collect_external_consts_from_parsed(&parsed_sources);
    let external_functions = collect_external_functions_from_parsed(&parsed_sources);
    let external_inline_bodies = collect_external_inline_bodies_from_parsed(&parsed_sources);
    let fs = StdAssetFS;

    sources
        .iter()
        .zip(source_ids.iter())
        .map(|(source, &source_id)| {
            compile_source_inner(
                &source_map,
                source_id,
                source.source_text,
                &fs,
                options,
                CompileExternals {
                    consts: Some(&external_consts),
                    functions: Some(&external_functions),
                    function_names: Some(&external_function_names),
                    inline_bodies: Some(&external_inline_bodies),
                },
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
    source_map: &SourceMap,
    source_id: SourceId,
    source_text: &str,
    fs: &dyn AssetFS,
    options: CompileRenderOptions,
    externals: CompileExternals<'_>,
) -> Result<CompileObjectOutput, CompileError> {
    let parsed =
        parse_with_warnings_and_externals(source_id, source_text, externals.function_names)
            .map_err(|diagnostics| fail_with_rendered(source_map, diagnostics, options))?;
    let ast = parsed.file;
    let mut warnings = parsed.warnings;

    let ast = expand_file(&ast, source_id)
        .map_err(|diagnostics| fail_with_rendered(source_map, diagnostics, options))?;
    let ast = normalize_file(&ast)
        .map_err(|diagnostics| fail_with_rendered(source_map, diagnostics, options))?;

    let mut sema = analyze_with_external_consts(&ast, externals.consts)
        .map_err(|diagnostics| fail_with_rendered(source_map, diagnostics, options))?;
    if let Some(external) = externals.functions {
        for (name, meta) in external {
            sema.functions
                .entry(name.clone())
                .or_insert_with(|| meta.clone());
        }
    }

    let lower_output = lower_with_warnings(&ast, &sema, fs, externals.inline_bodies)
        .map_err(|diagnostics| fail_with_rendered(source_map, diagnostics, options))?;
    warnings.extend(lower_output.warnings);
    let rendered_warnings = render_diagnostics_with_options(
        source_map,
        &warnings,
        RenderOptions {
            color: options.color,
        },
    );
    let hir = lower_output.program;
    let hir = eliminate_dead_mode_ops(&hir);
    let hir = fold_mode_ops(&hir);
    let hir = peephole_optimize(&hir);

    let emit_output = emit_object(&hir, source_map, &sema, externals.functions)
        .map_err(|diagnostics| fail_with_rendered(source_map, diagnostics, options))?;

    Ok(CompileObjectOutput {
        object: emit_output.object,
        addressable_sites: emit_output.addressable_sites,
        warnings,
        rendered_warnings,
    })
}

/// Parse, expand, and normalize one source with the given cross-unit function
/// name set. Returns `None` on any parse/expand/normalize failure so callers can
/// skip the source silently (diagnostics for that source will surface during
/// the main per-file compile anyway).
fn parse_expand_normalize_source(
    source_id: SourceId,
    source_text: &str,
    names: Option<&HashSet<String>>,
) -> Option<File> {
    let parsed = parse_with_warnings_and_externals(source_id, source_text, names).ok()?;
    let ast = expand_file(&parsed.file, source_id).ok()?;
    normalize_file(&ast).ok()
}

pub fn collect_external_consts_for_link_sources(
    sources: &[LinkCompileInput<'_>],
) -> IndexMap<String, ConstMeta> {
    let names = collect_all_declared_function_names(sources);
    let mut source_map = SourceMap::default();
    let parsed_sources: Vec<Option<File>> = sources
        .iter()
        .map(|source| {
            let source_id = source_map.add_source(source.source_name, source.source_text);
            parse_expand_normalize_source(source_id, source.source_text, Some(&names))
        })
        .collect();
    collect_external_consts_from_parsed(&parsed_sources)
}

fn collect_external_consts_from_parsed(
    parsed_sources: &[Option<File>],
) -> IndexMap<String, ConstMeta> {
    let mut consts: IndexMap<String, ConstMeta> = IndexMap::new();
    let mut ambiguous = HashSet::new();

    // Iterate to a fixed point: each round may resolve new consts that unblock others.
    loop {
        let prev_count = consts.len();

        for ast in parsed_sources {
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

/// Collect every declared top-level function name across all link sources via a
/// cheap token-level scan. Feeds the parser's `known_functions` set so call-site
/// syntax for cross-unit callees parses correctly.
pub fn collect_all_declared_function_names(sources: &[LinkCompileInput<'_>]) -> HashSet<String> {
    let mut names = HashSet::new();
    let mut source_map = SourceMap::default();
    for source in sources {
        let source_id = source_map.add_source(source.source_name, source.source_text);
        names.extend(scan_declared_function_names(source_id, source.source_text));
    }
    names
}

pub fn collect_external_functions_for_link_sources(
    sources: &[LinkCompileInput<'_>],
) -> IndexMap<String, FunctionMeta> {
    let names = collect_all_declared_function_names(sources);
    let mut source_map = SourceMap::default();
    let parsed_sources: Vec<Option<File>> = sources
        .iter()
        .map(|source| {
            let source_id = source_map.add_source(source.source_name, source.source_text);
            parse_expand_normalize_source(source_id, source.source_text, Some(&names))
        })
        .collect();
    collect_external_functions_from_parsed(&parsed_sources)
}

fn collect_external_functions_from_parsed(
    parsed_sources: &[Option<File>],
) -> IndexMap<String, FunctionMeta> {
    let mut functions: IndexMap<String, FunctionMeta> = IndexMap::new();
    for ast in parsed_sources {
        let Some(ast) = ast else { continue };
        let (sema, _) = analyze_partial(ast, None);
        for (name, meta) in sema.functions {
            functions.entry(name).or_insert(meta);
        }
    }
    functions
}

/// Collect `CodeBlock` bodies for every `inline` function declared across the
/// link sources. Enables cross-TU inline resolution: the lowerer's
/// `inline_bodies` map is extended with these entries so a call to an inline
/// function declared in another file expands at the call site like a local
/// inline.
///
/// First-declaration-wins on name collisions, matching the policy for consts
/// and function metadata.
pub fn collect_external_inline_bodies_for_link_sources(
    sources: &[LinkCompileInput<'_>],
) -> IndexMap<String, CodeBlock> {
    let names = collect_all_declared_function_names(sources);
    let mut source_map = SourceMap::default();
    let parsed_sources: Vec<Option<File>> = sources
        .iter()
        .map(|source| {
            let source_id = source_map.add_source(source.source_name, source.source_text);
            parse_expand_normalize_source(source_id, source.source_text, Some(&names))
        })
        .collect();
    collect_external_inline_bodies_from_parsed(&parsed_sources)
}

fn collect_external_inline_bodies_from_parsed(
    parsed_sources: &[Option<File>],
) -> IndexMap<String, CodeBlock> {
    let mut bodies: IndexMap<String, CodeBlock> = IndexMap::new();
    for ast in parsed_sources {
        let Some(ast) = ast else { continue };
        for item in &ast.items {
            if let Item::CodeBlock(block) = &item.node
                && block.is_inline
                && !bodies.contains_key(&block.name)
            {
                bodies.insert(block.name.clone(), block.clone());
            }
        }
    }
    bodies
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
        CompileRenderOptions, LinkCompileInput, compile_source, compile_sources,
        compile_sources_all_or_nothing,
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

        let outputs = compile_sources_all_or_nothing(&sources, CompileRenderOptions::plain())
            .expect("compile");
        let main_object = &outputs[0].object;
        let section = main_object
            .sections
            .get(crate::DEFAULT_SEGMENT)
            .expect("default section exists");
        let bytes = section
            .chunks
            .iter()
            .flat_map(|chunk| chunk.bytes.iter().copied())
            .collect::<Vec<_>>();

        assert!(bytes.windows(3).any(|window| window == [0xA9, 0x39, 0x05]));
    }

    /// When an inline body declared in a *different* source produces a
    /// diagnostic during emit, the diagnostic must:
    ///   - have its `primary` span resolve in the calling file (so the editor
    ///     squiggle lands at the call site, not at random byte offsets in the
    ///     wrong file);
    ///   - carry an `InlineOrigin` supplement whose `span.source_id` points
    ///     back to the foreign source, with a label naming that file:line.
    /// Regression coverage for the LSP `(TCB)` underline bug.
    #[test]
    fn inline_call_diagnostic_carries_inline_origin_supplement() {
        let helpers = "inline add (a, #b) -> a {\n    clc\n    adc #b\n}\n";
        let main =
            "func main {\n    @a8 { txa }\n    plp\n    add a, #1 -> a\n    rts\n}\n";
        let sources = [
            LinkCompileInput {
                source_name: "helpers.k65",
                source_text: helpers,
            },
            LinkCompileInput {
                source_name: "main.k65",
                source_text: main,
            },
        ];
        let results = compile_sources(&sources, CompileRenderOptions::plain());
        let main_err = results[1].as_ref().err().expect("main.k65 must error");
        let diagnostic = main_err
            .diagnostics
            .iter()
            .find(|d| d.message.contains("`adc`") && d.message.contains("accumulator width"))
            .expect("expected an adc width-unknown diagnostic from main.k65");

        // Primary span must resolve in main.k65 (source_id 1).
        assert_eq!(diagnostic.primary.source_id.0, 1);

        // The diagnostic must carry an InlineOrigin supplement with a label
        // pointing back at the *helpers.k65* source (source_id 0).
        let inline_origin = diagnostic
            .supplements
            .iter()
            .find_map(|s| match s {
                crate::diag::Supplemental::InlineOrigin { span, label } => Some((*span, label)),
                _ => None,
            })
            .expect("InlineOrigin supplement");
        assert_eq!(inline_origin.0.source_id.0, 0);
        assert!(
            inline_origin.1.starts_with("Inlined from helpers.k65:"),
            "label was {:?}",
            inline_origin.1,
        );
    }
}
