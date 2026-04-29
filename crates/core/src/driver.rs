use thiserror::Error;

use std::collections::HashSet;

use indexmap::IndexMap;
use k816_assets::{AssetFS, StdAssetFS};
use k816_o65::O65Object;

use crate::ast::{CodeBlock, File, Item, Stmt};
use crate::diag::{Diagnostic, RenderOptions, render_diagnostics_with_options};
use crate::emit_object::{AddressableSite, emit_object};
use crate::eval_expand::expand_file;
use crate::fold_mode::{eliminate_dead_mode_ops, fold_mode_ops};
use crate::lower::lower_with_warnings;
use crate::normalize_hla::normalize_file;
use crate::parser::{parse_with_warnings_and_externals, scan_declared_function_names};
use crate::peephole::peephole_optimize;
use crate::sema::{
    AnalysisExternals, ConstMeta, FunctionMeta, VarMeta, analyze_partial, analyze_with_externals,
};
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

/// Cross-unit symbols collected once per multi-source compile. Threaded into
/// each per-file compile so `lower` and `analyze_partial` can resolve symbols
/// declared in other translation units in the same link group.
///
/// The same value also drives the LSP's per-file diagnostics, so editor
/// squiggles agree with what `compile_sources` would actually emit.
#[derive(Debug, Clone, Default)]
pub struct WorkspaceExternals {
    pub consts: IndexMap<String, ConstMeta>,
    /// Cross-unit vars resolvable at compile time (declarations carrying an
    /// explicit `= <addr>` initializer). Auto-addressed vars are intentionally
    /// excluded — their per-file address is meaningless across translation
    /// units. Bare references to such names fall through
    /// `lower::resolve_operand_ident` to a label-relocation path the linker
    /// resolves.
    pub vars: IndexMap<String, VarMeta>,
    pub functions: IndexMap<String, FunctionMeta>,
    pub function_names: HashSet<String>,
    pub inline_bodies: IndexMap<String, CodeBlock>,
    /// Names of every declared addressable symbol across all link sources —
    /// functions, vars (regardless of whether the address is known), data
    /// blocks, and global (non-`.`-prefixed) code labels. Used by lower to
    /// validate `&&NAME` operands at compile time, surfacing what would
    /// otherwise be a link-time "undefined symbol" error during per-file LSP
    /// analysis.
    pub addressable_names: HashSet<String>,
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
    compile_source_inner(&source_map, source_id, source_text, fs, options, None)
}

/// Compile multiple sources for linking, sharing cross-unit symbol values.
/// Returns per-file results so each file gets its own diagnostics.
pub fn compile_sources(
    sources: &[LinkCompileInput<'_>],
    options: CompileRenderOptions,
) -> Vec<Result<CompileObjectOutput, CompileError>> {
    // Build a shared SourceMap up front so every parsed source — including the
    // silent metadata pre-scans inside `collect_workspace_externals` — gets a
    // distinct SourceId. This is what makes the cross-file inline detection in
    // `retargeted_body_if_foreign` actually fire and lets emit-time
    // diagnostics anchored to a foreign source resolve through one consistent
    // SourceMap.
    let mut source_map = SourceMap::default();
    let source_ids: Vec<SourceId> = sources
        .iter()
        .map(|source| source_map.add_source(source.source_name, source.source_text))
        .collect();

    let externals = collect_workspace_externals(sources);
    compile_sources_with_externals_inner(sources, &source_ids, &source_map, &externals, options)
}

/// Like [`compile_sources`], but reuses pre-collected cross-unit metadata.
/// Useful for callers (such as the LSP) that already have a
/// `WorkspaceExternals` in hand and want to avoid re-parsing every source.
pub fn compile_sources_with_externals(
    sources: &[LinkCompileInput<'_>],
    externals: &WorkspaceExternals,
    options: CompileRenderOptions,
) -> Vec<Result<CompileObjectOutput, CompileError>> {
    let mut source_map = SourceMap::default();
    let source_ids: Vec<SourceId> = sources
        .iter()
        .map(|source| source_map.add_source(source.source_name, source.source_text))
        .collect();
    compile_sources_with_externals_inner(sources, &source_ids, &source_map, externals, options)
}

fn compile_sources_with_externals_inner(
    sources: &[LinkCompileInput<'_>],
    source_ids: &[SourceId],
    source_map: &SourceMap,
    externals: &WorkspaceExternals,
    options: CompileRenderOptions,
) -> Vec<Result<CompileObjectOutput, CompileError>> {
    let fs = StdAssetFS;
    sources
        .iter()
        .zip(source_ids.iter())
        .map(|(source, &source_id)| {
            compile_source_inner(
                source_map,
                source_id,
                source.source_text,
                &fs,
                options,
                Some(externals),
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
    externals: Option<&WorkspaceExternals>,
) -> Result<CompileObjectOutput, CompileError> {
    let function_names = externals.map(|e| &e.function_names);
    let parsed = parse_with_warnings_and_externals(source_id, source_text, function_names)
        .map_err(|diagnostics| fail_with_rendered(source_map, diagnostics, options))?;
    let ast = parsed.file;
    let mut warnings = parsed.warnings;

    let ast = expand_file(&ast, source_id)
        .map_err(|diagnostics| fail_with_rendered(source_map, diagnostics, options))?;
    let ast = normalize_file(&ast)
        .map_err(|diagnostics| fail_with_rendered(source_map, diagnostics, options))?;

    let analysis_externals = AnalysisExternals {
        consts: externals.map(|e| &e.consts),
        vars: externals.map(|e| &e.vars),
    };
    let mut sema = analyze_with_externals(&ast, analysis_externals)
        .map_err(|diagnostics| fail_with_rendered(source_map, diagnostics, options))?;
    if let Some(external) = externals {
        for (name, meta) in &external.functions {
            sema.functions
                .entry(name.clone())
                .or_insert_with(|| meta.clone());
        }
    }

    let inline_bodies = externals.map(|e| &e.inline_bodies);
    let workspace_addressable = externals.map(|e| &e.addressable_names);
    let lower_output =
        lower_with_warnings(&ast, &sema, fs, inline_bodies, workspace_addressable)
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

    let external_functions = externals.map(|e| &e.functions);
    let emit_output = emit_object(&hir, source_map, &sema, external_functions)
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

/// Collect every cross-unit symbol — consts, explicit-address vars, function
/// metadata, function names, and inline bodies — from the given link sources
/// in one pass. The result is the canonical input to per-file compiles via
/// [`compile_sources_with_externals`] and to the LSP's `analyze_document`.
pub fn collect_workspace_externals(sources: &[LinkCompileInput<'_>]) -> WorkspaceExternals {
    let function_names = collect_all_declared_function_names(sources);
    let addressable_names = collect_all_declared_addressable_names(sources);
    let mut source_map = SourceMap::default();
    let parsed_sources: Vec<Option<File>> = sources
        .iter()
        .map(|source| {
            let source_id = source_map.add_source(source.source_name, source.source_text);
            parse_expand_normalize_source(source_id, source.source_text, Some(&function_names))
        })
        .collect();
    let consts = collect_external_consts_from_parsed(&parsed_sources);
    let vars = collect_external_vars_from_parsed(&parsed_sources, &consts);
    let functions = collect_external_functions_from_parsed(&parsed_sources);
    let inline_bodies = collect_external_inline_bodies_from_parsed(&parsed_sources);
    WorkspaceExternals {
        consts,
        vars,
        functions,
        function_names,
        inline_bodies,
        addressable_names,
    }
}

/// Token-scan every link source for all declared addressable symbol names.
/// See [`crate::parser::scan_declared_addressable_names`] for what counts.
pub fn collect_all_declared_addressable_names(
    sources: &[LinkCompileInput<'_>],
) -> HashSet<String> {
    let mut names = HashSet::new();
    let mut source_map = SourceMap::default();
    for source in sources {
        let source_id = source_map.add_source(source.source_name, source.source_text);
        names.extend(crate::parser::scan_declared_addressable_names(
            source_id,
            source.source_text,
        ));
    }
    names
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
            let externals = AnalysisExternals {
                consts: Some(&consts),
                vars: None,
            };
            let (sema, _diagnostics) = analyze_partial(ast, externals);

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

fn collect_external_functions_from_parsed(
    parsed_sources: &[Option<File>],
) -> IndexMap<String, FunctionMeta> {
    let mut functions: IndexMap<String, FunctionMeta> = IndexMap::new();
    for ast in parsed_sources {
        let Some(ast) = ast else { continue };
        let (sema, _) = analyze_partial(ast, AnalysisExternals::default());
        for (name, meta) in sema.functions {
            functions.entry(name).or_insert(meta);
        }
    }
    functions
}

/// Collect cross-unit vars whose addresses can be known at compile time —
/// i.e. those declared with an explicit initializer (`var X = $1234`). Vars
/// without an initializer use a per-file auto-allocator whose value is
/// meaningless across translation units, so they are intentionally excluded;
/// `lower::resolve_operand_ident` already falls through to a label-relocation
/// path for any name absent from the per-file `sema`, which the linker then
/// resolves.
///
/// First-declaration-wins on name collisions, matching the policy for consts,
/// functions, and inline bodies.
fn collect_external_vars_from_parsed(
    parsed_sources: &[Option<File>],
    consts: &IndexMap<String, ConstMeta>,
) -> IndexMap<String, VarMeta> {
    // Walk each AST to find names of top-level vars carrying an explicit
    // address initializer. Function-body local vars are deliberately skipped:
    // they share the global namespace today but should not leak across files.
    let mut explicit_addr_names: HashSet<String> = HashSet::new();
    for ast in parsed_sources {
        let Some(ast) = ast else { continue };
        for item in &ast.items {
            match &item.node {
                Item::Var(var) | Item::Statement(Stmt::Var(var)) => {
                    if var.initializer.is_some() {
                        explicit_addr_names.insert(var.name.clone());
                    }
                }
                _ => {}
            }
        }
    }

    // Run analyze_partial against each parsed source so var sizes, layouts,
    // and symbolic-subscript fields are evaluated with cross-unit consts in
    // scope, then keep only the entries belonging to the explicit-addr set.
    let mut vars: IndexMap<String, VarMeta> = IndexMap::new();
    for ast in parsed_sources {
        let Some(ast) = ast else { continue };
        let externals = AnalysisExternals {
            consts: Some(consts),
            vars: None,
        };
        let (sema, _) = analyze_partial(ast, externals);
        for (name, meta) in sema.vars {
            if !explicit_addr_names.contains(&name) {
                continue;
            }
            vars.entry(name).or_insert(meta);
        }
    }
    vars
}

/// Collect `CodeBlock` bodies for every `inline` function declared across the
/// link sources. Enables cross-TU inline resolution: the lowerer's
/// `inline_bodies` map is extended with these entries so a call to an inline
/// function declared in another file expands at the call site like a local
/// inline.
///
/// First-declaration-wins on name collisions, matching the policy for consts
/// and function metadata.
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

    /// Cross-unit var with an explicit-address initializer (`var BAR = $1234`)
    /// must resolve at compile time when referenced from a sibling source —
    /// previously this errored with `unknown identifier 'BAR'` because
    /// `compile_sources` only threaded cross-unit consts and functions.
    #[test]
    fn multi_source_cross_unit_explicit_addr_var_resolves() {
        let sources = [
            LinkCompileInput {
                source_name: "main.k65",
                source_text: "func main @a16 {\n  lda BAR\n}\n",
            },
            LinkCompileInput {
                source_name: "vars.k65",
                source_text: "var BAR = $1234\n",
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

        // `lda $1234` (absolute, A=16) → AD 34 12.
        assert!(
            bytes.windows(3).any(|window| window == [0xAD, 0x34, 0x12]),
            "expected `lda $1234` bytes in {bytes:02X?}"
        );
    }

    /// Cross-unit var with an explicit-address initializer must also resolve
    /// in arithmetic operand expressions (`BAR + 1`). Previously the inner
    /// `eval_to_number` short-circuited with `unknown identifier 'BAR'` before
    /// `try_label_offset_operand` got a chance to run.
    #[test]
    fn multi_source_cross_unit_explicit_addr_var_in_binary_expr() {
        let sources = [
            LinkCompileInput {
                source_name: "main.k65",
                source_text: "func main @a16 {\n  lda BAR + 2\n}\n",
            },
            LinkCompileInput {
                source_name: "vars.k65",
                source_text: "var BAR = $1234\n",
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

        // `lda $1236` → AD 36 12.
        assert!(
            bytes.windows(3).any(|window| window == [0xAD, 0x36, 0x12]),
            "expected `lda $1236` bytes in {bytes:02X?}"
        );
    }

    /// Cross-unit var with a symbolic-subscript field list (`var TASKS[.name(8), .state]:byte = $4000`)
    /// must allow field access (`TASKS.state`, `TASKS.name + idx`) from a
    /// sibling source. This is the exact pattern blocking `os-816/main.k65` —
    /// see the plan referenced in the user_role memory.
    #[test]
    fn multi_source_cross_unit_var_subscript_field() {
        let sources = [
            LinkCompileInput {
                source_name: "main.k65",
                source_text: "func main @a8 @i16 {\n  ldy #0\n  lda TASKS.state, y\n  sta TASKS.name + 1, y\n}\n",
            },
            LinkCompileInput {
                source_name: "task.k65",
                source_text: "var TASKS [\n  .name [4]:byte\n  .state :byte\n] = $4000\n",
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

        // `lda TASKS.state, y` → B9 04 40  (state offset = 4, base = $4000).
        // `sta TASKS.name + 1, y` → 99 01 40  (name offset = 0, +1, base = $4000).
        assert!(
            bytes.windows(3).any(|window| window == [0xB9, 0x04, 0x40]),
            "expected `lda $4004, y` bytes in {bytes:02X?}"
        );
        assert!(
            bytes.windows(3).any(|window| window == [0x99, 0x01, 0x40]),
            "expected `sta $4001, y` bytes in {bytes:02X?}"
        );
    }

    /// Auto-addressed cross-unit vars (declared without `= <addr>`) are
    /// intentionally NOT threaded through the workspace externals — their
    /// address would be a per-file auto-allocator value meaningless across
    /// translation units. Bare references should still compile via the
    /// label-relocation fallback in `lower::resolve_operand_ident`. This test
    /// locks in that behavior so a future change doesn't accidentally start
    /// emitting bogus literal addresses for auto-addressed externs.
    #[test]
    fn multi_source_cross_unit_auto_addr_var_falls_through_to_label() {
        let sources = [
            LinkCompileInput {
                source_name: "main.k65",
                source_text: "func main @a16 {\n  lda CURRENT_TASK\n}\n",
            },
            LinkCompileInput {
                source_name: "task.k65",
                source_text: "var CURRENT_TASK : word\n",
            },
        ];

        let results = compile_sources(&sources, CompileRenderOptions::plain());
        // main.k65 must compile (no `unknown identifier` diagnostic).
        let main_err = results[0].as_ref().err();
        if let Some(err) = main_err {
            for diag in &err.diagnostics {
                assert!(
                    !diag.message.contains("unknown identifier"),
                    "unexpected unknown-identifier diagnostic: {}",
                    diag.message
                );
            }
        }
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
