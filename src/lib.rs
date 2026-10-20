/*****************************************************************************/
/*                                                                           */
/*                                   lib.rs                                  */
/*                                                                           */
/*                    K816 compiler frontend library root                    */
/*                                                                           */
/*                                                                           */
/* Copyright (c) 2026 Tomasz Sterna (smokku)                                 */
/*                                                                           */
/* Permission to use, copy, modify, and/or distribute this software for any  */
/* purpose with or without fee is hereby granted.                            */
/*                                                                           */
/* THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES  */
/* WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF          */
/* MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR   */
/* ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES    */
/* WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN     */
/* ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF   */
/* OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.            */
/*                                                                           */
/*****************************************************************************/

//! K816 compiler frontend library.
//!
//! The library owns the complete pipeline — parsing, semantic analysis,
//! lowering, optimization, and ca65 emission — behind a small API consumed by
//! both the CLI (`main.rs`) and the LSP server. Neither tool reconstructs
//! project semantics on its own.

pub mod abi;
pub mod ast;
pub mod bind;
pub mod compat;
pub mod convert;
pub mod diagnostic;
pub mod effects;
pub mod emit_ca65;
pub mod eval;
pub mod expand;
pub mod hla;
pub mod imports;
pub mod isa;
pub mod layout;
pub mod linkage;
pub mod lower;
pub mod lsp;
pub mod names;
pub mod op;
pub mod optimize;
pub mod parser;
pub mod preprocess;
pub mod resolve;
pub mod semantic;
pub mod source;
pub mod spelling;
pub mod symbol;
pub mod value;
pub mod width;
pub mod workspace;

use std::path::{Path, PathBuf};

use diagnostic::Diagnostic;
use parser::ParseResult;
use source::{FileId, SourceMap};

/// Compilation options shared by the CLI and LSP.
#[derive(Clone, Debug)]
pub struct Options {
    /// Run the effect-aware optimizer (`false` for `compile --no-opt`).
    pub optimize: bool,
    /// Report every construct the original K65 language does not have
    /// (`--strict-k65`).
    ///
    /// A reporting mode and nothing else: it changes no width, no lowering and
    /// no byte. Whether a construct is an extension is
    /// `doc/k65-compat-manifest.md`'s answer, mirrored by [`compat::MANIFEST`]
    /// (`plans/06-compatibility.md` decision 20).
    pub strict_k65: bool,
    /// The state the machine is in when `main` gets control (`--entry`).
    ///
    /// Not a preference: it is what the build *states* about its runtime, and
    /// K816 checks a `main @a8` annotation against it rather than emitting
    /// anything to establish it (`abi.rs`, `plans/09-entry-abi.md` decision
    /// 2). Its default is the environment `examples/fullstack/startup.asm`
    /// hands over — native mode, both axes 8-bit — so a build that says
    /// nothing gets what the repository's own startup code does.
    pub entry: abi::EntryEnvironment,
}

impl Default for Options {
    fn default() -> Self {
        Self {
            optimize: true,
            strict_k65: false,
            entry: abi::EntryEnvironment::default(),
        }
    }
}

/// One analyzed unit of a workspace: what it parsed to and what it means.
///
/// Kept rather than dropped because everything an editor asks — hover,
/// definition, completion, rename, semantic tokens — is a question about one
/// of these three, and the alternative is a second, approximate reading of
/// the program in the server (`doc/design.md` §10 forbids exactly that).
/// `analyze_workspace` always computed all of it; before sub-plan H it threw
/// it away on return.
#[derive(Debug)]
pub struct AnalyzedUnit {
    pub file: FileId,
    /// The retained tree, the owned AST, and the syntax diagnostics.
    pub parse: ParseResult,
    pub model: semantic::SemanticModel,
}

impl AnalyzedUnit {
    /// The AST every stage after analysis walks: the preprocessed program
    /// where the preprocessor removed anything, and the parsed file
    /// otherwise.
    ///
    /// Never `parse.file` directly. A region an `#if` did not take has no
    /// symbols, no widths and no references, and the items after one have
    /// shifted — a consumer that reaches for the parsed file sees directives
    /// that no longer exist and indices that no longer line up
    /// (`postmortem.md` §G.6.9).
    pub fn program(&self) -> &ast::AstFile {
        self.model.program(&self.parse.file)
    }
}

/// Result of semantic analysis over a workspace. The LSP stops here; the CLI
/// may continue to ca65 emission.
///
/// The CLI reads `diagnostics` and ignores the rest, which is deliberate:
/// there is one analysis function, and what a consumer needs from it is the
/// consumer's business. Nothing here is cached — every change re-analyzes the
/// whole workspace and the previous `Analysis` is dropped, until profiling
/// says otherwise (`doc/design.md` §10, `plans/07-lsp-tooling.md` decision
/// 12).
#[derive(Debug, Default)]
pub struct Analysis {
    /// Syntax and semantic diagnostics for all analyzed files.
    pub diagnostics: Vec<Diagnostic>,
    /// Every analyzed unit, in the map's file order.
    pub units: Vec<AnalyzedUnit>,
    /// What the units know about each other's names.
    pub linkage: linkage::Linkage,
}

impl Analysis {
    /// The analyzed unit for `file`, if it is one of the workspace's.
    pub fn unit(&self, file: FileId) -> Option<&AnalyzedUnit> {
        self.units.iter().find(|unit| unit.file == file)
    }
}

/// Result of compiling one file to ca65 assembly.
#[derive(Debug)]
pub struct CompileResult {
    /// The emitted ca65 assembly, or `None` when errors prevented emission.
    pub assembly: Option<String>,
    pub diagnostics: Vec<Diagnostic>,
    /// The files ca65 will read that are not the assembly itself: every
    /// `binary "path"` this unit includes, as K816 resolved it.
    ///
    /// A build system needs them to know when the object is stale, and it
    /// cannot derive them — the emitted `.incbin` carries the path *as the
    /// source wrote it*, resolved later against ca65's search list. The
    /// source itself is the caller's own input and is
    /// not repeated here.
    ///
    /// Empty when errors prevented emission: nothing was read that a
    /// dependency could be stated about.
    pub binaries: Vec<PathBuf>,
    /// Every file an `import` reached, transitively, as K816 resolved it.
    ///
    /// The whole closure and not only what this unit named directly: a unit
    /// two imports away is not *visible* here, but its exit contracts were
    /// classified from its body, so a change to it can change these bytes.
    /// A build that rebuilt only on direct imports would ship a stale
    /// object.
    ///
    /// Filled whether or not emission succeeded, unlike `binaries`: the
    /// files were read, and a build system that learns what a failed
    /// compilation depended on can re-run it when one of them changes.
    pub imports: Vec<PathBuf>,
}

/// Loads every file the map's `import` items reach, transitively.
///
/// Returns every file an `import` edge reaches — whether this call added it
/// or it was there already — so a caller that keeps a map across edits can
/// tell what is still wanted from what merely used to be.
///
/// Library code over an injected reader rather than a routine in the CLI,
/// because discovering which files a program consists of is a third thing the
/// CLI and the LSP must agree on (non-negotiable decision 11, as sub-plan K
/// amends it). The CLI reads the filesystem; the editor reads the filesystem
/// but lets an open buffer win; a test reads a map it built itself.
///
/// The loop is parse, preprocess, collect. Preprocessing is not an
/// optimisation: it is what makes an `import` inside `#if 0` not a file this
/// build depends on.
///
/// A unit is preprocessed alone first, and a condition that reads a value an
/// import brings cannot be decided alone — so for a unit whose regions were
/// not all decided, only its imports outside every region are followed
/// then. Its regions are decided by [`linkage::Linkage::resolve`] over the
/// files read so far, which preprocesses with the workspace's values, and
/// the imports the regions it took name are read next; until a round reads
/// nothing new. An `import` makes available what typing the imported
/// declarations would, and that includes deciding a region that imports.
///
/// A file it cannot read is added as nothing at all. `Linkage` then finds an
/// `import` edge that reaches no unit and reports `K816-E0166`, which is what
/// makes the CLI, the editor and an in-memory test say it the same way.
pub fn load_imports(
    map: &mut SourceMap,
    read: &mut dyn FnMut(&Path) -> std::io::Result<String>,
) -> Vec<FileId> {
    let mut pending: Vec<FileId> = map.files().map(source::SourceFile::id).collect();
    let mut reached = Vec::new();
    let mut unreadable: std::collections::BTreeSet<PathBuf> = std::collections::BTreeSet::new();
    let mut undecided = false;
    loop {
        while let Some(file) = pending.pop() {
            let parse = parser::parse_source(map.file(file), None);
            let applied = preprocess::apply(&parse.file, None);
            let imports: Vec<String> = if applied.diagnostics.is_empty() {
                imports_in(&applied.program.items)
            } else {
                undecided = true;
                unconditional_imports(&parse.file.items)
            };
            for written in imports {
                want(
                    map,
                    read,
                    file,
                    &written,
                    &mut reached,
                    &mut pending,
                    &mut unreadable,
                );
            }
        }
        if !undecided {
            break;
        }
        let linkage = linkage::Linkage::resolve(map, abi::EntryEnvironment::default());
        let edges: Vec<(FileId, String)> = linkage
            .import_edges()
            .map(|(file, written, _)| (file, written.to_owned()))
            .collect();
        for (file, written) in edges {
            want(
                map,
                read,
                file,
                &written,
                &mut reached,
                &mut pending,
                &mut unreadable,
            );
        }
        if pending.is_empty() {
            break;
        }
    }
    reached
}

/// The paths the `import` items of a preprocessed program name.
fn imports_in(items: &[ast::Item]) -> Vec<String> {
    items
        .iter()
        .filter_map(|item| match &item.kind {
            ast::ItemKind::Import(import) => Some(import.file.path.clone()),
            _ => None,
        })
        .collect()
}

/// The paths the `import` items outside every `#if` region name — the ones
/// a program imports whatever its conditions say.
fn unconditional_imports(items: &[ast::Item]) -> Vec<String> {
    let mut depth = 0usize;
    let mut found = Vec::new();
    for item in items {
        match &item.kind {
            ast::ItemKind::Preprocessor(directive) => match directive.kind {
                ast::PreprocessorKind::If => depth += 1,
                ast::PreprocessorKind::EndIf => depth = depth.saturating_sub(1),
                _ => {}
            },
            ast::ItemKind::Import(import) if depth == 0 => found.push(import.file.path.clone()),
            _ => {}
        }
    }
    found
}

/// Follows one `import` that `file` wrote: records the unit it reaches, and
/// reads the file into `map` when it is not there yet.
fn want(
    map: &mut SourceMap,
    read: &mut dyn FnMut(&Path) -> std::io::Result<String>,
    file: FileId,
    written: &str,
    reached: &mut Vec<FileId>,
    pending: &mut Vec<FileId>,
    unreadable: &mut std::collections::BTreeSet<PathBuf>,
) {
    // Two resolutions of the same edge, and both are wanted. The *identity*
    // is taken from the importer's identity, which is canonical wherever the
    // file is on disk, so two spellings of one path are one unit — and it is
    // the same arithmetic `Linkage` does, so an edge that reaches a unit here
    // reaches the same one there. The *name* is taken from the importer's
    // name, so a file reached from `main.k816` is called `hw.k816` rather
    // than by an absolute path nobody wrote: it is what a diagnostic points
    // at, what a dependency file lists, and what a `.dbg` record names.
    let path = imports::resolve(map.file(file).path(), written);
    let name = imports::resolve(Path::new(map.file(file).name()), written);
    if let Some(found) = map.locate(&imports::identity(&path)) {
        if !reached.contains(&found) {
            reached.push(found);
        }
        return;
    }
    if unreadable.contains(&path) {
        return;
    }
    let Ok(text) = read(&path) else {
        unreadable.insert(path);
        return;
    };
    let id = map.add_imported_file(name.to_string_lossy(), &path, text);
    reached.push(id);
    pending.push(id);
}

/// Reads `path` into `map` and parses it.
pub fn parse_file(map: &mut SourceMap, path: &Path) -> std::io::Result<(FileId, ParseResult)> {
    let text = std::fs::read_to_string(path)?;
    let id = map.add_file(path.to_string_lossy(), text);
    Ok((id, parser::parse_source(map.file(id), None)))
}

/// Runs syntax and semantic analysis over every file in `map`.
///
/// This is where the LSP stops; the CLI may continue to ca65 emission.
/// Each file is currently analyzed as its own unit — cross-unit symbols are
/// sub-plan C territory.
pub fn analyze_workspace(map: &SourceMap, options: &Options) -> Analysis {
    analyze_parsed(map, options, |source| parser::parse_source(source, None))
}

/// [`analyze_workspace`], over parses the caller produces.
///
/// The hook exists for one caller: a live document store holds each
/// document's previous tree and wants the incremental reparse
/// (`parse_source(source, Some(previous))`) rather than a fresh one. Nothing
/// else about the analysis differs, which is the point — a store that parsed
/// its own way and then called a *different* analysis would be the second
/// reading of the program `doc/design.md` §10 forbids.
pub fn analyze_parsed(
    map: &SourceMap,
    options: &Options,
    mut parse_file: impl FnMut(&source::SourceFile) -> ParseResult,
) -> Analysis {
    // Every unit sees every other unit's definitions, which is how a name
    // one file defines resolves in another without any import syntax.
    let linkage = linkage::Linkage::resolve(map, options.entry);
    let mut diagnostics = linkage.diagnostics().to_vec();
    let mut units = Vec::new();
    for source in map.files() {
        let mut parse = parse_file(source);
        let (model, semantic_diagnostics) = semantic::analyze_unit(&parse.file, source, &linkage);
        diagnostics.append(&mut parse.diagnostics);
        diagnostics.extend(semantic_diagnostics);
        if options.strict_k65 {
            diagnostics.extend(compat::strict_k65(model.program(&parse.file), source));
        }
        units.push(AnalyzedUnit {
            file: source.id(),
            parse,
            model,
        });
    }
    // And what lowering would say. An `inline`'s arguments are checked where
    // it is expanded, which is here and not in analysis — so `K816-E0167` on
    // a link-time argument and `K816-E0170` on one that overruns its
    // parameter's narrowing are *errors* the build reports, and until this
    // ran, the editor showed the file clean and the next build refused it.
    // That is the disagreement sub-plan H's decision 1 exists to prevent.
    //
    // Only over a program with no errors, which is the rule
    // [`compile_to_ca65`] follows for the same reason: lowering a body
    // analysis refused reaches shapes it was promised were checked, and
    // answers `K816-E0900` about a construct the author has already been told
    // about. The two paths agree about *when* to lower, so they agree about
    // what lowering says.
    if !diagnostic::has_errors(&diagnostics) {
        for unit in &units {
            let source = map.file(unit.file);
            let (_, lowered) = lower::lower_unit(
                unit.model.program(&unit.parse.file),
                &unit.model,
                source.name(),
            );
            diagnostics.extend(lowered);
        }
    }
    Analysis {
        diagnostics,
        units,
        linkage,
    }
}

/// Compiles one file to ca65 assembly.
///
/// Runs the complete pipeline — parse, analyze, lower, emit — and returns
/// the assembly together with all diagnostics. Errors suppress emission but
/// never stop diagnostic collection.
pub fn compile_to_ca65(map: &SourceMap, file: FileId, options: &Options) -> CompileResult {
    let linkage = linkage::Linkage::resolve(map, options.entry);
    let source = map.file(file);
    let parse = parser::parse_source(source, None);
    let (model, semantic_diagnostics) = semantic::analyze_unit(&parse.file, source, &linkage);
    let mut diagnostics = linkage.diagnostics().to_vec();
    diagnostics.extend(parse.diagnostics);
    diagnostics.extend(semantic_diagnostics);
    // Strict mode runs on the program the build compiles — after the
    // preprocessor, so a construct inside a region this unit did not take is
    // not this unit's to answer for — and its extensions are errors, so it
    // runs before the check that stops emission.
    if options.strict_k65 {
        diagnostics.extend(compat::strict_k65(model.program(&parse.file), source));
    }
    if diagnostic::has_errors(&diagnostics) {
        return CompileResult {
            assembly: None,
            diagnostics,
            binaries: Vec::new(),
            imports: imported_paths(map),
        };
    }

    let (mut program, lower_diagnostics) =
        lower::lower_unit(model.program(&parse.file), &model, source.name());
    diagnostics.extend(lower_diagnostics);
    diagnostic::drop_repeats(&mut diagnostics);
    if diagnostic::has_errors(&diagnostics) {
        return CompileResult {
            assembly: None,
            diagnostics,
            binaries: Vec::new(),
            imports: imported_paths(map),
        };
    }

    if options.optimize {
        optimize::optimize(
            &mut program,
            &optimize::Context {
                model: &model,
                linkage: &linkage,
                file,
            },
        );
    }
    // Collected before emission rather than after, because the optimizer does
    // not touch data operations and the answer is the same either way — and
    // collecting from the emitted text would mean re-deriving a path the
    // emitter deliberately writes as the source wrote it.
    let mut binaries: Vec<PathBuf> = program
        .ops()
        .filter_map(|op| match &op.kind {
            op::KOpKind::Data(op::DataOp::IncBin { resolved, .. }) => Some(resolved.clone()),
            _ => None,
        })
        .collect();
    binaries.sort();
    binaries.dedup();

    let assembly = emit_ca65::emit(map, file, &model, &program, &linkage);
    CompileResult {
        assembly: Some(assembly),
        diagnostics,
        binaries,
        imports: imported_paths(map),
    }
}

/// Every file in `map` that an `import` reached, in a stable order.
fn imported_paths(map: &SourceMap) -> Vec<PathBuf> {
    let mut paths: Vec<PathBuf> = map
        .files()
        .filter(|source| source.origin() == source::Origin::Imported)
        .map(|source| PathBuf::from(source.name()))
        .collect();
    paths.sort();
    paths
}

/// Result of dumping a file's IR (`emit-ir`).
#[derive(Debug)]
pub struct IrResult {
    /// The IR dump, or `None` when errors prevented lowering.
    pub ir: Option<String>,
    /// What the optimizer did, when it ran.
    ///
    /// Carried rather than rendered here: `emit-ir --after-opt --explain` is
    /// the answer to "where did my instruction go", and the justification it
    /// prints comes from the same [`optimize::Change`] the pass tests assert
    /// on, so the explanation and the test cannot disagree.
    pub log: Option<optimize::OptimizationLog>,
    /// The same log, rendered — one line per change with its position, one
    /// indented line with the justification the pass recorded.
    ///
    /// Carried beside the log rather than left to the caller because rendering
    /// needs the unit's symbol table, and `main.rs` does not have one: all
    /// compiler logic lives in the library, and the CLI displays what it is
    /// handed. The two cannot disagree — the text comes from the same
    /// [`optimize::Change`] the pass tests assert on.
    pub explanation: Option<String>,
    pub diagnostics: Vec<Diagnostic>,
}

/// Dumps the lowered [`op::KProgram`] of one file as readable IR, optimized
/// when `options.optimize` is set (`emit-ir --after-opt`).
pub fn emit_ir(map: &SourceMap, file: FileId, options: &Options) -> IrResult {
    let linkage = linkage::Linkage::resolve(map, options.entry);
    let source = map.file(file);
    let parse = parser::parse_source(source, None);
    let (model, semantic_diagnostics) = semantic::analyze_unit(&parse.file, source, &linkage);
    let mut diagnostics = linkage.diagnostics().to_vec();
    diagnostics.extend(parse.diagnostics);
    diagnostics.extend(semantic_diagnostics);
    if options.strict_k65 {
        diagnostics.extend(compat::strict_k65(model.program(&parse.file), source));
    }
    if diagnostic::has_errors(&diagnostics) {
        return IrResult {
            ir: None,
            log: None,
            explanation: None,
            diagnostics,
        };
    }

    let (mut program, lower_diagnostics) =
        lower::lower_unit(model.program(&parse.file), &model, source.name());
    diagnostics.extend(lower_diagnostics);
    diagnostic::drop_repeats(&mut diagnostics);
    if diagnostic::has_errors(&diagnostics) {
        return IrResult {
            ir: None,
            log: None,
            explanation: None,
            diagnostics,
        };
    }

    let log = options.optimize.then(|| {
        optimize::optimize(
            &mut program,
            &optimize::Context {
                model: &model,
                linkage: &linkage,
                file,
            },
        )
    });
    let explanation = log.as_ref().map(|log| log.explain(&model.symbols));
    IrResult {
        ir: Some(program.dump(&model.symbols, map)),
        log,
        explanation,
        diagnostics,
    }
}
