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
pub mod compat;
pub mod diagnostic;
pub mod effects;
pub mod emit_ca65;
pub mod eval;
pub mod expand;
pub mod hla;
pub mod isa;
pub mod layout;
pub mod linkage;
pub mod lower;
pub mod lsp;
pub mod op;
pub mod optimize;
pub mod parser;
pub mod preprocess;
pub mod resolve;
pub mod semantic;
pub mod source;
pub mod symbol;
pub mod value;
pub mod width;
pub mod workspace;

use std::path::Path;

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

/// Result of semantic analysis over a workspace. The LSP stops here; the CLI
/// may continue to ca65 emission.
#[derive(Debug, Default)]
pub struct Analysis {
    /// Syntax and semantic diagnostics for all analyzed files.
    pub diagnostics: Vec<Diagnostic>,
}

/// Result of compiling one file to ca65 assembly.
#[derive(Debug)]
pub struct CompileResult {
    /// The emitted ca65 assembly, or `None` when errors prevented emission.
    pub assembly: Option<String>,
    pub diagnostics: Vec<Diagnostic>,
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
    // Every unit sees every other unit's definitions, which is how a name
    // one file defines resolves in another without any import syntax.
    let linkage = linkage::Linkage::resolve(map, options.entry);
    let mut diagnostics = linkage.diagnostics().to_vec();
    for source in map.files() {
        let parse = parser::parse_source(source, None);
        let (model, semantic_diagnostics) =
            semantic::analyze_unit(&parse.file, source.name(), &linkage);
        diagnostics.extend(parse.diagnostics);
        diagnostics.extend(semantic_diagnostics);
        if options.strict_k65 {
            diagnostics.extend(compat::strict_k65(model.program(&parse.file), source));
        }
    }
    Analysis { diagnostics }
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
    let (model, semantic_diagnostics) =
        semantic::analyze_unit(&parse.file, source.name(), &linkage);
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
        };
    }

    let (mut program, lower_diagnostics) =
        lower::lower_unit(model.program(&parse.file), &model, source.name());
    diagnostics.extend(lower_diagnostics);
    if diagnostic::has_errors(&diagnostics) {
        return CompileResult {
            assembly: None,
            diagnostics,
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
    let assembly = emit_ca65::emit(map, file, &model, &program, &linkage);
    CompileResult {
        assembly: Some(assembly),
        diagnostics,
    }
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
    let (model, semantic_diagnostics) =
        semantic::analyze_unit(&parse.file, source.name(), &linkage);
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
