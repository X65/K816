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

pub mod ast;
pub mod compat;
pub mod diagnostic;
pub mod effects;
pub mod emit_ca65;
pub mod isa;
pub mod lower;
pub mod lsp;
pub mod op;
pub mod optimize;
pub mod parser;
pub mod resolve;
pub mod semantic;
pub mod source;
pub mod symbol;
pub mod value;
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
}

impl Default for Options {
    fn default() -> Self {
        Self { optimize: true }
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
    let _ = options;
    let mut diagnostics = Vec::new();
    for source in map.files() {
        let parse = parser::parse_source(source, None);
        let (_, semantic_diagnostics) = semantic::analyze_file(&parse.file);
        diagnostics.extend(parse.diagnostics);
        diagnostics.extend(semantic_diagnostics);
    }
    Analysis { diagnostics }
}

/// Compiles one file to ca65 assembly.
///
/// Runs the complete pipeline — parse, analyze, lower, emit — and returns
/// the assembly together with all diagnostics. Errors suppress emission but
/// never stop diagnostic collection.
pub fn compile_to_ca65(map: &SourceMap, file: FileId, options: &Options) -> CompileResult {
    let parse = parser::parse_source(map.file(file), None);
    let (model, semantic_diagnostics) = semantic::analyze_file(&parse.file);
    let mut diagnostics = parse.diagnostics;
    diagnostics.extend(semantic_diagnostics);
    if diagnostic::has_errors(&diagnostics) {
        return CompileResult {
            assembly: None,
            diagnostics,
        };
    }

    let (mut program, lower_diagnostics) = lower::lower(&parse.file, &model);
    diagnostics.extend(lower_diagnostics);
    if diagnostic::has_errors(&diagnostics) {
        return CompileResult {
            assembly: None,
            diagnostics,
        };
    }

    if options.optimize {
        optimize::optimize(&mut program);
    }
    let assembly = emit_ca65::emit(map, file, &model, &program);
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
    pub diagnostics: Vec<Diagnostic>,
}

/// Dumps the lowered [`op::KProgram`] of one file as readable IR, optimized
/// when `options.optimize` is set (`emit-ir --after-opt`).
pub fn emit_ir(map: &SourceMap, file: FileId, options: &Options) -> IrResult {
    let parse = parser::parse_source(map.file(file), None);
    let (model, semantic_diagnostics) = semantic::analyze_file(&parse.file);
    let mut diagnostics = parse.diagnostics;
    diagnostics.extend(semantic_diagnostics);
    if diagnostic::has_errors(&diagnostics) {
        return IrResult {
            ir: None,
            diagnostics,
        };
    }

    let (mut program, lower_diagnostics) = lower::lower(&parse.file, &model);
    diagnostics.extend(lower_diagnostics);
    if diagnostic::has_errors(&diagnostics) {
        return IrResult {
            ir: None,
            diagnostics,
        };
    }

    if options.optimize {
        optimize::optimize(&mut program);
    }
    IrResult {
        ir: Some(program.dump(&model.symbols, map)),
        diagnostics,
    }
}
