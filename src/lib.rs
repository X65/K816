//! K816 compiler frontend library.
//!
//! The library owns the complete pipeline — parsing, semantic analysis,
//! lowering, optimization, and ca65 emission — behind a small API consumed by
//! both the CLI (`main.rs`) and the LSP server. Neither tool reconstructs
//! project semantics on its own.

pub mod diagnostic;
pub mod parser;
pub mod source;

use std::path::Path;

use diagnostic::Diagnostic;
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
///
/// Stub until the parser adapter lands (sub-plan A3).
pub fn parse_file(map: &mut SourceMap, path: &Path) -> std::io::Result<FileId> {
    let text = std::fs::read_to_string(path)?;
    let id = map.add_file(path.to_string_lossy(), text);
    Ok(id)
}

/// Runs syntax and semantic analysis over every file in `map`.
///
/// Stub until the semantic frontend lands (sub-plans A3–A5, wired in A8).
pub fn analyze_workspace(_map: &SourceMap, _options: &Options) -> Analysis {
    unimplemented!("analyze_workspace lands with sub-plan A8")
}

/// Compiles one file to ca65 assembly.
///
/// Stub until emission lands (sub-plan A6).
pub fn compile_to_ca65(_map: &SourceMap, _file: FileId, _options: &Options) -> CompileResult {
    unimplemented!("compile_to_ca65 lands with sub-plan A6")
}
