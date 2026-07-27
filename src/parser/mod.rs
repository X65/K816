/*****************************************************************************/
/*                                                                           */
/*                                   mod.rs                                  */
/*                                                                           */
/*           Parser adapter around the generated Tree-sitter parser          */
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

//! Parser adapter: wraps the generated Tree-sitter parser behind a small Rust
//! API and converts its CST into the owned semantic AST.
//!
//! Tree-sitter runtime types stop at this module boundary; semantic analysis,
//! lowering, optimization, and emission never see them.

mod ast_builder;
mod cst;
mod diagnostics;

pub use cst::SyntaxTree;

use crate::ast::AstFile;
use crate::diagnostic::Diagnostic;
use crate::source::SourceFile;

use tree_sitter::Language;

unsafe extern "C" {
    fn tree_sitter_k65() -> *const ();
}

/// The generated K65/K816 Tree-sitter language.
fn language() -> Language {
    // SAFETY: `tree_sitter_k65` is the entry point of the parser generated
    // from `tree-sitter-k65/grammar.js`, compiled and linked by `build.rs`.
    unsafe { Language::from_raw(tree_sitter_k65() as *const _) }
}

/// Result of parsing one source file: the retained syntax tree, the owned
/// AST, and structured syntax diagnostics.
pub struct ParseResult {
    pub tree: SyntaxTree,
    pub file: AstFile,
    pub diagnostics: Vec<Diagnostic>,
}

/// Parses `source`, optionally reusing a `previous` tree for an incremental
/// reparse.
///
/// A `previous` tree must already have been edited (via Tree-sitter's edit
/// protocol) to describe the change; callers without an edited tree — the CLI,
/// and the slice LSP which reparses from scratch — pass `None`.
pub fn parse_source(source: &SourceFile, previous: Option<&SyntaxTree>) -> ParseResult {
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&language())
        .expect("generated grammar ABI matches the tree-sitter runtime");
    let tree = parser
        .parse(source.text(), previous.map(|previous| &previous.tree))
        .expect("parsing is infallible without a timeout or cancellation");

    let root = tree.root_node();
    let mut diagnostics = diagnostics::collect_syntax_diagnostics(root, source);
    let (file, mut internal) = ast_builder::build(root, source);
    diagnostics.append(&mut internal);

    ParseResult {
        tree: SyntaxTree { tree },
        file,
        diagnostics,
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn generated_language_loads_and_knows_slice_nodes() {
        let language = super::language();
        assert!(language.abi_version() >= 14);
        let kinds: Vec<&str> = (0..language.node_kind_count() as u16)
            .filter_map(|id| language.node_kind_for_id(id))
            .collect();
        for expected in [
            "source_file",
            "var_declaration",
            "func_declaration",
            "width_annotation",
            "block",
            "native_instruction",
            "hla_load_immediate",
            "hla_store",
            "return_statement",
            "immediate",
            "address",
            "number",
            "identifier",
        ] {
            assert!(kinds.contains(&expected), "missing node kind {expected}");
        }
    }
}
