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

pub use cst::{SourceEdit, SyntaxTree};

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
    /// Every node kind `ast_builder` matches on by name. A grammar rename
    /// would otherwise drop the matching arm silently, leaving the construct
    /// to fall through to the builder's "unexpected node" path.
    const MATCHED_NODE_KINDS: &[&str] = &[
        // items
        "source_file",
        "var_declaration",
        "var_entry",
        "storage_class",
        "type_suffix",
        "field_list",
        "field_declaration",
        "const_declaration",
        "const_entry",
        "segment_declaration",
        "func_declaration",
        "far_marker",
        "call_contract",
        "contract_inputs",
        "contract_parameter",
        "contract_outputs",
        "module_width",
        "width_annotation",
        "block",
        // statements
        "label",
        "native_instruction",
        "block_move",
        "hla_assignment",
        "hla_exchange",
        "hla_arithmetic",
        "hla_shift",
        "hla_step",
        "hla_flag",
        "flag_name",
        "hla_stack",
        "hla_nop",
        "hla_break",
        "width_block",
        "call_statement",
        "bare_call",
        "branch_condition",
        "conditional_block",
        "else_clause",
        "conditional_jump",
        "postfix_loop",
        "loop_condition",
        "goto_statement",
        "break_statement",
        "repeat_statement",
        "nocross_block",
        "data_block",
        "data_body",
        "align_directive",
        "address_directive",
        "nocross_directive",
        "charset_directive",
        "data_label",
        "typed_entry",
        "data_value",
        "undefined_byte",
        "code_subblock",
        "data_repeat",
        "for_eval",
        "eval_block",
        "preprocessor_directive",
        "directive_name",
        "binary_entry",
        "binary_declaration",
        "reserved_image_block",
        "reserved_image_toplevel",
        "bank_declaration",
        "evalfunc_declaration",
        "evaluator_entry",
        "hla_illegal",
        "image_scan",
        "reserved_eval_call",
        "reserved_function",
        "return_statement",
        // operands
        "mnemonic",
        "immediate",
        "address",
        "address_encoding",
        "index_register",
        "indirect",
        "long_indirect",
        "register",
        // expressions
        "parenthesized_expression",
        "unary_expression",
        "address_of_expression",
        "binary_expression",
        "address_and_expression",
        "field_access",
        "field_name",
        "subscript_expression",
        "metadata_query",
        "typed_view",
        "eval_bracket",
        "eval_parenthesized",
        "eval_call",
        "eval_subscript",
        "eval_member",
        "eval_method_call",
        "eval_pi",
        "eval_if",
        "eval_while",
        "eval_statement_block",
        "eval_unary",
        "eval_binary",
        "eval_ternary",
        "eval_assignment",
        "eval_list",
        "float",
        "string",
        "number",
        "char_literal",
        "identifier",
        "local_identifier",
        "line_comment",
        "block_comment",
    ];

    #[test]
    fn the_generated_language_has_every_node_kind_the_builder_matches() {
        let language = super::language();
        assert!(language.abi_version() >= 14);
        let kinds: Vec<&str> = (0..language.node_kind_count() as u16)
            .filter_map(|id| language.node_kind_for_id(id))
            .collect();
        for expected in MATCHED_NODE_KINDS {
            assert!(kinds.contains(expected), "missing node kind {expected}");
        }
    }
}
