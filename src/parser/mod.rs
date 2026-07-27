//! Parser adapter: wraps the generated Tree-sitter parser behind a small Rust
//! API and converts its CST into the owned semantic AST.
//!
//! Tree-sitter runtime types stop at this module boundary; semantic analysis,
//! lowering, optimization, and emission never see them.

use tree_sitter::Language;

unsafe extern "C" {
    fn tree_sitter_k65() -> *const ();
}

/// The generated K65/K816 Tree-sitter language.
pub fn language() -> Language {
    // SAFETY: `tree_sitter_k65` is the entry point of the parser generated
    // from `tree-sitter-k65/grammar.js`, compiled and linked by `build.rs`.
    unsafe { Language::from_raw(tree_sitter_k65() as *const _) }
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
