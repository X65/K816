mod analysis;
mod convert;
mod debug;
mod document_symbols;
mod editor;
mod hover;
mod navigation;
mod project;
mod protocol;
mod server;
mod text;
mod types;
mod workspace;

use self::analysis::{
    analyze_document, canonical_symbol, completion_kind_for_symbol,
    extract_unknown_identifier_name, semantic_token_legend_modifiers, semantic_token_legend_types,
    semantic_token_modifier_bit, semantic_token_type_for_category, semantic_token_type_index,
};
use self::convert::{
    byte_range_to_lsp, dedup_diagnostics, diagnostic_to_lsp, is_valid_symbol_name,
};
use self::types::{
    ByteRange, DocumentAnalysis, DocumentState, INSTRUCTION_DESCRIPTIONS, INSTRUCTION_METADATA,
    LineIndex, PROJECT_MANIFEST, PROJECT_SRC_DIR, PROJECT_TESTS_DIR, ScopeRange, SemanticInfo,
    ServerState, SymbolCategory, SymbolDef, SymbolLocation, SymbolOccurrence,
};

#[cfg(test)]
use self::protocol::{
    QueryMemoryMapDetail, QueryMemoryMapParams, QueryMemoryMapStatus, ResolveInlineSymbolsParams,
};
#[cfg(test)]
use self::server::determine_workspace_root;
#[cfg(test)]
use self::text::{in_symbol_completion_context, token_at_offset, token_prefix_at_offset};
#[cfg(test)]
use lsp_types::InitializeParams;
#[cfg(test)]
use lsp_types::ReferenceContext;
#[cfg(test)]
use lsp_types::{GotoDefinitionResponse, HoverContents};
#[cfg(test)]
use std::path::PathBuf;

pub use self::server::run_stdio_server;

#[cfg(test)]
mod tests;
