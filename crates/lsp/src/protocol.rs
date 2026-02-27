use serde::{Deserialize, Serialize};

use super::SymbolCategory;

#[derive(Debug, Deserialize)]
pub(super) struct ResolveAddressesParams {
    pub uri: String,
    pub lines: Vec<u32>,
}

#[derive(Debug, Serialize)]
pub(super) struct ResolveAddressesResult {
    pub addresses: Vec<Option<u32>>,
}

#[derive(Debug, Deserialize)]
pub(super) struct ResolveInlineSymbolsParams {
    pub uri: String,
    pub start_line: u32,
    pub end_line: u32,
}

#[derive(Debug, Serialize)]
pub(super) struct ResolveInlineSymbolsResult {
    pub symbols: Vec<ResolveInlineSymbol>,
}

#[derive(Debug, Serialize)]
pub(super) struct ResolveInlineSymbol {
    pub name: String,
    pub category: String,
    pub start_line: u32,
    pub start_character: u32,
    pub end_line: u32,
    pub end_character: u32,
    pub address: u32,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub read_size_hint: Option<u32>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize, Default)]
#[serde(rename_all = "snake_case")]
pub(super) enum QueryMemoryMapDetail {
    #[default]
    Summary,
    Runs,
}

#[derive(Debug, Deserialize, Default)]
pub(super) struct QueryMemoryMapParams {
    #[serde(default)]
    pub memory_name: Option<String>,
    #[serde(default)]
    pub detail: QueryMemoryMapDetail,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub(super) enum QueryMemoryMapStatus {
    Ok,
    Unavailable,
}

#[derive(Debug, Serialize)]
pub(super) struct QueryMemoryMapResult {
    pub status: QueryMemoryMapStatus,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub reason: Option<String>,
    pub memories: Vec<QueryMemoryMapMemory>,
    pub runs: Vec<QueryMemoryMapRun>,
}

#[derive(Debug, Serialize)]
pub(super) struct QueryMemoryMapMemory {
    pub name: String,
    pub start: u32,
    pub size: u32,
    pub kind: String,
    pub used: u32,
    pub free: u32,
    pub utilization_percent: f64,
}

#[derive(Debug, Serialize)]
pub(super) struct QueryMemoryMapRun {
    pub memory_name: String,
    pub start: u32,
    pub end: u32,
    pub size: u32,
}

pub(super) fn memory_kind_label(kind: k816_link::MemoryKind) -> &'static str {
    match kind {
        k816_link::MemoryKind::ReadOnly => "read_only",
        k816_link::MemoryKind::ReadWrite => "read_write",
    }
}

pub(super) fn symbol_category_label(category: SymbolCategory) -> &'static str {
    match category {
        SymbolCategory::Function => "function",
        SymbolCategory::Constant => "constant",
        SymbolCategory::Variable => "variable",
        SymbolCategory::Label => "label",
        SymbolCategory::DataBlock => "data_block",
        SymbolCategory::Segment => "segment",
    }
}
