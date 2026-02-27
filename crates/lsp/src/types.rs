use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::LazyLock;
use std::time::Duration;

use lsp_types::{Position, SymbolKind, Uri};
use serde::Deserialize;

pub(super) static INSTRUCTION_DESCRIPTIONS: LazyLock<HashMap<String, String>> =
    LazyLock::new(|| {
        serde_json::from_str(include_str!(
            "../../../editors/vscode-k816/resources/instructions-description.json"
        ))
        .expect("embedded instruction descriptions must be valid JSON")
    });

#[derive(Debug, Clone, Deserialize)]
pub(super) struct InstructionMetadata {
    #[serde(default)]
    pub(super) flags: Vec<String>,
    #[serde(default)]
    pub(super) cycles: String,
}

pub(super) static INSTRUCTION_METADATA: LazyLock<HashMap<String, InstructionMetadata>> =
    LazyLock::new(|| {
        serde_json::from_str(include_str!("../resources/instruction-metadata.json"))
            .expect("embedded instruction metadata must be valid JSON")
    });

pub(super) const PROJECT_MANIFEST: &str = "k816.toml";
pub(super) const PROJECT_SRC_DIR: &str = "src";
pub(super) const PROJECT_TESTS_DIR: &str = "tests";
pub(super) const DID_CHANGE_DEBOUNCE: Duration = Duration::from_millis(200);
pub(super) const LOOP_POLL_INTERVAL: Duration = Duration::from_millis(50);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(super) struct FileId(pub(super) u32);

#[derive(Debug, Default)]
pub(super) struct SourceIndex {
    next_id: u32,
    by_uri: HashMap<Uri, FileId>,
    by_id: HashMap<FileId, Uri>,
    by_path: HashMap<PathBuf, FileId>,
}

impl SourceIndex {
    pub(super) fn ensure_file_id(&mut self, uri: Uri, path: Option<PathBuf>) -> FileId {
        if let Some(existing) = self.by_uri.get(&uri) {
            return *existing;
        }

        let file_id = FileId(self.next_id);
        self.next_id += 1;
        self.by_uri.insert(uri.clone(), file_id);
        self.by_id.insert(file_id, uri);
        if let Some(path) = path {
            self.by_path.insert(path, file_id);
        }
        file_id
    }
}

#[derive(Debug, Clone)]
pub(super) struct ByteRange {
    pub(super) start: usize,
    pub(super) end: usize,
}

impl ByteRange {
    pub(super) fn from_span(span: k816_core::span::Span) -> Self {
        Self {
            start: span.start,
            end: span.end,
        }
    }
}

#[derive(Debug, Clone)]
pub(super) struct ScopeRange {
    pub(super) name: String,
    pub(super) range: ByteRange,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(super) enum SymbolCategory {
    Function,
    Constant,
    Variable,
    Label,
    DataBlock,
    Segment,
}

impl SymbolCategory {
    pub(super) fn symbol_kind(self) -> SymbolKind {
        match self {
            Self::Function => SymbolKind::FUNCTION,
            Self::Constant => SymbolKind::CONSTANT,
            Self::Variable => SymbolKind::VARIABLE,
            Self::Label => SymbolKind::CONSTANT,
            Self::DataBlock => SymbolKind::OBJECT,
            Self::Segment => SymbolKind::NAMESPACE,
        }
    }

    pub(super) fn detail(self) -> &'static str {
        match self {
            Self::Function => "function",
            Self::Constant => "constant",
            Self::Variable => "variable",
            Self::Label => "label",
            Self::DataBlock => "data block",
            Self::Segment => "segment",
        }
    }
}

#[derive(Debug, Clone)]
pub(super) struct SymbolDef {
    pub(super) canonical: String,
    pub(super) name: String,
    pub(super) category: SymbolCategory,
    pub(super) selection: ByteRange,
    pub(super) scope: Option<String>,
}

#[derive(Debug, Clone)]
pub(super) struct SymbolLocation {
    pub(super) uri: Uri,
    pub(super) name: String,
    pub(super) category: SymbolCategory,
    pub(super) selection: ByteRange,
}

#[derive(Debug, Clone)]
pub(super) struct SymbolOccurrence {
    pub(super) uri: Uri,
    pub(super) range: ByteRange,
    pub(super) is_declaration: bool,
}

#[derive(Debug, Clone, Default)]
pub(super) struct SemanticInfo {
    pub(super) functions: HashMap<String, k816_core::sema::FunctionMeta>,
    pub(super) consts: HashMap<String, k816_core::sema::ConstMeta>,
    pub(super) vars: HashMap<String, k816_core::sema::VarMeta>,
}

#[derive(Debug, Clone, Default)]
pub(super) struct DocumentAnalysis {
    pub(super) diagnostics: Vec<k816_core::diag::Diagnostic>,
    pub(super) symbols: Vec<SymbolDef>,
    pub(super) scopes: Vec<ScopeRange>,
    pub(super) semantic: SemanticInfo,
    pub(super) ast: Option<k816_core::ast::File>,
}

impl DocumentAnalysis {
    pub(super) fn scope_at_offset(&self, offset: usize) -> Option<&str> {
        self.scopes
            .iter()
            .filter(|scope| offset >= scope.range.start && offset <= scope.range.end)
            .min_by_key(|scope| scope.range.end.saturating_sub(scope.range.start))
            .map(|scope| scope.name.as_str())
    }
}

#[derive(Debug, Clone)]
pub(super) struct LineIndex {
    pub(super) line_starts: Vec<usize>,
}

impl LineIndex {
    pub(super) fn new(text: &str) -> Self {
        let mut line_starts = vec![0];
        for (offset, ch) in text.char_indices() {
            if ch == '\n' {
                line_starts.push(offset + 1);
            }
        }
        Self { line_starts }
    }

    pub(super) fn to_position(&self, text: &str, offset: usize) -> Position {
        let offset = offset.min(text.len());
        let line_index = self
            .line_starts
            .partition_point(|line_start| *line_start <= offset)
            .saturating_sub(1);
        let line_start = self.line_starts[line_index];
        let line_slice = &text[line_start..offset];
        let utf16_col = line_slice.encode_utf16().count() as u32;
        Position {
            line: line_index as u32,
            character: utf16_col,
        }
    }

    pub(super) fn to_offset(&self, text: &str, position: Position) -> Option<usize> {
        let line = usize::try_from(position.line).ok()?;
        if line >= self.line_starts.len() {
            return Some(text.len());
        }

        let line_start = self.line_starts[line];
        let line_end = if line + 1 < self.line_starts.len() {
            self.line_starts[line + 1]
        } else {
            text.len()
        };
        let line_text = &text[line_start..line_end];
        let target = usize::try_from(position.character).ok()?;

        let mut utf16_col = 0usize;
        for (offset, ch) in line_text.char_indices() {
            if utf16_col >= target {
                return Some(line_start + offset);
            }
            utf16_col += ch.len_utf16();
        }

        Some(line_end)
    }
}

#[derive(Debug, Clone)]
pub(super) struct DocumentState {
    pub(super) _file_id: FileId,
    pub(super) uri: Uri,
    pub(super) path: Option<PathBuf>,
    pub(super) version: i32,
    pub(super) open: bool,
    pub(super) text: String,
    pub(super) line_index: LineIndex,
    pub(super) analysis: DocumentAnalysis,
    pub(super) object: Option<k816_o65::O65Object>,
    pub(super) addressable_sites: Vec<k816_core::AddressableSite>,
    pub(super) resolved_sites: Vec<(k816_core::span::Span, u32, u32)>,
}

impl DocumentState {
    pub(super) fn address_at_offset(&self, offset: usize) -> Option<(u32, u32)> {
        let idx = self
            .resolved_sites
            .partition_point(|(span, _, _)| span.start <= offset);
        if idx > 0 {
            let (span, addr, size) = &self.resolved_sites[idx - 1];
            if offset < span.end {
                return Some((*addr, *size));
            }
        }
        None
    }
}

#[derive(Debug)]
pub(super) struct ServerState {
    pub(super) workspace_root: PathBuf,
    pub(super) source_index: SourceIndex,
    pub(super) documents: HashMap<Uri, DocumentState>,
    pub(super) symbols: HashMap<String, Vec<SymbolLocation>>,
    pub(super) symbol_occurrences: HashMap<String, Vec<SymbolOccurrence>>,
    pub(super) linker_config: k816_link::LinkerConfig,
    pub(super) last_link_layout: Option<k816_link::LinkedLayout>,
    pub(super) last_link_error: Option<String>,
}

impl ServerState {
    pub(super) fn new(workspace_root: PathBuf) -> Self {
        Self {
            workspace_root,
            source_index: SourceIndex::default(),
            documents: HashMap::new(),
            symbols: HashMap::new(),
            symbol_occurrences: HashMap::new(),
            linker_config: k816_link::default_stub_config(),
            last_link_layout: None,
            last_link_error: None,
        }
    }
}
