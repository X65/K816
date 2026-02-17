use std::collections::{BTreeSet, HashMap};
use std::fs;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::sync::LazyLock;
use std::time::{Duration, Instant};

use anyhow::{Context, Result};
use crossbeam_channel::RecvTimeoutError;
use lsp_server::{Connection, Message, Notification, Request, RequestId, Response, ResponseError};
use lsp_types::notification::{
    DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument, DidSaveTextDocument,
    Notification as LspNotification, PublishDiagnostics,
};
use lsp_types::request::{
    Completion, DocumentSymbolRequest, Formatting, GotoDefinition, HoverRequest,
    Request as LspRequest,
};
use lsp_types::{
    CompletionItem, CompletionItemKind, CompletionOptions, CompletionParams, CompletionResponse,
    Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity, DidChangeTextDocumentParams,
    DidCloseTextDocumentParams, DidOpenTextDocumentParams, DidSaveTextDocumentParams,
    DocumentFormattingParams, DocumentSymbol, DocumentSymbolParams, DocumentSymbolResponse,
    GotoDefinitionParams, GotoDefinitionResponse, Hover, HoverContents, HoverParams,
    InitializeParams, Location, MarkupContent, MarkupKind, OneOf, Position, Range,
    ServerCapabilities, SymbolKind, TextDocumentSyncCapability, TextDocumentSyncKind, TextEdit,
    Uri,
};
use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};
use walkdir::WalkDir;

static INSTRUCTION_DESCRIPTIONS: LazyLock<HashMap<String, String>> = LazyLock::new(|| {
    serde_json::from_str(include_str!(
        "../../../editors/vscode-k816/resources/instructions-description.json"
    ))
    .expect("embedded instruction descriptions must be valid JSON")
});

const PROJECT_MANIFEST: &str = "k816.toml";
const PROJECT_SRC_DIR: &str = "src";
const PROJECT_TESTS_DIR: &str = "tests";
const DID_CHANGE_DEBOUNCE: Duration = Duration::from_millis(200);
const LOOP_POLL_INTERVAL: Duration = Duration::from_millis(50);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct FileId(u32);

#[derive(Debug, Default)]
struct SourceIndex {
    next_id: u32,
    by_uri: HashMap<Uri, FileId>,
    by_id: HashMap<FileId, Uri>,
    by_path: HashMap<PathBuf, FileId>,
}

impl SourceIndex {
    fn ensure_file_id(&mut self, uri: Uri, path: Option<PathBuf>) -> FileId {
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
struct ByteRange {
    start: usize,
    end: usize,
}

impl ByteRange {
    fn from_span(span: k816_core::span::Span) -> Self {
        Self {
            start: span.start,
            end: span.end,
        }
    }
}

#[derive(Debug, Clone)]
struct ScopeRange {
    name: String,
    range: ByteRange,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum SymbolCategory {
    Function,
    Constant,
    Variable,
    Label,
    DataBlock,
    Segment,
}

impl SymbolCategory {
    fn symbol_kind(self) -> SymbolKind {
        match self {
            Self::Function => SymbolKind::FUNCTION,
            Self::Constant => SymbolKind::CONSTANT,
            Self::Variable => SymbolKind::VARIABLE,
            Self::Label => SymbolKind::CONSTANT,
            Self::DataBlock => SymbolKind::OBJECT,
            Self::Segment => SymbolKind::NAMESPACE,
        }
    }

    fn detail(self) -> &'static str {
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
struct SymbolDef {
    canonical: String,
    name: String,
    category: SymbolCategory,
    selection: ByteRange,
    scope: Option<String>,
}

#[derive(Debug, Clone)]
struct SymbolLocation {
    uri: Uri,
    name: String,
    category: SymbolCategory,
    selection: ByteRange,
}

#[derive(Debug, Clone, Default)]
struct SemanticInfo {
    functions: HashMap<String, k816_core::sema::FunctionMeta>,
    consts: HashMap<String, k816_core::sema::ConstMeta>,
    vars: HashMap<String, k816_core::sema::VarMeta>,
}

#[derive(Debug, Clone, Default)]
struct DocumentAnalysis {
    diagnostics: Vec<k816_core::diag::Diagnostic>,
    symbols: Vec<SymbolDef>,
    scopes: Vec<ScopeRange>,
    semantic: SemanticInfo,
    ast: Option<k816_core::ast::File>,
}

impl DocumentAnalysis {
    fn scope_at_offset(&self, offset: usize) -> Option<&str> {
        self.scopes
            .iter()
            .filter(|scope| offset >= scope.range.start && offset <= scope.range.end)
            .min_by_key(|scope| scope.range.end.saturating_sub(scope.range.start))
            .map(|scope| scope.name.as_str())
    }
}

#[derive(Debug, Clone)]
struct LineIndex {
    line_starts: Vec<usize>,
}

impl LineIndex {
    fn new(text: &str) -> Self {
        let mut line_starts = vec![0];
        for (offset, ch) in text.char_indices() {
            if ch == '\n' {
                line_starts.push(offset + 1);
            }
        }
        Self { line_starts }
    }

    fn to_position(&self, text: &str, offset: usize) -> Position {
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

    fn to_offset(&self, text: &str, position: Position) -> Option<usize> {
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
struct DocumentState {
    _file_id: FileId,
    uri: Uri,
    path: Option<PathBuf>,
    version: i32,
    open: bool,
    text: String,
    line_index: LineIndex,
    analysis: DocumentAnalysis,
    object: Option<k816_o65::O65Object>,
    addressable_sites: Vec<k816_core::AddressableSite>,
    resolved_sites: Vec<(k816_core::span::Span, u32, u32)>,
}

impl DocumentState {
    fn address_at_offset(&self, offset: usize) -> Option<(u32, u32)> {
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
struct ServerState {
    workspace_root: PathBuf,
    source_index: SourceIndex,
    documents: HashMap<Uri, DocumentState>,
    symbols: HashMap<String, Vec<SymbolLocation>>,
    linker_config: k816_link::LinkerConfig,
    last_link_layout: Option<k816_link::LinkedLayout>,
    last_link_error: Option<String>,
}

impl ServerState {
    fn new(workspace_root: PathBuf) -> Self {
        Self {
            workspace_root,
            source_index: SourceIndex::default(),
            documents: HashMap::new(),
            symbols: HashMap::new(),
            linker_config: k816_link::default_stub_config(),
            last_link_layout: None,
            last_link_error: None,
        }
    }

    fn initialize_workspace(&mut self) -> Result<()> {
        self.load_linker_config();
        let sources = discover_workspace_sources(&self.workspace_root)?;
        eprintln!(
            "k816-lsp: workspace '{}', discovered {} source file(s)",
            self.workspace_root.display(),
            sources.len(),
        );
        for path in sources {
            if let Err(error) = self.load_from_disk(path.clone()) {
                eprintln!("k816-lsp: skipping '{}': {error}", path.display());
            }
        }
        self.rebuild_symbol_index();
        eprintln!(
            "k816-lsp: indexed {} unique symbol(s) across {} document(s)",
            self.symbols.len(),
            self.documents.len(),
        );
        Ok(())
    }

    fn load_linker_config(&mut self) {
        let manifest_path = self.workspace_root.join(PROJECT_MANIFEST);
        if manifest_path.is_file()
            && let Ok(text) = fs::read_to_string(&manifest_path)
            && let Ok(manifest) = toml::from_str::<ProjectManifest>(&text)
            && let Some(script) = manifest.link.script
        {
            let config_path = if script.is_absolute() {
                script
            } else {
                self.workspace_root.join(script)
            };
            if let Ok(config) = k816_link::load_config(&config_path) {
                eprintln!(
                    "k816-lsp: loaded linker config from '{}'",
                    config_path.display()
                );
                self.linker_config = config;
                return;
            }
        }

        let default_script = self.workspace_root.join("link.ron");
        if default_script.is_file()
            && let Ok(config) = k816_link::load_config(&default_script)
        {
            eprintln!(
                "k816-lsp: loaded linker config from '{}'",
                default_script.display()
            );
            self.linker_config = config;
            return;
        }

        eprintln!("k816-lsp: using default stub linker config");
    }

    fn load_from_disk(&mut self, path: PathBuf) -> Result<()> {
        let text = fs::read_to_string(&path)
            .with_context(|| format!("failed to read source '{}'", path.display()))?;
        let uri = uri_from_file_path(&path)?;
        let file_id = self
            .source_index
            .ensure_file_id(uri.clone(), Some(path.clone()));
        let source_name = path.display().to_string();
        eprintln!("k816-lsp: analyzing '{source_name}'");
        let (analysis, object, addressable_sites) = analyze_document(&source_name, &text);
        self.documents.insert(
            uri.clone(),
            DocumentState {
                _file_id: file_id,
                uri,
                path: Some(path),
                version: 0,
                open: false,
                line_index: LineIndex::new(&text),
                text,
                analysis,
                object,
                addressable_sites,
                resolved_sites: Vec::new(),
            },
        );
        Ok(())
    }

    fn upsert_document(&mut self, uri: Uri, text: String, version: i32, open: bool) -> Result<()> {
        self.upsert_document_without_analysis(uri.clone(), text, version, open);
        self.analyze_document(&uri);
        Ok(())
    }

    fn upsert_document_without_analysis(
        &mut self,
        uri: Uri,
        text: String,
        version: i32,
        open: bool,
    ) {
        let path = uri_to_file_path(&uri);
        let file_id = self.source_index.ensure_file_id(uri.clone(), path.clone());
        let prev = self.documents.remove(&uri);
        let analysis = prev
            .as_ref()
            .map(|doc| doc.analysis.clone())
            .unwrap_or_default();
        let object = prev.as_ref().and_then(|doc| doc.object.clone());
        let addressable_sites = prev
            .as_ref()
            .map(|doc| doc.addressable_sites.clone())
            .unwrap_or_default();
        let resolved_sites = prev.map(|doc| doc.resolved_sites).unwrap_or_default();

        self.documents.insert(
            uri.clone(),
            DocumentState {
                _file_id: file_id,
                uri,
                path,
                version,
                open,
                line_index: LineIndex::new(&text),
                text,
                analysis,
                object,
                addressable_sites,
                resolved_sites,
            },
        );
    }

    fn analyze_document(&mut self, uri: &Uri) {
        let Some(doc) = self.documents.get_mut(uri) else {
            return;
        };
        let source_name = doc
            .path
            .as_ref()
            .map(|path| path.display().to_string())
            .unwrap_or_else(|| doc.uri.to_string());
        eprintln!("k816-lsp: analyzing '{source_name}'");
        let (mut new_analysis, object, addressable_sites) =
            analyze_document(&source_name, &doc.text);
        if new_analysis.symbols.is_empty() && !doc.analysis.symbols.is_empty() {
            new_analysis.symbols = doc.analysis.symbols.clone();
            new_analysis.scopes = doc.analysis.scopes.clone();
        }
        doc.analysis = new_analysis;
        doc.object = object;
        doc.addressable_sites = addressable_sites;
        self.rebuild_symbol_index();
    }

    fn close_document(&mut self, uri: &Uri) {
        if let Some(path) = self.documents.get(uri).and_then(|doc| doc.path.clone())
            && let Ok(text) = fs::read_to_string(&path)
        {
            let file_id = self
                .source_index
                .ensure_file_id(uri.clone(), Some(path.clone()));
            let (analysis, object, addressable_sites) =
                analyze_document(&path.display().to_string(), &text);
            self.documents.insert(
                uri.clone(),
                DocumentState {
                    _file_id: file_id,
                    uri: uri.clone(),
                    path: Some(path),
                    version: 0,
                    open: false,
                    line_index: LineIndex::new(&text),
                    text,
                    analysis,
                    object,
                    addressable_sites,
                    resolved_sites: Vec::new(),
                },
            );
            self.rebuild_symbol_index();
            return;
        }

        self.documents.remove(uri);
        self.rebuild_symbol_index();
    }

    fn rebuild_symbol_index(&mut self) {
        self.symbols.clear();
        for doc in self.documents.values() {
            for symbol in &doc.analysis.symbols {
                self.symbols
                    .entry(symbol.canonical.clone())
                    .or_default()
                    .push(SymbolLocation {
                        uri: doc.uri.clone(),
                        name: symbol.name.clone(),
                        category: symbol.category,
                        selection: symbol.selection.clone(),
                    });
            }
        }
        self.try_link_workspace();
    }

    fn try_link_workspace(&mut self) {
        // Collect (uri, cloned object) pairs — must clone to release the borrow.
        let doc_entries: Vec<(Uri, k816_o65::O65Object)> = self
            .documents
            .iter()
            .filter_map(|(uri, doc)| doc.object.as_ref().map(|obj| (uri.clone(), obj.clone())))
            .collect();

        if doc_entries.is_empty() {
            for doc in self.documents.values_mut() {
                doc.resolved_sites.clear();
            }
            self.last_link_layout = None;
            self.last_link_error = Some("no compiled objects are available to link".to_string());
            return;
        }

        let objects: Vec<k816_o65::O65Object> =
            doc_entries.iter().map(|(_, obj)| obj.clone()).collect();

        match k816_link::link_objects_with_options(
            &objects,
            &self.linker_config,
            k816_link::LinkRenderOptions::plain(),
        ) {
            Ok(output) => {
                self.last_link_layout = Some(output.clone());
                self.last_link_error = None;
                for (obj_idx, (uri, _)) in doc_entries.iter().enumerate() {
                    if let Some(doc) = self.documents.get_mut(uri) {
                        let mut sites = Vec::new();
                        for site in &doc.addressable_sites {
                            let key = (obj_idx, site.segment.clone());
                            if let Some(placements) = output.section_placements.get(&key)
                                && let Some(addr) =
                                    k816_link::resolve_symbol_addr(placements, site.offset)
                            {
                                sites.push((site.span, addr, site.size));
                            }
                        }
                        sites.sort_by_key(|(span, _, _)| span.start);
                        doc.resolved_sites = sites;
                    }
                }
            }
            Err(error) => {
                self.last_link_layout = None;
                self.last_link_error = Some(error.to_string());
                for doc in self.documents.values_mut() {
                    doc.resolved_sites.clear();
                }
            }
        }
    }

    fn query_memory_map(&self, params: &QueryMemoryMapParams) -> QueryMemoryMapResult {
        let Some(layout) = self.last_link_layout.as_ref() else {
            return QueryMemoryMapResult {
                status: QueryMemoryMapStatus::Unavailable,
                reason: self
                    .last_link_error
                    .clone()
                    .or_else(|| Some("memory map is unavailable".to_string())),
                memories: Vec::new(),
                runs: Vec::new(),
            };
        };

        let filter = params
            .memory_name
            .as_deref()
            .map(str::trim)
            .filter(|name| !name.is_empty());

        let filtered_specs: Vec<&k816_link::MemoryArea> = self
            .linker_config
            .memory
            .iter()
            .filter(|memory| match filter {
                Some(name) => memory.name.eq_ignore_ascii_case(name),
                None => true,
            })
            .collect();

        if filtered_specs.is_empty() {
            return QueryMemoryMapResult {
                status: QueryMemoryMapStatus::Unavailable,
                reason: filter
                    .map(|name| format!("memory area '{name}' was not found in linker config")),
                memories: Vec::new(),
                runs: Vec::new(),
            };
        }

        let mut memories = Vec::with_capacity(filtered_specs.len());
        for spec in filtered_specs {
            let used = layout
                .runs
                .iter()
                .filter(|run| run.memory_name == spec.name)
                .fold(0u64, |acc, run| acc.saturating_add(run.bytes.len() as u64));
            let size = u64::from(spec.size);
            let free = size.saturating_sub(used);
            let utilization_percent = if size == 0 {
                0.0
            } else {
                ((used as f64) * 100.0) / (size as f64)
            };

            memories.push(QueryMemoryMapMemory {
                name: spec.name.clone(),
                start: spec.start,
                size: spec.size,
                kind: memory_kind_label(spec.kind).to_string(),
                used: used.min(u64::from(u32::MAX)) as u32,
                free: free.min(u64::from(u32::MAX)) as u32,
                utilization_percent,
            });
        }

        let runs = if params.detail == QueryMemoryMapDetail::Runs {
            let mut rows = Vec::new();
            for run in &layout.runs {
                if let Some(name) = filter
                    && !run.memory_name.eq_ignore_ascii_case(name)
                {
                    continue;
                }
                let size = run.bytes.len().min(u32::MAX as usize) as u32;
                let end = if size == 0 {
                    run.start_addr
                } else {
                    run.start_addr.saturating_add(size.saturating_sub(1))
                };
                rows.push(QueryMemoryMapRun {
                    memory_name: run.memory_name.clone(),
                    start: run.start_addr,
                    end,
                    size,
                });
            }
            rows.sort_by_key(|run| (run.memory_name.clone(), run.start));
            rows
        } else {
            Vec::new()
        };

        QueryMemoryMapResult {
            status: QueryMemoryMapStatus::Ok,
            reason: None,
            memories,
            runs,
        }
    }

    fn lsp_diagnostics(&self, uri: &Uri) -> Vec<Diagnostic> {
        let Some(doc) = self.documents.get(uri) else {
            return Vec::new();
        };
        doc.analysis
            .diagnostics
            .iter()
            .filter(|diag| !self.is_cross_module_false_positive(diag))
            .map(|diag| diagnostic_to_lsp(diag, &doc.uri, &doc.line_index, &doc.text))
            .collect()
    }

    fn is_cross_module_false_positive(&self, diag: &k816_core::diag::Diagnostic) -> bool {
        if diag.severity != k816_core::diag::Severity::Error {
            return false;
        }
        if let Some(name) = extract_unknown_identifier_name(&diag.message) {
            return self.symbols.contains_key(&name);
        }
        false
    }

    fn definition(&self, uri: &Uri, position: Position) -> Option<GotoDefinitionResponse> {
        let doc = self.documents.get(uri)?;
        let offset = doc.line_index.to_offset(&doc.text, position)?;
        let token = token_at_offset(&doc.text, offset)?;
        let scope = doc.analysis.scope_at_offset(offset);
        let canonical = canonical_symbol(&token.text, scope);
        let definitions = self.symbols.get(&canonical)?;

        let mut locations = Vec::new();
        for definition in definitions {
            let Some(def_doc) = self.documents.get(&definition.uri) else {
                continue;
            };
            let range =
                byte_range_to_lsp(&definition.selection, &def_doc.line_index, &def_doc.text);
            locations.push(Location::new(definition.uri.clone(), range));
        }

        if locations.is_empty() {
            None
        } else {
            Some(GotoDefinitionResponse::Array(locations))
        }
    }

    fn hover(&self, uri: &Uri, position: Position) -> Option<Hover> {
        let doc = self.documents.get(uri)?;
        let offset = doc.line_index.to_offset(&doc.text, position)?;

        if let Some((range, value)) = numeric_literal_at_offset(&doc.text, offset) {
            return Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: hover_contents_for_numeric_literal(value),
                }),
                range: Some(byte_range_to_lsp(&range, &doc.line_index, &doc.text)),
            });
        }

        if let Some(token) = token_at_offset(&doc.text, offset) {
            let token_range = byte_range_to_lsp(
                &ByteRange {
                    start: token.start,
                    end: token.end,
                },
                &doc.line_index,
                &doc.text,
            );

            let scope = doc.analysis.scope_at_offset(offset);
            let canonical = canonical_symbol(&token.text, scope);
            if let Some(definitions) = self.symbols.get(&canonical)
                && let Some(definition) = definitions.first()
            {
                let contents = hover_contents_for_symbol(&canonical, definition, self);
                return Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: contents,
                    }),
                    range: Some(token_range),
                });
            }

            if let Some(mut text) = builtin_hover_text(&token.text) {
                if let Some((addr, size)) = doc.address_at_offset(offset) {
                    text.push_str(&format!(
                        "\n- address: `{}`",
                        format_address_range(addr, size)
                    ));
                }
                return Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: text,
                    }),
                    range: Some(token_range),
                });
            }

            // No symbol or builtin match — show address if available
            if let Some((addr, size)) = doc.address_at_offset(offset) {
                return Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: format!("address: `{}`", format_address_range(addr, size)),
                    }),
                    range: Some(token_range),
                });
            }
        }

        // No token found — still show address if available
        doc.address_at_offset(offset).map(|(addr, size)| Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: format!("address: `{}`", format_address_range(addr, size)),
            }),
            range: None,
        })
    }

    fn completion(&self, uri: &Uri, position: Position) -> Option<CompletionResponse> {
        let doc = self.documents.get(uri)?;
        let offset = doc.line_index.to_offset(&doc.text, position)?;
        let prefix = token_prefix_at_offset(&doc.text, offset).to_ascii_lowercase();
        let scope = doc.analysis.scope_at_offset(offset);
        let allow_symbol_completions = in_symbol_completion_context(&doc.text, offset);

        let mut candidates: Vec<(String, CompletionItemKind, &'static str, Option<String>)> =
            opcode_keywords()
                .into_iter()
                .map(|mnemonic| {
                    let doc = INSTRUCTION_DESCRIPTIONS
                        .get(&mnemonic.to_ascii_uppercase())
                        .cloned();
                    (mnemonic, CompletionItemKind::KEYWORD, "opcode", doc)
                })
                .collect();
        candidates.extend(directive_keywords().iter().map(|directive| {
            (
                (*directive).to_string(),
                CompletionItemKind::KEYWORD,
                "directive",
                None,
            )
        }));

        if allow_symbol_completions {
            let mut visible_symbols: BTreeSet<(String, SymbolCategory)> = BTreeSet::new();
            for entries in self.symbols.values() {
                for entry in entries {
                    if entry.name.starts_with('.') {
                        continue;
                    }
                    visible_symbols.insert((entry.name.clone(), entry.category));
                }
            }

            if let Some(scope) = scope {
                for symbol in &doc.analysis.symbols {
                    if symbol.name.starts_with('.') && symbol.scope.as_deref() == Some(scope) {
                        visible_symbols.insert((symbol.name.clone(), symbol.category));
                    }
                }
            }

            candidates.extend(visible_symbols.into_iter().map(|(name, category)| {
                (
                    name,
                    completion_kind_for_symbol(category),
                    category.detail(),
                    None,
                )
            }));
        }

        let mut seen = BTreeSet::new();
        let items = candidates
            .into_iter()
            .filter(|(label, _, _, _)| {
                if prefix.is_empty() {
                    return true;
                }
                label.to_ascii_lowercase().starts_with(&prefix)
            })
            .filter(|(label, _, _, _)| seen.insert(label.clone()))
            .map(|(label, kind, detail, documentation)| CompletionItem {
                label,
                kind: Some(kind),
                detail: Some(detail.to_string()),
                documentation: documentation.map(|text| {
                    lsp_types::Documentation::MarkupContent(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: text,
                    })
                }),
                ..CompletionItem::default()
            })
            .collect::<Vec<_>>();

        Some(CompletionResponse::Array(items))
    }

    fn document_symbols(&self, uri: &Uri) -> Option<DocumentSymbolResponse> {
        let doc = self.documents.get(uri)?;
        let ast = doc.analysis.ast.as_ref()?;
        let symbols = document_symbols_from_ast(ast, &doc.line_index, &doc.text);
        Some(DocumentSymbolResponse::Nested(symbols))
    }

    fn formatting(&self, uri: &Uri) -> Vec<TextEdit> {
        let Some(doc) = self.documents.get(uri) else {
            return Vec::new();
        };

        let formatted = k816_fmt::format_source(&doc.text);
        if formatted == doc.text {
            return Vec::new();
        }

        let end = doc.line_index.to_position(&doc.text, doc.text.len());
        vec![TextEdit {
            range: Range {
                start: Position {
                    line: 0,
                    character: 0,
                },
                end,
            },
            new_text: formatted,
        }]
    }

    fn resolve_addresses_for_lines(&self, uri: &Uri, lines: &[u32]) -> ResolveAddressesResult {
        let Some(doc) = self.documents.get(uri) else {
            return ResolveAddressesResult {
                addresses: lines.iter().map(|_| None).collect(),
            };
        };
        let addresses = lines
            .iter()
            .map(|&line| {
                let line = line as usize;
                if line >= doc.line_index.line_starts.len() {
                    return None;
                }
                let line_start = doc.line_index.line_starts[line];
                let line_end = if line + 1 < doc.line_index.line_starts.len() {
                    doc.line_index.line_starts[line + 1]
                } else {
                    doc.text.len()
                };
                let idx = doc
                    .resolved_sites
                    .partition_point(|(span, _, _)| span.start < line_start);
                if idx < doc.resolved_sites.len() {
                    let (span, addr, _) = &doc.resolved_sites[idx];
                    if span.start < line_end {
                        return Some(*addr);
                    }
                }
                None
            })
            .collect();
        ResolveAddressesResult { addresses }
    }

    fn resolve_inline_symbols(
        &self,
        uri: &Uri,
        params: &ResolveInlineSymbolsParams,
    ) -> ResolveInlineSymbolsResult {
        let Some(doc) = self.documents.get(uri) else {
            return ResolveInlineSymbolsResult {
                symbols: Vec::new(),
            };
        };
        if doc.line_index.line_starts.is_empty() {
            return ResolveInlineSymbolsResult {
                symbols: Vec::new(),
            };
        }

        let line_count = doc.line_index.line_starts.len();
        let mut start_line = params.start_line.min(params.end_line) as usize;
        let mut end_line = params.start_line.max(params.end_line) as usize;
        if start_line >= line_count {
            return ResolveInlineSymbolsResult {
                symbols: Vec::new(),
            };
        }
        end_line = end_line.min(line_count.saturating_sub(1));
        start_line = start_line.min(end_line);

        let start_offset = doc.line_index.line_starts[start_line];
        let end_offset = if end_line + 1 < line_count {
            doc.line_index.line_starts[end_line + 1]
        } else {
            doc.text.len()
        };

        let mut seen = BTreeSet::new();
        let mut symbols = Vec::new();

        for token in token_matches_in_range(&doc.text, start_offset, end_offset) {
            let scope = doc.analysis.scope_at_offset(token.start);
            let canonical = canonical_symbol(&token.text, scope);
            let Some(definition) = self.symbols.get(&canonical).and_then(|defs| defs.first())
            else {
                continue;
            };
            let Some((address, read_size_hint)) =
                self.inline_symbol_address(definition, &canonical)
            else {
                continue;
            };

            if !seen.insert((token.start, token.end, canonical.clone())) {
                continue;
            }

            let start = doc.line_index.to_position(&doc.text, token.start);
            let end = doc.line_index.to_position(&doc.text, token.end);

            symbols.push(ResolveInlineSymbol {
                name: token.text,
                category: symbol_category_label(definition.category).to_string(),
                start_line: start.line,
                start_character: start.character,
                end_line: end.line,
                end_character: end.character,
                address,
                read_size_hint,
            });
        }

        symbols.sort_by_key(|row| (row.start_line, row.start_character, row.name.clone()));
        ResolveInlineSymbolsResult { symbols }
    }

    fn inline_symbol_address(
        &self,
        symbol: &SymbolLocation,
        canonical: &str,
    ) -> Option<(u32, Option<u32>)> {
        let doc = self.documents.get(&symbol.uri)?;

        if let Some(meta) = doc.analysis.semantic.vars.get(canonical) {
            let read_size_hint = match meta.size {
                1 | 2 => Some(meta.size),
                _ => None,
            };
            return Some((meta.address, read_size_hint));
        }

        if let Some((addr, _)) = doc.address_at_offset(symbol.selection.start) {
            return Some((addr, None));
        }

        if let Some((_, addr, _)) = doc.resolved_sites.iter().find(|(span, _, _)| {
            span.start >= symbol.selection.start && span.start < symbol.selection.end
        }) {
            return Some((*addr, None));
        }

        match symbol.category {
            SymbolCategory::Label => {
                let idx = doc
                    .resolved_sites
                    .partition_point(|(span, _, _)| span.start < symbol.selection.start);
                doc.resolved_sites
                    .get(idx)
                    .map(|(_, addr, _)| (*addr, None))
            }
            SymbolCategory::Function => {
                let scope = doc
                    .analysis
                    .scopes
                    .iter()
                    .find(|scope| scope.name == symbol.name)?;
                doc.resolved_sites
                    .iter()
                    .find(|(span, _, _)| {
                        span.start >= scope.range.start && span.start < scope.range.end
                    })
                    .map(|(_, addr, _)| (*addr, None))
            }
            _ => None,
        }
    }
}

pub fn run_stdio_server() -> Result<()> {
    let (connection, io_threads) = Connection::stdio();
    let capabilities = server_capabilities();
    let initialize_result = connection.initialize(serde_json::to_value(&capabilities)?)?;
    let initialize_params: InitializeParams =
        serde_json::from_value(initialize_result).context("invalid initialize params")?;

    let workspace_root = determine_workspace_root(&initialize_params)?;
    let mut server = Server {
        connection,
        state: ServerState::new(workspace_root),
        pending_changes: HashMap::new(),
    };
    server.state.initialize_workspace()?;
    server.run()?;
    drop(server);
    io_threads.join()?;
    Ok(())
}

struct Server {
    connection: Connection,
    state: ServerState,
    pending_changes: HashMap<Uri, Instant>,
}

impl Server {
    fn run(&mut self) -> Result<()> {
        loop {
            self.flush_due_did_change_analyses()?;
            let message = match self.connection.receiver.recv_timeout(LOOP_POLL_INTERVAL) {
                Ok(message) => message,
                Err(RecvTimeoutError::Timeout) => continue,
                Err(RecvTimeoutError::Disconnected) => break,
            };
            match message {
                Message::Request(request) => {
                    if self.connection.handle_shutdown(&request)? {
                        break;
                    }
                    self.handle_request(request)?;
                }
                Message::Notification(notification) => {
                    self.handle_notification(notification)?;
                }
                Message::Response(_) => {}
            }
        }
        Ok(())
    }

    fn handle_request(&mut self, request: Request) -> Result<()> {
        match request.method.as_str() {
            GotoDefinition::METHOD => self.on_definition(request),
            HoverRequest::METHOD => self.on_hover(request),
            Completion::METHOD => self.on_completion(request),
            DocumentSymbolRequest::METHOD => self.on_document_symbol(request),
            Formatting::METHOD => self.on_formatting(request),
            "k816/resolveAddresses" => self.on_resolve_addresses(request),
            "k816/resolveInlineSymbols" => self.on_resolve_inline_symbols(request),
            "k816/queryMemoryMap" => self.on_query_memory_map(request),
            _ => self.send_error(
                request.id,
                -32601,
                format!("unsupported request '{}'", request.method),
            ),
        }
    }

    fn handle_notification(&mut self, notification: Notification) -> Result<()> {
        match notification.method.as_str() {
            DidOpenTextDocument::METHOD => {
                let params: DidOpenTextDocumentParams = serde_json::from_value(notification.params)
                    .context("invalid didOpen params")?;
                let uri = params.text_document.uri;
                let version = params.text_document.version;
                let text = params.text_document.text;
                self.pending_changes.remove(&uri);
                self.state
                    .upsert_document(uri.clone(), text, version, true)?;
                let diagnostics = self.state.lsp_diagnostics(&uri);
                self.publish_diagnostics(uri, Some(version), diagnostics)
            }
            DidChangeTextDocument::METHOD => {
                let params: DidChangeTextDocumentParams =
                    serde_json::from_value(notification.params)
                        .context("invalid didChange params")?;
                let uri = params.text_document.uri;
                let version = params.text_document.version;
                let Some(text) = params
                    .content_changes
                    .into_iter()
                    .last()
                    .map(|change| change.text)
                else {
                    return Ok(());
                };
                self.state
                    .upsert_document_without_analysis(uri.clone(), text, version, true);
                self.pending_changes
                    .insert(uri, Instant::now() + DID_CHANGE_DEBOUNCE);
                Ok(())
            }
            DidSaveTextDocument::METHOD => {
                let params: DidSaveTextDocumentParams = serde_json::from_value(notification.params)
                    .context("invalid didSave params")?;
                let uri = params.text_document.uri;
                self.pending_changes.remove(&uri);
                let version = self
                    .state
                    .documents
                    .get(&uri)
                    .map(|doc| doc.version)
                    .unwrap_or_default();
                if let Some(text) = params.text {
                    self.state
                        .upsert_document(uri.clone(), text, version, true)?;
                } else if let Some(path) = uri_to_file_path(&uri)
                    && let Ok(text) = fs::read_to_string(&path)
                {
                    let open = self
                        .state
                        .documents
                        .get(&uri)
                        .map(|doc| doc.open)
                        .unwrap_or(true);
                    self.state
                        .upsert_document(uri.clone(), text, version, open)?;
                }
                let diagnostics = self.state.lsp_diagnostics(&uri);
                self.publish_diagnostics(uri, Some(version), diagnostics)
            }
            DidCloseTextDocument::METHOD => {
                let params: DidCloseTextDocumentParams =
                    serde_json::from_value(notification.params)
                        .context("invalid didClose params")?;
                let uri = params.text_document.uri;
                self.pending_changes.remove(&uri);
                self.state.close_document(&uri);
                self.publish_diagnostics(uri, None, Vec::new())
            }
            _ => Ok(()),
        }
    }

    fn flush_due_did_change_analyses(&mut self) -> Result<()> {
        let now = Instant::now();
        let due_uris = self
            .pending_changes
            .iter()
            .filter_map(|(uri, due)| if *due <= now { Some(uri.clone()) } else { None })
            .collect::<Vec<_>>();

        for uri in due_uris {
            self.pending_changes.remove(&uri);
            self.analyze_and_publish(uri)?;
        }
        Ok(())
    }

    fn flush_all_pending_changes(&mut self) -> Result<()> {
        let uris = self.pending_changes.keys().cloned().collect::<Vec<_>>();
        for uri in uris {
            self.pending_changes.remove(&uri);
            self.analyze_and_publish(uri)?;
        }
        Ok(())
    }

    fn ensure_document_fresh(&mut self, uri: &Uri) -> Result<()> {
        if self.pending_changes.remove(uri).is_some() {
            self.analyze_and_publish(uri.clone())?;
        }
        Ok(())
    }

    fn analyze_and_publish(&mut self, uri: Uri) -> Result<()> {
        if !self.state.documents.contains_key(&uri) {
            return Ok(());
        }
        self.state.analyze_document(&uri);
        let version = self.state.documents.get(&uri).map(|doc| doc.version);
        let diagnostics = self.state.lsp_diagnostics(&uri);
        self.publish_diagnostics(uri, version, diagnostics)
    }

    fn on_definition(&mut self, request: Request) -> Result<()> {
        let params: GotoDefinitionParams = parse_request_params(&request)?;
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        self.ensure_document_fresh(&uri)?;
        let result = self.state.definition(&uri, position);
        self.send_result(request.id, &result)
    }

    fn on_hover(&mut self, request: Request) -> Result<()> {
        let params: HoverParams = parse_request_params(&request)?;
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        self.ensure_document_fresh(&uri)?;
        let result = self.state.hover(&uri, position);
        self.send_result(request.id, &result)
    }

    fn on_completion(&mut self, request: Request) -> Result<()> {
        let params: CompletionParams = parse_request_params(&request)?;
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        self.ensure_document_fresh(&uri)?;
        let result = self.state.completion(&uri, position);
        self.send_result(request.id, &result)
    }

    fn on_document_symbol(&mut self, request: Request) -> Result<()> {
        let params: DocumentSymbolParams = parse_request_params(&request)?;
        self.ensure_document_fresh(&params.text_document.uri)?;
        let result = self.state.document_symbols(&params.text_document.uri);
        self.send_result(request.id, &result)
    }

    fn on_formatting(&mut self, request: Request) -> Result<()> {
        let params: DocumentFormattingParams = parse_request_params(&request)?;
        self.ensure_document_fresh(&params.text_document.uri)?;
        let edits = self.state.formatting(&params.text_document.uri);
        self.send_result(request.id, &edits)
    }

    fn on_resolve_addresses(&mut self, request: Request) -> Result<()> {
        let params: ResolveAddressesParams = serde_json::from_value(request.params)?;
        let uri = Uri::from_str(&params.uri)?;
        self.ensure_document_fresh(&uri)?;
        let result = self.state.resolve_addresses_for_lines(&uri, &params.lines);
        self.send_result(request.id, &result)
    }

    fn on_resolve_inline_symbols(&mut self, request: Request) -> Result<()> {
        let params: ResolveInlineSymbolsParams = parse_request_params(&request)?;
        let uri = Uri::from_str(&params.uri)?;
        self.ensure_document_fresh(&uri)?;
        let result = self.state.resolve_inline_symbols(&uri, &params);
        self.send_result(request.id, &result)
    }

    fn on_query_memory_map(&mut self, request: Request) -> Result<()> {
        let params: QueryMemoryMapParams = parse_request_params(&request)?;
        self.flush_all_pending_changes()?;
        let result = self.state.query_memory_map(&params);
        self.send_result(request.id, &result)
    }

    fn send_result<T: serde::Serialize>(&self, id: RequestId, value: &T) -> Result<()> {
        let result = serde_json::to_value(value)?;
        self.connection
            .sender
            .send(Message::Response(Response::new_ok(id, result)))
            .context("failed to send response")
    }

    fn send_error(&self, id: RequestId, code: i32, message: String) -> Result<()> {
        self.connection
            .sender
            .send(Message::Response(Response {
                id,
                result: None,
                error: Some(ResponseError {
                    code,
                    message,
                    data: None,
                }),
            }))
            .context("failed to send error response")
    }

    fn publish_diagnostics(
        &self,
        uri: Uri,
        version: Option<i32>,
        diagnostics: Vec<Diagnostic>,
    ) -> Result<()> {
        let params = lsp_types::PublishDiagnosticsParams {
            uri,
            diagnostics,
            version,
        };
        self.send_notification::<PublishDiagnostics>(params)
    }

    fn send_notification<N: LspNotification>(&self, params: N::Params) -> Result<()> {
        self.connection
            .sender
            .send(Message::Notification(Notification::new(
                N::METHOD.to_string(),
                params,
            )))
            .context("failed to send notification")
    }
}

fn parse_request_params<T: DeserializeOwned>(request: &Request) -> Result<T> {
    serde_json::from_value(request.params.clone())
        .with_context(|| format!("invalid params for '{}'", request.method))
}

fn server_capabilities() -> ServerCapabilities {
    ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        definition_provider: Some(OneOf::Left(true)),
        hover_provider: Some(lsp_types::HoverProviderCapability::Simple(true)),
        completion_provider: Some(CompletionOptions {
            resolve_provider: Some(false),
            trigger_characters: Some(vec![".".to_string(), "@".to_string()]),
            all_commit_characters: None,
            work_done_progress_options: lsp_types::WorkDoneProgressOptions::default(),
            completion_item: None,
        }),
        document_symbol_provider: Some(OneOf::Left(true)),
        document_formatting_provider: Some(OneOf::Left(true)),
        ..ServerCapabilities::default()
    }
}

#[allow(deprecated)]
fn determine_workspace_root(params: &InitializeParams) -> Result<PathBuf> {
    workspace_root_from_initialize_params(params).ok_or_else(|| {
        anyhow::anyhow!(
            "initialize request did not include a filesystem workspace root (workspaceFolders/rootUri/rootPath)"
        )
    })
}

#[allow(deprecated)]
fn workspace_root_from_initialize_params(params: &InitializeParams) -> Option<PathBuf> {
    params
        .workspace_folders
        .as_ref()
        .and_then(|folders| folders.first())
        .and_then(|folder| uri_to_file_path(&folder.uri))
        .or_else(|| params.root_uri.as_ref().and_then(uri_to_file_path))
        .or_else(|| params.root_path.as_ref().map(PathBuf::from))
}

#[derive(Debug, Deserialize)]
struct ProjectManifest {
    package: ProjectPackage,
    #[serde(default)]
    link: ProjectLink,
}

#[derive(Debug, Default, Deserialize)]
#[serde(default)]
struct ProjectLink {
    script: Option<PathBuf>,
}

#[derive(Debug, Deserialize)]
struct ProjectPackage {
    name: String,
}

#[derive(Debug, Deserialize)]
struct ResolveAddressesParams {
    uri: String,
    lines: Vec<u32>,
}

#[derive(Debug, Serialize)]
struct ResolveAddressesResult {
    addresses: Vec<Option<u32>>,
}

#[derive(Debug, Deserialize)]
struct ResolveInlineSymbolsParams {
    uri: String,
    start_line: u32,
    end_line: u32,
}

#[derive(Debug, Serialize)]
struct ResolveInlineSymbolsResult {
    symbols: Vec<ResolveInlineSymbol>,
}

#[derive(Debug, Serialize)]
struct ResolveInlineSymbol {
    name: String,
    category: String,
    start_line: u32,
    start_character: u32,
    end_line: u32,
    end_character: u32,
    address: u32,
    #[serde(skip_serializing_if = "Option::is_none")]
    read_size_hint: Option<u32>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize, Default)]
#[serde(rename_all = "snake_case")]
enum QueryMemoryMapDetail {
    #[default]
    Summary,
    Runs,
}

#[derive(Debug, Deserialize, Default)]
struct QueryMemoryMapParams {
    #[serde(default)]
    memory_name: Option<String>,
    #[serde(default)]
    detail: QueryMemoryMapDetail,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
enum QueryMemoryMapStatus {
    Ok,
    Unavailable,
}

#[derive(Debug, Serialize)]
struct QueryMemoryMapResult {
    status: QueryMemoryMapStatus,
    #[serde(skip_serializing_if = "Option::is_none")]
    reason: Option<String>,
    memories: Vec<QueryMemoryMapMemory>,
    runs: Vec<QueryMemoryMapRun>,
}

#[derive(Debug, Serialize)]
struct QueryMemoryMapMemory {
    name: String,
    start: u32,
    size: u32,
    kind: String,
    used: u32,
    free: u32,
    utilization_percent: f64,
}

#[derive(Debug, Serialize)]
struct QueryMemoryMapRun {
    memory_name: String,
    start: u32,
    end: u32,
    size: u32,
}

fn memory_kind_label(kind: k816_link::MemoryKind) -> &'static str {
    match kind {
        k816_link::MemoryKind::ReadOnly => "read_only",
        k816_link::MemoryKind::ReadWrite => "read_write",
    }
}

fn symbol_category_label(category: SymbolCategory) -> &'static str {
    match category {
        SymbolCategory::Function => "function",
        SymbolCategory::Constant => "constant",
        SymbolCategory::Variable => "variable",
        SymbolCategory::Label => "label",
        SymbolCategory::DataBlock => "data_block",
        SymbolCategory::Segment => "segment",
    }
}

fn discover_workspace_sources(root: &Path) -> Result<Vec<PathBuf>> {
    let manifest_path = root.join(PROJECT_MANIFEST);
    if manifest_path.is_file() {
        let text = fs::read_to_string(&manifest_path)
            .with_context(|| format!("failed to read '{}'", manifest_path.display()))?;
        let manifest: ProjectManifest = toml::from_str(&text)
            .with_context(|| format!("failed to parse '{}'", manifest_path.display()))?;
        if manifest.package.name.trim().is_empty() {
            anyhow::bail!(
                "package.name in '{}' must not be empty",
                manifest_path.display()
            );
        }
    }

    let mut files = Vec::new();
    for folder in [PROJECT_SRC_DIR, PROJECT_TESTS_DIR] {
        let dir = root.join(folder);
        if !dir.is_dir() {
            continue;
        }
        for entry in WalkDir::new(&dir).follow_links(false) {
            let entry = match entry {
                Ok(entry) => entry,
                Err(_) => continue,
            };
            if !entry.file_type().is_file() {
                continue;
            }
            if is_k65_source_path(entry.path()) {
                files.push(entry.path().to_path_buf());
            }
        }
    }

    files.sort();
    files.dedup();
    Ok(files)
}

fn is_k65_source_path(path: &Path) -> bool {
    path.extension()
        .and_then(|ext| ext.to_str())
        .is_some_and(|ext| ext.eq_ignore_ascii_case("k65"))
}

fn uri_to_file_path(uri: &Uri) -> Option<PathBuf> {
    let parsed = url::Url::parse(uri.as_str()).ok()?;
    parsed.to_file_path().ok()
}

fn uri_from_file_path(path: &Path) -> Result<Uri> {
    let url = url::Url::from_file_path(path)
        .map_err(|()| anyhow::anyhow!("path '{}' cannot be represented as URI", path.display()))?;
    Uri::from_str(url.as_str()).map_err(|error| anyhow::anyhow!("invalid URI '{}': {error}", url))
}

fn analyze_document(
    source_name: &str,
    source_text: &str,
) -> (
    DocumentAnalysis,
    Option<k816_o65::O65Object>,
    Vec<k816_core::AddressableSite>,
) {
    let (mut diagnostics, compile_failed, object, addressable_sites) =
        match k816_core::compile_source_to_object_with_options(
            source_name,
            source_text,
            k816_core::CompileRenderOptions::plain(),
        ) {
            Ok(output) => (
                output.warnings,
                false,
                Some(output.object),
                output.addressable_sites,
            ),
            Err(error) => (error.diagnostics, true, None, Vec::new()),
        };

    let mut source_map = k816_core::span::SourceMap::default();
    let source_id = source_map.add_source(source_name, source_text);

    let mut ast = None;
    let mut semantic = SemanticInfo::default();

    let (parsed_file, parse_diagnostics) = k816_core::parser::parse_lenient(source_id, source_text);
    if compile_failed {
        diagnostics.extend(parse_diagnostics);
    }

    let (symbols, scopes) = if let Some(parsed_file) = parsed_file {
        let symbol_collection = collect_symbols(&parsed_file);
        ast = Some(parsed_file.clone());

        if let Ok(expanded) = k816_core::eval_expand::expand_file(&parsed_file, source_id)
            && let Ok(normalized) = k816_core::normalize_hla::normalize_file(&expanded)
            && let Ok(model) = k816_core::sema::analyze(&normalized)
        {
            for (name, meta) in model.functions {
                semantic.functions.insert(name, meta);
            }
            for (name, meta) in model.consts {
                semantic.consts.insert(name, meta);
            }
            for (name, meta) in model.vars {
                semantic.vars.insert(name, meta);
            }
        }
        (symbol_collection.symbols, symbol_collection.scopes)
    } else {
        // Parser completely failed — fall back to token-level symbol extraction
        let fallback = scan_tokens_for_symbols(source_id, source_text);
        (fallback.symbols, fallback.scopes)
    };

    dedup_diagnostics(&mut diagnostics);

    (
        DocumentAnalysis {
            diagnostics,
            symbols,
            scopes,
            semantic,
            ast,
        },
        object,
        addressable_sites,
    )
}

#[derive(Debug, Default)]
struct SymbolCollection {
    symbols: Vec<SymbolDef>,
    scopes: Vec<ScopeRange>,
}

fn collect_symbols(file: &k816_core::ast::File) -> SymbolCollection {
    let mut out = SymbolCollection::default();
    for item in &file.items {
        match &item.node {
            k816_core::ast::Item::CodeBlock(block) => {
                let range = ByteRange::from_span(item.span);
                let selection = block
                    .name_span
                    .map(ByteRange::from_span)
                    .unwrap_or_else(|| range.clone());
                out.symbols.push(SymbolDef {
                    canonical: block.name.clone(),
                    name: block.name.clone(),
                    category: SymbolCategory::Function,
                    selection,
                    scope: None,
                });
                out.scopes.push(ScopeRange {
                    name: block.name.clone(),
                    range,
                });
                for stmt in &block.body {
                    collect_stmt_symbols(
                        &stmt.node,
                        stmt.span,
                        Some(&block.name),
                        &mut out.symbols,
                    );
                }
            }
            k816_core::ast::Item::Var(var) => {
                let range = ByteRange::from_span(item.span);
                out.symbols.push(SymbolDef {
                    canonical: var.name.clone(),
                    name: var.name.clone(),
                    category: SymbolCategory::Variable,
                    selection: range,
                    scope: None,
                });
            }
            k816_core::ast::Item::Const(const_decl) => {
                let range = ByteRange::from_span(item.span);
                out.symbols.push(SymbolDef {
                    canonical: const_decl.name.clone(),
                    name: const_decl.name.clone(),
                    category: SymbolCategory::Constant,
                    selection: range,
                    scope: None,
                });
            }
            k816_core::ast::Item::ConstGroup(consts) => {
                let range = ByteRange::from_span(item.span);
                for const_decl in consts {
                    out.symbols.push(SymbolDef {
                        canonical: const_decl.name.clone(),
                        name: const_decl.name.clone(),
                        category: SymbolCategory::Constant,
                        selection: range.clone(),
                        scope: None,
                    });
                }
            }
            k816_core::ast::Item::NamedDataBlock(block) => {
                out.symbols.push(SymbolDef {
                    canonical: block.name.clone(),
                    name: block.name.clone(),
                    category: SymbolCategory::DataBlock,
                    selection: ByteRange::from_span(block.name_span),
                    scope: None,
                });
            }
            k816_core::ast::Item::Segment(segment) => {
                let range = ByteRange::from_span(item.span);
                out.symbols.push(SymbolDef {
                    canonical: segment.name.clone(),
                    name: segment.name.clone(),
                    category: SymbolCategory::Segment,
                    selection: range,
                    scope: None,
                });
            }
            k816_core::ast::Item::Statement(stmt) => {
                collect_stmt_symbols(stmt, item.span, None, &mut out.symbols);
            }
            k816_core::ast::Item::EvaluatorBlock(_) => {}
            k816_core::ast::Item::DataBlock(_) => {}
        }
    }
    out
}

fn collect_stmt_symbols(
    stmt: &k816_core::ast::Stmt,
    span: k816_core::span::Span,
    scope: Option<&str>,
    out: &mut Vec<SymbolDef>,
) {
    match stmt {
        k816_core::ast::Stmt::Label(label) => {
            let range = ByteRange::from_span(span);
            let canonical = canonical_symbol(&label.name, scope);
            out.push(SymbolDef {
                canonical,
                name: label.name.clone(),
                category: SymbolCategory::Label,
                selection: range,
                scope: scope.map(str::to_string),
            });
        }
        k816_core::ast::Stmt::Var(var) => {
            let range = ByteRange::from_span(span);
            out.push(SymbolDef {
                canonical: var.name.clone(),
                name: var.name.clone(),
                category: SymbolCategory::Variable,
                selection: range,
                scope: scope.map(str::to_string),
            });
        }
        k816_core::ast::Stmt::ModeScopedBlock { body, .. } => {
            for stmt in body {
                collect_stmt_symbols(&stmt.node, stmt.span, scope, out);
            }
        }
        k816_core::ast::Stmt::Hla(k816_core::ast::HlaStmt::PrefixConditional { body, .. }) => {
            for stmt in body {
                collect_stmt_symbols(&stmt.node, stmt.span, scope, out);
            }
        }
        _ => {}
    }
}

/// Fallback symbol extraction from the token stream when the parser
/// completely fails to produce an AST. Scans for patterns like
/// `func IDENT`, `naked IDENT`, `var IDENT`, etc.
fn scan_tokens_for_symbols(
    source_id: k816_core::span::SourceId,
    source_text: &str,
) -> SymbolCollection {
    use k816_core::lexer::{TokenKind, lex_lenient};

    let (tokens, _) = lex_lenient(source_id, source_text);
    let mut out = SymbolCollection::default();

    let mut i = 0;
    while i < tokens.len() {
        let token = &tokens[i];
        match &token.kind {
            TokenKind::Func => {
                if let Some(name_token) = tokens.get(i + 1)
                    && let TokenKind::Ident(name) = &name_token.kind
                {
                    let selection = ByteRange::from_span(name_token.span);
                    let scope_range = find_brace_scope(&tokens, i + 2);
                    out.symbols.push(SymbolDef {
                        canonical: name.clone(),
                        name: name.clone(),
                        category: SymbolCategory::Function,
                        selection,
                        scope: None,
                    });
                    if let Some(scope_range) = scope_range {
                        out.scopes.push(ScopeRange {
                            name: name.clone(),
                            range: scope_range,
                        });
                    }
                    i += 2;
                    continue;
                }
            }
            TokenKind::Far | TokenKind::Naked | TokenKind::Inline => {
                // Skip modifiers to find the name: [far] [naked] [inline] [func] IDENT
                let mut j = i + 1;
                while j < tokens.len() {
                    match &tokens[j].kind {
                        TokenKind::Far | TokenKind::Naked | TokenKind::Inline | TokenKind::Func => {
                            j += 1;
                        }
                        _ => break,
                    }
                }
                if let Some(name_token) = tokens.get(j)
                    && let TokenKind::Ident(name) = &name_token.kind
                {
                    let selection = ByteRange::from_span(name_token.span);
                    let scope_range = find_brace_scope(&tokens, j + 1);
                    out.symbols.push(SymbolDef {
                        canonical: name.clone(),
                        name: name.clone(),
                        category: SymbolCategory::Function,
                        selection,
                        scope: None,
                    });
                    if let Some(scope_range) = scope_range {
                        out.scopes.push(ScopeRange {
                            name: name.clone(),
                            range: scope_range,
                        });
                    }
                    i = j + 1;
                    continue;
                }
            }
            TokenKind::Var => {
                if let Some(name_token) = tokens.get(i + 1)
                    && let TokenKind::Ident(name) = &name_token.kind
                {
                    out.symbols.push(SymbolDef {
                        canonical: name.clone(),
                        name: name.clone(),
                        category: SymbolCategory::Variable,
                        selection: ByteRange::from_span(name_token.span),
                        scope: None,
                    });
                    i += 2;
                    continue;
                }
            }
            TokenKind::Const => {
                if let Some(name_token) = tokens.get(i + 1)
                    && let TokenKind::Ident(name) = &name_token.kind
                {
                    out.symbols.push(SymbolDef {
                        canonical: name.clone(),
                        name: name.clone(),
                        category: SymbolCategory::Constant,
                        selection: ByteRange::from_span(name_token.span),
                        scope: None,
                    });
                    i += 2;
                    continue;
                }
            }
            TokenKind::Data => {
                if let Some(name_token) = tokens.get(i + 1)
                    && let TokenKind::Ident(name) = &name_token.kind
                {
                    out.symbols.push(SymbolDef {
                        canonical: name.clone(),
                        name: name.clone(),
                        category: SymbolCategory::DataBlock,
                        selection: ByteRange::from_span(name_token.span),
                        scope: None,
                    });
                    i += 2;
                    continue;
                }
            }
            TokenKind::Segment => {
                if let Some(name_token) = tokens.get(i + 1)
                    && let TokenKind::Ident(name) = &name_token.kind
                {
                    out.symbols.push(SymbolDef {
                        canonical: name.clone(),
                        name: name.clone(),
                        category: SymbolCategory::Segment,
                        selection: ByteRange::from_span(name_token.span),
                        scope: None,
                    });
                    i += 2;
                    continue;
                }
            }
            _ => {}
        }
        i += 1;
    }
    out
}

/// Scan forward from token index to find the byte range of a `{ ... }` block.
fn find_brace_scope(tokens: &[k816_core::lexer::Token], start: usize) -> Option<ByteRange> {
    use k816_core::lexer::TokenKind;

    // Skip mode annotations and newlines to find LBrace
    let mut j = start;
    while j < tokens.len() {
        match &tokens[j].kind {
            TokenKind::ModeA8
            | TokenKind::ModeA16
            | TokenKind::ModeI8
            | TokenKind::ModeI16
            | TokenKind::Newline => {
                j += 1;
            }
            _ => break,
        }
    }
    if j >= tokens.len() || tokens[j].kind != TokenKind::LBrace {
        return None;
    }
    let block_start = tokens[j].span.start;
    let mut depth = 1usize;
    j += 1;
    while j < tokens.len() && depth > 0 {
        match &tokens[j].kind {
            TokenKind::LBrace => depth += 1,
            TokenKind::RBrace => depth -= 1,
            _ => {}
        }
        j += 1;
    }
    let block_end = if j > 0 {
        tokens[j - 1].span.end
    } else {
        block_start
    };
    Some(ByteRange {
        start: block_start,
        end: block_end,
    })
}

fn canonical_symbol(name: &str, scope: Option<&str>) -> String {
    if name.starts_with('.')
        && let Some(scope) = scope
    {
        return format!("{scope}::{name}");
    }
    name.to_string()
}

fn extract_unknown_identifier_name(message: &str) -> Option<String> {
    let prefixes = ["unknown identifier '"];
    for prefix in prefixes {
        if let Some(rest) = message.strip_prefix(prefix)
            && let Some(end) = rest.find('\'')
        {
            return Some(rest[..end].to_string());
        }
    }
    None
}

fn completion_kind_for_symbol(category: SymbolCategory) -> CompletionItemKind {
    match category {
        SymbolCategory::Function => CompletionItemKind::FUNCTION,
        SymbolCategory::Constant => CompletionItemKind::CONSTANT,
        SymbolCategory::Variable => CompletionItemKind::VARIABLE,
        SymbolCategory::Label => CompletionItemKind::CONSTANT,
        SymbolCategory::DataBlock => CompletionItemKind::VALUE,
        SymbolCategory::Segment => CompletionItemKind::MODULE,
    }
}

fn byte_range_to_lsp(range: &ByteRange, line_index: &LineIndex, text: &str) -> Range {
    Range {
        start: line_index.to_position(text, range.start),
        end: line_index.to_position(text, range.end),
    }
}

fn diagnostic_to_lsp(
    diagnostic: &k816_core::diag::Diagnostic,
    uri: &Uri,
    line_index: &LineIndex,
    text: &str,
) -> Diagnostic {
    let primary = byte_range_to_lsp(&ByteRange::from_span(diagnostic.primary), line_index, text);
    let related_information = if diagnostic.labels.is_empty() {
        None
    } else {
        Some(
            diagnostic
                .labels
                .iter()
                .map(|label| DiagnosticRelatedInformation {
                    location: Location::new(
                        uri.clone(),
                        byte_range_to_lsp(&ByteRange::from_span(label.span), line_index, text),
                    ),
                    message: label.message.clone(),
                })
                .collect::<Vec<_>>(),
        )
    };

    let mut message = diagnostic.message.clone();
    for supplement in &diagnostic.supplements {
        match supplement {
            k816_core::diag::Supplemental::Help(help) => {
                message.push_str("\nhelp: ");
                message.push_str(help);
            }
            k816_core::diag::Supplemental::Note(note) => {
                message.push_str("\nnote: ");
                message.push_str(note);
            }
        }
    }

    Diagnostic {
        range: primary,
        severity: Some(match diagnostic.severity {
            k816_core::diag::Severity::Error => DiagnosticSeverity::ERROR,
            k816_core::diag::Severity::Warning => DiagnosticSeverity::WARNING,
        }),
        code: None,
        code_description: None,
        source: Some("k816".to_string()),
        message,
        related_information,
        tags: None,
        data: None,
    }
}

fn dedup_diagnostics(diagnostics: &mut Vec<k816_core::diag::Diagnostic>) {
    let mut seen = BTreeSet::new();
    diagnostics.retain(|diagnostic| {
        seen.insert((
            match diagnostic.severity {
                k816_core::diag::Severity::Error => 0_u8,
                k816_core::diag::Severity::Warning => 1_u8,
            },
            diagnostic.message.clone(),
            diagnostic.primary.start,
            diagnostic.primary.end,
        ))
    });
}

#[derive(Debug)]
struct TokenMatch {
    text: String,
    start: usize,
    end: usize,
}

fn token_at_offset(text: &str, offset: usize) -> Option<TokenMatch> {
    let bytes = text.as_bytes();
    if bytes.is_empty() {
        return None;
    }

    let mut index = offset.min(bytes.len().saturating_sub(1));
    if !is_ident_byte(bytes[index]) {
        if index > 0 && is_ident_byte(bytes[index - 1]) {
            index -= 1;
        } else {
            return None;
        }
    }

    let mut start = index;
    while start > 0 && is_ident_byte(bytes[start - 1]) {
        start -= 1;
    }

    let mut end = index + 1;
    while end < bytes.len() && is_ident_byte(bytes[end]) {
        end += 1;
    }

    Some(TokenMatch {
        text: text[start..end].to_string(),
        start,
        end,
    })
}

fn token_prefix_at_offset(text: &str, offset: usize) -> String {
    let bytes = text.as_bytes();
    let mut start = offset.min(bytes.len());
    while start > 0 && is_ident_byte(bytes[start - 1]) {
        start -= 1;
    }
    text[start..offset.min(text.len())].to_string()
}

fn token_matches_in_range(text: &str, start: usize, end: usize) -> Vec<TokenMatch> {
    let bytes = text.as_bytes();
    if bytes.is_empty() {
        return Vec::new();
    }
    let mut out = Vec::new();
    let mut index = start.min(bytes.len());
    let end = end.min(bytes.len());

    while index < end {
        if !is_ident_byte(bytes[index]) {
            index += 1;
            continue;
        }

        let token_start = index;
        while index < end && is_ident_byte(bytes[index]) {
            index += 1;
        }
        let token_end = index;
        if token_end <= token_start {
            continue;
        }

        let token_text = &text[token_start..token_end];
        let first = token_text.as_bytes()[0];
        if first.is_ascii_digit() || first == b'@' {
            continue;
        }

        out.push(TokenMatch {
            text: token_text.to_string(),
            start: token_start,
            end: token_end,
        });
    }

    out
}

fn is_ident_byte(byte: u8) -> bool {
    byte.is_ascii_alphanumeric() || byte == b'_' || byte == b'.' || byte == b'@'
}

fn in_symbol_completion_context(text: &str, offset: usize) -> bool {
    if !token_prefix_at_offset(text, offset).is_empty() {
        return true;
    }

    let offset = offset.min(text.len());
    let line_start = text[..offset].rfind('\n').map_or(0, |line| line + 1);
    let line_prefix = &text[line_start..offset];
    let trimmed = line_prefix.trim_start();
    if trimmed.is_empty() {
        return false;
    }

    if line_prefix.chars().last().is_some_and(char::is_whitespace) {
        return true;
    }

    trimmed.split_whitespace().count() > 1
}

fn hover_contents_for_symbol(
    canonical: &str,
    symbol: &SymbolLocation,
    state: &ServerState,
) -> String {
    if let Some(doc) = state.documents.get(&symbol.uri) {
        if let Some(meta) = doc.analysis.semantic.functions.get(canonical) {
            let mut lines = vec![
                format!("**function** `{}`", symbol.name),
                format!("- far: {}", yes_no(meta.is_far)),
                format!("- naked: {}", yes_no(meta.is_naked)),
                format!("- inline: {}", yes_no(meta.is_inline)),
            ];
            if let Some(width) = meta.mode_contract.a_width {
                lines.push(format!("- A width: {}", reg_width_name(width)));
            }
            if let Some(width) = meta.mode_contract.i_width {
                lines.push(format!("- I width: {}", reg_width_name(width)));
            }
            if let Some((addr, size)) = doc.address_at_offset(symbol.selection.start) {
                lines.push(format!("- address: `{}`", format_address_range(addr, size)));
            }
            return lines.join("\n");
        }
        if let Some(meta) = doc.analysis.semantic.consts.get(canonical) {
            return hover_contents_for_constant(symbol.name.as_str(), *meta);
        }
        if let Some(meta) = doc.analysis.semantic.vars.get(canonical) {
            return format!(
                "**variable** `{}`\n- address: `{}`\n- size: `{}`",
                symbol.name,
                format_address(meta.address),
                meta.size
            );
        }
    }

    let mut text = format!("**{}** `{}`", symbol.category.detail(), symbol.name);
    if let Some(doc) = state.documents.get(&symbol.uri)
        && let Some((addr, size)) = doc.address_at_offset(symbol.selection.start)
    {
        text.push_str(&format!(
            "\n- address: `{}`",
            format_address_range(addr, size)
        ));
    }
    text
}

fn hover_contents_for_constant(name: &str, value: k816_core::sema::ConstMeta) -> String {
    if let Some(value) = value.value.to_i64_exact() {
        return format!(
            "**constant** `{name}`\n{}",
            format_numeric_value_lines(value, false).join("\n")
        );
    }
    format!("**constant** `{name}`\n- value: `{}`", value.value)
}

fn hover_contents_for_numeric_literal(value: i64) -> String {
    format_numeric_value_lines(value, true).join("\n")
}

fn format_numeric_value_lines(value: i64, include_binary_for_u8: bool) -> Vec<String> {
    let mut lines = vec![
        format!("- decimal: `{value}`"),
        format!("- hex: `{}`", format_signed_hex(value)),
    ];
    if include_binary_for_u8 && (0..=255).contains(&value) {
        lines.push(format!("- binary: `%{:08b}`", value as u8));
    }
    lines
}

fn format_signed_hex(value: i64) -> String {
    if value < 0 {
        let magnitude = (-i128::from(value)) as u128;
        return format!("-${magnitude:X}");
    }
    format!("${:X}", value as u64)
}

fn numeric_literal_at_offset(text: &str, offset: usize) -> Option<(ByteRange, i64)> {
    let source_id = k816_core::span::SourceId(0);
    let (tokens, _) = k816_core::lexer::lex_lenient(source_id, text);
    let offset = offset.min(text.len());
    tokens.into_iter().find_map(|token| {
        if offset < token.span.start || offset >= token.span.end {
            return None;
        }
        match token.kind {
            k816_core::lexer::TokenKind::Number(num) => Some((
                ByteRange {
                    start: token.span.start,
                    end: token.span.end,
                },
                num.value,
            )),
            _ => None,
        }
    })
}

fn format_address(addr: u32) -> String {
    if addr > 0xFFFF {
        format!("${:06X}", addr)
    } else {
        format!("${:04X}", addr)
    }
}

fn format_address_range(addr: u32, size: u32) -> String {
    if size <= 1 {
        format_address(addr)
    } else {
        let end = addr + size - 1;
        format!("{}..{}", format_address(addr), format_address(end))
    }
}

fn yes_no(value: bool) -> &'static str {
    if value { "yes" } else { "no" }
}

fn reg_width_name(width: k816_core::ast::RegWidth) -> &'static str {
    match width {
        k816_core::ast::RegWidth::W8 => "8-bit",
        k816_core::ast::RegWidth::W16 => "16-bit",
    }
}

fn builtin_hover_text(token: &str) -> Option<String> {
    let token = token.to_ascii_lowercase();
    if opcode_keywords().iter().any(|opcode| opcode == &token) {
        let description = INSTRUCTION_DESCRIPTIONS
            .get(&token.to_ascii_uppercase())
            .map(|d| d.as_str())
            .unwrap_or("WDC 65816 instruction mnemonic.");
        return Some(format!("**opcode** `{token}`\n\n{description}"));
    }

    let text = match token.as_str() {
        "segment" => "Select an output segment for following code/data.",
        "const" => "Declare a compile-time numeric constant.",
        "var" => "Declare a variable symbol (optionally typed and/or initialized).",
        "func" => "Declare a function block.",
        "far" => "Marks a function for far call/return semantics.",
        "naked" => "Disables automatic function epilogue emission.",
        "inline" => "Marks function as inline-capable in HLA lowering.",
        "data" => "Open a data emission block.",
        "align" => "Align output location to a power-of-two boundary.",
        "address" => "Set absolute output address for following bytes.",
        "nocross" => "Prevent emitted bytes from crossing boundary size.",
        "call" => "Call a known function symbol.",
        "goto" => "Jump to an absolute address or indirect through a vector.",
        "return" => "Return from subroutine (RTS).",
        "return_i" => "Return from interrupt (RTI).",
        "else" => "Alternate branch for conditional blocks.",
        "break" => "Exit the current loop block.",
        "repeat" => "Restart the current loop block or repeat data in a data block.",
        "always" => "Unconditional loop (JMP back to start).",
        "never" => "One-shot block with no branch back.",
        "charset" => "Define character mapping for subsequent string data.",
        "image" => "Load pixel data from bitmap images.",
        "binary" => "Embed raw binary file contents.",
        "code" => "Embed executable code within a data block.",
        "@a8" => "Set accumulator width contract to 8-bit.",
        "@a16" => "Set accumulator width contract to 16-bit.",
        "@i8" => "Set index width contract to 8-bit.",
        "@i16" => "Set index width contract to 16-bit.",
        _ => return None,
    };

    Some(format!("**directive** `{token}`\n{text}"))
}

fn opcode_keywords() -> Vec<String> {
    let mut mnemonics = BTreeSet::new();
    for opcode in 0_u16..=255 {
        let descriptor = k816_isa65816::opcode_descriptor(opcode as u8);
        mnemonics.insert(descriptor.mnemonic.to_string());
    }
    mnemonics.into_iter().collect()
}

fn directive_keywords() -> &'static [&'static str] {
    &[
        "segment", "const", "var", "func", "far", "naked", "inline", "data", "align", "address",
        "nocross", "call", "goto", "return", "return_i", "else", "break", "repeat", "always",
        "never", "charset", "image", "binary", "code", "@a8", "@a16", "@i8", "@i16",
    ]
}

fn document_symbols_from_ast(
    file: &k816_core::ast::File,
    line_index: &LineIndex,
    text: &str,
) -> Vec<DocumentSymbol> {
    let mut symbols = Vec::new();
    for item in &file.items {
        match &item.node {
            k816_core::ast::Item::CodeBlock(block) => {
                let children = stmt_document_symbols(&block.body, line_index, text);
                let selection = block.name_span.unwrap_or(item.span);
                symbols.push(make_document_symbol(
                    block.name.clone(),
                    SymbolCategory::Function,
                    item.span,
                    selection,
                    if children.is_empty() {
                        None
                    } else {
                        Some(children)
                    },
                    line_index,
                    text,
                ));
            }
            k816_core::ast::Item::Var(var) => symbols.push(make_document_symbol(
                var.name.clone(),
                SymbolCategory::Variable,
                item.span,
                item.span,
                None,
                line_index,
                text,
            )),
            k816_core::ast::Item::Const(const_decl) => symbols.push(make_document_symbol(
                const_decl.name.clone(),
                SymbolCategory::Constant,
                item.span,
                item.span,
                None,
                line_index,
                text,
            )),
            k816_core::ast::Item::ConstGroup(consts) => {
                for const_decl in consts {
                    symbols.push(make_document_symbol(
                        const_decl.name.clone(),
                        SymbolCategory::Constant,
                        item.span,
                        item.span,
                        None,
                        line_index,
                        text,
                    ));
                }
            }
            k816_core::ast::Item::NamedDataBlock(block) => symbols.push(make_document_symbol(
                block.name.clone(),
                SymbolCategory::DataBlock,
                item.span,
                block.name_span,
                None,
                line_index,
                text,
            )),
            k816_core::ast::Item::Segment(segment) => symbols.push(make_document_symbol(
                segment.name.clone(),
                SymbolCategory::Segment,
                item.span,
                item.span,
                None,
                line_index,
                text,
            )),
            k816_core::ast::Item::Statement(stmt) => {
                if let Some(symbol) = stmt_to_document_symbol(stmt, item.span, line_index, text) {
                    symbols.push(symbol);
                }
            }
            k816_core::ast::Item::EvaluatorBlock(_) => {}
            k816_core::ast::Item::DataBlock(_) => {}
        }
    }
    symbols
}

fn stmt_document_symbols(
    statements: &[k816_core::span::Spanned<k816_core::ast::Stmt>],
    line_index: &LineIndex,
    text: &str,
) -> Vec<DocumentSymbol> {
    statements
        .iter()
        .filter_map(|stmt| stmt_to_document_symbol(&stmt.node, stmt.span, line_index, text))
        .collect()
}

fn stmt_to_document_symbol(
    stmt: &k816_core::ast::Stmt,
    span: k816_core::span::Span,
    line_index: &LineIndex,
    text: &str,
) -> Option<DocumentSymbol> {
    match stmt {
        k816_core::ast::Stmt::Label(label) => Some(make_document_symbol(
            label.name.clone(),
            SymbolCategory::Label,
            span,
            span,
            None,
            line_index,
            text,
        )),
        k816_core::ast::Stmt::Var(var) => Some(make_document_symbol(
            var.name.clone(),
            SymbolCategory::Variable,
            span,
            span,
            None,
            line_index,
            text,
        )),
        k816_core::ast::Stmt::ModeScopedBlock { body, .. } => {
            let children = stmt_document_symbols(body, line_index, text);
            Some(make_document_symbol(
                "@mode".to_string(),
                SymbolCategory::Segment,
                span,
                span,
                if children.is_empty() {
                    None
                } else {
                    Some(children)
                },
                line_index,
                text,
            ))
        }
        k816_core::ast::Stmt::Hla(k816_core::ast::HlaStmt::PrefixConditional { body, .. }) => {
            let children = stmt_document_symbols(body, line_index, text);
            Some(make_document_symbol(
                "prefix-conditional".to_string(),
                SymbolCategory::Segment,
                span,
                span,
                if children.is_empty() {
                    None
                } else {
                    Some(children)
                },
                line_index,
                text,
            ))
        }
        _ => None,
    }
}

#[allow(deprecated)]
fn make_document_symbol(
    name: String,
    category: SymbolCategory,
    range: k816_core::span::Span,
    selection: k816_core::span::Span,
    children: Option<Vec<DocumentSymbol>>,
    line_index: &LineIndex,
    text: &str,
) -> DocumentSymbol {
    DocumentSymbol {
        name,
        detail: Some(category.detail().to_string()),
        kind: category.symbol_kind(),
        tags: None,
        deprecated: None,
        range: byte_range_to_lsp(&ByteRange::from_span(range), line_index, text),
        selection_range: byte_range_to_lsp(&ByteRange::from_span(selection), line_index, text),
        children,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn initialize_params(value: serde_json::Value) -> InitializeParams {
        serde_json::from_value(value).expect("valid initialize params")
    }

    fn file_uri(path: &Path) -> String {
        url::Url::from_file_path(path)
            .expect("file uri")
            .to_string()
    }

    #[test]
    fn line_index_round_trip_handles_utf16_columns() {
        let text = "ab\nz🙂x\n";
        let index = LineIndex::new(text);

        let offset = text.find('x').expect("x offset");
        let position = index.to_position(text, offset);
        assert_eq!(position.line, 1);
        assert_eq!(position.character, 3);

        let back = index
            .to_offset(text, position)
            .expect("position should map to offset");
        assert_eq!(back, offset);
    }

    #[test]
    fn converts_diagnostics_with_related_info() {
        let uri = Uri::from_str("file:///tmp/test.k65").expect("uri");
        let text = "func main {\n  nop\n}\n";
        let index = LineIndex::new(text);
        let source_id = k816_core::span::SourceId(0);
        let primary = k816_core::span::Span::new(source_id, 2, 6);
        let related = k816_core::span::Span::new(source_id, 9, 12);
        let diagnostic = k816_core::diag::Diagnostic::error(primary, "oops")
            .with_label(related, "related")
            .with_help("fix it");

        let converted = diagnostic_to_lsp(&diagnostic, &uri, &index, text);
        assert_eq!(converted.severity, Some(DiagnosticSeverity::ERROR));
        assert!(converted.message.contains("help: fix it"));
        assert_eq!(
            converted
                .related_information
                .as_ref()
                .expect("related info")
                .len(),
            1
        );
    }

    #[test]
    fn resolves_local_label_definition_in_scope() {
        let uri = Uri::from_str("file:///tmp/test.k65").expect("uri");
        let text = "func main {\n.loop:\n  bra .loop\n}\n".to_string();
        let mut state = ServerState::new(PathBuf::from("/tmp"));
        state
            .upsert_document(uri.clone(), text, 1, true)
            .expect("document inserted");

        let doc = state.documents.get(&uri).expect("doc");
        let offset = doc.text.find(".loop").expect("loop");
        let position = doc.line_index.to_position(&doc.text, offset + 16);
        let response = state
            .definition(&uri, position)
            .expect("definition should resolve");

        match response {
            GotoDefinitionResponse::Array(locations) => assert_eq!(locations.len(), 1),
            _ => panic!("unexpected response shape"),
        }
    }

    #[test]
    fn resolves_cross_file_function_definition() {
        let uri_main = Uri::from_str("file:///project/src/main.k65").expect("uri");
        let text_main = "func main {\n  call app_init\n}\n".to_string();

        let uri_func = Uri::from_str("file:///project/src/func.k65").expect("uri");
        let text_func = "func app_init @a8 @i8 {\n  lda #1\n  ldx #2\n}\n".to_string();

        let mut state = ServerState::new(PathBuf::from("/project"));
        state
            .upsert_document(uri_func.clone(), text_func, 1, false)
            .expect("func doc");
        state
            .upsert_document(uri_main.clone(), text_main.clone(), 1, true)
            .expect("main doc");

        let doc = state.documents.get(&uri_main).expect("main doc");
        let offset = text_main.find("app_init").expect("app_init");
        let position = doc.line_index.to_position(&doc.text, offset);
        let response = state
            .definition(&uri_main, position)
            .expect("definition should resolve to func.k65");

        match response {
            GotoDefinitionResponse::Array(locations) => {
                assert_eq!(locations.len(), 1, "expected exactly one definition");
                assert_eq!(
                    locations[0].uri, uri_func,
                    "definition should be in func.k65"
                );
            }
            _ => panic!("unexpected response shape"),
        }
    }

    #[test]
    fn resolves_cross_file_variable_definition() {
        let uri_vars = Uri::from_str("file:///project/src/vars.k65").expect("uri");
        let text_vars = "var foo = $1234\n".to_string();

        let uri_test = Uri::from_str("file:///project/src/test.k65").expect("uri");
        let text_test = "func test @a8 {\n  lda foo\n}\n".to_string();

        let mut state = ServerState::new(PathBuf::from("/project"));
        state
            .upsert_document(uri_vars.clone(), text_vars, 1, false)
            .expect("vars doc");
        state
            .upsert_document(uri_test.clone(), text_test.clone(), 1, true)
            .expect("test doc");

        let doc = state.documents.get(&uri_test).expect("test doc");
        let foo_offset = text_test.find("foo").expect("foo");
        let position = doc.line_index.to_position(&doc.text, foo_offset);
        let response = state
            .definition(&uri_test, position)
            .expect("definition should resolve to vars.k65");

        match response {
            GotoDefinitionResponse::Array(locations) => {
                assert_eq!(locations.len(), 1);
                assert_eq!(locations[0].uri, uri_vars);
            }
            _ => panic!("unexpected response shape"),
        }
    }

    #[test]
    fn suppresses_cross_module_undefined_symbol_diagnostic() {
        let uri_vars = Uri::from_str("file:///project/src/vars.k65").expect("uri");
        let text_vars = "var foo = $1234\n".to_string();

        let uri_test = Uri::from_str("file:///project/src/test.k65").expect("uri");
        let text_test = "func test @a8 {\n  lda foo\n}\n".to_string();

        let mut state = ServerState::new(PathBuf::from("/project"));
        state
            .upsert_document(uri_vars.clone(), text_vars, 1, false)
            .expect("vars doc");
        state
            .upsert_document(uri_test.clone(), text_test, 1, true)
            .expect("test doc");

        let diagnostics = state.lsp_diagnostics(&uri_test);
        let has_foo_error = diagnostics
            .iter()
            .any(|d| d.message.contains("unknown identifier 'foo'"));
        assert!(
            !has_foo_error,
            "cross-module reference to 'foo' should not produce a diagnostic, got: {:?}",
            diagnostics.iter().map(|d| &d.message).collect::<Vec<_>>()
        );
    }

    #[test]
    fn symbols_survive_parse_failure() {
        let uri = Uri::from_str("file:///project/src/main.k65").expect("uri");
        let good_text = "func main {\n  nop\n}\n".to_string();

        let mut state = ServerState::new(PathBuf::from("/project"));
        state
            .upsert_document(uri.clone(), good_text, 1, true)
            .expect("good doc");
        assert!(
            state.symbols.contains_key("main"),
            "main should be in the symbol index after good parse"
        );

        // Introduce a syntax error — symbols should be preserved
        let bad_text = "func main {\n  <<<broken>>>\n}\n".to_string();
        state
            .upsert_document(uri.clone(), bad_text, 2, true)
            .expect("bad doc");
        assert!(
            state.symbols.contains_key("main"),
            "main should still be in the symbol index after failed parse"
        );
    }

    #[test]
    fn symbols_extracted_from_file_opened_with_errors() {
        let uri = Uri::from_str("file:///project/src/main.k65").expect("uri");
        // File has a valid func definition followed by a syntax error
        let text = "func main {\n  nop\n}\n\nfunc broken {\n  <<<bad>>>\n}\n".to_string();

        let mut state = ServerState::new(PathBuf::from("/project"));
        state
            .upsert_document(uri.clone(), text, 1, true)
            .expect("doc");

        // The parser should recover and still extract "main" (and possibly "broken")
        assert!(
            state.symbols.contains_key("main"),
            "main should be in the symbol index even when file has errors, got: {:?}",
            state.symbols.keys().collect::<Vec<_>>()
        );
    }

    #[test]
    fn symbols_extracted_despite_lex_errors() {
        let uri = Uri::from_str("file:///project/src/main.k65").expect("uri");
        // File has a valid func followed by an unrecognized token (~ is not in the lexer)
        let text = "func main {\n  nop\n}\n\nfunc other {\n  lda ~bogus\n}\n".to_string();

        let mut state = ServerState::new(PathBuf::from("/project"));
        state
            .upsert_document(uri.clone(), text, 1, true)
            .expect("doc");

        assert!(
            state.symbols.contains_key("main"),
            "main should be in the symbol index despite lex errors, got: {:?}",
            state.symbols.keys().collect::<Vec<_>>()
        );
    }

    #[test]
    fn symbols_extracted_with_double_colon_errors() {
        let uri = Uri::from_str("file:///project/src/main.k65").expect("uri");
        // Uses TCB::name syntax that produces parse errors
        let text = "\
func test {
    nop
}

func main @a16 @i16 {
    call task_init
    lda #dbg_task
    sta reg_pc,x
    @a8 {
        lda #0
        sta TASKS+TCB::name+0,y
        sta TASKS+TCB::state,y
    }
    cli
    {} always
}
"
        .to_string();

        let mut state = ServerState::new(PathBuf::from("/project"));
        state
            .upsert_document(uri.clone(), text, 1, true)
            .expect("doc");

        assert!(
            state.symbols.contains_key("test"),
            "test should be in the symbol index despite :: errors, got: {:?}",
            state.symbols.keys().collect::<Vec<_>>()
        );
        assert!(
            state.symbols.contains_key("main"),
            "main should be in the symbol index despite :: errors, got: {:?}",
            state.symbols.keys().collect::<Vec<_>>()
        );
    }

    #[test]
    fn token_match_includes_mode_directives() {
        let text = "@a16\n";
        let token = token_at_offset(text, 2).expect("token");
        assert_eq!(token.text, "@a16");
        assert_eq!(token_prefix_at_offset(text, 3), "@a1");
    }

    #[test]
    fn completion_context_prefers_symbols_in_operand_positions() {
        let text = "func main {\n  bra\n  bra start\n}\n";
        let statement_start_offset = text.find("  bra").expect("statement start") + 2;
        let operand_offset = text.find("bra start").expect("operand") + 4;

        assert!(!in_symbol_completion_context(text, statement_start_offset));
        assert!(in_symbol_completion_context(text, operand_offset));
    }

    #[test]
    fn const_hover_shows_decimal_and_hex_for_integer_value() {
        let uri = Uri::from_str("file:///project/src/main.k65").expect("uri");
        let text = "const FOO = $2A\nfunc main {\n  lda #FOO\n}\n".to_string();

        let mut state = ServerState::new(PathBuf::from("/project"));
        state
            .upsert_document(uri.clone(), text.clone(), 1, true)
            .expect("doc");

        let doc = state.documents.get(&uri).expect("doc");
        let offset = text.find("FOO").expect("FOO");
        let position = doc.line_index.to_position(&doc.text, offset);
        let hover = state.hover(&uri, position).expect("hover");
        let HoverContents::Markup(markup) = hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(markup.value.contains("**constant** `FOO`"));
        assert!(markup.value.contains("- decimal: `42`"));
        assert!(markup.value.contains("- hex: `$2A`"));
    }

    #[test]
    fn numeric_literal_hover_shows_decimal_hex_and_binary_for_u8_range() {
        let uri = Uri::from_str("file:///project/src/main.k65").expect("uri");
        let text = "func main {\n  lda #42\n}\n".to_string();

        let mut state = ServerState::new(PathBuf::from("/project"));
        state
            .upsert_document(uri.clone(), text.clone(), 1, true)
            .expect("doc");

        let doc = state.documents.get(&uri).expect("doc");
        let offset = text.find("42").expect("42");
        let position = doc.line_index.to_position(&doc.text, offset);
        let hover = state.hover(&uri, position).expect("hover");
        let HoverContents::Markup(markup) = hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(markup.value.contains("- decimal: `42`"));
        assert!(markup.value.contains("- hex: `$2A`"));
        assert!(markup.value.contains("- binary: `%00101010`"));
    }

    #[test]
    fn numeric_literal_hover_omits_binary_for_values_over_255() {
        let uri = Uri::from_str("file:///project/src/main.k65").expect("uri");
        let text = "func main {\n  lda #$1234\n}\n".to_string();

        let mut state = ServerState::new(PathBuf::from("/project"));
        state
            .upsert_document(uri.clone(), text.clone(), 1, true)
            .expect("doc");

        let doc = state.documents.get(&uri).expect("doc");
        let offset = text.find("$1234").expect("$1234");
        let position = doc.line_index.to_position(&doc.text, offset);
        let hover = state.hover(&uri, position).expect("hover");
        let HoverContents::Markup(markup) = hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(markup.value.contains("- decimal: `4660`"));
        assert!(markup.value.contains("- hex: `$1234`"));
        assert!(!markup.value.contains("binary"));
    }

    #[test]
    fn const_hover_formats_negative_integer_as_signed_magnitude_hex() {
        let uri = Uri::from_str("file:///project/src/main.k65").expect("uri");
        let text = "const NEG = 0 - 42\nfunc main {\n  lda #NEG\n}\n".to_string();

        let mut state = ServerState::new(PathBuf::from("/project"));
        state
            .upsert_document(uri.clone(), text.clone(), 1, true)
            .expect("doc");

        let doc = state.documents.get(&uri).expect("doc");
        let offset = text.find("NEG").expect("NEG");
        let position = doc.line_index.to_position(&doc.text, offset);
        let hover = state.hover(&uri, position).expect("hover");
        let HoverContents::Markup(markup) = hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(markup.value.contains("- decimal: `-42`"));
        assert!(markup.value.contains("- hex: `-$2A`"));
    }

    #[test]
    fn query_memory_map_reports_unavailable_when_no_link_layout() {
        let state = ServerState::new(PathBuf::from("/project"));
        let result = state.query_memory_map(&QueryMemoryMapParams::default());

        assert_eq!(result.status, QueryMemoryMapStatus::Unavailable);
        assert!(result.memories.is_empty());
        assert!(result.runs.is_empty());
    }

    #[test]
    fn query_memory_map_reports_area_usage() {
        let uri = Uri::from_str("file:///project/src/main.k65").expect("uri");
        let text = "func main {\n  nop\n}\n".to_string();
        let mut state = ServerState::new(PathBuf::from("/project"));
        state
            .upsert_document(uri, text, 1, true)
            .expect("document inserted");

        let result = state.query_memory_map(&QueryMemoryMapParams::default());
        assert_eq!(result.status, QueryMemoryMapStatus::Ok);
        let main = result
            .memories
            .iter()
            .find(|memory| memory.name == "MAIN")
            .expect("MAIN memory row");
        assert!(main.used > 0, "used bytes should be greater than zero");
        assert_eq!(main.free + main.used, main.size);
        let expected = (f64::from(main.used) * 100.0) / f64::from(main.size);
        assert!((main.utilization_percent - expected).abs() < 0.0001);
    }

    #[test]
    fn query_memory_map_filters_by_memory_name() {
        let uri = Uri::from_str("file:///project/src/main.k65").expect("uri");
        let text = "func main {\n  nop\n}\n".to_string();
        let mut state = ServerState::new(PathBuf::from("/project"));
        state
            .upsert_document(uri, text, 1, true)
            .expect("document inserted");

        let result = state.query_memory_map(&QueryMemoryMapParams {
            memory_name: Some("MAIN".to_string()),
            detail: QueryMemoryMapDetail::Summary,
        });

        assert_eq!(result.status, QueryMemoryMapStatus::Ok);
        assert_eq!(result.memories.len(), 1);
        assert_eq!(result.memories[0].name, "MAIN");
    }

    #[test]
    fn query_memory_map_includes_run_rows_with_detail_runs() {
        let uri = Uri::from_str("file:///project/src/main.k65").expect("uri");
        let text = "func main {\n  nop\n}\n".to_string();
        let mut state = ServerState::new(PathBuf::from("/project"));
        state
            .upsert_document(uri, text, 1, true)
            .expect("document inserted");

        let result = state.query_memory_map(&QueryMemoryMapParams {
            memory_name: None,
            detail: QueryMemoryMapDetail::Runs,
        });

        assert_eq!(result.status, QueryMemoryMapStatus::Ok);
        assert!(!result.runs.is_empty(), "expected run rows");
        for run in &result.runs {
            let expected_end = if run.size == 0 {
                run.start
            } else {
                run.start + run.size - 1
            };
            assert_eq!(run.end, expected_end);
        }
    }

    #[test]
    fn resolve_inline_symbols_resolves_variables_with_read_size_hint() {
        let uri = Uri::from_str("file:///project/src/main.k65").expect("uri");
        let text = "var foo = $1234\nfunc main {\n  lda foo\n}\n".to_string();
        let mut state = ServerState::new(PathBuf::from("/project"));
        state
            .upsert_document(uri.clone(), text, 1, true)
            .expect("document inserted");

        let result = state.resolve_inline_symbols(
            &uri,
            &ResolveInlineSymbolsParams {
                uri: uri.to_string(),
                start_line: 2,
                end_line: 2,
            },
        );

        let foo = result
            .symbols
            .iter()
            .find(|symbol| symbol.name == "foo")
            .expect("foo symbol");
        assert_eq!(foo.category, "variable");
        assert_eq!(foo.address, 0x1234);
        assert_eq!(foo.read_size_hint, Some(1));
    }

    #[test]
    fn resolve_inline_symbols_resolves_labels_and_functions() {
        let uri = Uri::from_str("file:///project/src/main.k65").expect("uri");
        let text = "func main {\nstart:\n  nop\n}\n".to_string();
        let mut state = ServerState::new(PathBuf::from("/project"));
        state
            .upsert_document(uri.clone(), text, 1, true)
            .expect("document inserted");

        let result = state.resolve_inline_symbols(
            &uri,
            &ResolveInlineSymbolsParams {
                uri: uri.to_string(),
                start_line: 0,
                end_line: 1,
            },
        );

        let start = result
            .symbols
            .iter()
            .find(|symbol| symbol.name == "start")
            .expect("start label");
        assert_eq!(start.category, "label");

        let main = result
            .symbols
            .iter()
            .find(|symbol| symbol.name == "main")
            .expect("main function");
        assert_eq!(main.category, "function");
    }

    #[test]
    fn resolve_inline_symbols_skips_unresolved_identifiers() {
        let uri = Uri::from_str("file:///project/src/main.k65").expect("uri");
        let text = "func main {\n  lda missing\n}\n".to_string();
        let mut state = ServerState::new(PathBuf::from("/project"));
        state
            .upsert_document(uri.clone(), text, 1, true)
            .expect("document inserted");

        let result = state.resolve_inline_symbols(
            &uri,
            &ResolveInlineSymbolsParams {
                uri: uri.to_string(),
                start_line: 1,
                end_line: 1,
            },
        );

        assert!(
            !result.symbols.iter().any(|symbol| symbol.name == "missing"),
            "unresolved token must not produce inline symbol"
        );
    }

    #[test]
    fn workspace_root_prefers_first_workspace_folder() {
        let temp = std::env::temp_dir();
        let workspace_root = temp.join("k816-lsp-workspace");
        let root_uri = temp.join("k816-lsp-root-uri");

        let params = initialize_params(json!({
            "capabilities": {},
            "rootUri": file_uri(&root_uri),
            "workspaceFolders": [
                {
                    "uri": file_uri(&workspace_root),
                    "name": "workspace"
                }
            ]
        }));

        let resolved = determine_workspace_root(&params).expect("workspace root");
        assert_eq!(resolved, workspace_root);
    }

    #[test]
    fn workspace_root_errors_when_client_omits_root_hints() {
        let params = initialize_params(json!({
            "capabilities": {}
        }));

        let error = determine_workspace_root(&params).expect_err("missing workspace root");
        assert!(
            error
                .to_string()
                .contains("workspaceFolders/rootUri/rootPath"),
            "unexpected error: {error}"
        );
    }
}
