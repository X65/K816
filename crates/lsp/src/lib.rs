use std::collections::{BTreeSet, HashMap};
use std::fs;
use std::path::{Path, PathBuf};
use std::str::FromStr;
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
use serde::Deserialize;
use serde::de::DeserializeOwned;
use walkdir::WalkDir;

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
    Variable,
    Label,
    DataBlock,
    Segment,
}

impl SymbolCategory {
    fn symbol_kind(self) -> SymbolKind {
        match self {
            Self::Function => SymbolKind::FUNCTION,
            Self::Variable => SymbolKind::VARIABLE,
            Self::Label => SymbolKind::CONSTANT,
            Self::DataBlock => SymbolKind::OBJECT,
            Self::Segment => SymbolKind::NAMESPACE,
        }
    }

    fn detail(self) -> &'static str {
        match self {
            Self::Function => "function",
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
}

#[derive(Debug)]
struct ServerState {
    workspace_root: PathBuf,
    source_index: SourceIndex,
    documents: HashMap<Uri, DocumentState>,
    symbols: HashMap<String, Vec<SymbolLocation>>,
}

impl ServerState {
    fn new(workspace_root: PathBuf) -> Self {
        Self {
            workspace_root,
            source_index: SourceIndex::default(),
            documents: HashMap::new(),
            symbols: HashMap::new(),
        }
    }

    fn initialize_workspace(&mut self) -> Result<()> {
        for path in discover_workspace_sources(&self.workspace_root)? {
            self.load_from_disk(path)?;
        }
        self.rebuild_symbol_index();
        Ok(())
    }

    fn load_from_disk(&mut self, path: PathBuf) -> Result<()> {
        let text = fs::read_to_string(&path)
            .with_context(|| format!("failed to read source '{}'", path.display()))?;
        let uri = uri_from_file_path(&path)?;
        let file_id = self
            .source_index
            .ensure_file_id(uri.clone(), Some(path.clone()));
        let source_name = path.display().to_string();
        let analysis = analyze_document(&source_name, &text);
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
        let analysis = self
            .documents
            .remove(&uri)
            .map(|doc| doc.analysis)
            .unwrap_or_default();

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
        doc.analysis = analyze_document(&source_name, &doc.text);
        self.rebuild_symbol_index();
    }

    fn close_document(&mut self, uri: &Uri) {
        if let Some(path) = self.documents.get(uri).and_then(|doc| doc.path.clone())
            && let Ok(text) = fs::read_to_string(&path)
        {
            let file_id = self
                .source_index
                .ensure_file_id(uri.clone(), Some(path.clone()));
            let analysis = analyze_document(&path.display().to_string(), &text);
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
    }

    fn lsp_diagnostics(&self, uri: &Uri) -> Vec<Diagnostic> {
        let Some(doc) = self.documents.get(uri) else {
            return Vec::new();
        };
        doc.analysis
            .diagnostics
            .iter()
            .map(|diag| diagnostic_to_lsp(diag, &doc.uri, &doc.line_index, &doc.text))
            .collect()
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
        let token = token_at_offset(&doc.text, offset)?;
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

        builtin_hover_text(&token.text).map(|text| Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: text,
            }),
            range: Some(token_range),
        })
    }

    fn completion(&self, uri: &Uri, position: Position) -> Option<CompletionResponse> {
        let doc = self.documents.get(uri)?;
        let offset = doc.line_index.to_offset(&doc.text, position)?;
        let prefix = token_prefix_at_offset(&doc.text, offset).to_ascii_lowercase();
        let scope = doc.analysis.scope_at_offset(offset);
        let allow_symbol_completions = in_symbol_completion_context(&doc.text, offset);

        let mut candidates: Vec<(String, CompletionItemKind, &'static str)> = opcode_keywords()
            .into_iter()
            .map(|mnemonic| (mnemonic, CompletionItemKind::KEYWORD, "opcode"))
            .collect();
        candidates.extend(directive_keywords().iter().map(|directive| {
            (
                (*directive).to_string(),
                CompletionItemKind::KEYWORD,
                "directive",
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
                )
            }));
        }

        let mut seen = BTreeSet::new();
        let items = candidates
            .into_iter()
            .filter(|(label, _, _)| {
                if prefix.is_empty() {
                    return true;
                }
                label.to_ascii_lowercase().starts_with(&prefix)
            })
            .filter(|(label, _, _)| seen.insert(label.clone()))
            .map(|(label, kind, detail)| CompletionItem {
                label,
                kind: Some(kind),
                detail: Some(detail.to_string()),
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

        let source_id = k816_core::span::SourceId(0);
        let Ok(ast) = k816_core::parser::parse(source_id, &doc.text) else {
            return Vec::new();
        };

        let formatted = k816_fmt::format_file(&ast);
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
}

#[derive(Debug, Deserialize)]
struct ProjectPackage {
    name: String,
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

fn analyze_document(source_name: &str, source_text: &str) -> DocumentAnalysis {
    let (mut diagnostics, compile_failed) = match k816_core::compile_source_to_object_with_options(
        source_name,
        source_text,
        k816_core::CompileRenderOptions::plain(),
    ) {
        Ok(output) => (output.warnings, false),
        Err(error) => (error.diagnostics, true),
    };

    let mut source_map = k816_core::span::SourceMap::default();
    let source_id = source_map.add_source(source_name, source_text);

    let mut ast = None;
    let mut symbols = Vec::new();
    let mut scopes = Vec::new();
    let mut semantic = SemanticInfo::default();

    if let Ok(parsed) = k816_core::parser::parse_with_warnings(source_id, source_text) {
        if compile_failed {
            diagnostics.extend(parsed.warnings);
        }

        let parsed_file = parsed.file;
        let symbol_collection = collect_symbols(&parsed_file);
        symbols = symbol_collection.symbols;
        scopes = symbol_collection.scopes;
        ast = Some(parsed_file.clone());

        if let Ok(expanded) = k816_core::eval_expand::expand_file(&parsed_file, source_id)
            && let Ok(normalized) = k816_core::normalize_hla::normalize_file(&expanded)
            && let Ok(model) = k816_core::sema::analyze(&normalized)
        {
            for (name, meta) in model.functions {
                semantic.functions.insert(name, meta);
            }
            for (name, meta) in model.vars {
                semantic.vars.insert(name, meta);
            }
        }
    }

    dedup_diagnostics(&mut diagnostics);

    DocumentAnalysis {
        diagnostics,
        symbols,
        scopes,
        semantic,
        ast,
    }
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

fn canonical_symbol(name: &str, scope: Option<&str>) -> String {
    if name.starts_with('.')
        && let Some(scope) = scope
    {
        return format!("{scope}::{name}");
    }
    name.to_string()
}

fn completion_kind_for_symbol(category: SymbolCategory) -> CompletionItemKind {
    match category {
        SymbolCategory::Function => CompletionItemKind::FUNCTION,
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
            return lines.join("\n");
        }
        if let Some(meta) = doc.analysis.semantic.vars.get(canonical) {
            return format!(
                "**variable** `{}`\n- address: `0x{:X}`\n- size: `{}`",
                symbol.name, meta.address, meta.size
            );
        }
    }

    format!("**{}** `{}`", symbol.category.detail(), symbol.name)
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
        return Some(format!(
            "**opcode** `{token}`\nWDC 65816 instruction mnemonic."
        ));
    }

    let text = match token.as_str() {
        "segment" => "Select an output segment for following code/data.",
        "var" => "Declare a variable symbol (optionally typed and/or initialized).",
        "func" => "Declare a function block.",
        "main" => "Declare the program entry block.",
        "far" => "Marks a function for far call/return semantics.",
        "naked" => "Disables automatic function epilogue emission.",
        "inline" => "Marks function as inline-capable in HLA lowering.",
        "data" => "Open a data emission block.",
        "align" => "Align output location to a power-of-two boundary.",
        "address" => "Set absolute output address for following bytes.",
        "nocross" => "Prevent emitted bytes from crossing boundary size.",
        "call" => "Call a known function symbol.",
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
        "segment", "var", "func", "main", "far", "naked", "inline", "data", "align", "address",
        "nocross", "call", "@a8", "@a16", "@i8", "@i16", ".byte",
    ]
}

fn document_symbols_from_ast(
    file: &k816_core::ast::File,
    line_index: &LineIndex,
    text: &str,
) -> Vec<DocumentSymbol> {
    file.items
        .iter()
        .filter_map(|item| match &item.node {
            k816_core::ast::Item::CodeBlock(block) => {
                let children = stmt_document_symbols(&block.body, line_index, text);
                let selection = block.name_span.unwrap_or(item.span);
                Some(make_document_symbol(
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
                ))
            }
            k816_core::ast::Item::Var(var) => Some(make_document_symbol(
                var.name.clone(),
                SymbolCategory::Variable,
                item.span,
                item.span,
                None,
                line_index,
                text,
            )),
            k816_core::ast::Item::NamedDataBlock(block) => Some(make_document_symbol(
                block.name.clone(),
                SymbolCategory::DataBlock,
                item.span,
                block.name_span,
                None,
                line_index,
                text,
            )),
            k816_core::ast::Item::Segment(segment) => Some(make_document_symbol(
                segment.name.clone(),
                SymbolCategory::Segment,
                item.span,
                item.span,
                None,
                line_index,
                text,
            )),
            k816_core::ast::Item::Statement(stmt) => {
                stmt_to_document_symbol(stmt, item.span, line_index, text)
            }
            k816_core::ast::Item::DataBlock(_) => None,
        })
        .collect()
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
        let text = "main {\n  nop\n}\n";
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
        let text = "main {\n.loop:\n  bra .loop\n}\n".to_string();
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
    fn token_match_includes_mode_directives() {
        let text = "@a16\n";
        let token = token_at_offset(text, 2).expect("token");
        assert_eq!(token.text, "@a16");
        assert_eq!(token_prefix_at_offset(text, 3), "@a1");
    }

    #[test]
    fn completion_context_prefers_symbols_in_operand_positions() {
        let text = "main {\n  bra\n  bra start\n}\n";
        let statement_start_offset = text.find("  bra").expect("statement start") + 2;
        let operand_offset = text.find("bra start").expect("operand") + 4;

        assert!(!in_symbol_completion_context(text, statement_start_offset));
        assert!(in_symbol_completion_context(text, operand_offset));
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
