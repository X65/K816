use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use std::str::FromStr;
use std::time::Instant;

use anyhow::{Context, Result};
use crossbeam_channel::RecvTimeoutError;
use lsp_server::{Connection, Message, Notification, Request, RequestId, Response, ResponseError};
use lsp_types::notification::{
    DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument, DidSaveTextDocument,
    Notification as LspNotification, PublishDiagnostics,
};
use lsp_types::request::{
    CodeLensRequest, Completion, DocumentSymbolRequest, FoldingRangeRequest, Formatting,
    GotoDefinition, HoverRequest, InlayHintRequest, PrepareRenameRequest, References, Rename,
    Request as LspRequest, SemanticTokensFullRequest, SignatureHelpRequest,
};
use lsp_types::{
    CodeLensOptions, CodeLensParams, CompletionOptions, CompletionParams, Diagnostic,
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    DidSaveTextDocumentParams, DocumentFormattingParams, DocumentSymbolParams, FoldingRangeParams,
    GotoDefinitionParams, HoverParams, HoverProviderCapability, InitializeParams, InlayHintParams,
    OneOf, ReferenceParams, RenameOptions, RenameParams, SemanticTokensFullOptions,
    SemanticTokensLegend, SemanticTokensOptions, SemanticTokensParams, ServerCapabilities,
    SignatureHelpOptions, SignatureHelpParams, TextDocumentSyncCapability, TextDocumentSyncKind,
    Uri, WorkDoneProgressOptions,
};
use serde::de::DeserializeOwned;

use super::project::uri_to_file_path;
use super::protocol::{QueryMemoryMapParams, ResolveAddressesParams, ResolveInlineSymbolsParams};
use super::types::{DID_CHANGE_DEBOUNCE, LOOP_POLL_INTERVAL, ServerState};
use super::{semantic_token_legend_modifiers, semantic_token_legend_types};

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
            References::METHOD => self.on_references(request),
            HoverRequest::METHOD => self.on_hover(request),
            Completion::METHOD => self.on_completion(request),
            DocumentSymbolRequest::METHOD => self.on_document_symbol(request),
            SemanticTokensFullRequest::METHOD => self.on_semantic_tokens_full(request),
            InlayHintRequest::METHOD => self.on_inlay_hints(request),
            Rename::METHOD => self.on_rename(request),
            PrepareRenameRequest::METHOD => self.on_prepare_rename(request),
            CodeLensRequest::METHOD => self.on_code_lens(request),
            FoldingRangeRequest::METHOD => self.on_folding_ranges(request),
            SignatureHelpRequest::METHOD => self.on_signature_help(request),
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

    fn on_references(&mut self, request: Request) -> Result<()> {
        let params: ReferenceParams = parse_request_params(&request)?;
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        self.flush_all_pending_changes()?;
        let result = self.state.references(&uri, position, &params.context);
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

    fn on_semantic_tokens_full(&mut self, request: Request) -> Result<()> {
        let params: SemanticTokensParams = parse_request_params(&request)?;
        let uri = params.text_document.uri;
        self.flush_all_pending_changes()?;
        let result = self.state.semantic_tokens(&uri);
        self.send_result(request.id, &result)
    }

    fn on_inlay_hints(&mut self, request: Request) -> Result<()> {
        let params: InlayHintParams = parse_request_params(&request)?;
        let uri = params.text_document.uri;
        self.flush_all_pending_changes()?;
        let result = self.state.inlay_hints(&uri, params.range);
        self.send_result(request.id, &result)
    }

    fn on_rename(&mut self, request: Request) -> Result<()> {
        let params: RenameParams = parse_request_params(&request)?;
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        self.flush_all_pending_changes()?;
        let result = self.state.rename(&uri, position, &params.new_name);
        self.send_result(request.id, &result)
    }

    fn on_prepare_rename(&mut self, request: Request) -> Result<()> {
        let params: lsp_types::TextDocumentPositionParams = parse_request_params(&request)?;
        let uri = params.text_document.uri;
        let position = params.position;
        self.flush_all_pending_changes()?;
        let result = self.state.prepare_rename(&uri, position);
        self.send_result(request.id, &result)
    }

    fn on_code_lens(&mut self, request: Request) -> Result<()> {
        let params: CodeLensParams = parse_request_params(&request)?;
        let uri = params.text_document.uri;
        self.flush_all_pending_changes()?;
        let result = self.state.code_lenses(&uri);
        self.send_result(request.id, &result)
    }

    fn on_folding_ranges(&mut self, request: Request) -> Result<()> {
        let params: FoldingRangeParams = parse_request_params(&request)?;
        let uri = params.text_document.uri;
        self.ensure_document_fresh(&uri)?;
        let result = self.state.folding_ranges(&uri);
        self.send_result(request.id, &result)
    }

    fn on_signature_help(&mut self, request: Request) -> Result<()> {
        let params: SignatureHelpParams = parse_request_params(&request)?;
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        self.ensure_document_fresh(&uri)?;
        let result = self.state.signature_help(&uri, position);
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
        references_provider: Some(OneOf::Left(true)),
        rename_provider: Some(OneOf::Right(RenameOptions {
            prepare_provider: Some(true),
            work_done_progress_options: WorkDoneProgressOptions::default(),
        })),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        completion_provider: Some(CompletionOptions {
            resolve_provider: Some(false),
            trigger_characters: Some(vec![".".to_string(), "@".to_string()]),
            all_commit_characters: None,
            work_done_progress_options: WorkDoneProgressOptions::default(),
            completion_item: None,
        }),
        semantic_tokens_provider: Some(
            lsp_types::SemanticTokensServerCapabilities::SemanticTokensOptions(
                SemanticTokensOptions {
                    work_done_progress_options: WorkDoneProgressOptions::default(),
                    legend: SemanticTokensLegend {
                        token_types: semantic_token_legend_types(),
                        token_modifiers: semantic_token_legend_modifiers(),
                    },
                    range: Some(false),
                    full: Some(SemanticTokensFullOptions::Bool(true)),
                },
            ),
        ),
        inlay_hint_provider: Some(OneOf::Left(true)),
        code_lens_provider: Some(CodeLensOptions {
            resolve_provider: Some(false),
        }),
        folding_range_provider: Some(lsp_types::FoldingRangeProviderCapability::Simple(true)),
        signature_help_provider: Some(SignatureHelpOptions {
            trigger_characters: Some(vec!["(".to_string(), ",".to_string()]),
            retrigger_characters: None,
            work_done_progress_options: WorkDoneProgressOptions::default(),
        }),
        document_symbol_provider: Some(OneOf::Left(true)),
        document_formatting_provider: Some(OneOf::Left(true)),
        ..ServerCapabilities::default()
    }
}

#[allow(deprecated)]
pub(super) fn determine_workspace_root(params: &InitializeParams) -> Result<PathBuf> {
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
