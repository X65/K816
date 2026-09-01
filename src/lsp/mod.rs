/*****************************************************************************/
/*                                                                           */
/*                                   mod.rs                                  */
/*                                                                           */
/*         Editor-neutral LSP server publishing compiler diagnostics         */
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

//! The synchronous LSP server: live syntax and semantic diagnostics, and
//! the surface built on the same analysis.
//!
//! The server never runs an approximate compiler — every analysis goes
//! through the same shared frontend the CLI uses, via
//! [`crate::workspace::Workspace`], and stops at [`crate::Analysis`]
//! (`doc/design.md` §10). What lives here is the protocol: the loop, the
//! capabilities, the dispatch, and the conversions.
//!
//! The modules divide by what they answer for. [`convert`] owns positions,
//! ranges and URIs, in both directions and nowhere else. [`diagnostics`]
//! renders the compiler's structured [`Diagnostic`](crate::diagnostic::Diagnostic)
//! values into their protocol form. [`config`] reads the options an editor
//! states, in the spellings the command line uses. [`context`] answers "what
//! is under this byte", which is where every request that is about a position
//! begins.
//!
//! `lsp_types` stops at this directory: nothing outside `src/lsp/` imports
//! it, the way nothing outside `src/parser/` imports `tree_sitter`.

pub mod completion;
pub mod config;
mod context;
pub mod convert;
pub mod diagnostics;
pub mod hover;
pub mod navigation;
pub mod rename;

pub use context::{Context, at};
pub use convert::{span_to_range, uri};
pub use diagnostics::{diagnostic_to_lsp, file_diagnostics};

use lsp_server::{Connection, Message, Notification as ServerNotification};
use lsp_types::notification::{
    DidChangeConfiguration, DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument,
    Notification, PublishDiagnostics, ShowMessage,
};
use lsp_types::request::{
    Completion, DocumentSymbolRequest, GotoDefinition, HoverRequest, PrepareRenameRequest,
    References, Rename, Request, WorkspaceSymbolRequest,
};
use lsp_types::{
    CompletionItem, CompletionItemKind, CompletionOptions, CompletionParams, CompletionResponse,
    DidChangeConfigurationParams, DidChangeTextDocumentParams, DidCloseTextDocumentParams,
    DidOpenTextDocumentParams, DocumentSymbolParams, DocumentSymbolResponse, GotoDefinitionParams,
    GotoDefinitionResponse, HoverContents, HoverParams, HoverProviderCapability, MarkupContent,
    MarkupKind, MessageType, OneOf, PrepareRenameResponse, PublishDiagnosticsParams,
    ReferenceParams, RenameOptions, RenameParams, ServerCapabilities, ShowMessageParams,
    TextDocumentPositionParams, TextDocumentSyncCapability, TextDocumentSyncKind, TextEdit, Uri,
    WorkspaceEdit, WorkspaceSymbolParams, WorkspaceSymbolResponse,
};

use crate::Options;
use crate::parser::SourceEdit;
use crate::workspace::Workspace;

/// Anything that goes wrong while the server is talking to its client.
type ServerError = Box<dyn std::error::Error + Sync + Send>;

/// Runs the synchronous LSP server over stdio until the client shuts down.
///
/// `options` is what the command line stated; a configuration the client
/// sends replaces it, wholly or key by key (`config`, decision 1).
pub fn run_stdio_server(options: Options) -> Result<(), ServerError> {
    let (connection, io_threads) = Connection::stdio();
    let capabilities = serde_json::to_value(ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(
            TextDocumentSyncKind::INCREMENTAL,
        )),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        completion_provider: Some(CompletionOptions {
            trigger_characters: Some(
                completion::TRIGGERS
                    .iter()
                    .map(|trigger| (*trigger).to_owned())
                    .collect(),
            ),
            ..Default::default()
        }),
        definition_provider: Some(OneOf::Left(true)),
        references_provider: Some(OneOf::Left(true)),
        document_symbol_provider: Some(OneOf::Left(true)),
        workspace_symbol_provider: Some(OneOf::Left(true)),
        // `prepareProvider`, so the editor asks before it opens a rename box
        // and never offers one for a mnemonic or a register token.
        rename_provider: Some(OneOf::Right(RenameOptions {
            prepare_provider: Some(true),
            work_done_progress_options: Default::default(),
        })),
        ..Default::default()
    })?;
    let params = connection.initialize(capabilities)?;
    serve(&connection, options, &params)?;
    // The writer thread finishes only once the connection's channel closes.
    drop(connection);
    io_threads.join()?;
    Ok(())
}

/// The `initializationOptions` member of an `initialize` request's params.
///
/// Read out of the raw value rather than through `InitializeParams` because
/// that is the only member of it this server uses, and deserializing the
/// whole structure would make an unrelated field the client spells oddly
/// into a failure to start.
fn initialization_options(params: &serde_json::Value) -> Option<&serde_json::Value> {
    params.get("initializationOptions")
}

/// Runs the server's message loop over `connection` until it shuts down.
///
/// Separate from [`run_stdio_server`] so the tests can drive the real loop
/// over an in-memory connection rather than a subprocess: what happens *in*
/// the loop — the coalescing above all — is not observable from the outside
/// except as a count of the notifications it sends.
///
/// **Messages are handled in batches, and the batch is one analysis.** After
/// any message arrives, everything the channel already holds is taken with
/// it; every notification in the batch is applied to the workspace, *then*
/// the workspace is analyzed once, *then* the batch's requests are answered
/// in the order they arrived. A burst of keystrokes is therefore one
/// analysis, and a request that arrived inside the burst is answered against
/// the analysis that burst produced — later than a strictly sequential server
/// would answer it, but with a newer answer (decision 3).
pub fn serve(
    connection: &Connection,
    options: Options,
    initialize_params: &serde_json::Value,
) -> Result<(), ServerError> {
    let mut workspace = Workspace::with_options(options);
    if let Some(value) = initialization_options(initialize_params) {
        configure(connection, &mut workspace, value)?;
    }

    while let Ok(first) = connection.receiver.recv() {
        let batch = drain(connection, first);
        let mut changed = false;
        let mut requests = Vec::new();

        for message in batch {
            match message {
                Message::Request(request) => requests.push(request),
                Message::Notification(notification) => {
                    changed |= apply(connection, &mut workspace, notification)?;
                }
                Message::Response(_) => {}
            }
        }

        if changed {
            publish_all(connection, &mut workspace)?;
        }
        for request in requests {
            if connection.handle_shutdown(&request)? {
                return Ok(());
            }
            let response_result = answer(&mut workspace, &request);
            connection
                .sender
                .send(Message::Response(lsp_server::Response {
                    id: request.id,
                    response_result,
                }))?;
        }
    }
    Ok(())
}

/// `first` plus everything the channel already holds, up to and including a
/// `shutdown` request.
///
/// Draining stops at `shutdown` because `Connection::handle_shutdown` waits
/// on the channel for the `exit` that follows it — a client that pipelines
/// the two would otherwise have its `exit` sitting in this vector while the
/// server waits for it to arrive.
fn drain(connection: &Connection, first: Message) -> Vec<Message> {
    let mut batch = vec![first];
    while !is_shutdown(batch.last().expect("just pushed")) {
        match connection.receiver.try_recv() {
            Ok(message) => batch.push(message),
            Err(_) => break,
        }
    }
    batch
}

fn is_shutdown(message: &Message) -> bool {
    matches!(message, Message::Request(request) if request.method == "shutdown")
}

/// Applies one notification to the workspace, returning whether the analysis
/// it describes is now stale.
fn apply(
    connection: &Connection,
    workspace: &mut Workspace,
    notification: ServerNotification,
) -> Result<bool, ServerError> {
    match notification.method.as_str() {
        DidOpenTextDocument::METHOD => {
            let params: DidOpenTextDocumentParams = serde_json::from_value(notification.params)?;
            workspace.update(
                params.text_document.uri.to_string(),
                params.text_document.text,
            );
            Ok(true)
        }
        DidChangeTextDocument::METHOD => {
            let params: DidChangeTextDocumentParams = serde_json::from_value(notification.params)?;
            let name = params.text_document.uri.to_string();
            for change in params.content_changes {
                match change.range {
                    // A ranged change says what moved, so the retained tree
                    // can be told and the next parse is incremental. The
                    // range is converted against the text the *preceding*
                    // changes left, which is what the map holds by now.
                    Some(range) => {
                        let Some(file) = workspace.file(&name) else {
                            continue;
                        };
                        let span = convert::range_to_span(workspace.map().file(file), range);
                        workspace.apply_edit(&name, &SourceEdit::new(span, change.text));
                    }
                    // A whole-document change: no tree to reuse.
                    None => workspace.update(&name, change.text),
                }
            }
            Ok(true)
        }
        DidChangeConfiguration::METHOD => {
            let params: DidChangeConfigurationParams = serde_json::from_value(notification.params)?;
            configure(connection, workspace, &params.settings)?;
            // The options may have changed what every open document reports.
            Ok(true)
        }
        DidCloseTextDocument::METHOD => {
            let params: DidCloseTextDocumentParams = serde_json::from_value(notification.params)?;
            workspace.close(params.text_document.uri.as_str());
            // Clear stale diagnostics for the closed document: it is no
            // longer in the workspace, so the republish below says nothing
            // about it.
            send_diagnostics(connection, params.text_document.uri, Vec::new())?;
            Ok(true)
        }
        _ => Ok(false),
    }
}

/// Answers one request against the workspace's current analysis.
///
/// Every arm returns the protocol's empty answer rather than an error when it
/// cannot answer — a document the workspace does not hold, a position off the
/// end of one, a parse that is all `ERROR`. A panic in a request handler
/// kills the editor's session (decision 18), and there is nothing useful to
/// say to a client that asked about a document it has not opened.
fn answer(
    workspace: &mut Workspace,
    request: &lsp_server::Request,
) -> Result<serde_json::Value, lsp_server::ResponseError> {
    let analysis = workspace.analyze();
    let map = workspace.map();
    match request.method.as_str() {
        Completion::METHOD => {
            let items: Vec<CompletionItem> =
                serde_json::from_value::<CompletionParams>(request.params.clone())
                    .ok()
                    .and_then(|params| {
                        let trigger = params
                            .context
                            .as_ref()
                            .and_then(|context| context.trigger_character.as_ref())
                            .and_then(|trigger| trigger.chars().next());
                        let (file, offset) = position(workspace, &params.text_document_position)?;
                        Some(
                            completion::complete(&analysis, file, offset, trigger)
                                .into_iter()
                                .map(completion_item)
                                .collect(),
                        )
                    })
                    .unwrap_or_default();
            Ok(serde_json::to_value(CompletionResponse::Array(items))
                .unwrap_or(serde_json::Value::Null))
        }
        HoverRequest::METHOD => {
            let answer = serde_json::from_value::<HoverParams>(request.params.clone())
                .ok()
                .and_then(|params| {
                    let (file, offset) =
                        position(workspace, &params.text_document_position_params)?;
                    let found = hover::hover(&analysis, file, offset)?;
                    Some(lsp_types::Hover {
                        contents: HoverContents::Markup(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: found.markdown,
                        }),
                        range: Some(convert::span_to_range(map, found.span)),
                    })
                });
            Ok(serde_json::to_value(answer).unwrap_or(serde_json::Value::Null))
        }
        GotoDefinition::METHOD => {
            let answer = serde_json::from_value::<GotoDefinitionParams>(request.params.clone())
                .ok()
                .and_then(|params| {
                    let (file, offset) =
                        position(workspace, &params.text_document_position_params)?;
                    let span = navigation::definition(&analysis, file, offset)?;
                    convert::location(map, span)
                })
                .map(GotoDefinitionResponse::Scalar);
            Ok(serde_json::to_value(answer).unwrap_or(serde_json::Value::Null))
        }
        References::METHOD => {
            let locations: Vec<lsp_types::Location> =
                serde_json::from_value::<ReferenceParams>(request.params.clone())
                    .ok()
                    .and_then(|params| {
                        let include = params.context.include_declaration;
                        let (file, offset) = position(workspace, &params.text_document_position)?;
                        Some(
                            navigation::references(&analysis, file, offset, include)
                                .into_iter()
                                .filter_map(|span| convert::location(map, span))
                                .collect(),
                        )
                    })
                    .unwrap_or_default();
            Ok(serde_json::to_value(locations).unwrap_or(serde_json::Value::Null))
        }
        DocumentSymbolRequest::METHOD => {
            let symbols: Vec<lsp_types::DocumentSymbol> =
                serde_json::from_value::<DocumentSymbolParams>(request.params.clone())
                    .ok()
                    .and_then(|params| workspace.file(&params.text_document.uri.to_string()))
                    .map(|file| {
                        navigation::document_symbols(&analysis, file)
                            .iter()
                            .map(|outline| document_symbol(map, outline))
                            .collect()
                    })
                    .unwrap_or_default();
            Ok(
                serde_json::to_value(DocumentSymbolResponse::Nested(symbols))
                    .unwrap_or(serde_json::Value::Null),
            )
        }
        WorkspaceSymbolRequest::METHOD => {
            let symbols: Vec<lsp_types::SymbolInformation> =
                serde_json::from_value::<WorkspaceSymbolParams>(request.params.clone())
                    .ok()
                    .map(|params| {
                        navigation::workspace_symbols(&analysis, &params.query)
                            .iter()
                            .filter_map(|(_, outline)| symbol_information(map, outline))
                            .collect()
                    })
                    .unwrap_or_default();
            Ok(serde_json::to_value(WorkspaceSymbolResponse::Flat(symbols))
                .unwrap_or(serde_json::Value::Null))
        }
        PrepareRenameRequest::METHOD => {
            let answer =
                serde_json::from_value::<TextDocumentPositionParams>(request.params.clone())
                    .ok()
                    .and_then(|params| {
                        let (file, offset) = position(workspace, &params)?;
                        let span = rename::prepare(&analysis, file, offset)?;
                        Some(PrepareRenameResponse::Range(convert::span_to_range(
                            map, span,
                        )))
                    });
            Ok(serde_json::to_value(answer).unwrap_or(serde_json::Value::Null))
        }
        Rename::METHOD => {
            let Ok(params) = serde_json::from_value::<RenameParams>(request.params.clone()) else {
                return Ok(serde_json::Value::Null);
            };
            let Some((file, offset)) = position(workspace, &params.text_document_position) else {
                return Ok(serde_json::Value::Null);
            };
            match rename::rename(&analysis, file, offset, &params.new_name) {
                Ok(renamed) => Ok(serde_json::to_value(workspace_edit(map, &renamed))
                    .unwrap_or(serde_json::Value::Null)),
                // A refusal carries the compiler's own words, and the
                // protocol has exactly one way to put them in front of the
                // user: a response error, which the client shows beside the
                // rename box. Answering `null` instead would refuse silently.
                Err(refusal) => Err(lsp_server::ResponseError {
                    code: lsp_server::ErrorCode::RequestFailed as i32,
                    message: refusal,
                    data: None,
                }),
            }
        }
        _ => Ok(serde_json::Value::Null),
    }
}

/// One candidate in the protocol's form.
///
/// `sort_text` is the label itself: decision 9 declines to rank, so the
/// server states no preference and the client sorts as it likes.
fn completion_item(candidate: completion::Candidate) -> CompletionItem {
    use completion::Kind;
    let kind = match candidate.kind {
        Kind::Mnemonic => CompletionItemKind::OPERATOR,
        Kind::Keyword => CompletionItemKind::KEYWORD,
        Kind::Function => CompletionItemKind::FUNCTION,
        Kind::Variable => CompletionItemKind::VARIABLE,
        Kind::Constant => CompletionItemKind::CONSTANT,
        Kind::Label => CompletionItemKind::REFERENCE,
        Kind::Field => CompletionItemKind::FIELD,
        Kind::Annotation => CompletionItemKind::TYPE_PARAMETER,
    };
    CompletionItem {
        insert_text: candidate.insert.filter(|insert| *insert != candidate.label),
        label: candidate.label,
        kind: Some(kind),
        detail: candidate.detail,
        ..Default::default()
    }
}

/// A rename's spans as the protocol's `WorkspaceEdit`.
#[expect(
    clippy::mutable_key_type,
    reason = "`WorkspaceEdit.changes` is keyed by `Uri`, whose interior state \
              clippy reads as mutable; the protocol chooses the type and the \
              state does not reach its hash"
)]
fn workspace_edit(map: &crate::source::SourceMap, renamed: &rename::Rename) -> WorkspaceEdit {
    let mut changes: std::collections::HashMap<Uri, Vec<TextEdit>> =
        std::collections::HashMap::new();
    for (file, spans) in &renamed.edits {
        let Some(uri) = convert::uri(map, *file) else {
            continue;
        };
        changes.insert(
            uri,
            spans
                .iter()
                .map(|span| TextEdit {
                    range: convert::span_to_range(map, *span),
                    new_text: renamed.replacement.clone(),
                })
                .collect(),
        );
    }
    WorkspaceEdit {
        changes: Some(changes),
        ..Default::default()
    }
}

/// The file and byte offset a text-document position names.
fn position(
    workspace: &Workspace,
    position: &lsp_types::TextDocumentPositionParams,
) -> Option<(crate::source::FileId, u32)> {
    let file = workspace.file(&position.text_document.uri.to_string())?;
    Some((
        file,
        convert::offset_in(workspace.map().file(file), position.position),
    ))
}

/// K816's outline kinds, rendered into the protocol's one list for every
/// language. Which entry a `data` label belongs in is a rendering decision.
fn symbol_kind(kind: navigation::OutlineKind) -> lsp_types::SymbolKind {
    use navigation::OutlineKind;
    match kind {
        OutlineKind::Function => lsp_types::SymbolKind::FUNCTION,
        OutlineKind::Inline => lsp_types::SymbolKind::METHOD,
        OutlineKind::Variable => lsp_types::SymbolKind::VARIABLE,
        OutlineKind::Constant => lsp_types::SymbolKind::CONSTANT,
        OutlineKind::Field => lsp_types::SymbolKind::FIELD,
        OutlineKind::Label => lsp_types::SymbolKind::KEY,
        OutlineKind::Data => lsp_types::SymbolKind::ARRAY,
    }
}

#[expect(deprecated, reason = "lsp-types keeps `deprecated` on the struct")]
fn document_symbol(
    map: &crate::source::SourceMap,
    outline: &navigation::Outline,
) -> lsp_types::DocumentSymbol {
    lsp_types::DocumentSymbol {
        name: outline.name.clone(),
        detail: outline.detail.clone(),
        kind: symbol_kind(outline.kind),
        tags: None,
        deprecated: None,
        range: convert::span_to_range(map, outline.full),
        selection_range: convert::span_to_range(map, outline.declaration),
        children: (!outline.children.is_empty()).then(|| {
            outline
                .children
                .iter()
                .map(|child| document_symbol(map, child))
                .collect()
        }),
    }
}

#[expect(deprecated, reason = "lsp-types keeps `deprecated` on the struct")]
fn symbol_information(
    map: &crate::source::SourceMap,
    outline: &navigation::Outline,
) -> Option<lsp_types::SymbolInformation> {
    Some(lsp_types::SymbolInformation {
        name: outline.name.clone(),
        kind: symbol_kind(outline.kind),
        tags: None,
        deprecated: None,
        location: convert::location(map, outline.full)?,
        container_name: None,
    })
}

/// Analyzes the whole workspace through the shared frontend and publishes
/// per-document diagnostics.
fn publish_all(connection: &Connection, workspace: &mut Workspace) -> Result<(), ServerError> {
    let analysis = workspace.analyze();
    let map = workspace.map();
    for source in map.files() {
        let Ok(uri) = source.name().parse::<Uri>() else {
            continue;
        };
        let diagnostics = file_diagnostics(map, source.id(), &analysis.diagnostics);
        send_diagnostics(connection, uri, diagnostics)?;
    }
    Ok(())
}

/// Reads a configuration object into the workspace and tells the editor
/// about anything it could not use.
///
/// A refused value leaves the option standing rather than falling back to a
/// default: the client stated something, the server could not read it, and
/// quietly compiling under a *third* configuration would be the disagreement
/// this seam exists to remove.
fn configure(
    connection: &Connection,
    workspace: &mut Workspace,
    settings: &serde_json::Value,
) -> Result<(), ServerError> {
    let mut options = workspace.options().clone();
    let messages = config::apply(&mut options, settings);
    workspace.set_options(options);
    for message in &messages {
        show_message(connection, message)?;
    }
    Ok(())
}

fn show_message(
    connection: &Connection,
    message: &config::ConfigMessage,
) -> Result<(), ServerError> {
    let params = ShowMessageParams {
        typ: if message.is_error {
            MessageType::ERROR
        } else {
            MessageType::WARNING
        },
        message: message.render(),
    };
    connection
        .sender
        .send(Message::Notification(ServerNotification::new(
            ShowMessage::METHOD.to_owned(),
            params,
        )))?;
    Ok(())
}

fn send_diagnostics(
    connection: &Connection,
    uri: Uri,
    diagnostics: Vec<lsp_types::Diagnostic>,
) -> Result<(), ServerError> {
    let params = PublishDiagnosticsParams {
        uri,
        diagnostics,
        version: None,
    };
    connection
        .sender
        .send(Message::Notification(ServerNotification::new(
            PublishDiagnostics::METHOD.to_owned(),
            params,
        )))?;
    Ok(())
}
