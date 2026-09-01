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

pub mod config;
mod context;
pub mod convert;
pub mod diagnostics;

pub use context::{Context, at};
pub use convert::{span_to_range, uri};
pub use diagnostics::{diagnostic_to_lsp, file_diagnostics};

use lsp_server::{Connection, Message, Notification as ServerNotification};
use lsp_types::notification::{
    DidChangeConfiguration, DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument,
    Notification, PublishDiagnostics, ShowMessage,
};
use lsp_types::{
    DidChangeConfigurationParams, DidChangeTextDocumentParams, DidCloseTextDocumentParams,
    DidOpenTextDocumentParams, MessageType, PublishDiagnosticsParams, ServerCapabilities,
    ShowMessageParams, TextDocumentSyncCapability, TextDocumentSyncKind, Uri,
};

use crate::Options;
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
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        ..Default::default()
    })?;
    let params = connection.initialize(capabilities)?;
    main_loop(&connection, options, &params)?;
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

fn main_loop(
    connection: &Connection,
    options: Options,
    initialize_params: &serde_json::Value,
) -> Result<(), ServerError> {
    let mut workspace = Workspace::with_options(options);
    if let Some(value) = initialization_options(initialize_params) {
        configure(connection, &mut workspace, value)?;
    }
    for message in &connection.receiver {
        match message {
            Message::Request(request) => {
                if connection.handle_shutdown(&request)? {
                    return Ok(());
                }
                // No other requests are implemented in the slice.
            }
            Message::Notification(notification) => match notification.method.as_str() {
                DidOpenTextDocument::METHOD => {
                    let params: DidOpenTextDocumentParams =
                        serde_json::from_value(notification.params)?;
                    workspace.update(
                        params.text_document.uri.to_string(),
                        params.text_document.text,
                    );
                    publish_all(connection, &mut workspace)?;
                }
                DidChangeTextDocument::METHOD => {
                    let params: DidChangeTextDocumentParams =
                        serde_json::from_value(notification.params)?;
                    // Full sync: the last content change is the new text.
                    if let Some(change) = params.content_changes.into_iter().next_back() {
                        workspace.update(params.text_document.uri.to_string(), change.text);
                        publish_all(connection, &mut workspace)?;
                    }
                }
                DidChangeConfiguration::METHOD => {
                    let params: DidChangeConfigurationParams =
                        serde_json::from_value(notification.params)?;
                    configure(connection, &mut workspace, &params.settings)?;
                    // The options may have changed what every open document
                    // reports, so the whole workspace is republished.
                    publish_all(connection, &mut workspace)?;
                }
                DidCloseTextDocument::METHOD => {
                    let params: DidCloseTextDocumentParams =
                        serde_json::from_value(notification.params)?;
                    workspace.close(params.text_document.uri.as_str());
                    // Clear stale diagnostics for the closed document.
                    send_diagnostics(connection, params.text_document.uri, Vec::new())?;
                    publish_all(connection, &mut workspace)?;
                }
                _ => {}
            },
            Message::Response(_) => {}
        }
    }
    Ok(())
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
