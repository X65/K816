/*****************************************************************************/
/*                                                                           */
/*                                   lsp.rs                                  */
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

//! The minimal, synchronous LSP server: live syntax and semantic
//! diagnostics.
//!
//! The server never runs an approximate compiler — every analysis goes
//! through the same shared frontend the CLI uses, via
//! [`crate::workspace::Workspace`]. This module only converts between the
//! protocol and the compiler's structured [`Diagnostic`] values. There is no
//! editor-specific code here; hover, definition, rename, completion, and
//! formatting arrive with sub-plan H.

use lsp_server::{Connection, Message, Notification as ServerNotification};
use lsp_types::notification::{
    DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument, Notification,
    PublishDiagnostics,
};
use lsp_types::{
    DiagnosticRelatedInformation, DiagnosticSeverity, DidChangeTextDocumentParams,
    DidCloseTextDocumentParams, DidOpenTextDocumentParams, Location, NumberOrString, Position,
    PublishDiagnosticsParams, Range, ServerCapabilities, TextDocumentSyncCapability,
    TextDocumentSyncKind, Uri,
};

use crate::Options;
use crate::diagnostic::{Diagnostic, Severity};
use crate::source::{FileId, SourceMap, Span};
use crate::workspace::Workspace;

/// Converts a byte [`Span`] into an LSP range (UTF-16 columns).
pub fn span_to_range(map: &SourceMap, span: Span) -> Range {
    Range::new(
        position(map, span.file, span.start),
        position(map, span.file, span.end),
    )
}

fn position(map: &SourceMap, file: FileId, offset: u32) -> Position {
    let source = map.file(file);
    let location = source.line_col(offset);
    let line_text = source.line_text(location.line);
    let byte_column = (location.column as usize).min(line_text.len());
    let utf16_column: usize = line_text[..byte_column].chars().map(char::len_utf16).sum();
    Position::new(location.line, utf16_column as u32)
}

/// Converts one structured compiler diagnostic into its protocol form.
///
/// The CLI renders the same [`Diagnostic`] values with Ariadne; neither side
/// re-derives anything, so they cannot disagree.
pub fn diagnostic_to_lsp(map: &SourceMap, diagnostic: &Diagnostic) -> lsp_types::Diagnostic {
    let severity = match diagnostic.severity {
        Severity::Error => DiagnosticSeverity::ERROR,
        Severity::Warning => DiagnosticSeverity::WARNING,
        Severity::Note => DiagnosticSeverity::INFORMATION,
        Severity::Help => DiagnosticSeverity::HINT,
    };

    let mut message = diagnostic.title.clone();
    if !diagnostic.primary.message.is_empty() {
        message.push_str(&format!("\n{}", diagnostic.primary.message));
    }
    if let Some(help) = &diagnostic.help {
        message.push_str(&format!("\nhelp: {help}"));
    }

    let related: Vec<DiagnosticRelatedInformation> = diagnostic
        .secondary
        .iter()
        .filter_map(|label| {
            let uri: Uri = map.file(label.span.file).name().parse().ok()?;
            Some(DiagnosticRelatedInformation {
                location: Location::new(uri, span_to_range(map, label.span)),
                message: label.message.clone(),
            })
        })
        .collect();

    lsp_types::Diagnostic {
        range: span_to_range(map, diagnostic.primary.span),
        severity: Some(severity),
        code: Some(NumberOrString::String(diagnostic.code.as_str().to_owned())),
        code_description: None,
        source: Some("k816".to_owned()),
        message,
        related_information: (!related.is_empty()).then_some(related),
        tags: None,
        data: None,
    }
}

/// All protocol diagnostics belonging to `file`.
pub fn file_diagnostics(
    map: &SourceMap,
    file: FileId,
    diagnostics: &[Diagnostic],
) -> Vec<lsp_types::Diagnostic> {
    diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.primary.span.file == file)
        .map(|diagnostic| diagnostic_to_lsp(map, diagnostic))
        .collect()
}

/// Runs the synchronous LSP server over stdio until the client shuts down.
pub fn run_stdio_server() -> Result<(), Box<dyn std::error::Error + Sync + Send>> {
    let (connection, io_threads) = Connection::stdio();
    let capabilities = serde_json::to_value(ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        ..Default::default()
    })?;
    connection.initialize(capabilities)?;
    main_loop(&connection)?;
    // The writer thread finishes only once the connection's channel closes.
    drop(connection);
    io_threads.join()?;
    Ok(())
}

fn main_loop(connection: &Connection) -> Result<(), Box<dyn std::error::Error + Sync + Send>> {
    let mut workspace = Workspace::new();
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
                    publish_all(connection, &workspace)?;
                }
                DidChangeTextDocument::METHOD => {
                    let params: DidChangeTextDocumentParams =
                        serde_json::from_value(notification.params)?;
                    // Full sync: the last content change is the new text.
                    if let Some(change) = params.content_changes.into_iter().next_back() {
                        workspace.update(params.text_document.uri.to_string(), change.text);
                        publish_all(connection, &workspace)?;
                    }
                }
                DidCloseTextDocument::METHOD => {
                    let params: DidCloseTextDocumentParams =
                        serde_json::from_value(notification.params)?;
                    workspace.close(params.text_document.uri.as_str());
                    // Clear stale diagnostics for the closed document.
                    send_diagnostics(connection, params.text_document.uri, Vec::new())?;
                    publish_all(connection, &workspace)?;
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
fn publish_all(
    connection: &Connection,
    workspace: &Workspace,
) -> Result<(), Box<dyn std::error::Error + Sync + Send>> {
    let (map, analysis) = workspace.analyze(&Options::default());
    for source in map.files() {
        let Ok(uri) = source.name().parse::<Uri>() else {
            continue;
        };
        let diagnostics = file_diagnostics(&map, source.id(), &analysis.diagnostics);
        send_diagnostics(connection, uri, diagnostics)?;
    }
    Ok(())
}

fn send_diagnostics(
    connection: &Connection,
    uri: Uri,
    diagnostics: Vec<lsp_types::Diagnostic>,
) -> Result<(), Box<dyn std::error::Error + Sync + Send>> {
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
