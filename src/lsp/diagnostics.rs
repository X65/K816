/*****************************************************************************/
/*                                                                           */
/*                               diagnostics.rs                              */
/*                                                                           */
/*        The compiler's structured diagnostics in their protocol form       */
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

//! Rendering [`Diagnostic`] into the protocol's form.
//!
//! The CLI renders the same values with Ariadne; neither side re-derives
//! anything, so they cannot disagree (`doc/design.md` §9 — a diagnostic is
//! structured data and its rendering is an edge concern; this module is the
//! second renderer).

use lsp_types::{DiagnosticRelatedInformation, DiagnosticSeverity, Location, NumberOrString, Uri};

use super::convert::{span_to_range, uri};
use crate::diagnostic::{Diagnostic, Severity};
use crate::source::{FileId, SourceMap};

/// Converts one structured compiler diagnostic into its protocol form.
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
            let uri: Uri = uri(map, label.span.file)?;
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
        // What the diagnostic carries that the protocol has no field for: how
        // many structured fixes it has. A client that wants them asks for
        // code actions; a client that only wants to know whether to draw a
        // lightbulb can read this without a round trip.
        //
        // Deliberately *not* an index into the analysis that produced it
        // (`plans/07-lsp-tooling.md` H7 asked for one): every request
        // re-analyzes (decision 12), so an index would name a position in a
        // list the request itself has just rebuilt. `actions` matches by span
        // instead, which degrades gracefully when the document moved under a
        // stale diagnostic rather than pointing at the wrong fix.
        data: (!diagnostic.fixes.is_empty())
            .then(|| serde_json::json!({ "fixes": diagnostic.fixes.len() })),
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
