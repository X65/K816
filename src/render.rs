/*****************************************************************************/
/*                                                                           */
/*                                 render.rs                                 */
/*                                                                           */
/*             CLI rendering of structured diagnostics via Ariadne           */
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

//! Renders structured [`Diagnostic`] values for the terminal.
//!
//! This is the *only* place CLI presentation happens; the compiler library
//! produces structured data and never renders. The LSP performs its own
//! conversion in `lsp.rs`, from the same structured values.
//!
//! Three formats are offered: the default `human` layout, a `compact`
//! variant, and machine-readable `json` (one object per diagnostic, one per
//! line). The human layouts fall back to plain ASCII without colors when
//! stderr is not a terminal.

use std::io::IsTerminal;

use ariadne::{CharSet, Config, Label, Report, ReportKind, Source};
use serde_json::json;

use k816::diagnostic::{Applicability, Diagnostic, Label as DiagnosticLabel, Severity};
use k816::source::{FileId, SourceMap, Span};

/// How diagnostics are presented on stderr (`--message-format`).
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum MessageFormat {
    /// The full Ariadne layout.
    #[default]
    Human,
    /// The Ariadne compact layout: one line per label, no underlines.
    Compact,
    /// Newline-delimited JSON objects, for tools.
    Json,
}

impl MessageFormat {
    pub fn parse(text: &str) -> Option<Self> {
        match text {
            "human" => Some(Self::Human),
            "compact" => Some(Self::Compact),
            "json" => Some(Self::Json),
            _ => None,
        }
    }
}

/// Renders every diagnostic to stderr in the requested format.
pub fn render_diagnostics(map: &SourceMap, diagnostics: &[Diagnostic], format: MessageFormat) {
    match format {
        MessageFormat::Human | MessageFormat::Compact => {
            render_ariadne(map, diagnostics, format == MessageFormat::Compact);
        }
        MessageFormat::Json => {
            for diagnostic in diagnostics {
                eprintln!("{}", json_diagnostic(map, diagnostic));
            }
        }
    }
}

fn render_ariadne(map: &SourceMap, diagnostics: &[Diagnostic], compact: bool) {
    // Pretty output is for terminals; pipes and log files get plain ASCII.
    let terminal = std::io::stderr().is_terminal();
    let config = Config::default()
        .with_index_type(ariadne::IndexType::Byte)
        .with_compact(compact)
        .with_color(terminal)
        .with_char_set(if terminal {
            CharSet::Unicode
        } else {
            CharSet::Ascii
        });

    let mut cache = MapCache {
        map,
        sources: Vec::new(),
    };
    for diagnostic in diagnostics {
        let report = build_report(diagnostic, config);
        let _ = report.eprint(&mut cache);
    }
}

fn build_report(
    diagnostic: &Diagnostic,
    config: Config,
) -> Report<'static, (FileId, std::ops::Range<usize>)> {
    let kind = match diagnostic.severity {
        Severity::Error => ReportKind::Error,
        Severity::Warning => ReportKind::Warning,
        Severity::Note | Severity::Help => ReportKind::Advice,
    };
    let primary = &diagnostic.primary;
    let range = primary.span.start as usize..primary.span.end as usize;

    let mut report = Report::build(kind, (primary.span.file, range.clone()))
        .with_config(config)
        .with_code(diagnostic.code.as_str())
        .with_message(&diagnostic.title)
        .with_label(
            Label::new((primary.span.file, range))
                .with_message(&primary.message)
                .with_priority(1),
        );
    for label in &diagnostic.secondary {
        report = report.with_label(
            Label::new((
                label.span.file,
                label.span.start as usize..label.span.end as usize,
            ))
            .with_message(&label.message),
        );
    }
    for note in &diagnostic.notes {
        report = report.with_note(note);
    }
    if let Some(help) = &diagnostic.help {
        report = report.with_help(help);
    }
    for fix in &diagnostic.fixes {
        report = report.with_note(format!("fix available: {}", fix.title));
    }
    report.finish()
}

/// One diagnostic as a JSON object. Spans carry byte offsets plus 1-based
/// line/column so both machines and humans can locate them.
fn json_diagnostic(map: &SourceMap, diagnostic: &Diagnostic) -> serde_json::Value {
    let severity = match diagnostic.severity {
        Severity::Error => "error",
        Severity::Warning => "warning",
        Severity::Note => "note",
        Severity::Help => "help",
    };
    json!({
        "code": diagnostic.code.as_str(),
        "severity": severity,
        "title": diagnostic.title,
        "primary": json_label(map, &diagnostic.primary),
        "secondary": diagnostic
            .secondary
            .iter()
            .map(|label| json_label(map, label))
            .collect::<Vec<_>>(),
        "notes": diagnostic.notes,
        "help": diagnostic.help,
        "fixes": diagnostic
            .fixes
            .iter()
            .map(|fix| {
                json!({
                    "title": fix.title,
                    "applicability": match fix.applicability {
                        Applicability::MachineApplicable => "machine-applicable",
                        Applicability::MaybeIncorrect => "maybe-incorrect",
                    },
                    "edits": fix
                        .edits
                        .iter()
                        .map(|edit| {
                            let mut value = json_span(map, edit.span);
                            value["replacement"] = json!(edit.replacement);
                            value
                        })
                        .collect::<Vec<_>>(),
                })
            })
            .collect::<Vec<_>>(),
        "docs": diagnostic.docs.as_ref().map(|docs| docs.0.clone()),
    })
}

fn json_label(map: &SourceMap, label: &DiagnosticLabel) -> serde_json::Value {
    let mut value = json_span(map, label.span);
    value["message"] = json!(label.message);
    value
}

fn json_span(map: &SourceMap, span: Span) -> serde_json::Value {
    let location = map.line_col(span.file, span.start);
    json!({
        "file": map.file(span.file).name(),
        "start": span.start,
        "end": span.end,
        "line": location.line + 1,
        "column": location.column + 1,
    })
}

/// Adapts [`SourceMap`] to Ariadne's cache interface.
struct MapCache<'a> {
    map: &'a SourceMap,
    sources: Vec<Option<Source<String>>>,
}

impl ariadne::Cache<FileId> for &mut MapCache<'_> {
    type Storage = String;

    fn fetch(&mut self, id: &FileId) -> Result<&Source<String>, impl std::fmt::Debug> {
        let index = id.index();
        if self.sources.len() <= index {
            self.sources.resize_with(index + 1, || None);
        }
        if self.sources[index].is_none() {
            self.sources[index] = Some(Source::from(self.map.file(*id).text().to_owned()));
        }
        Ok::<_, std::convert::Infallible>(self.sources[index].as_ref().unwrap())
    }

    fn display<'b>(&self, id: &'b FileId) -> Option<impl std::fmt::Display + 'b> {
        Some(self.map.file(*id).name().to_owned())
    }
}
