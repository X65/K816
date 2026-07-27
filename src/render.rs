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

use ariadne::{Config, Label, Report, ReportKind, Source};

use k816::diagnostic::{Diagnostic, Severity};
use k816::source::{FileId, SourceMap};

/// Renders every diagnostic to stderr.
pub fn render_diagnostics(map: &SourceMap, diagnostics: &[Diagnostic]) {
    let mut cache = MapCache {
        map,
        sources: Vec::new(),
    };
    for diagnostic in diagnostics {
        let report = build_report(diagnostic);
        let _ = report.eprint(&mut cache);
    }
}

fn build_report(diagnostic: &Diagnostic) -> Report<'static, (FileId, std::ops::Range<usize>)> {
    let kind = match diagnostic.severity {
        Severity::Error => ReportKind::Error,
        Severity::Warning => ReportKind::Warning,
        Severity::Note | Severity::Help => ReportKind::Advice,
    };
    let primary = &diagnostic.primary;
    let range = primary.span.start as usize..primary.span.end as usize;

    let mut report = Report::build(kind, (primary.span.file, range.clone()))
        .with_config(Config::default().with_index_type(ariadne::IndexType::Byte))
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
