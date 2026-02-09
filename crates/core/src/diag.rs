use std::fmt;

use ariadne::{Color, Label, Report, ReportKind, Source};

use crate::span::{SourceMap, Span};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
}

#[derive(Debug, Clone)]
pub struct LabelledSpan {
    pub span: Span,
    pub message: String,
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub severity: Severity,
    pub message: String,
    pub primary: Span,
    pub labels: Vec<LabelledSpan>,
    pub hint: Option<String>,
}

impl Diagnostic {
    pub fn error(primary: Span, message: impl Into<String>) -> Self {
        Self {
            severity: Severity::Error,
            message: message.into(),
            primary,
            labels: Vec::new(),
            hint: None,
        }
    }

    pub fn with_label(mut self, span: Span, message: impl Into<String>) -> Self {
        self.labels.push(LabelledSpan {
            span,
            message: message.into(),
        });
        self
    }

    pub fn with_hint(mut self, hint: impl Into<String>) -> Self {
        self.hint = Some(hint.into());
        self
    }
}

impl fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

pub fn render_diagnostic(source_map: &SourceMap, diagnostic: &Diagnostic) -> String {
    let file = source_map.must_get(diagnostic.primary.source_id);
    let mut output = Vec::new();
    let mut report = Report::build(
        match diagnostic.severity {
            Severity::Error => ReportKind::Error,
            Severity::Warning => ReportKind::Warning,
        },
        file.name.clone(),
        diagnostic.primary.start,
    )
    .with_message(diagnostic.message.clone())
    .with_label(
        Label::new((
            file.name.clone(),
            diagnostic.primary.start..diagnostic.primary.end,
        ))
        .with_color(Color::Red)
        .with_message("here"),
    );

    for label in &diagnostic.labels {
        let label_file = source_map.must_get(label.span.source_id);
        report = report.with_label(
            Label::new((label_file.name.clone(), label.span.start..label.span.end))
                .with_color(Color::Yellow)
                .with_message(label.message.clone()),
        );
    }

    if let Some(hint) = &diagnostic.hint {
        report = report.with_note(hint.clone());
    }

    let _ = report.finish().write(
        (file.name.clone(), Source::from(file.text.clone())),
        &mut output,
    );

    String::from_utf8_lossy(&output).into_owned()
}

pub fn render_diagnostics(source_map: &SourceMap, diagnostics: &[Diagnostic]) -> String {
    diagnostics
        .iter()
        .map(|diag| render_diagnostic(source_map, diag))
        .collect::<Vec<_>>()
        .join("\n")
}
