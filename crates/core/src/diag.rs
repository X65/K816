use std::collections::{HashMap, hash_map::Entry};
use std::fmt;

use ariadne::{Cache, Color, ColorGenerator, Config, IndexType, Label, Report, ReportKind, Source};

use crate::span::{SourceId, SourceMap, Span};

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
pub enum Supplemental {
    Help(String),
    Note(String),
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub severity: Severity,
    pub message: String,
    pub primary: Span,
    pub labels: Vec<LabelledSpan>,
    pub supplements: Vec<Supplemental>,
}

impl Diagnostic {
    fn new(severity: Severity, primary: Span, message: impl Into<String>) -> Self {
        Self {
            severity,
            message: message.into(),
            primary,
            labels: Vec::new(),
            supplements: Vec::new(),
        }
    }

    pub fn error(primary: Span, message: impl Into<String>) -> Self {
        Self::new(Severity::Error, primary, message)
    }

    pub fn warning(primary: Span, message: impl Into<String>) -> Self {
        Self::new(Severity::Warning, primary, message)
    }

    pub fn with_label(mut self, span: Span, message: impl Into<String>) -> Self {
        self.labels.push(LabelledSpan {
            span,
            message: message.into(),
        });
        self
    }

    pub fn with_help(mut self, help: impl Into<String>) -> Self {
        self.supplements.push(Supplemental::Help(help.into()));
        self
    }

    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.supplements.push(Supplemental::Note(note.into()));
        self
    }
}

impl fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

#[derive(Debug)]
struct SourceMapCache<'a> {
    source_map: &'a SourceMap,
    sources: HashMap<SourceId, Source<&'a str>>,
}

impl<'a> SourceMapCache<'a> {
    fn new(source_map: &'a SourceMap) -> Self {
        Self {
            source_map,
            sources: HashMap::new(),
        }
    }
}

impl<'a> Cache<SourceId> for SourceMapCache<'a> {
    type Storage = &'a str;

    fn fetch(&mut self, id: &SourceId) -> Result<&Source<Self::Storage>, impl fmt::Debug> {
        match self.sources.entry(*id) {
            Entry::Occupied(entry) => Ok::<_, String>(entry.into_mut()),
            Entry::Vacant(entry) => {
                let file = self
                    .source_map
                    .get(*id)
                    .ok_or_else(|| format!("missing source for id {:?}", id))?;
                Ok::<_, String>(entry.insert(Source::from(file.text.as_str())))
            }
        }
    }

    fn display<'b>(&self, id: &'b SourceId) -> Option<impl fmt::Display + 'b> {
        self.source_map.get(*id).map(|file| file.name.clone())
    }
}

fn report_kind(severity: Severity) -> ReportKind<'static> {
    match severity {
        Severity::Error => ReportKind::Error,
        Severity::Warning => ReportKind::Warning,
    }
}

fn primary_color(severity: Severity) -> Color {
    match severity {
        Severity::Error => Color::Red,
        Severity::Warning => Color::Yellow,
    }
}

fn plain_severity_name(severity: Severity) -> &'static str {
    match severity {
        Severity::Error => "error",
        Severity::Warning => "warning",
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RenderOptions {
    pub color: bool,
}

impl RenderOptions {
    pub const fn plain() -> Self {
        Self { color: false }
    }

    pub const fn colored() -> Self {
        Self { color: true }
    }
}

pub fn render_diagnostic(source_map: &SourceMap, diagnostic: &Diagnostic) -> String {
    render_diagnostic_with_options(source_map, diagnostic, RenderOptions::plain())
}

pub fn render_diagnostic_with_options(
    source_map: &SourceMap,
    diagnostic: &Diagnostic,
    options: RenderOptions,
) -> String {
    let primary_file = source_map.must_get(diagnostic.primary.source_id);
    let primary_span = (diagnostic.primary.source_id, diagnostic.primary.as_range());
    let mut report = Report::build(report_kind(diagnostic.severity), primary_span.clone())
        .with_config(
            Config::default()
                .with_index_type(IndexType::Byte)
                .with_color(options.color),
        )
        .with_message(diagnostic.message.clone())
        .with_label(
            Label::new(primary_span)
                .with_color(primary_color(diagnostic.severity))
                .with_priority(100)
                .with_order(0)
                .with_message("here"),
        );

    let mut colors = ColorGenerator::new();
    for (index, label) in diagnostic.labels.iter().enumerate() {
        report = report.with_label(
            Label::new((label.span.source_id, label.span.as_range()))
                .with_color(colors.next())
                .with_order((index + 1) as i32)
                .with_message(label.message.clone()),
        );
    }

    for supplement in &diagnostic.supplements {
        report = match supplement {
            Supplemental::Help(help) => report.with_help(help.clone()),
            Supplemental::Note(note) => report.with_note(note.clone()),
        };
    }

    let mut output = Vec::new();
    let mut cache = SourceMapCache::new(source_map);
    if report.finish().write(&mut cache, &mut output).is_ok() {
        return String::from_utf8_lossy(&output).into_owned();
    }

    let (line, col) = primary_file.line_col(diagnostic.primary.start);
    let mut fallback = format!(
        "{}: {}\n --> {}:{}:{}",
        plain_severity_name(diagnostic.severity),
        diagnostic.message,
        primary_file.name,
        line,
        col
    );
    for supplement in &diagnostic.supplements {
        match supplement {
            Supplemental::Help(help) => fallback.push_str(&format!("\nHelp: {help}")),
            Supplemental::Note(note) => fallback.push_str(&format!("\nNote: {note}")),
        }
    }
    fallback
}

pub fn render_diagnostics(source_map: &SourceMap, diagnostics: &[Diagnostic]) -> String {
    render_diagnostics_with_options(source_map, diagnostics, RenderOptions::plain())
}

pub fn render_diagnostics_with_options(
    source_map: &SourceMap,
    diagnostics: &[Diagnostic],
    options: RenderOptions,
) -> String {
    diagnostics
        .iter()
        .map(|diag| render_diagnostic_with_options(source_map, diag, options))
        .collect::<Vec<_>>()
        .join("\n")
}
