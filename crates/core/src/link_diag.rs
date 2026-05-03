//! Adapter from `k816_link::LinkDiagnostic` to the unified
//! `k816_core::diag::Diagnostic`. Keeps the linker source-text-free while
//! letting CLI/LSP render link errors through the same machinery as compile
//! errors.

use crate::diag::{Diagnostic, RenderOptions, render_diagnostics_with_options};
use crate::span::{SourceId, SourceMap, Span};

/// Convert a single `LinkDiagnostic` into a renderable `Diagnostic`. Returns
/// `None` only when the diagnostic carries an anchor pointing at a source the
/// caller can't resolve — anchorless diagnostics still produce a `Diagnostic`
/// (with a zero-width span) so callers don't lose them silently.
pub fn link_diagnostic_to_diagnostic(
    link_diag: &k816_link::LinkDiagnostic,
    source_map: &SourceMap,
    resolve_path: &dyn Fn(&str) -> Option<SourceId>,
) -> Option<Diagnostic> {
    let (primary_span, primary_label) = match &link_diag.anchor {
        Some(anchor) => {
            let source_id = resolve_path(&anchor.file)?;
            let (start, end) = anchor_byte_range(source_map, source_id, anchor)?;
            let span = Span::new(source_id, start, end);
            let label = link_diag.primary_label.clone().unwrap_or_else(|| {
                // Mirror the link crate's previous default label so message text
                // stays familiar in golden output.
                "here".to_string()
            });
            (span, label)
        }
        None => {
            // Anchorless: pin to a synthetic zero-width span at byte 0 of the
            // first source if available, just so ariadne has somewhere to
            // anchor the message line. Renderers that want pure message-only
            // output can drop the label.
            let source_id = SourceId(0);
            let span = Span::new(source_id, 0, 0);
            let label = link_diag
                .primary_label
                .clone()
                .unwrap_or_else(|| String::new());
            (span, label)
        }
    };

    let mut diag = match link_diag.severity {
        k816_link::LinkSeverity::Error => Diagnostic::error(primary_span, link_diag.message.clone()),
        k816_link::LinkSeverity::Warning => {
            Diagnostic::warning(primary_span, link_diag.message.clone())
        }
    }
    .with_primary_label(primary_label);

    for related in &link_diag.related {
        if let Some(source_id) = resolve_path(&related.anchor.file)
            && let Some((start, end)) = anchor_byte_range(source_map, source_id, &related.anchor)
        {
            diag = diag.with_label(Span::new(source_id, start, end), related.message.clone());
        }
    }

    if let Some(help) = &link_diag.help {
        diag = diag.with_help(help.clone());
    }

    Some(diag)
}

/// Render a batch of `LinkDiagnostic`s into the same string format the
/// compiler uses for `Diagnostic`. Drops anchorless diagnostics that can't
/// be mapped through the resolver — they fall back to message-only rendering
/// so the user never silently loses error text.
///
/// Used by CLI and golden harness to emit a single rendered string suitable
/// for wrapping in `RenderedDiagnosticError` and for `expected.err` fixtures.
pub fn render_link_errors(
    link_errors: &k816_link::LinkErrors,
    source_map: &SourceMap,
    resolve_path: &dyn Fn(&str) -> Option<SourceId>,
    options: RenderOptions,
) -> String {
    let mut rendered_diags: Vec<Diagnostic> = Vec::with_capacity(link_errors.0.len());
    let mut leftover_messages: Vec<String> = Vec::new();
    for link_diag in &link_errors.0 {
        match link_diagnostic_to_diagnostic(link_diag, source_map, resolve_path) {
            Some(diag) => rendered_diags.push(diag),
            None => leftover_messages.push(link_diag.message.clone()),
        }
    }
    let mut output = render_diagnostics_with_options(source_map, &rendered_diags, options);
    if !leftover_messages.is_empty() {
        if !output.is_empty() && !output.ends_with('\n') {
            output.push('\n');
        }
        for msg in &leftover_messages {
            if !output.is_empty() && !output.ends_with('\n') {
                output.push('\n');
            }
            output.push_str(msg);
        }
    }
    output
}

fn anchor_byte_range(
    source_map: &SourceMap,
    source_id: SourceId,
    anchor: &k816_o65::SourceLocation,
) -> Option<(usize, usize)> {
    let file = source_map.get(source_id)?;
    let line = anchor.line as usize;
    let start_col = anchor.column as usize;
    let end_col = anchor.column_end as usize;
    let start = file.line_col_to_offset(line, start_col)?;
    let end = file
        .line_col_to_offset(line, end_col.max(start_col))
        .unwrap_or(start);
    let end = end.max(start);
    let end = if end == start {
        // Zero-width anchors render with no underline; widen to one byte so
        // there's something to highlight.
        (start + 1).min(file.text.len())
    } else {
        end
    };
    Some((start, end))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn dummy_anchor(file: &str, line: u32, col: u32, col_end: u32, line_text: &str) -> k816_o65::SourceLocation {
        k816_o65::SourceLocation {
            file: file.to_string(),
            line,
            column: col,
            column_end: col_end,
            line_text: line_text.to_string(),
        }
    }

    #[test]
    fn maps_anchor_line_column_into_byte_range() {
        let mut map = SourceMap::default();
        let text = "func main {\n    lda foo\n}\n";
        let id = map.add_source("main.k65", text);

        let anchor = dummy_anchor("main.k65", 2, 9, 12, "    lda foo");
        let link_diag = k816_link::LinkDiagnostic {
            severity: k816_link::LinkSeverity::Error,
            message: "undefined symbol 'foo'".to_string(),
            primary_label: Some("symbol 'foo' referenced here".to_string()),
            anchor: Some(anchor),
            help: None,
            related: Vec::new(),
        };

        let diag = link_diagnostic_to_diagnostic(&link_diag, &map, &|path| {
            (path == "main.k65").then_some(id)
        })
        .expect("diagnostic must resolve");

        assert_eq!(diag.message, "undefined symbol 'foo'");
        assert_eq!(diag.primary.source_id, id);
        let slice = &text[diag.primary.start..diag.primary.end];
        assert_eq!(slice, "foo");
    }

    #[test]
    fn drops_diagnostic_when_path_does_not_resolve() {
        let mut map = SourceMap::default();
        let _ = map.add_source("main.k65", "");
        let anchor = dummy_anchor("missing.k65", 1, 1, 2, "x");
        let link_diag = k816_link::LinkDiagnostic {
            severity: k816_link::LinkSeverity::Error,
            message: "x".to_string(),
            primary_label: None,
            anchor: Some(anchor),
            help: None,
            related: Vec::new(),
        };

        let diag = link_diagnostic_to_diagnostic(&link_diag, &map, &|_| None);
        assert!(diag.is_none());
    }
}
