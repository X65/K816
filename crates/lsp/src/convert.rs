use std::collections::BTreeSet;

use lsp_types::{
    Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity, Location, Range, Uri,
};

use super::{ByteRange, LineIndex};

pub(super) fn is_valid_symbol_name(name: &str) -> bool {
    let mut bytes = name.as_bytes().iter().copied();
    let first = match bytes.next() {
        Some(first) => first,
        None => return false,
    };
    let first_valid = first.is_ascii_alphabetic() || first == b'_' || first == b'.';
    if !first_valid {
        return false;
    }
    bytes.all(|byte| byte.is_ascii_alphanumeric() || byte == b'_' || byte == b'.')
}

pub(super) fn byte_range_to_lsp(range: &ByteRange, line_index: &LineIndex, text: &str) -> Range {
    Range {
        start: line_index.to_position(text, range.start),
        end: line_index.to_position(text, range.end),
    }
}

/// Per-source rendering context the LSP can resolve a foreign `Span` against.
/// Indexed by `SourceId.0 as usize` matching the ordering used when the
/// workspace handed sources to `compile_sources`.
pub(super) struct ForeignSource<'a> {
    pub uri: Uri,
    pub line_index: &'a LineIndex,
    pub text: &'a str,
}

pub(super) fn diagnostic_to_lsp(
    diagnostic: &k816_core::diag::Diagnostic,
    uri: &Uri,
    line_index: &LineIndex,
    text: &str,
    foreign_sources: &[Option<ForeignSource<'_>>],
) -> Diagnostic {
    let primary = byte_range_to_lsp(&ByteRange::from_span(diagnostic.primary), line_index, text);

    let mut related = Vec::new();
    for label in &diagnostic.labels {
        related.push(DiagnosticRelatedInformation {
            location: Location::new(
                uri.clone(),
                byte_range_to_lsp(&ByteRange::from_span(label.span), line_index, text),
            ),
            message: label.message.clone(),
        });
    }

    let mut message = diagnostic.message.clone();
    // Inline-origin annotations come first so they read like a continuation of
    // the message body, before help/note prefixes. Foreign-source spans get
    // turned into related-information entries so editors can navigate to the
    // original location.
    for supplement in &diagnostic.supplements {
        if let k816_core::diag::Supplemental::InlineOrigin { span, label } = supplement {
            message.push('\n');
            message.push('(');
            message.push_str(label);
            message.push(')');
            if let Some(foreign) = foreign_sources
                .get(span.source_id.0 as usize)
                .and_then(|slot| slot.as_ref())
            {
                related.push(DiagnosticRelatedInformation {
                    location: Location::new(
                        foreign.uri.clone(),
                        byte_range_to_lsp(
                            &ByteRange::from_span(*span),
                            foreign.line_index,
                            foreign.text,
                        ),
                    ),
                    message: label.clone(),
                });
            }
        }
    }
    for supplement in &diagnostic.supplements {
        match supplement {
            k816_core::diag::Supplemental::Help(help) => {
                message.push_str("\nhelp: ");
                message.push_str(help);
            }
            k816_core::diag::Supplemental::Note(note) => {
                message.push_str("\nnote: ");
                message.push_str(note);
            }
            k816_core::diag::Supplemental::InlineOrigin { .. } => {}
        }
    }

    Diagnostic {
        range: primary,
        severity: Some(match diagnostic.severity {
            k816_core::diag::Severity::Error => DiagnosticSeverity::ERROR,
            k816_core::diag::Severity::Warning => DiagnosticSeverity::WARNING,
        }),
        code: None,
        code_description: None,
        source: Some("k816".to_string()),
        message,
        related_information: if related.is_empty() {
            None
        } else {
            Some(related)
        },
        tags: None,
        data: None,
    }
}

pub(super) fn dedup_diagnostics(diagnostics: &mut Vec<k816_core::diag::Diagnostic>) {
    let mut seen = BTreeSet::new();
    diagnostics.retain(|diagnostic| {
        seen.insert((
            match diagnostic.severity {
                k816_core::diag::Severity::Error => 0_u8,
                k816_core::diag::Severity::Warning => 1_u8,
            },
            diagnostic.message.clone(),
            diagnostic.primary.start,
            diagnostic.primary.end,
        ))
    });
}
