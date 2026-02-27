use crate::ast::{Expr, File};
use crate::diag::Diagnostic;
use crate::lexer::{lex, lex_lenient};
use crate::span::{SourceId, Span, Spanned};
use chumsky::{
    Parser as _,
    input::{Input as _, Stream},
};

use super::{
    coalesce_non_var_brackets, collect_parser_warnings, expression_fragment_parser, file_parser,
    preprocess_source, rich_error_to_diagnostic, strip_comments,
};

#[derive(Debug, Clone)]
pub struct ParseOutput {
    pub file: File,
    pub warnings: Vec<Diagnostic>,
}

pub fn parse(source_id: SourceId, source_text: &str) -> Result<File, Vec<Diagnostic>> {
    parse_with_warnings(source_id, source_text).map(|parsed| parsed.file)
}

pub fn parse_with_warnings(
    source_id: SourceId,
    source_text: &str,
) -> Result<ParseOutput, Vec<Diagnostic>> {
    let preprocessed = preprocess_source(source_text);
    let source_text = preprocessed.as_str();
    let (tokens, lex_diagnostics) = lex_lenient(source_id, source_text);
    let tokens = coalesce_non_var_brackets(tokens, source_text);
    let (tokens, comments) = strip_comments(tokens);

    let eval_ranges: Vec<(usize, usize)> = tokens
        .iter()
        .filter_map(|t| match &t.kind {
            crate::lexer::TokenKind::Eval(_) => Some((t.span.start, t.span.end)),
            _ => None,
        })
        .collect();
    let lex_errors: Vec<Diagnostic> = lex_diagnostics
        .into_iter()
        .filter(|d| {
            !eval_ranges
                .iter()
                .any(|(start, end)| d.primary.start >= *start && d.primary.end <= *end)
        })
        .collect();
    if !lex_errors.is_empty() {
        return Err(lex_errors);
    }

    let end_offset = tokens.last().map(|token| token.span.end).unwrap_or(0);
    let token_stream = Stream::from_iter(tokens.into_iter().map(|token| {
        let span = (token.span.start..token.span.end).into();
        (token.kind, span)
    }))
    .map((end_offset..end_offset).into(), |(kind, span): (_, _)| {
        (kind, span)
    });

    let (output, errors) = file_parser(source_id)
        .parse(token_stream)
        .into_output_errors();
    let diagnostics = errors
        .into_iter()
        .map(|error| rich_error_to_diagnostic(source_id, source_text, error, "invalid syntax"))
        .collect::<Vec<_>>();

    match output {
        Some(mut file) => {
            file.comments = comments;
            let mut warnings = collect_parser_warnings(&file);
            let mut errors = Vec::new();
            for diag in diagnostics {
                match diag.severity {
                    crate::diag::Severity::Warning => warnings.push(diag),
                    crate::diag::Severity::Error => errors.push(diag),
                }
            }
            if errors.is_empty() {
                Ok(ParseOutput { file, warnings })
            } else {
                Err(errors)
            }
        }
        None => Err(diagnostics),
    }
}

pub fn parse_lenient(source_id: SourceId, source_text: &str) -> (Option<File>, Vec<Diagnostic>) {
    let preprocessed = preprocess_source(source_text);
    let source_text = preprocessed.as_str();
    let (tokens, lex_diagnostics) = lex_lenient(source_id, source_text);
    let tokens = coalesce_non_var_brackets(tokens, source_text);
    let (tokens, comments) = strip_comments(tokens);
    let end_offset = tokens.last().map(|token| token.span.end).unwrap_or(0);
    let token_stream = Stream::from_iter(tokens.into_iter().map(|token| {
        let span = (token.span.start..token.span.end).into();
        (token.kind, span)
    }))
    .map((end_offset..end_offset).into(), |(kind, span): (_, _)| {
        (kind, span)
    });

    let (output, errors) = file_parser(source_id)
        .parse(token_stream)
        .into_output_errors();
    let mut diagnostics: Vec<Diagnostic> = lex_diagnostics;
    diagnostics.extend(
        errors
            .into_iter()
            .map(|error| rich_error_to_diagnostic(source_id, source_text, error, "invalid syntax")),
    );

    let output = output.map(|mut file| {
        file.comments = comments;
        file
    });

    if let Some(ref file) = output {
        let mut warnings = collect_parser_warnings(file);
        warnings.append(&mut diagnostics);
        return (output, warnings);
    }

    (output, diagnostics)
}

pub fn parse_lenient_raw(
    source_id: SourceId,
    source_text: &str,
) -> (Option<File>, Vec<Diagnostic>) {
    let (tokens, lex_diagnostics) = lex_lenient(source_id, source_text);
    let tokens = coalesce_non_var_brackets(tokens, source_text);
    let (tokens, comments) = strip_comments(tokens);
    let end_offset = tokens.last().map(|token| token.span.end).unwrap_or(0);
    let token_stream = Stream::from_iter(tokens.into_iter().map(|token| {
        let span = (token.span.start..token.span.end).into();
        (token.kind, span)
    }))
    .map((end_offset..end_offset).into(), |(kind, span): (_, _)| {
        (kind, span)
    });

    let (output, errors) = file_parser(source_id)
        .parse(token_stream)
        .into_output_errors();
    let mut diagnostics: Vec<Diagnostic> = lex_diagnostics;
    diagnostics.extend(
        errors
            .into_iter()
            .map(|error| rich_error_to_diagnostic(source_id, source_text, error, "invalid syntax")),
    );

    let output = output.map(|mut file| {
        file.comments = comments;
        file
    });

    if let Some(ref file) = output {
        let mut warnings = collect_parser_warnings(file);
        warnings.append(&mut diagnostics);
        return (output, warnings);
    }

    (output, diagnostics)
}

pub fn parse_expression_fragment(
    source_id: SourceId,
    source_text: &str,
) -> Result<Spanned<Expr>, Diagnostic> {
    let tokens = match lex(source_id, source_text) {
        Ok(tokens) => tokens,
        Err(mut diagnostics) => {
            return Err(diagnostics
                .pop()
                .expect("lexer should produce at least one diagnostic"));
        }
    };

    let end_offset = tokens.last().map(|token| token.span.end).unwrap_or(0);
    let token_stream = Stream::from_iter(tokens.into_iter().map(|token| {
        let span = (token.span.start..token.span.end).into();
        (token.kind, span)
    }))
    .map((end_offset..end_offset).into(), |(kind, span): (_, _)| {
        (kind, span)
    });

    expression_fragment_parser()
        .parse(token_stream)
        .into_result()
        .map(|(expr, span)| {
            let range = span.into_range();
            Spanned::new(expr, Span::new(source_id, range.start, range.end))
        })
        .map_err(|mut errors| {
            let error = errors
                .pop()
                .expect("chumsky should return at least one parse error");
            rich_error_to_diagnostic(source_id, source_text, error, "invalid expression fragment")
        })
}
