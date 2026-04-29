use crate::ast::{Comment, Expr, File};
use crate::diag::Diagnostic;
use crate::lexer::{Token, TokenKind, lex, lex_lenient};
use crate::span::{SourceId, Span, Spanned};
use chumsky::{
    Parser as _,
    input::{Input as _, Stream},
};
use k816_isa65816::is_mnemonic;
use std::collections::HashSet;
use std::sync::Arc;

use super::{
    coalesce_non_var_brackets, collect_parser_warnings, expression_fragment_parser, file_parser,
    preprocess_source, rich_error_to_diagnostic, strip_comments,
};

/// Scan a source file for top-level function/inline/naked declarations without
/// running the full parser. Used to build the cross-unit function name set for
/// multi-file compilation, so per-file parsing can dispatch call-site syntax
/// against callees defined in other translation units.
pub fn scan_declared_function_names(source_id: SourceId, source_text: &str) -> HashSet<String> {
    let preprocessed = preprocess_source(source_text);
    let (tokens, _) = lex_lenient(source_id, preprocessed.as_str());
    let tokens = coalesce_non_var_brackets(tokens, preprocessed.as_str());
    let (tokens, _) = strip_comments(tokens);
    let arc = collect_declared_function_names(&tokens);
    (*arc).clone()
}

/// Scan a source for the names of every declared addressable symbol — every
/// `func`/`naked`/`inline`/`var`/`data` declaration, plus every `name:` label
/// (top-level or inside a function body). Used for `&&name` validation: the
/// lower pass rejects `&&UNKNOWN` references where `UNKNOWN` isn't declared
/// anywhere in the workspace, surfacing what would otherwise be a link-time
/// "undefined symbol" error during compile/LSP analysis.
pub fn scan_declared_addressable_names(
    source_id: SourceId,
    source_text: &str,
) -> HashSet<String> {
    let preprocessed = preprocess_source(source_text);
    let (tokens, _) = lex_lenient(source_id, preprocessed.as_str());
    let tokens = coalesce_non_var_brackets(tokens, preprocessed.as_str());
    let (tokens, _) = strip_comments(tokens);
    collect_declared_addressable_names(&tokens)
}

fn collect_declared_addressable_names(tokens: &[Token]) -> HashSet<String> {
    let mut names = HashSet::new();
    // Functions (top-level only — nested funcs aren't K65 syntax anyway, but
    // brace depth tracking matches `collect_declared_function_names`).
    let function_names = collect_declared_function_names(tokens);
    names.extend((*function_names).iter().cloned());

    let mut i = 0usize;
    while i < tokens.len() {
        match &tokens[i].kind {
            // `var NAME`, `var NAME, NAME2, ...`. Multiple names per line allowed.
            TokenKind::Var => {
                let mut j = i + 1;
                loop {
                    if let Some(Token {
                        kind: TokenKind::Ident(name),
                        ..
                    }) = tokens.get(j)
                    {
                        if !name.starts_with('.') {
                            names.insert(name.clone());
                        }
                        j += 1;
                        // Comma → consume and look for another ident.
                        if matches!(
                            tokens.get(j),
                            Some(Token { kind: TokenKind::Comma, .. })
                        ) {
                            j += 1;
                            continue;
                        }
                    }
                    break;
                }
                i = j.max(i + 1);
                continue;
            }
            // `data NAME { ... }` — the name immediately follows `data`.
            TokenKind::Data => {
                if let Some(Token {
                    kind: TokenKind::Ident(name),
                    ..
                }) = tokens.get(i + 1)
                    && !name.starts_with('.')
                {
                    names.insert(name.clone());
                }
                i += 1;
                continue;
            }
            // Labels: `name:` — only count global (non-`.`-prefixed) ones.
            // Local `.label` forms are scope-local and never used as `&&` targets.
            TokenKind::Ident(name) if !name.starts_with('.') => {
                if matches!(
                    tokens.get(i + 1),
                    Some(Token { kind: TokenKind::Colon, .. })
                ) {
                    names.insert(name.clone());
                }
                i += 1;
                continue;
            }
            _ => {
                i += 1;
            }
        }
    }
    names
}

fn merge_known_functions(
    local: Arc<HashSet<String>>,
    external: Option<&HashSet<String>>,
) -> Arc<HashSet<String>> {
    match external {
        Some(ext) if !ext.is_empty() => {
            let mut merged = (*local).clone();
            merged.extend(ext.iter().cloned());
            Arc::new(merged)
        }
        _ => local,
    }
}

fn is_reserved_function_name(name: &str) -> bool {
    is_mnemonic(name)
}

fn collect_declared_function_names(tokens: &[Token]) -> Arc<HashSet<String>> {
    let mut names = HashSet::new();
    let mut brace_depth = 0usize;
    let mut i = 0usize;

    while i < tokens.len() {
        match tokens[i].kind {
            TokenKind::LBrace => {
                brace_depth += 1;
                i += 1;
                continue;
            }
            TokenKind::RBrace => {
                brace_depth = brace_depth.saturating_sub(1);
                i += 1;
                continue;
            }
            _ => {}
        }

        if brace_depth != 0 {
            i += 1;
            continue;
        }

        let mut j = i;
        let mut saw_modifier = false;
        while let Some(token) = tokens.get(j) {
            match token.kind {
                TokenKind::Far | TokenKind::Naked | TokenKind::Inline => {
                    saw_modifier = true;
                    j += 1;
                }
                _ => break,
            }
        }

        if let Some(Token {
            kind: TokenKind::Func,
            ..
        }) = tokens.get(j)
        {
            j += 1;
            if let Some(Token {
                kind: TokenKind::Ident(name),
                ..
            }) = tokens.get(j)
            {
                if !is_reserved_function_name(name) {
                    names.insert(name.clone());
                }
            }
        } else if saw_modifier
            && let Some(Token {
                kind: TokenKind::Ident(name),
                ..
            }) = tokens.get(j)
        {
            if !is_reserved_function_name(name) {
                names.insert(name.clone());
            }
        }

        i += 1;
    }

    Arc::new(names)
}

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
    parse_with_warnings_and_externals(source_id, source_text, None)
}

/// Output of the shared "lenient" lex+coalesce+strip+function-name pipeline,
/// reused by every public file-level parse entry point in this module.
struct PreparedTokens {
    tokens: Vec<Token>,
    comments: Vec<Comment>,
    lex_diagnostics: Vec<Diagnostic>,
    known_functions: Arc<HashSet<String>>,
}

/// Run the lenient lex prologue: lex, coalesce bracket sequences, strip
/// comments, and collect the declared-function name set (optionally merged
/// with cross-unit externals). Callers are responsible for any source-text
/// preprocessing they need before calling.
fn prepare_lenient_tokens(
    source_id: SourceId,
    source_text: &str,
    external_function_names: Option<&HashSet<String>>,
) -> PreparedTokens {
    let (tokens, lex_diagnostics) = lex_lenient(source_id, source_text);
    let tokens = coalesce_non_var_brackets(tokens, source_text);
    let (tokens, comments) = strip_comments(tokens);
    let known_functions = merge_known_functions(
        collect_declared_function_names(&tokens),
        external_function_names,
    );
    PreparedTokens {
        tokens,
        comments,
        lex_diagnostics,
        known_functions,
    }
}

/// Build the chumsky token stream for `tokens` and run `file_parser` against
/// it. Returns the `Option<File>` output alongside parser-error diagnostics
/// (lex diagnostics are the caller's responsibility).
fn run_file_parser(
    source_id: SourceId,
    source_text: &str,
    tokens: Vec<Token>,
    known_functions: Arc<HashSet<String>>,
) -> (Option<File>, Vec<Diagnostic>) {
    let end_offset = tokens.last().map(|token| token.span.end).unwrap_or(0);
    let token_stream = Stream::from_iter(tokens.into_iter().map(|token| {
        let span = (token.span.start..token.span.end).into();
        (token.kind, span)
    }))
    .map((end_offset..end_offset).into(), |(kind, span): (_, _)| {
        (kind, span)
    });

    let (output, errors) = file_parser(source_id, known_functions)
        .parse(token_stream)
        .into_output_errors();
    let diagnostics = errors
        .into_iter()
        .map(|error| rich_error_to_diagnostic(source_id, source_text, error, "invalid syntax"))
        .collect();
    (output, diagnostics)
}

/// Shared body for the two lenient-mode entry points
/// (`parse_lenient_and_externals` and `parse_lenient_raw`). Combines the
/// prepared tokens with the parser run, attaches comments to the output AST,
/// and folds parser warnings back into the diagnostic stream.
fn run_lenient_pipeline(
    source_id: SourceId,
    source_text: &str,
    prepared: PreparedTokens,
) -> (Option<File>, Vec<Diagnostic>) {
    let PreparedTokens {
        tokens,
        comments,
        lex_diagnostics,
        known_functions,
    } = prepared;

    let (output, parse_diagnostics) =
        run_file_parser(source_id, source_text, tokens, known_functions);

    let mut diagnostics = lex_diagnostics;
    diagnostics.extend(parse_diagnostics);

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

pub fn parse_with_warnings_and_externals(
    source_id: SourceId,
    source_text: &str,
    external_function_names: Option<&HashSet<String>>,
) -> Result<ParseOutput, Vec<Diagnostic>> {
    let preprocessed = preprocess_source(source_text);
    let source_text = preprocessed.as_str();
    let prepared = prepare_lenient_tokens(source_id, source_text, external_function_names);

    // Lex diagnostics inside `#eval { ... }` blocks are intentionally suppressed
    // here — the eval expander reports them with their own evaluator context.
    let eval_ranges: Vec<(usize, usize)> = prepared
        .tokens
        .iter()
        .filter_map(|t| match &t.kind {
            crate::lexer::TokenKind::Eval(_) => Some((t.span.start, t.span.end)),
            _ => None,
        })
        .collect();
    let lex_errors: Vec<Diagnostic> = prepared
        .lex_diagnostics
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

    let (output, diagnostics) = run_file_parser(
        source_id,
        source_text,
        prepared.tokens,
        prepared.known_functions,
    );

    match output {
        Some(mut file) => {
            file.comments = prepared.comments;
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
    parse_lenient_and_externals(source_id, source_text, None)
}

pub fn parse_lenient_and_externals(
    source_id: SourceId,
    source_text: &str,
    external_function_names: Option<&HashSet<String>>,
) -> (Option<File>, Vec<Diagnostic>) {
    let preprocessed = preprocess_source(source_text);
    let source_text = preprocessed.as_str();
    let prepared = prepare_lenient_tokens(source_id, source_text, external_function_names);
    run_lenient_pipeline(source_id, source_text, prepared)
}

pub fn parse_lenient_raw(
    source_id: SourceId,
    source_text: &str,
) -> (Option<File>, Vec<Diagnostic>) {
    let prepared = prepare_lenient_tokens(source_id, source_text, None);
    run_lenient_pipeline(source_id, source_text, prepared)
}

#[allow(clippy::result_large_err)]
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
