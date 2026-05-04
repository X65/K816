use crate::diag::Diagnostic;
use crate::lexer::TokenKind;
use crate::span::{SourceId, Span};
use chumsky::{
    error::{RichPattern, RichReason},
    prelude::Rich,
};

use super::is_ident_text;

pub(super) fn rich_error_to_diagnostic(
    source_id: SourceId,
    source_text: &str,
    error: Rich<'_, TokenKind>,
    context: &str,
) -> Diagnostic {
    let range = error.span().into_range();
    let mut span = Span::new(source_id, range.start, range.end);
    let mut primary_label: String = "here".to_string();
    let mut explicit_note: Option<String> = None;
    let (is_warning, message, help) = match error.reason() {
        RichReason::Custom(custom) => {
            let custom = custom.to_string();
            let (is_warning, custom) = if let Some(rest) = custom.strip_prefix("[warn] ") {
                (true, rest.to_string())
            } else {
                (false, custom)
            };
            let RichCustomParts {
                message: message_part,
                hint: embedded_hint,
                label: embedded_label,
                note: embedded_note,
            } = parse_rich_custom(&custom);
            if let Some(label) = embedded_label {
                primary_label = label;
            }
            if let Some(note) = embedded_note {
                explicit_note = Some(note);
            }
            let help = embedded_hint.or_else(|| {
                custom
                    .starts_with("unsupported flag shorthand '")
                    .then_some("expected one of c+, c-, d+, d-, i+, i-, or v-".to_string())
            });
            (is_warning, format!("{context}: {message_part}"), help)
        }
        RichReason::ExpectedFound { expected, found } => {
            if found
                .as_deref()
                .is_some_and(|token| matches!(token, TokenKind::Newline))
            {
                span = Span::new(source_id, range.start, range.start);
                primary_label = "expected here".to_string();
            }
            if found
                .as_deref()
                .is_some_and(|token| matches!(token, TokenKind::Question))
            {
                if let Some(shorthand) =
                    detect_invalid_flag_goto_shorthand(source_text, range.start)
                {
                    let message = format!("{context}: unsupported flag shorthand '{shorthand}'");
                    let help = "expected one of c+, c-, d+, d-, i+, i-, or v-".to_string();
                    (false, message, Some(help))
                } else {
                    let found = found
                        .as_deref()
                        .map(token_kind_message)
                        .unwrap_or_else(|| "end of input".to_string());
                    let expected = format_expected_patterns(expected);
                    if expected.len() > 80 {
                        (false, format!("{context}: unexpected {found}"), None)
                    } else {
                        (
                            false,
                            format!("{context}: expected {expected}, found {found}"),
                            None,
                        )
                    }
                }
            } else {
                let found = found
                    .as_deref()
                    .map(token_kind_message)
                    .unwrap_or_else(|| "end of input".to_string());
                let expected = format_expected_patterns(expected);
                if expected.len() > 80 {
                    (false, format!("{context}: unexpected {found}"), None)
                } else {
                    (
                        false,
                        format!("{context}: expected {expected}, found {found}"),
                        None,
                    )
                }
            }
        }
    };
    let help = help.or_else(|| detect_var_width_after_array_hint(source_text, range.start));
    let token_enrichment = match error.reason() {
        RichReason::ExpectedFound { found, .. } => {
            found.as_deref().and_then(unexpected_token_enrichment)
        }
        _ => None,
    };
    let context_enrichment = detect_double_colon_typo(source_text, range.start)
        .or_else(|| detect_abs_suffix_typo(source_text, range.start))
        .or_else(|| detect_symbolic_subscript_context(source_text, range.start));
    let (label, help, note) = match (token_enrichment, context_enrichment) {
        (Some(token), Some(ctx)) => (
            token.label.or(ctx.label).unwrap_or(primary_label),
            help.or_else(|| token.help.map(str::to_string))
                .or(Some(ctx.help)),
            explicit_note
                .or_else(|| token.note.map(str::to_string))
                .or(Some(ctx.note)),
        ),
        (Some(enriched), None) => (
            enriched.label.unwrap_or(primary_label),
            help.or_else(|| enriched.help.map(str::to_string)),
            explicit_note.or_else(|| enriched.note.map(str::to_string)),
        ),
        (None, Some(ctx)) => (
            ctx.label.unwrap_or(primary_label),
            help.or(Some(ctx.help)),
            explicit_note.or(Some(ctx.note)),
        ),
        (None, None) => (primary_label, help, explicit_note),
    };
    let make_diagnostic = if is_warning {
        Diagnostic::warning
    } else {
        Diagnostic::error
    };
    let mut diagnostic = make_diagnostic(span, message).with_primary_label(label);
    if let Some(help) = help {
        diagnostic = diagnostic.with_help(help);
    }
    if let Some(note) = note {
        diagnostic = diagnostic.with_note(note);
    }
    diagnostic
}

struct TokenEnrichment {
    label: Option<String>,
    help: Option<&'static str>,
    note: Option<&'static str>,
}

/// When the parser bails on a structurally surprising token at a recovery
/// point — a stray closing brace, a binary operator at start-of-statement, or
/// an unrecognized identifier-as-directive — the bare "unexpected X" message
/// rarely tells the user what they should have written. Look up an explicit
/// label/help/note pair so the rendered diagnostic explains the mistake the
/// way an experienced K65 author would.
fn unexpected_token_enrichment(token: &TokenKind) -> Option<TokenEnrichment> {
    match token {
        TokenKind::RBrace => Some(TokenEnrichment {
            label: Some("stray '}'".to_string()),
            help: Some(
                "the parser was looking for a top-level item (`func`, `data`, `var`, `const`, `segment`, `eval`, ...) and found `}` instead — usually this means an earlier `}` already closed the enclosing block; check brace balance from the matching `{` upwards",
            ),
            note: Some(
                "Top-level K816 sources contain only declarations (`func`, `data`, `var`, `const`, `segment`, `eval` blocks); a bare `}` at file scope cannot close anything because nothing is open.",
            ),
        }),
        TokenKind::Gt
        | TokenKind::Lt
        | TokenKind::Plus
        | TokenKind::Minus
        | TokenKind::Star
        | TokenKind::Percent
        | TokenKind::Amp
        | TokenKind::Pipe
        | TokenKind::Caret
        | TokenKind::EqEq
        | TokenKind::BangEq
        | TokenKind::LtEq
        | TokenKind::GtEq => Some(TokenEnrichment {
            label: Some("operator without left-hand side".to_string()),
            help: Some(
                "this operator must follow an expression — at start-of-statement the parser sees nothing for it to act on; if the previous line was meant to continue, remove the line break, or supply the missing left-hand operand",
            ),
            note: Some(
                "K816 statements terminate at a newline unless the previous line ends mid-expression (after an operator, an open paren, etc.); a stray operator at column-0 will not be glued onto the line above.",
            ),
        }),
        _ => None,
    }
}

struct ContextEnrichment {
    label: Option<String>,
    help: String,
    note: String,
}

/// Recognise common syntax-error situations inside a symbolic-subscript field
/// list (the `[ .name[count]:type, ... ]` payload of `var IDENT [ ... ] = base`).
/// All six `syntax-symbolic-subscripts-err-garbage-*` and the
/// `unsupported-field-type` fixtures hit chumsky's generic `expected ..., found
/// X` machinery — bare on its own, that message rarely tells the user *which*
/// shape the field-list grammar expects. When we can recognise the situation
/// from the source line, attach a tailored label + uniform help/note that
/// re-states the field-entry grammar.
fn detect_symbolic_subscript_context(
    source_text: &str,
    error_offset: usize,
) -> Option<ContextEnrichment> {
    if !cursor_inside_var_brackets(source_text, error_offset)
        && !near_var_bracket_opening(source_text, error_offset)
    {
        return None;
    }
    Some(ContextEnrichment {
        label: Some("inside symbolic-subscript field list".to_string()),
        help: "each entry inside `var NAME [ ... ]` is `.field` (optional `[count]`) (optional `:byte`/`:word`/`:far`); separate entries with `,` or a newline; field-list entries must lead with a dot".to_string(),
        note: "Symbolic-subscript field lists declare named offsets inside a var's address window. The grammar is: `[ .field0[count0]:type0, .field1:type1, ... ]`. Only `byte`, `word`, and `far` are accepted as the type tag.".to_string(),
    })
}

/// Even when a stray `]` has prematurely closed the bracket pair, the user is
/// almost certainly trying to write a symbolic-subscript field list. Look at
/// the previous handful of lines for a `var IDENT[` (or `var IDENT [`) opener
/// followed by entries that begin with `.`. If we see that pattern, the
/// diagnostic almost certainly belongs to the same broken construct.
fn near_var_bracket_opening(source_text: &str, error_offset: usize) -> bool {
    let cursor = error_offset.min(source_text.len());
    let prefix = &source_text[..cursor];
    let mut last_lines = prefix
        .rsplit('\n')
        .take(8)
        .collect::<Vec<_>>();
    last_lines.reverse();
    let mut saw_opener = false;
    for line in last_lines {
        let trimmed = line.trim_start();
        if let Some(rest) = trimmed.strip_prefix("var ") {
            let mut chars = rest.chars();
            if chars.next().is_some_and(|c| c.is_ascii_alphabetic() || c == '_')
                && chars
                    .by_ref()
                    .take_while(|c| c.is_ascii_alphanumeric() || *c == '_')
                    .count()
                    > 0
                && rest.contains('[')
            {
                saw_opener = true;
            }
        }
        if saw_opener && (trimmed.starts_with('.') || trimmed.starts_with(']') || trimmed.is_empty())
        {
            continue;
        }
    }
    saw_opener
}

/// Recognise a `name::other` typo (C++/Rust scoping syntax) that the K816
/// parser rejects. K816 uses `.` for field access on symbolic subscripts and
/// has no namespace operator; the second `:` is the unexpected token, so we
/// look one byte back and detect the doubled colon, then point the user at
/// `.` or the legal alternative.
fn detect_double_colon_typo(
    source_text: &str,
    error_offset: usize,
) -> Option<ContextEnrichment> {
    let bytes = source_text.as_bytes();
    if bytes.get(error_offset).copied()? != b':' {
        return None;
    }
    if error_offset == 0 || bytes[error_offset - 1] != b':' {
        return None;
    }
    Some(ContextEnrichment {
        label: Some("`::` is not a K816 operator".to_string()),
        help: "K816 does not have C++/Rust-style `::` scoping; use `.` for symbolic-subscript field access (`base.field`), or rename the symbol to its bare form if you intended a single identifier".to_string(),
        note: "K816 has a flat global symbol space (`const`, `var`, function, label) plus per-var symbolic-subscript fields accessed with `.`. There is no namespace prefix, so `::` carries no meaning to the parser.".to_string(),
    })
}

/// Recognise the removed `name:abs` suffix and steer the user to the new
/// declaration-time syntax. The legacy `:abs` (and `:dp`/`:far`) on
/// expressions was replaced by per-declaration prefixes (`var name:abs = ...`)
/// and per-call-site prefixes (`abs name`/`dp name`/`far name`).
fn detect_abs_suffix_typo(
    source_text: &str,
    error_offset: usize,
) -> Option<ContextEnrichment> {
    let cursor = error_offset.min(source_text.len());
    let after = source_text.get(cursor..)?;
    let trimmed = after.trim_start();
    let suffix = trimmed.split(|c: char| !c.is_ascii_alphabetic()).next()?;
    let suffix_lower = suffix.to_ascii_lowercase();
    if !matches!(suffix_lower.as_str(), "abs" | "dp" | "far" | "word" | "byte") {
        return None;
    }
    let prefix = &source_text[..cursor];
    let preceding = prefix.trim_end_matches(|c: char| c.is_ascii_whitespace());
    if !preceding.ends_with(':') {
        return None;
    }
    let mode = suffix_lower;
    let label = format!("`:{mode}` use-site suffix");
    let help = match mode.as_str() {
        "abs" | "dp" | "far" => format!(
            "the use-site `:{mode}` suffix on plain expressions was removed; declare the var with `var name:{mode} = ...` to set a default, or write the prefix in operand position (`{mode} name`) to override per call site"
        ),
        "word" | "byte" => format!(
            "the use-site `:{mode}` suffix on plain expressions was removed for address operands; declare width on the var (`var name:{mode}[count] = ...`) or use the explicit view form `(name:{mode})` only on expressions, not on bare identifiers"
        ),
        _ => format!("the use-site `:{mode}` suffix is not accepted in this position"),
    };
    Some(ContextEnrichment {
        label: Some(label),
        help,
        note: "K816 separates *declaration-time* width / addressing prefixes (`var name:abs = ...`) from *use-site* prefixes (`lda abs name`, `lda far name`); the old `:abs`/`:dp`/`:far` suffix on plain identifiers was removed because it conflated the two.".to_string(),
    })
}

/// Cheap heuristic: is the byte offset between an open `[` of a `var IDENT [`
/// and its matching `]`? Scans back from the cursor, skipping any matched
/// `[...]` pairs, and stops at the first unmatched `[`. If that `[` is
/// preceded by an identifier (whitespace permitting), we are inside a var's
/// bracket payload. Conservative on purpose — a false positive only adds
/// extra help text to an unrelated diagnostic, but a false negative just
/// reverts to the un-enriched fallback.
fn cursor_inside_var_brackets(source_text: &str, cursor: usize) -> bool {
    let cursor = cursor.min(source_text.len());
    let prefix = &source_text[..cursor];
    let bytes = prefix.as_bytes();
    let mut depth: i32 = 0;
    let mut idx = bytes.len();
    while idx > 0 {
        idx -= 1;
        match bytes[idx] {
            b']' => depth += 1,
            b'[' => {
                if depth == 0 {
                    return preceding_token_is_identifier(bytes, idx);
                }
                depth -= 1;
            }
            _ => {}
        }
    }
    false
}

/// Walk backwards from `bracket_idx` (position of `[`) over whitespace, then
/// confirm at least one identifier character precedes it. Used by
/// `cursor_inside_var_brackets` to tell `var FOO [...]` from a bare `[ ... ]`
/// expression.
fn preceding_token_is_identifier(bytes: &[u8], bracket_idx: usize) -> bool {
    let mut i = bracket_idx;
    while i > 0 {
        let prev = bytes[i - 1];
        if prev == b' ' || prev == b'\t' {
            i -= 1;
        } else {
            break;
        }
    }
    if i == 0 {
        return false;
    }
    let prev = bytes[i - 1];
    prev.is_ascii_alphanumeric() || prev == b'_'
}

struct RichCustomParts {
    message: String,
    hint: Option<String>,
    label: Option<String>,
    note: Option<String>,
}

type CustomTagSetter = fn(&mut RichCustomParts, String);

/// Parse a chumsky `Rich::custom` payload that may carry inline supplemental
/// segments — `; hint: ...`, `; label: ...`, `; note: ...` — appended to the
/// human-readable message. Order does not matter; the first occurrence of each
/// tag wins. Tags are simple substring markers, so message text must not embed
/// any of them literally.
fn parse_rich_custom(raw: &str) -> RichCustomParts {
    const TAGS: [(&str, CustomTagSetter); 3] = [
        ("; hint: ", |parts, value| {
            if parts.hint.is_none() {
                parts.hint = Some(value);
            }
        }),
        ("; label: ", |parts, value| {
            if parts.label.is_none() {
                parts.label = Some(value);
            }
        }),
        ("; note: ", |parts, value| {
            if parts.note.is_none() {
                parts.note = Some(value);
            }
        }),
    ];

    let mut earliest: Option<(usize, usize, CustomTagSetter)> = None;
    for (tag, setter) in TAGS {
        if let Some(idx) = raw.find(tag) {
            match earliest {
                Some((cur_idx, _, _)) if cur_idx <= idx => {}
                _ => earliest = Some((idx, tag.len(), setter)),
            }
        }
    }

    let mut parts = RichCustomParts {
        message: raw.to_string(),
        hint: None,
        label: None,
        note: None,
    };

    let Some((cut, tag_len, first_setter)) = earliest else {
        return parts;
    };

    parts.message = raw[..cut].to_string();
    let mut tail = &raw[cut + tag_len..];
    let mut setter = first_setter;
    loop {
        let mut next_split: Option<(usize, &str, CustomTagSetter)> = None;
        for (tag, candidate) in TAGS {
            if let Some(idx) = tail.find(tag) {
                match next_split {
                    Some((cur_idx, _, _)) if cur_idx <= idx => {}
                    _ => next_split = Some((idx, tag, candidate)),
                }
            }
        }
        match next_split {
            Some((idx, tag, next_setter)) => {
                let value = tail[..idx].to_string();
                setter(&mut parts, value);
                tail = &tail[idx + tag.len()..];
                setter = next_setter;
            }
            None => {
                setter(&mut parts, tail.to_string());
                break;
            }
        }
    }

    parts
}

fn detect_invalid_flag_goto_shorthand(source_text: &str, question_offset: usize) -> Option<String> {
    let bytes = source_text.as_bytes();
    if bytes.get(question_offset).copied()? != b'?' {
        return None;
    }
    if question_offset < 2 {
        return None;
    }

    let sign = bytes[question_offset - 1] as char;
    if sign != '+' && sign != '-' {
        return None;
    }

    let flag = bytes[question_offset - 2] as char;
    if !flag.is_ascii_alphabetic() {
        return None;
    }

    if matches!(flag.to_ascii_lowercase(), 'c' | 'z' | 'n' | 'v' | 'o') {
        return None;
    }

    if question_offset >= 3 {
        let prev = bytes[question_offset - 3] as char;
        if prev.is_ascii_alphanumeric() || prev == '_' || prev == '.' {
            return None;
        }
    }

    let mut idx = question_offset + 1;
    while idx < bytes.len() && matches!(bytes[idx], b' ' | b'\t' | b'\r' | b'\n') {
        idx += 1;
    }

    let tail = source_text.get(idx..)?;
    if !tail.to_ascii_lowercase().starts_with("goto") {
        return None;
    }

    let after_goto = idx + 4;
    if let Some(next) = source_text
        .get(after_goto..)
        .and_then(|rest| rest.chars().next())
        && (next.is_ascii_alphanumeric() || next == '_' || next == '.')
    {
        return None;
    }

    Some(format!("{flag}{sign}?"))
}

fn detect_var_width_after_array_hint(source_text: &str, error_offset: usize) -> Option<String> {
    if source_text.as_bytes().get(error_offset).copied()? != b':' {
        return None;
    }

    let line_start = source_text[..error_offset]
        .rfind('\n')
        .map_or(0, |index| index + 1);
    let line_end = source_text[error_offset..]
        .find('\n')
        .map_or(source_text.len(), |index| error_offset + index);
    let line = source_text.get(line_start..line_end)?.trim();
    let rest = line.strip_prefix("var ")?.trim();

    let bracket_open = rest.find('[')?;
    let bracket_close = rest.rfind(']')?;
    if bracket_close <= bracket_open {
        return None;
    }

    let name = rest[..bracket_open].trim();
    if !is_ident_text(name) {
        return None;
    }
    let count = rest[bracket_open + 1..bracket_close].trim();
    if count.is_empty() {
        return None;
    }
    let suffix = rest[bracket_close + 1..].trim_start();
    if !suffix.starts_with(':') {
        return None;
    }

    let width = suffix
        .trim_start_matches(':')
        .split(|ch: char| ch.is_ascii_whitespace())
        .next()
        .filter(|w| matches!(w.to_ascii_lowercase().as_str(), "byte" | "word"))
        .unwrap_or("word");
    let example = format!("var {name}:{width}[{count}]");
    Some(format!(
        "for typed arrays, place the type before the array length (e.g. `{example}`)"
    ))
}

fn format_expected_patterns(expected: &[RichPattern<'_, TokenKind>]) -> String {
    let mut values = Vec::new();
    for pattern in expected {
        let text = rich_pattern_message(pattern);
        if !values.contains(&text) {
            values.push(text);
        }
    }

    match values.as_slice() {
        [] => "something else".to_string(),
        [single] => single.clone(),
        [a, b] => format!("{a} or {b}"),
        _ => {
            let head = values[..values.len() - 1].join(", ");
            let tail = values.last().expect("non-empty values");
            format!("{head}, or {tail}")
        }
    }
}

fn rich_pattern_message(pattern: &RichPattern<'_, TokenKind>) -> String {
    match pattern {
        RichPattern::Token(token) => token_kind_message(token),
        RichPattern::Label(label) => label.to_string(),
        RichPattern::Identifier(identifier) => format!("'{}'", identifier),
        RichPattern::Any => "any token".to_string(),
        RichPattern::SomethingElse => "something else".to_string(),
        RichPattern::EndOfInput => "end of input".to_string(),
        _ => "something else".to_string(),
    }
}

fn token_kind_message(token: &TokenKind) -> String {
    if let TokenKind::Ident(value) = token {
        return format!("identifier '{value}'");
    }
    token_kind_label(token).to_string()
}

fn token_kind_label(token: &TokenKind) -> &'static str {
    match token {
        TokenKind::Segment => "'segment'",
        TokenKind::Const => "'const'",
        TokenKind::Var => "'var'",
        TokenKind::Func => "'func'",
        TokenKind::Naked => "'naked'",
        TokenKind::Inline => "'inline'",
        TokenKind::Far => "'far'",
        TokenKind::Data => "'data'",
        TokenKind::Align => "'align'",
        TokenKind::Address => "'address'",
        TokenKind::Nocross => "'nocross'",
        TokenKind::Call => "'call'",
        TokenKind::LBrace => "'{'",
        TokenKind::RBrace => "'}'",
        TokenKind::LParen => "'('",
        TokenKind::RParen => "')'",
        TokenKind::Comma => "','",
        TokenKind::Colon => "':'",
        TokenKind::DotDot => "'..'",
        TokenKind::Semi => "';'",
        TokenKind::PlusPlus => "'++'",
        TokenKind::Plus => "'+'",
        TokenKind::Minus => "'-'",
        TokenKind::Star => "'*'",
        TokenKind::Percent => "'%'",
        TokenKind::Amp => "'&'",
        TokenKind::Pipe => "'|'",
        TokenKind::Caret => "'^'",
        TokenKind::Bang => "'!'",
        TokenKind::Question => "'?'",
        TokenKind::EqEq => "'=='",
        TokenKind::BangEq => "'!='",
        TokenKind::LtLtEq => "'<<='",
        TokenKind::GtGtEq => "'>>='",
        TokenKind::LtEq => "'<='",
        TokenKind::GtEq => "'>='",
        TokenKind::Arrow => "'->'",
        TokenKind::Lt => "'<'",
        TokenKind::Gt => "'>'",
        TokenKind::Hash => "'#'",
        TokenKind::Eq => "'='",
        TokenKind::Newline => "newline",
        TokenKind::LBracket => "'['",
        TokenKind::RBracket => "']'",
        TokenKind::Eval(_) => "eval fragment",
        TokenKind::String(_) => "string literal",
        TokenKind::Number(_) => "number literal",
        TokenKind::Ident(_) => "identifier",
        TokenKind::ModeA8 => "'@a8'",
        TokenKind::ModeA16 => "'@a16'",
        TokenKind::ModeI8 => "'@i8'",
        TokenKind::ModeI16 => "'@i16'",
        TokenKind::SwapOp => "'><'",
        TokenKind::LineComment(_) => "line comment",
        TokenKind::BlockComment(_) => "block comment",
    }
}
