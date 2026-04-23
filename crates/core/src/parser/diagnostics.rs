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
    let mut primary_label = "here";
    let (is_warning, message, help) = match error.reason() {
        RichReason::Custom(custom) => {
            let custom = custom.to_string();
            let (is_warning, custom) = if let Some(rest) = custom.strip_prefix("[warn] ") {
                (true, rest.to_string())
            } else {
                (false, custom)
            };
            let (message_part, embedded_hint) = match custom.find("; hint: ") {
                Some(idx) => (
                    custom[..idx].to_string(),
                    Some(custom[idx + 8..].to_string()),
                ),
                None => (custom.clone(), None),
            };
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
                primary_label = "expected here";
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
    let make_diagnostic = if is_warning {
        Diagnostic::warning
    } else {
        Diagnostic::error
    };
    let diagnostic = make_diagnostic(span, message).with_primary_label(primary_label);
    match help {
        Some(help) => diagnostic.with_help(help),
        None => diagnostic,
    }
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
