use super::ByteRange;

#[derive(Debug)]
pub(super) struct TokenMatch {
    pub(super) text: String,
    pub(super) start: usize,
    pub(super) end: usize,
}

pub(super) fn token_at_offset(text: &str, offset: usize) -> Option<TokenMatch> {
    let bytes = text.as_bytes();
    if bytes.is_empty() {
        return None;
    }

    let mut index = offset.min(bytes.len().saturating_sub(1));
    if !is_ident_byte(bytes[index]) {
        if index > 0 && is_ident_byte(bytes[index - 1]) {
            index -= 1;
        } else {
            return None;
        }
    }

    let mut start = index;
    while start > 0 && is_ident_byte(bytes[start - 1]) {
        start -= 1;
    }

    let mut end = index + 1;
    while end < bytes.len() && is_ident_byte(bytes[end]) {
        end += 1;
    }

    // Skip tokens preceded by a colon — they are address/width modifiers (:abs, :byte, :word),
    // not symbol references.
    if start > 0 && text.as_bytes()[start - 1] == b':' {
        return None;
    }

    Some(TokenMatch {
        text: text[start..end].to_string(),
        start,
        end,
    })
}

pub(super) fn token_prefix_at_offset(text: &str, offset: usize) -> String {
    let bytes = text.as_bytes();
    let mut start = offset.min(bytes.len());
    while start > 0 && is_ident_byte(bytes[start - 1]) {
        start -= 1;
    }
    text[start..offset.min(text.len())].to_string()
}

pub(super) fn token_matches_in_range(text: &str, start: usize, end: usize) -> Vec<TokenMatch> {
    let bytes = text.as_bytes();
    if bytes.is_empty() {
        return Vec::new();
    }
    let mut out = Vec::new();
    let mut index = start.min(bytes.len());
    let end = end.min(bytes.len());

    while index < end {
        if !is_ident_byte(bytes[index]) {
            index += 1;
            continue;
        }

        let token_start = index;
        while index < end && is_ident_byte(bytes[index]) {
            index += 1;
        }
        let token_end = index;
        if token_end <= token_start {
            continue;
        }

        let token_text = &text[token_start..token_end];
        let first = token_text.as_bytes()[0];
        if first.is_ascii_digit() || first == b'@' {
            continue;
        }

        out.push(TokenMatch {
            text: token_text.to_string(),
            start: token_start,
            end: token_end,
        });
    }

    out
}

fn is_ident_byte(byte: u8) -> bool {
    byte.is_ascii_alphanumeric() || byte == b'_' || byte == b'.' || byte == b'@'
}

pub(super) fn in_symbol_completion_context(text: &str, offset: usize) -> bool {
    if !token_prefix_at_offset(text, offset).is_empty() {
        return true;
    }

    let offset = offset.min(text.len());
    let line_start = text[..offset].rfind('\n').map_or(0, |line| line + 1);
    let line_prefix = &text[line_start..offset];
    let trimmed = line_prefix.trim_start();
    if trimmed.is_empty() {
        return false;
    }

    if line_prefix.chars().last().is_some_and(char::is_whitespace) {
        return true;
    }

    trimmed.split_whitespace().count() > 1
}

pub(super) fn numeric_literal_at_offset(text: &str, offset: usize) -> Option<(ByteRange, i64)> {
    let source_id = k816_core::span::SourceId(0);
    let (tokens, _) = k816_core::lexer::lex_lenient(source_id, text);
    let offset = offset.min(text.len());
    tokens.into_iter().find_map(|token| {
        if offset < token.span.start || offset >= token.span.end {
            return None;
        }
        match token.kind {
            k816_core::lexer::TokenKind::Number(num) => Some((
                ByteRange {
                    start: token.span.start,
                    end: token.span.end,
                },
                num.value,
            )),
            _ => None,
        }
    })
}

pub(super) fn evaluator_call_at_offset(text: &str, offset: usize) -> Option<(String, u32)> {
    let bytes = text.as_bytes();
    if bytes.is_empty() {
        return None;
    }
    let mut index = offset.min(bytes.len());
    let mut paren_depth = 0usize;

    while index > 0 {
        index -= 1;
        match bytes[index] {
            b')' => paren_depth += 1,
            b'(' => {
                if paren_depth > 0 {
                    paren_depth -= 1;
                    continue;
                }

                let mut name_end = index;
                while name_end > 0 && bytes[name_end - 1].is_ascii_whitespace() {
                    name_end -= 1;
                }
                let mut name_start = name_end;
                while name_start > 0 && is_ident_byte(bytes[name_start - 1]) {
                    name_start -= 1;
                }
                if name_start == name_end {
                    continue;
                }

                let name = text[name_start..name_end].to_string();
                let prefix = &text[..name_start];
                let open_eval = prefix.rfind('[');
                let close_eval = prefix.rfind(']');
                let Some(open_idx) = open_eval else {
                    continue;
                };
                if close_eval.is_some_and(|close_idx| close_idx > open_idx) {
                    continue;
                }

                let mut active_param = 0u32;
                let mut nested = 0usize;
                for ch in text[index + 1..offset.min(text.len())].chars() {
                    match ch {
                        '(' => nested += 1,
                        ')' => nested = nested.saturating_sub(1),
                        ',' if nested == 0 => active_param = active_param.saturating_add(1),
                        _ => {}
                    }
                }
                return Some((name, active_param));
            }
            _ => {}
        }
    }

    None
}
