use crate::ast::Comment;
use crate::lexer::{Token, TokenKind};
use crate::span::Span;

pub(super) fn coalesce_non_var_brackets(tokens: Vec<Token>, source: &str) -> Vec<Token> {
    let is_var_bracket = |idx: usize| -> bool {
        let mut j = idx;
        while j > 0 {
            j -= 1;
            match &tokens[j].kind {
                TokenKind::Var => return true,
                TokenKind::Eq
                | TokenKind::Newline
                | TokenKind::Semi
                | TokenKind::LBrace
                | TokenKind::RBrace => return false,
                _ => continue,
            }
        }
        false
    };

    let find_matching_rbracket = |start: usize| -> Option<usize> {
        let mut depth = 1usize;
        let mut j = start + 1;
        while j < tokens.len() {
            match &tokens[j].kind {
                TokenKind::LBracket => depth += 1,
                TokenKind::RBracket => {
                    depth -= 1;
                    if depth == 0 {
                        return Some(j);
                    }
                }
                _ => {}
            }
            j += 1;
        }
        None
    };

    let mut result = Vec::with_capacity(tokens.len());
    let mut i = 0;
    while i < tokens.len() {
        match &tokens[i].kind {
            TokenKind::LBracket => {
                if is_var_bracket(i) {
                    if let Some(close) = find_matching_rbracket(i) {
                        for token in &tokens[i..=close] {
                            result.push(token.clone());
                        }
                        i = close + 1;
                    } else {
                        result.push(tokens[i].clone());
                        i += 1;
                    }
                } else if let Some(close) = find_matching_rbracket(i) {
                    let content_start = tokens[i].span.end;
                    let content_end = tokens[close].span.start;
                    let text = source[content_start..content_end].to_string();
                    let span = Span::new(
                        tokens[i].span.source_id,
                        tokens[i].span.start,
                        tokens[close].span.end,
                    );
                    result.push(Token {
                        kind: TokenKind::Eval(text),
                        span,
                        text: source[tokens[i].span.start..tokens[close].span.end].to_string(),
                    });
                    i = close + 1;
                } else {
                    result.push(tokens[i].clone());
                    i += 1;
                }
            }
            TokenKind::RBracket => {
                result.push(tokens[i].clone());
                i += 1;
            }
            _ => {
                result.push(tokens[i].clone());
                i += 1;
            }
        }
    }

    result
}

pub(super) fn strip_comments(tokens: Vec<Token>) -> (Vec<Token>, Vec<Comment>) {
    let mut filtered = Vec::with_capacity(tokens.len());
    let mut comments = Vec::new();
    for token in tokens {
        match &token.kind {
            TokenKind::LineComment(text) | TokenKind::BlockComment(text) => {
                comments.push(Comment {
                    text: text.clone(),
                    span: token.span,
                });
            }
            _ => filtered.push(token),
        }
    }
    (filtered, comments)
}

pub(super) fn preprocess_source(source_text: &str) -> String {
    let mut out = Vec::new();
    let mut data_block_depth = 0usize;
    let mut skipped_nested_block_depth = 0usize;

    for raw_line in source_text.lines() {
        let mut line = raw_line.to_string();
        let mut trimmed = line.trim();

        if skipped_nested_block_depth > 0 {
            let opens = trimmed.chars().filter(|ch| *ch == '{').count();
            let closes = trimmed.chars().filter(|ch| *ch == '}').count();
            if opens >= closes {
                skipped_nested_block_depth += opens - closes;
            } else {
                skipped_nested_block_depth =
                    skipped_nested_block_depth.saturating_sub(closes - opens);
            }
            continue;
        }

        if trimmed.starts_with("nocross data {") {
            let indent = raw_line
                .chars()
                .take_while(|ch| ch.is_ascii_whitespace())
                .collect::<String>();
            line = format!("{indent}data {{");
            trimmed = line.trim();
        } else if data_block_depth == 0 && trimmed == "nocross {" {
            let indent = raw_line
                .chars()
                .take_while(|ch| ch.is_ascii_whitespace())
                .collect::<String>();
            line = format!("{indent}{{");
            trimmed = line.trim();
        }

        if data_block_depth > 0 && trimmed == "nocross {" {
            skipped_nested_block_depth = 1;
            continue;
        }

        if trimmed.starts_with("var ") {
            let indent = raw_line
                .chars()
                .take_while(|ch| ch.is_ascii_whitespace())
                .collect::<String>();
            let mut payload = trimmed.trim_start_matches("var ").trim().to_string();
            if payload.ends_with('?') {
                payload.pop();
                payload = payload.trim_end().to_string();
                line = format!("{indent}var {payload}");
                trimmed = line.trim();
            }
            if payload.contains(',') {
                for part in payload.split(',') {
                    let part = part.trim();
                    if !part.is_empty() {
                        out.push(format!("{indent}var {part}"));
                    }
                }
                continue;
            }
        }
        if trimmed.starts_with("data ") && trimmed.ends_with('{') {
            data_block_depth += 1;
        } else if data_block_depth > 0 && trimmed == "}" {
            data_block_depth -= 1;
        }

        out.push(line);
    }

    let mut text = out.join("\n");
    if source_text.ends_with('\n') {
        text.push('\n');
    }
    text
}
