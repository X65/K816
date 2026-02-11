use logos::Logos;

use crate::diag::Diagnostic;
use crate::span::{SourceId, Span};

#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip(r"[ \t\r\f]+"))]
#[logos(skip(r"//[^\r\n]*", allow_greedy = true))]
#[logos(skip(r"/\*([^*]|\*+[^*/])*\*+/"))]
pub enum TokenKind {
    #[token("segment")]
    Segment,
    #[token("var")]
    Var,
    #[token("func")]
    Func,
    #[token("main")]
    Main,
    #[token("naked")]
    Naked,
    #[token("inline")]
    Inline,
    #[token("far")]
    Far,
    #[token("data")]
    Data,
    #[token("align")]
    Align,
    #[token("address")]
    Address,
    #[token("nocross")]
    Nocross,
    #[token("call")]
    Call,

    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token(";")]
    Semi,
    #[token("++")]
    PlusPlus,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("%")]
    Percent,
    #[token("&")]
    Amp,
    #[token("|")]
    Pipe,
    #[token("^")]
    Caret,
    #[token("!")]
    Bang,
    #[token("?")]
    Question,
    #[token("==")]
    EqEq,
    #[token("!=")]
    BangEq,
    #[token("<=")]
    LtEq,
    #[token(">=")]
    GtEq,
    #[token("<")]
    Lt,
    #[token(">")]
    Gt,
    #[token("#")]
    Hash,
    #[token("=")]
    Eq,

    #[regex(r"\n+")]
    Newline,

    #[regex(r"\[[^\]]*\]", parse_eval)]
    Eval(String),

    #[regex(r#""([^"\\]|\\.)*""#, parse_string)]
    String(String),

    #[regex(r"%[01]+|0b[01]+|0x[0-9a-fA-F]+|\$[0-9a-fA-F]+|[0-9]+", parse_number)]
    Number(i64),

    #[regex(r"[A-Za-z_.][A-Za-z0-9_.]*", parse_ident)]
    Ident(String),
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
    pub text: String,
}

pub fn lex(source_id: SourceId, input: &str) -> Result<Vec<Token>, Vec<Diagnostic>> {
    let mut lexer = TokenKind::lexer(input);
    let mut tokens = Vec::new();
    let mut diagnostics = Vec::new();

    while let Some(next) = lexer.next() {
        let range = lexer.span();
        let span = Span::new(source_id, range.start, range.end);
        match next {
            Ok(kind) => {
                tokens.push(Token {
                    kind,
                    span,
                    text: lexer.slice().to_string(),
                });
            }
            Err(_) => {
                let token = format_token_for_message(lexer.slice());
                diagnostics.push(
                    Diagnostic::error(span, format!("unexpected token {token}"))
                        .with_help("remove or fix this token"),
                );
            }
        }
    }

    if diagnostics.is_empty() {
        Ok(tokens)
    } else {
        Err(diagnostics)
    }
}

fn parse_number(lex: &mut logos::Lexer<TokenKind>) -> Option<i64> {
    let slice = lex.slice();
    if let Some(bin) = slice.strip_prefix('%') {
        return i64::from_str_radix(bin, 2).ok();
    }
    if let Some(bin) = slice.strip_prefix("0b") {
        return i64::from_str_radix(bin, 2).ok();
    }
    if let Some(hex) = slice.strip_prefix("0x") {
        return i64::from_str_radix(hex, 16).ok();
    }
    if let Some(hex) = slice.strip_prefix('$') {
        return i64::from_str_radix(hex, 16).ok();
    }
    slice.parse::<i64>().ok()
}

fn parse_ident(lex: &mut logos::Lexer<TokenKind>) -> String {
    lex.slice().to_string()
}

fn parse_eval(lex: &mut logos::Lexer<TokenKind>) -> String {
    let slice = lex.slice();
    slice[1..slice.len() - 1].trim().to_string()
}

fn parse_string(lex: &mut logos::Lexer<TokenKind>) -> String {
    let slice = lex.slice();
    let content = &slice[1..slice.len() - 1];
    let mut out = String::with_capacity(content.len());
    let mut chars = content.chars().peekable();
    while let Some(ch) = chars.next() {
        if ch == '\\' {
            if let Some(escaped) = chars.next() {
                let resolved = match escaped {
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    '\\' => '\\',
                    '"' => '"',
                    other => other,
                };
                out.push(resolved);
            }
        } else {
            out.push(ch);
        }
    }
    out
}

fn format_token_for_message(token: &str) -> String {
    let escaped: String = token.chars().flat_map(char::escape_default).collect();
    format!("'{escaped}'")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lexes_eval_fragment() {
        let tokens = lex(SourceId(0), "lda #[1 + 2]").expect("lex");
        assert!(
            tokens
                .iter()
                .any(|token| matches!(token.kind, TokenKind::Eval(_)))
        );
    }

    #[test]
    fn reports_unrecognized_token_text() {
        let diagnostics = lex(SourceId(0), "@").expect_err("expected lex error");
        assert_eq!(diagnostics.len(), 1);
        assert_eq!(diagnostics[0].message, "unexpected token '@'");
    }

    #[test]
    fn lexes_percent_prefixed_binary_literal() {
        let tokens = lex(SourceId(0), "%01001010").expect("lex");
        assert_eq!(tokens.len(), 1);
        assert!(matches!(tokens[0].kind, TokenKind::Number(0x4A)));
    }
}
