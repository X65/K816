use logos::Logos;

use crate::ast::NumFmt;
use crate::diag::Diagnostic;
use crate::span::{SourceId, Span};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct NumLit {
    pub value: i64,
    pub fmt: NumFmt,
}

#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip(r"[ \t\r\f]+"))]
pub enum TokenKind {
    #[regex(r"//[^\r\n]*", |lex| lex.slice().to_string(), priority = 5, allow_greedy = true)]
    LineComment(String),
    #[regex(r"/\*([^*]|\*+[^*/])*\*+/", |lex| lex.slice().to_string(), priority = 5)]
    BlockComment(String),

    #[token("segment")]
    Segment,
    #[token("const")]
    Const,
    #[token("var")]
    Var,
    #[token("func")]
    Func,
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

    #[token("@a8")]
    ModeA8,
    #[token("@a16")]
    ModeA16,
    #[token("@i8")]
    ModeI8,
    #[token("@i16")]
    ModeI16,

    #[token("><")]
    SwapOp,

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
    #[token("..")]
    DotDot,
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
    #[token("<<=")]
    LtLtEq,
    #[token(">>=")]
    GtGtEq,
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

    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,

    /// Bracket-delimited text produced by the post-lex coalescing pass
    /// (not matched directly by logos).
    Eval(String),

    #[regex(r#""([^"\\]|\\.)*""#, parse_string)]
    String(String),

    #[regex(r"%[01]+|0b[01]+|0x[0-9a-fA-F]+|\$[0-9a-fA-F]+|[0-9]+", parse_number)]
    #[regex(r"'([^'\\]|\\.)'", parse_char)]
    Number(NumLit),

    #[regex(
        r"(?:[A-Za-z_]|[.][A-Za-z_])[A-Za-z0-9_]*(?:(?:[.][A-Za-z_]|::[A-Za-z_])[A-Za-z0-9_]*)*",
        parse_ident
    )]
    Ident(String),
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
    pub text: String,
}

pub fn lex(source_id: SourceId, input: &str) -> Result<Vec<Token>, Vec<Diagnostic>> {
    let (tokens, diagnostics) = lex_lenient(source_id, input);
    if diagnostics.is_empty() {
        Ok(tokens)
    } else {
        Err(diagnostics)
    }
}

pub fn lex_lenient(source_id: SourceId, input: &str) -> (Vec<Token>, Vec<Diagnostic>) {
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

    (tokens, diagnostics)
}

fn parse_number(lex: &mut logos::Lexer<TokenKind>) -> Option<NumLit> {
    let slice = lex.slice();
    if let Some(bin) = slice.strip_prefix('%') {
        let w = bin.len() as u8;
        return i64::from_str_radix(bin, 2).ok().map(|v| NumLit {
            value: v,
            fmt: NumFmt::Percent(w),
        });
    }
    if let Some(bin) = slice.strip_prefix("0b") {
        let w = bin.len() as u8;
        return i64::from_str_radix(bin, 2).ok().map(|v| NumLit {
            value: v,
            fmt: NumFmt::Bin(w),
        });
    }
    if let Some(hex) = slice.strip_prefix("0x") {
        let w = hex.len() as u8;
        return i64::from_str_radix(hex, 16).ok().map(|v| NumLit {
            value: v,
            fmt: NumFmt::Hex(w),
        });
    }
    if let Some(hex) = slice.strip_prefix('$') {
        let w = hex.len() as u8;
        return i64::from_str_radix(hex, 16).ok().map(|v| NumLit {
            value: v,
            fmt: NumFmt::Dollar(w),
        });
    }
    if slice.len() > 1 && slice.bytes().all(|b| b == b'0') {
        return Some(NumLit {
            value: 0,
            fmt: NumFmt::Zero(slice.len() as u8),
        });
    }
    slice.parse::<i64>().ok().map(|v| NumLit {
        value: v,
        fmt: NumFmt::Dec,
    })
}

fn parse_ident(lex: &mut logos::Lexer<TokenKind>) -> String {
    lex.slice().to_string()
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

fn parse_char(lex: &mut logos::Lexer<TokenKind>) -> Option<NumLit> {
    let slice = lex.slice();
    let content = &slice[1..slice.len() - 1];
    let mut chars = content.chars();
    let ch = match chars.next()? {
        '\\' => match chars.next()? {
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            '\\' => '\\',
            '\'' => '\'',
            '0' => '\0',
            other => other,
        },
        ch => ch,
    };
    Some(NumLit {
        value: ch as i64,
        fmt: NumFmt::Char,
    })
}

fn format_token_for_message(token: &str) -> String {
    let escaped: String = token.chars().flat_map(char::escape_default).collect();
    format!("'{escaped}'")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lexes_bracket_tokens() {
        let tokens = lex(SourceId(0), "lda #[1 + 2]").expect("lex");
        assert!(
            tokens
                .iter()
                .any(|token| matches!(token.kind, TokenKind::LBracket))
        );
        assert!(
            tokens
                .iter()
                .any(|token| matches!(token.kind, TokenKind::RBracket))
        );
    }

    #[test]
    fn lexes_nested_brackets() {
        let tokens = lex(SourceId(0), "var mmio[.data[4]:byte] = 0x4000").expect("lex");
        let bracket_count = tokens
            .iter()
            .filter(|token| matches!(token.kind, TokenKind::LBracket))
            .count();
        assert_eq!(
            bracket_count, 2,
            "expected 2 LBracket tokens (outer + inner)"
        );
    }

    #[test]
    fn lexes_range_operator_in_data_for_eval() {
        let tokens = lex(SourceId(0), "for i=0..4 eval [i]").expect("lex");
        assert!(
            tokens
                .iter()
                .any(|token| matches!(token.kind, TokenKind::DotDot))
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
        assert!(matches!(
            tokens[0].kind,
            TokenKind::Number(NumLit {
                value: 0x4A,
                fmt: NumFmt::Percent(8)
            })
        ));
    }

    #[test]
    fn lexes_char_literal() {
        let tokens = lex(SourceId(0), "'A'").expect("lex");
        assert_eq!(tokens.len(), 1);
        assert!(matches!(
            tokens[0].kind,
            TokenKind::Number(NumLit {
                value: 65,
                fmt: NumFmt::Char
            })
        ));
    }

    #[test]
    fn lexes_char_literal_escape() {
        let tokens = lex(SourceId(0), "'\\n'").expect("lex");
        assert_eq!(tokens.len(), 1);
        assert!(matches!(
            tokens[0].kind,
            TokenKind::Number(NumLit {
                value: 10,
                fmt: NumFmt::Char
            })
        ));
    }

    #[test]
    fn lexes_char_literal_as_immediate() {
        let tokens = lex(SourceId(0), "lda #'s'").expect("lex");
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::Hash)));
        assert!(
            tokens
                .iter()
                .any(|t| matches!(t.kind, TokenKind::Number(NumLit { value: 115, .. })))
        );
    }

    #[test]
    fn lexes_zero_runs_as_zero_format() {
        let tokens = lex(SourceId(0), "00 000").expect("lex");
        assert!(matches!(
            tokens[0].kind,
            TokenKind::Number(NumLit {
                value: 0,
                fmt: NumFmt::Zero(2)
            })
        ));
        assert!(matches!(
            tokens[1].kind,
            TokenKind::Number(NumLit {
                value: 0,
                fmt: NumFmt::Zero(3)
            })
        ));
    }
}
