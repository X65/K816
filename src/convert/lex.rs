/*****************************************************************************/
/*                                                                           */
/*                                   lex.rs                                  */
/*                                                                           */
/*                   The tokens of one line of ca65 source                   */
/*                                                                           */
/*                                                                           */
/* Copyright (c) 2026 Tomasz Sterna (smokku)                                 */
/*                                                                           */
/* Permission to use, copy, modify, and/or distribute this software for any  */
/* purpose with or without fee is hereby granted.                            */
/*                                                                           */
/* THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES  */
/* WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF          */
/* MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR   */
/* ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES    */
/* WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN     */
/* ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF   */
/* OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.            */
/*                                                                           */
/*****************************************************************************/

//! One line of ca65 source as tokens, and the comment after them.
//!
//! ca65 is line-oriented, and so is everything in this module: a token never
//! spans a line, and the comment is the rest of the line after the first `;`
//! outside a string or a character literal. Each token keeps its byte range
//! and whether whitespace came before it, which is what lets an expression be
//! written back with the spacing its author chose.

/// What a token is.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Tok {
    /// An identifier, as written.
    Ident(String),
    /// A cheap local, `@name`, without the `@`.
    Cheap(String),
    /// A control command, `.name`, lowercased and without the `.`.
    Directive(String),
    /// A number, with the text that spelled it.
    Number { value: u32, text: String },
    /// A character literal, with its quotes.
    Char { value: u32, text: String },
    /// A string literal: the text between the quotes as written, and the
    /// bytes it stands for.
    Str { text: String, bytes: Vec<u8> },
    /// An operator or punctuation.
    Punct(&'static str),
}

/// One token and where it sits on its line.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Token {
    pub tok: Tok,
    pub start: usize,
    pub end: usize,
    /// Whitespace immediately before the token.
    pub space_before: bool,
}

/// The tokens of one line, and its comment.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Line {
    pub tokens: Vec<Token>,
    /// The column the `;` is in, and the text after it.
    pub comment: Option<(usize, String)>,
}

/// Every punctuation spelling, longest first so that a prefix never wins.
const PUNCTUATION: [&str; 31] = [
    "::", ":=", "&&", "||", "<<", ">>", "<=", ">=", "<>", "==", ":", ",", "#", "(", ")", "[", "]",
    "{", "}", "+", "-", "*", "/", "&", "|", "^", "~", "!", "<", ">", "=",
];

/// Splits `text` into tokens and a comment.
///
/// `escapes` is `.feature string_escapes`: without it a backslash is an
/// ordinary character, as it is in ca65.
pub fn lex(text: &str, escapes: bool) -> Result<Line, String> {
    let bytes = text.as_bytes();
    let mut line = Line::default();
    let mut at = 0;
    let mut space = true;
    while at < bytes.len() {
        let c = bytes[at];
        if c == b' ' || c == b'\t' || c == b'\r' {
            at += 1;
            space = true;
            continue;
        }
        if c == b';' {
            line.comment = Some((column(text, at), text[at + 1..].to_owned()));
            break;
        }
        let start = at;
        let tok = if c == b'"' {
            let (text, bytes, end) = string(text, at, escapes)?;
            at = end;
            Tok::Str { text, bytes }
        } else if c == b'\'' {
            let (value, end) = character(text, at, escapes)?;
            at = end;
            Tok::Char {
                value,
                text: text[start..at].to_owned(),
            }
        } else if c == b'$' && bytes.get(at + 1).is_some_and(u8::is_ascii_hexdigit) {
            at += 1;
            while bytes.get(at).is_some_and(u8::is_ascii_hexdigit) {
                at += 1;
            }
            number(&text[start..at], &text[start + 1..at], 16)?
        } else if c == b'%' && bytes.get(at + 1).is_some_and(|b| *b == b'0' || *b == b'1') {
            at += 1;
            while bytes.get(at).is_some_and(|b| *b == b'0' || *b == b'1') {
                at += 1;
            }
            number(&text[start..at], &text[start + 1..at], 2)?
        } else if c.is_ascii_digit() {
            while bytes.get(at).is_some_and(u8::is_ascii_alphanumeric) {
                at += 1;
            }
            let spelled = &text[start..at];
            if let Some(hex) = spelled
                .strip_prefix("0x")
                .or_else(|| spelled.strip_prefix("0X"))
            {
                number(spelled, hex, 16)?
            } else {
                number(spelled, spelled, 10)?
            }
        } else if c == b'@' || c == b'.' || c == b'_' || c.is_ascii_alphabetic() {
            let sigil = c == b'@' || c == b'.';
            at += usize::from(sigil);
            let name_start = at;
            while bytes
                .get(at)
                .is_some_and(|b| *b == b'_' || b.is_ascii_alphanumeric())
            {
                at += 1;
            }
            let name = &text[name_start..at];
            if name.is_empty() {
                if c == b'.' {
                    return Err(format!(
                        "a `.` at column {} names nothing",
                        column(text, start)
                    ));
                }
                return Err(format!(
                    "an `@` at column {} names nothing",
                    column(text, start)
                ));
            }
            match c {
                b'@' => Tok::Cheap(name.to_owned()),
                b'.' => Tok::Directive(name.to_ascii_lowercase()),
                _ => Tok::Ident(name.to_owned()),
            }
        } else if let Some(punct) = PUNCTUATION
            .iter()
            .find(|punct| text[at..].starts_with(**punct))
        {
            at += punct.len();
            Tok::Punct(punct)
        } else {
            return Err(format!(
                "`{}` at column {} is not ca65 syntax this converter reads",
                char::from(c),
                column(text, start)
            ));
        };
        line.tokens.push(Token {
            tok,
            start,
            end: at,
            space_before: space,
        });
        space = false;
    }
    Ok(line)
}

/// The visual column of byte `at`, with tabs at every eighth column — which
/// is how ca65 sources are laid out and how the comment column is kept.
pub fn column(text: &str, at: usize) -> usize {
    let mut column = 0;
    for c in text[..at].chars() {
        if c == '\t' {
            column = (column / 8 + 1) * 8;
        } else {
            column += 1;
        }
    }
    column
}

fn number(spelled: &str, digits: &str, radix: u32) -> Result<Tok, String> {
    let value = u32::from_str_radix(digits, radix)
        .map_err(|_| format!("`{spelled}` is not a number this converter reads"))?;
    Ok(Tok::Number {
        value,
        text: spelled.to_owned(),
    })
}

/// A string literal starting at `at`: its text, its bytes, and where it ends.
fn string(text: &str, at: usize, escapes: bool) -> Result<(String, Vec<u8>, usize), String> {
    let bytes = text.as_bytes();
    let mut end = at + 1;
    let mut decoded = Vec::new();
    while end < bytes.len() && bytes[end] != b'"' {
        if escapes && bytes[end] == b'\\' {
            let (byte, next) = escape(text, end)?;
            decoded.push(byte);
            end = next;
        } else {
            decoded.push(bytes[end]);
            end += 1;
        }
    }
    if end >= bytes.len() {
        return Err(format!(
            "the string at column {} does not end on its line",
            column(text, at)
        ));
    }
    Ok((text[at + 1..end].to_owned(), decoded, end + 1))
}

/// A character literal starting at `at`: its value, and where it ends.
fn character(text: &str, at: usize, escapes: bool) -> Result<(u32, usize), String> {
    let bytes = text.as_bytes();
    let (value, next) = match bytes.get(at + 1) {
        Some(b'\\') if escapes => {
            let (byte, next) = escape(text, at + 1)?;
            (u32::from(byte), next)
        }
        Some(byte) => (u32::from(*byte), at + 2),
        None => return Err("a `'` ends the line".to_owned()),
    };
    // ca65 accepts a character constant without its closing quote, which
    // `'a` is in some sources; the closing one is taken when it is there.
    let end = if bytes.get(next) == Some(&b'\'') {
        next + 1
    } else {
        next
    };
    Ok((value, end))
}

/// One backslash escape at `at`, under `.feature string_escapes`.
fn escape(text: &str, at: usize) -> Result<(u8, usize), String> {
    let bytes = text.as_bytes();
    let byte = match bytes.get(at + 1) {
        Some(b'n') => b'\n',
        Some(b'r') => b'\r',
        Some(b't') => b'\t',
        Some(b'0') => 0,
        Some(b'\\') => b'\\',
        Some(b'"') => b'"',
        Some(b'\'') => b'\'',
        Some(b'x') => {
            let digits = text
                .get(at + 2..at + 4)
                .ok_or("a `\\x` escape needs two hex digits")?;
            let value = u8::from_str_radix(digits, 16)
                .map_err(|_| format!("`\\x{digits}` is not a byte"))?;
            return Ok((value, at + 4));
        }
        _ => {
            return Err(format!(
                "an escape at column {} this converter does not read",
                column(text, at)
            ));
        }
    };
    Ok((byte, at + 2))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn kinds(text: &str) -> Vec<Tok> {
        lex(text, true)
            .expect("lexes")
            .tokens
            .into_iter()
            .map(|token| token.tok)
            .collect()
    }

    #[test]
    fn a_line_is_tokens_and_the_comment_after_them() {
        let line = lex("loop:   lda #$FF        ; load ; twice", false).expect("lexes");
        assert_eq!(line.comment, Some((24, " load ; twice".to_owned())));
        assert_eq!(
            line.tokens
                .iter()
                .map(|token| &token.tok)
                .collect::<Vec<_>>(),
            vec![
                &Tok::Ident("loop".to_owned()),
                &Tok::Punct(":"),
                &Tok::Ident("lda".to_owned()),
                &Tok::Punct("#"),
                &Tok::Number {
                    value: 0xFF,
                    text: "$FF".to_owned()
                },
            ]
        );
    }

    #[test]
    fn sigils_and_scopes_lex_as_ca65_spells_them() {
        assert_eq!(
            kinds("@x: .A16 RIA::op <> %0101"),
            vec![
                Tok::Cheap("x".to_owned()),
                Tok::Punct(":"),
                Tok::Directive("a16".to_owned()),
                Tok::Ident("RIA".to_owned()),
                Tok::Punct("::"),
                Tok::Ident("op".to_owned()),
                Tok::Punct("<>"),
                Tok::Number {
                    value: 5,
                    text: "%0101".to_owned()
                },
            ]
        );
    }

    #[test]
    fn a_semicolon_in_a_string_is_not_a_comment() {
        let line = lex(r#".byte "a;b", ';', 1"#, true).expect("lexes");
        assert_eq!(line.comment, None);
        assert_eq!(
            line.tokens[1].tok,
            Tok::Str {
                text: "a;b".to_owned(),
                bytes: b"a;b".to_vec()
            }
        );
        assert_eq!(
            line.tokens[3].tok,
            Tok::Char {
                value: u32::from(b';'),
                text: "';'".to_owned()
            }
        );
    }

    #[test]
    fn escapes_are_read_only_under_the_feature() {
        assert_eq!(
            kinds(r#""\r\n""#),
            vec![Tok::Str {
                text: r"\r\n".to_owned(),
                bytes: vec![13, 10]
            }]
        );
        let plain = lex(r#""\r""#, false).expect("lexes");
        assert_eq!(
            plain.tokens[0].tok,
            Tok::Str {
                text: r"\r".to_owned(),
                bytes: b"\\r".to_vec()
            }
        );
    }

    #[test]
    fn the_comment_column_counts_tabs_as_ca65_lays_them_out() {
        let line = lex("\tnop\t\t; x", false).expect("lexes");
        assert_eq!(line.comment.map(|(column, _)| column), Some(24));
    }
}
