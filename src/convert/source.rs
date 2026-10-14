/*****************************************************************************/
/*                                                                           */
/*                                 source.rs                                 */
/*                                                                           */
/*              A ca65 file as lines: labels, a body, a comment              */
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

//! What each line of a ca65 file says, before anything decides what it means.
//!
//! A line is zero or more label definitions, at most one body, and a comment.
//! The body is an instruction, a control command, an assignment, or a name
//! this module does not know — a macro invocation, or, inside `.struct` and
//! `.enum`, a member. Which of those a name is depends on context the line
//! does not carry, so the reading is left to the caller.

use super::lex::{self, Line as Lexed, Tok, Token};

/// Every 65816 mnemonic ca65 accepts, lowercased, plus the two 65C02
/// spellings it keeps for `inc a` and `dec a`.
pub const MNEMONICS: [&str; 94] = [
    "adc", "and", "asl", "bcc", "bcs", "beq", "bit", "bmi", "bne", "bpl", "bra", "brk", "brl",
    "bvc", "bvs", "clc", "cld", "cli", "clv", "cmp", "cop", "cpx", "cpy", "dea", "dec", "dex",
    "dey", "eor", "ina", "inc", "inx", "iny", "jml", "jmp", "jsl", "jsr", "lda", "ldx", "ldy",
    "lsr", "mvn", "mvp", "nop", "ora", "pea", "pei", "per", "pha", "phb", "phd", "phk", "php",
    "phx", "phy", "pla", "plb", "pld", "plp", "plx", "ply", "rep", "rol", "ror", "rti", "rtl",
    "rts", "sbc", "sec", "sed", "sei", "sep", "sta", "stp", "stx", "sty", "stz", "tax", "tay",
    "tcd", "tcs", "tdc", "trb", "tsb", "tsc", "tsx", "txa", "txs", "txy", "tya", "tyx", "wai",
    "wdm", "xba", "xce",
];

/// One label a line defines.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Label {
    Named(String),
    Cheap(String),
    Unnamed,
}

/// What a line does, if anything.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Body {
    /// A mnemonic, lowercased, and the tokens of its operand.
    Instruction {
        mnemonic: String,
        operand: Vec<Token>,
    },
    /// `.name args`, lowercased.
    Directive { name: String, args: Vec<Token> },
    /// `name = value` and `name := value`.
    Assign {
        name: String,
        value: Vec<Token>,
        label: bool,
    },
    /// A name that is not a mnemonic, and whatever follows it: a macro
    /// invocation, or a `.struct`/`.enum` member.
    Name { name: String, rest: Vec<Token> },
}

/// One line of a ca65 file.
#[derive(Clone, Debug)]
pub struct Line {
    /// 1-based.
    pub number: usize,
    pub text: String,
    /// The visual column the first token starts in.
    pub indent: usize,
    pub labels: Vec<Label>,
    pub body: Option<Body>,
    /// The column of the `;` and the text after it.
    pub comment: Option<(usize, String)>,
    /// The column the body starts in, for re-aligning a trailing comment.
    pub body_column: usize,
}

impl Line {
    /// True for a line with nothing on it but, perhaps, a comment.
    pub fn is_trivia(&self) -> bool {
        self.labels.is_empty() && self.body.is_none()
    }

    /// The directive this line is, if it is one.
    pub fn directive(&self) -> Option<(&str, &[Token])> {
        match &self.body {
            Some(Body::Directive { name, args }) => Some((name, args)),
            _ => None,
        }
    }
}

/// Reads one line.
pub fn read(number: usize, text: &str, escapes: bool) -> Result<Line, String> {
    let Lexed { tokens, comment } = lex::lex(text, escapes)?;
    let indent = tokens
        .first()
        .map_or_else(|| text.len() - text.trim_start().len(), |token| token.start);
    let mut line = Line {
        number,
        text: text.to_owned(),
        indent: lex::column(text, indent.min(text.len())),
        labels: Vec::new(),
        body: None,
        comment,
        body_column: 0,
    };
    let mut at = 0;
    // Labels: `name:`, `@name:` and `:`, as many as the line has.
    loop {
        // A label's colon is the next character after its name: `beq :+`
        // is a branch, and `loop:` a label.
        let attached = tokens.get(at + 1).is_some_and(|token| !token.space_before);
        match (
            tokens.get(at).map(|token| &token.tok),
            tokens.get(at + 1).map(|token| &token.tok),
        ) {
            (Some(Tok::Ident(name)), Some(Tok::Punct(":"))) if attached => {
                line.labels.push(Label::Named(name.clone()));
                at += 2;
            }
            (Some(Tok::Cheap(name)), Some(Tok::Punct(":"))) if attached => {
                line.labels.push(Label::Cheap(name.clone()));
                at += 2;
            }
            (Some(Tok::Punct(":")), next)
                if !matches!(next, Some(Tok::Punct("+" | "-")))
                    || tokens.get(at + 1).is_some_and(|token| token.space_before) =>
            {
                line.labels.push(Label::Unnamed);
                at += 1;
            }
            _ => break,
        }
    }
    let Some(first) = tokens.get(at) else {
        return Ok(line);
    };
    line.body_column = lex::column(text, first.start);
    let rest = tokens[at + 1..].to_vec();
    line.body = Some(match &first.tok {
        Tok::Directive(name) => Body::Directive {
            name: name.clone(),
            args: rest,
        },
        Tok::Ident(name) => match rest.first().map(|token| &token.tok) {
            Some(Tok::Punct("=")) => Body::Assign {
                name: name.clone(),
                value: rest[1..].to_vec(),
                label: false,
            },
            Some(Tok::Punct(":=")) => Body::Assign {
                name: name.clone(),
                value: rest[1..].to_vec(),
                label: true,
            },
            _ if is_mnemonic(name) => Body::Instruction {
                mnemonic: name.to_ascii_lowercase(),
                operand: rest,
            },
            _ => Body::Name {
                name: name.clone(),
                rest,
            },
        },
        other => {
            return Err(format!(
                "a line cannot start with `{}`",
                super::expr::describe(other)
            ));
        }
    });
    Ok(line)
}

/// True for a 65816 mnemonic, in any case.
pub fn is_mnemonic(name: &str) -> bool {
    MNEMONICS.contains(&name.to_ascii_lowercase().as_str())
}

/// Splits a token list at top-level commas — commas outside parentheses and
/// brackets — which is how ca65 separates arguments.
pub fn split_commas(tokens: &[Token]) -> Vec<&[Token]> {
    let mut parts = Vec::new();
    let mut depth = 0i32;
    let mut start = 0;
    for (index, token) in tokens.iter().enumerate() {
        match token.tok {
            Tok::Punct("(" | "[" | "{") => depth += 1,
            Tok::Punct(")" | "]" | "}") => depth -= 1,
            Tok::Punct(",") if depth == 0 => {
                parts.push(&tokens[start..index]);
                start = index + 1;
            }
            _ => {}
        }
    }
    if start < tokens.len() || !parts.is_empty() {
        parts.push(&tokens[start..]);
    }
    parts
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_line_is_labels_a_body_and_a_comment() {
        let line = read(7, "loop:   lda #1  ; one", false).expect("reads");
        assert_eq!(line.labels, vec![Label::Named("loop".to_owned())]);
        assert!(matches!(
            &line.body,
            Some(Body::Instruction { mnemonic, .. }) if mnemonic == "lda"
        ));
        assert_eq!(line.body_column, 8);
        assert_eq!(line.comment, Some((16, " one".to_owned())));
    }

    #[test]
    fn an_unnamed_label_is_a_colon_and_a_reference_is_not() {
        let line = read(1, ":       inc A", false).expect("reads");
        assert_eq!(line.labels, vec![Label::Unnamed]);
        let line = read(1, "        bne :-", false).expect("reads");
        assert!(line.labels.is_empty());
    }

    #[test]
    fn a_branch_to_an_unnamed_label_is_not_a_label() {
        let line = read(1, "        beq :+", false).expect("reads");
        assert!(line.labels.is_empty());
        assert!(matches!(line.body, Some(Body::Instruction { .. })));
    }

    #[test]
    fn an_address_size_prefix_is_not_a_label() {
        let line = read(1, "        lda a:$0000,x", false).expect("reads");
        assert!(line.labels.is_empty());
        assert!(matches!(line.body, Some(Body::Instruction { .. })));
    }

    #[test]
    fn assignments_and_members_are_told_apart_from_instructions() {
        assert!(matches!(
            read(1, "reg_y    =1", false).expect("reads").body,
            Some(Body::Assign { label: false, .. })
        ));
        assert!(matches!(
            read(1, "    tm          .byte 6", false)
                .expect("reads")
                .body,
            Some(Body::Name { .. })
        ));
    }
}
