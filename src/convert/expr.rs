/*****************************************************************************/
/*                                                                           */
/*                                  expr.rs                                  */
/*                                                                           */
/*            ca65 expressions: the tree, the parser, and folding            */
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

//! A ca65 expression as a tree.
//!
//! The precedence table is ca65's own (`doc/ca65.sgml`, "Expressions"), and
//! it is not K816's: ca65 puts `&`, `^`, `<<` and `>>` with `*`, and `|` with
//! `+`, and its unary `<`, `>` and `^` bind tighter than anything binary — so
//! `<label+1` is the low byte of `label`, plus one. Parsing into a tree and
//! writing the tree back out under K816's rules is how a conversion keeps the
//! value; copying the tokens would keep the spelling and change the number.

use super::lex::{Tok, Token};

/// A unary operator.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Unary {
    Plus,
    Negate,
    BitNot,
    LogicalNot,
    /// `<` and `.lobyte`.
    LowByte,
    /// `>` and `.hibyte`.
    HighByte,
    /// `^` and `.bankbyte`.
    BankByte,
    /// `.loword`.
    LowWord,
    /// `.hiword`.
    HighWord,
}

/// A binary operator.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Binary {
    Mul,
    Div,
    Mod,
    BitAnd,
    BitXor,
    Shl,
    Shr,
    Add,
    Sub,
    BitOr,
    Equal,
    NotEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    And,
    Xor,
    Or,
}

impl Binary {
    /// ca65's precedence level; a larger number binds tighter.
    pub fn level(self) -> u8 {
        match self {
            Self::Mul
            | Self::Div
            | Self::Mod
            | Self::BitAnd
            | Self::BitXor
            | Self::Shl
            | Self::Shr => 5,
            Self::Add | Self::Sub | Self::BitOr => 4,
            Self::Equal
            | Self::NotEqual
            | Self::Less
            | Self::Greater
            | Self::LessEqual
            | Self::GreaterEqual => 3,
            Self::And | Self::Xor => 2,
            Self::Or => 1,
        }
    }
}

/// An expression.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expr {
    Number {
        value: u32,
        text: String,
    },
    Char {
        value: u32,
        text: String,
    },
    /// A string, which only a data directive takes.
    Str {
        text: String,
        bytes: Vec<u8>,
    },
    /// A name, scoped with `::` when the path has more than one part:
    /// `RIA::op` is `["RIA", "op"]`.
    Symbol(Vec<String>),
    /// `@name`.
    Cheap(String),
    /// `:+` is `1`, `:-` is `-1`, `:++` is `2`.
    Unnamed(i32),
    /// `*`, the program counter.
    Here,
    Unary(Unary, Box<Expr>),
    Binary {
        op: Binary,
        left: Box<Expr>,
        right: Box<Expr>,
        /// Whitespace around the operator in the source.
        spaced: bool,
    },
    Paren(Box<Expr>),
    /// A pseudo-function the tree keeps by name: `.sizeof`, `.defined`,
    /// `.strlen`, and every one this converter does not fold.
    Function {
        name: String,
        arguments: Vec<Expr>,
    },
}

impl Expr {
    /// The plain name this expression is, when it is exactly one.
    pub fn name(&self) -> Option<&str> {
        match self {
            Self::Symbol(path) if path.len() == 1 => Some(&path[0]),
            _ => None,
        }
    }

    /// Every symbol path the expression mentions, in tree order.
    pub fn symbols<'a>(&'a self, out: &mut Vec<&'a [String]>) {
        match self {
            Self::Symbol(path) => out.push(path),
            Self::Unary(_, operand) | Self::Paren(operand) => operand.symbols(out),
            Self::Binary { left, right, .. } => {
                left.symbols(out);
                right.symbols(out);
            }
            Self::Function { arguments, .. } => {
                for argument in arguments {
                    argument.symbols(out);
                }
            }
            Self::Number { .. }
            | Self::Char { .. }
            | Self::Str { .. }
            | Self::Cheap(_)
            | Self::Unnamed(_)
            | Self::Here => {}
        }
    }

    /// Folds to a number, asking `lookup` for each symbol path.
    ///
    /// `None` when a symbol has no value `lookup` will give, or an operation
    /// has no answer — division by zero, a comparison under K816, which has
    /// none. ca65's arithmetic is 32-bit and its comparisons are 0 or 1.
    pub fn fold(&self, lookup: &dyn Fn(&[String]) -> Option<i64>) -> Option<i64> {
        Some(match self {
            Self::Number { value, .. } | Self::Char { value, .. } => i64::from(*value),
            Self::Symbol(path) => lookup(path)?,
            Self::Paren(inner) => inner.fold(lookup)?,
            Self::Unary(op, operand) => {
                let value = operand.fold(lookup)?;
                match op {
                    Unary::Plus => value,
                    Unary::Negate => -value,
                    Unary::BitNot => !value,
                    Unary::LogicalNot => i64::from(value == 0),
                    Unary::LowByte => value & 0xFF,
                    Unary::HighByte => (value >> 8) & 0xFF,
                    Unary::BankByte => (value >> 16) & 0xFF,
                    Unary::LowWord => value & 0xFFFF,
                    Unary::HighWord => (value >> 16) & 0xFFFF,
                }
            }
            Self::Binary {
                op, left, right, ..
            } => {
                let (a, b) = (left.fold(lookup)?, right.fold(lookup)?);
                match op {
                    Binary::Mul => a.wrapping_mul(b),
                    Binary::Div => a.checked_div(b)?,
                    Binary::Mod => a.checked_rem(b)?,
                    Binary::BitAnd => a & b,
                    Binary::BitXor => a ^ b,
                    Binary::Shl => a.checked_shl(u32::try_from(b).ok()?)?,
                    Binary::Shr => a.checked_shr(u32::try_from(b).ok()?)?,
                    Binary::Add => a.wrapping_add(b),
                    Binary::Sub => a.wrapping_sub(b),
                    Binary::BitOr => a | b,
                    Binary::Equal => i64::from(a == b),
                    Binary::NotEqual => i64::from(a != b),
                    Binary::Less => i64::from(a < b),
                    Binary::Greater => i64::from(a > b),
                    Binary::LessEqual => i64::from(a <= b),
                    Binary::GreaterEqual => i64::from(a >= b),
                    Binary::And => i64::from(a != 0 && b != 0),
                    Binary::Xor => i64::from((a != 0) != (b != 0)),
                    Binary::Or => i64::from(a != 0 || b != 0),
                }
            }
            Self::Function { name, arguments } => match (name.as_str(), arguments.as_slice()) {
                ("sizeof", [Self::Symbol(path)]) => {
                    let mut query = path.clone();
                    query.push(".sizeof".to_owned());
                    lookup(&query)?
                }
                _ => return None,
            },
            Self::Str { .. } | Self::Cheap(_) | Self::Unnamed(_) | Self::Here => return None,
        })
    }
}

/// Parses an expression out of `tokens`, starting at `*at` and leaving `*at`
/// on the first token it did not take.
pub fn parse(tokens: &[Token], at: &mut usize) -> Result<Expr, String> {
    let mut parser = Parser { tokens, at: *at };
    let expr = parser.expression(0)?;
    *at = parser.at;
    Ok(expr)
}

/// Parses `tokens` as exactly one expression.
pub fn parse_all(tokens: &[Token]) -> Result<Expr, String> {
    let mut at = 0;
    let expr = parse(tokens, &mut at)?;
    if at != tokens.len() {
        return Err(format!(
            "unexpected `{}` after an expression",
            describe(&tokens[at].tok)
        ));
    }
    Ok(expr)
}

/// A token as a reader would quote it.
pub fn describe(tok: &Tok) -> String {
    match tok {
        Tok::Ident(name) => name.clone(),
        Tok::Cheap(name) => format!("@{name}"),
        Tok::Directive(name) => format!(".{name}"),
        Tok::Number { text, .. } | Tok::Char { text, .. } => text.clone(),
        Tok::Str { text, .. } => format!("\"{text}\""),
        Tok::Punct(punct) => (*punct).to_owned(),
    }
}

struct Parser<'t> {
    tokens: &'t [Token],
    at: usize,
}

impl Parser<'_> {
    fn peek(&self) -> Option<&Tok> {
        self.tokens.get(self.at).map(|token| &token.tok)
    }

    fn binary_operator(&self) -> Option<(Binary, bool)> {
        let token = self.tokens.get(self.at)?;
        let op = match &token.tok {
            Tok::Punct("*") => Binary::Mul,
            Tok::Punct("/") => Binary::Div,
            Tok::Punct("&") => Binary::BitAnd,
            Tok::Punct("^") => Binary::BitXor,
            Tok::Punct("<<") => Binary::Shl,
            Tok::Punct(">>") => Binary::Shr,
            Tok::Punct("+") => Binary::Add,
            Tok::Punct("-") => Binary::Sub,
            Tok::Punct("|") => Binary::BitOr,
            Tok::Punct("=" | "==") => Binary::Equal,
            Tok::Punct("<>") => Binary::NotEqual,
            Tok::Punct("<") => Binary::Less,
            Tok::Punct(">") => Binary::Greater,
            Tok::Punct("<=") => Binary::LessEqual,
            Tok::Punct(">=") => Binary::GreaterEqual,
            Tok::Punct("&&") => Binary::And,
            Tok::Punct("||") => Binary::Or,
            Tok::Directive(name) => match name.as_str() {
                "mod" => Binary::Mod,
                "bitand" => Binary::BitAnd,
                "bitxor" => Binary::BitXor,
                "shl" => Binary::Shl,
                "shr" => Binary::Shr,
                "bitor" => Binary::BitOr,
                "and" => Binary::And,
                "xor" => Binary::Xor,
                "or" => Binary::Or,
                _ => return None,
            },
            _ => return None,
        };
        let spaced = token.space_before
            || self
                .tokens
                .get(self.at + 1)
                .is_some_and(|next| next.space_before);
        Some((op, spaced))
    }

    fn expression(&mut self, floor: u8) -> Result<Expr, String> {
        let mut left = self.unary()?;
        while let Some((op, spaced)) = self.binary_operator() {
            let level = op.level();
            if level <= floor {
                break;
            }
            self.at += 1;
            let right = self.expression(level)?;
            left = Expr::Binary {
                op,
                left: Box::new(left),
                right: Box::new(right),
                spaced,
            };
        }
        Ok(left)
    }

    fn unary(&mut self) -> Result<Expr, String> {
        let op = match self.peek() {
            Some(Tok::Punct("+")) => Some(Unary::Plus),
            Some(Tok::Punct("-")) => Some(Unary::Negate),
            Some(Tok::Punct("~")) => Some(Unary::BitNot),
            Some(Tok::Punct("!")) => Some(Unary::LogicalNot),
            Some(Tok::Punct("<")) => Some(Unary::LowByte),
            Some(Tok::Punct(">")) => Some(Unary::HighByte),
            Some(Tok::Punct("^")) => Some(Unary::BankByte),
            Some(Tok::Directive(name)) if name == "bitnot" => Some(Unary::BitNot),
            Some(Tok::Directive(name)) if name == "not" => Some(Unary::LogicalNot),
            _ => None,
        };
        if let Some(op) = op {
            self.at += 1;
            let operand = self.unary()?;
            return Ok(Expr::Unary(op, Box::new(operand)));
        }
        self.primary()
    }

    fn primary(&mut self) -> Result<Expr, String> {
        let Some(token) = self.tokens.get(self.at) else {
            return Err("an expression ends before it begins".to_owned());
        };
        self.at += 1;
        Ok(match &token.tok {
            Tok::Number { value, text } => Expr::Number {
                value: *value,
                text: text.clone(),
            },
            Tok::Char { value, text } => Expr::Char {
                value: *value,
                text: text.clone(),
            },
            Tok::Str { text, bytes } => Expr::Str {
                text: text.clone(),
                bytes: bytes.clone(),
            },
            Tok::Cheap(name) => Expr::Cheap(name.clone()),
            Tok::Ident(name) => {
                let mut path = vec![name.clone()];
                while self.peek() == Some(&Tok::Punct("::")) {
                    let Some(Tok::Ident(part)) = self.tokens.get(self.at + 1).map(|t| &t.tok)
                    else {
                        return Err(format!("`{name}::` names nothing"));
                    };
                    path.push(part.clone());
                    self.at += 2;
                }
                Expr::Symbol(path)
            }
            Tok::Punct("::") => {
                // A leading `::` is ca65's global scope; this converter reads
                // the name as the unit-level one it then is.
                return self.primary();
            }
            Tok::Punct("*") => Expr::Here,
            Tok::Punct(":") => {
                let mut steps = 0i32;
                while let Some(Tok::Punct(sign @ ("+" | "-"))) = self.peek() {
                    if self.tokens[self.at].space_before {
                        break;
                    }
                    steps += if *sign == "+" { 1 } else { -1 };
                    self.at += 1;
                }
                if steps == 0 {
                    return Err("a `:` that refers to no unnamed label".to_owned());
                }
                Expr::Unnamed(steps)
            }
            Tok::Punct("(") => {
                let inner = self.expression(0)?;
                self.expect(")")?;
                Expr::Paren(Box::new(inner))
            }
            Tok::Directive(name) => self.function(name)?,
            other => return Err(format!("`{}` cannot start an expression", describe(other))),
        })
    }

    fn function(&mut self, name: &str) -> Result<Expr, String> {
        self.expect("(")?;
        let mut arguments = Vec::new();
        if self.peek() != Some(&Tok::Punct(")")) {
            loop {
                arguments.push(self.expression(0)?);
                if self.peek() == Some(&Tok::Punct(",")) {
                    self.at += 1;
                    continue;
                }
                break;
            }
        }
        self.expect(")")?;
        let unary = match name {
            "lobyte" => Some(Unary::LowByte),
            "hibyte" => Some(Unary::HighByte),
            "bankbyte" => Some(Unary::BankByte),
            "loword" => Some(Unary::LowWord),
            "hiword" => Some(Unary::HighWord),
            _ => None,
        };
        match (unary, arguments.len()) {
            (Some(op), 1) => Ok(Expr::Unary(op, Box::new(arguments.remove(0)))),
            (Some(_), _) => Err(format!("`.{name}` takes one argument")),
            (None, _) => Ok(Expr::Function {
                name: name.to_owned(),
                arguments,
            }),
        }
    }

    fn expect(&mut self, punct: &str) -> Result<(), String> {
        match self.peek() {
            Some(Tok::Punct(found)) if *found == punct => {
                self.at += 1;
                Ok(())
            }
            Some(other) => Err(format!("expected `{punct}`, found `{}`", describe(other))),
            None => Err(format!("expected `{punct}` before the end of the line")),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::lex::lex;
    use super::*;

    fn tree(text: &str) -> Expr {
        let line = lex(text, false).expect("lexes");
        parse_all(&line.tokens).expect("parses")
    }

    fn value(text: &str) -> Option<i64> {
        tree(text).fold(&|path| match path {
            [name] if name == "label" => Some(0x12FF),
            _ => None,
        })
    }

    #[test]
    fn the_unary_byte_operators_bind_tighter_than_addition() {
        // ca65: the low byte of `label`, plus one — not the low byte of the sum.
        assert_eq!(value("<label+1"), Some(0x100));
        assert_eq!(value("<(label+1)"), Some(0x00));
        assert_eq!(value(">label"), Some(0x12));
    }

    #[test]
    fn bitwise_operators_take_ca65s_levels() {
        // `&` sits with `*`, so this is 1 + (2 & 3).
        assert_eq!(value("1 + 2 & 3"), Some(3));
        // `|` sits with `+`, left to right.
        assert_eq!(value("1 | 2 + 4"), Some(7));
    }

    #[test]
    fn a_scoped_name_and_a_pseudo_function_are_kept_by_name() {
        assert_eq!(
            tree("RIA::op"),
            Expr::Symbol(vec!["RIA".to_owned(), "op".to_owned()])
        );
        assert_eq!(
            tree(".sizeof(TCB) - 3"),
            Expr::Binary {
                op: Binary::Sub,
                left: Box::new(Expr::Function {
                    name: "sizeof".to_owned(),
                    arguments: vec![Expr::Symbol(vec!["TCB".to_owned()])],
                }),
                right: Box::new(Expr::Number {
                    value: 3,
                    text: "3".to_owned()
                }),
                spaced: true,
            }
        );
    }

    #[test]
    fn unnamed_references_count_their_steps() {
        assert_eq!(tree(":+"), Expr::Unnamed(1));
        assert_eq!(tree(":--"), Expr::Unnamed(-2));
    }
}
