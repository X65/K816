/*****************************************************************************/
/*                                                                           */
/*                                 render.rs                                 */
/*                                                                           */
/*               ca65 operands and expressions, written as K816              */
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

//! A ca65 operand read into its shape, and an expression written back under
//! K816's rules.
//!
//! Two things differ and both change numbers if they are missed. The
//! precedence: ca65's unary `<` binds tighter than `+` and K816's `&<` takes
//! the whole offset expression, so `<label+1` is `(&<label) + 1`; ca65 puts
//! `&` with `*` and `|` with `+`, K816 has C's order. And the names: a ca65
//! `RIA::op` is a field of a layout at an address, `TCB::sp` an offset in
//! one with none, `task01` an element of a table — each a different K816
//! spelling, which [`Names`] answers.

use super::expr::{self, Binary, Expr, Unary};
use super::lex::{Tok, Token};

/// A ca65 operand, by shape.
#[derive(Clone, Debug)]
pub enum Operand {
    None,
    /// `a` written as the operand, `asl a`.
    Accumulator(String),
    Immediate(Expr),
    /// `expr`, `expr,x`, `expr,y`, `expr,s`, with a `z:`/`a:`/`f:` prefix.
    Direct {
        prefix: Option<char>,
        expr: Expr,
        index: Option<char>,
    },
    /// `(expr)`, `(expr,x)`, `(expr),y`, `(expr,s),y`.
    Indirect {
        prefix: Option<char>,
        expr: Expr,
        inner: Option<char>,
        outer: Option<char>,
    },
    /// `[expr]`, `[expr],y`.
    Long {
        prefix: Option<char>,
        expr: Expr,
        y: bool,
    },
    /// `mvn src, dst`, with or without `#`.
    Move(Expr, Expr),
}

/// Reads `tokens` as the operand of `mnemonic`.
pub fn operand(mnemonic: &str, tokens: &[Token]) -> Result<Operand, String> {
    if tokens.is_empty() {
        return Ok(Operand::None);
    }
    if mnemonic == "mvn" || mnemonic == "mvp" {
        let parts = super::source::split_commas(tokens);
        let [source, destination] = parts.as_slice() else {
            return Err(format!("`{mnemonic}` takes two banks"));
        };
        let bank = |part: &[Token]| {
            let part = match part.first() {
                Some(Token {
                    tok: Tok::Punct("#"),
                    ..
                }) => &part[1..],
                _ => part,
            };
            expr::parse_all(part)
        };
        return Ok(Operand::Move(bank(source)?, bank(destination)?));
    }
    if let [
        Token {
            tok: Tok::Ident(register),
            ..
        },
    ] = tokens
        && register.eq_ignore_ascii_case("a")
    {
        return Ok(Operand::Accumulator(register.clone()));
    }
    if tokens[0].tok == Tok::Punct("#") {
        return Ok(Operand::Immediate(expr::parse_all(&tokens[1..])?));
    }
    // `z:`, `a:` and `f:` force an address size.
    let (prefix, tokens) = match tokens {
        [
            Token {
                tok: Tok::Ident(size),
                ..
            },
            Token {
                tok: Tok::Punct(":"),
                ..
            },
            rest @ ..,
        ] if matches!(size.to_ascii_lowercase().as_str(), "z" | "a" | "f") => {
            (size.to_ascii_lowercase().chars().next(), rest)
        }
        _ => (None, tokens),
    };
    match tokens.first().map(|token| &token.tok) {
        Some(Tok::Punct("(")) if let Some(close) = closing(tokens, "(", ")") => {
            let after = &tokens[close + 1..];
            let indirect = after.is_empty() || index_of(after).is_some();
            if indirect {
                let inside = &tokens[1..close];
                let (expr, inner) = indexed(inside)?;
                return Ok(Operand::Indirect {
                    prefix,
                    expr,
                    inner,
                    outer: index_of(after),
                });
            }
        }
        Some(Tok::Punct("[")) if let Some(close) = closing(tokens, "[", "]") => {
            let after = &tokens[close + 1..];
            let expr = expr::parse_all(&tokens[1..close])?;
            return match index_of(after) {
                Some('y') => Ok(Operand::Long {
                    prefix,
                    expr,
                    y: true,
                }),
                None if after.is_empty() => Ok(Operand::Long {
                    prefix,
                    expr,
                    y: false,
                }),
                _ => Err("an indirect-long operand indexes by `y` or nothing".to_owned()),
            };
        }
        _ => {}
    }
    let (expr, index) = indexed(tokens)?;
    Ok(Operand::Direct {
        prefix,
        expr,
        index,
    })
}

/// The position of the token closing the group `tokens` opens.
fn closing(tokens: &[Token], open: &str, close: &str) -> Option<usize> {
    let mut depth = 0;
    for (index, token) in tokens.iter().enumerate() {
        match &token.tok {
            Tok::Punct(punct) if *punct == open => depth += 1,
            Tok::Punct(punct) if *punct == close => {
                depth -= 1;
                if depth == 0 {
                    return Some(index);
                }
            }
            _ => {}
        }
    }
    None
}

/// `,x` / `,y` / `,s`, when `tokens` is exactly that.
fn index_of(tokens: &[Token]) -> Option<char> {
    match tokens {
        [
            Token {
                tok: Tok::Punct(","),
                ..
            },
            Token {
                tok: Tok::Ident(register),
                ..
            },
        ] => match register.to_ascii_lowercase().as_str() {
            "x" => Some('x'),
            "y" => Some('y'),
            "s" => Some('s'),
            _ => None,
        },
        _ => None,
    }
}

/// An expression and the index register after it, if one is.
fn indexed(tokens: &[Token]) -> Result<(Expr, Option<char>), String> {
    if tokens.len() >= 2
        && let Some(index) = index_of(&tokens[tokens.len() - 2..])
    {
        return Ok((expr::parse_all(&tokens[..tokens.len() - 2])?, Some(index)));
    }
    Ok((expr::parse_all(tokens)?, None))
}

/// What a name is, for writing it as K816.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Meaning {
    /// A value: a `const`, an enum member.
    Value,
    /// A place: a variable, a label, a section, a data block.
    Place,
    /// A field layout at an address, written `NAME.field` for the address.
    FixedLayout,
    /// A field layout with no address, written `NAME.field:offsetof`.
    AbstractLayout,
    /// An element of a table, `TASKS[1]`, spelled out in full.
    Element(String),
}

/// Everything the renderer asks about names.
pub trait Names {
    /// The K816 spelling of the unit-level `name`.
    fn spelling(&self, name: &str) -> String;
    /// What `name` is.
    fn meaning(&self, name: &str) -> Meaning;
    /// The layout a place is declared with, for `BASE + S::field`.
    fn layout_of(&self, name: &str) -> Option<String>;
    /// The K816 spelling of a cheap local, `@name`.
    fn cheap(&self, name: &str) -> String;
    /// The K816 spelling of an unnamed reference, `:+`.
    fn unnamed(&self, steps: i32) -> String;
    /// The K816 spelling of `*`.
    fn here(&self) -> String;
    /// The value of a name, for folding what K816 cannot write.
    fn value(&self, path: &[String]) -> Option<i64>;
    /// Whether `field` of `layout` is an array of bytes, so that `+ 3` past
    /// it is its element `[3]`.
    fn is_byte_array(&self, layout: &str, field: &str) -> bool;
    /// The bytes one element of a field of `layout` holds — its declared
    /// width — following `fields` through nested layouts.
    fn field_width(&self, layout: &str, fields: &[String]) -> Option<u8>;
}

/// K816's binding strength of a rendered expression: what a parent has to
/// parenthesize. Larger binds tighter.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Strength {
    BitOr = 1,
    BitXor = 2,
    BitAnd = 3,
    /// `&<`, `&>`, `&&`: K816's address-byte operators take a whole offset
    /// expression and bind tighter than the bitwise ones.
    AddressByte = 4,
    Shift = 5,
    Additive = 6,
    Multiplicative = 7,
    Unary = 8,
    Atom = 9,
}

/// An expression written as K816, and how tightly it binds.
#[derive(Clone, Debug)]
struct Rendered {
    text: String,
    strength: Strength,
}

impl Rendered {
    fn atom(text: String) -> Self {
        Self {
            text,
            strength: Strength::Atom,
        }
    }

    /// The text, parenthesized when it binds looser than `needed`.
    fn at(&self, needed: Strength) -> String {
        if self.strength < needed {
            format!("({})", self.text)
        } else {
            self.text.clone()
        }
    }
}

/// Writes `expr` as a K816 expression, or says why it cannot be.
pub fn expression(expr: &Expr, names: &dyn Names) -> Result<String, String> {
    render(&rewrite_fields(expr, names), names).map(|rendered| rendered.text)
}

/// The width K816 gives the datum `expr` names: a field of a layout at an
/// address (`RIA::op`) or of a table (`TASKS + TCB::state`), the way
/// [`rewrite_fields`] writes it. Arithmetic is no access path, and a name
/// with no field declares nothing the converter wrote, so both are `None`.
pub fn declared_width(expr: &Expr, names: &dyn Names) -> Option<u8> {
    let Expr::Symbol(path) = rewrite_fields(expr, names) else {
        return None;
    };
    let (first, rest) = path.split_first()?;
    // `BASE.field`, `BASE.name[2]`: a field of the base's layout.
    if rest.iter().all(|step| step.starts_with(['.', '['])) {
        let fields: Vec<String> = rest
            .iter()
            .filter_map(|step| step.strip_prefix('.').map(str::to_owned))
            .collect();
        if fields.is_empty() {
            return None;
        }
        return names.field_width(&names.layout_of(first)?, &fields);
    }
    match names.meaning(first) {
        Meaning::FixedLayout => names.field_width(first, rest),
        _ => None,
    }
}

/// `BASE + S::field …` as `BASE.field …`, where `BASE` is declared with
/// layout `S` — the ca65 spelling of a field access, written as one.
pub fn rewrite_fields(expr: &Expr, names: &dyn Names) -> Expr {
    let Expr::Binary {
        op: Binary::Add, ..
    } = expr
    else {
        return expr.clone();
    };
    let mut terms = Vec::new();
    flatten_sum(expr, &mut terms);
    let Some(base) = terms
        .iter()
        .position(|(_, term)| term.name().and_then(|name| names.layout_of(name)).is_some())
    else {
        return expr.clone();
    };
    let layout = names
        .layout_of(terms[base].1.name().unwrap_or_default())
        .unwrap_or_default();
    let Some(field) = terms.iter().position(|(positive, term)| {
        *positive && matches!(term, Expr::Symbol(path) if path.len() >= 2 && path[0] == layout)
    }) else {
        return expr.clone();
    };
    let Expr::Symbol(path) = &terms[field].1 else {
        return expr.clone();
    };
    let Expr::Symbol(base_path) = &terms[base].1 else {
        return expr.clone();
    };
    let mut access = base_path.clone();
    access.extend(path[1..].iter().map(|step| format!(".{step}")));
    // `TASKS + TCB::name + 2` is element 2 of the name: one more number, and a
    // field of bytes, is the subscript a reader would write.
    let rest: Vec<usize> = (0..terms.len())
        .filter(|index| *index != base && *index != field)
        .collect();
    if let [only] = rest.as_slice()
        && let (true, Expr::Number { text, .. }) = &terms[*only]
        && path.len() == 2
        && names.is_byte_array(&layout, &path[1])
    {
        access.push(format!("[{text}]"));
        return Expr::Symbol(access);
    }
    let mut rebuilt = Expr::Symbol(access);
    for (index, (positive, term)) in terms.into_iter().enumerate() {
        if index == base || index == field {
            continue;
        }
        rebuilt = Expr::Binary {
            op: if positive { Binary::Add } else { Binary::Sub },
            left: Box::new(rebuilt),
            right: Box::new(term),
            spaced: false,
        };
    }
    rebuilt
}

/// The terms of a chain of `+` and `-`, each with its sign.
fn flatten_sum(expr: &Expr, out: &mut Vec<(bool, Expr)>) {
    match expr {
        Expr::Binary {
            op: Binary::Add,
            left,
            right,
            ..
        } => {
            flatten_sum(left, out);
            flatten_sum(right, out);
        }
        Expr::Binary {
            op: Binary::Sub,
            left,
            right,
            ..
        } => {
            flatten_sum(left, out);
            let start = out.len();
            flatten_sum(right, out);
            for term in &mut out[start..] {
                term.0 = !term.0;
            }
        }
        other => out.push((true, other.clone())),
    }
}

fn render(expr: &Expr, names: &dyn Names) -> Result<Rendered, String> {
    Ok(match expr {
        Expr::Number { text, .. } => Rendered::atom(number(text)),
        Expr::Char { text, .. } => Rendered::atom(text.clone()),
        Expr::Str { .. } => return Err("a string is not a value here".to_owned()),
        Expr::Symbol(path) => Rendered::atom(symbol(path, names)?),
        Expr::Cheap(name) => Rendered::atom(names.cheap(name)),
        Expr::Unnamed(steps) => Rendered::atom(names.unnamed(*steps)),
        Expr::Here => Rendered::atom(names.here()),
        Expr::Paren(inner) => {
            let inner = render(inner, names)?;
            Rendered::atom(format!("({})", inner.text))
        }
        Expr::Unary(op, operand) => {
            let operand = render(operand, names)?;
            match op {
                Unary::Plus => operand,
                Unary::Negate => Rendered {
                    text: format!("-{}", operand.at(Strength::Unary)),
                    strength: Strength::Unary,
                },
                Unary::BitNot => Rendered {
                    text: format!("~{}", operand.at(Strength::Unary)),
                    strength: Strength::Unary,
                },
                Unary::LowByte => byte_operator("&<", &operand),
                Unary::HighByte => byte_operator("&>", &operand),
                Unary::LowWord => byte_operator("&&", &operand),
                Unary::BankByte => Rendered {
                    text: format!("{} >> 16", operand.at(Strength::Additive)),
                    strength: Strength::Shift,
                },
                Unary::LogicalNot | Unary::HighWord => return fold(expr, names),
            }
        }
        Expr::Binary {
            op,
            left,
            right,
            spaced,
        } => {
            let (spelling, strength) = match op {
                Binary::Mul => ("*", Strength::Multiplicative),
                Binary::Add => ("+", Strength::Additive),
                Binary::Sub => ("-", Strength::Additive),
                Binary::Shl => ("<<", Strength::Shift),
                Binary::Shr => (">>", Strength::Shift),
                Binary::BitAnd => ("&", Strength::BitAnd),
                Binary::BitXor => ("^", Strength::BitXor),
                Binary::BitOr => ("|", Strength::BitOr),
                // K816 has no division, remainder, comparison or logic in an
                // expression; a value that folds is written as its number.
                _ => return fold(expr, names),
            };
            let left = render(left, names)?;
            let right = render(right, names)?;
            // Left-associative: the left side may bind as loosely as the
            // operator, the right side must bind tighter.
            let left_text = left.at(strength);
            let right_text = if right.strength <= strength {
                format!("({})", right.text)
            } else {
                right.text.clone()
            };
            let gap = if *spaced { " " } else { "" };
            Rendered {
                text: format!("{left_text}{gap}{spelling}{gap}{right_text}"),
                strength,
            }
        }
        Expr::Function { name, arguments } => match (name.as_str(), arguments.as_slice()) {
            ("sizeof", [Expr::Symbol(path)]) => Rendered::atom(size_of(path, names)?),
            _ => return fold(expr, names),
        },
    })
}

/// `&<`, `&>` or `&&` over an operand: K816's address-byte operators take
/// the whole offset expression that follows them, so an operand looser than
/// that is parenthesized — and the result binds as an address-byte operator.
fn byte_operator(operator: &str, operand: &Rendered) -> Rendered {
    let text = if operand.strength < Strength::AddressByte {
        format!("{operator}({})", operand.text)
    } else {
        format!("{operator}{}", operand.text)
    };
    Rendered {
        text,
        strength: Strength::AddressByte,
    }
}

/// An expression K816 has no operator for, written as the number it folds
/// to.
fn fold(expr: &Expr, names: &dyn Names) -> Result<Rendered, String> {
    match expr.fold(&|path| names.value(path)) {
        Some(value) if value >= 0 => Ok(Rendered::atom(format!("{value}"))),
        Some(value) => Ok(Rendered {
            text: format!("{value}"),
            strength: Strength::Unary,
        }),
        None => {
            Err("an expression K816 has no operator for, over a value only ld65 knows".to_owned())
        }
    }
}

/// A ca65 number as K816 writes it. The two spell `$` and `%` alike; a
/// decimal with leading zeros is the same number either way.
fn number(text: &str) -> String {
    text.to_owned()
}

/// A name, or a scoped path `S::field`.
fn symbol(path: &[String], names: &dyn Names) -> Result<String, String> {
    let (first, rest) = path.split_first().ok_or("an empty name")?;
    // An access path the field rewrite built: `BASE.field`, `BASE.name[2]`.
    if rest
        .iter()
        .all(|step| step.starts_with('.') || step.starts_with('['))
        && !rest.is_empty()
    {
        let base = match names.meaning(first) {
            Meaning::Element(spelled) => spelled,
            _ => names.spelling(first),
        };
        return Ok(format!("{base}{}", rest.concat()));
    }
    if rest.is_empty() {
        return Ok(match names.meaning(first) {
            Meaning::Element(spelled) => spelled,
            _ => names.spelling(first),
        });
    }
    let fields: String = rest.iter().map(|step| format!(".{step}")).collect();
    match names.meaning(first) {
        Meaning::FixedLayout => Ok(format!("{}{fields}", names.spelling(first))),
        Meaning::AbstractLayout => Ok(format!("{}{fields}:offsetof", names.spelling(first))),
        _ => Err(format!(
            "`{}` is a scope this converter does not know",
            path.join("::")
        )),
    }
}

/// `.sizeof(S)` / `.sizeof(S::field)`.
fn size_of(path: &[String], names: &dyn Names) -> Result<String, String> {
    let (first, rest) = path.split_first().ok_or("an empty name")?;
    let fields: String = rest.iter().map(|step| format!(".{step}")).collect();
    Ok(format!("{}{fields}:sizeof", names.spelling(first)))
}

#[cfg(test)]
mod tests {
    use super::super::lex::lex;
    use super::*;

    struct Fixture;

    impl Names for Fixture {
        fn spelling(&self, name: &str) -> String {
            name.to_owned()
        }
        fn meaning(&self, name: &str) -> Meaning {
            match name {
                "RIA" => Meaning::FixedLayout,
                "TCB" => Meaning::AbstractLayout,
                "task01" => Meaning::Element("TASKS[1]".to_owned()),
                "K" => Meaning::Value,
                _ => Meaning::Place,
            }
        }
        fn layout_of(&self, name: &str) -> Option<String> {
            (name == "TASKS" || name == "task01").then(|| "TCB".to_owned())
        }
        fn cheap(&self, name: &str) -> String {
            format!(".{name}")
        }
        fn unnamed(&self, steps: i32) -> String {
            format!(".anon{steps}")
        }
        fn here(&self) -> String {
            ".here".to_owned()
        }
        fn value(&self, path: &[String]) -> Option<i64> {
            (path == ["K"]).then_some(10)
        }
        fn is_byte_array(&self, layout: &str, field: &str) -> bool {
            layout == "TCB" && field == "name"
        }
        fn field_width(&self, layout: &str, fields: &[String]) -> Option<u8> {
            match (layout, fields) {
                ("TCB", [field]) if field == "sp" => Some(2),
                ("TCB" | "RIA", [_]) => Some(1),
                _ => None,
            }
        }
    }

    fn written(text: &str) -> String {
        let line = lex(text, false).expect("lexes");
        let expr = expr::parse_all(&line.tokens).expect("parses");
        expression(&expr, &Fixture).expect("renders")
    }

    #[test]
    fn a_low_byte_keeps_ca65s_binding() {
        assert_eq!(written("<label+1"), "(&<label)+1");
        assert_eq!(written("<(label+1)"), "&<(label+1)");
        assert_eq!(written(">label"), "&>label");
    }

    #[test]
    fn bitwise_operators_are_parenthesized_for_k816s_order() {
        // ca65 reads `1 + 2 & 3` as `1 + (2 & 3)`; K816 would read it as
        // `(1 + 2) & 3`, so the parentheses are what keep the value.
        assert_eq!(written("1 + 2 & 3"), "1 + (2 & 3)");
        assert_eq!(written("A | B | C"), "A | B | C");
    }

    #[test]
    fn scoped_names_take_the_layouts_spelling() {
        assert_eq!(written("RIA::op"), "RIA.op");
        assert_eq!(written("TCB::sp"), "TCB.sp:offsetof");
        assert_eq!(
            written(".sizeof(TCB) - TCB::sp - 3"),
            "TCB:sizeof - TCB.sp:offsetof - 3"
        );
    }

    #[test]
    fn a_field_of_a_declared_layout_is_a_field_access() {
        assert_eq!(written("TASKS+TCB::name+0"), "TASKS.name[0]");
        assert_eq!(written("TASKS+TCB::state+1"), "TASKS.state+1");
        assert_eq!(written("task01+TCB::state"), "TASKS[1].state");
    }

    #[test]
    fn a_field_access_declares_its_fields_width_and_arithmetic_none() {
        let width = |text: &str| {
            let line = lex(text, false).expect("lexes");
            declared_width(&expr::parse_all(&line.tokens).expect("parses"), &Fixture)
        };
        assert_eq!(width("RIA::op"), Some(1));
        assert_eq!(width("TASKS+TCB::sp"), Some(2));
        assert_eq!(width("task01+TCB::state"), Some(1));
        assert_eq!(width("TASKS+TCB::name+2"), Some(1));
        assert_eq!(width("TASKS+TCB::state+1"), None);
        assert_eq!(width("TCB::sp"), None);
        assert_eq!(width("TASKS"), None);
    }

    #[test]
    fn what_k816_cannot_write_is_folded() {
        assert_eq!(written("K / 2"), "5");
        assert_eq!(written("K * 2"), "K * 2");
    }

    #[test]
    fn operand_shapes_are_read_as_ca65_reads_them() {
        let read = |mnemonic: &str, text: &str| {
            let line = lex(text, false).expect("lexes");
            operand(mnemonic, &line.tokens).expect("reads")
        };
        assert!(matches!(
            read("lda", "(ptr),y"),
            Operand::Indirect {
                outer: Some('y'),
                ..
            }
        ));
        assert!(matches!(
            read("lda", "(bufptr,s),y"),
            Operand::Indirect {
                inner: Some('s'),
                outer: Some('y'),
                ..
            }
        ));
        assert!(matches!(read("lda", "(K-1)*2"), Operand::Direct { .. }));
        assert!(matches!(
            read("lda", "f:TCB::state,x"),
            Operand::Direct {
                prefix: Some('f'),
                index: Some('x'),
                ..
            }
        ));
        assert!(matches!(
            read("lda", "[ptr],y"),
            Operand::Long { y: true, .. }
        ));
        assert!(matches!(read("mvn", "#0,#0"), Operand::Move(..)));
        assert!(matches!(read("asl", "A"), Operand::Accumulator(_)));
    }
}
