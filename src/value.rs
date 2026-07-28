/*****************************************************************************/
/*                                                                           */
/*                                  value.rs                                 */
/*                                                                           */
/*        Folded values and the link-time expressions ld65 resolves          */
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

//! The value model: what an expression is worth, and who gets to work it out.
//!
//! [`Value`] is an *absolute* integer — one K816 folded at compile time.
//! [`KExpr`] is what survives folding when a link-time symbol is involved:
//! a small expression tree that ca65 stores in the object file and ld65
//! evaluates after placement. K816 never resolves an address ld65 owns, so
//! anything symbolic stays a `KExpr` all the way to emission.
//!
//! The operators here are the *lowered* set, deliberately independent of the
//! syntax that produced them: after lowering no stage may care whether an
//! expression came from native or HLA source. `doc/syntax-reference.md`
//! ("Expression Operators") is the surface authority; ca65's precedence table
//! differs from K65's, which is why emission fully parenthesizes rather than
//! reproducing operator text (sub-plan C, backend decision 1).
//!
//! This module holds the data types only. Folding, classification, and range
//! validation are implemented on them in task C1.

use crate::symbol::SymbolId;

/// An absolute integer value, known at compile time.
///
/// The 65816 address space is 24 bits and every K816 literal, address, and
/// data value fits `u32`; wrapping arithmetic during folding is masked back
/// into that range by [`Value::wrapping`].
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Value(u32);

impl Value {
    pub const fn new(value: u32) -> Self {
        Self(value)
    }

    /// The result of an arithmetic step, truncated back into range.
    ///
    /// Folding computes in `u64` so an intermediate product or shift cannot
    /// panic in a debug build; the result is then truncated, which is what
    /// the 65816 does with an over-wide constant anyway. Whether the
    /// truncated value still fits its operand field is a separate check.
    pub const fn wrapping(value: u64) -> Self {
        Self(value as u32)
    }

    pub const fn get(self) -> u32 {
        self.0
    }

    /// True when the value fits `bytes` bytes unsigned.
    pub const fn fits(self, bytes: u32) -> bool {
        bytes >= 4 || self.0 >> (bytes * 8) == 0
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0 <= 0xFF {
            write!(f, "${:02X}", self.0)
        } else if self.0 <= 0xFFFF {
            write!(f, "${:04X}", self.0)
        } else {
            write!(f, "${:06X}", self.0)
        }
    }
}

/// A unary operator that survives to emission.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    /// `-x`
    Negate,
    /// `~x`
    BitNot,
    /// `&<x` — the low byte, ca65 `.lobyte`.
    LowByte,
    /// `&>x` — the high byte, ca65 `.hibyte`.
    HighByte,
    /// The bank byte, ca65 `.bankbyte`. K65 has no spelling for it; K816
    /// synthesizes it when decomposing a 24-bit value into byte fields
    /// (sub-plan C, backend decision 2).
    BankByte,
    /// `&&x` — the 16-bit address value, ca65 `.loword`.
    Word,
    /// `&&&x` — the 24-bit address value.
    Long,
}

impl UnaryOp {
    /// The ca65 function-like spelling, for the operators that have one.
    /// `Negate`, `BitNot`, and `Long` are emitted structurally instead.
    pub fn ca65_function(self) -> Option<&'static str> {
        match self {
            Self::LowByte => Some(".lobyte"),
            Self::HighByte => Some(".hibyte"),
            Self::BankByte => Some(".bankbyte"),
            Self::Word => Some(".loword"),
            Self::Negate | Self::BitNot | Self::Long => None,
        }
    }
}

/// A binary operator that survives to emission.
///
/// The K65 precedence that ordered these is already encoded in the tree
/// shape by the time a `KExpr` exists; emission re-parenthesizes rather than
/// relying on ca65 agreeing about precedence.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    Mul,
    Add,
    Sub,
    Shl,
    Shr,
    BitAnd,
    BitXor,
    BitOr,
}

impl BinaryOp {
    /// The ca65 infix spelling. ca65's named bitwise and shift operators are
    /// infix operators, not functions (`doc/ca65.sgml`, "Available
    /// operators").
    pub fn ca65_operator(self) -> &'static str {
        match self {
            Self::Mul => "*",
            Self::Add => "+",
            Self::Sub => "-",
            Self::Shl => ".shl",
            Self::Shr => ".shr",
            Self::BitAnd => ".bitand",
            Self::BitXor => ".bitxor",
            Self::BitOr => ".bitor",
        }
    }
}

/// A lowered expression: either a folded constant or a tree ld65 finishes.
///
/// A `KExpr` that mentions no symbol is always `Number`; folding collapses it
/// (task C1). A `KExpr` that mentions a symbol keeps every operator ca65 and
/// ld65 can evaluate, including multiplication and the named bitwise and
/// shift operators — K816 does not invent a narrower relocation language on
/// top of ca65 (sub-plan C, backend decision 10).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum KExpr {
    Number(Value),
    Symbol(SymbolId),
    Unary {
        op: UnaryOp,
        operand: Box<KExpr>,
    },
    Binary {
        op: BinaryOp,
        left: Box<KExpr>,
        right: Box<KExpr>,
    },
}

impl KExpr {
    pub fn number(value: u32) -> Self {
        Self::Number(Value::new(value))
    }

    pub fn unary(op: UnaryOp, operand: KExpr) -> Self {
        Self::Unary {
            op,
            operand: Box::new(operand),
        }
    }

    pub fn binary(op: BinaryOp, left: KExpr, right: KExpr) -> Self {
        Self::Binary {
            op,
            left: Box::new(left),
            right: Box::new(right),
        }
    }

    /// The folded value, when this expression is already absolute.
    ///
    /// Structural, not an evaluator: folding happens before a `KExpr` is
    /// built, so anything still shaped as a tree mentions a symbol somewhere
    /// and belongs to ld65.
    pub fn value(&self) -> Option<Value> {
        match self {
            Self::Number(value) => Some(*value),
            Self::Symbol(_) | Self::Unary { .. } | Self::Binary { .. } => None,
        }
    }

    /// True when ld65 — not K816 — decides this expression's value.
    pub fn is_link_time(&self) -> bool {
        self.value().is_none()
    }

    /// Every symbol this expression references, in tree order.
    pub fn symbols(&self, out: &mut Vec<SymbolId>) {
        match self {
            Self::Number(_) => {}
            Self::Symbol(id) => out.push(*id),
            Self::Unary { operand, .. } => operand.symbols(out),
            Self::Binary { left, right, .. } => {
                left.symbols(out);
                right.symbols(out);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn values_render_at_their_natural_width() {
        assert_eq!(Value::new(0x0F).to_string(), "$0F");
        assert_eq!(Value::new(0x0400).to_string(), "$0400");
        assert_eq!(Value::new(0x01_2345).to_string(), "$012345");
    }

    #[test]
    fn fits_checks_the_encoded_byte_count() {
        assert!(Value::new(0xFF).fits(1));
        assert!(!Value::new(0x100).fits(1));
        assert!(Value::new(0xFFFF).fits(2));
        assert!(!Value::new(0x1_0000).fits(2));
        assert!(Value::new(0xFF_FFFF).fits(3));
        assert!(Value::new(u32::MAX).fits(4));
    }

    #[test]
    fn a_number_is_absolute_and_a_tree_is_link_time() {
        assert!(!KExpr::number(7).is_link_time());
        assert_eq!(KExpr::number(7).value(), Some(Value::new(7)));

        let tree = KExpr::binary(BinaryOp::Add, KExpr::number(1), KExpr::number(2));
        assert!(tree.is_link_time(), "an unfolded tree belongs to ld65");
        assert_eq!(tree.value(), None);
    }

    #[test]
    fn ca65_spellings_are_the_named_forms() {
        assert_eq!(BinaryOp::BitAnd.ca65_operator(), ".bitand");
        assert_eq!(BinaryOp::Shl.ca65_operator(), ".shl");
        assert_eq!(UnaryOp::LowByte.ca65_function(), Some(".lobyte"));
        assert_eq!(UnaryOp::BankByte.ca65_function(), Some(".bankbyte"));
        assert_eq!(UnaryOp::Negate.ca65_function(), None);
    }
}
