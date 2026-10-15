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

    /// True when the value fits a use `bytes` wide: when it lies in
    /// −2^(n−1) … 2^n−1 for that use's *n* bits.
    ///
    /// The rule that lets a constant with no size of its own take the size
    /// of each place it is used (`plans/15-constant-sizes.md` decision 2):
    /// −1 and 255 both fit a byte, as `$FF`, and −129 and 256 do not. A
    /// value is a 32-bit two's-complement number, so one whose top bit is
    /// set is the negative it spells — `$FFFFFFFF` is −1 — and an address
    /// is not a value: it keeps [`Value::fits`].
    pub const fn fits_use(self, bytes: u32) -> bool {
        if bytes >= 4 {
            return true;
        }
        let bits = bytes * 8;
        self.0 >> bits == 0 || (self.0 as i32) >> (bits - 1) == -1
    }

    /// The low `bytes` bytes: what a use that fits emits.
    pub const fn truncated(self, bytes: u32) -> Self {
        if bytes >= 4 {
            self
        } else {
            Self(self.0 & ((1 << (bytes * 8)) - 1))
        }
    }

    /// The value read as the signed 32-bit number its pattern spells.
    pub const fn signed(self) -> i32 {
        self.0 as i32
    }

    /// The value as a reader wrote it: a negative one as a negative
    /// decimal, any other at its natural hexadecimal width.
    ///
    /// For the messages about a *value* — an immediate, a datum, a
    /// constant — where `$FFFFFF7F` would be a number nobody typed and
    /// `-129` is the one they did.
    pub fn written(self) -> String {
        if self.signed() < 0 {
            self.signed().to_string()
        } else {
            self.to_string()
        }
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

    /// Applies `op`, collapsing to a number when the operand is absolute and
    /// keeping the tree for ld65 when it is not.
    pub fn fold_unary(op: UnaryOp, operand: KExpr) -> Self {
        match operand.value() {
            Some(value) => Self::Number(apply_unary(op, value)),
            None => Self::unary(op, operand),
        }
    }

    /// Applies `op`, collapsing to a number when both operands are absolute.
    ///
    /// A link-time operand keeps the whole expression symbolic — including
    /// multiplication and the named bitwise and shift operators, which ca65
    /// stores and ld65 evaluates after placement. K816 does not reject
    /// `label * 2` merely because ld65 owns `label`.
    pub fn fold_binary(op: BinaryOp, left: KExpr, right: KExpr) -> Self {
        match (left.value(), right.value()) {
            (Some(left), Some(right)) => Self::Number(apply_binary(op, left, right)),
            _ => Self::binary(op, left, right),
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

    /// The value with each symbol replaced by what `known` says it is, when
    /// it says something for every one.
    ///
    /// For the one position where a name with a value of its own may stand
    /// for it: a pinned variable's address in another pinned variable's
    /// initializer. Everywhere else a symbol stays a symbol for ld65.
    pub fn value_with(&self, known: &impl Fn(SymbolId) -> Option<Value>) -> Option<Value> {
        match self {
            Self::Number(value) => Some(*value),
            Self::Symbol(id) => known(*id),
            Self::Unary { op, operand } => Some(apply_unary(*op, operand.value_with(known)?)),
            Self::Binary { op, left, right } => Some(apply_binary(
                *op,
                left.value_with(known)?,
                right.value_with(known)?,
            )),
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

/// Applies a unary operator to an absolute value.
pub fn apply_unary(op: UnaryOp, value: Value) -> Value {
    let value = value.get();
    Value::new(match op {
        UnaryOp::Negate => value.wrapping_neg(),
        UnaryOp::BitNot => !value,
        UnaryOp::LowByte => value & 0xFF,
        UnaryOp::HighByte => (value >> 8) & 0xFF,
        UnaryOp::BankByte => (value >> 16) & 0xFF,
        UnaryOp::Word => value & 0xFFFF,
        UnaryOp::Long => value & 0xFF_FFFF,
    })
}

/// Applies a binary operator to two absolute values.
///
/// Arithmetic runs in `u64` and truncates, so an over-wide intermediate
/// cannot panic in a debug build; a shift count past the value's width
/// produces zero rather than wrapping the count, which is what a reader of
/// `1 << 40` expects. Whether the result still fits its operand field is a
/// separate check at the use site.
pub fn apply_binary(op: BinaryOp, left: Value, right: Value) -> Value {
    let (a, b) = (left.get() as u64, right.get() as u64);
    match op {
        BinaryOp::Mul => Value::wrapping(a.wrapping_mul(b)),
        BinaryOp::Add => Value::wrapping(a.wrapping_add(b)),
        BinaryOp::Sub => Value::wrapping(a.wrapping_sub(b)),
        BinaryOp::Shl => Value::new(left.get().checked_shl(right.get()).unwrap_or(0)),
        BinaryOp::Shr => Value::new(left.get().checked_shr(right.get()).unwrap_or(0)),
        BinaryOp::BitAnd => Value::new(left.get() & right.get()),
        BinaryOp::BitXor => Value::new(left.get() ^ right.get()),
        BinaryOp::BitOr => Value::new(left.get() | right.get()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn folded(op: BinaryOp, left: u32, right: u32) -> u32 {
        apply_binary(op, Value::new(left), Value::new(right)).get()
    }

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

    fn negative(value: i32) -> Value {
        Value::new(value as u32)
    }

    #[test]
    fn a_use_fits_from_minus_half_its_range_to_its_unsigned_maximum() {
        for (bytes, low, high) in [
            (1, -0x80, 0xFF),
            (2, -0x8000, 0xFFFF),
            (3, -0x80_0000, 0xFF_FFFF),
        ] {
            assert!(negative(low).fits_use(bytes), "{low} fits {bytes}");
            assert!(
                !negative(low - 1).fits_use(bytes),
                "{} does not fit {bytes}",
                low - 1
            );
            assert!(Value::new(high).fits_use(bytes), "{high:#X} fits {bytes}");
            assert!(
                !Value::new(high + 1).fits_use(bytes),
                "{:#X} does not fit {bytes}",
                high + 1
            );
            assert!(negative(-1).fits_use(bytes));
            assert!(Value::new(0).fits_use(bytes));
        }
        assert!(Value::new(u32::MAX).fits_use(4));
        assert!(negative(i32::MIN).fits_use(4));
    }

    #[test]
    fn a_use_emits_the_low_bits_of_what_fits() {
        assert_eq!(negative(-1).truncated(1), Value::new(0xFF));
        assert_eq!(negative(-1).truncated(2), Value::new(0xFFFF));
        assert_eq!(negative(-1).truncated(3), Value::new(0xFF_FFFF));
        assert_eq!(negative(-1).truncated(4), negative(-1));
        assert_eq!(negative(-128).truncated(1), Value::new(0x80));
        assert_eq!(Value::new(0x1234).truncated(2), Value::new(0x1234));
    }

    #[test]
    fn a_negative_value_is_written_negative() {
        assert_eq!(negative(-129).written(), "-129");
        assert_eq!(negative(-1).written(), "-1");
        assert_eq!(Value::new(0x100).written(), "$0100");
        assert_eq!(Value::new(0x7FFF_FFFF).written(), "$7FFFFFFF");
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
    fn binary_operators_fold_as_documented() {
        // `doc/syntax-reference.md`, "Binary operators".
        assert_eq!(folded(BinaryOp::BitOr, 0x80, 0x40), 0xC0);
        assert_eq!(folded(BinaryOp::BitXor, 0x80, 0x40), 0xC0);
        assert_eq!(folded(BinaryOp::BitAnd, 0x80, !0x40), 0x80);
        assert_eq!(folded(BinaryOp::Shl, 1, 3), 0x08);
        assert_eq!(folded(BinaryOp::Shr, 0xFF, 4), 0x0F);
        assert_eq!(folded(BinaryOp::Mul, 6, 7), 42);
        assert_eq!(folded(BinaryOp::Sub, 1, 2), u32::MAX);
    }

    #[test]
    fn oversized_shifts_produce_zero_rather_than_wrapping_the_count() {
        assert_eq!(folded(BinaryOp::Shl, 1, 40), 0);
        assert_eq!(folded(BinaryOp::Shr, 0xFFFF, 32), 0);
    }

    #[test]
    fn address_byte_operators_select_the_documented_byte() {
        let address = Value::new(0x12_3456);
        assert_eq!(apply_unary(UnaryOp::LowByte, address).get(), 0x56);
        assert_eq!(apply_unary(UnaryOp::HighByte, address).get(), 0x34);
        assert_eq!(apply_unary(UnaryOp::BankByte, address).get(), 0x12);
        assert_eq!(apply_unary(UnaryOp::Word, address).get(), 0x3456);
        assert_eq!(apply_unary(UnaryOp::Long, address).get(), 0x12_3456);
    }

    #[test]
    fn folding_collapses_absolute_trees_and_preserves_symbolic_ones() {
        let absolute = KExpr::fold_binary(BinaryOp::Add, KExpr::number(1), KExpr::number(2));
        assert_eq!(absolute, KExpr::number(3));

        let symbol = KExpr::Symbol(crate::symbol::SymbolId::from_index(0));
        // `label * 2` stays whole: ca65 stores it and ld65 finishes it.
        let relocatable = KExpr::fold_binary(BinaryOp::Mul, symbol.clone(), KExpr::number(2));
        assert!(relocatable.is_link_time());
        assert_eq!(
            relocatable,
            KExpr::binary(BinaryOp::Mul, symbol.clone(), KExpr::number(2))
        );
        assert!(KExpr::fold_unary(UnaryOp::LowByte, symbol).is_link_time());
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
