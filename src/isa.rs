/*****************************************************************************/
/*                                                                           */
/*                                   isa.rs                                  */
/*                                                                           */
/*             Typed 65816 instruction set: mnemonics and modes              */
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

//! The typed ISA: one authoritative description of every supported
//! instruction and addressing mode.
//!
//! K816 never encodes opcodes — ca65 owns encoding — so an
//! [`InstructionSpec`] describes legality, operand shape, operand width, and
//! ca65 spelling only. CPU effects are derived from this table in
//! `effects.rs`, keyed by the same (mnemonic, mode) pair, so they cannot
//! silently go stale. `doc/w65c816s.md` is the hardware authority behind
//! every entry.
//!
//! # Hardware mode and source shape are different axes
//!
//! [`AddressingMode`] carries the CPU's hardware mode — the 24 modes of
//! `doc/w65c816s.md` §3.5 — and nothing else. It deliberately cannot
//! classify the *source* spelling: WDC's [`AddressingMode::Stack`] groups the
//! operandless pushes, `PEA #word`, `PEI (dp)`, `PER label`, and the
//! `BRK`/`COP` signature byte, which share no operand syntax at all.
//! [`OperandForm`] is the second axis: it records the AST operand shape a
//! spec accepts. `(mnemonic, mode)` stays the unique hardware key; operand
//! classification (sub-plan C, task C4) selects among a mnemonic's specs by
//! matching `OperandForm` and then choosing an address encoding.
//!
//! Only the mnemonics the vertical slice lowers to are populated here; the
//! complete 256-pair table lands with task C3.

/// A validated 65816 mnemonic. Adding a variant without handling it
/// everywhere is a compile error by design: matches over `Mnemonic` do not
/// use wildcards.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Mnemonic {
    Lda,
    Sta,
    Rts,
    Sep,
    Rep,
}

impl Mnemonic {
    /// Resolves a source spelling (case-insensitive) to a known mnemonic.
    pub fn parse(text: &str) -> Option<Self> {
        let lower = text.to_ascii_lowercase();
        match lower.as_str() {
            "lda" => Some(Self::Lda),
            "sta" => Some(Self::Sta),
            "rts" => Some(Self::Rts),
            "sep" => Some(Self::Sep),
            "rep" => Some(Self::Rep),
            _ => None,
        }
    }

    /// The ca65 spelling of this mnemonic.
    pub fn ca65(self) -> &'static str {
        match self {
            Self::Lda => "lda",
            Self::Sta => "sta",
            Self::Rts => "rts",
            Self::Sep => "sep",
            Self::Rep => "rep",
        }
    }
}

/// A hardware addressing mode: the 24 modes of `doc/w65c816s.md` §3.5, in the
/// document's own order, each named after the symbol of its Table 5-6 row.
///
/// This is the CPU's axis only. See the module documentation for why it
/// cannot double as a classification of the source operand spelling.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum AddressingMode {
    /// `a` — absolute.
    Absolute,
    /// `(a,x)` — absolute indexed indirect.
    AbsoluteIndexedIndirect,
    /// `a,x` — absolute indexed with X.
    AbsoluteIndexedX,
    /// `a,y` — absolute indexed with Y.
    AbsoluteIndexedY,
    /// `(a)` — absolute indirect.
    AbsoluteIndirect,
    /// `al,x` — absolute long indexed with X.
    AbsoluteLongIndexedX,
    /// `al` — absolute long.
    AbsoluteLong,
    /// `A` — accumulator.
    Accumulator,
    /// `xyc` — block move.
    BlockMove,
    /// `(d,x)` — direct indexed indirect.
    DirectIndexedIndirect,
    /// `d,x` — direct indexed with X.
    DirectIndexedX,
    /// `d,y` — direct indexed with Y.
    DirectIndexedY,
    /// `(d),y` — direct indirect indexed.
    DirectIndirectIndexed,
    /// `[d],y` — direct indirect long indexed.
    DirectIndirectLongIndexed,
    /// `[d]` — direct indirect long.
    DirectIndirectLong,
    /// `(d)` — direct indirect.
    DirectIndirect,
    /// `d` — direct.
    Direct,
    /// `#` — immediate.
    Immediate,
    /// `i` — implied.
    Implied,
    /// `rl` — program counter relative long.
    RelativeLong,
    /// `r` — program counter relative.
    Relative,
    /// `s` — stack.
    Stack,
    /// `d,s` — stack relative.
    StackRelative,
    /// `(d,s),y` — stack relative indirect indexed.
    StackRelativeIndirectIndexed,
}

impl AddressingMode {
    /// Every hardware mode, for table-driven tests.
    pub const ALL: [Self; 24] = [
        Self::Absolute,
        Self::AbsoluteIndexedIndirect,
        Self::AbsoluteIndexedX,
        Self::AbsoluteIndexedY,
        Self::AbsoluteIndirect,
        Self::AbsoluteLongIndexedX,
        Self::AbsoluteLong,
        Self::Accumulator,
        Self::BlockMove,
        Self::DirectIndexedIndirect,
        Self::DirectIndexedX,
        Self::DirectIndexedY,
        Self::DirectIndirectIndexed,
        Self::DirectIndirectLongIndexed,
        Self::DirectIndirectLong,
        Self::DirectIndirect,
        Self::Direct,
        Self::Immediate,
        Self::Implied,
        Self::RelativeLong,
        Self::Relative,
        Self::Stack,
        Self::StackRelative,
        Self::StackRelativeIndirectIndexed,
    ];

    /// The Table 5-6 symbol for this mode, e.g. `"(d,s),y"`.
    pub fn symbol(self) -> &'static str {
        match self {
            Self::Absolute => "a",
            Self::AbsoluteIndexedIndirect => "(a,x)",
            Self::AbsoluteIndexedX => "a,x",
            Self::AbsoluteIndexedY => "a,y",
            Self::AbsoluteIndirect => "(a)",
            Self::AbsoluteLongIndexedX => "al,x",
            Self::AbsoluteLong => "al",
            Self::Accumulator => "A",
            Self::BlockMove => "xyc",
            Self::DirectIndexedIndirect => "(d,x)",
            Self::DirectIndexedX => "d,x",
            Self::DirectIndexedY => "d,y",
            Self::DirectIndirectIndexed => "(d),y",
            Self::DirectIndirectLongIndexed => "[d],y",
            Self::DirectIndirectLong => "[d]",
            Self::DirectIndirect => "(d)",
            Self::Direct => "d",
            Self::Immediate => "#",
            Self::Implied => "i",
            Self::RelativeLong => "rl",
            Self::Relative => "r",
            Self::Stack => "s",
            Self::StackRelative => "d,s",
            Self::StackRelativeIndirectIndexed => "(d,s),y",
        }
    }

    /// The address encoding this mode's operand field uses, or `None` when
    /// the mode encodes no address (implied, accumulator, immediate, stack,
    /// relative, and block move).
    pub fn encoding(self) -> Option<AddressEncoding> {
        match self {
            Self::Direct
            | Self::DirectIndexedX
            | Self::DirectIndexedY
            | Self::DirectIndexedIndirect
            | Self::DirectIndirect
            | Self::DirectIndirectIndexed
            | Self::DirectIndirectLong
            | Self::DirectIndirectLongIndexed => Some(AddressEncoding::Direct),
            Self::Absolute
            | Self::AbsoluteIndexedX
            | Self::AbsoluteIndexedY
            | Self::AbsoluteIndexedIndirect
            | Self::AbsoluteIndirect => Some(AddressEncoding::Absolute),
            Self::AbsoluteLong | Self::AbsoluteLongIndexedX => Some(AddressEncoding::Long),
            Self::Accumulator
            | Self::BlockMove
            | Self::Immediate
            | Self::Implied
            | Self::Relative
            | Self::RelativeLong
            | Self::Stack
            | Self::StackRelative
            | Self::StackRelativeIndirectIndexed => None,
        }
    }
}

/// The width of the address an operand encodes, and the ca65 address-size
/// prefix that pins it (`doc/ca65.sgml`, "Address sizes").
///
/// K816 chooses and validates the encoding; every emitted memory operand
/// carries the prefix explicitly so ca65 cannot pick a different one from its
/// own symbol knowledge (sub-plan C, backend decision 3).
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum AddressEncoding {
    /// Direct page: one operand byte, `z:` in ca65.
    Direct,
    /// Absolute: two operand bytes, `a:` in ca65.
    Absolute,
    /// Absolute long: three operand bytes, `f:` in ca65.
    Long,
}

impl AddressEncoding {
    /// The ca65 address-size prefix, including its colon.
    pub fn ca65_prefix(self) -> &'static str {
        match self {
            Self::Direct => "z:",
            Self::Absolute => "a:",
            Self::Long => "f:",
        }
    }

    /// The K65 operand prefix keyword that forces this encoding.
    pub fn k65_keyword(self) -> &'static str {
        match self {
            Self::Direct => "dp",
            Self::Absolute => "abs",
            Self::Long => "far",
        }
    }

    /// The largest value this encoding can hold.
    pub fn max_value(self) -> u32 {
        match self {
            Self::Direct => 0xFF,
            Self::Absolute => 0xFFFF,
            Self::Long => 0xFF_FFFF,
        }
    }
}

/// Width of one CPU register pair per the 65816 M/X flags.
///
/// Lives here rather than in `op.rs` because it describes the CPU, and both
/// the ISA table and the effect model need it without depending on the
/// lowered operation model. `op::RegisterWidth` re-exports it.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum RegisterWidth {
    W8,
    W16,
}

/// How wide an immediate operand is encoded.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ImmediateWidthClass {
    /// Always one byte (`SEP`/`REP` masks, the `BRK`/`COP` signature byte).
    Fixed8,
    /// Always two bytes (`PEA`).
    Fixed16,
    /// One byte with an 8-bit accumulator, two bytes with a 16-bit one.
    MDependent,
    /// One byte with 8-bit index registers, two bytes with 16-bit ones.
    XDependent,
}

impl ImmediateWidthClass {
    /// The largest value this class admits at `width`, or `None` when the
    /// class is width-dependent and the width is unknown.
    pub fn max_value(self, a: Option<RegisterWidth>, i: Option<RegisterWidth>) -> Option<u32> {
        let limit = |width: Option<RegisterWidth>| match width {
            Some(RegisterWidth::W8) => Some(0xFF),
            Some(RegisterWidth::W16) => Some(0xFFFF),
            None => None,
        };
        match self {
            Self::Fixed8 => Some(0xFF),
            Self::Fixed16 => Some(0xFFFF),
            Self::MDependent => limit(a),
            Self::XDependent => limit(i),
        }
    }
}

/// The shape of the operand as *written in K816 source*.
///
/// One form may map to several hardware modes: `OperandForm::Address` covers
/// direct, absolute, and absolute long, which K65 does not spell differently
/// (`doc/syntax-reference.md`, "Address-encoding (prefixes)"). Classification
/// therefore matches the form first and then selects the encoding from the
/// operand prefix and the value.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum OperandForm {
    /// No operand at all: `rts`, `pha`, `nop`.
    Nothing,
    /// The accumulator, written explicitly (`asl a`) or left out (`asl`).
    Accumulator,
    /// `#expr`.
    Immediate,
    /// A bare address expression: `lda addr`, and the branch targets
    /// `bcc label` / `brl label` / `per label`.
    Address,
    /// `expr,X`.
    AddressIndexedX,
    /// `expr,Y`.
    AddressIndexedY,
    /// `expr,S`.
    StackRelative,
    /// `(expr)`.
    Indirect,
    /// `(expr,X)`.
    IndirectIndexedX,
    /// `(expr),Y`.
    IndirectIndexedY,
    /// `(expr,S),Y`.
    StackRelativeIndirectIndexedY,
    /// `[expr]`.
    LongIndirect,
    /// `[expr],Y`.
    LongIndirectIndexedY,
    /// `src,dst` — the two-operand block moves.
    BlockMove,
}

/// The encoded width of a spec's operand field, which fixes the range K816
/// validates the folded value against.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum OperandWidth {
    /// No operand bytes are encoded.
    None,
    /// A constant operand, sized by its [`ImmediateWidthClass`].
    Immediate(ImmediateWidthClass),
    /// An address of the given encoding width.
    Address(AddressEncoding),
    /// Two bank bytes, source then destination.
    BankPair,
    /// A displacement ca65 computes from the target address. K816 never
    /// computes a branch offset, because it does not own placement
    /// (sub-plan C, backend decision 7).
    Displacement,
}

/// What a spec accepts in operand position, and how ca65 spells it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct OperandConstraint {
    /// The source operand shape this spec accepts.
    pub form: OperandForm,
    /// The encoded operand width, and therefore the legal value range.
    pub width: OperandWidth,
    /// Whether ca65 prints a `#` before the operand.
    ///
    /// Not derivable from the source form: K65 writes `pea #addr`,
    /// `brk #sig`, and `cop #sig` with a `#`, while ca65 spells all three
    /// without one, because on the hardware they are Stack-mode instructions
    /// rather than Immediate-mode ones.
    pub ca65_immediate_sigil: bool,
}

impl OperandConstraint {
    /// A form ca65 prints with a `#`.
    pub const fn immediate(form: OperandForm, width: ImmediateWidthClass) -> Self {
        Self {
            form,
            width: OperandWidth::Immediate(width),
            ca65_immediate_sigil: true,
        }
    }

    /// A form ca65 prints without a `#`.
    pub const fn plain(form: OperandForm, width: OperandWidth) -> Self {
        Self {
            form,
            width,
            ca65_immediate_sigil: false,
        }
    }
}

/// One legal (mnemonic, addressing mode) combination.
#[derive(Debug, PartialEq, Eq)]
pub struct InstructionSpec {
    pub mnemonic: Mnemonic,
    pub mode: AddressingMode,
    pub operand: OperandConstraint,
}

impl InstructionSpec {
    /// The immediate width class of this spec's operand, when it encodes a
    /// constant rather than an address, a displacement, or nothing.
    pub fn immediate_width(&self) -> Option<ImmediateWidthClass> {
        match self.operand.width {
            OperandWidth::Immediate(class) => Some(class),
            OperandWidth::None
            | OperandWidth::Address(_)
            | OperandWidth::BankPair
            | OperandWidth::Displacement => None,
        }
    }

    /// The address encoding this spec's operand uses, if it encodes one.
    pub fn encoding(&self) -> Option<AddressEncoding> {
        match self.operand.width {
            OperandWidth::Address(encoding) => Some(encoding),
            OperandWidth::None
            | OperandWidth::Immediate(_)
            | OperandWidth::BankPair
            | OperandWidth::Displacement => None,
        }
    }
}

/// Every legal combination the slice supports, per `doc/w65c816s.md`.
const SPECS: &[InstructionSpec] = &[
    InstructionSpec {
        mnemonic: Mnemonic::Lda,
        mode: AddressingMode::Immediate,
        operand: OperandConstraint::immediate(
            OperandForm::Immediate,
            ImmediateWidthClass::MDependent,
        ),
    },
    InstructionSpec {
        mnemonic: Mnemonic::Sta,
        mode: AddressingMode::Absolute,
        operand: OperandConstraint::plain(
            OperandForm::Address,
            OperandWidth::Address(AddressEncoding::Absolute),
        ),
    },
    InstructionSpec {
        mnemonic: Mnemonic::Rts,
        mode: AddressingMode::Stack,
        operand: OperandConstraint::plain(OperandForm::Nothing, OperandWidth::None),
    },
    InstructionSpec {
        mnemonic: Mnemonic::Sep,
        mode: AddressingMode::Immediate,
        operand: OperandConstraint::immediate(OperandForm::Immediate, ImmediateWidthClass::Fixed8),
    },
    InstructionSpec {
        mnemonic: Mnemonic::Rep,
        mode: AddressingMode::Immediate,
        operand: OperandConstraint::immediate(OperandForm::Immediate, ImmediateWidthClass::Fixed8),
    },
];

/// Looks up the specification for a (mnemonic, mode) pair; `None` means the
/// combination is illegal on the 65816 or not yet supported.
pub fn spec(mnemonic: Mnemonic, mode: AddressingMode) -> Option<&'static InstructionSpec> {
    SPECS
        .iter()
        .find(|spec| spec.mnemonic == mnemonic && spec.mode == mode)
}

/// Every spec of one mnemonic, in table order — the candidate set operand
/// classification narrows by matching [`OperandForm`].
pub fn specs_for(mnemonic: Mnemonic) -> impl Iterator<Item = &'static InstructionSpec> {
    SPECS.iter().filter(move |spec| spec.mnemonic == mnemonic)
}

/// All supported instruction specifications, for table-driven tests.
pub fn all_specs() -> &'static [InstructionSpec] {
    SPECS
}

/// The hardware mode of `mnemonic` written without an operand, or `None`
/// when the mnemonic always takes one.
///
/// WDC spreads the operandless instructions across `i` (implied), `s`
/// (stack), and `A` (accumulator), so the mode cannot be read off the absent
/// operand; it has to be looked up.
pub fn operandless_mode(mnemonic: Mnemonic) -> Option<AddressingMode> {
    specs_for(mnemonic)
        .find(|spec| {
            matches!(
                spec.operand.form,
                OperandForm::Nothing | OperandForm::Accumulator
            )
        })
        .map(|spec| spec.mode)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn every_spec_is_reachable_through_lookup() {
        for expected in all_specs() {
            assert_eq!(spec(expected.mnemonic, expected.mode), Some(expected));
        }
    }

    #[test]
    fn hardware_keys_are_unique() {
        let mut seen = std::collections::HashSet::new();
        for spec in all_specs() {
            assert!(
                seen.insert((spec.mnemonic, spec.mode)),
                "duplicate key {:?} {:?}",
                spec.mnemonic,
                spec.mode
            );
        }
    }

    #[test]
    fn immediate_width_is_present_exactly_for_constant_operands() {
        for spec in all_specs() {
            assert_eq!(
                spec.immediate_width().is_some(),
                spec.operand.form == OperandForm::Immediate,
                "{:?}",
                spec.mnemonic
            );
        }
    }

    #[test]
    fn mode_symbols_are_unique_across_the_twenty_four_modes() {
        let mut seen = std::collections::HashSet::new();
        for mode in AddressingMode::ALL {
            assert!(seen.insert(mode.symbol()), "duplicate symbol {mode:?}");
        }
        assert_eq!(seen.len(), 24);
    }

    #[test]
    fn address_modes_report_the_encoding_they_use() {
        assert_eq!(
            AddressingMode::Direct.encoding(),
            Some(AddressEncoding::Direct)
        );
        assert_eq!(
            AddressingMode::AbsoluteIndexedY.encoding(),
            Some(AddressEncoding::Absolute)
        );
        assert_eq!(
            AddressingMode::AbsoluteLongIndexedX.encoding(),
            Some(AddressEncoding::Long)
        );
        assert_eq!(AddressingMode::Stack.encoding(), None);
        assert_eq!(AddressingMode::Relative.encoding(), None);
    }

    #[test]
    fn immediate_width_limits_follow_the_tracked_widths() {
        use ImmediateWidthClass::*;
        assert_eq!(Fixed8.max_value(None, None), Some(0xFF));
        assert_eq!(Fixed16.max_value(None, None), Some(0xFFFF));
        assert_eq!(MDependent.max_value(None, None), None);
        assert_eq!(
            MDependent.max_value(Some(RegisterWidth::W16), None),
            Some(0xFFFF)
        );
        assert_eq!(
            XDependent.max_value(Some(RegisterWidth::W16), Some(RegisterWidth::W8)),
            Some(0xFF)
        );
    }

    #[test]
    fn illegal_combinations_are_rejected() {
        assert_eq!(spec(Mnemonic::Lda, AddressingMode::Implied), None);
        assert_eq!(spec(Mnemonic::Sta, AddressingMode::Immediate), None);
        assert_eq!(spec(Mnemonic::Rts, AddressingMode::Absolute), None);
        assert_eq!(spec(Mnemonic::Sep, AddressingMode::Absolute), None);
        assert_eq!(spec(Mnemonic::Rep, AddressingMode::Implied), None);
    }

    #[test]
    fn mnemonics_parse_case_insensitively_and_round_trip() {
        for spec in all_specs() {
            let spelling = spec.mnemonic.ca65();
            assert_eq!(Mnemonic::parse(spelling), Some(spec.mnemonic));
            assert_eq!(
                Mnemonic::parse(&spelling.to_ascii_uppercase()),
                Some(spec.mnemonic)
            );
        }
        assert_eq!(Mnemonic::parse("nop"), None);
        assert_eq!(Mnemonic::parse(""), None);
    }
}
