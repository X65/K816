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
//! Only the mnemonics the vertical slice lowers to are populated; the enum
//! shapes are the frozen contract and entries grow in sub-plans C/F. K816
//! never encodes opcodes — ca65 owns encoding — so an [`InstructionSpec`]
//! describes legality, immediate width class, and ca65 spelling only.
//! CPU effects are derived from this table in `effects.rs`, keyed by the
//! same (mnemonic, mode) pair, so they cannot silently go stale.
//! `doc/w65c816s.md` is the hardware authority behind every entry.

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

/// A typed addressing mode. The slice needs three; the full 65816 set grows
/// here in sub-plan C.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum AddressingMode {
    Implied,
    Immediate,
    Absolute,
}

/// How wide an immediate operand is encoded.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ImmediateWidthClass {
    /// Always one byte (e.g. `SEP`/`REP` masks).
    Fixed8,
    /// One byte with an 8-bit accumulator, two bytes with a 16-bit one.
    MDependent,
    /// One byte with 8-bit index registers, two bytes with 16-bit ones.
    XDependent,
}

/// One legal (mnemonic, addressing mode) combination.
#[derive(Debug, PartialEq, Eq)]
pub struct InstructionSpec {
    pub mnemonic: Mnemonic,
    pub mode: AddressingMode,
    /// `Some` exactly when `mode` is [`AddressingMode::Immediate`].
    pub immediate_width: Option<ImmediateWidthClass>,
}

/// Every legal combination the slice supports, per `doc/w65c816s.md`.
const SPECS: &[InstructionSpec] = &[
    InstructionSpec {
        mnemonic: Mnemonic::Lda,
        mode: AddressingMode::Immediate,
        immediate_width: Some(ImmediateWidthClass::MDependent),
    },
    InstructionSpec {
        mnemonic: Mnemonic::Sta,
        mode: AddressingMode::Absolute,
        immediate_width: None,
    },
    InstructionSpec {
        mnemonic: Mnemonic::Rts,
        mode: AddressingMode::Implied,
        immediate_width: None,
    },
    InstructionSpec {
        mnemonic: Mnemonic::Sep,
        mode: AddressingMode::Immediate,
        immediate_width: Some(ImmediateWidthClass::Fixed8),
    },
    InstructionSpec {
        mnemonic: Mnemonic::Rep,
        mode: AddressingMode::Immediate,
        immediate_width: Some(ImmediateWidthClass::Fixed8),
    },
];

/// Looks up the specification for a (mnemonic, mode) pair; `None` means the
/// combination is illegal on the 65816 or not yet supported.
pub fn spec(mnemonic: Mnemonic, mode: AddressingMode) -> Option<&'static InstructionSpec> {
    SPECS
        .iter()
        .find(|spec| spec.mnemonic == mnemonic && spec.mode == mode)
}

/// All supported instruction specifications, for table-driven tests.
pub fn all_specs() -> &'static [InstructionSpec] {
    SPECS
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
    fn immediate_width_is_present_exactly_for_immediate_mode() {
        for spec in all_specs() {
            assert_eq!(
                spec.immediate_width.is_some(),
                spec.mode == AddressingMode::Immediate,
                "{:?}",
                spec.mnemonic
            );
        }
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
