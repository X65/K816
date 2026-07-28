/*****************************************************************************/
/*                                                                           */
/*                                 effects.rs                                */
/*                                                                           */
/*            Conservative CPU effect model derived from the ISA             */
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

//! The effect model: which CPU state an instruction reads and writes.
//!
//! Every optimization must be justified by these explicit effects; unknown
//! effects inhibit optimization. Effects are derived on demand from the
//! (mnemonic, mode) pair — the same key as the `isa.rs` table — never copied
//! into individual operations, so they cannot silently become stale.
//! `doc/w65c816s.md` is the hardware authority.

use crate::isa::{AddressingMode, Mnemonic, RegisterWidth};

/// A set of CPU registers, as bits.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct RegisterSet(u8);

impl RegisterSet {
    pub const NONE: Self = Self(0);
    pub const A: Self = Self(1 << 0);
    pub const X: Self = Self(1 << 1);
    pub const Y: Self = Self(1 << 2);
    pub const S: Self = Self(1 << 3);
    pub const D: Self = Self(1 << 4);
    pub const DBR: Self = Self(1 << 5);
    pub const PBR: Self = Self(1 << 6);

    pub const fn union(self, other: Self) -> Self {
        Self(self.0 | other.0)
    }

    pub const fn intersects(self, other: Self) -> bool {
        self.0 & other.0 != 0
    }

    pub const fn contains(self, other: Self) -> bool {
        self.0 & other.0 == other.0
    }

    pub const fn is_empty(self) -> bool {
        self.0 == 0
    }

    /// The registers in `self` that are not in `other`.
    pub const fn without(self, other: Self) -> Self {
        Self(self.0 & !other.0)
    }
}

impl std::fmt::Display for RegisterSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let names = [
            (Self::A, "A"),
            (Self::X, "X"),
            (Self::Y, "Y"),
            (Self::S, "S"),
            (Self::D, "D"),
            (Self::DBR, "DBR"),
            (Self::PBR, "PBR"),
        ];
        let mut first = true;
        for (bit, name) in names {
            if self.contains(bit) {
                if !first {
                    f.write_str(" ")?;
                }
                f.write_str(name)?;
                first = false;
            }
        }
        if first {
            f.write_str("-")?;
        }
        Ok(())
    }
}

/// A set of processor status flags, as bits matching the P register layout
/// (`N V M X D I Z C`), plus the hidden emulation flag `E`.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct FlagSet(u16);

impl FlagSet {
    pub const NONE: Self = Self(0);
    pub const C: Self = Self(1 << 0);
    pub const Z: Self = Self(1 << 1);
    pub const I: Self = Self(1 << 2);
    pub const D: Self = Self(1 << 3);
    /// Index register width (65816 native mode).
    pub const X: Self = Self(1 << 4);
    /// Accumulator/memory width (65816 native mode).
    pub const M: Self = Self(1 << 5);
    pub const V: Self = Self(1 << 6);
    pub const N: Self = Self(1 << 7);
    /// Hidden emulation flag, outside the P byte.
    pub const E: Self = Self(1 << 8);

    /// All eight P-register flags (not `E`).
    pub const ALL_P: Self = Self(0xFF);

    /// The P-register flags selected by an 8-bit `SEP`/`REP` mask operand.
    pub const fn from_p_mask(mask: u8) -> Self {
        Self(mask as u16)
    }

    pub const fn union(self, other: Self) -> Self {
        Self(self.0 | other.0)
    }

    pub const fn intersects(self, other: Self) -> bool {
        self.0 & other.0 != 0
    }

    pub const fn contains(self, other: Self) -> bool {
        self.0 & other.0 == other.0
    }

    pub const fn is_empty(self) -> bool {
        self.0 == 0
    }

    /// The flags in `self` that are not in `other`.
    pub const fn without(self, other: Self) -> Self {
        Self(self.0 & !other.0)
    }
}

impl std::fmt::Display for FlagSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let names = [
            (Self::N, "N"),
            (Self::V, "V"),
            (Self::M, "M"),
            (Self::X, "X"),
            (Self::D, "D"),
            (Self::I, "I"),
            (Self::Z, "Z"),
            (Self::C, "C"),
            (Self::E, "E"),
        ];
        let mut first = true;
        for (bit, name) in names {
            if self.contains(bit) {
                if !first {
                    f.write_str(" ")?;
                }
                f.write_str(name)?;
                first = false;
            }
        }
        if first {
            f.write_str("-")?;
        }
        Ok(())
    }
}

/// Registers and flags together: the CPU state an instruction touches.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct CpuState {
    pub registers: RegisterSet,
    pub flags: FlagSet,
}

impl CpuState {
    pub const NONE: Self = Self {
        registers: RegisterSet::NONE,
        flags: FlagSet::NONE,
    };

    pub const fn new(registers: RegisterSet, flags: FlagSet) -> Self {
        Self { registers, flags }
    }

    pub const fn intersects(self, other: Self) -> bool {
        self.registers.intersects(other.registers) || self.flags.intersects(other.flags)
    }

    pub const fn is_empty(self) -> bool {
        self.registers.is_empty() && self.flags.is_empty()
    }

    /// The state in `self` not covered by `other`.
    pub const fn without(self, other: Self) -> Self {
        Self {
            registers: self.registers.without(other.registers),
            flags: self.flags.without(other.flags),
        }
    }
}

/// How an instruction interacts with memory.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MemoryEffect {
    None,
    Read,
    Write,
    ReadWrite,
    /// Pushes or pulls through the processor stack.
    Stack,
    /// Touches memory that must be treated as observable (memory-mapped I/O).
    Volatile,
    /// Cannot be classified; inhibits optimization.
    Unknown,
}

/// How an instruction affects control flow.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ControlEffect {
    /// Execution continues with the next operation.
    FallThrough,
    /// May or may not transfer control (conditional branch).
    Branch,
    /// Unconditionally transfers control.
    Jump,
    Call,
    Return,
    /// Software interrupt (`BRK`/`COP`).
    Trap,
    /// Stops the CPU (`STP`/`WAI`).
    Halt,
    /// Cannot be classified; inhibits optimization.
    Unknown,
}

/// What an instruction does to the M/X register widths.
///
/// A [`FlagSet`] cannot answer this: `FlagSet::M` says only that M was
/// written, not whether the accumulator became 8 or 16 bits wide, so `SEP`
/// and `REP` are indistinguishable through it. Every assembler width
/// directive comes from here instead, and [`WidthEffect::Unknown`] emits no
/// `.a…`/`.i…` claim at all rather than a false one.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum WidthEffect {
    /// Neither width changes.
    Unchanged,
    /// A known transition. `None` on an axis means that axis is untouched.
    Known {
        a: Option<RegisterWidth>,
        i: Option<RegisterWidth>,
    },
    /// The resulting widths cannot be known (`PLP`, `RTI`, `XCE`, or a
    /// `SEP`/`REP` mask that did not fold). Width reasoning stops here.
    Unknown,
}

impl WidthEffect {
    /// The known accumulator width after this instruction, if any.
    pub fn accumulator(self) -> Option<RegisterWidth> {
        match self {
            Self::Known { a, .. } => a,
            Self::Unchanged | Self::Unknown => None,
        }
    }

    /// The known index register width after this instruction, if any.
    pub fn index(self) -> Option<RegisterWidth> {
        match self {
            Self::Known { i, .. } => i,
            Self::Unchanged | Self::Unknown => None,
        }
    }

    /// True when the instruction leaves the tracked widths unusable.
    pub fn is_unknown(self) -> bool {
        self == Self::Unknown
    }

    /// The transition a `SEP`/`REP` mask performs. `mask` is `None` when the
    /// operand did not fold, which makes the transition unknown rather than
    /// absent: the instruction still executes.
    pub fn from_p_mask(mask: Option<u8>, width: RegisterWidth) -> Self {
        let Some(mask) = mask else {
            return Self::Unknown;
        };
        let selected = |bit: u8| (mask & bit != 0).then_some(width);
        match (selected(0x20), selected(0x10)) {
            (None, None) => Self::Unchanged,
            (a, i) => Self::Known { a, i },
        }
    }
}

/// The complete conservative effect description of one instruction.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Effects {
    pub reads: CpuState,
    pub writes: CpuState,
    pub memory: MemoryEffect,
    pub control: ControlEffect,
    /// The M/X width transition, which `writes.flags` cannot express.
    pub width: WidthEffect,
}

impl Effects {
    /// True when removing this operation can be justified at all: it neither
    /// touches memory observably nor changes control flow.
    pub fn is_removal_candidate(&self) -> bool {
        self.memory == MemoryEffect::None && self.control == ControlEffect::FallThrough
    }
}

/// Derives the effects of one instruction.
///
/// `immediate` is the operand value when statically known; `SEP`/`REP` use it
/// to narrow their flag writes to the actual mask. An unknown mask is treated
/// conservatively as writing all P flags.
///
/// The match is exhaustive over [`Mnemonic`] without wildcards, so adding an
/// instruction without describing its effects fails to compile.
pub fn instruction_effects(
    mnemonic: Mnemonic,
    mode: AddressingMode,
    immediate: Option<u32>,
) -> Effects {
    match mnemonic {
        // LDA loads A and sets N/Z from the value; the operand size of the
        // immediate form depends on the M flag. Only the memory-addressed
        // modes read memory — `lda #imm` reads none.
        Mnemonic::Lda => Effects {
            reads: CpuState::new(RegisterSet::NONE, FlagSet::M),
            writes: CpuState::new(RegisterSet::A, FlagSet::N.union(FlagSet::Z)),
            memory: if mode == AddressingMode::Immediate {
                MemoryEffect::None
            } else {
                MemoryEffect::Read
            },
            control: ControlEffect::FallThrough,
            width: WidthEffect::Unchanged,
        },
        // STA stores A; the store width depends on M, and absolute
        // addressing goes through the data bank register.
        Mnemonic::Sta => Effects {
            reads: CpuState::new(RegisterSet::A.union(RegisterSet::DBR), FlagSet::M),
            writes: CpuState::NONE,
            memory: MemoryEffect::Write,
            control: ControlEffect::FallThrough,
            width: WidthEffect::Unchanged,
        },
        // RTS pulls the return address from the stack.
        Mnemonic::Rts => Effects {
            reads: CpuState::new(RegisterSet::S, FlagSet::NONE),
            writes: CpuState::new(RegisterSet::S, FlagSet::NONE),
            memory: MemoryEffect::Stack,
            control: ControlEffect::Return,
            width: WidthEffect::Unchanged,
        },
        // SEP/REP set/clear exactly the P flags in the mask operand. The
        // flag set records *which* bits were written; the width effect
        // records what M and X became, which is the part `SEP` and `REP`
        // disagree about.
        Mnemonic::Sep | Mnemonic::Rep => {
            let mask = immediate.map(|mask| mask as u8);
            let written = match mask {
                Some(mask) => FlagSet::from_p_mask(mask),
                None => FlagSet::ALL_P,
            };
            let width = match mnemonic {
                Mnemonic::Sep => RegisterWidth::W8,
                Mnemonic::Rep => RegisterWidth::W16,
                Mnemonic::Lda | Mnemonic::Sta | Mnemonic::Rts => unreachable!(),
            };
            Effects {
                reads: CpuState::NONE,
                writes: CpuState::new(RegisterSet::NONE, written),
                memory: MemoryEffect::None,
                control: ControlEffect::FallThrough,
                width: WidthEffect::from_p_mask(mask, width),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::isa::all_specs;

    #[test]
    fn every_supported_spec_has_derivable_effects() {
        for spec in all_specs() {
            let effects = instruction_effects(spec.mnemonic, spec.mode, Some(0x20));
            // Conservative sanity: control classification exists for all.
            assert!(matches!(
                effects.control,
                ControlEffect::FallThrough | ControlEffect::Return
            ));
        }
    }

    #[test]
    fn lda_immediate_writes_a_and_nz_and_reads_m() {
        let effects = instruction_effects(Mnemonic::Lda, AddressingMode::Immediate, Some(1));
        assert!(effects.writes.registers.contains(RegisterSet::A));
        assert!(effects.writes.flags.contains(FlagSet::N.union(FlagSet::Z)));
        assert!(effects.reads.flags.contains(FlagSet::M));
        assert!(effects.is_removal_candidate());
    }

    #[test]
    fn sta_absolute_is_an_observable_write() {
        let effects = instruction_effects(Mnemonic::Sta, AddressingMode::Absolute, None);
        assert!(effects.reads.registers.contains(RegisterSet::A));
        assert_eq!(effects.memory, MemoryEffect::Write);
        assert!(!effects.is_removal_candidate());
    }

    #[test]
    fn rts_is_a_stack_using_return_barrier() {
        let effects = instruction_effects(Mnemonic::Rts, AddressingMode::Stack, None);
        assert_eq!(effects.control, ControlEffect::Return);
        assert_eq!(effects.memory, MemoryEffect::Stack);
        assert!(!effects.is_removal_candidate());
    }

    #[test]
    fn memory_effects_are_mode_sensitive() {
        // `lda #imm` touches no memory; every addressed form reads it.
        assert_eq!(
            instruction_effects(Mnemonic::Lda, AddressingMode::Immediate, Some(1)).memory,
            MemoryEffect::None
        );
        assert_eq!(
            instruction_effects(Mnemonic::Lda, AddressingMode::Absolute, None).memory,
            MemoryEffect::Read
        );
    }

    #[test]
    fn width_effects_distinguish_sep_from_rep() {
        let sep = instruction_effects(Mnemonic::Sep, AddressingMode::Immediate, Some(0x20));
        let rep = instruction_effects(Mnemonic::Rep, AddressingMode::Immediate, Some(0x20));
        // Both write M; only the width effect says which width followed.
        assert_eq!(sep.writes.flags, rep.writes.flags);
        assert_eq!(sep.width.accumulator(), Some(RegisterWidth::W8));
        assert_eq!(rep.width.accumulator(), Some(RegisterWidth::W16));
        assert_eq!(sep.width.index(), None);

        let both = instruction_effects(Mnemonic::Rep, AddressingMode::Immediate, Some(0x30));
        assert_eq!(both.width.accumulator(), Some(RegisterWidth::W16));
        assert_eq!(both.width.index(), Some(RegisterWidth::W16));
    }

    #[test]
    fn a_mask_that_touches_no_width_flag_changes_no_width() {
        let effects = instruction_effects(Mnemonic::Sep, AddressingMode::Immediate, Some(0x01));
        assert_eq!(effects.width, WidthEffect::Unchanged);
        assert_eq!(effects.writes.flags, FlagSet::C);
    }

    #[test]
    fn an_unfolded_mask_makes_the_width_unknown_not_unchanged() {
        let effects = instruction_effects(Mnemonic::Rep, AddressingMode::Immediate, None);
        assert!(effects.width.is_unknown());
        assert_eq!(effects.width.accumulator(), None);
    }

    #[test]
    fn instructions_that_do_not_touch_p_leave_the_widths_alone() {
        for (mnemonic, mode) in [
            (Mnemonic::Lda, AddressingMode::Immediate),
            (Mnemonic::Sta, AddressingMode::Absolute),
            (Mnemonic::Rts, AddressingMode::Stack),
        ] {
            assert_eq!(
                instruction_effects(mnemonic, mode, None).width,
                WidthEffect::Unchanged,
                "{mnemonic:?}"
            );
        }
    }

    #[test]
    fn sep_rep_write_exactly_the_masked_flags() {
        let sep = instruction_effects(Mnemonic::Sep, AddressingMode::Immediate, Some(0x20));
        assert_eq!(sep.writes.flags, FlagSet::M);
        let rep = instruction_effects(Mnemonic::Rep, AddressingMode::Immediate, Some(0x10));
        assert_eq!(rep.writes.flags, FlagSet::X);
        let both = instruction_effects(Mnemonic::Rep, AddressingMode::Immediate, Some(0x30));
        assert_eq!(both.writes.flags, FlagSet::M.union(FlagSet::X));
    }

    #[test]
    fn unknown_sep_rep_mask_is_conservative() {
        let effects = instruction_effects(Mnemonic::Sep, AddressingMode::Immediate, None);
        assert_eq!(effects.writes.flags, FlagSet::ALL_P);
        assert!(!effects.writes.flags.contains(FlagSet::E));
    }

    #[test]
    fn set_displays_are_stable() {
        assert_eq!(RegisterSet::NONE.to_string(), "-");
        assert_eq!(RegisterSet::A.union(RegisterSet::S).to_string(), "A S");
        assert_eq!(FlagSet::N.union(FlagSet::Z).to_string(), "N Z");
        assert_eq!(FlagSet::M.union(FlagSet::X).to_string(), "M X");
    }
}
