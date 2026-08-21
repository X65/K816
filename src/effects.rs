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
    /// Every register the effect model names.
    pub const ALL: Self = Self(0x7F);
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
    /// Every flag, the hidden emulation flag included.
    pub const ALL: Self = Self(0x1FF);

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

    /// Every register and every flag.
    ///
    /// This is the optimizer's live-out seed at a function exit, and its
    /// answer at every barrier. An exit block has no successors, so liveness
    /// needs a seed there and the seed is not "nothing" — without one, dead
    /// register elimination deletes return values. A declared `-> a` is not a
    /// licence to narrow it: `call foo` opts out of contract checking, a
    /// hand-written `jsr foo` never had a contract, and an undeclared clobber
    /// is a warning rather than a guarantee. Flags cannot be narrowed even in
    /// principle, because the contract syntax has no way to name one
    /// (`plans/05-optimizer.md` decision 12).
    pub const ALL: Self = Self {
        registers: RegisterSet::ALL,
        flags: FlagSet::ALL,
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

    /// The state either side touches.
    pub const fn union(self, other: Self) -> Self {
        Self {
            registers: self.registers.union(other.registers),
            flags: self.flags.union(other.flags),
        }
    }

    /// The state in both.
    pub const fn intersection(self, other: Self) -> Self {
        self.without(other.complement())
    }

    /// Everything this state does not name.
    pub const fn complement(self) -> Self {
        Self::ALL.without(self)
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
/// to narrow their effects to the actual mask.
///
/// The match is exhaustive over [`Mnemonic`] without wildcards, so adding an
/// instruction without describing its effects fails to compile. Exhaustiveness
/// is not the whole gate, though: memory and register reads are
/// *mode-sensitive*, and a table that answered the same way for `lda #imm`
/// and `lda addr` would be exhaustive and wrong.
///
/// # What the two state sets promise
///
/// `reads` is a **may-read** set: over-approximating it keeps a value alive
/// that might have been dead, which costs an instruction and never breaks a
/// program. `writes` is a **must-write** set: the optimizer subtracts it from
/// the state a candidate produced, so over-approximating it would declare a
/// live value dead. `SEP`/`REP` with an unfolded mask therefore write
/// *nothing* — the mask may be zero — while reading every P flag and
/// reporting [`WidthEffect::Unknown`], which is the signal that stops the
/// scan entirely.
pub fn instruction_effects(
    mnemonic: Mnemonic,
    mode: AddressingMode,
    immediate: Option<u32>,
) -> Effects {
    use ControlEffect as C;
    use MemoryEffect as M;

    let nz = FlagSet::N.union(FlagSet::Z);
    let nzc = nz.union(FlagSet::C);
    let nvzc = nzc.union(FlagSet::V);
    // The address-forming registers this mode reads, which is the part of an
    // effect a mnemonic alone cannot answer.
    let addressing = mode_registers(mode);
    // A load-style instruction touches memory in every mode but immediate.
    let loads = if mode == AddressingMode::Immediate {
        M::None
    } else {
        M::Read
    };
    // A read-modify-write instruction touches memory in every mode but
    // accumulator, where it rewrites A instead.
    let modifies = if mode == AddressingMode::Accumulator {
        M::None
    } else {
        M::ReadWrite
    };
    let on_accumulator = mode == AddressingMode::Accumulator;

    /// Reads and writes the given state, with the given memory and control
    /// effects and no width transition.
    fn make(
        reads: CpuState,
        writes: CpuState,
        memory: MemoryEffect,
        control: ControlEffect,
    ) -> Effects {
        Effects {
            reads,
            writes,
            memory,
            control,
            width: WidthEffect::Unchanged,
        }
    }
    let state = CpuState::new;
    let regs = |registers| state(registers, FlagSet::NONE);
    let flags = |flags| state(RegisterSet::NONE, flags);

    match mnemonic {
        // -------------------------------------------------------------- ALU
        // ADC and SBC read the carry and the decimal flag and write all four
        // arithmetic flags; the accumulator's width comes from M.
        Mnemonic::Adc | Mnemonic::Sbc => make(
            state(
                RegisterSet::A.union(addressing),
                FlagSet::M.union(FlagSet::C).union(FlagSet::D),
            ),
            state(RegisterSet::A, nvzc),
            loads,
            C::FallThrough,
        ),
        Mnemonic::And | Mnemonic::Eor | Mnemonic::Ora => make(
            state(RegisterSet::A.union(addressing), FlagSet::M),
            state(RegisterSet::A, nz),
            loads,
            C::FallThrough,
        ),
        // The compares write flags only.
        Mnemonic::Cmp => make(
            state(RegisterSet::A.union(addressing), FlagSet::M),
            flags(nzc),
            loads,
            C::FallThrough,
        ),
        Mnemonic::Cpx => make(
            state(RegisterSet::X.union(addressing), FlagSet::X),
            flags(nzc),
            loads,
            C::FallThrough,
        ),
        Mnemonic::Cpy => make(
            state(RegisterSet::Y.union(addressing), FlagSet::X),
            flags(nzc),
            loads,
            C::FallThrough,
        ),
        // `doc/w65c816s.md` Table 5-5 note 1: the immediate form of BIT
        // affects Z only, while every addressed form also takes N and V from
        // the operand's top bits. Claiming N and V for `bit #imm` would let
        // the optimizer delete a live flag write.
        Mnemonic::Bit => make(
            state(RegisterSet::A.union(addressing), FlagSet::M),
            flags(if mode == AddressingMode::Immediate {
                FlagSet::Z
            } else {
                nz.union(FlagSet::V)
            }),
            loads,
            C::FallThrough,
        ),
        // ------------------------------------------------------- loads/stores
        Mnemonic::Lda => make(
            state(addressing, FlagSet::M),
            state(RegisterSet::A, nz),
            loads,
            C::FallThrough,
        ),
        Mnemonic::Ldx => make(
            state(addressing, FlagSet::X),
            state(RegisterSet::X, nz),
            loads,
            C::FallThrough,
        ),
        Mnemonic::Ldy => make(
            state(addressing, FlagSet::X),
            state(RegisterSet::Y, nz),
            loads,
            C::FallThrough,
        ),
        Mnemonic::Sta => make(
            state(RegisterSet::A.union(addressing), FlagSet::M),
            CpuState::NONE,
            M::Write,
            C::FallThrough,
        ),
        Mnemonic::Stx => make(
            state(RegisterSet::X.union(addressing), FlagSet::X),
            CpuState::NONE,
            M::Write,
            C::FallThrough,
        ),
        Mnemonic::Sty => make(
            state(RegisterSet::Y.union(addressing), FlagSet::X),
            CpuState::NONE,
            M::Write,
            C::FallThrough,
        ),
        // STZ stores a constant zero, so it reads no register but the ones
        // that form the address — and M, which sizes the store.
        Mnemonic::Stz => make(
            state(addressing, FlagSet::M),
            CpuState::NONE,
            M::Write,
            C::FallThrough,
        ),
        // -------------------------------------------- shifts, rotates, steps
        Mnemonic::Asl | Mnemonic::Lsr => make(
            state(
                if on_accumulator {
                    RegisterSet::A
                } else {
                    addressing
                },
                FlagSet::M,
            ),
            state(
                if on_accumulator {
                    RegisterSet::A
                } else {
                    RegisterSet::NONE
                },
                nzc,
            ),
            modifies,
            C::FallThrough,
        ),
        // The rotates additionally shift the carry in.
        Mnemonic::Rol | Mnemonic::Ror => make(
            state(
                if on_accumulator {
                    RegisterSet::A
                } else {
                    addressing
                },
                FlagSet::M.union(FlagSet::C),
            ),
            state(
                if on_accumulator {
                    RegisterSet::A
                } else {
                    RegisterSet::NONE
                },
                nzc,
            ),
            modifies,
            C::FallThrough,
        ),
        Mnemonic::Inc | Mnemonic::Dec => make(
            state(
                if on_accumulator {
                    RegisterSet::A
                } else {
                    addressing
                },
                FlagSet::M,
            ),
            state(
                if on_accumulator {
                    RegisterSet::A
                } else {
                    RegisterSet::NONE
                },
                nz,
            ),
            modifies,
            C::FallThrough,
        ),
        Mnemonic::Inx | Mnemonic::Dex => make(
            state(RegisterSet::X, FlagSet::X),
            state(RegisterSet::X, nz),
            M::None,
            C::FallThrough,
        ),
        Mnemonic::Iny | Mnemonic::Dey => make(
            state(RegisterSet::Y, FlagSet::X),
            state(RegisterSet::Y, nz),
            M::None,
            C::FallThrough,
        ),
        // TSB and TRB set or clear the accumulator's bits in memory and
        // report only whether the tested bits were all zero.
        Mnemonic::Tsb | Mnemonic::Trb => make(
            state(RegisterSet::A.union(addressing), FlagSet::M),
            flags(FlagSet::Z),
            M::ReadWrite,
            C::FallThrough,
        ),
        // ----------------------------------------------------------- branches
        Mnemonic::Bcc | Mnemonic::Bcs => {
            make(flags(FlagSet::C), CpuState::NONE, M::None, C::Branch)
        }
        Mnemonic::Beq | Mnemonic::Bne => {
            make(flags(FlagSet::Z), CpuState::NONE, M::None, C::Branch)
        }
        Mnemonic::Bmi | Mnemonic::Bpl => {
            make(flags(FlagSet::N), CpuState::NONE, M::None, C::Branch)
        }
        Mnemonic::Bvc | Mnemonic::Bvs => {
            make(flags(FlagSet::V), CpuState::NONE, M::None, C::Branch)
        }
        Mnemonic::Bra | Mnemonic::Brl => make(CpuState::NONE, CpuState::NONE, M::None, C::Jump),
        // ------------------------------------------------------- control flow
        // The indirect jumps read their pointer out of memory.
        Mnemonic::Jmp | Mnemonic::Jml => make(
            regs(addressing),
            CpuState::NONE,
            match mode {
                AddressingMode::AbsoluteIndirect | AddressingMode::AbsoluteIndexedIndirect => {
                    M::Read
                }
                _ => M::None,
            },
            C::Jump,
        ),
        Mnemonic::Jsr => make(
            state(
                RegisterSet::S.union(RegisterSet::PBR).union(addressing),
                FlagSet::NONE,
            ),
            regs(RegisterSet::S),
            M::Stack,
            C::Call,
        ),
        // JSL pushes the program bank as well, and loads a new one.
        Mnemonic::Jsl => make(
            regs(RegisterSet::S.union(RegisterSet::PBR)),
            regs(RegisterSet::S.union(RegisterSet::PBR)),
            M::Stack,
            C::Call,
        ),
        Mnemonic::Rts => make(
            regs(RegisterSet::S),
            regs(RegisterSet::S),
            M::Stack,
            C::Return,
        ),
        Mnemonic::Rtl => make(
            regs(RegisterSet::S),
            regs(RegisterSet::S.union(RegisterSet::PBR)),
            M::Stack,
            C::Return,
        ),
        // RTI restores the whole status byte, so the widths that follow are
        // whatever the interrupted code had. That is unknowable here, and an
        // 8-bit index width would have zeroed the high bytes of X and Y.
        Mnemonic::Rti => Effects {
            reads: regs(RegisterSet::S),
            writes: state(
                RegisterSet::S
                    .union(RegisterSet::PBR)
                    .union(RegisterSet::X)
                    .union(RegisterSet::Y),
                FlagSet::ALL_P,
            ),
            memory: M::Stack,
            control: C::Return,
            width: WidthEffect::Unknown,
        },
        // BRK and COP push the status byte, disable interrupts, and clear
        // decimal mode; the widths are untouched.
        Mnemonic::Brk | Mnemonic::Cop => make(
            state(RegisterSet::S.union(RegisterSet::PBR), FlagSet::ALL_P),
            state(
                RegisterSet::S.union(RegisterSet::PBR),
                FlagSet::I.union(FlagSet::D),
            ),
            M::Stack,
            C::Trap,
        ),
        Mnemonic::Wai | Mnemonic::Stp => make(CpuState::NONE, CpuState::NONE, M::None, C::Halt),
        // ------------------------------------------------------------- stack
        Mnemonic::Pha => make(
            state(RegisterSet::A.union(RegisterSet::S), FlagSet::M),
            regs(RegisterSet::S),
            M::Stack,
            C::FallThrough,
        ),
        Mnemonic::Phx => make(
            state(RegisterSet::X.union(RegisterSet::S), FlagSet::X),
            regs(RegisterSet::S),
            M::Stack,
            C::FallThrough,
        ),
        Mnemonic::Phy => make(
            state(RegisterSet::Y.union(RegisterSet::S), FlagSet::X),
            regs(RegisterSet::S),
            M::Stack,
            C::FallThrough,
        ),
        Mnemonic::Phb => make(
            regs(RegisterSet::DBR.union(RegisterSet::S)),
            regs(RegisterSet::S),
            M::Stack,
            C::FallThrough,
        ),
        Mnemonic::Phd => make(
            regs(RegisterSet::D.union(RegisterSet::S)),
            regs(RegisterSet::S),
            M::Stack,
            C::FallThrough,
        ),
        Mnemonic::Phk => make(
            regs(RegisterSet::PBR.union(RegisterSet::S)),
            regs(RegisterSet::S),
            M::Stack,
            C::FallThrough,
        ),
        Mnemonic::Php => make(
            state(RegisterSet::S, FlagSet::ALL_P),
            regs(RegisterSet::S),
            M::Stack,
            C::FallThrough,
        ),
        Mnemonic::Pla => make(
            state(RegisterSet::S, FlagSet::M),
            state(RegisterSet::A.union(RegisterSet::S), nz),
            M::Stack,
            C::FallThrough,
        ),
        Mnemonic::Plx => make(
            state(RegisterSet::S, FlagSet::X),
            state(RegisterSet::X.union(RegisterSet::S), nz),
            M::Stack,
            C::FallThrough,
        ),
        Mnemonic::Ply => make(
            state(RegisterSet::S, FlagSet::X),
            state(RegisterSet::Y.union(RegisterSet::S), nz),
            M::Stack,
            C::FallThrough,
        ),
        Mnemonic::Plb => make(
            regs(RegisterSet::S),
            state(RegisterSet::DBR.union(RegisterSet::S), nz),
            M::Stack,
            C::FallThrough,
        ),
        Mnemonic::Pld => make(
            regs(RegisterSet::S),
            state(RegisterSet::D.union(RegisterSet::S), nz),
            M::Stack,
            C::FallThrough,
        ),
        // PLP pulls a whole status byte, so M and X become whatever was
        // pushed — and an 8-bit index width zeroes the high bytes of X and Y.
        Mnemonic::Plp => Effects {
            reads: regs(RegisterSet::S),
            writes: state(
                RegisterSet::S.union(RegisterSet::X).union(RegisterSet::Y),
                FlagSet::ALL_P,
            ),
            memory: M::Stack,
            control: C::FallThrough,
            width: WidthEffect::Unknown,
        },
        // The push-effective family computes a value and pushes it; only PEI
        // reads memory to do so, through the direct page register.
        Mnemonic::Pea => make(
            regs(RegisterSet::S),
            regs(RegisterSet::S),
            M::Stack,
            C::FallThrough,
        ),
        Mnemonic::Pei => make(
            regs(RegisterSet::S.union(RegisterSet::D)),
            regs(RegisterSet::S),
            M::Stack,
            C::FallThrough,
        ),
        Mnemonic::Per => make(
            regs(RegisterSet::S.union(RegisterSet::PBR)),
            regs(RegisterSet::S),
            M::Stack,
            C::FallThrough,
        ),
        // --------------------------------------------------------- block move
        // MVN and MVP move C+1 bytes, stepping X and Y and counting C down,
        // and load the destination bank byte into DBR.
        Mnemonic::Mvn | Mnemonic::Mvp => make(
            regs(
                RegisterSet::A
                    .union(RegisterSet::X)
                    .union(RegisterSet::Y)
                    .union(RegisterSet::DBR),
            ),
            regs(
                RegisterSet::A
                    .union(RegisterSet::X)
                    .union(RegisterSet::Y)
                    .union(RegisterSet::DBR),
            ),
            M::ReadWrite,
            C::FallThrough,
        ),
        // ----------------------------------------------------------- transfers
        Mnemonic::Tax => make(
            state(RegisterSet::A, FlagSet::X),
            state(RegisterSet::X, nz),
            M::None,
            C::FallThrough,
        ),
        Mnemonic::Tay => make(
            state(RegisterSet::A, FlagSet::X),
            state(RegisterSet::Y, nz),
            M::None,
            C::FallThrough,
        ),
        Mnemonic::Txa => make(
            state(RegisterSet::X, FlagSet::M),
            state(RegisterSet::A, nz),
            M::None,
            C::FallThrough,
        ),
        Mnemonic::Tya => make(
            state(RegisterSet::Y, FlagSet::M),
            state(RegisterSet::A, nz),
            M::None,
            C::FallThrough,
        ),
        Mnemonic::Txy => make(
            state(RegisterSet::X, FlagSet::X),
            state(RegisterSet::Y, nz),
            M::None,
            C::FallThrough,
        ),
        Mnemonic::Tyx => make(
            state(RegisterSet::Y, FlagSet::X),
            state(RegisterSet::X, nz),
            M::None,
            C::FallThrough,
        ),
        Mnemonic::Tsx => make(
            state(RegisterSet::S, FlagSet::X),
            state(RegisterSet::X, nz),
            M::None,
            C::FallThrough,
        ),
        // The two transfers into the stack pointer are the full-width ones
        // that set no flags at all.
        Mnemonic::Txs => make(
            regs(RegisterSet::X),
            regs(RegisterSet::S),
            M::None,
            C::FallThrough,
        ),
        Mnemonic::Tcs => make(
            regs(RegisterSet::A),
            regs(RegisterSet::S),
            M::None,
            C::FallThrough,
        ),
        // The remaining 65816 transfers move all 16 bits regardless of width.
        Mnemonic::Tcd => make(
            regs(RegisterSet::A),
            state(RegisterSet::D, nz),
            M::None,
            C::FallThrough,
        ),
        Mnemonic::Tdc => make(
            regs(RegisterSet::D),
            state(RegisterSet::A, nz),
            M::None,
            C::FallThrough,
        ),
        Mnemonic::Tsc => make(
            regs(RegisterSet::S),
            state(RegisterSet::A, nz),
            M::None,
            C::FallThrough,
        ),
        Mnemonic::Xba => make(
            regs(RegisterSet::A),
            state(RegisterSet::A, nz),
            M::None,
            C::FallThrough,
        ),
        // ------------------------------------------------------- flags & mode
        Mnemonic::Clc | Mnemonic::Sec => {
            make(CpuState::NONE, flags(FlagSet::C), M::None, C::FallThrough)
        }
        Mnemonic::Cld | Mnemonic::Sed => {
            make(CpuState::NONE, flags(FlagSet::D), M::None, C::FallThrough)
        }
        Mnemonic::Cli | Mnemonic::Sei => {
            make(CpuState::NONE, flags(FlagSet::I), M::None, C::FallThrough)
        }
        Mnemonic::Clv => make(CpuState::NONE, flags(FlagSet::V), M::None, C::FallThrough),
        // SEP and REP set or clear exactly the flags in their mask. With the
        // mask unknown the *must-write* set is empty — the mask may be zero —
        // while the width transition is Unknown, which is what stops the
        // optimizer rather than a pretend flag write.
        //
        // The read set is every P flag the mask does *not* write. The
        // instruction preserves those bits and decides nothing from the ones
        // it sets, so `ALL_P` would say a `sep #$20` depends on M — which
        // makes it an observer of the very axis it establishes, and a
        // dead-mode pass seeded from flag reads could then never fire
        // (`plans/05-optimizer.md` probe 5). Still an over-approximation, and
        // an unfolded mask still reads everything, because it writes nothing.
        Mnemonic::Sep | Mnemonic::Rep => {
            let mask = immediate.map(|mask| mask as u8);
            let to = match mnemonic {
                Mnemonic::Sep => RegisterWidth::W8,
                _ => RegisterWidth::W16,
            };
            let width = WidthEffect::from_p_mask(mask, to);
            // Narrowing the index registers zeroes their high bytes; every
            // other transition leaves the register contents alone.
            let truncated = if width.index() == Some(RegisterWidth::W8) {
                RegisterSet::X.union(RegisterSet::Y)
            } else {
                RegisterSet::NONE
            };
            let written = mask.map_or(FlagSet::NONE, FlagSet::from_p_mask);
            Effects {
                reads: CpuState::NONE,
                writes: state(truncated, written),
                memory: M::None,
                control: C::FallThrough,
                width,
            }
        }
        // XCE swaps the carry with the hidden emulation flag. Entering
        // emulation forces 8-bit registers and a page-1 stack, so the widths
        // and the high bytes of X, Y, and S all become unknowable here.
        Mnemonic::Xce => Effects {
            reads: flags(FlagSet::C.union(FlagSet::E)),
            writes: state(RegisterSet::NONE, FlagSet::C.union(FlagSet::E)),
            memory: M::None,
            control: C::FallThrough,
            width: WidthEffect::Unknown,
        },
        // ------------------------------------------------------------ no-ops
        Mnemonic::Nop | Mnemonic::Wdm => {
            make(CpuState::NONE, CpuState::NONE, M::None, C::FallThrough)
        }
    }
}

/// The registers an addressing mode reads to form its effective address.
///
/// This is the half of an effect the mnemonic cannot supply: `lda $10,x`
/// reads the direct page register and X, `lda $1234` reads the data bank
/// register, and `lda #1` reads neither.
fn mode_registers(mode: AddressingMode) -> RegisterSet {
    use AddressingMode as A;
    let d = RegisterSet::D;
    let x = RegisterSet::X;
    let y = RegisterSet::Y;
    let s = RegisterSet::S;
    let dbr = RegisterSet::DBR;
    match mode {
        A::Direct => d,
        A::DirectIndexedX => d.union(x),
        A::DirectIndexedY => d.union(y),
        A::DirectIndexedIndirect => d.union(x).union(dbr),
        A::DirectIndirect => d.union(dbr),
        A::DirectIndirectIndexed => d.union(y).union(dbr),
        // The long indirect forms carry their own bank byte, so they do not
        // read DBR.
        A::DirectIndirectLong => d,
        A::DirectIndirectLongIndexed => d.union(y),
        A::StackRelative => s,
        A::StackRelativeIndirectIndexed => s.union(y).union(dbr),
        A::Absolute => dbr,
        A::AbsoluteIndexedX => dbr.union(x),
        A::AbsoluteIndexedY => dbr.union(y),
        A::AbsoluteLong => RegisterSet::NONE,
        A::AbsoluteLongIndexedX => x,
        // The absolute indirect pointers live in bank 0; the indexed form
        // jumps within the current program bank.
        A::AbsoluteIndirect => RegisterSet::NONE,
        A::AbsoluteIndexedIndirect => x.union(RegisterSet::PBR),
        A::Accumulator
        | A::BlockMove
        | A::Immediate
        | A::Implied
        | A::Relative
        | A::RelativeLong
        | A::Stack => RegisterSet::NONE,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::isa::all_specs;

    fn effects_of(mnemonic: Mnemonic, mode: AddressingMode) -> Effects {
        instruction_effects(mnemonic, mode, None)
    }

    #[test]
    fn every_legal_pair_has_a_classified_effect() {
        for spec in all_specs() {
            let effects = instruction_effects(spec.mnemonic, spec.mode, Some(0x20));
            assert_ne!(
                effects.control,
                ControlEffect::Unknown,
                "{:?} {:?}",
                spec.mnemonic,
                spec.mode
            );
            assert_ne!(
                effects.memory,
                MemoryEffect::Unknown,
                "{:?} {:?}",
                spec.mnemonic,
                spec.mode
            );
        }
    }

    #[test]
    fn control_flow_is_classified_by_family() {
        use ControlEffect::*;
        for (mnemonic, mode, expected) in [
            (Mnemonic::Bcc, AddressingMode::Relative, Branch),
            (Mnemonic::Bvs, AddressingMode::Relative, Branch),
            (Mnemonic::Bra, AddressingMode::Relative, Jump),
            (Mnemonic::Brl, AddressingMode::RelativeLong, Jump),
            (Mnemonic::Jmp, AddressingMode::Absolute, Jump),
            (Mnemonic::Jml, AddressingMode::AbsoluteIndirect, Jump),
            (Mnemonic::Jsr, AddressingMode::Absolute, Call),
            (Mnemonic::Jsl, AddressingMode::AbsoluteLong, Call),
            (Mnemonic::Rts, AddressingMode::Stack, Return),
            (Mnemonic::Rtl, AddressingMode::Stack, Return),
            (Mnemonic::Rti, AddressingMode::Stack, Return),
            (Mnemonic::Brk, AddressingMode::Stack, Trap),
            (Mnemonic::Cop, AddressingMode::Stack, Trap),
            (Mnemonic::Wai, AddressingMode::Implied, Halt),
            (Mnemonic::Stp, AddressingMode::Implied, Halt),
            (Mnemonic::Nop, AddressingMode::Implied, FallThrough),
        ] {
            assert_eq!(effects_of(mnemonic, mode).control, expected, "{mnemonic:?}");
        }
    }

    #[test]
    fn memory_effects_are_mode_sensitive() {
        // The same mnemonic answers differently per mode, which is the part
        // an exhaustive match over mnemonics alone cannot get right.
        assert_eq!(
            effects_of(Mnemonic::Lda, AddressingMode::Immediate).memory,
            MemoryEffect::None
        );
        assert_eq!(
            effects_of(Mnemonic::Lda, AddressingMode::Absolute).memory,
            MemoryEffect::Read
        );
        assert_eq!(
            effects_of(Mnemonic::Asl, AddressingMode::Accumulator).memory,
            MemoryEffect::None
        );
        assert_eq!(
            effects_of(Mnemonic::Asl, AddressingMode::Direct).memory,
            MemoryEffect::ReadWrite
        );
        assert_eq!(
            effects_of(Mnemonic::Sta, AddressingMode::Absolute).memory,
            MemoryEffect::Write
        );
        // A plain jump reads no memory; an indirect one fetches its pointer.
        assert_eq!(
            effects_of(Mnemonic::Jmp, AddressingMode::Absolute).memory,
            MemoryEffect::None
        );
        assert_eq!(
            effects_of(Mnemonic::Jmp, AddressingMode::AbsoluteIndirect).memory,
            MemoryEffect::Read
        );
    }

    #[test]
    fn bank_and_page_registers_are_read_per_mode() {
        let reads = |mode| effects_of(Mnemonic::Lda, mode).reads.registers;
        assert!(reads(AddressingMode::Absolute).contains(RegisterSet::DBR));
        assert!(reads(AddressingMode::Direct).contains(RegisterSet::D));
        assert!(reads(AddressingMode::DirectIndexedX).contains(RegisterSet::X));
        assert!(reads(AddressingMode::StackRelative).contains(RegisterSet::S));
        // A long address carries its own bank, so it does not read DBR.
        assert!(!reads(AddressingMode::AbsoluteLong).contains(RegisterSet::DBR));
        assert!(reads(AddressingMode::Immediate).is_empty());
    }

    #[test]
    fn accumulator_mode_writes_a_while_memory_mode_does_not() {
        let acc = effects_of(Mnemonic::Asl, AddressingMode::Accumulator);
        assert!(acc.writes.registers.contains(RegisterSet::A));
        assert!(acc.reads.registers.contains(RegisterSet::A));
        assert!(acc.is_removal_candidate());

        let memory = effects_of(Mnemonic::Asl, AddressingMode::Absolute);
        assert!(!memory.writes.registers.contains(RegisterSet::A));
        assert!(!memory.is_removal_candidate());
    }

    /// `doc/w65c816s.md` Table 5-5 note 1: `BIT #imm` affects Z only.
    /// Claiming N and V there would let the optimizer delete a live write.
    #[test]
    fn bit_immediate_writes_only_z() {
        assert_eq!(
            effects_of(Mnemonic::Bit, AddressingMode::Immediate)
                .writes
                .flags,
            FlagSet::Z
        );
        assert_eq!(
            effects_of(Mnemonic::Bit, AddressingMode::Absolute)
                .writes
                .flags,
            FlagSet::N.union(FlagSet::V).union(FlagSet::Z)
        );
    }

    #[test]
    fn sep_and_rep_write_exactly_the_masked_flags() {
        let sep = instruction_effects(Mnemonic::Sep, AddressingMode::Immediate, Some(0x20));
        assert_eq!(sep.writes.flags, FlagSet::M);
        let rep = instruction_effects(Mnemonic::Rep, AddressingMode::Immediate, Some(0x10));
        assert_eq!(rep.writes.flags, FlagSet::X);
        let both = instruction_effects(Mnemonic::Rep, AddressingMode::Immediate, Some(0x30));
        assert_eq!(both.writes.flags, FlagSet::M.union(FlagSet::X));
    }

    /// `SEP` and `REP` decide nothing from the status register.
    ///
    /// `ALL_P` is a safe may-read over-approximation and too coarse to reason
    /// with: it says a `sep #$20` depends on M, so the `sep #$20` below a
    /// `rep #$20` would keep it alive and dead mode elimination could never
    /// fire. "Every bit the mask does not write" is not enough either — then
    /// `rep #$10` reads M and a `rep #$20` above it stays alive until folding
    /// merges the two, which makes the pass's answer depend on whether a later
    /// pass has run (`plans/05-optimizer.md` decision 2).
    #[test]
    fn sep_and_rep_read_no_flag_at_all() {
        for mask in [Some(0x20), Some(0x30), Some(0x21), None] {
            for mnemonic in [Mnemonic::Sep, Mnemonic::Rep] {
                let effects = instruction_effects(mnemonic, AddressingMode::Immediate, mask);
                assert_eq!(effects.reads, CpuState::NONE, "{mnemonic:?} {mask:?}");
            }
        }
        // An unfolded mask writes nothing either — the mask may be zero — and
        // says so through `WidthEffect::Unknown` instead.
        let unfolded = instruction_effects(Mnemonic::Sep, AddressingMode::Immediate, None);
        assert_eq!(unfolded.writes.flags, FlagSet::NONE);
        assert!(unfolded.width.is_unknown());
    }

    /// A preserved bit stays live through a mode instruction because the
    /// transfer function passes it through, not because the instruction reads
    /// it — and a bit the mask *does* write is a real overwrite.
    #[test]
    fn a_mode_mask_overwrites_exactly_the_flags_it_names() {
        let plain = instruction_effects(Mnemonic::Rep, AddressingMode::Immediate, Some(0x20));
        assert!(!plain.writes.flags.intersects(FlagSet::C));
        let carry = instruction_effects(Mnemonic::Rep, AddressingMode::Immediate, Some(0x21));
        assert!(carry.writes.flags.contains(FlagSet::C));
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
    }

    /// Narrowing the index registers zeroes their high bytes, so `sep #$10`
    /// is a write to X and Y — widening them is not.
    #[test]
    fn narrowing_the_index_width_truncates_x_and_y() {
        let narrow = instruction_effects(Mnemonic::Sep, AddressingMode::Immediate, Some(0x10));
        assert!(narrow.writes.registers.contains(RegisterSet::X));
        assert!(narrow.writes.registers.contains(RegisterSet::Y));

        let widen = instruction_effects(Mnemonic::Rep, AddressingMode::Immediate, Some(0x10));
        assert!(widen.writes.registers.is_empty());
    }

    /// An unfolded mask may be zero, so nothing is *definitely* written. The
    /// optimizer subtracts `writes` from a candidate's live state, so a
    /// pretend write here would declare a live flag dead; the barrier is the
    /// unknown width transition instead, which is what makes every flag live
    /// across it rather than a pretend read.
    #[test]
    fn an_unfolded_mask_writes_nothing_and_makes_the_width_unknown() {
        let effects = instruction_effects(Mnemonic::Sep, AddressingMode::Immediate, None);
        assert_eq!(effects.writes, CpuState::NONE);
        assert!(effects.width.is_unknown());
    }

    #[test]
    fn a_mask_that_touches_no_width_flag_changes_no_width() {
        let effects = instruction_effects(Mnemonic::Sep, AddressingMode::Immediate, Some(0x01));
        assert_eq!(effects.width, WidthEffect::Unchanged);
        assert_eq!(effects.writes.flags, FlagSet::C);
    }

    /// `PLP` and `RTI` restore a whole status byte and `XCE` may enter
    /// emulation mode; none of the three can say what width follows, and the
    /// first two may have truncated the index registers on the way.
    #[test]
    fn plp_rti_and_xce_are_unknown_width_transitions() {
        for (mnemonic, mode) in [
            (Mnemonic::Plp, AddressingMode::Stack),
            (Mnemonic::Rti, AddressingMode::Stack),
            (Mnemonic::Xce, AddressingMode::Implied),
        ] {
            assert!(
                effects_of(mnemonic, mode).width.is_unknown(),
                "{mnemonic:?}"
            );
        }
        for mnemonic in [Mnemonic::Plp, Mnemonic::Rti] {
            let writes = effects_of(mnemonic, AddressingMode::Stack).writes;
            assert_eq!(writes.flags, FlagSet::ALL_P, "{mnemonic:?}");
            assert!(writes.registers.contains(RegisterSet::X), "{mnemonic:?}");
            assert!(writes.registers.contains(RegisterSet::Y), "{mnemonic:?}");
        }
        let xce = effects_of(Mnemonic::Xce, AddressingMode::Implied);
        assert_eq!(xce.writes.flags, FlagSet::C.union(FlagSet::E));
        assert!(xce.reads.flags.contains(FlagSet::E));
    }

    /// `doc/w65c816s.md` §3.5.9: the block moves step X and Y, count the
    /// accumulator down, and load the destination bank into DBR.
    #[test]
    fn block_moves_write_the_data_bank_register() {
        for mnemonic in [Mnemonic::Mvn, Mnemonic::Mvp] {
            let effects = effects_of(mnemonic, AddressingMode::BlockMove);
            for register in [
                RegisterSet::A,
                RegisterSet::X,
                RegisterSet::Y,
                RegisterSet::DBR,
            ] {
                assert!(effects.reads.registers.contains(register), "{mnemonic:?}");
                assert!(effects.writes.registers.contains(register), "{mnemonic:?}");
            }
            assert_eq!(effects.memory, MemoryEffect::ReadWrite);
            assert!(!effects.is_removal_candidate());
        }
    }

    #[test]
    fn the_stack_family_is_never_a_removal_candidate() {
        for mnemonic in [
            Mnemonic::Pha,
            Mnemonic::Php,
            Mnemonic::Pla,
            Mnemonic::Plb,
            Mnemonic::Pea,
            Mnemonic::Pei,
            Mnemonic::Per,
            Mnemonic::Rts,
        ] {
            let effects = effects_of(mnemonic, AddressingMode::Stack);
            assert_eq!(effects.memory, MemoryEffect::Stack, "{mnemonic:?}");
            assert!(!effects.is_removal_candidate(), "{mnemonic:?}");
            assert!(
                effects.reads.registers.contains(RegisterSet::S),
                "{mnemonic:?}"
            );
        }
    }

    #[test]
    fn full_width_transfers_set_no_flags() {
        // TXS and TCS move all 16 bits and touch no flag at all.
        for mnemonic in [Mnemonic::Txs, Mnemonic::Tcs] {
            let effects = effects_of(mnemonic, AddressingMode::Implied);
            assert_eq!(effects.writes.flags, FlagSet::NONE, "{mnemonic:?}");
            assert!(effects.writes.registers.contains(RegisterSet::S));
        }
        // TSX does set them.
        assert_eq!(
            effects_of(Mnemonic::Tsx, AddressingMode::Implied)
                .writes
                .flags,
            FlagSet::N.union(FlagSet::Z)
        );
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
        let effects = effects_of(Mnemonic::Sta, AddressingMode::Absolute);
        assert!(effects.reads.registers.contains(RegisterSet::A));
        assert_eq!(effects.memory, MemoryEffect::Write);
        assert!(!effects.is_removal_candidate());
    }

    #[test]
    fn rts_is_a_stack_using_return_barrier() {
        let effects = effects_of(Mnemonic::Rts, AddressingMode::Stack);
        assert_eq!(effects.control, ControlEffect::Return);
        assert_eq!(effects.memory, MemoryEffect::Stack);
        assert!(!effects.is_removal_candidate());
    }

    #[test]
    fn set_displays_are_stable() {
        assert_eq!(RegisterSet::NONE.to_string(), "-");
        assert_eq!(RegisterSet::A.union(RegisterSet::S).to_string(), "A S");
        assert_eq!(FlagSet::N.union(FlagSet::Z).to_string(), "N Z");
        assert_eq!(FlagSet::M.union(FlagSet::X).to_string(), "M X");
    }
}
