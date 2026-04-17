use thiserror::Error;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AddressingMode {
    Implied,
    Accumulator,
    Immediate8,
    Immediate16,
    ImmediateM,
    ImmediateX,
    DirectPage,
    DirectPageX,
    DirectPageY,
    DirectPageIndirect,
    DirectPageIndirectLong,
    DirectPageIndexedIndirectX,
    DirectPageIndirectIndexedY,
    DirectPageIndirectLongIndexedY,
    StackRelative,
    StackRelativeIndirectIndexedY,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    AbsoluteLong,
    AbsoluteLongX,
    AbsoluteIndirect,
    AbsoluteIndexedIndirectX,
    AbsoluteIndirectLong,
    Relative8,
    Relative16,
    BlockMove,
}

#[derive(Debug, Clone, Copy)]
pub enum OperandShape {
    None,
    Immediate(i64),
    Address {
        literal: Option<u32>,
        size_hint: AddressSizeHint,
        mode: AddressOperandMode,
    },
    BlockMove(u8, u8),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AddressSizeHint {
    Auto,
    ForceAbsolute16,
    ForceAbsoluteLong,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AddressOperandMode {
    Direct { index: Option<IndexRegister> },
    Indirect,
    IndirectLong,
    IndexedIndirectX,
    IndirectIndexedY,
    IndirectLongIndexedY,
    StackRelativeIndirectIndexedY,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IndexRegister {
    X,
    Y,
    S,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Encoding {
    pub opcode: u8,
    pub mode: AddressingMode,
}

#[derive(Debug, Error)]
pub enum EncodeError {
    #[error("unknown mnemonic '{mnemonic}'")]
    UnknownMnemonic { mnemonic: String },
    #[error("mnemonic '{mnemonic}' does not accept this operand")]
    InvalidOperand { mnemonic: String },
    #[error("mnemonic '{mnemonic}' does not accept #immediate operand")]
    InvalidImmediateOperand { mnemonic: String },
    #[error("mnemonic '{mnemonic}' has no long form")]
    NoLongForm { mnemonic: String },
    #[error("This operand does not support forced absolute addressing")]
    NoAbsolute16Form,
    #[error("immediate value out of range for the selected operand width")]
    ImmediateOutOfRange,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct OpcodeDescriptor {
    pub mnemonic: &'static str,
    pub mode: AddressingMode,
}

#[derive(Debug)]
pub struct DecodedInstruction<'a> {
    pub opcode: u8,
    pub mnemonic: &'static str,
    pub mode: AddressingMode,
    pub operand: &'a [u8],
}

impl DecodedInstruction<'_> {
    pub fn len(&self) -> usize {
        1 + self.operand.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

#[derive(Debug, Error)]
pub enum DecodeError {
    #[error("cannot decode instruction from empty byte slice")]
    EmptyInput,
    #[error(
        "truncated instruction for opcode {opcode:#04X}: expected {expected} operand bytes, got {actual}"
    )]
    Truncated {
        opcode: u8,
        expected: usize,
        actual: usize,
    },
}

macro_rules! op {
    ($mnemonic:literal, $mode:ident) => {
        OpcodeDescriptor {
            mnemonic: $mnemonic,
            mode: AddressingMode::$mode,
        }
    };
}

const OPCODE_TABLE: [OpcodeDescriptor; 256] = [
    // 0x00
    op!("brk", Immediate8),
    op!("ora", DirectPageIndexedIndirectX),
    op!("cop", Immediate8),
    op!("ora", StackRelative),
    op!("tsb", DirectPage),
    op!("ora", DirectPage),
    op!("asl", DirectPage),
    op!("ora", DirectPageIndirectLong),
    op!("php", Implied),
    op!("ora", ImmediateM),
    op!("asl", Accumulator),
    op!("phd", Implied),
    op!("tsb", Absolute),
    op!("ora", Absolute),
    op!("asl", Absolute),
    op!("ora", AbsoluteLong),
    // 0x10
    op!("bpl", Relative8),
    op!("ora", DirectPageIndirectIndexedY),
    op!("ora", DirectPageIndirect),
    op!("ora", StackRelativeIndirectIndexedY),
    op!("trb", DirectPage),
    op!("ora", DirectPageX),
    op!("asl", DirectPageX),
    op!("ora", DirectPageIndirectLongIndexedY),
    op!("clc", Implied),
    op!("ora", AbsoluteY),
    op!("inc", Accumulator),
    op!("tcs", Implied),
    op!("trb", Absolute),
    op!("ora", AbsoluteX),
    op!("asl", AbsoluteX),
    op!("ora", AbsoluteLongX),
    // 0x20
    op!("jsr", Absolute),
    op!("and", DirectPageIndexedIndirectX),
    op!("jsl", AbsoluteLong),
    op!("and", StackRelative),
    op!("bit", DirectPage),
    op!("and", DirectPage),
    op!("rol", DirectPage),
    op!("and", DirectPageIndirectLong),
    op!("plp", Implied),
    op!("and", ImmediateM),
    op!("rol", Accumulator),
    op!("pld", Implied),
    op!("bit", Absolute),
    op!("and", Absolute),
    op!("rol", Absolute),
    op!("and", AbsoluteLong),
    // 0x30
    op!("bmi", Relative8),
    op!("and", DirectPageIndirectIndexedY),
    op!("and", DirectPageIndirect),
    op!("and", StackRelativeIndirectIndexedY),
    op!("bit", DirectPageX),
    op!("and", DirectPageX),
    op!("rol", DirectPageX),
    op!("and", DirectPageIndirectLongIndexedY),
    op!("sec", Implied),
    op!("and", AbsoluteY),
    op!("dec", Accumulator),
    op!("tsc", Implied),
    op!("bit", AbsoluteX),
    op!("and", AbsoluteX),
    op!("rol", AbsoluteX),
    op!("and", AbsoluteLongX),
    // 0x40
    op!("rti", Implied),
    op!("eor", DirectPageIndexedIndirectX),
    op!("wdm", Immediate8),
    op!("eor", StackRelative),
    op!("mvp", BlockMove),
    op!("eor", DirectPage),
    op!("lsr", DirectPage),
    op!("eor", DirectPageIndirectLong),
    op!("pha", Implied),
    op!("eor", ImmediateM),
    op!("lsr", Accumulator),
    op!("phk", Implied),
    op!("jmp", Absolute),
    op!("eor", Absolute),
    op!("lsr", Absolute),
    op!("eor", AbsoluteLong),
    // 0x50
    op!("bvc", Relative8),
    op!("eor", DirectPageIndirectIndexedY),
    op!("eor", DirectPageIndirect),
    op!("eor", StackRelativeIndirectIndexedY),
    op!("mvn", BlockMove),
    op!("eor", DirectPageX),
    op!("lsr", DirectPageX),
    op!("eor", DirectPageIndirectLongIndexedY),
    op!("cli", Implied),
    op!("eor", AbsoluteY),
    op!("phy", Implied),
    op!("tcd", Implied),
    op!("jml", AbsoluteLong),
    op!("eor", AbsoluteX),
    op!("lsr", AbsoluteX),
    op!("eor", AbsoluteLongX),
    // 0x60
    op!("rts", Implied),
    op!("adc", DirectPageIndexedIndirectX),
    op!("per", Relative16),
    op!("adc", StackRelative),
    op!("stz", DirectPage),
    op!("adc", DirectPage),
    op!("ror", DirectPage),
    op!("adc", DirectPageIndirectLong),
    op!("pla", Implied),
    op!("adc", ImmediateM),
    op!("ror", Accumulator),
    op!("rtl", Implied),
    op!("jmp", AbsoluteIndirect),
    op!("adc", Absolute),
    op!("ror", Absolute),
    op!("adc", AbsoluteLong),
    // 0x70
    op!("bvs", Relative8),
    op!("adc", DirectPageIndirectIndexedY),
    op!("adc", DirectPageIndirect),
    op!("adc", StackRelativeIndirectIndexedY),
    op!("stz", DirectPageX),
    op!("adc", DirectPageX),
    op!("ror", DirectPageX),
    op!("adc", DirectPageIndirectLongIndexedY),
    op!("sei", Implied),
    op!("adc", AbsoluteY),
    op!("ply", Implied),
    op!("tdc", Implied),
    op!("jmp", AbsoluteIndexedIndirectX),
    op!("adc", AbsoluteX),
    op!("ror", AbsoluteX),
    op!("adc", AbsoluteLongX),
    // 0x80
    op!("bra", Relative8),
    op!("sta", DirectPageIndexedIndirectX),
    op!("brl", Relative16),
    op!("sta", StackRelative),
    op!("sty", DirectPage),
    op!("sta", DirectPage),
    op!("stx", DirectPage),
    op!("sta", DirectPageIndirectLong),
    op!("dey", Implied),
    op!("bit", ImmediateM),
    op!("txa", Implied),
    op!("phb", Implied),
    op!("sty", Absolute),
    op!("sta", Absolute),
    op!("stx", Absolute),
    op!("sta", AbsoluteLong),
    // 0x90
    op!("bcc", Relative8),
    op!("sta", DirectPageIndirectIndexedY),
    op!("sta", DirectPageIndirect),
    op!("sta", StackRelativeIndirectIndexedY),
    op!("sty", DirectPageX),
    op!("sta", DirectPageX),
    op!("stx", DirectPageY),
    op!("sta", DirectPageIndirectLongIndexedY),
    op!("tya", Implied),
    op!("sta", AbsoluteY),
    op!("txs", Implied),
    op!("txy", Implied),
    op!("stz", Absolute),
    op!("sta", AbsoluteX),
    op!("stz", AbsoluteX),
    op!("sta", AbsoluteLongX),
    // 0xA0
    op!("ldy", ImmediateX),
    op!("lda", DirectPageIndexedIndirectX),
    op!("ldx", ImmediateX),
    op!("lda", StackRelative),
    op!("ldy", DirectPage),
    op!("lda", DirectPage),
    op!("ldx", DirectPage),
    op!("lda", DirectPageIndirectLong),
    op!("tay", Implied),
    op!("lda", ImmediateM),
    op!("tax", Implied),
    op!("plb", Implied),
    op!("ldy", Absolute),
    op!("lda", Absolute),
    op!("ldx", Absolute),
    op!("lda", AbsoluteLong),
    // 0xB0
    op!("bcs", Relative8),
    op!("lda", DirectPageIndirectIndexedY),
    op!("lda", DirectPageIndirect),
    op!("lda", StackRelativeIndirectIndexedY),
    op!("ldy", DirectPageX),
    op!("lda", DirectPageX),
    op!("ldx", DirectPageY),
    op!("lda", DirectPageIndirectLongIndexedY),
    op!("clv", Implied),
    op!("lda", AbsoluteY),
    op!("tsx", Implied),
    op!("tyx", Implied),
    op!("ldy", AbsoluteX),
    op!("lda", AbsoluteX),
    op!("ldx", AbsoluteY),
    op!("lda", AbsoluteLongX),
    // 0xC0
    op!("cpy", ImmediateX),
    op!("cmp", DirectPageIndexedIndirectX),
    op!("rep", Immediate8),
    op!("cmp", StackRelative),
    op!("cpy", DirectPage),
    op!("cmp", DirectPage),
    op!("dec", DirectPage),
    op!("cmp", DirectPageIndirectLong),
    op!("iny", Implied),
    op!("cmp", ImmediateM),
    op!("dex", Implied),
    op!("wai", Implied),
    op!("cpy", Absolute),
    op!("cmp", Absolute),
    op!("dec", Absolute),
    op!("cmp", AbsoluteLong),
    // 0xD0
    op!("bne", Relative8),
    op!("cmp", DirectPageIndirectIndexedY),
    op!("cmp", DirectPageIndirect),
    op!("cmp", StackRelativeIndirectIndexedY),
    op!("pei", DirectPageIndirect),
    op!("cmp", DirectPageX),
    op!("dec", DirectPageX),
    op!("cmp", DirectPageIndirectLongIndexedY),
    op!("cld", Implied),
    op!("cmp", AbsoluteY),
    op!("phx", Implied),
    op!("stp", Implied),
    op!("jmp", AbsoluteIndirectLong),
    op!("cmp", AbsoluteX),
    op!("dec", AbsoluteX),
    op!("cmp", AbsoluteLongX),
    // 0xE0
    op!("cpx", ImmediateX),
    op!("sbc", DirectPageIndexedIndirectX),
    op!("sep", Immediate8),
    op!("sbc", StackRelative),
    op!("cpx", DirectPage),
    op!("sbc", DirectPage),
    op!("inc", DirectPage),
    op!("sbc", DirectPageIndirectLong),
    op!("inx", Implied),
    op!("sbc", ImmediateM),
    op!("nop", Implied),
    op!("xba", Implied),
    op!("cpx", Absolute),
    op!("sbc", Absolute),
    op!("inc", Absolute),
    op!("sbc", AbsoluteLong),
    // 0xF0
    op!("beq", Relative8),
    op!("sbc", DirectPageIndirectIndexedY),
    op!("sbc", DirectPageIndirect),
    op!("sbc", StackRelativeIndirectIndexedY),
    op!("pea", Immediate16),
    op!("sbc", DirectPageX),
    op!("inc", DirectPageX),
    op!("sbc", DirectPageIndirectLongIndexedY),
    op!("sed", Implied),
    op!("sbc", AbsoluteY),
    op!("plx", Implied),
    op!("xce", Implied),
    op!("jsr", AbsoluteIndexedIndirectX),
    op!("sbc", AbsoluteX),
    op!("inc", AbsoluteX),
    op!("sbc", AbsoluteLongX),
];

pub fn opcode_descriptor(opcode: u8) -> OpcodeDescriptor {
    OPCODE_TABLE[usize::from(opcode)]
}

pub fn is_mnemonic(mnemonic: &str) -> bool {
    let lower = mnemonic.to_ascii_lowercase();
    OPCODE_TABLE.iter().any(|entry| entry.mnemonic == lower)
}

/// Bitset over the CPU registers tracked by the damage/clobber analysis.
///
/// v1 intentionally covers only the user-visible accumulator and index
/// registers; status-flag bits are excluded because no feature consumes them
/// yet (see the "Out of Scope" note in the function-contracts plan).
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct RegSet(u8);

impl RegSet {
    pub const NONE: Self = Self(0);
    pub const A: Self = Self(1 << 0);
    pub const X: Self = Self(1 << 1);
    pub const Y: Self = Self(1 << 2);

    pub const fn contains(self, other: Self) -> bool {
        self.0 & other.0 != 0
    }

    pub const fn is_empty(self) -> bool {
        self.0 == 0
    }
}

impl std::ops::BitOr for RegSet {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self::Output {
        Self(self.0 | rhs.0)
    }
}

impl std::ops::BitOrAssign for RegSet {
    fn bitor_assign(&mut self, rhs: Self) {
        self.0 |= rhs.0;
    }
}

impl std::ops::BitAnd for RegSet {
    type Output = Self;
    fn bitand(self, rhs: Self) -> Self::Output {
        Self(self.0 & rhs.0)
    }
}

impl std::ops::Sub for RegSet {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        Self(self.0 & !rhs.0)
    }
}

/// Register read/write footprint of a single instruction.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct RegEffects {
    pub reads: RegSet,
    pub modifies: RegSet,
}

/// Mnemonic-level read/modify footprint of a 65816 instruction.
///
/// `on_accumulator` must be true when the instruction targets the accumulator
/// via the `Accumulator` addressing mode (e.g. bare `asl` / `rol` with no
/// operand); it is ignored for mnemonics that are not read-modify-write. Index
/// register reads that come from addressing modes are NOT included here — the
/// caller composes those on top, since they depend on the operand form chosen
/// in the AST rather than on the mnemonic alone.
pub fn mnemonic_effects(mnemonic: &str, on_accumulator: bool) -> RegEffects {
    let lower = mnemonic.to_ascii_lowercase();
    let m = lower.as_str();
    let rmw_on_a = matches!(m, "asl" | "dec" | "inc" | "lsr" | "rol" | "ror") && on_accumulator;
    let a_mod = matches!(
        m,
        "adc"
            | "and"
            | "eor"
            | "lda"
            | "ora"
            | "pla"
            | "sbc"
            | "txa"
            | "tya"
            | "tsc"
            | "tdc"
            | "xba"
    ) || rmw_on_a;
    let a_read = matches!(
        m,
        "adc"
            | "and"
            | "bit"
            | "cmp"
            | "eor"
            | "ora"
            | "pha"
            | "sbc"
            | "sta"
            | "tax"
            | "tay"
            | "tcd"
            | "tcs"
            | "trb"
            | "tsb"
            | "xba"
    ) || rmw_on_a;
    let x_mod = matches!(m, "dex" | "inx" | "ldx" | "plx" | "tax" | "tsx" | "tyx");
    let x_read = matches!(
        m,
        "cpx" | "dex" | "inx" | "phx" | "stx" | "txa" | "txs" | "txy"
    );
    let y_mod = matches!(m, "dey" | "iny" | "ldy" | "ply" | "tay" | "txy");
    let y_read = matches!(m, "cpy" | "dey" | "iny" | "phy" | "sty" | "tya" | "tyx");
    let block_move = matches!(m, "mvn" | "mvp");

    let mut reads = RegSet::NONE;
    let mut modifies = RegSet::NONE;
    if a_read || block_move {
        reads = reads | RegSet::A;
    }
    if x_read || block_move {
        reads = reads | RegSet::X;
    }
    if y_read || block_move {
        reads = reads | RegSet::Y;
    }
    if a_mod || block_move {
        modifies = modifies | RegSet::A;
    }
    if x_mod || block_move {
        modifies = modifies | RegSet::X;
    }
    if y_mod || block_move {
        modifies = modifies | RegSet::Y;
    }

    RegEffects { reads, modifies }
}

pub fn select_encoding(mnemonic: &str, operand: OperandShape) -> Result<Encoding, EncodeError> {
    let lower = mnemonic.to_ascii_lowercase();
    let modes = matching_modes(&lower);
    if modes.is_empty() {
        return Err(EncodeError::UnknownMnemonic {
            mnemonic: mnemonic.to_string(),
        });
    }

    match operand {
        OperandShape::None => {
            if let Some(opcode) = find_opcode(&lower, AddressingMode::Implied) {
                return Ok(Encoding {
                    opcode,
                    mode: AddressingMode::Implied,
                });
            }
            if let Some(opcode) = find_opcode(&lower, AddressingMode::Accumulator) {
                return Ok(Encoding {
                    opcode,
                    mode: AddressingMode::Accumulator,
                });
            }
            Err(EncodeError::InvalidOperand {
                mnemonic: mnemonic.to_string(),
            })
        }
        OperandShape::Immediate(value) => {
            let mut has_immediate_mode = false;
            for mode in [
                AddressingMode::Immediate8,
                AddressingMode::ImmediateM,
                AddressingMode::ImmediateX,
                AddressingMode::Immediate16,
            ] {
                if !modes.contains(&mode) {
                    continue;
                }
                has_immediate_mode = true;
                if immediate_fits(mode, value) {
                    let opcode =
                        find_opcode(&lower, mode).expect("mode presence should imply opcode");
                    return Ok(Encoding { opcode, mode });
                }
            }

            if has_immediate_mode {
                Err(EncodeError::ImmediateOutOfRange)
            } else {
                Err(EncodeError::InvalidImmediateOperand {
                    mnemonic: mnemonic.to_string(),
                })
            }
        }
        OperandShape::Address {
            literal,
            size_hint,
            mode,
        } => {
            let wants_long = size_hint == AddressSizeHint::ForceAbsoluteLong
                || literal.is_some_and(|value| value > 0xFFFF);
            match mode {
                AddressOperandMode::Direct { index } => {
                    if index.is_none() {
                        if modes.contains(&AddressingMode::Relative8) {
                            if size_hint != AddressSizeHint::Auto {
                                return Err(EncodeError::InvalidOperand {
                                    mnemonic: mnemonic.to_string(),
                                });
                            }
                            let opcode = find_opcode(&lower, AddressingMode::Relative8)
                                .expect("relative mode should exist");
                            return Ok(Encoding {
                                opcode,
                                mode: AddressingMode::Relative8,
                            });
                        }
                        if modes.contains(&AddressingMode::Relative16) {
                            if size_hint != AddressSizeHint::Auto {
                                return Err(EncodeError::InvalidOperand {
                                    mnemonic: mnemonic.to_string(),
                                });
                            }
                            let opcode = find_opcode(&lower, AddressingMode::Relative16)
                                .expect("relative mode should exist");
                            return Ok(Encoding {
                                opcode,
                                mode: AddressingMode::Relative16,
                            });
                        }
                    }

                    match index {
                        Some(IndexRegister::X) => {
                            if wants_long {
                                if let Some(opcode) =
                                    find_opcode(&lower, AddressingMode::AbsoluteLongX)
                                {
                                    return Ok(Encoding {
                                        opcode,
                                        mode: AddressingMode::AbsoluteLongX,
                                    });
                                }
                                return Err(EncodeError::NoLongForm {
                                    mnemonic: mnemonic.to_string(),
                                });
                            }

                            if size_hint == AddressSizeHint::Auto
                                && literal.is_some_and(|value| value <= 0xFF)
                                && let Some(opcode) =
                                    find_opcode(&lower, AddressingMode::DirectPageX)
                            {
                                return Ok(Encoding {
                                    opcode,
                                    mode: AddressingMode::DirectPageX,
                                });
                            }

                            if size_hint == AddressSizeHint::ForceAbsolute16
                                && find_opcode(&lower, AddressingMode::AbsoluteX).is_none()
                            {
                                return Err(EncodeError::NoAbsolute16Form);
                            }
                            if let Some(opcode) = find_opcode(&lower, AddressingMode::AbsoluteX) {
                                return Ok(Encoding {
                                    opcode,
                                    mode: AddressingMode::AbsoluteX,
                                });
                            }
                            if let Some(opcode) = find_opcode(&lower, AddressingMode::AbsoluteLongX)
                            {
                                return Ok(Encoding {
                                    opcode,
                                    mode: AddressingMode::AbsoluteLongX,
                                });
                            }
                            if let Some(opcode) = find_opcode(&lower, AddressingMode::DirectPageX) {
                                return Ok(Encoding {
                                    opcode,
                                    mode: AddressingMode::DirectPageX,
                                });
                            }
                        }
                        Some(IndexRegister::S) => {
                            if wants_long || size_hint == AddressSizeHint::ForceAbsolute16 {
                                return Err(EncodeError::InvalidOperand {
                                    mnemonic: mnemonic.to_string(),
                                });
                            }
                            if let Some(opcode) = find_opcode(&lower, AddressingMode::StackRelative)
                            {
                                return Ok(Encoding {
                                    opcode,
                                    mode: AddressingMode::StackRelative,
                                });
                            }
                        }
                        Some(IndexRegister::Y) => {
                            if wants_long {
                                return Err(EncodeError::NoLongForm {
                                    mnemonic: mnemonic.to_string(),
                                });
                            }

                            if size_hint == AddressSizeHint::Auto
                                && literal.is_some_and(|value| value <= 0xFF)
                                && let Some(opcode) =
                                    find_opcode(&lower, AddressingMode::DirectPageY)
                            {
                                return Ok(Encoding {
                                    opcode,
                                    mode: AddressingMode::DirectPageY,
                                });
                            }

                            if size_hint == AddressSizeHint::ForceAbsolute16
                                && find_opcode(&lower, AddressingMode::AbsoluteY).is_none()
                            {
                                return Err(EncodeError::NoAbsolute16Form);
                            }
                            if let Some(opcode) = find_opcode(&lower, AddressingMode::AbsoluteY) {
                                return Ok(Encoding {
                                    opcode,
                                    mode: AddressingMode::AbsoluteY,
                                });
                            }
                            if let Some(opcode) = find_opcode(&lower, AddressingMode::DirectPageY) {
                                return Ok(Encoding {
                                    opcode,
                                    mode: AddressingMode::DirectPageY,
                                });
                            }
                        }
                        None => {
                            if wants_long {
                                if let Some(opcode) =
                                    find_opcode(&lower, AddressingMode::AbsoluteLong)
                                {
                                    return Ok(Encoding {
                                        opcode,
                                        mode: AddressingMode::AbsoluteLong,
                                    });
                                }
                                return Err(EncodeError::NoLongForm {
                                    mnemonic: mnemonic.to_string(),
                                });
                            }

                            if size_hint == AddressSizeHint::Auto
                                && literal.is_some_and(|value| value <= 0xFF)
                                && let Some(opcode) =
                                    find_opcode(&lower, AddressingMode::DirectPage)
                            {
                                return Ok(Encoding {
                                    opcode,
                                    mode: AddressingMode::DirectPage,
                                });
                            }

                            if size_hint == AddressSizeHint::ForceAbsolute16
                                && find_opcode(&lower, AddressingMode::Absolute).is_none()
                            {
                                return Err(EncodeError::NoAbsolute16Form);
                            }
                            if let Some(opcode) = find_opcode(&lower, AddressingMode::Absolute) {
                                return Ok(Encoding {
                                    opcode,
                                    mode: AddressingMode::Absolute,
                                });
                            }
                            if let Some(opcode) = find_opcode(&lower, AddressingMode::AbsoluteLong)
                            {
                                return Ok(Encoding {
                                    opcode,
                                    mode: AddressingMode::AbsoluteLong,
                                });
                            }
                        }
                    }
                }
                AddressOperandMode::Indirect => {
                    if wants_long {
                        return Err(EncodeError::InvalidOperand {
                            mnemonic: mnemonic.to_string(),
                        });
                    }

                    if size_hint == AddressSizeHint::Auto
                        && literal.is_some_and(|value| value <= 0xFF)
                        && let Some(opcode) =
                            find_opcode(&lower, AddressingMode::DirectPageIndirect)
                    {
                        return Ok(Encoding {
                            opcode,
                            mode: AddressingMode::DirectPageIndirect,
                        });
                    }

                    if size_hint == AddressSizeHint::ForceAbsolute16
                        && find_opcode(&lower, AddressingMode::AbsoluteIndirect).is_none()
                        && find_opcode(&lower, AddressingMode::AbsoluteIndirectLong).is_none()
                    {
                        return Err(EncodeError::NoAbsolute16Form);
                    }
                    if let Some(opcode) = find_opcode(&lower, AddressingMode::AbsoluteIndirect) {
                        return Ok(Encoding {
                            opcode,
                            mode: AddressingMode::AbsoluteIndirect,
                        });
                    }
                    if let Some(opcode) = find_opcode(&lower, AddressingMode::AbsoluteIndirectLong)
                    {
                        return Ok(Encoding {
                            opcode,
                            mode: AddressingMode::AbsoluteIndirectLong,
                        });
                    }
                }
                AddressOperandMode::IndexedIndirectX => {
                    if wants_long {
                        return Err(EncodeError::InvalidOperand {
                            mnemonic: mnemonic.to_string(),
                        });
                    }

                    if size_hint == AddressSizeHint::Auto
                        && literal.is_some_and(|value| value <= 0xFF)
                        && let Some(opcode) =
                            find_opcode(&lower, AddressingMode::DirectPageIndexedIndirectX)
                    {
                        return Ok(Encoding {
                            opcode,
                            mode: AddressingMode::DirectPageIndexedIndirectX,
                        });
                    }
                    if let Some(opcode) =
                        find_opcode(&lower, AddressingMode::AbsoluteIndexedIndirectX)
                    {
                        return Ok(Encoding {
                            opcode,
                            mode: AddressingMode::AbsoluteIndexedIndirectX,
                        });
                    }
                    if size_hint == AddressSizeHint::ForceAbsolute16 {
                        return Err(EncodeError::NoAbsolute16Form);
                    }
                }
                AddressOperandMode::IndirectIndexedY => {
                    if wants_long || literal.is_none() || literal.is_some_and(|value| value > 0xFF)
                    {
                        return Err(EncodeError::InvalidOperand {
                            mnemonic: mnemonic.to_string(),
                        });
                    }

                    if let Some(opcode) =
                        find_opcode(&lower, AddressingMode::DirectPageIndirectIndexedY)
                    {
                        return Ok(Encoding {
                            opcode,
                            mode: AddressingMode::DirectPageIndirectIndexedY,
                        });
                    }
                }
                AddressOperandMode::IndirectLong => {
                    if size_hint == AddressSizeHint::Auto
                        && literal.is_some_and(|value| value <= 0xFF)
                        && let Some(opcode) =
                            find_opcode(&lower, AddressingMode::DirectPageIndirectLong)
                    {
                        return Ok(Encoding {
                            opcode,
                            mode: AddressingMode::DirectPageIndirectLong,
                        });
                    }
                    if let Some(opcode) =
                        find_opcode(&lower, AddressingMode::AbsoluteIndirectLong)
                    {
                        return Ok(Encoding {
                            opcode,
                            mode: AddressingMode::AbsoluteIndirectLong,
                        });
                    }
                }
                AddressOperandMode::IndirectLongIndexedY => {
                    if size_hint != AddressSizeHint::Auto
                        || literal.is_none()
                        || literal.is_some_and(|value| value > 0xFF)
                    {
                        return Err(EncodeError::InvalidOperand {
                            mnemonic: mnemonic.to_string(),
                        });
                    }
                    if let Some(opcode) =
                        find_opcode(&lower, AddressingMode::DirectPageIndirectLongIndexedY)
                    {
                        return Ok(Encoding {
                            opcode,
                            mode: AddressingMode::DirectPageIndirectLongIndexedY,
                        });
                    }
                }
                AddressOperandMode::StackRelativeIndirectIndexedY => {
                    if wants_long
                        || size_hint == AddressSizeHint::ForceAbsolute16
                        || literal.is_none()
                        || literal.is_some_and(|value| value > 0xFF)
                    {
                        return Err(EncodeError::InvalidOperand {
                            mnemonic: mnemonic.to_string(),
                        });
                    }
                    if let Some(opcode) =
                        find_opcode(&lower, AddressingMode::StackRelativeIndirectIndexedY)
                    {
                        return Ok(Encoding {
                            opcode,
                            mode: AddressingMode::StackRelativeIndirectIndexedY,
                        });
                    }
                }
            }

            if size_hint == AddressSizeHint::ForceAbsoluteLong {
                Err(EncodeError::NoLongForm {
                    mnemonic: mnemonic.to_string(),
                })
            } else if size_hint == AddressSizeHint::ForceAbsolute16 {
                Err(EncodeError::NoAbsolute16Form)
            } else {
                Err(EncodeError::InvalidOperand {
                    mnemonic: mnemonic.to_string(),
                })
            }
        }
        OperandShape::BlockMove(_, _) => {
            if let Some(opcode) = find_opcode(&lower, AddressingMode::BlockMove) {
                Ok(Encoding {
                    opcode,
                    mode: AddressingMode::BlockMove,
                })
            } else {
                Err(EncodeError::InvalidOperand {
                    mnemonic: mnemonic.to_string(),
                })
            }
        }
    }
}

/// Operand width assuming 8-bit immediates (no CPU mode context).
/// Used by the disassembler when mode state is unknown.
pub fn operand_width(mode: AddressingMode) -> usize {
    operand_width_for_mode(mode, false, false)
}

/// Operand width with CPU mode context.
/// `m_wide` = true when accumulator is 16-bit (M flag clear).
/// `x_wide` = true when index registers are 16-bit (X flag clear).
pub fn operand_width_for_mode(mode: AddressingMode, m_wide: bool, x_wide: bool) -> usize {
    match mode {
        AddressingMode::Implied | AddressingMode::Accumulator => 0,
        AddressingMode::ImmediateM => {
            if m_wide {
                2
            } else {
                1
            }
        }
        AddressingMode::ImmediateX => {
            if x_wide {
                2
            } else {
                1
            }
        }
        AddressingMode::Immediate8
        | AddressingMode::DirectPage
        | AddressingMode::DirectPageX
        | AddressingMode::DirectPageY
        | AddressingMode::DirectPageIndirect
        | AddressingMode::DirectPageIndirectLong
        | AddressingMode::DirectPageIndexedIndirectX
        | AddressingMode::DirectPageIndirectIndexedY
        | AddressingMode::DirectPageIndirectLongIndexedY
        | AddressingMode::StackRelative
        | AddressingMode::StackRelativeIndirectIndexedY
        | AddressingMode::Relative8 => 1,
        AddressingMode::Immediate16
        | AddressingMode::Absolute
        | AddressingMode::AbsoluteX
        | AddressingMode::AbsoluteY
        | AddressingMode::AbsoluteIndirect
        | AddressingMode::AbsoluteIndexedIndirectX
        | AddressingMode::AbsoluteIndirectLong
        | AddressingMode::Relative16
        | AddressingMode::BlockMove => 2,
        AddressingMode::AbsoluteLong | AddressingMode::AbsoluteLongX => 3,
    }
}

pub fn decode_instruction(bytes: &[u8]) -> Result<DecodedInstruction<'_>, DecodeError> {
    decode_instruction_with_mode(bytes, false, false)
}

pub fn decode_instruction_with_mode(
    bytes: &[u8],
    m_wide: bool,
    x_wide: bool,
) -> Result<DecodedInstruction<'_>, DecodeError> {
    let (&opcode, rest) = bytes.split_first().ok_or(DecodeError::EmptyInput)?;
    let descriptor = opcode_descriptor(opcode);
    let width = operand_width_for_mode(descriptor.mode, m_wide, x_wide);
    if rest.len() < width {
        return Err(DecodeError::Truncated {
            opcode,
            expected: width,
            actual: rest.len(),
        });
    }

    Ok(DecodedInstruction {
        opcode,
        mnemonic: descriptor.mnemonic,
        mode: descriptor.mode,
        operand: &rest[..width],
    })
}

pub fn format_instruction(decoded: &DecodedInstruction<'_>, address: u32) -> String {
    let operand = format_operand(decoded.mode, decoded.operand, address);
    if operand.is_empty() {
        decoded.mnemonic.to_string()
    } else {
        format!("{} {}", decoded.mnemonic, operand)
    }
}

fn format_operand(mode: AddressingMode, operand: &[u8], address: u32) -> String {
    match mode {
        AddressingMode::Implied => String::new(),
        AddressingMode::Accumulator => "A".to_string(),
        AddressingMode::Immediate8 => {
            format!("#${:02X}", operand[0])
        }
        AddressingMode::ImmediateM | AddressingMode::ImmediateX => {
            if operand.len() >= 2 {
                let value = u16::from_le_bytes([operand[0], operand[1]]);
                format!("#${value:04X}")
            } else {
                format!("#${:02X}", operand[0])
            }
        }
        AddressingMode::Immediate16 => {
            let value = u16::from_le_bytes([operand[0], operand[1]]);
            format!("#${value:04X}")
        }
        AddressingMode::DirectPage => format!("${:02X}", operand[0]),
        AddressingMode::DirectPageX => format!("${:02X},X", operand[0]),
        AddressingMode::DirectPageY => format!("${:02X},Y", operand[0]),
        AddressingMode::DirectPageIndirect => format!("(${:02X})", operand[0]),
        AddressingMode::DirectPageIndirectLong => format!("[${:02X}]", operand[0]),
        AddressingMode::DirectPageIndexedIndirectX => format!("(${:02X},X)", operand[0]),
        AddressingMode::DirectPageIndirectIndexedY => format!("(${:02X}),Y", operand[0]),
        AddressingMode::DirectPageIndirectLongIndexedY => format!("[${:02X}],Y", operand[0]),
        AddressingMode::StackRelative => format!("${:02X},S", operand[0]),
        AddressingMode::StackRelativeIndirectIndexedY => format!("(${:02X},S),Y", operand[0]),
        AddressingMode::Absolute => {
            let value = u16::from_le_bytes([operand[0], operand[1]]);
            format!("${value:04X}")
        }
        AddressingMode::AbsoluteX => {
            let value = u16::from_le_bytes([operand[0], operand[1]]);
            format!("${value:04X},X")
        }
        AddressingMode::AbsoluteY => {
            let value = u16::from_le_bytes([operand[0], operand[1]]);
            format!("${value:04X},Y")
        }
        AddressingMode::AbsoluteLong => {
            let value = u32::from(operand[0])
                | (u32::from(operand[1]) << 8)
                | (u32::from(operand[2]) << 16);
            format!("${value:06X}")
        }
        AddressingMode::AbsoluteLongX => {
            let value = u32::from(operand[0])
                | (u32::from(operand[1]) << 8)
                | (u32::from(operand[2]) << 16);
            format!("${value:06X},X")
        }
        AddressingMode::AbsoluteIndirect => {
            let value = u16::from_le_bytes([operand[0], operand[1]]);
            format!("(${value:04X})")
        }
        AddressingMode::AbsoluteIndexedIndirectX => {
            let value = u16::from_le_bytes([operand[0], operand[1]]);
            format!("(${value:04X},X)")
        }
        AddressingMode::AbsoluteIndirectLong => {
            let value = u16::from_le_bytes([operand[0], operand[1]]);
            format!("[${value:04X}]")
        }
        AddressingMode::Relative8 => {
            let delta = i8::from_le_bytes([operand[0]]) as i32;
            let target = (address.wrapping_add(2)).wrapping_add_signed(delta) & 0x00FF_FFFF;
            format!("${target:06X}")
        }
        AddressingMode::Relative16 => {
            let delta = i16::from_le_bytes([operand[0], operand[1]]) as i32;
            let target = (address.wrapping_add(3)).wrapping_add_signed(delta) & 0x00FF_FFFF;
            format!("${target:06X}")
        }
        AddressingMode::BlockMove => format!("${:02X},${:02X}", operand[1], operand[0]),
    }
}

fn matching_modes(mnemonic: &str) -> Vec<AddressingMode> {
    OPCODE_TABLE
        .iter()
        .filter(|entry| entry.mnemonic == mnemonic)
        .map(|entry| entry.mode)
        .collect()
}

fn find_opcode(mnemonic: &str, mode: AddressingMode) -> Option<u8> {
    OPCODE_TABLE
        .iter()
        .position(|entry| entry.mnemonic == mnemonic && entry.mode == mode)
        .map(|index| index as u8)
}

fn immediate_fits(mode: AddressingMode, value: i64) -> bool {
    match mode {
        AddressingMode::Immediate8 | AddressingMode::ImmediateM | AddressingMode::ImmediateX => {
            (0..=0xFF).contains(&value)
        }
        AddressingMode::Immediate16 => (0..=0xFFFF).contains(&value),
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn decodes_common_opcode() {
        let decoded = decode_instruction(&[0xA9, 0x42]).expect("decode");
        assert_eq!(decoded.mnemonic, "lda");
        assert_eq!(decoded.mode, AddressingMode::ImmediateM);
        assert_eq!(format_instruction(&decoded, 0x1000), "lda #$42");
    }

    #[test]
    fn disassembles_relative_targets() {
        let decoded = decode_instruction(&[0xD0, 0xFC]).expect("decode");
        assert_eq!(format_instruction(&decoded, 0x0010), "bne $00000E");
    }

    #[test]
    fn far_is_rejected_for_nop() {
        let err = select_encoding(
            "nop",
            OperandShape::Address {
                literal: Some(1),
                size_hint: AddressSizeHint::ForceAbsoluteLong,
                mode: AddressOperandMode::Direct { index: None },
            },
        )
        .expect_err("must fail");
        assert!(matches!(err, EncodeError::NoLongForm { .. }));
    }

    #[test]
    fn selects_jsl_long_opcode() {
        let encoding = select_encoding(
            "jsl",
            OperandShape::Address {
                literal: Some(0x1234),
                size_hint: AddressSizeHint::Auto,
                mode: AddressOperandMode::Direct { index: None },
            },
        )
        .expect("encoding");
        assert_eq!(encoding.opcode, 0x22);
        assert_eq!(encoding.mode, AddressingMode::AbsoluteLong);
    }

    #[test]
    fn selects_lda_abs_x_vs_long_x() {
        let abs = select_encoding(
            "lda",
            OperandShape::Address {
                literal: Some(0x1234),
                size_hint: AddressSizeHint::Auto,
                mode: AddressOperandMode::Direct {
                    index: Some(IndexRegister::X),
                },
            },
        )
        .expect("encoding");
        assert_eq!(abs.opcode, 0xBD);

        let long = select_encoding(
            "lda",
            OperandShape::Address {
                literal: Some(0x12_3456),
                size_hint: AddressSizeHint::Auto,
                mode: AddressOperandMode::Direct {
                    index: Some(IndexRegister::X),
                },
            },
        )
        .expect("encoding");
        assert_eq!(long.opcode, 0xBF);
    }

    #[test]
    fn selects_lda_abs_y() {
        let abs_y = select_encoding(
            "lda",
            OperandShape::Address {
                literal: Some(0x1234),
                size_hint: AddressSizeHint::Auto,
                mode: AddressOperandMode::Direct {
                    index: Some(IndexRegister::Y),
                },
            },
        )
        .expect("encoding");
        assert_eq!(abs_y.opcode, 0xB9);
        assert_eq!(abs_y.mode, AddressingMode::AbsoluteY);
    }

    #[test]
    fn selects_lda_dp_indirect_indexed_modes() {
        let ind_x = select_encoding(
            "lda",
            OperandShape::Address {
                literal: Some(0x20),
                size_hint: AddressSizeHint::Auto,
                mode: AddressOperandMode::IndexedIndirectX,
            },
        )
        .expect("encoding");
        assert_eq!(ind_x.opcode, 0xA1);
        assert_eq!(ind_x.mode, AddressingMode::DirectPageIndexedIndirectX);

        let ind_y = select_encoding(
            "lda",
            OperandShape::Address {
                literal: Some(0x20),
                size_hint: AddressSizeHint::Auto,
                mode: AddressOperandMode::IndirectIndexedY,
            },
        )
        .expect("encoding");
        assert_eq!(ind_y.opcode, 0xB1);
        assert_eq!(ind_y.mode, AddressingMode::DirectPageIndirectIndexedY);
    }

    #[test]
    fn auto_shrinks_page_zero_direct_operands() {
        let encoding = select_encoding(
            "lda",
            OperandShape::Address {
                literal: Some(0x12),
                size_hint: AddressSizeHint::Auto,
                mode: AddressOperandMode::Direct { index: None },
            },
        )
        .expect("encoding");
        assert_eq!(encoding.opcode, 0xA5);
        assert_eq!(encoding.mode, AddressingMode::DirectPage);
    }

    #[test]
    fn force_absolute16_skips_direct_page() {
        let encoding = select_encoding(
            "lda",
            OperandShape::Address {
                literal: Some(0x12),
                size_hint: AddressSizeHint::ForceAbsolute16,
                mode: AddressOperandMode::Direct { index: None },
            },
        )
        .expect("encoding");
        assert_eq!(encoding.opcode, 0xAD);
        assert_eq!(encoding.mode, AddressingMode::Absolute);
    }

    #[test]
    fn all_opcode_slots_are_populated() {
        assert_eq!(OPCODE_TABLE.len(), 256);
        assert!(OPCODE_TABLE.iter().all(|entry| !entry.mnemonic.is_empty()));
    }

    #[test]
    fn mnemonic_effects_spot_checks() {
        let lda = mnemonic_effects("lda", false);
        assert_eq!(lda.modifies, RegSet::A);
        assert!(!lda.reads.contains(RegSet::A));

        let sta = mnemonic_effects("sta", false);
        assert_eq!(sta.reads, RegSet::A);
        assert_eq!(sta.modifies, RegSet::NONE);

        let tax = mnemonic_effects("tax", false);
        assert_eq!(tax.reads, RegSet::A);
        assert_eq!(tax.modifies, RegSet::X);

        let tay = mnemonic_effects("tay", false);
        assert_eq!(tay.reads, RegSet::A);
        assert_eq!(tay.modifies, RegSet::Y);

        let tya = mnemonic_effects("tya", false);
        assert_eq!(tya.reads, RegSet::Y);
        assert_eq!(tya.modifies, RegSet::A);

        let inx = mnemonic_effects("inx", false);
        assert!(inx.reads.contains(RegSet::X));
        assert!(inx.modifies.contains(RegSet::X));

        let jsr = mnemonic_effects("jsr", false);
        assert_eq!(jsr.reads, RegSet::NONE);
        assert_eq!(jsr.modifies, RegSet::NONE);

        let rep = mnemonic_effects("rep", false);
        assert_eq!(rep.reads, RegSet::NONE);
        assert_eq!(rep.modifies, RegSet::NONE);

        let sep = mnemonic_effects("sep", false);
        assert_eq!(sep.reads, RegSet::NONE);
        assert_eq!(sep.modifies, RegSet::NONE);

        // RMW on memory touches neither reads nor modifies for A.
        let asl_mem = mnemonic_effects("asl", false);
        assert_eq!(asl_mem.reads, RegSet::NONE);
        assert_eq!(asl_mem.modifies, RegSet::NONE);

        // RMW on the accumulator both reads and modifies A.
        let asl_a = mnemonic_effects("asl", true);
        assert_eq!(asl_a.reads, RegSet::A);
        assert_eq!(asl_a.modifies, RegSet::A);

        let rol_mem = mnemonic_effects("rol", false);
        assert_eq!(rol_mem.reads, RegSet::NONE);
        assert_eq!(rol_mem.modifies, RegSet::NONE);

        let rol_a = mnemonic_effects("rol", true);
        assert_eq!(rol_a.reads, RegSet::A);
        assert_eq!(rol_a.modifies, RegSet::A);

        let mvn = mnemonic_effects("mvn", false);
        assert_eq!(mvn.reads, RegSet::A | RegSet::X | RegSet::Y);
        assert_eq!(mvn.modifies, RegSet::A | RegSet::X | RegSet::Y);
    }
}
