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
    ForceDirectPage,
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
    #[error("This operand does not support forced direct-page addressing")]
    NoDirectPageForm,
    #[error("forced direct-page operand value out of range (must fit in 0x00..=0xFF)")]
    DirectPageOutOfRange,
    #[error("immediate value out of range for the selected operand width")]
    ImmediateOutOfRange,
}

/// What an instruction does to memory at the address resolved from its operand.
///
/// Orthogonal to the M/X width axis tracked by `mnemonic_effects` /
/// `RegEffects`, which model only A/X/Y register flow. `MemoryEffect` covers
/// what the instruction does to its memory operand (or the stack, for
/// implied-stack ops). It exists so callers can answer questions like "does
/// this instruction load, store, or read-modify-write its operand?" without
/// hardcoding mnemonic lists.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemoryEffect {
    /// No operand memory access (Implied / Accumulator / Immediate* / Relative branches / control transfers / register transfers / flag toggles).
    None,
    /// Reads operand bytes (lda/cmp/adc/sbc/and/ora/eor/bit-with-address, ldx/ldy/cpx/cpy with address operand).
    Load,
    /// Writes operand bytes (sta/stx/sty/stz).
    Store,
    /// Reads then writes operand bytes (trb/tsb, asl/lsr/rol/ror/inc/dec on memory).
    Modify,
    /// Pushes onto the stack (pha/phb/phd/phk/php/phx/phy/pea/pei/per/jsr/jsl/brk/cop).
    StackPush,
    /// Pulls from the stack (pla/plb/pld/plp/plx/ply/rts/rtl/rti).
    StackPull,
    /// Block move — reads source range, writes destination range (mvn/mvp).
    BlockMove,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct OpcodeDescriptor {
    pub mnemonic: &'static str,
    pub mode: AddressingMode,
    pub memory_effect: MemoryEffect,
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
    ($mnemonic:literal, $mode:ident, $effect:ident) => {
        OpcodeDescriptor {
            mnemonic: $mnemonic,
            mode: AddressingMode::$mode,
            memory_effect: MemoryEffect::$effect,
        }
    };
}

const OPCODE_TABLE: [OpcodeDescriptor; 256] = [
    // 0x00
    op!("brk", Immediate8, StackPush),
    op!("ora", DirectPageIndexedIndirectX, Load),
    op!("cop", Immediate8, StackPush),
    op!("ora", StackRelative, Load),
    op!("tsb", DirectPage, Modify),
    op!("ora", DirectPage, Load),
    op!("asl", DirectPage, Modify),
    op!("ora", DirectPageIndirectLong, Load),
    op!("php", Implied, StackPush),
    op!("ora", ImmediateM, None),
    op!("asl", Accumulator, None),
    op!("phd", Implied, StackPush),
    op!("tsb", Absolute, Modify),
    op!("ora", Absolute, Load),
    op!("asl", Absolute, Modify),
    op!("ora", AbsoluteLong, Load),
    // 0x10
    op!("bpl", Relative8, None),
    op!("ora", DirectPageIndirectIndexedY, Load),
    op!("ora", DirectPageIndirect, Load),
    op!("ora", StackRelativeIndirectIndexedY, Load),
    op!("trb", DirectPage, Modify),
    op!("ora", DirectPageX, Load),
    op!("asl", DirectPageX, Modify),
    op!("ora", DirectPageIndirectLongIndexedY, Load),
    op!("clc", Implied, None),
    op!("ora", AbsoluteY, Load),
    op!("inc", Accumulator, None),
    op!("tcs", Implied, None),
    op!("trb", Absolute, Modify),
    op!("ora", AbsoluteX, Load),
    op!("asl", AbsoluteX, Modify),
    op!("ora", AbsoluteLongX, Load),
    // 0x20
    op!("jsr", Absolute, StackPush),
    op!("and", DirectPageIndexedIndirectX, Load),
    op!("jsl", AbsoluteLong, StackPush),
    op!("and", StackRelative, Load),
    op!("bit", DirectPage, Load),
    op!("and", DirectPage, Load),
    op!("rol", DirectPage, Modify),
    op!("and", DirectPageIndirectLong, Load),
    op!("plp", Implied, StackPull),
    op!("and", ImmediateM, None),
    op!("rol", Accumulator, None),
    op!("pld", Implied, StackPull),
    op!("bit", Absolute, Load),
    op!("and", Absolute, Load),
    op!("rol", Absolute, Modify),
    op!("and", AbsoluteLong, Load),
    // 0x30
    op!("bmi", Relative8, None),
    op!("and", DirectPageIndirectIndexedY, Load),
    op!("and", DirectPageIndirect, Load),
    op!("and", StackRelativeIndirectIndexedY, Load),
    op!("bit", DirectPageX, Load),
    op!("and", DirectPageX, Load),
    op!("rol", DirectPageX, Modify),
    op!("and", DirectPageIndirectLongIndexedY, Load),
    op!("sec", Implied, None),
    op!("and", AbsoluteY, Load),
    op!("dec", Accumulator, None),
    op!("tsc", Implied, None),
    op!("bit", AbsoluteX, Load),
    op!("and", AbsoluteX, Load),
    op!("rol", AbsoluteX, Modify),
    op!("and", AbsoluteLongX, Load),
    // 0x40
    op!("rti", Implied, StackPull),
    op!("eor", DirectPageIndexedIndirectX, Load),
    op!("wdm", Immediate8, None),
    op!("eor", StackRelative, Load),
    op!("mvp", BlockMove, BlockMove),
    op!("eor", DirectPage, Load),
    op!("lsr", DirectPage, Modify),
    op!("eor", DirectPageIndirectLong, Load),
    op!("pha", Implied, StackPush),
    op!("eor", ImmediateM, None),
    op!("lsr", Accumulator, None),
    op!("phk", Implied, StackPush),
    op!("jmp", Absolute, None),
    op!("eor", Absolute, Load),
    op!("lsr", Absolute, Modify),
    op!("eor", AbsoluteLong, Load),
    // 0x50
    op!("bvc", Relative8, None),
    op!("eor", DirectPageIndirectIndexedY, Load),
    op!("eor", DirectPageIndirect, Load),
    op!("eor", StackRelativeIndirectIndexedY, Load),
    op!("mvn", BlockMove, BlockMove),
    op!("eor", DirectPageX, Load),
    op!("lsr", DirectPageX, Modify),
    op!("eor", DirectPageIndirectLongIndexedY, Load),
    op!("cli", Implied, None),
    op!("eor", AbsoluteY, Load),
    op!("phy", Implied, StackPush),
    op!("tcd", Implied, None),
    op!("jml", AbsoluteLong, None),
    op!("eor", AbsoluteX, Load),
    op!("lsr", AbsoluteX, Modify),
    op!("eor", AbsoluteLongX, Load),
    // 0x60
    op!("rts", Implied, StackPull),
    op!("adc", DirectPageIndexedIndirectX, Load),
    op!("per", Relative16, StackPush),
    op!("adc", StackRelative, Load),
    op!("stz", DirectPage, Store),
    op!("adc", DirectPage, Load),
    op!("ror", DirectPage, Modify),
    op!("adc", DirectPageIndirectLong, Load),
    op!("pla", Implied, StackPull),
    op!("adc", ImmediateM, None),
    op!("ror", Accumulator, None),
    op!("rtl", Implied, StackPull),
    op!("jmp", AbsoluteIndirect, None),
    op!("adc", Absolute, Load),
    op!("ror", Absolute, Modify),
    op!("adc", AbsoluteLong, Load),
    // 0x70
    op!("bvs", Relative8, None),
    op!("adc", DirectPageIndirectIndexedY, Load),
    op!("adc", DirectPageIndirect, Load),
    op!("adc", StackRelativeIndirectIndexedY, Load),
    op!("stz", DirectPageX, Store),
    op!("adc", DirectPageX, Load),
    op!("ror", DirectPageX, Modify),
    op!("adc", DirectPageIndirectLongIndexedY, Load),
    op!("sei", Implied, None),
    op!("adc", AbsoluteY, Load),
    op!("ply", Implied, StackPull),
    op!("tdc", Implied, None),
    op!("jmp", AbsoluteIndexedIndirectX, None),
    op!("adc", AbsoluteX, Load),
    op!("ror", AbsoluteX, Modify),
    op!("adc", AbsoluteLongX, Load),
    // 0x80
    op!("bra", Relative8, None),
    op!("sta", DirectPageIndexedIndirectX, Store),
    op!("brl", Relative16, None),
    op!("sta", StackRelative, Store),
    op!("sty", DirectPage, Store),
    op!("sta", DirectPage, Store),
    op!("stx", DirectPage, Store),
    op!("sta", DirectPageIndirectLong, Store),
    op!("dey", Implied, None),
    op!("bit", ImmediateM, None),
    op!("txa", Implied, None),
    op!("phb", Implied, StackPush),
    op!("sty", Absolute, Store),
    op!("sta", Absolute, Store),
    op!("stx", Absolute, Store),
    op!("sta", AbsoluteLong, Store),
    // 0x90
    op!("bcc", Relative8, None),
    op!("sta", DirectPageIndirectIndexedY, Store),
    op!("sta", DirectPageIndirect, Store),
    op!("sta", StackRelativeIndirectIndexedY, Store),
    op!("sty", DirectPageX, Store),
    op!("sta", DirectPageX, Store),
    op!("stx", DirectPageY, Store),
    op!("sta", DirectPageIndirectLongIndexedY, Store),
    op!("tya", Implied, None),
    op!("sta", AbsoluteY, Store),
    op!("txs", Implied, None),
    op!("txy", Implied, None),
    op!("stz", Absolute, Store),
    op!("sta", AbsoluteX, Store),
    op!("stz", AbsoluteX, Store),
    op!("sta", AbsoluteLongX, Store),
    // 0xA0
    op!("ldy", ImmediateX, None),
    op!("lda", DirectPageIndexedIndirectX, Load),
    op!("ldx", ImmediateX, None),
    op!("lda", StackRelative, Load),
    op!("ldy", DirectPage, Load),
    op!("lda", DirectPage, Load),
    op!("ldx", DirectPage, Load),
    op!("lda", DirectPageIndirectLong, Load),
    op!("tay", Implied, None),
    op!("lda", ImmediateM, None),
    op!("tax", Implied, None),
    op!("plb", Implied, StackPull),
    op!("ldy", Absolute, Load),
    op!("lda", Absolute, Load),
    op!("ldx", Absolute, Load),
    op!("lda", AbsoluteLong, Load),
    // 0xB0
    op!("bcs", Relative8, None),
    op!("lda", DirectPageIndirectIndexedY, Load),
    op!("lda", DirectPageIndirect, Load),
    op!("lda", StackRelativeIndirectIndexedY, Load),
    op!("ldy", DirectPageX, Load),
    op!("lda", DirectPageX, Load),
    op!("ldx", DirectPageY, Load),
    op!("lda", DirectPageIndirectLongIndexedY, Load),
    op!("clv", Implied, None),
    op!("lda", AbsoluteY, Load),
    op!("tsx", Implied, None),
    op!("tyx", Implied, None),
    op!("ldy", AbsoluteX, Load),
    op!("lda", AbsoluteX, Load),
    op!("ldx", AbsoluteY, Load),
    op!("lda", AbsoluteLongX, Load),
    // 0xC0
    op!("cpy", ImmediateX, None),
    op!("cmp", DirectPageIndexedIndirectX, Load),
    op!("rep", Immediate8, None),
    op!("cmp", StackRelative, Load),
    op!("cpy", DirectPage, Load),
    op!("cmp", DirectPage, Load),
    op!("dec", DirectPage, Modify),
    op!("cmp", DirectPageIndirectLong, Load),
    op!("iny", Implied, None),
    op!("cmp", ImmediateM, None),
    op!("dex", Implied, None),
    op!("wai", Implied, None),
    op!("cpy", Absolute, Load),
    op!("cmp", Absolute, Load),
    op!("dec", Absolute, Modify),
    op!("cmp", AbsoluteLong, Load),
    // 0xD0
    op!("bne", Relative8, None),
    op!("cmp", DirectPageIndirectIndexedY, Load),
    op!("cmp", DirectPageIndirect, Load),
    op!("cmp", StackRelativeIndirectIndexedY, Load),
    op!("pei", DirectPageIndirect, StackPush),
    op!("cmp", DirectPageX, Load),
    op!("dec", DirectPageX, Modify),
    op!("cmp", DirectPageIndirectLongIndexedY, Load),
    op!("cld", Implied, None),
    op!("cmp", AbsoluteY, Load),
    op!("phx", Implied, StackPush),
    op!("stp", Implied, None),
    op!("jmp", AbsoluteIndirectLong, None),
    op!("cmp", AbsoluteX, Load),
    op!("dec", AbsoluteX, Modify),
    op!("cmp", AbsoluteLongX, Load),
    // 0xE0
    op!("cpx", ImmediateX, None),
    op!("sbc", DirectPageIndexedIndirectX, Load),
    op!("sep", Immediate8, None),
    op!("sbc", StackRelative, Load),
    op!("cpx", DirectPage, Load),
    op!("sbc", DirectPage, Load),
    op!("inc", DirectPage, Modify),
    op!("sbc", DirectPageIndirectLong, Load),
    op!("inx", Implied, None),
    op!("sbc", ImmediateM, None),
    op!("nop", Implied, None),
    op!("xba", Implied, None),
    op!("cpx", Absolute, Load),
    op!("sbc", Absolute, Load),
    op!("inc", Absolute, Modify),
    op!("sbc", AbsoluteLong, Load),
    // 0xF0
    op!("beq", Relative8, None),
    op!("sbc", DirectPageIndirectIndexedY, Load),
    op!("sbc", DirectPageIndirect, Load),
    op!("sbc", StackRelativeIndirectIndexedY, Load),
    op!("pea", Immediate16, StackPush),
    op!("sbc", DirectPageX, Load),
    op!("inc", DirectPageX, Modify),
    op!("sbc", DirectPageIndirectLongIndexedY, Load),
    op!("sed", Implied, None),
    op!("sbc", AbsoluteY, Load),
    op!("plx", Implied, StackPull),
    op!("xce", Implied, None),
    op!("jsr", AbsoluteIndexedIndirectX, StackPush),
    op!("sbc", AbsoluteX, Load),
    op!("inc", AbsoluteX, Modify),
    op!("sbc", AbsoluteLongX, Load),
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
        reads |= RegSet::A;
    }
    if x_read || block_move {
        reads |= RegSet::X;
    }
    if y_read || block_move {
        reads |= RegSet::Y;
    }
    if a_mod || block_move {
        modifies |= RegSet::A;
    }
    if x_mod || block_move {
        modifies |= RegSet::X;
    }
    if y_mod || block_move {
        modifies |= RegSet::Y;
    }

    RegEffects { reads, modifies }
}

/// Whether this mnemonic's operand-byte count tracks the M flag, the X flag,
/// or neither. Returns `(needs_m, needs_x)` where both can be true for
/// cross-bank transfers (`tax`/`tay`/`txa`/`tya`) whose two register operands
/// must agree on width.
///
/// This is *operand-width* sensitivity, not register flow — distinct from
/// `mnemonic_effects`. The two diverge in three exception classes:
///   - `stz` is M-sensitive (1- vs 2-byte store) but touches no register.
///   - `tcd`/`tcs`/`tdc`/`tsc` touch A but transfer a fixed 16-bit value
///     regardless of M.
///   - `xba` touches A but always swaps a 16-bit value regardless of M.
///
/// `on_accumulator` must be true when the operand is the accumulator
/// (`asl a`, `inc a`, …); the shift/rotate/inc/dec family is M-sensitive
/// only in that form.
pub fn mnemonic_width_sensitivity(mnemonic: &str, on_accumulator: bool) -> (bool, bool) {
    let lower = mnemonic.to_ascii_lowercase();
    match lower.as_str() {
        // Accumulator-width sensitive (M flag).
        "adc" | "and" | "bit" | "cmp" | "eor" | "lda" | "ora" | "pha" | "pla" | "sbc" | "sta"
        | "stz" | "trb" | "tsb" => (true, false),
        // Index-width sensitive (X flag, applies to both X and Y registers).
        "cpx" | "cpy" | "dex" | "dey" | "inx" | "iny" | "ldx" | "ldy" | "phx" | "phy" | "plx"
        | "ply" | "stx" | "sty" | "tsx" | "txs" | "txy" | "tyx" => (false, true),
        // Transfers crossing A and an index register depend on both axes.
        "tax" | "tay" | "txa" | "tya" => (true, true),
        // Shift/rotate/inc/dec are M-sensitive only in accumulator form.
        "asl" | "dec" | "inc" | "lsr" | "rol" | "ror" if on_accumulator => (true, false),
        _ => (false, false),
    }
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
            let wants_dp = size_hint == AddressSizeHint::ForceDirectPage;
            if wants_dp && literal.is_some_and(|value| value > 0xFF) {
                return Err(EncodeError::DirectPageOutOfRange);
            }
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

                            if wants_dp {
                                if let Some(opcode) =
                                    find_opcode(&lower, AddressingMode::DirectPageX)
                                {
                                    return Ok(Encoding {
                                        opcode,
                                        mode: AddressingMode::DirectPageX,
                                    });
                                }
                                return Err(EncodeError::NoDirectPageForm);
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
                            if wants_long
                                || wants_dp
                                || size_hint == AddressSizeHint::ForceAbsolute16
                            {
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

                            if wants_dp {
                                if let Some(opcode) =
                                    find_opcode(&lower, AddressingMode::DirectPageY)
                                {
                                    return Ok(Encoding {
                                        opcode,
                                        mode: AddressingMode::DirectPageY,
                                    });
                                }
                                return Err(EncodeError::NoDirectPageForm);
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

                            if wants_dp {
                                if let Some(opcode) =
                                    find_opcode(&lower, AddressingMode::DirectPage)
                                {
                                    return Ok(Encoding {
                                        opcode,
                                        mode: AddressingMode::DirectPage,
                                    });
                                }
                                return Err(EncodeError::NoDirectPageForm);
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

                    if wants_dp {
                        if let Some(opcode) =
                            find_opcode(&lower, AddressingMode::DirectPageIndirect)
                        {
                            return Ok(Encoding {
                                opcode,
                                mode: AddressingMode::DirectPageIndirect,
                            });
                        }
                        return Err(EncodeError::NoDirectPageForm);
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

                    if wants_dp {
                        if let Some(opcode) =
                            find_opcode(&lower, AddressingMode::DirectPageIndexedIndirectX)
                        {
                            return Ok(Encoding {
                                opcode,
                                mode: AddressingMode::DirectPageIndexedIndirectX,
                            });
                        }
                        return Err(EncodeError::NoDirectPageForm);
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
                    // (zp),y is inherently DP-only. wants_dp is permitted; wants_long is rejected.
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
                    if wants_dp {
                        if let Some(opcode) =
                            find_opcode(&lower, AddressingMode::DirectPageIndirectLong)
                        {
                            return Ok(Encoding {
                                opcode,
                                mode: AddressingMode::DirectPageIndirectLong,
                            });
                        }
                        return Err(EncodeError::NoDirectPageForm);
                    }

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
                    if let Some(opcode) = find_opcode(&lower, AddressingMode::AbsoluteIndirectLong)
                    {
                        return Ok(Encoding {
                            opcode,
                            mode: AddressingMode::AbsoluteIndirectLong,
                        });
                    }
                }
                AddressOperandMode::IndirectLongIndexedY => {
                    // [zp],y is inherently DP-only.
                    if size_hint != AddressSizeHint::Auto
                        && size_hint != AddressSizeHint::ForceDirectPage
                    {
                        return Err(EncodeError::InvalidOperand {
                            mnemonic: mnemonic.to_string(),
                        });
                    }
                    if literal.is_none() || literal.is_some_and(|value| value > 0xFF) {
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
                        || wants_dp
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
            } else if size_hint == AddressSizeHint::ForceDirectPage {
                Err(EncodeError::NoDirectPageForm)
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

/// Public, case-insensitive view of the addressing modes accepted by `mnemonic`.
/// Used by diagnostic code to tailor hints for invalid operands.
pub fn supported_modes(mnemonic: &str) -> Vec<AddressingMode> {
    matching_modes(&mnemonic.to_ascii_lowercase())
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

    #[test]
    fn memory_effect_spot_checks() {
        // Loads.
        assert_eq!(opcode_descriptor(0xA5).memory_effect, MemoryEffect::Load); // lda DirectPage
        assert_eq!(opcode_descriptor(0xAD).memory_effect, MemoryEffect::Load); // lda Absolute
        assert_eq!(opcode_descriptor(0xA6).memory_effect, MemoryEffect::Load); // ldx DirectPage
        assert_eq!(opcode_descriptor(0x2C).memory_effect, MemoryEffect::Load); // bit Absolute
        assert_eq!(opcode_descriptor(0xC5).memory_effect, MemoryEffect::Load); // cmp DirectPage
        // Stores.
        assert_eq!(opcode_descriptor(0x85).memory_effect, MemoryEffect::Store); // sta DirectPage
        assert_eq!(opcode_descriptor(0x8D).memory_effect, MemoryEffect::Store); // sta Absolute
        assert_eq!(opcode_descriptor(0x86).memory_effect, MemoryEffect::Store); // stx DirectPage
        assert_eq!(opcode_descriptor(0x64).memory_effect, MemoryEffect::Store); // stz DirectPage
        assert_eq!(opcode_descriptor(0x9C).memory_effect, MemoryEffect::Store); // stz Absolute
        // Read-modify-write on memory.
        assert_eq!(opcode_descriptor(0x06).memory_effect, MemoryEffect::Modify); // asl DirectPage
        assert_eq!(opcode_descriptor(0xE6).memory_effect, MemoryEffect::Modify); // inc DirectPage
        assert_eq!(opcode_descriptor(0xC6).memory_effect, MemoryEffect::Modify); // dec DirectPage
        assert_eq!(opcode_descriptor(0x14).memory_effect, MemoryEffect::Modify); // trb DirectPage
        assert_eq!(opcode_descriptor(0x04).memory_effect, MemoryEffect::Modify); // tsb DirectPage
        // Accumulator-form RMW: no memory access.
        assert_eq!(opcode_descriptor(0x0A).memory_effect, MemoryEffect::None); // asl Accumulator
        assert_eq!(opcode_descriptor(0x1A).memory_effect, MemoryEffect::None); // inc Accumulator
        assert_eq!(opcode_descriptor(0x3A).memory_effect, MemoryEffect::None); // dec Accumulator
        // Immediate forms: operand is data, not address.
        assert_eq!(opcode_descriptor(0xA9).memory_effect, MemoryEffect::None); // lda ImmediateM
        assert_eq!(opcode_descriptor(0x89).memory_effect, MemoryEffect::None); // bit ImmediateM
        // Stack pushes.
        assert_eq!(
            opcode_descriptor(0x48).memory_effect,
            MemoryEffect::StackPush
        ); // pha
        assert_eq!(
            opcode_descriptor(0xDA).memory_effect,
            MemoryEffect::StackPush
        ); // phx
        assert_eq!(
            opcode_descriptor(0x08).memory_effect,
            MemoryEffect::StackPush
        ); // php
        assert_eq!(
            opcode_descriptor(0x20).memory_effect,
            MemoryEffect::StackPush
        ); // jsr Absolute
        assert_eq!(
            opcode_descriptor(0x22).memory_effect,
            MemoryEffect::StackPush
        ); // jsl
        assert_eq!(
            opcode_descriptor(0x00).memory_effect,
            MemoryEffect::StackPush
        ); // brk
        assert_eq!(
            opcode_descriptor(0x02).memory_effect,
            MemoryEffect::StackPush
        ); // cop
        assert_eq!(
            opcode_descriptor(0xF4).memory_effect,
            MemoryEffect::StackPush
        ); // pea
        assert_eq!(
            opcode_descriptor(0xD4).memory_effect,
            MemoryEffect::StackPush
        ); // pei
        assert_eq!(
            opcode_descriptor(0x62).memory_effect,
            MemoryEffect::StackPush
        ); // per
        // Stack pulls.
        assert_eq!(
            opcode_descriptor(0x68).memory_effect,
            MemoryEffect::StackPull
        ); // pla
        assert_eq!(
            opcode_descriptor(0xFA).memory_effect,
            MemoryEffect::StackPull
        ); // plx
        assert_eq!(
            opcode_descriptor(0x28).memory_effect,
            MemoryEffect::StackPull
        ); // plp
        assert_eq!(
            opcode_descriptor(0x60).memory_effect,
            MemoryEffect::StackPull
        ); // rts
        assert_eq!(
            opcode_descriptor(0x6B).memory_effect,
            MemoryEffect::StackPull
        ); // rtl
        assert_eq!(
            opcode_descriptor(0x40).memory_effect,
            MemoryEffect::StackPull
        ); // rti
        // Block move.
        assert_eq!(
            opcode_descriptor(0x44).memory_effect,
            MemoryEffect::BlockMove
        ); // mvp
        assert_eq!(
            opcode_descriptor(0x54).memory_effect,
            MemoryEffect::BlockMove
        ); // mvn
        // Pure register / control flow / flag toggles.
        assert_eq!(opcode_descriptor(0xEA).memory_effect, MemoryEffect::None); // nop
        assert_eq!(opcode_descriptor(0xAA).memory_effect, MemoryEffect::None); // tax
        assert_eq!(opcode_descriptor(0xE8).memory_effect, MemoryEffect::None); // inx
        assert_eq!(opcode_descriptor(0xCA).memory_effect, MemoryEffect::None); // dex
        assert_eq!(opcode_descriptor(0x18).memory_effect, MemoryEffect::None); // clc
        assert_eq!(opcode_descriptor(0x4C).memory_effect, MemoryEffect::None); // jmp Absolute
        assert_eq!(opcode_descriptor(0x6C).memory_effect, MemoryEffect::None); // jmp AbsoluteIndirect
        assert_eq!(opcode_descriptor(0xC2).memory_effect, MemoryEffect::None); // rep
        assert_eq!(opcode_descriptor(0xE2).memory_effect, MemoryEffect::None); // sep
    }

    /// Cross-check: every opcode whose mnemonic models a register-modifying
    /// load (e.g. `lda`/`ldx`/`ldy` modify A/X/Y in `mnemonic_effects`) must
    /// also carry `MemoryEffect::Load` when it has an address-bearing mode —
    /// catches accidentally-missed annotations across the 256 entries.
    #[test]
    fn memory_effect_consistent_with_register_loads() {
        for opcode in 0u16..=0xFFu16 {
            let descriptor = opcode_descriptor(opcode as u8);
            let on_accumulator = descriptor.mode == AddressingMode::Accumulator;
            let effects = mnemonic_effects(descriptor.mnemonic, on_accumulator);
            let has_address_operand = !matches!(
                descriptor.mode,
                AddressingMode::Implied
                    | AddressingMode::Accumulator
                    | AddressingMode::Immediate8
                    | AddressingMode::Immediate16
                    | AddressingMode::ImmediateM
                    | AddressingMode::ImmediateX
                    | AddressingMode::Relative8
                    | AddressingMode::Relative16
                    | AddressingMode::BlockMove
            );

            // ldx/ldy/lda load directly into A/X/Y. With an address operand,
            // the MemoryEffect must be Load.
            if matches!(descriptor.mnemonic, "lda" | "ldx" | "ldy") && has_address_operand {
                assert_eq!(
                    descriptor.memory_effect,
                    MemoryEffect::Load,
                    "opcode {opcode:#04X} {} {:?} should be Load",
                    descriptor.mnemonic,
                    descriptor.mode
                );
                // Register modify should match.
                let expected_modify = match descriptor.mnemonic {
                    "lda" => RegSet::A,
                    "ldx" => RegSet::X,
                    "ldy" => RegSet::Y,
                    _ => unreachable!(),
                };
                assert_eq!(effects.modifies, expected_modify);
            }

            // sta/stx/sty/stz: must be Store with an address operand.
            if matches!(descriptor.mnemonic, "sta" | "stx" | "sty" | "stz") && has_address_operand {
                assert_eq!(
                    descriptor.memory_effect,
                    MemoryEffect::Store,
                    "opcode {opcode:#04X} {} {:?} should be Store",
                    descriptor.mnemonic,
                    descriptor.mode
                );
            }

            // RMW mnemonics on memory must be Modify.
            if matches!(
                descriptor.mnemonic,
                "asl" | "lsr" | "rol" | "ror" | "inc" | "dec" | "trb" | "tsb"
            ) && has_address_operand
            {
                assert_eq!(
                    descriptor.memory_effect,
                    MemoryEffect::Modify,
                    "opcode {opcode:#04X} {} {:?} should be Modify",
                    descriptor.mnemonic,
                    descriptor.mode
                );
            }

            // Accumulator-form RMW must be None.
            if matches!(
                descriptor.mnemonic,
                "asl" | "lsr" | "rol" | "ror" | "inc" | "dec"
            ) && on_accumulator
            {
                assert_eq!(
                    descriptor.memory_effect,
                    MemoryEffect::None,
                    "opcode {opcode:#04X} {} Accumulator should be None",
                    descriptor.mnemonic,
                );
            }
        }
    }

    #[test]
    fn width_sensitivity_axes() {
        // M-only.
        assert_eq!(mnemonic_width_sensitivity("lda", false), (true, false));
        assert_eq!(mnemonic_width_sensitivity("sta", false), (true, false));
        assert_eq!(mnemonic_width_sensitivity("stz", false), (true, false));
        assert_eq!(mnemonic_width_sensitivity("trb", false), (true, false));
        assert_eq!(mnemonic_width_sensitivity("pha", false), (true, false));
        assert_eq!(mnemonic_width_sensitivity("pla", false), (true, false));
        // X-only.
        assert_eq!(mnemonic_width_sensitivity("ldx", false), (false, true));
        assert_eq!(mnemonic_width_sensitivity("cpy", false), (false, true));
        assert_eq!(mnemonic_width_sensitivity("inx", false), (false, true));
        assert_eq!(mnemonic_width_sensitivity("txs", false), (false, true));
        // Transfers — both axes.
        assert_eq!(mnemonic_width_sensitivity("tax", false), (true, true));
        assert_eq!(mnemonic_width_sensitivity("tay", false), (true, true));
        assert_eq!(mnemonic_width_sensitivity("txa", false), (true, true));
        assert_eq!(mnemonic_width_sensitivity("tya", false), (true, true));
        // Exceptions: touch A but transfer a fixed 16-bit value, so not M-sensitive.
        assert_eq!(mnemonic_width_sensitivity("tcd", false), (false, false));
        assert_eq!(mnemonic_width_sensitivity("tcs", false), (false, false));
        assert_eq!(mnemonic_width_sensitivity("tdc", false), (false, false));
        assert_eq!(mnemonic_width_sensitivity("tsc", false), (false, false));
        assert_eq!(mnemonic_width_sensitivity("xba", false), (false, false));
        // Shift/rotate/inc/dec: M-sensitive only on the accumulator form.
        assert_eq!(mnemonic_width_sensitivity("asl", true), (true, false));
        assert_eq!(mnemonic_width_sensitivity("asl", false), (false, false));
        assert_eq!(mnemonic_width_sensitivity("inc", true), (true, false));
        assert_eq!(mnemonic_width_sensitivity("inc", false), (false, false));
        assert_eq!(mnemonic_width_sensitivity("rol", true), (true, false));
        assert_eq!(mnemonic_width_sensitivity("ror", false), (false, false));
        // Pure control flow / flag toggles: insensitive.
        assert_eq!(mnemonic_width_sensitivity("nop", false), (false, false));
        assert_eq!(mnemonic_width_sensitivity("jmp", false), (false, false));
        assert_eq!(mnemonic_width_sensitivity("clc", false), (false, false));
        assert_eq!(mnemonic_width_sensitivity("rts", false), (false, false));
        // Case-insensitive.
        assert_eq!(mnemonic_width_sensitivity("LDA", false), (true, false));
    }
}
