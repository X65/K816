use crate::span::Spanned;

#[derive(Debug, Clone, Default)]
pub struct Program {
    pub ops: Vec<Spanned<Op>>,
}

#[derive(Debug, Clone)]
pub enum Op {
    SelectSegment(String),
    FunctionStart(String),
    FunctionEnd,
    Label(String),
    Instruction(InstructionOp),
    EmitBytes(Vec<u8>),
    EmitRelocBytes {
        bytes: Vec<u8>,
        relocations: Vec<ByteRelocation>,
    },
    Align(u16),
    Address(u32),
    Nocross(u16),
    /// REP pseudo-op: clear processor status bits (set register to 16-bit).
    /// mask bits: 0x20 = M flag (A width), 0x10 = X flag (index width).
    Rep(u8),
    /// SEP pseudo-op: set processor status bits (set register to 8-bit).
    /// mask bits: 0x20 = M flag (A width), 0x10 = X flag (index width).
    Sep(u8),
}

#[derive(Debug, Clone)]
pub struct ByteRelocation {
    pub offset: u32,
    pub kind: ByteRelocationKind,
    pub label: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ByteRelocationKind {
    LowByte,
    HighByte,
}

#[derive(Debug, Clone)]
pub struct InstructionOp {
    pub mnemonic: String,
    pub operand: Option<OperandOp>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IndexRegister {
    X,
    Y,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AddressOperandMode {
    Direct { index: Option<IndexRegister> },
    Indirect,
    IndexedIndirectX,
    IndirectIndexedY,
}

#[derive(Debug, Clone)]
pub enum OperandOp {
    Immediate(i64),
    Address {
        value: AddressValue,
        force_far: bool,
        mode: AddressOperandMode,
    },
}

#[derive(Debug, Clone)]
pub enum AddressValue {
    Literal(u32),
    Label(String),
}
