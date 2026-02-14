use crate::ast::ModeContract;
use crate::span::Spanned;

#[derive(Debug, Clone, Default)]
pub struct Program {
    pub ops: Vec<Spanned<Op>>,
}

#[derive(Debug, Clone)]
pub enum Op {
    SelectSegment(String),
    FunctionStart {
        name: String,
        mode_contract: ModeContract,
        /// True for `main` blocks — defaults to 8-bit mode when uncolored.
        is_entry: bool,
        /// True for `far func` blocks — uses JSL/RTL calling convention.
        is_far: bool,
    },
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
    /// Fixed REP pseudo-op anchored at a label bridge point.
    /// Never removed or folded away by mode optimization passes.
    FixedRep(u8),
    /// Fixed SEP pseudo-op anchored at a label bridge point.
    /// Never removed or folded away by mode optimization passes.
    FixedSep(u8),
    /// Defines an absolute symbol exported to object/link stage.
    DefineAbsoluteSymbol {
        name: String,
        address: u32,
    },
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
    /// Immediate operand derived from `&<label` / `&>label`.
    /// Resolved during emit/link as a byte relocation against `label`.
    ImmediateByteRelocation {
        kind: ByteRelocationKind,
        label: String,
    },
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
