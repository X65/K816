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

#[derive(Debug, Clone)]
pub enum OperandOp {
    Immediate(i64),
    Address {
        value: AddressValue,
        force_far: bool,
        index_x: bool,
    },
}

#[derive(Debug, Clone)]
pub enum AddressValue {
    Literal(u32),
    Label(String),
}
