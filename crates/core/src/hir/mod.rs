use crate::span::Spanned;

#[derive(Debug, Clone, Default)]
pub struct Program {
    pub ops: Vec<Spanned<Op>>,
}

#[derive(Debug, Clone)]
pub enum Op {
    SelectBank(String),
    Label(String),
    Instruction(InstructionOp),
    EmitBytes(Vec<u8>),
    Align(u16),
    Address(u32),
    Nocross(u16),
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
    },
}

#[derive(Debug, Clone)]
pub enum AddressValue {
    Literal(u32),
    Label(String),
}
