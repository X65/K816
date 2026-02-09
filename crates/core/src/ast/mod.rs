use crate::span::{Span, Spanned};

#[derive(Debug, Clone, Default)]
pub struct File {
    pub items: Vec<Spanned<Item>>,
}

#[derive(Debug, Clone)]
pub enum Item {
    Segment(SegmentDecl),
    Var(VarDecl),
    DataBlock(DataBlock),
    CodeBlock(CodeBlock),
    Statement(Stmt),
}

#[derive(Debug, Clone)]
pub struct SegmentDecl {
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub name: String,
    pub array_len: Option<Expr>,
    pub initializer: Option<Expr>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BlockKind {
    Main,
    Func,
}

#[derive(Debug, Clone)]
pub struct CodeBlock {
    pub name: String,
    pub name_span: Option<Span>,
    pub kind: BlockKind,
    pub is_far: bool,
    pub is_naked: bool,
    pub is_inline: bool,
    pub body: Vec<Spanned<Stmt>>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Segment(SegmentDecl),
    Label(LabelDecl),
    Var(VarDecl),
    DataBlock(DataBlock),
    Address(u32),
    Instruction(Instruction),
    Call(CallStmt),
    Bytes(Vec<Expr>),
    Empty,
}

#[derive(Debug, Clone)]
pub struct LabelDecl {
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct Instruction {
    pub mnemonic: String,
    pub operand: Option<Operand>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IndexRegister {
    X,
}

#[derive(Debug, Clone)]
pub enum Operand {
    Immediate(Expr),
    Value {
        expr: Expr,
        force_far: bool,
        index: Option<IndexRegister>,
    },
}

#[derive(Debug, Clone)]
pub struct CallStmt {
    pub target: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Number(i64),
    Ident(String),
    EvalText(String),
}

#[derive(Debug, Clone)]
pub struct DataBlock {
    pub commands: Vec<Spanned<DataCommand>>,
}

#[derive(Debug, Clone)]
pub enum DataCommand {
    Align(u16),
    Address(u32),
    Nocross(u16),
    Convert { kind: String, args: Vec<DataArg> },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DataArg {
    Int(i64),
    Str(String),
}
