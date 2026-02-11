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
    NamedDataBlock(NamedDataBlock),
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
    Align(u16),
    Nocross(u16),
    Instruction(Instruction),
    Call(CallStmt),
    Bytes(Vec<Expr>),
    Hla(HlaStmt),
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
    Y,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OperandAddrMode {
    Direct,
    Indirect,
    IndexedIndirectX,
    IndirectIndexedY,
}

#[derive(Debug, Clone)]
pub enum Operand {
    Immediate(Expr),
    Value {
        expr: Expr,
        force_far: bool,
        index: Option<IndexRegister>,
        addr_mode: OperandAddrMode,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HlaRegister {
    A,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HlaCompareOp {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

#[derive(Debug, Clone)]
pub struct HlaCondition {
    pub lhs: HlaRegister,
    pub op: HlaCompareOp,
    pub rhs: Option<Expr>,
}

#[derive(Debug, Clone)]
pub enum HlaRhs {
    Immediate(Expr),
    Value {
        expr: Expr,
        index: Option<IndexRegister>,
        addr_mode: OperandAddrMode,
    },
}

#[derive(Debug, Clone)]
pub enum HlaStmt {
    XAssignImmediate { rhs: Expr },
    XIncrement,
    StoreFromA { dest: String, rhs: HlaRhs },
    WaitLoopWhileNFlagClear { symbol: String },
    ConditionSeed { lhs: HlaRegister, rhs: Expr },
    DoOpen,
    DoCloseNFlagClear,
    DoCloseNFlagSet,
    DoCloseWithOp { op: HlaCompareOp },
    DoClose { condition: HlaCondition },
    DoCloseAlways,
    DoCloseNever,
    DoCloseBranch { mnemonic: String },
    RepeatNop(usize),
    PrefixConditional {
        skip_mnemonic: String,
        body: Vec<Spanned<Stmt>>,
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
    Binary {
        op: ExprBinaryOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Unary {
        op: ExprUnaryOp,
        expr: Box<Expr>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExprBinaryOp {
    Add,
    Sub,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExprUnaryOp {
    LowByte,
    HighByte,
}

#[derive(Debug, Clone)]
pub struct NamedDataBlock {
    pub name: String,
    pub name_span: Span,
    pub entries: Vec<Spanned<NamedDataEntry>>,
}

#[derive(Debug, Clone)]
pub enum NamedDataEntry {
    Segment(SegmentDecl),
    Address(u32),
    Align(u16),
    Nocross(u16),
    Bytes(Vec<Expr>),
    String(String),
    Convert { kind: String, args: Vec<DataArg> },
    Ignored,
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
    Ignored,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DataArg {
    Int(i64),
    Str(String),
}
