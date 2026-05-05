use crate::ast::ModeContract;
use crate::span::{Span, Spanned};

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
        /// True for `func main` blocks — defaults to 8-bit mode when uncolored.
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
    Align {
        boundary: u16,
        offset: u16,
    },
    Address(u32),
    Nocross(u16),
    /// REP pseudo-op: clear processor status bits (set register to 16-bit).
    /// mask bits: 0x20 = M flag (A width), 0x10 = X flag (index width).
    /// `fixed: true` anchors the op at a label bridge point — never removed or
    /// folded away by mode optimization passes.
    Rep {
        mask: u8,
        fixed: bool,
    },
    /// SEP pseudo-op: set processor status bits (set register to 8-bit).
    /// mask bits: 0x20 = M flag (A width), 0x10 = X flag (index width).
    /// `fixed: true` anchors the op at a label bridge point — never removed or
    /// folded away by mode optimization passes.
    Sep {
        mask: u8,
        fixed: bool,
    },
    /// Defines an absolute symbol exported to object/link stage.
    DefineAbsoluteSymbol {
        name: String,
        address: u32,
    },
    /// Defines a section-relative symbol at the current segment's current
    /// emit offset. Used for `var` declarations whose address the linker
    /// assigns from segment placement (no compile-time initializer). The op
    /// behaves like `Op::Label` for symbol registration but is kept distinct
    /// so peephole/format passes can tell var symbols apart from code
    /// labels.
    DefineSectionSymbol {
        name: String,
    },
    /// Pins a `var dp` to a specific 8-bit DP offset. Emitted from a
    /// declaration with explicit initializer (`var dp foo = $42`). The linker
    /// records the slot as occupied so auto-allocated DP vars skip it;
    /// multiple DP vars may share the same offset by design (intentional
    /// aliasing).
    DefineDpFixedSymbol {
        name: String,
        offset: u8,
    },
    /// Requests an auto-allocated DP slot of `size` bytes. The linker assigns
    /// a final 8-bit offset by first-fit across all input objects in
    /// link-input order, after honoring all `DefineDpFixedSymbol` pins.
    /// `size` is bounded by 256 — sema rejects anything larger.
    DefineDpAllocSymbol {
        name: String,
        size: u8,
    },
    /// Aliases a symbolic-subscript field name to `parent`'s DP slot plus
    /// `field_offset`. Resolved at link time after `parent` receives its
    /// auto-allocated DP offset; the alias inherits `parent_offset + field_offset`.
    DefineDpAllocAlias {
        name: String,
        parent: String,
        field_offset: u8,
    },
    /// Sets the register width state for the emitter without emitting any bytes.
    /// Used by code blocks inside data sections where 8-bit mode is implied.
    SetMode(ModeContract),
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
    FullWord,
    FullLong,
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
    S,
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
pub enum AddressSizeHint {
    Auto,
    ForceDirectPage,
    ForceAbsolute16,
    ForceAbsoluteLong,
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
    /// 16-bit immediate address-of, from `&&label` (with optional `+N` addend).
    /// Resolved during emit/link as a 2-byte word relocation against `label`.
    /// Requires the surrounding accumulator/index width to be 16-bit.
    /// `label_span` covers just the symbol name in source — used by the
    /// compile-time `&&` validator to anchor diagnostics on the symbol rather
    /// than on the whole instruction.
    ImmediateWordRelocation {
        label: String,
        addend: i32,
        label_span: Option<Span>,
    },
    /// 24-bit immediate far address-of, from `&&&label` (with optional `+N`
    /// addend). Resolved during emit/link as a 3-byte far relocation. Requires
    /// the surrounding accumulator width to be 16-bit (`@a16` / `@al`).
    ImmediateFarRelocation {
        label: String,
        addend: i32,
        label_span: Option<Span>,
    },
    Address {
        value: AddressValue,
        size_hint: AddressSizeHint,
        mode: AddressOperandMode,
    },
    BlockMove {
        src: u8,
        dst: u8,
    },
}

#[derive(Debug, Clone)]
pub enum AddressValue {
    Literal(u32),
    Label(String),
    LabelOffset { label: String, addend: i32 },
}
