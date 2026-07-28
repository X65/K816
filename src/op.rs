/*****************************************************************************/
/*                                                                           */
/*                                   op.rs                                   */
/*                                                                           */
/*        The common lowered operation model shared by native and HLA        */
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

//! `KOp`: the common, assembly-shaped operation both native and HLA syntax
//! lower into.
//!
//! After lowering, the optimizer and emitter must not care which syntax
//! produced an operation — only its [`Origin`] records that, for source
//! annotations. `doc/design.md` §7 is the authority for the shape.
//!
//! An [`Instruction`] stores its [`AddressingMode`] explicitly. Deriving the
//! mode from the operand shape only works while direct, absolute, and long
//! are distinguishable by shape, which stops being true the moment the ISA
//! table carries all 24 hardware modes: `lda $10`, `lda a:$10`, and
//! `lda f:$10` are one source shape and three modes. Classification chooses
//! the mode; `effects()` keys on `(mnemonic, mode)`.

use crate::effects::{Effects, instruction_effects};
use crate::isa::{AddressEncoding, AddressingMode, Mnemonic};
use crate::source::Span;
use crate::symbol::SymbolId;
use crate::value::{KExpr, Value};

pub use crate::isa::RegisterWidth;

/// Where a lowered operation came from.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Origin {
    /// Written by the user as a native instruction.
    User,
    /// Expanded from an HLA statement.
    HlaExpansion,
    /// Synthesized by the compiler (entry-mode setup, implicit `rts`).
    CompilerGenerated,
}

/// One lowered operation with its source position and origin.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct KOp {
    pub kind: KOpKind,
    pub span: Span,
    pub origin: Origin,
}

impl KOp {
    pub fn new(kind: KOpKind, span: Span, origin: Origin) -> Self {
        Self { kind, span, origin }
    }
}

/// The operation itself. `ModeAssumption` — the assembler-visible width claim
/// of `doc/design.md` §7 — arrives with sub-plan D, which owns the width
/// model that decides when a claim is justified.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum KOpKind {
    Instruction(Instruction),
    Label(SymbolId),
    /// `segment NAME` — selects the output segment for what follows.
    ///
    /// Carries the name rather than a [`SymbolId`] as sketched in
    /// `doc/design.md` §7: segment names live in ld65's namespace, not in the
    /// unit's symbol table, and interning them there would make a segment
    /// named like a function a duplicate-symbol error.
    Segment(SegmentName),
    /// Emitted bytes: `.byte` / `.word` / `.faraddr` / `.res`.
    Data(DataOp),
    /// `align N` — `.align` in ca65.
    Align(KExpr),
    /// This unit defines the symbol and other units may use it.
    Export(SymbolId),
    /// Another unit in the workspace defines the symbol.
    Import(SymbolId),
}

/// An output segment name, as ld65 sees it.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SegmentName(String);

impl SegmentName {
    pub fn new(name: impl Into<String>) -> Self {
        Self(name.into())
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl std::fmt::Display for SegmentName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

/// One emitted data directive. The entry widths beyond raw bytes belong to
/// sub-plan E's typed entries; the shape is here so C's emission machinery
/// and E's lowering meet in the same place.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DataOp {
    /// `.byte` — one byte per value.
    Byte(Vec<KExpr>),
    /// `.word` — two bytes per value.
    Word(Vec<KExpr>),
    /// `.faraddr` — three bytes per value.
    Far(Vec<KExpr>),
    /// `.res N` — N reserved bytes, the `?` placeholder's expansion.
    Reserve(KExpr),
}

/// A strongly typed instruction: validated mnemonic, chosen hardware mode,
/// and typed operand.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Instruction {
    pub mnemonic: Mnemonic,
    pub mode: AddressingMode,
    pub operand: Option<Operand>,
}

/// Which index register an operand applies.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum IndexRegister {
    X,
    Y,
}

/// A typed operand. Every value-carrying variant holds a [`KExpr`], because
/// an operand that mentions a link-time symbol must reach emission unsolved.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Operand {
    /// `#expr`. Whether ca65 prints the `#` is the ISA table's to say:
    /// `pea`, `brk`, and `cop` take a source `#` and emit without one.
    Immediate(KExpr),
    /// A memory reference with a chosen address encoding.
    Address {
        encoding: AddressEncoding,
        expr: KExpr,
        index: Option<IndexRegister>,
    },
    /// `(expr)`, `(expr,X)`, `(expr),Y`, `[expr]`, `[expr],Y`, `(expr,S),Y`.
    Indirect {
        encoding: AddressEncoding,
        expr: KExpr,
        /// `[expr]` rather than `(expr)`.
        long: bool,
        /// Indexing applied before the pointer is read, or the `S` of
        /// `(expr,S),Y` when `stack_relative` is set.
        inner: Option<IndexRegister>,
        /// Indexing applied to the pointer that was read.
        outer: Option<IndexRegister>,
        /// `(expr,S),Y` — the base is stack-relative.
        stack_relative: bool,
    },
    /// `expr,S`.
    StackRelative(KExpr),
    /// `src,dst` — the block moves' bank byte pair.
    BlockMove { source: KExpr, destination: KExpr },
    /// A branch, jump, or `PER` target.
    Label(KExpr),
}

impl Operand {
    /// The immediate's folded value, when this operand is an already-absolute
    /// immediate. `SEP`/`REP` narrow their flag writes with it.
    pub fn immediate_value(&self) -> Option<Value> {
        match self {
            Self::Immediate(expr) => expr.value(),
            Self::Address { .. }
            | Self::Indirect { .. }
            | Self::StackRelative(_)
            | Self::BlockMove { .. }
            | Self::Label(_) => None,
        }
    }
}

impl Instruction {
    pub fn new(mnemonic: Mnemonic, mode: AddressingMode, operand: Option<Operand>) -> Self {
        Self {
            mnemonic,
            mode,
            operand,
        }
    }

    /// An instruction with no operand at all.
    pub fn implied(mnemonic: Mnemonic, mode: AddressingMode) -> Self {
        Self::new(mnemonic, mode, None)
    }

    /// The ISA row this instruction matched.
    pub fn spec(&self) -> Option<&'static crate::isa::InstructionSpec> {
        crate::isa::spec(self.mnemonic, self.mode)
    }

    /// The conservative CPU effects of this instruction, derived from the
    /// ISA table (never stored on the op itself).
    pub fn effects(&self) -> Effects {
        let immediate = self
            .operand
            .as_ref()
            .and_then(Operand::immediate_value)
            .map(Value::get);
        instruction_effects(self.mnemonic, self.mode, immediate)
    }
}

/// A function's entry-mode contract: the widths its body assumes on entry.
/// `None` on an axis means the contract leaves that width unspecified.
///
/// This is the minimal slice of the width model (sub-plan A contract 6);
/// scoped blocks, exit inference, and call-site bridging land in sub-plan D.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct WidthContract {
    pub a: Option<RegisterWidth>,
    pub i: Option<RegisterWidth>,
}

/// One lowered function: its symbol, entry contract, and operation stream.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct KFunction {
    pub symbol: SymbolId,
    pub contract: WidthContract,
    pub ops: Vec<KOp>,
}

/// One entry of a unit's ordered item stream.
///
/// Top-level labels, segment selections, data, alignment, and linkage
/// directives interleave with functions in source order, and separate
/// unordered collections would lose the lexical segment state that decides
/// where each of them lands. Emission walks this stream in order.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum KProgramItem {
    Function(KFunction),
    /// A unit-level operation: a segment selection, a label, data, alignment,
    /// or a linkage directive.
    Op(KOp),
}

/// A complete lowered compilation unit.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct KProgram {
    pub items: Vec<KProgramItem>,
}

impl KProgram {
    pub fn push_function(&mut self, function: KFunction) {
        self.items.push(KProgramItem::Function(function));
    }

    pub fn push_op(&mut self, op: KOp) {
        self.items.push(KProgramItem::Op(op));
    }

    /// The unit's functions, in source order.
    pub fn functions(&self) -> impl Iterator<Item = &KFunction> {
        self.items.iter().filter_map(|item| match item {
            KProgramItem::Function(function) => Some(function),
            KProgramItem::Op(_) => None,
        })
    }

    /// The unit's functions, in source order, for passes that rewrite them.
    pub fn functions_mut(&mut self) -> impl Iterator<Item = &mut KFunction> {
        self.items.iter_mut().filter_map(|item| match item {
            KProgramItem::Function(function) => Some(function),
            KProgramItem::Op(_) => None,
        })
    }

    /// Renders the operation streams as a stable, human-readable IR dump —
    /// the `emit-ir` output that makes optimizer behavior inspectable.
    pub fn dump(
        &self,
        symbols: &crate::symbol::SymbolTable,
        map: &crate::source::SourceMap,
    ) -> String {
        let mut out = String::new();
        for (index, item) in self.items.iter().enumerate() {
            if index > 0 {
                out.push('\n');
            }
            match item {
                KProgramItem::Op(op) => {
                    let text = dump_op(op, symbols);
                    out.push_str(&format!("{:<26} ; {}\n", text, origin_note(op, map)));
                }
                KProgramItem::Function(function) => {
                    out.push_str(&format!("func {}", symbols.name(function.symbol)));
                    for (width, names) in [
                        (function.contract.a, ["@a8", "@a16"]),
                        (function.contract.i, ["@i8", "@i16"]),
                    ] {
                        match width {
                            Some(RegisterWidth::W8) => out.push_str(&format!(" {}", names[0])),
                            Some(RegisterWidth::W16) => out.push_str(&format!(" {}", names[1])),
                            None => {}
                        }
                    }
                    out.push_str(" {\n");
                    for op in &function.ops {
                        let text = dump_op(op, symbols);
                        out.push_str(&format!("  {:<24} ; {}\n", text, origin_note(op, map)));
                    }
                    out.push_str("}\n");
                }
            }
        }
        out
    }
}

/// The IR text of one operation, without its origin annotation.
fn dump_op(op: &KOp, symbols: &crate::symbol::SymbolTable) -> String {
    match &op.kind {
        KOpKind::Label(symbol) => format!("label {}", symbols.name(*symbol)),
        KOpKind::Segment(name) => format!("segment {name}"),
        KOpKind::Align(expr) => format!("align {}", dump_expr(expr, symbols)),
        KOpKind::Export(symbol) => format!("export {}", symbols.name(*symbol)),
        KOpKind::Import(symbol) => format!("import {}", symbols.name(*symbol)),
        KOpKind::Data(data) => dump_data(data, symbols),
        KOpKind::Instruction(instruction) => {
            let operand = match &instruction.operand {
                None => String::new(),
                Some(operand) => format!(" {}", dump_operand(operand, symbols)),
            };
            format!("{}{}", instruction.mnemonic.ca65(), operand)
        }
    }
}

fn dump_data(data: &DataOp, symbols: &crate::symbol::SymbolTable) -> String {
    let (keyword, values) = match data {
        DataOp::Byte(values) => ("byte", values),
        DataOp::Word(values) => ("word", values),
        DataOp::Far(values) => ("far", values),
        DataOp::Reserve(count) => return format!("res {}", dump_expr(count, symbols)),
    };
    let rendered: Vec<String> = values
        .iter()
        .map(|value| dump_expr(value, symbols))
        .collect();
    format!("{keyword} {}", rendered.join(" "))
}

fn dump_operand(operand: &Operand, symbols: &crate::symbol::SymbolTable) -> String {
    match operand {
        Operand::Immediate(expr) => format!("#{}", dump_expr(expr, symbols)),
        Operand::Label(expr) => dump_expr(expr, symbols),
        Operand::StackRelative(expr) => format!("{},s", dump_expr(expr, symbols)),
        Operand::BlockMove {
            source,
            destination,
        } => format!(
            "{},{}",
            dump_expr(source, symbols),
            dump_expr(destination, symbols)
        ),
        Operand::Address {
            encoding,
            expr,
            index,
        } => format!(
            "{} {}{}",
            encoding.k65_keyword(),
            dump_expr(expr, symbols),
            dump_index(*index)
        ),
        Operand::Indirect {
            encoding,
            expr,
            long,
            inner,
            outer,
            stack_relative,
        } => {
            let (open, close) = if *long { ("[", "]") } else { ("(", ")") };
            let inner_text = if *stack_relative {
                ",s".to_owned()
            } else {
                dump_index(*inner)
            };
            format!(
                "{open}{} {}{inner_text}{close}{}",
                encoding.k65_keyword(),
                dump_expr(expr, symbols),
                dump_index(*outer)
            )
        }
    }
}

fn dump_index(index: Option<IndexRegister>) -> String {
    match index {
        Some(IndexRegister::X) => ",x".to_owned(),
        Some(IndexRegister::Y) => ",y".to_owned(),
        None => String::new(),
    }
}

/// A stable, fully parenthesized rendering of a lowered expression.
fn dump_expr(expr: &KExpr, symbols: &crate::symbol::SymbolTable) -> String {
    match expr {
        KExpr::Number(value) => value.to_string(),
        KExpr::Symbol(symbol) => symbols.name(*symbol).to_owned(),
        KExpr::Unary { op, operand } => {
            format!("{op:?}({})", dump_expr(operand, symbols))
        }
        KExpr::Binary { op, left, right } => format!(
            "({} {} {})",
            dump_expr(left, symbols),
            op.ca65_operator(),
            dump_expr(right, symbols)
        ),
    }
}

/// `origin file:line` annotation for one dumped op.
fn origin_note(op: &KOp, map: &crate::source::SourceMap) -> String {
    let source = map.file(op.span.file);
    let line = source.line_col(op.span.start).line + 1;
    let origin = match op.origin {
        Origin::User => "user",
        Origin::HlaExpansion => "hla",
        Origin::CompilerGenerated => "compiler",
    };
    format!("{origin} {}:{line}", source.name())
}
