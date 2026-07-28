/*****************************************************************************/
/*                                                                           */
/*                                emit_ca65.rs                               */
/*                                                                           */
/*           Readable ca65 emission with source/debug annotations            */
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

//! ca65 emission: readable assembly with a source or debug annotation for
//! every emitted operation.
//!
//! Every instruction is preceded by a `.dbg line` directive pointing at its
//! K816 origin line (ld65 then records K816 source ownership in the debug
//! database) and carries a trailing comment quoting the source statement.
//! K816 never resolves addresses that belong to ld65: memory operands are
//! emitted as symbol references; only user-pinned fixed addresses become
//! equates.
//!
//! Emitted expressions are **fully parenthesized** and use ca65's named
//! operator forms. ca65's precedence table is not K65's — `&`, `^`, `<<`,
//! and `>>` sit alongside `*` there — so `A + B & C` means `(A+B)&C` in K65
//! and `A + (B&C)` in ca65. Concatenating operator text without parentheses
//! would silently change a working program's numbers, which is why
//! [`format_expr`] never does (sub-plan C, backend decision 1).

use std::collections::HashMap;

use crate::effects::WidthEffect;
use crate::isa::{ImmediateWidthClass, Mnemonic, RegisterWidth, spec};
use crate::op::{
    DataOp, IndexRegister, Instruction, KFunction, KOp, KOpKind, KProgram, KProgramItem, Operand,
    Origin, WidthContract,
};
use crate::semantic::SemanticModel;
use crate::source::{FileId, SourceMap, Span};
use crate::symbol::{LabelScope, SymbolId, SymbolKind, SymbolTable};
use crate::value::{KExpr, UnaryOp, Value};

/// The ca65 spelling chosen for every symbol, decided once per unit.
///
/// A K65 function-local `.name` label normally becomes a ca65 cheap local,
/// `@name`. K65 scopes a local label to its enclosing function and ca65
/// scopes a cheap local to the preceding non-cheap label, so the mapping is
/// exact *as long as the function emits exactly one non-cheap label* — its
/// own name (sub-plan C, backend decision 4).
///
/// A global label written inside a function body breaks that premise: ca65
/// starts a new cheap-local scope there, and a later reference to an earlier
/// `@name` becomes "Symbol '@name' is undefined" — an error far from its
/// cause. Such a function's local labels are therefore emitted as unit-level
/// identifiers qualified by the function name instead. The readable form is
/// kept wherever it is correct, and the correct form is used everywhere.
struct Names<'a> {
    symbols: &'a SymbolTable,
    /// Local labels that cannot be cheap, and the name they take instead.
    qualified: HashMap<SymbolId, String>,
}

impl<'a> Names<'a> {
    fn new(symbols: &'a SymbolTable, program: &KProgram) -> Self {
        let mut qualified = HashMap::new();
        for function in program.functions() {
            if !splits_cheap_local_scope(symbols, function) {
                continue;
            }
            let prefix = symbols.name(function.symbol).to_owned();
            for op in &function.ops {
                let KOpKind::Label(id) = &op.kind else {
                    continue;
                };
                if !matches!(
                    symbols.kind(*id),
                    SymbolKind::Label {
                        scope: LabelScope::Local,
                        ..
                    }
                ) {
                    continue;
                }
                let bare = symbols.name(*id).trim_start_matches('.');
                let mut name = format!("{prefix}_{bare}");
                // A user symbol may already own that spelling; lengthening
                // is deterministic, so the choice stays stable across runs.
                while symbols.lookup(&name).is_some() {
                    name.push('_');
                }
                qualified.insert(*id, name);
            }
        }
        Self { symbols, qualified }
    }

    /// The ca65 identifier for `id`.
    fn of(&self, id: SymbolId) -> String {
        if let Some(name) = self.qualified.get(&id) {
            return name.clone();
        }
        let symbol = self.symbols.symbol(id);
        match symbol.kind {
            SymbolKind::Label {
                scope: LabelScope::Local,
                ..
            } => format!("@{}", symbol.name.trim_start_matches('.')),
            SymbolKind::Label { .. }
            | SymbolKind::DataLabel { .. }
            | SymbolKind::Var { .. }
            | SymbolKind::Function { .. }
            | SymbolKind::Const { .. } => symbol.name.clone(),
        }
    }
}

/// True when `function` emits a non-cheap label other than its own.
fn splits_cheap_local_scope(symbols: &SymbolTable, function: &KFunction) -> bool {
    function.ops.iter().any(|op| match &op.kind {
        KOpKind::Label(id) => {
            *id != function.symbol
                && !matches!(
                    symbols.kind(*id),
                    SymbolKind::Label {
                        scope: LabelScope::Local,
                        ..
                    }
                )
        }
        _ => false,
    })
}

/// Emits the whole lowered `program` as ca65 assembly.
pub fn emit(map: &SourceMap, file: FileId, model: &SemanticModel, program: &KProgram) -> String {
    let source = map.file(file);
    let mut out = String::new();

    out.push_str(&format!("; Generated by k816 from {}\n", source.name()));
    out.push_str(".setcpu \"65816\"\n");
    out.push_str(".debuginfo on\n");
    out.push_str(&format!(".dbg    file, \"{}\", 0, 0\n", source.name()));

    let mut equates = String::new();
    for (_, symbol) in model.symbols.iter() {
        if let SymbolKind::Var {
            address: Some(address),
            ..
        } = symbol.kind
        {
            let target = format!("{:<15} := {}", symbol.name, format_address(address));
            equates.push_str(&annotated(&target, &line_comment(map, symbol.declaration)));
        }
    }
    if !equates.is_empty() {
        out.push('\n');
        out.push_str(&equates);
    }

    // Functions are visible across modules, so hand-written ca65 code (and
    // later K816 units) can reference them; the derived import/export model
    // lands with task C7.
    let mut exports = String::new();
    for function in program.functions() {
        exports.push_str(&format!(
            ".export {}\n",
            model.symbols.name(function.symbol)
        ));
    }
    if !exports.is_empty() {
        out.push('\n');
        out.push_str(&exports);
    }

    out.push_str("\n.segment \"CODE\"\n");

    let names = Names::new(&model.symbols, program);
    for item in &program.items {
        match item {
            KProgramItem::Op(op) => emit_unit_op(map, source.name(), &names, op, &mut out),
            KProgramItem::Function(function) => {
                out.push('\n');
                let mut widths = WidthState::default();
                let mut assumed = false;
                for op in &function.ops {
                    // The entry contract's width directives follow the
                    // function label, before the first instruction.
                    if !assumed && !matches!(op.kind, KOpKind::Label(_)) {
                        widths.assume(function.contract, &mut out);
                        assumed = true;
                    }
                    match &op.kind {
                        KOpKind::Instruction(instruction) => emit_instruction(
                            map,
                            source.name(),
                            &names,
                            instruction,
                            op,
                            &mut widths,
                            &mut out,
                        ),
                        _ => emit_unit_op(map, source.name(), &names, op, &mut out),
                    }
                }
            }
        }
    }
    out
}

/// Emits a non-instruction operation: a label or one of the directives.
///
/// The *decision* to produce these — segment selection, derived linkage,
/// data blocks, alignment — belongs to tasks C7 and C8; this is only their
/// ca65 spelling, so both tasks extend the callers rather than the printer.
fn emit_unit_op(map: &SourceMap, file_name: &str, names: &Names<'_>, op: &KOp, out: &mut String) {
    match &op.kind {
        KOpKind::Instruction(_) => unreachable!("instructions are emitted with their width state"),
        KOpKind::Label(symbol) => out.push_str(&format!("{}:\n", names.of(*symbol))),
        KOpKind::Export(symbol) => out.push_str(&format!(".export {}\n", names.of(*symbol))),
        KOpKind::Import(symbol) => out.push_str(&format!(".import {}\n", names.of(*symbol))),
        KOpKind::Segment(name) => out.push_str(&annotated(
            &format!(".segment \"{name}\""),
            &source_comment(map, op.span),
        )),
        KOpKind::Align(expr) => out.push_str(&annotated(
            &format!("        .align   {}", format_expr(expr, names)),
            &source_comment(map, op.span),
        )),
        KOpKind::Data(data) => {
            // Data carries the same debug annotation as an instruction, so
            // ld65's database records it as K816-owned too.
            let line = map.line_col(op.span.file, op.span.start).line + 1;
            out.push_str(&format!("        .dbg    line, \"{file_name}\", {line}\n"));
            let list = |values: &[KExpr]| {
                values
                    .iter()
                    .map(|value| format_expr(value, names))
                    .collect::<Vec<_>>()
                    .join(", ")
            };
            let text = match data {
                DataOp::Byte(values) => format!("        .byte    {}", list(values)),
                DataOp::Word(values) => format!("        .word    {}", list(values)),
                DataOp::Far(values) => format!("        .faraddr {}", list(values)),
                DataOp::Reserve(count) => {
                    format!("        .res     {}", format_expr(count, names))
                }
            };
            out.push_str(&annotated(&text, &source_comment(map, op.span)));
        }
    }
}

/// Tracked assembler width state, kept in sync with `.a8`-family directives
/// so ca65 sizes width-dependent immediates exactly as the width model does.
#[derive(Default)]
struct WidthState {
    a: Option<RegisterWidth>,
    i: Option<RegisterWidth>,
}

impl WidthState {
    /// Establishes a function's entry contract as the assumed widths.
    fn assume(&mut self, contract: WidthContract, out: &mut String) {
        if let Some(width) = contract.a {
            self.set_a(width, out);
        }
        if let Some(width) = contract.i {
            self.set_i(width, out);
        }
    }

    fn set_a(&mut self, width: RegisterWidth, out: &mut String) {
        if self.a != Some(width) {
            self.a = Some(width);
            out.push_str(match width {
                RegisterWidth::W8 => "        .a8\n",
                RegisterWidth::W16 => "        .a16\n",
            });
        }
    }

    fn set_i(&mut self, width: RegisterWidth, out: &mut String) {
        if self.i != Some(width) {
            self.i = Some(width);
            out.push_str(match width {
                RegisterWidth::W8 => "        .i8\n",
                RegisterWidth::W16 => "        .i16\n",
            });
        }
    }

    /// Applies the width transition of an executed instruction.
    ///
    /// Driven by the effect table's [`WidthEffect`], not by a match over
    /// mnemonics: a `FlagSet` only says that M or X was written, and a
    /// mnemonic match would need one arm per instruction. An unknown
    /// transition drops the tracked widths so nothing downstream emits a
    /// width claim K816 cannot justify.
    fn apply(&mut self, width: WidthEffect, out: &mut String) {
        match width {
            WidthEffect::Unchanged => {}
            WidthEffect::Known { a, i } => {
                if let Some(a) = a {
                    self.set_a(a, out);
                }
                if let Some(i) = i {
                    self.set_i(i, out);
                }
            }
            WidthEffect::Unknown => {
                self.a = None;
                self.i = None;
            }
        }
    }
}

fn emit_instruction(
    map: &SourceMap,
    file_name: &str,
    names: &Names<'_>,
    instruction: &Instruction,
    op: &KOp,
    widths: &mut WidthState,
    out: &mut String,
) {
    // Debug annotation: ld65 records this op as owned by the K816 source.
    let line = map.line_col(op.span.file, op.span.start).line + 1;
    out.push_str(&format!("        .dbg    line, \"{file_name}\", {line}\n"));

    let operand = instruction
        .operand
        .as_ref()
        .map(|operand| format_operand(instruction, operand, widths, names));
    let mut text = format!("        {:<7}", instruction.mnemonic.ca65());
    if let Some(operand) = &operand {
        text.push_str(&format!(" {operand}"));
    }

    let comment = match op.origin {
        Origin::User | Origin::HlaExpansion => source_comment(map, op.span),
        Origin::CompilerGenerated => format!("k816: {}", generated_note(instruction)),
    };
    out.push_str(&annotated(text.trim_end(), &comment));

    // Keep the assembler's width tracking in sync after mode changes.
    widths.apply(instruction.effects().width, out);
}

fn format_operand(
    instruction: &Instruction,
    operand: &Operand,
    widths: &WidthState,
    names: &Names<'_>,
) -> String {
    match operand {
        Operand::Immediate(expr) => {
            let spec = spec(instruction.mnemonic, instruction.mode);
            let sigil = spec.is_none_or(|spec| spec.operand.ca65_immediate_sigil);
            let width_class = spec.and_then(|spec| spec.immediate_width());
            let text = match expr.value() {
                Some(value) => format_immediate_value(value, width_class, widths),
                None => format_expr(expr, names),
            };
            if sigil { format!("#{text}") } else { text }
        }
        // The symbol is emitted by name; ca65/ld65 own resolution. The
        // explicit `z:`/`a:`/`f:` address-size prefix arrives with task C6,
        // together with the golden it changes.
        Operand::Address { expr, index, .. } => {
            format!("{}{}", format_expr(expr, names), format_index(*index))
        }
        Operand::Label(expr) => format_expr(expr, names),
        Operand::StackRelative(expr) => format!("{},s", format_expr(expr, names)),
        Operand::BlockMove {
            source,
            destination,
        } => format!(
            "{},{}",
            format_expr(source, names),
            format_expr(destination, names)
        ),
        Operand::Indirect {
            expr,
            long,
            inner,
            outer,
            stack_relative,
            ..
        } => {
            let (open, close) = if *long { ("[", "]") } else { ("(", ")") };
            let inner_text = if *stack_relative {
                ",s".to_owned()
            } else {
                format_index(*inner)
            };
            format!(
                "{open}{}{inner_text}{close}{}",
                format_expr(expr, names),
                format_index(*outer)
            )
        }
    }
}

fn format_index(index: Option<IndexRegister>) -> String {
    match index {
        Some(IndexRegister::X) => ",x".to_owned(),
        Some(IndexRegister::Y) => ",y".to_owned(),
        None => String::new(),
    }
}

/// An absolute immediate, printed at the width the instruction encodes.
fn format_immediate_value(
    value: Value,
    width_class: Option<ImmediateWidthClass>,
    widths: &WidthState,
) -> String {
    let wide = match width_class {
        Some(ImmediateWidthClass::Fixed16) => true,
        Some(ImmediateWidthClass::MDependent) => widths.a == Some(RegisterWidth::W16),
        Some(ImmediateWidthClass::XDependent) => widths.i == Some(RegisterWidth::W16),
        Some(ImmediateWidthClass::Fixed8) | None => false,
    };
    if wide {
        format!("${:04X}", value.get())
    } else {
        format!("${:02X}", value.get())
    }
}

/// A ca65 expression, fully parenthesized and spelled with ca65's named
/// operators. See the module documentation for why nothing here relies on
/// ca65 and K65 agreeing about precedence.
fn format_expr(expr: &KExpr, names: &Names<'_>) -> String {
    match expr {
        KExpr::Number(value) => value.to_string(),
        KExpr::Symbol(symbol) => names.of(*symbol),
        KExpr::Unary { op, operand } => {
            let inner = format_expr(operand, names);
            match op.ca65_function() {
                Some(function) => format!("{function}({inner})"),
                // `&&&x` is already a 24-bit address value in ca65 terms;
                // negation and complement are the two operators with no
                // named form.
                None => match op {
                    UnaryOp::Negate => format!("(0 - {inner})"),
                    UnaryOp::BitNot => format!("(~ {inner})"),
                    UnaryOp::Long => inner,
                    UnaryOp::LowByte | UnaryOp::HighByte | UnaryOp::BankByte | UnaryOp::Word => {
                        unreachable!("these operators have a named ca65 form")
                    }
                },
            }
        }
        KExpr::Binary { op, left, right } => format!(
            "({} {} {})",
            format_expr(left, names),
            op.ca65_operator(),
            format_expr(right, names)
        ),
    }
}

fn format_address(address: u32) -> String {
    if address <= 0xFFFF {
        format!("${address:04X}")
    } else {
        format!("${address:06X}")
    }
}

/// `text` padded to the comment column with `; comment` appended.
fn annotated(text: &str, comment: &str) -> String {
    format!("{text:<39} ; {comment}\n")
}

/// `file:line: source text` for a user-written operation.
fn source_comment(map: &SourceMap, span: Span) -> String {
    let source = map.file(span.file);
    let line = source.line_col(span.start).line;
    format!(
        "{}:{}: {}",
        source.name(),
        line + 1,
        source.span_text(span).trim()
    )
}

/// `file:line: whole source line`, for declarations whose recorded span
/// covers only the declaring name.
fn line_comment(map: &SourceMap, span: Span) -> String {
    let source = map.file(span.file);
    let line = source.line_col(span.start).line;
    format!(
        "{}:{}: {}",
        source.name(),
        line + 1,
        source.line_text(line).trim()
    )
}

/// A short explanation for compiler-generated operations.
fn generated_note(instruction: &Instruction) -> &'static str {
    match instruction.mnemonic {
        Mnemonic::Sep => "entry width setup (8-bit)",
        Mnemonic::Rep => "entry width setup (16-bit)",
        Mnemonic::Rts => "implicit return",
        Mnemonic::Lda | Mnemonic::Sta => "generated",
    }
}
