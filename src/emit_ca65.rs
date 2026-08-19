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
use crate::isa::{ImmediateWidthClass, RegisterWidth, spec};
use crate::linkage::Linkage;
use crate::op::{
    AxisState, DataOp, EmulationState, IndexRegister, Instruction, KFunction, KOp, KOpKind,
    KProgram, KProgramItem, ModeState, Operand, Origin, Reason,
};
use crate::semantic::SemanticModel;
use crate::source::{FileId, SourceMap, Span};
use crate::symbol::{AddressSize, LabelScope, SymbolId, SymbolKind, SymbolTable};
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
            | SymbolKind::AbstractVar { .. }
            | SymbolKind::Function { .. }
            | SymbolKind::Const { .. } => symbol.name.clone(),
        }
    }
}

/// True when this unit exports `id`.
///
/// A function, a global label, and a data-block label name *places*, and a
/// place is the only thing another unit can be handed, so they are always
/// exported. A `const` or a pinned `var` is a value K816 folds; exporting
/// one nobody reads would put a name in the object file for no reason, so
/// those wait to be asked for (`plans/feature-manifest.md` §3).
fn exported(
    model: &SemanticModel,
    linkage: &Linkage,
    file: FileId,
    id: crate::symbol::SymbolId,
) -> bool {
    let symbol = model.symbols.symbol(id);
    if symbol.imported || !symbol.is_unit_level() {
        return false;
    }
    crate::linkage::always_exported(symbol.kind) || linkage.referenced_elsewhere(&symbol.name, file)
}

/// One `.import`/`.export` line, qualified by the symbol's address size.
fn linkage_directive(direction: &str, name: &str, size: Option<AddressSize>) -> String {
    match size {
        // ca65 spells the direct-page forms with their own directive.
        Some(AddressSize::DirectPage) => format!(".{direction}zp {name}"),
        Some(AddressSize::Long) => format!(".{direction} {name}: far"),
        Some(AddressSize::Absolute) | None => format!(".{direction} {name}"),
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
pub fn emit(
    map: &SourceMap,
    file: FileId,
    model: &SemanticModel,
    program: &KProgram,
    linkage: &Linkage,
) -> String {
    let source = map.file(file);
    let mut out = String::new();

    out.push_str(&format!("; Generated by k816 from {}\n", source.name()));
    out.push_str(".setcpu \"65816\"\n");
    out.push_str(".debuginfo on\n");
    out.push_str(&format!(".dbg    file, \"{}\", 0, 0\n", source.name()));

    // A pinned variable and an exported constant are both equates: no bytes,
    // just a name ca65 and ld65 can resolve.
    let mut equates = String::new();
    for (_, symbol) in model.symbols.iter() {
        if symbol.imported {
            continue;
        }
        let value = match symbol.kind {
            SymbolKind::Var {
                address: Some(address),
                ..
            } => Some(format_address(address)),
            _ => None,
        };
        if let Some(value) = value {
            let target = format!("{:<15} := {value}", symbol.name);
            equates.push_str(&annotated(&target, &line_comment(map, symbol.declaration)));
        }
    }
    if !equates.is_empty() {
        out.push('\n');
        out.push_str(&equates);
    }

    // Linkage is derived from the symbol tables, because K65 has no syntax
    // to declare it. Both directions carry the symbol's address size: ca65
    // validates addressing modes against an imported symbol's, so a missing
    // qualifier is an assembly error rather than a wrong byte.
    let mut linkage_directives = String::new();
    for (id, symbol) in model.symbols.iter() {
        // A `const` has no address, so it has nothing to link: it folds
        // into its value wherever it is used, in this unit or another.
        if !symbol.is_unit_level() || matches!(symbol.kind, SymbolKind::Const { .. }) {
            continue;
        }
        let size = symbol.kind.address_size();
        if symbol.imported {
            // Only a name this unit actually used needs importing.
            if model.referenced.contains(&id) {
                let where_from = map.file(symbol.declaration.file);
                let line = where_from.line_col(symbol.declaration.start).line + 1;
                linkage_directives.push_str(&annotated(
                    &linkage_directive("import", &symbol.name, size),
                    &format!("k816: defined by {}:{line}", where_from.name()),
                ));
            }
        } else if exported(model, linkage, file, id) {
            linkage_directives.push_str(&annotated(
                &linkage_directive("export", &symbol.name, size),
                &format!(
                    "k816: {} defined here, visible to other units",
                    symbol.kind.describe()
                ),
            ));
        }
    }
    if !linkage_directives.is_empty() {
        out.push('\n');
        out.push_str(&linkage_directives);
    }

    out.push_str(&format!(
        "\n{}",
        annotated(
            "        .segment \"CODE\"",
            "k816: the unit's default segment",
        )
    ));

    let names = Names::new(&model.symbols, program);
    let unit = Unit {
        map,
        file_name: source.name(),
        names: &names,
    };
    for item in &program.items {
        match item {
            KProgramItem::Op(op) => emit_unit_op(&unit, op, &mut out),
            KProgramItem::Function(function) => {
                out.push('\n');
                let mut widths = WidthState::default();
                let references = unnamed_references(&function.ops);
                for (index, op) in function.ops.iter().enumerate() {
                    match &op.kind {
                        KOpKind::Instruction(instruction) => {
                            emit_instruction(
                                &unit,
                                instruction,
                                op,
                                references[index].as_deref(),
                                &mut widths,
                                &mut out,
                            );
                        }
                        // The model's claim is authoritative: it overrides
                        // whatever the preceding instructions carried.
                        KOpKind::ModeAssumption(state) => {
                            widths.claim(*state, op, &unit, &mut out);
                        }
                        KOpKind::SkipData => out.push_str(&annotated(
                            &format!(
                                "        jmp     {}",
                                references[index].as_deref().unwrap_or(":+")
                            ),
                            "k816: step over the inline data below",
                        )),
                        _ => emit_unit_op(&unit, op, &mut out),
                    }
                }
            }
        }
    }
    out
}

/// The per-unit context every emitted operation needs: the source it came
/// from, the file name the debug directives carry, and the ca65 spelling
/// chosen for each symbol.
struct Unit<'a> {
    map: &'a SourceMap,
    file_name: &'a str,
    names: &'a Names<'a>,
}

/// Emits a non-instruction operation: a label or one of the directives.
///
/// The *decision* to produce these — segment selection, derived linkage,
/// data blocks, alignment — belongs to tasks C7 and C8; this is only their
/// ca65 spelling, so both tasks extend the callers rather than the printer.
fn emit_unit_op(unit: &Unit<'_>, op: &KOp, out: &mut String) {
    let (map, file_name, names) = (unit.map, unit.file_name, unit.names);
    match &op.kind {
        KOpKind::Instruction(_) => unreachable!("instructions are emitted with their width state"),
        // A mode assumption is a claim about the assembler's tracked widths,
        // so it is emitted by the width state inside a function and means
        // nothing at unit level. Sub-plan D task D4 gives it its directives.
        KOpKind::ModeAssumption(_) => {}
        KOpKind::Label(symbol) => out.push_str(&annotated(
            &format!("{}:", names.of(*symbol)),
            &match op.origin {
                // The whole line, because a function's label sits on its
                // declaration and the name alone would not show the widths.
                Origin::User | Origin::HlaExpansion => line_comment(map, op.span),
                Origin::CompilerGenerated => "k816: generated".to_owned(),
            },
        )),
        KOpKind::Export(symbol) => out.push_str(&annotated(
            &format!(".export {}", names.of(*symbol)),
            &annotation(map, op),
        )),
        KOpKind::Import(symbol) => out.push_str(&annotated(
            &format!(".import {}", names.of(*symbol)),
            &annotation(map, op),
        )),
        KOpKind::Segment(name) => out.push_str(&annotated(
            &format!("        .segment \"{name}\""),
            &annotation(map, op),
        )),
        // Emitted with the function's ops, where the reference count is
        // known; at unit level there is no inline data to step over.
        KOpKind::SkipData => out.push_str(&annotated(
            "        jmp     :+",
            "k816: step over the inline data below",
        )),
        // A ca65 unnamed label: it names the landing site without touching
        // the cheap-local scope the function's own labels live in.
        KOpKind::SkipTarget => out.push_str(&annotated(":", "k816: past the inline data")),
        // The same, for the targets branch lowering generates. What it is
        // *for* rides on the branch that names it, so the annotation here
        // says only that something lands.
        KOpKind::Anchor(_) => out.push_str(&annotated(":", "k816: a branch lands here")),
        // ld65 evaluates this after placement, which is the only moment
        // the answer exists.
        KOpKind::NocrossGuard { length } => out.push_str(&annotated(
            &format!(
                "        .assert  .hibyte(* - 1) = .hibyte(* - {length}), error, \
                 \"nocross run crosses a page boundary\""
            ),
            &annotation(map, op),
        )),
        KOpKind::Align(expr) => out.push_str(&annotated(
            &format!("        .align   {}", format_expr(expr, names)),
            &annotation(map, op),
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
            out.push_str(&annotated(&text, &annotation(map, op)));
        }
    }
}

/// The trailing comment for one operation.
///
/// A user-written construct quotes its source; a synthesized one says what
/// K816 made it for. Every emitted operation gets one — instruction, label,
/// directive, and data alike — because an unannotated line in the output is
/// a line whose provenance a reader has to guess at (`plans/`
/// `02-native-translator.md`, phase 6 success criteria).
fn annotation(map: &SourceMap, op: &KOp) -> String {
    match op.origin {
        Origin::User | Origin::HlaExpansion => source_comment(map, op.span),
        Origin::CompilerGenerated => format!("k816: {}", generated_directive_note(&op.kind)),
    }
}

/// A short explanation for a synthesized directive.
fn generated_directive_note(kind: &KOpKind) -> &'static str {
    match kind {
        KOpKind::Segment(_) => "`address` placement needs a segment of its own",
        KOpKind::NocrossGuard { .. } => "ld65 checks the page after placement",
        KOpKind::Export(_) => "defined here, visible to other units",
        KOpKind::Import(_) => "defined by another unit in the workspace",
        KOpKind::Label(_) | KOpKind::Align(_) | KOpKind::Data(_) => "generated",
        KOpKind::SkipData => "step over the inline data below",
        KOpKind::SkipTarget => "past the inline data",
        KOpKind::Anchor(_) => "a branch lands here",
        KOpKind::Instruction(_) | KOpKind::ModeAssumption(_) => "generated",
    }
}

/// One of the two width axes, for the code that treats them alike.
#[derive(Clone, Copy)]
enum Axis {
    A,
    I,
}

/// Tracked assembler width state, kept in sync with `.a8`-family directives
/// so ca65 sizes width-dependent immediates exactly as the width model does.
#[derive(Default)]
struct WidthState {
    a: Option<RegisterWidth>,
    i: Option<RegisterWidth>,
}

impl WidthState {
    /// Applies a width claim from the model, which overrides whatever the
    /// preceding instructions carried.
    ///
    /// An unknown axis emits *no* directive, because ca65 has no way to
    /// un-assume one. That silence is sound only because K816 refuses
    /// width-dependent immediates while the state is unknown, so the two
    /// have to be tested together: a change that removes the refusal must
    /// fail on this side too.
    fn claim(&mut self, state: ModeState, op: &KOp, unit: &Unit<'_>, out: &mut String) {
        let why = reason_text(op, unit);
        let why = why.as_str();
        let before = out.len();
        match state.a {
            AxisState::Known(width) => self.set_a(width, why, out),
            AxisState::Unknown => self.forget(Axis::A, state.e, out),
            // The model emits no claim in an unreachable block, so a `Bottom`
            // axis here would be a compiler bug rather than a silence to
            // honour; either way there is nothing to say about it.
            AxisState::Bottom => {}
        }
        match state.i {
            AxisState::Known(width) => self.set_i(width, why, out),
            AxisState::Unknown => self.forget(Axis::I, state.e, out),
            AxisState::Bottom => {}
        }
        // A claim that changed no directive still has something to say when
        // it exists *because* the model could not see a callee: the widths
        // are unchanged because the language says such a callee must leave
        // them alone, not because K816 worked out that it does.
        if out.len() == before && op.reason == Some(Reason::AssumedExit) {
            out.push_str(&annotated("", &format!("k816: {why}")));
        }
    }

    /// Drops a tracked width and says why, without a directive.
    fn forget(&mut self, axis: Axis, emulation: EmulationState, out: &mut String) {
        let tracked = match axis {
            Axis::A => &mut self.a,
            Axis::I => &mut self.i,
        };
        if tracked.is_none() {
            return;
        }
        *tracked = None;
        let name = match axis {
            Axis::A => "accumulator",
            Axis::I => "index register",
        };
        let cause = match emulation {
            EmulationState::Native => "",
            EmulationState::Unknown => ", because emulation mode is unknown here",
        };
        out.push_str(&annotated(
            "",
            &format!("k816: {name} width is unknown from here{cause}; ca65 has no directive to un-assume one"),
        ));
    }

    fn set_a(&mut self, width: RegisterWidth, why: &str, out: &mut String) {
        if self.a != Some(width) {
            self.a = Some(width);
            let directive = match width {
                RegisterWidth::W8 => "        .a8",
                RegisterWidth::W16 => "        .a16",
            };
            out.push_str(&annotated(directive, &format!("k816: {why}")));
        }
    }

    fn set_i(&mut self, width: RegisterWidth, why: &str, out: &mut String) {
        if self.i != Some(width) {
            self.i = Some(width);
            let directive = match width {
                RegisterWidth::W8 => "        .i8",
                RegisterWidth::W16 => "        .i16",
            };
            out.push_str(&annotated(directive, &format!("k816: {why}")));
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
                    self.set_a(a, "the width the instruction above established", out);
                }
                if let Some(i) = i {
                    self.set_i(i, "the width the instruction above established", out);
                }
            }
            // The instruction destroyed both tracked widths. ca65 has no
            // directive to un-assume one, so the drop is stated in a comment
            // and the next claim is what brings a width back.
            WidthEffect::Unknown => {
                self.forget(Axis::A, EmulationState::Native, out);
                self.forget(Axis::I, EmulationState::Native, out);
            }
        }
    }
}

/// The ca65 unnamed-label reference each operation needs, by position.
///
/// ca65 spells a reference to an unnamed label by *counting* the ones between
/// it and the target — `:+`, `:++`, `:-` — so the text depends on where the
/// referring operation sits, not only on which target it names. Computing it
/// once per function keeps the counting in one place and out of the printer.
///
/// Anchors and skip targets are counted together, because ca65 counts every
/// unnamed label it has seen and does not know which pass emitted one.
fn unnamed_references(ops: &[KOp]) -> Vec<Option<String>> {
    let sites: Vec<usize> = ops
        .iter()
        .enumerate()
        .filter(|(_, op)| matches!(op.kind, KOpKind::Anchor(_) | KOpKind::SkipTarget))
        .map(|(index, _)| index)
        .collect();
    let reference = |from: usize, to: usize| -> String {
        if to > from {
            let count = sites
                .iter()
                .filter(|&&site| site > from && site <= to)
                .count();
            format!(":{}", "+".repeat(count.max(1)))
        } else {
            let count = sites
                .iter()
                .filter(|&&site| site >= to && site < from)
                .count();
            format!(":{}", "-".repeat(count.max(1)))
        }
    };
    ops.iter()
        .enumerate()
        .map(|(index, op)| match &op.kind {
            KOpKind::Instruction(Instruction {
                operand: Some(Operand::Anchor(anchor)),
                ..
            }) => {
                let target = ops
                    .iter()
                    .position(|op| op.kind == KOpKind::Anchor(*anchor))?;
                Some(reference(index, target))
            }
            // The jump around inline data names the skip target below it.
            KOpKind::SkipData => {
                let target = sites.iter().find(|&&site| site > index)?;
                Some(reference(index, *target))
            }
            _ => None,
        })
        .collect()
}

fn emit_instruction(
    unit: &Unit<'_>,
    instruction: &Instruction,
    op: &KOp,
    anchor: Option<&str>,
    widths: &mut WidthState,
    out: &mut String,
) {
    let (map, file_name, names) = (unit.map, unit.file_name, unit.names);
    // Debug annotation: ld65 records this op as owned by the K816 source.
    let line = map.line_col(op.span.file, op.span.start).line + 1;
    out.push_str(&format!("        .dbg    line, \"{file_name}\", {line}\n"));

    let operand = instruction
        .operand
        .as_ref()
        .map(|operand| format_operand(instruction, operand, anchor, widths, names));
    let mut text = format!("        {:<7}", instruction.mnemonic.ca65());
    if let Some(operand) = &operand {
        text.push_str(&format!(" {operand}"));
    }

    let comment = match op.origin {
        Origin::User | Origin::HlaExpansion => source_comment(map, op.span),
        Origin::CompilerGenerated => format!("k816: {}", reason_text(op, unit)),
    };
    out.push_str(&annotated(text.trim_end(), &comment));

    // Keep the assembler's width tracking in sync after mode changes.
    widths.apply(instruction.effects().width, out);
}

fn format_operand(
    instruction: &Instruction,
    operand: &Operand,
    anchor: Option<&str>,
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
        // The symbol is emitted by name — ca65 and ld65 own resolution —
        // but the *encoding* is K816's, so it is stated rather than left for
        // ca65 to infer from its own symbol knowledge. Without the prefix a
        // `far`-forced reference to a page-zero equate would silently
        // assemble as a one-byte instruction (sub-plan C, decision 3).
        Operand::Address {
            encoding,
            expr,
            index,
        } => {
            let text = format_operand_expr(expr, names);
            let prefix = if instruction
                .spec()
                .is_some_and(|spec| spec.operand.form.takes_address_size_prefix())
            {
                encoding.ca65_prefix()
            } else {
                ""
            };
            format!("{prefix}{text}{}", format_index(*index))
        }
        Operand::Label(expr) => format_operand_expr(expr, names),
        // The reference text was counted with the function's other unnamed
        // labels; an anchor with no reference is a lowering bug, and `:+`
        // keeps the output assemblable while the internal error reports it.
        Operand::Anchor(_) => anchor.unwrap_or(":+").to_owned(),
        Operand::StackRelative(expr) => format!("{},s", format_operand_expr(expr, names)),
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

/// A ca65 expression in *operand* position, which is the one place the
/// outermost parentheses have to go.
///
/// `lda a:(spot + 2)` is not the absolute load it looks like: ca65 reads the
/// parenthesis as an indirection and rejects the line. Dropping only the
/// outermost pair costs nothing — an operand expression is terminated by the
/// end of the line or by a `,x` / `,y` / `,s` trailer, and no ca65 operator
/// binds looser than either — while every nested level stays parenthesized,
/// so the precedence guarantee is unchanged.
fn format_operand_expr(expr: &KExpr, names: &Names<'_>) -> String {
    let text = format_expr(expr, names);
    match expr {
        KExpr::Binary { .. } => text
            .strip_prefix('(')
            .and_then(|text| text.strip_suffix(')'))
            .map_or(text.clone(), str::to_owned),
        // A unary form is either a ca65 function call, which needs its own
        // parentheses, or `(0 - x)` / `(~ x)`, whose parentheses are the
        // same outermost pair.
        KExpr::Unary { op, .. } if op.ca65_function().is_none() => text
            .strip_prefix('(')
            .and_then(|text| text.strip_suffix(')'))
            .map_or(text.clone(), str::to_owned),
        _ => text,
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
///
/// A construct may span several source lines — a data block's `nocross`
/// entry reaches into the next one — and a comment that contains a newline
/// would split the instruction it annotates in two. The quoted text is
/// therefore collapsed onto one line.
fn source_comment(map: &SourceMap, span: Span) -> String {
    let source = map.file(span.file);
    let line = source.line_col(span.start).line;
    format!(
        "{}:{}: {}",
        source.name(),
        line + 1,
        one_line(source.span_text(span))
    )
}

/// `text` with every run of whitespace collapsed to a single space.
fn one_line(text: &str) -> String {
    text.split_whitespace().collect::<Vec<_>>().join(" ")
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
        one_line(source.line_text(line))
    )
}

/// A short explanation for compiler-generated operations.
///
/// Only a handful of mnemonics are ever synthesized, and this is comment
/// text rather than semantics — the one place a wildcard over [`Mnemonic`]
/// costs nothing, because a missing arm produces a vaguer comment and not a
/// wrong instruction.
fn reason_text(op: &KOp, unit: &Unit<'_>) -> String {
    match op.reason {
        Some(reason) => reason.describe(|symbol| unit.names.of(symbol)),
        // An operation with no recorded reason says only that the compiler
        // made it, which is what every pre-width generated operation said.
        None => generated_directive_note(&op.kind).to_owned(),
    }
}
