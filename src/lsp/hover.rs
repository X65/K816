/*****************************************************************************/
/*                                                                           */
/*                                  hover.rs                                 */
/*                                                                           */
/*            What the model already knows, rendered as Markdown             */
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

//! Hover: the facts the model already holds, rendered once, here.
//!
//! Every number, name and span comes from [`crate::Analysis`]; nothing is
//! re-derived from source text and nothing is guessed. A position the model
//! holds no fact about returns `None` rather than a sentence that sounds like
//! one — an editor that shows a plausible wrong width is worse than one that
//! shows nothing (`plans/07-lsp-tooling.md` decision 7).
//!
//! Four kinds of position answer, in this order: a name the symbol table
//! knows, a `.field` step, a native mnemonic, and a statement inside a
//! function body. A hover on `main` says which machine the *build* stated it
//! is entered on, because that is the fact an annotation there is checked
//! against and it is not in the source at all.

use std::fmt::Write as _;

use crate::ast::{ItemKind, StatementKind, StmtId};
use crate::isa::RegisterWidth;
use crate::op::{
    AxisExit, AxisState, EmulationExit, EmulationState, ExitContract, ModeState, Operand,
};
use crate::resolve::Resolver;
use crate::semantic::{CallFlow, FlowInput, FlowOutput, FunctionInfo};
use crate::source::{FileId, Span};
use crate::symbol::{AddressSize, LayoutId, SymbolId, SymbolKind};
use crate::value::KExpr;
use crate::width::{Axis, Bridge, BridgeAnchor};
use crate::{Analysis, AnalyzedUnit};

use super::context::{Context, at};

/// What an editor shows for one position.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Hover {
    /// Markdown, produced here and nowhere else.
    pub markdown: String,
    /// The construct the hover is about, which the editor highlights.
    pub span: Span,
}

/// What the model holds at `offset`, as Markdown.
pub fn hover(analysis: &Analysis, file: FileId, offset: u32) -> Option<Hover> {
    let context = at(analysis, file, offset)?;
    // Whitespace between tokens is not a construct, so it is not a question.
    if !context.on_token() {
        return None;
    }
    symbol_hover(analysis, &context)
        .or_else(|| field_hover(&context))
        .or_else(|| instruction_hover(&context))
        .or_else(|| statement_hover(&context))
}

// ---------------------------------------------------------------------------
// Symbols
// ---------------------------------------------------------------------------

fn symbol_hover(analysis: &Analysis, context: &Context<'_>) -> Option<Hover> {
    let id = context.symbol()?;
    let unit = context.unit;
    let symbol = unit.model.symbols.symbol(id);
    let span = context.name_span()?;

    let mut out = format!("`{}` — {}\n", symbol.name, symbol.kind.describe());
    if symbol.imported {
        // Which of the two it is, because they are different answers: one
        // sends the reader to a body in this program, and the other says
        // there is no body here to go to.
        out.push_str(
            match analysis
                .linkage
                .definition(&symbol.name)
                .map(|found| found.origin)
            {
                Some(crate::linkage::Origin::Declared) => {
                    "\nDeclared `extern`: another object provides it, and ld65 \
                     resolves it. The contract above is what the declaration \
                     promises, and nothing checked it.\n"
                }
                _ => "\nDefined by another unit of this program.\n",
            },
        );
    }

    match symbol.kind {
        SymbolKind::Const { value: Some(value) } => {
            let _ = writeln!(out, "\nValue `${value:04X}` ({value}).");
        }
        SymbolKind::Const { value: None } => {
            out.push_str("\nValue resolved by the linker (`extern const`).\n");
        }
        SymbolKind::Var {
            address,
            size,
            width,
            element,
            repeat,
            layout,
        } => {
            match address {
                Some(address) => {
                    let _ = writeln!(out, "\nAt `${address:04X}`, {}.", address_size(size));
                }
                None => {
                    let _ = writeln!(out, "\nAllocated by ld65, {}.", address_size(size));
                }
            }
            if let Some(width) = width {
                let _ = writeln!(out, "\nDeclared `{}`.", type_width(width));
            }
            let _ = writeln!(out, "\nOne element is {element} byte(s).");
            if let Some(repeat) = repeat {
                let _ = writeln!(out, "\n`* {repeat}` — {repeat} elements.");
            }
            let total = element * repeat.unwrap_or(1);
            if let Some(address) = address
                && total > 1
            {
                let _ = writeln!(
                    out,
                    "\nOccupies `${address:04X}..${:04X}` — {total} byte(s).",
                    address + total
                );
            }
            if let Some(reservation) = unit
                .model
                .reservations
                .values()
                .find(|reservation| reservation.symbol == id)
            {
                let _ = writeln!(
                    out,
                    "\nReserves {} byte(s){}.",
                    reservation.bytes,
                    if reservation.direct_page {
                        " in the direct-page pool"
                    } else {
                        ""
                    }
                );
            }
            if let Some(layout) = layout {
                out.push_str(&layout_fields(unit, layout));
            }
        }
        SymbolKind::AbstractVar { layout } => {
            out.push_str("\nNo storage: it answers `:sizeof` and `:offsetof` and nothing else.\n");
            out.push_str(&layout_fields(unit, layout));
        }
        SymbolKind::Label { size, .. } | SymbolKind::DataLabel { size } => {
            let _ = writeln!(out, "\nAn address ld65 places, {}.", address_size(size));
            if let Some(owner) = symbol.scope {
                let _ = writeln!(out, "\nScoped to `{}`.", unit.model.symbols.name(owner));
            }
        }
        SymbolKind::Alias { immediate } => {
            let _ = writeln!(
                out,
                "\nBound at each use site to the argument written in its position — {}.",
                if immediate {
                    "`#name`, so it stands for a value"
                } else {
                    "a bare name, so it stands for a place"
                }
            );
        }
        SymbolKind::Inline | SymbolKind::Function { .. } => {
            out.push_str(&function_facts(analysis, unit, id, &symbol.kind));
        }
    }

    Some(Hover {
        markdown: out,
        span,
    })
}

/// A sentence fragment as a sentence.
fn capitalize(text: &str) -> String {
    let mut characters = text.chars();
    match characters.next() {
        Some(first) => first.to_uppercase().collect::<String>() + characters.as_str(),
        None => String::new(),
    }
}

fn address_size(size: AddressSize) -> &'static str {
    match size {
        AddressSize::DirectPage => "one operand byte (direct page)",
        AddressSize::Absolute => "two operand bytes (absolute)",
        AddressSize::Long => "three operand bytes (long)",
    }
}

fn type_width(width: crate::symbol::DataWidth) -> &'static str {
    match width {
        crate::symbol::DataWidth::Byte => ":byte",
        crate::symbol::DataWidth::Word => ":word",
        crate::symbol::DataWidth::Far => ":far",
    }
}

fn layout_fields(unit: &AnalyzedUnit, layout: LayoutId) -> String {
    let fields = &unit.model.symbols.layout(layout).fields;
    if fields.is_empty() {
        return String::new();
    }
    let mut out = String::from("\nFields:\n\n");
    for field in fields {
        let repeat = if field.count > 1 {
            format!(" ×{}", field.count)
        } else {
            String::new()
        };
        let _ = writeln!(out, "- `.{}` at `+{}`{repeat}", field.name, field.offset);
    }
    out
}

/// A code section's facts: its convention, what it declares, and what the
/// model inferred about the widths a caller may believe afterwards.
fn function_facts(
    analysis: &Analysis,
    unit: &AnalyzedUnit,
    id: SymbolId,
    kind: &SymbolKind,
) -> String {
    let mut out = String::new();
    let info = unit.model.functions.iter().find(|info| info.symbol == id);
    // Which of the four section keywords was written. The model keeps the
    // program it walked, so this is a fact it holds rather than a second
    // reading of the source.
    if let Some(declaration) = info
        .and_then(|info| unit.program().items.get(info.item_index))
        .and_then(|item| match &item.kind {
            ItemKind::Func(function) => Some(function),
            _ => None,
        })
    {
        let _ = writeln!(out, "\nWritten `{}`.", section_keyword(declaration));
    }
    if kind.is_far_function() {
        out.push_str("\n`far` — called with `JSL`, returns with `RTL`.\n");
    }

    if let Some(entry) = info.and_then(|info| info.entry) {
        // The one fact that is not in the source: what the build says the
        // machine is doing when this section gets control.
        let environment = analysis.linkage.environment();
        let _ = writeln!(
            out,
            "\n**{}.** {} — so the build states what machine it is entered on, and states \
             `{}`. That is what a width annotation here is checked against.",
            capitalize(entry.describe()),
            capitalize(entry.caller()),
            describe_environment(environment.state()),
        );
    }

    if let Some(contract) = declared_contract(unit, id) {
        let _ = writeln!(out, "\nDeclares `{contract}`.");
    }
    if let Some(flow) = unit.model.contracts.get(&id) {
        let _ = writeln!(out, "\nRegister flow `{}`.", describe_flow(unit, flow));
    }
    if let Some(info) = info {
        out.push_str(&exit_facts(info));
    }
    out
}

/// The section keyword the declaration was written with.
fn section_keyword(declaration: &crate::ast::Function) -> String {
    use crate::ast::FunctionKind;
    let keyword = match declaration.kind {
        FunctionKind::Func => "func",
        // `naked` keeps the address and drops the automatic return.
        FunctionKind::Naked => "naked",
        // `inline` has no address at all: it is expanded, never called.
        FunctionKind::Inline => "inline",
        // The legacy bare `main { … }`, with no section keyword.
        FunctionKind::Main => "main",
    };
    if declaration.is_far {
        format!("far {keyword}")
    } else {
        keyword.to_owned()
    }
}

/// The `@a8 @i16` a section's declaration wrote, as written.
fn declared_contract(unit: &AnalyzedUnit, id: SymbolId) -> Option<String> {
    let analysis = unit.model.widths.get(&id)?;
    let mut parts = Vec::new();
    if let AxisState::Known(width) = analysis.entry.a {
        parts.push(Axis::Accumulator.annotation(width));
    }
    if let AxisState::Known(width) = analysis.entry.i {
        parts.push(Axis::Index.annotation(width));
    }
    (!parts.is_empty()).then(|| parts.join(" "))
}

fn exit_facts(info: &FunctionInfo) -> String {
    let mut out = format!("\nOn return: {}.\n", describe_exit(info.exit));
    if info.exit_is_assumed {
        out.push_str(
            "\n**Assumed, not inferred.** Nothing here classified this section's exit, so \
             callers fall back to the language's rule that a callee leaves the tracked \
             widths alone.\n",
        );
    }
    out
}

fn describe_exit(exit: ExitContract) -> String {
    let axis = |axis: Axis, exit: AxisExit| {
        let (name, verb) = axis_name(axis);
        match exit {
            AxisExit::Fixed(width) => format!("{name} {verb} `{}`", axis.annotation(width)),
            AxisExit::Preserve => format!("{name} {verb} unchanged"),
            AxisExit::Unknown => format!("{name} {verb} not classified"),
        }
    };
    let emulation = match exit.e {
        EmulationExit::Preserve => "the emulation flag is unchanged",
        EmulationExit::Fixed(EmulationState::Native) => "the machine is native",
        EmulationExit::Fixed(EmulationState::Emulation) => "the machine is in emulation mode",
        EmulationExit::Fixed(EmulationState::Unknown) | EmulationExit::Unknown => {
            "the emulation flag is not classified"
        }
        EmulationExit::Assumed => "the emulation flag is assumed unchanged",
    };
    format!(
        "{}, {}, and {emulation}",
        axis(Axis::Accumulator, exit.a),
        axis(Axis::Index, exit.i)
    )
}

/// How to name an axis, and the verb that agrees with it.
fn axis_name(axis: Axis) -> (&'static str, &'static str) {
    match axis {
        Axis::Accumulator => ("the accumulator", "is"),
        Axis::Index => ("the index registers", "are"),
    }
}

fn describe_flow(unit: &AnalyzedUnit, flow: &CallFlow) -> String {
    let inputs: Vec<String> = flow
        .inputs
        .iter()
        .map(|input| match input {
            FlowInput::Register(register) => register.k65().to_owned(),
            FlowInput::Alias { symbol, immediate } => {
                let name = unit.model.symbols.name(*symbol);
                if *immediate {
                    format!("#{name}")
                } else {
                    name.to_owned()
                }
            }
        })
        .collect();
    let outputs: Vec<String> = flow
        .outputs
        .iter()
        .map(|output| match output {
            FlowOutput::Register(register) => register.k65().to_owned(),
            FlowOutput::Flag(flag) => flag_letter(*flag).to_owned(),
        })
        .collect();
    if outputs.is_empty() {
        format!("({})", inputs.join(", "))
    } else {
        format!("({}) -> {}", inputs.join(", "), outputs.join(", "))
    }
}

fn flag_letter(flag: crate::ast::TestableFlag) -> &'static str {
    use crate::ast::TestableFlag;
    match flag {
        TestableFlag::Carry => "c",
        TestableFlag::Zero => "z",
        TestableFlag::Negative => "n",
        TestableFlag::Overflow => "v",
    }
}

// ---------------------------------------------------------------------------
// Fields
// ---------------------------------------------------------------------------

fn field_hover(context: &Context<'_>) -> Option<Hover> {
    let (layout, index) = context.field?;
    let field = context
        .unit
        .model
        .symbols
        .layout(layout)
        .fields
        .get(index)?;
    let span = context.name_span()?;
    let mut out = format!("`.{}` — a field\n", field.name);
    let _ = writeln!(out, "\nAt offset `+{}` in its layout.", field.offset);
    if field.count > 1 {
        let _ = writeln!(out, "\n`[{}]` — {} elements.", field.count, field.count);
    }
    Some(Hover {
        markdown: out,
        span,
    })
}

// ---------------------------------------------------------------------------
// Native instructions
// ---------------------------------------------------------------------------

/// The native instruction whose mnemonic the cursor sits on.
///
/// Classified by the compiler's own classifier, against the widths the model
/// tracked at this statement — because `lda #1` is two bytes under `@a8` and
/// three under `@a16`, and a hover that answered from the mnemonic alone
/// would be wrong exactly where the language is interesting.
fn instruction_hover(context: &Context<'_>) -> Option<Hover> {
    if context.node_kind() != "mnemonic" {
        return None;
    }
    let statement = context.statement?;
    let native = find_native(context, statement)?;
    let state = width_state(context, statement).map(|(before, _)| before);
    let mut resolver = Resolver::new(&context.unit.model.symbols, &context.unit.model.eval)
        .in_function(context.function)
        .with_widths(state.unwrap_or_else(ModeState::unknown));
    let classified = resolver.classify(native)?;
    let spec = classified.spec;

    let mut out = format!(
        "`{}` — addressing mode `{}`\n",
        native.mnemonic.text,
        spec.mode.symbol()
    );
    // At the widths the model tracked here, not the upper bound: `lda #1` is
    // two bytes under `@a8` and three under `@a16`, and the whole point of
    // the width model is that the compiler knows which.
    let known = state.unwrap_or_else(ModeState::unknown);
    let width = |axis: AxisState| match axis {
        AxisState::Known(width) => Some(width),
        _ => None,
    };
    let length = spec.encoded_length_at(width(known.a), width(known.i));
    let _ = writeln!(
        out,
        "\nEncodes to {} byte(s){}.",
        length * classified.repeat,
        if classified.repeat > 1 {
            format!(" ({length} × {})", classified.repeat)
        } else {
            String::new()
        }
    );
    if let Some(class) = spec.immediate_width() {
        let _ = writeln!(out, "\nOperand: {}.", immediate_class(class));
    }

    // The operand, where it folded to a literal. `SEP`/`REP` derive their
    // effects *from the mask*: with `None` the effect table has to answer for
    // every mask at once, so `sep #$20` reports writing nothing definite when
    // what it writes is `M`. The classifier already folded it, so throwing it
    // away here made the hover say less than the model knows.
    let immediate = match &classified.operand {
        Some(Operand::Immediate(KExpr::Number(value))) => Some(value.get()),
        _ => None,
    };
    let effects = crate::effects::instruction_effects(spec.mnemonic, spec.mode, immediate);
    let _ = writeln!(
        out,
        "\nReads `{}`; writes `{}`.",
        effects.reads, effects.writes
    );

    if let Some(state) = state {
        let _ = writeln!(out, "\nSized against `{}`.", describe_state(state));
    }
    Some(Hover {
        markdown: out,
        span: native.mnemonic.span,
    })
}

fn immediate_class(class: crate::isa::ImmediateWidthClass) -> &'static str {
    use crate::isa::ImmediateWidthClass;
    match class {
        ImmediateWidthClass::Fixed8 => "always one byte",
        ImmediateWidthClass::Fixed16 => "always two bytes",
        ImmediateWidthClass::MDependent => "one or two bytes, as the `M` flag selects",
        ImmediateWidthClass::XDependent => "one or two bytes, as the `X` flag selects",
    }
}

/// The `NativeInstruction` node carrying `statement`.
fn find_native<'a>(
    context: &Context<'a>,
    statement: StmtId,
) -> Option<&'a crate::ast::NativeInstruction> {
    match &context.statement_node(statement)?.kind {
        StatementKind::Native(native) => Some(native),
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Statements
// ---------------------------------------------------------------------------

/// What the width model believed on either side of a statement.
fn statement_hover(context: &Context<'_>) -> Option<Hover> {
    let statement = context.statement?;
    let (before, after) = width_state(context, statement)?;
    let span = statement_span(context, statement)?;

    let mut out = format!("Widths here: `{}`\n", describe_state(before));
    if after != before {
        let _ = writeln!(out, "\nAfter this statement: `{}`", describe_state(after));
    }
    for bridge in bridges(context, statement) {
        let _ = writeln!(
            out,
            "\nThe model inserts a `{}` {} this statement.",
            if bridge.width == RegisterWidth::W8 {
                "SEP"
            } else {
                "REP"
            },
            match bridge.anchor {
                BridgeAnchor::Before => "before",
                _ => "at the label of",
            }
        );
    }
    Some(Hover {
        markdown: out,
        span,
    })
}

fn width_state(context: &Context<'_>, statement: StmtId) -> Option<(ModeState, ModeState)> {
    let analysis = analysis_for(context)?;
    Some((analysis.before(statement)?, analysis.after(statement)?))
}

fn bridges<'a>(context: &Context<'a>, statement: StmtId) -> &'a [Bridge] {
    analysis_for(context).map_or(&[], |analysis| analysis.bridges(statement))
}

/// The width analysis covering the position: the enclosing function's, or the
/// one belonging to whichever `code { }` subblock this statement sits in.
fn analysis_for<'a>(context: &Context<'a>) -> Option<&'a crate::width::WidthAnalysis> {
    if let Some(function) = context.function {
        return context.unit.model.widths.get(&function);
    }
    // A `code { }` subblock of a top-level data block has no symbol; its
    // analysis is keyed by position, and only one of them can hold a given
    // statement.
    let statement = context.statement?;
    context
        .unit
        .model
        .data_code
        .values()
        .find(|analysis| analysis.before(statement).is_some())
}

fn statement_span(context: &Context<'_>, statement: StmtId) -> Option<Span> {
    context
        .statement_node(statement)
        .map(|statement| statement.span)
}

/// A width state in the spelling the language uses.
pub fn describe_state(state: ModeState) -> String {
    let axis = |axis: Axis, state: AxisState| match state {
        AxisState::Known(width) => axis.annotation(width).to_owned(),
        AxisState::Unknown => format!(
            "{} unknown",
            axis.annotation(RegisterWidth::W8).trim_end_matches('8')
        ),
        AxisState::Bottom => format!(
            "{} unreached",
            axis.annotation(RegisterWidth::W8).trim_end_matches('8')
        ),
    };
    let mode = match state.e {
        EmulationState::Native => "",
        EmulationState::Emulation => ", emulation mode",
        EmulationState::Unknown => ", emulation flag unknown",
    };
    format!(
        "{} {}{mode}",
        axis(Axis::Accumulator, state.a),
        axis(Axis::Index, state.i)
    )
}

/// The environment a build states, in the `--entry` spelling.
fn describe_environment(state: ModeState) -> String {
    let mode = match state.e {
        EmulationState::Emulation => "emulation",
        _ => "native",
    };
    let width = |state: AxisState, wide: &'static str, narrow: &'static str| match state {
        AxisState::Known(RegisterWidth::W16) => wide,
        _ => narrow,
    };
    format!(
        "{mode},{},{}",
        width(state.a, "a16", "a8"),
        width(state.i, "i16", "i8")
    )
}
