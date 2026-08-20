/*****************************************************************************/
/*                                                                           */
/*                                resolve.rs                                 */
/*                                                                           */
/*        Name resolution, expression folding, operand classification        */
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

//! The shared read-only layer between analysis and lowering.
//!
//! Resolving a name, folding an expression, and choosing an instruction's
//! addressing mode are the same questions whether the caller is producing
//! diagnostics or producing [`KOp`](crate::op::KOp)s, and the two must not
//! answer them differently. Both go through a [`Resolver`]: analysis reports
//! the diagnostics it collects, and lowering — which runs only after
//! error-free analysis — treats any as an internal error.
//!
//! # Classification has two steps, not one
//!
//! K65 does not spell direct, absolute, and long differently: `lda addr` is
//! one source shape covering three hardware modes
//! (`doc/syntax-reference.md`, "Address-encoding (prefixes)"). So the
//! mnemonic and the AST shape narrow the candidates by
//! [`OperandForm`](crate::isa::OperandForm), and the *encoding* is chosen
//! after that — from an operand prefix when one is written, and otherwise
//! from what K816 actually knows about the address.
//!
//! Auto-shrink is the correctness cliff here. Picking a direct-page encoding
//! for an address K816 only *thinks* is in page zero changes the
//! instruction's length and corrupts everything after it, so it applies to
//! compile-time-known addresses only — numeric literals and pinned variables
//! — and never to a symbol ld65 places.

use std::collections::BTreeMap;

use crate::ast::{
    AddressEncoding as AstEncoding, AddressOperand, BlockMoveOperand, DataBlock, DataEntry,
    DataEntryKind, DataValue, Expr, ExprKind, FieldAccess, HlaOperand, HlaOperandKind,
    IndexRegisterKind, IndirectOperand, LongIndirectOperand, MetadataQuery, MetadataQueryKind,
    Name, NativeInstruction, NativeOperand, Register, StorageClassKind, SubscriptExpr,
};
use crate::compat;
use crate::diagnostic::{Applicability, Diagnostic, DiagnosticCode, Fix, Label, TextEdit};
use crate::effects::{RegisterSet, instruction_effects};
use crate::isa::{
    AddressEncoding, AddressingMode, InstructionSpec, Mnemonic, OperandForm, OperandWidth,
    RegisterWidth, spec_for_form,
};
use crate::layout;
use crate::op::{
    AxisState, DataOp, EmulationState, IndexRegister, Instruction, KOp, KOpKind, ModeState,
    Operand, Origin, SegmentName,
};
use crate::source::{Provenance, Span};
use crate::symbol::{
    AddressSize, DataWidth, FieldShape, LayoutId, SymbolId, SymbolKind, SymbolTable,
};
use crate::value::{BinaryOp, KExpr, UnaryOp, Value};
use crate::width::Axis;

/// One classified instruction: the ISA row it matched, the typed operand
/// built from the source expression, and how many times it is emitted.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Classified {
    pub spec: &'static InstructionSpec,
    pub operand: Option<Operand>,
    /// `<mnemonic> * N` — the repetition count, `1` unless one was written.
    ///
    /// It rides on the classification rather than being read again by whoever
    /// emits, so analysis and lowering cannot fold the count differently
    /// (`doc/syntax-reference.md`, "NOP and Wait Shorthand").
    pub repeat: u32,
}

impl Classified {
    fn new(spec: &'static InstructionSpec, operand: Option<Operand>) -> Self {
        Self {
            spec,
            operand,
            repeat: 1,
        }
    }

    /// The same classification, emitted `repeat` times.
    pub fn repeated(mut self, repeat: u32) -> Self {
        self.repeat = repeat;
        self
    }

    /// The lowered instruction this classification produces.
    pub fn instruction(&self) -> Instruction {
        Instruction::new(self.spec.mnemonic, self.spec.mode, self.operand.clone())
    }
}

/// One operand, borrowed from whichever syntax wrote it.
///
/// An HLA operand and a native operand that denote the same thing must
/// produce the same [`Operand`], chosen by the same code (sub-plan E,
/// decision 1). This is the vocabulary they meet in: `a = SCREEN,x` and
/// `lda SCREEN,x` reach classification as the same variant, so nothing after
/// this point *can* classify them differently. The two syntaxes stay distinct
/// in the AST — `doc/design.md` non-negotiable 7 — and converge here, at the
/// one question they were never allowed to answer separately.
#[derive(Clone, Copy, Debug)]
pub enum OperandSource<'a> {
    /// No operand at all: an implied instruction.
    Nothing,
    Immediate(&'a Expr),
    Register(&'a Register),
    Address(&'a AddressOperand),
    Indirect(&'a IndirectOperand),
    LongIndirect(&'a LongIndirectOperand),
    BlockMove(&'a BlockMoveOperand),
}

impl<'a> OperandSource<'a> {
    /// What a native instruction wrote, `None` for an implied one.
    pub fn native(operand: Option<&'a NativeOperand>) -> Self {
        match operand {
            None => Self::Nothing,
            Some(NativeOperand::Immediate(expr)) => Self::Immediate(expr),
            Some(NativeOperand::Register(register)) => Self::Register(register),
            Some(NativeOperand::Address(address)) => Self::Address(address),
            Some(NativeOperand::Indirect(indirect)) => Self::Indirect(indirect),
            Some(NativeOperand::LongIndirect(long)) => Self::LongIndirect(long),
            Some(NativeOperand::BlockMove(banks)) => Self::BlockMove(banks),
        }
    }

    /// What an HLA statement wrote, or `None` for the error node the parser
    /// has already reported.
    ///
    /// The HLA operand shapes are a subset of the native ones and share their
    /// types, so this is a rewrapping rather than a translation — which is the
    /// point: a translation could lose something, and there is nothing here
    /// that could.
    pub fn hla(operand: &'a HlaOperand) -> Option<Self> {
        match &operand.kind {
            HlaOperandKind::Register(register) => Some(Self::Register(register)),
            HlaOperandKind::Immediate(expr) => Some(Self::Immediate(expr)),
            HlaOperandKind::Memory(address) => Some(Self::Address(address)),
            HlaOperandKind::Indirect(indirect) => Some(Self::Indirect(indirect)),
            HlaOperandKind::Error => None,
        }
    }

    /// The span to blame for an operand problem: the operand itself when
    /// there is one, and `site` — the mnemonic, or the HLA statement — when
    /// there is not.
    fn span(self, site: Span) -> Span {
        match self {
            Self::Nothing => site,
            Self::Immediate(expr) => expr.span,
            Self::Register(register) => register.span,
            Self::Address(address) => address.value.span,
            Self::Indirect(indirect) => indirect.value.span,
            Self::LongIndirect(long) => long.value.span,
            Self::BlockMove(banks) => banks.source.span,
        }
    }
}

/// The width-dependent immediate K816 cannot size, and why.
///
/// The width is the one tracked *here*, so the reason an axis is unknown is
/// worth stating: a `plp` or an unresolved merge destroys a width the
/// enclosing function does declare, and emulation mode neutralizes a widening
/// `rep` outright. Each case gets the annotation that would fix it.
fn unknown_width(
    mnemonic: Mnemonic,
    span: Span,
    class: crate::isa::ImmediateWidthClass,
    widths: ModeState,
) -> Diagnostic {
    let (axis, help, state) = match class {
        crate::isa::ImmediateWidthClass::XDependent => (
            "index register",
            "add @i8 or @i16 to the enclosing function, or set the width here",
            widths.i,
        ),
        _ => (
            "accumulator",
            "add @a8 or @a16 to the enclosing function, or set the width here",
            widths.a,
        ),
    };
    let mut diagnostic = Diagnostic::error(
        DiagnosticCode::UNKNOWN_WIDTH,
        format!(
            "`{}` uses a width-dependent immediate but {axis} width is unknown",
            mnemonic.ca65()
        ),
        Label::new(span, "cannot size this immediate"),
    )
    .with_help(help)
    .with_docs("doc/register-width-aware-syntax.md");
    if state == AxisState::Bottom {
        diagnostic = diagnostic
            .with_note("no path reaches this instruction, so there is no width to size it against");
    } else if widths.e == EmulationState::Unknown {
        diagnostic = diagnostic.with_note(
            "emulation mode is unknown after `xce`, and the M and X flags are always \
             set in emulation mode, so a widening `rep` establishes nothing",
        );
    }
    diagnostic
}

/// The name an access path is ultimately *of*.
///
/// `COMP[2].two`, `COMP:word`, and `COMP` all name `COMP`; arithmetic over
/// one does not, because the result is an address and not the variable. It is
/// what decides an access's encoding — every step of a path is a constant
/// offset from the declaration's own base, so a field of a direct-page
/// variable is still in the direct page.
fn base_name(expr: &Expr) -> Option<&Name> {
    match &expr.kind {
        ExprKind::Symbol(name) => Some(name),
        ExprKind::Field(access) => base_name(&access.base),
        ExprKind::Subscript(subscript) => base_name(&subscript.base),
        ExprKind::TypedView(view) => base_name(&view.base),
        _ => None,
    }
}

/// One resolved access path: where it starts, how far into it the path
/// reached, and what it landed on.
///
/// `TASKS`, `TASKS.sp`, `TASKS[2].str[3]`, and `TASKS.sp:word` are all one
/// walk. Every step contributes a *constant* offset — that is decision 15,
/// and it is what lets the emitted operand be the declaration's own name plus
/// a number, and what lets `:offsetof` answer at all.
#[derive(Clone, Debug)]
struct Access {
    /// The folded root: a symbol most of the time, and whatever expression a
    /// subscript happened to sit on otherwise.
    base: KExpr,
    /// Bytes between the root's address and what the path selected.
    offset: u32,
    /// The root symbol, when the path starts at a declared name.
    root: Option<SymbolId>,
    place: Place,
}

/// What a path has reached, and what one more step would do to it.
///
/// Every field is optional because a path can be rooted in something that
/// declared nothing — a literal, a local label, arithmetic — and a
/// declaration that said nothing must not be read as one that said the
/// default (`postmortem.md` §E.1.17, the same rule as the unwritten `:`).
#[derive(Clone, Copy, Debug, Default)]
struct Place {
    /// The bytes the selection occupies: `:sizeof`'s answer.
    size: Option<u32>,
    /// Its declared data width, and where the declaration said so.
    width: Option<(DataWidth, Span)>,
    /// The layout a further `.field` step would look in.
    layout: Option<LayoutId>,
    /// What a further `[i]` step would stride by, and how far it may go.
    extent: Option<Extent>,
    /// The `.field` the last step selected, when it selected one. It is what
    /// makes `:offsetof` legal, since an offset is only ever a field's.
    field: Option<Span>,
}

/// The stride and the extent of a bracket step.
///
/// The two brackets in the language differ here and nowhere else. A field's
/// `[N]` makes elements — `doc/symbolic-subscripts.md` gives an indexed field
/// element the address `field_base + index * element_width` — so it strides
/// by the element and is checked against `N`. A declaration's `[N]` is a raw
/// span with no element metadata, so it strides one byte and there is nothing
/// to check; only `* count` gives a variable elements of its own.
#[derive(Clone, Copy, Debug)]
struct Extent {
    stride: u32,
    /// The number of elements, when a declaration fixes one.
    count: Option<u32>,
}

/// A layout used where a value is needed.
fn abstract_has_no_storage(name: &str, span: Span, declaration: Span) -> Diagnostic {
    Diagnostic::error(
        DiagnosticCode::ABSTRACT_HAS_NO_STORAGE,
        format!("`{name}` is an `abstract var`, which has no storage"),
        Label::new(span, "a layout, not a memory location"),
    )
    .with_secondary(Label::new(declaration, "declared here"))
    .with_help(format!(
        "query the layout with `{name}:sizeof` or `{name}.field:offsetof`, or declare a \
         `var` with the same fields to give it storage"
    ))
    .with_docs("doc/symbolic-subscripts.md#abstract-layout-only-form")
}

/// A `.field` step on something that has no fields to select from.
///
/// `foreign` is the case that needs its own words: the name *was* declared
/// with a field list, in another unit, and "has no fields" would look like a
/// lie to anyone who can see that declaration. A layout does not cross a unit
/// boundary, so what is true here is that the offsets are not this unit's.
fn selection_without_fields(
    base: &Expr,
    field: &Name,
    described: &str,
    foreign: bool,
) -> Diagnostic {
    let diagnostic = Diagnostic::error(
        DiagnosticCode::SYMBOL_KIND_MISMATCH,
        format!("{described} has no fields"),
        Label::new(field.span, "no field list declares this"),
    )
    .with_secondary(Label::new(base.span, "selected from here"))
    .with_docs("doc/symbolic-subscripts.md#declaration-syntax");
    if foreign {
        return diagnostic
            .with_note(
                "this name is defined by another unit, and a field layout does not cross \
                 a unit boundary: field offsets belong to the unit that computed them",
            )
            .with_help(
                "declare the layout in this unit too — an `abstract var` is not a \
                 definition, so both units may have one — or select the field where the \
                 variable is declared",
            );
    }
    diagnostic.with_note(
        "only a declaration written with a field list — `var NAME[ .lo :byte ]` — has \
         fields to select",
    )
}

/// A `.field` step naming something the layout does not declare.
fn unknown_field(field: &Name, owner: &str, candidates: &[String]) -> Diagnostic {
    let name = field.text.trim_start_matches('.');
    let mut diagnostic = Diagnostic::error(
        DiagnosticCode::UNDECLARED_SYMBOL,
        format!("`{owner}` has no field `{}`", field.text),
        Label::new(field.span, "no such field"),
    );
    if let Some(nearest) = nearest_name(name, candidates) {
        diagnostic = diagnostic
            .with_help(format!("there is a field `.{nearest}`"))
            .with_fix(Fix {
                title: format!("change it to `.{nearest}`"),
                applicability: Applicability::MaybeIncorrect,
                edits: vec![TextEdit::replace(field.span, format!(".{nearest}"))],
            });
    } else if candidates.is_empty() {
        diagnostic = diagnostic.with_note("this layout declares no fields at all");
    } else {
        diagnostic = diagnostic.with_note(format!(
            "`{owner}` declares {}",
            candidates
                .iter()
                .map(|field| format!("`.{field}`"))
                .collect::<Vec<_>>()
                .join(", ")
        ));
    }
    diagnostic.with_docs("doc/symbolic-subscripts.md#access-syntax")
}

/// The nearest of `candidates` to `name`, when one is near enough to suggest.
///
/// Near enough is a third of the name's length, rounded down, and at least
/// one: a suggestion that shares less than that is noise, and offering the
/// wrong field is worse than offering none.
fn nearest_name<'a>(name: &str, candidates: &'a [String]) -> Option<&'a str> {
    let budget = (name.chars().count() / 3).max(1);
    candidates
        .iter()
        .map(|candidate| (edit_distance(name, candidate), candidate))
        .filter(|(distance, _)| *distance <= budget)
        .min_by_key(|(distance, _)| *distance)
        .map(|(_, candidate)| candidate.as_str())
}

/// The Levenshtein distance between two names, over one row of the matrix.
fn edit_distance(left: &str, right: &str) -> usize {
    let right: Vec<char> = right.chars().collect();
    let mut row: Vec<usize> = (0..=right.len()).collect();
    for (i, from) in left.chars().enumerate() {
        let mut previous = row[0];
        row[0] = i + 1;
        for (j, to) in right.iter().enumerate() {
            let substitute = previous + usize::from(from != *to);
            previous = row[j + 1];
            row[j + 1] = substitute.min(row[j] + 1).min(previous + 1);
        }
    }
    row[right.len()]
}

/// A metadata query asked of something that cannot answer it.
fn no_metadata(query: &MetadataQuery, span: Span, what: &str) -> Diagnostic {
    let suffix = match query.query {
        MetadataQueryKind::SizeOf => ":sizeof",
        MetadataQueryKind::OffsetOf => ":offsetof",
    };
    Diagnostic::error(
        DiagnosticCode::SYMBOL_KIND_MISMATCH,
        format!("`{suffix}` needs {what}"),
        Label::new(query.query_span, format!("nothing here answers `{suffix}`")),
    )
    .with_secondary(Label::new(span, "asked of this"))
    .with_note(
        "`:sizeof` is the size of a variable element or a field, and `:offsetof` is a \
         field's distance from the base of its variable",
    )
    .with_docs("doc/syntax-reference.md#metadata-query-suffixes")
}

/// An index outside the extent its declaration gives.
///
/// The two brackets fail differently and the wording follows: a variable's
/// extent comes from its `* count`, and a field's from its `[N]` — where the
/// unwritten one is a single element, and indexing that is not a range error
/// so much as a misreading of what the field is.
fn subscript_out_of_range(subscript: &SubscriptExpr, index: u32, count: u32) -> Diagnostic {
    let owner = match &subscript.base.kind {
        ExprKind::Field(access) => format!("`{}`", access.field.text),
        _ => "this variable".to_owned(),
    };
    if count == 1 {
        return Diagnostic::error(
            DiagnosticCode::SUBSCRIPT_OUT_OF_RANGE,
            format!("{owner} is a single element, and `[{index}]` names another"),
            Label::new(subscript.index.span, "the only index is 0"),
        )
        .with_note(
            "a declaration with no count declares one element, so there is nothing for \
             an index to select",
        )
        .with_help("declare how many there are — `:byte [4]` on a field, `* 4` on a variable")
        .with_docs("doc/syntax-reference.md#repeat-element-accessors");
    }
    Diagnostic::error(
        DiagnosticCode::SUBSCRIPT_OUT_OF_RANGE,
        format!("element {index} is outside the {count} elements of {owner}"),
        Label::new(
            subscript.index.span,
            format!("the last element is {}", count - 1),
        ),
    )
    .with_note(
        "a declared count fixes how many elements there are, and an index past them \
         names storage the declaration did not reserve",
    )
    .with_docs("doc/syntax-reference.md#repeat-element-accessors")
}

/// The register axis a memory datum passes through, or `None` when no
/// register carries it.
///
/// Asked of the *mnemonic*, at `Absolute` — a mode whose only
/// address-forming register is the data bank register, so the
/// general-purpose registers left in the effect are the ones the datum
/// itself moves through. `LDA` writes A, `STX` reads X, `CPY` reads Y, and
/// `JMP` touches none of them because a jump target is not a datum.
///
/// A read-modify-write like `INC` also touches none, which is why it is not
/// checked: the document's rule is about loads and stores, and `INC` moves
/// its datum through no register at all.
fn datum_axis(mnemonic: Mnemonic) -> Option<Axis> {
    let effects = instruction_effects(mnemonic, AddressingMode::Absolute, None);
    let touched = effects.reads.registers.union(effects.writes.registers);
    if touched.intersects(RegisterSet::X.union(RegisterSet::Y)) {
        Some(Axis::Index)
    } else if touched.intersects(RegisterSet::A) {
        Some(Axis::Accumulator)
    } else {
        None
    }
}

/// A `:far` datum in register position: there is no register to put it in.
fn datum_too_wide(mnemonic: Mnemonic, operand: Span, declaration: Span) -> Diagnostic {
    Diagnostic::error(
        DiagnosticCode::DATA_WIDTH_MISMATCH,
        format!(
            "`{}` cannot move a 3-byte datum: no register is that wide",
            mnemonic.ca65()
        ),
        Label::new(operand, "declared `:far`, which is 3 bytes"),
    )
    .with_secondary(Label::new(declaration, "the width is declared here"))
    .with_note(
        "the accumulator and the index registers are 8 or 16 bits wide; 24-bit data is \
         reached a piece at a time, or through a pointer",
    )
    .with_help(
        "take a partial view — `ptr:word`, `(ptr+2):byte` — or use a long-indirect \
         operand such as `lda [ptr]`",
    )
    .with_docs("doc/syntax-reference.md#typed-width-suffixes")
}

/// A datum whose declared width is not the width of the register reading it.
fn datum_width_mismatch(
    mnemonic: Mnemonic,
    operand: Span,
    declaration: Span,
    datum: DataWidth,
    axis: Axis,
    register: RegisterWidth,
) -> Diagnostic {
    let bytes = datum.bytes();
    let register_bytes = match register {
        RegisterWidth::W8 => 1,
        RegisterWidth::W16 => 2,
    };
    let wanted = match datum {
        DataWidth::Byte => RegisterWidth::W8,
        _ => RegisterWidth::W16,
    };
    Diagnostic::error(
        DiagnosticCode::DATA_WIDTH_MISMATCH,
        format!(
            "`{}` moves {register_bytes} byte{} here, and this datum is {bytes}",
            mnemonic.ca65(),
            if register_bytes == 1 { "" } else { "s" },
        ),
        Label::new(
            operand,
            format!(
                "declared `{}`",
                match datum {
                    DataWidth::Byte => ":byte",
                    DataWidth::Word => ":word",
                    DataWidth::Far => ":far",
                }
            ),
        ),
    )
    .with_secondary(Label::new(declaration, "the width is declared here"))
    .with_note(format!(
        "the {} is {register_bytes}-byte wide here, so the access would {}",
        axis.name(),
        if register_bytes < bytes {
            "read only part of the datum"
        } else {
            "run past the end of it"
        }
    ))
    .with_help(format!(
        "set the width — `{}` on the enclosing section, or a width block here — or take \
         a typed view of the operand",
        axis.annotation(wanted)
    ))
    .with_docs("doc/syntax-reference.md#typed-width-suffixes")
}

/// What one resolver run found.
#[derive(Debug, Default)]
pub struct Resolved {
    pub referenced: Vec<SymbolId>,
    pub diagnostics: Vec<Diagnostic>,
}

/// Resolves names, folds expressions, and classifies operands.
pub struct Resolver<'a> {
    symbols: &'a SymbolTable,
    /// The function whose scope `.name` labels resolve in.
    function: Option<SymbolId>,
    /// The widths the model tracks *here*, which size the width-dependent
    /// immediates. A state rather than the enclosing function's contract: a
    /// `rep #$20` two lines above changes the answer, and a `plp` destroys it
    /// even inside a function that declares the axis.
    widths: ModeState,
    diagnostics: Vec<Diagnostic>,
    /// Every symbol this resolver looked up, which is what tells an
    /// `.import` a unit needs from one it merely could have had.
    referenced: Vec<SymbolId>,
    /// What an `inline`'s aliases stand for, while an expansion is lowered.
    /// Empty everywhere else, including at the declaration.
    aliases: BTreeMap<SymbolId, KExpr>,
}

impl<'a> Resolver<'a> {
    pub fn new(symbols: &'a SymbolTable) -> Self {
        Self {
            symbols,
            function: None,
            widths: ModeState::unknown(),
            diagnostics: Vec::new(),
            referenced: Vec::new(),
            aliases: BTreeMap::new(),
        }
    }

    /// Scopes local label resolution to `function`.
    /// Binds an `inline`'s aliases to what a use site passed.
    ///
    /// Set only while lowering an expansion. It is what makes the value known
    /// where it is known: the same classifier that sizes every other operand
    /// sizes an alias-bearing one, against the same widths, so a substituted
    /// immediate that does not fit is reported here rather than by ca65 — and
    /// a substituted *place* takes the encoding its own declaration earns
    /// instead of the conservative one an unbound alias has to assume.
    pub fn with_aliases(mut self, aliases: BTreeMap<SymbolId, KExpr>) -> Self {
        self.aliases = aliases;
        self
    }

    pub fn in_function(mut self, function: Option<SymbolId>) -> Self {
        self.function = function;
        self
    }

    /// Sizes width-dependent immediates with the widths tracked at this
    /// point, which the width model — never the entry contract — supplies.
    pub fn with_widths(mut self, widths: ModeState) -> Self {
        self.widths = widths;
        self
    }

    pub fn take_diagnostics(&mut self) -> Vec<Diagnostic> {
        std::mem::take(&mut self.diagnostics)
    }

    /// Consumes the resolver and hands back what it found: the symbols it
    /// resolved and the diagnostics it collected. Consuming it is what
    /// releases the borrow of the symbol table, so the caller can record
    /// both into the model it owns.
    pub fn finish(self) -> Resolved {
        Resolved {
            referenced: self.referenced,
            diagnostics: self.diagnostics,
        }
    }

    pub fn has_diagnostics(&self) -> bool {
        !self.diagnostics.is_empty()
    }

    pub(crate) fn report(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }

    // ---------------------------------------------------------- expressions

    /// Folds `expr` into a lowered expression.
    ///
    /// Absolute sub-expressions collapse to numbers; anything rooted in a
    /// symbol ld65 places stays a tree, because K816 never resolves an
    /// address the linker owns. Every operator the K65 expression layer has
    /// is preserved symbolically — ca65 stores unresolved expressions in the
    /// object file and ld65 evaluates them after placement, so `label * 2`
    /// is not an error merely because a label is involved.
    ///
    /// Returns `None` when the expression is malformed or names something
    /// undeclared; both are reported here, so callers stay quiet.
    pub fn fold(&mut self, expr: &Expr) -> Option<KExpr> {
        match &expr.kind {
            ExprKind::Number(value) => Some(KExpr::number(*value)),
            ExprKind::Symbol(name) => {
                let id = self.resolve(&name.text, name.span)?;
                self.symbol_value(id, name.span)
            }
            ExprKind::LocalSymbol(name) => {
                let id = self.resolve_local(&name.text, name.span)?;
                Some(KExpr::Symbol(id))
            }
            ExprKind::Unary(unary) => {
                let operand = self.fold(&unary.operand)?;
                Some(KExpr::fold_unary(lowered_unary_op(unary.op), operand))
            }
            ExprKind::Binary(binary) => {
                // Both sides are folded even when the first fails, so one
                // undeclared name does not hide another.
                let left = self.fold(&binary.left);
                let right = self.fold(&binary.right);
                Some(KExpr::fold_binary(
                    lowered_binary_op(binary.op),
                    left?,
                    right?,
                ))
            }
            // A typed view restates how wide the datum is and says nothing
            // about which one it is, so the value it folds to is the base's.
            // The width it declares is read where widths are checked.
            ExprKind::TypedView(view) => self.fold(&view.base),
            // An access path is a base and a constant offset, and folding one
            // is asking for its address. Which is why the walk is shared with
            // `:sizeof` and `:offsetof` but the *address* is not: a layout
            // answers those two and has no address to give.
            ExprKind::Field(_) | ExprKind::Subscript(_) => {
                let access = self.access(expr)?;
                self.address_of(&access, expr.span)
            }
            ExprKind::Metadata(query) => self.metadata(query, expr.span),
            ExprKind::Eval(_) => {
                self.report(compat::unimplemented(
                    expr.span,
                    "compile-time evaluator brackets",
                    compat::Owner::Compatibility,
                ));
                None
            }
            // A syntax diagnostic was already reported for this node.
            ExprKind::Error => None,
        }
    }

    /// The value a plain reference to `id` folds to.
    ///
    /// Shared by the two paths that turn a name into something: an
    /// expression, and the root of an access path. It is the single point
    /// where a name becomes a value, which is why the kinds that have no
    /// value are refused *here* rather than at each position that rejects
    /// them.
    fn symbol_value(&mut self, id: SymbolId, span: Span) -> Option<KExpr> {
        match self.symbols.kind(id) {
            // A `const` has no storage, so it can only ever be its value;
            // everything else keeps its name for ca65/ld65.
            SymbolKind::Const { value } => Some(KExpr::number(value)),
            // An `abstract var` has neither a value nor a place, so there is
            // nothing for a mention of one to fold to.
            SymbolKind::AbstractVar { .. } => {
                let declaration = self.symbols.symbol(id).declaration;
                let name = self.symbols.name(id).to_owned();
                self.report(abstract_has_no_storage(&name, span, declaration));
                None
            }
            // An `inline` is a body, not a place: nothing links to it, so a
            // mention of one outside a use site has nothing to become.
            SymbolKind::Inline => {
                let declaration = self.symbols.symbol(id).declaration;
                let name = self.symbols.name(id).to_owned();
                self.report(
                    Diagnostic::error(
                        DiagnosticCode::SYMBOL_KIND_MISMATCH,
                        format!("`{name}` is an `inline`, which has no address"),
                        Label::new(span, "a body that is expanded, not a place"),
                    )
                    .with_secondary(Label::new(declaration, "declared here"))
                    .with_note(
                        "an `inline` is copied into each use site, so there is no one \
                         address for a reference to name",
                    )
                    .with_help(format!(
                        "write `{name}` on its own to expand it, or make it a `func` to \
                         give it an address"
                    ))
                    .with_docs("doc/syntax-reference.md#inline"),
                );
                None
            }
            // An alias is whatever the use site passed, where a use site is
            // what is being lowered; at the declaration there is nothing
            // bound and the alias stays itself, which is what lets the body
            // be classified once with a shape rather than with a hole.
            SymbolKind::Alias { .. } => {
                Some(self.aliases.get(&id).cloned().unwrap_or(KExpr::Symbol(id)))
            }
            SymbolKind::Var { .. }
            | SymbolKind::Label { .. }
            | SymbolKind::DataLabel { .. }
            | SymbolKind::Function { .. } => Some(KExpr::Symbol(id)),
        }
    }

    /// Walks an access path — `NAME`, `.field` steps, `[i]` steps, and typed
    /// views over any of them — into a base, a constant offset, and a
    /// description of what it landed on.
    ///
    /// One walk for every consumer, which is decision 15's real content:
    /// `TASKS.state`'s address, `TASKS.state:sizeof`, `TASKS.state:offsetof`,
    /// and the width `lda TASKS.state` is checked against are four questions
    /// about one path, and answering them separately is how they come to
    /// disagree.
    fn access(&mut self, expr: &Expr) -> Option<Access> {
        match &expr.kind {
            ExprKind::Symbol(name) => {
                let id = self.resolve(&name.text, name.span)?;
                let place = self.place_of(id);
                // A layout is not refused here: whether having no address is
                // a problem depends on what the path is *for*, and
                // `DEV_HANDLER.init:offsetof` is the case that must not be.
                let base = match self.symbols.kind(id) {
                    SymbolKind::AbstractVar { .. } => KExpr::Symbol(id),
                    _ => self.symbol_value(id, name.span)?,
                };
                Some(Access {
                    base,
                    offset: 0,
                    root: Some(id),
                    place,
                })
            }
            ExprKind::Field(field) => {
                let base = self.access(&field.base)?;
                self.select_field(base, field)
            }
            ExprKind::Subscript(subscript) => {
                let base = self.access(&subscript.base)?;
                self.element(base, subscript)
            }
            // A typed view restates how wide the datum is and says nothing
            // about which one it is, so it replaces the width the path
            // carries and leaves every other answer alone.
            ExprKind::TypedView(view) => {
                let mut access = self.access(&view.base)?;
                access.place.width = Some((layout::data_width(view.width.kind), view.width.span));
                Some(access)
            }
            // Anything else is a root that declared nothing: a literal, a
            // local label, arithmetic. It still has an address to offset
            // from; it just has no extent, no width, and no fields.
            _ => Some(Access {
                base: self.fold(expr)?,
                offset: 0,
                root: None,
                place: Place::default(),
            }),
        }
    }

    /// What a declared name is, as an access path sees it.
    fn place_of(&self, id: SymbolId) -> Place {
        match self.symbols.kind(id) {
            SymbolKind::Var {
                width,
                element,
                repeat,
                layout,
                ..
            } => Place {
                size: Some(element),
                width: width.map(|width| (width, self.symbols.symbol(id).declaration)),
                layout,
                // Only `* count` gives a variable elements. Without one the
                // bracket steps bytes and has nothing to be checked against,
                // which is the document's own wording and covers the raw
                // span too.
                extent: Some(Extent {
                    stride: if repeat.is_some() { element } else { 1 },
                    count: repeat,
                }),
                field: None,
            },
            SymbolKind::AbstractVar { layout } => Place {
                size: Some(self.symbols.layout(layout).size),
                width: None,
                layout: Some(layout),
                // A layout has no storage, so it has no elements either: an
                // allocation count is one of the forms its declaration may
                // not write.
                extent: None,
                field: None,
            },
            _ => Place::default(),
        }
    }

    /// Takes one `.field` step, which adds the field's offset and narrows the
    /// path to what that field holds.
    fn select_field(&mut self, base: Access, access: &FieldAccess) -> Option<Access> {
        let name = access.field.text.trim_start_matches('.');
        let Some(layout) = base.place.layout else {
            let (described, foreign) = match base.root {
                Some(id) => (
                    format!("`{}`", self.symbols.name(id)),
                    self.symbols.symbol(id).imported,
                ),
                None => ("this".to_owned(), false),
            };
            self.report(selection_without_fields(
                &access.base,
                &access.field,
                &described,
                foreign,
            ));
            return None;
        };
        let layouts = self.symbols.layouts();
        let Some(field) = self.symbols.layout(layout).field(name) else {
            let owner = base.root.map_or_else(
                || "this layout".to_owned(),
                |id| self.symbols.name(id).to_owned(),
            );
            let candidates: Vec<String> = self
                .symbols
                .layout(layout)
                .fields
                .iter()
                .map(|field| field.name.clone())
                .collect();
            self.report(unknown_field(&access.field, &owner, &candidates));
            return None;
        };
        let place = Place {
            size: Some(field.size(layouts)),
            width: match field.shape {
                FieldShape::Scalar(width) => Some((width, field.declaration)),
                // A nested list is a struct, and a struct has no register to
                // agree or disagree with.
                FieldShape::Nested(_) => None,
            },
            layout: match field.shape {
                FieldShape::Nested(layout) => Some(layout),
                FieldShape::Scalar(_) => None,
            },
            // A field's own bracket is a count, checked: this is where the
            // two brackets of the language part company.
            extent: Some(Extent {
                stride: field.element_size(layouts),
                count: Some(field.count),
            }),
            field: Some(access.field.span),
        };
        let offset = base.offset.saturating_add(field.offset);
        Some(Access {
            base: base.base,
            offset,
            root: base.root,
            place,
        })
    }

    /// Takes one `[i]` step, which is compile-time address arithmetic and
    /// never anything else.
    ///
    /// The index must fold. A run-time index is the CPU's indexed addressing
    /// modes, which are a different spelling for a different thing.
    fn element(&mut self, base: Access, subscript: &SubscriptExpr) -> Option<Access> {
        let index = self.fold(&subscript.index)?;
        let Some(index) = index.value().map(|value| value.get()) else {
            self.report(
                Diagnostic::error(
                    DiagnosticCode::SUBSCRIPT_NOT_CONSTANT,
                    "a subscript index must be known at compile time",
                    Label::new(subscript.index.span, "this is not a constant"),
                )
                .with_note(
                    "`NAME[i]` is address arithmetic K816 performs, not an index the CPU \
                     applies at run time",
                )
                .with_help(
                    "use an indexed addressing mode — `NAME,x` — or compute the address \
                     into a pointer",
                )
                .with_docs("doc/syntax-reference.md#repeat-element-accessors"),
            );
            return None;
        };
        let extent = base.place.extent.unwrap_or(Extent {
            stride: 1,
            count: None,
        });
        if let Some(count) = extent.count
            && index >= count
        {
            self.report(subscript_out_of_range(subscript, index, count));
            return None;
        }
        let place = Place {
            // One element of what was indexed, so its width and its layout
            // survive the step and its extent does not.
            size: Some(extent.stride),
            extent: None,
            ..base.place
        };
        let offset = base
            .offset
            .saturating_add(index.saturating_mul(extent.stride));
        Some(Access {
            base: base.base,
            offset,
            root: base.root,
            place,
        })
    }

    /// The address an access path denotes.
    ///
    /// This is the one question a layout cannot answer, so it is the one
    /// place `abstract var` is refused. `NAME[0]` and a path whose steps all
    /// landed at zero return the base unchanged: `NAME + 0` assembles to the
    /// same bytes and reads like arithmetic nobody wrote.
    fn address_of(&mut self, access: &Access, span: Span) -> Option<KExpr> {
        if let Some(root) = access.root
            && matches!(self.symbols.kind(root), SymbolKind::AbstractVar { .. })
        {
            let declaration = self.symbols.symbol(root).declaration;
            let name = self.symbols.name(root).to_owned();
            self.report(abstract_has_no_storage(&name, span, declaration));
            return None;
        }
        if access.offset == 0 {
            return Some(access.base.clone());
        }
        Some(KExpr::fold_binary(
            BinaryOp::Add,
            access.base.clone(),
            KExpr::number(access.offset),
        ))
    }

    /// Folds `NAME:sizeof` and `NAME.field:offsetof` to the integers they
    /// are, which is all they ever are (decision 15).
    ///
    /// Deliberately *not* an address: a metadata query walks the same path an
    /// access does and then asks the path about itself, so an `abstract var`
    /// — which has no address and exists for exactly this — answers both.
    fn metadata(&mut self, query: &MetadataQuery, span: Span) -> Option<KExpr> {
        let access = self.access(&query.base)?;
        match query.query {
            MetadataQueryKind::SizeOf => match access.place.size {
                Some(size) => Some(KExpr::number(size)),
                None => {
                    self.report(no_metadata(query, span, "a variable or a field"));
                    None
                }
            },
            // An offset is a field's distance from its variable's base, so a
            // path that selected no field has none — `counter:offsetof` is
            // not zero, it is a question about nothing.
            MetadataQueryKind::OffsetOf => match access.place.field {
                Some(_) => Some(KExpr::number(access.offset)),
                None => {
                    self.report(no_metadata(query, span, "a field"));
                    None
                }
            },
        }
    }

    /// The absolute value of `expr` in a position that requires one.
    ///
    /// `position` names where the expression appeared, in plural form
    /// ("immediate operands"). A link-time expression is rejected here and
    /// only here: elsewhere it is preserved and handed to ld65.
    pub fn constant(&mut self, expr: &Expr, position: &str) -> Option<u32> {
        let folded = self.fold(expr)?;
        match folded.value() {
            Some(value) => Some(value.get()),
            None => {
                let mut symbols = Vec::new();
                folded.symbols(&mut symbols);
                let named = symbols.first().map(|id| self.symbols.name(*id).to_owned());
                let mut diagnostic = Diagnostic::error(
                    DiagnosticCode::NOT_CONSTANT,
                    format!("{position} must be known at compile time"),
                    Label::new(expr.span, "this value is not known until link time"),
                )
                .with_help("use a numeric literal or a `const` here")
                .with_docs("doc/syntax-reference.md#constant-declaration");
                if let Some(name) = named {
                    diagnostic =
                        diagnostic.with_note(format!("`{name}` gets its address from ld65"));
                }
                self.report(diagnostic);
                None
            }
        }
    }

    /// Resolves a plain name, reporting it when nothing declares it.
    ///
    /// The enclosing section's own scope is tried first, because an `inline`
    /// alias is a plain identifier scoped to the section that declares it.
    /// Nothing else in that scope can be reached this way: a local label's
    /// name keeps its leading `.`, so `lookup_local` cannot match an
    /// identifier, and an alias shadows a unit-level name of the same
    /// spelling only inside the one body that declared it.
    pub fn resolve(&mut self, name: &str, span: Span) -> Option<SymbolId> {
        if let Some(alias) = self
            .function
            .and_then(|owner| self.symbols.lookup_local(owner, name))
        {
            return Some(alias);
        }
        match self.symbols.lookup(name) {
            Some(id) => {
                self.referenced.push(id);
                Some(id)
            }
            None => {
                self.report(
                    Diagnostic::error(
                        DiagnosticCode::UNDECLARED_SYMBOL,
                        format!("`{name}` is not declared"),
                        Label::new(span, "used here without a declaration"),
                    )
                    .with_help(format!("declare it first, e.g. `var {name} = $0400`"))
                    .with_docs("doc/syntax-reference.md#variable-declaration"),
                );
                None
            }
        }
    }

    /// Resolves a `.name` label in the enclosing function's scope.
    fn resolve_local(&mut self, name: &str, span: Span) -> Option<SymbolId> {
        let owner = self.function?;
        match self.symbols.lookup_local(owner, name) {
            Some(id) => Some(id),
            None => {
                let function = self.symbols.name(owner).to_owned();
                self.report(
                    Diagnostic::error(
                        DiagnosticCode::UNDECLARED_SYMBOL,
                        format!("`{name}` is not declared in `{function}`"),
                        Label::new(span, "no such local label in this function"),
                    )
                    .with_note("local labels are scoped to their enclosing function")
                    .with_docs("doc/syntax-reference.md#local"),
                );
                None
            }
        }
    }

    // --------------------------------------------------------- instructions

    /// Classifies one native instruction against the ISA table.
    pub fn classify(&mut self, native: &NativeInstruction) -> Option<Classified> {
        if native.mnemonic.provenance == Provenance::Synthetic {
            return None;
        }
        // The grammar produces only real 65816 mnemonics, so an unresolved
        // spelling is a typo the ISA table can name precisely.
        let Some(mnemonic) = Mnemonic::parse(&native.mnemonic.text) else {
            self.report(
                Diagnostic::error(
                    DiagnosticCode::UNKNOWN_MNEMONIC,
                    format!("`{}` is not a 65816 instruction", native.mnemonic.text),
                    Label::new(native.mnemonic.span, "unknown mnemonic"),
                )
                .with_docs("doc/w65c816s.md"),
            );
            return None;
        };
        // `<mnemonic> * N` repeats what follows it N times. The count folds
        // here, once, so that the operation stream cannot depend on which
        // stage did the folding.
        let repeat = match &native.repeat {
            None => 1,
            Some(count) => self.constant(count, "instruction repetition counts")?,
        };
        let classified = self.classify_operand(
            mnemonic,
            OperandSource::native(native.operand.as_ref()),
            native.mnemonic.span,
        )?;
        Some(classified.repeated(repeat))
    }

    /// Classifies an operand the caller has already read into the shared
    /// vocabulary, against the mnemonic that caller's statement selects.
    ///
    /// The split is deliberate: which instruction a statement means is the
    /// HLA statement's to decide, because that is what the spelling says;
    /// what its operand is is not, because a native instruction already asked
    /// the same question (decision 1). `site` is the span to blame when the
    /// operand itself is not what is wrong — the statement's, where a native
    /// instruction would give its mnemonic's.
    pub fn classify_source(
        &mut self,
        mnemonic: Mnemonic,
        source: OperandSource<'_>,
        site: Span,
    ) -> Option<Classified> {
        self.classify_operand(mnemonic, source, site)
    }

    /// The same, for a mnemonic that may legitimately have no form for this
    /// operand and whose caller has a documented fallback. Nothing is
    /// reported: the refusal is the answer, not a diagnostic.
    pub fn try_classify_source(
        &mut self,
        mnemonic: Mnemonic,
        source: OperandSource<'_>,
        site: Span,
    ) -> Option<Classified> {
        let reported = self.diagnostics.len();
        let classified = self.classify_operand(mnemonic, source, site);
        if self.diagnostics.len() > reported {
            self.diagnostics.truncate(reported);
            return None;
        }
        classified
    }

    /// Classifies an instruction written with no operand at all — the
    /// register transfers an HLA chain expands to.
    pub fn classify_implied(&mut self, mnemonic: Mnemonic, site: Span) -> Option<Classified> {
        self.classify_operand(mnemonic, OperandSource::Nothing, site)
    }

    /// How an HLA operand reads in *source* position: the value it denotes
    /// where it denotes one, and the place it names otherwise.
    ///
    /// K65 writes a load's constant without a `#`: `a=0` sets the accumulator
    /// to zero and `a=RND` reads the byte at `RND`
    /// (`doc/syntax-reference.md`, "Code Sections"; `doc/k65/syntax.md`).
    /// Which one a bare operand means is a *statement* question — the same
    /// text after `lda` is an address — so it is answered here, once, and the
    /// classifier is handed the answer rather than the text.
    pub fn hla_source<'b>(&self, operand: &'b HlaOperand) -> Option<OperandSource<'b>> {
        match self.hla_value(operand) {
            Some(value) => Some(OperandSource::Immediate(value)),
            None => OperandSource::hla(operand),
        }
    }

    /// How an HLA operand reads in *target* position, where there is nothing
    /// to decide: a chain stores into a place, and a bare number is the
    /// address of one.
    pub fn hla_place<'b>(&self, operand: &'b HlaOperand) -> Option<OperandSource<'b>> {
        OperandSource::hla(operand)
    }

    /// The value expression an operand denotes, or `None` when it names a
    /// place.
    pub fn hla_value<'b>(&self, operand: &'b HlaOperand) -> Option<&'b Expr> {
        match &operand.kind {
            HlaOperandKind::Immediate(expr) => Some(expr),
            // An index, an encoding prefix, or an offset marker all describe
            // how to *reach* something, so the operand names a place whatever
            // its expression says.
            HlaOperandKind::Memory(address)
                if address.index.is_none()
                    && address.encoding.is_none()
                    && address.offset_marker.is_none()
                    && self.denotes_value(&address.value) =>
            {
                Some(&address.value)
            }
            _ => None,
        }
    }

    /// True when `expr` denotes a value rather than a place: a literal, a
    /// `const`, a metadata query, an address-byte operator, or arithmetic
    /// over those.
    ///
    /// A name K816 cannot resolve counts as a place, so the operand keeps its
    /// address reading and classification reports the undeclared symbol —
    /// rather than this deciding a typo is an immediate.
    fn denotes_value(&self, expr: &Expr) -> bool {
        use crate::ast::UnaryOp;
        match &expr.kind {
            ExprKind::Number(_) => true,
            ExprKind::Symbol(name) => matches!(
                self.symbols
                    .lookup(&name.text)
                    .map(|id| self.symbols.kind(id)),
                Some(SymbolKind::Const { .. })
            ),
            // `:sizeof` and `:offsetof` fold to integers at analysis time
            // and are ordinary constants afterwards (sub-plan E, decision
            // 15), so they read on the value side exactly as a `const` does.
            // A `.field` or `[i]` step is the opposite: those name a place.
            ExprKind::Metadata(_) => true,
            ExprKind::Unary(unary) => match unary.op {
                UnaryOp::LowByte | UnaryOp::HighByte | UnaryOp::Address16 | UnaryOp::Address24 => {
                    true
                }
                UnaryOp::Negate | UnaryOp::BitNot => self.denotes_value(&unary.operand),
            },
            ExprKind::Binary(binary) => {
                self.denotes_value(&binary.left) && self.denotes_value(&binary.right)
            }
            _ => false,
        }
    }

    fn classify_operand(
        &mut self,
        mnemonic: Mnemonic,
        source: OperandSource<'_>,
        site: Span,
    ) -> Option<Classified> {
        // `&&label` and `&&&label` produce immediate relocations even in
        // address position — the one documented exception to "an unprefixed
        // value operand is an address operand".
        if let OperandSource::Address(address) = source
            && address.index.is_none()
            && address.encoding.is_none()
            && is_address_of(&address.value)
        {
            return self.immediate(mnemonic, site, &address.value);
        }

        let Some(form) = source_form(source) else {
            self.report(
                Diagnostic::error(
                    DiagnosticCode::INVALID_OPERAND,
                    format!("`{}` cannot take this operand shape", mnemonic.ca65()),
                    Label::new(
                        source.span(site),
                        "no 65816 addressing mode is written like this",
                    ),
                )
                .with_docs("doc/syntax-reference.md#65816-addressing-modes-in-k65"),
            );
            return None;
        };

        self.check_offset_marker(source, form);
        let form = accumulator_default(mnemonic, form);
        let mnemonic = aliased(mnemonic, form);

        match form {
            OperandForm::Immediate => {
                let OperandSource::Immediate(value) = source else {
                    unreachable!("only an immediate operand has the immediate form");
                };
                self.immediate(mnemonic, site, value)
            }
            OperandForm::Nothing | OperandForm::Accumulator => {
                let spec = self.select(mnemonic, form, site, source.span(site))?;
                Some(Classified::new(spec, None))
            }
            OperandForm::BlockMove => {
                let OperandSource::BlockMove(banks) = source else {
                    unreachable!("only a block move has the block-move form");
                };
                let spec = self.select(mnemonic, form, site, source.span(site))?;
                let source = self.bank_byte(&banks.source)?;
                let destination = self.bank_byte(&banks.destination)?;
                Some(Classified::new(
                    spec,
                    Some(Operand::BlockMove {
                        source,
                        destination,
                    }),
                ))
            }
            _ => self.addressed(mnemonic, form, source, site),
        }
    }

    /// Classifies `#expr`, and the `&&label` forms that behave like one.
    fn immediate(&mut self, mnemonic: Mnemonic, span: Span, value: &Expr) -> Option<Classified> {
        let spec = self.select(mnemonic, OperandForm::Immediate, span, value.span)?;
        let expr = self.fold(value)?;
        // A link-time immediate is legal — ca65 relocates it — and simply
        // cannot be range-checked here, because ld65 has not placed it yet.
        if let Some(folded) = expr.value()
            && let Some(class) = spec.immediate_width()
        {
            match class.max_value(self.widths.a.width(), self.widths.i.width()) {
                Some(limit) if folded.get() > limit => self.report(
                    Diagnostic::error(
                        DiagnosticCode::IMMEDIATE_RANGE,
                        format!("immediate {folded} does not fit in the selected width"),
                        Label::new(value.span, format!("maximum here is ${limit:X}")),
                    )
                    .with_docs("doc/register-width-aware-syntax.md"),
                ),
                Some(_) => {}
                None => {
                    let diagnostic = unknown_width(mnemonic, value.span, class, self.widths);
                    self.report(diagnostic);
                }
            }
        }
        Some(Classified::new(spec, Some(Operand::Immediate(expr))))
    }

    /// One bank byte of a block move, which is always a compile-time value.
    fn bank_byte(&mut self, expr: &Expr) -> Option<KExpr> {
        let value = self.constant(expr, "block move bank bytes")?;
        if value > 0xFF {
            self.report(
                Diagnostic::error(
                    DiagnosticCode::ADDRESS_RANGE,
                    format!("bank byte ${value:X} does not fit in one byte"),
                    Label::new(expr.span, "a block move takes bank numbers, not addresses"),
                )
                .with_docs("doc/syntax-reference.md#65816-block-moves"),
            );
            return None;
        }
        Some(KExpr::number(value))
    }

    /// Classifies every operand form that carries an address.
    fn addressed(
        &mut self,
        mnemonic: Mnemonic,
        form: OperandForm,
        source: OperandSource<'_>,
        site: Span,
    ) -> Option<Classified> {
        let (value, forced, index) = match source {
            OperandSource::Address(address) => (&address.value, address.encoding, address.index),
            OperandSource::Indirect(indirect) => (&indirect.value, indirect.encoding, None),
            OperandSource::LongIndirect(long) => (&long.value, long.encoding, None),
            _ => unreachable!("an addressed form always carries an address expression"),
        };

        self.check_const_in_address_position(mnemonic, value, source);
        let spec = self.select_encoding(mnemonic, form, source, site, value, forced)?;
        let expr = self.fold(value)?;
        self.check_fits(spec, &expr, value.span);
        self.check_datum_width(mnemonic, form, value);

        let index = index.and_then(|index| match index.kind {
            IndexRegisterKind::X => Some(IndexRegister::X),
            IndexRegisterKind::Y => Some(IndexRegister::Y),
            // `expr,S` is a mode of its own, not an index.
            IndexRegisterKind::Stack => None,
        });
        let operand = match form {
            OperandForm::Address => match spec.operand.width {
                // A branch or `per` target is a displacement ca65 computes.
                OperandWidth::Displacement => Operand::Label(expr),
                _ => Operand::Address {
                    encoding: spec.encoding().expect("an address form encodes an address"),
                    expr,
                    index: None,
                },
            },
            OperandForm::AddressIndexedX | OperandForm::AddressIndexedY => Operand::Address {
                encoding: spec.encoding().expect("an address form encodes an address"),
                expr,
                index,
            },
            OperandForm::StackRelative => Operand::StackRelative(expr),
            OperandForm::Indirect
            | OperandForm::IndirectIndexedX
            | OperandForm::IndirectIndexedY
            | OperandForm::StackRelativeIndirectIndexedY
            | OperandForm::LongIndirect
            | OperandForm::LongIndirectIndexedY => Operand::Indirect {
                encoding: spec.encoding().expect("an indirect form encodes a pointer"),
                expr,
                long: matches!(
                    form,
                    OperandForm::LongIndirect | OperandForm::LongIndirectIndexedY
                ),
                inner: matches!(form, OperandForm::IndirectIndexedX).then_some(IndexRegister::X),
                outer: matches!(
                    form,
                    OperandForm::IndirectIndexedY
                        | OperandForm::LongIndirectIndexedY
                        | OperandForm::StackRelativeIndirectIndexedY
                )
                .then_some(IndexRegister::Y),
                stack_relative: form == OperandForm::StackRelativeIndirectIndexedY,
            },
            OperandForm::Nothing
            | OperandForm::Accumulator
            | OperandForm::Immediate
            | OperandForm::BlockMove => {
                unreachable!("these forms carry no address")
            }
        };
        Some(Classified::new(spec, Some(operand)))
    }

    /// Checks a datum's declared width against the register the access moves
    /// it through.
    ///
    /// This is the *data-width* axis and nothing else. Where the variable
    /// lives and how the operand encodes were decided above, from the
    /// storage class; how many bytes it holds was decided by its `:byte` /
    /// `:word` / `:far` suffix, and the two are independent
    /// (`doc/syntax-reference.md`, "Two orthogonal axes"). What is checked
    /// here is that the register can hold the datum: a `:word` read with an
    /// 8-bit accumulator reads half of it, and a `:far` one has no register
    /// wide enough at all.
    ///
    /// Only the direct address forms qualify. A long-indirect operand
    /// (`lda [ptr]`) names a *pointer*, and the document offers that spelling
    /// as the way to reach a `:far` variable — checking `ptr`'s own width
    /// there would refuse the thing it recommends.
    fn check_datum_width(&mut self, mnemonic: Mnemonic, form: OperandForm, value: &Expr) {
        if !matches!(
            form,
            OperandForm::Address | OperandForm::AddressIndexedX | OperandForm::AddressIndexedY
        ) {
            return;
        }
        let Some((width, declaration)) = self.declared_width(value) else {
            return;
        };
        let Some(axis) = datum_axis(mnemonic) else {
            return;
        };
        // A datum wider than any register is wrong whatever the widths are.
        if width == DataWidth::Far {
            self.report(datum_too_wide(mnemonic, value.span, declaration));
            return;
        }
        let state = match axis {
            Axis::Accumulator => self.widths.a,
            Axis::Index => self.widths.i,
        };
        // An unknown width makes no claim, so it cannot disagree with one.
        let AxisState::Known(register) = state else {
            return;
        };
        let held = match register {
            RegisterWidth::W8 => DataWidth::Byte,
            RegisterWidth::W16 => DataWidth::Word,
        };
        if held != width {
            self.report(datum_width_mismatch(
                mnemonic,
                value.span,
                declaration,
                width,
                axis,
                register,
            ));
        }
    }

    /// The data width one address expression declares, and where it said so.
    ///
    /// The access path's answer, so `TASKS.state` is checked against the
    /// field's width and not the variable's. A use-site typed view is the
    /// nearer answer and overrides the declaration's, which is what makes
    /// `lda far x:byte` a one-byte view of a variable declared however it
    /// was. An expression that declares nothing — arithmetic, a literal, an
    /// address-of — has no width to disagree with.
    fn declared_width(&self, value: &Expr) -> Option<(DataWidth, Span)> {
        // A quiet probe: the operand was folded for real before this check
        // runs, and every diagnostic the path deserves was reported there.
        let mut probe = Resolver::new(self.symbols)
            .in_function(self.function)
            .with_widths(self.widths)
            .with_aliases(self.aliases.clone());
        probe.access(value)?.place.width
    }

    /// Narrows a mnemonic's specs to one by source form, for the forms whose
    /// shape already fixes the mode.
    fn select(
        &mut self,
        mnemonic: Mnemonic,
        form: OperandForm,
        mnemonic_span: Span,
        operand_span: Span,
    ) -> Option<&'static InstructionSpec> {
        let mut candidates = spec_for_form(mnemonic, form);
        match candidates.next() {
            Some(spec) => {
                debug_assert!(
                    candidates.next().is_none(),
                    "{mnemonic:?} has two {form:?} specs; only the address family may"
                );
                Some(spec)
            }
            None => {
                self.report_invalid_form(mnemonic, form, mnemonic_span, operand_span);
                None
            }
        }
    }

    /// Chooses both the spec and its address encoding.
    ///
    /// An operand prefix forces the encoding outright; without one the
    /// narrowest encoding that provably holds the address wins, and "provably"
    /// is the whole point — see the module documentation.
    fn select_encoding(
        &mut self,
        mnemonic: Mnemonic,
        form: OperandForm,
        source: OperandSource<'_>,
        site: Span,
        value: &Expr,
        forced: Option<AstEncoding>,
    ) -> Option<&'static InstructionSpec> {
        let candidates: Vec<&'static InstructionSpec> = spec_for_form(mnemonic, form).collect();
        if candidates.is_empty() {
            self.report_invalid_form(mnemonic, form, site, source.span(site));
            return None;
        }
        // A displacement operand has no encoding to choose.
        if candidates.iter().all(|spec| spec.encoding().is_none()) {
            if let Some(forced) = forced {
                self.report_invalid_encoding(mnemonic, forced, &candidates);
                return None;
            }
            return Some(candidates[0]);
        }

        if let Some(prefix) = forced {
            let wanted = encoding_of(prefix.kind);
            return match candidates
                .iter()
                .find(|spec| spec.encoding() == Some(wanted))
            {
                Some(spec) => Some(spec),
                None => {
                    self.report_invalid_encoding(mnemonic, prefix, &candidates);
                    None
                }
            };
        }

        // Auto-shrink: only an address K816 knows now may narrow.
        let preferred = encoding_of_size(self.preferred_size(value));
        // Widening is always safe, narrowing never is, so the choice is the
        // narrowest available encoding that is at least as wide as needed.
        let mut usable: Vec<&'static InstructionSpec> = candidates
            .iter()
            .copied()
            .filter(|spec| {
                spec.encoding()
                    .is_some_and(|encoding| encoding >= preferred)
            })
            .collect();
        usable.sort_by_key(|spec| spec.encoding());
        if let Some(spec) = usable.first() {
            return Some(*spec);
        }

        // Nothing this instruction offers is wide enough for the value.
        let widest = candidates
            .iter()
            .filter_map(|spec| spec.encoding())
            .max()
            .expect("at least one candidate encodes an address");
        self.report(
            Diagnostic::error(
                DiagnosticCode::ADDRESS_RANGE,
                format!(
                    "`{}` cannot reach this address in any of its forms",
                    mnemonic.ca65()
                ),
                Label::new(
                    value.span,
                    format!(
                        "needs a {} encoding; the widest available is {}",
                        size_name(size_of_encoding(preferred)),
                        size_name(size_of_encoding(widest))
                    ),
                ),
            )
            .with_docs("doc/syntax-reference.md#address-encoding-prefixes"),
        );
        None
    }

    /// The address size a plain reference to `value` should take.
    ///
    /// A folded literal reports its own magnitude and a pinned variable its
    /// declared size. Everything else — a label, a linker-allocated
    /// variable, any expression over one — is placed by ld65, so K816 cannot
    /// prove page-zero fit and must not narrow.
    fn preferred_size(&mut self, value: &Expr) -> AddressSize {
        // Through a subscript and a typed view too: `COMP[2]` and `x:byte`
        // are the declaration's storage seen from a use site, so the
        // encoding is the declaration's as much as a bare name's is.
        if let Some(name) = base_name(value)
            && let Some(id) = self.symbols.lookup(&name.text)
            && let Some(size) = self.symbols.kind(id).address_size()
        {
            return size;
        }
        // Folding here must stay quiet: any diagnostic the expression
        // deserves is reported when it is folded for real.
        let mut probe = Resolver::new(self.symbols)
            .in_function(self.function)
            .with_widths(self.widths)
            .with_aliases(self.aliases.clone());
        match probe.fold(value) {
            // A folded name is as good an answer as a written one, which is
            // what lets an `inline` alias bound to a direct-page variable
            // take the direct-page encoding at the use site.
            Some(KExpr::Symbol(id)) => self
                .symbols
                .kind(id)
                .address_size()
                .unwrap_or(AddressSize::Absolute),
            Some(folded) => folded.value().map_or(AddressSize::Absolute, |value| {
                AddressSize::of_value(value.get())
            }),
            None => AddressSize::Absolute,
        }
    }

    /// The address an expression denotes, when K816 knows it now.
    ///
    /// A pinned variable keeps its name in the emitted assembly — ca65 owns
    /// resolution — but its address is still known here, which is what lets
    /// a forced `dp` prefix on a `$0400` variable be rejected rather than
    /// silently handed to ca65 to encode differently.
    fn known_address(&self, expr: &KExpr) -> Option<Value> {
        match expr {
            KExpr::Number(value) => Some(*value),
            KExpr::Symbol(id) => self.symbols.kind(*id).known_address().map(Value::new),
            KExpr::Unary { .. } | KExpr::Binary { .. } => None,
        }
    }

    /// Rejects a value the chosen encoding cannot hold.
    fn check_fits(&mut self, spec: &InstructionSpec, expr: &KExpr, span: Span) {
        let (Some(encoding), Some(value)) = (spec.encoding(), self.known_address(expr)) else {
            return; // A link-time address is ld65's to check.
        };
        if value.get() <= encoding.max_value() {
            return;
        }
        self.report(
            Diagnostic::error(
                DiagnosticCode::ADDRESS_RANGE,
                format!(
                    "{value} does not fit a {} address",
                    size_name(size_of_encoding(encoding))
                ),
                Label::new(span, format!("maximum here is ${:X}", encoding.max_value())),
            )
            .with_help(format!(
                "drop the `{}` prefix, or use an address that fits",
                encoding.k65_keyword()
            ))
            .with_docs("doc/syntax-reference.md#address-encoding-prefixes"),
        );
    }

    /// A `const` names a value, not a memory location.
    ///
    /// The `&expr` marker is the way to use one as a small field offset in
    /// an indexed mode, and `&&label` / `&&&label` are the address-of
    /// exception; neither reaches here as a bare const-rooted expression.
    fn check_const_in_address_position(
        &mut self,
        mnemonic: Mnemonic,
        value: &Expr,
        source: OperandSource<'_>,
    ) {
        if let OperandSource::Address(address) = source
            && address.offset_marker.is_some()
        {
            return;
        }
        let Some((name, span)) = self.first_const(value) else {
            return;
        };
        let declaration = self
            .symbols
            .lookup(&name)
            .map(|id| self.symbols.symbol(id).declaration);
        let mut diagnostic = Diagnostic::error(
            DiagnosticCode::CONST_IN_ADDRESS_POSITION,
            format!("`{name}` is a `const`, not a memory location"),
            Label::new(span, "a `const` names a value, not an address"),
        )
        .with_docs("doc/syntax-reference.md#65816-addressing-modes-in-k65");
        // The immediate is only a fix where the instruction has one: `sta #K`
        // is not a store, so offering it would trade one error for another.
        if spec_for_form(mnemonic, OperandForm::Immediate)
            .next()
            .is_some()
        {
            diagnostic = diagnostic
                .with_fix(Fix {
                    title: format!("load the value with `#{name}`"),
                    applicability: Applicability::MachineApplicable,
                    edits: vec![TextEdit::insert(span, "#")],
                })
                .with_help("or write `&` before it to use it as an indexed field offset");
        } else {
            diagnostic =
                diagnostic.with_help("declare a `var` or a label for the location to address");
        }
        if let Some(declaration) = declaration {
            diagnostic = diagnostic.with_secondary(Label::new(declaration, "declared here"));
        }
        self.report(diagnostic);
    }

    /// The first `const` name anywhere in `expr`, with its span.
    fn first_const(&self, expr: &Expr) -> Option<(String, Span)> {
        match &expr.kind {
            ExprKind::Symbol(name) => {
                let id = self.symbols.lookup(&name.text)?;
                matches!(self.symbols.kind(id), SymbolKind::Const { .. })
                    .then(|| (name.text.clone(), name.span))
            }
            ExprKind::Unary(unary) => self.first_const(&unary.operand),
            ExprKind::Binary(binary) => self
                .first_const(&binary.left)
                .or_else(|| self.first_const(&binary.right)),
            _ => None,
        }
    }

    /// The `&expr` marker is legal only in the `,X` and `,Y` indexed modes.
    fn check_offset_marker(&mut self, source: OperandSource<'_>, form: OperandForm) {
        let OperandSource::Address(address) = source else {
            return;
        };
        let Some(marker) = address.offset_marker else {
            return;
        };
        if matches!(
            form,
            OperandForm::AddressIndexedX | OperandForm::AddressIndexedY
        ) {
            return;
        }
        self.report(
            Diagnostic::error(
                DiagnosticCode::OFFSET_MARKER_POSITION,
                "the `&` offset marker is only legal in a `,x` or `,y` indexed mode",
                Label::new(marker, "no index register to offset from"),
            )
            .with_note(
                "the marker exists so a `const` can serve as a field offset \
                 when the struct base is in X or Y",
            )
            .with_fix(Fix {
                title: "remove the offset marker".to_owned(),
                applicability: Applicability::MaybeIncorrect,
                edits: vec![TextEdit::delete(marker)],
            })
            .with_docs("doc/syntax-reference.md#65816-addressing-modes-in-k65"),
        );
    }

    fn report_invalid_form(
        &mut self,
        mnemonic: Mnemonic,
        form: OperandForm,
        mnemonic_span: Span,
        operand_span: Span,
    ) {
        let legal: Vec<&'static str> = crate::isa::specs_for(mnemonic)
            .map(|spec| spec.mode.symbol())
            .collect();
        let mut diagnostic = Diagnostic::error(
            DiagnosticCode::INVALID_OPERAND,
            format!("`{}` does not support this operand form", mnemonic.ca65()),
            Label::new(
                operand_span,
                format!("{} is not legal here", form_name(form)),
            ),
        )
        .with_secondary(Label::new(mnemonic_span, "this instruction"))
        .with_docs("doc/w65c816s.md");
        if !legal.is_empty() {
            diagnostic = diagnostic.with_note(format!(
                "`{}` accepts: {}",
                mnemonic.ca65(),
                legal.join(", ")
            ));
        }
        self.report(diagnostic);
    }

    fn report_invalid_encoding(
        &mut self,
        mnemonic: Mnemonic,
        prefix: AstEncoding,
        candidates: &[&'static InstructionSpec],
    ) {
        let mut legal: Vec<&'static str> = candidates
            .iter()
            .filter_map(|spec| spec.encoding())
            .map(AddressEncoding::k65_keyword)
            .collect();
        legal.sort_unstable();
        legal.dedup();
        let wanted = encoding_of(prefix.kind);
        let mut diagnostic = Diagnostic::error(
            DiagnosticCode::INVALID_ENCODING,
            format!(
                "`{}` has no {} form",
                mnemonic.ca65(),
                size_name(size_of_encoding(wanted))
            ),
            Label::new(prefix.span, "this encoding prefix is not available here"),
        )
        .with_docs("doc/syntax-reference.md#address-encoding-prefixes");
        match legal.as_slice() {
            [] => {
                diagnostic = diagnostic
                    .with_note("this operand form encodes no address, so it takes no prefix")
                    .with_fix(Fix {
                        title: "remove the encoding prefix".to_owned(),
                        applicability: Applicability::MachineApplicable,
                        edits: vec![TextEdit::delete(prefix.span)],
                    });
            }
            [only] => {
                diagnostic = diagnostic
                    .with_help(format!("the only legal prefix here is `{only}`"))
                    .with_fix(Fix {
                        title: format!("use `{only}` instead"),
                        applicability: Applicability::MaybeIncorrect,
                        edits: vec![TextEdit::replace(prefix.span, *only)],
                    });
            }
            several => {
                diagnostic =
                    diagnostic.with_help(format!("legal prefixes here: {}", several.join(", ")));
            }
        }
        self.report(diagnostic);
    }
}

/// The source operand shape, or `None` when no 65816 mode is written that
/// way (`(expr,Y)`, say, which no instruction has).
fn source_form(source: OperandSource<'_>) -> Option<OperandForm> {
    use IndexRegisterKind as I;
    let index = |register: Option<crate::ast::IndexRegister>| register.map(|index| index.kind);
    Some(match source {
        OperandSource::Nothing => OperandForm::Nothing,
        OperandSource::Register(register) => {
            if register.kind == crate::ast::RegisterKind::A {
                OperandForm::Accumulator
            } else {
                return None;
            }
        }
        OperandSource::Immediate(_) => OperandForm::Immediate,
        OperandSource::BlockMove(_) => OperandForm::BlockMove,
        OperandSource::Address(AddressOperand { index: idx, .. }) => match index(*idx) {
            None => OperandForm::Address,
            Some(I::X) => OperandForm::AddressIndexedX,
            Some(I::Y) => OperandForm::AddressIndexedY,
            Some(I::Stack) => OperandForm::StackRelative,
        },
        OperandSource::Indirect(IndirectOperand {
            inner_index,
            outer_index,
            ..
        }) => match (index(*inner_index), index(*outer_index)) {
            (None, None) => OperandForm::Indirect,
            (Some(I::X), None) => OperandForm::IndirectIndexedX,
            (None, Some(I::Y)) => OperandForm::IndirectIndexedY,
            (Some(I::Stack), Some(I::Y)) => OperandForm::StackRelativeIndirectIndexedY,
            _ => return None,
        },
        OperandSource::LongIndirect(long) => match index(long.outer_index) {
            None => OperandForm::LongIndirect,
            Some(I::Y) => OperandForm::LongIndirectIndexedY,
            Some(_) => return None,
        },
    })
}

/// True when the expression's outermost operator is `&&` or `&&&`, the
/// address-of forms that produce an immediate even in address position.
fn is_address_of(expr: &Expr) -> bool {
    matches!(
        &expr.kind,
        ExprKind::Unary(unary)
            if matches!(
                unary.op,
                crate::ast::UnaryOp::Address16 | crate::ast::UnaryOp::Address24
            )
    )
}

fn encoding_of(kind: StorageClassKind) -> AddressEncoding {
    match kind {
        StorageClassKind::DirectPage => AddressEncoding::Direct,
        StorageClassKind::Absolute => AddressEncoding::Absolute,
        StorageClassKind::Far => AddressEncoding::Long,
    }
}

fn encoding_of_size(size: AddressSize) -> AddressEncoding {
    match size {
        AddressSize::DirectPage => AddressEncoding::Direct,
        AddressSize::Absolute => AddressEncoding::Absolute,
        AddressSize::Long => AddressEncoding::Long,
    }
}

fn size_of_encoding(encoding: AddressEncoding) -> AddressSize {
    match encoding {
        AddressEncoding::Direct => AddressSize::DirectPage,
        AddressEncoding::Absolute => AddressSize::Absolute,
        AddressEncoding::Long => AddressSize::Long,
    }
}

fn size_name(size: AddressSize) -> &'static str {
    match size {
        AddressSize::DirectPage => "direct-page",
        AddressSize::Absolute => "absolute",
        AddressSize::Long => "long",
    }
}

fn form_name(form: OperandForm) -> &'static str {
    match form {
        OperandForm::Nothing => "no operand",
        OperandForm::Accumulator => "the accumulator",
        OperandForm::Immediate => "an immediate",
        OperandForm::Address => "an address",
        OperandForm::AddressIndexedX => "`,x` indexing",
        OperandForm::AddressIndexedY => "`,y` indexing",
        OperandForm::StackRelative => "stack-relative addressing",
        OperandForm::Indirect => "indirection",
        OperandForm::IndirectIndexedX => "`(expr,x)` indirection",
        OperandForm::IndirectIndexedY => "`(expr),y` indirection",
        OperandForm::StackRelativeIndirectIndexedY => "`(expr,s),y` indirection",
        OperandForm::LongIndirect => "`[expr]` long indirection",
        OperandForm::LongIndirectIndexedY => "`[expr],y` long indirection",
        OperandForm::BlockMove => "a block move",
    }
}

/// The lowered form of a source unary operator.
fn lowered_unary_op(op: crate::ast::UnaryOp) -> UnaryOp {
    match op {
        crate::ast::UnaryOp::Negate => UnaryOp::Negate,
        crate::ast::UnaryOp::BitNot => UnaryOp::BitNot,
        crate::ast::UnaryOp::LowByte => UnaryOp::LowByte,
        crate::ast::UnaryOp::HighByte => UnaryOp::HighByte,
        crate::ast::UnaryOp::Address16 => UnaryOp::Word,
        crate::ast::UnaryOp::Address24 => UnaryOp::Long,
    }
}

/// The lowered form of a source binary operator. K65's C precedence is
/// already encoded in the tree B built, so nothing here reorders anything.
fn lowered_binary_op(op: crate::ast::BinaryOp) -> BinaryOp {
    match op {
        crate::ast::BinaryOp::Mul => BinaryOp::Mul,
        crate::ast::BinaryOp::Add => BinaryOp::Add,
        crate::ast::BinaryOp::Sub => BinaryOp::Sub,
        crate::ast::BinaryOp::Shl => BinaryOp::Shl,
        crate::ast::BinaryOp::Shr => BinaryOp::Shr,
        crate::ast::BinaryOp::BitAnd => BinaryOp::BitAnd,
        crate::ast::BinaryOp::BitXor => BinaryOp::BitXor,
        crate::ast::BinaryOp::BitOr => BinaryOp::BitOr,
    }
}

/// `asl` with no operand means `asl a`.
///
/// The accumulator modes are written with or without naming the register:
/// `doc/syntax-reference.md` ("Address Modes") calls `OPC A` an "implied
/// single byte instruction". Only a mnemonic that has no operandless form of
/// its own can mean the accumulator, so `nop` and `pha` are untouched.
fn accumulator_default(mnemonic: Mnemonic, form: OperandForm) -> OperandForm {
    if form == OperandForm::Nothing
        && spec_for_form(mnemonic, OperandForm::Nothing)
            .next()
            .is_none()
        && spec_for_form(mnemonic, OperandForm::Accumulator)
            .next()
            .is_some()
    {
        OperandForm::Accumulator
    } else {
        form
    }
}

/// The mnemonic a source spelling resolves to when a synonym is written.
///
/// ca65 also accepts `jmp [addr]` for the absolute indirect long jump. The
/// table files that opcode under `JML`, where `doc/w65c816s.md` §3.5.5 puts
/// it, so the synonym is resolved here rather than duplicating the row and
/// breaking the one-spec-per-opcode key.
fn aliased(mnemonic: Mnemonic, form: OperandForm) -> Mnemonic {
    match (mnemonic, form) {
        (Mnemonic::Jmp, OperandForm::LongIndirect) => Mnemonic::Jml,
        _ => mnemonic,
    }
}

// ---------------------------------------------------------------------------
// Data blocks (sub-plan C, task C8).
// ---------------------------------------------------------------------------

/// Where a data block sits, which is what makes the segment name an
/// `address` entry needs a pure function of the source.
///
/// The ld65 config entry that goes with an `address` block is written by
/// hand, so a name that renumbers when an unrelated block is inserted
/// earlier in the file would be a real failure: ld65 would report an
/// unplaced segment and the user would re-edit a config that was correct.
/// A *named* block is therefore identified by its name alone and is stable
/// under any edit that does not rename it; only the anonymous forms carry a
/// position-derived counter, which is what `W0116`'s help says naming the
/// block buys you.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DataSite {
    /// `data NAME { … }`. Named blocks are unique across the workspace, so
    /// no unit prefix is needed.
    Named(String),
    /// An anonymous block inline in a function.
    InFunction {
        unit: String,
        function: String,
        index: usize,
    },
    /// An anonymous block at the top level.
    TopLevel { unit: String, index: usize },
}

impl DataSite {
    /// The `<SITE>` half of a synthetic segment name.
    fn identity(&self) -> String {
        match self {
            Self::Named(name) => sanitize(name),
            Self::InFunction {
                unit,
                function,
                index,
            } => format!("{}_{}_D{index}", sanitize(unit), sanitize(function)),
            Self::TopLevel { unit, index } => format!("{}_D{index}", sanitize(unit)),
        }
    }
}

/// Uppercases and reduces a name to `[A-Z0-9_]`, which is how ca65 asks
/// segment names to be spelled (`doc/ca65.sgml`, `.SEGMENT`).
fn sanitize(name: &str) -> String {
    let cleaned: String = name
        .chars()
        .map(|c| {
            if c.is_ascii_alphanumeric() {
                c.to_ascii_uppercase()
            } else {
                '_'
            }
        })
        .collect();
    cleaned.trim_matches('_').to_owned()
}

/// The segment an `address $addr` entry places its block into.
///
/// ca65 cannot pin a relocatable block to an address, so the request
/// becomes a segment the ld65 config gives a fixed `start`. A block may
/// hold more than one `address` entry — it is an entry, not a header — so
/// the second and later ones append `_A<k>` counting from 2.
fn address_segment(site: &DataSite, address: u32, occurrence: usize) -> SegmentName {
    let digits = if address > 0xFFFF { 6 } else { 4 };
    let suffix = if occurrence > 1 {
        format!("_A{occurrence}")
    } else {
        String::new()
    };
    SegmentName::new(format!(
        "K816_AT_{address:0digits$X}__{}{suffix}",
        site.identity()
    ))
}

/// The index of the last entry in a block's leading statically-sizeable
/// run, which is where a `nocross` guard goes.
fn last_index_of_run(block: &DataBlock) -> usize {
    block
        .entries
        .iter()
        .take_while(|entry| entry_size(entry).is_some())
        .count()
        .saturating_sub(1)
}

/// The bytes one data entry emits, or `None` when the entry is not a
/// statically-sizeable run member.
fn entry_size(entry: &DataEntry) -> Option<u32> {
    match &entry.kind {
        // A label takes no space and does not interrupt a run.
        DataEntryKind::Label(_) | DataEntryKind::Nocross | DataEntryKind::Segment(_) => Some(0),
        DataEntryKind::Value(DataValue::Expr(expr)) => Some(address_of_width(expr).unwrap_or(1)),
        DataEntryKind::Value(_) => Some(1),
        _ => None,
    }
}

/// The width in bytes an address-of operator gives a data value, or `None`
/// when the value takes the one-byte default.
fn address_of_width(expr: &Expr) -> Option<u32> {
    match &expr.kind {
        ExprKind::Unary(unary) => match unary.op {
            crate::ast::UnaryOp::Address16 => Some(2),
            crate::ast::UnaryOp::Address24 => Some(3),
            _ => None,
        },
        _ => None,
    }
}

impl Resolver<'_> {
    /// Lowers one data block, reporting everything wrong with it.
    ///
    /// Analysis calls this for the diagnostics and drops the operations;
    /// lowering calls it for the operations, having already established that
    /// there are no diagnostics. One walk, so the two cannot disagree about
    /// what a block emits.
    pub fn data_block(&mut self, block: &DataBlock, span: Span, site: &DataSite) -> Vec<KOp> {
        let mut ops = Vec::new();
        let mut addresses = 0usize;
        // A `segment` inside a block is scope-local: it switches segments
        // for the block's contents and is restored when the block ends.
        let mut switched = false;
        // The block's name labels the *first emitted byte*, so it follows
        // any leading placement directive: an `align` before it would move
        // the bytes out from under the label.
        let mut labelled = block.name.is_none();
        // `nocross data { … }` is the wrapper spelling of a leading bare
        // `nocross`, so both arrive at the same guard.
        let mut guard = if block.nocross {
            self.nocross_run(block, span)
        } else {
            None
        };
        // Where the guard goes: after the last entry of the leading run,
        // whose page it compares against the run's first byte.
        let run_ends = last_index_of_run(block);

        for (index, entry) in block.entries.iter().enumerate() {
            let placement = matches!(
                entry.kind,
                DataEntryKind::Segment(_)
                    | DataEntryKind::Align(_)
                    | DataEntryKind::Address(_)
                    | DataEntryKind::Nocross
            );
            if !placement && !labelled {
                labelled = true;
                if let Some(name) = &block.name
                    && let Some(id) = self.symbols.lookup(&name.text)
                {
                    ops.push(KOp::new(KOpKind::Label(id), name.span, Origin::User));
                }
            }
            match &entry.kind {
                DataEntryKind::Segment(name) => {
                    // The rule exists because a segment switch after bytes
                    // have already been emitted has no defined placement
                    // relative to them.
                    if index != 0 {
                        self.report(
                            Diagnostic::error(
                                DiagnosticCode::SEGMENT_PLACEMENT,
                                "a `segment` directive must be the first entry of its data block",
                                Label::new(entry.span, "bytes are already placed before this"),
                            )
                            .with_note(
                                "a block-local `segment` switches segments for the whole \
                                 block and is restored when it ends",
                            )
                            .with_docs("doc/syntax-reference.md#segment-placement-rule"),
                        );
                        continue;
                    }
                    switched = true;
                    ops.push(KOp::new(
                        KOpKind::Segment {
                            name: SegmentName::new(&name.text),
                            addressing: None,
                        },
                        entry.span,
                        Origin::User,
                    ));
                }
                DataEntryKind::Align(count) => {
                    if let Some(value) = self.constant(count, "`align` counts") {
                        ops.push(KOp::new(
                            KOpKind::Align(KExpr::number(value)),
                            entry.span,
                            Origin::User,
                        ));
                    }
                }
                DataEntryKind::Address(address) => {
                    let Some(value) = self.constant(address, "`address` placements") else {
                        continue;
                    };
                    addresses += 1;
                    let segment = address_segment(site, value, addresses);
                    self.report_address_placement(entry.span, &segment, value, site);
                    ops.push(KOp::new(
                        KOpKind::Segment {
                            name: segment,
                            addressing: None,
                        },
                        entry.span,
                        Origin::CompilerGenerated,
                    ));
                }
                DataEntryKind::Nocross => {
                    if let Some(length) = self.nocross_run(block, entry.span) {
                        guard = Some(length);
                    }
                }
                DataEntryKind::Label(name) => {
                    if let Some(id) = self.symbols.lookup(&name.text) {
                        ops.push(KOp::new(KOpKind::Label(id), name.span, Origin::User));
                    }
                }
                DataEntryKind::Value(value) => {
                    if let Some(data) = self.data_value(value, entry.span) {
                        ops.push(KOp::new(KOpKind::Data(data), entry.span, Origin::User));
                    }
                }
                // Typed entries, strings, `charset`, `repeat`, `code`, and
                // `for … eval` are `HLA-core` rows; C builds the emission
                // machinery they extend rather than implementing them early.
                DataEntryKind::Typed { .. }
                | DataEntryKind::Charset(_)
                | DataEntryKind::Code { .. }
                | DataEntryKind::Repeat { .. }
                | DataEntryKind::ForEval { .. } => self.report(compat::unimplemented(
                    entry.span,
                    "typed entries, strings, `charset`, `repeat`, `code`, and `for … eval`",
                    compat::Owner::HlaLowering,
                )),
                DataEntryKind::Preprocessor(_) => self.report(compat::unimplemented(
                    entry.span,
                    "preprocessor directives",
                    compat::Owner::Compatibility,
                )),
                DataEntryKind::Binary(_) => self.report(compat::unimplemented(
                    entry.span,
                    "`binary` inclusions",
                    compat::Owner::Compatibility,
                )),
                DataEntryKind::Reserved(_) | DataEntryKind::Error => {}
            }
            // The guard compares the page of the run's last byte with the
            // page of its first, so it follows the run rather than leading
            // it.
            if let Some(length) = guard
                && index == run_ends
            {
                ops.push(KOp::new(
                    KOpKind::NocrossGuard { length },
                    span,
                    Origin::CompilerGenerated,
                ));
                guard = None;
            }
        }

        // Restore the outer segment, so a block-local switch really is local.
        if switched {
            ops.push(KOp::new(
                KOpKind::Segment {
                    name: SegmentName::new(OUTER_SEGMENT),
                    addressing: None,
                },
                span,
                Origin::CompilerGenerated,
            ));
        }
        ops
    }

    /// One value of an implicit-default entry.
    ///
    /// The default is one byte, with one documented exception: the
    /// address-of operators carry their own width, so `&&ptr` is "the full
    /// 16-bit address (two bytes, little-endian)" and `&&&ptr` a 24-bit one
    /// (`doc/syntax-reference.md`, "Address Byte Operators"). Emitting
    /// either as a byte would be a range error at best and a truncated
    /// address at worst.
    ///
    /// `?` reserves a slot whose value is deliberately unspecified and emits
    /// as a zero of the entry's width — the affordance is at the source
    /// level, not on the wire.
    fn data_value(&mut self, value: &DataValue, span: Span) -> Option<DataOp> {
        match value {
            DataValue::Expr(expr) => {
                let width = address_of_width(expr);
                let folded = self.fold(expr)?;
                if width.is_none()
                    && let Some(known) = folded.value()
                    && known.get() > 0xFF
                {
                    self.report(
                        Diagnostic::error(
                            DiagnosticCode::ADDRESS_RANGE,
                            format!("{known} does not fit in a byte"),
                            Label::new(expr.span, "an untyped data value is one byte wide"),
                        )
                        .with_help("use `&<`/`&>` to select a byte, or a `word` entry")
                        .with_docs("doc/syntax-reference.md#raw-bytes-and-strings"),
                    );
                    return None;
                }
                Some(match width {
                    Some(2) => DataOp::Word(vec![folded]),
                    Some(_) => DataOp::Far(vec![folded]),
                    None => DataOp::Byte(vec![folded]),
                })
            }
            DataValue::Undefined => Some(DataOp::Byte(vec![KExpr::number(0)])),
            // Strings go through the active `charset`, which is E's row.
            DataValue::String(_) => {
                self.report(compat::unimplemented(
                    span,
                    "string data and `charset` mapping",
                    compat::Owner::HlaLowering,
                ));
                None
            }
        }
    }

    /// The length of a `nocross`-guarded run, once it is known to fit.
    ///
    /// The guarded run is the leading stretch of statically-sizeable
    /// entries; it stops at anything whose size only placement decides. A
    /// run larger than a page can never fit wherever ld65 puts it, so that
    /// is rejected here rather than deferred.
    fn nocross_run(&mut self, block: &DataBlock, span: Span) -> Option<u32> {
        let run: u32 = block
            .entries
            .iter()
            .map(entry_size)
            .take_while(Option::is_some)
            .flatten()
            .sum();
        if run > 256 {
            self.report(
                Diagnostic::error(
                    DiagnosticCode::NOCROSS_TOO_LARGE,
                    format!("a `nocross` run of {run} bytes cannot fit one page"),
                    Label::new(span, "a page is 256 bytes"),
                )
                .with_note(
                    "K816 checks the size; ld65 checks the placement, through an \
                     assertion in the emitted assembly",
                )
                .with_docs("doc/syntax-reference.md#placement-directives"),
            );
            return None;
        }
        (run > 0).then_some(run)
    }

    /// `address` needs an ld65 config entry, and K816 does not write linker
    /// configs — so it says exactly what to paste rather than dropping the
    /// request silently.
    fn report_address_placement(
        &mut self,
        span: Span,
        segment: &SegmentName,
        address: u32,
        site: &DataSite,
    ) {
        let mut diagnostic = Diagnostic::warning(
            DiagnosticCode::ADDRESS_NEEDS_CONFIG,
            format!("`address` places this block through the segment `{segment}`"),
            Label::new(span, "ld65 decides where a segment starts"),
        )
        .with_note(format!(
            "add to your linker config: SEGMENTS {{ {segment}: load = MAIN, \
             type = ro, start = ${address:04X}; }}"
        ))
        .with_docs("doc/syntax-reference.md#placement-directives");
        if !matches!(site, DataSite::Named(_)) {
            diagnostic = diagnostic.with_help(
                "name the block (`data NAME { … }`) to get a segment name that \
                 does not renumber when an earlier block is added",
            );
        }
        self.report(diagnostic);
    }
}

/// The segment a block-local `segment` switch returns to.
///
/// K816 emits a unit's code into `CODE` unless a top-level `segment` says
/// otherwise; restoring to it is what makes a block-local switch local.
pub const OUTER_SEGMENT: &str = "CODE";

/// How many anonymous data blocks each site has produced so far.
///
/// The counters are per site — top level, and each function separately — so
/// inserting a block in one function cannot renumber another's.
#[derive(Debug, Default)]
pub struct AnonymousBlocks {
    top_level: usize,
    in_function: std::collections::BTreeMap<String, usize>,
}

/// The site of one data block, bumping the counter its form uses.
pub fn data_site(
    counters: &mut AnonymousBlocks,
    unit: &str,
    data: &DataBlock,
    function: Option<&str>,
) -> DataSite {
    if let Some(name) = &data.name {
        return DataSite::Named(name.text.clone());
    }
    match function {
        Some(function) => {
            let index = counters.in_function.entry(function.to_owned()).or_insert(0);
            *index += 1;
            DataSite::InFunction {
                unit: unit.to_owned(),
                function: function.to_owned(),
                index: *index,
            }
        }
        None => {
            counters.top_level += 1;
            DataSite::TopLevel {
                unit: unit.to_owned(),
                index: counters.top_level,
            }
        }
    }
}
