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

use crate::ast::{
    AddressEncoding as AstEncoding, AddressOperand, BlockMoveOperand, DataBlock, DataEntry,
    DataEntryKind, DataValue, Expr, ExprKind, HlaOperand, HlaOperandKind, IndexRegisterKind,
    IndirectOperand, LongIndirectOperand, NativeInstruction, NativeOperand, Register,
    StorageClassKind,
};
use crate::compat;
use crate::diagnostic::{Applicability, Diagnostic, DiagnosticCode, Fix, Label, TextEdit};
use crate::isa::{
    AddressEncoding, InstructionSpec, Mnemonic, OperandForm, OperandWidth, spec_for_form,
};
use crate::op::{
    AxisState, DataOp, EmulationState, IndexRegister, Instruction, KOp, KOpKind, ModeState,
    Operand, Origin, SegmentName,
};
use crate::source::{Provenance, Span};
use crate::symbol::{AddressSize, SymbolId, SymbolKind, SymbolTable};
use crate::value::{BinaryOp, KExpr, UnaryOp, Value};

/// One classified native instruction: the ISA row it matched and the typed
/// operand built from the source expression.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Classified {
    pub spec: &'static InstructionSpec,
    pub operand: Option<Operand>,
}

impl Classified {
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
}

impl<'a> Resolver<'a> {
    pub fn new(symbols: &'a SymbolTable) -> Self {
        Self {
            symbols,
            function: None,
            widths: ModeState::unknown(),
            diagnostics: Vec::new(),
            referenced: Vec::new(),
        }
    }

    /// Scopes local label resolution to `function`.
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

    fn report(&mut self, diagnostic: Diagnostic) {
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
                match self.symbols.kind(id) {
                    // A `const` has no storage, so it can only ever be its
                    // value; everything else keeps its name for ca65/ld65.
                    SymbolKind::Const { value } => Some(KExpr::number(value)),
                    // An `abstract var` has neither a value nor a place, so
                    // there is nothing for a mention of one to fold to. It is
                    // reported here rather than at each position that rejects
                    // it, because this is the single point where a name
                    // becomes a value.
                    SymbolKind::AbstractVar { .. } => {
                        let declaration = self.symbols.symbol(id).declaration;
                        self.report(
                            Diagnostic::error(
                                DiagnosticCode::ABSTRACT_HAS_NO_STORAGE,
                                format!("`{}` is an `abstract var`, which has no storage", name.text),
                                Label::new(name.span, "a layout, not a memory location"),
                            )
                            .with_secondary(Label::new(declaration, "declared here"))
                            .with_help(format!(
                                "query the layout with `{0}:sizeof` or `{0}.field:offsetof`, or declare a `var` with the same fields to give it storage",
                                name.text
                            ))
                            .with_docs("doc/symbolic-subscripts.md#abstract-layout-only-form"),
                        );
                        None
                    }
                    SymbolKind::Var { .. }
                    | SymbolKind::Label { .. }
                    | SymbolKind::DataLabel { .. }
                    | SymbolKind::Function { .. } => Some(KExpr::Symbol(id)),
                }
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
            ExprKind::Field(_)
            | ExprKind::Subscript(_)
            | ExprKind::Metadata(_)
            | ExprKind::TypedView(_) => {
                self.report(compat::unimplemented(
                    expr.span,
                    "symbolic subscripts, metadata queries, and typed views",
                    compat::Owner::HlaLowering,
                ));
                None
            }
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

    /// Resolves a unit-level name, reporting it when nothing declares it.
    pub fn resolve(&mut self, name: &str, span: Span) -> Option<SymbolId> {
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
        if let Some(count) = &native.repeat {
            self.report(compat::unimplemented(
                count.span,
                "instruction repetition counts",
                compat::Owner::HlaLowering,
            ));
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
        self.classify_operand(
            mnemonic,
            OperandSource::native(native.operand.as_ref()),
            native.mnemonic.span,
        )
    }

    /// Classifies the operand of an HLA statement against the mnemonic that
    /// statement's shape selects.
    ///
    /// The split is deliberate: which instruction a statement means is HLA's
    /// to decide, because that is what the spelling says; what its operand is
    /// is not HLA's to decide, because a native instruction already asked the
    /// same question (decision 1). `site` is the span to blame when the
    /// operand itself is not what is wrong — the statement's, where a native
    /// instruction would give its mnemonic's.
    pub fn classify_hla(
        &mut self,
        mnemonic: Mnemonic,
        operand: &HlaOperand,
        site: Span,
    ) -> Option<Classified> {
        let source = OperandSource::hla(operand)?;
        self.classify_operand(mnemonic, source, site)
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
                Some(Classified {
                    spec,
                    operand: None,
                })
            }
            OperandForm::BlockMove => {
                let OperandSource::BlockMove(banks) = source else {
                    unreachable!("only a block move has the block-move form");
                };
                let spec = self.select(mnemonic, form, site, source.span(site))?;
                let source = self.bank_byte(&banks.source)?;
                let destination = self.bank_byte(&banks.destination)?;
                Some(Classified {
                    spec,
                    operand: Some(Operand::BlockMove {
                        source,
                        destination,
                    }),
                })
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
        Some(Classified {
            spec,
            operand: Some(Operand::Immediate(expr)),
        })
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
        Some(Classified {
            spec,
            operand: Some(operand),
        })
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
        if let ExprKind::Symbol(name) = &value.kind
            && let Some(id) = self.symbols.lookup(&name.text)
            && let Some(size) = self.symbols.kind(id).address_size()
        {
            return size;
        }
        // Folding here must stay quiet: any diagnostic the expression
        // deserves is reported when it is folded for real.
        let mut probe = Resolver::new(self.symbols)
            .in_function(self.function)
            .with_widths(self.widths);
        match probe.fold(value).and_then(|expr| expr.value()) {
            Some(folded) => AddressSize::of_value(folded.get()),
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
                        KOpKind::Segment(SegmentName::new(&name.text)),
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
                        KOpKind::Segment(segment),
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
                KOpKind::Segment(SegmentName::new(OUTER_SEGMENT)),
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
