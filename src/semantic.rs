/*****************************************************************************/
/*                                                                           */
/*                                semantic.rs                                */
/*                                                                           */
/*        Symbol, contract, operand, and width checks over the AST           */
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

//! Semantic analysis for the vertical slice: declarations, function
//! entry-mode contracts, symbol lookup, native operand legality, and
//! immediate width/range checks.
//!
//! Operates purely on the owned AST — no Tree-sitter types — and produces a
//! [`SemanticModel`] that lowering and emission consume, plus structured
//! diagnostics.

use crate::ast::{
    self, AstFile, DataBlock, DataEntryKind, DataValue, EvalCallName, EvalExpr, EvalExprKind, Expr,
    ExprKind, Function, FunctionKind, HlaOperandKind, HlaStatement, ItemKind, RegisterKind,
    ReservedKind, ReturnKind, Statement, StatementKind,
};
use std::collections::{BTreeMap, BTreeSet};

use crate::compat;
use crate::diagnostic::{Applicability, Diagnostic, DiagnosticCode, Fix, Label, TextEdit};
use crate::effects::ControlEffect;
use crate::isa::{ImmediateWidthClass, Mnemonic, RegisterWidth};
use crate::linkage::Linkage;
use crate::op::{ExitContract, ModeState, WidthContract};
use crate::resolve::{AnonymousBlocks, Classified, DataSite, Resolved, Resolver, data_site};
use crate::source::{Provenance, Span};
use crate::symbol::{AddressSize, LabelScope, SymbolId, SymbolKind, SymbolTable};
use crate::value::KExpr;
use crate::width::{self, WidthAnalysis};

/// The result of analyzing one file: symbols plus per-function facts.
#[derive(Debug, Default)]
pub struct SemanticModel {
    pub symbols: SymbolTable,
    pub functions: Vec<FunctionInfo>,
    /// Every unit-level symbol the unit actually referred to, which is what
    /// separates an `.import` this unit needs from one it merely could have.
    pub referenced: BTreeSet<SymbolId>,
    /// The width dataflow's output, per function.
    ///
    /// Stored rather than recomputed: lowering looks a statement's state up
    /// here instead of walking the body a second time, which is what keeps
    /// analysis and lowering from arriving at two different widths.
    pub widths: BTreeMap<SymbolId, WidthAnalysis>,
}

impl SemanticModel {
    /// The width analysis of one function.
    pub fn width(&self, function: SymbolId) -> Option<&WidthAnalysis> {
        self.widths.get(&function)
    }
}

/// Facts about one function needed by lowering and emission.
///
/// Deliberately not `Copy`: `callees` is a `Vec`, and keeping the per-function
/// facts in one struct is worth a clone per function at the one site that
/// relied on the copy to end a borrow.
#[derive(Clone, Debug)]
pub struct FunctionInfo {
    pub symbol: SymbolId,
    /// Index of the corresponding [`ItemKind::Func`] in `AstFile::items`.
    pub item_index: usize,
    pub contract: WidthContract,
    /// `main` is the program entry point and receives entry-mode setup.
    pub is_entry_point: bool,
    /// What callers may believe about their widths after calling it.
    ///
    /// Consumed by call-site lowering, by the workspace exit classification,
    /// and by native-call handling. Until exit inference lands this is the
    /// language's `Unknown ⇒ Preserve` fallback for every function.
    pub exit: ExitContract,
    /// Whether the body contains a width-dependent immediate on each axis.
    ///
    /// D computes it and does not act on it: it is the seed of the
    /// optimizer's transitive dead-mode-elimination pass (sub-plan F).
    pub needs: WidthNeeds,
    /// Call edges, from both `call NAME` and a native `jsr`/`jsl` with a
    /// resolvable target. Consumed by exit inference, by the workspace exit
    /// classification, by damage tracking, and by F's transitive pass.
    pub callees: Vec<SymbolId>,
    /// True when the inferred exit fell back to the language's
    /// `Unknown ⇒ Preserve` rule rather than being derived from the body.
    pub exit_is_assumed: bool,
}

/// Which width axes a function body's immediates depend on.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct WidthNeeds {
    /// The body contains an `ImmediateM` instruction.
    pub m: bool,
    /// The body contains an `ImmediateX` instruction.
    pub x: bool,
}

/// Analyzes `file`, returning the semantic model and all semantic
/// diagnostics. Runs to completion even on errors so tools see everything.
pub fn analyze_file(file: &AstFile) -> (SemanticModel, Vec<Diagnostic>) {
    analyze(file, "", None)
}

/// Analyzes `file`, naming the unit so `address` placements can derive
/// stable segment names from it.
pub fn analyze_named(file: &AstFile, unit: &str) -> (SemanticModel, Vec<Diagnostic>) {
    analyze(file, unit, None)
}

/// Analyzes `file` as one unit of a workspace, so that names other units
/// define resolve instead of reporting themselves as undeclared.
pub fn analyze_unit(
    file: &AstFile,
    unit: &str,
    linkage: &Linkage,
) -> (SemanticModel, Vec<Diagnostic>) {
    analyze(file, unit, Some(linkage))
}

fn analyze(
    file: &AstFile,
    unit_name: &str,
    linkage: Option<&Linkage>,
) -> (SemanticModel, Vec<Diagnostic>) {
    let mut analysis = Analysis {
        model: SemanticModel::default(),
        diagnostics: Vec::new(),
        current_function: None,
        unit: unit_name.to_owned(),
        anonymous_blocks: AnonymousBlocks::default(),
        defaults: WidthContract::default(),
        externally_reachable: BTreeSet::new(),
        deferred_sections: BTreeMap::new(),
    };
    analysis.declare_placed_symbols(file);
    // Module defaults are file-wide, so they are known before any function's
    // contract is folded.
    analysis.defaults = analysis.module_defaults(file);
    analysis.declare_items(file);
    // Foreign names are declared *after* the unit's own, so a local
    // declaration always wins its name. A `const` or `var` initializer
    // therefore cannot fold a foreign name — which is right anyway: a
    // foreign symbol's address is ld65's, so it could never fold.
    if let Some(linkage) = linkage {
        analysis.declare_imports(linkage);
        analysis.collect_externally_reachable(linkage);
    }
    // The width dataflow runs before the checks that consume it, and after
    // imports so that a foreign callee resolves.
    analysis.solve_widths(file);
    analysis.check_functions(file);
    (analysis.model, analysis.diagnostics)
}

struct Analysis {
    model: SemanticModel,
    diagnostics: Vec<Diagnostic>,
    /// The function whose body is being walked, for local label scope.
    current_function: Option<SymbolId>,
    /// The unit's name, for the synthetic segment names of `address`.
    unit: String,
    /// Anonymous data blocks seen so far, per site.
    anonymous_blocks: AnonymousBlocks,
    /// The module's default widths, merged into every function that does not
    /// declare the axis itself.
    defaults: WidthContract,
    /// Global labels another unit refers to, which the width solver treats
    /// as reachable from a width nothing in this unit establishes.
    externally_reachable: BTreeSet<SymbolId>,
    /// Code sections the parser recognized but no sub-plan compiles yet, by
    /// name, so a call that names one says what is actually missing.
    deferred_sections: BTreeMap<String, (&'static str, compat::Owner)>,
}

impl Analysis {
    /// First pass: declares every symbol ld65 places — functions, labels, and
    /// data-block labels.
    ///
    /// These have no value to fold, so declaring them all before anything is
    /// checked is what makes forward references work: a `goto` may name a
    /// label further down its function, and an operand may name a data block
    /// declared at the end of the file.
    ///
    /// `const` and `var` are *not* declared here. Their values fold, and a
    /// fold reads other declarations, so they stay in source order — the
    /// same rule the documentation states for evaluator constants. Declaring
    /// them early with a placeholder value would let a forward reference
    /// silently read the placeholder.
    fn declare_placed_symbols(&mut self, file: &AstFile) {
        for item in &file.items {
            match &item.kind {
                ItemKind::Func(func) => {
                    if func.name.provenance == Provenance::Synthetic
                        || unsupported_function_feature(func).is_some()
                    {
                        continue;
                    }
                    let Some(symbol) = self.declare(
                        &func.name.text,
                        SymbolKind::Function {
                            size: AddressSize::Absolute,
                        },
                        func.name.span,
                    ) else {
                        continue;
                    };
                    self.declare_labels_in(symbol, &func.body);
                }
                ItemKind::Data(data) => self.declare_data_labels(data),
                ItemKind::Var(_)
                | ItemKind::Const(_)
                | ItemKind::Segment(_)
                | ItemKind::EvalBlock(_)
                | ItemKind::Preprocessor(_)
                | ItemKind::Binary(_)
                | ItemKind::Reserved(_)
                | ItemKind::ModuleWidth(_)
                | ItemKind::Error => {}
            }
        }
    }

    /// Declares the labels of one function body. A global label is visible
    /// across the unit; a `.name` label is scoped to `owner`, so two
    /// functions may use the same one.
    fn declare_labels_in(&mut self, owner: SymbolId, block: &crate::ast::Block) {
        for statement in &block.statements {
            match &statement.kind {
                StatementKind::Label(label) => {
                    if label.name.provenance == Provenance::Synthetic {
                        continue;
                    }
                    let kind = SymbolKind::Label {
                        scope: match label.scope {
                            crate::ast::LabelScope::Global => LabelScope::Global,
                            crate::ast::LabelScope::Local => LabelScope::Local,
                        },
                        size: AddressSize::Absolute,
                    };
                    match label.scope {
                        crate::ast::LabelScope::Global => {
                            self.declare(&label.name.text, kind, label.name.span);
                        }
                        crate::ast::LabelScope::Local => {
                            self.declare_local(owner, &label.name.text, kind, label.name.span);
                        }
                    }
                }
                // Labels nest wherever a block does; the scope stays the
                // enclosing function's either way.
                StatementKind::WidthBlock(width) => self.declare_labels_in(owner, &width.body),
                StatementKind::Nocross(body) => self.declare_labels_in(owner, body),
                StatementKind::Loop(loop_statement) => {
                    self.declare_labels_in(owner, &loop_statement.body);
                }
                StatementKind::Conditional(conditional) => {
                    self.declare_labels_in(owner, &conditional.body);
                    if let Some(otherwise) = &conditional.otherwise {
                        self.declare_labels_in(owner, otherwise);
                    }
                }
                StatementKind::Data(data) => self.declare_data_labels(data),
                StatementKind::Native(_)
                | StatementKind::Hla(_)
                | StatementKind::ModeSet(_)
                | StatementKind::Call(_)
                | StatementKind::BareCall(_)
                | StatementKind::ConditionalJump(_)
                | StatementKind::Goto(_)
                | StatementKind::Break
                | StatementKind::Repeat
                | StatementKind::Preprocessor(_)
                | StatementKind::Return(_)
                | StatementKind::Error => {}
            }
        }
    }

    /// Declares a data block's own name and every label inside it. Both are
    /// unit-level: a data label is exactly how one unit hands another the
    /// address of some bytes.
    fn declare_data_labels(&mut self, data: &DataBlock) {
        if let Some(name) = &data.name
            && name.provenance != Provenance::Synthetic
        {
            self.declare(
                &name.text,
                SymbolKind::DataLabel {
                    size: AddressSize::Absolute,
                },
                name.span,
            );
        }
        for entry in &data.entries {
            match &entry.kind {
                DataEntryKind::Label(name) if name.provenance != Provenance::Synthetic => {
                    self.declare(
                        &name.text,
                        SymbolKind::DataLabel {
                            size: AddressSize::Absolute,
                        },
                        name.span,
                    );
                }
                DataEntryKind::Repeat { body, .. } => self.declare_data_labels(body),
                _ => {}
            }
        }
    }

    /// Second pass: declares the symbols whose value has to be folded, and
    /// records what the rest of analysis needs about each item.
    fn declare_items(&mut self, file: &AstFile) {
        for (item_index, item) in file.items.iter().enumerate() {
            match &item.kind {
                ItemKind::Var(var) => {
                    if var.name.provenance == Provenance::Synthetic {
                        continue; // Recovery invented the name; syntax error already reported.
                    }
                    // Only the fixed-address form has semantics so far; every
                    // other axis of the declaration says who owns it.
                    if let Some(feature) = unsupported_var_feature(var) {
                        self.diagnostics.push(compat::unimplemented(
                            item.span,
                            feature,
                            compat::Owner::HlaLowering,
                        ));
                        continue;
                    }
                    let Some(address) = var
                        .address
                        .as_ref()
                        .and_then(|address| self.constant(address, "variable addresses"))
                    else {
                        continue;
                    };
                    // A declaration-position prefix sets the default
                    // encoding for plain references; without one the value's
                    // own magnitude decides (`doc/syntax-reference.md`,
                    // "Address-encoding (prefixes)").
                    let size = var.storage.map_or_else(
                        || AddressSize::of_value(address),
                        |storage| address_size_of(storage.kind),
                    );
                    self.declare(
                        &var.name.text,
                        SymbolKind::Var {
                            address: Some(address),
                            size,
                        },
                        var.name.span,
                    );
                }
                ItemKind::Const(declaration) => self.declare_const_group(declaration),
                ItemKind::Data(data) => {
                    let site = self.data_site(data, None);
                    let mut resolver = self.resolver();
                    resolver.data_block(data, item.span, &site);
                    let resolved = resolver.finish();
                    self.absorb(resolved);
                    self.report_reserved_in_data(data);
                }
                ItemKind::EvalBlock(expression) => {
                    self.diagnostics.push(compat::unimplemented(
                        item.span,
                        "top-level evaluator blocks",
                        compat::Owner::Compatibility,
                    ));
                    self.report_reserved_in_expr(expression);
                }
                ItemKind::Preprocessor(_) => self.diagnostics.push(compat::unimplemented(
                    item.span,
                    "preprocessor directives",
                    compat::Owner::Compatibility,
                )),
                ItemKind::Binary(_) => self.diagnostics.push(compat::unimplemented(
                    item.span,
                    "`binary` inclusions",
                    compat::Owner::Compatibility,
                )),
                // Reserved legacy syntax reports itself through the
                // `K816-K65-####` family, not as unimplemented K816 work.
                ItemKind::Reserved(reserved) => {
                    let diagnostic = reserved_diagnostic(reserved);
                    self.diagnostics.push(diagnostic);
                }
                // Module defaults were folded before this pass; their
                // placement and conflicts were reported there.
                ItemKind::ModuleWidth(_) => {}
                // A `segment` selection is lexical state, carried to
                // emission as an ordered operation rather than checked here.
                ItemKind::Segment(_) => {}
                ItemKind::Func(func) => {
                    if func.name.provenance == Provenance::Synthetic {
                        continue;
                    }
                    // Only a plain `func` (or the legacy bare `main`) with no
                    // call contract has semantics so far.
                    if let Some((feature, owner)) = unsupported_function_feature(func) {
                        self.diagnostics
                            .push(compat::unimplemented(item.span, feature, owner));
                        // Remembered so that a call naming it says which
                        // section kind is missing rather than "undeclared".
                        self.deferred_sections
                            .insert(func.name.text.clone(), (feature, owner));
                        continue;
                    }
                    // The first pass declared the name; a duplicate reported
                    // itself there, and this pass records what lowering and
                    // emission need about the function it kept.
                    let Some(symbol) = self.model.symbols.lookup(&func.name.text) else {
                        continue;
                    };
                    if self.model.symbols.symbol(symbol).declaration != func.name.span {
                        continue; // Another declaration won this name.
                    }
                    let declared = width::fold_annotations(&func.width, &mut self.diagnostics);
                    self.model.functions.push(FunctionInfo {
                        symbol,
                        item_index,
                        contract: width::merge_defaults(self.defaults, declared),
                        is_entry_point: func.name.text == "main",
                        // Nothing derives an exit yet, so every function
                        // takes the documented fallback and says so.
                        exit: ExitContract::preserving(),
                        exit_is_assumed: true,
                        needs: WidthNeeds::default(),
                        callees: Vec::new(),
                    });
                }
                ItemKind::Error => {}
            }
        }
    }

    fn declare(&mut self, name: &str, kind: SymbolKind, span: Span) -> Option<SymbolId> {
        let result = self.model.symbols.declare(name, kind, span);
        self.declared(name, span, result)
    }

    fn declare_local(
        &mut self,
        owner: SymbolId,
        name: &str,
        kind: SymbolKind,
        span: Span,
    ) -> Option<SymbolId> {
        let result = self.model.symbols.declare_local(owner, name, kind, span);
        self.declared(name, span, result)
    }

    /// Reports a duplicate declaration, pointing the "first declared here"
    /// label at whichever declaration actually comes first in the source.
    ///
    /// Declaration runs in two passes, so the surviving symbol is not always
    /// the earlier one — a function declared in the first pass wins a name a
    /// `var` on an earlier line asked for. Ordering by span keeps the
    /// diagnostic honest about which line the reader should look at first.
    fn declared(
        &mut self,
        name: &str,
        span: Span,
        result: Result<SymbolId, SymbolId>,
    ) -> Option<SymbolId> {
        let existing = match result {
            Ok(id) => return Some(id),
            Err(existing) => existing,
        };
        let previous = self.model.symbols.symbol(existing).declaration;
        let (first, again) = if previous.start <= span.start {
            (previous, span)
        } else {
            (span, previous)
        };
        self.diagnostics.push(
            Diagnostic::error(
                DiagnosticCode::DUPLICATE_SYMBOL,
                format!("`{name}` is declared more than once"),
                Label::new(again, "redeclared here"),
            )
            .with_secondary(Label::new(first, "first declared here"))
            .with_help("rename one of the declarations"),
        );
        None
    }

    /// Folds the file's module-level default widths.
    ///
    /// A module default is a file-wide statement, so it must precede every
    /// code section: a positional reading would make the same annotation mean
    /// different things depending on where an unrelated function was inserted.
    fn module_defaults(&mut self, file: &AstFile) -> WidthContract {
        let mut annotations = Vec::new();
        let mut first_section: Option<Span> = None;
        for item in &file.items {
            match &item.kind {
                ItemKind::Func(_) => {
                    first_section.get_or_insert(item.span);
                }
                ItemKind::ModuleWidth(annotation) => match first_section {
                    Some(section) => self.diagnostics.push(late_module_width(
                        item.span,
                        annotation.span,
                        section,
                    )),
                    None => annotations.push(annotation.clone()),
                },
                _ => {}
            }
        }
        width::fold_annotations(&annotations, &mut self.diagnostics)
    }

    /// Records which of this unit's global labels another unit refers to.
    ///
    /// Such a label may be branched to from code K816 is not looking at, so
    /// the width solver cannot claim anything about the widths that reach it.
    /// A global label nobody else names is an ordinary merge point.
    fn collect_externally_reachable(&mut self, linkage: &Linkage) {
        for (id, symbol) in self.model.symbols.iter() {
            if symbol.imported
                || !matches!(
                    symbol.kind,
                    SymbolKind::Label {
                        scope: LabelScope::Global,
                        ..
                    }
                )
            {
                continue;
            }
            if linkage.referenced_elsewhere(&symbol.name, symbol.declaration.file) {
                self.externally_reachable.insert(id);
            }
        }
    }

    /// Runs the width dataflow over every function, and records what it found.
    ///
    /// Every call takes the language's `Unknown ⇒ Preserve` rule here, which
    /// is sound precisely because nothing yet reads a callee's exit or emits a
    /// bridge: the placeholder and the emitted code agree. Exit inference
    /// replaces this analysis wholesale rather than patching it, so no stage
    /// ever mixes a resolved exit with a state computed from the placeholder.
    fn solve_widths(&mut self, file: &AstFile) {
        // Summarize: one solver run per function with every call taking the
        // language's fallback. It yields `needs` and the call edges, and
        // nothing else — its per-statement states are provisional and its
        // diagnostics are dropped, because the run that classifies each
        // function reports them for real.
        let fallback: BTreeMap<SymbolId, width::CalleeContract> = BTreeMap::new();
        for index in 0..self.model.functions.len() {
            let info = self.model.functions[index].clone();
            let Some(func) = function_of(file, &info) else {
                continue;
            };
            let (analysis, _) = width::analyze_function(
                &func.body,
                ModeState::from_contract(info.contract),
                &width::Context {
                    symbols: &self.model.symbols,
                    function: Some(info.symbol),
                    callees: &fallback,
                    externally_reachable: &self.externally_reachable,
                },
            );
            let function = &mut self.model.functions[index];
            function.needs = analysis.needs;
            function.callees.clone_from(&analysis.callees);
        }
        self.classify_exits(file);
    }

    /// Classifies every function's exit, in an order that has each function's
    /// callees final before it is reached.
    ///
    /// The graph is condensed into strongly connected components and
    /// processed in reverse topological order rather than iterated to a
    /// fixpoint. `AxisExit` has no order that would make an
    /// iterate-until-stable scheme monotone — `Fixed` and `Preserve` are
    /// incomparable claims and `Unknown` is not a sound top — so an iterating
    /// implementation would converge by luck on the inputs it was tested on.
    /// One pass, no iteration count, no cap.
    ///
    /// Classifying a function *re-runs* the solver with its callees' final
    /// exits, and that run's analysis is the one stored. Reading a stale
    /// analysis instead would let call-site bridging use a resolved exit
    /// while the immediates after the call were sized from the placeholder —
    /// two answers again, arriving by staging rather than by duplication.
    fn classify_exits(&mut self, file: &AstFile) {
        let mut callees: BTreeMap<SymbolId, width::CalleeContract> = BTreeMap::new();
        for component in strongly_connected_components(&self.model.functions) {
            // A non-trivial component, or a self-loop, is recursion: there is
            // no fixpoint to find and no answer better than the fallback.
            let recursive = component.len() > 1 || {
                let index = component[0];
                let symbol = self.model.functions[index].symbol;
                self.model.functions[index].callees.contains(&symbol)
            };
            for index in component {
                let info = self.model.functions[index].clone();
                let Some(func) = function_of(file, &info) else {
                    continue;
                };
                let (analysis, diagnostics) = width::analyze_function(
                    &func.body,
                    ModeState::from_contract(info.contract),
                    &width::Context {
                        symbols: &self.model.symbols,
                        function: Some(info.symbol),
                        callees: &callees,
                        externally_reachable: &self.externally_reachable,
                    },
                );
                self.diagnostics.extend(diagnostics);

                let classified = width::classify_exit(&analysis);
                for divergence in &classified.divergent {
                    self.diagnostics
                        .push(divergent_exit(&func.name.text, *divergence));
                }
                let (exit, assumed) = if recursive {
                    (ExitContract::preserving(), true)
                } else {
                    (classified.exit, classified.assumed)
                };
                if assumed {
                    // An axis whose divergence was already reported needs no
                    // second diagnostic: the hard error at the callee is the
                    // whole story, and the fallback is what follows from it.
                    self.report_assumed_exit(
                        func,
                        &analysis,
                        &exit,
                        recursive,
                        &classified.divergent,
                    );
                }
                self.check_declared_exit(func, &analysis);

                callees.insert(
                    info.symbol,
                    width::CalleeContract {
                        entry: info.contract,
                        exit,
                    },
                );
                let function = &mut self.model.functions[index];
                function.exit = exit;
                function.exit_is_assumed = assumed;
                self.model.widths.insert(info.symbol, analysis);
            }
        }
    }

    /// Warns where K816 read the body, can see it may change width, and
    /// cannot say to what.
    ///
    /// Only for bodies K816 has actually read: a callee it cannot see gets a
    /// call-site annotation instead, because there is nothing at the callee
    /// to point at and nothing the author of *this* unit could do about it.
    /// Keeping the warning to visible bodies is what makes it worth reading.
    fn report_assumed_exit(
        &mut self,
        func: &Function,
        analysis: &WidthAnalysis,
        exit: &ExitContract,
        recursive: bool,
        divergent: &[width::Divergence],
    ) {
        let name = &func.name.text;
        if recursive {
            self.diagnostics.push(
                Diagnostic::warning(
                    DiagnosticCode::ASSUMED_EXIT,
                    format!("`{name}` is part of a recursion cycle, so its exit widths cannot be derived"),
                    Label::new(func.name.span, "callers will assume it preserves their widths"),
                )
                .with_note(
                    "a cycle has no exit contract to compute: every member's answer \
                     depends on every other's",
                )
                .with_help("declare the exit widths with `-> @a8` or re-establish them before returning")
                .with_docs("doc/register-width-aware-syntax.md#unknown-exit-contracts-preserve-caller-mode"),
            );
            return;
        }
        for axis in [width::Axis::Accumulator, width::Axis::Index] {
            let unknown = match axis {
                width::Axis::Accumulator => exit.a == crate::op::AxisExit::Unknown,
                width::Axis::Index => exit.i == crate::op::AxisExit::Unknown,
            };
            if !unknown || divergent.iter().any(|divergence| divergence.axis == axis) {
                continue;
            }
            let evidence = width::unclassifiable_return(analysis, axis).unwrap_or(func.name.span);
            self.diagnostics.push(
                Diagnostic::warning(
                    DiagnosticCode::ASSUMED_EXIT,
                    format!(
                        "`{name}` may change the {} width, and K816 cannot say to what",
                        axis.name()
                    ),
                    Label::new(evidence, "this return leaves no width a caller can rely on"),
                )
                .with_secondary(Label::new(func.name.span, "the function it returns from"))
                .with_note(
                    "the language requires callers to assume such a function preserves \
                     their widths, which this body may not do",
                )
                .with_help(format!(
                    "declare the exit width with `-> {}`, or re-establish it before returning",
                    axis.annotation(RegisterWidth::W8)
                ))
                .with_docs("doc/register-width-aware-syntax.md#unknown-exit-contracts-preserve-caller-mode"),
            );
        }
    }

    /// Checks a declared `-> @aN` against the widths the body actually
    /// returns at. The declaration is check-only: it never establishes a
    /// width, so a mismatch is the body's to fix.
    fn check_declared_exit(&mut self, func: &Function, analysis: &WidthAnalysis) {
        let Some(contract) = &func.contract else {
            return;
        };
        let Some(outputs) = &contract.outputs else {
            return;
        };
        let declared = width::fold_annotations(&outputs.width, &mut self.diagnostics);
        for (axis, width) in [
            (width::Axis::Accumulator, declared.a),
            (width::Axis::Index, declared.i),
        ] {
            let Some(width) = width else { continue };
            let Some(site) = analysis.returns.iter().find(|site| {
                let state = match axis {
                    width::Axis::Accumulator => site.state.a,
                    width::Axis::Index => site.state.i,
                };
                state != crate::op::AxisState::Known(width)
            }) else {
                continue;
            };
            self.diagnostics.push(
                Diagnostic::error(
                    DiagnosticCode::EXIT_WIDTH_MISMATCH,
                    format!(
                        "`{}` declares it exits at {} but a reachable return does not",
                        func.name.text,
                        axis.annotation(width)
                    ),
                    Label::new(site.span, "this return leaves a different width"),
                )
                .with_secondary(Label::new(outputs.span, "the declared exit width"))
                .with_note(
                    "an exit-width clause is checked, never established: it states what \
                     the body already does",
                )
                .with_docs("doc/syntax-reference.md#code-sections"),
            );
        }
    }

    fn check_functions(&mut self, file: &AstFile) {
        for info in 0..self.model.functions.len() {
            // Cloned to end the borrow of `self.model` before the body walk,
            // which the dropped `Copy` derive used to do for free.
            let function = self.model.functions[info].clone();
            let ItemKind::Func(func) = &file.items[function.item_index].kind else {
                continue;
            };
            self.current_function = Some(function.symbol);
            self.check_block(function.symbol, &func.body);
            self.current_function = None;
        }
    }

    /// Checks one block's statements, and the blocks nested in them.
    fn check_block(&mut self, function: SymbolId, block: &crate::ast::Block) {
        for statement in &block.statements {
            let widths = self.widths_at(function, statement);
            self.check_statement(statement, widths);
            // A scoped width block's body is ordinary code at the block's
            // width, so it is checked with the state the solver tracks inside
            // it rather than skipped along with the statement that opens it.
            if let StatementKind::WidthBlock(width) = &statement.kind {
                self.check_block(function, &width.body);
            }
        }
    }

    /// The width state the model tracks before `statement`.
    ///
    /// A statement the analysis does not describe is a compiler bug, not a
    /// case to fall back from: substituting the entry contract there is
    /// exactly how one question comes to have two answers.
    fn widths_at(&mut self, function: SymbolId, statement: &Statement) -> ModeState {
        match self
            .model
            .width(function)
            .and_then(|analysis| analysis.before(statement.id))
        {
            Some(state) => state,
            None => {
                self.diagnostics.push(Diagnostic::error(
                    DiagnosticCode::INTERNAL,
                    "internal: the width model has no state for this statement",
                    Label::new(statement.span, "no tracked width here"),
                ));
                ModeState::unknown()
            }
        }
    }

    fn check_statement(&mut self, statement: &Statement, widths: ModeState) {
        match &statement.kind {
            StatementKind::Native(native) => {
                // Classification is shared with lowering, so the two cannot
                // disagree about which addressing mode an operand selects.
                let mut resolver = self.resolver().with_widths(widths);
                let classified = resolver.classify(native);
                let resolved = resolver.finish();
                self.absorb(resolved);
                if let Some(classified) = classified {
                    self.check_addressable(native, &classified);
                }
            }
            StatementKind::Hla(HlaStatement::Assign(assign)) => {
                match (assign.targets.as_slice(), &assign.source.kind) {
                    // `a = #imm` sizes like the native `lda #imm`.
                    ([target], HlaOperandKind::Immediate(value))
                        if is_accumulator(&target.kind) =>
                    {
                        self.check_immediate(
                            value,
                            Some(ImmediateWidthClass::MDependent),
                            widths,
                            Mnemonic::Lda,
                        );
                    }
                    // `MEM = a` is the store the slice compiles.
                    ([target], HlaOperandKind::Register(register))
                        if register.kind == RegisterKind::A =>
                    {
                        match plain_symbol(&target.kind) {
                            Some(name) if name.provenance != Provenance::Synthetic => {
                                self.check_memory_operand(&name.text, name.span);
                            }
                            Some(_) => {}
                            None => self.diagnostics.push(compat::unimplemented(
                                statement.span,
                                "these assignment targets",
                                compat::Owner::HlaLowering,
                            )),
                        }
                    }
                    _ => self.diagnostics.push(compat::unimplemented(
                        statement.span,
                        "these assignment chains",
                        compat::Owner::HlaLowering,
                    )),
                }
            }
            // Everything else in the HLA statement family is recognized but
            // not lowered yet.
            StatementKind::Hla(
                HlaStatement::Exchange(_)
                | HlaStatement::Arithmetic(_)
                | HlaStatement::Shift(_)
                | HlaStatement::Step(_)
                | HlaStatement::Flag(_)
                | HlaStatement::Stack(_)
                | HlaStatement::Nop { .. }
                | HlaStatement::Break,
            ) => {
                self.diagnostics.push(compat::unimplemented(
                    statement.span,
                    "these HLA shorthands",
                    compat::Owner::HlaLowering,
                ));
            }
            // Both label scopes were declared by the first pass, so a label
            // statement has nothing left to check here.
            StatementKind::Label(_) => {}
            // `return` is the slice's; `return_i` and everything else in the
            // section family belongs to later work.
            StatementKind::Return(ReturnKind::Subroutine) | StatementKind::Error => {}
            StatementKind::Return(ReturnKind::Interrupt) => {
                self.diagnostics.push(compat::unimplemented(
                    statement.span,
                    "`return_i` statements",
                    compat::Owner::HlaLowering,
                ));
            }
            // Both establish widths, which the solver has already tracked;
            // the body of a block is checked by the walk that got here.
            StatementKind::WidthBlock(_) | StatementKind::ModeSet(_) => {}
            // Branches, loops, and jumps are recognized but not lowered yet.
            StatementKind::Conditional(_)
            | StatementKind::ConditionalJump(_)
            | StatementKind::Loop(_)
            | StatementKind::Goto(_)
            | StatementKind::Break
            | StatementKind::Repeat
            | StatementKind::Nocross(_) => self.diagnostics.push(compat::unimplemented(
                statement.span,
                "branch and loop statements",
                compat::Owner::HlaLowering,
            )),
            StatementKind::Data(data) => {
                let function = self
                    .current_function
                    .map(|id| self.model.symbols.name(id).to_owned());
                let site = self.data_site(data, function.as_deref());
                let mut resolver = self.resolver();
                resolver.data_block(data, statement.span, &site);
                let resolved = resolver.finish();
                self.absorb(resolved);
                self.report_reserved_in_data(data);
            }
            StatementKind::Preprocessor(_) => self.diagnostics.push(compat::unimplemented(
                statement.span,
                "preprocessor directives",
                compat::Owner::Compatibility,
            )),
            StatementKind::Call(call) => {
                if call.is_far {
                    self.diagnostics.push(compat::unimplemented(
                        statement.span,
                        "far calls",
                        compat::Owner::HlaLowering,
                    ));
                    return;
                }
                self.check_call_target(&call.target);
            }
            StatementKind::BareCall(call) => {
                // A flow clause makes the call *checked*, which is contract
                // validation rather than bridging.
                if call.arguments.is_some() || call.outputs.is_some() {
                    self.diagnostics.push(compat::unimplemented(
                        statement.span,
                        "call contracts",
                        compat::Owner::WidthAndContracts,
                    ));
                    return;
                }
                self.check_call_target(&call.target);
            }
        }
    }

    /// Validates an immediate operand against its width class and the width
    /// the model tracks *here*.
    fn check_immediate(
        &mut self,
        value: &Expr,
        width: Option<ImmediateWidthClass>,
        widths: ModeState,
        mnemonic: Mnemonic,
    ) {
        let Some(folded) = self.fold(value) else {
            return;
        };
        let Some(number) = folded.value().map(|value| value.get()) else {
            // A link-time immediate such as `#&&label` is legal — ca65
            // relocates it — and the native path carries one through
            // classification. This is the HLA slice's own immediate check;
            // it goes away when HLA operands lower through the shared
            // classifier, which is sub-plan E's work, not C's.
            self.diagnostics.push(compat::unimplemented(
                value.span,
                "link-time values in HLA immediate operands",
                compat::Owner::HlaLowering,
            ));
            return;
        };
        let Some(width) = width else {
            return;
        };
        let Some(limit) = width.max_value(widths.a.width(), widths.i.width()) else {
            // Only the width-dependent classes can fail to report a limit,
            // and they fail exactly when their axis is unconstrained.
            let (axis, help) = match width {
                ImmediateWidthClass::MDependent => {
                    ("accumulator", "add @a8 or @a16 to the enclosing function")
                }
                ImmediateWidthClass::XDependent => (
                    "index register",
                    "add @i8 or @i16 to the enclosing function",
                ),
                ImmediateWidthClass::Fixed8 | ImmediateWidthClass::Fixed16 => {
                    unreachable!("fixed immediate widths always report a limit")
                }
            };
            self.diagnostics.push(
                Diagnostic::error(
                    DiagnosticCode::UNKNOWN_WIDTH,
                    format!(
                        "`{}` uses a width-dependent immediate but {axis} width is unknown",
                        mnemonic.ca65()
                    ),
                    Label::new(value.span, "cannot size this immediate"),
                )
                .with_help(help)
                .with_docs("doc/register-width-aware-syntax.md"),
            );
            return;
        };
        if number > limit {
            self.diagnostics.push(
                Diagnostic::error(
                    DiagnosticCode::IMMEDIATE_RANGE,
                    format!("immediate ${number:X} does not fit in the selected width"),
                    Label::new(value.span, format!("maximum here is ${limit:X}")),
                )
                .with_docs("doc/register-width-aware-syntax.md"),
            );
        }
    }

    /// Reports every reserved legacy construct nested inside a data block.
    ///
    /// A reserved construct keeps its own `K816-K65-####` diagnostic wherever
    /// it appears, not only when it *is* the whole item: an `image` entry and
    /// a `size(...)` call inside an expression are as reserved as a top-level
    /// `image NAME = "file"`, and reporting only the enclosing block as
    /// not-yet-implemented would hide which construct the compiler refuses.
    fn report_reserved_in_data(&mut self, data: &DataBlock) {
        for entry in &data.entries {
            match &entry.kind {
                DataEntryKind::Reserved(reserved) => {
                    let diagnostic = reserved_diagnostic(reserved);
                    self.diagnostics.push(diagnostic);
                }
                DataEntryKind::Align(expr) | DataEntryKind::Address(expr) => {
                    self.report_reserved_in_expr(expr)
                }
                DataEntryKind::Typed { values, .. } => {
                    for value in values {
                        if let DataValue::Expr(expr) = value {
                            self.report_reserved_in_expr(expr);
                            self.check_data_atom(expr);
                        }
                    }
                }
                DataEntryKind::Value(DataValue::Expr(expr)) => {
                    self.report_reserved_in_expr(expr);
                    self.check_data_atom(expr);
                }
                DataEntryKind::Repeat { count, body } => {
                    self.report_reserved_in_expr(count);
                    self.report_reserved_in_data(body);
                }
                DataEntryKind::ForEval { from, to, body, .. } => {
                    self.report_reserved_in_expr(from);
                    self.report_reserved_in_expr(to);
                    self.report_reserved_in_expr(body);
                }
                DataEntryKind::Code { body, .. } => {
                    for statement in &body.statements {
                        if let StatementKind::Data(nested) = &statement.kind {
                            self.report_reserved_in_data(nested);
                        }
                    }
                }
                DataEntryKind::Segment(_)
                | DataEntryKind::Nocross
                | DataEntryKind::Charset(_)
                | DataEntryKind::Label(_)
                | DataEntryKind::Value(_)
                | DataEntryKind::Preprocessor(_)
                | DataEntryKind::Binary(_)
                | DataEntryKind::Error => {}
            }
        }
    }

    /// Enforces the atom-only rule for data-block value lists.
    ///
    /// Each value must be a single atom: a literal, an identifier, an
    /// address-byte operator over a symbol with at most `+`/`-` offset
    /// arithmetic, or `?` (`doc/syntax-reference.md`, "Where compound
    /// expressions are accepted"). The grammar already refuses most compound
    /// shapes; parentheses are the escape hatch it cannot close, because
    /// `&<(A | B)` is a legal *parse* of an illegal value. A compound
    /// expression belongs in a `const` that pre-folds it.
    fn check_data_atom(&mut self, expr: &Expr) {
        let Some(compound) = first_compound(expr) else {
            return;
        };
        self.diagnostics.push(
            Diagnostic::error(
                DiagnosticCode::COMPOUND_DATA_VALUE,
                "data value lists take single atoms, not compound expressions",
                Label::new(compound, "this operator is not allowed in a value list"),
            )
            .with_note(
                "a value list separates entries by whitespace, so an operator between \
                 two of them would silently merge them into one",
            )
            .with_help("declare a `const` that folds the expression and list the `const`")
            .with_docs("doc/syntax-reference.md#where-compound-expressions-are-accepted"),
        );
    }

    /// Reports reserved evaluator calls anywhere inside `expr`.
    fn report_reserved_in_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Eval(inner) => self.report_reserved_in_eval(inner),
            ExprKind::Unary(unary) => self.report_reserved_in_expr(&unary.operand),
            ExprKind::Binary(binary) => {
                self.report_reserved_in_expr(&binary.left);
                self.report_reserved_in_expr(&binary.right);
            }
            ExprKind::Field(access) => self.report_reserved_in_expr(&access.base),
            ExprKind::Subscript(subscript) => {
                self.report_reserved_in_expr(&subscript.base);
                self.report_reserved_in_expr(&subscript.index);
            }
            ExprKind::Metadata(query) => self.report_reserved_in_expr(&query.base),
            ExprKind::TypedView(view) => self.report_reserved_in_expr(&view.base),
            ExprKind::Number(_)
            | ExprKind::Symbol(_)
            | ExprKind::LocalSymbol(_)
            | ExprKind::Error => {}
        }
    }

    /// `[ 1 + size(section) ]` is arithmetic around a reserved call and
    /// `[ max(size(a), 1) ]` nests one, so the whole evaluator tree is walked.
    fn report_reserved_in_eval(&mut self, expr: &EvalExpr) {
        match &expr.kind {
            EvalExprKind::ReservedCall(call) => {
                let diagnostic = compat::k65_reserved(
                    DiagnosticCode::K65_RESERVED_EVAL_FUNCTION,
                    call.keyword_span,
                    eval_call_text(call.name),
                );
                self.diagnostics.push(diagnostic);
                for argument in &call.arguments {
                    self.report_reserved_in_eval(argument);
                }
            }
            EvalExprKind::Call { arguments, .. } => {
                for argument in arguments {
                    self.report_reserved_in_eval(argument);
                }
            }
            EvalExprKind::Unary { operand, .. } => self.report_reserved_in_eval(operand),
            EvalExprKind::Binary { left, right, .. } => {
                self.report_reserved_in_eval(left);
                self.report_reserved_in_eval(right);
            }
            EvalExprKind::Ternary {
                condition,
                consequence,
                alternative,
            } => {
                self.report_reserved_in_eval(condition);
                self.report_reserved_in_eval(consequence);
                self.report_reserved_in_eval(alternative);
            }
            EvalExprKind::Assignment { target, value, .. } => {
                self.report_reserved_in_eval(target);
                self.report_reserved_in_eval(value);
            }
            EvalExprKind::Subscript { base, index } => {
                self.report_reserved_in_eval(base);
                self.report_reserved_in_eval(index);
            }
            EvalExprKind::Member { base, .. } => self.report_reserved_in_eval(base),
            EvalExprKind::List(items) => {
                for item in items {
                    self.report_reserved_in_eval(item);
                }
            }
            EvalExprKind::Number(_)
            | EvalExprKind::Float(_)
            | EvalExprKind::String(_)
            | EvalExprKind::Symbol(_)
            | EvalExprKind::Error => {}
        }
    }

    /// Declares every workspace name this unit does not define itself.
    ///
    /// The kind and address size are the *defining* unit's, because that is
    /// what ca65 needs to validate an addressing mode against the imported
    /// symbol. Declaring a name does not import it: only the ones this unit
    /// goes on to reference reach the emitted `.import` list.
    fn declare_imports(&mut self, linkage: &Linkage) {
        for (name, definition) in linkage.definitions() {
            self.model
                .symbols
                .declare_imported(name, definition.kind, definition.declaration);
        }
    }

    /// The site of one data block, which its synthetic `address` segment
    /// names derive from.
    ///
    /// Analysis and lowering both walk the unit in source order and count
    /// anonymous blocks the same way per site, so both compute the same
    /// names — which is what an ld65 config entry needs of them.
    fn data_site(&mut self, data: &DataBlock, function: Option<&str>) -> DataSite {
        let unit = self.unit.clone();
        data_site(&mut self.anonymous_blocks, &unit, data, function)
    }

    /// A resolver over this unit's symbols, scoped to the function being
    /// checked. Analysis owns the diagnostics it produces.
    fn resolver(&self) -> Resolver<'_> {
        Resolver::new(&self.model.symbols).in_function(self.current_function)
    }

    /// Records what a resolver run found into the model this analysis owns.
    fn absorb(&mut self, resolved: Resolved) {
        self.model.referenced.extend(resolved.referenced);
        self.diagnostics.extend(resolved.diagnostics);
    }

    /// Folds `expr`, reporting whatever the fold turns up.
    fn fold(&mut self, expr: &Expr) -> Option<KExpr> {
        let mut resolver = self.resolver();
        let folded = resolver.fold(expr);
        let resolved = resolver.finish();
        self.absorb(resolved);
        folded
    }

    /// The absolute value of `expr` in a position that requires one.
    fn constant(&mut self, expr: &Expr, position: &str) -> Option<u32> {
        let mut resolver = self.resolver();
        let value = resolver.constant(expr, position);
        let resolved = resolver.finish();
        self.absorb(resolved);
        value
    }

    /// Declares one `const` group, applying the enum-like auto-increment.
    ///
    /// An entry without a value continues the previous one; a group that
    /// starts without one starts at zero. A *standalone* declaration with no
    /// initializer is an error rather than an implicit zero, because the
    /// shorthand only exists to number a comma-separated group
    /// (`doc/syntax-reference.md`, "Constant Declaration").
    fn declare_const_group(&mut self, declaration: &crate::ast::ConstDeclaration) {
        let standalone = declaration.entries.len() == 1;
        // `None` after an entry whose expression did not fold: the sequence
        // is broken, and guessing a successor would invent a value.
        let mut next = Some(0u32);
        for entry in &declaration.entries {
            let value = match &entry.value {
                Some(expression) => self.constant(expression, "`const` values"),
                None if standalone => {
                    self.diagnostics.push(
                        Diagnostic::error(
                            DiagnosticCode::NOT_CONSTANT,
                            format!("`const {}` has no value", entry.name.text),
                            Label::new(entry.span, "a standalone `const` needs an initializer"),
                        )
                        .with_note(
                            "the auto-increment shorthand only applies inside a \
                             comma-separated group",
                        )
                        .with_fix(Fix {
                            title: "give the constant a value".to_owned(),
                            applicability: Applicability::MaybeIncorrect,
                            edits: vec![TextEdit::replace(
                                Span::empty_at(entry.span.file, entry.span.end),
                                " = 0",
                            )],
                        })
                        .with_docs("doc/syntax-reference.md#constant-declaration"),
                    );
                    None
                }
                None => next,
            };
            next = value.map(|value| value.wrapping_add(1));
            if entry.name.provenance == Provenance::Synthetic {
                continue; // Recovery invented the name; a syntax error covers it.
            }
            if let Some(value) = value {
                self.declare(
                    &entry.name.text,
                    SymbolKind::Const { value },
                    entry.name.span,
                );
            }
        }
    }

    /// Rejects a symbol used in a role its declaration does not allow.
    ///
    /// Classification already checked the *shape* of an operand against the
    /// ISA table; this is the other axis — whether the name in it denotes
    /// something an operand may address at all. A `const` in address
    /// position is its own diagnostic, reported during classification.
    fn check_addressable(
        &mut self,
        native: &crate::ast::NativeInstruction,
        classified: &Classified,
    ) {
        let Some(operand) = &classified.operand else {
            return;
        };
        // A jump, call, or branch target names code, so a function is
        // exactly what belongs there. Only an operand the instruction reads
        // or writes as data has to name storage.
        if classified.instruction().effects().control != ControlEffect::FallThrough {
            return;
        }
        let expr = match operand {
            crate::op::Operand::Address { expr, .. }
            | crate::op::Operand::Indirect { expr, .. }
            | crate::op::Operand::StackRelative(expr) => expr,
            // An immediate, a branch target, and a block move's bank bytes
            // name values rather than storage.
            crate::op::Operand::Immediate(_)
            | crate::op::Operand::Label(_)
            | crate::op::Operand::BlockMove { .. } => return,
        };
        let mut referenced = Vec::new();
        expr.symbols(&mut referenced);
        for id in referenced {
            let symbol = self.model.symbols.symbol(id);
            if symbol.kind.is_addressable() {
                continue;
            }
            let name = symbol.name.clone();
            let declaration = symbol.declaration;
            self.diagnostics.push(
                Diagnostic::error(
                    DiagnosticCode::SYMBOL_KIND_MISMATCH,
                    format!("`{name}` is a function, not a variable"),
                    Label::new(native.mnemonic.span, "cannot be used as a memory operand"),
                )
                .with_secondary(Label::new(declaration, "declared here"))
                .with_help(format!(
                    "call it with `call {name}`, or take its address with `&&{name}`"
                )),
            );
        }
    }

    /// Validates the name a call transfers to.
    ///
    /// A code section K816 recognizes but does not compile yet was never
    /// declared, so its name would otherwise report itself as undeclared —
    /// true, and useless. Naming the section kind instead points at the work
    /// that is actually missing.
    fn check_call_target(&mut self, target: &crate::ast::Name) {
        if target.provenance == Provenance::Synthetic {
            return;
        }
        if let Some((feature, owner)) = self.deferred_sections.get(&target.text).copied() {
            self.diagnostics
                .push(compat::unimplemented(target.span, feature, owner));
            return;
        }
        let Some(id) = self.model.symbols.lookup(&target.text) else {
            let mut resolver = self.resolver();
            resolver.resolve(&target.text, target.span);
            let resolved = resolver.finish();
            self.absorb(resolved);
            return;
        };
        self.model.referenced.insert(id);
        let symbol = self.model.symbols.symbol(id);
        // A call transfers to an *address*, and a K65 program routinely
        // names one that is not a section of its own: `var openmode = $EF9C`
        // and `call openmode` is how the legacy corpus reaches a ROM routine.
        // Only a `const` has no address to transfer to.
        if !matches!(symbol.kind, SymbolKind::Const { .. }) {
            return;
        }
        let declaration = symbol.declaration;
        self.diagnostics.push(
            Diagnostic::error(
                DiagnosticCode::SYMBOL_KIND_MISMATCH,
                format!("`{}` is a `const`, not a place to call", target.text),
                Label::new(target.span, "a call transfers to an address"),
            )
            .with_secondary(Label::new(declaration, "declared here"))
            .with_help(format!(
                "declare it as an address instead, e.g. `var {} = ...`",
                target.text
            ))
            .with_docs("doc/syntax-reference.md#code-sections"),
        );
    }

    /// Validates a memory operand named by an HLA statement.
    fn check_memory_operand(&mut self, name: &str, span: Span) {
        let Some(id) = self.model.symbols.lookup(name) else {
            let mut resolver = self.resolver();
            resolver.resolve(name, span);
            let resolved = resolver.finish();
            self.absorb(resolved);
            return;
        };
        self.model.referenced.insert(id);
        let symbol = self.model.symbols.symbol(id);
        let declaration = symbol.declaration;
        match symbol.kind {
            SymbolKind::Var { .. } | SymbolKind::Label { .. } | SymbolKind::DataLabel { .. } => {}
            SymbolKind::Const { .. } => self.diagnostics.push(
                Diagnostic::error(
                    DiagnosticCode::CONST_IN_ADDRESS_POSITION,
                    format!("`{name}` is a `const`, not a memory location"),
                    Label::new(span, "a `const` names a value, not an address"),
                )
                .with_secondary(Label::new(declaration, "declared here"))
                .with_fix(Fix {
                    title: format!("load the value with `#{name}`"),
                    applicability: Applicability::MachineApplicable,
                    edits: vec![TextEdit::insert(span, "#")],
                })
                .with_docs("doc/syntax-reference.md#65816-addressing-modes-in-k65"),
            ),
            SymbolKind::Function { .. } => self.diagnostics.push(
                Diagnostic::error(
                    DiagnosticCode::SYMBOL_KIND_MISMATCH,
                    format!("`{name}` is a function, not a variable"),
                    Label::new(span, "cannot be used as a memory operand"),
                )
                .with_secondary(Label::new(declaration, "declared here"))
                .with_help(format!(
                    "call it with `call {name}`, or take its address with `&&{name}`"
                )),
            ),
        }
    }
}

/// The span of the first operator that makes `expr` compound rather than an
/// atom, or `None` when it is a legal data-value atom.
///
/// An address-byte operator is an atom, and so is symbol±offset arithmetic
/// underneath one — `&<ptr+1` is the documented spelling. Everything else
/// with an operator in it is compound.
fn first_compound(expr: &Expr) -> Option<Span> {
    match &expr.kind {
        ExprKind::Number(_)
        | ExprKind::Symbol(_)
        | ExprKind::LocalSymbol(_)
        | ExprKind::Eval(_)
        | ExprKind::Error => None,
        ExprKind::Unary(unary) => match unary.op {
            ast::UnaryOp::LowByte
            | ast::UnaryOp::HighByte
            | ast::UnaryOp::Address16
            | ast::UnaryOp::Address24 => first_non_offset(&unary.operand),
            ast::UnaryOp::Negate | ast::UnaryOp::BitNot => Some(unary.op_span),
        },
        ExprKind::Binary(binary) => Some(binary.op_span),
        ExprKind::Field(_)
        | ExprKind::Subscript(_)
        | ExprKind::Metadata(_)
        | ExprKind::TypedView(_) => Some(expr.span),
    }
}

/// The span of the first operator inside an address-byte operator's operand
/// that is not plain `+`/`-` offset arithmetic.
fn first_non_offset(expr: &Expr) -> Option<Span> {
    match &expr.kind {
        ExprKind::Number(_) | ExprKind::Symbol(_) | ExprKind::LocalSymbol(_) => None,
        ExprKind::Binary(binary) => match binary.op {
            ast::BinaryOp::Add | ast::BinaryOp::Sub => {
                first_non_offset(&binary.left).or_else(|| first_non_offset(&binary.right))
            }
            ast::BinaryOp::Mul
            | ast::BinaryOp::Shl
            | ast::BinaryOp::Shr
            | ast::BinaryOp::BitAnd
            | ast::BinaryOp::BitXor
            | ast::BinaryOp::BitOr => Some(binary.op_span),
        },
        ExprKind::Unary(unary) => Some(unary.op_span),
        ExprKind::Field(_)
        | ExprKind::Subscript(_)
        | ExprKind::Metadata(_)
        | ExprKind::TypedView(_)
        | ExprKind::Eval(_) => Some(expr.span),
        ExprKind::Error => None,
    }
}

/// The `K816-K65-####` diagnostic for a reserved legacy construct.
fn reserved_diagnostic(reserved: &crate::ast::ReservedConstruct) -> Diagnostic {
    let (code, construct) = match reserved.kind {
        ReservedKind::ImageBlock | ReservedKind::ImageTopLevel => {
            (DiagnosticCode::K65_RESERVED_IMAGE, "image")
        }
        ReservedKind::EvalCall(name) => (
            DiagnosticCode::K65_RESERVED_EVAL_FUNCTION,
            eval_call_text(name),
        ),
    };
    compat::k65_reserved(code, reserved.keyword_span, construct)
}

/// The source spelling of a reserved evaluator function.
fn eval_call_text(name: EvalCallName) -> &'static str {
    match name {
        EvalCallName::Size => "size",
        EvalCallName::AddByte => "addbyte",
        EvalCallName::Color => "color",
        EvalCallName::Rnd => "rnd",
        EvalCallName::Print => "print",
        EvalCallName::Error => "error",
    }
}

/// Names the first part of a code section that has no semantics yet and the
/// sub-plan that owns it, or `None` for the plain `func` K816 compiles.
///
/// The owner is per feature rather than per call site: `plans/`
/// `feature-manifest.md` §2 gives `naked`, `inline`, and `far func` to E — the
/// function *kinds* — while the call contract is D's width work.
fn unsupported_function_feature(func: &Function) -> Option<(&'static str, compat::Owner)> {
    if func.is_far {
        Some(("`far func` sections", compat::Owner::HlaLowering))
    } else if func
        .contract
        .as_ref()
        .is_some_and(|contract| !is_exit_width_only(contract))
    {
        Some(("call contracts", compat::Owner::WidthAndContracts))
    } else {
        match func.kind {
            FunctionKind::Func | FunctionKind::Main => None,
            FunctionKind::Naked => Some(("`naked` sections", compat::Owner::HlaLowering)),
            FunctionKind::Inline => Some(("`inline` sections", compat::Owner::HlaLowering)),
        }
    }
}

/// True when a contract is nothing but an exit-width clause.
///
/// `func f -> @a8 { }` declares what the body already does and asks nothing
/// of a call site, so it is checked here. Register flow and aliases are the
/// call-contract work proper and still report their owner.
fn is_exit_width_only(contract: &crate::ast::CallContract) -> bool {
    contract.inputs.is_none()
        && contract
            .outputs
            .as_ref()
            .is_some_and(|outputs| outputs.registers.is_empty() && !outputs.width.is_empty())
}

/// True when this operand is the accumulator, the only register the slice
/// loads.
fn is_accumulator(kind: &HlaOperandKind) -> bool {
    matches!(kind, HlaOperandKind::Register(register) if register.kind == RegisterKind::A)
}

/// The plain symbol an assignment target names, if that is all it is: no
/// index, no encoding prefix, no arithmetic.
fn plain_symbol(kind: &HlaOperandKind) -> Option<&crate::ast::Name> {
    let HlaOperandKind::Memory(memory) = kind else {
        return None;
    };
    if memory.encoding.is_some() || memory.index.is_some() || memory.offset_marker.is_some() {
        return None;
    }
    match &memory.value.kind {
        ExprKind::Symbol(name) => Some(name),
        _ => None,
    }
}

/// Names the first part of `var` that has no semantics yet, or `None` when
/// the declaration is the fixed-address form the slice compiles.
fn unsupported_var_feature(var: &crate::ast::VarDeclaration) -> Option<&'static str> {
    if var.is_abstract {
        Some("`abstract var` layouts")
    } else if !var.fields.is_empty() {
        Some("symbolic subscript field lists")
    } else if var.storage.is_some() && var.address.is_none() {
        // On a pinned variable the prefix only picks the encoding plain
        // references default to, which is the address-size axis C owns. On
        // an allocated one it also asks for a storage pool — the direct-page
        // pool, or far storage — and that allocation is E's.
        Some("`dp` / `abs` / `far` storage classes on allocated variables")
    } else if var.width.is_some() {
        Some("typed width suffixes")
    } else if var.size.is_some() {
        Some("array declarations")
    } else if var.count.is_some() {
        Some("allocation counts")
    } else if var.address.is_none() {
        Some("linker-allocated variables")
    } else {
        None
    }
}

/// The [`ItemKind::Func`] one [`FunctionInfo`] describes.
fn function_of<'a>(file: &'a AstFile, info: &FunctionInfo) -> Option<&'a Function> {
    match &file.items.get(info.item_index)?.kind {
        ItemKind::Func(func) => Some(func),
        _ => None,
    }
}

/// The unit's call graph, condensed into strongly connected components in
/// reverse topological order — every component's callees come before it.
///
/// Tarjan's algorithm, which emits a component only once everything it can
/// reach has been emitted, so the order is the one exit classification needs
/// without a separate topological sort.
fn strongly_connected_components(functions: &[FunctionInfo]) -> Vec<Vec<usize>> {
    let index_of: std::collections::HashMap<SymbolId, usize> = functions
        .iter()
        .enumerate()
        .map(|(index, info)| (info.symbol, index))
        .collect();

    struct Tarjan<'a> {
        functions: &'a [FunctionInfo],
        index_of: &'a std::collections::HashMap<SymbolId, usize>,
        /// Discovery index per node, `None` until visited.
        order: Vec<Option<usize>>,
        /// The lowest discovery index reachable from the node's subtree.
        low: Vec<usize>,
        on_stack: Vec<bool>,
        stack: Vec<usize>,
        next: usize,
        components: Vec<Vec<usize>>,
    }

    impl Tarjan<'_> {
        fn visit(&mut self, node: usize) {
            self.order[node] = Some(self.next);
            self.low[node] = self.next;
            self.next += 1;
            self.stack.push(node);
            self.on_stack[node] = true;

            for callee in &self.functions[node].callees {
                // A callee this unit does not define is not a node: it
                // contributes the language rule at the call site instead.
                let Some(&target) = self.index_of.get(callee) else {
                    continue;
                };
                match self.order[target] {
                    None => {
                        self.visit(target);
                        self.low[node] = self.low[node].min(self.low[target]);
                    }
                    Some(discovered) if self.on_stack[target] => {
                        self.low[node] = self.low[node].min(discovered);
                    }
                    Some(_) => {}
                }
            }

            if self.low[node] == self.order[node].expect("visited") {
                let mut component = Vec::new();
                while let Some(member) = self.stack.pop() {
                    self.on_stack[member] = false;
                    component.push(member);
                    if member == node {
                        break;
                    }
                }
                self.components.push(component);
            }
        }
    }

    let mut tarjan = Tarjan {
        functions,
        index_of: &index_of,
        order: vec![None; functions.len()],
        low: vec![0; functions.len()],
        on_stack: vec![false; functions.len()],
        stack: Vec::new(),
        next: 0,
        components: Vec::new(),
    };
    for node in 0..functions.len() {
        if tarjan.order[node].is_none() {
            tarjan.visit(node);
        }
    }
    tarjan.components
}

/// Two reachable returns of one function that leave an axis at different
/// fixed widths.
///
/// A hard error at the callee rather than something a caller absorbs: the
/// `Unknown ⇒ Preserve` rule never silently masks a genuinely divergent exit,
/// because there is no width a caller could be told to expect.
fn divergent_exit(name: &str, divergence: width::Divergence) -> Diagnostic {
    Diagnostic::error(
        DiagnosticCode::DIVERGENT_EXIT,
        format!(
            "`{name}` returns at two different {} widths",
            divergence.axis.name()
        ),
        Label::new(
            divergence.second,
            format!(
                "this return leaves {}",
                divergence.axis.annotation(divergence.second_width)
            ),
        ),
    )
    .with_secondary(Label::new(
        divergence.first,
        format!(
            "this one leaves {}",
            divergence.axis.annotation(divergence.first_width)
        ),
    ))
    .with_note("a caller has one tracked width, so it cannot follow both paths")
    .with_help("re-establish the same width on every path before returning")
    .with_docs("doc/register-width-aware-syntax.md#unknown-exit-contracts-preserve-caller-mode")
}

/// A module-level width annotation that a code section already precedes.
fn late_module_width(item: Span, annotation: Span, section: Span) -> Diagnostic {
    Diagnostic::error(
        DiagnosticCode::MODULE_WIDTH_PLACEMENT,
        "a module-level width default must come before every code section",
        Label::new(annotation, "this default arrives too late"),
    )
    .with_secondary(Label::new(section, "the first code section"))
    .with_note(
        "a module default is a file-wide statement, so reading it positionally \
         would make it mean different things depending on what was inserted above it",
    )
    .with_help("move the annotation to the top of the file")
    .with_fix(Fix {
        title: "move the default to the top of the file".to_owned(),
        applicability: Applicability::MachineApplicable,
        edits: vec![
            TextEdit::replace(Span::empty_at(item.file, 0), "@a8\n"),
            TextEdit::delete(item),
        ],
    })
    .with_docs("doc/register-width-aware-syntax.md#module-level-default-coloring")
}

/// The address size a `dp` / `abs` / `far` prefix forces.
fn address_size_of(kind: crate::ast::StorageClassKind) -> AddressSize {
    match kind {
        crate::ast::StorageClassKind::DirectPage => AddressSize::DirectPage,
        crate::ast::StorageClassKind::Absolute => AddressSize::Absolute,
        crate::ast::StorageClassKind::Far => AddressSize::Long,
    }
}
