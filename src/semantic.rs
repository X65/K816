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
    self, AstFile, ContractParameter, DataBlock, DataEntryKind, DataValue, EvalCallName, EvalExpr,
    EvalExprKind, Expr, ExprKind, Function, FunctionKind, HlaStatement, ItemKind, RegisterKind,
    ReservedKind, Statement, StatementKind,
};
use std::collections::{BTreeMap, BTreeSet};

use crate::compat;
use crate::diagnostic::{Applicability, Diagnostic, DiagnosticCode, Fix, Label, TextEdit};
use crate::effects::ControlEffect;
use crate::isa::RegisterWidth;
use crate::layout;
use crate::linkage::Linkage;
use crate::op::{ExitContract, ModeState, WidthContract};
use crate::resolve::{AnonymousBlocks, Classified, DataSite, Resolved, Resolver, data_site};
use crate::source::{Provenance, Span};
use crate::symbol::{AddressSize, LabelScope, SymbolId, SymbolKind, SymbolTable};
use crate::width::{self, WidthAnalysis};

/// The result of analyzing one file: symbols plus per-function facts.
#[derive(Debug, Default)]
pub struct SemanticModel {
    pub symbols: SymbolTable,
    pub functions: Vec<FunctionInfo>,
    /// Every unit-level symbol the unit actually referred to, which is what
    /// separates an `.import` this unit needs from one it merely could have.
    pub referenced: BTreeSet<SymbolId>,
    /// The declared register flow of each contract-bearing function.
    ///
    /// Separate from [`FunctionInfo`] because most functions have none: a
    /// legacy K65 call declares nothing, and the unchecked form stays the
    /// default.
    pub contracts: BTreeMap<SymbolId, CallFlow>,
    /// The width dataflow's output, per function.
    ///
    /// Stored rather than recomputed: lowering looks a statement's state up
    /// here instead of walking the body a second time, which is what keeps
    /// analysis and lowering from arriving at two different widths.
    pub widths: BTreeMap<SymbolId, WidthAnalysis>,
    /// The function each label was written inside.
    ///
    /// A local label already records its owner as its scope, but a *global*
    /// one is a unit-level name that happens to sit in some function's body,
    /// and nothing else remembers which. `goto` needs the answer:
    /// [`DiagnosticCode::GOTO_FOREIGN_LABEL`] is exactly the question "is
    /// this label in the function jumping to it".
    pub label_owner: BTreeMap<SymbolId, SymbolId>,
    /// The storage each allocated `var` asks the linker for, keyed by the
    /// index of its item in [`AstFile::items`].
    ///
    /// Lowering emits the reservation and needs the item's *position*, not
    /// just the symbol: an absolute variable lands in whichever segment the
    /// preceding items selected, which is a fact about where the declaration
    /// sits rather than about what it declares. A pinned variable has no
    /// entry — it is an equate, and equates reserve nothing.
    pub reservations: BTreeMap<usize, Reservation>,
}

/// One allocated variable's claim on the linker.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Reservation {
    pub symbol: SymbolId,
    /// Bytes to reserve, already multiplied out by the declaration's span
    /// and count.
    pub bytes: u32,
    /// True for a `var dp`, which is placed in the direct-page pool rather
    /// than in the segment its declaration happens to sit in — the direct
    /// page is "segment-orthogonal" and the document says so twice.
    pub direct_page: bool,
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

/// The register flow a `func NAME (a, x) -> a, y` header declares.
///
/// Only the registers and aliases: the exit *widths* of the same clause are
/// the width model's, and are checked against the body's reachable returns.
#[derive(Clone, Debug)]
pub struct CallFlow {
    /// The input list **in written order**, because an `inline`'s aliases
    /// bind positionally against the use site's arguments.
    pub inputs: Vec<FlowInput>,
    pub outputs: Vec<RegisterKind>,
    /// The clause itself, for the "declared here" label.
    pub span: Span,
}

/// One entry of a declared input list.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FlowInput {
    /// A register the caller is expected to have loaded.
    Register(RegisterKind),
    /// An `inline` alias, bound to the argument written in this position.
    Alias {
        symbol: SymbolId,
        /// Written `#name` — the alias stands for a value rather than a
        /// place. Both bind from an immediate argument; the difference is
        /// what the body may do with the result.
        immediate: bool,
    },
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
        loops: 0,
        unit: unit_name.to_owned(),
        anonymous_blocks: AnonymousBlocks::default(),
        defaults: WidthContract::default(),
        externally_reachable: BTreeSet::new(),
        workspace_contracts: BTreeMap::new(),
        workspace_recursive: BTreeSet::new(),
        direct_page: layout::DirectPage::default(),
        direct_page_reported: false,
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
        analysis.workspace_contracts = analysis.collect_workspace_contracts(linkage);
        analysis.workspace_recursive = analysis
            .model
            .symbols
            .iter()
            .filter(|(_, symbol)| linkage.is_recursive(&symbol.name))
            .map(|(id, _)| id)
            .collect();
    }
    // The width dataflow runs before the checks that consume it, and after
    // imports so that a foreign callee resolves.
    analysis.solve_widths(file);
    // Before the bodies are checked, because a cycle of expansions is a
    // question about the sections rather than about any statement in them —
    // and because lowering walks the same edges and must not be reached with
    // one.
    analysis.check_inline_recursion();
    analysis.check_functions(file);
    (analysis.model, analysis.diagnostics)
}

struct Analysis {
    model: SemanticModel,
    diagnostics: Vec<Diagnostic>,
    /// The function whose body is being walked, for local label scope.
    current_function: Option<SymbolId>,
    /// How many loops enclose the statement being checked, which is what
    /// `break` and `repeat` need one of.
    loops: usize,
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
    /// Every function's contract as the workspace classified it, translated
    /// into this unit's symbol ids.
    ///
    /// Empty when this unit is analyzed alone, in which case the unit's own
    /// classification is the authority.
    workspace_contracts: BTreeMap<SymbolId, width::CalleeContract>,
    /// Functions the workspace call graph put in a cycle, including ones
    /// that only close through another unit.
    workspace_recursive: BTreeSet<SymbolId>,
    /// The unit's running claim on the 256-byte direct page.
    direct_page: layout::DirectPage,
    /// True once the pool has overflowed and said so, so the declarations
    /// after it do not each repeat the news.
    direct_page_reported: bool,
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
                    if func.name.provenance == Provenance::Synthetic {
                        continue;
                    }
                    let kind = match func.kind {
                        FunctionKind::Inline => SymbolKind::Inline,
                        _ => SymbolKind::function(func.is_far),
                    };
                    let Some(symbol) = self.declare(&func.name.text, kind, func.name.span) else {
                        continue;
                    };
                    // Aliases are declared before the body is walked, for the
                    // same reason labels are: the body reads them, and an
                    // alias that is not a symbol by then resolves as an
                    // undeclared name.
                    self.declare_aliases_of(symbol, func);
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

    /// Declares an `inline`'s parameter aliases into its own scope.
    ///
    /// They share the scope local labels live in, which is what makes two
    /// `inline`s free to use the same alias name and what keeps an alias
    /// invisible everywhere but the body that declared it. There is no
    /// collision to arbitrate: a local label's name begins with `.` and an
    /// alias's does not.
    ///
    /// An alias on a section that is not `inline` is `E0121`, reported by
    /// `check_contract`; it is declared here anyway, so the body still reads
    /// as the author meant and the section does not produce a second wave of
    /// undeclared-name errors on top of the one real complaint.
    fn declare_aliases_of(&mut self, owner: SymbolId, func: &Function) {
        let Some(contract) = &func.contract else {
            return;
        };
        for parameter in contract.inputs.iter().flatten() {
            let (name, immediate) = match parameter {
                ContractParameter::Register(_) => continue,
                ContractParameter::ImmediateAlias { name, .. } => (name, true),
                ContractParameter::Alias(name) => (name, false),
            };
            self.declare_local(
                owner,
                &name.text,
                SymbolKind::Alias { immediate },
                name.span,
            );
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
                    let declared = match label.scope {
                        crate::ast::LabelScope::Global => {
                            self.declare(&label.name.text, kind, label.name.span)
                        }
                        crate::ast::LabelScope::Local => {
                            self.declare_local(owner, &label.name.text, kind, label.name.span)
                        }
                    };
                    if let Some(label) = declared {
                        self.model.label_owner.insert(label, owner);
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
                ItemKind::Var(var) => self.declare_var(item_index, item.span, var),
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
                    self.check_contract(func, symbol);
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

    /// Validates a section's call contract and records its register flow.
    ///
    /// The rules are keyed on *what the parameter is*, not on which section
    /// kind is currently implemented. `doc/syntax-reference.md` is explicit
    /// that a `naked` function may carry entry-mode annotations and an input
    /// parameter list, so register parameters are legal everywhere; aliases
    /// are `inline`-only because they bind during expansion rather than
    /// being passed at a call.
    fn check_contract(&mut self, func: &Function, symbol: SymbolId) {
        let Some(contract) = &func.contract else {
            return;
        };
        let mut inputs = Vec::new();
        for parameter in contract.inputs.iter().flatten() {
            let (name, immediate) = match parameter {
                ContractParameter::Register(register) => {
                    inputs.push(FlowInput::Register(register.kind));
                    continue;
                }
                ContractParameter::ImmediateAlias { name, .. } => (name, true),
                ContractParameter::Alias(name) => (name, false),
            };
            if func.kind != FunctionKind::Inline {
                self.diagnostics.push(alias_not_inline(name, func));
                continue;
            }
            // Declared into the section's own scope by `declare_aliases_of`,
            // which ran in the first pass so the body could read it.
            if let Some(symbol) = self.model.symbols.lookup_local(symbol, &name.text) {
                inputs.push(FlowInput::Alias { symbol, immediate });
            }
        }
        // A `naked` section never returns to its caller, so it has no exit
        // for a `->` clause to describe — neither the registers it hands back
        // nor the widths they arrive at. The clause is refused rather than
        // ignored, because a declaration nothing checks is worse than none.
        if func.kind == FunctionKind::Naked
            && let Some(outputs) = &contract.outputs
        {
            self.diagnostics
                .push(naked_exit_contract(&func.name, outputs.span));
        }
        let outputs: Vec<RegisterKind> = contract
            .outputs
            .iter()
            .flat_map(|outputs| outputs.registers.iter())
            .map(|register| register.kind)
            .collect();
        // Only a clause that names registers asks anything of a call site; an
        // exit-width-only clause is checked against the body instead.
        if contract.inputs.is_some() || !outputs.is_empty() {
            self.model.contracts.insert(
                symbol,
                CallFlow {
                    inputs,
                    outputs,
                    span: contract.span,
                },
            );
        }
    }

    /// Whether a use site's arguments match what the callee declares.
    ///
    /// Positional, entry by entry: a register parameter wants that register
    /// named, an alias parameter wants an immediate. That is the whole
    /// difference an `inline` makes at a use site — `scale a, #16 -> a` binds
    /// `#factor` to `16`, and the `#` is argument syntax rather than a claim
    /// about what the substituted expression will be used *as*.
    fn arguments_match(&self, flow: &CallFlow, arguments: &[crate::ast::CallArgument]) -> bool {
        use crate::ast::CallArgument;
        flow.inputs.len() == arguments.len()
            && flow
                .inputs
                .iter()
                .zip(arguments)
                .all(|(declared, written)| match (declared, written) {
                    (FlowInput::Register(kind), CallArgument::Register(register)) => {
                        *kind == register.kind
                    }
                    (FlowInput::Alias { .. }, CallArgument::Immediate(_)) => true,
                    _ => false,
                })
    }

    /// The source spelling of a declared flow, for the help line.
    fn describe_flow(&self, flow: &CallFlow) -> String {
        let inputs = flow
            .inputs
            .iter()
            .map(|input| match input {
                FlowInput::Register(register) => register_name(*register).to_owned(),
                FlowInput::Alias { symbol, immediate } => {
                    let name = self.model.symbols.name(*symbol);
                    if *immediate {
                        format!("#{name}")
                    } else {
                        name.to_owned()
                    }
                }
            })
            .collect::<Vec<_>>()
            .join(", ");
        if flow.outputs.is_empty() {
            return inputs;
        }
        let outputs = flow
            .outputs
            .iter()
            .map(|register| register_name(*register))
            .collect::<Vec<_>>()
            .join(", ");
        format!("{inputs} -> {outputs}")
    }

    /// Checks a bare call's register flow against the callee's declaration.
    ///
    /// `call NAME` opts out of both directions of this check — it is the
    /// documented escape hatch — which is why it never reaches here.
    fn check_call_flow(&mut self, statement: &Statement, call: &crate::ast::BareCall) {
        let Some(symbol) = self.model.symbols.lookup(&call.target.text) else {
            return; // Undeclared; the target check reported it.
        };
        let flow = self.model.contracts.get(&symbol).cloned();
        let written = call.arguments.as_ref();
        let declared_outputs: Option<Vec<RegisterKind>> = call
            .outputs
            .as_ref()
            .map(|outputs| outputs.iter().map(|register| register.kind).collect());

        match flow {
            None if written.is_none() && declared_outputs.is_none() => {}
            None => self.diagnostics.push(call_flow_mismatch(
                statement.span,
                format!(
                    "`{}` declares no register flow, so this call cannot state one",
                    call.target.text
                ),
                "drop the flow clause, or declare it on the section",
                None,
            )),
            Some(flow) if written.is_none() && declared_outputs.is_none() => {
                self.diagnostics.push(call_flow_mismatch(
                    statement.span,
                    format!(
                        "`{}` declares a register flow, and this call states none",
                        call.target.text
                    ),
                    format!(
                        "write `{} {}` to state the flow, or `call {}` to opt out of the check",
                        call.target.text,
                        self.describe_flow(&flow),
                        call.target.text
                    ),
                    Some(flow.span),
                ));
            }
            Some(flow) => {
                let arguments = written.map_or(&[][..], Vec::as_slice);
                if !self.arguments_match(&flow, arguments) {
                    self.diagnostics.push(call_flow_mismatch(
                        statement.span,
                        format!(
                            "this call hands `{}` different arguments than it declares",
                            call.target.text
                        ),
                        format!("the declaration reads `{}`", self.describe_flow(&flow)),
                        Some(flow.span),
                    ));
                }
                let outputs = declared_outputs.unwrap_or_default();
                if outputs != flow.outputs {
                    self.diagnostics.push(call_flow_mismatch(
                        statement.span,
                        format!(
                            "this call expects different registers back from `{}` than it declares",
                            call.target.text
                        ),
                        format!("the declaration reads `{}`", self.describe_flow(&flow)),
                        Some(flow.span),
                    ));
                }
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

    /// The workspace's classified contract for every name this unit can see.
    fn collect_workspace_contracts(
        &self,
        linkage: &Linkage,
    ) -> BTreeMap<SymbolId, width::CalleeContract> {
        self.model
            .symbols
            .iter()
            .filter_map(|(id, symbol)| {
                linkage
                    .contract(&symbol.name)
                    .map(|contract| (id, contract))
            })
            .collect()
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
        // With a workspace to consult, its classification is the authority:
        // it saw every callee, cross-unit cycles included, so the code this
        // unit emits and the contract it exports come from one answer rather
        // than from two that agree only by luck.
        let mut callees: BTreeMap<SymbolId, width::CalleeContract> =
            self.workspace_contracts.clone();
        let adjacency = call_graph(&self.model.functions);
        for component in width::strongly_connected_components(&adjacency) {
            // A non-trivial component, or a self-loop, is recursion: there is
            // no fixpoint to find and no answer better than the fallback.
            let recursive = component.len() > 1 || {
                let index = component[0];
                let symbol = self.model.functions[index].symbol;
                self.model.functions[index].callees.contains(&symbol)
                    // A cycle that closes through another unit is invisible
                    // from here, so the workspace answer carries it.
                    || self.workspace_recursive.contains(&symbol)
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
                let (exit, assumed) = match self.workspace_contracts.get(&info.symbol) {
                    // The workspace already classified this function, over a
                    // graph that includes the callers and callees this unit
                    // cannot see.
                    Some(_) if recursive => (ExitContract::preserving(), true),
                    Some(contract) => (
                        contract.exit,
                        contract.exit.a == crate::op::AxisExit::Unknown
                            || contract.exit.i == crate::op::AxisExit::Unknown,
                    ),
                    None if recursive => (ExitContract::preserving(), true),
                    None => (classified.exit, classified.assumed),
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
                .with_help(exit_width_help(func, axis))
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

    /// Reports every `inline` that expands into itself.
    ///
    /// Expansion is a copy, so a cycle of them does not terminate — unlike a
    /// cycle of *calls*, which is ordinary recursion and costs only stack.
    /// The edges are the ones the width solver already collected: it walks
    /// every use site to find what a body calls, and an `inline` use site is
    /// a call as far as that walk is concerned.
    fn check_inline_recursion(&mut self) {
        let expansions: BTreeMap<SymbolId, Vec<SymbolId>> = self
            .model
            .functions
            .iter()
            .filter(|info| self.model.symbols.kind(info.symbol) == SymbolKind::Inline)
            .map(|info| {
                let callees = info
                    .callees
                    .iter()
                    .copied()
                    .filter(|callee| self.model.symbols.kind(*callee) == SymbolKind::Inline)
                    .collect();
                (info.symbol, callees)
            })
            .collect();

        for &section in expansions.keys() {
            let Some(cycle) = reaches(section, section, &expansions, &mut BTreeSet::new()) else {
                continue;
            };
            // One complaint per cycle, not one per member: every section in a
            // cycle is equally unable to expand, and saying so three times
            // does not tell the reader anything the first did not.
            if cycle.iter().any(|&member| member < section) {
                continue;
            }
            let route: Vec<&str> = cycle
                .iter()
                .map(|&member| self.model.symbols.name(member))
                .collect();
            let name = self.model.symbols.name(section);
            let declaration = self.model.symbols.symbol(section).declaration;
            self.diagnostics.push(
                Diagnostic::error(
                    DiagnosticCode::INLINE_RECURSION,
                    format!("`{name}` expands into itself"),
                    Label::new(declaration, "expanding this would not terminate"),
                )
                .with_note(format!("the cycle is `{}`", route.join("` → `")))
                .with_note(
                    "an `inline` is copied into each use site, so a cycle of them has no \
                     base case the way a cycle of calls does",
                )
                .with_help("make one of the sections a `func`, so the cycle becomes a call")
                .with_docs("doc/syntax-reference.md#inline"),
            );
        }
    }

    /// Checks one block's statements, and the blocks nested in them.
    fn check_block(&mut self, function: SymbolId, block: &crate::ast::Block) {
        for statement in &block.statements {
            let widths = self.widths_at(function, statement);
            self.check_statement(statement, widths);
            // A nested body is ordinary code, checked with the state the
            // solver tracks inside it rather than skipped along with the
            // statement that opens it — a scoped width block's body at the
            // block's width, a conditional's and a loop's at theirs.
            match &statement.kind {
                StatementKind::WidthBlock(width) => self.check_block(function, &width.body),
                StatementKind::Conditional(conditional) => {
                    self.check_block(function, &conditional.body);
                    if let Some(otherwise) = &conditional.otherwise {
                        self.check_block(function, otherwise);
                    }
                }
                StatementKind::Loop(loop_statement) => {
                    self.loops += 1;
                    self.check_block(function, &loop_statement.body);
                    self.loops -= 1;
                }
                StatementKind::Nocross(body) => self.check_block(function, body),
                _ => {}
            }
        }
    }

    /// `break` and `repeat` bind to the innermost enclosing loop, so outside
    /// one there is nothing for them to bind to (sub-plan E, decision 6).
    fn check_loop_control(&mut self, statement: &Statement, keyword: &str) {
        if self.loops > 0 {
            return;
        }
        self.diagnostics.push(
            Diagnostic::error(
                DiagnosticCode::LOOP_CONTROL_OUTSIDE_LOOP,
                format!("`{keyword}` is not inside a loop"),
                Label::new(statement.span, format!("no loop for `{keyword}` to leave")),
            )
            .with_help(
                "`break` and `repeat` bind to the innermost enclosing `{ ... }` loop; \
                 `goto` leaves a function body",
            )
            .with_docs("doc/syntax-reference.md#break-and-repeat"),
        );
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
                    self.check_addressable(native.mnemonic.span, &classified);
                }
            }
            StatementKind::Hla(hla) => {
                self.check_hla(hla, statement.span, widths);
            }
            // Both label scopes were declared by the first pass, so a label
            // statement has nothing left to check here.
            StatementKind::Label(_) => {}
            // Both returns are ordinary statements. Which one a body may
            // write is not checked: `rti` in a `func` is legal 65816 and the
            // legacy corpus writes `return_i` inside a `naked` section, so
            // the pairing is the author's to get right.
            StatementKind::Return(_) | StatementKind::Error => {}
            // Both establish widths, which the solver has already tracked;
            // the body of a block is checked by the walk that got here.
            StatementKind::WidthBlock(_) | StatementKind::ModeSet(_) => {}
            // The condition itself needs no checking beyond what the grammar
            // did: every operator maps to a branch, and the bodies are walked
            // by `check_block`.
            StatementKind::Conditional(_) | StatementKind::Loop(_) | StatementKind::Nocross(_) => {}
            StatementKind::Break => self.check_loop_control(statement, "break"),
            StatementKind::Repeat => self.check_loop_control(statement, "repeat"),
            StatementKind::ConditionalJump(jump) => match &jump.jump.kind {
                StatementKind::Break => self.check_loop_control(&jump.jump, "break"),
                StatementKind::Repeat => self.check_loop_control(&jump.jump, "repeat"),
                StatementKind::Goto(goto) => self.check_goto(goto),
                // The grammar admits nothing else after a condition; an
                // `Error` statement is already reported where it was parsed.
                _ => {}
            },
            StatementKind::Goto(goto) => self.check_goto(goto),
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
                self.check_call_target(&call.target);
                self.check_call_convention(&call.target, call.is_far);
            }
            StatementKind::BareCall(call) => {
                self.check_call_target(&call.target);
                self.check_call_convention(&call.target, false);
                self.check_call_flow(statement, call);
            }
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

    /// The absolute value of `expr` in a position that requires one.
    fn constant(&mut self, expr: &Expr, position: &str) -> Option<u32> {
        let mut resolver = self.resolver();
        let value = resolver.constant(expr, position);
        let resolved = resolver.finish();
        self.absorb(resolved);
        value
    }

    /// Declares one `var`, in every shape the document gives it.
    ///
    /// Three axes are decided here and they stay independent, which is
    /// sub-plan E's decision 14. The *storage class* says where the variable
    /// lives and how a plain reference to it encodes; the *type width* says
    /// how wide one datum is; the two multipliers say how many of them there
    /// are. `var dp p:far` is a 24-bit value in the direct page, and it is
    /// legal precisely because those are three answers and not one.
    ///
    /// What K816 decides is size. Where an allocated variable lands is
    /// ld65's, and stays ld65's (`plans/feature-manifest.md` §3).
    fn declare_var(&mut self, item_index: usize, span: Span, var: &ast::VarDeclaration) {
        if var.name.provenance == Provenance::Synthetic {
            return; // Recovery invented the name; syntax error already reported.
        }
        // Field lists and the storage-less `abstract var` are the two axes
        // E7 leaves; both need a layout, and layouts are E8's.
        if let Some(feature) = unsupported_var_feature(var) {
            self.diagnostics.push(compat::unimplemented(
                span,
                feature,
                compat::Owner::HlaLowering,
            ));
            return;
        }

        let address = match &var.address {
            Some(expr) => match self.constant(expr, "variable addresses") {
                Some(address) => Some(address),
                // An address that does not fold leaves nothing to declare:
                // every reference would need a value this one cannot supply.
                None => return,
            },
            None => None,
        };
        // An extent that does not fold has already reported itself, and the
        // declaration goes on without the multiplier — one bad count should
        // not turn every use of the name into an undeclared-symbol error too.
        let extent = var
            .size
            .as_ref()
            .and_then(|expr| self.constant(expr, "array extents"));
        let count = var
            .count
            .as_ref()
            .and_then(|expr| self.constant(expr, "allocation counts"));
        let width = var.width.map(|width| layout::data_width(width.kind));
        let storage = var.storage.map(|storage| storage.kind);

        // A declaration-position prefix sets the encoding plain references
        // default to; without one a pinned variable's own magnitude decides
        // and an allocated one is absolute, which is the documented default
        // (`doc/syntax-reference.md`, "Variable Declaration").
        let size = match (storage, address) {
            (Some(storage), _) => address_size_of(storage),
            (None, Some(address)) => AddressSize::of_value(address),
            (None, None) => AddressSize::Absolute,
        };

        let datum = layout::datum_size(width, None, self.model.symbols.layouts());
        let allocation = layout::allocation(datum, extent, count);

        // `far` is the storage class with no auto-allocator: nothing knows
        // which of the 256 banks an unaddressed 24-bit symbol belongs to.
        let far_unplaced = storage == Some(ast::StorageClassKind::Far) && address.is_none();
        if far_unplaced {
            self.diagnostics.push(far_needs_address(var, span));
        }

        let direct_page = storage == Some(ast::StorageClassKind::DirectPage);
        if direct_page {
            self.claim_direct_page(var, span, address, allocation.total);
        }

        let Some(symbol) = self.declare(
            &var.name.text,
            SymbolKind::Var {
                address,
                size,
                width,
                element: allocation.element,
                repeat: count,
                // No declaration reaching here carries a field list; E8's
                // layouts arrive with the pass that computes them.
                layout: None,
            },
            var.name.span,
        ) else {
            return;
        };

        // A pinned variable is an equate and reserves nothing. An unplaced
        // `far` one was refused just above, and emitting a reservation for it
        // would put 24-bit storage in a segment chosen by accident.
        if address.is_none() && !far_unplaced {
            self.model.reservations.insert(
                item_index,
                Reservation {
                    symbol,
                    bytes: allocation.total,
                    direct_page,
                },
            );
        }
    }

    /// Records one `var dp` against the unit's direct page and reports the
    /// declaration that does not fit.
    ///
    /// Two ways not to fit, and they are different complaints: a *pinned*
    /// slot can name an offset the page does not have, and the pool as a
    /// whole can be asked for more than 256 bytes. Only the first
    /// declaration to exhaust the pool is reported — every later one would
    /// repeat the same news about the same page.
    fn claim_direct_page(
        &mut self,
        var: &ast::VarDeclaration,
        span: Span,
        address: Option<u32>,
        bytes: u32,
    ) {
        let claimed = match address {
            Some(pinned) => self.direct_page.pin(pinned, bytes),
            None => self.direct_page.request(bytes),
        };
        if let Some(pinned) = address
            && layout::pinned_slot_overflows(pinned, bytes)
        {
            self.diagnostics
                .push(pinned_slot_outside_page(var, span, pinned, bytes));
            return;
        }
        if claimed > layout::DIRECT_PAGE_BYTES && !self.direct_page_reported {
            self.direct_page_reported = true;
            self.diagnostics
                .push(direct_page_overflow(var, span, claimed));
        }
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

    /// Checks an HLA statement by expanding it.
    ///
    /// The instructions a statement means are the instructions analysis
    /// validates, so a statement analysis accepts is one lowering can emit —
    /// and the expansion itself is written once, in `hla.rs`, rather than
    /// here and again there (sub-plan E, decision 1).
    fn check_hla(&mut self, hla: &HlaStatement, site: Span, widths: ModeState) {
        let mut resolver = self.resolver().with_widths(widths);
        let steps = crate::hla::statement(&mut resolver, hla, site);
        let resolved = resolver.finish();
        self.absorb(resolved);
        for step in steps.into_iter().flatten() {
            self.check_addressable(site, &step);
        }
    }

    /// Rejects a symbol used in a role its declaration does not allow.
    ///
    /// Classification already checked the *shape* of an operand against the
    /// ISA table; this is the other axis — whether the name in it denotes
    /// something an operand may address at all. A `const` in address
    /// position is its own diagnostic, reported during classification.
    fn check_addressable(&mut self, site: Span, classified: &Classified) {
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
            | crate::op::Operand::Anchor(_)
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
                    Label::new(site, "cannot be used as a memory operand"),
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

    /// Checks that a call site and its callee agree on the calling
    /// convention.
    ///
    /// `jsr` pushes two bytes and `rts` pops two; `jsl` pushes three and
    /// `rtl` pops three. Mixing them does not fail at the call — it fails at
    /// the return, with the stack off by a byte and control landing wherever
    /// that points, which is why the legacy linker refused the pairing and
    /// K816 refuses it here.
    ///
    /// This is also the whole cross-unit half of the check: a foreign
    /// callee's kind is imported verbatim, so a `far func` in another unit
    /// arrives carrying its convention and is checked the same way as one
    /// next door (`plans/feature-manifest.md` §3).
    fn check_call_convention(&mut self, target: &crate::ast::Name, site_is_far: bool) {
        if target.provenance == Provenance::Synthetic {
            return;
        }
        let Some(id) = self.model.symbols.lookup(&target.text) else {
            return;
        };
        let symbol = self.model.symbols.symbol(id);
        // An `inline` is expanded rather than called, so there is no
        // transfer for `far` to lengthen. The original K65 reference says so
        // in as many words: "Inlines do not use far prefix, because the code
        // is simply inlined."
        if symbol.kind == SymbolKind::Inline {
            if site_is_far {
                let declaration = symbol.declaration;
                self.diagnostics.push(
                    Diagnostic::error(
                        DiagnosticCode::CALL_CONVENTION_MISMATCH,
                        format!("`{}` is an `inline`, so it is not called at all", target.text),
                        Label::new(target.span, "there is no transfer for `far` to lengthen"),
                    )
                    .with_secondary(Label::new(declaration, "declared here"))
                    .with_note(
                        "an `inline` body is copied into the use site, so it runs in                          whichever bank the use site is in",
                    )
                    .with_help(format!("write `{}` on its own", target.text))
                    .with_docs("doc/syntax-reference.md#far-calls"),
                );
            }
            return;
        }
        // Only a section declares a convention. A call to a bare address —
        // the legacy `var openmode = $EF9C` route into ROM — declares
        // nothing, so the call site's spelling is all there is and it wins.
        if !matches!(symbol.kind, SymbolKind::Function { .. }) {
            return;
        }
        let callee_is_far = symbol.kind.is_far_function();
        if callee_is_far == site_is_far {
            return;
        }
        let declaration = symbol.declaration;
        let (title, at, help) = if callee_is_far {
            (
                format!("`{}` is a `far func`, and this call is near", target.text),
                "this would `jsr` a section that returns with `rtl`",
                format!("write `call far {}`", target.text),
            )
        } else {
            (
                format!("`{}` is a near `func`, and this call is far", target.text),
                "this would `jsl` a section that returns with `rts`",
                format!(
                    "write `call {0}`, or declare the section as `far func {0}`",
                    target.text
                ),
            )
        };
        self.diagnostics.push(
            Diagnostic::error(DiagnosticCode::CALL_CONVENTION_MISMATCH, title, {
                Label::new(target.span, at)
            })
            .with_secondary(Label::new(declaration, "declared here"))
            .with_note("the call and the return have to push and pop the same number of bytes")
            .with_help(help)
            .with_docs("doc/syntax-reference.md#far-calls"),
        );
    }

    /// Checks a `goto`'s target: every name in it has to resolve, and a label
    /// it names has to belong to the function doing the jumping.
    fn check_goto(&mut self, goto: &crate::ast::GotoStatement) {
        if let Some((name, label)) = self.foreign_label(&goto.target) {
            let owner = self.model.label_owner.get(&label).copied();
            let (name, span) = (name.text.clone(), name.span);
            let declaration = self.model.symbols.symbol(label).declaration;
            let owner = owner.map(|owner| self.model.symbols.name(owner).to_owned());
            self.diagnostics.push(goto_foreign_label(
                &name,
                span,
                declaration,
                owner.as_deref(),
            ));
            return;
        }
        let mut resolver = self.resolver();
        resolver.fold(&goto.target);
        let resolved = resolver.finish();
        self.absorb(resolved);
    }

    /// The label `target` names when it belongs to some *other* function.
    ///
    /// Two shapes reach here. A `.name` this function does not declare, where
    /// another function does — the common typo, and the one a plain
    /// "not declared" would describe uselessly. And a global label, which is
    /// a unit-level name and so resolves from anywhere, written inside
    /// another function's body.
    fn foreign_label<'a>(&self, target: &'a Expr) -> Option<(&'a crate::ast::Name, SymbolId)> {
        let (name, label) = match &target.kind {
            ExprKind::Symbol(name) => (name, self.model.symbols.lookup(&name.text)?),
            ExprKind::LocalSymbol(name) => {
                let owner = self.current_function?;
                match self.model.symbols.lookup_local(owner, &name.text) {
                    // The function's own local label: exactly what a `goto`
                    // is for.
                    Some(_) => return None,
                    None => (name, self.label_named(&name.text)?),
                }
            }
            // Anything else is an address expression, not a label reference:
            // `goto (vec)` names the pointer's storage, and arithmetic names
            // no single declaration to attribute the jump to.
            _ => return None,
        };
        if !matches!(self.model.symbols.kind(label), SymbolKind::Label { .. }) {
            // A `func` is a legal target — the documented `goto inc_x` tail
            // jump — and so is a `var` holding an address.
            return None;
        }
        let owner = self.model.label_owner.get(&label).copied();
        (owner != self.current_function).then_some((name, label))
    }

    /// Any label declared under `name`, in whichever function declared it.
    ///
    /// Local labels are keyed by owner, so this is the only way to ask
    /// whether a `.name` exists *somewhere* — which is what turns "not
    /// declared" into "declared, but not here".
    fn label_named(&self, name: &str) -> Option<SymbolId> {
        self.model
            .label_owner
            .keys()
            .copied()
            .find(|&label| self.model.symbols.name(label) == name)
    }
}

/// A `goto` naming a label that is not in the function jumping to it.
fn goto_foreign_label(
    name: &str,
    span: Span,
    declaration: Span,
    owner: Option<&str>,
) -> Diagnostic {
    let title = match owner {
        Some(owner) => format!("`{name}` is a label of `{owner}`, not of this function"),
        None => format!("`{name}` is not a label of this function"),
    };
    Diagnostic::error(
        DiagnosticCode::GOTO_FOREIGN_LABEL,
        title,
        Label::new(span, "a `goto` stays inside its own function"),
    )
    .with_secondary(Label::new(declaration, "declared here"))
    .with_note(
        "arriving in the middle of another function means arriving with widths that \
         function's contract never promised, which is not a shape the width model can \
         reason about",
    )
    .with_help("call the function instead, or move the label into this one")
    .with_docs("doc/syntax-reference.md#labels")
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

/// Names the part of `var` that has no semantics yet, or `None` when the
/// declaration is one E7 allocates.
///
/// Both surviving arms are the same missing piece: a *layout*. A field list
/// needs one computed, and an `abstract var` is a layout and nothing else.
/// E8 computes them, and this function goes with them.
fn unsupported_var_feature(var: &crate::ast::VarDeclaration) -> Option<&'static str> {
    if var.is_abstract {
        Some("`abstract var` layouts")
    } else if !var.fields.is_empty() {
        Some("symbolic subscript field lists")
    } else {
        None
    }
}

/// `var far NAME` with nothing to place it at.
fn far_needs_address(var: &crate::ast::VarDeclaration, span: Span) -> Diagnostic {
    let storage = var.storage.map_or(span, |storage| storage.span);
    Diagnostic::error(
        DiagnosticCode::FAR_NEEDS_ADDRESS,
        format!("`far` variable `{}` has no address", var.name.text),
        Label::new(storage, "24-bit storage is never auto-allocated"),
    )
    .with_secondary(Label::new(var.name.span, "nothing places this"))
    .with_note(
        "an address is 24 bits wide because it names a bank, and neither K816 nor ld65 \
         has a rule for which of the 256 banks an unaddressed `far` symbol belongs in",
    )
    .with_help(format!(
        "give it an address — `var far {} = $C00000` — or drop `far` and let the linker \
         place it",
        var.name.text
    ))
    .with_docs("doc/syntax-reference.md#far-storage-var-far")
}

/// A pinned `var dp` whose slot runs past the end of the direct page.
fn pinned_slot_outside_page(
    var: &crate::ast::VarDeclaration,
    span: Span,
    address: u32,
    bytes: u32,
) -> Diagnostic {
    let last = address.saturating_add(bytes.max(1)) - 1;
    Diagnostic::error(
        DiagnosticCode::DIRECT_PAGE_OVERFLOW,
        format!(
            "`{}` is pinned at direct-page offset ${address:02X}, and its {bytes} bytes \
             run to ${last:02X}",
            var.name.text
        ),
        Label::new(
            var.address.as_ref().map_or(span, |address| address.span),
            "the direct page ends at $FF",
        ),
    )
    .with_note(
        "a pinned slot is an offset into the 256-byte direct page, not an absolute \
         address: the D register decides where that page is at run time",
    )
    .with_help("pin it lower, or drop `dp` and let the variable live in a segment")
    .with_docs("doc/syntax-reference.md#direct-page-storage-var-dp")
}

/// The unit has asked the direct page for more than it has.
fn direct_page_overflow(var: &crate::ast::VarDeclaration, span: Span, claimed: u32) -> Diagnostic {
    Diagnostic::error(
        DiagnosticCode::DIRECT_PAGE_OVERFLOW,
        format!("this unit's `dp` declarations need {claimed} bytes of the 256-byte direct page"),
        Label::new(span, "this is the declaration that exceeds it"),
    )
    .with_secondary(Label::new(var.name.span, "declared here"))
    .with_note(
        "the direct page is 256 bytes and no linker configuration makes it larger, so \
         the overflow is decided at the declaration rather than at the link",
    )
    .with_help(
        "drop `dp` from the declarations that do not need one-byte addressing, or make \
         them smaller",
    )
    .with_docs("doc/syntax-reference.md#direct-page-storage-var-dp")
}

/// The unit's call graph as adjacency lists over `functions`' indices.
///
/// A callee this unit does not define is not a node: it contributes the
/// language rule at the call site, or the workspace's answer for it.
fn call_graph(functions: &[FunctionInfo]) -> Vec<Vec<usize>> {
    let index_of: std::collections::HashMap<SymbolId, usize> = functions
        .iter()
        .enumerate()
        .map(|(index, info)| (info.symbol, index))
        .collect();
    functions
        .iter()
        .map(|info| {
            info.callees
                .iter()
                .filter_map(|callee| index_of.get(callee).copied())
                .collect()
        })
        .collect()
}

/// The [`ItemKind::Func`] one [`FunctionInfo`] describes.
fn function_of<'a>(file: &'a AstFile, info: &FunctionInfo) -> Option<&'a Function> {
    match &file.items.get(info.item_index)?.kind {
        ItemKind::Func(func) => Some(func),
        _ => None,
    }
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

/// How to give an axis an exit a caller can rely on.
///
/// A `naked` section is offered only the second half: declaring one is
/// [`DiagnosticCode::NAKED_EXIT_CONTRACT`], so suggesting it would be
/// advising an edit that produces another error.
fn exit_width_help(func: &Function, axis: width::Axis) -> String {
    if func.kind == FunctionKind::Naked {
        return "re-establish the width before control leaves the section".to_owned();
    }
    format!(
        "declare the exit width with `-> {}`, or re-establish it before returning",
        axis.annotation(RegisterWidth::W8)
    )
}

/// A `->` clause on a `naked` section.
fn naked_exit_contract(name: &crate::ast::Name, clause: Span) -> Diagnostic {
    Diagnostic::error(
        DiagnosticCode::NAKED_EXIT_CONTRACT,
        format!("`{}` is `naked`, so it has no exit to declare", name.text),
        Label::new(clause, "an exit contract describes what a return leaves"),
    )
    .with_secondary(Label::new(name.span, "declared `naked` here"))
    .with_note(
        "a `naked` section emits no `rts`, so control leaves it by whatever the body \
                     does last rather than by returning to the caller",
    )
    .with_help("drop the clause, or make the section a `func` so that it returns")
    .with_fix(Fix {
        title: "drop the exit contract".to_owned(),
        applicability: Applicability::MachineApplicable,
        edits: vec![TextEdit::delete(clause)],
    })
    .with_docs("doc/syntax-reference.md#naked")
}

/// An alias parameter on a section that is not `inline`.
///
/// An alias binds during macro expansion, which only an `inline` has: a
/// `JSR` hands over registers and nothing else, because K816 has no
/// parameter-passing mechanism.
fn alias_not_inline(name: &crate::ast::Name, func: &Function) -> Diagnostic {
    Diagnostic::error(
        DiagnosticCode::ALIAS_NOT_INLINE,
        format!(
            "`{}` can only declare an alias on an `inline` section",
            func.name.text
        ),
        Label::new(name.span, "an alias binds during expansion"),
    )
    .with_secondary(Label::new(func.name.span, "this section is not `inline`"))
    .with_note(
        "a call hands over registers and immediates only, because a `JSR` has \
         nowhere else to put an argument",
    )
    .with_help("pass a register instead, or make the section `inline`")
    .with_docs("doc/syntax-reference.md#why-aliases-are-inline-only")
}

/// A call site whose register flow disagrees with the callee's declaration.
fn call_flow_mismatch(
    span: Span,
    title: impl Into<String>,
    help: impl Into<String>,
    declared: Option<Span>,
) -> Diagnostic {
    let mut diagnostic = Diagnostic::error(
        DiagnosticCode::CALL_FLOW_MISMATCH,
        title,
        Label::new(span, "the register flow here"),
    )
    .with_help(help)
    .with_docs("doc/syntax-reference.md#code-sections");
    if let Some(declared) = declared {
        diagnostic = diagnostic.with_secondary(Label::new(declared, "declared here"));
    }
    diagnostic
}

/// The route from `from` back to `goal`, or `None` when there is none.
///
/// The path is returned rather than a bare yes, because "`a` expands into
/// itself" is unhelpful without the sections in between.
fn reaches(
    from: SymbolId,
    goal: SymbolId,
    edges: &BTreeMap<SymbolId, Vec<SymbolId>>,
    seen: &mut BTreeSet<SymbolId>,
) -> Option<Vec<SymbolId>> {
    for &next in edges.get(&from).map_or(&[][..], Vec::as_slice) {
        if next == goal {
            return Some(vec![from, goal]);
        }
        if !seen.insert(next) {
            continue;
        }
        if let Some(mut route) = reaches(next, goal, edges, seen) {
            route.insert(0, from);
            return Some(route);
        }
    }
    None
}

/// The K65 spelling of one register.
fn register_name(register: RegisterKind) -> &'static str {
    match register {
        RegisterKind::A => "a",
        RegisterKind::X => "x",
        RegisterKind::Y => "y",
        RegisterKind::S => "s",
        RegisterKind::D => "d",
        RegisterKind::B => "b",
        RegisterKind::P => "p",
        RegisterKind::C => "c",
    }
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
