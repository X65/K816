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
    EvalExprKind, Expr, ExprKind, Function, FunctionKind, HlaStatement, IllegalOpcode, ItemKind,
    RegisterKind, ReservedKind, Statement, StatementKind,
};
use std::collections::{BTreeMap, BTreeSet};

use crate::abi;
use crate::compat;
use crate::diagnostic::{Applicability, Diagnostic, DiagnosticCode, Fix, Label, TextEdit};
use crate::effects::ControlEffect;
use crate::eval;
use crate::isa::RegisterWidth;
use crate::layout;
use crate::linkage::Linkage;
use crate::op::{ExitContract, ModeState, WidthContract};
use crate::resolve::{AnonymousBlocks, Classified, DataSite, Resolved, Resolver, data_site};
use crate::source::{Provenance, Span};
use crate::symbol::{
    AddressSize, DataWidth, FieldShape, LabelScope, LayoutId, SymbolId, SymbolKind, SymbolTable,
};
use crate::width::{self, Axis, WidthAnalysis};

/// Where a `code { }` subblock sits: the item index of its data block, and
/// which subblock of that block it is.
pub type DataCodeId = (usize, usize);

/// Where the width states of a body come from.
///
/// A function has an entry contract, a name, and callers; a `code { }`
/// subblock of a data block has none of those and is still a body that the
/// solver walks. One enum rather than an `Option<SymbolId>` plus a
/// convention, so that every reader has to say which it means.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum WidthScope {
    Function(SymbolId),
    DataCode(DataCodeId),
}

/// The result of analyzing one file: symbols plus per-function facts.
#[derive(Debug, Default)]
pub struct SemanticModel {
    pub symbols: SymbolTable,
    /// The program the preprocessor left, when it removed anything.
    ///
    /// `None` for a file with no directive in it, which is the common case
    /// and where the parsed file *is* the program. Everything after analysis
    /// — lowering above all — must walk the same tree analysis did, because
    /// a `var`'s storage and a function's facts are keyed by position in it;
    /// [`SemanticModel::program`] is how a caller gets it without having to
    /// remember which case it is in.
    pub preprocessed: Option<AstFile>,
    /// What the compile-time evaluator answered, keyed by where it was asked.
    ///
    /// Analysis runs every bracket exactly once, in source order, and every
    /// stage after it looks the answer up. That is not an optimization: an
    /// evaluation assigns names, can `print`, and spends a step budget, and
    /// lowering re-resolves data blocks analysis has already resolved — so a
    /// second run would be a second `print` and, where a bracket assigns, a
    /// different answer.
    pub eval: eval::Outcome,
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
    /// Global labels another unit in the workspace refers to.
    ///
    /// Published because two consumers need the same answer: the width solver
    /// treats such a label as reachable from a width nothing here
    /// establishes, and the optimizer's block graph treats its block as
    /// reachable from an edge it cannot see. A global label nobody else names
    /// is an ordinary label to both.
    pub externally_reachable: BTreeSet<SymbolId>,
    /// The width analysis of each `code { }` subblock of a top-level data
    /// block, keyed by where it sits.
    ///
    /// A function's analysis is keyed by its symbol; a subblock has none —
    /// it is a body with no name, no contract, and no caller — so it is
    /// keyed by position instead. Both are the same kind of answer to the
    /// same question, which is why lowering reads one or the other and does
    /// nothing differently after that.
    pub data_code: BTreeMap<DataCodeId, WidthAnalysis>,
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
    /// Filler bytes that must precede this reservation in the pool.
    ///
    /// Direct-page only, and it is what keeps an allocated slot off a *pinned*
    /// one: a pinned declaration is an equate and reserves nothing, so without
    /// these bytes in the way ld65 would hand out an address that is already
    /// spoken for. Zero everywhere else.
    pub padding: u32,
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
    /// Set when this section's caller is not a call site, so its widths come
    /// from the entry ABI rather than from its own contract.
    pub entry: Option<abi::EntryKind>,
    /// What callers may believe about their widths after calling it.
    ///
    /// Consumed by call-site lowering, by the workspace exit classification,
    /// and by native-call handling. Until exit inference lands this is the
    /// language's `Unknown ⇒ Preserve` fallback for every function.
    pub exit: ExitContract,
    /// Which width axes the body observes *directly*.
    ///
    /// The seed of [`Self::observes`]; see [`WidthNeeds`] for why "observes"
    /// and not "has a width-dependent immediate".
    pub needs: WidthNeeds,
    /// Which width axes calling this function observes, transitively.
    ///
    /// The workspace answer where there is one, and this unit's own fixpoint
    /// otherwise. The optimizer asks it one question at every call site: is
    /// there anything reachable from this call that would behave differently
    /// if the caller stopped establishing the axis.
    pub observes: WidthNeeds,
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

/// Which width axes a function body *observes*.
///
/// Not "which axes its immediates depend on". The M and X flags govern far
/// more than immediate lengths: `lda addr` and `sta addr` read M and change
/// width with it, `inx` and `cpx` read X, and every push, pull, compare, and
/// width-dependent transfer reads one or the other. A seed counting
/// `ImmediateM`/`ImmediateX` instructions — which is what
/// `doc/register-width-aware-syntax.md` "Dead mode elimination" step 1 says —
/// licenses deleting the `rep #$20` above an `lda addr`, turning a 16-bit
/// load into an 8-bit one. That is not a lost optimization, it is a different
/// program (`plans/05-optimizer.md` decision 2).
///
/// So an axis is observed when some instruction in the body reads its flag,
/// or when the body transfers to code K816 cannot see — an unresolvable
/// branch target, or a call whose target is not a function this workspace
/// defines — which may observe anything.
///
/// This is the *direct* seed. Observation through the call graph is the
/// optimizer's transitive fixpoint over it, published per function as
/// [`crate::width::CalleeContract::observes`].
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct WidthNeeds {
    /// Some instruction in the body reads the M flag, or the body reaches
    /// code K816 cannot see.
    pub m: bool,
    /// The same, for the X flag.
    pub x: bool,
}

impl WidthNeeds {
    /// Everything is observed: what a transfer to code K816 cannot see says.
    pub fn opaque() -> Self {
        Self { m: true, x: true }
    }

    /// The axes either side observes.
    pub fn union(self, other: Self) -> Self {
        Self {
            m: self.m || other.m,
            x: self.x || other.x,
        }
    }

    /// The axis named by `axis`.
    pub fn axis(self, axis: crate::width::Axis) -> bool {
        match axis {
            crate::width::Axis::Accumulator => self.m,
            crate::width::Axis::Index => self.x,
        }
    }

    /// The same, for assignment.
    pub fn set(&mut self, axis: crate::width::Axis, observed: bool) {
        match axis {
            crate::width::Axis::Accumulator => self.m = observed,
            crate::width::Axis::Index => self.x = observed,
        }
    }
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
    // The preprocessor runs first and nothing below it ever sees a
    // directive. It has to run before a single symbol is declared: a `func`
    // inside a region that is not taken must not exist, or the ordinary
    // `#if / func f / #else / func f / #endif` is a duplicate definition
    // (`plans/06-compatibility.md` decision 9, `preprocess.rs`).
    let applied = crate::preprocess::apply(file);
    let mut preprocessed = match applied.program {
        std::borrow::Cow::Borrowed(_) => None,
        std::borrow::Cow::Owned(program) => Some(program),
    };
    // An `inline` another unit defines and this one expands joins this
    // program, here, before a symbol exists. It is the one section kind ld65
    // cannot resolve — it has no address — so what crosses the boundary is
    // the body, and copying it in is what makes every stage below this line
    // see an ordinary local `inline`: one declaration pass, one width
    // analysis, one expansion in lowering, and the imports the body needs
    // derived from this unit like any other (`postmortem.md` §G.9.4).
    //
    // After the preprocessor and before `declare_placed_symbols`: a body
    // inside a region this unit did not take must not arrive, and a body that
    // does arrive must be declared with the rest.
    //
    // What is kept is each borrowed section's name, its declaration span, and
    // its whole extent. The extent is what decides *which* borrowed section a
    // silenced complaint belongs to, below: two sections from one file are two
    // answers, and a mistake in one is no reason to refuse the other.
    let mut borrowed_sections: Vec<(String, Span, Span)> = Vec::new();
    // Sections this unit expands whose declaration cannot be copied at all,
    // which is a parse the owner had to recover inside. They are reported
    // below with the same line a body that does not compile gets, because
    // that is the same news: the section is here and cannot be expanded.
    let mut unusable: Vec<(String, Span)> = Vec::new();
    if let Some(linkage) = linkage {
        let borrowed = linkage.inline_expansions(preprocessed.as_ref().unwrap_or(file));
        unusable = borrowed.unusable;
        if !borrowed.items.is_empty() {
            borrowed_sections.extend(borrowed.items.iter().filter_map(|item| match &item.kind {
                ItemKind::Func(func) => Some((func.name.text.clone(), func.name.span, item.span)),
                _ => None,
            }));
            let mut program = preprocessed.take().unwrap_or_else(|| file.clone());
            program.items.extend(borrowed.items);
            preprocessed = Some(program);
        }
    }
    let file = preprocessed.as_ref().unwrap_or(file);
    let effects = applied.effects;
    let mut analysis = Analysis {
        model: SemanticModel::default(),
        diagnostics: applied.diagnostics,
        current_function: None,
        loops: 0,
        unit: unit_name.to_owned(),
        anonymous_blocks: AnonymousBlocks::default(),
        defaults: WidthContract::default(),
        borrowed_from: borrowed_sections
            .iter()
            .map(|(_, _, extent)| extent.file)
            .collect(),
        observation: Vec::new(),
        workspace_contracts: BTreeMap::new(),
        workspace_recursive: BTreeSet::new(),
        direct_page: layout::DirectPage::default(),
        direct_page_reported: false,
        eval_env: eval::Environment::default(),
        eval_block: 0,
        eval_effects: effects,
    };
    analysis.declare_placed_symbols(file);
    // Module defaults are file-wide, so they are known before any function's
    // contract is folded.
    analysis.defaults = analysis.module_defaults(file);
    // What the other units of the workspace assembled, before this unit's
    // brackets run — because they run inside `declare_items`, and the import
    // pass below is far too late for them. A foreign *symbol* still arrives
    // there and still loses to a local declaration; what is seeded here is
    // only the bytes, which a local `data` of the same name shadows anyway
    // (a section value resolves through the symbol table first).
    if let Some(linkage) = linkage {
        for (name, bytes) in linkage.sections() {
            analysis
                .model
                .eval
                .record_foreign_section(name, bytes.clone());
        }
        // And the workspace's `const` values, for the same reason and with
        // the same rule: a `const` folds, so its value is a compile-time fact
        // wherever it was written, and the symbol table is asked first so a
        // local declaration still wins the name. A *borrowed* `inline` body
        // needs this — its brackets read its own unit's `const`s, and
        // `declare_imports` runs after the brackets have already run.
        for (name, definition) in linkage.definitions() {
            if let SymbolKind::Const { value } = definition.kind {
                analysis
                    .model
                    .eval
                    .record_foreign_const(name, f64::from(value));
            }
        }
    }
    analysis.declare_items(file);
    analysis.place_direct_page();
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
    // A borrowed body's own mistakes belong to the unit that wrote it, and
    // that unit is in this workspace and reports them itself. What stays is
    // what *borrowing* revealed: a name the defining unit resolves and this
    // one cannot is the case this filter must not swallow, and it does not,
    // because the owner never made that complaint.
    if let Some(linkage) = linkage
        && !borrowed_sections.is_empty()
    {
        let mut silenced: Vec<Span> = Vec::new();
        analysis.diagnostics.retain(|diagnostic| {
            let at = diagnostic.primary.span;
            let inside = borrowed_sections
                .iter()
                .any(|(_, _, extent)| extent.file == at.file && extent.contains(at.start));
            if !inside || !linkage.reported_by_owner(diagnostic) {
                return true;
            }
            if diagnostic.severity == crate::diagnostic::Severity::Error {
                silenced.push(at);
            }
            false
        });
        // Dropping an *error* without putting one back would leave this unit
        // emitting an expansion of a body nothing checked, and lowering would
        // meet the hole as `K816-E0900`. One line per borrowed section that
        // does not compile says where the real complaints are.
        for (name, declaration, extent) in &borrowed_sections {
            if silenced
                .iter()
                .any(|at| at.file == extent.file && extent.contains(at.start))
            {
                analysis.diagnostics.push(borrowed_inline_unusable(
                    name,
                    *declaration,
                    linkage.unit_of(extent.file),
                ));
            }
        }
    }
    if let Some(linkage) = linkage {
        for (name, declaration) in &unusable {
            analysis.diagnostics.push(borrowed_inline_unusable(
                name,
                *declaration,
                linkage.unit_of(declaration.file),
            ));
        }
    }
    analysis.model.preprocessed = preprocessed;
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
    /// The files this unit borrowed an `inline` body from.
    ///
    /// A borrowed section's contract is finished when it arrives: the module
    /// defaults that apply to it are its *own* unit's, folded into the copy
    /// there, and an axis its declaration leaves unspecified is unspecified
    /// at home too. Merging this unit's defaults into it would give a body a
    /// width nobody wrote and its caller a bridge nobody asked for
    /// (`postmortem.md` §G.10.2).
    borrowed_from: BTreeSet<crate::source::FileId>,
    /// Every function's contract as the workspace classified it, translated
    /// into this unit's symbol ids.
    ///
    /// Empty when this unit is analyzed alone, in which case the unit's own
    /// classification is the authority.
    workspace_contracts: BTreeMap<SymbolId, width::CalleeContract>,
    /// Functions the workspace call graph put in a cycle, including ones
    /// that only close through another unit.
    workspace_recursive: BTreeSet<SymbolId>,
    /// This unit's own transitive observation answer, per function index —
    /// the fallback for a function the workspace did not classify.
    observation: Vec<WidthNeeds>,
    /// The unit's running claim on the 256-byte direct page.
    direct_page: layout::DirectPage,
    /// True once the pool has overflowed and said so, so the declarations
    /// after it do not each repeat the news.
    direct_page_reported: bool,
    /// The evaluator variables assigned so far, in source order.
    eval_env: eval::Environment,
    /// Which bracket is being evaluated, so a name assigned in one block and
    /// again in another is caught.
    eval_block: eval::BlockId,
    /// What the preprocessor's live `#if` conditions assigned, waiting to be
    /// installed at the item each one is anchored to.
    ///
    /// A condition is evaluated by `preprocess.rs` and by nothing else — the
    /// directive is deleted before this pass begins — so its assignments have
    /// to be carried here rather than re-derived (`postmortem.md` §G.6.8).
    eval_effects: Vec<crate::preprocess::Effect>,
}

/// A preprocessor directive that survived `preprocess::apply`.
///
/// Not a user error: every directive is deleted before analysis begins, so
/// one arriving here means a caller walked the parsed tree instead of
/// [`SemanticModel::program`].
fn unpreprocessed(span: Span) -> Diagnostic {
    Diagnostic::error(
        DiagnosticCode::INTERNAL,
        "internal: a preprocessor directive reached semantic analysis",
        Label::new(span, "the preprocessor should have removed this"),
    )
}

impl SemanticModel {
    /// The program this model describes: the parsed file, or what the
    /// preprocessor left of it.
    ///
    /// Every stage after analysis walks it through this call rather than
    /// reaching for the parse result, because a `var`'s storage
    /// ([`SemanticModel::var_storage`]) and a function's facts are keyed by
    /// position in the tree analysis walked. Handing lowering the *parsed*
    /// tree of a file that uses the preprocessor would shift every one of
    /// those keys by the number of items a dead region removed.
    pub fn program<'a>(&'a self, parsed: &'a AstFile) -> &'a AstFile {
        self.preprocessed.as_ref().unwrap_or(parsed)
    }
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
                    self.declare_labels_in(symbol, &func.body, func.kind);
                    if func.kind == FunctionKind::Inline {
                        self.check_inline_label_values(&func.body);
                    }
                }
                ItemKind::Data(data) => self.declare_data_labels(data),
                // A top-level `binary` is the section it describes, so its
                // name is a data label like any other block's.
                ItemKind::Binary(declaration) => {
                    self.declare_data_labels(&crate::resolve::binary_block(declaration, item.span));
                }
                ItemKind::Var(_)
                | ItemKind::Const(_)
                | ItemKind::Segment(_)
                | ItemKind::EvalBlock(_)
                | ItemKind::EvalFunc(_)
                | ItemKind::Preprocessor(_)
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

    /// Refuses an `inline` body that uses one of its own labels as a *value*.
    ///
    /// Expansion turns those labels into the enclosing function's unnamed
    /// targets, which is what keeps two use sites from colliding. A transfer
    /// to one is rewritten to the anchor and works; an *expression*
    /// mentioning one cannot be, because the lowered expression vocabulary
    /// names symbols and the symbol is exactly what expansion removed. The
    /// operand that came out named a definition that no longer existed, and
    /// ca65 refused the file — so it is refused here instead, where the
    /// message can say why.
    ///
    /// A branch, a `goto`, and a plain `lda .spot` are all fine: those carry
    /// the label as a *target*, which expansion does rewrite.
    fn check_inline_label_values(&mut self, block: &crate::ast::Block) {
        for statement in &block.statements {
            match &statement.kind {
                StatementKind::Native(native) => {
                    if let Some(operand) = &native.operand {
                        for (expr, target) in native_operand_expressions(operand) {
                            self.report_label_value(expr, target);
                        }
                    }
                }
                StatementKind::Hla(hla) => {
                    for (expr, target) in hla_expressions(hla) {
                        self.report_label_value(expr, target);
                    }
                }
                StatementKind::WidthBlock(width) => self.check_inline_label_values(&width.body),
                StatementKind::Nocross(body) => self.check_inline_label_values(body),
                StatementKind::Loop(loop_statement) => {
                    self.check_inline_label_values(&loop_statement.body);
                }
                StatementKind::Conditional(conditional) => {
                    self.check_inline_label_values(&conditional.body);
                    if let Some(otherwise) = &conditional.otherwise {
                        self.check_inline_label_values(otherwise);
                    }
                }
                _ => {}
            }
        }
    }

    /// Reports every `.name` inside `expr` that expansion cannot rewrite.
    ///
    /// `target` marks the one shape it can: a bare `.spot` as the whole of an
    /// *address* operand, which is what `lda .spot` and every branch carry,
    /// and which becomes a reference to the anchor. A bare `.spot` as an
    /// *immediate* is not that shape — it is the label's address as a number
    /// — and neither is anything compound.
    fn report_label_value(&mut self, expr: &Expr, target: bool) {
        if target && matches!(expr.kind, ExprKind::LocalSymbol(_)) {
            return;
        }
        let mut found = Vec::new();
        collect_local_symbols(expr, &mut found);
        for name in found {
            self.diagnostics.push(inline_label_as_a_value(name));
        }
    }

    /// Declares the labels of one function body. A global label is visible
    /// across the unit; a `.name` label is scoped to `owner`, so two
    /// functions may use the same one.
    ///
    /// An `inline` body may declare only the local kind. Its labels are
    /// renamed per expansion — they become the enclosing function's unnamed
    /// targets — so a *global* one would be a unit-level name with no
    /// definition left to point at, exported and undefined, and two use sites
    /// would ask for two of it.
    fn declare_labels_in(
        &mut self,
        owner: SymbolId,
        block: &crate::ast::Block,
        kind: FunctionKind,
    ) {
        for statement in &block.statements {
            match &statement.kind {
                StatementKind::Label(label) => {
                    if label.name.provenance == Provenance::Synthetic {
                        continue;
                    }
                    if kind == FunctionKind::Inline && label.scope == crate::ast::LabelScope::Global
                    {
                        self.diagnostics.push(global_label_in_inline(label));
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
                StatementKind::WidthBlock(width) => {
                    self.declare_labels_in(owner, &width.body, kind)
                }
                StatementKind::Nocross(body) => self.declare_labels_in(owner, body, kind),
                StatementKind::Loop(loop_statement) => {
                    self.declare_labels_in(owner, &loop_statement.body, kind);
                }
                StatementKind::Conditional(conditional) => {
                    self.declare_labels_in(owner, &conditional.body, kind);
                    if let Some(otherwise) = &conditional.otherwise {
                        self.declare_labels_in(owner, otherwise, kind);
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
                | StatementKind::Align(_)
                | StatementKind::Address(_)
                | StatementKind::Reserved(_)
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
        // Through a `nocross { … }` group: a label inside one is a label of
        // the section, at a brace's remove.
        for entry in crate::ast::flat_data_entries(&data.entries) {
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
        // A condition after the last surviving item is anchored past the end,
        // so the drain runs once more when the walk is over.
        let last = file.items.len();
        for (item_index, item) in file.items.iter().enumerate() {
            self.install_eval_effects(item_index);

            // Every evaluator bracket this item contains runs here, before
            // the item itself is processed, because the two directions of
            // the one namespace meet at exactly this point: a bracket reads
            // the `const` values declared above it, and the names a bracket
            // assigns are declared as constants the K65 layer below it can
            // read. `plans/06-compatibility.md` decision 4.
            self.run_evaluator(item);
            match &item.kind {
                ItemKind::Var(var) => self.declare_var(item_index, item.span, var),
                ItemKind::Const(declaration) => self.declare_const_group(declaration),
                ItemKind::Data(data) => {
                    self.resolve_section(data, item.span);
                    self.report_reserved_in_data(data);
                    self.check_data_code(item_index, data);
                }
                // The section a top-level `binary` describes, resolved where
                // it stands so that a read below it has bytes.
                ItemKind::Binary(declaration) => {
                    let block = crate::resolve::binary_block(declaration, item.span);
                    self.resolve_section(&block, item.span);
                }
                // The bracket already ran, above. What is left is the
                // reserved-function report, which is structural: `rnd()` in
                // a branch this evaluation did not take is still `rnd()`.
                ItemKind::EvalBlock(expression) => self.report_reserved_in_expr(expression),
                // Declared by `run_evaluator`, in source order, because the
                // original resolves a call at the point it is written: a call
                // above the definition does not compile there.
                ItemKind::EvalFunc(_) => {}
                // `preprocess::apply` ran before this pass and removed every
                // directive, live region and dead alike. One arriving here
                // is a compiler bug, not a source the user has to fix.
                ItemKind::Preprocessor(_) => self.diagnostics.push(unpreprocessed(item.span)),
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
                    let contract = if self.borrowed_from.contains(&func.name.span.file) {
                        declared
                    } else {
                        width::merge_defaults(self.defaults, declared)
                    };
                    let entry = entry_kind(func);
                    if let Some(entry) = entry {
                        self.check_entry_abi(func, entry, contract);
                    }
                    self.model.functions.push(FunctionInfo {
                        symbol,
                        item_index,
                        contract,
                        entry,
                        // Nothing derives an exit yet, so every function
                        // takes the documented fallback and says so.
                        exit: ExitContract::preserving(),
                        exit_is_assumed: true,
                        needs: WidthNeeds::default(),
                        observes: WidthNeeds::default(),
                        callees: Vec::new(),
                    });
                }
                ItemKind::Error => {}
            }
        }
        self.install_eval_effects(last);
    }

    /// Runs every evaluator invocation this item contains, in source order.
    ///
    /// The pass is here rather than in `resolve.rs` because evaluation has
    /// effects: it assigns names, it can `print`, and it spends a step
    /// budget. Running a bracket where its value happens to be *needed* would
    /// run some of them twice — lowering re-resolves every data block — and
    /// would run them in an order the source does not have. So each bracket
    /// runs once, here, and everything downstream reads
    /// [`SemanticModel::eval`].
    fn run_evaluator(&mut self, item: &crate::ast::Item) {
        if let ItemKind::EvalFunc(function) = &item.kind {
            self.declare_eval_function(function);
            return;
        }
        let mut sites = Vec::new();
        eval::sites_of_item(item, &mut sites);
        for site in sites {
            match site {
                eval::Site::Bracket(body) => self.run_bracket(body),
                // A loop body's bounds are K65 expressions and are not known
                // until a resolver folds them, so what is recorded is the
                // environment it would have seen.
                eval::Site::Deferred { span, body: _ } => {
                    let environment = self.eval_env.clone();
                    self.model.eval.record_deferred(span, environment);
                }
            }
        }
    }

    /// Installs what a live `#if` condition assigned, at the item the
    /// preprocessor anchored it to.
    ///
    /// The name becomes a compiler constant, exactly as one a bracket
    /// assigned does, and it is bound in the evaluator environment so a
    /// bracket below can read it by either route. Each gets a block of its
    /// own, so a bracket further down that assigns the same name is
    /// `EVAL_REASSIGNED` — the rule that applies between any two blocks.
    fn install_eval_effects(&mut self, item_index: usize) {
        while self
            .eval_effects
            .first()
            .is_some_and(|effect| effect.anchor <= item_index)
        {
            let effect = self.eval_effects.remove(0);
            self.eval_block += 1;
            self.eval_env
                .bind(&effect.name, effect.value.clone(), self.eval_block);
            if let Some(number) = effect.value.number() {
                let value = eval::to_integer(number) as u32;
                self.declare(&effect.name, SymbolKind::Const { value }, effect.span);
            }
        }
    }

    /// Evaluates one bracket and records what it produced.
    fn run_bracket(&mut self, body: &crate::ast::EvalExpr) {
        if eval::is_recovered(body) {
            return;
        }
        self.eval_block += 1;
        let mut environment = std::mem::take(&mut self.eval_env);
        let mut interpreter = eval::Interpreter::new(
            &mut environment,
            self.model.eval.functions(),
            &self.model.symbols,
            self.model.eval.sections(),
            self.eval_block,
        );
        let value = interpreter.evaluate(body);
        let notes = interpreter.take_notes();
        let fresh = environment.take_new_names();
        self.eval_env = environment;

        for note in notes {
            self.diagnostics.push(crate::resolve::eval_note(note));
        }
        // A name the evaluator assigned is a compiler constant from here
        // down, which is what makes `[ FOUR = 4 ]` above a data block that
        // writes `FOUR` mean anything. The value crosses at the integer
        // boundary and wraps into the `const` representation exactly as
        // `const NEG = 0 - 1` already does.
        for (name, assigned) in fresh {
            let Some(number) = assigned.number() else {
                continue;
            };
            let value = eval::to_integer(number) as u32;
            self.declare(&name, SymbolKind::Const { value }, body.span);
        }
        match value {
            Ok(value) => self.model.eval.record(body.span, value),
            Err(error) => {
                if let Some(diagnostic) = crate::resolve::eval_diagnostic(error) {
                    self.diagnostics.push(diagnostic);
                }
            }
        }
    }

    /// Declares one `evalfunc`, refusing a name and arity already taken.
    fn declare_eval_function(&mut self, function: &crate::ast::EvalFunction) {
        let arity = function.parameters.len();
        if eval::Functions::is_builtin(&function.name.text, arity) {
            self.diagnostics.push(
                Diagnostic::error(
                    DiagnosticCode::DUPLICATE_SYMBOL,
                    format!(
                        "the evaluator library already has `{}` with {arity} argument{}",
                        function.name.text,
                        if arity == 1 { "" } else { "s" }
                    ),
                    Label::new(function.name.span, "this name is the library's"),
                )
                .with_note(
                    "the argument count is part of an evaluator function's identity, so a                      library name is free at every other count",
                )
                .with_docs("doc/syntax-reference.md#functions"),
            );
            return;
        }
        let declared = eval::UserFunction {
            parameters: function
                .parameters
                .iter()
                .map(|parameter| parameter.text.clone())
                .collect(),
            body: function.body.clone(),
            declaration: function.name.span,
        };
        if let Err(first) = self
            .model
            .eval
            .functions_mut()
            .declare(&function.name.text, declared)
        {
            self.diagnostics.push(
                Diagnostic::error(
                    DiagnosticCode::DUPLICATE_SYMBOL,
                    format!(
                        "`{}` already names an `evalfunc` with {arity} argument{}",
                        function.name.text,
                        if arity == 1 { "" } else { "s" }
                    ),
                    Label::new(function.name.span, "declared again here"),
                )
                .with_secondary(Label::new(first, "first declared here"))
                .with_docs("doc/syntax-reference.md#functions"),
            );
            return;
        }
        self.check_eval_body(&function.body);
    }

    /// Checks an `evalfunc` body where it is *written*, not where it runs.
    ///
    /// Two things are decided at a body's definition rather than at its call
    /// sites, and both follow from the original compiling the body on the
    /// spot (`Eval_Precompile`):
    ///
    /// - a call resolves against the functions declared **so far**, so a
    ///   function used above its definition does not resolve there and does
    ///   not resolve here;
    /// - a reserved function inside it is reserved whether or not any call
    ///   ever reaches it.
    ///
    /// Both used to be missed for the same reason: an `evalfunc` body is the
    /// one evaluator tree nothing evaluates where it stands, so a check that
    /// only ran during evaluation never saw it. `evalfunc hidden() [ rnd() ]`
    /// compiled clean and emitted no byte.
    fn check_eval_body(&mut self, body: &crate::ast::EvalExpr) {
        self.report_reserved_in_eval(body);
        let mut calls = Vec::new();
        eval::calls_in(body, &mut calls);
        for (name, arity) in calls {
            if self.model.eval.functions().resolves(&name.text, arity) {
                continue;
            }
            self.diagnostics.push(
                Diagnostic::error(
                    DiagnosticCode::UNDECLARED_SYMBOL,
                    format!(
                        "no evaluator function `{}` takes {arity} argument{} here",
                        name.text,
                        if arity == 1 { "" } else { "s" }
                    ),
                    Label::new(name.span, "not declared above this body"),
                )
                .with_note(
                    "an `evalfunc` body resolves its calls where it is written, so a \
                     function it uses has to be declared above it — and the argument \
                     count is part of what it names",
                )
                .with_docs("doc/syntax-reference.md#evalfunc"),
            );
        }
    }

    /// Checks the entry point's declared widths against the entry ABI.
    ///
    /// Every other function's contract is a requirement on its *callers*, and
    /// K816 satisfies it by bridging at each call site. `main`'s caller is
    /// the environment, which cannot be bridged — so an annotation on `main`
    /// is a claim *about* the ABI rather than a request of it, and one the
    /// ABI does not satisfy is simply false. Emitting a `REP` to make it true
    /// would be K816 choosing an ABI that belongs to the startup code
    /// (`plans/09-entry-abi.md`).
    ///
    /// Restating what the environment provides is fine: `func main @a8` is a
    /// true claim, and a true claim costs nothing to allow.
    fn check_entry_abi(&mut self, func: &Function, entry: abi::EntryKind, contract: WidthContract) {
        let environment = abi::environment();
        for axis in environment.mismatches(contract) {
            // The annotation is the thing the author can change, so it is
            // what the diagnostic points at. A width inherited from a module
            // default has none here, and the function's name is the nearest
            // honest place to put the caret.
            let site = entry_annotation(func, axis).unwrap_or(func.name.span);
            let mut diagnostic = entry_abi_mismatch(entry, axis, environment, site, func);
            if entry_annotation(func, axis).is_none() {
                diagnostic = diagnostic.with_note(
                    "this width comes from the module-level default, which reaches this \
                     section like any other function",
                );
            }
            self.diagnostics.push(diagnostic);
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
                self.model.externally_reachable.insert(id);
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
                abi::entry_state(info.entry, info.contract),
                &width::Context {
                    symbols: &self.model.symbols,
                    eval: &self.model.eval,
                    function: Some(info.symbol),
                    callees: &fallback,
                    externally_reachable: &self.model.externally_reachable,
                },
            );
            let function = &mut self.model.functions[index];
            function.needs = analysis.needs;
            function.callees.clone_from(&analysis.callees);
        }
        // The transitive answer over this unit's own call graph. A callee
        // another unit defines is not a node here, so its edge is dropped and
        // the seed says "observes everything" instead — the same conservative
        // answer the workspace pass gives a name nothing defines.
        let adjacency = call_graph(&self.model.functions);
        let seeds: Vec<WidthNeeds> = self
            .model
            .functions
            .iter()
            .zip(&adjacency)
            .map(|(info, edges)| {
                if edges.len() == info.callees.len() {
                    info.needs
                } else {
                    info.needs.union(WidthNeeds::opaque())
                }
            })
            .collect();
        self.observation = width::observation_fixpoint(&seeds, &adjacency);
        self.classify_exits(file);
    }

    /// Which axes calling `symbol` observes.
    ///
    /// The workspace answer wherever there is one — it saw the callees this
    /// unit cannot — and this unit's own fixpoint otherwise, which is what a
    /// file compiled alone has. The width solver reads neither; it is carried
    /// on [`width::CalleeContract`] so that one type answers everything a
    /// call site needs to know about its callee.
    fn observed(&self, symbol: SymbolId, index: usize) -> WidthNeeds {
        self.workspace_contracts
            .get(&symbol)
            .map_or(self.observation[index], |contract| contract.observes)
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
                    abi::entry_state(info.entry, info.contract),
                    &width::Context {
                        symbols: &self.model.symbols,
                        eval: &self.model.eval,
                        function: Some(info.symbol),
                        callees: &callees,
                        externally_reachable: &self.model.externally_reachable,
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

                let observes = self.observed(info.symbol, index);
                callees.insert(
                    info.symbol,
                    width::CalleeContract {
                        entry: info.contract,
                        exit,
                        observes,
                    },
                );
                let function = &mut self.model.functions[index];
                function.exit = exit;
                function.exit_is_assumed = assumed;
                function.observes = observes;
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
            // An `inline` is not a section: it has no address, and its body
            // is copied into each use site, so there is nothing for a
            // placement directive to place. Every other section kind — the
            // three the original's `Align` slot sits in — takes one.
            let placeable = func.kind != FunctionKind::Inline;
            self.check_block_at(WidthScope::Function(function.symbol), &func.body, placeable);
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
    fn check_block(&mut self, scope: WidthScope, block: &crate::ast::Block) {
        self.check_block_at(scope, block, false);
    }

    /// `is_section_body` is true for the one block that *is* a code section,
    /// which is the only place a placement directive can sit — and only as
    /// its first statement.
    ///
    /// The original's grammar puts the slot there and nowhere else: `Align`
    /// is one optional directive between `SecStart` and `Sequence`
    /// (`compiler.inc:121-136`). K816's grammar takes one anywhere in a body
    /// on purpose, so that one further down is refused with a sentence
    /// instead of `expected a statement` (decision 15).
    fn check_block_at(
        &mut self,
        scope: WidthScope,
        block: &crate::ast::Block,
        is_section_body: bool,
    ) {
        for (index, statement) in block.statements.iter().enumerate() {
            if matches!(
                statement.kind,
                StatementKind::Align(_) | StatementKind::Address(_)
            ) && !(is_section_body && index == 0)
            {
                let in_inline = matches!(
                    self.current_function
                        .map(|owner| self.model.symbols.kind(owner)),
                    Some(SymbolKind::Inline)
                );
                self.diagnostics
                    .push(placement_not_at_section_head(statement, in_inline));
            }
            let widths = self.widths_at(scope, statement);
            self.check_statement(statement, widths);
            // A nested body is ordinary code, checked with the state the
            // solver tracks inside it rather than skipped along with the
            // statement that opens it — a scoped width block's body at the
            // block's width, a conditional's and a loop's at theirs.
            match &statement.kind {
                StatementKind::WidthBlock(width) => self.check_block(scope, &width.body),
                StatementKind::Conditional(conditional) => {
                    self.check_block(scope, &conditional.body);
                    if let Some(otherwise) = &conditional.otherwise {
                        self.check_block(scope, otherwise);
                    }
                }
                StatementKind::Loop(loop_statement) => {
                    self.loops += 1;
                    self.check_block(scope, &loop_statement.body);
                    self.loops -= 1;
                }
                StatementKind::Nocross(body) => self.check_block(scope, body),
                // A `code { }` subblock of an inline data block is ordinary
                // code of the enclosing function: the solver flattened it
                // into the same graph, so it is checked in the same scope.
                StatementKind::Data(data) => {
                    for entry in crate::ast::flat_data_entries(&data.entries) {
                        if let DataEntryKind::Code { body, .. } = &entry.kind {
                            self.check_block(scope, body);
                        }
                    }
                }
                _ => {}
            }
        }
    }

    /// Analyzes the `code { }` subblocks of a *top-level* data block.
    ///
    /// They are bodies without a function: no entry contract, no caller, no
    /// name. So the solver runs over each one from an unknown entry state —
    /// which is the honest one, since nothing says what widths hold when
    /// control reaches bytes in a data block — and the result is keyed by
    /// position rather than by symbol.
    ///
    /// An *inline* data block's subblocks are not here: those are statements
    /// of the enclosing function, flattened into its graph by the solver and
    /// checked in its scope.
    fn check_data_code(&mut self, item: usize, data: &DataBlock) {
        // The *flattened* order, because that is the order the resolver
        // pushes subblocks into `DataOps::code` and the ordinal is an index
        // into that list. A walk that stopped at a `nocross { … }` group's
        // brace would skip the subblock inside it — leaving its statements
        // unanalysed — and hand every later subblock someone else's widths.
        for (ordinal, entry) in crate::ast::flat_data_entries(&data.entries)
            .into_iter()
            .filter(|entry| matches!(entry.kind, DataEntryKind::Code { .. }))
            .enumerate()
        {
            let DataEntryKind::Code { body, .. } = &entry.kind else {
                continue;
            };
            let id: DataCodeId = (item, ordinal);
            let (analysis, diagnostics) = width::analyze_function(
                body,
                ModeState::unknown(),
                &width::Context {
                    symbols: &self.model.symbols,
                    eval: &self.model.eval,
                    // No function, so no scope for `.name` labels — which
                    // `resolve.rs` reports where one is written.
                    function: None,
                    callees: &BTreeMap::new(),
                    externally_reachable: &BTreeSet::new(),
                },
            );
            self.diagnostics.extend(diagnostics);
            self.model.data_code.insert(id, analysis);
            self.check_block(WidthScope::DataCode(id), body);
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
    fn widths_at(&mut self, scope: WidthScope, statement: &Statement) -> ModeState {
        let analysis = match scope {
            WidthScope::Function(function) => self.model.width(function),
            WidthScope::DataCode(id) => self.model.data_code.get(&id),
        };
        match analysis.and_then(|analysis| analysis.before(statement.id)) {
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
            // The walk is shared with lowering, so the checks the directive
            // has to pass — a power-of-two boundary, an offset inside it, an
            // ld65 entry for `address` — are reported here exactly once and
            // by the same code that emits the operations.
            StatementKind::Align(align) => {
                let mut resolver = self.resolver();
                resolver.align_ops(align, statement.span);
                let resolved = resolver.finish();
                self.absorb(resolved);
            }
            StatementKind::Address(address) => {
                let section = self
                    .current_function
                    .map(|symbol| self.model.symbols.name(symbol).to_owned())
                    .unwrap_or_default();
                let mut resolver = self.resolver();
                resolver.code_address_op(address, statement.span, &section);
                let resolved = resolver.finish();
                self.absorb(resolved);
            }
            StatementKind::Reserved(reserved) => {
                let diagnostic = reserved_diagnostic(reserved);
                self.diagnostics.push(diagnostic);
            }
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
            StatementKind::Preprocessor(_) => {
                self.diagnostics.push(unpreprocessed(statement.span));
            }
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
                DataEntryKind::Align(align) => {
                    self.report_reserved_in_expr(&align.boundary);
                    if let Some(offset) = &align.offset {
                        self.report_reserved_in_expr(offset);
                    }
                }
                DataEntryKind::Address(expr) => self.report_reserved_in_expr(expr),
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
                DataEntryKind::NocrossGroup(entries) => {
                    self.report_reserved_in_data(&DataBlock {
                        name: None,
                        nocross: false,
                        entries: entries.clone(),
                    });
                }
                // The transform's body is a bracket, and a reserved call
                // inside it is as reserved as one anywhere else — and it has
                // to be reported *structurally*, because the interpreter
                // answers a reserved call with `Error::Reported` and the
                // transform then keeps the byte it was handed. Without this
                // arm `evaluator [ rnd() ]` compiled clean and emitted
                // untransformed bytes.
                DataEntryKind::Evaluator(body) => self.report_reserved_in_eval(body),
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
    ///
    /// The report is *structural*, not something the interpreter produces
    /// when it reaches one: `rnd()` in the arm of a ternary this evaluation
    /// did not take is still `rnd()`, and a program is no more portable for
    /// having hidden it behind a condition.
    fn report_reserved_in_eval(&mut self, expr: &EvalExpr) {
        match &expr.kind {
            EvalExprKind::ReservedCall(call) => {
                // `print` and `error` are implemented (decision 6), so they
                // are not reported here; the interpreter carries their
                // messages instead.
                if !matches!(call.name, EvalCallName::Print | EvalCallName::Error) {
                    self.diagnostics.push(reserved_eval_call(call));
                }
                for argument in &call.arguments {
                    self.report_reserved_in_eval(argument);
                }
            }
            EvalExprKind::MethodCall {
                base, arguments, ..
            } => {
                self.report_reserved_in_eval(base);
                for argument in arguments {
                    self.report_reserved_in_eval(argument);
                }
            }
            EvalExprKind::If {
                condition,
                consequence,
                alternative,
            } => {
                self.report_reserved_in_eval(condition);
                self.report_reserved_in_eval(consequence);
                if let Some(alternative) = alternative {
                    self.report_reserved_in_eval(alternative);
                }
            }
            EvalExprKind::While { condition, body } => {
                self.report_reserved_in_eval(condition);
                self.report_reserved_in_eval(body);
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
            EvalExprKind::Subscript { base, indices } => {
                self.report_reserved_in_eval(base);
                for index in indices {
                    self.report_reserved_in_eval(index);
                }
            }
            EvalExprKind::Member { base, .. } => self.report_reserved_in_eval(base),
            EvalExprKind::List(items) | EvalExprKind::Block(items) => {
                for item in items {
                    self.report_reserved_in_eval(item);
                }
            }
            EvalExprKind::Number { .. }
            | EvalExprKind::Float(_)
            | EvalExprKind::String(_)
            | EvalExprKind::Symbol(_)
            | EvalExprKind::Pi
            | EvalExprKind::Error => {}
        }
    }

    /// Declares every workspace name this unit does not define itself.
    ///
    /// The kind and address size are the *defining* unit's, because that is
    /// what ca65 needs to validate an addressing mode against the imported
    /// symbol. What does not cross is the field layout: a `LayoutId` names
    /// the declaring unit's arena, so [`SymbolKind::foreign`] strips it and a
    /// foreign `NAME.field` says the name has no fields rather than reading
    /// whichever layout sits at that index here.
    ///
    /// Declaring a name does not import it: only the ones this unit goes on
    /// to reference reach the emitted `.import` list.
    fn declare_imports(&mut self, linkage: &Linkage) {
        for (name, definition) in linkage.definitions() {
            self.model.symbols.declare_imported(
                name,
                definition.kind.foreign(),
                definition.declaration,
            );
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

    /// Resolves one top-level data section and records what it assembled to.
    ///
    /// The recording is what makes `index(sec, i)` and `size(sec)` answerable
    /// at all, and *where* it happens is the whole rule: analysis walks items
    /// in source order, so a section is in the table from the line after it
    /// closes and not before — which is exactly the point after which the
    /// original could answer about it too.
    ///
    /// A section nested in a function is deliberately not recorded: the
    /// original's `index` takes a name a `data` block declares, and one
    /// inside a `func` is not reachable from the top level where evaluation
    /// runs.
    fn resolve_section(&mut self, data: &DataBlock, span: Span) {
        let site = self.data_site(data, None);
        let mut resolver = self.resolver();
        let walked = resolver.data_block(data, span, &site);
        let resolved = resolver.finish();
        self.absorb(resolved);
        let Some(name) = &data.name else {
            return;
        };
        let Some(section) = self.model.symbols.lookup(&name.text) else {
            return;
        };
        let bytes = section_bytes(&walked, section);
        self.model.eval.record_section(&name.text, bytes);
        // A label written inside the section names a place, not a section.
        // Recording it says which mistake a read of it is, instead of
        // leaving it to look like a section written further down.
        for label in inner_labels(data) {
            if label != name.text {
                self.model
                    .eval
                    .record_section(&label, eval::SectionBytes::InsideASection);
            }
        }
    }

    /// A resolver over this unit's symbols, scoped to the function being
    /// checked. Analysis owns the diagnostics it produces.
    fn resolver(&self) -> Resolver<'_> {
        Resolver::new(&self.model.symbols, &self.model.eval).in_function(self.current_function)
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
        // A layout with no storage answers none of the questions below —
        // there is no address to fold, no extent to multiply, and nothing to
        // reserve — so it is declared by itself.
        if var.is_abstract {
            self.declare_abstract_var(var);
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
        // The raw span is deliberately *not* checked for zero. `var X[0]`
        // is a K65 declaration — the original accepts it, reserves nothing,
        // and carries the allocator on (`tests/compat/k65/zero-element-span`)
        // — so refusing it would be a narrowing decision 0 does not allow
        // where compatibility is this cheap. The forms below have no K65
        // spelling and keep the rule.
        let count = self.checked_count(count, var.count.as_ref());
        let declared = var.width.map(|width| layout::data_width(width.kind));
        // On a declaration with fields the `:byte` suffix is the *default
        // field width* and not a datum width of its own: the datum is the
        // whole layout, and a struct has no register to disagree with
        // (`doc/symbolic-subscripts.md`, "Default field width on the
        // symbol"). One suffix, two meanings, decided by whether a field
        // list follows.
        let (width, layout) = if var.fields.is_empty() {
            (declared, None)
        } else {
            (None, Some(self.build_layout(&var.fields, declared)))
        };
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

        let datum = layout::datum_size(width, layout, self.model.symbols.layouts());
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
                layout,
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
                    // Filled in once every declaration has been seen: a
                    // pinned slot declared *after* this one still has to be
                    // avoided, and nothing knows the whole pinned set until
                    // the pass is over.
                    padding: 0,
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

    /// Gives every allocated direct-page slot the filler that keeps it off a
    /// pinned one.
    ///
    /// A second pass, and it has to be: the pool is first-fit *around* the
    /// pinned addresses, and a pinned declaration may come after an allocated
    /// one in the file. Nothing knows the whole pinned set until every
    /// declaration has been read.
    ///
    /// Order matters twice over. The placement walks the reservations in
    /// declaration order because that is the order ld65 lays the pool out in,
    /// and the document says the allocator is first-fit in declaration order
    /// within a unit.
    fn place_direct_page(&mut self) {
        for reservation in self.model.reservations.values_mut() {
            if !reservation.direct_page {
                continue;
            }
            // `None` is the page being full, which `E0133` already reported
            // against the declaration that exhausted it. Emitting the slot
            // without filler is the least wrong thing left: the diagnostic
            // stops the compile before any of it reaches ld65.
            reservation.padding = self.direct_page.place(reservation.bytes).unwrap_or(0);
        }
    }

    /// Declares one `abstract var`: a packed field layout and nothing else.
    ///
    /// It is a kind of its own rather than a `var` with no address, because
    /// a linker-allocated `var` also has no address *yet* and the two must
    /// not be confused. What it may not do is ask for storage — no storage
    /// class, no initializer, no allocation count, no raw span — and the
    /// document rejects each of those (`doc/syntax-reference.md`, "Abstract
    /// symbolic layouts").
    fn declare_abstract_var(&mut self, var: &ast::VarDeclaration) {
        // One complaint per declaration. All four forms are the same
        // misunderstanding — that this reserves something — and a
        // declaration writing two of them would otherwise hear it twice.
        let offense = var
            .storage
            .map(|storage| (storage.span, "a storage class"))
            .or_else(|| var.address.as_ref().map(|expr| (expr.span, "an address")))
            .or_else(|| {
                var.count
                    .as_ref()
                    .map(|expr| (expr.span, "an allocation count"))
            })
            .or_else(|| var.size.as_ref().map(|expr| (expr.span, "a raw span")));
        if let Some((site, what)) = offense {
            self.diagnostics
                .push(abstract_declares_storage(var, site, what));
        } else if var.fields.is_empty() {
            self.diagnostics.push(abstract_has_no_fields(var));
        }

        let default = var.width.map(|width| layout::data_width(width.kind));
        let layout = self.build_layout(&var.fields, default);
        // Declared even when it was refused above: the fields are still what
        // the author wrote, and leaving the name undeclared would turn one
        // complaint into one more at every use of it.
        let _ = self.declare(
            &var.name.text,
            SymbolKind::AbstractVar { layout },
            var.name.span,
        );
    }

    /// Computes and interns the layout a field list declares.
    ///
    /// `default` is the width fields take when they name none — the
    /// declaration's own `:byte` suffix, which on a declaration that has
    /// fields means *default field width*. It carries into nested lists, and
    /// a nested field's own suffix replaces it for its sub-list, so
    /// `var v:word[ .a .b:byte[ .c .d ] ]` gives `.a` a word and `.c` a byte.
    ///
    /// Nested layouts are interned before the field that names them, which is
    /// what keeps a [`FieldShape::Nested`] identifier resolvable.
    fn build_layout(
        &mut self,
        fields: &[ast::FieldDeclaration],
        default: Option<DataWidth>,
    ) -> LayoutId {
        let mut packing = layout::Packing::default();
        for field in fields {
            if field.name.provenance == Provenance::Synthetic {
                continue; // Recovery invented the name; the syntax error stands.
            }
            let declared = field.width.map(|width| width.kind);
            let shape = if field.fields.is_empty() {
                FieldShape::Scalar(layout::field_width(declared, default))
            } else {
                let nested = declared.map(layout::data_width).or(default);
                FieldShape::Nested(self.build_layout(&field.fields, nested))
            };
            // A field's bracket is a count and not a span: an indexed field
            // element is `field_base + index * element_width`
            // (`doc/symbolic-subscripts.md`, "Semantics"), where a
            // declaration's `[N]` is a raw span the bracket steps in bytes.
            let count = field
                .size
                .as_ref()
                .and_then(|expr| self.constant(expr, "field element counts"));
            let count = self.checked_count(count, field.size.as_ref()).unwrap_or(1);

            let name = field.name.text.trim_start_matches('.');
            if let Some(previous) = packing.declared(name).map(|field| field.declaration) {
                self.diagnostics.push(duplicate_field(field, previous));
                continue;
            }
            packing.push(name, shape, count, field.span, self.model.symbols.layouts());
        }
        self.model.symbols.add_layout(packing.finish())
    }

    /// Rejects an element count of zero, and passes every other answer on.
    ///
    /// A count of zero describes storage with no elements: nothing is
    /// reserved, and every index into it is out of range the moment it is
    /// written. The declaration carries on without the multiplier, for the
    /// same reason a count that did not fold does — one bad number should not
    /// turn every use of the name into an undeclared-symbol error too.
    ///
    /// Asked of the `* count` allocation form and of a field's `[N]`, and
    /// **not** of a declaration's raw span: `var X[0]` is a K65 declaration
    /// and the original accepts it, where neither of these two has a K65
    /// spelling to be compatible with (`postmortem.md` §G.3.2).
    fn checked_count(&mut self, count: Option<u32>, expr: Option<&Expr>) -> Option<u32> {
        match (count, expr) {
            (Some(0), Some(expr)) => {
                self.diagnostics.push(zero_element_count(expr.span));
                None
            }
            _ => count,
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
            // Per entry, and before this entry's own value folds: a group is
            // shorthand for consecutive declarations, so `const A = 3, B =
            // [A + 1]` has to see `A` — which it cannot if every bracket in
            // the item ran before the item was processed.
            if let Some(expression) = &entry.value {
                let mut sites = Vec::new();
                eval::sites_of_expr(expression, &mut sites);
                for site in sites {
                    if let eval::Site::Bracket(body) = site {
                        self.run_bracket(body);
                    }
                }
            }
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

/// A placement directive that is not the first statement of a code section.
///
/// It is the same complaint `segment` gets in a data block — placement is a
/// property of where the section's bytes land, and there is no answer to give
/// once bytes are already placed before it.
fn placement_not_at_section_head(statement: &Statement, in_inline: bool) -> Diagnostic {
    let keyword = match statement.kind {
        StatementKind::Align(_) => "align",
        _ => "address",
    };
    if in_inline {
        return Diagnostic::error(
            DiagnosticCode::SEGMENT_PLACEMENT,
            format!("an `inline` cannot carry an `{keyword}` directive"),
            Label::new(statement.span, "an `inline` has no address to place"),
        )
        .with_note(
            "an `inline` body is copied into each use site rather than \
             placed, so there is no section here for a directive to move",
        )
        .with_help("make it a `func`, or place the section the body expands into")
        .with_docs("doc/syntax-reference.md#placement-directives");
    }
    Diagnostic::error(
        DiagnosticCode::SEGMENT_PLACEMENT,
        format!("an `{keyword}` directive must be the first statement of its code section"),
        Label::new(statement.span, "bytes are already placed before this"),
    )
    .with_note(
        "a placement directive moves the whole section, so it has to be read \
         before any of its bytes are emitted",
    )
    .with_help("move it to just after the section's opening `{`")
    .with_docs("doc/syntax-reference.md#placement-directives")
}

/// The `K816-K65-####` diagnostic for a reserved legacy construct.
fn reserved_diagnostic(reserved: &crate::ast::ReservedConstruct) -> Diagnostic {
    // Nothing here reads as "reserved, coming later" any more: every K65
    // construct K816 recognises and does not compile is refused for a reason
    // that will not change. `image` is removed and names its replacement,
    // `bank` is removed and names `segment`, an illegal opcode is outside the
    // instruction sets K816 assembles (`doc/design.md` §1), and the three
    // remaining evaluator functions mutate a section or are not
    // deterministic.
    match reserved.kind {
        ReservedKind::ImageBlock | ReservedKind::ImageTopLevel => {
            compat::k65_removed_image(reserved.keyword_span, "image")
        }
        ReservedKind::EvalCall(name) => {
            compat::k65_refused_eval_function(reserved.keyword_span, eval_call_text(name))
        }
        ReservedKind::Bank => {
            let name = reserved.name.as_ref().map_or("NAME", |name| &name.text);
            compat::k65_removed_bank(reserved.keyword_span, name)
        }
        ReservedKind::IllegalOpcode(opcode) => {
            let (form, illegal, native) = illegal_opcode_text(opcode);
            compat::k65_illegal_opcode(reserved.keyword_span, form, illegal, native)
        }
    }
}

/// The raw bytes a resolved section assembles to, or why it has none.
///
/// The original's `AsmSection::BuildRawTable` (`asm.cpp:599`), rule for rule:
/// labels contribute nothing, a reserved byte contributes a zero — which is
/// what `?` produced there through `AST_IGNORED_BYTE` — and anything whose
/// value is not a constant makes the whole section unreadable rather than
/// partially readable. A `code { }` subblock does the same, because its bytes
/// are an instruction stream three passes away from being decided.
fn section_bytes(walked: &crate::resolve::DataOps<'_>, section: SymbolId) -> eval::SectionBytes {
    use crate::op::{DataOp, KOpKind};
    if !walked.code.is_empty() {
        return eval::SectionBytes::Unresolved;
    }
    // From the section's own label, not from the top of what the block
    // emitted. A leading placement directive comes *before* the label —
    // `align 16 + 4` lowers to `.align 16` and a `.res 4`, and the label
    // follows both, because the block's name labels the first emitted byte
    // and not the padding that puts it where the source asked. Counting the
    // filler made `data t { align 16 + 4  7 }` answer `size(t) == 5` and
    // `index(t, 0) == 0`; the original answers 1 and 7, measured, because an
    // alignment is section metadata there and never a code symbol.
    let Some(start) = walked
        .ops
        .iter()
        .position(|op| matches!(op.kind, KOpKind::Label(id) if id == section))
    else {
        return eval::SectionBytes::Unresolved;
    };
    let mut bytes = Vec::new();
    for op in &walked.ops[start + 1..] {
        // Placement *after* the label moves the bytes that follow it, and
        // neither this pass nor the original can say where to. `.align`
        // emits padding only ld65 sizes; a segment switch relocates the
        // rest. The one exception is the restore a block-local `segment`
        // leaves at the very end, which emits nothing and follows every byte.
        match &op.kind {
            KOpKind::Align(_) => return eval::SectionBytes::Unresolved,
            KOpKind::Segment { name, .. }
                if name.as_str() != crate::resolve::OUTER_SEGMENT
                    || op.origin != crate::op::Origin::CompilerGenerated =>
            {
                return eval::SectionBytes::Unresolved;
            }
            _ => {}
        }
        let KOpKind::Data(data) = &op.kind else {
            continue;
        };
        let wide = |bytes: &mut Vec<u8>, values: &[crate::value::KExpr], width: u32| {
            for value in values {
                let Some(known) = value.value() else {
                    return false;
                };
                let known = known.get();
                for shift in 0..width {
                    bytes.push((known >> (8 * shift)) as u8);
                }
            }
            true
        };
        let kept = match data {
            DataOp::Byte(values) => wide(&mut bytes, values, 1),
            DataOp::Word(values) => wide(&mut bytes, values, 2),
            DataOp::Far(values) => wide(&mut bytes, values, 3),
            DataOp::Reserve(count) => match count.value() {
                Some(count) => {
                    bytes.resize(bytes.len() + count.get() as usize, 0);
                    true
                }
                None => false,
            },
            // Read only because something is asking: a `binary` nobody
            // indexes stays an `.incbin` the assembler resolves.
            DataOp::IncBin {
                resolved, length, ..
            } => match std::fs::read(resolved) {
                Ok(contents) if contents.len() as u32 == *length => {
                    bytes.extend_from_slice(&contents);
                    true
                }
                _ => false,
            },
        };
        if !kept {
            return eval::SectionBytes::Unresolved;
        }
    }
    eval::SectionBytes::Known(bytes)
}

/// The names of the labels written *inside* a data block.
fn inner_labels(data: &DataBlock) -> Vec<String> {
    let mut names = Vec::new();
    collect_inner_labels(&data.entries, &mut names);
    names
}

fn collect_inner_labels(entries: &[crate::ast::DataEntry], names: &mut Vec<String>) {
    for entry in entries {
        match &entry.kind {
            DataEntryKind::Label(name) => names.push(name.text.clone()),
            DataEntryKind::NocrossGroup(inner) => collect_inner_labels(inner, names),
            DataEntryKind::Repeat { body, .. } => collect_inner_labels(&body.entries, names),
            _ => {}
        }
    }
}

/// What an illegal-opcode HLA form is spelled, what 6502 opcode it means, and
/// what the 65816 does with the same bytes.
///
/// The 65816 column is `doc/w65c816s.md` Table 5-4 read against the original's
/// own opcode table (`k65/src/asm.cpp:68-89`). Two of the five land on
/// `STP` and `XCE` — an accidental processor stop and an accidental mode
/// switch — which is the clearest statement of why the forms cannot simply
/// be carried over.
fn illegal_opcode_text(opcode: IllegalOpcode) -> (&'static str, &'static str, &'static str) {
    match opcode {
        IllegalOpcode::Anc => (
            "a &. n",
            "ANC",
            "`PHD`, which pushes the direct-page register",
        ),
        IllegalOpcode::Sax => (
            "mem = a & x",
            "SAX",
            "`STA` in its long, indirect-long and stack-relative forms",
        ),
        IllegalOpcode::Axs => (
            "x &= a - n",
            "AXS",
            "`WAI`, which stops the processor until an interrupt",
        ),
        IllegalOpcode::Dcp => (
            "a ?-- mem",
            "DCP",
            "`CMP` in its long, indirect-long and stack-relative forms — and \
             `STP`, which stops the processor, for an `abs,y` operand",
        ),
        IllegalOpcode::Isc => (
            "a -++ mem",
            "ISC",
            "`SBC` in its long, indirect-long and stack-relative forms — and \
             `XCE`, which leaves native mode, for an `abs,y` operand",
        ),
    }
}

/// The source spelling of a reserved evaluator function.
/// One of the four evaluator functions K816 refuses, at its call site.
fn reserved_eval_call(call: &crate::ast::ReservedCall) -> Diagnostic {
    compat::k65_refused_eval_function(call.keyword_span, eval_call_text(call.name))
}

fn eval_call_text(name: EvalCallName) -> &'static str {
    match name {
        EvalCallName::AddByte => "addbyte",
        EvalCallName::Color => "color",
        EvalCallName::Rnd => "rnd",
        EvalCallName::Print => "print",
        EvalCallName::Error => "error",
    }
}

/// Every `.name` mentioned anywhere inside an expression.
fn collect_local_symbols<'a>(expr: &'a Expr, found: &mut Vec<&'a crate::ast::Name>) {
    match &expr.kind {
        ExprKind::LocalSymbol(name) => found.push(name),
        ExprKind::Unary(unary) => collect_local_symbols(&unary.operand, found),
        ExprKind::Binary(binary) => {
            collect_local_symbols(&binary.left, found);
            collect_local_symbols(&binary.right, found);
        }
        ExprKind::Field(access) => collect_local_symbols(&access.base, found),
        ExprKind::Subscript(subscript) => {
            collect_local_symbols(&subscript.base, found);
            collect_local_symbols(&subscript.index, found);
        }
        ExprKind::Metadata(query) => collect_local_symbols(&query.base, found),
        ExprKind::TypedView(view) => collect_local_symbols(&view.base, found),
        _ => {}
    }
}

/// The expressions one native operand carries, and whether each sits in the
/// *target* position expansion can rewrite.
fn native_operand_expressions(operand: &crate::ast::NativeOperand) -> Vec<(&Expr, bool)> {
    use crate::ast::NativeOperand;
    match operand {
        NativeOperand::Immediate(expr) => vec![(expr, false)],
        // An unindexed address is the branch-and-load shape: `bra .spot`,
        // `lda .spot`. Indexed, it is arithmetic on the label's address.
        NativeOperand::Address(address) => vec![(&address.value, address.index.is_none())],
        NativeOperand::Indirect(indirect) => vec![(&indirect.value, false)],
        NativeOperand::LongIndirect(indirect) => vec![(&indirect.value, false)],
        NativeOperand::BlockMove(moves) => {
            vec![(&moves.source, false), (&moves.destination, false)]
        }
        NativeOperand::Register(_) => Vec::new(),
    }
}

/// The expressions one HLA statement carries.
fn hla_expressions(hla: &HlaStatement) -> Vec<(&Expr, bool)> {
    use crate::ast::HlaOperandKind;
    let operands: Vec<&crate::ast::HlaOperand> = match hla {
        HlaStatement::Assign(assign) => assign
            .targets
            .iter()
            .chain(std::iter::once(&assign.source))
            .collect(),
        HlaStatement::Arithmetic(arithmetic) => vec![&arithmetic.operand],
        HlaStatement::Shift(shift) => vec![&shift.target],
        HlaStatement::Step(step) => vec![&step.target],
        _ => Vec::new(),
    };
    operands
        .into_iter()
        .filter_map(|operand| match &operand.kind {
            HlaOperandKind::Immediate(expr) => Some((expr, false)),
            HlaOperandKind::Memory(address) => Some((&address.value, address.index.is_none())),
            HlaOperandKind::Indirect(indirect) => Some((&indirect.value, false)),
            HlaOperandKind::Register(_) | HlaOperandKind::Error => None,
        })
        .collect()
}

/// An `inline`'s own label, used where only a name would do.
/// An `inline` this unit expands, whose own unit does not compile it.
///
/// The complaints themselves stay where they belong. This one exists so that
/// dropping them does not leave the unit quietly emitting an expansion of a
/// body nothing checked — which is what lowering would meet as a hole.
fn borrowed_inline_unusable(name: &str, declaration: Span, unit: &str) -> Diagnostic {
    Diagnostic::error(
        DiagnosticCode::BORROWED_INLINE_UNUSABLE,
        format!("`{name}` cannot be expanded here: its own unit does not compile it"),
        Label::new(
            declaration,
            "this body is expanded into the unit being compiled",
        ),
    )
    .with_note(format!(
        "the errors in the body are reported against `{unit}`, which is where the body is \
         written; repeating them in every unit that expands the section would say one thing \
         many times"
    ))
    .with_help(format!("compile `{unit}` and fix what it reports"))
    .with_docs("doc/syntax-reference.md#inline")
}

fn inline_label_as_a_value(name: &crate::ast::Name) -> Diagnostic {
    Diagnostic::error(
        DiagnosticCode::SYMBOL_KIND_MISMATCH,
        format!(
            "`{}` is an `inline` label, which has no address to take",
            name.text
        ),
        Label::new(name.span, "used here as a value, not as a target"),
    )
    .with_note(
        "expansion renames an `inline`'s labels per use site — they become the enclosing \
         function's unnamed targets — so there is no name left for an expression to \
         mention, though a branch or a plain `lda .name` still reaches one",
    )
    .with_help(
        "use a unit-level label outside the `inline`, or make the section a `func`, which \
         has one address",
    )
    .with_docs("doc/syntax-reference.md#inline")
}

/// A unit-level label written inside an `inline` body.
fn global_label_in_inline(label: &crate::ast::LabelStatement) -> Diagnostic {
    Diagnostic::error(
        DiagnosticCode::DUPLICATE_SYMBOL,
        format!(
            "`{}` is a unit-level label inside an `inline`, which has no one address",
            label.name.text
        ),
        Label::new(
            label.name.span,
            "every use site would define this name again",
        ),
    )
    .with_note(
        "an `inline` body is copied into each use site and its labels are renamed per \
         copy, so a name the whole unit can see has no definition left to point at",
    )
    .with_help(format!(
        "make it local — `.{}` — or move the code into a `func`, which has one address",
        label.name.text
    ))
    .with_docs("doc/syntax-reference.md#inline")
}

/// Whether the entry ABI answers for this section, and as what.
///
/// Two ways in, and neither is a call site. `main` is entered by the startup
/// code. A section that returns through `RTI` is entered by the *hardware*:
/// the instruction exists to pop the status the interrupt pushed, so a body
/// containing one is a body the interrupt vector reaches. Both take the
/// environment's widths, because a caller that is not a call site cannot be
/// bridged to.
///
/// The `RTI` is looked for syntactically, and it has to be: the widths are
/// needed *before* the body is analyzed, so nothing the solver learns is
/// available yet.
fn entry_kind(func: &Function) -> Option<abi::EntryKind> {
    if func.name.text == "main" {
        return Some(abi::EntryKind::Main);
    }
    returns_through_rti(&func.body).then_some(abi::EntryKind::Interrupt)
}

/// True when any statement of `block`, at any depth, returns through `RTI`.
fn returns_through_rti(block: &crate::ast::Block) -> bool {
    block
        .statements
        .iter()
        .any(|statement| match &statement.kind {
            StatementKind::Return(ast::ReturnKind::Interrupt) => true,
            StatementKind::Native(native) => native.mnemonic.text.eq_ignore_ascii_case("rti"),
            // A handler may put its return inside a conditional or a loop, and a
            // width block around it changes nothing about who entered the body.
            StatementKind::WidthBlock(width) => returns_through_rti(&width.body),
            StatementKind::Conditional(conditional) => {
                returns_through_rti(&conditional.body)
                    || conditional
                        .otherwise
                        .as_ref()
                        .is_some_and(returns_through_rti)
            }
            StatementKind::Loop(loop_statement) => returns_through_rti(&loop_statement.body),
            StatementKind::Nocross(body) => returns_through_rti(body),
            _ => false,
        })
}

/// The span of the annotation that set `axis` on this function, if it wrote
/// one.
fn entry_annotation(func: &Function, axis: Axis) -> Option<Span> {
    func.width
        .iter()
        .find(|annotation| {
            matches!(
                (annotation.kind, axis),
                (
                    ast::WidthAnnotationKind::A8 | ast::WidthAnnotationKind::A16,
                    Axis::Accumulator
                ) | (
                    ast::WidthAnnotationKind::I8 | ast::WidthAnnotationKind::I16,
                    Axis::Index
                )
            )
        })
        .map(|annotation| annotation.span)
}

/// A section the entry ABI answers for declares a width it does not provide.
fn entry_abi_mismatch(
    entry: abi::EntryKind,
    axis: Axis,
    environment: abi::EntryEnvironment,
    site: Span,
    func: &Function,
) -> Diagnostic {
    let (name, annotation) = match axis {
        Axis::Accumulator => ("an accumulator width", ["@a8", "@a16"]),
        Axis::Index => ("an index-register width", ["@i8", "@i16"]),
    };
    let provided = match environment.width(axis) {
        RegisterWidth::W8 => annotation[0],
        RegisterWidth::W16 => annotation[1],
    };
    Diagnostic::error(
        DiagnosticCode::ENTRY_ABI_MISMATCH,
        format!(
            "`{}` declares {name} the environment does not provide",
            func.name.text
        ),
        Label::new(site, format!("the environment provides `{provided}`")),
    )
    .with_secondary(Label::new(
        func.name.span,
        format!("{}, so its widths are the environment's", entry.describe()),
    ))
    .with_note(format!(
        "an annotation on an ordinary function is a requirement on its callers, which \
         K816 satisfies by bridging at each call site; {}, and that cannot be bridged",
        entry.caller()
    ))
    .with_help(format!(
        "write `{provided}`, or drop the annotation and set the width in the body — a \
         scoped `{} {{ … }}` block, or an explicit `rep`/`sep`",
        annotation[1]
    ))
    .with_docs("doc/register-width-aware-syntax.md#the-entry-point")
}

/// A field name that appears twice in one list.
fn duplicate_field(field: &ast::FieldDeclaration, previous: Span) -> Diagnostic {
    Diagnostic::error(
        DiagnosticCode::DUPLICATE_SYMBOL,
        format!("`{}` is declared twice in this field list", field.name.text),
        Label::new(field.name.span, "declared again here"),
    )
    .with_secondary(Label::new(previous, "first declared here"))
    .with_note(
        "a field name selects one offset, so a second entry under the same name could \
         never be reached",
    )
    .with_docs("doc/symbolic-subscripts.md#field-item-grammar")
}

/// `var t * 0`, or the field count `.str[0]`. Not a declaration's raw span:
/// `var buf[0]` is K65 and is accepted.
fn zero_element_count(span: Span) -> Diagnostic {
    Diagnostic::error(
        DiagnosticCode::ELEMENT_COUNT_RANGE,
        "an element count must be at least 1",
        Label::new(span, "this declares no elements"),
    )
    .with_note(
        "storage with no elements reserves nothing, and every index into it is out of \
         range the moment it is written",
    )
    .with_help("write how many elements there are, or drop the count to declare one")
    .with_docs("doc/syntax-reference.md#allocation-count")
}

/// An `abstract var` whose declaration asks for storage it cannot have.
fn abstract_declares_storage(var: &ast::VarDeclaration, site: Span, what: &str) -> Diagnostic {
    Diagnostic::error(
        DiagnosticCode::ABSTRACT_HAS_NO_STORAGE,
        format!("`abstract var {}` declares {what}", var.name.text),
        Label::new(site, "an abstract layout reserves nothing"),
    )
    .with_secondary(Label::new(var.name.span, "declared `abstract` here"))
    .with_note(
        "an `abstract var` exists for `:sizeof` and `:offsetof`: it emits no bytes, no \
         object symbol, and no linker allocation",
    )
    .with_help(format!(
        "drop {what}, or drop `abstract` to declare a variable that has storage"
    ))
    .with_docs("doc/syntax-reference.md#abstract-symbolic-layouts")
}

/// An `abstract var` with no field list, which leaves nothing declared.
fn abstract_has_no_fields(var: &ast::VarDeclaration) -> Diagnostic {
    Diagnostic::error(
        DiagnosticCode::ABSTRACT_HAS_NO_STORAGE,
        format!("`abstract var {}` declares no fields", var.name.text),
        Label::new(
            var.name.span,
            "an abstract layout is a field list and nothing else",
        ),
    )
    .with_note(
        "with no storage and no fields there is nothing left to declare: no bytes, no \
         address, and no offsets to query",
    )
    .with_help(format!(
        "write the fields, as in `abstract var {}[ .lo :byte .hi :byte ]`",
        var.name.text
    ))
    .with_docs("doc/syntax-reference.md#abstract-symbolic-layouts")
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
