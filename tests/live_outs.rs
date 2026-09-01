/*****************************************************************************/
/*                                                                           */
/*                               live_outs.rs                                */
/*                                                                           */
/*        What narrowing the live-out seed would buy, measured (G13)         */
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

//! Sub-plan G, task G13: **what narrowing the live-out seed would buy**.
//!
//! Sub-plan F's decision 12 seeds liveness with everything at a function
//! exit, and named three objections to narrowing it to what a `->` clause
//! declares. **G9b closed all three of the ones about call spelling.** A
//! clause may name a flag; an output a clause does not list is undefined
//! after the call *at every call site, `call foo` included* (decision 19, and
//! `doc/syntax-reference.md`). The escape hatch escapes the `K816-W0123`
//! warning, not the language rule — so a caller reading outside the clause
//! reads undefined state, and narrowing is licensed against it.
//!
//! **A deferral deserves a number and a reason that survives**, and this file
//! produces both. Nothing here changes the compiler:
//! `optimize::measure_narrowed_live_outs` runs the pass sequence twice over a
//! clone of each unit — once as it ships, once with every declaring function's
//! exit seeded with its clause — and reports what pass 3 removed each way and
//! what the unit assembles to each way.
//!
//! # The two answers
//!
//!   - **What it would buy**, in [`narrowing_the_live_out_seed_buys_only_what_the_table_lists`]:
//!     three instructions, six bytes, in three units of the whole corpus.
//!     [`the_escape_hatch_is_how_most_of_the_corpus_calls`] says why so
//!     little — four of 244 emitted sections declare an output at all, so
//!     most of the corpus cannot be narrowed by any rule.
//!   - **What still stands in the way**, in
//!     [`a_tail_jump_makes_a_clause_the_wrong_contract_for_a_return`]: a
//!     function control enters by a tail `goto` returns on *its* `rts` to the
//!     jumping function's caller, bound by a different clause. The narrowing
//!     deletes a register that caller reads. That objection is about control
//!     flow, not about how a call is spelled, and no language rule G landed
//!     touches it.
//!
//! The census counts call spellings anyway, because they say what the corpus
//! is like even where they no longer decide soundness — and because an
//! `inline` use is not a call at all: nothing is `jsr`-ed, nothing returns,
//! and there is no exit to narrow. Counting one would inflate every number
//! here.
//!
//! # The corpus
//!
//! `examples/`, `tests/golden/`, and the imported corpus under
//! `tests/compat/` — every unit of every set, compiled as the entry with its
//! siblings beside it, which is the shape `--unit` gives the command line.
//! A source that does not compile is measured by nobody, so each one has to
//! say why: a golden or an example names its reason in [`REFUSALS`], and a
//! compat fixture is held to the `expect` list its own `fixture.txt`
//! publishes.

use std::path::Path;

use k816::abi::EntryEnvironment;
use k816::ast::{
    AstFile, Block, DataBlock, DataEntryKind, FunctionKind, ItemKind, Statement, StatementKind,
};
use k816::diagnostic::{Severity, has_errors};
use k816::linkage::Linkage;
use k816::optimize::{self, Narrowing};
use k816::source::{FileId, SourceMap};
use k816::symbol::{SymbolKind, SymbolTable};
use k816::{lower, parser, semantic};

// ---------------------------------------------------------------------------
// The answer
// ---------------------------------------------------------------------------

/// Every unit the narrowed seed would change, with what it would buy there.
///
/// `(unit, operations, bytes, why)`. The table is the measurement's result
/// and `postmortem.md` §G.22 is where it is argued; a row that appears or
/// disappears is a change in what the deferral costs, which is exactly the
/// thing that should not happen quietly.
///
/// **Listed per unit rather than as one total** because a total moves
/// whenever any fixture is added, and these rows move only when a fixture
/// that *uses a contract* is. A new row is news; a shifted total is noise.
///
/// The byte column is [`Narrowing::bytes`]'s difference, and it is the bytes
/// the object file loses: `emit_ca65::emitted_bytes` sizes a width-dependent
/// immediate at the width ca65 is told to assemble it at. All three
/// operations here are eight-bit immediates, so each is two bytes and the
/// column adds up to six.
const DIFFERENCE: &[(&str, usize, u32, &str)] = &[
    (
        "tests/compat/k816/contract-flag-outputs/input.k65",
        1,
        2,
        "`silent` declares `-> a` and ends `cmp #$0A`, which writes N, Z and \
         C and nothing else. Under the clause the compare is dead. The \
         fixture's other half is why the row is a language rule and not a \
         heuristic: `check` has the identical body, declares `-> a, c`, and \
         keeps its compare",
    ),
    (
        "tests/compat/k816/strict-k65/input.k65",
        1,
        2,
        "`step` declares `-> a` and adds to the accumulator before two later \
         loads overwrite it. What kept the `adc` alive as the compiler ships \
         is not A at all — it is the N, V, Z and C the clause does not name, \
         which the seed says a caller may read",
    ),
    (
        "tests/golden/opt-liveout.k816",
        1,
        2,
        "`produce` declares `-> a` and ends `ldy #$09`. F's fixture says the \
         `ldy` survives because a caller reached through `call produce` may \
         read Y — and the same file calls `produce` both ways, which is why \
         this row is the deferral's whole argument in one function",
    ),
];

/// What the corpus looks like from the narrowing rule's point of view.
///
/// Not a total of anything the optimizer removed: these are counts of
/// *declarations and call sites*, and they are what decide whether the
/// numbers in [`DIFFERENCE`] are a floor or a ceiling.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
struct Census {
    /// Sections the compiler emits, in every unit that compiles.
    ///
    /// **An `inline` is not one.** It has no address and no body of its own —
    /// `lower.rs` emits no `KFunction` for it and its operations appear once
    /// per use site — so it has no exit for a live-out seed to sit at, and
    /// counting its clause here would inflate the only number that says
    /// whether narrowing has anything to work on.
    sections: usize,
    /// Of those, the ones whose `->` clause names a register or a flag.
    declaring: usize,
    /// `inline` sections, kept apart rather than dropped: an `inline` whose
    /// clause declares an output is a real declaration that the rule still
    /// cannot use, and a reader comparing this file to the source will count
    /// them.
    inlines: usize,
    /// Of those, the ones with an output clause.
    inlines_declaring: usize,
    /// `call NAME`, `call far NAME`, `far NAME` — the escape hatch, which
    /// opts out of the *check* in both directions.
    escape_hatch: usize,
    /// `add a, x -> a, y` — a call site that restates the callee's flow, so
    /// the two are compared.
    stating: usize,
    /// `foo` on its own: checked, but with nothing to check unless the callee
    /// declares a flow — in which case it is an error and the call site has
    /// to state it.
    bare: usize,
    /// A hand-written `jsr`/`jsl`, which never had a contract.
    native: usize,
    /// A use of an `inline`, in any spelling. Not a call: no `jsr` is
    /// emitted, no callee returns, and there is no exit to narrow.
    expansions: usize,
}

/// The census, as measured. Update it and `postmortem.md` §G.22 together.
const CENSUS: Census = Census {
    sections: 244,
    declaring: 4,
    inlines: 12,
    inlines_declaring: 6,
    escape_hatch: 27,
    stating: 3,
    bare: 4,
    native: 15,
    expansions: 23,
};

#[path = "common/corpus.rs"]
mod corpus_files;

use corpus_files::{UnitSet, corpus, fixture_header, root};

/// The corpus sources that pin a *refusal*, and so have no optimized form.
///
/// Named individually rather than counted, because "did not compile" and
/// "does not compile on purpose" are different facts and only the second is
/// allowed. A compat fixture is held to its own `fixture.txt` instead, which
/// publishes the codes it expects; these three have no such file.
const REFUSALS: &[(&str, &str)] = &[
    (
        "examples/k65/atari-xl/starfield/main.k65",
        "two K816-K65-0002: reserved legacy `rnd()` syntax, which keeps its \
         own code by design (sub-plan E, decision 18)",
    ),
    (
        "tests/golden/nocross-oversized.k816",
        "K816-E0112: a `nocross` run larger than a page, refused before \
         anything is lowered",
    ),
    (
        "tests/golden/width-emulation.k816",
        "two K816-E0104: a widening `rep` after an `xce`, which is the \
         fixture's whole subject. No entry environment makes it compile — the \
         emulation flag is unknown *inside* the program",
    ),
];

/// The codes a fixture publishes for one of its units.
fn fixture_expects(dir: &Path, unit: &str) -> Vec<String> {
    fixture_header(dir)
        .remove("expect")
        .unwrap_or_default()
        .iter()
        .filter_map(|value| value.split_once(char::is_whitespace))
        .filter(|(named, _)| *named == unit)
        .map(|(_, code)| code.trim().to_owned())
        .collect()
}

// ---------------------------------------------------------------------------
// Compiling one unit set
// ---------------------------------------------------------------------------

/// One unit measured, or the error codes that stopped it.
#[derive(Debug)]
struct Measured {
    narrowing: Narrowing,
    census: Census,
}

fn measure(set: &UnitSet) -> Result<Measured, Vec<String>> {
    let sources: Vec<(String, String)> = set
        .files
        .iter()
        .map(|path| {
            let text = std::fs::read_to_string(path)
                .unwrap_or_else(|e| panic!("cannot read {}: {e}", path.display()));
            (path.to_string_lossy().into_owned(), text)
        })
        .collect();
    measure_sources(&sources, set.entry)
}

/// [`measure`], over sources supplied rather than read — which is what lets a
/// case written in this file be measured the same way a corpus file is.
fn measure_sources(sources: &[(String, String)], entry: usize) -> Result<Measured, Vec<String>> {
    let mut map = SourceMap::new();
    let mut id: Option<FileId> = None;
    for (index, (name, text)) in sources.iter().enumerate() {
        let added = map.add_file(name.clone(), text.clone());
        if index == entry {
            id = Some(added);
        }
    }
    let file = id.expect("the entry is one of the files");

    // The pipeline `compile_to_ca65` runs, stopped where the optimizer would
    // start. Stated here rather than reached through the driver because the
    // measurement needs the lowered program and the model, and the driver
    // hands back assembly.
    let linkage = Linkage::resolve(&map, EntryEnvironment::default());
    let source = map.file(file);
    let parse = parser::parse_source(source, None);
    let (model, semantic_diagnostics) =
        semantic::analyze_unit(&parse.file, source.name(), &linkage);
    let mut diagnostics = linkage.diagnostics().to_vec();
    diagnostics.extend(parse.diagnostics);
    diagnostics.extend(semantic_diagnostics);
    if has_errors(&diagnostics) {
        return Err(error_codes(&diagnostics));
    }
    let program = model.program(&parse.file);
    let census = census_of(program, &model.symbols);
    let (lowered, lower_diagnostics) = lower::lower_unit(program, &model, source.name());
    diagnostics.extend(lower_diagnostics);
    if has_errors(&diagnostics) {
        return Err(error_codes(&diagnostics));
    }
    let narrowing = optimize::measure_narrowed_live_outs(
        &lowered,
        &optimize::Context {
            model: &model,
            linkage: &linkage,
            file,
        },
    );
    Ok(Measured { narrowing, census })
}

fn error_codes(diagnostics: &[k816::diagnostic::Diagnostic]) -> Vec<String> {
    let mut codes: Vec<String> = diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.severity == Severity::Error)
        .map(|diagnostic| diagnostic.code.as_str().to_owned())
        .collect();
    codes.sort();
    codes.dedup();
    codes
}

// ---------------------------------------------------------------------------
// The census
// ---------------------------------------------------------------------------

/// Counts the declarations and call sites of one preprocessed unit.
///
/// Every match here is **exhaustive** — over items, over statements, over
/// data entries — so a construct that grows a block later stops this file
/// from compiling rather than going silently uncounted. That is the whole
/// reason the arms are written out instead of matching only the kinds this
/// cares about.
fn census_of(file: &AstFile, symbols: &SymbolTable) -> Census {
    let mut census = Census::default();
    for item in &file.items {
        match &item.kind {
            ItemKind::Func(function) => {
                // The same rule `declared_outputs` uses: a width-only clause
                // declares an exit width and not a register flow, so it
                // names no outputs and narrowing has nothing to narrow to.
                let declares = function
                    .contract
                    .as_ref()
                    .and_then(|contract| contract.outputs.as_ref())
                    .is_some_and(|outputs| !outputs.outputs.is_empty());
                if function.kind == FunctionKind::Inline {
                    census.inlines += 1;
                    census.inlines_declaring += usize::from(declares);
                } else {
                    census.sections += 1;
                    census.declaring += usize::from(declares);
                }
                block(&function.body, symbols, &mut census);
            }
            ItemKind::Data(data) => data_block(data, symbols, &mut census),
            ItemKind::Var(_)
            | ItemKind::Const(_)
            | ItemKind::Segment(_)
            | ItemKind::EvalBlock(_)
            | ItemKind::EvalFunc(_)
            | ItemKind::Preprocessor(_)
            | ItemKind::Binary(_)
            | ItemKind::Reserved(_)
            | ItemKind::ModuleWidth(_)
            | ItemKind::Error => {}
        }
    }
    census
}

fn block(body: &Block, symbols: &SymbolTable, census: &mut Census) {
    for statement in &body.statements {
        count(statement, symbols, census);
    }
}

/// True when this call target is an `inline`, so the site expands rather than
/// calls: no `jsr`, no callee return, and no exit to narrow.
fn expands(symbols: &SymbolTable, target: &str) -> bool {
    symbols
        .lookup(target)
        .is_some_and(|symbol| symbols.kind(symbol) == SymbolKind::Inline)
}

fn count(statement: &Statement, symbols: &SymbolTable, census: &mut Census) {
    match &statement.kind {
        StatementKind::Call(call) => {
            if expands(symbols, &call.target.text) {
                census.expansions += 1;
            } else {
                census.escape_hatch += 1;
            }
        }
        StatementKind::BareCall(call) => {
            if expands(symbols, &call.target.text) {
                census.expansions += 1;
            } else if call.arguments.is_some() || call.outputs.is_some() {
                census.stating += 1;
            } else {
                census.bare += 1;
            }
        }
        StatementKind::Native(instruction) => {
            if matches!(
                instruction.mnemonic.text.to_ascii_lowercase().as_str(),
                "jsr" | "jsl"
            ) {
                census.native += 1;
            }
        }
        StatementKind::WidthBlock(scoped) => block(&scoped.body, symbols, census),
        StatementKind::Conditional(conditional) => {
            block(&conditional.body, symbols, census);
            if let Some(otherwise) = &conditional.otherwise {
                block(otherwise, symbols, census);
            }
        }
        StatementKind::ConditionalJump(jump) => count(&jump.jump, symbols, census),
        StatementKind::Loop(loop_statement) => block(&loop_statement.body, symbols, census),
        StatementKind::Nocross(body) => block(body, symbols, census),
        StatementKind::Data(data) => data_block(data, symbols, census),
        StatementKind::Label(_)
        | StatementKind::Hla(_)
        | StatementKind::ModeSet(_)
        | StatementKind::Break
        | StatementKind::Repeat
        | StatementKind::Goto(_)
        | StatementKind::Align(_)
        | StatementKind::Address(_)
        | StatementKind::Reserved(_)
        | StatementKind::Preprocessor(_)
        | StatementKind::Return(_)
        | StatementKind::Error => {}
    }
}

fn data_block(data: &DataBlock, symbols: &SymbolTable, census: &mut Census) {
    for entry in &data.entries {
        data_entry(&entry.kind, symbols, census);
    }
}

fn data_entry(kind: &DataEntryKind, symbols: &SymbolTable, census: &mut Census) {
    match kind {
        DataEntryKind::Code { body, .. } => block(body, symbols, census),
        DataEntryKind::NocrossGroup(entries) => {
            for entry in entries {
                data_entry(&entry.kind, symbols, census);
            }
        }
        DataEntryKind::Repeat { body, .. } => data_block(body, symbols, census),
        DataEntryKind::Segment(_)
        | DataEntryKind::Align(_)
        | DataEntryKind::Address(_)
        | DataEntryKind::Nocross
        | DataEntryKind::Charset(_)
        | DataEntryKind::Label(_)
        | DataEntryKind::Typed { .. }
        | DataEntryKind::Value(_)
        | DataEntryKind::Evaluator(_)
        | DataEntryKind::ForEval { .. }
        | DataEntryKind::Preprocessor(_)
        | DataEntryKind::Binary(_)
        | DataEntryKind::Reserved(_)
        | DataEntryKind::Error => {}
    }
}

// ---------------------------------------------------------------------------
// The measurement
// ---------------------------------------------------------------------------

/// Every unit the narrowed seed would change is in [`DIFFERENCE`], with what
/// it would buy there.
#[test]
fn narrowing_the_live_out_seed_buys_only_what_the_table_lists() {
    let mut found: Vec<(String, usize, u32)> = Vec::new();
    for set in corpus() {
        let Ok(measured) = measure(&set) else {
            continue; // Accounted for by the refusal test below.
        };
        let narrowing = measured.narrowing;
        assert!(
            narrowing.removed_narrowed >= narrowing.removed,
            "{}: the narrowed seed removed *fewer* operations ({} against {}), \
             which a smaller live set cannot do — the measurement is wrong, \
             not the corpus",
            set.name,
            narrowing.removed_narrowed,
            narrowing.removed,
        );
        assert!(
            narrowing.bytes_narrowed <= narrowing.bytes,
            "{}: the narrowed seed produced a *larger* unit",
            set.name,
        );
        if narrowing.removed_narrowed > narrowing.removed
            || narrowing.bytes_narrowed < narrowing.bytes
        {
            found.push((
                set.name.clone(),
                narrowing.removed_narrowed - narrowing.removed,
                narrowing.bytes - narrowing.bytes_narrowed,
            ));
        }
    }
    found.sort();

    let mut expected: Vec<(String, usize, u32)> = DIFFERENCE
        .iter()
        .map(|(name, operations, bytes, _)| ((*name).to_owned(), *operations, *bytes))
        .collect();
    expected.sort();
    assert_eq!(
        found, expected,
        "the corpus no longer buys what `DIFFERENCE` says. A new row is a \
         fixture that would gain from narrowing; a missing one is a fixture \
         that stopped. Either way `postmortem.md` §G.22 and the \
         `k816-unlisted-output` row of `doc/k65-compat-manifest.md` state \
         this number and have to move with it",
    );
    for (name, _, _, why) in DIFFERENCE {
        assert!(why.len() > 30, "{name}: the row argues nothing");
    }
}

/// The narrowing is measured against a program written to benefit from it, so
/// that the corpus total is read as "what these programs are like" and not as
/// "what the rule is worth".
///
/// Three registers written at the end of a function that declares one of them
/// as its output. Two of the three are dead under the clause and live under
/// the seed the compiler ships, and the difference is the ceiling: it is what
/// the rule buys where every function declares and nothing calls through the
/// escape hatch.
#[test]
fn a_program_written_for_the_rule_shows_what_it_could_buy() {
    let source = "\
var CELL = $0400

func answer @a8 @i8 (a) -> a {
    a+#$01
    tax
    tay
}

func caller @a8 @i8 {
    a = #$01
    answer a -> a
    CELL = a
}
";
    let measured = measure_sources(&[("narrowing.k816".to_owned(), source.to_owned())], 0)
        .expect("the case compiles");

    assert_eq!(
        measured.census.declaring, 1,
        "one function declares an output"
    );
    assert_eq!(
        measured.narrowing.removed, 0,
        "as the compiler ships, everything is live at the exit and nothing goes"
    );
    assert_eq!(
        measured.narrowing.removed_narrowed, 2,
        "the `tax` and the `tay`, neither of which the clause names"
    );
    assert_eq!(
        measured.narrowing.bytes - measured.narrowing.bytes_narrowed,
        2,
        "two one-byte transfers"
    );
}

/// The objection that survives, demonstrated: a tail jump makes a clause the
/// wrong contract for a return.
///
/// F's decision 12 gave `call foo` as the reason nothing may be narrowed —
/// the escape hatch opts out of contract checking, so a caller might read a
/// register the clause does not name. **G9b closed that**: an output a clause
/// does not list is undefined after the call *at every call site, `call foo`
/// included* (decision 19, and `doc/syntax-reference.md`). The escape hatch
/// escapes the `K816-W0123` warning, not the language rule, so a caller that
/// reads outside the clause is reading undefined state and an optimizer may
/// break it.
///
/// What is not covered is a function control **enters without calling**.
/// `jumper` declares `-> a, x` and its body is `goto target`; `target`
/// declares `-> a`. The `rts` that ends `target` returns to `jumper`'s
/// caller, which was compiled against `jumper`'s clause — so `target`'s
/// clause is not the contract that return has to satisfy, and the `ldx #$09`
/// that looks dead under `-> a` is the X `caller` reads.
///
/// The narrowing deletes it. That is the objection a rule would have to
/// answer, and it has nothing to do with how anything is spelled at a call.
#[test]
fn a_tail_jump_makes_a_clause_the_wrong_contract_for_a_return() {
    let source = "\
var CELL = $0400

func target @a8 @i8 (a) -> a {
    ldx #$09
    a + #$01
}

func jumper @a8 @i8 (a) -> a, x {
    goto target
}

func caller @a8 @i8 {
    a = #$01
    jumper a -> a, x
    CELL,x = a
}
";
    let measured = measure_sources(&[("tail.k816".to_owned(), source.to_owned())], 0)
        .expect("the case compiles");
    assert_eq!(
        measured.narrowing.removed, 0,
        "as the compiler ships, the `ldx` survives and `caller` reads what it wrote"
    );
    assert_eq!(
        measured.narrowing.removed_narrowed, 1,
        "under `target`'s own clause the `ldx` is dead — and `caller` still reads X"
    );
    assert_eq!(
        measured.narrowing.bytes - measured.narrowing.bytes_narrowed,
        2,
        "the two bytes of an eight-bit `ldx #$09`"
    );
}

/// A source that does not compile is measured by nobody, and says why.
///
/// Read from each side that can be enumerated: a compat fixture publishes its
/// codes in its own `fixture.txt`, and the codes that stopped it have to be
/// among them. A golden and an example have no such file, so [`REFUSALS`]
/// names the three — and a name in it that no longer refuses fails just as
/// loudly as one that refuses and is not named.
#[test]
fn every_source_the_measurement_skips_says_why() {
    for set in corpus() {
        let Err(codes) = measure(&set) else {
            assert!(
                !REFUSALS.iter().any(|(named, _)| *named == set.name),
                "{}: is excused as a refusal and compiles",
                set.name,
            );
            continue;
        };
        if let Some(fixture) = set.name.strip_prefix("tests/compat/") {
            let (fixture, unit) = fixture.rsplit_once('/').expect("a fixture path");
            let dir = root().join("tests/compat").join(fixture);
            let published = fixture_expects(&dir, unit);
            for code in &codes {
                assert!(
                    published.contains(code),
                    "{}: stopped by {code}, which its fixture.txt does not \
                     publish for {unit}",
                    set.name,
                );
            }
            continue;
        }
        let why = REFUSALS
            .iter()
            .find(|(named, _)| *named == set.name)
            .map(|(_, why)| *why);
        let Some(why) = why else {
            panic!(
                "{}: does not compile ({codes:?}) and nothing says why. Pair \
                 it in `GOLDEN_PAIRS`, or name it in `REFUSALS`",
                set.name,
            );
        };
        assert!(why.len() > 30, "{}: the excuse argues nothing", set.name);
    }
}

/// The census counts a call wherever one can be written, and counts an
/// `inline` use as neither a section nor a call.
///
/// The corpus does not exercise the first half: no call site in `examples/`,
/// `tests/golden/` or `tests/compat/` sits inside a conditional, a loop, a
/// width block, a `nocross`, or a `data`/`code` subblock, so a walk that
/// stopped at the first brace would count the same totals. **A corpus that
/// does not use a construct is not coverage for it**, so the construct is
/// written here instead — one call of each spelling in each place a block can
/// nest, with the counts stated.
///
/// The second half is the distinction G13's first draft got wrong. `bump` is
/// an `inline` with an output clause, and both of its uses are expansions:
/// lowering emits no function for an `inline`, so there is no exit for a seed
/// to sit at and no `jsr` for a caller to return from.
#[test]
fn the_census_follows_a_call_into_every_block_that_can_hold_one() {
    let source = "\
var CELL = $0400

func leaf @a8 @i8 {
}

inline bump @a8 @i8 (a) -> a {
    a + #$01
}

data TOP {
    code {
        call leaf
    }
}

func nested @a8 @i8 {
    call leaf
    a - #$01
    >= {
        call leaf
    } else {
        call leaf
    }
    {
        call leaf
    } never
    @a16 {
        call leaf
    }
    nocross {
        call leaf
    }
    data {
        code {
            call leaf
        }
        nocross {
            byte 1 2
        }
        repeat 2 {
            byte 1 2
        }
    }
    leaf
    bump a -> a
    call bump
    jsr leaf
}
";
    let measured = measure_sources(&[("census.k816".to_owned(), source.to_owned())], 0)
        .expect("the case compiles");
    assert_eq!(
        measured.census,
        Census {
            sections: 2,
            declaring: 0,
            // `bump` declares `-> a` and is still not something a seed can
            // sit at: lowering emits no function for an `inline`.
            inlines: 1,
            inlines_declaring: 1,
            // The top-level `data`, then the body: bare, both arms of the
            // conditional, the loop, the width block, the `nocross`, and the
            // `code` subblock.
            escape_hatch: 8,
            stating: 0,
            bare: 1,
            native: 1,
            // Both uses of `bump`, in either spelling: no `jsr` is emitted
            // and nothing returns, so neither is a call site.
            expansions: 2,
        },
    );
}

/// The one descent with no case, and the language rule that is why.
///
/// [`data_entry`] follows a `repeat` body, and nothing in the corpus or above
/// reaches it with a call in hand — because a `code` subblock inside a
/// `repeat` is refused: it would define one label per copy. The descent stays
/// (a `repeat` body is a data block like any other, and the rule could
/// change), and this is the case that says why it is empty.
#[test]
fn a_call_cannot_be_written_inside_a_repeat() {
    let source = "\
func leaf @a8 @i8 {
}

inline bump @a8 @i8 (a) -> a {
    a + #$01
}

data TOP {
    repeat 2 {
        code {
            call leaf
        }
    }
}
";
    let codes = measure_sources(&[("repeat.k816".to_owned(), source.to_owned())], 0)
        .expect_err("a `code` subblock in a `repeat` is refused");
    assert_eq!(codes, ["K816-E0102"]);
}

/// The two numbers that decide whether the count is usable.
///
/// The escape hatch and the clause census, over everything that compiles.
#[test]
fn the_escape_hatch_is_how_most_of_the_corpus_calls() {
    let mut total = Census::default();
    for set in corpus() {
        let Ok(measured) = measure(&set) else {
            continue;
        };
        assert_eq!(
            measured.census.declaring, measured.narrowing.declaring,
            "{}: the census counts a `->` clause from the syntax and the \
             measurement counts one from the semantic model, and they \
             disagree",
            set.name,
        );
        total.sections += measured.census.sections;
        total.declaring += measured.census.declaring;
        total.inlines += measured.census.inlines;
        total.inlines_declaring += measured.census.inlines_declaring;
        total.escape_hatch += measured.census.escape_hatch;
        total.stating += measured.census.stating;
        total.bare += measured.census.bare;
        total.native += measured.census.native;
        total.expansions += measured.census.expansions;
    }
    assert_eq!(
        total, CENSUS,
        "the corpus census moved. `postmortem.md` §G.22 reads these numbers \
         as the reason the count in `DIFFERENCE` is small",
    );
}
