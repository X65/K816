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
//! declares. G9b closed two — a clause may name a flag, and an unlisted
//! output of a clause-bearing function is undefined — leaving one: `call
//! NAME` opts out of contract checking, so a callee whose call sites all
//! spell it that way has a declaration nothing verified. **A deferral
//! standing on one objection deserves a number**, and this file is where the
//! number is produced.
//!
//! Nothing here changes the compiler. `optimize::measure_narrowed_live_outs`
//! runs the pass sequence twice over a clone of each unit — once as it ships,
//! once with every declaring function's exit seeded with its clause — and
//! reports what pass 3 removed each way and what the unit weighs each way.
//!
//! # What is measured, and why the census sits beside it
//!
//! The count alone does not say whether it is *usable*. Two more numbers do,
//! and they are what [`the_escape_hatch_is_how_most_of_the_corpus_calls`]
//! counts:
//!
//!   - **how many call sites spell `call NAME`** — the escape hatch, which
//!     opts out of the check that would make a clause a fact, and
//!   - **how many functions declare a clause at all** — a corpus of
//!     contract-less functions cannot be narrowed by any rule, and would say
//!     the deferral costs nothing whatever the rule was.
//!
//! A hand-written `jsr` is counted as its own kind for the same reason: it
//! never had a contract to check, and it is the third route into a callee.
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

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use k816::abi::EntryEnvironment;
use k816::ast::{AstFile, Block, DataBlock, DataEntryKind, ItemKind, Statement, StatementKind};
use k816::diagnostic::{Severity, has_errors};
use k816::linkage::Linkage;
use k816::optimize::{self, Narrowing};
use k816::source::{FileId, SourceMap};
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
const DIFFERENCE: &[(&str, usize, u32, &str)] = &[
    (
        "tests/compat/k816/contract-flag-outputs/input.k65",
        1,
        3,
        "`silent` declares `-> a` and ends with a `cmp` that writes N, Z and \
         C. Under the clause the compare is dead. It is also the fixture's \
         whole point: `check` declares `-> a, c` and keeps it, so the pair \
         measures the language rule and not an analysis heuristic",
    ),
    (
        "tests/compat/k816/strict-k65/input.k65",
        1,
        3,
        "one function declaring `-> a` whose last instruction writes X",
    ),
    (
        "tests/golden/opt-liveout.k816",
        1,
        3,
        "`produce` declares `-> a` and ends `ldy #$09`. F's fixture says the \
         `ldy` survives because a caller reached through `call produce` may \
         read Y — and the same file calls it both ways, which is why this \
         row is the deferral's whole argument in one function",
    ),
];

/// What the corpus looks like from the narrowing rule's point of view.
///
/// Not a total of anything the optimizer removed: these are counts of
/// *declarations and call sites*, and they are what decide whether the
/// numbers in [`DIFFERENCE`] are a floor or a ceiling.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
struct Census {
    /// Functions in every unit that compiles.
    functions: usize,
    /// Of those, the ones whose `->` clause names a register or a flag.
    declaring: usize,
    /// `call NAME`, `call far NAME`, `far NAME` — the escape hatch, which
    /// opts out of the check in both directions.
    escape_hatch: usize,
    /// `add a, x -> a, y` — a call site that restates the callee's flow, so
    /// the two are compared. This is the only spelling that makes a clause a
    /// verified fact at the call.
    stating: usize,
    /// `foo` on its own: checked, but with nothing to check unless the callee
    /// declares a flow — in which case it is an error and the call site has
    /// to state it.
    bare: usize,
    /// A hand-written `jsr`/`jsl`, which never had a contract.
    native: usize,
}

/// The census, as measured. Update it and `postmortem.md` §G.22 together.
const CENSUS: Census = Census {
    functions: 256,
    declaring: 10,
    escape_hatch: 27,
    stating: 12,
    bare: 18,
    native: 15,
};

// ---------------------------------------------------------------------------
// The corpus
// ---------------------------------------------------------------------------

/// `examples/`, with the unit set each program is compiled in.
///
/// The same rows `tests/corpus.rs` carries, for the same reason: K65 has no
/// import syntax, so which files are compiled together is build structure and
/// has to be stated.
const EXAMPLES: &[(&str, &[&str])] = &[
    ("examples/fullstack/main.k816", &[]),
    ("examples/hello_uart.k65", &[]),
    (
        "examples/k65/atari-xl/dli-example/main.k65",
        &["examples/k65/atari-xl/include/defs.k65"],
    ),
    (
        "examples/k65/atari-xl/landscape/main.k65",
        &["examples/k65/atari-xl/include/defs.k65"],
    ),
    (
        "examples/k65/atari-xl/rainbow/main.k65",
        &["examples/k65/atari-xl/include/defs.k65"],
    ),
    (
        "examples/k65/atari-xl/starfield/main.k65",
        &["examples/k65/atari-xl/include/defs.k65"],
    ),
    ("examples/k65/atari-xl/include/defs.k65", &[]),
];

/// The goldens that are two units of one program.
///
/// `tests/optimizer.rs` skips the second of each pair because it cannot be
/// compiled alone; here both are compiled, with the sibling in the map, since
/// a cross-unit call is the case the escape-hatch objection is about.
const GOLDEN_PAIRS: &[[&str; 2]] = &[
    ["contract-unit-a.k816", "contract-unit-b.k816"],
    ["layout-unit-a.k816", "layout-unit-b.k816"],
    ["opt-crossunit-a.k816", "opt-crossunit-b.k816"],
    ["unit-a.k816", "unit-b.k816"],
];

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

/// One set of files compiled together, and which of them is the entry.
///
/// Every corpus source compiles under the default entry environment, so no
/// set carries one: `--entry` describes the machine `main` is handed, and
/// nothing here is written for a different machine.
struct UnitSet {
    /// How the source is named in [`DIFFERENCE`] — repository-relative.
    name: String,
    files: Vec<PathBuf>,
    entry: usize,
}

fn root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

/// Every unit of every set, in a stable order.
fn corpus() -> Vec<UnitSet> {
    let mut sets = Vec::new();
    for (path, units) in EXAMPLES {
        let mut files = vec![root().join(path)];
        files.extend(units.iter().map(|unit| root().join(unit)));
        sets.push(UnitSet {
            name: (*path).to_owned(),
            files,
            entry: 0,
        });
    }
    sets.extend(golden_sets());
    sets.extend(compat_sets());
    sets
}

/// `tests/golden/`, read from the directory rather than listed.
///
/// A fixture added later is measured without anyone remembering to add it,
/// which is the difference between a corpus and a list of the files somebody
/// thought of.
fn golden_sets() -> Vec<UnitSet> {
    let dir = root().join("tests/golden");
    let mut names: Vec<String> = std::fs::read_dir(&dir)
        .expect("read tests/golden")
        .map(|entry| {
            entry
                .expect("read tests/golden entry")
                .file_name()
                .to_string_lossy()
                .into_owned()
        })
        .filter(|name| name.ends_with(".k816") || name.ends_with(".k65"))
        .collect();
    names.sort();

    let mut sets = Vec::new();
    for name in &names {
        let pair = GOLDEN_PAIRS
            .iter()
            .find(|pair| pair.contains(&name.as_str()));
        let files: Vec<PathBuf> = match pair {
            Some(pair) => pair.iter().map(|unit| dir.join(unit)).collect(),
            None => vec![dir.join(name)],
        };
        let entry = files
            .iter()
            .position(|file| file.ends_with(name))
            .expect("the entry is one of the files");
        sets.push(UnitSet {
            name: format!("tests/golden/{name}"),
            files,
            entry,
        });
    }
    for pair in GOLDEN_PAIRS {
        for unit in pair {
            assert!(dir.join(unit).exists(), "{unit} is paired but is gone");
        }
    }
    sets
}

/// The imported corpus: every unit of every `tests/compat/*/*/` fixture.
fn compat_sets() -> Vec<UnitSet> {
    let mut sets = Vec::new();
    for set in ["legacy-k816", "k65", "k816"] {
        let dir = root().join("tests/compat").join(set);
        let mut fixtures: Vec<PathBuf> = std::fs::read_dir(&dir)
            .expect("read a tests/compat set")
            .map(|entry| entry.expect("read a fixture entry").path())
            .filter(|path| path.is_dir())
            .collect();
        fixtures.sort();
        for fixture in fixtures {
            let units = fixture_units(&fixture);
            let files: Vec<PathBuf> = units.iter().map(|unit| fixture.join(unit)).collect();
            for (entry, unit) in units.iter().enumerate() {
                sets.push(UnitSet {
                    name: format!(
                        "tests/compat/{set}/{}/{unit}",
                        fixture.file_name().unwrap().to_string_lossy()
                    ),
                    files: files.clone(),
                    entry,
                });
            }
        }
    }
    sets
}

/// One fixture's `key value` header, up to the first blank line.
fn fixture_header(dir: &Path) -> BTreeMap<String, Vec<String>> {
    let text = std::fs::read_to_string(dir.join("fixture.txt"))
        .unwrap_or_else(|e| panic!("{}: cannot read fixture.txt: {e}", dir.display()));
    let mut keys: BTreeMap<String, Vec<String>> = BTreeMap::new();
    for line in text.lines().take_while(|line| !line.trim().is_empty()) {
        if let Some((key, value)) = line.split_once(char::is_whitespace) {
            keys.entry(key.to_owned())
                .or_default()
                .push(value.trim().to_owned());
        }
    }
    keys
}

fn fixture_units(dir: &Path) -> Vec<String> {
    fixture_header(dir)
        .remove("units")
        .unwrap_or_else(|| panic!("{}: fixture.txt has no `units`", dir.display()))
        .join(" ")
        .split_whitespace()
        .map(str::to_owned)
        .collect()
}

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
    let census = census_of(program);
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
/// The walk matches **exhaustively** on every statement and data entry, so a
/// construct that grows a block later stops this file from compiling rather
/// than silently going uncounted. That is the whole reason it is written out
/// instead of matching only the three kinds it cares about.
fn census_of(file: &AstFile) -> Census {
    let mut census = Census::default();
    for item in &file.items {
        let ItemKind::Func(function) = &item.kind else {
            if let ItemKind::Data(data) = &item.kind {
                data_block(data, &mut census);
            }
            continue;
        };
        census.functions += 1;
        if function
            .contract
            .as_ref()
            .and_then(|contract| contract.outputs.as_ref())
            .is_some_and(|outputs| !outputs.outputs.is_empty())
        {
            census.declaring += 1;
        }
        block(&function.body, &mut census);
    }
    census
}

fn block(body: &Block, census: &mut Census) {
    for statement in &body.statements {
        count(statement, census);
    }
}

fn count(statement: &Statement, census: &mut Census) {
    match &statement.kind {
        StatementKind::Call(_) => census.escape_hatch += 1,
        StatementKind::BareCall(call) => {
            if call.arguments.is_some() || call.outputs.is_some() {
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
        StatementKind::WidthBlock(scoped) => block(&scoped.body, census),
        StatementKind::Conditional(conditional) => {
            block(&conditional.body, census);
            if let Some(otherwise) = &conditional.otherwise {
                block(otherwise, census);
            }
        }
        StatementKind::ConditionalJump(jump) => count(&jump.jump, census),
        StatementKind::Loop(loop_statement) => block(&loop_statement.body, census),
        StatementKind::Nocross(body) => block(body, census),
        StatementKind::Data(data) => data_block(data, census),
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

fn data_block(data: &DataBlock, census: &mut Census) {
    for entry in &data.entries {
        data_entry(&entry.kind, census);
    }
}

fn data_entry(kind: &DataEntryKind, census: &mut Census) {
    match kind {
        DataEntryKind::Code { body, .. } => block(body, census),
        DataEntryKind::NocrossGroup(entries) => {
            for entry in entries {
                data_entry(&entry.kind, census);
            }
        }
        DataEntryKind::Repeat { body, .. } => data_block(body, census),
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

    let expected: Vec<(String, usize, u32)> = DIFFERENCE
        .iter()
        .map(|(name, operations, bytes, _)| ((*name).to_owned(), *operations, *bytes))
        .collect();
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
        "two one-byte transfers — chosen so that the saving is exact, since \
         `Narrowing::bytes` over-measures a width-dependent immediate"
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

/// The census counts a call wherever one can be written.
///
/// The corpus does not exercise this: no call site in `examples/`,
/// `tests/golden/` or `tests/compat/` sits inside a conditional, a loop, a
/// width block, a `nocross`, or a `data`/`code` subblock, so a walk that
/// stopped at the first brace would count the same totals. **A corpus that
/// does not use a construct is not coverage for it**, so the construct is
/// written here instead — one call of each spelling in each place a block can
/// nest, with the counts stated.
#[test]
fn the_census_follows_a_call_into_every_block_that_can_hold_one() {
    let source = "\
var CELL = $0400

func leaf @a8 @i8 {
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
    jsr leaf
}
";
    let measured = measure_sources(&[("census.k816".to_owned(), source.to_owned())], 0)
        .expect("the case compiles");
    assert_eq!(
        measured.census,
        Census {
            functions: 2,
            declaring: 0,
            // The top-level `data`, then the body: bare, both arms of the
            // conditional, the loop, the width block, the `nocross`, and the
            // `code` subblock.
            escape_hatch: 8,
            stating: 0,
            bare: 1,
            native: 1,
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
        total.functions += measured.census.functions;
        total.declaring += measured.census.declaring;
        total.escape_hatch += measured.census.escape_hatch;
        total.stating += measured.census.stating;
        total.bare += measured.census.bare;
        total.native += measured.census.native;
    }
    assert_eq!(
        total, CENSUS,
        "the corpus census moved. `postmortem.md` §G.22 reads these numbers \
         as the reason the count in `DIFFERENCE` is small",
    );
}
