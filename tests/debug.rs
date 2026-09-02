/*****************************************************************************/
/*                                                                           */
/*                                  debug.rs                                 */
/*                                                                           */
/*     The debug database, read the way its consumer reads it                */
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

//! Master-plan phase 12's "verify cc65-dbg source mappings", as sub-plan I's
//! decision 13 states it.
//!
//! **The gate is over the database the consumer reads, not over the
//! directives K816 writes.** `tests/emit_ca65.rs` already asserts the
//! directives, and a directive ca65 silently dropped would pass every one of
//! those assertions: what a debugger loads is what ld65 wrote after ca65
//! turned each `.dbg line` into a line record and each instruction into a
//! span. So every program in the corpus is built through `cl65 -g` with
//! `--dbgfile`, and the four properties below are read off the result.
//!
//! The database is parsed as text. It is line-oriented `key=value`, and
//! linking `src/dbginfo/` into a Rust test would be a build dependency for a
//! format that fits in the fifty lines below.

mod common;

use std::collections::{BTreeMap, BTreeSet};
use std::process::Command;
use std::sync::OnceLock;

use common::{run_ok, toolchain, work_dir};

#[path = "common/corpus.rs"]
mod corpus;

use corpus::{Program, compile_units, programs, stated_units, stem};

// ---------------------------------------------------------------------------
// The database
// ---------------------------------------------------------------------------

/// One record: its kind, and its `key=value` fields.
type Record = BTreeMap<String, String>;

/// Every record of the database, by kind.
struct Database {
    records: BTreeMap<String, Vec<Record>>,
}

impl Database {
    /// Parses `<kind>\t<key>=<value>,…` lines.
    ///
    /// Values may be quoted and a quoted value may hold a comma — a source
    /// path is a value, and a path may hold anything — so the field split is
    /// quote-aware rather than a `split(',')`.
    fn parse(text: &str) -> Self {
        let mut records: BTreeMap<String, Vec<Record>> = BTreeMap::new();
        for line in text.lines() {
            let Some((kind, rest)) = line.split_once('\t') else {
                continue;
            };
            let mut fields = Record::new();
            for field in split_fields(rest) {
                if let Some((key, value)) = field.split_once('=') {
                    fields.insert(key.to_owned(), value.trim_matches('"').to_owned());
                }
            }
            records.entry(kind.to_owned()).or_default().push(fields);
        }
        Self { records }
    }

    fn of(&self, kind: &str) -> &[Record] {
        self.records.get(kind).map_or(&[], Vec::as_slice)
    }
}

fn split_fields(text: &str) -> Vec<String> {
    let mut fields = Vec::new();
    let mut current = String::new();
    let mut quoted = false;
    for character in text.chars() {
        match character {
            '"' => {
                quoted = !quoted;
                current.push(character);
            }
            ',' if !quoted => fields.push(std::mem::take(&mut current)),
            _ => current.push(character),
        }
    }
    if !current.is_empty() {
        fields.push(current);
    }
    fields
}

fn number(record: &Record, key: &str) -> Option<u64> {
    let text = record.get(key)?;
    Some(match text.strip_prefix("0x") {
        Some(hex) => u64::from_str_radix(hex, 16).ok()?,
        None => text.parse().ok()?,
    })
}

/// A `span=21+20` field: the ids it names.
fn id_list(record: &Record, key: &str) -> Vec<u64> {
    record
        .get(key)
        .map(|text| text.split('+').filter_map(|id| id.parse().ok()).collect())
        .unwrap_or_default()
}

// ---------------------------------------------------------------------------
// Building one program with its database
// ---------------------------------------------------------------------------

/// A program built through `cl65 -g`, with the database and what K816 emitted.
struct Built {
    name: String,
    database: Database,
    /// The names every unit's assembly exports, which is what property (c)
    /// requires an addressed symbol for.
    exports: BTreeSet<String>,
}

/// Every corpus program that compiles, built once with its database.
///
/// Once, because all four properties read the same databases and building
/// the corpus four times would be four times the work — and because the four
/// tests are four threads of one process, so a scratch directory per program
/// per *test* is the alternative. Sharing one build is also what makes a
/// second property cost nothing to add.
fn built_corpus() -> &'static [Built] {
    static CORPUS: OnceLock<Vec<Built>> = OnceLock::new();
    CORPUS
        .get_or_init(|| programs().iter().filter_map(build).collect())
        .as_slice()
}

/// Builds `program` through the driver with debug info, or `None` if a unit
/// of it is a refusal.
fn build(program: &Program) -> Option<Built> {
    let name = program.name.clone();
    let directory = work_dir(&format!("debug/{}", stem(&program.name)));
    let assemblies = compile_units(&directory, program)?;

    let mut exports = BTreeSet::new();
    for assembly in &assemblies {
        let text = std::fs::read_to_string(assembly).expect("read a generated assembly");
        for line in text.lines() {
            // `.export name` or `.export name: far` — the address-size
            // qualifier a `far` function's export carries, which ld65 records
            // on the symbol rather than in its name.
            if let Some(rest) = line.trim_start().strip_prefix(".export ") {
                let name: String = rest
                    .trim_start()
                    .chars()
                    .take_while(|c| c.is_alphanumeric() || *c == '_')
                    .collect();
                assert!(!name.is_empty(), "an .export with no name: {line}");
                exports.insert(name);
            }
        }
    }

    let database = directory.join("out.dbg");
    let mut command = Command::new(toolchain().join("cl65"));
    command
        .arg("-g")
        .args(["-t", "none", "--no-target-lib"])
        .args(["-C".as_ref(), program.config.as_os_str()])
        .arg("-Wl")
        .arg(format!("--dbgfile,{}", database.display()))
        .args(["-o".as_ref(), directory.join("out.bin").as_os_str()]);
    for source in &program.extra {
        command.arg(source);
    }
    // The same units `compile_units` compiled: a K65 fixture that does not
    // name its own siblings is staged with the `import` lines a K816 port of
    // it would carry, and the driver has to be handed those rather than the
    // originals.
    for unit in stated_units(&directory, &program.units) {
        command.arg(unit);
    }
    run_ok(&mut command, "cl65 -g building a K816 program");

    Some(Built {
        name,
        database: Database::parse(
            &std::fs::read_to_string(&database).expect("read the debug database"),
        ),
        exports,
    })
}

/// The `file` records that are K816 sources: id and name.
///
/// A K816 source reaches the file table through the `.dbg file` K816 emits,
/// and it is the *source* — the intermediate assembly ca65 actually read is a
/// separate entry with a temporary name.
fn k816_files(database: &Database) -> BTreeMap<u64, String> {
    database
        .of("file")
        .iter()
        .filter_map(|record| {
            let name = record.get("name")?;
            (name.ends_with(".k816") || name.ends_with(".k65"))
                .then(|| Some((number(record, "id")?, name.clone())))?
        })
        .collect()
}

/// The modules a K816 source contributed to.
///
/// A file's `mod` is an id *list*: a unit that declares an `inline` another
/// unit expands appears in both modules (`mod=0+1`), because the expansion
/// carries the spans of the file that wrote it. Reading it as a single number
/// silently drops the file, and then every borrowed byte looks unattributed —
/// which is how this was found.
fn k816_modules(database: &Database) -> BTreeSet<u64> {
    database
        .of("file")
        .iter()
        .filter(|record| {
            record
                .get("name")
                .is_some_and(|name| name.ends_with(".k816") || name.ends_with(".k65"))
        })
        .flat_map(|record| id_list(record, "mod"))
        .collect()
}

/// `span id → (segment, start, size)`.
fn spans(database: &Database) -> BTreeMap<u64, (u64, u64, u64)> {
    database
        .of("span")
        .iter()
        .filter_map(|record| {
            Some((
                number(record, "id")?,
                (
                    number(record, "seg")?,
                    number(record, "start")?,
                    number(record, "size")?,
                ),
            ))
        })
        .collect()
}

/// One byte of the linked program: which segment, and how far into it.
type Byte = (u64, u64);

/// The `(segment, offset)` bytes a set of spans covers.
fn covered(
    span_table: &BTreeMap<u64, (u64, u64, u64)>,
    ids: impl IntoIterator<Item = u64>,
) -> BTreeSet<Byte> {
    let mut bytes = BTreeSet::new();
    for id in ids {
        let Some(&(segment, start, size)) = span_table.get(&id) else {
            continue;
        };
        for offset in start..start + size {
            bytes.insert((segment, offset));
        }
    }
    bytes
}

// ---------------------------------------------------------------------------
// (a) Every byte a K816 module contributed is attributed to a source line
// ---------------------------------------------------------------------------

/// Every byte a K816 module produced is covered by a line record of its own.
///
/// The module's extent is its scopes' spans, which is how ld65 records what a
/// module contributed, and it covers the reservations as well as the code:
/// K816 emits a `.dbg line` before every operation, not just before every
/// instruction, so a `var` in BSS is attributed to the line that declared it.
/// A debugger stopped anywhere in a K816 program can therefore always answer
/// "which line is this".
///
/// `type=1` is `LI_TYPE_EXT`, which is what ca65 records for a line that came
/// from a `.dbg line` directive rather than from the assembly's own text. The
/// distinction matters: every byte also has an ordinary assembly line record
/// naming the *intermediate*, and a gate that accepted those would pass on a
/// program with no K816 attribution at all.
/// The bytes a K816 module emitted, and the ones a source line claims.
///
/// A unit that is only declarations — `examples/k65/atari-xl/include` is 213
/// lines of equates — contributes no bytes at all, and its extent is empty.
fn attribution(built: &Built) -> (BTreeSet<Byte>, BTreeSet<Byte>) {
    let files = k816_files(&built.database);
    let span_table = spans(&built.database);
    let modules = k816_modules(&built.database);

    let extent = covered(
        &span_table,
        built
            .database
            .of("scope")
            .iter()
            .filter(|scope| number(scope, "mod").is_some_and(|id| modules.contains(&id)))
            .flat_map(|scope| id_list(scope, "span")),
    );
    let attributed = covered(
        &span_table,
        built
            .database
            .of("line")
            .iter()
            .filter(|line| line.get("type").map(String::as_str) == Some("1"))
            .filter(|line| number(line, "file").is_some_and(|id| files.contains_key(&id)))
            .flat_map(|line| id_list(line, "span")),
    );
    (extent, attributed)
}

#[test]
fn every_byte_a_k816_module_emits_is_attributed_to_a_source_line() {
    let mut checked = 0;
    for built in built_corpus() {
        let (extent, attributed) = attribution(built);
        if extent.is_empty() {
            continue;
        }
        let orphans: Vec<Byte> = extent.difference(&attributed).copied().collect();
        assert!(
            orphans.is_empty(),
            "{}: {} of the {} bytes the K816 module emitted are attributed to no \
             source line. First: segment {} offset {}.",
            built.name,
            orphans.len(),
            extent.len(),
            orphans[0].0,
            orphans[0].1,
        );
        checked += 1;
    }
    assert!(checked > 60, "only {checked} programs were checked");
}

// ---------------------------------------------------------------------------
// (b) The line a record names holds something
// ---------------------------------------------------------------------------

/// The lines of `text` that hold something other than whitespace and comments.
///
/// A one-pass scanner rather than a regex, because `//` inside a string is not
/// a comment and `*/` inside a `//` comment does not end anything. Lines are
/// 1-based, matching a `.dbg line` record.
fn lines_with_content(text: &str) -> BTreeSet<usize> {
    let mut lines = BTreeSet::new();
    let mut line = 1usize;
    let mut chars = text.chars().peekable();
    let (mut in_block, mut in_line_comment, mut in_string) = (false, false, false);
    while let Some(character) = chars.next() {
        if character == '\n' {
            line += 1;
            in_line_comment = false;
            in_string = false;
            continue;
        }
        if in_line_comment {
            continue;
        }
        if in_block {
            if character == '*' && chars.peek() == Some(&'/') {
                chars.next();
                in_block = false;
            }
            continue;
        }
        if in_string {
            lines.insert(line);
            if character == '"' {
                in_string = false;
            }
            continue;
        }
        match character {
            '/' if chars.peek() == Some(&'/') => {
                chars.next();
                in_line_comment = true;
            }
            '/' if chars.peek() == Some(&'*') => {
                chars.next();
                in_block = true;
            }
            '"' => {
                in_string = true;
                lines.insert(line);
            }
            c if c.is_whitespace() => {}
            _ => {
                lines.insert(line);
            }
        }
    }
    lines
}

/// Every line a debug record names is a line with something on it.
///
/// A record pointing at a blank line or at a comment is worse than no record:
/// the debugger highlights it, and the reader concludes the compiler put the
/// instruction there. The commonest way to produce one is an off-by-one in a
/// span-to-line conversion, which no assertion on the emitted directives can
/// see — the directive is well-formed either way.
/// `(records checked, the ones naming a blank or comment-only line)`.
fn line_records(built: &Built) -> (usize, Vec<String>) {
    let mut sources: BTreeMap<String, BTreeSet<usize>> = BTreeMap::new();
    let mut checked = 0;
    let mut bad = Vec::new();
    let files = k816_files(&built.database);
    for record in built.database.of("line") {
        if record.get("type").map(String::as_str) != Some("1") {
            continue;
        }
        let Some(name) = number(record, "file").and_then(|id| files.get(&id)) else {
            continue;
        };
        // Line 0 is ca65's "no line", and it names nothing.
        let Some(line) = number(record, "line").filter(|line| *line != 0) else {
            continue;
        };
        let content = sources.entry(name.clone()).or_insert_with(|| {
            lines_with_content(&std::fs::read_to_string(name).expect("read a corpus source"))
        });
        if !content.contains(&(line as usize)) {
            bad.push(format!("{name}:{line}"));
        }
        checked += 1;
    }
    (checked, bad)
}

#[test]
fn every_line_a_record_names_is_a_line_of_the_program() {
    let mut checked = 0;
    for built in built_corpus() {
        let (counted, bad) = line_records(built);
        assert!(
            bad.is_empty(),
            "{}: a debug record points at {}, which is blank or only a comment",
            built.name,
            bad[0]
        );
        checked += counted;
    }
    assert!(checked > 1000, "only {checked} line records were checked");
}

// ---------------------------------------------------------------------------
// (c) Every name K816 exports has an address in the database
// ---------------------------------------------------------------------------

/// Every name a K816 unit exports is a symbol with an address.
///
/// A function whose name a debugger cannot resolve is a function nobody can
/// set a breakpoint on, and `doc/design.md` §11's whole claim about the debug
/// database is that a K816 program is debuggable like an assembly one. The
/// symbols come from ca65's `-g`, which `cl65` passes to the assembler for a
/// K816 intermediate exactly as it does for a C file's — so this is also the
/// gate that would notice `-g` ceasing to reach it.
///
/// An import has no address, and the same name is usually two records — an
/// `imp` in the caller's scope and a `lab` in the definer's — so the property
/// is that *some* record for the name carries a value.
#[test]
fn every_name_a_unit_exports_is_an_addressed_symbol() {
    let mut checked = 0;
    for built in built_corpus() {
        let addressed: BTreeSet<&str> = built
            .database
            .of("sym")
            .iter()
            .filter(|record| record.contains_key("val"))
            .filter_map(|record| record.get("name").map(String::as_str))
            .collect();

        for name in &built.exports {
            assert!(
                addressed.contains(name.as_str()),
                "{}: `{name}` is exported but has no addressed symbol in the \
                 debug database",
                built.name
            );
            checked += 1;
        }
    }
    assert!(checked > 100, "only {checked} exported names were checked");
}

// ---------------------------------------------------------------------------
// (d) A borrowed body keeps its own file
// ---------------------------------------------------------------------------

/// An `inline` another unit declares is attributed to *that* unit's source.
///
/// Sub-plan G7 made a cross-unit `inline` expand into the unit that uses it,
/// and its operations keep the spans of the file that wrote them. That is why
/// `emit_ca65.rs` emits a second `.dbg file`: a `.dbg line` naming a file ca65
/// was never told about is a debug entry ld65 cannot place, and one naming the
/// *using* unit would point the debugger at whatever happens to be on that
/// line over there.
///
/// The consequence in the database is visible and specific: the borrowed
/// file's record carries a `mod` id list naming both modules, and the line
/// records for the expanded body name the declaring file while sitting in the
/// using module's bytes.
#[test]
fn a_borrowed_inline_body_is_attributed_to_the_unit_that_wrote_it() {
    let built = built_corpus()
        .iter()
        .find(|built| {
            built
                .name
                .starts_with("tests/compat/k816/inline-across-units/")
        })
        .expect("the cross-unit inline fixture is in the corpus and compiles");

    let files = k816_files(&built.database);
    let declaring = files
        .iter()
        .find(|(_, name)| name.ends_with("defs.k65"))
        .map(|(id, _)| *id)
        .expect("defs.k65 is in the file table");
    let using = files
        .iter()
        .find(|(_, name)| name.ends_with("main.k65"))
        .map(|(id, _)| *id)
        .expect("main.k65 is in the file table");

    // The declaring unit emits nothing of its own — the `inline` has no
    // address — so every byte it owns is inside the *using* module.
    let modules: BTreeSet<u64> = built
        .database
        .of("file")
        .iter()
        .filter(|record| number(record, "id") == Some(declaring))
        .flat_map(|record| id_list(record, "mod"))
        .collect();
    assert!(
        modules.len() > 1,
        "defs.k65 is recorded in one module, so its body was not expanded \
         across the boundary: {modules:?}"
    );

    let span_table = spans(&built.database);
    let attributed = |file: u64| {
        covered(
            &span_table,
            built
                .database
                .of("line")
                .iter()
                .filter(|line| line.get("type").map(String::as_str) == Some("1"))
                .filter(|line| number(line, "file") == Some(file))
                .flat_map(|line| id_list(line, "span")),
        )
    };
    let borrowed = attributed(declaring);
    let own = attributed(using);
    assert!(
        !borrowed.is_empty(),
        "no byte is attributed to the unit that declared the inline"
    );
    assert!(
        borrowed.is_disjoint(&own),
        "a byte is attributed to both units: {:?}",
        borrowed.intersection(&own).collect::<Vec<_>>()
    );
}

// ---------------------------------------------------------------------------
// The gate, mutation-tested
// ---------------------------------------------------------------------------
//
// A green first run means nothing until the gate has been shown to go red
// (`memory/idempotence-catches-pass-ordering`'s sibling lesson, G12). Both
// mutations are made to the *assembly*, between `k816 compile` and `ca65`,
// because that is where a lost or misplaced `.dbg line` would come from: the
// database is downstream of it, and a mutation to the database itself would
// only test the parser.

/// Builds one program by hand, with `mutate` applied to each unit's assembly.
///
/// `cl65` cannot be used here — it removes the intermediate, and the point is
/// to edit it — so this is the manual recipe. That is fine for a mutation:
/// what is under test is the *property*, and `tests/driver.rs` has already
/// established that the driver produces the manual path's bytes.
fn build_mutated(test: &str, program: &Program, mutate: impl Fn(&str) -> String) -> Built {
    let directory = work_dir(&format!("debug-mutation/{test}"));
    let assemblies = compile_units(&directory, program).expect("the program compiles");

    let mut objects = Vec::new();
    let mut exports = BTreeSet::new();
    for (index, assembly) in assemblies.iter().enumerate() {
        let text = std::fs::read_to_string(assembly).expect("read a generated assembly");
        for line in text.lines() {
            if let Some(rest) = line.trim_start().strip_prefix(".export ") {
                exports.insert(
                    rest.trim_start()
                        .chars()
                        .take_while(|c| c.is_alphanumeric() || *c == '_')
                        .collect(),
                );
            }
        }
        std::fs::write(assembly, mutate(&text)).expect("write the mutated assembly");

        let object = directory.join(format!("unit-{index}.o"));
        run_ok(
            Command::new(toolchain().join("ca65"))
                .arg("-g")
                .arg(assembly)
                .args(["-o".as_ref(), object.as_os_str()]),
            "ca65 on a mutated module",
        );
        objects.push(object);
    }

    let database = directory.join("out.dbg");
    let mut link = Command::new(toolchain().join("ld65"));
    link.args(["-C".as_ref(), program.config.as_os_str()])
        .args(["--dbgfile".as_ref(), database.as_os_str()])
        .args(["-o".as_ref(), directory.join("out.bin").as_os_str()]);
    for object in &objects {
        link.arg(object);
    }
    run_ok(&mut link, "ld65 linking a mutated program");

    Built {
        name: program.name.clone(),
        database: Database::parse(
            &std::fs::read_to_string(&database).expect("read the debug database"),
        ),
        exports,
    }
}

/// A corpus program with code, reservations, and no layout of its own.
fn mutation_subject() -> Program {
    programs()
        .into_iter()
        .find(|program| program.name == "tests/golden/vars.k816")
        .expect("tests/golden/vars.k816 is in the corpus")
}

/// Dropping one `.dbg line` leaves bytes nothing attributes.
#[test]
fn a_lost_line_directive_makes_the_attribution_gate_fail() {
    let program = mutation_subject();

    // Unmutated, as the control: the same build, the same property, green.
    let (extent, attributed) = attribution(&build_mutated("control", &program, str::to_owned));
    assert!(
        extent.difference(&attributed).next().is_none(),
        "the control build is already unattributed, so the mutation proves nothing"
    );

    let mutated = build_mutated("lost-line", &program, |text| {
        let mut dropped = false;
        text.lines()
            .filter(|line| {
                let is_directive = line.trim_start().starts_with(".dbg    line");
                let drop = is_directive && !dropped;
                dropped |= drop;
                !drop
            })
            .map(|line| format!("{line}\n"))
            .collect()
    });
    let (extent, attributed) = attribution(&mutated);
    assert!(
        extent.difference(&attributed).next().is_some(),
        "a `.dbg line` was dropped and every byte is still attributed - the gate \
         is not reading what it thinks it is"
    );
}

/// Moving a line number onto a comment makes the line gate fail.
#[test]
fn a_line_number_moved_onto_a_comment_makes_the_line_gate_fail() {
    let program = mutation_subject();
    let source = &program.units[0];
    let text = std::fs::read_to_string(source).expect("read the subject");
    let content = lines_with_content(&text);
    let comment = (1..=text.lines().count())
        .find(|line| !content.contains(line))
        .expect("the subject has a blank or comment-only line");

    let (checked, bad) = line_records(&build_mutated("control-lines", &program, str::to_owned));
    assert!(
        checked > 0 && bad.is_empty(),
        "the control build is already bad"
    );

    let name = source.to_string_lossy().into_owned();
    let mutated = build_mutated("comment-line", &program, |text| {
        let mut moved = false;
        text.lines()
            .map(|line| {
                let prefix = format!(".dbg    line, \"{name}\", ");
                if !moved && line.trim_start().starts_with(&prefix) {
                    moved = true;
                    return format!("        {prefix}{comment}\n");
                }
                format!("{line}\n")
            })
            .collect()
    });
    let (_, bad) = line_records(&mutated);
    assert_eq!(
        bad,
        vec![format!("{name}:{comment}")],
        "a record was pointed at a comment-only line and the gate did not say so"
    );
}

/// A K816 module's debug data does not depend on `cl65 -g`; a hand-written
/// module's does.
///
/// Decision 8 expected `-g` to be what put the K816 module's labels in the
/// database. It is not: K816 emits `.debuginfo on` at the top of every
/// assembly it writes, so ca65 records that module's spans, line records and
/// symbols whether or not anyone asked, and a K816 program is debuggable
/// through a `cl65` build that never mentions debugging. **[I3]**
///
/// What `-g` buys is the *rest* of the program. `examples/fullstack` links a
/// hand-written `startup.asm`, and nothing in that file turns debug info on —
/// so without `-g` it contributes no scope, and a debugger stopped in the
/// startup stub has nothing to say. That is the half of the flag a K816 build
/// still wants, and the reason `cl65` passing it to ca65 for both kinds of
/// module is the right arrangement.
#[test]
fn the_k816_modules_debug_data_does_not_depend_on_the_debug_flag() {
    let program = programs()
        .into_iter()
        .find(|program| program.name == "examples/fullstack/main.k816")
        .expect("the full-stack example is in the corpus");

    let build_with = |test: &str, debug: bool| -> Database {
        let directory = work_dir(&format!("debug-flag/{test}"));
        let database = directory.join("out.dbg");
        let mut command = Command::new(toolchain().join("cl65"));
        if debug {
            command.arg("-g");
        }
        command
            .args(["-t", "none", "--no-target-lib"])
            .args(["-C".as_ref(), program.config.as_os_str()])
            .arg("-Wl")
            .arg(format!("--dbgfile,{}", database.display()))
            .args(["-o".as_ref(), directory.join("out.bin").as_os_str()])
            .arg(&program.extra[0])
            .arg(&program.units[0]);
        run_ok(&mut command, "cl65 building the full-stack example");
        Database::parse(&std::fs::read_to_string(&database).expect("read the debug database"))
    };

    let without = build_with("without", false);
    let with = build_with("with", true);

    // The K816 module's own attribution is the same either way.
    let k816_extent = |database: &Database| {
        let modules = k816_modules(database);
        let span_table = spans(database);
        let ranges = covered(
            &span_table,
            database
                .of("scope")
                .iter()
                .filter(|scope| number(scope, "mod").is_some_and(|id| modules.contains(&id)))
                .flat_map(|scope| id_list(scope, "span")),
        );
        // By offset within the segment, because the span *ids* differ when
        // the other module contributes spans of its own.
        ranges.len()
    };
    assert_eq!(
        k816_extent(&without),
        k816_extent(&with),
        "the K816 module's extent changed with -g"
    );
    let k816_lines = |database: &Database| {
        database
            .of("line")
            .iter()
            .filter(|line| line.get("type").map(String::as_str) == Some("1"))
            .count()
    };
    assert_eq!(
        k816_lines(&without),
        k816_lines(&with),
        "the K816 module's line records changed with -g"
    );
    assert!(
        k816_lines(&with) > 0,
        "there are no K816 line records at all"
    );

    // The hand-written module's does not exist without it.
    let modules = |database: &Database| -> BTreeSet<u64> {
        database
            .of("scope")
            .iter()
            .filter_map(|scope| number(scope, "mod"))
            .collect()
    };
    assert_eq!(
        modules(&with).len(),
        2,
        "with -g, both modules have a scope"
    );
    assert_eq!(
        modules(&without).len(),
        1,
        "without -g, only the module that asked for debug info has a scope"
    );
}
