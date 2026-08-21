/*****************************************************************************/
/*                                                                           */
/*                                 compat.rs                                 */
/*                                                                           */
/*        The K65 compatibility corpus and the manifest that indexes it      */
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

//! Sub-plan G's corpus: three sets of fixtures under `tests/compat/`, and
//! `plans/compat-manifest.md`, which classifies every rule of the original K65
//! grammar and every K816 construct that is not one of them.
//!
//! The three sets answer different questions
//! (`plans/06-compatibility.md`, "The fixture programs"):
//!
//!   - `legacy-k816/` — the legacy compiler's fixtures, imported as *inputs*
//!     with their expectations re-derived. Where the two compilers disagree,
//!     each fixture's note says which is right and why.
//!   - `k65/` — sources the original K65 compiler was asked about, with its
//!     answer committed beside them as `oracle.hex`. `cargo test` never runs
//!     the original: `tests/compat/oracle.sh` is how those bytes were made
//!     and how they are re-made (decision 2).
//!   - `k816/` — fixtures invented for the decisions G takes, one per claim.
//!
//! **What this file asserts and what it does not.** It asserts that K816's
//! behaviour on each fixture is what the fixture says it is, and that the
//! manifest and the corpus index each other. It does **not** assert that
//! K816's bytes equal the original's: the two lay sections out differently and
//! the original's linker prepends a startup stub K816 does not generate, so
//! byte equality would be a false claim. A row's `compatible` verdict is
//! argued in the manifest, with `oracle.hex` as its evidence; what the suite
//! guards is that the argued behaviour does not change unnoticed.

mod common;

use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};
use std::process::Command;

use common::{run_ok, tool, work_dir};
use k816::source::SourceMap;
use k816::{Options, compile_to_ca65};

/// The original grammar's length, in lines. Every one of them is classified
/// by the manifest — either by a row, or by the block of lines that carry no
/// production.
const GRAMMAR_LINES: usize = 515;

fn root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

fn compat_dir() -> PathBuf {
    root().join("tests/compat")
}

// ---------------------------------------------------------------------------
// Fixtures
// ---------------------------------------------------------------------------

/// One fixture directory, as `fixture.txt` describes it.
///
/// The file is `key value` lines up to the first blank line, then prose. The
/// prose is for a reader and is deliberately not parsed: what a fixture
/// *proves* is an argument, and an argument does not belong in an assertion.
struct Fixture {
    name: String,
    dir: PathBuf,
    row: String,
    config: String,
    compare: String,
    units: Vec<String>,
    expect: Vec<String>,
}

fn fixtures() -> Vec<Fixture> {
    let mut out = Vec::new();
    for set in ["legacy-k816", "k65", "k816"] {
        let set_dir = compat_dir().join(set);
        let Ok(entries) = std::fs::read_dir(&set_dir) else {
            continue;
        };
        let mut dirs: Vec<PathBuf> = entries
            .map(|e| e.expect("read tests/compat entry").path())
            .filter(|p| p.is_dir())
            .collect();
        dirs.sort();
        for dir in dirs {
            let name = format!("{set}/{}", dir.file_name().unwrap().to_string_lossy());
            let text = std::fs::read_to_string(dir.join("fixture.txt"))
                .unwrap_or_else(|e| panic!("{name}: cannot read fixture.txt: {e}"));
            let mut keys: BTreeMap<&str, Vec<&str>> = BTreeMap::new();
            for line in text.lines().take_while(|l| !l.trim().is_empty()) {
                let (key, value) = line
                    .split_once(char::is_whitespace)
                    .unwrap_or_else(|| panic!("{name}: `{line}` is not a `key value` line"));
                keys.entry(key).or_default().push(value.trim());
            }
            let one = |key: &str| -> String {
                let values = keys
                    .get(key)
                    .unwrap_or_else(|| panic!("{name}: fixture.txt has no `{key}`"));
                assert_eq!(values.len(), 1, "{name}: `{key}` is given more than once");
                values[0].to_string()
            };
            out.push(Fixture {
                row: one("row"),
                config: one("config"),
                compare: one("compare"),
                units: one("units").split_whitespace().map(String::from).collect(),
                expect: keys
                    .get("expect")
                    .map(|v| v.iter().map(|s| s.to_string()).collect())
                    .unwrap_or_default(),
                name,
                dir,
            });
        }
    }
    assert!(!out.is_empty(), "tests/compat/ holds no fixtures");
    out
}

/// Compiles a fixture's units, returning the diagnostic codes and, when there
/// are none, the assembly.
fn compile(fixture: &Fixture) -> (Vec<String>, Option<String>) {
    let mut map = SourceMap::new();
    let mut entry = None;
    for unit in &fixture.units {
        let path = fixture.dir.join(unit);
        let text = std::fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("{}: cannot read {unit}: {e}", fixture.name));
        // The last unit is the one compiled; the others are its siblings, the
        // same shape `--unit` gives the command line.
        entry = Some(map.add_file(path.to_string_lossy(), text));
    }
    let result = compile_to_ca65(
        &map,
        entry.expect("a fixture names at least one unit"),
        &Options { optimize: true },
    );
    let codes = result
        .diagnostics
        .iter()
        .map(|d| d.code.as_str().to_string())
        .collect();
    (codes, result.assembly)
}

/// Reads a committed `.hex` golden: `origin AAAA` then hex bytes per line,
/// `#` comments ignored.
fn read_hex(path: &Path) -> (u32, Vec<u8>) {
    let text = std::fs::read_to_string(path)
        .unwrap_or_else(|e| panic!("cannot read {}: {e}", path.display()));
    let mut origin = None;
    let mut bytes = Vec::new();
    for line in text.lines() {
        let line = line.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }
        if let Some(rest) = line.strip_prefix("origin ") {
            origin = Some(u32::from_str_radix(rest.trim(), 16).expect("origin is hex"));
            continue;
        }
        // `AAAA  xx xx xx …` — the address column is a reader's aid.
        for field in line.split_whitespace().skip(1) {
            bytes.push(
                u8::from_str_radix(field, 16)
                    .unwrap_or_else(|_| panic!("{}: `{field}` is not a hex byte", path.display())),
            );
        }
    }
    (
        origin.expect("a .hex golden starts with an `origin` line"),
        bytes,
    )
}

fn format_hex(origin: u32, bytes: &[u8]) -> String {
    bytes
        .chunks(16)
        .enumerate()
        .map(|(i, chunk)| {
            let mut line = format!("{:04X} ", origin as usize + i * 16);
            for byte in chunk {
                line.push_str(&format!(" {byte:02X}"));
            }
            line
        })
        .collect::<Vec<_>>()
        .join("\n")
}

/// Assembles and links a fixture, returning the image with its trailing fill
/// removed — the fill is the configuration's, not the program's.
///
/// The trim is by value, so a program whose last byte is genuinely `$00` loses
/// it. That is a fair trade for `atarixl.cfg`, which fills 20 KiB: a golden
/// that is mostly zeros is a golden nobody reads. A fixture that needs to
/// assert a trailing zero ends with something else.
fn assemble_and_link(fixture: &Fixture, assembly: &str) -> Vec<u8> {
    let dir = work_dir("compat");
    let stem = fixture.name.replace('/', "-");
    let asm = dir.join(format!("{stem}.s"));
    let obj = dir.join(format!("{stem}.o"));
    let bin = dir.join(format!("{stem}.bin"));
    std::fs::write(&asm, assembly).expect("write assembly");
    run_ok(
        Command::new(tool("CA65", "ca65"))
            .arg(&asm)
            .args(["-o".as_ref(), obj.as_os_str()]),
        "ca65 (build it with `make -C ../.. ca65` first)",
    );
    let config = compat_dir().join(&fixture.config);
    run_ok(
        Command::new(tool("LD65", "ld65"))
            .args(["-C".as_ref(), config.as_os_str()])
            .args(["-o".as_ref(), bin.as_os_str()])
            .arg(&obj),
        "ld65 (build it with `make -C ../.. ld65` first)",
    );
    let mut bytes = std::fs::read(&bin).expect("read linked binary");
    while bytes.last() == Some(&0) {
        bytes.pop();
    }
    bytes
}

/// Every fixture behaves the way its `fixture.txt` says it does.
///
/// A fixture that does not compile lists the codes it reports, in order: a row
/// that goes silent is a construct that landed, a row that grows is a
/// regression, and both should be noticed here. A fixture that does compile
/// commits its linked bytes, because "it compiles" is not the claim — the
/// claim is which bytes.
#[test]
fn every_fixture_behaves_as_its_header_says() {
    for fixture in fixtures() {
        let (codes, assembly) = compile(&fixture);
        assert_eq!(
            codes, fixture.expect,
            "{}: the diagnostics changed",
            fixture.name
        );
        match fixture.compare.as_str() {
            "diagnostics" => assert!(
                !fixture.expect.is_empty(),
                "{}: `compare diagnostics` with nothing to compare - a fixture \
                 that compiles cleanly compares its bytes",
                fixture.name
            ),
            "bytes" => {
                let assembly = assembly.unwrap_or_else(|| {
                    panic!("{}: `compare bytes` but nothing was emitted", fixture.name)
                });
                let bytes = assemble_and_link(&fixture, &assembly);
                let (origin, expected) = read_hex(&fixture.dir.join("expected.hex"));
                assert_eq!(
                    format_hex(origin, &bytes),
                    format_hex(origin, &expected),
                    "{}: the linked bytes changed.\n\
                     If the change is intended, say so in fixture.txt and \
                     re-bless expected.hex; a re-blessed diff with no sentence \
                     beside it is how a wrong-bytes bug ships.",
                    fixture.name
                );
            }
            other => panic!("{}: unknown `compare {other}`", fixture.name),
        }
    }
}

/// Every fixture's configuration exists, and every configuration is used.
#[test]
fn the_link_configurations_are_the_ones_the_fixtures_name() {
    let used: BTreeSet<String> = fixtures().into_iter().map(|f| f.config).collect();
    for config in &used {
        assert!(
            compat_dir().join(config).exists(),
            "a fixture names tests/compat/{config}, which does not exist"
        );
    }
    let mut present = BTreeSet::new();
    for entry in std::fs::read_dir(compat_dir()).expect("read tests/compat") {
        let path = entry.expect("read tests/compat entry").path();
        if path.extension().is_some_and(|e| e == "cfg") {
            present.insert(path.file_name().unwrap().to_string_lossy().into_owned());
        }
    }
    assert_eq!(
        present, used,
        "tests/compat holds a link configuration no fixture uses, or the other \
         way round"
    );
}

// ---------------------------------------------------------------------------
// The manifest
// ---------------------------------------------------------------------------

struct Row {
    id: String,
    lines: Vec<usize>,
    status: String,
    fixture: String,
}

/// Parses `plans/compat-manifest.md`.
///
/// The file is written to be read by this function — one row per line, fixed
/// columns — which `tests/manifest.rs` could not do for the feature manifest
/// (`plans/06-compatibility.md`, "Tests / fixtures"). Both tables are parsed
/// the same way; the grammar table carries line numbers and the extension
/// table does not.
fn manifest_rows() -> (Vec<Row>, String) {
    let text = std::fs::read_to_string(root().join("plans/compat-manifest.md"))
        .expect("read plans/compat-manifest.md");
    let mut rows = Vec::new();
    for line in text.lines() {
        let line = line.trim();
        if !line.starts_with("| `") {
            continue;
        }
        // A `Construct` cell spells alternatives with `\|`, which is a
        // Markdown-escaped pipe and not a column separator.
        let unescaped = line.replace("\\|", "\u{1}");
        let cells: Vec<String> = unescaped
            .trim_matches('|')
            .split('|')
            .map(|cell| cell.trim().replace('\u{1}', "|"))
            .collect();
        // The status table at the top of the file is two columns wide; the
        // grammar table is six and the extension table five.
        if cells.len() < 5 {
            continue;
        }
        let id = cells[0].trim_matches('`').to_string();
        // The grammar table is `id | lines | construct | status | fixture |
        // reason`; the extension table drops the line column.
        let (lines, status, fixture) = if cells.len() == 6 {
            (parse_lines(&id, &cells[1]), &cells[3], &cells[4])
        } else {
            (Vec::new(), &cells[2], &cells[3])
        };
        rows.push(Row {
            id,
            lines,
            status: status.to_string(),
            fixture: fixture.to_string(),
        });
    }
    assert!(!rows.is_empty(), "the manifest has no rows");
    (rows, text)
}

fn parse_lines(row: &str, cell: &str) -> Vec<usize> {
    let mut out = Vec::new();
    for part in cell.split(',') {
        let part = part.trim();
        if part.is_empty() {
            continue;
        }
        let numbers: Vec<usize> = part
            .split('-')
            .map(|n| {
                n.trim()
                    .parse()
                    .unwrap_or_else(|_| panic!("{row}: `{part}` is not a line range"))
            })
            .collect();
        match numbers.as_slice() {
            [only] => out.push(*only),
            [first, last] => out.extend(*first..=*last),
            _ => panic!("{row}: `{part}` is not a line range"),
        }
    }
    out
}

/// The walk covers the original grammar exactly once.
///
/// This is what makes "a row per rule" checkable rather than a claim. A rule
/// that was skipped leaves its lines uncovered; a rule filed twice leaves them
/// covered twice; and the lines that carry no production are listed
/// explicitly, so "there is nothing there" is also a statement someone made
/// rather than an omission.
#[test]
fn the_manifest_walks_every_line_of_the_original_grammar() {
    let (rows, text) = manifest_rows();
    let mut owner: BTreeMap<usize, String> = BTreeMap::new();
    let mut claim = |line: usize, by: &str| {
        if let Some(first) = owner.get(&line) {
            panic!("compiler.inc:{line} is classified by both `{first}` and `{by}`");
        }
        assert!(
            (1..=GRAMMAR_LINES).contains(&line),
            "`{by}` classifies compiler.inc:{line}, which is past the end of the grammar"
        );
        owner.insert(line, by.to_string());
    };
    for row in &rows {
        for line in &row.lines {
            claim(*line, &row.id);
        }
    }

    let tail = text
        .split_once("that carry no production:")
        .expect("the manifest ends with the unclassified lines")
        .1;
    for block in tail.lines() {
        let Some(block) = block.strip_prefix("    ") else {
            continue;
        };
        if !block.starts_with(|c: char| c.is_ascii_digit()) {
            continue;
        }
        for line in parse_lines("the unclassified block", block) {
            claim(line, "the unclassified block");
        }
    }

    let missing: Vec<usize> = (1..=GRAMMAR_LINES)
        .filter(|l| !owner.contains_key(l))
        .collect();
    assert!(
        missing.is_empty(),
        "compiler.inc lines {missing:?} are classified by no row and are not \
         listed as carrying no production"
    );
}

/// The manifest and the corpus index each other.
///
/// Every fixture answers to a row, every row that names a fixture names one
/// that exists, and no row id is used twice. A row whose fixture reads `—` is
/// waiting on the task its reason names; G10's criterion is that none do.
#[test]
fn the_manifest_and_the_corpus_agree() {
    let (rows, _) = manifest_rows();
    let ids: BTreeSet<&str> = rows.iter().map(|r| r.id.as_str()).collect();
    assert_eq!(ids.len(), rows.len(), "a row id is used twice");

    for row in &rows {
        if row.fixture == "—" || row.fixture == "-" {
            continue;
        }
        let path = match row.fixture.strip_prefix("golden/") {
            Some(rest) => root().join("tests/golden").join(rest),
            None => compat_dir().join(&row.fixture),
        };
        assert!(
            path.exists(),
            "row `{}` names the fixture `{}`, which does not exist",
            row.id,
            row.fixture
        );
    }

    for fixture in fixtures() {
        assert!(
            ids.contains(fixture.row.as_str()),
            "{} answers to the row `{}`, which the manifest does not have",
            fixture.name,
            fixture.row
        );
    }
}

/// Every status is one of the six the manifest defines.
#[test]
fn every_row_carries_a_status_the_manifest_defines() {
    const STATUSES: [&str; 6] = [
        "compatible",
        "intentionally-changed",
        "legacy-defective",
        "backend-different",
        "K816-extension",
        "open",
    ];
    for row in manifest_rows().0 {
        assert!(
            STATUSES.contains(&row.status.as_str()),
            "row `{}` has the status `{}`, which is not one of {STATUSES:?}",
            row.id,
            row.status
        );
    }
}
