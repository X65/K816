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
//! `doc/k65-compat-manifest.md`, which classifies every rule of the original K65
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
use k816::abi::EntryEnvironment;
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
#[derive(Clone)]
struct Fixture {
    name: String,
    dir: PathBuf,
    row: String,
    config: String,
    compare: String,
    units: Vec<String>,
    /// `strict yes` — compile this fixture with `--strict-k65`, so its
    /// `expect` list is what the flag says. Off by default, because every
    /// other fixture is about the ordinary build.
    strict: bool,
    /// `(unit, code)` in the order the compiler reports them, unit by unit.
    /// Every unit is compiled as an entry, so a fixture with siblings has an
    /// expectation per unit rather than one for whichever happens to be last.
    expect: Vec<(String, String)>,
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
                strict: keys.get("strict").is_some_and(|v| v.as_slice() == ["yes"]),
                expect: keys
                    .get("expect")
                    .map(|v| {
                        v.iter()
                            .map(|value| {
                                let (unit, code) =
                                    value.split_once(char::is_whitespace).unwrap_or_else(|| {
                                        panic!("{name}: `expect {value}` is not `<unit> <code>`")
                                    });
                                (unit.to_string(), code.trim().to_string())
                            })
                            .collect()
                    })
                    .unwrap_or_default(),
                name,
                dir,
            });
        }
    }
    assert!(!out.is_empty(), "tests/compat/ holds no fixtures");
    out
}

/// Compiles each of a fixture's units as an entry, with the others as its
/// siblings — the shape `--unit` gives the command line.
///
/// Every unit is compiled, not just the last: a fixture's units are a program,
/// and a program's second file reports its own diagnostics. Compiling only one
/// of them lets the rest rot unnoticed, which is exactly what happened to this
/// harness's first version.
fn compile(fixture: &Fixture) -> (Vec<(String, String)>, Vec<String>) {
    compile_texts(fixture, &sources_of(fixture))
}

/// Every unit's source, in the fixture's order.
fn sources_of(fixture: &Fixture) -> Vec<String> {
    fixture
        .units
        .iter()
        .map(|unit| {
            std::fs::read_to_string(fixture.dir.join(unit))
                .unwrap_or_else(|e| panic!("{}: cannot read {unit}: {e}", fixture.name))
        })
        .collect()
}

/// [`compile`], over sources supplied rather than read — which is what lets a
/// test compile a *modified* fixture and compare the two.
fn compile_texts(fixture: &Fixture, sources: &[String]) -> (Vec<(String, String)>, Vec<String>) {
    compile_with(fixture, sources, fixture.strict)
}

/// [`compile_texts`], with the flag chosen by the caller — which is what lets
/// `strict_mode_only_adds_diagnostics` compile one fixture both ways.
fn compile_with(
    fixture: &Fixture,
    sources: &[String],
    strict_k65: bool,
) -> (Vec<(String, String)>, Vec<String>) {
    let mut codes = Vec::new();
    let mut assemblies = Vec::new();
    for unit in &fixture.units {
        let mut map = SourceMap::new();
        let mut entry = None;
        for (other, text) in fixture.units.iter().zip(sources) {
            let path = fixture.dir.join(other);
            let id = map.add_file(path.to_string_lossy(), text.clone());
            if other == unit {
                entry = Some(id);
            }
        }
        let result = compile_to_ca65(
            &map,
            entry.expect("the entry unit is one of the units"),
            &Options {
                optimize: true,
                strict_k65,
                ..Options::default()
            },
        );
        codes.extend(
            result
                .diagnostics
                .iter()
                .map(|d| (unit.clone(), d.code.as_str().to_string())),
        );
        if let Some(assembly) = result.assembly {
            assemblies.push(assembly);
        }
    }
    (codes, assemblies)
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
fn assemble_and_link(fixture: &Fixture, assemblies: &[String]) -> Vec<u8> {
    let dir = work_dir("compat");
    let stem = fixture.name.replace('/', "-");
    let bin = dir.join(format!("{stem}.bin"));
    let mut objects = Vec::new();
    for (index, assembly) in assemblies.iter().enumerate() {
        let asm = dir.join(format!("{stem}-{index}.s"));
        let obj = dir.join(format!("{stem}-{index}.o"));
        std::fs::write(&asm, assembly).expect("write assembly");
        run_ok(
            Command::new(tool("CA65", "ca65"))
                .arg(&asm)
                // K816 emits a `binary`'s path exactly as the source wrote
                // it, and checked it against the source's own directory;
                // ca65 resolves the same name against its search list, and
                // the fixture's directory is what has to be on it. Passing
                // it is the driver's job in a real build
                // (`plans/06-compatibility.md` decision 8) and this
                // harness's here — the assembly goes to a scratch directory,
                // so without this every `binary` fixture would fail in the
                // assembler rather than in the compiler.
                .args(["--bin-include-dir".as_ref(), fixture.dir.as_os_str()])
                .args(["-o".as_ref(), obj.as_os_str()]),
            "ca65 (build it with `make -C ../.. ca65` first)",
        );
        objects.push(obj);
    }
    let config = compat_dir().join(&fixture.config);
    let mut link = Command::new(tool("LD65", "ld65"));
    link.args(["-C".as_ref(), config.as_os_str()])
        .args(["-o".as_ref(), bin.as_os_str()]);
    for object in &objects {
        link.arg(object);
    }
    run_ok(&mut link, "ld65 (build it with `make -C ../.. ld65` first)");
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
        let (codes, assemblies) = compile(&fixture);
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
                assert_eq!(
                    assemblies.len(),
                    fixture.units.len(),
                    "{}: `compare bytes` but not every unit emitted assembly",
                    fixture.name
                );
                let bytes = assemble_and_link(&fixture, &assemblies);
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

/// Wrapping a whole unit in `#if 1` … `#endif` changes nothing.
///
/// The claim `preprocess.rs` makes is that a directive selects parts of the
/// program and does nothing else. That claim is checkable over the whole
/// corpus rather than over examples of it, and checking it that way is what
/// finds the failure examples do not: every index the compiler keys a fact by
/// — a `var`'s storage, a function's `item_index`, a statement's id — is a
/// position in the tree *analysis* walked, and a directive shifts them all.
/// A stage that reaches for the parsed tree instead reads someone else's
/// entry and says nothing (`postmortem.md` §G.6.9).
///
/// Both sides are compared: the diagnostics, and — where the fixture commits
/// bytes — the linked image. Only the bytes could have caught the workspace
/// bug, which emitted a wrong-width immediate while exiting zero.
///
/// Fixtures whose own expectations include `K816-E0152` are skipped: they are
/// the ones about *unbalanced* regions, and wrapping an unbalanced file pairs
/// its stray directive with the wrapper.
#[test]
fn a_live_region_around_a_whole_unit_changes_nothing() {
    for fixture in fixtures() {
        if fixture.expect.iter().any(|(_, code)| code == "K816-E0152") {
            continue;
        }
        let plain = sources_of(&fixture);
        let wrapped: Vec<String> = plain
            .iter()
            .map(|text| {
                format!(
                    "#if 1
{text}
#endif
"
                )
            })
            .collect();
        let (plain_codes, plain_assemblies) = compile_texts(&fixture, &plain);
        let (wrapped_codes, wrapped_assemblies) = compile_texts(&fixture, &wrapped);
        assert_eq!(
            plain_codes, wrapped_codes,
            "{}: wrapping the unit in `#if 1` changed the diagnostics",
            fixture.name
        );
        if fixture.compare != "bytes" {
            continue;
        }
        assert_eq!(
            plain_assemblies.len(),
            wrapped_assemblies.len(),
            "{}: wrapping the unit in `#if 1` changed which units emitted",
            fixture.name
        );
        // Both sides get a stem of their own. The scratch directory is
        // shared with `every_fixture_behaves_as_its_header_says`, which runs
        // on another thread and assembles the same fixtures under the same
        // names; two writers on one `.o` is a corrupt read, not a failure
        // anyone would read as one.
        let mut plain_fixture = fixture.clone();
        plain_fixture.name = format!("{}-live-plain", fixture.name);
        let mut wrapped_fixture = fixture.clone();
        wrapped_fixture.name = format!("{}-live-wrapped", fixture.name);
        let plain_bytes = assemble_and_link(&plain_fixture, &plain_assemblies);
        let wrapped_bytes = assemble_and_link(&wrapped_fixture, &wrapped_assemblies);
        assert_eq!(
            format_hex(0, &plain_bytes),
            format_hex(0, &wrapped_bytes),
            "{}: wrapping the unit in `#if 1` changed the linked bytes",
            fixture.name
        );
    }
}

/// `--strict-k65` reports and changes nothing else.
///
/// The claim the flag rests on, and the reason it is not "a separate compiler
/// architecture" (`doc/k65-compat.md`): it selects no lowering, no width and
/// no byte. Every fixture is compiled both ways, and with the flag's own two
/// codes removed the strict run has to say what the ordinary run said.
///
/// **With one allowance, and it is not a loophole.** `K816-E0160` is an error,
/// and an error stops the pipeline where every error does — before lowering.
/// So a fixture whose strict run reports an extension legitimately loses the
/// diagnostics of the stages that never ran:
/// `legacy-k816/syntax-postfix-gt-efficiency-warning` keeps its `K816-W0157`
/// in an ordinary build and cannot in a strict one, because that warning is
/// decided at lowering. What is asserted is therefore that the strict run's
/// non-strict diagnostics are a *prefix* of the ordinary run's, per unit —
/// nothing removed from the middle, nothing added, nothing reordered — and
/// that the two are equal wherever strict mode reported no error at all.
#[test]
fn strict_mode_only_adds_diagnostics() {
    for fixture in fixtures() {
        // A fixture that is *about* the flag is already compiled with it.
        if fixture.strict {
            continue;
        }
        let sources = sources_of(&fixture);
        let (ordinary, _) = compile_with(&fixture, &sources, false);
        let (strict, _) = compile_with(&fixture, &sources, true);
        let rejected = strict.iter().any(|(_, code)| code == "K816-E0160");
        let by_unit = |codes: Vec<(String, String)>| -> BTreeMap<String, Vec<String>> {
            let mut out: BTreeMap<String, Vec<String>> = BTreeMap::new();
            for (unit, code) in codes {
                out.entry(unit).or_default().push(code);
            }
            out
        };
        let ordinary = by_unit(ordinary);
        let filtered = by_unit(
            strict
                .into_iter()
                .filter(|(_, code)| code != "K816-E0160" && code != "K816-W0161")
                .collect(),
        );
        let units: BTreeSet<&String> = ordinary.keys().chain(filtered.keys()).collect();
        for unit in units {
            let expected = ordinary.get(unit).cloned().unwrap_or_default();
            let codes = filtered.get(unit).cloned().unwrap_or_default();
            assert!(
                expected.starts_with(codes.as_slice()),
                "{} ({unit}): `--strict-k65` reports {codes:?}, which is not what the \
                 ordinary build reports first: {expected:?}",
                fixture.name
            );
            if !rejected {
                assert_eq!(
                    codes, expected,
                    "{} ({unit}): `--strict-k65` reported no extension and still \
                     changed what the compiler says",
                    fixture.name
                );
            }
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
// Oracle evidence
// ---------------------------------------------------------------------------

/// Adler-32, matching the awk in `oracle.sh`.
///
/// A staleness detector rather than a security primitive — the same framing
/// `tests/corpus.rs` uses for the generated-parser fingerprint. It is this and
/// not something stronger because both sides have to compute it and one of
/// them is a POSIX shell script.
fn adler32(bytes: &[u8]) -> String {
    let (mut a, mut b) = (1u32, 0u32);
    for byte in bytes {
        a = (a + u32::from(*byte)) % 65521;
        b = (b + a) % 65521;
    }
    format!("{b:04X}{a:04X}")
}

/// `key value` lines from a golden's `#`-comment header.
fn header_fields(text: &str) -> Vec<(String, String)> {
    let mut out = Vec::new();
    for line in text.lines() {
        let Some(rest) = line.strip_prefix("# ") else {
            continue;
        };
        if let Some((key, value)) = rest.trim_end().split_once(char::is_whitespace) {
            out.push((key.to_string(), value.trim().to_string()));
        }
    }
    out
}

fn oracle_lock() -> (String, String) {
    let text = std::fs::read_to_string(compat_dir().join("oracle.lock"))
        .expect("read tests/compat/oracle.lock");
    let field = |key: &str| -> String {
        text.lines()
            .find_map(|line| line.strip_prefix(key))
            .unwrap_or_else(|| panic!("oracle.lock has no `{key}`"))
            .trim()
            .to_string()
    };
    (field("k65-revision"), field("k65-version"))
}

/// Every oracle golden is bound to the compiler that made it and to the
/// sources it was made from.
///
/// The goldens are the only evidence in the tree for what the original K65
/// compiler does, and `cargo test` cannot re-derive them — the compiler is
/// outside the tree and may not be installed. So the binding is checked
/// instead: an input edited without re-running `oracle.sh` fails here, and so
/// does a golden produced against a compiler the lock does not name. Without
/// this the fixtures could drift from their evidence and stay green, which is
/// worse than having no evidence at all.
#[test]
fn every_oracle_golden_is_bound_to_its_inputs_and_its_compiler() {
    let (revision, version) = oracle_lock();
    let generated = [
        "oracle.hex",
        "oracle.lst",
        "oracle.err",
        "fixture.txt",
        "expected.hex",
        "PROVENANCE.md",
    ];

    let mut checked = 0;
    for fixture in fixtures().iter().filter(|f| f.name.starts_with("k65/")) {
        let hex = fixture.dir.join("oracle.hex");
        let err = fixture.dir.join("oracle.err");
        let golden = match (hex.exists(), err.exists()) {
            (true, false) => hex,
            (false, true) => err,
            (true, true) => panic!(
                "{}: has both oracle.hex and oracle.err - one build, one result",
                fixture.name
            ),
            (false, false) => panic!(
                "{}: is under k65/ and has no oracle golden. Produce one with \
                 `tests/compat/oracle.sh {}`, or move the fixture to k816/ if \
                 it is not asking the original anything.",
                fixture.name,
                fixture.dir.display()
            ),
        };
        let text = std::fs::read_to_string(&golden)
            .unwrap_or_else(|e| panic!("{}: cannot read its golden: {e}", fixture.name));
        let fields = header_fields(&text);
        let one = |key: &str| -> String {
            fields
                .iter()
                .find(|(k, _)| k == key)
                .unwrap_or_else(|| panic!("{}: its golden records no `{key}`", fixture.name))
                .1
                .clone()
        };
        assert_eq!(
            one("revision"),
            revision,
            "{}: its golden was produced against a K65 revision oracle.lock does \
             not pin. Re-run tests/compat/oracle.sh, or move the pin deliberately.",
            fixture.name
        );
        assert_eq!(
            one("compiler"),
            version,
            "{}: its golden records a compiler build oracle.lock does not pin",
            fixture.name
        );

        // Every recorded input still hashes to what it hashed to.
        let mut recorded = BTreeSet::new();
        for (key, value) in &fields {
            if key != "input" {
                continue;
            }
            let mut words = value.split_whitespace();
            let name = words.next().expect("an input line names a file");
            let expected = words
                .nth(1)
                .unwrap_or_else(|| panic!("{}: `input {value}` has no checksum", fixture.name));
            let bytes = std::fs::read(fixture.dir.join(name)).unwrap_or_else(|e| {
                panic!(
                    "{}: its golden records the input `{name}`, which is gone: {e}",
                    fixture.name
                )
            });
            assert_eq!(
                adler32(&bytes),
                expected,
                "{}: `{name}` changed since its golden was produced. Re-run \
                 tests/compat/oracle.sh and review what moved.",
                fixture.name
            );
            recorded.insert(name.to_string());
        }
        assert!(
            !recorded.is_empty(),
            "{}: its golden records no inputs at all",
            fixture.name
        );

        // And nothing the build reads is missing from that list.
        for entry in std::fs::read_dir(&fixture.dir).expect("read fixture directory") {
            let path = entry.expect("read fixture entry").path();
            if !path.is_file() {
                continue;
            }
            let name = path.file_name().unwrap().to_string_lossy().into_owned();
            if generated.contains(&name.as_str()) {
                continue;
            }
            assert!(
                recorded.contains(&name),
                "{}: `{name}` is an input the golden does not record. Re-run \
                 tests/compat/oracle.sh.",
                fixture.name
            );
        }
        checked += 1;
    }
    assert!(checked > 0, "no oracle fixtures were checked");
}

// ---------------------------------------------------------------------------
// The manifest
// ---------------------------------------------------------------------------

struct Row {
    id: String,
    lines: Vec<usize>,
    /// The `Construct` cell: the grammar rule the row classifies, from which
    /// `every_rows_fixture_contains_the_construct_it_claims` derives what the
    /// fixture has to contain.
    construct: String,
    /// The `Cheap` cell — decision 0's per-rule judgment, `yes`, `no` or
    /// `n-a`. Empty for the extension table, which has no such column: a
    /// construct K65 does not have is not one K816 could be compatible with.
    cheap: String,
    status: String,
    /// The `Strict` cell — what `--strict-k65` does where this row's
    /// construct appears: `rejects`, `warns`, or `—` for nothing.
    strict: String,
    fixture: String,
}

/// A `Fixture` cell that names no fixture, and the two spellings mean
/// different things: `n/a` is a row whose grammar line carries no live
/// production, so no source could contain it; `—` is a row nobody has written
/// a fixture for, which G10 left none of.
fn names_no_fixture(cell: &str) -> bool {
    matches!(cell, "n/a" | "—" | "-")
}

/// Parses `doc/k65-compat-manifest.md`.
///
/// The file is written to be read by this function — one row per line, fixed
/// columns — which `tests/manifest.rs` could not do for the feature manifest
/// (`plans/06-compatibility.md`, "Tests / fixtures"). Both tables are parsed
/// the same way; the grammar table carries line numbers and the extension
/// table does not.
fn manifest_rows() -> (Vec<Row>, String) {
    let text = std::fs::read_to_string(root().join("doc/k65-compat-manifest.md"))
        .expect("read doc/k65-compat-manifest.md");
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
        // The three definition tables at the top of the file are two columns
        // wide; the grammar table is eight and the extension table six.
        let id = cells[0].trim_matches('`').to_string();
        let (lines, construct, cheap, status, strict, fixture) = match cells.len() {
            // `id | lines | construct | cheap | status | strict | fixture | reason`
            8 => (
                parse_lines(&id, &cells[1]),
                &cells[2],
                cells[3].as_str(),
                &cells[4],
                &cells[5],
                &cells[6],
            ),
            // The extension table drops the line and cheap columns.
            6 => (Vec::new(), &cells[1], "", &cells[2], &cells[3], &cells[4]),
            _ => continue,
        };
        rows.push(Row {
            id,
            lines,
            construct: construct.to_string(),
            cheap: cheap.to_string(),
            status: status.to_string(),
            strict: strict.to_string(),
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
        assert!(
            row.fixture != "—" && row.fixture != "-",
            "row `{}` names no fixture. Every row names one, or `n/a` where the \
             grammar line carries no live production for a source to contain",
            row.id
        );
        if names_no_fixture(&row.fixture) {
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

/// A row's fixture contains the construct the row is about.
///
/// The gate this file was missing, and what it costs to be missing it: twice
/// now a row has read `compatible` for a *K65 spelling* while citing a fixture
/// that contained only K816's — `binary Ident` in G6 and `far goto` in G7,
/// both valid K65 forms, both reaching a wrong meaning rather than a refusal,
/// and both found by review because the suite was agreeing with itself.
///
/// The witness is derived from the row's own `Construct` cell rather than
/// declared separately, so it cannot drift from what the row claims. In this
/// grammar a non-terminal is capitalised and a terminal is not, so a run of
/// adjacent lowercase tokens on the right of a `->` is a phrase the source
/// must contain literally — `far goto` for `Instr -> far goto Arg`. Runs are
/// per alternative and every one of them must appear: a row covering
/// `return` and `return_i` is answerable only by a fixture with both.
///
/// **Punctuation is deliberately not a witness.** `Arg -> ( VarArg , X )` is
/// checkable in principle and useless in practice: a fixture contains commas
/// and parentheses for a hundred reasons, so the check would pass on
/// everything. Only what a reader would call a keyword is asked for. That
/// leaves rules whose terminals are all punctuation unchecked, and the
/// `Lines` tiling above is what keeps *those* rows honest.
#[test]
fn every_rows_fixture_contains_the_construct_it_claims() {
    let (rows, _) = manifest_rows();
    let mut missing: Vec<String> = Vec::new();
    let mut checked = 0;
    for row in &rows {
        // The extension table has no grammar rule to derive a witness from.
        if row.lines.is_empty() || names_no_fixture(&row.fixture) {
            continue;
        }
        let phrases = witnesses(&row.construct);
        if phrases.is_empty() {
            continue;
        }
        let source = fixture_source(&row.fixture);
        if source.is_empty() {
            continue;
        }
        checked += 1;
        for phrase in phrases {
            if !contains_phrase(&source, &phrase) {
                missing.push(format!(
                    "  `{}` claims `{}` and its fixture `{}` does not contain it",
                    row.id,
                    phrase.join(" "),
                    row.fixture
                ));
            }
        }
    }
    assert!(checked > 0, "no row had a checkable witness");
    assert!(
        missing.is_empty(),
        "a row's fixture does not exercise what the row claims:\n{}\n\n\
         Either point the row at a fixture that contains the construct, or write \
         the case. A row citing a fixture that exercises the half which already \
         worked is how `binary Ident` and `far goto` both shipped.",
        missing.join("\n")
    );
}

/// The keyword phrases a `Construct` cell promises.
///
/// Only rule-shaped backtick spans are read — a cell may also carry prose,
/// and prose is full of lowercase words that are not terminals.
fn witnesses(construct: &str) -> Vec<Vec<String>> {
    let mut out = Vec::new();
    for span in construct.split('`').skip(1).step_by(2) {
        let Some((_, rhs)) = span.split_once("->") else {
            continue;
        };
        for alternative in rhs.split('|') {
            let mut run: Vec<String> = Vec::new();
            for token in alternative.replace("<empty>", " ").split_whitespace() {
                // A terminal: begins with a lowercase letter and carries no
                // capital, which is what tells `far` from `Arg`.
                if token.starts_with(|c: char| c.is_ascii_lowercase())
                    && !token.contains(|c: char| c.is_ascii_uppercase())
                {
                    run.push(token.to_string());
                } else if !run.is_empty() {
                    out.push(std::mem::take(&mut run));
                }
            }
            if !run.is_empty() {
                out.push(run);
            }
        }
    }
    out
}

/// Every source of a fixture, with comments removed.
///
/// Comments are stripped because a fixture's prose routinely *names* the
/// construct it is about — `golden/unit-a.k816` says "a call into the sibling
/// unit" beside a `jsr`. Matching that would make the gate agree with a
/// fixture's description of itself instead of its code.
fn fixture_source(fixture: &str) -> String {
    let mut paths = Vec::new();
    match fixture.strip_prefix("golden/") {
        Some(rest) => paths.push(root().join("tests/golden").join(rest)),
        None => {
            let dir = compat_dir().join(fixture);
            let Ok(entries) = std::fs::read_dir(&dir) else {
                return String::new();
            };
            let mut found: Vec<PathBuf> = entries
                .filter_map(Result::ok)
                .map(|entry| entry.path())
                .filter(|path| {
                    matches!(
                        path.extension().and_then(|e| e.to_str()),
                        Some("k65") | Some("k816")
                    )
                })
                .collect();
            found.sort();
            paths.extend(found);
        }
    }
    let mut text = String::new();
    for path in paths {
        let Ok(body) = std::fs::read_to_string(&path) else {
            continue;
        };
        text.push_str(&strip_comments(&body));
        text.push('\n');
    }
    text
}

fn strip_comments(source: &str) -> String {
    let mut out = String::with_capacity(source.len());
    let bytes: Vec<char> = source.chars().collect();
    let mut i = 0;
    let mut block = 0usize;
    while i < bytes.len() {
        if block > 0 {
            if bytes[i] == '*' && bytes.get(i + 1) == Some(&'/') {
                block -= 1;
                i += 2;
                continue;
            }
            if bytes[i] == '\n' {
                out.push('\n');
            }
            i += 1;
            continue;
        }
        if bytes[i] == '/' && bytes.get(i + 1) == Some(&'*') {
            block += 1;
            i += 2;
            continue;
        }
        if bytes[i] == '/' && bytes.get(i + 1) == Some(&'/') {
            while i < bytes.len() && bytes[i] != '\n' {
                i += 1;
            }
            continue;
        }
        out.push(bytes[i]);
        i += 1;
    }
    out
}

/// Whether `source` contains `phrase` as consecutive whitespace-separated
/// tokens.
///
/// The boundary rule is the source's own. Every token but the last must match
/// exactly — the phrase's spacing is what `far goto` means — and the last may
/// carry trailing punctuation, so `return_i}` counts and `return_ix` does not.
fn contains_phrase(source: &str, phrase: &[String]) -> bool {
    let tokens: Vec<&str> = source.split_whitespace().collect();
    let ends_cleanly = |token: &str, want: &str| match token.strip_prefix(want) {
        Some(rest) => !rest.starts_with(|c: char| c.is_alphanumeric() || c == '_'),
        None => false,
    };
    tokens.windows(phrase.len()).any(|window| {
        window.iter().enumerate().all(|(i, token)| {
            if i + 1 == phrase.len() {
                ends_cleanly(token, &phrase[i])
            } else {
                *token == phrase[i]
            }
        })
    })
}

/// Every construct the AST can hold, and the manifest row that classifies it.
///
/// **The audit that found four missing rows, kept so it cannot be needed
/// twice.** The second table claims one row per K816 construct that is not in
/// the original grammar, and until G10b it was written by walking the
/// *decisions* G took — which finds every construct a decision was about and
/// none of the ones nobody argued over. Raw mnemonics were the largest:
/// `compiler.inc` has no mnemonic production at all, so the whole native layer
/// sub-plan C landed is an extension, and no row said so. Typed data entries,
/// `p??`, and everything richer than `NAME + NAME` in an operand went the same
/// way.
///
/// Walking the AST instead asks the question from the side that has an
/// answer: a construct the compiler can represent either has a row or is a
/// gap. The table is checked against `src/ast.rs` itself, so a variant added
/// later fails here until someone classifies it.
const AST_VARIANTS: &[(&str, &str, &str)] = &[
    // ---- ItemKind ---------------------------------------------------------
    (
        "ItemKind",
        "Var",
        "topelem-var var-declaration k816-symbolic-subscript k816-abstract-var \
         k816-direct-page-pool k816-typed-view",
    ),
    (
        "ItemKind",
        "Const",
        "topelem-const k816-const-auto-increment",
    ),
    ("ItemKind", "Segment", "k816-segment"),
    ("ItemKind", "Data", "topelem-data"),
    ("ItemKind", "EvalBlock", "topelem-eval-block"),
    ("ItemKind", "EvalFunc", "topelem-evalfunc"),
    (
        "ItemKind",
        "Preprocessor",
        "topelem-preprocessor topelem-pperror ppif-directives ppifnumber-condition",
    ),
    ("ItemKind", "Binary", "topelem-binary"),
    (
        "ItemKind",
        "Reserved",
        "topelem-bank topelem-image instr-sax instr-axs arithop-anc arithop-dcp \
         arithop-isc instr-stack-flags flag-tokens",
    ),
    ("ItemKind", "ModuleWidth", "k816-width-annotations"),
    (
        "ItemKind",
        "Func",
        "topelem-func topelem-naked topelem-inline topelem-main k816-far-function \
         k816-call-contracts",
    ),
    ("ItemKind", "Error", "n/a"),
    // ---- StatementKind ----------------------------------------------------
    ("StatementKind", "Label", "instr-label"),
    ("StatementKind", "Native", "k816-native-mnemonics"),
    (
        "StatementKind",
        "Hla",
        "instr-arith instr-mov instr-postfix instr-flag-set instr-flag-clear \
         instr-stack-a instr-stack-flags instr-nop-wait k816-status-register-stack \
         k816-accumulator-one-name",
    ),
    ("StatementKind", "WidthBlock", "k816-width-annotations"),
    ("StatementKind", "ModeSet", "k816-width-annotations"),
    ("StatementKind", "Call", "instr-call-label instr-far-call"),
    (
        "StatementKind",
        "BareCall",
        "instr-call k816-call-contracts",
    ),
    ("StatementKind", "Conditional", "instr-branch-block"),
    (
        "StatementKind",
        "ConditionalJump",
        "instr-goto instr-break instr-repeat",
    ),
    (
        "StatementKind",
        "Loop",
        "instr-branch-block k816-postfix-gt k816-postfix-le",
    ),
    ("StatementKind", "Goto", "instr-goto instr-far-goto"),
    ("StatementKind", "Break", "instr-break"),
    ("StatementKind", "Repeat", "instr-repeat"),
    (
        "StatementKind",
        "Nocross",
        "data-nocross-flag k816-nocross-shape",
    ),
    ("StatementKind", "Data", "instr-data-block"),
    ("StatementKind", "Align", "align-directive"),
    ("StatementKind", "Address", "align-directive"),
    (
        "StatementKind",
        "Reserved",
        "instr-sax instr-axs arithop-anc arithop-dcp arithop-isc instr-stack-flags \
         flag-tokens",
    ),
    ("StatementKind", "Preprocessor", "instr-preprocessor"),
    ("StatementKind", "Return", "instr-return"),
    ("StatementKind", "Error", "n/a"),
    // ---- DataEntryKind ----------------------------------------------------
    ("DataEntryKind", "Segment", "k816-segment"),
    ("DataEntryKind", "Align", "align-directive"),
    ("DataEntryKind", "Address", "align-directive"),
    ("DataEntryKind", "Nocross", "data-nocross-flag"),
    ("DataEntryKind", "Charset", "data-charset"),
    ("DataEntryKind", "Label", "data-label"),
    ("DataEntryKind", "Typed", "k816-typed-data-entries"),
    (
        "DataEntryKind",
        "Value",
        "data-arg data-address-word data-string data-undef",
    ),
    ("DataEntryKind", "Code", "data-code-block"),
    ("DataEntryKind", "NocrossGroup", "data-nocross-block"),
    ("DataEntryKind", "Evaluator", "data-evaluator-entry"),
    ("DataEntryKind", "Repeat", "data-repeat"),
    ("DataEntryKind", "ForEval", "data-for-eval"),
    ("DataEntryKind", "Preprocessor", "data-preprocessor"),
    ("DataEntryKind", "Binary", "data-binary"),
    (
        "DataEntryKind",
        "Reserved",
        "topelem-image data-image-tiles data-imgwave",
    ),
    ("DataEntryKind", "Error", "n/a"),
    // ---- ExprKind ---------------------------------------------------------
    (
        "ExprKind",
        "Number",
        "number terminal-numbers k816-hex-binary-char-literals",
    ),
    ("ExprKind", "Symbol", "terminal-ident vararg-label-offset"),
    ("ExprKind", "LocalSymbol", "ident"),
    (
        "ExprKind",
        "Unary",
        "arg-forms data-address-word k816-operand-expressions",
    ),
    (
        "ExprKind",
        "Binary",
        "vararg-label-offset k816-operand-expressions",
    ),
    ("ExprKind", "Field", "k816-field-access"),
    ("ExprKind", "Subscript", "k816-field-access"),
    ("ExprKind", "Metadata", "k816-metadata-query"),
    ("ExprKind", "TypedView", "k816-typed-view"),
    ("ExprKind", "Eval", "number-eval-bracket"),
    ("ExprKind", "Error", "n/a"),
    // ---- HlaStatement -----------------------------------------------------
    ("HlaStatement", "Assign", "instr-mov"),
    ("HlaStatement", "Exchange", "k816-accumulator-one-name"),
    ("HlaStatement", "Arithmetic", "instr-arith"),
    ("HlaStatement", "Shift", "instr-postfix"),
    ("HlaStatement", "Step", "instr-postfix"),
    (
        "HlaStatement",
        "Flag",
        "instr-flag-set instr-flag-clear flag-tokens",
    ),
    (
        "HlaStatement",
        "Stack",
        "instr-stack-a instr-stack-flags k816-status-register-stack",
    ),
    ("HlaStatement", "Nop", "instr-nop-wait"),
    ("HlaStatement", "Break", "instr-nop-wait"),
    // ---- StorageClassKind ----------------------------------------------
    ("StorageClassKind", "DirectPage", "k816-direct-page-pool"),
    ("StorageClassKind", "Absolute", "k816-address-encoding"),
    ("StorageClassKind", "Far", "k816-address-encoding"),
    // ---- TypeWidthKind -------------------------------------------------
    (
        "TypeWidthKind",
        "Byte",
        "k816-typed-view k816-typed-data-entries",
    ),
    (
        "TypeWidthKind",
        "Word",
        "k816-typed-view k816-typed-data-entries",
    ),
    (
        "TypeWidthKind",
        "Far",
        "k816-typed-view k816-typed-data-entries",
    ),
    // ---- FunctionKind --------------------------------------------------
    ("FunctionKind", "Func", "topelem-func"),
    ("FunctionKind", "Naked", "topelem-naked"),
    ("FunctionKind", "Inline", "topelem-inline"),
    ("FunctionKind", "Main", "topelem-main"),
    // ---- ContractParameter ---------------------------------------------
    ("ContractParameter", "Register", "k816-call-contracts"),
    ("ContractParameter", "ImmediateAlias", "k816-call-contracts"),
    ("ContractParameter", "Alias", "k816-call-contracts"),
    // ---- ContractOutput ------------------------------------------------
    ("ContractOutput", "Register", "k816-call-contracts"),
    ("ContractOutput", "Flag", "k816-call-contracts"),
    // ---- WidthAnnotationKind -------------------------------------------
    ("WidthAnnotationKind", "A8", "k816-width-annotations"),
    ("WidthAnnotationKind", "A16", "k816-width-annotations"),
    ("WidthAnnotationKind", "I8", "k816-width-annotations"),
    ("WidthAnnotationKind", "I16", "k816-width-annotations"),
    // ---- BranchConditionKind -------------------------------------------
    ("BranchConditionKind", "Comparison", "branchop"),
    ("BranchConditionKind", "Flag", "branchop"),
    // ---- ComparisonKind ------------------------------------------------
    ("ComparisonKind", "Less", "branchop"),
    ("ComparisonKind", "GreaterEqual", "branchop"),
    ("ComparisonKind", "Equal", "branchop"),
    ("ComparisonKind", "NotEqual", "branchop"),
    ("ComparisonKind", "Negative", "branchop"),
    ("ComparisonKind", "NotNegative", "branchop"),
    ("ComparisonKind", "OverflowSet", "branchop"),
    ("ComparisonKind", "OverflowClear", "branchop"),
    ("ComparisonKind", "Greater", "k816-postfix-gt"),
    ("ComparisonKind", "LessEqual", "k816-postfix-le"),
    // ---- TestableFlag --------------------------------------------------
    ("TestableFlag", "Carry", "branchop"),
    ("TestableFlag", "Negative", "branchop"),
    ("TestableFlag", "Zero", "branchop"),
    ("TestableFlag", "Overflow", "branchop"),
    // ---- LoopCondition -------------------------------------------------
    ("LoopCondition", "Branch", "instr-branch-block"),
    ("LoopCondition", "Always", "instr-branch-block"),
    ("LoopCondition", "Never", "instr-branch-block"),
    // ---- CallArgument --------------------------------------------------
    ("CallArgument", "Register", "k816-call-contracts"),
    ("CallArgument", "Immediate", "k816-call-contracts"),
    // ---- ReturnKind ----------------------------------------------------
    ("ReturnKind", "Subroutine", "instr-return"),
    ("ReturnKind", "Interrupt", "instr-return"),
    // ---- LabelScope ----------------------------------------------------
    ("LabelScope", "Global", "instr-label"),
    ("LabelScope", "Local", "ident"),
    // ---- NativeOperand -------------------------------------------------
    ("NativeOperand", "Immediate", "k816-native-mnemonics"),
    ("NativeOperand", "Register", "k816-native-mnemonics"),
    ("NativeOperand", "Address", "k816-native-mnemonics"),
    ("NativeOperand", "Indirect", "k816-native-mnemonics"),
    ("NativeOperand", "LongIndirect", "k816-native-mnemonics"),
    ("NativeOperand", "BlockMove", "k816-native-mnemonics"),
    // ---- IndexRegisterKind ---------------------------------------------
    ("IndexRegisterKind", "X", "arg-forms"),
    ("IndexRegisterKind", "Y", "arg-forms"),
    ("IndexRegisterKind", "Stack", "k816-stack-relative"),
    // ---- ArithmeticOp --------------------------------------------------
    ("ArithmeticOp", "Add", "instr-arith arithop"),
    ("ArithmeticOp", "Subtract", "instr-arith arithop"),
    ("ArithmeticOp", "And", "instr-arith arithop"),
    ("ArithmeticOp", "Or", "instr-arith arithop"),
    ("ArithmeticOp", "Xor", "instr-arith arithop"),
    ("ArithmeticOp", "Compare", "instr-arith arithop"),
    ("ArithmeticOp", "BitTest", "instr-arith arithop"),
    // ---- ShiftOp -------------------------------------------------------
    ("ShiftOp", "Left", "instr-postfix postfixop"),
    ("ShiftOp", "Right", "instr-postfix postfixop"),
    ("ShiftOp", "RotateLeft", "instr-postfix postfixop"),
    ("ShiftOp", "RotateRight", "instr-postfix postfixop"),
    // ---- StepOp --------------------------------------------------------
    ("StepOp", "Increment", "instr-postfix postfixop"),
    ("StepOp", "Decrement", "instr-postfix postfixop"),
    // ---- FlagKind ------------------------------------------------------
    (
        "FlagKind",
        "Carry",
        "instr-flag-set instr-flag-clear flag-tokens",
    ),
    (
        "FlagKind",
        "Decimal",
        "instr-flag-set instr-flag-clear flag-tokens",
    ),
    (
        "FlagKind",
        "Interrupt",
        "instr-flag-set instr-flag-clear flag-tokens",
    ),
    (
        "FlagKind",
        "Overflow",
        "instr-flag-set instr-flag-clear flag-tokens",
    ),
    // ---- StackOp -------------------------------------------------------
    ("StackOp", "Push", "instr-stack-a instr-stack-flags"),
    ("StackOp", "Pull", "instr-stack-a instr-stack-flags"),
    // ---- HlaOperandKind ------------------------------------------------
    ("HlaOperandKind", "Register", "register-tokens"),
    ("HlaOperandKind", "Immediate", "k816-immediate-marker"),
    ("HlaOperandKind", "Memory", "arg-forms"),
    ("HlaOperandKind", "Indirect", "arg-forms"),
    ("HlaOperandKind", "Error", "n/a"),
    // ---- RegisterKind --------------------------------------------------
    (
        "RegisterKind",
        "A",
        "register-tokens k816-accumulator-one-name",
    ),
    ("RegisterKind", "B", "k816-accumulator-one-name"),
    ("RegisterKind", "Dpr", "k816-register-names"),
    ("RegisterKind", "Dbr", "k816-register-names"),
    ("RegisterKind", "S", "register-tokens"),
    ("RegisterKind", "X", "register-tokens"),
    ("RegisterKind", "Y", "register-tokens"),
    ("RegisterKind", "P", "k816-status-register-stack"),
    // ---- UnaryOp -------------------------------------------------------
    ("UnaryOp", "Negate", "k816-operand-expressions"),
    ("UnaryOp", "BitNot", "k816-operand-expressions"),
    ("UnaryOp", "LowByte", "arg-forms"),
    ("UnaryOp", "HighByte", "arg-forms"),
    ("UnaryOp", "Address16", "data-address-word"),
    ("UnaryOp", "Address24", "k816-operand-expressions"),
    // ---- EvalExprKind --------------------------------------------------
    ("EvalExprKind", "Number", "evelem-values terminal-numbers"),
    ("EvalExprKind", "Pi", "evelem-values"),
    ("EvalExprKind", "Float", "evelem-values terminal-numbers"),
    ("EvalExprKind", "String", "evelem-values terminal-string"),
    ("EvalExprKind", "Symbol", "evelem-values allident"),
    ("EvalExprKind", "Call", "evelem-call k816-eval-extensions"),
    (
        "EvalExprKind",
        "Unary",
        "evelem-incdec k816-eval-extensions",
    ),
    (
        "EvalExprKind",
        "Binary",
        "evexpr-operators k816-eval-extensions",
    ),
    ("EvalExprKind", "Ternary", "evexpr-operators"),
    ("EvalExprKind", "Assignment", "evexpr-operators"),
    ("EvalExprKind", "Subscript", "evelem-index"),
    ("EvalExprKind", "Member", "evelem-method"),
    ("EvalExprKind", "MethodCall", "evelem-method"),
    ("EvalExprKind", "List", "evexpr-operators"),
    ("EvalExprKind", "Block", "evalinstr-statements"),
    ("EvalExprKind", "If", "evalinstr-statements"),
    ("EvalExprKind", "While", "evalinstr-statements"),
    ("EvalExprKind", "ReservedCall", "evelem-call"),
    ("EvalExprKind", "Error", "n/a"),
    // ---- PreprocessorKind ----------------------------------------------
    ("PreprocessorKind", "If", "ppif-directives"),
    ("PreprocessorKind", "ElseIf", "ppif-directives"),
    ("PreprocessorKind", "Else", "ppif-directives"),
    ("PreprocessorKind", "EndIf", "ppif-directives"),
    ("PreprocessorKind", "Error", "topelem-pperror"),
    ("PreprocessorKind", "Warn", "topelem-pperror"),
    // ---- ReservedKind --------------------------------------------------
    (
        "ReservedKind",
        "ImageBlock",
        "data-image-tiles data-imgwave",
    ),
    ("ReservedKind", "ImageTopLevel", "topelem-image"),
    ("ReservedKind", "EvalCall", "evelem-call"),
    ("ReservedKind", "Bank", "topelem-bank"),
    (
        "ReservedKind",
        "IllegalOpcode",
        "instr-sax instr-axs arithop-anc arithop-dcp arithop-isc",
    ),
    ("ReservedKind", "StackFlag", "instr-stack-flags"),
    ("ReservedKind", "OverflowFlagSpelling", "flag-tokens"),
    // ---- IllegalOpcode -------------------------------------------------
    ("IllegalOpcode", "Anc", "arithop-anc"),
    ("IllegalOpcode", "Sax", "instr-sax"),
    ("IllegalOpcode", "Axs", "instr-axs"),
    ("IllegalOpcode", "Dcp", "arithop-dcp"),
    ("IllegalOpcode", "Isc", "arithop-isc"),
    // ---- EvalCallName --------------------------------------------------
    ("EvalCallName", "AddByte", "evelem-call"),
    ("EvalCallName", "Color", "evelem-call"),
    ("EvalCallName", "Rnd", "evelem-call"),
    ("EvalCallName", "Print", "evelem-call"),
    ("EvalCallName", "Error", "evelem-call"),
    // ---- DataValue -----------------------------------------------------
    ("DataValue", "Expr", "data-arg data-address-word"),
    ("DataValue", "String", "data-string"),
    ("DataValue", "Undefined", "data-undef"),
    // ---- MetadataQueryKind ---------------------------------------------
    ("MetadataQueryKind", "SizeOf", "k816-metadata-query"),
    ("MetadataQueryKind", "OffsetOf", "k816-metadata-query"),
    // ---- BinaryOp ------------------------------------------------------
    ("BinaryOp", "Add", "vararg-label-offset"),
    ("BinaryOp", "Sub", "vararg-label-offset"),
    ("BinaryOp", "Mul", "k816-operand-expressions"),
    ("BinaryOp", "Shl", "k816-operand-expressions"),
    ("BinaryOp", "Shr", "k816-operand-expressions"),
    ("BinaryOp", "BitAnd", "k816-operand-expressions"),
    ("BinaryOp", "BitXor", "k816-operand-expressions"),
    ("BinaryOp", "BitOr", "k816-operand-expressions"),
];

/// Every optional or boolean AST *field*, and the manifest row that
/// classifies it.
///
/// The blind spot the variant table had, and it cost a row: `&expr,x`'s
/// indexed-offset marker is an `Option<Span>` field of `AddressOperand`, not a
/// variant of any enum, so nothing asked whether it was classified and it was
/// not. A field whose type is `bool` or `Option<_>` records whether a piece of
/// syntax was **written**, which makes it a construct in exactly the way a
/// variant is — and a construct is what the manifest claims to have a row for.
///
/// Fields that always carry a value are not here: a `span`, a `kind`, a body,
/// an entry list are the shape of a node rather than something a source
/// chooses to write.
const AST_FIELDS: &[(&str, &str, &str)] = &[
    ("VarDeclaration", "is_abstract", "k816-abstract-var"),
    (
        "VarDeclaration",
        "storage",
        "k816-direct-page-pool k816-address-encoding",
    ),
    ("VarDeclaration", "width", "k816-typed-view"),
    ("VarDeclaration", "size", "var-declaration"),
    ("VarDeclaration", "count", "k816-symbolic-subscript"),
    ("VarDeclaration", "address", "var-declaration"),
    ("FieldDeclaration", "width", "k816-typed-view"),
    ("FieldDeclaration", "size", "k816-symbolic-subscript"),
    (
        "ConstEntry",
        "value",
        "topelem-const k816-const-auto-increment",
    ),
    ("Function", "is_far", "k816-far-function"),
    ("Function", "contract", "k816-call-contracts"),
    ("CallContract", "inputs", "k816-call-contracts"),
    ("CallContract", "outputs", "k816-call-contracts"),
    ("ConditionalBlock", "otherwise", "instr-branch-block"),
    (
        "LoopStatement",
        "nocross",
        "data-nocross-flag k816-nocross-shape",
    ),
    ("GotoStatement", "indirect", "instr-goto"),
    ("GotoStatement", "is_far", "instr-far-goto"),
    ("CallStatement", "is_far", "instr-far-call"),
    ("BareCall", "arguments", "k816-call-contracts"),
    ("BareCall", "outputs", "k816-call-contracts"),
    ("NativeInstruction", "operand", "k816-native-mnemonics"),
    ("NativeInstruction", "repeat", "k816-native-mnemonics"),
    ("AddressOperand", "encoding", "k816-address-encoding"),
    ("AddressOperand", "offset_marker", "k816-indexed-offset"),
    ("AddressOperand", "index", "arg-forms k816-stack-relative"),
    ("LongIndirectOperand", "encoding", "k816-native-mnemonics"),
    (
        "LongIndirectOperand",
        "outer_index",
        "k816-native-mnemonics",
    ),
    ("IndirectOperand", "encoding", "k816-address-encoding"),
    (
        "IndirectOperand",
        "inner_index",
        "arg-forms k816-stack-relative",
    ),
    (
        "IndirectOperand",
        "outer_index",
        "arg-forms k816-stack-relative",
    ),
    ("HlaFlag", "set", "instr-flag-set instr-flag-clear"),
    ("DataBlock", "name", "topelem-data instr-data-block"),
    ("DataBlock", "nocross", "data-nocross-flag"),
    ("AlignDirective", "offset", "align-directive"),
    ("PreprocessorDirective", "condition", "ppifnumber-condition"),
    ("PreprocessorDirective", "message", "topelem-pperror"),
    ("ReservedConstruct", "name", "topelem-bank"),
];

/// Every AST variant is classified, and every classification names a real row.
///
/// `src/ast.rs` is read rather than trusted, the way `diagnostic.rs` is read
/// by `every_declared_code_is_registered` and for the same reason: a variant
/// nothing references is invisible at run time, and a table that lists only
/// what someone remembered is a table that agrees with itself.
#[test]
fn every_ast_construct_answers_to_a_manifest_row() {
    let source = std::fs::read_to_string(root().join("src/ast.rs")).expect("read src/ast.rs");
    let (rows, _) = manifest_rows();
    let ids: BTreeSet<&str> = rows.iter().map(|row| row.id.as_str()).collect();

    for (enum_name, variant, classification) in AST_VARIANTS {
        assert!(
            AST_VARIANTS
                .iter()
                .filter(|(e, v, _)| e == enum_name && v == variant)
                .count()
                == 1,
            "`{enum_name}::{variant}` is listed twice"
        );
        if *classification == "n/a" {
            continue;
        }
        for id in classification.split_whitespace() {
            assert!(
                ids.contains(id),
                "`{enum_name}::{variant}` names the row `{id}`, which the manifest \
                 does not have"
            );
        }
    }

    // Every `pub enum` of the AST is in the table, not only the ones somebody
    // thought to list: a whole enum added later is as much a construct as a
    // variant is, and the point of reading the file is to be told rather than
    // to remember.
    let listed: BTreeSet<&str> = AST_VARIANTS.iter().map(|(e, _, _)| *e).collect();
    let declared: BTreeSet<&str> = source
        .lines()
        .filter_map(|line| line.strip_prefix("pub enum "))
        .filter_map(|rest| rest.split_once(' '))
        .map(|(name, _)| name)
        .collect();
    let unlisted: Vec<&&str> = declared.difference(&listed).collect();
    assert!(
        unlisted.is_empty(),
        "`src/ast.rs` declares these enums and no manifest row classifies their \
         variants: {unlisted:?}"
    );
    let unknown: Vec<&&str> = listed.difference(&declared).collect();
    assert!(
        unknown.is_empty(),
        "these are listed here and are not enums of `src/ast.rs`: {unknown:?}"
    );

    // Every optional field is classified too, and every one that exists is
    // listed. The two directions matter for the same reason they do above:
    // `AddressOperand::offset_marker` was a construct nothing had a row for,
    // and no gate could see it because it is not a variant.
    let listed_fields: BTreeSet<(&str, &str)> =
        AST_FIELDS.iter().map(|(s, f, _)| (*s, *f)).collect();
    assert_eq!(
        listed_fields.len(),
        AST_FIELDS.len(),
        "a field is listed twice"
    );
    for (owner, field, classification) in AST_FIELDS {
        for id in classification.split_whitespace() {
            assert!(
                ids.contains(id),
                "`{owner}.{field}` names the row `{id}`, which the manifest does not have"
            );
        }
    }
    for (owner, field, ty) in optional_fields(&source) {
        assert!(
            listed_fields.contains(&(owner.as_str(), field.as_str())),
            "`{owner}.{field}` is a `{ty}`, which records whether a piece of syntax \
             was written, and no manifest row classifies it. Add the row, or list the \
             field beside the variants — `AddressOperand::offset_marker` is the one \
             that got through, because a field is not a variant and the variant table \
             could not see it."
        );
    }

    for enum_name in listed {
        let body = source
            .split_once(&format!("pub enum {enum_name} {{"))
            .unwrap_or_else(|| panic!("src/ast.rs has no `pub enum {enum_name}`"))
            .1
            .split_once("\n}")
            .expect("the enum ends")
            .0;
        let mut variants: Vec<&str> = Vec::new();
        for line in body.lines() {
            let Some(rest) = line.strip_prefix("    ") else {
                continue;
            };
            if !rest.starts_with(|c: char| c.is_ascii_uppercase()) {
                continue;
            }
            let name = rest
                .split(|c: char| !c.is_alphanumeric() && c != '_')
                .next()
                .unwrap_or_default();
            if !name.is_empty() {
                variants.push(name);
            }
        }
        assert!(!variants.is_empty(), "{enum_name} parsed to no variants");
        for variant in variants {
            assert!(
                AST_VARIANTS
                    .iter()
                    .any(|(e, v, _)| *e == enum_name && *v == variant),
                "`{enum_name}::{variant}` is a construct the compiler can hold and no \
                 manifest row classifies it. Add the row, or list the variant here with \
                 the rows that cover it — a construct nobody argued over is exactly how \
                 raw mnemonics went unclassified from sub-plan C to G10b."
            );
        }
    }
}

/// Every `bool` or `Option<_>` field of an AST struct, as `(struct, field,
/// type)`.
///
/// Read off `src/ast.rs` rather than listed, so a field added later is found.
fn optional_fields(source: &str) -> Vec<(String, String, String)> {
    let mut out = Vec::new();
    let mut owner: Option<&str> = None;
    for line in source.lines() {
        if let Some(rest) = line.strip_prefix("pub struct ") {
            owner = rest
                .split(|c: char| !c.is_alphanumeric() && c != '_')
                .next();
            continue;
        }
        if line == "}" {
            owner = None;
            continue;
        }
        let (Some(name), Some(rest)) = (owner, line.strip_prefix("    pub ")) else {
            continue;
        };
        let Some((field, ty)) = rest.split_once(": ") else {
            continue;
        };
        let ty = ty.trim_end_matches(',');
        if ty == "bool" || ty.starts_with("Option<") {
            out.push((name.to_string(), field.to_string(), ty.to_string()));
        }
    }
    assert!(!out.is_empty(), "src/ast.rs parsed to no optional fields");
    out
}

/// Every status is one of the six the manifest defines, and none is `open`.
///
/// `open` stays a legal status so that a rule a later sub-plan adds has
/// somewhere to sit before it is decided — but no row may read it now. That is
/// sub-plan G's own criterion, and it is asserted here rather than argued in
/// prose because "every construct is classified" is the whole claim of this
/// file.
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
        assert_ne!(
            row.status, "open",
            "row `{}` is still `open`. A row with no decision is what this \
             manifest exists to have none of",
            row.id
        );
    }
}

/// Every grammar rule carries decision 0's "cheap or not" judgment.
///
/// The judgment is the whole of decision 0 — K816 is compatible *wherever
/// compatibility is cheap* — and until G10 it lived in whichever `Reason`
/// cells happened to mention it, which is to say in seven of eighty-nine. A
/// column can be checked; a phrase a reader has to find cannot.
///
/// Two invariants come with it. A rule that was **not** cheap cannot have come
/// out `compatible`: if it had, the judgment is wrong and the row is claiming
/// something it did not pay for. And `n-a` names exactly the rules with no
/// live production — commented out, or referenced by nothing — which are the
/// same rows whose `Fixture` reads `n/a`, because a rule no source can contain
/// is a rule no fixture can exercise.
#[test]
fn every_grammar_rule_carries_its_cheap_judgment() {
    const CHEAP: [&str; 3] = ["yes", "no", "n-a"];
    for row in manifest_rows().0 {
        if row.lines.is_empty() {
            assert!(
                row.cheap.is_empty(),
                "row `{}` is not a grammar rule and has a `Cheap` cell",
                row.id
            );
            continue;
        }
        assert!(
            CHEAP.contains(&row.cheap.as_str()),
            "row `{}` reads `{}` in the `Cheap` column, which is not one of {CHEAP:?}",
            row.id,
            row.cheap
        );
        if row.cheap == "no" {
            assert_ne!(
                row.status, "compatible",
                "row `{}` says compatibility was not cheap and claims to be \
                 compatible anyway",
                row.id
            );
        }
        assert_eq!(
            row.cheap == "n-a",
            row.fixture == "n/a",
            "row `{}` reads `{}` in `Cheap` and `{}` in `Fixture`; a rule with no \
             live production has neither, and a rule with one has both",
            row.id,
            row.cheap,
            row.fixture
        );
    }
}

/// `compat::MANIFEST` and `doc/k65-compat-manifest.md` agree, row for row.
///
/// The mirror decision 20 asks for, and the risk note's whole point: a table
/// in the compiler and a table in a document will drift unless something reads
/// both. Both directions are checked — a row added to the file and not to the
/// code is as much a failure as the reverse — and so are the status and the
/// report set, because a row whose `Strict` cell says `rejects` while the code
/// says `Quiet` is a construct the document promises to reject and nothing
/// rejects.
#[test]
fn the_code_mirrors_the_manifest_row_for_row() {
    use k816::compat::{Classification, MANIFEST, Strict};

    let (rows, _) = manifest_rows();
    let file: BTreeMap<&str, (&str, &str)> = rows
        .iter()
        .map(|row| (row.id.as_str(), (row.status.as_str(), row.strict.as_str())))
        .collect();
    let code: BTreeMap<&str, (&str, &str)> = MANIFEST
        .iter()
        .map(|row| {
            let status = match row.status {
                Classification::Compatible => "compatible",
                Classification::IntentionallyChanged => "intentionally-changed",
                Classification::LegacyDefective => "legacy-defective",
                Classification::BackendDifferent => "backend-different",
                Classification::K816Extension => "K816-extension",
            };
            let strict = match row.strict {
                Strict::Rejects => "rejects",
                Strict::Warns => "warns",
                Strict::Quiet => "—",
            };
            (row.id, (status, strict))
        })
        .collect();

    assert_eq!(
        code.len(),
        MANIFEST.len(),
        "`compat::MANIFEST` names a row twice"
    );
    let missing: Vec<&&str> = file.keys().filter(|id| !code.contains_key(*id)).collect();
    assert!(
        missing.is_empty(),
        "these rows are in doc/k65-compat-manifest.md and not in `compat::MANIFEST`: {missing:?}"
    );
    let extra: Vec<&&str> = code.keys().filter(|id| !file.contains_key(*id)).collect();
    assert!(
        extra.is_empty(),
        "these rows are in `compat::MANIFEST` and not in doc/k65-compat-manifest.md: {extra:?}"
    );
    for (id, cells) in &file {
        assert_eq!(
            code[id], *cells,
            "row `{id}` reads (status, strict) = {:?} in the code and {cells:?} in the file",
            code[id]
        );
    }
}

/// Every row strict mode reports is a row the manifest marks, and every row
/// the manifest marks is one the corpus makes it report.
///
/// The gate the risk note names, and it runs in both directions for the same
/// reason the mirror does. A construct the walk finds under a row the file
/// leaves `—` is the flag saying something the document does not; a row marked
/// `rejects` that nothing in the corpus produces is a promise with no witness,
/// which is how `binary Ident` and `far goto` both shipped.
///
/// The corpus is every fixture under `tests/compat/` plus every `tests/golden/`
/// program, because the extension rows are what `golden/` is mostly *about* —
/// subscripts, width annotations, contracts — and asking only the compatibility
/// fixtures would need each of them written twice.
#[test]
fn strict_mode_reports_exactly_the_rows_the_manifest_marks() {
    let (rows, _) = manifest_rows();
    let marked: BTreeSet<&str> = rows
        .iter()
        .filter(|row| row.strict == "rejects" || row.strict == "warns")
        .map(|row| row.id.as_str())
        .collect();

    let mut found: BTreeMap<&'static str, BTreeSet<String>> = BTreeMap::new();
    for (name, sources) in strict_corpus() {
        let mut map = SourceMap::new();
        let ids: Vec<_> = sources
            .iter()
            .enumerate()
            .map(|(index, text)| map.add_file(format!("{name}-{index}.k65"), text.clone()))
            .collect();
        for id in ids {
            let source = map.file(id);
            let parse = k816::parser::parse_source(source, None);
            let linkage = k816::linkage::Linkage::resolve(&map, EntryEnvironment::default());
            let (model, _) = k816::semantic::analyze_unit(&parse.file, source.name(), &linkage);
            for (row, _) in k816::compat::classify(model.program(&parse.file), source) {
                found.entry(row).or_default().insert(name.clone());
            }
        }
    }

    assert_eq!(
        marked.len(),
        27,
        "the manifest marks {} rows for the flag; if that is deliberate, move the \
         number here so the count stays a statement someone made",
        marked.len()
    );
    // A marked row is witnessed by the fixture it *names*, not merely by
    // something somewhere. Pointing a row at a fixture that does not exercise
    // it is how `binary Ident` and `far goto` both shipped, and the gate that
    // caught those derives its witness from grammar-rule syntax — which the
    // extension table does not have, because its `Construct` cells are prose.
    // This is that check for the rows the flag reports.
    let mut miscited: Vec<String> = Vec::new();
    for row in &rows {
        if row.strict == "—" || names_no_fixture(&row.fixture) {
            continue;
        }
        let cited = found.get(row.id.as_str());
        if !cited.is_some_and(|fixtures| fixtures.contains(&row.fixture)) {
            miscited.push(format!(
                "  `{}` is marked `{}` and names `{}`; produced by {:?}",
                row.id,
                row.strict,
                row.fixture,
                cited
                    .map(|f| f.iter().cloned().collect::<Vec<_>>())
                    .unwrap_or_default()
            ));
        }
    }

    assert!(
        miscited.is_empty(),
        "a marked row names a fixture that does not produce it:\n{}",
        miscited.join("\n")
    );

    let unmarked: Vec<&&str> = found.keys().filter(|id| !marked.contains(*id)).collect();
    assert!(
        unmarked.is_empty(),
        "strict mode reports these rows, which the manifest leaves silent: {unmarked:?}"
    );
    let unwitnessed: Vec<&&str> = marked
        .iter()
        .filter(|id| !found.contains_key(*id))
        .collect();
    assert!(
        unwitnessed.is_empty(),
        "the manifest marks these rows for `--strict-k65` and no fixture in the corpus \
         makes it report them: {unwitnessed:?}\n\nWrite the case. A row marked in the \
         document and found by nothing is a promise with no witness."
    );
}

/// Every source the strict gate walks: the compatibility corpus and the
/// golden programs, each as its unit group.
fn strict_corpus() -> Vec<(String, Vec<String>)> {
    let mut out: Vec<(String, Vec<String>)> = fixtures()
        .iter()
        .map(|fixture| (fixture.name.clone(), sources_of(fixture)))
        .collect();
    let golden = root().join("tests/golden");
    let mut paths: Vec<PathBuf> = std::fs::read_dir(&golden)
        .expect("read tests/golden")
        .filter_map(Result::ok)
        .map(|entry| entry.path())
        .filter(|path| {
            matches!(
                path.extension().and_then(|e| e.to_str()),
                Some("k65") | Some("k816")
            )
        })
        .collect();
    paths.sort();
    for path in paths {
        let name = format!("golden/{}", path.file_name().unwrap().to_string_lossy());
        let text = std::fs::read_to_string(&path).expect("read golden source");
        out.push((name, vec![text]));
    }
    out
}

/// The `Strict` column agrees with the status it sits beside.
///
/// `--strict-k65` rejects extensions and warns at intentional changes
/// (`plans/06-compatibility.md` decision 20), so a row that reports anything
/// has to be one of those two — and a row that reports nothing has to say in
/// its `Reason` why not. The column exists because the status alone does not
/// decide it: most `intentionally-changed` rows are constructs K816 already
/// refuses, where a warning would repeat an error the author has already read.
#[test]
fn the_strict_column_agrees_with_the_status() {
    const STRICT: [&str; 3] = ["rejects", "warns", "—"];
    for row in manifest_rows().0 {
        assert!(
            STRICT.contains(&row.strict.as_str()),
            "row `{}` reads `{}` in the `Strict` column, which is not one of {STRICT:?}",
            row.id,
            row.strict
        );
        match row.strict.as_str() {
            "rejects" => assert_eq!(
                row.status, "K816-extension",
                "row `{}` is rejected by strict mode but is not an extension",
                row.id
            ),
            "warns" => assert_eq!(
                row.status, "intentionally-changed",
                "row `{}` is warned about by strict mode but is not an \
                 intentional change",
                row.id
            ),
            _ => {}
        }
    }
}
