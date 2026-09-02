/*****************************************************************************/
/*                                                                           */
/*                                 corpus.rs                                 */
/*                                                                           */
/*        Every K816/K65 program in the tree, with its unit set              */
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

//! The corpus: every program under `examples/`, `tests/golden/` and
//! `tests/compat/`, each with the units it is compiled with.
//!
//! Shared because more than one gate is over "every program in the tree" —
//! `tests/live_outs.rs` measures each one, `tests/diagnostics.rs` walks each
//! one's names — and a second enumeration would be a second, weaker corpus.

// Included with `#[path]` by every suite that needs the corpus, so a helper
// one of them does not use is dead code *there* and not here.
#![allow(dead_code)]

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

/// `examples/`, with the unit set each program is compiled in.
///
/// The same rows `tests/corpus.rs` carries, for the same reason: K65 has no
/// import syntax, so which files are compiled together is build structure and
/// has to be stated.
pub const EXAMPLES: &[(&str, &[&str])] = &[
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

/// The goldens that are two units of one program **and do not say so**.
///
/// `tests/optimizer.rs` skips the second of each pair because it cannot be
/// compiled alone; here both are compiled, with the sibling in the map, since
/// a cross-unit call is the case the escape-hatch objection is about.
///
/// A pair whose units `import` each other needs no entry: [`import_closure`]
/// finds it by reading the source, which is the whole point of the
/// construct. This list is what is left over — the goldens written before
/// there was a way to say it — and sub-plan K's task K5 empties it.
pub const GOLDEN_PAIRS: &[[&str; 2]] = &[
    ["contract-unit-a.k816", "contract-unit-b.k816"],
    ["layout-unit-a.k816", "layout-unit-b.k816"],
    ["opt-crossunit-a.k816", "opt-crossunit-b.k816"],
    ["unit-a.k816", "unit-b.k816"],
];

/// One set of files compiled together, and which of them is the entry.
///
/// Every corpus source compiles under the default entry environment, so no
/// set carries one: `--entry` describes the machine `main` is handed, and
/// nothing here is written for a different machine.
pub struct UnitSet {
    /// How the source is named in [`DIFFERENCE`] — repository-relative.
    pub name: String,
    pub files: Vec<PathBuf>,
    pub entry: usize,
}

pub fn root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

/// Every unit of every set, in a stable order.
pub fn corpus() -> Vec<UnitSet> {
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
            None => import_closure(&dir.join(name)),
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

/// `entry` and every file it `import`s, transitively, in load order.
///
/// The compiler's own loader rather than a second reading of the same
/// question: a set assembled by a rule of this file's own would be a place
/// for the corpus and the compiler to disagree about what a program is.
pub fn import_closure(entry: &Path) -> Vec<PathBuf> {
    let Ok(text) = std::fs::read_to_string(entry) else {
        return vec![entry.to_path_buf()];
    };
    let mut map = k816::source::SourceMap::new();
    map.add_file_at(entry.to_string_lossy(), entry, text);
    k816::load_imports(&mut map, &mut |path| std::fs::read_to_string(path));
    map.files().map(|file| PathBuf::from(file.name())).collect()
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
pub fn fixture_header(dir: &Path) -> BTreeMap<String, Vec<String>> {
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

pub fn fixture_units(dir: &Path) -> Vec<String> {
    fixture_header(dir)
        .remove("units")
        .unwrap_or_else(|| panic!("{}: fixture.txt has no `units`", dir.display()))
        .join(" ")
        .split_whitespace()
        .map(str::to_owned)
        .collect()
}

/// One program of the corpus, with what it takes to link it.
pub struct Program {
    pub name: String,
    pub units: Vec<PathBuf>,
    pub config: PathBuf,
    /// Hand-written ca65 modules the program is linked with. Only
    /// `examples/fullstack` has one.
    pub extra: Vec<PathBuf>,
}

/// Every distinct program in the tree, with the layout each one links against.
///
/// `corpus()` yields one entry per *unit compilation* — a fixture with two
/// units appears twice, once with each as the entry — because that is what
/// the suites which measure a compilation need. A link is over the whole set
/// at once, so the sets are deduplicated by their file list here; the entry
/// index is not consulted, because `cl65` compiles every K816 file on its
/// command line and so does the manual path.
pub fn programs() -> Vec<Program> {
    let root = root();
    let driver_cfg = root.join("tests/driver.cfg");
    let mut seen: Vec<Vec<PathBuf>> = Vec::new();
    let mut programs = Vec::new();

    for set in corpus() {
        if seen.contains(&set.files) {
            continue;
        }
        seen.push(set.files.clone());

        // A compat fixture states its layout in its header, and
        // `tests/compat.rs` requires every configuration under
        // `tests/compat/` to be named by one; an example is not a fixture and
        // does not share them.
        let (config, extra) = if set.name.starts_with("tests/compat/") {
            let fixture = set.files[0].parent().expect("a fixture directory");
            let named = fixture_header(fixture)
                .remove("config")
                .and_then(|values| values.into_iter().next())
                .unwrap_or_else(|| panic!("{}: fixture.txt has no `config`", set.name));
            (root.join("tests/compat").join(named), Vec::new())
        } else if set.name == "examples/fullstack/main.k816" {
            (
                root.join("examples/fullstack/fullstack.cfg"),
                vec![root.join("examples/fullstack/startup.asm")],
            )
        } else if set.name.starts_with("examples/k65/atari-xl/") && !set.name.contains("/include/")
        {
            (root.join("examples/k65/atari-xl/atarixl.cfg"), Vec::new())
        } else {
            (driver_cfg.clone(), Vec::new())
        };

        programs.push(Program {
            name: set.name,
            units: set.files,
            config,
            extra,
        });
    }
    programs
}

/// Compiles every unit of `program` into `directory`, returning the assembly
/// each one emitted.
///
/// `None` when any unit is a refusal — forty of the corpus's unit
/// compilations are, by design: a fixture whose `expect` list is its point.
/// Those are the business of `tests/compat.rs` and `tests/diagnostics.rs`,
/// not of a gate over what a build produces.
pub fn compile_units(directory: &Path, program: &Program) -> Option<Vec<PathBuf>> {
    let mut assemblies = Vec::new();
    for (index, unit) in program.units.iter().enumerate() {
        let assembly = directory.join(format!("unit-{index}.s"));
        let mut compile = std::process::Command::new(env!("CARGO_BIN_EXE_k816"));
        compile.arg("compile").arg(unit);
        for sibling in program.units.iter().filter(|sibling| *sibling != unit) {
            compile.args(["--unit".as_ref(), sibling.as_os_str()]);
        }
        compile.args(["-o".as_ref(), assembly.as_os_str()]);
        let output = compile
            .output()
            .unwrap_or_else(|error| panic!("cannot run k816 compile: {error}"));
        if !output.status.success() {
            return None;
        }
        assemblies.push(assembly);
    }
    Some(assemblies)
}

/// Whether every unit of `program` compiles, which is what makes it linkable.
pub fn compiles(directory: &Path, program: &Program) -> bool {
    compile_units(directory, program).is_some()
}

/// A file-system-safe stem for a program's scratch directory.
pub fn stem(name: &str) -> String {
    name.replace(['/', '.'], "-")
}
