/*****************************************************************************/
/*                                                                           */
/*                                 format.rs                                 */
/*                                                                           */
/*      The formatter changes whitespace and nothing else, over the whole    */
/*                               corpus                                      */
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

//! The formatter, held to its one claim: **it changes whitespace and nothing
//! else** (`plans/07-lsp-tooling.md` decision 4).
//!
//! The claim is checked three ways over every program in the tree — the
//! non-whitespace bytes are identical, the emitted assembly is identical, and
//! formatting twice changes nothing the second time — because a formatter
//! that is *nearly* whitespace-only is a formatter that has silently edited
//! somebody's program.

use std::collections::BTreeSet;
use std::path::PathBuf;

use k816::parser::{format, parse_source};
use k816::source::SourceMap;
use k816::{Options, compile_to_ca65};

#[path = "common/corpus.rs"]
mod corpus_files;

/// Every source in the tree, once, with its text.
fn programs() -> Vec<(String, PathBuf, String)> {
    use corpus_files::{corpus, root};

    let mut seen = BTreeSet::new();
    let mut programs = Vec::new();
    for set in corpus() {
        for entry in &set.files {
            let name = entry
                .strip_prefix(root())
                .unwrap_or(entry)
                .to_string_lossy()
                .into_owned();
            if !seen.insert(name.clone()) {
                continue;
            }
            let text = std::fs::read_to_string(entry).expect("read a corpus source");
            programs.push((name, entry.clone(), text));
        }
    }
    programs
}

/// `text` formatted, or `None` when its parse could not be read.
fn formatted(name: &str, text: &str) -> Option<String> {
    let mut map = SourceMap::new();
    let file = map.add_file(name, text);
    let parse = parse_source(map.file(file), None);
    format::format(map.file(file), &parse.tree)
}

/// Everything but whitespace, which is what the formatter may not change.
fn skeleton(text: &str) -> String {
    text.chars().filter(|c| !c.is_whitespace()).collect()
}

/// The measurement decision 4 asks for before the rule is believed: over
/// every program in the tree, no file's non-whitespace bytes change.
///
/// Empty by construction — the formatter emits each token verbatim from the
/// source and only decides what goes *between* them — and this is what says
/// so rather than the construction being trusted.
#[test]
fn formatting_changes_no_programs_non_whitespace_bytes() {
    let mut changed = 0;
    let mut unchanged = 0;
    let mut refused = 0;
    let mut different: Vec<String> = Vec::new();

    for (name, _, text) in programs() {
        match formatted(&name, &text) {
            None => refused += 1,
            Some(out) => {
                if skeleton(&out) != skeleton(&text) {
                    different.push(name.clone());
                }
                if out == text {
                    unchanged += 1;
                } else {
                    changed += 1;
                }
            }
        }
    }

    assert!(
        different.is_empty(),
        "the formatter changed more than whitespace in {different:#?}"
    );
    assert!(
        unchanged + changed > 130,
        "only {} programs were formatted",
        unchanged + changed
    );
    // A file the grammar could not read is not formatted, and the corpus has
    // a few that pin a refusal.
    assert!(refused > 0 && refused < 10, "{refused} refusals");
}

/// Formatting is idempotent: the second run changes nothing.
///
/// The property that catches a rule which oscillates — an indent computed
/// from the text it is about to replace, most of all.
#[test]
fn formatting_is_idempotent_over_the_corpus() {
    for (name, _, text) in programs() {
        let Some(once) = formatted(&name, &text) else {
            continue;
        };
        let twice = formatted(&name, &once)
            .unwrap_or_else(|| panic!("{name}: formatting broke its own output"));
        assert_eq!(once, twice, "{name}: formatting is not idempotent");
    }
}

/// A formatted program compiles to the same assembly and reports the same
/// diagnostics.
///
/// The strongest statement of "whitespace and nothing else": the compiler is
/// asked, and it cannot tell the two apart except by span.
#[test]
fn a_formatted_program_compiles_to_the_same_assembly() {
    let mut compared = 0;
    for (name, _, text) in programs() {
        let Some(out) = formatted(&name, &text) else {
            continue;
        };
        if out == text {
            continue;
        }

        let compile = |source: &str| {
            let mut map = SourceMap::new();
            let file = map.add_file(&name, source);
            let result = compile_to_ca65(&map, file, &Options::default());
            let codes: Vec<String> = result
                .diagnostics
                .iter()
                .map(|diagnostic| diagnostic.code.as_str().to_owned())
                .collect();
            (result.assembly, codes)
        };
        let (before_assembly, before_codes) = compile(&text);
        let (after_assembly, after_codes) = compile(&out);

        assert_eq!(before_codes, after_codes, "{name}: the diagnostics moved");
        match (before_assembly, after_assembly) {
            (Some(before), Some(after)) => {
                // The `.dbg line` directives carry line numbers, and the
                // formatter moves no line — so even those are identical.
                assert_eq!(before, after, "{name}: the assembly moved");
                compared += 1;
            }
            (None, None) => {}
            (before, after) => panic!(
                "{name}: one form compiled and the other did not ({} / {})",
                before.is_some(),
                after.is_some()
            ),
        }
    }
    assert!(compared > 40, "only {compared} programs were compared");
}

/// The goldens and the legacy fixtures are already formatted.
///
/// They are the files the rule is written for. Four of them were not, when
/// the rule first ran: `branch-range.k816` and `data-entries.k816` wrote a
/// block's body at the block's own indent, and `native-modes.k816` and
/// `native-reloc.k816` indented a label by two where every other file puts
/// one at column 0. Each was looked at and the *file* was wrong; they are
/// reformatted, and their assembly is unchanged
/// (`postmortem.md` §H.10.3).
#[test]
fn the_goldens_and_the_legacy_fixtures_are_already_formatted() {
    let mut checked = 0;
    for (name, _, text) in programs() {
        if !name.starts_with("tests/golden/") && !name.starts_with("tests/compat/legacy-k816/") {
            continue;
        }
        let Some(out) = formatted(&name, &text) else {
            continue;
        };
        assert_eq!(out, text, "{name} is not formatted");
        checked += 1;
    }
    assert!(checked > 50, "only {checked} fixtures were checked");
}

/// The two-space K65 sources change only in leading whitespace, and they are
/// counted.
///
/// The K65 fixtures and the atari-xl examples indent by two, which is the
/// original compiler's style. They are imported corpus, so they keep their
/// own formatting; what this asserts is that re-indenting them would touch
/// nothing but the indent.
#[test]
fn the_two_space_sources_change_only_in_leading_whitespace() {
    let mut reindented = 0;
    for (name, _, text) in programs() {
        let Some(out) = formatted(&name, &text) else {
            continue;
        };
        if out == text {
            continue;
        }
        for (before, after) in text.lines().zip(out.lines()) {
            assert_eq!(
                before.trim_start(),
                after.trim_start(),
                "{name}: a line changed beyond its indent"
            );
        }
        reindented += 1;
    }
    assert!(
        reindented > 60,
        "only {reindented} sources would be re-indented; the two-space corpus is larger"
    );
}

/// The one file in the corpus written with tabs keeps them, because they are
/// *inside* lines rather than in front of them.
///
/// Decision 4 expected it to re-indent. Its tabs align `=` and the comment
/// columns of a hardware-register table; the leading whitespace is a single
/// space. Recorded rather than forced: a formatter that only normalizes the
/// indent has nothing to say about a column of aligned comments
/// (`postmortem.md` §H.10.1).
#[test]
fn the_tab_file_keeps_its_interior_tabs() {
    use corpus_files::root;

    let path = root().join("examples/k65/atari-xl/include/defs.k65");
    let text = std::fs::read_to_string(&path).expect("read the tab file");
    assert!(text.contains('\t'), "the fixture no longer has tabs");
    let out = formatted("defs.k65", &text).expect("it parses");
    assert!(out.contains('\t'), "the interior tabs were removed");
    // And its CRLF line endings survive, which is the other thing a
    // formatter must not quietly convert.
    assert!(text.contains("\r\n"));
    assert!(out.contains("\r\n"), "the line endings were converted");
}

/// A file the grammar could not read is returned unchanged, and `--check`
/// says which one and exits non-zero.
#[test]
fn a_file_with_a_parse_error_is_left_alone() {
    const BROKEN: &str = "func main @a8 {\n    return\n";
    assert_eq!(formatted("broken.k816", BROKEN), None);

    let dir = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join("fmt-error");
    std::fs::create_dir_all(&dir).expect("create the work dir");
    let path = dir.join("broken.k816");
    std::fs::write(&path, BROKEN).expect("write the source");

    let output = std::process::Command::new(env!("CARGO_BIN_EXE_k816"))
        .arg("fmt")
        .arg(&path)
        .output()
        .expect("run k816 fmt");
    let stderr = String::from_utf8(output.stderr).expect("stderr is UTF-8");
    assert!(!output.status.success(), "{stderr}");
    assert!(stderr.contains("not formatted"), "{stderr}");
    assert!(stderr.contains("would be guessing"), "{stderr}");
    // And it wrote nothing.
    assert_eq!(
        std::fs::read_to_string(&path).expect("read it back"),
        BROKEN
    );
}

/// `k816 fmt` writes, `k816 fmt --check` does not and exits non-zero.
#[test]
fn fmt_writes_and_check_only_reports() {
    const UGLY: &str = "func main @a8 @i8 {\n\treturn   \n}\n";
    const TIDY: &str = "func main @a8 @i8 {\n    return\n}\n";

    let dir = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join("fmt-write");
    std::fs::create_dir_all(&dir).expect("create the work dir");
    let path = dir.join("ugly.k816");
    let run = |args: &[&std::ffi::OsStr]| {
        std::process::Command::new(env!("CARGO_BIN_EXE_k816"))
            .arg("fmt")
            .args(args)
            .output()
            .expect("run k816 fmt")
    };

    std::fs::write(&path, UGLY).expect("write the source");
    let output = run(&["--check".as_ref(), path.as_os_str()]);
    assert!(!output.status.success(), "--check should exit non-zero");
    assert!(
        String::from_utf8_lossy(&output.stderr).contains("would be reformatted"),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(
        std::fs::read_to_string(&path).expect("read it back"),
        UGLY,
        "--check wrote to the file"
    );

    let output = run(&[path.as_os_str()]);
    assert!(output.status.success());
    assert_eq!(std::fs::read_to_string(&path).expect("read it back"), TIDY);

    // And a second `--check` is now clean, which is idempotence at the CLI.
    let output = run(&["--check".as_ref(), path.as_os_str()]);
    assert!(output.status.success());
}

/// `indents.scm` and the formatter agree, line by line.
///
/// Two artifacts describe the same indentation — the query an editor reads
/// and the rule `k816 fmt` runs — and this is what stops them drifting
/// (decision 14). They are held to the same *list*: every node the query
/// marks `@indent` is one the formatter indents, and the reverse.
#[test]
fn the_indent_query_and_the_formatter_name_the_same_constructs() {
    let query =
        std::fs::read_to_string(corpus_files::root().join("tree-sitter-k65/queries/indents.scm"))
            .expect("read indents.scm");

    let marked: BTreeSet<String> = query
        .lines()
        .filter_map(|line| {
            let line = line.trim();
            let node = line.strip_prefix('(')?;
            let node = node.split(')').next()?;
            line.contains("@indent").then(|| node.to_owned())
        })
        .collect();
    let indenting: BTreeSet<String> = format::INDENTING
        .iter()
        .map(|node| (*node).to_owned())
        .collect();

    assert_eq!(
        marked, indenting,
        "indents.scm and `format::INDENTING` name different constructs"
    );
    assert!(marked.len() >= 6, "only {} constructs indent", marked.len());
}

/// How long analyzing the whole corpus at once takes, and the largest single
/// program in it.
///
/// Decision 12 defers incremental semantic analysis "until profiling proves
/// otherwise", and this is the number a future argument starts from rather
/// than an assertion about the machine it runs on. The bound is loose on
/// purpose: it is here to fail if the analysis becomes *seconds*, not to pin
/// a millisecond count. The measurement is printed, and
/// `postmortem.md` §H.10.5 records what it said on the development machine.
#[test]
fn the_analysis_of_a_whole_workspace_is_measured() {
    use k816::workspace::Workspace;

    // The largest single program in the tree, whatever it is: a fixture that
    // grows past the current one should be the one measured.
    let (largest, _, text) = programs()
        .into_iter()
        .max_by_key(|(_, _, text)| text.len())
        .expect("the corpus is not empty");
    let mut one = Workspace::new();
    one.update("file:///largest.k816", text.clone());
    let start = std::time::Instant::now();
    let analysis = one.analyze();
    let single = start.elapsed();
    assert_eq!(analysis.units.len(), 1);

    // And every program in the tree, open at once — decision 2's program is
    // "the set of open documents", so this is the upper bound on what an
    // editor can be asked to do.
    let mut all = Workspace::new();
    let mut bytes = 0;
    for (name, _, text) in programs() {
        bytes += text.len();
        all.update(format!("file:///{name}"), text);
    }
    let start = std::time::Instant::now();
    let analysis = all.analyze();
    let whole = start.elapsed();

    println!(
        "largest program `{largest}` ({} bytes) analyzed in {single:?}; \
         all {} programs ({bytes} bytes) in {whole:?}",
        text.len(),
        analysis.units.len(),
    );
    assert!(
        single < std::time::Duration::from_secs(1),
        "the largest single program took {single:?}"
    );
    assert!(
        whole < std::time::Duration::from_secs(30),
        "the whole corpus took {whole:?}"
    );
}
