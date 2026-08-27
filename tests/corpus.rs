/*****************************************************************************/
/*                                                                           */
/*                                 corpus.rs                                 */
/*                                                                           */
/*        Tree-sitter corpus runner and generated-artifact fingerprint       */
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

//! Runs `tree-sitter-k65/test/corpus/*.txt` from `cargo test`, and checks that
//! the committed generated parser still matches `grammar.js`.
//!
//! The two halves answer different questions. The corpus runner proves the
//! *compiled* parser produces the expected trees; it cannot notice a
//! `parser.c` that was never regenerated after a grammar edit, because it never
//! regenerates anything. The `generated.lock` fingerprint closes exactly that
//! gap. Regeneration itself stays a maintainer operation (`npx
//! tree-sitter-cli@0.26 generate`), so a clean checkout still builds and tests
//! without Node.js.
//!
//! Corpus files stay in the vanilla Tree-sitter format, so
//! `npx tree-sitter-cli@0.26 test` keeps working for maintainers.

use std::path::{Path, PathBuf};

use k816::parser::parse_source;
use k816::source::SourceMap;

/// The Tree-sitter CLI release the committed artifacts were generated with.
const CLI_VERSION: &str = "0.26";

/// Generated files whose contents the fingerprint covers, plus the grammar
/// they are generated from.
const FINGERPRINTED: [&str; 4] = [
    "grammar.js",
    "src/parser.c",
    "src/grammar.json",
    "src/node-types.json",
];

fn grammar_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tree-sitter-k65")
}

// ---------------------------------------------------------------------------
// Generation fingerprint
// ---------------------------------------------------------------------------

/// FNV-1a (64-bit). A staleness detector, not a security primitive: it only
/// has to change when a file changes, and it keeps the test dependency-free.
fn fingerprint(bytes: &[u8]) -> String {
    let mut hash: u64 = 0xcbf2_9ce4_8422_2325;
    for byte in bytes {
        hash ^= u64::from(*byte);
        hash = hash.wrapping_mul(0x0000_0100_0000_01b3);
    }
    format!("{hash:016x}")
}

fn current_lock() -> String {
    let mut lock = String::from(
        "# Generation fingerprint for the committed Tree-sitter artifacts.\n\
         #\n\
         # `cargo test --test corpus` re-hashes every file listed here and fails when\n\
         # a hash differs, so editing `grammar.js` without regenerating cannot pass\n\
         # quietly. Regenerate with `npx tree-sitter-cli@0.26 generate` in\n\
         # `tree-sitter-k65/`, then refresh this file with\n\
         # `UPDATE_GENERATED_LOCK=1 cargo test --test corpus`.\n\
         #\n\
         # Hashes are FNV-1a/64 over the raw file bytes.\n\n",
    );
    lock.push_str(&format!("tree-sitter-cli = {CLI_VERSION}\n"));
    for name in FINGERPRINTED {
        let bytes = std::fs::read(grammar_dir().join(name))
            .unwrap_or_else(|error| panic!("cannot read tree-sitter-k65/{name}: {error}"));
        lock.push_str(&format!("{name} = {}\n", fingerprint(&bytes)));
    }
    lock
}

#[test]
fn generated_artifacts_match_the_grammar() {
    let path = grammar_dir().join("generated.lock");
    let current = current_lock();

    if std::env::var_os("UPDATE_GENERATED_LOCK").is_some() {
        std::fs::write(&path, &current).expect("write generated.lock");
        return;
    }

    let committed = std::fs::read_to_string(&path).expect(
        "tree-sitter-k65/generated.lock is missing; \
         run `UPDATE_GENERATED_LOCK=1 cargo test --test corpus` after regenerating",
    );
    assert_eq!(
        committed.trim_end(),
        current.trim_end(),
        "\ntree-sitter-k65/generated.lock does not describe the files on disk.\n\
         If you edited grammar.js, regenerate the parser:\n\
         \x20   cd tree-sitter-k65 && npx tree-sitter-cli@{CLI_VERSION} generate\n\
         then refresh the fingerprint:\n\
         \x20   UPDATE_GENERATED_LOCK=1 cargo test --test corpus\n"
    );
}

// ---------------------------------------------------------------------------
// Corpus runner
// ---------------------------------------------------------------------------

/// One corpus case: a name, the source to parse, and the expected tree.
struct Case {
    name: String,
    source: String,
    expected: String,
    /// 1-based line of the case header, for failure messages.
    line: usize,
}

/// Splits a corpus file into cases.
///
/// The format is Tree-sitter's: a rule of `=` characters, the case name and
/// any `:attribute` lines, another rule of `=`, the source, a rule of `-`, and
/// the expected S-expression.
fn parse_corpus(text: &str, path: &Path) -> Vec<Case> {
    let is_rule = |line: &str, character: char| {
        let line = line.trim_end();
        line.len() >= 3 && line.chars().all(|c| c == character)
    };
    let context = |line: usize| format!("{}:{line}", path.display());

    let lines: Vec<&str> = text.lines().collect();
    let mut cases = Vec::new();
    let mut index = 0;
    while index < lines.len() {
        if !is_rule(lines[index], '=') {
            assert!(
                lines[index].trim().is_empty(),
                "{}: expected a `===` case header",
                context(index + 1)
            );
            index += 1;
            continue;
        }
        let header_line = index + 1;
        index += 1;

        // Name and attributes, up to the closing `===` rule.
        let mut name = None;
        while index < lines.len() && !is_rule(lines[index], '=') {
            let line = lines[index].trim();
            if !line.is_empty() && !line.starts_with(':') && name.is_none() {
                name = Some(line.to_owned());
            }
            index += 1;
        }
        assert!(
            index < lines.len(),
            "{}: unterminated case header",
            context(header_line)
        );
        index += 1;

        let mut source = String::new();
        while index < lines.len() && !is_rule(lines[index], '-') {
            source.push_str(lines[index]);
            source.push('\n');
            index += 1;
        }
        assert!(
            index < lines.len(),
            "{}: case has no `---` separator",
            context(header_line)
        );
        index += 1;

        let mut expected = String::new();
        while index < lines.len() && !is_rule(lines[index], '=') {
            expected.push_str(lines[index]);
            expected.push('\n');
            index += 1;
        }

        cases.push(Case {
            name: name.unwrap_or_else(|| panic!("{}: case has no name", context(header_line))),
            source: source.trim_matches('\n').to_owned() + "\n",
            expected,
            line: header_line,
        });
    }
    cases
}

/// Splits an S-expression into comparable tokens: parentheses, quoted
/// literals (`"}"`, `' '`), and bare words such as `identifier` or `name:`.
fn tokenize_sexp(text: &str) -> Vec<String> {
    let mut tokens = Vec::new();
    let mut word = String::new();
    let mut characters = text.chars();
    while let Some(character) = characters.next() {
        match character {
            '(' | ')' => {
                if !word.is_empty() {
                    tokens.push(std::mem::take(&mut word));
                }
                tokens.push(character.to_string());
            }
            '"' | '\'' => {
                if !word.is_empty() {
                    tokens.push(std::mem::take(&mut word));
                }
                let mut literal = String::from(character);
                for inner in characters.by_ref() {
                    literal.push(inner);
                    if inner == character && !literal.ends_with(&format!("\\{character}")) {
                        break;
                    }
                }
                tokens.push(literal);
            }
            character if character.is_whitespace() => {
                if !word.is_empty() {
                    tokens.push(std::mem::take(&mut word));
                }
            }
            character => word.push(character),
        }
    }
    if !word.is_empty() {
        tokens.push(word);
    }
    tokens
}

fn corpus_files() -> Vec<PathBuf> {
    let dir = grammar_dir().join("test/corpus");
    let mut files: Vec<PathBuf> = std::fs::read_dir(&dir)
        .unwrap_or_else(|error| panic!("cannot read {}: {error}", dir.display()))
        .map(|entry| entry.expect("corpus directory entry").path())
        .filter(|path| path.extension().is_some_and(|extension| extension == "txt"))
        .collect();
    files.sort();
    assert!(!files.is_empty(), "no corpus files in {}", dir.display());
    files
}

#[test]
fn corpus_cases_produce_the_expected_trees() {
    for path in corpus_files() {
        let text = std::fs::read_to_string(&path).expect("read corpus file");
        let cases = parse_corpus(&text, &path);
        assert!(!cases.is_empty(), "{} contains no cases", path.display());
        for case in cases {
            let mut map = SourceMap::new();
            let id = map.add_file(format!("{}:{}", path.display(), case.name), &case.source);
            let parsed = parse_source(map.file(id), None);
            let actual = tokenize_sexp(&parsed.tree.to_sexp()).join(" ");
            let expected = tokenize_sexp(&case.expected).join(" ");
            assert_eq!(
                actual,
                expected,
                "\ncorpus case `{}` ({}:{}) parsed differently:\n\
                 source:\n{}\n",
                case.name,
                path.display(),
                case.line,
                case.source
            );
        }
    }
}

/// Every source under `examples/` and what it still reports.
///
/// One row per program, with the *unit set* it is compiled in — K65 has no
/// import syntax, so which files are compiled together is the build structure
/// and has to be stated. The expected codes are exact: a row that goes
/// silent is a construct that landed, and a row that grows is a regression,
/// and both should be noticed here rather than in a user's build.
const CORPUS: &[(&str, &[&str], &[&str])] = &[
    ("examples/fullstack/main.k816", &[], &[]),
    // Clean since G4: its one remaining construct was an evaluator bracket
    // in operand position (`a = [$FF]`), and the evaluator landed.
    ("examples/hello_uart.k65", &[], &[]),
    (
        "examples/k65/atari-xl/dli-example/main.k65",
        &["examples/k65/atari-xl/include/defs.k65"],
        &[],
    ),
    (
        "examples/k65/atari-xl/landscape/main.k65",
        &["examples/k65/atari-xl/include/defs.k65"],
        &[],
    ),
    (
        "examples/k65/atari-xl/rainbow/main.k65",
        &["examples/k65/atari-xl/include/defs.k65"],
        &[],
    ),
    // `rnd()` in a `for … eval` body: reserved legacy syntax that keeps its
    // own code by design (sub-plan E, decision 18), not a construct waiting
    // to be implemented.
    (
        "examples/k65/atari-xl/starfield/main.k65",
        &["examples/k65/atari-xl/include/defs.k65"],
        &["K816-K65-0002", "K816-K65-0002"],
    ),
    ("examples/k65/atari-xl/include/defs.k65", &[], &[]),
];

fn read(path: &str) -> String {
    let full = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join(path);
    std::fs::read_to_string(&full).unwrap_or_else(|e| panic!("read {full:?}: {e}"))
}

/// The legacy corpus compiles, and what it does not compile is named.
///
/// This is sub-plan E's own success criterion: the HLA statement family, the
/// declaration family, and the data blocks are done when the programs that
/// use them come out the other side. What remains is listed per file so that
/// "still failing" and "failing differently" are different test results.
#[test]
fn every_corpus_program_compiles_or_names_what_is_left() {
    for (path, units, expected) in CORPUS {
        let mut map = SourceMap::new();
        let file = map.add_file(*path, read(path));
        for unit in *units {
            map.add_file(*unit, read(unit));
        }
        let result = k816::compile_to_ca65(&map, file, &k816::Options { optimize: false });
        let codes: Vec<&str> = result
            .diagnostics
            .iter()
            .map(|diagnostic| diagnostic.code.as_str())
            .collect();
        assert_eq!(codes, *expected, "{path}");
    }
}

/// Nothing in the corpus is waiting on anyone.
///
/// Sub-plan E's gate was "no deferral names *E*", because the evaluator was
/// still sub-plan G's and `K816-E0201` could not go. G6 was the last task a
/// deferral could name, so the code is retired and the gate is the one the
/// plan wrote in the first place: **no `K816-E02xx` anywhere in the corpus**.
/// It reads as a range rather than a constant on purpose — the range stays
/// reserved for "recognized and not compiled", and a future one has to fail
/// here.
#[test]
fn no_corpus_program_waits_on_a_sub_plan() {
    for (path, units, _) in CORPUS {
        let mut map = SourceMap::new();
        let file = map.add_file(*path, read(path));
        for unit in *units {
            map.add_file(*unit, read(unit));
        }
        let result = k816::compile_to_ca65(&map, file, &k816::Options { optimize: false });
        for diagnostic in &result.diagnostics {
            assert!(
                !diagnostic.code.as_str().starts_with("K816-E02"),
                "{path} still waits on a sub-plan: {diagnostic:?}"
            );
        }
    }
}

#[test]
fn corpus_case_names_are_unique() {
    // B12's manifest audit maps every feature row to a corpus case name, so
    // the names have to identify a case on their own.
    let mut seen: Vec<(String, PathBuf)> = Vec::new();
    for path in corpus_files() {
        let text = std::fs::read_to_string(&path).expect("read corpus file");
        for case in parse_corpus(&text, &path) {
            if let Some((_, first)) = seen.iter().find(|(name, _)| *name == case.name) {
                panic!(
                    "corpus case name `{}` is used in both {} and {}",
                    case.name,
                    first.display(),
                    path.display()
                );
            }
            seen.push((case.name, path.clone()));
        }
    }
}

// ---------------------------------------------------------------------------
// The optimizer over the corpus (sub-plan F, task F8)
// ---------------------------------------------------------------------------

/// What the optimizer does to every corpus program, stated per file: the
/// instruction lines it stops emitting, and why.
///
/// **Decision 21: an optimized program is compared to an unoptimized one at
/// the bytes, and every difference is argued.** This is F's own success
/// measure, as `examples/` compiling was E's — and the lines are listed here
/// rather than accepted as a diff because non-negotiable 15 makes a re-blessed
/// diff the single easiest way to ship a wrong-bytes bug in this sub-plan.
///
/// A program that does not compile has nothing to compare, and its row says so
/// with the code that stops it.
const OPTIMIZER_EFFECT: &[(&str, &[&str], &str)] = &[
    (
        "examples/fullstack/main.k816",
        &["lda     #$00"],
        "one dead load: the next line overwrites A, N, and Z before anything \
         reads them. There is no mode operation in the file to eliminate or \
         fold",
    ),
    (
        "examples/hello_uart.k65",
        &[],
        "nothing: every instruction in it is reached and its one bracket is an \
         immediate the optimizer has no reason to touch",
    ),
    (
        "examples/k65/atari-xl/dli-example/main.k65",
        &["rts"],
        "the implicit return after a handler that ends in an unconditional \
         transfer, which no edge reaches. Nothing else moves: the two reads of \
         a hardware register stay, because value tracking never reads memory",
    ),
    (
        "examples/k65/atari-xl/landscape/main.k65",
        &["rts"],
        "the implicit return after a `main` that loops forever. Both reads of \
         the POKEY random-number register survive, which is the case decision \
         14 is written for",
    ),
    (
        "examples/k65/atari-xl/rainbow/main.k65",
        &["rts"],
        "the implicit return after a `main` that loops forever",
    ),
    (
        "examples/k65/atari-xl/starfield/main.k65",
        &[],
        "does not compile: two K816-K65-0002, reserved legacy `rnd()` syntax",
    ),
    (
        "examples/k65/atari-xl/include/defs.k65",
        &[],
        "declarations only: there is no code in it to optimize",
    ),
];

/// The instruction lines of an emitted assembly, in order, without comments.
fn instruction_lines(assembly: &str) -> Vec<String> {
    assembly
        .lines()
        .map(|line| line.split(';').next().unwrap_or_default().trim_end())
        .filter(|line| line.starts_with("        ") && !line.trim_start().starts_with('.'))
        .map(|line| line.trim().to_owned())
        .collect()
}

/// Every corpus program compiles the same, optimized or not, and every
/// instruction the optimizer stopped emitting is named and argued.
#[test]
fn every_corpus_program_survives_the_optimizer_with_its_difference_stated() {
    for (path, units, expected) in CORPUS {
        let (_, removed_lines, why) = OPTIMIZER_EFFECT
            .iter()
            .find(|(named, _, _)| named == path)
            .unwrap_or_else(|| panic!("{path} has no stated optimizer effect"));
        assert!(why.len() > 30, "{path}: {why}");

        let compile = |optimize: bool| {
            let mut map = SourceMap::new();
            let file = map.add_file(*path, read(path));
            for unit in *units {
                map.add_file(*unit, read(unit));
            }
            k816::compile_to_ca65(&map, file, &k816::Options { optimize })
        };
        let optimized = compile(true);
        let codes: Vec<&str> = optimized
            .diagnostics
            .iter()
            .map(|diagnostic| diagnostic.code.as_str())
            .collect();
        // The optimizer reports nothing and refuses nothing: it introduces no
        // diagnostic at all, which is a decision rather than an oversight — a
        // warning for a *correct* optimization would be noise on every build,
        // and "where did my instruction go" is answered by
        // `emit-ir --after-opt --explain` on demand.
        assert_eq!(codes, *expected, "{path}");

        let Some(optimized) = optimized.assembly else {
            assert!(
                why.starts_with("does not compile"),
                "{path} produced no assembly but its row does not say so"
            );
            continue;
        };
        let unoptimized = compile(false).assembly.expect("both runs agree");

        let mut remaining = instruction_lines(&optimized);
        let mut removed: Vec<String> = Vec::new();
        for line in instruction_lines(&unoptimized) {
            match remaining.iter().position(|kept| *kept == line) {
                Some(index) => {
                    remaining.remove(index);
                }
                None => removed.push(line),
            }
        }
        removed.sort();
        assert!(
            remaining.is_empty(),
            "{path}: the optimizer emitted instructions the unoptimized run did not: \
             {remaining:?}"
        );
        assert_eq!(removed, *removed_lines, "{path}: {why}");
        assert!(
            optimized.len() <= unoptimized.len(),
            "{path}: the optimizer made the output longer"
        );
    }
}
