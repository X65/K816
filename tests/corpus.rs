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
