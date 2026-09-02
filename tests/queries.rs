/*****************************************************************************/
/*                                                                           */
/*                                queries.rs                                 */
/*                                                                           */
/*        The five query files compile, and say what they claim to say       */
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

//! The Tree-sitter query files, tested from `cargo test`.
//!
//! **Compiling them is the drift gate.** A query naming a node the grammar no
//! longer has is silently *empty* at run time — an editor just stops
//! highlighting something — and loudly wrong here, because
//! `tree_sitter::Query::new` refuses an unknown node kind, field or capture.
//! The grammar has been regenerated nine times across sub-plan G; a tenth
//! that renames a node fails this file rather than a user's editor
//! (`plans/07-lsp-tooling.md` decision 14).
//!
//! The rest of the file holds each query to what it *claims*: `tags.scm`
//! captures the unit-level definitions the symbol table has and no others,
//! `highlights.scm`'s capture names are the closed set the server's token
//! legend maps, and `locals.scm` binds what `Symbol.scope` scopes.

use std::path::{Path, PathBuf};

use k816::parser::{Capture, Query, parse_source};
use k816::source::SourceMap;

#[path = "common/corpus.rs"]
mod corpus_files;

/// The five files, which are the whole set: a sixth would be one nothing
/// compiles.
const QUERIES: &[&str] = &["highlights", "locals", "tags", "indents", "injections"];

fn queries_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tree-sitter-k65/queries")
}

fn query_source(name: &str) -> String {
    std::fs::read_to_string(queries_dir().join(format!("{name}.scm")))
        .unwrap_or_else(|error| panic!("read {name}.scm: {error}"))
}

fn compiled(name: &str) -> Query {
    Query::compile(&query_source(name))
        .unwrap_or_else(|error| panic!("{name}.scm does not compile: {error}"))
}

/// Every capture the query yields over `source`.
fn captures(name: &str, source: &str) -> Vec<Capture> {
    let mut map = SourceMap::new();
    let file = map.add_file("query.k816", source);
    let parse = parse_source(map.file(file), None);
    compiled(name).captures(&parse.tree, file, source)
}

/// All five compile against the committed grammar.
#[test]
fn every_query_compiles_against_the_committed_grammar() {
    for name in QUERIES {
        let query = compiled(name);
        // `injections.scm` is allowed to capture nothing — K816 embeds no
        // other language, and its comment says so. Every other one has to do
        // something.
        if *name != "injections" {
            assert!(
                !query.capture_names().is_empty(),
                "{name}.scm compiles and captures nothing"
            );
        }
    }
    // And the directory holds these five and no strays.
    let mut present: Vec<String> = std::fs::read_dir(queries_dir())
        .expect("read the queries directory")
        .filter_map(|entry| {
            let name = entry.ok()?.file_name().to_string_lossy().into_owned();
            name.strip_suffix(".scm").map(str::to_owned)
        })
        .collect();
    present.sort();
    let mut expected: Vec<String> = QUERIES.iter().map(|name| (*name).to_owned()).collect();
    expected.sort();
    assert_eq!(present, expected);
}

/// No query file is a placeholder any more.
///
/// Each one was a single comment line before sub-plan H; a file that reverts
/// to one would still compile, and would still capture nothing.
#[test]
fn no_query_file_is_a_placeholder() {
    for name in QUERIES {
        let source = query_source(name);
        let patterns = source
            .lines()
            .filter(|line| {
                let line = line.trim();
                !line.is_empty() && !line.starts_with(';')
            })
            .count();
        if *name == "injections" {
            // The one file that may legitimately hold no pattern — and it
            // says so in a comment rather than by being empty.
            assert!(
                source.contains("embeds no other language"),
                "injections.scm has no pattern and does not say why"
            );
            continue;
        }
        assert!(patterns > 3, "{name}.scm holds only {patterns} lines");
    }
}

/// The capture names `highlights.scm` uses are the closed set the server's
/// semantic-token legend maps.
///
/// Two formats, one description: a capture the legend does not map would be
/// highlighted in Helix and invisible in VS Code, which is the drift
/// decision 6 exists to prevent.
#[test]
fn every_highlight_capture_is_in_the_token_legend() {
    let query = compiled("highlights");
    for capture in query.capture_names() {
        assert!(
            k816::lsp::tokens::LEGEND
                .iter()
                .any(|(name, _)| *name == capture),
            "`@{capture}` is captured by highlights.scm and is not in the token legend"
        );
    }
    // And the legend has nothing the query never produces: an entry nothing
    // captures is a token type the server can never emit.
    let produced = query.capture_names();
    for (name, _) in k816::lsp::tokens::LEGEND {
        assert!(
            produced.iter().any(|capture| capture == name),
            "the legend maps `@{name}` and highlights.scm never captures it"
        );
    }
}

/// `tags.scm` captures exactly the unit-level definitions the symbol table
/// has.
///
/// The claim a code index rests on, checked against the compiler rather than
/// against a list: a declaration form added to the language and not added to
/// the query is a definition no index would find.
#[test]
fn tags_capture_the_unit_level_definitions_the_symbol_table_has() {
    use corpus_files::{corpus, root};
    use k816::{Options, analyze_workspace};

    let mut checked = 0;
    for set in corpus() {
        for entry in &set.files {
            let source = std::fs::read_to_string(entry).expect("read a corpus source");
            let mut map = SourceMap::new();
            let file = map.add_file(
                entry
                    .strip_prefix(root())
                    .unwrap_or(entry)
                    .to_string_lossy(),
                source.clone(),
            );
            let analysis = analyze_workspace(&map, &Options::default());
            let Some(unit) = analysis.unit(file) else {
                continue;
            };
            // A program the grammar could not read has no tags to compare.
            if !unit.parse.diagnostics.is_empty() {
                continue;
            }

            let parse = parse_source(map.file(file), None);
            let tagged: std::collections::BTreeSet<String> = compiled("tags")
                .captures(&parse.tree, file, &source)
                .into_iter()
                .filter(|capture| capture.name == "name")
                .map(|capture| map.span_text(capture.span).to_owned())
                .collect();

            // Every unit-level declaration the model made, minus the ones a
            // *directive* removed — the query reads the parsed text and the
            // model reads the preprocessed program.
            let declared: std::collections::BTreeSet<String> = unit
                .model
                .symbols
                .iter()
                .filter(|(_, symbol)| {
                    symbol.is_unit_level()
                        && !symbol.imported
                        && symbol.declaration.file == file
                        // A name the *evaluator* created — `[ FACTOR = 3 ]`
                        // publishes one as a compiler constant — is declared
                        // at the whole bracket rather than at a name a query
                        // could match. There is no declaration syntax to tag.
                        && map.span_text(symbol.declaration) == symbol.name
                })
                .map(|(_, symbol)| symbol.name.clone())
                .collect();

            let missed: Vec<&String> = declared.difference(&tagged).collect();
            assert!(
                missed.is_empty(),
                "{}: the symbol table declares {missed:?} and tags.scm captures none of them",
                entry.display()
            );
            checked += 1;
        }
    }
    assert!(checked > 50, "only {checked} programs were compared");
}

/// `locals.scm` scopes what `Symbol.scope` scopes.
///
/// K816 has two scopes and no more — the unit, and a code section — so the
/// query's `@local.scope` captures are the file and its sections, and nothing
/// finer.
#[test]
fn locals_scope_the_sections_the_symbol_table_scopes() {
    const SOURCE: &str = "var SCREEN = $0400\n\
        func first @a8 @i8 {\n\
        .spin:\n\
        \x20   nop\n\
        \x20   != goto .spin\n\
        }\n\
        func second @a8 @i8 {\n\
        .spin:\n\
        \x20   nop\n\
        \x20   != goto .spin\n\
        }\n";

    let found = captures("locals", SOURCE);
    let scopes: Vec<&Capture> = found
        .iter()
        .filter(|capture| capture.name == "local.scope")
        .collect();
    // The file, and one per section.
    assert_eq!(scopes.len(), 3, "{scopes:?}");
    assert_eq!(scopes[0].span.start, 0, "the file is the outermost scope");

    // Each `.spin` definition is inside a different section's scope, which is
    // what makes an editor resolve one to its own function.
    let definitions: Vec<&Capture> = found
        .iter()
        .filter(|capture| capture.name.starts_with("local.definition"))
        .collect();
    let labels: Vec<&Capture> = definitions
        .iter()
        .copied()
        .filter(|capture| {
            &SOURCE[capture.span.start as usize..capture.span.end as usize] == ".spin"
        })
        .collect();
    assert_eq!(labels.len(), 2);
    assert_ne!(labels[0].span, labels[1].span);
    for label in &labels {
        let owner = scopes[1..]
            .iter()
            .find(|scope| scope.span.start <= label.span.start && label.span.end <= scope.span.end);
        assert!(owner.is_some(), "a local label outside every section");
    }
}

/// Snapshots: what `highlights.scm` and `locals.scm` capture over three
/// programs, as counts per capture name.
///
/// Counts rather than spans, because a span table over a whole corpus program
/// is a wall nobody reads and a count changes for every reason a table would.
#[test]
fn the_capture_snapshots_hold() {
    let slice = include_str!("golden/slice.k816");
    let hello = std::fs::read_to_string(
        Path::new(env!("CARGO_MANIFEST_DIR")).join("examples/hello_uart.k65"),
    )
    .expect("read the stress-test fixture");
    let whole = std::fs::read_to_string(
        Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/compat/k65/whole-program/main.k65"),
    )
    .expect("read the whole-program fixture");

    for (label, source, query, expected) in [
        (
            "slice / highlights",
            slice,
            "highlights",
            &[
                // `main`, and `var`, and `func`, and `return`.
                ("function", 1usize),
                ("keyword", 1),
                ("keyword.function", 1),
                ("keyword.return", 1),
                // `$0400`, `#0`, `#1`.
                ("number", 3),
                ("operator", 4),
                ("punctuation", 2),
                // The two `#` markers.
                ("punctuation.special", 2),
                // `@a8`.
                ("type", 1),
                // `SCREEN` declared, `SCREEN` used, and `main` twice — once
                // as an identifier and once refined to a function.
                ("variable", 4),
                // The three `a`s.
                ("variable.builtin", 3),
            ][..],
        ),
        (
            "slice / locals",
            slice,
            "locals",
            &[
                // The file, and `main`.
                ("local.scope", 2),
                ("local.definition.var", 1),
                ("local.definition.function", 1),
                // `SCREEN` twice and `main` once.
                ("local.reference", 3),
            ][..],
        ),
    ] {
        let found = captures(query, source);
        for (name, count) in expected {
            let actual = found.iter().filter(|capture| capture.name == *name).count();
            assert_eq!(actual, *count, "{label}: `@{name}`");
        }
    }

    // The two larger programs are checked for shape rather than for exact
    // counts: what matters is that every construct they hold is captured by
    // something, and that nothing is captured twice under one name.
    for (label, source) in [("hello_uart", &hello), ("whole-program", &whole)] {
        let found = captures("highlights", source);
        assert!(found.len() > 80, "{label}: only {} captures", found.len());
        let comments = found.iter().filter(|c| c.name == "comment").count();
        let names = found.iter().filter(|c| c.name == "variable").count();
        assert!(comments > 0, "{label}: no comment captured");
        assert!(names > 0, "{label}: no name captured");
        // `hello_uart.k65` writes no native mnemonic at all — it is HLA
        // throughout — so "some mnemonic is captured" is not a property of
        // every corpus program. What is: every name the query produces is one
        // the legend maps.
        for capture in &found {
            assert!(
                k816::lsp::tokens::LEGEND
                    .iter()
                    .any(|(name, _)| *name == capture.name),
                "{label}: `@{}` is captured and not in the legend",
                capture.name
            );
        }
        // Every capture is inside the file, and none is empty.
        for capture in &found {
            assert!(capture.span.end as usize <= source.len(), "{label}");
            assert!(
                capture.span.start < capture.span.end,
                "{label}: {capture:?}"
            );
        }
    }
}

/// Nothing under `tree-sitter-k65/` references the compiler.
///
/// The phase-11 criterion: the grammar and its query artifacts have to work
/// in an editor that has never seen `src/`, so a query that reached for a
/// K816 semantic fact would be one no editor could run.
#[test]
fn the_grammar_directory_references_no_compiler_module() {
    let dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("tree-sitter-k65");
    let mut checked = 0;
    let mut stack = vec![dir];
    while let Some(path) = stack.pop() {
        for entry in std::fs::read_dir(&path).expect("read the grammar directory") {
            let entry = entry.expect("a directory entry").path();
            if entry.is_dir() {
                stack.push(entry);
                continue;
            }
            let Some(extension) = entry.extension() else {
                continue;
            };
            if extension != "scm" && extension != "js" && extension != "json" {
                continue;
            }
            let text = std::fs::read_to_string(&entry).unwrap_or_default();
            for line in text.lines() {
                // The grammar's *own* `src/` is where its generated parser
                // lives; what must not appear is a reference to the
                // compiler — a Rust path, or a file of the crate around it.
                for forbidden in ["k816::", "crate::", "../src/", "src/semantic", "src/lsp"] {
                    assert!(
                        !line.contains(forbidden),
                        "{}: references the compiler (`{forbidden}`): {line}",
                        entry.display()
                    );
                }
            }
            checked += 1;
        }
    }
    assert!(checked > 5, "only {checked} files were read");
}

/// `tags.scm` captures an `extern` declaration's name.
///
/// Explicit, because the corpus gate above cannot reach it: that test filters
/// the symbol table to `!symbol.imported`, and an `extern` declares an
/// imported symbol by construction. So the one form whose whole purpose is to
/// tell a reader where a name comes from would be the one form a code index
/// could not find, and nothing would have said so.
#[test]
fn extern_declarations_are_tagged_as_definitions() {
    let source = "extern func _puts\n\
                  extern far func rom_entry\n\
                  extern var dp ptr\n\
                  extern const ROWS\n\
                  import \"lib.k816\"\n";
    let mut map = SourceMap::new();
    let file = map.add_file("unit.k816", source);
    let parse = parse_source(map.file(file), None);
    assert_eq!(parse.diagnostics, Vec::new());

    let tagged: Vec<(String, String)> = compiled("tags")
        .captures(&parse.tree, file, source)
        .into_iter()
        .filter(|capture| capture.name.starts_with("definition."))
        .map(|capture| (capture.name.clone(), map.span_text(capture.span).to_owned()))
        .collect();

    // Each extern is tagged under the kind it declares, and the `import` is
    // tagged not at all: a path is not a definition, and an index that
    // carried one would be listing files as symbols.
    // The span is the declaration node's, which starts after `extern`
    // because that keyword belongs to the wrapper. The order is the query's,
    // not the source's.
    assert_eq!(
        tagged,
        vec![
            ("definition.constant".to_owned(), "const ROWS".to_owned()),
            ("definition.function".to_owned(), "func _puts".to_owned()),
            (
                "definition.function".to_owned(),
                "far func rom_entry".to_owned()
            ),
            ("definition.variable".to_owned(), "var dp ptr".to_owned()),
        ]
    );

    // And the names themselves, which is what an index stores.
    let names: Vec<String> = compiled("tags")
        .captures(&parse.tree, file, source)
        .into_iter()
        .filter(|capture| capture.name == "name")
        .map(|capture| map.span_text(capture.span).to_owned())
        .collect();
    assert_eq!(names, vec!["_puts", "rom_entry", "ptr", "ROWS"]);
}
