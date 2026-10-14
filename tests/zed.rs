/*****************************************************************************/
/*                                                                           */
/*                                   zed.rs                                  */
/*                                                                           */
/*    The Zed extension, held to the grammar, the formatter and the server   */
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

//! `editors/zed-k816/` against the files it is a consumer of (sub-plan L).
//!
//! An editor extension is a second place several facts are written down: the
//! grammar it compiles, the queries it reads, the file types it claims, the
//! indent width it types. Every gate here holds one of those to the file that
//! states it first — `generated.lock`, `tree-sitter-k65/queries/`,
//! `tree-sitter.json`, `format::INDENT` — so the extension cannot drift from
//! the build while still looking right in the editor.
//!
//! The two manifests are TOML, and this file reads them with a scanner of its
//! own rather than a new dependency. It understands exactly what the
//! manifests are written in — sections, strings, integers, booleans, arrays —
//! keeps an inline table as opaque text, and refuses anything else, so a
//! manifest edited past what it reads fails loudly rather than being misread.

use std::collections::{BTreeMap, BTreeSet};
use std::path::PathBuf;
use std::process::Command;

use k816::parser::{Capture, Query, format, parse_source};
use k816::source::SourceMap;

#[path = "common/corpus.rs"]
mod corpus_files;

#[path = "common/fingerprint.rs"]
mod fingerprint;

use fingerprint::fingerprint;

fn root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

fn extension_dir() -> PathBuf {
    root().join("editors/zed-k816")
}

/// Where Zed reads the language's configuration and queries from.
fn language_dir() -> PathBuf {
    extension_dir().join("languages/K816")
}

/// The grammar's own queries, which the extension copies or derives from.
fn grammar_queries() -> PathBuf {
    root().join("tree-sitter-k65/queries")
}

fn read(path: PathBuf) -> String {
    std::fs::read_to_string(&path)
        .unwrap_or_else(|error| panic!("read {}: {error}", path.display()))
}

// ---------------------------------------------------------------------------
// The TOML the manifests are written in
// ---------------------------------------------------------------------------

/// A value, as far as the scanner reads one.
#[derive(Clone, Debug, PartialEq)]
enum Value {
    Str(String),
    Int(i64),
    Bool(bool),
    Array(Vec<Value>),
    /// An inline table, kept as its source text.
    Table(String),
}

impl Value {
    fn str(&self) -> &str {
        match self {
            Value::Str(text) => text,
            other => panic!("expected a string, found {other:?}"),
        }
    }

    fn strings(&self) -> Vec<&str> {
        match self {
            Value::Array(items) => items.iter().map(Value::str).collect(),
            other => panic!("expected an array of strings, found {other:?}"),
        }
    }
}

/// A scanned file: every key as `section.key`, and every section header seen
/// — a section may hold no keys, and `[workspace]` is exactly that.
struct Toml {
    keys: BTreeMap<String, Value>,
    sections: BTreeSet<String>,
}

impl Toml {
    fn get(&self, key: &str) -> &Value {
        self.keys
            .get(key)
            .unwrap_or_else(|| panic!("no `{key}`; the file has {:?}", self.keys.keys()))
    }
}

struct Scanner<'a> {
    text: &'a [u8],
    at: usize,
}

impl Scanner<'_> {
    fn peek(&self) -> Option<u8> {
        self.text.get(self.at).copied()
    }

    /// Whitespace, newlines and comments — everywhere a value may not start.
    fn skip(&mut self) {
        while let Some(byte) = self.peek() {
            match byte {
                b' ' | b'\t' | b'\r' | b'\n' => self.at += 1,
                b'#' => {
                    while self.peek().is_some_and(|byte| byte != b'\n') {
                        self.at += 1;
                    }
                }
                _ => break,
            }
        }
    }

    fn word(&mut self) -> String {
        let start = self.at;
        while self
            .peek()
            .is_some_and(|byte| byte.is_ascii_alphanumeric() || b"_-.".contains(&byte))
        {
            self.at += 1;
        }
        String::from_utf8_lossy(&self.text[start..self.at]).into_owned()
    }

    fn expect(&mut self, byte: u8) {
        self.skip();
        assert_eq!(
            self.peek(),
            Some(byte),
            "expected `{}` at byte {}",
            byte as char,
            self.at
        );
        self.at += 1;
    }

    fn string(&mut self) -> String {
        self.expect(b'"');
        let mut out = String::new();
        loop {
            match self.peek().expect("an unterminated string") {
                b'"' => break,
                b'\\' => {
                    self.at += 1;
                    match self.peek() {
                        Some(b'"') => out.push('"'),
                        Some(b'\\') => out.push('\\'),
                        other => panic!("the scanner does not read the escape {other:?}"),
                    }
                }
                byte => out.push(byte as char),
            }
            self.at += 1;
        }
        self.at += 1;
        out
    }

    fn value(&mut self) -> Value {
        self.skip();
        match self.peek() {
            Some(b'"') => Value::Str(self.string()),
            Some(b'[') => {
                self.at += 1;
                let mut items = Vec::new();
                loop {
                    self.skip();
                    if self.peek() == Some(b']') {
                        self.at += 1;
                        break;
                    }
                    items.push(self.value());
                    self.skip();
                    if self.peek() == Some(b',') {
                        self.at += 1;
                    }
                }
                Value::Array(items)
            }
            Some(b'{') => {
                let start = self.at;
                while self.peek() != Some(b'}') {
                    if self.peek() == Some(b'"') {
                        self.string();
                    } else {
                        self.at += 1;
                    }
                }
                self.at += 1;
                Value::Table(String::from_utf8_lossy(&self.text[start..self.at]).into_owned())
            }
            _ => match self.word().as_str() {
                "true" => Value::Bool(true),
                "false" => Value::Bool(false),
                word => Value::Int(
                    word.parse()
                        .unwrap_or_else(|_| panic!("the scanner does not read the value `{word}`")),
                ),
            },
        }
    }
}

fn scan_toml(text: &str) -> Toml {
    let mut scanner = Scanner {
        text: text.as_bytes(),
        at: 0,
    };
    let mut toml = Toml {
        keys: BTreeMap::new(),
        sections: BTreeSet::new(),
    };
    let mut section = String::new();
    loop {
        scanner.skip();
        match scanner.peek() {
            None => return toml,
            Some(b'[') => {
                scanner.at += 1;
                assert_ne!(scanner.peek(), Some(b'['), "arrays of tables are not read");
                section = scanner.word();
                scanner.expect(b']');
                toml.sections.insert(section.clone());
            }
            Some(_) => {
                let key = scanner.word();
                assert!(!key.is_empty(), "a key at byte {}", scanner.at);
                scanner.expect(b'=');
                let value = scanner.value();
                let key = if section.is_empty() {
                    key
                } else {
                    format!("{section}.{key}")
                };
                toml.keys.insert(key, value);
            }
        }
    }
}

fn extension_manifest() -> Toml {
    scan_toml(&read(extension_dir().join("extension.toml")))
}

fn grammar_manifest() -> serde_json::Value {
    serde_json::from_str(&read(root().join("tree-sitter-k65/tree-sitter.json")))
        .expect("tree-sitter.json is JSON")
}

// ---------------------------------------------------------------------------
// The grammar Zed compiles
// ---------------------------------------------------------------------------

/// What Zed needs to find the grammar, and what it must call it.
///
/// The key under `[grammars.*]` is not a label: Zed links the symbol
/// `tree_sitter_<key>`, so it has to be the grammar's own name.
#[test]
fn the_extension_manifest_pins_this_grammar() {
    let manifest = extension_manifest();
    let grammar = grammar_manifest();

    assert_eq!(manifest.get("id").str(), "k816");
    assert_eq!(manifest.get("schema_version"), &Value::Int(1));

    let name = grammar["grammars"][0]["name"].as_str().expect("a name");
    let key = |field: &str| format!("grammars.{name}.{field}");
    assert_eq!(
        manifest.get(&key("repository")).str(),
        grammar["metadata"]["links"]["repository"],
        "the extension clones the grammar from where tree-sitter.json says it lives"
    );
    assert_eq!(manifest.get(&key("path")).str(), "tree-sitter-k65");
    let rev = manifest.get(&key("rev")).str();
    assert!(
        rev.len() == 40
            && rev
                .bytes()
                .all(|byte| matches!(byte, b'0'..=b'9' | b'a'..=b'f')),
        "`rev` is a full commit hash, not a branch: `{rev}`"
    );

    assert_eq!(
        manifest.get("language_servers.k816.languages").strings(),
        ["K816"]
    );
}

/// The parser Zed compiles is the parser the server runs.
///
/// `tests/corpus.rs` holds `generated.lock` to the files on disk; this holds
/// the commit the extension pins to the same lock. Between them, a grammar
/// regenerated without moving the pin fails here — and since a commit cannot
/// name its own hash, a regeneration is two commits: the regeneration, red on
/// this gate, and the pin bump that turns it green.
#[test]
fn the_pinned_grammar_is_the_committed_grammar() {
    let manifest = extension_manifest();
    let rev = manifest.get("grammars.k65.rev").str();
    let lock = read(root().join("tree-sitter-k65/generated.lock"));

    let mut checked = 0;
    for line in lock.lines().filter(|line| !line.starts_with('#')) {
        let Some((name, hash)) = line.split_once(" = ") else {
            continue;
        };
        if name == "tree-sitter-cli" {
            continue;
        }
        let output = Command::new("git")
            .arg("-C")
            .arg(root())
            .arg("show")
            .arg(format!("{rev}:./tree-sitter-k65/{name}"))
            .output()
            .expect("run git");
        if !output.status.success() {
            eprintln!(
                "skipped: git cannot show {rev}:tree-sitter-k65/{name} ({}). \
                 A shallow clone, or a commit this checkout does not have.",
                String::from_utf8_lossy(&output.stderr).trim()
            );
            return;
        }
        assert_eq!(
            fingerprint(&output.stdout),
            hash,
            "editors/zed-k816/extension.toml pins {rev}, whose tree-sitter-k65/{name} is not \
             the one generated.lock describes. After regenerating the grammar, commit, then \
             set `rev` to that commit."
        );
        checked += 1;
    }
    assert_eq!(checked, 5, "generated.lock lists five artifacts");
}

/// The extension's crate is its own workspace root, so it is never built by
/// the compiler's `cargo test`, and it depends on exactly the API version its
/// lock resolved.
#[test]
fn the_extension_crate_is_its_own_workspace() {
    let cargo = scan_toml(&read(extension_dir().join("Cargo.toml")));
    assert!(
        cargo.sections.contains("workspace"),
        "editors/zed-k816/Cargo.toml needs its own `[workspace]`"
    );
    assert_eq!(cargo.get("lib.crate-type").strings(), ["cdylib"]);

    let requirement = cargo.get("dependencies.zed_extension_api").str();
    let pinned = requirement
        .strip_prefix('=')
        .unwrap_or_else(|| panic!("zed_extension_api is pinned exactly, not `{requirement}`"));

    let lock = read(extension_dir().join("Cargo.lock"));
    let locked = lock
        .split_once("name = \"zed_extension_api\"\nversion = \"")
        .and_then(|(_, rest)| rest.split_once('"'))
        .map(|(version, _)| version)
        .expect("Cargo.lock resolves zed_extension_api");
    assert_eq!(pinned, locked);
}

// ---------------------------------------------------------------------------
// The language
// ---------------------------------------------------------------------------

/// The query files Zed reads from `languages/K816/`, and so the whole set: a
/// file not listed is one no gate holds to anything.
const ZED_QUERIES: &[&str] = &["highlights", "injections", "brackets", "indents", "outline"];

/// The queries Zed reads unchanged. Zed reads queries only from the
/// extension's own directory, so these are copies — and a copy is a second
/// source of truth unless something compares it with the first.
const COPIED: &[&str] = &["highlights", "injections"];

#[test]
fn the_copied_queries_are_the_grammars_own() {
    for name in COPIED {
        let original = grammar_queries().join(format!("{name}.scm"));
        let copy = language_dir().join(format!("{name}.scm"));
        assert!(
            std::fs::read(&original).expect("read the grammar's query")
                == std::fs::read(&copy).expect("read the extension's copy"),
            "the extension's {name}.scm differs from the grammar's. Edit the grammar's file — \
             the server derives its semantic tokens from it too — and copy it:\n    \
             cp tree-sitter-k65/queries/{name}.scm editors/zed-k816/languages/K816/"
        );
    }
}

/// Each fact the language configuration repeats, held to where it is stated
/// first.
#[test]
fn the_language_config_states_what_the_grammar_and_the_formatter_state() {
    let config = scan_toml(&read(language_dir().join("config.toml")));
    let manifest = extension_manifest();
    let grammar = &grammar_manifest()["grammars"][0];

    assert_eq!(
        config.get("name").str(),
        manifest.get("language_servers.k816.languages").strings()[0],
        "the server is attached to the language by this name"
    );
    assert_eq!(config.get("grammar").str(), grammar["name"]);
    assert!(
        manifest
            .keys
            .contains_key(&format!("grammars.{}.rev", config.get("grammar").str())),
        "the language names a grammar the extension declares"
    );

    let file_types: Vec<&str> = grammar["file-types"]
        .as_array()
        .expect("file-types")
        .iter()
        .map(|suffix| suffix.as_str().expect("a suffix"))
        .collect();
    let mut suffixes = config.get("path_suffixes").strings();
    suffixes.sort_unstable();
    assert_eq!(suffixes, file_types);

    assert_eq!(
        config.get("tab_size"),
        &Value::Int(i64::try_from(format::INDENT).expect("a small indent")),
        "Zed indents by the width `k816 fmt` writes"
    );
    assert_eq!(config.get("hard_tabs"), &Value::Bool(false));
    assert_eq!(config.get("line_comments").strings(), ["// "]);
}

/// Every query file Zed reads compiles against the committed grammar, and
/// each uses only the captures its reader understands.
#[test]
fn every_zed_query_compiles() {
    let mut present: Vec<String> = std::fs::read_dir(language_dir())
        .expect("read languages/K816")
        .filter_map(|entry| {
            let name = entry.ok()?.file_name().to_string_lossy().into_owned();
            name.strip_suffix(".scm").map(str::to_owned)
        })
        .collect();
    present.sort();
    let mut expected: Vec<String> = ZED_QUERIES.iter().map(|name| (*name).to_owned()).collect();
    expected.sort();
    assert_eq!(
        present, expected,
        "languages/K816 holds exactly these queries"
    );

    let names = |dir: PathBuf, name: &str| -> BTreeSet<String> {
        let source = read(dir.join(format!("{name}.scm")));
        Query::compile(&source)
            .unwrap_or_else(|error| panic!("{name}.scm does not compile: {error}"))
            .capture_names()
            .into_iter()
            .collect()
    };
    for name in ZED_QUERIES {
        names(language_dir(), name);
    }

    assert_eq!(
        names(language_dir(), "brackets"),
        BTreeSet::from(["open".to_owned(), "close".to_owned()])
    );
    assert_eq!(
        names(language_dir(), "highlights"),
        names(grammar_queries(), "highlights")
    );
    assert!(names(language_dir(), "injections").is_empty());
    assert_eq!(
        names(language_dir(), "indents"),
        BTreeSet::from(["indent".to_owned(), "end".to_owned()])
    );
    assert_eq!(
        names(language_dir(), "outline"),
        BTreeSet::from(["item".to_owned(), "name".to_owned(), "context".to_owned()])
    );
}

/// Every source in the corpus, once each, in a stable order.
fn corpus_sources() -> Vec<(String, String)> {
    let mut seen = BTreeSet::new();
    for set in corpus_files::corpus() {
        seen.extend(set.files);
    }
    assert!(seen.len() > 100, "the corpus has {} sources", seen.len());
    seen.into_iter()
        .map(|path| (path.to_string_lossy().into_owned(), read(path)))
        .collect()
}

/// Every capture `query` yields over `sources`.
///
/// Spans carry the file they are in, so one sorted list over every source
/// compares as exactly as a list per file. Both queries of a comparison are
/// run over one `SourceMap`, so their file ids agree.
fn captures_over(query: &str, sources: &[(String, String)]) -> Vec<Capture> {
    let query = Query::compile(query).expect("the query compiles");
    let mut map = SourceMap::new();
    let mut found = Vec::new();
    for (name, source) in sources {
        let file = map.add_file(name.as_str(), source.clone());
        let parse = parse_source(map.file(file), None);
        found.extend(query.captures(&parse.tree, file, source));
    }
    found
}

fn corpus_captures(query: &str) -> Vec<Capture> {
    captures_over(query, &corpus_sources())
}

/// The spans of every capture named `name`, sorted.
fn spans_named(captures: &[Capture], name: &str) -> Vec<k816::source::Span> {
    let mut spans: Vec<_> = captures
        .iter()
        .filter(|capture| capture.name == name)
        .map(|capture| capture.span)
        .collect();
    spans.sort();
    spans.dedup();
    spans
}

// ---------------------------------------------------------------------------
// Indentation
// ---------------------------------------------------------------------------

/// The constructs Zed indents are the formatter's.
///
/// The same claim `tests/format.rs` makes of the grammar's query, read the
/// same way — the construct named first in each `@indent` pattern — so a
/// construct added to `format::INDENTING` and not here fails, as does one
/// added here and not there.
#[test]
fn the_zed_indent_query_names_the_formatters_constructs() {
    let query = read(language_dir().join("indents.scm"));
    let marked: BTreeSet<String> = query
        .lines()
        .filter(|line| line.contains("@indent") && !line.trim_start().starts_with(';'))
        .filter_map(|line| {
            let rest = line.trim().strip_prefix('(')?;
            let end = rest.find(|c: char| c.is_whitespace() || c == ')')?;
            Some(rest[..end].to_owned())
        })
        .collect();
    let indenting: BTreeSet<String> = format::INDENTING
        .iter()
        .map(|node| (*node).to_owned())
        .collect();
    assert_eq!(marked, indenting);
}

/// Over the whole corpus, Zed's query indents exactly what the grammar's
/// does, and closes each range at that construct's own last delimiter.
///
/// The first half catches a pattern that is right by name and wrong by shape
/// — `(code_subblock "}" @end)` names the construct and matches nothing,
/// because the brace is the body's. The second catches an `@end` on the wrong
/// delimiter, which would leave the closing line indented.
#[test]
fn the_zed_indent_query_indents_what_the_grammars_does() {
    let zed = corpus_captures(&read(language_dir().join("indents.scm")));
    let grammar = corpus_captures(&read(grammar_queries().join("indents.scm")));

    let indents = spans_named(&zed, "indent");
    assert_eq!(
        indents,
        spans_named(&grammar, "indent"),
        "Zed's indents.scm indents different spans from the grammar's"
    );
    assert!(indents.len() > 500, "only {} indented spans", indents.len());

    // Pattern by pattern, so a construct cannot borrow the `@end` of the
    // body it wraps: `(nocross_data_group) @indent` ends where its
    // `data_body`'s `@end` does, and a check by position alone passes it.
    let patterns: BTreeSet<usize> = zed.iter().map(|capture| capture.pattern).collect();
    for pattern in patterns {
        let closed = |name: &str| -> BTreeSet<_> {
            zed.iter()
                .filter(|capture| capture.pattern == pattern && capture.name == name)
                .map(|capture| (capture.span.file, capture.span.end))
                .collect()
        };
        assert_eq!(
            closed("indent"),
            closed("end"),
            "pattern {pattern} of Zed's indents.scm does not close each range it opens at \
             that construct's own last delimiter"
        );
    }
}

// ---------------------------------------------------------------------------
// The outline
// ---------------------------------------------------------------------------

/// Every form the outline lists, written once — including the two spellings
/// of a function, the keyworded `func main` that must not be listed twice,
/// and each `extern`.
const EVERY_DEFINITION: &str = "\
const ROWS = 24, COLS
var RESULT :word, dp ptr
data table { 1 2 3 }
data { entry: 4 }
binary blob = \"blob.bin\"
evalfunc twice(n) [ n * 2 ]
extern func _puts
extern far func rom_entry
extern var dp counter
extern const LIMIT
far func helper {
loop:
    rts
}
naked isr { rti }
main { }
func main { }
";

/// Zed's outline lists the definitions `tags.scm` lists — the same names, at
/// the same items — over the corpus and over one source holding every form.
#[test]
fn the_outline_lists_the_definitions_the_tags_query_lists() {
    let mut sources = corpus_sources();
    sources.push((
        "every-definition.k816".to_owned(),
        EVERY_DEFINITION.to_owned(),
    ));

    // A form the parser recovered from is not the form it claims to be.
    let mut map = SourceMap::new();
    let file = map.add_file("every-definition.k816", EVERY_DEFINITION);
    assert_eq!(parse_source(map.file(file), None).diagnostics, Vec::new());

    let outline_query = read(language_dir().join("outline.scm"));
    let outline = captures_over(&outline_query, &sources);
    let tags = captures_over(&read(grammar_queries().join("tags.scm")), &sources);

    // `tags.scm` also captures call sites; its definitions are the patterns
    // that capture a `definition.*`, and `@name` is shared between the two.
    let defining: BTreeSet<usize> = tags
        .iter()
        .filter(|capture| capture.name.starts_with("definition."))
        .map(|capture| capture.pattern)
        .collect();
    let definitions: Vec<Capture> = tags
        .into_iter()
        .filter(|capture| defining.contains(&capture.pattern))
        .collect();

    let mut tagged = spans_named(&definitions, "definition.function");
    tagged.extend(spans_named(&definitions, "definition.variable"));
    tagged.extend(spans_named(&definitions, "definition.constant"));
    tagged.sort();
    tagged.dedup();

    let items = spans_named(&outline, "item");
    assert_eq!(
        items, tagged,
        "the outline lists different items from tags.scm"
    );
    assert_eq!(
        spans_named(&outline, "name"),
        spans_named(&definitions, "name"),
        "the outline names different things from tags.scm"
    );

    // One match per item: two patterns matching one declaration would list
    // it twice, and the deduplication above would hide that.
    let mut matched: BTreeMap<k816::source::Span, BTreeSet<usize>> = BTreeMap::new();
    for capture in outline.iter().filter(|capture| capture.name == "item") {
        matched
            .entry(capture.span)
            .or_default()
            .insert(capture.pattern);
    }
    for (span, patterns) in &matched {
        assert_eq!(
            patterns.len(),
            1,
            "{span:?} is listed by patterns {patterns:?}"
        );
    }

    // And every pattern lists something, so none of them is untested.
    let patterns = top_level_patterns(&outline_query);
    let used: BTreeSet<usize> = matched.values().flatten().copied().collect();
    assert_eq!(
        used,
        (0..patterns).collect::<BTreeSet<_>>(),
        "an outline pattern matched nothing"
    );
}

/// What Zed prints for each item — its `@context` and `@name` in source order
/// — for the source holding every form.
#[test]
fn the_outline_says_what_each_definition_is() {
    let outline = captures_over(
        &read(language_dir().join("outline.scm")),
        &[(
            "every-definition.k816".to_owned(),
            EVERY_DEFINITION.to_owned(),
        )],
    );
    let text = |span: k816::source::Span| &EVERY_DEFINITION[span.start as usize..span.end as usize];
    let items: Vec<&Capture> = outline
        .iter()
        .filter(|capture| capture.name == "item")
        .collect();
    let labels: Vec<String> = items
        .iter()
        .map(|item| {
            let inside = |span: k816::source::Span| {
                span.start >= item.span.start && span.end <= item.span.end
            };
            // A part of a nested item belongs to that item, not this one.
            let nested = |span: k816::source::Span| {
                items.iter().any(|other| {
                    other.span != item.span
                        && inside(other.span)
                        && span.start >= other.span.start
                        && span.end <= other.span.end
                })
            };
            outline
                .iter()
                .filter(|part| part.pattern == item.pattern)
                .filter(|part| part.name == "context" || part.name == "name")
                .filter(|part| inside(part.span) && !nested(part.span))
                .map(|part| text(part.span))
                .collect::<Vec<_>>()
                .join(" ")
        })
        .collect();
    assert_eq!(
        labels,
        [
            "ROWS",
            "COLS",
            "RESULT",
            "ptr",
            "data table",
            "entry",
            "binary blob",
            "evalfunc twice",
            "func _puts",
            "far func rom_entry",
            "var counter",
            "const LIMIT",
            "far func helper",
            "loop",
            "naked isr",
            "main",
            "func main",
        ]
    );
}

/// How many patterns a query file holds: its top-level parenthesized forms,
/// outside comments and strings. Enough for a file written as one form per
/// pattern, which is every file here — none uses a top-level `[ … ]`.
fn top_level_patterns(query: &str) -> usize {
    let mut depth = 0usize;
    let mut patterns = 0;
    let mut chars = query.chars();
    while let Some(c) = chars.next() {
        match c {
            ';' => {
                for c in chars.by_ref() {
                    if c == '\n' {
                        break;
                    }
                }
            }
            '"' => {
                while let Some(c) = chars.next() {
                    match c {
                        '\\' => {
                            chars.next();
                        }
                        '"' => break,
                        _ => {}
                    }
                }
            }
            '(' => {
                if depth == 0 {
                    patterns += 1;
                }
                depth += 1;
            }
            ')' => depth -= 1,
            '[' if depth == 0 => panic!("a top-level alternation is not counted"),
            _ => {}
        }
    }
    patterns
}
