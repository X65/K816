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

#[path = "common/fingerprint.rs"]
mod fingerprint;

use fingerprint::fingerprint;

fn root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

fn extension_dir() -> PathBuf {
    root().join("editors/zed-k816")
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
    assert_eq!(checked, 4, "generated.lock lists four artifacts");
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
