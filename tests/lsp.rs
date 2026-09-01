/*****************************************************************************/
/*                                                                           */
/*                                   lsp.rs                                  */
/*                                                                           */
/*          CLI and LSP return identical diagnostics for the slice           */
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

//! Master plan Phase 11 (minimal): the LSP path and the CLI path go through
//! the same frontend and therefore produce identical diagnostics.

use lsp_types::{DiagnosticSeverity, NumberOrString, Position};

use k816::abi::EntryEnvironment;
use k816::lsp::config;
use k816::lsp::{diagnostic_to_lsp, file_diagnostics, span_to_range};
use k816::source::{SourceMap, Span};
use k816::workspace::{Workspace, find_file};
use k816::{Options, compile_to_ca65};

/// The slice with its declaration removed: one syntax-clean semantic error.
// A duplicate of `golden/slice.k816`'s `main`, which is what makes it
// "broken" in a workspace that holds both — the entry contract restates the
// ABI, so nothing else here is wrong.
const BROKEN_SLICE: &str = "func main @a8 {
    a = #0
    a = #1
    SCREEN = a
    return
}
";

/// Malformed source: exercises the syntax-diagnostic path too.
const MALFORMED: &str = "func work @a8 {\n    a = =\n    return\n";

#[test]
fn cli_and_lsp_report_identical_diagnostics_for_the_slice() {
    for (name, text) in [
        ("file:///slice-broken.k816", BROKEN_SLICE),
        ("file:///malformed.k816", MALFORMED),
        ("file:///clean.k816", include_str!("golden/slice.k816")),
    ] {
        // The CLI path: compile_to_ca65 over a plain source map.
        let mut cli_map = SourceMap::new();
        let cli_file = cli_map.add_file(name, text);
        let cli_result = compile_to_ca65(&cli_map, cli_file, &Options::default());
        let cli_diagnostics: Vec<lsp_types::Diagnostic> = cli_result
            .diagnostics
            .iter()
            .map(|diagnostic| diagnostic_to_lsp(&cli_map, diagnostic))
            .collect();

        // The LSP path: a workspace document analyzed via the shared
        // frontend, published per file.
        let mut workspace = Workspace::new();
        workspace.update(name, text);
        let (lsp_map, analysis) = workspace.analyze();
        let lsp_file = find_file(&lsp_map, name).expect("document analyzed");
        let lsp_diagnostics = file_diagnostics(&lsp_map, lsp_file, &analysis.diagnostics);

        assert_eq!(cli_diagnostics, lsp_diagnostics, "diverged for {name}");
    }
}

#[test]
fn published_diagnostics_carry_code_range_severity_and_help() {
    let mut workspace = Workspace::new();
    let uri = "file:///broken.k816";
    workspace.update(uri, BROKEN_SLICE);
    let (map, analysis) = workspace.analyze();
    let file = find_file(&map, uri).expect("document analyzed");
    let diagnostics = file_diagnostics(&map, file, &analysis.diagnostics);

    assert_eq!(diagnostics.len(), 1);
    let diagnostic = &diagnostics[0];
    assert_eq!(
        diagnostic.code,
        Some(NumberOrString::String("K816-E0101".to_owned()))
    );
    assert_eq!(diagnostic.severity, Some(DiagnosticSeverity::ERROR));
    assert_eq!(diagnostic.source.as_deref(), Some("k816"));
    // `SCREEN` sits on zero-based line 3, columns 4..10.
    assert_eq!(diagnostic.range.start, Position::new(3, 4));
    assert_eq!(diagnostic.range.end, Position::new(3, 10));
    assert!(diagnostic.message.contains("`SCREEN` is not declared"));
    assert!(
        diagnostic
            .message
            .contains("help: declare it first, e.g. `var SCREEN = $0400`")
    );
}

#[test]
fn ranges_use_utf16_columns() {
    let mut map = SourceMap::new();
    // "ż" is 2 bytes in UTF-8 but 1 UTF-16 unit; "𝕏" is 4 bytes / 2 units.
    let id = map.add_file("uni.k816", "// ż𝕏!\n");
    // Span of "!": bytes 9..10 (2 for "ż" + 4 for "𝕏" after "// ").
    let range = span_to_range(&map, Span::new(id, 9, 10));
    assert_eq!(range.start, Position::new(0, 6)); // 3 + 1 + 2 UTF-16 units
    assert_eq!(range.end, Position::new(0, 7));
}

#[test]
fn closing_a_document_removes_it_from_analysis() {
    let mut workspace = Workspace::new();
    workspace.update("file:///a.k816", BROKEN_SLICE);
    workspace.update("file:///b.k816", include_str!("golden/slice.k816"));

    let (_, analysis) = workspace.analyze();
    assert_eq!(analysis.diagnostics.len(), 1);

    workspace.close("file:///a.k816");
    let (_, analysis) = workspace.analyze();
    assert_eq!(analysis.diagnostics.len(), 0);
    assert!(!workspace.is_empty());
}

// ---------------------------------------------------------------------------
// H0 — the configuration seam
// ---------------------------------------------------------------------------

/// `--strict-k65`'s own fixture: thirteen reports with the flag, none without.
const STRICT_FIXTURE: &str = include_str!("compat/k816/strict-k65/input.k65");

/// The entry-ABI claim from `tests/abi.rs`: true under the startup stub's
/// environment, `K816-E0143` under one that hands `main` a wide accumulator.
const ENTRY_CLAIM: &str = "func main @a8 {\n    a = #$01\n    return\n}\n";

/// Every option the seam carries, in the spellings a build states them in.
///
/// The matrix is the point: an identity that holds only for the defaults is
/// the identity the server already had, and it is exactly the one
/// `postmortem.md` §G.21.3 recorded as broken.
fn option_matrix() -> Vec<(&'static str, Options)> {
    let entry = |axes: &str| EntryEnvironment::parse(axes).expect("a machine that exists");
    vec![
        ("default", Options::default()),
        (
            "--strict-k65",
            Options {
                strict_k65: true,
                ..Options::default()
            },
        ),
        (
            "--entry native,a16,i8",
            Options {
                entry: entry("native,a16,i8"),
                ..Options::default()
            },
        ),
        (
            "--entry native,a8,i16",
            Options {
                entry: entry("native,a8,i16"),
                ..Options::default()
            },
        ),
        (
            "--entry emulation",
            Options {
                entry: entry("emulation"),
                ..Options::default()
            },
        ),
        (
            "--strict-k65 --entry native,a16,i16",
            Options {
                strict_k65: true,
                entry: entry("native,a16,i16"),
                ..Options::default()
            },
        ),
    ]
}

/// The codes the CLI reports for `text` under `options`.
fn cli_codes(name: &str, text: &str, options: &Options) -> Vec<String> {
    let mut map = SourceMap::new();
    let file = map.add_file(name, text);
    compile_to_ca65(&map, file, options)
        .diagnostics
        .iter()
        .map(|diagnostic| diagnostic.code.as_str().to_owned())
        .collect()
}

/// The codes the LSP publishes for `text` under `options`.
fn lsp_codes(name: &str, text: &str, options: &Options) -> Vec<String> {
    let mut workspace = Workspace::with_options(options.clone());
    workspace.update(name, text);
    let (map, analysis) = workspace.analyze();
    let file = find_file(&map, name).expect("document analyzed");
    file_diagnostics(&map, file, &analysis.diagnostics)
        .iter()
        .map(|diagnostic| match &diagnostic.code {
            Some(NumberOrString::String(code)) => code.clone(),
            other => panic!("a published diagnostic without a string code: {other:?}"),
        })
        .collect()
}

/// The identity gate, over the whole option matrix rather than the defaults.
///
/// This is what `plans/07-lsp-tooling.md` H0 is for: before it, the server
/// analyzed with `Options::default()` and a `--strict-k65` project saw none
/// of its `K816-K65-*` rows in the editor.
#[test]
fn cli_and_lsp_agree_under_every_option_the_seam_carries() {
    for (label, options) in option_matrix() {
        for (name, text) in [
            ("file:///strict.k65", STRICT_FIXTURE),
            ("file:///entry.k816", ENTRY_CLAIM),
            ("file:///clean.k816", include_str!("golden/slice.k816")),
        ] {
            assert_eq!(
                cli_codes(name, text, &options),
                lsp_codes(name, text, &options),
                "diverged for {name} under {label}"
            );
        }
    }
}

/// The two options actually change what the editor is told — otherwise the
/// gate above would hold for a server that ignored them both.
#[test]
fn the_configured_options_change_what_the_editor_is_told() {
    let strict = Options {
        strict_k65: true,
        ..Options::default()
    };
    assert!(lsp_codes("file:///s.k65", STRICT_FIXTURE, &Options::default()).is_empty());
    let reported = lsp_codes("file:///s.k65", STRICT_FIXTURE, &strict);
    assert_eq!(
        reported.iter().filter(|code| *code == "K816-E0160").count(),
        12
    );
    assert_eq!(
        reported.iter().filter(|code| *code == "K816-W0161").count(),
        1
    );

    let wide = Options {
        entry: EntryEnvironment::parse("native,a16,i8").expect("a machine that exists"),
        ..Options::default()
    };
    assert!(lsp_codes("file:///e.k816", ENTRY_CLAIM, &Options::default()).is_empty());
    assert_eq!(
        lsp_codes("file:///e.k816", ENTRY_CLAIM, &wide),
        ["K816-E0143"]
    );
}

/// A configuration object is read whether it arrives bare or nested under
/// `"k816"` — VS Code sends its whole settings tree, a hand-written client
/// sends the object.
#[test]
fn a_configuration_applies_bare_and_under_the_k816_section() {
    for value in [
        serde_json::json!({ "strict-k65": true, "entry": "emulation" }),
        serde_json::json!({ "k816": { "strict-k65": true, "entry": "emulation" } }),
    ] {
        let mut options = Options::default();
        let messages = config::apply(&mut options, &value);
        assert!(messages.is_empty(), "{messages:?}");
        assert!(options.strict_k65);
        assert_eq!(
            options.entry,
            EntryEnvironment::parse("emulation").expect("a machine that exists")
        );
    }
}

/// An absent key leaves the value alone. Neither source of the configuration
/// is a default, which is what keeps two sources of one fact safe
/// (`memory/entry-abi-assumption-conflict`).
#[test]
fn an_absent_key_leaves_the_option_alone() {
    let mut options = Options {
        strict_k65: true,
        entry: EntryEnvironment::parse("emulation").expect("a machine that exists"),
        ..Options::default()
    };

    // A statement about `entry` alone must not reset `strict-k65`.
    let messages = config::apply(
        &mut options,
        &serde_json::json!({ "entry": "native,a16,i8" }),
    );
    assert!(messages.is_empty(), "{messages:?}");
    assert!(options.strict_k65);
    assert_eq!(
        options.entry,
        EntryEnvironment::parse("native,a16,i8").expect("a machine that exists")
    );

    // An empty object, a null, and a settings tree with no `k816` section in
    // it are each a statement about nothing.
    for value in [
        serde_json::json!({}),
        serde_json::Value::Null,
        serde_json::json!({ "k816": {} }),
    ] {
        let before = format!("{:?}", (options.strict_k65, options.entry));
        assert!(config::apply(&mut options, &value).is_empty());
        assert_eq!(before, format!("{:?}", (options.strict_k65, options.entry)));
    }
}

/// A refused `entry` is the library's own refusal, carries the code the CLI
/// prints, and leaves the option standing.
#[test]
fn a_refused_entry_value_is_reported_and_changes_nothing() {
    let mut options = Options::default();
    let messages = config::apply(
        &mut options,
        &serde_json::json!({ "entry": "emulation,a16" }),
    );

    assert_eq!(messages.len(), 1);
    assert!(messages[0].is_error);
    assert_eq!(messages[0].code, Some(k816::abi::EntrySpelling::CODE));
    assert!(messages[0].render().starts_with("K816-E0162: "));
    // The wording is `EntryEnvironment::parse`'s, not a second one here.
    let refusal = EntryEnvironment::parse("emulation,a16").expect_err("names no machine");
    assert!(
        messages[0].text.contains(refusal.message()),
        "{}",
        messages[0].text
    );
    assert_eq!(options.entry, EntryEnvironment::default());
}

/// A key the server does not know is a warning naming it and the two it does.
#[test]
fn an_unknown_configuration_key_is_a_warning_naming_it() {
    let mut options = Options::default();
    let messages = config::apply(
        &mut options,
        &serde_json::json!({ "k816": { "target": "atarixl", "strict-k65": true } }),
    );

    assert_eq!(messages.len(), 1);
    assert!(!messages[0].is_error);
    assert!(
        messages[0].text.contains("`target`"),
        "{}",
        messages[0].text
    );
    assert!(messages[0].text.contains("strict-k65"));
    assert!(messages[0].text.contains("entry"));
    // The keys it does know still applied.
    assert!(options.strict_k65);
}

/// A value of the wrong JSON type is refused, not coerced.
#[test]
fn a_value_of_the_wrong_type_is_refused_and_changes_nothing() {
    let mut options = Options::default();
    let messages = config::apply(
        &mut options,
        &serde_json::json!({ "strict-k65": "yes", "entry": true }),
    );

    assert_eq!(messages.len(), 2);
    assert!(messages.iter().all(|message| message.is_error));
    assert!(!options.strict_k65);
    assert_eq!(options.entry, EntryEnvironment::default());
}

// ---------------------------------------------------------------------------
// The one test that drives the binary (decision 17)
// ---------------------------------------------------------------------------

/// Writes one LSP message with its `Content-Length` header.
fn write_message(to: &mut impl std::io::Write, value: &serde_json::Value) {
    let body = serde_json::to_string(value).expect("a serializable message");
    write!(to, "Content-Length: {}\r\n\r\n{body}", body.len()).expect("the server is listening");
    to.flush().expect("the server is listening");
}

/// Reads one LSP message, header and all.
fn read_message(from: &mut impl std::io::BufRead) -> serde_json::Value {
    let mut length = None;
    loop {
        let mut line = String::new();
        let read = from.read_line(&mut line).expect("the server is talking");
        assert_ne!(read, 0, "the server closed the connection mid-message");
        let trimmed = line.trim_end_matches(['\r', '\n']);
        if trimmed.is_empty() {
            break;
        }
        if let Some(value) = trimmed.strip_prefix("Content-Length: ") {
            length = Some(value.parse::<usize>().expect("a numeric Content-Length"));
        }
    }
    let mut body = vec![0; length.expect("a Content-Length header")];
    std::io::Read::read_exact(from, &mut body).expect("the announced body");
    serde_json::from_slice(&body).expect("a JSON body")
}

/// The transport, the capabilities object, and the configuration path — the
/// three things the in-process tests above cannot reach (decision 17).
#[test]
fn the_binary_speaks_the_protocol_and_reads_its_initialization_options() {
    use std::io::BufRead as _;

    let mut server = std::process::Command::new(env!("CARGO_BIN_EXE_k816"))
        .arg("lsp")
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .spawn()
        .expect("the k816 binary built for this test");
    let mut input = server.stdin.take().expect("a piped stdin");
    let mut output = std::io::BufReader::new(server.stdout.take().expect("a piped stdout"));

    write_message(
        &mut input,
        &serde_json::json!({
            "jsonrpc": "2.0",
            "id": 1,
            "method": "initialize",
            "params": {
                "processId": null,
                "capabilities": {},
                // Nested under `k816`, the way a settings tree arrives, and
                // stating the flag the fixture below needs.
                "initializationOptions": { "k816": { "strict-k65": true } },
            },
        }),
    );

    let response = read_message(&mut output);
    assert_eq!(response["id"], 1);
    assert!(
        !response["result"]["capabilities"]["textDocumentSync"].is_null(),
        "the capabilities object reached the client: {response}"
    );

    write_message(
        &mut input,
        &serde_json::json!({ "jsonrpc": "2.0", "method": "initialized", "params": {} }),
    );
    write_message(
        &mut input,
        &serde_json::json!({
            "jsonrpc": "2.0",
            "method": "textDocument/didOpen",
            "params": {
                "textDocument": {
                    "uri": "file:///strict.k65",
                    "languageId": "k65",
                    "version": 1,
                    "text": STRICT_FIXTURE,
                },
            },
        }),
    );

    // The strict fixture is clean without the flag, so a published `E0160`
    // is the initializationOptions arriving and being believed.
    let published = loop {
        let message = read_message(&mut output);
        if message["method"] == "textDocument/publishDiagnostics" {
            break message;
        }
    };
    assert_eq!(published["params"]["uri"], "file:///strict.k65");
    let codes: Vec<&str> = published["params"]["diagnostics"]
        .as_array()
        .expect("a diagnostics array")
        .iter()
        .map(|diagnostic| diagnostic["code"].as_str().expect("a string code"))
        .collect();
    assert_eq!(
        codes.iter().filter(|code| **code == "K816-E0160").count(),
        12
    );
    assert_eq!(
        codes.iter().filter(|code| **code == "K816-W0161").count(),
        1
    );

    write_message(
        &mut input,
        &serde_json::json!({ "jsonrpc": "2.0", "id": 2, "method": "shutdown", "params": null }),
    );
    loop {
        let message = read_message(&mut output);
        if message["id"] == 2 {
            break;
        }
    }
    write_message(
        &mut input,
        &serde_json::json!({ "jsonrpc": "2.0", "method": "exit", "params": null }),
    );
    drop(input);
    // Draining stdout lets the server's writer thread finish.
    let mut rest = String::new();
    while output.read_line(&mut rest).unwrap_or(0) > 0 {}
    let status = server.wait().expect("the server exits after `exit`");
    assert!(status.success(), "the server exited with {status}");
}
