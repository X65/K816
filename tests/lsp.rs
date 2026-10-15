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
use k816::workspace::Workspace;
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
        let analysis = workspace.analyze();
        let lsp_file = workspace.file(name).expect("document analyzed");
        let lsp_diagnostics = file_diagnostics(workspace.map(), lsp_file, &analysis.diagnostics);

        assert_eq!(cli_diagnostics, lsp_diagnostics, "diverged for {name}");
    }
}

#[test]
fn published_diagnostics_carry_code_range_severity_and_help() {
    let mut workspace = Workspace::new();
    let uri = "file:///broken.k816";
    workspace.update(uri, BROKEN_SLICE);
    let analysis = workspace.analyze();
    let file = workspace.file(uri).expect("document analyzed");
    let diagnostics = file_diagnostics(workspace.map(), file, &analysis.diagnostics);

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
    // A second document that neither collides with the first nor is seen by
    // it: what is being checked is that closing one takes its diagnostics
    // with it, and a shared name or a shared symbol would put a second
    // reason in the count.
    workspace.update("file:///b.k816", "func other @a8 {\n    return\n}\n");

    let analysis = workspace.analyze();
    assert_eq!(analysis.diagnostics.len(), 1, "{:?}", analysis.diagnostics);

    workspace.close("file:///a.k816");
    let analysis = workspace.analyze();
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
    let analysis = workspace.analyze();
    let file = workspace.file(name).expect("document analyzed");
    file_diagnostics(workspace.map(), file, &analysis.diagnostics)
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

// ---------------------------------------------------------------------------
// H1 — the analysis the server reads, and what is under a byte
// ---------------------------------------------------------------------------

/// A document holding one of everything the conversion has to survive: an
/// ASCII line, a two-byte character, and a four-byte one.
const MIXED: &str = "// żab\n𝕏x\n";

/// Decision 13's table, both ways, on one-, two-, and four-byte characters.
///
/// Byte offsets and UTF-16 positions that name the same place, so a single
/// list checks the conversion and its inverse against each other *and*
/// against an independently computed answer — a pair of functions that are
/// each other's inverse can still both be wrong.
#[test]
fn positions_and_offsets_convert_both_ways() {
    use k816::lsp::convert::{offset_in, position_in};

    let mut map = SourceMap::new();
    let id = map.add_file("mixed.k816", MIXED);
    let source = map.file(id);

    // (byte offset, line, UTF-16 character)
    for (offset, line, character) in [
        (0, 0, 0),  // `/`
        (3, 0, 3),  // `ż` — two bytes, one UTF-16 unit
        (5, 0, 4),  // `a`, after it
        (7, 0, 6),  // end of the first line
        (8, 1, 0),  // `𝕏` — four bytes, two UTF-16 units
        (12, 1, 2), // `x`, after it
        (13, 1, 3), // end of the second line
        (14, 2, 0), // the empty line a trailing newline leaves
    ] {
        assert_eq!(
            position_in(source, offset),
            Position::new(line, character),
            "byte {offset}"
        );
        assert_eq!(
            offset_in(source, Position::new(line, character)),
            offset,
            "position {line}:{character}"
        );
    }
}

/// The positions that do not round-trip, and what each of them means.
///
/// A client legitimately sends every one of these; none may panic and none
/// may guess (decision 18).
#[test]
fn out_of_range_and_split_positions_clamp_rather_than_panic() {
    use k816::lsp::convert::{offset_in, position_in, range_to_span};

    let mut map = SourceMap::new();
    let id = map.add_file("mixed.k816", MIXED);
    let source = map.file(id);

    // Inside `𝕏`'s second UTF-16 unit: rounds down to the character's start.
    assert_eq!(offset_in(source, Position::new(1, 1)), 8);
    // Past the end of a line: that line's end, not the next line's start.
    assert_eq!(offset_in(source, Position::new(0, 99)), 7);
    // Past the last line: the end of the file.
    assert_eq!(offset_in(source, Position::new(9, 0)), MIXED.len() as u32);
    // A byte offset that splits a character: the character's own position.
    assert_eq!(position_in(source, 4), Position::new(0, 3));
    assert_eq!(position_in(source, 10), Position::new(1, 0));
    // Past the end of the file.
    assert_eq!(position_in(source, 9_999), Position::new(2, 0));
    // A reversed range covers what it covers.
    let span = range_to_span(
        source,
        lsp_types::Range::new(Position::new(1, 2), Position::new(0, 0)),
    );
    assert_eq!((span.start, span.end), (0, 12));
}

/// `at` on the slice: the node, the statement, the function and the
/// declaration each answered for a table of offsets.
///
/// One function answers "what is under this byte" for hover, definition,
/// references, rename and completion alike, so this table is the place all
/// five of them are wrong at once if it is.
#[test]
fn at_names_the_node_the_statement_and_the_function() {
    let mut workspace = Workspace::new();
    let uri = "file:///slice.k816";
    workspace.update(uri, include_str!("golden/slice.k816"));
    let analysis = workspace.analyze();
    let file = workspace.file(uri).expect("document analyzed");
    let names = |id| {
        analysis
            .unit(file)
            .unwrap()
            .model
            .symbols
            .name(id)
            .to_owned()
    };

    /// One row: the byte offset, the innermost node's kind, whether it is a
    /// token rather than the whitespace between two, whether a statement
    /// covers it, and the function and declaration it names.
    struct Row {
        offset: u32,
        kind: &'static str,
        on_token: bool,
        has_statement: bool,
        function: Option<&'static str>,
        declaration: Option<&'static str>,
    }
    const fn row(
        offset: u32,
        kind: &'static str,
        on_token: bool,
        has_statement: bool,
        function: Option<&'static str>,
        declaration: Option<&'static str>,
    ) -> Row {
        Row {
            offset,
            kind,
            on_token,
            has_statement,
            function,
            declaration,
        }
    }

    let table = &[
        // `var` on line 0 — a unit-level declaration, no function around it.
        row(0, "var", true, false, None, None),
        row(4, "identifier", true, false, None, Some("SCREEN")),
        row(13, "number", true, false, None, None),
        // The `func` keyword is inside the item, so `main` is the function.
        row(20, "func", true, false, Some("main"), None),
        row(25, "identifier", true, false, Some("main"), Some("main")),
        row(31, "width_annotation", true, false, Some("main"), None),
        // Column 0 of a body line: between tokens, so no statement.
        row(36, "block", false, false, Some("main"), None),
        row(40, "register", true, true, Some("main"), None),
        row(44, "#", true, true, Some("main"), None),
        // A use of `SCREEN`: H3's reference index is what makes this a
        // symbol; H1 answers the statement and the function.
        row(64, "identifier", true, true, Some("main"), None),
        row(84, "}", true, false, Some("main"), None),
        // End of file, and past it.
        row(86, "source_file", false, false, None, None),
        row(9_999, "source_file", false, false, None, None),
    ];

    for expected in table {
        let offset = expected.offset;
        let context = k816::lsp::at(&analysis, file, offset).expect("an analyzed document");
        assert_eq!(context.node_kind(), expected.kind, "node at {offset}");
        assert_eq!(context.on_token(), expected.on_token, "token at {offset}");
        assert_eq!(
            context.statement.is_some(),
            expected.has_statement,
            "statement at {offset}"
        );
        assert_eq!(
            context.function.map(&names).as_deref(),
            expected.function,
            "function at {offset}"
        );
        assert_eq!(
            context.declaration.map(&names).as_deref(),
            expected.declaration,
            "declaration at {offset}"
        );
    }
}

/// The node path is a path: outermost first, innermost last, every element
/// covering the offset.
#[test]
fn the_node_path_runs_from_the_file_to_the_token() {
    let mut workspace = Workspace::new();
    let uri = "file:///slice.k816";
    workspace.update(uri, include_str!("golden/slice.k816"));
    let analysis = workspace.analyze();
    let file = workspace.file(uri).expect("document analyzed");

    let context = k816::lsp::at(&analysis, file, 64).expect("an analyzed document");
    let kinds: Vec<&str> = context.path.iter().map(|node| node.kind.as_str()).collect();
    assert_eq!(
        kinds,
        [
            "source_file",
            "func_declaration",
            "block",
            "hla_assignment",
            "address",
            "identifier",
        ]
    );
    for node in &context.path {
        assert!(node.span.contains(64), "{} does not cover 64", node.kind);
    }
    // The innermost *named* node is the identifier itself; on the `=` two
    // characters earlier it is the assignment around the anonymous token.
    assert_eq!(
        context.named_node().map(|node| node.kind.as_str()),
        Some("identifier")
    );
    let context = k816::lsp::at(&analysis, file, 65).expect("an analyzed document");
    assert_eq!(context.node_kind(), "identifier");
}

/// The analysis carries the model the CLI throws away, and it is the same
/// model: the unit's symbols, its program, and its widths are all there.
#[test]
fn the_analysis_carries_every_units_model() {
    let mut workspace = Workspace::new();
    // Named as the sources name each other: a program says which units
    // it has, so a document store that renamed one would be describing a
    // different program.
    workspace.update(UNIT_A, include_str!("golden/unit-a.k816"));
    workspace.update(UNIT_B, include_str!("golden/unit-b.k816"));
    let analysis = workspace.analyze();

    assert_eq!(analysis.units.len(), 2);
    for unit in &analysis.units {
        assert_eq!(
            analysis.unit(unit.file).map(|found| found.file),
            Some(unit.file)
        );
        // A program with no preprocessor directive in it *is* the parsed
        // file, and `program()` is how a consumer gets the right one without
        // having to know which case it is in.
        assert!(!unit.program().items.is_empty());
        assert!(unit.model.symbols.iter().count() > 0);
    }
    // The linkage that resolved the two against each other is kept too.
    assert!(analysis.linkage.diagnostics().is_empty());
}

/// Nothing panics on a document the workspace does not hold, on an empty
/// workspace, or on a document that is one `ERROR` node (decision 18).
#[test]
fn at_answers_for_an_empty_workspace_and_an_error_document() {
    // An empty workspace has no units, so every file is a miss.
    let mut workspace = Workspace::new();
    let analysis = workspace.analyze();
    assert!(analysis.units.is_empty());
    let mut map = SourceMap::new();
    let stranger = map.add_file("file:///absent.k816", "");
    assert!(k816::lsp::at(&analysis, stranger, 0).is_none());

    // A document the grammar cannot read still answers, from what parsed.
    let uri = "file:///garbage.k816";
    workspace.update(uri, "}{ ??? &&&\n");
    let analysis = workspace.analyze();
    let file = workspace.file(uri).expect("document analyzed");
    assert!(!analysis.diagnostics.is_empty());
    for offset in 0..=12 {
        let context = k816::lsp::at(&analysis, file, offset).expect("an analyzed document");
        assert_eq!(
            context.path.first().map(|node| node.kind.as_str()),
            Some("source_file")
        );
        assert_eq!(context.statement, None);
    }
}

/// The store keeps a document's identity across an edit and reassigns them on
/// a close, and an analysis is always over the text the store holds.
///
/// A `FileId` is what every span points into, so an edit that changed one
/// would invalidate the identity of every *other* document too. Closing one
/// is the case where identities legitimately move, and the test says which is
/// which.
#[test]
fn a_document_keeps_its_identity_across_an_edit_and_the_map_follows_the_text() {
    let mut workspace = Workspace::new();
    workspace.update("file:///a.k816", "var LEFT = 1\n");
    workspace.update("file:///b.k816", "var RIGHT = 2\n");
    let (a, b) = (
        workspace.file("file:///a.k816").expect("open"),
        workspace.file("file:///b.k816").expect("open"),
    );
    assert_ne!(a, b);

    workspace.update("file:///a.k816", "var LEFT = 1\nvar EXTRA = 3\n");
    assert_eq!(workspace.file("file:///a.k816"), Some(a));
    assert_eq!(workspace.file("file:///b.k816"), Some(b));
    assert_eq!(
        workspace.map().file(a).text(),
        "var LEFT = 1\nvar EXTRA = 3\n"
    );

    // The analysis is over what the store holds, and its spans point into it.
    let analysis = workspace.analyze();
    let unit = analysis.unit(a).expect("an analyzed unit");
    // The unit's own declarations; the third symbol is the `RIGHT` the other
    // open document defines, whose declaration points into *that* file.
    let own: Vec<&str> = unit
        .model
        .symbols
        .iter()
        .filter(|(_, symbol)| !symbol.imported)
        .map(|(_, symbol)| symbol.name.as_str())
        .collect();
    assert_eq!(own, ["LEFT", "EXTRA"]);
    for (_, symbol) in unit.model.symbols.iter().filter(|(_, s)| !s.imported) {
        assert_eq!(symbol.declaration.file, a);
        assert_eq!(
            workspace.map().span_text(symbol.declaration),
            symbol.name.as_str()
        );
    }

    // Closing the first document renumbers the second, and the store says so.
    workspace.close("file:///a.k816");
    assert_eq!(workspace.file("file:///a.k816"), None);
    let b = workspace.file("file:///b.k816").expect("still open");
    assert_eq!(workspace.map().files().count(), 1);
    assert_eq!(workspace.map().file(b).text(), "var RIGHT = 2\n");
    let analysis = workspace.analyze();
    assert_eq!(analysis.units.len(), 1);
    assert_eq!(analysis.units[0].file, b);
}

/// Re-analyzing an unchanged workspace produces the same answers.
///
/// The store hands the previous tree back to the parser, so this is the
/// property that says reuse is not a change: same diagnostics, same CST.
#[test]
fn re_analyzing_an_unchanged_workspace_answers_the_same() {
    let mut workspace = Workspace::new();
    workspace.update("file:///slice.k816", include_str!("golden/slice.k816"));

    let first = workspace.analyze();
    let second = workspace.analyze();
    assert_eq!(
        format!("{:?}", first.diagnostics),
        format!("{:?}", second.diagnostics)
    );
    assert_eq!(
        first.units[0].parse.tree.dump(),
        second.units[0].parse.tree.dump()
    );
    assert_eq!(first.units[0].parse.file, second.units[0].parse.file);
}

// ---------------------------------------------------------------------------
// H2 — incremental sync and coalescing
// ---------------------------------------------------------------------------

#[path = "common/scenarios.rs"]
mod scenarios;

const DOC: &str = "file:///scenario.k816";

/// Applies one *ranged* content change the way the server does: convert the
/// UTF-16 range against the text the workspace currently holds, then hand the
/// byte-span edit to the store.
///
/// The two calls `lsp::apply` makes, in the order it makes them, so the test
/// exercises the conversion and the store's edit path and not a third
/// arrangement of them (decision 17).
fn apply_ranged_change(workspace: &mut Workspace, range: lsp_types::Range, text: &str) {
    use k816::lsp::convert::range_to_span;
    use k816::parser::SourceEdit;

    let file = workspace.file(DOC).expect("an open document");
    let span = range_to_span(workspace.map().file(file), range);
    workspace.apply_edit(DOC, &SourceEdit::new(span, text));
}

/// Every edit scenario, run through the server's edit path.
///
/// The edit is found by text, turned into a UTF-16 range *by the same
/// conversion the server uses in the other direction*, and fed back in — so
/// the two halves of decision 13 are checked against each other on every
/// character class the corpus has. The claim at the end is the phase-11 one:
/// what the incremental workspace holds is what a fresh workspace holding the
/// final text would produce, on the CST, the AST and the diagnostics.
#[test]
fn every_edit_scenario_survives_the_servers_incremental_path() {
    use k816::lsp::convert::span_to_range;
    use k816::source::Span;

    for scenario in scenarios::scenarios() {
        let mut workspace = Workspace::new();
        workspace.update(DOC, scenario.source.clone());
        // The first analysis is what gives the store a tree to edit, exactly
        // as the `didOpen` publish does in the server.
        workspace.analyze();

        for (step, edit) in scenario.edits.iter().enumerate() {
            let step = format!("{} (edit {})", scenario.name, step + 1);
            let file = workspace.file(DOC).expect("an open document");
            let text = workspace.map().file(file).text();
            let start = text
                .find(&edit.find)
                .unwrap_or_else(|| panic!("{step}: `{}` not found", edit.find));
            let span = Span::new(file, start as u32, (start + edit.find.len()) as u32);
            let range = span_to_range(workspace.map(), span);

            apply_ranged_change(&mut workspace, range, &edit.replace);
            workspace.analyze();
        }

        let final_text = {
            let file = workspace.file(DOC).expect("an open document");
            workspace.map().file(file).text().to_owned()
        };
        let incremental = workspace.analyze();

        let mut fresh = Workspace::new();
        fresh.update(DOC, final_text.clone());
        let clean = fresh.analyze();

        assert_eq!(
            incremental.units[0].parse.tree.dump(),
            clean.units[0].parse.tree.dump(),
            "{}: CST differs for:\n{final_text}",
            scenario.name
        );
        assert_eq!(
            incremental.units[0].parse.file, clean.units[0].parse.file,
            "{}: AST differs for:\n{final_text}",
            scenario.name
        );
        assert_eq!(
            format!("{:?}", incremental.diagnostics),
            format!("{:?}", clean.diagnostics),
            "{}: diagnostics differ for:\n{final_text}",
            scenario.name
        );
    }
}

/// A range off the end of the document is clamped, not a panic (decision 18).
#[test]
fn a_change_beyond_the_end_of_the_document_is_clamped() {
    let mut workspace = Workspace::new();
    workspace.update(DOC, "var SCREEN = $0400\n");
    workspace.analyze();

    // Both ends past the last line: an insertion at end of file.
    apply_ranged_change(
        &mut workspace,
        lsp_types::Range::new(Position::new(99, 0), Position::new(200, 5)),
        "var OTHER = $0500\n",
    );
    let file = workspace.file(DOC).expect("an open document");
    assert_eq!(
        workspace.map().file(file).text(),
        "var SCREEN = $0400\nvar OTHER = $0500\n"
    );

    // A column past the end of a line is that line's end, and a reversed
    // range covers what it covers.
    apply_ranged_change(
        &mut workspace,
        lsp_types::Range::new(Position::new(1, 999), Position::new(0, 0)),
        "",
    );
    let file = workspace.file(DOC).expect("an open document");
    assert_eq!(workspace.map().file(file).text(), "\n");
    assert!(workspace.analyze().units[0].parse.diagnostics.is_empty());
}

/// The workspace counts its own analyses, which is what makes "one analysis"
/// a checkable claim rather than an unobservable one.
#[test]
fn the_workspace_counts_its_analyses() {
    let mut workspace = Workspace::new();
    assert_eq!(workspace.analyses(), 0);
    workspace.update(DOC, "var SCREEN = $0400\n");
    assert_eq!(workspace.analyses(), 0, "an edit is not an analysis");
    workspace.analyze();
    workspace.analyze();
    assert_eq!(workspace.analyses(), 2);
}

/// Drives the real server loop over an in-memory connection with `messages`
/// already queued, and returns every notification it sent back.
/// [`run_server`], and how many times the workspace was analyzed.
///
/// The count is the only way to see the batch contract: the same diagnostics
/// arrive whether they were computed once or six times.
fn run_server_counting(messages: Vec<serde_json::Value>) -> (Vec<lsp_server::Notification>, u64) {
    let (server, client) = lsp_server::Connection::memory();
    for message in messages {
        client
            .sender
            .send(serde_json::from_value(message).expect("a protocol message"))
            .expect("the server is listening");
    }
    let mut workspace = Workspace::new();
    k816::lsp::serve(&server, &mut workspace, &serde_json::Value::Null)
        .expect("the server runs to shutdown");
    drop(server);

    let mut sent = Vec::new();
    while let Ok(message) = client.receiver.try_recv() {
        if let lsp_server::Message::Notification(notification) = message {
            sent.push(notification);
        }
    }
    (sent, workspace.analyses())
}

fn did_open(text: &str) -> serde_json::Value {
    serde_json::json!({
        "jsonrpc": "2.0",
        "method": "textDocument/didOpen",
        "params": {
            "textDocument": { "uri": DOC, "languageId": "k816", "version": 1, "text": text },
        },
    })
}

fn did_change(line: u32, character: u32, text: &str) -> serde_json::Value {
    serde_json::json!({
        "jsonrpc": "2.0",
        "method": "textDocument/didChange",
        "params": {
            "textDocument": { "uri": DOC, "version": 2 },
            "contentChanges": [{
                "range": {
                    "start": { "line": line, "character": character },
                    "end": { "line": line, "character": character },
                },
                "text": text,
            }],
        },
    })
}

fn shutdown_and_exit() -> Vec<serde_json::Value> {
    vec![
        serde_json::json!({ "jsonrpc": "2.0", "id": 1, "method": "shutdown", "params": null }),
        serde_json::json!({ "jsonrpc": "2.0", "method": "exit", "params": null }),
    ]
}

/// A burst of notifications is one analysis, and therefore one publish.
///
/// The server drains everything already queued before it analyzes, so six
/// notifications that arrived together produce one answer rather than six
/// (decision 3). Queueing them before the loop starts is what makes the test
/// deterministic: there is no timer to race.
#[test]
fn a_burst_of_notifications_produces_one_analysis() {
    let mut messages = vec![did_open("func main @a8 {\n    return\n}\n")];
    for character in 0..5 {
        messages.push(did_change(1, 4 + character, "n"));
    }
    messages.extend(shutdown_and_exit());

    let (sent, analyses) = run_server_counting(messages);
    let published = sent
        .iter()
        .filter(|notification| notification.method == "textDocument/publishDiagnostics")
        .count();
    assert_eq!(published, 1, "one analysis for the whole burst");
    assert_eq!(analyses, 1, "the workspace was analyzed more than once");
}

/// A batch holding *requests* as well as notifications is still one analysis.
///
/// The publish count cannot see this: the server published once either way,
/// and answered each request against an analysis of its own. That is the
/// coalescing buying nothing for exactly the case it exists for — a burst of
/// keystrokes with a `hover` in it — so the count is asserted directly.
#[test]
fn a_batch_of_notifications_and_requests_is_one_analysis() {
    let position = serde_json::json!({
        "textDocument": { "uri": DOC },
        "position": { "line": 1, "character": 4 },
    });
    let mut messages = vec![did_open("func main @a8 {\n    return\n}\n")];
    for character in 0..3 {
        messages.push(did_change(1, 4 + character, "n"));
    }
    for (id, method) in [
        (20, "textDocument/hover"),
        (21, "textDocument/definition"),
        (22, "textDocument/documentSymbol"),
    ] {
        messages.push(serde_json::json!({
            "jsonrpc": "2.0",
            "id": id,
            "method": method,
            "params": if method == "textDocument/documentSymbol" {
                serde_json::json!({ "textDocument": { "uri": DOC } })
            } else {
                position.clone()
            },
        }));
    }
    messages.extend(shutdown_and_exit());

    let (sent, analyses) = run_server_counting(messages);
    assert_eq!(
        analyses, 1,
        "four notifications and three requests, one analysis"
    );
    assert_eq!(
        sent.iter()
            .filter(|n| n.method == "textDocument/publishDiagnostics")
            .count(),
        1
    );
}

/// A batch with nothing in it but requests analyzes once, not zero times.
///
/// The requests still need an analysis to be answered against; skipping it
/// would answer them all with nothing.
#[test]
fn a_batch_of_requests_alone_still_analyzes_once() {
    let mut messages = vec![did_open("var SCREEN = $0400\n")];
    messages.extend(shutdown_and_exit());
    let (_, opening) = run_server_counting(messages);
    assert_eq!(opening, 1);
}

/// The same six notifications, delivered one at a time, are six analyses.
///
/// Without this the test above would pass for a server that published once
/// and stopped. The server runs on its own thread and the test waits for each
/// answer before sending the next, so "arrived together" is the only
/// difference between the two.
#[test]
fn notifications_delivered_one_at_a_time_are_analyzed_one_at_a_time() {
    let (server, client) = lsp_server::Connection::memory();
    let worker = std::thread::spawn(move || {
        let mut workspace = Workspace::new();
        k816::lsp::serve(&server, &mut workspace, &serde_json::Value::Null)
            .expect("the server runs to shutdown");
        drop(server);
    });

    let send = |message: serde_json::Value| {
        client
            .sender
            .send(serde_json::from_value(message).expect("a protocol message"))
            .expect("the server is listening");
    };
    let await_publish = || {
        loop {
            let message = client.receiver.recv().expect("the server answers");
            if let lsp_server::Message::Notification(notification) = message
                && notification.method == "textDocument/publishDiagnostics"
            {
                break;
            }
        }
    };

    send(did_open("func main @a8 {\n    return\n}\n"));
    await_publish();
    let mut publishes = 1;
    for character in 0..5 {
        send(did_change(1, 4 + character, "n"));
        await_publish();
        publishes += 1;
    }
    assert_eq!(publishes, 6);

    for message in shutdown_and_exit() {
        send(message);
    }
    worker.join().expect("the server thread finishes");
}

// ---------------------------------------------------------------------------
// H3 — definition, references, and the symbol table as an outline
// ---------------------------------------------------------------------------

const UNIT_A: &str = "file:///unit-a.k816";
const UNIT_B: &str = "file:///unit-b.k816";

/// The two-unit golden pair, open together the way an editor would have them.
fn cross_unit_workspace() -> Workspace {
    let mut workspace = Workspace::new();
    workspace.update(UNIT_A, include_str!("golden/unit-a.k816"));
    workspace.update(UNIT_B, include_str!("golden/unit-b.k816"));
    workspace
}

/// Definition of a name a *sibling* unit defines lands in that unit.
///
/// The reference index keys by span and a `Symbol.declaration` of an imported
/// symbol points into the defining unit, which together are what makes a
/// cross-unit answer possible at all: a `SymbolId` means nothing outside its
/// own table.
#[test]
fn definition_of_an_imported_symbol_lands_in_the_defining_unit() {
    use k816::lsp::navigation::definition;

    let mut workspace = cross_unit_workspace();
    let analysis = workspace.analyze();
    let (a, b) = (
        workspace.file(UNIT_A).expect("open"),
        workspace.file(UNIT_B).expect("open"),
    );

    // `jsr helper`, `lda CURSOR` and `lda BANKED` — the three imports, at
    // their uses. Located rather than counted: an offset written out is a
    // number that goes stale whenever a line is added above it, and the
    // fixture is a source somebody may edit.
    for (use_site, name) in [
        (at(UNIT_A_TEXT, "jsr helper", 4), "helper"),
        (at(UNIT_A_TEXT, "lda CURSOR", 4), "CURSOR"),
        (at(UNIT_A_TEXT, "lda BANKED", 4), "BANKED"),
    ] {
        let target = definition(&analysis, a, use_site).expect("resolves");
        assert_eq!(target.file, b, "`{name}`");
        assert_eq!(workspace.map().span_text(target), name);
    }

    // A declaration answers with itself, which is what an editor shows when
    // a user asks a declaration where it is declared.
    let declaration = at(UNIT_B_TEXT, "func helper", 5);
    let target = definition(&analysis, b, declaration).expect("`helper` is declared here");
    assert_eq!((target.file, target.start), (b, declaration));
}

const UNIT_A_TEXT: &str = include_str!("golden/unit-a.k816");
const UNIT_B_TEXT: &str = include_str!("golden/unit-b.k816");

/// The offset of `needle` in `text`, plus `skip` bytes into it.
fn at(text: &str, needle: &str, skip: usize) -> u32 {
    (text
        .find(needle)
        .unwrap_or_else(|| panic!("`{needle}` is in the fixture"))
        + skip) as u32
}

/// References of an imported symbol span both units, and the declaration is
/// included only when asked for.
#[test]
fn references_of_an_imported_symbol_reach_every_open_unit() {
    use k816::lsp::navigation::references;

    let mut workspace = cross_unit_workspace();
    let analysis = workspace.analyze();
    let (a, b) = (
        workspace.file(UNIT_A).expect("open"),
        workspace.file(UNIT_B).expect("open"),
    );

    // From the *use* in unit A.
    let use_site = at(UNIT_A_TEXT, "lda CURSOR", 4);
    let found = references(&analysis, a, use_site, true);
    let places: Vec<(bool, &str)> = found
        .iter()
        .map(|span| (span.file == b, workspace.map().span_text(*span)))
        .collect();
    // Sorted by span, and a span orders by file first: unit A is the first
    // document the workspace holds, so its two uses come before unit B's
    // declaration.
    assert_eq!(
        places,
        [(false, "CURSOR"), (false, "CURSOR"), (true, "CURSOR")],
        "both uses in A and the declaration in B"
    );

    // Without the declaration, only the uses.
    let uses = references(&analysis, a, use_site, false);
    assert_eq!(uses.len(), 2);
    assert!(uses.iter().all(|span| span.file == a));

    // From the *declaration* in unit B, the same answer. The `CURSOR`
    // inside that unit's header comment is not it, which is why the
    // declaration is located by the line that declares it.
    let declaration = at(UNIT_B_TEXT, "var dp CURSOR", 7);
    assert_eq!(references(&analysis, b, declaration, true), found);
}

/// A local label's references stay inside its own function, even when
/// another function declares a label of the same name.
#[test]
fn references_of_a_local_label_are_confined_to_its_function() {
    use k816::lsp::navigation::{definition, references};

    const SOURCE: &str = "func first @a8 @i8 {\n\
        \x20   .spin:\n\
        \x20   x++\n\
        \x20   != goto .spin\n\
        }\n\
        \n\
        func second @a8 @i8 {\n\
        \x20   .spin:\n\
        \x20   y++\n\
        \x20   != goto .spin\n\
        }\n";

    let mut workspace = Workspace::new();
    let uri = "file:///labels.k816";
    workspace.update(uri, SOURCE);
    let analysis = workspace.analyze();
    let file = workspace.file(uri).expect("open");
    assert!(
        analysis.diagnostics.is_empty(),
        "{:?}",
        analysis.diagnostics
    );

    let first_use = SOURCE.find("goto .spin").expect("a use") as u32 + 5;
    let second_use = SOURCE.rfind("goto .spin").expect("a second use") as u32 + 5;
    assert_ne!(first_use, second_use);

    // Each use resolves to its *own* function's label.
    let first_declaration = definition(&analysis, file, first_use).expect("resolves");
    let second_declaration = definition(&analysis, file, second_use).expect("resolves");
    assert_ne!(first_declaration, second_declaration);
    assert!(first_declaration.start < second_declaration.start);

    // And each label's references are its own function's.
    let found = references(&analysis, file, first_use, true);
    assert_eq!(
        found,
        [first_declaration, Span::new(file, first_use, first_use + 5)]
    );
    assert!(
        found
            .iter()
            .all(|span| span.start < second_declaration.start),
        "a reference escaped into the second function"
    );
}

/// A `.field` step resolves to the field's declaration, not to the variable's.
#[test]
fn a_field_reference_resolves_to_the_field_declaration() {
    use k816::lsp::navigation::{definition, references};

    const LAYOUT: &str = include_str!("golden/layout-unit-a.k816");
    let mut workspace = Workspace::new();
    // Named as the pair names itself: the fixture imports its sibling, and
    // a document store that called it something else would be describing a
    // different program.
    workspace.update(UNIT_A_LAYOUT, LAYOUT);
    workspace.update(UNIT_B_LAYOUT, include_str!("golden/layout-unit-b.k816"));
    let analysis = workspace.analyze();
    let file = workspace.file(UNIT_A_LAYOUT).expect("open");

    // `TABLE[3].hi = a` — the `.hi` of the use, and the `.hi :byte` of the
    // field list it resolves to.
    let use_site = at(LAYOUT, "TABLE[3].hi", 9);
    let declaration = at(LAYOUT, ".hi :byte", 0);
    let target = definition(&analysis, file, use_site).expect("`.hi` resolves");
    assert_eq!(
        target.start, declaration,
        "the `.hi :byte` entry of the field list"
    );
    assert!(workspace.map().span_text(target).starts_with(".hi"));

    // The base name of the same expression resolves to the variable.
    let base = definition(&analysis, file, at(LAYOUT, "TABLE[3].hi", 1)).expect("`TABLE` resolves");
    assert_eq!(workspace.map().span_text(base), "TABLE");

    // Both uses of `.hi` are found; the declaration is the field list entry.
    let found = references(&analysis, file, use_site, true);
    assert_eq!(found.len(), 2, "the declaration and the one use");
    assert_eq!(found[0].start, declaration);
    assert_eq!(found[1].start, use_site - 1);
}

const UNIT_A_LAYOUT: &str = "file:///layout-unit-a.k816";
const UNIT_B_LAYOUT: &str = "file:///layout-unit-b.k816";

/// The document outline is the symbol table, with local labels nested under
/// the function that scopes them and fields under their variable.
#[test]
fn the_document_outline_is_the_symbol_table() {
    use k816::lsp::navigation::{OutlineKind, document_symbols};

    const SOURCE: &str = "var SCREEN = $0400\n\
        const LIMIT = 8\n\
        var PAIR[ .lo :byte .hi :byte ]\n\
        \n\
        data table {\n\
        \x20   $01 $02\n\
        }\n\
        \n\
        func work @a8 @i8 {\n\
        \x20   .again:\n\
        \x20   x++\n\
        \x20   != goto .again\n\
        }\n";

    let mut workspace = Workspace::new();
    let uri = "file:///outline.k816";
    workspace.update(uri, SOURCE);
    let analysis = workspace.analyze();
    let file = workspace.file(uri).expect("open");

    let outline = document_symbols(&analysis, file);
    let top: Vec<(&str, OutlineKind)> = outline
        .iter()
        .map(|entry| (entry.name.as_str(), entry.kind))
        .collect();
    assert_eq!(
        top,
        [
            ("SCREEN", OutlineKind::Variable),
            ("LIMIT", OutlineKind::Constant),
            ("PAIR", OutlineKind::Variable),
            ("table", OutlineKind::Data),
            ("work", OutlineKind::Function),
        ]
    );

    // The fields hang off the variable that declares them.
    let pair = &outline[2];
    let fields: Vec<&str> = pair.children.iter().map(|f| f.name.as_str()).collect();
    assert_eq!(fields, [".lo", ".hi"]);
    assert_eq!(pair.children[1].detail.as_deref(), Some("+1"));

    // The local label hangs off its function, and the function's full range
    // covers its whole body while the selection is just the name.
    let work = &outline[4];
    assert_eq!(
        work.children
            .iter()
            .map(|c| c.name.as_str())
            .collect::<Vec<_>>(),
        [".again"]
    );
    assert_eq!(workspace.map().span_text(work.declaration), "work");
    assert!(work.full.start < work.declaration.start);
    assert!(work.full.end > work.children[0].declaration.end);

    // Details are the facts the model holds, not text re-read from source.
    assert_eq!(outline[0].detail.as_deref(), Some("@ $0400"));
    assert_eq!(outline[1].detail.as_deref(), Some("= $0008"));
}

/// The workspace outline is every open unit's, filtered by substring, and it
/// never lists an import — a symbol is listed by the unit that declares it.
#[test]
fn workspace_symbols_filter_across_open_units_and_skip_imports() {
    use k816::lsp::navigation::workspace_symbols;

    let mut workspace = cross_unit_workspace();
    let analysis = workspace.analyze();
    let b = workspace.file(UNIT_B).expect("open");

    let all: Vec<String> = workspace_symbols(&analysis, "")
        .into_iter()
        .map(|(_, outline)| outline.name)
        .collect();
    assert_eq!(all, ["caller", "CURSOR", "BANKED", "helper"]);

    // `CURSOR` is declared once even though both units name it.
    let cursor = workspace_symbols(&analysis, "curs");
    assert_eq!(cursor.len(), 1);
    assert_eq!(cursor[0].0, b);

    // The filter is case-insensitive and matches anywhere in the name.
    assert_eq!(workspace_symbols(&analysis, "ANK").len(), 1);
    assert!(workspace_symbols(&analysis, "nothing-like-this").is_empty());
}

/// A name inside a cross-unit `inline` body resolves to the *defining* unit's
/// declarations, not to whatever the expanding unit happens to call the same
/// thing.
///
/// G7 copies the body into the expanding unit's program carrying the defining
/// unit's spans, so the index records a span in one file mapping to a symbol
/// resolved in another unit's table. Keying by `Span` — which carries the
/// file — is what makes that answerable.
#[test]
fn names_inside_a_borrowed_inline_body_point_at_the_defining_unit() {
    use k816::lsp::navigation::definition;

    let dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/compat/k816/inline-across-units");
    let mut workspace = Workspace::new();
    let mut names = Vec::new();
    for entry in std::fs::read_dir(&dir).expect("read the fixture") {
        let path = entry.expect("a fixture entry").path();
        if path
            .extension()
            .is_none_or(|ext| ext != "k65" && ext != "k816" && ext != "kk")
        {
            continue;
        }
        let text = std::fs::read_to_string(&path).expect("read a unit");
        let uri = format!("file:///{}", path.file_name().unwrap().to_string_lossy());
        workspace.update(uri.clone(), text);
        names.push(uri);
    }
    names.sort();
    assert!(names.len() >= 2, "the fixture is two units: {names:?}");

    let analysis = workspace.analyze();
    // Every reference the index recorded resolves to a declaration, and the
    // declaration is in whichever file actually declares it — which for a
    // borrowed body is the file the body was written in.
    let mut cross_file = 0;
    for unit in &analysis.units {
        for span in unit.model.references.keys() {
            let target = definition(&analysis, span.file, span.start)
                .unwrap_or_else(|| panic!("a recorded reference that resolves to nothing"));
            assert!(
                !workspace.map().span_text(target).is_empty(),
                "a declaration with no text"
            );
            if target.file != unit.file {
                cross_file += 1;
            }
        }
    }
    assert!(
        cross_file > 0,
        "the fixture is supposed to resolve names across the unit boundary"
    );
}

/// Every request the server answers, thrown at an empty workspace, at a
/// document it does not hold, and at one that is a single `ERROR` node.
///
/// A panic in a request handler kills the editor's session, so the claim is
/// not "the answer is right" but "there is an answer" (decision 18). The
/// sweep grows with every task that adds a request.
#[test]
fn no_request_panics_on_an_empty_workspace_or_a_broken_document() {
    let request = |id: i32, method: &str, params: serde_json::Value| serde_json::json!({ "jsonrpc": "2.0", "id": id, "method": method, "params": params });
    let at_position = |uri: &str| {
        serde_json::json!({
            "textDocument": { "uri": uri },
            "position": { "line": 400, "character": 900 },
        })
    };
    let sweep = |uri: &str| {
        vec![
            request(
                5,
                "textDocument/formatting",
                serde_json::json!({
                    "textDocument": { "uri": uri },
                    "options": { "tabSize": 4, "insertSpaces": true },
                }),
            ),
            request(
                6,
                "textDocument/semanticTokens/full",
                serde_json::json!({ "textDocument": { "uri": uri } }),
            ),
            request(
                7,
                "textDocument/codeAction",
                serde_json::json!({
                    "textDocument": { "uri": uri },
                    "range": {
                        "start": { "line": 0, "character": 0 },
                        "end": { "line": 400, "character": 900 },
                    },
                    "context": { "diagnostics": [] },
                }),
            ),
            request(8, "textDocument/completion", at_position(uri)),
            request(9, "textDocument/hover", at_position(uri)),
            request(10, "textDocument/definition", at_position(uri)),
            request(
                11,
                "textDocument/references",
                serde_json::json!({
                    "textDocument": { "uri": uri },
                    "position": { "line": 0, "character": 0 },
                    "context": { "includeDeclaration": true },
                }),
            ),
            request(
                12,
                "textDocument/documentSymbol",
                serde_json::json!({ "textDocument": { "uri": uri } }),
            ),
            request(
                13,
                "workspace/symbol",
                serde_json::json!({ "query": "anything" }),
            ),
            request(15, "textDocument/prepareRename", at_position(uri)),
            request(
                16,
                "textDocument/rename",
                serde_json::json!({
                    "textDocument": { "uri": uri },
                    "position": { "line": 0, "character": 0 },
                    "newName": "renamed",
                }),
            ),
            // A method the server does not implement is answered, not
            // dropped: a client that never gets a response waits forever.
            request(14, "textDocument/somethingElse", serde_json::json!({})),
        ]
    };

    for messages in [
        // Nothing open at all.
        sweep("file:///never-opened.k816"),
        // A document open and unreadable by the grammar.
        std::iter::once(did_open("}{ ??? &&&\n"))
            .chain(sweep(DOC))
            .collect(),
        // A document the workspace does not hold, while it holds another.
        std::iter::once(did_open("var SCREEN = $0400\n"))
            .chain(sweep("file:///other.k816"))
            .collect(),
    ] {
        let mut all = messages;
        all.extend(shutdown_and_exit());
        let (server, client) = lsp_server::Connection::memory();
        for message in all {
            client
                .sender
                .send(serde_json::from_value(message).expect("a protocol message"))
                .expect("the server is listening");
        }
        let mut workspace = Workspace::new();
        k816::lsp::serve(&server, &mut workspace, &serde_json::Value::Null)
            .expect("the server survives the sweep");
        drop(server);

        let mut answered = Vec::new();
        while let Ok(message) = client.receiver.try_recv() {
            if let lsp_server::Message::Response(response) = message {
                // A `rename` refusal is a response error carrying the
                // compiler's words, which is the protocol's only way to put
                // them in front of the user; every other request answers.
                if let Err(error) = &response.response_result {
                    assert_eq!(
                        format!("{}", response.id),
                        "16",
                        "only a rename refuses: {error:?}"
                    );
                    assert!(!error.message.is_empty(), "a refusal with no words");
                }
                answered.push(format!("{}", response.id));
            }
        }
        // Every request plus the shutdown, each answered exactly once.
        assert_eq!(
            answered,
            [
                "5", "6", "7", "8", "9", "10", "11", "12", "13", "15", "16", "14", "1"
            ],
            "every request is answered, in order"
        );
    }
}

// ---------------------------------------------------------------------------
// H4 — hover
// ---------------------------------------------------------------------------

const HOVER_SOURCE: &str = include_str!("golden/lsp-hover.k816");
const HOVER_DOC: &str = "file:///lsp-hover.k816";

/// The byte offset of the `nth` occurrence of `needle`, plus `into`.
fn offset_of(needle: &str, nth: usize, into: u32) -> u32 {
    let mut at = 0usize;
    for step in 0..=nth {
        at = HOVER_SOURCE[at..]
            .find(needle)
            .map(|found| found + at)
            .unwrap_or_else(|| panic!("`{needle}` occurrence {step} is not in the fixture"));
        if step < nth {
            at += 1;
        }
    }
    at as u32 + into
}

/// The hover Markdown at the `nth` occurrence of `needle`, `into` bytes in.
fn hover_at(options: &Options, needle: &str, nth: usize, into: u32) -> Option<String> {
    let mut workspace = Workspace::with_options(options.clone());
    workspace.update(HOVER_DOC, HOVER_SOURCE);
    let analysis = workspace.analyze();
    let file = workspace.file(HOVER_DOC).expect("open");
    k816::lsp::hover::hover(&analysis, file, offset_of(needle, nth, into))
        .map(|hover| hover.markdown)
}

/// One of every `SymbolKind`, and every fact the model holds about it.
///
/// The fixture is one file so the table has one source, and the assertions
/// are on the *facts* rather than on the whole rendering: a sentence reworded
/// is not a regression, and a number that changed is.
#[test]
fn hover_answers_for_one_symbol_of_every_kind() {
    let table: &[(&str, usize, &[&str])] = &[
        // `const` — a value, no address.
        ("LIMIT = $0F", 0, &["a `const`", "`$000F` (15)"]),
        // A pinned variable and a linker-allocated one.
        (
            "SCREEN = $0400",
            0,
            &["a variable", "At `$0400`", "two operand bytes"],
        ),
        (
            "COUNTER\n",
            0,
            &["a variable", "Allocated by ld65", "Reserves 1 byte"],
        ),
        // The direct page and the long encoding.
        ("CURSOR = $10", 0, &["At `$0010`", "one operand byte"]),
        ("FARDATA = $030000", 0, &["three operand bytes"]),
        // A field layout, with its offsets and its repeat count.
        (
            "PAIR[",
            0,
            &[
                "`* 4` — 4 elements",
                "Reserves 8 byte",
                "- `.lo` at `+0`",
                "- `.hi` at `+1`",
            ],
        ),
        // An `abstract var`: a layout and nothing else.
        (
            "HANDLER[",
            0,
            &[
                "an `abstract var`",
                "No storage",
                "- `.open` at `+0`",
                "- `.close` at `+2`",
            ],
        ),
        // A data label.
        ("message {", 0, &["a data label", "An address ld65 places"]),
        // The three section kinds, their conventions, and their contracts.
        (
            "stash @a8",
            0,
            &[
                "an `inline`",
                "Written `inline`",
                "Declares `@a8`",
                "Register flow `(a, #amount, slot) -> a`",
            ],
        ),
        (
            "banked @a8",
            0,
            &[
                "Written `far func`",
                "called with `JSL`",
                "returns with `RTL`",
            ],
        ),
        (
            "opaque @a8",
            0,
            &[
                "Written `naked`",
                "not classified",
                "**Assumed, not inferred.**",
            ],
        ),
        (
            "settle @a8",
            0,
            &["Written `func`", "the accumulator is unchanged"],
        ),
        // A local label, and the function it is scoped to.
        (".again:", 0, &["a local label", "Scoped to `work`"]),
        // Both alias kinds.
        (
            "amount, slot",
            0,
            &["an `inline` alias", "`#name`, so it stands for a value"],
        ),
        (
            "slot) -> a",
            0,
            &["an `inline` alias", "a bare name, so it stands for a place"],
        ),
        // A `.field` step, which resolves through the layout.
        ("hi\n    a", 0, &["`.hi` — a field", "offset `+1`"]),
    ];

    for (needle, nth, expected) in table {
        let markdown = hover_at(&Options::default(), needle, *nth, 0)
            .unwrap_or_else(|| panic!("no hover at `{needle}`"));
        for fact in *expected {
            assert!(
                markdown.contains(fact),
                "hover at `{needle}` is missing `{fact}`:\n{markdown}"
            );
        }
    }
}

/// One instruction of every operand class, sized against the widths the model
/// tracked *there*.
///
/// The length is the point: `encoded_length` is an upper bound, and `lda #1`
/// is two bytes under `@a8` and three under `@a16`. A hover that answered
/// from the mnemonic alone would be wrong exactly where the language is
/// interesting.
#[test]
fn hover_sizes_an_instruction_against_the_tracked_width() {
    let table: &[(&str, &str, &[&str])] = &[
        // `M`-dependent, at a point the model knows is `@a8`.
        (
            "lda #LIMIT",
            "`lda` — addressing mode `#`",
            &["Encodes to 2 byte", "as the `M` flag selects"],
        ),
        // …and the same mnemonic inside `@a16 { }`, one byte longer.
        (
            "lda #$1234",
            "`lda` — addressing mode `#`",
            &["Encodes to 3 byte", "Sized against `@a16 @i8`"],
        ),
        // `X`-dependent, and a fixed-width immediate.
        (
            "ldx #0",
            "`ldx` — addressing mode `#`",
            &["Encodes to 2 byte", "as the `X` flag selects"],
        ),
        // `SEP`/`REP` derive their effects *from the mask*, and the
        // classifier has already folded it: with the mask withheld the
        // effect table has to answer for every mask at once, and `sep #$20`
        // reports writing nothing definite when what it writes is `M`.
        (
            "sep #$20",
            "`sep` — addressing mode `#`",
            &["Encodes to 2 byte", "always one byte", "writes `M`"],
        ),
        // The three address encodings.
        (
            "lda CURSOR",
            "`lda` — addressing mode `d`",
            &["Encodes to 2 byte"],
        ),
        (
            "sta COUNTER",
            "`sta` — addressing mode `a`",
            &["Encodes to 3 byte"],
        ),
        (
            "lda FARDATA",
            "`lda` — addressing mode `al`",
            &["Encodes to 4 byte"],
        ),
        // No operand, the accumulator, a bank pair, and a displacement.
        (
            "nop\n",
            "`nop` — addressing mode `i`",
            &["Encodes to 1 byte"],
        ),
        (
            "asl a",
            "`asl` — addressing mode `A`",
            &["Encodes to 1 byte", "writes `A, N Z C`"],
        ),
        (
            "mvn $01",
            "`mvn` — addressing mode `xyc`",
            &["Encodes to 3 byte", "reads `A X Y DBR`"],
        ),
        (
            "bra .onwards",
            "`bra` — addressing mode `r`",
            &["Encodes to 2 byte"],
        ),
    ];

    for (needle, headline, expected) in table {
        let markdown = hover_at(&Options::default(), needle, 0, 0)
            .unwrap_or_else(|| panic!("no hover at `{needle}`"));
        // `Reads`/`reads` — the first word of its own line either way.
        let lowered = markdown.to_lowercase();
        assert!(
            markdown.starts_with(headline),
            "hover at `{needle}` does not open with `{headline}`:\n{markdown}"
        );
        for fact in *expected {
            assert!(
                lowered.contains(&fact.to_lowercase()),
                "hover at `{needle}` is missing `{fact}`:\n{markdown}"
            );
        }
    }
}

/// A statement reports what the model believed on either side of it.
#[test]
fn hover_on_a_statement_reports_the_width_state() {
    // `call settle` — the callee's exit is inferred, so the widths do not
    // change across it and the hover says nothing about "after".
    let settled = hover_at(&Options::default(), "call settle", 0, 0).expect("a hover");
    assert!(settled.starts_with("Widths here: `@a8 @i8"), "{settled}");
    assert!(!settled.contains("After this statement"), "{settled}");

    // `call opaque` — the callee runs an `xce`. The widths survive it by the
    // language's rule and the emulation flag does not, so the state after the
    // call differs from the state before it and the hover reports the change.
    let opaque = hover_at(&Options::default(), "call opaque", 0, 0).expect("a hover");
    assert!(opaque.starts_with("Widths here: `@a8 @i8`"), "{opaque}");
    assert!(
        opaque.contains("After this statement: `@a8 @i8, emulation flag unknown`"),
        "{opaque}"
    );

    // Inside a `@a16 { }` block the state is the block's, not the section's:
    // `widen` declares `@a8` and the statement inside its block is `@a16`.
    let inside = hover_at(&Options::default(), "lda #$1234", 0, 0).expect("a hover");
    assert!(inside.contains("Sized against `@a16 @i8`"), "{inside}");
    let around = hover_at(&Options::default(), "widen @a8", 0, 0).expect("a hover");
    assert!(around.contains("Declares `@a8 @i8`"), "{around}");
}

/// A hover on `main` says which machine the *build* states — the one fact
/// about the entry point that is nowhere in the source.
#[test]
fn hover_on_main_states_the_machine_the_build_stated() {
    let default = hover_at(&Options::default(), "main @a8", 0, 0).expect("a hover");
    assert!(default.contains("**The entry point.**"), "{default}");
    assert!(default.contains("`native,a8,i8`"), "{default}");

    // The same source, a different build: the hover follows the option, which
    // is H0's seam observed from a second place.
    let emulation = Options {
        entry: EntryEnvironment::parse("emulation").expect("a machine that exists"),
        ..Options::default()
    };
    let stated = hover_at(&emulation, "main @a8", 0, 0).expect("a hover");
    assert!(stated.contains("`emulation,a8,i8`"), "{stated}");
    assert_ne!(default, stated);
}

/// Whitespace is not a construct, so it is not a question.
#[test]
fn hover_answers_nothing_where_the_model_holds_nothing() {
    // Column 0 of a body line, between the brace above and the token right.
    assert_eq!(
        hover_at(&Options::default(), "\n    lda #LIMIT", 0, 1),
        None
    );
    // Inside a comment.
    assert_eq!(hover_at(&Options::default(), "// A `const`", 0, 3), None);
    // Past the end of the file.
    let mut workspace = Workspace::new();
    workspace.update(HOVER_DOC, HOVER_SOURCE);
    let analysis = workspace.analyze();
    let file = workspace.file(HOVER_DOC).expect("open");
    assert_eq!(k816::lsp::hover::hover(&analysis, file, 9_999_999), None);
}

// ---------------------------------------------------------------------------
// H5 — semantic rename
// ---------------------------------------------------------------------------

const RENAME_SOURCE: &str = include_str!("golden/lsp-rename.k816");
const RENAME_DOC: &str = "file:///lsp-rename.k816";

/// The rename fixture, analyzed.
fn rename_workspace() -> Workspace {
    let mut workspace = Workspace::new();
    workspace.update(RENAME_DOC, RENAME_SOURCE);
    workspace
}

/// The spans a rename at `offset` would replace, as `(file, text)` pairs.
fn rename_edits(
    workspace: &mut Workspace,
    doc: &str,
    offset: u32,
    to: &str,
) -> Result<(String, Vec<Span>), String> {
    let analysis = workspace.analyze();
    let file = workspace.file(doc).expect("open");
    let renamed = k816::lsp::rename::rename(&analysis, file, offset, to)?;
    let spans: Vec<Span> = renamed.spans().collect();
    Ok((renamed.replacement, spans))
}

/// The decision-8 fixture: one identifier text, five different things, and a
/// rename that touches exactly the semantic occurrences.
///
/// `state` is a local label in `first`, a local label in `second`, a field of
/// `TASKS`, a word inside a string, and a word inside a comment. A textual
/// rename changes all of them.
#[test]
fn rename_touches_the_occurrences_the_compiler_resolved_and_no_others() {
    let mut workspace = rename_workspace();

    // From `first`'s local label: its declaration and its one `goto`.
    let (replacement, spans) =
        rename_edits(&mut workspace, RENAME_DOC, 564, "spin").expect("a rename");
    assert_eq!(replacement, ".spin", "a local label keeps its leading `.`");
    assert_eq!(spans.len(), 2);
    // Both inside `first`, which ends before `second` begins.
    let second_label = 825;
    assert!(
        spans.iter().all(|span| span.start < second_label),
        "a reference escaped into the second function: {spans:?}"
    );

    // From `second`'s: a different symbol, a different pair of spans.
    let (_, other) = rename_edits(&mut workspace, RENAME_DOC, 826, "spin").expect("a rename");
    assert_eq!(other.len(), 2);
    assert!(other.iter().all(|span| span.start >= second_label));
    assert!(spans.iter().all(|span| !other.contains(span)));

    // From the field: its declaration and both `.state` selections, and
    // nothing in the string or the comment.
    let (replacement, fields) =
        rename_edits(&mut workspace, RENAME_DOC, 506, "phase").expect("a rename");
    assert_eq!(replacement, ".phase", "a field keeps its leading `.`");
    assert_eq!(fields.len(), 3);
    let text = |span: &Span| workspace.map().span_text(*span).to_owned();
    assert!(fields.iter().all(|span| text(span) == ".state"));

    // The string and the comment are untouched by all three.
    let string_at = 968;
    let comment_at = 83;
    for span in spans.iter().chain(&other).chain(&fields) {
        assert_ne!(span.start, string_at, "the string was renamed");
        assert_ne!(span.start, comment_at, "the comment was renamed");
    }
}

/// Renaming an imported symbol edits its definition and every importer.
#[test]
fn rename_of_an_imported_symbol_edits_both_units() {
    let mut workspace = cross_unit_workspace();
    let (a, b) = (
        workspace.file(UNIT_A).expect("open"),
        workspace.file(UNIT_B).expect("open"),
    );

    // From the *use* in unit A, at the `lda CURSOR` on its line.
    let use_site = at(UNIT_A_TEXT, "lda CURSOR", 4);
    let (replacement, spans) =
        rename_edits(&mut workspace, UNIT_A, use_site, "SCRATCH").expect("a rename");
    assert_eq!(replacement, "SCRATCH");
    assert_eq!(spans.len(), 3, "the declaration in B and both uses in A");
    assert_eq!(spans.iter().filter(|span| span.file == b).count(), 1);
    assert_eq!(spans.iter().filter(|span| span.file == a).count(), 2);
    for span in &spans {
        assert_eq!(workspace.map().span_text(*span), "CURSOR");
    }
}

/// The editor and the build report the same diagnostics, over every program
/// in the tree.
///
/// Decision 1's property, checked rather than intended. It was not true: the
/// editor's analysis stopped after the semantic pass and the build lowered,
/// so every diagnostic lowering produces was invisible in the editor —
/// including *errors*. An `inline`'s arguments are checked where it is
/// expanded, which is lowering, so `shift a, #SYM -> a` with a link-time
/// argument showed a clean file and failed the next build.
///
/// The sweep is the point. Two hand-written cases pin the two errors that
/// found this, and the corpus is what stops a *new* lowering diagnostic from
/// reopening the gap without anyone noticing — the two warnings it caught
/// (`K816-W0123`, `K816-W0157`) were nobody's deliberate choice either.
#[test]
fn the_editor_and_the_build_report_the_same_diagnostics() {
    use std::collections::BTreeSet;

    let both = |name: &str, text: &str| -> (BTreeSet<String>, BTreeSet<String>) {
        let mut map = SourceMap::new();
        let id = map.add_file(name, text);
        let cli = k816::compile_to_ca65(&map, id, &k816::Options::default())
            .diagnostics
            .iter()
            .map(|diagnostic| diagnostic.code.0.to_owned())
            .collect();
        let mut workspace = Workspace::new();
        workspace.update(format!("file:///{name}"), text);
        let lsp = workspace
            .analyze()
            .diagnostics
            .iter()
            .map(|diagnostic| diagnostic.code.0.to_owned())
            .collect();
        (cli, lsp)
    };

    let mut disagreed = Vec::new();
    let mut checked = 0;
    let golden = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/golden");
    let mut entries: Vec<std::path::PathBuf> = std::fs::read_dir(&golden)
        .expect("read tests/golden")
        .filter_map(|entry| entry.ok())
        .map(|entry| entry.path())
        .filter(|path| {
            path.extension()
                .is_some_and(|extension| matches!(extension.to_str(), Some("k816" | "k65" | "kk")))
        })
        .collect();
    entries.sort();
    // The two that found it: an argument ld65 decides, and one that overruns
    // the narrowing its parameter declares. Both are reported by `lower.rs`,
    // where an `inline` is expanded.
    let hand: &[(&str, &str)] = &[
        (
            "lsp-agree-extern-argument.k816",
            "extern const SYM\n\ninline shift (a, #n) -> a {\n    lsr * n\n}\n\n\
             func main @a8 @i8 {\n    shift a, #SYM -> a\n}\n",
        ),
        (
            "lsp-agree-narrowed-argument.k816",
            "inline scale @a16 (a, #by:byte) -> a {\n  a & #by\n}\n\n\
             func work @a16 @i8 {\n  a = #$FF\n  scale a, #$1234 -> a\n}\n",
        ),
    ];
    for (name, text) in hand {
        let (cli, lsp) = both(name, text);
        checked += 1;
        assert!(
            !cli.is_empty(),
            "`{name}` is here because the build refuses it"
        );
        if cli != lsp {
            disagreed.push(format!("  {name}\n    build: {cli:?}\n    editor: {lsp:?}"));
        }
    }
    for path in entries {
        let name = path
            .file_name()
            .expect("a file")
            .to_string_lossy()
            .to_string();
        let text = std::fs::read_to_string(&path).expect("read a golden");
        let (cli, lsp) = both(&name, &text);
        checked += 1;
        if cli != lsp {
            disagreed.push(format!("  {name}\n    build: {cli:?}\n    editor: {lsp:?}"));
        }
    }
    assert!(checked > 40, "the sweep found only {checked} programs");
    assert!(
        disagreed.is_empty(),
        "the editor and the build disagree about {} of {checked} programs:\n{}",
        disagreed.len(),
        disagreed.join("\n")
    );
}

/// Every refusal decision 8 names, in the compiler's own words where it has
/// them.
#[test]
fn rename_refuses_what_the_compiler_would_refuse() {
    let mut workspace = rename_workspace();
    let refuse = |workspace: &mut Workspace, offset, to: &str| {
        rename_edits(workspace, RENAME_DOC, offset, to)
            .expect_err("this rename should have been refused")
    };

    // A reserved spelling, worded by `compat::reserved_name_refusal` itself.
    // Asked of a *unit-level* name, because that is where the compiler
    // reserves them: `TASKS` at 494.
    let refusal = refuse(&mut workspace, 494, "x");
    assert_eq!(
        refusal,
        k816::compat::reserved_name_refusal("x").expect("`x` is reserved")
    );
    assert!(refusal.contains("names a register"), "{refusal}");
    assert!(refuse(&mut workspace, 494, "c").contains("names a flag"));
    assert!(refuse(&mut workspace, 494, "lda").contains("is a 65816 instruction"));

    // …and *not* of a dotted one. A local label and a field keep their `.`,
    // so `.x` is not the register spelling and the compiler takes it —
    // `var pos[ .x :byte .y :byte ]` is how the original writes a point. The
    // editor declined that rename until the question was asked about the
    // replacement rather than about the name with its dot stripped off.
    for offset in [506, 564] {
        for to in [".x", "x", ".c", ".lda"] {
            rename_edits(&mut workspace, RENAME_DOC, offset, to)
                .unwrap_or_else(|e| panic!("renaming at {offset} to `{to}`: {e}"));
        }
    }

    // A collision inside the owner's scope: `first` already has `.state`.
    let refusal = refuse(&mut workspace, 564, "state");
    assert!(refusal.contains("is already its name"), "{refusal}");

    // A collision with a unit-level name, in the duplicate-declaration
    // wording the build would use.
    let mut cross = cross_unit_workspace();
    let refusal = rename_edits(
        &mut cross,
        UNIT_A,
        at(UNIT_A_TEXT, "lda CURSOR", 4),
        "BANKED",
    )
    .expect_err("`BANKED` is taken by the other unit");
    assert!(
        refusal.starts_with(&k816::semantic::duplicate_symbol_title("BANKED")),
        "{refusal}"
    );

    // A field's scope is its own layout, and `TASKS` already has `.other`.
    // This was the one target that reached the edits with no collision check
    // at all, so the rename was offered and the next build refused it.
    let refusal = refuse(&mut workspace, 506, "other");
    assert!(
        refusal.contains("declared twice in this field list"),
        "{refusal}"
    );
    // A field of a *different* layout is not a collision.
    let mut two = Workspace::new();
    two.update(
        "file:///two.k816",
        "var LEFT[ .state :byte ]\nvar RIGHT[ .other :byte ]\n",
    );
    let (_, spans) = rename_edits(&mut two, "file:///two.k816", 11, "other")
        .expect("a different layout's field is not a collision");
    assert_eq!(spans.len(), 1);

    // Not a name at all: a mnemonic, a register token, and whitespace.
    let mut hover = Workspace::new();
    hover.update(HOVER_DOC, HOVER_SOURCE);
    for (needle, into) in [("lda #LIMIT", 1u32), ("asl a", 4), ("\n    nop", 0)] {
        let offset = offset_of(needle, 0, into);
        let refusal =
            rename_edits(&mut hover, HOVER_DOC, offset, "thing").expect_err("this is not a name");
        assert!(
            refusal.contains("nothing here that K816 resolved"),
            "at `{needle}`: {refusal}"
        );
    }

    // An `inline` alias binds positionally, so renaming it is renaming the
    // contract; refused rather than reasoned about.
    let alias = offset_of("amount, slot", 0, 0);
    assert!(
        rename_edits(&mut hover, HOVER_DOC, alias, "howmuch")
            .expect_err("an alias is refused")
            .contains("nothing here that K816 resolved")
    );

    // A name that is not an identifier.
    for bad in ["9lives", "has-dash", "", "  "] {
        assert!(
            rename_edits(&mut workspace, RENAME_DOC, 564, bad).is_err(),
            "`{bad}` was accepted as a name"
        );
    }
}

/// `prepareRename` answers exactly where `rename` would work.
///
/// The two share one gate, so an editor never offers a rename box for
/// something the rename would then refuse.
#[test]
fn prepare_rename_answers_where_a_rename_would_work() {
    use k816::lsp::rename::prepare;

    let mut workspace = rename_workspace();
    let analysis = workspace.analyze();
    let file = workspace.file(RENAME_DOC).expect("open");

    // On a name: the span whose text the editor offers.
    let span = prepare(&analysis, file, 564).expect("a local label is renameable");
    assert_eq!(workspace.map().span_text(span), ".state");
    let span = prepare(&analysis, file, 506).expect("a field is renameable");
    assert_eq!(workspace.map().span_text(span), ".state");
    assert!(prepare(&analysis, file, 495).is_some(), "`TASKS` is a name");

    // Inside the string and inside the comment: nothing.
    assert_eq!(prepare(&analysis, file, 969), None, "inside a string");
    assert_eq!(prepare(&analysis, file, 84), None, "inside a comment");

    // A document the workspace does not hold, and a position past the end.
    assert_eq!(prepare(&analysis, file, 9_999_999), None);
}

/// A rename applied and re-analyzed produces the same diagnostics.
///
/// This is what makes "semantic, not textual" a property rather than a claim:
/// if the rename had touched a string, a comment, or another function's
/// label, the program would compile to something else — or stop compiling.
#[test]
fn applying_a_rename_leaves_the_program_saying_the_same_things() {
    for (offset, to) in [(564u32, "spin"), (506, "phase"), (495, "SLOTS")] {
        let mut workspace = rename_workspace();
        let before: Vec<String> = workspace
            .analyze()
            .diagnostics
            .iter()
            .map(|diagnostic| diagnostic.code.as_str().to_owned())
            .collect();

        let (replacement, mut spans) =
            rename_edits(&mut workspace, RENAME_DOC, offset, to).expect("a rename");
        // Applied back to front so an earlier edit does not move a later one.
        spans.sort_by_key(|span| std::cmp::Reverse(span.start));
        let mut text = RENAME_SOURCE.to_owned();
        for span in &spans {
            text.replace_range(span.start as usize..span.end as usize, &replacement);
        }
        assert!(text.contains(&replacement), "the rename changed nothing");

        let mut renamed = Workspace::new();
        renamed.update(RENAME_DOC, text.clone());
        let after: Vec<String> = renamed
            .analyze()
            .diagnostics
            .iter()
            .map(|diagnostic| diagnostic.code.as_str().to_owned())
            .collect();
        assert_eq!(before, after, "renaming to `{to}` changed the diagnostics");

        // And the assembly is the same program, modulo the name itself.
        let compile = |text: &str| {
            let mut map = SourceMap::new();
            let file = map.add_file("r.k816", text);
            compile_to_ca65(&map, file, &Options::default())
                .assembly
                .expect("it compiles")
        };
        let original = compile(RENAME_SOURCE);
        let renamed = compile(&text);
        assert_eq!(
            original.lines().count(),
            renamed.lines().count(),
            "renaming to `{to}` changed the emitted program"
        );
    }
}

// ---------------------------------------------------------------------------
// H6 — completion
// ---------------------------------------------------------------------------

/// The candidates at an offset of `source`, as `(label, inserted text)`.
fn candidates(source: &str, offset: u32, trigger: Option<char>) -> Vec<(String, String)> {
    let mut workspace = Workspace::new();
    let uri = "file:///completion.k816";
    workspace.update(uri, source);
    let analysis = workspace.analyze();
    let file = workspace.file(uri).expect("open");
    k816::lsp::completion::complete(&analysis, file, offset, trigger)
        .items
        .into_iter()
        .map(|candidate| {
            let inserted = candidate.insert.unwrap_or_else(|| candidate.label.clone());
            (candidate.label, inserted)
        })
        .collect()
}

/// The offset just after the first occurrence of `needle`.
fn after(source: &str, needle: &str) -> u32 {
    (source
        .find(needle)
        .unwrap_or_else(|| panic!("`{needle}` is not in the source"))
        + needle.len()) as u32
}

/// A statement position offers every mnemonic, the statement keywords, the
/// unit's names, and the enclosing function's local labels — and nothing that
/// exists only in a comment or a string.
#[test]
fn a_statement_position_offers_what_may_stand_there() {
    const SOURCE: &str = "// commentonly is written here and nowhere else\n\
        var SCREEN = $0400\n\
        const LIMIT = 8\n\
        \n\
        func other @a8 @i8 {\n\
        \x20   .elsewhere:\n\
        \x20   nop\n\
        \x20   != goto .elsewhere\n\
        }\n\
        \n\
        func work @a8 @i8 {\n\
        \x20   .again:\n\
        \x20   \n\
        \x20   != goto .again\n\
        }\n\
        \n\
        data words {\n\
        \x20   \"stringonly\"\n\
        }\n";

    let offset = after(SOURCE, ".again:\n    ");
    let found = candidates(SOURCE, offset, None);
    let labels: Vec<&str> = found.iter().map(|(label, _)| label.as_str()).collect();

    // Every mnemonic the ISA table has, and no fewer.
    for mnemonic in k816::isa::Mnemonic::ALL {
        assert!(
            labels.contains(&mnemonic.ca65()),
            "`{}` is not offered at a statement start",
            mnemonic.ca65()
        );
    }
    // Every statement keyword, and every unit-level name.
    for keyword in k816::lsp::completion::STATEMENT_KEYWORDS {
        assert!(labels.contains(keyword), "`{keyword}` is not offered");
    }
    for name in ["SCREEN", "LIMIT", "other", "work", "words"] {
        assert!(labels.contains(&name), "`{name}` is not offered");
    }
    // The register spellings a statement may open with: an HLA statement
    // starts with one, so a statement position without them would be missing
    // the language's commonest first token.
    for register in ["a", "x", "y", "dpr"] {
        assert!(labels.contains(&register), "`{register}` is not offered");
    }
    // This function's local label, and not the other function's.
    assert!(labels.contains(&".again"), "the enclosing label is missing");
    assert!(
        !labels.contains(&".elsewhere"),
        "another function's local label leaked in"
    );
    // A word that exists only in a comment or only in a string was never
    // declared, so it is not a candidate — and nothing filters it out.
    assert!(!labels.contains(&"commentonly"));
    assert!(!labels.contains(&"stringonly"));
}

/// An empty document offers the item keywords, and nothing else.
#[test]
fn an_empty_document_offers_the_item_keywords() {
    let found = candidates("", 0, None);
    let labels: Vec<String> = found.into_iter().map(|(label, _)| label).collect();
    let expected: Vec<String> = k816::lsp::completion::ITEM_KEYWORDS
        .iter()
        .map(|keyword| (*keyword).to_owned())
        .collect();
    assert_eq!(labels, expected);
}

/// After `.` on a laid-out variable: exactly that layout's fields.
#[test]
fn a_field_step_offers_exactly_that_layouts_fields() {
    const SOURCE: &str = "var PAIR[ .lo :byte .hi :byte ]\n\
        var TRIPLE[ .one :byte .two :byte .three :byte ]\n\
        func work @a8 @i8 {\n\
        \x20   a = PAIR.\n\
        }\n";

    let found = candidates(SOURCE, after(SOURCE, "a = PAIR."), Some('.'));
    let labels: Vec<String> = found.into_iter().map(|(label, _)| label).collect();
    assert_eq!(
        labels,
        [".lo", ".hi"],
        "the other layout's fields leaked in"
    );
}

/// After `.` where no variable precedes it: the enclosing function's local
/// labels, which is the language's *other* meaning for a leading dot.
#[test]
fn a_bare_dot_offers_the_enclosing_functions_local_labels() {
    const SOURCE: &str = "func other @a8 @i8 {\n\
        .elsewhere:\n\
        \x20   nop\n\
        \x20   != goto .elsewhere\n\
        }\n\
        \n\
        func work @a8 @i8 {\n\
        .again:\n\
        \x20   goto .\n\
        }\n";

    // The needle has to be unique: `other`'s `!= goto .elsewhere` contains
    // `goto .` too.
    let found = candidates(SOURCE, after(SOURCE, "\n    goto ."), Some('.'));
    let labels: Vec<String> = found.into_iter().map(|(label, _)| label).collect();
    assert_eq!(labels, [".again"]);
}

/// After `@`: exactly the four width annotations.
#[test]
fn an_annotation_position_offers_exactly_the_four_annotations() {
    let source = "func work @";
    let found = candidates(source, after(source, "@"), Some('@'));
    let labels: Vec<String> = found.into_iter().map(|(label, _)| label).collect();
    assert_eq!(labels, ["@a8", "@a16", "@i8", "@i16"]);
}

/// In an operand, a `#` is offered only where the ISA admits an immediate.
///
/// `lda #K` is a load and `sta #K` is not a store, so offering the second
/// would trade one error for another — the same reasoning `resolve.rs` uses
/// when it decides whether to *fix* a `const` in address position.
#[test]
fn an_immediate_is_offered_only_where_the_isa_admits_one() {
    const SOURCE: &str = "var SCREEN = $0400\n\
        const LIMIT = 8\n\
        func work @a8 @i8 {\n\
        \x20   lda LIM\n\
        \x20   sta LIM\n\
        }\n";

    let loading = candidates(SOURCE, after(SOURCE, "lda LIM"), None);
    assert!(
        loading.contains(&("LIMIT".to_owned(), "#LIMIT".to_owned())),
        "`lda` should offer `#LIMIT`: {loading:?}"
    );

    let storing = candidates(SOURCE, after(SOURCE, "sta LIM"), None);
    assert!(
        storing.iter().any(|(label, _)| label == "LIMIT"),
        "`sta` should still offer the name: {storing:?}"
    );
    assert!(
        !storing
            .iter()
            .any(|(_, inserted)| inserted.starts_with('#')),
        "`sta` has no immediate form, so nothing may be offered with a `#`: {storing:?}"
    );

    // After a `#` the user has already committed to an immediate, so the
    // candidates are the values, each with the marker they follow.
    let immediate = candidates(SOURCE, after(SOURCE, "lda LIM"), Some('#'));
    assert!(
        immediate
            .iter()
            .all(|(_, inserted)| inserted.starts_with('#')),
        "{immediate:?}"
    );
    assert!(immediate.iter().any(|(label, _)| label == "LIMIT"));
    assert!(immediate.iter().any(|(label, _)| label == "SCREEN"));
}

/// After `:` — the data widths and the two metadata queries.
///
/// Not one of decision 9's four contexts; added because the legacy server
/// offered it and these are the language's own spellings (probe 11).
#[test]
fn a_colon_offers_the_widths_and_the_metadata_queries() {
    const SOURCE: &str = "var PAIR[ .lo :byte ]\n\
        func work @a8 @i8 {\n\
        \x20   a = PAIR:\n\
        }\n";
    let found = candidates(SOURCE, after(SOURCE, "PAIR:"), Some(':'));
    let labels: Vec<String> = found.into_iter().map(|(label, _)| label).collect();
    assert_eq!(
        labels,
        [":byte", ":word", ":far", ":sizeof", ":offsetof"],
        "a colon suffix is labelled with its `:`, like `.lo` and `@a8`"
    );
}

/// Every keyword the server offers is one the grammar has.
///
/// The lists are written out — a grammar rule is a shape, not a list of what
/// a user may usefully type next — so this is what stops them naming a
/// keyword the language does not have, or keeping one it removed.
#[test]
fn every_offered_keyword_is_a_literal_the_grammar_has() {
    let grammar = std::fs::read_to_string(
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("tree-sitter-k65/src/grammar.json"),
    )
    .expect("read the generated grammar");
    let grammar: serde_json::Value = serde_json::from_str(&grammar).expect("valid JSON");

    let mut literals = std::collections::BTreeSet::new();
    fn walk(value: &serde_json::Value, literals: &mut std::collections::BTreeSet<String>) {
        match value {
            serde_json::Value::Object(map) => {
                if map.get("type").and_then(|t| t.as_str()) == Some("STRING")
                    && let Some(text) = map.get("value").and_then(|v| v.as_str())
                {
                    literals.insert(text.to_owned());
                }
                for nested in map.values() {
                    walk(nested, literals);
                }
            }
            serde_json::Value::Array(items) => {
                for item in items {
                    walk(item, literals);
                }
            }
            _ => {}
        }
    }
    walk(&grammar["rules"], &mut literals);
    assert!(literals.len() > 100, "the grammar walk found nothing");

    for keyword in k816::lsp::completion::ITEM_KEYWORDS
        .iter()
        .chain(k816::lsp::completion::STATEMENT_KEYWORDS)
        .chain(k816::lsp::completion::COLON_SUFFIXES)
    {
        assert!(
            literals.contains(*keyword),
            "`{keyword}` is offered and the grammar has no such literal"
        );
    }
    // The registers, whose spellings are `RegisterKind`'s own.
    for register in k816::lsp::completion::REGISTERS {
        assert!(
            literals.contains(register.k65()),
            "`{}` is offered and the grammar has no such literal",
            register.k65()
        );
    }
    // The annotations are spelled with their `@` in the grammar.
    for annotation in k816::lsp::completion::ANNOTATIONS {
        assert!(
            literals.contains(&format!("@{annotation}")),
            "`@{annotation}` is offered and the grammar has no such literal"
        );
    }
}

// ---------------------------------------------------------------------------
// H7 — code actions
// ---------------------------------------------------------------------------

/// Every fix a diagnostic carries becomes an action, and none is filtered.
///
/// The `isPreferred` flag is the applicability: a `MachineApplicable` fix is
/// one an editor may apply on a bare Quick Fix, and a `MaybeIncorrect` one is
/// not.
#[test]
fn every_fix_becomes_an_action_with_its_applicability() {
    use k816::diagnostic::Applicability;
    use k816::lsp::actions::actions;

    // One source with a machine-applicable fix, and one with a plausible one.
    for (source, title, preferred) in [
        (
            "const K = $10\nfunc main @a8 @i8 {\n    lda K\n    return\n}\n",
            "load the value with `#K`",
            true,
        ),
        (
            "func w @a8 @a16 @i8 {\n    return\n}\n",
            "drop the contradicting annotation",
            false,
        ),
    ] {
        let mut workspace = Workspace::new();
        let uri = "file:///actions.k816";
        workspace.update(uri, source);
        let analysis = workspace.analyze();
        let file = workspace.file(uri).expect("open");

        let whole = Span::new(file, 0, source.len() as u32);
        let found = actions(&analysis.diagnostics, whole);
        let offered: Vec<&str> = found
            .iter()
            .map(|action| action.fix.title.as_str())
            .collect();
        assert!(offered.contains(&title), "{offered:?}");

        let action = found
            .iter()
            .find(|action| action.fix.title == title)
            .expect("the fix is offered");
        assert_eq!(action.preferred, preferred);
        assert_eq!(
            action.fix.applicability == Applicability::MachineApplicable,
            preferred
        );
        assert!(!action.fix.edits.is_empty());
        // The diagnostic comes with it, so the editor shows what it answers.
        assert!(action.diagnostic.fixes.iter().any(|fix| fix.title == title));
    }
}

/// A range meets a diagnostic it overlaps, and a diagnostic with no fixes
/// yields no action.
#[test]
fn actions_are_offered_by_overlap_and_only_where_a_fix_exists() {
    use k816::lsp::actions::actions;

    // `SCREEN` is undeclared: a diagnostic, and no fix for it.
    const SOURCE: &str = "func main @a8 @i8 {\n    SCREEN = a\n    return\n}\n";
    let mut workspace = Workspace::new();
    let uri = "file:///nofix.k816";
    workspace.update(uri, SOURCE);
    let analysis = workspace.analyze();
    let file = workspace.file(uri).expect("open");

    assert_eq!(analysis.diagnostics.len(), 1);
    assert!(analysis.diagnostics[0].fixes.is_empty());
    let whole = Span::new(file, 0, SOURCE.len() as u32);
    assert!(actions(&analysis.diagnostics, whole).is_empty());

    // Overlap: a caret anywhere in the diagnostic's span, including at either
    // end of it, offers the fix.
    const FIXABLE: &str = "const K = $10\nfunc main @a8 @i8 {\n    lda K\n    return\n}\n";
    workspace.update(uri, FIXABLE);
    let analysis = workspace.analyze();
    let file = workspace.file(uri).expect("open");
    let at = FIXABLE.find("lda K").expect("the load") as u32 + 4;
    for caret in [at, at + 1] {
        let range = Span::new(file, caret, caret);
        assert_eq!(
            actions(&analysis.diagnostics, range).len(),
            1,
            "no action at byte {caret}"
        );
    }
    // A caret far away offers nothing.
    let elsewhere = Span::new(file, 0, 1);
    assert!(actions(&analysis.diagnostics, elsewhere).is_empty());
}

/// A published diagnostic says how many fixes it carries, so a client can
/// draw its lightbulb without a round trip.
#[test]
fn a_published_diagnostic_says_whether_it_has_fixes() {
    let mut workspace = Workspace::new();
    let uri = "file:///data.k816";
    workspace.update(
        uri,
        "const K = $10\nfunc main @a8 @i8 {\n    lda K\n    return\n}\n",
    );
    let analysis = workspace.analyze();
    let file = workspace.file(uri).expect("open");
    let published = file_diagnostics(workspace.map(), file, &analysis.diagnostics);
    assert_eq!(published.len(), 1);
    assert_eq!(published[0].data, Some(serde_json::json!({ "fixes": 1 })));

    // A diagnostic with no fix carries nothing rather than a zero.
    workspace.update(uri, "func main @a8 @i8 {\n    SCREEN = a\n    return\n}\n");
    let analysis = workspace.analyze();
    let file = workspace.file(uri).expect("open");
    let published = file_diagnostics(workspace.map(), file, &analysis.diagnostics);
    assert_eq!(published[0].data, None);
}

// ---------------------------------------------------------------------------
// H8b — semantic tokens, derived from the highlighting query
// ---------------------------------------------------------------------------

/// The tokens for `source`, decoded back into `(span, type, modifiers)`.
fn decoded_tokens(source: &str) -> Vec<(Span, String, Vec<String>)> {
    use k816::lsp::tokens;

    let mut workspace = Workspace::new();
    let uri = "file:///tokens.k816";
    workspace.update(uri, source);
    let analysis = workspace.analyze();
    let file = workspace.file(uri).expect("open");
    let types = tokens::token_types();
    let modifiers = tokens::token_modifiers();

    tokens::tokens(&analysis, file, workspace.map().file(file))
        .into_iter()
        .map(|token| {
            let named: Vec<String> = modifiers
                .iter()
                .enumerate()
                .filter(|(bit, _)| token.modifiers & (1 << bit) != 0)
                .map(|(_, modifier)| modifier.as_str().to_owned())
                .collect();
            (
                token.span,
                types[token.token_type as usize].as_str().to_owned(),
                named,
            )
        })
        .collect()
}

/// The token stream is the highlighting query's captures, refined where the
/// symbol table knows more than the grammar can.
///
/// This is what makes the second format *derived* from the first rather than
/// a second description of the same thing (decision 6): every token comes
/// from a capture, and the only differences are refinements the compiler
/// resolved.
#[test]
fn the_token_stream_is_the_query_refined_by_the_symbol_table() {
    use k816::parser::{Query, parse_source};

    let source = include_str!("golden/slice.k816");
    let highlights = std::fs::read_to_string(
        std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("tree-sitter-k65/queries/highlights.scm"),
    )
    .expect("read highlights.scm");
    let query = Query::compile(&highlights).expect("the query compiles");

    let mut map = SourceMap::new();
    let file = map.add_file("slice.k816", source);
    let parse = parse_source(map.file(file), None);
    let captured = query.captures(&parse.tree, file, source);

    let tokens = decoded_tokens(source);
    assert!(!tokens.is_empty());

    // Every token sits exactly where a capture did.
    for (span, _, _) in &tokens {
        assert!(
            captured.iter().any(|capture| capture.span == *span),
            "a token at {span:?} that no capture produced"
        );
    }
    // Nothing overlaps, and everything is in source order — the protocol
    // requires both.
    for pair in tokens.windows(2) {
        assert!(pair[0].0.end <= pair[1].0.start, "{pair:?}");
    }

    // The refinements, listed. `main` is captured `@variable` by the general
    // `(identifier)` pattern and `@function` by the specific one; `SCREEN`'s
    // declaring occurrence carries `declaration`.
    let by_text = |needle: &str, nth: usize| {
        let mut at = 0usize;
        for _ in 0..=nth {
            at = source[at..]
                .find(needle)
                .map(|found| found + at)
                .expect(needle)
                + 1;
        }
        (at - 1) as u32
    };
    let token_at = |offset: u32| {
        tokens
            .iter()
            .find(|(span, _, _)| span.contains(offset))
            .unwrap_or_else(|| panic!("no token at {offset}"))
            .clone()
    };

    let (_, kind, modifiers) = token_at(by_text("SCREEN", 0));
    assert_eq!(kind, "variable");
    assert_eq!(modifiers, ["declaration"], "the declaring occurrence");
    let (_, kind, modifiers) = token_at(by_text("SCREEN", 1));
    assert_eq!(kind, "variable");
    assert!(modifiers.is_empty(), "a use carries no declaration");
    let (_, kind, modifiers) = token_at(by_text("main", 0));
    assert_eq!(kind, "function", "refined from `@variable` by the query");
    assert_eq!(modifiers, ["declaration"]);
}

/// A `const` carries `readonly`, which is the second and last modifier.
#[test]
fn a_constant_is_marked_readonly_and_nothing_else_is() {
    const SOURCE: &str = "const LIMIT = 8\nvar SCREEN = $0400\nfunc main @a8 @i8 {\n    lda #LIMIT\n    sta SCREEN\n    return\n}\n";
    let tokens = decoded_tokens(SOURCE);
    let modifiers_of = |needle: &str, nth: usize| {
        let mut at = 0usize;
        for _ in 0..=nth {
            at = SOURCE[at..].find(needle).map(|f| f + at).expect(needle) + 1;
        }
        let offset = (at - 1) as u32;
        tokens
            .iter()
            .find(|(span, _, _)| span.contains(offset))
            .unwrap_or_else(|| panic!("no token at `{needle}`"))
            .2
            .clone()
    };

    assert_eq!(modifiers_of("LIMIT", 0), ["declaration", "readonly"]);
    assert_eq!(modifiers_of("LIMIT", 1), ["readonly"], "a use of a `const`");
    assert_eq!(modifiers_of("SCREEN", 1), Vec::<String>::new());
    // Nothing else carries either modifier.
    for (span, _, modifiers) in &tokens {
        if modifiers.contains(&"readonly".to_owned()) {
            assert_eq!(
                &SOURCE[span.start as usize..span.end as usize],
                "LIMIT",
                "only a `const` is readonly"
            );
        }
    }
}

/// The encoding is relative, per line, and counts UTF-16 units.
#[test]
fn the_encoding_is_relative_and_counts_utf16_units() {
    use k816::lsp::tokens;

    // `ż` is two bytes and one unit; `𝕏` is four bytes and two.
    const SOURCE: &str = "// żab 𝕏\nvar SCREEN = $0400\n";
    let mut workspace = Workspace::new();
    let uri = "file:///utf16.k816";
    workspace.update(uri, SOURCE);
    let analysis = workspace.analyze();
    let file = workspace.file(uri).expect("open");
    let source = workspace.map().file(file);
    let found = tokens::tokens(&analysis, file, source);
    let encoded = tokens::encode(&found, source);

    assert!(!encoded.is_empty());
    // The comment is the first token: line 0, column 0, and its length is
    // the line's UTF-16 length — 9 units for eleven bytes.
    assert_eq!(encoded[0].delta_line, 0);
    assert_eq!(encoded[0].delta_start, 0);
    assert_eq!(encoded[0].length, 9, "`// żab 𝕏` is nine UTF-16 units");
    // The next token is on the following line, so its start is absolute.
    assert_eq!(encoded[1].delta_line, 1);
    assert_eq!(encoded[1].delta_start, 0);
}

/// A document the grammar could not read still returns tokens for what
/// parsed (decision 18).
#[test]
fn a_broken_document_still_returns_tokens_for_what_parsed() {
    const SOURCE: &str = "var SCREEN = $0400\n}{ ??? &&&\nconst LIMIT = 8\n";
    let tokens = decoded_tokens(SOURCE);
    let texts: Vec<&str> = tokens
        .iter()
        .map(|(span, _, _)| &SOURCE[span.start as usize..span.end as usize])
        .collect();
    assert!(texts.contains(&"SCREEN"), "{texts:?}");
    assert!(texts.contains(&"LIMIT"), "{texts:?}");
    // And the stream is still ordered and non-overlapping.
    for pair in tokens.windows(2) {
        assert!(pair[0].0.end <= pair[1].0.start, "{pair:?}");
    }
}

/// A block comment spanning lines becomes one token per line, because the
/// protocol's encoding is per line.
#[test]
fn a_multi_line_token_is_split_per_line() {
    const SOURCE: &str = "/* one\n   two\n   three */\nvar SCREEN = $0400\n";
    let tokens = decoded_tokens(SOURCE);
    let comments: Vec<&(Span, String, Vec<String>)> = tokens
        .iter()
        .filter(|(_, kind, _)| kind == "comment")
        .collect();
    assert_eq!(comments.len(), 3, "one per line: {comments:?}");
    for (span, _, _) in &comments {
        assert!(
            !SOURCE[span.start as usize..span.end as usize].contains('\n'),
            "a token still crosses a line"
        );
    }
}

/// Accepting a completion replaces the punctuation that provoked it.
///
/// A candidate's label carries the `.`, `@` or `#` — an editor showing `lo`
/// under a list of fields is showing the wrong thing — so the character the
/// user already typed has to be *replaced*. Without a range the client
/// appends at the cursor and produces `PAIR..lo`, `@@a8`, `##LIMIT`: text the
/// compiler then refuses, from a completion it offered.
///
/// The test applies the edit, which is the only way to see it.
#[test]
fn accepting_a_completion_does_not_double_the_punctuation() {
    let apply = |source: &str, offset: u32, trigger: Option<char>, label: &str| -> String {
        let mut workspace = Workspace::new();
        let uri = "file:///accept.k816";
        workspace.update(uri, source);
        let analysis = workspace.analyze();
        let file = workspace.file(uri).expect("open");
        let found = k816::lsp::completion::complete(&analysis, file, offset, trigger);
        let candidate = found
            .items
            .iter()
            .find(|candidate| candidate.label == label)
            .unwrap_or_else(|| panic!("`{label}` was not offered"));
        let new_text = candidate
            .insert
            .clone()
            .unwrap_or_else(|| candidate.label.clone());
        let mut edited = source.to_owned();
        edited.replace_range(
            found.replace.start as usize..found.replace.end as usize,
            &new_text,
        );
        edited
    };

    // A field step: the `.` is replaced, not appended to.
    const FIELDS: &str = "var PAIR[ .lo :byte .hi :byte ]\nfunc w @a8 @i8 {\n    a = PAIR.\n}\n";
    let edited = apply(FIELDS, after(FIELDS, "a = PAIR."), Some('.'), ".lo");
    assert!(edited.contains("a = PAIR.lo"), "{edited}");
    assert!(
        !edited.contains("PAIR..lo"),
        "the dot was doubled: {edited}"
    );

    // A width annotation.
    let source = "func work @";
    let edited = apply(source, after(source, "@"), Some('@'), "@a8");
    assert_eq!(edited, "func work @a8");

    // An immediate: the `#` is part of what is inserted *and* part of what is
    // replaced.
    const IMM: &str = "const LIMIT = 8\nfunc w @a8 @i8 {\n    lda #\n}\n";
    let edited = apply(IMM, after(IMM, "lda #"), Some('#'), "LIMIT");
    assert!(edited.contains("lda #LIMIT"), "{edited}");
    assert!(!edited.contains("##"), "the marker was doubled: {edited}");

    // A colon suffix.
    const SUFFIX: &str = "var PAIR[ .lo :byte ]\nfunc w @a8 @i8 {\n    a = PAIR:\n}\n";
    let edited = apply(SUFFIX, after(SUFFIX, "PAIR:"), Some(':'), ":sizeof");
    assert!(edited.contains("a = PAIR:sizeof"), "{edited}");

    // And an *untriggered* request part-way through a name replaces the part
    // typed so far rather than doubling it.
    const PARTIAL: &str = "var SCREEN = $0400\nfunc w @a8 @i8 {\n    lda SCR\n}\n";
    let edited = apply(PARTIAL, after(PARTIAL, "lda SCR"), None, "SCREEN");
    assert!(edited.contains("lda SCREEN"), "{edited}");
    assert!(!edited.contains("SCRSCREEN"), "{edited}");
}

/// The replaced span never covers a token that is not part of the name.
///
/// A request just after `{` must not offer to overwrite the brace, which is
/// what an unguarded "replace the token behind the cursor" would do.
#[test]
fn an_untriggered_completion_replaces_only_a_partial_name() {
    let span = |source: &str, offset: u32| {
        let mut workspace = Workspace::new();
        let uri = "file:///span.k816";
        workspace.update(uri, source);
        let analysis = workspace.analyze();
        let file = workspace.file(uri).expect("open");
        k816::lsp::completion::complete(&analysis, file, offset, None).replace
    };

    // Immediately after a brace: an insertion, not a replacement.
    const BRACE: &str = "func w @a8 @i8 {\n    nop\n}\n";
    let at = after(BRACE, "{");
    assert_eq!(span(BRACE, at), Span::new(span(BRACE, at).file, at, at));
    // Immediately after a name: the name.
    let at = after(BRACE, "nop");
    let replaced = span(BRACE, at);
    assert_eq!(
        &BRACE[replaced.start as usize..replaced.end as usize],
        "nop"
    );
}

/// A field's declaring `.name` carries the `declaration` modifier.
///
/// It was the one declaring occurrence in the language that did not: a field
/// has no `SymbolId`, so its declaration is neither in the symbol table nor
/// in the reference index, and the refinement looked in both.
#[test]
fn a_field_declaration_is_marked_as_one() {
    const SOURCE: &str = "var PAIR[ .lo :byte .hi :byte ]\nfunc w @a8 @i8 {\n    a = PAIR.hi\n}\n";
    let tokens = decoded_tokens(SOURCE);
    let at = |needle: &str, nth: usize| {
        let mut found = 0usize;
        for _ in 0..=nth {
            found = SOURCE[found..].find(needle).expect(needle) + found + 1;
        }
        let offset = (found - 1) as u32;
        tokens
            .iter()
            .find(|(span, _, _)| span.contains(offset))
            .unwrap_or_else(|| panic!("no token at `{needle}` #{nth}"))
            .clone()
    };

    // The declaration in the field list.
    let (_, kind, modifiers) = at(".lo", 0);
    assert_eq!(kind, "property");
    assert_eq!(modifiers, ["declaration"]);
    let (_, kind, modifiers) = at(".hi", 0);
    assert_eq!(kind, "property");
    assert_eq!(modifiers, ["declaration"]);

    // The *use* is a property too, and carries no modifier.
    let (_, kind, modifiers) = at(".hi", 1);
    assert_eq!(kind, "property");
    assert!(modifiers.is_empty(), "a use is not a declaration");
}

// ---------------------------------------------------------------------------
// K8 — the editor's program is the open documents plus what they import
// ---------------------------------------------------------------------------

/// A scratch directory holding `main.k816`, `lib.k816` and `hw.k816`.
fn imported_workspace(test: &str) -> (std::path::PathBuf, Workspace) {
    let dir = std::path::PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join(test);
    std::fs::create_dir_all(&dir).expect("create the work dir");
    std::fs::write(
        dir.join("lib.k816"),
        "import \"hw.k816\"\n\nfunc helper @a8 @i8 {\n    a = PORT\n}\n",
    )
    .expect("write lib");
    std::fs::write(dir.join("hw.k816"), "extern var abs PORT\n").expect("write hw");

    let mut workspace = Workspace::new();
    let main = dir.join("main.k816");
    std::fs::write(
        &main,
        "import \"lib.k816\"\n\nfunc main @a8 @i8 {\n    call helper\n}\n",
    )
    .expect("write main");
    workspace.update(format!("file://{}", main.display()), {
        std::fs::read_to_string(&main).expect("read main")
    });
    (dir, workspace)
}

/// One open document, and the editor sees the whole program.
///
/// The difference K makes to the editor: a name a document imports resolves
/// whether or not the file defining it has ever been opened, so the server
/// answers the question a build answers rather than a subset of it. `helper`
/// is in `lib.k816` and `PORT` two imports further on, and neither is open.
#[test]
fn an_open_document_sees_what_it_imports_without_those_files_being_opened() {
    let (_, mut workspace) = imported_workspace("lsp-import-closure");
    let analysis = workspace.analyze();
    assert!(
        analysis.linkage.diagnostics().is_empty(),
        "{:?}",
        analysis.linkage.diagnostics()
    );
    // Three units from one document: the entry, what it imports, and what
    // *that* imports — the whole closure is read, because a contract two
    // imports away decides bytes here.
    assert_eq!(analysis.units.len(), 3);
    assert!(
        analysis.diagnostics.is_empty(),
        "{:?}",
        analysis.diagnostics
    );
}

/// Opening a file the workspace already read keeps its identity.
///
/// A `FileId` is a position in the map, and every span already taken from an
/// imported file points into it. Re-adding the file on `didOpen` would give
/// it a second identity, leaving the first one's spans pointing at a file
/// nothing analyzes any more.
#[test]
fn opening_an_imported_file_keeps_its_identity() {
    let (dir, mut workspace) = imported_workspace("lsp-import-promote");
    workspace.analyze();
    let lib_uri = format!("file://{}", dir.join("lib.k816").display());
    let before = k816::workspace::find_file(workspace.map(), &lib_uri)
        .or_else(|| {
            k816::workspace::find_file(workspace.map(), &dir.join("lib.k816").to_string_lossy())
        })
        .expect("the imported file is in the map");

    workspace.update(
        &lib_uri,
        std::fs::read_to_string(dir.join("lib.k816")).expect("read lib"),
    );
    assert_eq!(
        workspace.file(&lib_uri),
        Some(before),
        "the identity survived the open"
    );

    // And an unsaved edit to the now-open buffer wins over the disk, which
    // is what an editor is for.
    workspace.update(
        &lib_uri,
        "import \"hw.k816\"\n\nfunc helper @a8 @i8 {\n    a = MISSING\n}\n",
    );
    let analysis = workspace.analyze();
    assert!(
        analysis
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.code.0 == "K816-E0101"),
        "the buffer is the truth: {:?}",
        analysis.diagnostics
    );
}

/// A problem in a file nobody opened is still a problem of the program, and
/// is reportable.
#[test]
fn a_diagnostic_in_an_imported_file_can_be_told_to_the_client() {
    let dir = std::path::PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join("lsp-import-diagnostic");
    std::fs::create_dir_all(&dir).expect("create the work dir");
    std::fs::write(
        dir.join("broken.k816"),
        "func helper @a8 @i8 {\n    a = MISSING\n}\n",
    )
    .expect("write the broken import");
    let main = dir.join("main.k816");
    std::fs::write(
        &main,
        "import \"broken.k816\"\n\nfunc main @a8 @i8 {\n    call helper\n}\n",
    )
    .expect("write main");

    let mut workspace = Workspace::new();
    let uri = format!("file://{}", main.display());
    workspace.update(&uri, std::fs::read_to_string(&main).expect("read main"));
    let analysis = workspace.analyze();
    let reported: Vec<&k816::diagnostic::Diagnostic> = analysis
        .diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.code.0 == "K816-E0101")
        .collect();
    assert_eq!(reported.len(), 1, "{:?}", analysis.diagnostics);
    // And the client can be told where: an imported file is named by a path,
    // and a path a client can act on is a `file://` URI.
    let where_it_is = k816::lsp::convert::uri(workspace.map(), reported[0].primary.span.file)
        .expect("an imported file has a URI");
    assert!(
        where_it_is.as_str().starts_with("file:///")
            && where_it_is.as_str().ends_with("broken.k816"),
        "{where_it_is:?}"
    );
}

/// A negative constant is shown as the number it was written as, in the
/// hover and in the outline, and not as the 32-bit pattern it is held in:
/// no use of it has that width (`plans/15-constant-sizes.md` decision 3).
#[test]
fn a_negative_constant_is_shown_as_written() {
    use k816::lsp::navigation::document_symbols;

    let source = "const EUNKNOWN = -1\nconst LIMIT = 15\n\n\
                  func main @a8 @i8 {\n    lda #EUNKNOWN\n}\n";
    let mut workspace = Workspace::new();
    let uri = "file:///negative.kk";
    workspace.update(uri, source);
    let analysis = workspace.analyze();
    let file = workspace.file(uri).expect("open");

    let outline = document_symbols(&analysis, file);
    let details: Vec<_> = outline
        .iter()
        .map(|entry| (entry.name.as_str(), entry.detail.as_deref()))
        .collect();
    assert_eq!(
        &details[..2],
        [("EUNKNOWN", Some("= -1")), ("LIMIT", Some("= $000F"))]
    );

    let use_site = source.rfind("EUNKNOWN").expect("the use") as u32 + 1;
    let hover = k816::lsp::hover::hover(&analysis, file, use_site).expect("a hover");
    assert!(hover.markdown.contains("Value `-1`."), "{}", hover.markdown);
    assert!(!hover.markdown.contains("FFFFFFFF"), "{}", hover.markdown);
}

/// Hover on an `extern` says it is a promise, not a body somewhere.
#[test]
fn hover_on_an_extern_says_another_object_provides_it() {
    let source = "extern func _puts\n\nfunc main @a8 @i8 {\n    call _puts\n}\n";
    let mut workspace = Workspace::new();
    let uri = "file:///hover-extern.k816";
    workspace.update(uri, source);
    let analysis = workspace.analyze();
    let file = workspace.file(uri).expect("open");

    let use_site = source.rfind("_puts").expect("the call") as u32 + 1;
    let hover = k816::lsp::hover::hover(&analysis, file, use_site).expect("a hover");
    assert!(
        hover.markdown.contains("Declared `extern`"),
        "{}",
        hover.markdown
    );
    assert!(
        hover.markdown.contains("nothing checked it"),
        "the promise is named as one: {}",
        hover.markdown
    );
}

/// Deleting an `import` line drops the file it reached.
///
/// The editor keeps its map across edits, and nothing else prunes it — a
/// close rebuilds, an ordinary edit does not. So a file that was imported
/// and no longer is would stay a unit of the program for the rest of the
/// session: still defining what it defines, and still colliding with a
/// definition the author has just written in its place. The build compiles
/// the same text cleanly, because the CLI builds its map once per run, and
/// an editor that disagrees with the build about a name is the failure
/// sub-plan H's decision 1 exists to prevent.
#[test]
fn deleting_an_import_drops_the_unit_it_reached() {
    let dir = std::path::PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join("lsp-import-dropped");
    std::fs::create_dir_all(&dir).expect("create the work dir");
    std::fs::write(dir.join("b.k816"), "func helper @a8 @i8 {\n    nop\n}\n").expect("write b");
    let main = dir.join("main.k816");
    let uri = format!("file://{}", main.display());

    let mut workspace = Workspace::new();
    workspace.update(
        &uri,
        "import \"b.k816\"\n\nfunc main @a8 @i8 {\n    call helper\n}\n",
    );
    let analysis = workspace.analyze();
    assert_eq!(analysis.units.len(), 2);
    assert!(
        analysis.diagnostics.is_empty(),
        "{:?}",
        analysis.diagnostics
    );

    // The author deletes the import and writes the function here instead.
    workspace.update(
        &uri,
        "func helper @a8 @i8 {\n    nop\n}\n\nfunc main @a8 @i8 {\n    call helper\n}\n",
    );
    let analysis = workspace.analyze();
    assert_eq!(
        analysis.units.len(),
        1,
        "the file nothing imports any more is not a unit"
    );
    assert!(
        analysis.diagnostics.is_empty(),
        "the editor invented a clash the build does not report: {:?}",
        analysis.diagnostics
    );
}
