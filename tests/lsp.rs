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

use k816::lsp::{diagnostic_to_lsp, file_diagnostics, span_to_range};
use k816::source::{SourceMap, Span};
use k816::workspace::{Workspace, find_file};
use k816::{Options, compile_to_ca65};

/// The slice with its declaration removed: one syntax-clean semantic error.
const BROKEN_SLICE: &str = "func main @a8 @i16 {
    a = #0
    a = #1
    SCREEN = a
    return
}
";

/// Malformed source: exercises the syntax-diagnostic path too.
const MALFORMED: &str = "func main @a8 {\n    a = =\n    return\n";

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
        let (lsp_map, analysis) = workspace.analyze(&Options::default());
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
    let (map, analysis) = workspace.analyze(&Options::default());
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

    let (_, analysis) = workspace.analyze(&Options::default());
    assert_eq!(analysis.diagnostics.len(), 1);

    workspace.close("file:///a.k816");
    let (_, analysis) = workspace.analyze(&Options::default());
    assert_eq!(analysis.diagnostics.len(), 0);
    assert!(!workspace.is_empty());
}
