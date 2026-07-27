/*****************************************************************************/
/*                                                                           */
/*                                semantic.rs                                */
/*                                                                           */
/*             Semantic analysis and diagnostic tests for the slice          */
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

//! Semantic diagnostics for the slice (master plan Phase 5): exact code,
//! span, and help for the undeclared-symbol negative path, plus contract,
//! width, and operand checks.

use k816::diagnostic::{DiagnosticCode, Severity};
use k816::op::RegisterWidth;
use k816::parser::parse_source;
use k816::semantic::analyze_file;
use k816::source::SourceMap;

fn analyze(
    text: &str,
) -> (
    k816::semantic::SemanticModel,
    Vec<k816::diagnostic::Diagnostic>,
) {
    let mut map = SourceMap::new();
    let id = map.add_file("test.k816", text);
    let parse = parse_source(map.file(id), None);
    assert_eq!(parse.diagnostics, vec![], "unexpected syntax diagnostics");
    analyze_file(&parse.file)
}

#[test]
fn undeclared_screen_yields_exact_diagnostic() {
    // The slice program without its `var SCREEN = $0400` declaration.
    let source = "func main @a8 @i16 {
    a = #0
    a = #1
    SCREEN = a
    return
}
";
    let (_, diagnostics) = analyze(source);
    assert_eq!(diagnostics.len(), 1);

    let diagnostic = &diagnostics[0];
    assert_eq!(diagnostic.code, DiagnosticCode::UNDECLARED_SYMBOL);
    assert_eq!(diagnostic.code.as_str(), "K816-E0101");
    assert_eq!(diagnostic.severity, Severity::Error);
    assert_eq!(diagnostic.title, "`SCREEN` is not declared");

    // Exact span: `SCREEN` on line 3 (offset 47..53 in this source).
    let mut map = SourceMap::new();
    let id = map.add_file("test.k816", source);
    assert_eq!(diagnostic.primary.span.start, 47);
    assert_eq!(diagnostic.primary.span.end, 53);
    assert_eq!(
        map.file(id).span_text(k816::source::Span::new(
            id,
            diagnostic.primary.span.start,
            diagnostic.primary.span.end
        )),
        "SCREEN"
    );

    assert_eq!(
        diagnostic.help.as_deref(),
        Some("declare it first, e.g. `var SCREEN = $0400`")
    );
    assert!(diagnostic.docs.is_some());
}

#[test]
fn slice_program_analyzes_cleanly_with_contract() {
    let source = "var SCREEN = $0400

func main @a8 @i16 {
    a = #0
    a = #1
    SCREEN = a
    return
}
";
    let (model, diagnostics) = analyze(source);
    assert_eq!(diagnostics, vec![]);

    assert_eq!(model.functions.len(), 1);
    let main = &model.functions[0];
    assert!(main.is_entry_point);
    assert_eq!(main.contract.a, Some(RegisterWidth::W8));
    assert_eq!(main.contract.i, Some(RegisterWidth::W16));

    let screen = model.symbols.lookup("SCREEN").expect("SCREEN declared");
    assert_eq!(
        model.symbols.symbol(screen).kind,
        k816::symbol::SymbolKind::FixedAddressVar { address: 0x0400 }
    );
}

#[test]
fn width_dependent_immediate_without_contract_is_rejected() {
    let (_, diagnostics) = analyze("func main {\n    a = #1\n}\n");
    assert_eq!(diagnostics.len(), 1);
    assert_eq!(diagnostics[0].code, DiagnosticCode::UNKNOWN_WIDTH);
    assert_eq!(
        diagnostics[0].help.as_deref(),
        Some("add @a8 or @a16 to the enclosing function")
    );
}

#[test]
fn immediate_must_fit_the_contracted_width() {
    let (_, diagnostics) = analyze("func main @a8 {\n    a = #256\n}\n");
    assert_eq!(diagnostics.len(), 1);
    assert_eq!(diagnostics[0].code, DiagnosticCode::IMMEDIATE_RANGE);

    // 16-bit accumulator accepts the same value.
    let (_, diagnostics) = analyze("func main @a16 {\n    a = #256\n}\n");
    assert_eq!(diagnostics, vec![]);

    // SEP/REP masks are fixed 8-bit regardless of contract.
    let (_, diagnostics) = analyze("func main @a16 {\n    sep #$100\n}\n");
    assert_eq!(diagnostics.len(), 1);
    assert_eq!(diagnostics[0].code, DiagnosticCode::IMMEDIATE_RANGE);
}

#[test]
fn duplicate_declarations_point_at_the_first_one() {
    let (_, diagnostics) = analyze("var A = $10\nvar A = $20\n");
    assert_eq!(diagnostics.len(), 1);
    let diagnostic = &diagnostics[0];
    assert_eq!(diagnostic.code, DiagnosticCode::DUPLICATE_SYMBOL);
    assert_eq!(diagnostic.secondary.len(), 1);
    assert!(diagnostic.primary.span.start > diagnostic.secondary[0].span.start);
}

#[test]
fn storing_through_a_function_symbol_is_rejected() {
    // The slice grammar only parses each mnemonic's single legal operand
    // shape, so unknown-mnemonic and illegal-mode diagnostics stay
    // grammar-unreachable until sub-plan B broadens the syntax. The symbol
    // misuse path is reachable today:
    let (_, diagnostics) = analyze("func f @a8 {\n    sta f\n}\n");
    assert_eq!(diagnostics.len(), 1);
    assert_eq!(diagnostics[0].code, DiagnosticCode::INVALID_OPERAND);
    assert_eq!(diagnostics[0].title, "`f` is a function, not a variable");
    assert_eq!(diagnostics[0].secondary.len(), 1);
}

#[test]
fn functions_are_not_entry_points_unless_named_main() {
    let (model, diagnostics) = analyze("func helper @a8 {\n    return\n}\n");
    assert_eq!(diagnostics, vec![]);
    assert_eq!(model.functions.len(), 1);
    assert!(!model.functions[0].is_entry_point);
}
