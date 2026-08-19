/*****************************************************************************/
/*                                                                           */
/*                                  hla.rs                                   */
/*                                                                           */
/*          Native and HLA spellings lower to the same operations            */
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

//! The equivalence discipline: an HLA statement and its native spelling lower
//! to `KOp` streams that differ in [`Origin`] and in nothing else.
//!
//! This is the test that keeps a second operand model from growing back. A
//! golden of emitted text would pass with two classifiers as long as they
//! happened to agree on the fixtures; this fails the moment they disagree
//! about anything — the addressing mode, the encoding, the folded expression.
//!
//! Each sub-plan E task adds its family here. E1 covers what the two models
//! actually disagreed about before there was one.

use k816::diagnostic::{Diagnostic, DiagnosticCode};
use k816::lower::lower;
use k816::op::{KOpKind, Origin};
use k816::parser::parse_source;
use k816::semantic::analyze_file;
use k816::source::SourceMap;

/// The lowered operations of `text`'s single function, and their origins.
fn lower_ops(text: &str) -> Vec<(KOpKind, Origin)> {
    let mut map = SourceMap::new();
    let id = map.add_file("test.k816", text);
    let parse = parse_source(map.file(id), None);
    assert_eq!(parse.diagnostics, vec![], "unexpected syntax diagnostics");
    let (model, diagnostics) = analyze_file(&parse.file);
    assert_eq!(diagnostics, vec![], "unexpected semantic diagnostics");
    let (program, diagnostics) = lower(&parse.file, &model);
    assert_eq!(diagnostics, vec![], "unexpected lowering diagnostics");
    program
        .functions()
        .next()
        .expect("one lowered function")
        .ops
        .iter()
        .map(|op| (op.kind.clone(), op.origin))
        .collect()
}

/// Asserts that the HLA body and the native body lower identically, and that
/// the HLA one is recorded as expanded rather than written.
///
/// Both bodies are compiled in the same declaration context, so a difference
/// can only come from the statements themselves.
fn equivalent(declarations: &str, hla_body: &str, native_body: &str) {
    let program = |body: &str| format!("{declarations}\nfunc main @a8 @i16 {{\n{body}\n}}\n");
    let hla = lower_ops(&program(hla_body));
    let native = lower_ops(&program(native_body));

    let kinds = |ops: &[(KOpKind, Origin)]| -> Vec<KOpKind> {
        ops.iter().map(|(kind, _)| kind.clone()).collect()
    };
    assert_eq!(
        kinds(&hla),
        kinds(&native),
        "`{}` and `{}` must lower to the same operations",
        hla_body.trim(),
        native_body.trim()
    );

    let expanded = hla.iter().any(|(kind, origin)| {
        matches!(kind, KOpKind::Instruction(_)) && *origin == Origin::HlaExpansion
    });
    assert!(expanded, "the HLA form must record that it was expanded");
}

/// The diagnostics `text` produces, syntax included.
fn diagnostics(text: &str) -> Vec<Diagnostic> {
    let mut map = SourceMap::new();
    let id = map.add_file("test.k816", text);
    let parse = parse_source(map.file(id), None);
    assert_eq!(parse.diagnostics, vec![], "unexpected syntax diagnostics");
    analyze_file(&parse.file).1
}

#[test]
fn a_literal_immediate_load_matches_its_native_spelling() {
    equivalent("", "    a = #1", "    lda #1");
}

/// A `const` immediate folds at classification. Before one classifier, this
/// pair was accepted by analysis — which folded it — and refused by lowering,
/// which demanded a literal: an internal error on a legal program.
#[test]
fn a_const_immediate_load_matches_its_native_spelling() {
    equivalent("const K = 5", "    a = #K", "    lda #K");
}

/// A link-time immediate has no value to check and never had one; ca65
/// relocates it. The HLA model could not fold what it could not evaluate, so
/// it refused the form outright.
#[test]
fn a_link_time_immediate_load_matches_its_native_spelling() {
    equivalent(
        "var SCREEN = $0400",
        "    a = #&&SCREEN",
        "    lda #&&SCREEN",
    );
}

/// The encoding is chosen from what K816 knows about the address, and a
/// pinned direct-page variable is exactly what it knows. The HLA store used
/// to emit an absolute encoding unconditionally — the same store, one byte
/// longer, decided by different code.
#[test]
fn a_store_takes_the_encoding_the_address_earns() {
    equivalent("var Z = $10", "    Z = a", "    sta Z");
    equivalent("var SCREEN = $0400", "    SCREEN = a", "    sta SCREEN");
}

/// Every operand shape a load and a store accept, against its native
/// spelling. The classifier picks the mode and the encoding, so this is one
/// assertion per *shape* rather than per instruction.
#[test]
fn every_load_and_store_shape_matches_its_native_spelling() {
    let declarations = "var Z = $10\nvar SCREEN = $0400\nvar PTR = $20";
    for (hla, native) in [
        ("    a = SCREEN", "    lda SCREEN"),
        ("    a = SCREEN,x", "    lda SCREEN,x"),
        ("    a = SCREEN,y", "    lda SCREEN,y"),
        ("    a = (PTR,x)", "    lda (PTR,x)"),
        ("    a = (PTR),y", "    lda (PTR),y"),
        ("    x = SCREEN", "    ldx SCREEN"),
        ("    x = SCREEN,y", "    ldx SCREEN,y"),
        ("    y = SCREEN", "    ldy SCREEN"),
        ("    y = SCREEN,x", "    ldy SCREEN,x"),
        ("    SCREEN = a", "    sta SCREEN"),
        ("    SCREEN,x = a", "    sta SCREEN,x"),
        ("    SCREEN,y = a", "    sta SCREEN,y"),
        ("    (PTR,x) = a", "    sta (PTR,x)"),
        ("    (PTR),y = a", "    sta (PTR),y"),
        ("    SCREEN = x", "    stx SCREEN"),
        ("    SCREEN = y", "    sty SCREEN"),
        ("    Z = a", "    sta Z"),
    ] {
        equivalent(declarations, hla, native);
    }
}

/// Operand *rejections* are shared too, which is the half that would rot
/// silently: a second model that agrees on every legal program can still
/// disagree about which illegal ones to report.
#[test]
fn an_operand_rejection_is_the_same_for_both_spellings() {
    let hla = diagnostics("const K = 5\nfunc main @a8 {\n    K = a\n}\n");
    let native = diagnostics("const K = 5\nfunc main @a8 {\n    sta K\n}\n");
    assert_eq!(hla.len(), 1);
    assert_eq!(hla[0].code, DiagnosticCode::CONST_IN_ADDRESS_POSITION);
    assert_eq!(native[0].code, hla[0].code);
    assert_eq!(native[0].title, hla[0].title);
    assert_eq!(native[0].fixes, hla[0].fixes);
}

/// The fixture's paired functions: `NAME` written as HLA, `NAME_native`
/// written as the instructions it must produce.
///
/// One file rather than one program per case, because a chain's operands are
/// resolved against real declarations and the pairing should be readable as a
/// document — someone checking a chain's expansion reads the two bodies side
/// by side.
#[test]
fn every_chain_matches_the_native_sequence_beside_it() {
    let mut map = SourceMap::new();
    let id = map.add_file("hla-chains.k816", include_str!("golden/hla-chains.k816"));
    let parse = parse_source(map.file(id), None);
    assert_eq!(parse.diagnostics, vec![], "the fixture must parse");
    let (model, diagnostics) = analyze_file(&parse.file);
    assert_eq!(diagnostics, vec![], "the fixture must analyze cleanly");
    let (program, diagnostics) = lower(&parse.file, &model);
    assert_eq!(diagnostics, vec![], "the fixture must lower cleanly");

    // A function's own label names a different symbol in each half of a pair,
    // so the comparison is over what the two *do*.
    let body = |name: &str| -> Vec<KOpKind> {
        let symbol = model
            .symbols
            .lookup(name)
            .unwrap_or_else(|| panic!("`{name}` is declared"));
        program
            .functions()
            .find(|function| function.symbol == symbol)
            .unwrap_or_else(|| panic!("`{name}` was lowered"))
            .ops
            .iter()
            .filter(|op| !matches!(op.kind, KOpKind::Label(_)))
            .map(|op| op.kind.clone())
            .collect()
    };

    let mut pairs = 0;
    for (symbol, _) in model.symbols.iter() {
        let Some(hla) = model.symbols.name(symbol).strip_suffix("_native") else {
            continue;
        };
        let hla = hla.to_owned();
        assert_eq!(
            body(&hla),
            body(&format!("{hla}_native")),
            "`{hla}` and its native sequence must lower alike"
        );
        pairs += 1;
    }
    assert_eq!(pairs, 6, "every pair in the fixture is compared");
}
