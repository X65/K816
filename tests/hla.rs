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

/// The shapes E2 will open classify identically *now*, which is the claim
/// E1 actually makes: the chain lowering that reaches them later has nothing
/// left to decide about their operands.
///
/// Reached through the classifier directly, because analysis still refuses
/// these chains — a divergence here would otherwise stay invisible until the
/// task that opens them, and look like that task's bug.
#[test]
fn an_indexed_operand_classifies_the_same_from_either_syntax() {
    use k816::ast::{HlaStatement, ItemKind, StatementKind};
    use k816::isa::Mnemonic;
    use k816::resolve::Resolver;

    let source = "var SCREEN = $0400
func main @a8 @i16 {
    SCREEN,x = a
    sta SCREEN,x
}
";
    let mut map = SourceMap::new();
    let id = map.add_file("test.k816", source);
    let parse = parse_source(map.file(id), None);
    assert_eq!(parse.diagnostics, vec![], "unexpected syntax diagnostics");
    // Analysis refuses the chain — that is E2's to open — so its diagnostics
    // are not what this test is about; its symbol table is.
    let (model, _) = analyze_file(&parse.file);

    let ItemKind::Func(function) = &parse.file.items[1].kind else {
        panic!("the second item is the function");
    };
    let StatementKind::Hla(HlaStatement::Assign(assign)) = &function.body.statements[0].kind else {
        panic!("the first statement is the assignment");
    };
    let StatementKind::Native(native) = &function.body.statements[1].kind else {
        panic!("the second statement is the native store");
    };

    let mut resolver = Resolver::new(&model.symbols);
    let hla = resolver
        .classify_hla(
            Mnemonic::Sta,
            &assign.targets[0],
            function.body.statements[0].span,
        )
        .expect("the HLA target classifies");
    let native = resolver
        .classify(native)
        .expect("the native operand classifies");
    assert_eq!(
        hla, native,
        "`SCREEN,x = a` and `sta SCREEN,x` are one store"
    );
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
