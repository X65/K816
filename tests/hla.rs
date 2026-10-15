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

use k816::diagnostic::{Diagnostic, DiagnosticCode, Severity};
use k816::lower::lower;
use k816::op::{KOpKind, Operand, Origin};
use k816::parser::parse_source;
use k816::semantic::analyze_file;
use k816::source::{SourceMap, Span};
use k816::value::KExpr;

/// The lowered operations of `text`'s single function, and their origins.
fn lower_ops(text: &str) -> Vec<(KOpKind, Origin)> {
    let mut map = SourceMap::new();
    let id = map.add_file("test.k816", text);
    let parse = parse_source(map.file(id), None);
    assert_eq!(parse.diagnostics, vec![], "unexpected syntax diagnostics");
    let (model, diagnostics) = analyze_file(&parse.file);
    // Errors only: a *warning* is a claim about the program that lowering is
    // supposed to keep working through, and the shapes E6 added — an `rti`
    // exit above all — carry one by design.
    let errors: Vec<_> = diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.severity == Severity::Error)
        .collect();
    assert_eq!(
        errors,
        Vec::<&Diagnostic>::new(),
        "unexpected semantic errors"
    );
    let (program, diagnostics) = lower(model.program(&parse.file), &model);
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
    let program = |body: &str| format!("{declarations}\nfunc work @a8 @i16 {{\n{body}\n}}\n");
    equivalent_programs(
        &program(hla_body),
        &program(native_body),
        hla_body,
        native_body,
    );
}

/// The same, for a pair whose surrounding declarations cannot simply be
/// prepended.
///
/// [`lower_ops`] reads the *first* lowered function, so a section the pair
/// refers to has to come after `main` rather than before it — which works
/// because sections are declared in a pass of their own, before any body is
/// checked.
fn equivalent_programs(hla_program: &str, native_program: &str, hla_body: &str, native_body: &str) {
    let hla = lower_ops(hla_program);
    let native = lower_ops(native_program);

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
    equivalent("var ZZ = $10", "    ZZ = a", "    sta ZZ");
    equivalent("var SCREEN = $0400", "    SCREEN = a", "    sta SCREEN");
}

/// Every operand shape a load and a store accept, against its native
/// spelling. The classifier picks the mode and the encoding, so this is one
/// assertion per *shape* rather than per instruction.
#[test]
fn every_load_and_store_shape_matches_its_native_spelling() {
    let declarations = "var ZZ = $10\nvar SCREEN = $0400\nvar PTR = $20";
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
        ("    ZZ = a", "    sta ZZ"),
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
    paired_fixture("hla-chains.k816", include_str!("golden/hla-chains.k816"), 6);
}

/// The shorthand families, paired the same way.
#[test]
fn every_shorthand_matches_the_native_sequence_beside_it() {
    paired_fixture("hla-alu.k816", include_str!("golden/hla-alu.k816"), 6);
}

/// Every branch operator in every form, against the native branch it must
/// emit — which is the assertion that catches an inverted *direction*, the
/// one thing a golden of emitted text would happily agree with.
#[test]
fn every_branch_form_matches_the_native_sequence_beside_it() {
    paired_fixture_warning(
        "hla-branches.k816",
        include_str!("golden/hla-branches.k816"),
        6,
        2,
    );
}

fn paired_fixture(name: &str, text: &str, expected_pairs: usize) {
    paired_fixture_warning(name, text, expected_pairs, 0);
}

/// The same, for a fixture that legitimately warns: `hla-branches.k816`
/// exercises `>` and `<=`, and G8's efficiency warning is what those two
/// cost. The count is written down rather than the warning being filtered,
/// so a *third* one appearing is still a failure.
fn paired_fixture_warning(name: &str, text: &str, expected_pairs: usize, expected_warnings: usize) {
    let mut map = SourceMap::new();
    let id = map.add_file(name, text);
    let parse = parse_source(map.file(id), None);
    assert_eq!(parse.diagnostics, vec![], "the fixture must parse");
    let (model, diagnostics) = analyze_file(&parse.file);
    assert_eq!(diagnostics, vec![], "the fixture must analyze cleanly");
    let (program, diagnostics) = lower(model.program(&parse.file), &model);
    let warnings = diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.code == DiagnosticCode::INEFFICIENT_BRANCH)
        .count();
    assert_eq!(
        warnings, expected_warnings,
        "the fixture's efficiency warnings"
    );
    let rest: Vec<_> = diagnostics
        .into_iter()
        .filter(|diagnostic| diagnostic.code != DiagnosticCode::INEFFICIENT_BRANCH)
        .collect();
    assert_eq!(rest, vec![], "the fixture must otherwise lower cleanly");

    // Two halves of a pair name their branch targets differently — the HLA
    // form generates anchors, the native form is written with labels — so the
    // comparison normalizes a target to *where it is*: a branch becomes its
    // mnemonic and the distance to the operation it lands on.
    //
    // Width claims are left out of it. They emit no bytes, and the two forms
    // place them at different granularities: a written label is a merge point
    // the model annotates, and a generated anchor is not a label. What a
    // statement does to the widths is checked in `tests/width.rs` and by
    // `a_shorthand_that_destroys_the_widths_is_seen_by_the_width_model`.
    let body = |name: &str| -> Vec<String> {
        let symbol = model
            .symbols
            .lookup(name)
            .unwrap_or_else(|| panic!("`{name}` is declared"));
        let ops = &program
            .functions()
            .find(|function| function.symbol == symbol)
            .unwrap_or_else(|| panic!("`{name}` was lowered"))
            .ops;

        // Positions are counted over the operations that survive the filter,
        // so a distance means "how many emitted operations away" in both
        // halves rather than how many the model happened to annotate.
        let kept: Vec<usize> = ops
            .iter()
            .enumerate()
            .filter(|(_, op)| match &op.kind {
                KOpKind::Label(id) => *id != symbol,
                KOpKind::ModeAssumption(_) => false,
                _ => true,
            })
            .map(|(index, _)| index)
            .collect();
        let position = |raw: usize| kept.iter().position(|&kept| kept == raw);
        let site = |target: &KOpKind| {
            ops.iter()
                .position(|op| &op.kind == target)
                .and_then(position)
        };

        kept.iter()
            .enumerate()
            .map(|(index, &raw)| match &ops[raw].kind {
                KOpKind::Label(_) | KOpKind::Anchor(_) => "target".to_owned(),
                KOpKind::Instruction(instruction) => {
                    let target = match &instruction.operand {
                        Some(Operand::Anchor(anchor)) => site(&KOpKind::Anchor(*anchor)),
                        // A written `jmp label` classifies as an address and a
                        // written `bcc label` as a branch target; both name a
                        // place this function defines.
                        Some(Operand::Label(KExpr::Symbol(id)))
                        | Some(Operand::Address {
                            expr: KExpr::Symbol(id),
                            index: None,
                            ..
                        }) => site(&KOpKind::Label(*id)),
                        _ => None,
                    };
                    match target {
                        Some(target) => format!(
                            "{} ->{}",
                            instruction.mnemonic.ca65(),
                            target as isize - index as isize
                        ),
                        None => format!("{instruction:?}"),
                    }
                }
                other => format!("{other:?}"),
            })
            .collect()
    };

    let mut pairs = 0;
    for (symbol, _) in model.symbols.iter() {
        let Some(hla) = model.symbols.name(symbol).strip_suffix("_native") else {
            continue;
        };
        let hla = hla.to_owned();
        let expanded = body(&hla);
        assert!(!expanded.is_empty(), "`{hla}` lowered to nothing");
        assert_eq!(
            expanded,
            body(&format!("{hla}_native")),
            "`{hla}` and its native sequence must lower alike"
        );
        pairs += 1;
    }
    assert_eq!(
        pairs, expected_pairs,
        "every pair in the fixture is compared"
    );
}

/// A retired K816 spelling names its replacement, rather than falling out as
/// whatever the surrounding rule makes of it.
///
/// Each of these was a working K816 spelling until decisions 29 and 30, and
/// each fails a *different* way without the refusal: `c` and `d` become
/// undeclared identifiers, so `c = s` reads as a store and reports on the
/// source while the help offers `var c = $0400`; `b` is still a register
/// token, so it reaches the transfer table and gets a generic refusal about a
/// register that only half exists. Decision 33 is why they are named.
#[test]
fn a_retired_register_spelling_names_its_replacement() {
    for (source, expected_help) in [
        ("    c = s", "the accumulator is `a` at both widths"),
        ("    dpr = c", "the accumulator is `a` at both widths"),
        ("    d = a", "the direct page register is `dpr`"),
        ("    b = x", "write `a~b` to swap the halves"),
        ("    x = b", "write `a~b` to swap the halves"),
        ("    b!!", "write `push dbr`"),
    ] {
        let program = format!("func main @a8 @i8 {{\n{source}\n}}\n");
        let diagnostics = diagnostics(&program);
        let found = diagnostics
            .iter()
            .find(|d| d.code == DiagnosticCode::RETIRED_REGISTER_SPELLING)
            .unwrap_or_else(|| {
                panic!("`{source}` did not report a retired spelling: {diagnostics:?}")
            });
        let help = found.help.clone().unwrap_or_default();
        assert!(
            help.contains(expected_help),
            "`{source}` help was `{help}`, expected it to name `{expected_help}`"
        );
    }
}

/// The shorthands the 65816 has no instruction for, all reported the same
/// way: one code, and a help that names the route where one exists.
///
/// These live here rather than in `tests/semantic.rs` for the reason
/// `postmortem.md` §D.3.2 settled for the width cases — a rejection belongs
/// next to the behaviour it belongs to.
#[test]
fn a_shorthand_the_cpu_cannot_do_is_refused_with_its_route() {
    for (source, expected_help) in [
        ("    x+1", Some("only the accumulator")),
        ("    y&mask", Some("only the accumulator")),
        ("    dpr?1", Some("only `a`, `x`, and `y` compare")),
        ("    v+", Some("only `v-` exists")),
        ("    s!!", Some("have push and pull")),
        ("    x~y", Some("`XBA` swaps the accumulator halves")),
        ("    dpr<<", Some("only the accumulator and memory shift")),
        ("    s++", Some("step by one")),
    ] {
        let source = format!("var mask = $10\nfunc main @a8 @i8 {{\n{source}\n}}\n");
        let reported = diagnostics(&source);
        assert_eq!(reported.len(), 1, "`{source}`: {reported:?}");
        assert_eq!(
            reported[0].code,
            DiagnosticCode::NO_SUCH_INSTRUCTION,
            "`{source}`"
        );
        let help = reported[0].help.as_deref().unwrap_or_default();
        let expected = expected_help.expect("every case names a route");
        assert!(
            help.contains(expected),
            "`{source}` helps with {help:?}, expected to mention {expected:?}"
        );
    }
}

/// The efficiency warning names the pair the *site* emits, not the pair the
/// written token would suggest.
///
/// `hla::branch` negates a condition whose form skips its body, and `>` and
/// `<=` negate into each other, so a prefix block written `<=` emits the
/// guarded `beq`/`bcs` pair and one written `>` emits `bcc`/`beq`. The legacy
/// K816 warned per spelling — "`} >` expands to three branch instructions",
/// "`} <=` to two" — and both halves are wrong for K816, which is what
/// `plans/06-compatibility.md` decision 23 has to be read past. Each row here
/// is one form, and the mnemonics it must name.
///
/// The four rows are also the claim that the *set* is right: {`>`, `<=`} is
/// closed under negation, so no form makes either of them free.
#[test]
fn the_efficiency_warning_names_what_the_site_emits() {
    for (body, expected) in [
        (
            "    {\n        x++\n    } >",
            "`beq` over the branch, then `bcs`",
        ),
        (
            "    {\n        x++\n    } <=",
            "`bcc` to the target, then `beq`",
        ),
        (
            "    > {\n        x++\n    }",
            "`bcc` to the target, then `beq`",
        ),
        (
            "    <= {\n        x++\n    }",
            "`beq` over the branch, then `bcs`",
        ),
    ] {
        let source = format!("func main @a8 @i8 {{\n{body}\n}}\n");
        let reported = lowering_warnings(&source);
        assert_eq!(reported.len(), 1, "`{source}`: {reported:?}");
        assert_eq!(reported[0].code, DiagnosticCode::INEFFICIENT_BRANCH);
        let note = reported[0].notes.last().expect("the expansion note");
        assert!(
            note.contains(expected),
            "`{source}` says {note:?}, expected to name {expected:?}"
        );
    }

    // And a single branch says nothing at all.
    let single = lowering_warnings("func main @a8 @i8 {\n    {\n        x++\n    } !=\n}\n");
    assert_eq!(single, Vec::<Diagnostic>::new());
}

/// One written condition is one warning, however many copies the compiler
/// makes of it.
///
/// An `inline` is expanded per use site, so the body's `>` reaches lowering
/// once per use. Sub-plan E's decision 12 keeps an inline's *analysis* at its
/// declaration for exactly this reason, and the efficiency warning is decided
/// after the copies exist — so the same rule is applied to the reports. The
/// caret stays in the body, because unlike the diagnostics `at_use_site`
/// moves, nothing about the argument made this one true.
#[test]
fn a_condition_inside_an_inline_warns_once_per_writing() {
    let source = "\
inline costly @a8 @i8 {
    a?5
    > {
        x++
    }
}

func main @a8 @i8 {
    costly
    costly
    costly
}
";
    let reported = lowering_warnings(source);
    assert_eq!(reported.len(), 1, "{reported:?}");
    let mut map = SourceMap::new();
    let id = map.add_file("test.k816", source);
    let span = reported[0].primary.span;
    assert_eq!(
        map.span_text(Span::new(id, span.start, span.end)),
        ">",
        "the caret stays on the operator in the body"
    );
}

/// Analysis and lowering together, for the diagnostics only lowering decides.
fn lowering_warnings(text: &str) -> Vec<Diagnostic> {
    let mut map = SourceMap::new();
    let id = map.add_file("test.k816", text);
    let parse = parse_source(map.file(id), None);
    assert_eq!(parse.diagnostics, vec![], "unexpected syntax diagnostics");
    let (model, diagnostics) = analyze_file(&parse.file);
    assert_eq!(diagnostics, vec![], "unexpected analysis diagnostics");
    lower(model.program(&parse.file), &model).1
}

/// `p??` is `PLP`, so the width model must lose the widths at it exactly as
/// it does for the mnemonic.
///
/// This is the seam the shorthand families put back at risk: an expansion the
/// model does not look at is an expansion whose effects it does not know, and
/// the failure is a width K816 keeps believing rather than a wrong opcode.
#[test]
fn a_shorthand_that_destroys_the_widths_is_seen_by_the_width_model() {
    let codes = |body: &str| -> Vec<&'static str> {
        let source = format!("func f @a8 @i8 {{\n    {body}\n    return\n}}\n");
        diagnostics(&source)
            .iter()
            .map(|diagnostic| diagnostic.code.as_str())
            .collect()
    };
    let hla = codes("p??");
    assert_eq!(
        hla,
        ["K816-W0124", "K816-W0124"],
        "pulling the status register leaves no width a caller can rely on"
    );
    assert_eq!(hla, codes("plp"), "the two spellings are one instruction");

    // And the pair that changes nothing stays quiet in both spellings.
    assert!(codes("a!!").is_empty());
    assert_eq!(codes("a!!"), codes("pha"));
}

/// A `nocross` run that stopped fitting its page because the split grew it
/// says so, and one that never fitted keeps the answer it always had.
///
/// The guard is evaluated *after* splitting, which is the consequence
/// sub-plan D's decision 13 named and review accepted along with the split
/// (`postmortem.md` §D.6.2).
#[test]
fn a_nocross_run_the_split_grew_names_the_split() {
    let filler = |count: usize| "    mem++\n".repeat(count);
    let with_split = format!(
        "var mem = $10\nfunc main @a8 @i8 {{\n  nocross {{\n    @a16 {{\n      bra .out\n    }}\n.out:\n{}  }}\n  return\n}}\n",
        filler(124)
    );
    let without = format!(
        "var mem = $10\nfunc main @a8 @i8 {{\n  nocross {{\n{}  }}\n  return\n}}\n",
        filler(130)
    );

    let codes = |source: &str| -> Vec<&'static str> {
        let mut map = SourceMap::new();
        let id = map.add_file("nocross.k816", source.to_owned());
        let parse = parse_source(map.file(id), None);
        assert_eq!(parse.diagnostics, vec![]);
        let (model, diagnostics) = analyze_file(&parse.file);
        assert_eq!(
            diagnostics,
            vec![],
            "analysis has nothing to say about size"
        );
        lower(model.program(&parse.file), &model)
            .1
            .iter()
            .map(|diagnostic| diagnostic.code.as_str())
            .collect()
    };

    assert_eq!(codes(&with_split), ["K816-E0130"]);
    assert_eq!(
        codes(&without),
        ["K816-E0112"],
        "a run nothing grew is the size it was written"
    );
}

/// Every jump-family HLA statement is its native mnemonic.
///
/// This is the pairing E's discipline asks for, applied to the forms E5
/// opened: the managed spelling and the raw one must lower to the same
/// operation, operand encoding included. A `jsl` that came out `a:` rather
/// than `f:` would assemble and link and jump into the wrong bank.
#[test]
fn the_jump_family_matches_its_native_spelling() {
    equivalent("var vec = $80", "    goto (vec)", "    jmp (vec)");

    let with_sections = |sections: &str, hla_body: &str, native_body: &str| {
        let program = |body: &str| format!("func work @a8 @i16 {{\n{body}\n}}\n\n{sections}\n");
        equivalent_programs(
            &program(hla_body),
            &program(native_body),
            hla_body,
            native_body,
        );
    };
    let banked = "far func banked @a8 @i16 { nop }";
    with_sections(banked, "    goto far banked", "    jml banked");
    with_sections(banked, "    far banked", "    jsl banked");
    with_sections(banked, "    call far banked", "    jsl banked");
    with_sections(
        "func near_one @a8 @i16 { nop }",
        "    call near_one",
        "    jsr near_one",
    );
}

/// A `far func` returns with `rtl`, written or implicit.
#[test]
fn a_far_section_returns_the_way_its_callers_arrived() {
    use k816::isa::Mnemonic;

    let mnemonics = |source: &str| -> Vec<Mnemonic> {
        lower_ops(source)
            .into_iter()
            .filter_map(|(kind, _)| match kind {
                KOpKind::Instruction(instruction) => Some(instruction.mnemonic),
                _ => None,
            })
            .collect()
    };
    assert_eq!(
        mnemonics("far func banked @a8 @i8 {\n    return\n}\n"),
        [Mnemonic::Rtl],
        "the written return is `rtl`, and nothing is appended after it"
    );
    assert_eq!(
        mnemonics("far func banked @a8 @i8 {\n    nop\n}\n"),
        [Mnemonic::Nop, Mnemonic::Rtl],
        "and the one added at the closing brace is `rtl` too"
    );
    assert_eq!(
        mnemonics("func near_one @a8 @i8 {\n    nop\n}\n"),
        [Mnemonic::Nop, Mnemonic::Rts],
        "a near section is untouched by any of it"
    );
}

/// An expansion is the body, written out where its name was.
///
/// The pairing E's discipline asks for, applied to the one construct that
/// changes the shape of the stream rather than adding to it: what a use site
/// produces has to equal what the same statements produce written in place.
#[test]
fn an_expansion_matches_the_body_written_out() {
    // Both halves declare the same sections, used or not: a symbol the one
    // program has and the other lacks shifts every later identifier, and the
    // comparison is over identifiers.
    let program = |body: &str| {
        format!(
            "var mem = $0400\nfunc work @a8 @i16 {{\n{body}\n}}\n\n\
             inline bump @a8 @i16 {{ mem++ }}\n\
             inline put @a8 @i16 (a, where) -> a {{ where = a }}\n"
        )
    };
    let kinds = |text: &str| -> Vec<KOpKind> {
        lower_ops(text).into_iter().map(|(kind, _)| kind).collect()
    };
    assert_eq!(
        kinds(&program("    bump\n    bump")),
        kinds(&program("    mem++\n    mem++")),
        "two expansions are the two statements"
    );
    // And the alias is the argument, not a name of its own.
    assert_eq!(
        kinds(&program("    put a, #mem -> a")),
        kinds(&program("    mem = a")),
    );
}

/// `naked` is the section kind with no closing `rts`, and nothing else.
#[test]
fn a_naked_section_ends_where_its_body_does() {
    use k816::isa::Mnemonic;

    let mnemonics = |source: &str| -> Vec<Mnemonic> {
        lower_ops(source)
            .into_iter()
            .filter_map(|(kind, _)| match kind {
                KOpKind::Instruction(instruction) => Some(instruction.mnemonic),
                _ => None,
            })
            .collect()
    };
    assert_eq!(
        mnemonics("naked f @a8 @i8 {\n    x++\n}\n"),
        [Mnemonic::Inx],
        "nothing is appended at the closing brace"
    );
    assert_eq!(
        mnemonics("func f @a8 @i8 {\n    x++\n}\n"),
        [Mnemonic::Inx, Mnemonic::Rts],
        "and a `func` is unchanged by any of it"
    );
    assert_eq!(
        mnemonics("naked f @a8 @i8 {\n    return_i\n}\n"),
        [Mnemonic::Rti],
        "`return_i` is `rti`, the interrupt handler's return"
    );
}
