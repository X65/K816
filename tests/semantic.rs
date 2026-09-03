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
    let source = "func work @a8 @i16 {
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

func main @a8 {
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
    assert_eq!(main.entry, Some(k816::abi::EntryKind::Main));
    assert_eq!(main.contract.a, Some(RegisterWidth::W8));
    assert_eq!(main.contract.i, None, "the axis it did not annotate");

    let screen = model.symbols.lookup("SCREEN").expect("SCREEN declared");
    assert_eq!(
        model.symbols.symbol(screen).kind,
        k816::symbol::SymbolKind::Var {
            address: Some(0x0400),
            size: k816::symbol::AddressSize::Absolute,
            width: None,
            element: 1,
            repeat: None,
            layout: None,
        }
    );
}

#[test]
fn width_dependent_immediate_without_contract_is_rejected() {
    // An ordinary function: its caller is bridged at the call site, so an
    // axis it did not annotate is genuinely unknown here.
    let (_, diagnostics) = analyze("func f {\n    a = #1\n}\n");
    assert_eq!(diagnostics.len(), 1);
    assert_eq!(diagnostics[0].code, DiagnosticCode::UNKNOWN_WIDTH);
    assert_eq!(
        diagnostics[0].help.as_deref(),
        Some("add @a8 or @a16 to the enclosing function, or set the width here")
    );

    // And it is the *same* refusal the native spelling gets, down to the
    // help and notes: one operand model answers for both syntaxes.
    let (_, native) = analyze("func f {\n    lda #1\n}\n");
    assert_eq!(native.len(), 1);
    assert_eq!(native[0].code, diagnostics[0].code);
    assert_eq!(native[0].help, diagnostics[0].help);
    assert_eq!(native[0].notes, diagnostics[0].notes);
}

/// The entry point is the exception, and this is what the entry ABI buys.
///
/// `main`'s caller is the environment, which cannot be bridged, so its widths
/// are the environment's whether it annotated them or not. An unannotated
/// legacy `main` therefore sizes its immediates instead of reporting one
/// `K816-E0104` per immediate — which is the whole reason the ABI exists as a
/// value rather than as an assumption in two passes.
#[test]
fn the_entry_point_takes_its_widths_from_the_environment() {
    let (_, diagnostics) = analyze("func main {\n    a = #1\n    x = #2\n}\n");
    assert_eq!(diagnostics, vec![], "the ABI answers both axes");

    // The value is 8-bit on both axes, so a two-byte immediate does not fit.
    let (_, diagnostics) = analyze("func main {\n    a = #$100\n}\n");
    assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
    assert_eq!(diagnostics[0].code, DiagnosticCode::IMMEDIATE_RANGE);

    // Restating the ABI is allowed and changes nothing.
    let (_, diagnostics) = analyze("func main @a8 @i8 {\n    a = #1\n    x = #2\n}\n");
    assert_eq!(diagnostics, vec![]);

    // Contradicting it is refused — the entry point cannot ask the
    // environment for widths it does not provide.
    let (_, diagnostics) = analyze("func main @a16 {\n    a = #$100\n}\n");
    assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
    assert_eq!(diagnostics[0].code, DiagnosticCode::ENTRY_ABI_MISMATCH);

    // Widening for the body is how a body gets wider registers, and that is
    // not a claim about the environment at all.
    let (_, diagnostics) = analyze("func main {\n    @a16 {\n        a = #$100\n    }\n}\n");
    assert_eq!(diagnostics, vec![]);
}

#[test]
fn immediate_must_fit_the_contracted_width() {
    let (_, diagnostics) = analyze("func work @a8 {\n    a = #256\n}\n");
    assert_eq!(diagnostics.len(), 1);
    assert_eq!(diagnostics[0].code, DiagnosticCode::IMMEDIATE_RANGE);

    // 16-bit accumulator accepts the same value.
    let (_, diagnostics) = analyze("func work @a16 {\n    a = #256\n}\n");
    assert_eq!(diagnostics, vec![]);

    // SEP/REP masks are fixed 8-bit regardless of contract.
    let (_, diagnostics) = analyze("func work @a16 {\n    sep #$100\n}\n");
    assert_eq!(diagnostics.len(), 1);
    assert_eq!(diagnostics[0].code, DiagnosticCode::IMMEDIATE_RANGE);
}

#[test]
fn duplicate_declarations_point_at_the_first_one() {
    let (_, diagnostics) = analyze("var AA = $10\nvar AA = $20\n");
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
    assert_eq!(diagnostics[0].code, DiagnosticCode::SYMBOL_KIND_MISMATCH);
    assert_eq!(diagnostics[0].title, "`f` is a function, not a variable");
    assert_eq!(diagnostics[0].secondary.len(), 1);
}

#[test]
fn functions_are_not_entry_points_unless_named_main() {
    let (model, diagnostics) = analyze("func helper @a8 {\n    return\n}\n");
    assert_eq!(diagnostics, vec![]);
    assert_eq!(model.functions.len(), 1);
    assert_eq!(model.functions[0].entry, None);
}

// ---------------------------------------------------------------------------
// The value and expression layer (sub-plan C, task C1).
// ---------------------------------------------------------------------------

/// The folded value of `name` after analyzing `source`.
fn const_value(source: &str, name: &str) -> u32 {
    let (model, diagnostics) = analyze(source);
    assert_eq!(diagnostics, vec![], "unexpected diagnostics");
    let id = model
        .symbols
        .lookup(name)
        .unwrap_or_else(|| panic!("`{name}` is declared"));
    match model.symbols.symbol(id).kind {
        k816::symbol::SymbolKind::Const { value } => {
            value.expect("an ordinary `const` has a value; only an `extern const` does not")
        }
        other => panic!("`{name}` is {other:?}, not a const"),
    }
}

#[test]
fn constants_fold_with_c_precedence() {
    // `doc/syntax-reference.md`, "Binary operators": the tree B builds
    // already encodes the precedence, so folding must not reorder anything.
    let source = "const FLAG_NMI = $80
const FLAG_DLI = $40
const FLAG_VBI = $20
const ALL_IRQ = FLAG_NMI | FLAG_DLI | FLAG_VBI
const NMI_NOT_DLI = FLAG_NMI & ~FLAG_DLI
const NMI_XOR_DLI = FLAG_NMI ^ FLAG_DLI
const BIT_3 = 1 << 3
const HIGH_NIBBLE = $FF >> 4
const OR_BINDS_LOOSEST = $01 | $06 & $04
const AND_BINDS_LOOSER_THAN_ADD = 1 + 2 & 2
";
    assert_eq!(const_value(source, "ALL_IRQ"), 0xE0);
    assert_eq!(const_value(source, "NMI_NOT_DLI"), 0x80);
    assert_eq!(const_value(source, "NMI_XOR_DLI"), 0xC0);
    assert_eq!(const_value(source, "BIT_3"), 0x08);
    assert_eq!(const_value(source, "HIGH_NIBBLE"), 0x0F);
    // `A | B & C` is `A | (B & C)`, not `(A | B) & C` — which would be $04.
    assert_eq!(const_value(source, "OR_BINDS_LOOSEST"), 0x05);
    // `A + B & C` is `(A + B) & C`, not `A + (B & C)` — which would be 3.
    assert_eq!(const_value(source, "AND_BINDS_LOOSER_THAN_ADD"), 2);
}

#[test]
fn unary_operators_repeat_in_the_order_written() {
    // `-~FOO` is Negate(BitNot(FOO)) and `~-FOO` is BitNot(Negate(FOO)).
    let source = "const FOO = 5\nconst AA = -~FOO\nconst BB = ~-FOO\n";
    assert_eq!(const_value(source, "AA"), 6);
    assert_eq!(const_value(source, "BB"), 4);
}

#[test]
fn character_literals_fold_as_atoms() {
    let source = "const AA = 'A'\nconst NEXT = 'A' + 1\n";
    assert_eq!(const_value(source, "AA"), u32::from(b'A'));
    assert_eq!(const_value(source, "NEXT"), u32::from(b'A') + 1);
}

#[test]
fn address_byte_operators_fold_over_absolute_values() {
    let source = "const FAR = $123456
const LO = &<FAR
const HI = &>FAR
const W = &&FAR
const L = &&&FAR
";
    assert_eq!(const_value(source, "LO"), 0x56);
    assert_eq!(const_value(source, "HI"), 0x34);
    assert_eq!(const_value(source, "W"), 0x3456);
    assert_eq!(const_value(source, "L"), 0x123456);
}

#[test]
fn const_groups_auto_increment() {
    // `doc/syntax-reference.md`, "Constant Declaration".
    let source = "const AA = 0, BB, CC
const DD, E, F
const BASE = 10
const XX = BASE, YY, ZZ
const M = 0, NN, OO = 10, PP
";
    for (name, expected) in [
        ("AA", 0),
        ("BB", 1),
        ("CC", 2),
        ("DD", 0),
        ("E", 1),
        ("F", 2),
        ("XX", 10),
        ("YY", 11),
        ("ZZ", 12),
        ("M", 0),
        ("NN", 1),
        ("OO", 10),
        ("PP", 11),
    ] {
        assert_eq!(const_value(source, name), expected, "`{name}`");
    }
}

#[test]
fn a_standalone_const_without_an_initializer_is_an_error() {
    let (_, diagnostics) = analyze("const LIMIT\n");
    assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
    let diagnostic = &diagnostics[0];
    assert_eq!(diagnostic.code, DiagnosticCode::NOT_CONSTANT);
    assert_eq!(diagnostic.title, "`const LIMIT` has no value");
    assert_eq!(diagnostic.fixes.len(), 1);
    assert_eq!(diagnostic.fixes[0].edits[0].replacement, " = 0");

    // The group form of the same shorthand stays legal.
    let (_, diagnostics) = analyze("const LIMIT, OTHER\n");
    assert_eq!(diagnostics, vec![]);
}

#[test]
fn a_link_time_value_is_rejected_only_where_an_absolute_one_is_required() {
    // A `var` address becomes a ca65 equate, so it has to be a number.
    let (_, diagnostics) = analyze("func f @a8 {\n    return\n}\nvar XX = f + 1\n");
    assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
    assert_eq!(diagnostics[0].code, DiagnosticCode::NOT_CONSTANT);
    assert_eq!(
        diagnostics[0].title,
        "variable addresses must be known at compile time"
    );
    assert_eq!(
        diagnostics[0].notes,
        ["`f` gets its address from ld65"],
        "the note names the symbol ld65 owns"
    );
}

#[test]
fn a_const_cannot_stand_in_for_a_memory_location() {
    let diagnostic = only_diagnostic("const K = $10\nfunc f @a8 {\n    lda K\n}\n");
    assert_eq!(diagnostic.code, DiagnosticCode::CONST_IN_ADDRESS_POSITION);
    assert_eq!(
        diagnostic.secondary.len(),
        1,
        "it points at the declaration"
    );
    assert_eq!(diagnostic.fixes[0].edits[0].replacement, "#");

    // A store has no immediate form, so no fix is offered there: `sta #K`
    // would trade one error for another.
    let diagnostic = only_diagnostic("const K = $10\nfunc f @a8 {\n    sta K\n}\n");
    assert_eq!(diagnostic.code, DiagnosticCode::CONST_IN_ADDRESS_POSITION);
    assert!(diagnostic.fixes.is_empty(), "{diagnostic:?}");

    // …but a `const` is exactly what an immediate wants.
    let (_, diagnostics) = analyze("const K = $10\nfunc f @a8 {\n    lda #K\n}\n");
    assert_eq!(diagnostics, vec![]);
}

#[test]
fn an_undeclared_name_inside_an_expression_names_itself() {
    let (_, diagnostics) = analyze("const AA = MISSING + 1\n");
    assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
    assert_eq!(diagnostics[0].code, DiagnosticCode::UNDECLARED_SYMBOL);
    assert_eq!(diagnostics[0].title, "`MISSING` is not declared");
}

#[test]
fn both_sides_of_a_binary_expression_are_diagnosed() {
    // One undeclared name must not hide another.
    let (_, diagnostics) = analyze("const AA = LEFT + RIGHT\n");
    let names: Vec<&str> = diagnostics.iter().map(|d| d.title.as_str()).collect();
    assert_eq!(names, ["`LEFT` is not declared", "`RIGHT` is not declared"]);
}

#[test]
fn a_compound_expression_in_a_data_value_list_is_rejected() {
    // The grammar closes most of this; parentheses are the hole it cannot,
    // because `&<(A | B)` is a legal parse of an illegal value.
    let (_, diagnostics) =
        analyze("const AA = 1\nconst BB = 2\ndata DD {\n    byte &<(AA | BB)\n}\n");
    let compound: Vec<&k816::diagnostic::Diagnostic> = diagnostics
        .iter()
        .filter(|d| d.code == DiagnosticCode::COMPOUND_DATA_VALUE)
        .collect();
    assert_eq!(compound.len(), 1, "{diagnostics:?}");
    assert!(
        compound[0]
            .help
            .as_deref()
            .is_some_and(|help| help.contains("`const`")),
        "the help points at the documented workaround"
    );

    // Offset arithmetic under an address-byte operator stays an atom.
    let (_, diagnostics) = analyze("var PP = $80\ndata DD {\n    byte &<PP+1\n}\n");
    assert!(
        !diagnostics
            .iter()
            .any(|d| d.code == DiagnosticCode::COMPOUND_DATA_VALUE),
        "{diagnostics:?}"
    );
}

// ---------------------------------------------------------------------------
// Symbols, labels, and address sizes (sub-plan C, task C2).
// ---------------------------------------------------------------------------

fn kind_of(model: &k816::semantic::SemanticModel, name: &str) -> k816::symbol::SymbolKind {
    let id = model
        .symbols
        .lookup(name)
        .unwrap_or_else(|| panic!("`{name}` is declared"));
    model.symbols.symbol(id).kind
}

#[test]
fn a_global_label_is_visible_before_it_is_written() {
    // Declaration is a real first pass, so a forward reference resolves.
    let source = "func main @a8 {\n    sta later\n  later:\n    return\n}\n";
    let (model, diagnostics) = analyze(source);
    assert_eq!(diagnostics, vec![], "{diagnostics:?}");
    assert!(matches!(
        kind_of(&model, "later"),
        k816::symbol::SymbolKind::Label {
            scope: k816::symbol::LabelScope::Global,
            ..
        }
    ));
}

#[test]
fn two_functions_may_reuse_one_local_label_name() {
    let source =
        "func one @a8 {\n  .loop:\n    return\n}\nfunc two @a8 {\n  .loop:\n    return\n}\n";
    let (model, diagnostics) = analyze(source);
    assert_eq!(diagnostics, vec![], "{diagnostics:?}");

    let one = model.symbols.lookup("one").unwrap();
    let two = model.symbols.lookup("two").unwrap();
    let first = model.symbols.lookup_local(one, ".loop").expect("one's");
    let second = model.symbols.lookup_local(two, ".loop").expect("two's");
    assert_ne!(first, second);
    // …and neither leaks into unit scope.
    assert_eq!(model.symbols.lookup(".loop"), None);
}

#[test]
fn a_local_label_of_another_function_is_not_in_scope() {
    let source = "func one @a8 {\n  .loop:\n    return\n}\nfunc two @a8 {\n    sta .loop\n}\n";
    let (_, diagnostics) = analyze(source);
    assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
    assert_eq!(diagnostics[0].code, DiagnosticCode::UNDECLARED_SYMBOL);
    assert_eq!(diagnostics[0].title, "`.loop` is not declared in `two`");
}

#[test]
fn a_local_label_declared_twice_in_one_function_is_a_duplicate() {
    let source = "func one @a8 {\n  .loop:\n  .loop:\n    return\n}\n";
    let (_, diagnostics) = analyze(source);
    assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
    assert_eq!(diagnostics[0].code, DiagnosticCode::DUPLICATE_SYMBOL);
}

#[test]
fn data_block_names_and_labels_are_unit_level() {
    // The block itself is still C8's to compile, so its own diagnostic is
    // expected; what this pins down is that its names are declared.
    let source = "data text {\n  hello:\n  1 2 3\n}\n";
    let (model, _) = analyze(source);
    for name in ["text", "hello"] {
        assert!(
            matches!(
                kind_of(&model, name),
                k816::symbol::SymbolKind::DataLabel { .. }
            ),
            "`{name}`"
        );
    }
}

#[test]
fn address_size_comes_from_the_prefix_or_else_the_value() {
    use k816::symbol::{AddressSize, SymbolKind};
    let source = "var ZP = $80\nvar MID = $0400\nvar FAR = $030000\nvar abs WIDE = $0012\n";
    let (model, diagnostics) = analyze(source);
    assert_eq!(diagnostics, vec![], "{diagnostics:?}");
    for (name, expected) in [
        ("ZP", AddressSize::DirectPage),
        ("MID", AddressSize::Absolute),
        ("FAR", AddressSize::Long),
        // The declaration prefix overrides what the value alone would pick.
        ("WIDE", AddressSize::Absolute),
    ] {
        let SymbolKind::Var { size, .. } = kind_of(&model, name) else {
            panic!("`{name}` is not a variable");
        };
        assert_eq!(size, expected, "`{name}`");
    }
}

#[test]
fn a_label_is_placed_by_ld65_and_never_carries_a_known_address() {
    let source = "func main @a8 {\n  spot:\n    return\n}\n";
    let (model, diagnostics) = analyze(source);
    assert_eq!(diagnostics, vec![]);
    assert_eq!(
        kind_of(&model, "spot").known_address(),
        None,
        "assuming an address for a label is what makes auto-shrink unsound"
    );
    assert_eq!(
        kind_of(&model, "spot").address_size(),
        Some(k816::symbol::AddressSize::Absolute)
    );
}

#[test]
fn duplicate_reports_point_at_the_earlier_declaration() {
    // Functions are declared in the first pass and variables in the second,
    // so the surviving symbol is not the earlier one — the label ordering
    // has to come from the spans, not the declaration order.
    let source = "var thing = $10\nfunc thing @a8 {\n    return\n}\n";
    let (_, diagnostics) = analyze(source);
    assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
    let diagnostic = &diagnostics[0];
    assert_eq!(diagnostic.code, DiagnosticCode::DUPLICATE_SYMBOL);
    assert!(
        diagnostic.secondary[0].span.start < diagnostic.primary.span.start,
        "`first declared here` must point at the earlier line"
    );
}

// ---------------------------------------------------------------------------
// Operand classification and address encoding (sub-plan C, task C4).
// ---------------------------------------------------------------------------

/// The single diagnostic `source` produces, or a panic naming what it did.
fn only_diagnostic(source: &str) -> k816::diagnostic::Diagnostic {
    let (_, mut diagnostics) = analyze(source);
    assert_eq!(diagnostics.len(), 1, "`{source}`: {diagnostics:?}");
    diagnostics.remove(0)
}

/// A function body wrapped around `body`, with both widths pinned.
fn body(body: &str) -> String {
    format!("var ZP = $80\nvar SCREEN = $0400\nfunc work @a8 @i16 {{\n    {body}\n}}\n")
}

#[test]
fn one_source_shape_picks_the_narrowest_encoding_that_holds_the_address() {
    use k816::isa::AddressingMode;
    // `lda ZP` and `lda SCREEN` are written identically and are different
    // instructions; the value decides, and only because K816 knows it.
    for (source, expected) in [
        ("lda ZP", AddressingMode::Direct),
        ("lda SCREEN", AddressingMode::Absolute),
        ("lda dp ZP", AddressingMode::Direct),
        ("lda abs ZP", AddressingMode::Absolute),
        ("lda far ZP", AddressingMode::AbsoluteLong),
        ("lda SCREEN,x", AddressingMode::AbsoluteIndexedX),
        ("lda ZP,x", AddressingMode::DirectIndexedX),
        // LDA has no direct-page `,y`, so the absolute form is the narrowest
        // one that exists at all.
        ("lda ZP,y", AddressingMode::AbsoluteIndexedY),
        ("lda (ZP,x)", AddressingMode::DirectIndexedIndirect),
        ("lda (ZP),y", AddressingMode::DirectIndirectIndexed),
        ("lda [ZP]", AddressingMode::DirectIndirectLong),
        ("lda [ZP],y", AddressingMode::DirectIndirectLongIndexed),
        ("lda 4,s", AddressingMode::StackRelative),
        ("lda (4,s),y", AddressingMode::StackRelativeIndirectIndexed),
        ("lda #1", AddressingMode::Immediate),
        ("asl a", AddressingMode::Accumulator),
        ("asl", AddressingMode::Accumulator),
        ("inx", AddressingMode::Implied),
        ("pha", AddressingMode::Stack),
        ("bne work", AddressingMode::Relative),
        ("brl work", AddressingMode::RelativeLong),
        ("mvn $01,$02", AddressingMode::BlockMove),
    ] {
        let mut map = SourceMap::new();
        let id = map.add_file("mode.k816", body(source));
        let parse = parse_source(map.file(id), None);
        assert_eq!(parse.diagnostics, vec![], "`{source}` did not parse");
        let (model, diagnostics) = analyze_file(&parse.file);
        // `brl main` tail-transfers into the function itself, which is
        // recursion and warns about its exit widths; the encoding is what
        // this test is about.
        let errors: Vec<&k816::diagnostic::Diagnostic> =
            diagnostics.iter().filter(|d| d.is_error()).collect();
        assert!(errors.is_empty(), "`{source}`: {errors:?}");

        let (program, lowered) = k816::lower::lower(model.program(&parse.file), &model);
        assert_eq!(lowered, vec![], "`{source}`");
        let modes: Vec<AddressingMode> = program
            .functions()
            .flat_map(|function| function.ops.iter())
            .filter_map(|op| match &op.kind {
                k816::op::KOpKind::Instruction(instruction) => Some(instruction.mode),
                _ => None,
            })
            .collect();
        assert!(modes.contains(&expected), "`{source}`: got {modes:?}");
    }
}

#[test]
fn a_label_never_auto_shrinks_to_the_direct_page() {
    // ld65 places the label, so K816 cannot prove page-zero fit — and
    // guessing would change the instruction's length.
    use k816::isa::AddressingMode;
    let source = "func main @a8 {\n  spot:\n    lda spot\n}\n";
    let mut map = SourceMap::new();
    let id = map.add_file("label.k816", source);
    let parse = parse_source(map.file(id), None);
    let (model, diagnostics) = analyze_file(&parse.file);
    assert_eq!(diagnostics, vec![]);
    let (program, _) = k816::lower::lower(model.program(&parse.file), &model);
    let modes: Vec<AddressingMode> = program
        .functions()
        .flat_map(|function| function.ops.iter())
        .filter_map(|op| match &op.kind {
            k816::op::KOpKind::Instruction(instruction) => Some(instruction.mode),
            _ => None,
        })
        .collect();
    assert!(modes.contains(&AddressingMode::Absolute), "{modes:?}");
    assert!(!modes.contains(&AddressingMode::Direct), "{modes:?}");
}

#[test]
fn an_encoding_prefix_the_instruction_lacks_is_rejected_with_the_legal_ones() {
    // `stx` has no absolute-indexed form at all, and no `,x` form either.
    let diagnostic = only_diagnostic(&body("lda dp SCREEN"));
    assert_eq!(diagnostic.code, DiagnosticCode::ADDRESS_RANGE);

    // `jmp` has no direct-page form, so `dp` names an encoding it lacks.
    let diagnostic = only_diagnostic(&body("jmp dp ZP"));
    assert_eq!(diagnostic.code, DiagnosticCode::INVALID_ENCODING);
    assert!(
        diagnostic
            .help
            .as_deref()
            .is_some_and(|help| help.contains("abs")),
        "the help names the legal prefixes: {diagnostic:?}"
    );
    assert_eq!(diagnostic.fixes.len(), 1);
}

#[test]
fn an_operand_shape_the_instruction_lacks_lists_what_it_accepts() {
    let diagnostic = only_diagnostic(&body("inx #1"));
    assert_eq!(diagnostic.code, DiagnosticCode::INVALID_OPERAND);
    assert!(
        diagnostic.notes[0].contains("`inx` accepts: i"),
        "{diagnostic:?}"
    );
}

#[test]
fn a_const_in_address_position_is_rejected_but_the_offset_marker_is_not() {
    let source = "const FIELD = 2\nvar ZP = $80\nfunc work @a8 @i16 {\n    lda FIELD\n}\n";
    let diagnostic = only_diagnostic(source);
    assert_eq!(diagnostic.code, DiagnosticCode::CONST_IN_ADDRESS_POSITION);
    assert_eq!(diagnostic.fixes[0].edits[0].replacement, "#");

    // The `&` marker is exactly how a const becomes a field offset.
    let source = "const FIELD = 2\nfunc work @a8 @i16 {\n    lda &FIELD,x\n}\n";
    let (_, diagnostics) = analyze(source);
    assert_eq!(diagnostics, vec![], "{diagnostics:?}");
}

#[test]
fn the_offset_marker_needs_an_index_register() {
    let source = "const FIELD = 2\nfunc work @a8 @i16 {\n    lda &FIELD\n}\n";
    let diagnostic = only_diagnostic(source);
    assert_eq!(diagnostic.code, DiagnosticCode::OFFSET_MARKER_POSITION);
    assert_eq!(diagnostic.fixes[0].edits[0].replacement, "");
}

#[test]
fn the_address_of_operators_produce_an_immediate_without_a_hash() {
    // `doc/syntax-reference.md`: "&&label and &&&label are the intentional
    // exception: they produce immediate relocations even without `#`".
    use k816::isa::AddressingMode;
    let source = "func work @a16 {\n  spot:\n    lda &&spot\n}\n";
    let mut map = SourceMap::new();
    let id = map.add_file("addrof.k816", source);
    let parse = parse_source(map.file(id), None);
    let (model, diagnostics) = analyze_file(&parse.file);
    assert_eq!(diagnostics, vec![], "{diagnostics:?}");
    let (program, _) = k816::lower::lower(model.program(&parse.file), &model);
    let modes: Vec<AddressingMode> = program
        .functions()
        .flat_map(|function| function.ops.iter())
        .filter_map(|op| match &op.kind {
            k816::op::KOpKind::Instruction(instruction) => Some(instruction.mode),
            _ => None,
        })
        .collect();
    assert!(modes.contains(&AddressingMode::Immediate), "{modes:?}");
}

#[test]
fn a_link_time_immediate_is_accepted_and_simply_not_range_checked() {
    // ca65 relocates it; K816 has no address to check it against.
    let source = "func main @a8 {\n  spot:\n    lda #&<spot\n}\n";
    let (_, diagnostics) = analyze(source);
    assert_eq!(diagnostics, vec![], "{diagnostics:?}");
}

#[test]
fn a_call_target_may_name_a_function_but_a_load_may_not() {
    let source = "func helper @a8 {\n    return\n}\nfunc main @a8 {\n    jsr helper\n}\n";
    let (_, diagnostics) = analyze(source);
    assert_eq!(diagnostics, vec![], "{diagnostics:?}");

    let source = "func helper @a8 {\n    return\n}\nfunc main @a8 {\n    lda helper\n}\n";
    let diagnostic = only_diagnostic(source);
    assert_eq!(diagnostic.code, DiagnosticCode::SYMBOL_KIND_MISMATCH);
}

#[test]
fn jmp_through_brackets_resolves_to_the_long_jump() {
    use k816::isa::{AddressingMode, Mnemonic};
    let source = "var VEC = $0400\nfunc main @a8 {\n    jmp [VEC]\n}\n";
    let mut map = SourceMap::new();
    let id = map.add_file("alias.k816", source);
    let parse = parse_source(map.file(id), None);
    let (model, diagnostics) = analyze_file(&parse.file);
    assert_eq!(diagnostics, vec![], "{diagnostics:?}");
    let (program, _) = k816::lower::lower(model.program(&parse.file), &model);
    let found = program
        .functions()
        .flat_map(|function| function.ops.iter())
        .any(|op| match &op.kind {
            k816::op::KOpKind::Instruction(instruction) => {
                instruction.mnemonic == Mnemonic::Jml
                    && instruction.mode == AddressingMode::AbsoluteIndirect
            }
            _ => false,
        });
    assert!(found, "`jmp [vec]` is the `jml [vec]` opcode");
}

#[test]
fn a_block_move_takes_bank_numbers_not_addresses() {
    let diagnostic = only_diagnostic(&body("mvn $1234,$02"));
    assert_eq!(diagnostic.code, DiagnosticCode::ADDRESS_RANGE);
    assert!(diagnostic.title.contains("bank byte"), "{diagnostic:?}");
}

// ---------------------------------------------------------------------------
// Data blocks (sub-plan C, task C8).
// ---------------------------------------------------------------------------

#[test]
fn a_segment_directive_must_be_a_blocks_first_entry() {
    // Its placement relative to bytes already emitted would otherwise be
    // undefined, which is the whole reason for the rule.
    let diagnostic = only_diagnostic("data bad {\n    1 2\n    segment OTHER\n    3\n}\n");
    assert_eq!(diagnostic.code, DiagnosticCode::SEGMENT_PLACEMENT);

    let (_, diagnostics) = analyze("data good {\n    segment OTHER\n    1 2\n}\n");
    assert_eq!(diagnostics, vec![], "{diagnostics:?}");
}

#[test]
fn a_nocross_run_larger_than_a_page_is_rejected_now() {
    // No placement can save it, so this is decided at compile time rather
    // than deferred to the link-time assertion.
    let diagnostic = only_diagnostic(include_str!("golden/nocross-oversized.k816"));
    assert_eq!(diagnostic.code, DiagnosticCode::NOCROSS_TOO_LARGE);
    assert!(diagnostic.title.contains("257 bytes"), "{diagnostic:?}");

    // A run that fits is guarded rather than rejected, and `nocross` takes
    // no operand: every value after it belongs to the run.
    let (_, diagnostics) = analyze("data ok {\n    nocross\n    1 2\n}\n");
    assert_eq!(diagnostics, vec![], "{diagnostics:?}");
}

#[test]
fn an_untyped_data_value_is_one_byte_wide() {
    let diagnostic = only_diagnostic("data wide {\n    $1234\n}\n");
    assert_eq!(diagnostic.code, DiagnosticCode::ADDRESS_RANGE);
    assert!(
        diagnostic
            .help
            .as_deref()
            .is_some_and(|help| help.contains("&<")),
        "the help names the byte selectors: {diagnostic:?}"
    );
}

#[test]
fn a_data_blocks_names_are_declared_and_addressable() {
    let source =
        "data text {\n    hello:\n    1 2\n}\nfunc main @a8 {\n    lda text\n    lda hello\n}\n";
    let (model, diagnostics) = analyze(source);
    assert_eq!(diagnostics, vec![], "{diagnostics:?}");
    for name in ["text", "hello"] {
        assert!(
            matches!(
                kind_of(&model, name),
                k816::symbol::SymbolKind::DataLabel { .. }
            ),
            "`{name}`"
        );
    }
}

/// The three declaration axes land on the symbol independently.
///
/// Sub-plan E's decision 14 exists because the document is explicit that they
/// are unrelated: `var dp pointer:far` is a 24-bit datum at a one-byte
/// address, and a compiler that collapsed the axes into one enum could not
/// represent it. These are the four answers a declaration produces —
/// encoding, datum width, element stride, repeat count — checked against
/// declarations that vary each one on its own.
#[test]
fn a_declarations_axes_stay_independent_on_the_symbol() {
    use k816::symbol::{AddressSize, DataWidth, SymbolKind};

    let source = "\
var plain
var dp small
var far rom = $C08000
var w:word
var dp pointer:far = $F0
var buffer[32]
var slots:word * 4
var pairs:word[2] * 3
";
    let (model, diagnostics) = analyze(source);
    assert_eq!(diagnostics, vec![]);

    let axes = |name: &str| match model
        .symbols
        .symbol(
            model
                .symbols
                .lookup(name)
                .unwrap_or_else(|| panic!("`{name}` declared")),
        )
        .kind
    {
        SymbolKind::Var {
            size,
            width,
            element,
            repeat,
            ..
        } => (size, width, element, repeat),
        other => panic!("`{name}` is {other:?}"),
    };

    // Encoding follows the storage class, or the value when there is none.
    assert_eq!(axes("plain").0, AddressSize::Absolute);
    assert_eq!(axes("small").0, AddressSize::DirectPage);
    assert_eq!(axes("rom").0, AddressSize::Long);

    // The datum width is the `:` suffix and nothing else. An unwritten one
    // is `None` rather than a defaulted byte: it is the absence of a claim,
    // and one byte is a claim.
    assert_eq!(axes("plain").1, None);
    assert_eq!(axes("w").1, Some(DataWidth::Word));

    // Both axes at once, on one declaration, disagreeing — and legal.
    assert_eq!(
        (axes("pointer").0, axes("pointer").1),
        (AddressSize::DirectPage, Some(DataWidth::Far))
    );

    // The element is the datum times the raw span; the count is outside it.
    assert_eq!((axes("buffer").2, axes("buffer").3), (32, None));
    assert_eq!((axes("slots").2, axes("slots").3), (2, Some(4)));
    assert_eq!((axes("pairs").2, axes("pairs").3), (4, Some(3)));
}

/// A field list separates its entries by a newline, by a comma, or by both,
/// and tolerates a trailing comma — and every spelling packs the same layout.
///
/// K65 is newline-insensitive everywhere else and `var a, b, c` is a comma
/// list already, so the field list was the one place a newline was
/// load-bearing. The legacy K816's own fixtures wrote the comma form, which
/// is why four of them would not parse here.
#[test]
fn a_field_list_takes_a_comma_a_newline_or_both() {
    use k816::symbol::SymbolKind;

    let spellings = [
        "var gpio[.ddr :byte, .data :byte, .latch :word]\n",
        "var gpio[.ddr :byte .data :byte .latch :word]\n",
        "var gpio[\n    .ddr :byte,\n    .data :byte,\n    .latch :word,\n]\n",
        "var gpio[\n    .ddr :byte\n    .data :byte\n    .latch :word\n]\n",
        // A comma and a newline together, and a nested list that ends on one.
        "var gpio[\n    .ddr :byte,\n    .data :byte, .latch :word,\n]\n",
    ];
    for source in spellings {
        let (model, diagnostics) = analyze(source);
        assert_eq!(diagnostics, vec![], "{source:?}");
        let id = model.symbols.lookup("gpio").expect("declared");
        let SymbolKind::Var { layout, .. } = model.symbols.symbol(id).kind else {
            panic!("`gpio` is not a var");
        };
        let layout = model.symbols.layout(layout.expect("a field list"));
        assert_eq!(layout.size, 4, "{source:?}");
        assert_eq!(layout.field("ddr").expect("packed").offset, 0);
        assert_eq!(layout.field("data").expect("packed").offset, 1);
        assert_eq!(layout.field("latch").expect("packed").offset, 2);
    }
}

/// A nested list takes the separator too, and the comma after a nested list
/// belongs to the list that contains it.
#[test]
fn a_nested_field_list_takes_the_separator_as_well() {
    use k816::symbol::SymbolKind;

    let (model, diagnostics) = analyze(
        "var REGS[\n    .status :byte,\n    .data[.lo :byte, .hi :byte],\n    .end :byte\n]\n",
    );
    assert_eq!(diagnostics, vec![]);
    let id = model.symbols.lookup("REGS").expect("declared");
    let SymbolKind::Var { layout, .. } = model.symbols.symbol(id).kind else {
        panic!("`REGS` is not a var");
    };
    let layout = model.symbols.layout(layout.expect("a field list"));
    assert_eq!(layout.size, 4);
    assert_eq!(layout.field("data").expect("packed").offset, 1);
    assert_eq!(layout.field("end").expect("packed").offset, 3);
}

/// What a field list packs, and what the three widths in one declaration
/// each decide.
#[test]
fn a_layout_packs_its_fields_in_order_and_nests_them() {
    use k816::symbol::{DataWidth, FieldShape, SymbolKind};

    let source = "\
var TASKS[
    .sp      :word
    .state   :byte
    .signals :byte
] = $4000
var baz:word[
    .a
    .b
    .len :byte
]
var REGS[
    .status :byte
    .data[
        .lo :byte
        .hi :byte
    ]
] = $D000
abstract var DEV[ .open :word .close :word .init :word ]
";
    let (model, diagnostics) = analyze(source);
    assert_eq!(diagnostics, vec![]);

    let layout_of = |name: &str| {
        let id = model.symbols.lookup(name).expect("declared");
        match model.symbols.symbol(id).kind {
            SymbolKind::Var { layout, .. } => layout.expect("a field list"),
            SymbolKind::AbstractVar { layout } => layout,
            other => panic!("`{name}` is {other:?}"),
        }
    };

    // Packed and sequential, with no padding: each offset is the sum of the
    // sizes before it, and the layout's size is the sum of them all.
    let tasks = model.symbols.layout(layout_of("TASKS"));
    assert_eq!(tasks.size, 4);
    assert_eq!(tasks.field("sp").expect("packed").offset, 0);
    assert_eq!(tasks.field("state").expect("packed").offset, 2);
    assert_eq!(tasks.field("signals").expect("packed").offset, 3);

    // The declaration's `:word` is the *default field width* here, and a
    // field that names its own replaces it — so the symbol itself claims no
    // datum width at all, since the datum is the whole layout.
    let baz = model.symbols.layout(layout_of("baz"));
    assert_eq!(baz.size, 5);
    assert_eq!(
        baz.field("a").expect("packed").shape,
        FieldShape::Scalar(DataWidth::Word)
    );
    assert_eq!(
        baz.field("len").expect("packed").shape,
        FieldShape::Scalar(DataWidth::Byte)
    );
    let SymbolKind::Var { width, element, .. } = model
        .symbols
        .symbol(model.symbols.lookup("baz").expect("declared"))
        .kind
    else {
        panic!("`baz` is a variable");
    };
    assert_eq!(width, None, "the suffix went to the fields, not the datum");
    assert_eq!(element, 5, "and `baz:sizeof` is the layout's size");

    // A nested field is sized by the layout it names, and its own fields
    // keep offsets relative to it.
    let regs = model.symbols.layout(layout_of("REGS"));
    assert_eq!(regs.size, 3);
    let data = regs.field("data").expect("packed");
    assert_eq!(data.offset, 1);
    let FieldShape::Nested(inner) = data.shape else {
        panic!("`.data` nests");
    };
    let inner = model.symbols.layout(inner);
    assert_eq!(inner.size, 2);
    assert_eq!(inner.field("hi").expect("packed").offset, 1);

    // A layout with no storage answers the same questions and reserves
    // nothing.
    assert_eq!(model.symbols.layout(layout_of("DEV")).size, 6);
    let dev = model.symbols.lookup("DEV").expect("declared");
    assert!(
        !model
            .reservations
            .values()
            .any(|reservation| reservation.symbol == dev),
        "an `abstract var` asks the linker for nothing"
    );
}

/// Every way an `abstract var` can ask for storage it cannot have.
///
/// One code for all of them, because they are one misunderstanding — this
/// declares a layout, and a layout has no bytes — and one diagnostic per
/// declaration, because a declaration writing two of them would otherwise
/// hear the same news twice.
#[test]
fn an_abstract_layout_may_not_ask_for_storage() {
    for source in [
        "abstract var PP:byte[ .x ] = $4000\n", // an address
        "abstract var dp PP[ .x :byte ]\n",     // a storage class
        "abstract var far PP[ .x :byte ]\n",
        "abstract var PP[ .x :byte ] * 4\n", // an allocation count
        "abstract var PP[16]\n",             // a raw span, and no fields
        "abstract var PP\n",                 // nothing to declare at all
    ] {
        let (model, diagnostics) = analyze(source);
        assert_eq!(diagnostics.len(), 1, "`{source}`: {diagnostics:?}");
        assert_eq!(
            diagnostics[0].code,
            DiagnosticCode::ABSTRACT_HAS_NO_STORAGE,
            "`{source}`"
        );
        // Refused, and still declared: a use of the name should report what
        // is wrong with the use, not that the name does not exist.
        let id = model.symbols.lookup("PP").expect("declared anyway");
        assert!(matches!(
            model.symbols.symbol(id).kind,
            k816::symbol::SymbolKind::AbstractVar { .. }
        ));
        // And it reserves nothing whatever it was refused for.
        assert!(
            !model
                .reservations
                .values()
                .any(|reservation| reservation.symbol == id),
            "`{source}` reserved bytes"
        );
    }

    // The layout itself is still what the author wrote, so the queries that
    // are the whole point of the declaration still answer.
    let (model, _) = analyze("abstract var PP[ .x :word .y :word ]\nvar q = PP:sizeof\n");
    assert_eq!(
        model
            .symbols
            .symbol(model.symbols.lookup("q").expect("declared"))
            .kind,
        k816::symbol::SymbolKind::Var {
            address: Some(4),
            size: k816::symbol::AddressSize::DirectPage,
            width: None,
            element: 1,
            repeat: None,
            layout: None,
        }
    );
}

/// Where a field's width comes from, and what its bracket counts.
///
/// Three sources, in order of nearness: the field's own suffix, the default
/// the enclosing declaration set, and `:byte`. The bracket is the other half
/// — on a field it is a *count* of elements of that width, where the same
/// bracket on a declaration is a raw span.
#[test]
fn a_field_takes_the_nearest_width_and_counts_elements_of_it() {
    use k816::symbol::{DataWidth, FieldShape, SymbolKind};

    let source = "\
var vv:word[
    .plain
    .narrow  :byte
    .wide    :far
    .repeats[3]
    .counted :byte [5]
    .nested:byte[
        .c
        .d :word
    ]
    .deep[
        .middle[
            .leaf
        ]
    ]
]
";
    let (model, diagnostics) = analyze(source);
    assert_eq!(diagnostics, vec![]);

    let SymbolKind::Var {
        layout, element, ..
    } = model
        .symbols
        .symbol(model.symbols.lookup("vv").expect("declared"))
        .kind
    else {
        panic!("`vv` is a variable");
    };
    let layout = model.symbols.layout(layout.expect("a field list"));
    let field = |name: &str| layout.field(name).unwrap_or_else(|| panic!("`{name}`"));

    // The declaration's default, a field's own suffix, and `:far`.
    assert_eq!(field("plain").shape, FieldShape::Scalar(DataWidth::Word));
    assert_eq!(field("narrow").shape, FieldShape::Scalar(DataWidth::Byte));
    assert_eq!(field("wide").shape, FieldShape::Scalar(DataWidth::Far));

    // `.repeats[3]` is the shorthand: three of the *default* width, not
    // three bytes. `.counted :byte [5]` names its own width and counts that.
    assert_eq!(field("repeats").count, 3);
    assert_eq!(field("repeats").shape, FieldShape::Scalar(DataWidth::Word));
    assert_eq!(field("repeats").size(model.symbols.layouts()), 6);
    assert_eq!(field("counted").count, 5);
    assert_eq!(field("counted").size(model.symbols.layouts()), 5);

    // Offsets are the running sum, so every width above is visible in them.
    assert_eq!(field("plain").offset, 0);
    assert_eq!(field("narrow").offset, 2);
    assert_eq!(field("wide").offset, 3);
    assert_eq!(field("repeats").offset, 6);
    assert_eq!(field("counted").offset, 12);
    assert_eq!(field("nested").offset, 17);

    // A width written before a nested list is that list's default, and it
    // replaces the declaration's for the fields inside it.
    let FieldShape::Nested(nested) = field("nested").shape else {
        panic!("`.nested` nests");
    };
    let nested = model.symbols.layout(nested);
    assert_eq!(
        nested.field("c").expect("packed").shape,
        FieldShape::Scalar(DataWidth::Byte),
        "the nested list's own `:byte` default"
    );
    assert_eq!(
        nested.field("d").expect("packed").shape,
        FieldShape::Scalar(DataWidth::Word)
    );
    assert_eq!(nested.size, 3);

    // Nesting is not limited to one level, and a list that names no width
    // inherits the enclosing default rather than falling back to `:byte`.
    let FieldShape::Nested(deep) = field("deep").shape else {
        panic!("`.deep` nests");
    };
    let FieldShape::Nested(middle) = model
        .symbols
        .layout(deep)
        .field("middle")
        .expect("packed")
        .shape
    else {
        panic!("`.middle` nests");
    };
    let middle = model.symbols.layout(middle);
    assert_eq!(
        middle.field("leaf").expect("packed").shape,
        FieldShape::Scalar(DataWidth::Word),
        "the declaration's default reaches the innermost list"
    );

    // 2 + 1 + 3 + 6 + 5 + 3 + 2, packed and in order — and that is the whole
    // element, which is what `v:sizeof` answers.
    assert_eq!(layout.size, 22);
    assert_eq!(element, 22);
}

/// A field name is unique inside *its own* list, not inside the declaration.
#[test]
fn a_nested_list_has_its_own_field_namespace() {
    // `.lo` twice in one list is a name that could never be reached again.
    let (_, diagnostics) = analyze("var outer[\n  .data[\n    .lo :byte\n    .lo :word\n  ]\n]\n");
    assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
    assert_eq!(diagnostics[0].code, DiagnosticCode::DUPLICATE_SYMBOL);

    // The same name in two *different* lists is two fields, and neither
    // shadows the other: `outer.a.lo` and `outer.b.lo` are different offsets.
    let (model, diagnostics) = analyze("var outer[\n  .a[ .lo :byte ]\n  .b[ .lo :byte ]\n]\n");
    assert_eq!(diagnostics, vec![]);
    let k816::symbol::SymbolKind::Var { layout, .. } = model
        .symbols
        .symbol(model.symbols.lookup("outer").expect("declared"))
        .kind
    else {
        panic!("`outer` is a variable");
    };
    let layout = model.symbols.layout(layout.expect("a field list"));
    assert_eq!(layout.field("a").expect("packed").offset, 0);
    assert_eq!(layout.field("b").expect("packed").offset, 1);
    assert_eq!(layout.size, 2);
}

/// What a declaration reserves, and which of them reserve nothing.
#[test]
fn only_an_allocated_variable_asks_the_linker_for_bytes() {
    let source = "\
var pinned = $2000
var dp slot = $10
var counter
var dp fast
var grid:word[4] * 2
";
    let (model, diagnostics) = analyze(source);
    assert_eq!(diagnostics, vec![]);

    let of = |name: &str| {
        let id = model.symbols.lookup(name).expect("declared");
        model
            .reservations
            .values()
            .find(|reservation| reservation.symbol == id)
            .copied()
    };
    // A pinned variable is an equate: someone else owns those bytes.
    assert_eq!(of("pinned"), None);
    assert_eq!(of("slot"), None);

    let counter = of("counter").expect("allocated");
    assert_eq!((counter.bytes, counter.direct_page), (1, false));
    let fast = of("fast").expect("allocated");
    assert_eq!((fast.bytes, fast.direct_page), (1, true));
    // 2 elements x 4 words.
    assert_eq!(of("grid").expect("allocated").bytes, 16);
}
