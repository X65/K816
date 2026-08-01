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
        k816::symbol::SymbolKind::Var {
            address: Some(0x0400),
            size: k816::symbol::AddressSize::Absolute,
        }
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
    assert_eq!(diagnostics[0].code, DiagnosticCode::SYMBOL_KIND_MISMATCH);
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
        k816::symbol::SymbolKind::Const { value } => value,
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
    let source = "const FOO = 5\nconst A = -~FOO\nconst B = ~-FOO\n";
    assert_eq!(const_value(source, "A"), 6);
    assert_eq!(const_value(source, "B"), 4);
}

#[test]
fn character_literals_fold_as_atoms() {
    let source = "const A = 'A'\nconst NEXT = 'A' + 1\n";
    assert_eq!(const_value(source, "A"), u32::from(b'A'));
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
    let source = "const A = 0, B, C
const D, E, F
const BASE = 10
const X = BASE, Y, Z
const M = 0, N, O = 10, P
";
    for (name, expected) in [
        ("A", 0),
        ("B", 1),
        ("C", 2),
        ("D", 0),
        ("E", 1),
        ("F", 2),
        ("X", 10),
        ("Y", 11),
        ("Z", 12),
        ("M", 0),
        ("N", 1),
        ("O", 10),
        ("P", 11),
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
    let (_, diagnostics) = analyze("func f @a8 {\n    return\n}\nvar X = f + 1\n");
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
    let (_, diagnostics) = analyze("const A = MISSING + 1\n");
    assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
    assert_eq!(diagnostics[0].code, DiagnosticCode::UNDECLARED_SYMBOL);
    assert_eq!(diagnostics[0].title, "`MISSING` is not declared");
}

#[test]
fn both_sides_of_a_binary_expression_are_diagnosed() {
    // One undeclared name must not hide another.
    let (_, diagnostics) = analyze("const A = LEFT + RIGHT\n");
    let names: Vec<&str> = diagnostics.iter().map(|d| d.title.as_str()).collect();
    assert_eq!(names, ["`LEFT` is not declared", "`RIGHT` is not declared"]);
}

#[test]
fn a_compound_expression_in_a_data_value_list_is_rejected() {
    // The grammar closes most of this; parentheses are the hole it cannot,
    // because `&<(A | B)` is a legal parse of an illegal value.
    let (_, diagnostics) = analyze("const A = 1\nconst B = 2\ndata D {\n    byte &<(A | B)\n}\n");
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
    let (_, diagnostics) = analyze("var P = $80\ndata D {\n    byte &<P+1\n}\n");
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
    format!("var ZP = $80\nvar SCREEN = $0400\nfunc main @a8 @i16 {{\n    {body}\n}}\n")
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
        ("bne main", AddressingMode::Relative),
        ("brl main", AddressingMode::RelativeLong),
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

        let (program, lowered) = k816::lower::lower(&parse.file, &model);
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
    let (program, _) = k816::lower::lower(&parse.file, &model);
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
    let source = "const FIELD = 2\nvar ZP = $80\nfunc main @a8 @i16 {\n    lda FIELD\n}\n";
    let diagnostic = only_diagnostic(source);
    assert_eq!(diagnostic.code, DiagnosticCode::CONST_IN_ADDRESS_POSITION);
    assert_eq!(diagnostic.fixes[0].edits[0].replacement, "#");

    // The `&` marker is exactly how a const becomes a field offset.
    let source = "const FIELD = 2\nfunc main @a8 @i16 {\n    lda &FIELD,x\n}\n";
    let (_, diagnostics) = analyze(source);
    assert_eq!(diagnostics, vec![], "{diagnostics:?}");
}

#[test]
fn the_offset_marker_needs_an_index_register() {
    let source = "const FIELD = 2\nfunc main @a8 @i16 {\n    lda &FIELD\n}\n";
    let diagnostic = only_diagnostic(source);
    assert_eq!(diagnostic.code, DiagnosticCode::OFFSET_MARKER_POSITION);
    assert_eq!(diagnostic.fixes[0].edits[0].replacement, "");
}

#[test]
fn the_address_of_operators_produce_an_immediate_without_a_hash() {
    // `doc/syntax-reference.md`: "&&label and &&&label are the intentional
    // exception: they produce immediate relocations even without `#`".
    use k816::isa::AddressingMode;
    let source = "func main @a16 {\n  spot:\n    lda &&spot\n}\n";
    let mut map = SourceMap::new();
    let id = map.add_file("addrof.k816", source);
    let parse = parse_source(map.file(id), None);
    let (model, diagnostics) = analyze_file(&parse.file);
    assert_eq!(diagnostics, vec![], "{diagnostics:?}");
    let (program, _) = k816::lower::lower(&parse.file, &model);
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
    let (program, _) = k816::lower::lower(&parse.file, &model);
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
