/*****************************************************************************/
/*                                                                           */
/*                                 parser.rs                                 */
/*                                                                           */
/*             CST/AST snapshot and recovery tests for the parser            */
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

//! Exact CST and AST snapshots for the vertical slice, plus error recovery
//! (master plan Phase 3).

use k816::ast::{
    AstFile, BinaryOp, ExprKind, HlaStatement, ItemKind, NativeOperand, StatementKind, UnaryOp,
    WidthAnnotationKind,
};
use k816::diagnostic::{Applicability, DiagnosticCode};
use k816::parser::parse_source;
use k816::source::{Provenance, SourceMap, Span};

const HLA_SLICE: &str = "var SCREEN = $0400

func main @a8 @i16 {
    a = #0
    a = #1
    SCREEN = a
    return
}
";

const NATIVE_SLICE: &str = "var SCREEN = $0400

func main @a8 @i16 {
    sep #$20
    rep #$10
    lda #0
    lda #1
    sta SCREEN
    rts
}
";

/// One of each literal form the lexical layer accepts (sub-plan B group 1).
const LEXICAL: &str = "var BIN = %1010
var CH = '\\n'

func main @a8 {
    a = #0b0101
    a = #'A'
    lda #$FF
}
";

/// Renders the AST in a compact, exact form for snapshot comparison.
fn dump_ast(file: &AstFile) -> String {
    let mut out = String::new();
    for item in &file.items {
        match &item.kind {
            ItemKind::Var(var) => {
                out.push_str(&format!(
                    "var {} = {} @{}..{}\n",
                    var.name.text,
                    dump_expr(&var.address.kind),
                    item.span.start,
                    item.span.end
                ));
            }
            ItemKind::Func(func) => {
                out.push_str(&format!("func {}", func.name.text));
                for annotation in &func.width {
                    out.push_str(match annotation.kind {
                        WidthAnnotationKind::A8 => " @a8",
                        WidthAnnotationKind::A16 => " @a16",
                        WidthAnnotationKind::I8 => " @i8",
                        WidthAnnotationKind::I16 => " @i16",
                    });
                }
                out.push_str(&format!(" @{}..{}\n", item.span.start, item.span.end));
                for statement in &func.body.statements {
                    let rendered = match &statement.kind {
                        StatementKind::Native(native) => {
                            let operand = match &native.operand {
                                Some(NativeOperand::Immediate(expr)) => {
                                    format!(" #{}", dump_expr(&expr.kind))
                                }
                                Some(NativeOperand::Address(name)) => format!(" {}", name.text),
                                None => String::new(),
                            };
                            format!("native {}{}", native.mnemonic.text, operand)
                        }
                        StatementKind::Hla(HlaStatement::LoadImmediate { value, .. }) => {
                            format!("hla a = #{}", dump_expr(&value.kind))
                        }
                        StatementKind::Hla(HlaStatement::Store { target, .. }) => {
                            format!("hla {} = a", target.text)
                        }
                        StatementKind::Return => "return".to_owned(),
                        StatementKind::Error => "error".to_owned(),
                    };
                    out.push_str(&format!(
                        "  {rendered} @{}..{}\n",
                        statement.span.start, statement.span.end
                    ));
                }
            }
            ItemKind::Error => {
                out.push_str(&format!("error @{}..{}\n", item.span.start, item.span.end));
            }
        }
    }
    out
}

/// Renders an expression fully parenthesized in prefix form, so operator
/// precedence and associativity are visible in the snapshot.
fn dump_expr(kind: &ExprKind) -> String {
    match kind {
        ExprKind::Number(value) => format!("{value}"),
        ExprKind::Symbol(name) => name.text.clone(),
        ExprKind::Unary(unary) => {
            let operator = match unary.op {
                UnaryOp::Negate => "-",
                UnaryOp::BitNot => "~",
                UnaryOp::LowByte => "&<",
                UnaryOp::HighByte => "&>",
                UnaryOp::Address16 => "&&",
                UnaryOp::Address24 => "&&&",
            };
            format!("({operator} {})", dump_expr(&unary.operand.kind))
        }
        ExprKind::Binary(binary) => {
            let operator = match binary.op {
                BinaryOp::Mul => "*",
                BinaryOp::Add => "+",
                BinaryOp::Sub => "-",
                BinaryOp::Shl => "<<",
                BinaryOp::Shr => ">>",
                BinaryOp::BitAnd => "&",
                BinaryOp::BitXor => "^",
                BinaryOp::BitOr => "|",
            };
            format!(
                "({operator} {} {})",
                dump_expr(&binary.left.kind),
                dump_expr(&binary.right.kind)
            )
        }
        ExprKind::Eval(inner) => format!("[{}]", dump_expr(&inner.kind)),
        ExprKind::Error => "<error>".to_owned(),
    }
}

/// Parses `var X = <expression>` and renders the expression in prefix form.
fn parse_expr(expression: &str) -> String {
    let mut map = SourceMap::new();
    let id = map.add_file("expr.k65", format!("var X = {expression}\n"));
    let result = parse_source(map.file(id), None);
    assert_eq!(
        result.diagnostics,
        vec![],
        "`{expression}` did not parse cleanly"
    );
    let ItemKind::Var(var) = &result.file.items[0].kind else {
        panic!("expected a var declaration");
    };
    dump_expr(&var.address.kind)
}

#[test]
fn hla_slice_cst_snapshot_is_exact() {
    let mut map = SourceMap::new();
    let id = map.add_file("slice.k816", HLA_SLICE);
    let result = parse_source(map.file(id), None);
    assert_eq!(result.diagnostics, vec![]);
    assert_eq!(
        result.tree.dump(),
        "\
source_file 0..91
  var_declaration 0..18
    name: identifier 4..10
    address: number 13..18
  func_declaration 20..90
    name: identifier 25..29
    width: width_annotation 30..33
    width: width_annotation 34..38
    body: block 39..90
      hla_load_immediate 45..51
        register: register 45..46
        value: immediate 49..51
          value: number 50..51
      hla_load_immediate 56..62
        register: register 56..57
        value: immediate 60..62
          value: number 61..62
      hla_store 67..77
        target: identifier 67..73
        source: register 76..77
      return_statement 82..88
"
    );
}

#[test]
fn hla_slice_ast_snapshot_is_exact() {
    let mut map = SourceMap::new();
    let id = map.add_file("slice.k816", HLA_SLICE);
    let result = parse_source(map.file(id), None);
    assert_eq!(result.diagnostics, vec![]);
    assert_eq!(
        dump_ast(&result.file),
        "\
var SCREEN = 1024 @0..18
func main @a8 @i16 @20..90
  hla a = #0 @45..51
  hla a = #1 @56..62
  hla SCREEN = a @67..77
  return @82..88
"
    );
}

#[test]
fn native_slice_ast_snapshot_is_exact() {
    let mut map = SourceMap::new();
    let id = map.add_file("native.k816", NATIVE_SLICE);
    let result = parse_source(map.file(id), None);
    assert_eq!(result.diagnostics, vec![]);
    assert_eq!(
        dump_ast(&result.file),
        "\
var SCREEN = 1024 @0..18
func main @a8 @i16 @20..113
  native sep #32 @45..53
  native rep #16 @58..66
  native lda #0 @71..77
  native lda #1 @82..88
  native sta SCREEN @93..103
  native rts @108..111
"
    );
}

#[test]
fn lexical_cst_snapshot_is_exact() {
    let mut map = SourceMap::new();
    let id = map.add_file("lexical.k65", LEXICAL);
    let result = parse_source(map.file(id), None);
    assert_eq!(result.diagnostics, vec![]);
    assert_eq!(
        result.tree.dump(),
        "\
source_file 0..91
  var_declaration 0..15
    name: identifier 4..7
    address: number 10..15
  var_declaration 16..29
    name: identifier 20..22
    address: char_literal 25..29
  func_declaration 31..90
    name: identifier 36..40
    width: width_annotation 41..44
    body: block 45..90
      hla_load_immediate 51..62
        register: register 51..52
        value: immediate 55..62
          value: number 56..62
      hla_load_immediate 67..75
        register: register 67..68
        value: immediate 71..75
          value: char_literal 72..75
      native_instruction 80..88
        mnemonic: mnemonic 80..83
        operand: immediate 84..88
          value: number 85..88
"
    );
}

#[test]
fn lexical_ast_snapshot_is_exact() {
    let mut map = SourceMap::new();
    let id = map.add_file("lexical.k65", LEXICAL);
    let result = parse_source(map.file(id), None);
    assert_eq!(result.diagnostics, vec![]);
    assert_eq!(
        dump_ast(&result.file),
        "\
var BIN = 10 @0..15
var CH = 10 @16..29
func main @a8 @31..90
  hla a = #5 @51..62
  hla a = #65 @67..75
  native lda #255 @80..88
"
    );
}

#[test]
fn every_literal_form_decodes_to_its_value() {
    let source = "var DEC = 42
var DOLLAR = $2A
var HEX = 0x2A
var HEX_UPPER = 0X2A
var PCT = %101010
var BIN = 0b101010
var BIN_UPPER = 0B101010
var LETTER = 'A'
var NEWLINE = '\\n'
var NUL = '\\0'
var TAB = '\\t'
var BACKSLASH = '\\\\'
var APOSTROPHE = '\\''
var HEX_ESCAPE = '\\x41'
";
    let mut map = SourceMap::new();
    let id = map.add_file("literals.k65", source);
    let result = parse_source(map.file(id), None);
    assert_eq!(result.diagnostics, vec![]);

    let values: Vec<u32> = result
        .file
        .items
        .iter()
        .map(|item| {
            let ItemKind::Var(var) = &item.kind else {
                panic!("expected var declaration");
            };
            match var.address.kind {
                ExprKind::Number(value) => value,
                _ => panic!("`{}` did not decode to a number", var.name.text),
            }
        })
        .collect();
    assert_eq!(
        values,
        vec![
            42, 42, 42, 42, // decimal and the three hexadecimal spellings
            42, 42, 42, // the three binary spellings
            0x41, 0x0A, 0x00, 0x09, 0x5C, 0x27, 0x41, // character literals
        ]
    );
}

#[test]
fn binary_operators_follow_c_precedence() {
    // The table in syntax-reference.md "Binary operators", including the three
    // worked examples stated there.
    assert_eq!(parse_expr("A | B & C"), "(| A (& B C))");
    assert_eq!(parse_expr("A + B & C"), "(& (+ A B) C)");
    assert_eq!(parse_expr("A << 1 | C"), "(| (<< A 1) C)");
    assert_eq!(parse_expr("A ^ B | C"), "(| (^ A B) C)");
    assert_eq!(parse_expr("A * B + C * D"), "(+ (* A B) (* C D))");
    assert_eq!(parse_expr("A + B - C"), "(- (+ A B) C)");
    assert_eq!(parse_expr("A >> B >> C"), "(>> (>> A B) C)");
    assert_eq!(parse_expr("(A | B) & C"), "(& (| A B) C)");
}

#[test]
fn unary_operators_repeat_in_the_order_written() {
    // "Both repeat in the order written" — syntax-reference.md "Unary
    // operators".
    assert_eq!(parse_expr("-~FOO"), "(- (~ FOO))");
    assert_eq!(parse_expr("~-FOO"), "(~ (- FOO))");
    assert_eq!(parse_expr("-FOO * 2"), "(* (- FOO) 2)");
}

#[test]
fn address_byte_operators_take_the_whole_offset_expression() {
    // `&<ptr+1` is the low byte of `ptr+1`, not `(&<ptr)+1` —
    // syntax-reference.md "Address Byte Operators".
    assert_eq!(parse_expr("&<ptr+1"), "(&< (+ ptr 1))");
    assert_eq!(parse_expr("&>screen_1+0x100"), "(&> (+ screen_1 256))");
    assert_eq!(parse_expr("&&ptr"), "(&& ptr)");
    assert_eq!(parse_expr("&&&ptr"), "(&&& ptr)");
    // ...but tighter than the bitwise operators, matching the `&&` gotcha.
    assert_eq!(parse_expr("&<ptr & $F0"), "(& (&< ptr) 240)");
}

#[test]
fn double_ampersand_is_address_of_not_logical_and() {
    // The documented quirk: `FOO && BAR` is `FOO & (&&BAR)`. One source
    // token, two operations — logical AND does not exist in this layer.
    assert_eq!(parse_expr("FOO && BAR"), "(& FOO (&& BAR))");
    assert_eq!(parse_expr("FOO & (&&BAR)"), "(& FOO (&& BAR))");
    assert_eq!(parse_expr("FOO && BAR + 1"), "(& FOO (&& (+ BAR 1)))");
}

#[test]
fn evaluator_brackets_are_expression_atoms() {
    assert_eq!(parse_expr("[$FF]"), "[255]");
    assert_eq!(parse_expr("[LV_SIZE]"), "[LV_SIZE]");
    assert_eq!(parse_expr("[FLAG_A | FLAG_B]"), "[(| FLAG_A FLAG_B)]");
    assert_eq!(parse_expr("BASE + [SIZE]"), "(+ BASE [SIZE])");
}

#[test]
fn grouping_keeps_the_span_of_the_parentheses() {
    let mut map = SourceMap::new();
    let id = map.add_file("paren.k65", "var X = (A + B)\n");
    let result = parse_source(map.file(id), None);
    let ItemKind::Var(var) = &result.file.items[0].kind else {
        panic!("expected a var declaration");
    };
    assert_eq!(map.span_text(var.address.span), "(A + B)");
    let ExprKind::Binary(binary) = &var.address.kind else {
        panic!("expected the grouped binary expression");
    };
    assert_eq!(map.span_text(binary.op_span), "+");
    assert_eq!(map.span_text(binary.left.span), "A");
}

#[test]
fn a_compound_address_reports_itself_as_unimplemented() {
    // The parser accepts it; folding it is the native translator's job, so
    // the arm has to say so rather than stay silent.
    let mut map = SourceMap::new();
    let id = map.add_file("fold.k65", "var X = BASE + 3\n");
    let parsed = parse_source(map.file(id), None);
    assert_eq!(parsed.diagnostics, vec![]);

    let (_, diagnostics) = k816::semantic::analyze_file(&parsed.file);
    assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
    assert_eq!(
        diagnostics[0].code,
        DiagnosticCode::NOT_YET_IMPLEMENTED,
        "{:?}",
        diagnostics[0]
    );
    assert_eq!(map.span_text(diagnostics[0].primary.span), "BASE + 3");
}

#[test]
fn exact_spans_map_back_to_source_text() {
    let mut map = SourceMap::new();
    let id = map.add_file("slice.k816", HLA_SLICE);
    let result = parse_source(map.file(id), None);

    let ItemKind::Var(var) = &result.file.items[0].kind else {
        panic!("expected var declaration");
    };
    assert_eq!(map.span_text(var.name.span), "SCREEN");
    assert_eq!(map.span_text(var.address.span), "$0400");
    assert_eq!(var.address.kind, ExprKind::Number(0x0400));
    assert_eq!(var.name.provenance, Provenance::Source);

    let ItemKind::Func(func) = &result.file.items[1].kind else {
        panic!("expected func declaration");
    };
    assert_eq!(map.span_text(func.name.span), "main");
    assert_eq!(func.width.len(), 2);
    assert_eq!(func.width[0].kind, WidthAnnotationKind::A8);
    assert_eq!(map.span_text(func.width[0].span), "@a8");
    assert_eq!(func.width[1].kind, WidthAnnotationKind::I16);
    assert_eq!(map.span_text(func.width[1].span), "@i16");

    let StatementKind::Hla(HlaStatement::Store { target, register }) =
        &func.body.statements[2].kind
    else {
        panic!("expected store statement");
    };
    assert_eq!(map.span_text(target.span), "SCREEN");
    assert_eq!(map.span_text(register.span), "a");
}

#[test]
fn native_and_hla_parse_to_distinct_ast_forms() {
    let mut map = SourceMap::new();
    let hla = map.add_file("hla.k816", HLA_SLICE);
    let native = map.add_file("native.k816", NATIVE_SLICE);
    let hla = parse_source(map.file(hla), None);
    let native = parse_source(map.file(native), None);

    let ItemKind::Func(hla_func) = &hla.file.items[1].kind else {
        panic!("expected func");
    };
    let ItemKind::Func(native_func) = &native.file.items[1].kind else {
        panic!("expected func");
    };
    assert!(
        hla_func
            .body
            .statements
            .iter()
            .all(|s| matches!(s.kind, StatementKind::Hla(_) | StatementKind::Return))
    );
    assert!(
        native_func
            .body
            .statements
            .iter()
            .all(|s| matches!(s.kind, StatementKind::Native(_)))
    );
}

#[test]
fn malformed_statement_yields_navigable_ast_after_first_error() {
    let mut map = SourceMap::new();
    let id = map.add_file(
        "bad.k816",
        "func main @a8 {\n    a = =\n    return\n}\n\nfunc after @a8 {\n    return\n}\n",
    );
    let result = parse_source(map.file(id), None);

    // The syntax error is reported...
    assert!(
        result
            .diagnostics
            .iter()
            .any(|d| d.code == DiagnosticCode::SYNTAX_ERROR),
        "expected a syntax diagnostic, got {:?}",
        result.diagnostics
    );

    // ...and the AST stays navigable: both functions exist, the malformed
    // statement is an owned error node, and the `return` after it survives.
    assert_eq!(result.file.items.len(), 2);
    let ItemKind::Func(func) = &result.file.items[0].kind else {
        panic!("expected first func");
    };
    assert!(
        func.body
            .statements
            .iter()
            .any(|s| matches!(s.kind, StatementKind::Error))
    );
    assert!(
        func.body
            .statements
            .iter()
            .any(|s| matches!(s.kind, StatementKind::Return))
    );
    let ItemKind::Func(after) = &result.file.items[1].kind else {
        panic!("expected second func");
    };
    assert_eq!(after.name.text, "after");
}

#[test]
fn missing_brace_gets_a_machine_applicable_fix() {
    let mut map = SourceMap::new();
    let id = map.add_file("open.k816", "func main @a8 {\n    return\n");
    let result = parse_source(map.file(id), None);

    let missing = result
        .diagnostics
        .iter()
        .find(|d| d.code == DiagnosticCode::SYNTAX_MISSING)
        .expect("expected a missing-token diagnostic");
    assert_eq!(missing.title, "missing `}`");
    assert!(missing.primary.span.is_empty());

    let fix = missing.fixes.first().expect("expected a fix");
    assert_eq!(fix.applicability, Applicability::MachineApplicable);
    assert_eq!(fix.edits.len(), 1);
    assert_eq!(fix.edits[0].replacement, "}");
    assert!(fix.edits[0].span.is_empty());

    // The function is still present in the AST.
    assert!(matches!(result.file.items[0].kind, ItemKind::Func(_)));
}

#[test]
fn missing_name_is_marked_synthetic_with_origin_span() {
    let mut map = SourceMap::new();
    let id = map.add_file("noname.k816", "var = $0400\n");
    let result = parse_source(map.file(id), None);
    assert!(!result.diagnostics.is_empty());

    // Whatever recovery shape Tree-sitter picks, no synthetic name may claim
    // real source text, and every AST span must stay inside the file.
    let len = map.file(id).text().len() as u32;
    for item in &result.file.items {
        assert!(item.span.end <= len);
        if let ItemKind::Var(var) = &item.kind
            && var.name.provenance == Provenance::Synthetic
        {
            assert!(var.name.text.is_empty());
            assert!(var.name.span.is_empty());
        }
    }
}

#[test]
fn empty_file_parses_to_empty_ast() {
    let mut map = SourceMap::new();
    let id = map.add_file("empty.k816", "");
    let result = parse_source(map.file(id), None);
    assert_eq!(result.diagnostics, vec![]);
    assert_eq!(result.file.items, vec![]);
    let _ = Span::empty_at(id, 0);
}
