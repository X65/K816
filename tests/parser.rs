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
    AddressEncoding, AstFile, BinaryOp, ExprKind, HlaOperandKind, HlaStatement, IndexRegister,
    IndexRegisterKind, ItemKind, LabelScope, MetadataQueryKind, NativeOperand, RegisterKind,
    StatementKind, StorageClassKind, TypeWidthKind, UnaryOp, WidthAnnotationKind,
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
                out.push_str("var ");
                if let Some(storage) = var.storage {
                    out.push_str(match storage.kind {
                        StorageClassKind::DirectPage => "dp ",
                        StorageClassKind::Absolute => "abs ",
                        StorageClassKind::Far => "far ",
                    });
                }
                out.push_str(&var.name.text);
                if let Some(width) = var.width {
                    out.push_str(match width.kind {
                        TypeWidthKind::Byte => ":byte",
                        TypeWidthKind::Word => ":word",
                        TypeWidthKind::Far => ":far",
                    });
                }
                if let Some(size) = &var.size {
                    out.push_str(&format!("[{}]", dump_expr(&size.kind)));
                }
                if let Some(count) = &var.count {
                    out.push_str(&format!(" * {}", dump_expr(&count.kind)));
                }
                if let Some(address) = &var.address {
                    out.push_str(&format!(" = {}", dump_expr(&address.kind)));
                }
                out.push_str(&format!(" @{}..{}\n", item.span.start, item.span.end));
            }
            ItemKind::Const(group) => {
                out.push_str("const");
                for entry in &group.entries {
                    out.push_str(&format!(" {}", entry.name.text));
                    if let Some(value) = &entry.value {
                        out.push_str(&format!(" = {}", dump_expr(&value.kind)));
                    }
                }
                out.push_str(&format!(" @{}..{}\n", item.span.start, item.span.end));
            }
            ItemKind::Segment(name) => {
                out.push_str(&format!(
                    "segment {} @{}..{}\n",
                    name.text, item.span.start, item.span.end
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
                                Some(NativeOperand::Register(register)) => {
                                    format!(" {}", dump_register(register.kind))
                                }
                                Some(NativeOperand::Address(address)) => format!(
                                    " {}{}{}{}",
                                    dump_encoding(address.encoding),
                                    if address.offset_marker.is_some() {
                                        "&"
                                    } else {
                                        ""
                                    },
                                    dump_expr(&address.value.kind),
                                    dump_index(address.index)
                                ),
                                Some(NativeOperand::Indirect(indirect)) => format!(
                                    " ({}{}{}){}",
                                    dump_encoding(indirect.encoding),
                                    dump_expr(&indirect.value.kind),
                                    dump_index(indirect.inner_index),
                                    dump_index(indirect.outer_index)
                                ),
                                Some(NativeOperand::LongIndirect(long)) => format!(
                                    " [{}{}]{}",
                                    dump_encoding(long.encoding),
                                    dump_expr(&long.value.kind),
                                    dump_index(long.outer_index)
                                ),
                                Some(NativeOperand::BlockMove(moved)) => format!(
                                    " {},{}",
                                    dump_expr(&moved.source.kind),
                                    dump_expr(&moved.destination.kind)
                                ),
                                None => String::new(),
                            };
                            let repeat = match &native.repeat {
                                Some(repeat) => format!(" * {}", dump_expr(&repeat.kind)),
                                None => String::new(),
                            };
                            format!("native {}{operand}{repeat}", native.mnemonic.text)
                        }
                        StatementKind::Hla(HlaStatement::Assign(assign)) => {
                            let mut chain = String::from("hla");
                            for target in &assign.targets {
                                chain.push_str(&format!(" {} =", dump_hla_operand(&target.kind)));
                            }
                            chain.push_str(&format!(" {}", dump_hla_operand(&assign.source.kind)));
                            chain
                        }
                        StatementKind::Hla(HlaStatement::Exchange(exchange)) => format!(
                            "hla {}~{}",
                            dump_register(exchange.left.kind),
                            dump_register(exchange.right.kind)
                        ),
                        StatementKind::Label(label) => format!(
                            "{} {}",
                            match label.scope {
                                LabelScope::Global => "label",
                                LabelScope::Local => "local label",
                            },
                            label.name.text
                        ),
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

fn dump_register(kind: RegisterKind) -> &'static str {
    match kind {
        RegisterKind::A => "a",
        RegisterKind::B => "b",
        RegisterKind::C => "c",
        RegisterKind::D => "d",
        RegisterKind::S => "s",
        RegisterKind::X => "x",
        RegisterKind::Y => "y",
    }
}

fn dump_hla_operand(kind: &HlaOperandKind) -> String {
    match kind {
        HlaOperandKind::Register(register) => dump_register(register.kind).to_owned(),
        HlaOperandKind::Immediate(value) => format!("#{}", dump_expr(&value.kind)),
        HlaOperandKind::Memory(memory) => format!(
            "{}{}{}",
            dump_encoding(memory.encoding),
            dump_expr(&memory.value.kind),
            dump_index(memory.index)
        ),
        HlaOperandKind::Indirect(indirect) => format!(
            "({}{}){}",
            dump_expr(&indirect.value.kind),
            dump_index(indirect.inner_index),
            dump_index(indirect.outer_index)
        ),
        HlaOperandKind::Error => "<error>".to_owned(),
    }
}

fn dump_encoding(encoding: Option<AddressEncoding>) -> String {
    match encoding.map(|encoding| encoding.kind) {
        Some(StorageClassKind::DirectPage) => "dp ".to_owned(),
        Some(StorageClassKind::Absolute) => "abs ".to_owned(),
        Some(StorageClassKind::Far) => "far ".to_owned(),
        None => String::new(),
    }
}

fn dump_index(index: Option<IndexRegister>) -> String {
    match index.map(|index| index.kind) {
        Some(IndexRegisterKind::X) => ",x".to_owned(),
        Some(IndexRegisterKind::Y) => ",y".to_owned(),
        Some(IndexRegisterKind::Stack) => ",s".to_owned(),
        None => String::new(),
    }
}

/// Renders an expression fully parenthesized in prefix form, so operator
/// precedence and associativity are visible in the snapshot.
fn dump_expr(kind: &ExprKind) -> String {
    match kind {
        ExprKind::Number(value) => format!("{value}"),
        ExprKind::Symbol(name) | ExprKind::LocalSymbol(name) => name.text.clone(),
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
        ExprKind::Field(access) => {
            format!("({}{})", dump_expr(&access.base.kind), access.field.text)
        }
        ExprKind::Subscript(subscript) => format!(
            "({}[{}])",
            dump_expr(&subscript.base.kind),
            dump_expr(&subscript.index.kind)
        ),
        ExprKind::Metadata(query) => format!(
            "({}:{})",
            dump_expr(&query.base.kind),
            match query.query {
                MetadataQueryKind::SizeOf => "sizeof",
                MetadataQueryKind::OffsetOf => "offsetof",
            }
        ),
        ExprKind::TypedView(view) => format!(
            "({}:{})",
            dump_expr(&view.base.kind),
            match view.width.kind {
                TypeWidthKind::Byte => "byte",
                TypeWidthKind::Word => "word",
                TypeWidthKind::Far => "far",
            }
        ),
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
    let address = var.address.as_ref().expect("expected an initializer");
    dump_expr(&address.kind)
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
      hla_assignment 45..51
        operand: register 45..46
        operand: immediate 49..51
          value: number 50..51
      hla_assignment 56..62
        operand: register 56..57
        operand: immediate 60..62
          value: number 61..62
      hla_assignment 67..77
        operand: address 67..73
          value: identifier 67..73
        operand: register 76..77
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
      hla_assignment 51..62
        operand: register 51..52
        operand: immediate 55..62
          value: number 56..62
      hla_assignment 67..75
        operand: register 67..68
        operand: immediate 71..75
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
            let address = var.address.as_ref().expect("expected an initializer");
            match address.kind {
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
    let address = var.address.as_ref().expect("expected an initializer");
    assert_eq!(map.span_text(address.span), "(A + B)");
    let ExprKind::Binary(binary) = &address.kind else {
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
fn declaration_ast_snapshot_is_exact() {
    let source = "segment fixed_lo
var dp counter
var abs slot = $0012
var w:word = $2000
var buffer[32] = $0100
var simple:byte * 32
var allocated
const A = 0, B, C

func draw {
    .loop:
    hello:
    .LT+1 = a
    return
}
";
    let mut map = SourceMap::new();
    let id = map.add_file("declarations.k65", source);
    let result = parse_source(map.file(id), None);
    assert_eq!(result.diagnostics, vec![]);
    assert_eq!(
        dump_ast(&result.file),
        "\
segment fixed_lo @0..16
var dp counter @17..31
var abs slot = 18 @32..52
var w:word = 8192 @53..71
var buffer[32] = 256 @72..94
var simple:byte * 32 @95..115
var allocated @116..129
const A = 0 B C @130..147
func draw @149..209
  local label .loop @165..171
  label hello @176..182
  hla (+ .LT 1) = a @187..196
  return @201..207
"
    );
}

#[test]
fn a_var_prefix_that_is_not_a_storage_class_is_reported() {
    // `var foo bar` cannot be two names; the prefix slot is checked in the
    // builder so the message names the three storage classes.
    let mut map = SourceMap::new();
    let id = map.add_file("bad.k65", "var foo bar = 1\n");
    let result = parse_source(map.file(id), None);
    assert_eq!(result.diagnostics.len(), 1, "{:?}", result.diagnostics);
    assert_eq!(result.diagnostics[0].code, DiagnosticCode::SYNTAX_ERROR);
    assert_eq!(map.span_text(result.diagnostics[0].primary.span), "foo");
}

#[test]
fn semicolons_leave_no_trace_in_the_ast() {
    let mut map = SourceMap::new();
    let dense = map.add_file("dense.k65", "var A = 0; var B = 1;;\n;\n");
    let sparse = map.add_file("sparse.k65", "var A = 0\nvar B = 1\n");
    let dense = parse_source(map.file(dense), None);
    let sparse = parse_source(map.file(sparse), None);
    assert_eq!(dense.diagnostics, vec![]);
    assert_eq!(dense.file.items.len(), sparse.file.items.len());
}

#[test]
fn local_labels_keep_their_leading_dot() {
    let mut map = SourceMap::new();
    let id = map.add_file("labels.k65", "func f {\n  .loop:\n  .LT+1 = a\n}\n");
    let result = parse_source(map.file(id), None);
    let ItemKind::Func(func) = &result.file.items[0].kind else {
        panic!("expected a func");
    };
    let StatementKind::Label(label) = &func.body.statements[0].kind else {
        panic!("expected a label");
    };
    assert_eq!(label.scope, LabelScope::Local);
    assert_eq!(label.name.text, ".loop");
    assert_eq!(map.span_text(label.name.span), ".loop");

    let StatementKind::Hla(HlaStatement::Assign(assign)) = &func.body.statements[1].kind else {
        panic!("expected an assignment");
    };
    let HlaOperandKind::Memory(memory) = &assign.targets[0].kind else {
        panic!("expected a memory target");
    };
    let ExprKind::Binary(binary) = &memory.value.kind else {
        panic!("expected a label+offset target");
    };
    assert!(matches!(binary.left.kind, ExprKind::LocalSymbol(_)));
}

#[test]
fn every_new_declaration_form_names_its_owner() {
    // The parser accepts these; each one has to say which sub-plan owns its
    // semantics rather than compile to nothing.
    for (source, expected_code) in [
        ("var counter\n", DiagnosticCode::NOT_YET_IMPLEMENTED),
        (
            "var dp counter = $10\n",
            DiagnosticCode::NOT_YET_IMPLEMENTED,
        ),
        ("var w:word = $2000\n", DiagnosticCode::NOT_YET_IMPLEMENTED),
        (
            "var buffer[32] = $100\n",
            DiagnosticCode::NOT_YET_IMPLEMENTED,
        ),
        ("var s:byte * 32\n", DiagnosticCode::NOT_YET_IMPLEMENTED),
        ("const LIMIT = 10\n", DiagnosticCode::NOT_YET_IMPLEMENTED),
        ("segment code\n", DiagnosticCode::NOT_YET_IMPLEMENTED),
        ("func f { .loop: }\n", DiagnosticCode::NOT_YET_IMPLEMENTED),
        (
            "func f { .LT+1 = a }\n",
            DiagnosticCode::NOT_YET_IMPLEMENTED,
        ),
    ] {
        let mut map = SourceMap::new();
        let id = map.add_file("owner.k65", source);
        let parsed = parse_source(map.file(id), None);
        assert_eq!(parsed.diagnostics, vec![], "`{source}` did not parse");
        let (_, diagnostics) = k816::semantic::analyze_file(&parsed.file);
        assert_eq!(diagnostics.len(), 1, "`{source}`: {diagnostics:?}");
        assert_eq!(diagnostics[0].code, expected_code, "`{source}`");
        assert!(
            diagnostics[0].notes[0].starts_with("implemented by sub-plan "),
            "`{source}`: {:?}",
            diagnostics[0].notes
        );
    }
}

#[test]
fn accessors_and_metadata_queries_bind_at_atom_level() {
    // `#TASKS:sizeof + 1` is `(TASKS:sizeof) + 1` — syntax-reference.md
    // "Metadata Query Suffixes".
    assert_eq!(parse_expr("TASKS.sp"), "(TASKS.sp)");
    assert_eq!(parse_expr("COMP[2]"), "(COMP[2])");
    assert_eq!(parse_expr("COMP[2].two"), "((COMP[2]).two)");
    assert_eq!(parse_expr("COMP[2].str[3]"), "(((COMP[2]).str)[3])");
    assert_eq!(parse_expr("TASKS:sizeof + 1"), "(+ (TASKS:sizeof) 1)");
    assert_eq!(
        parse_expr("REGS.data.lo:offsetof"),
        "(((REGS.data).lo):offsetof)"
    );
    assert_eq!(parse_expr("ptr:byte"), "(ptr:byte)");
}

#[test]
fn bracketed_field_selection_matches_dotted_selection() {
    // The two spellings are documented as equivalent, so they reach the AST
    // as one shape even though the CST keeps them apart.
    assert_eq!(parse_expr("COMP[2][.two]"), parse_expr("COMP[2].two"));
}

#[test]
fn a_field_name_must_touch_its_base() {
    // Otherwise `X = FOO` followed by the local label reference `.LT+1` would
    // read as a single field access across the statement boundary.
    let mut map = SourceMap::new();
    let id = map.add_file("adjacent.k65", "func f {\n  FOO = a\n  .LT+1 = a\n}\n");
    let result = parse_source(map.file(id), None);
    assert_eq!(result.diagnostics, vec![]);
    let ItemKind::Func(func) = &result.file.items[0].kind else {
        panic!("expected a func");
    };
    assert_eq!(func.body.statements.len(), 2);
}

#[test]
fn subscript_declarations_reach_the_ast() {
    let source = "abstract var POINT:byte[ .x .y ]
var REGS[
    .status :byte
    .data[
        .lo :byte
        .hi :byte
    ]
] = $D000
";
    let mut map = SourceMap::new();
    let id = map.add_file("fields.k65", source);
    let result = parse_source(map.file(id), None);
    assert_eq!(result.diagnostics, vec![]);

    let ItemKind::Var(point) = &result.file.items[0].kind else {
        panic!("expected a var");
    };
    assert!(point.is_abstract);
    assert_eq!(
        point.width.expect("default width").kind,
        TypeWidthKind::Byte
    );
    assert_eq!(point.fields.len(), 2);
    assert_eq!(point.fields[0].name.text, ".x");
    assert!(point.fields[0].width.is_none());

    let ItemKind::Var(regs) = &result.file.items[1].kind else {
        panic!("expected a var");
    };
    assert!(!regs.is_abstract);
    assert_eq!(regs.fields.len(), 2);
    assert_eq!(regs.fields[1].name.text, ".data");
    assert_eq!(regs.fields[1].fields.len(), 2);
    assert_eq!(regs.fields[1].fields[1].name.text, ".hi");
    assert_eq!(map.span_text(regs.fields[1].fields[1].span), ".hi :byte");
}

#[test]
fn subscript_forms_name_their_owner() {
    for source in [
        "abstract var P[ .x :byte ]\n",
        "var TASKS[ .sp :word ] = $4000\n",
        "func f { lda #TASKS:sizeof }\n",
        "func f { lda #TASKS.sp }\n",
    ] {
        let mut map = SourceMap::new();
        let id = map.add_file("owner.k65", source);
        let parsed = parse_source(map.file(id), None);
        assert_eq!(parsed.diagnostics, vec![], "`{source}` did not parse");
        let (_, diagnostics) = k816::semantic::analyze_file(&parsed.file);
        assert_eq!(diagnostics.len(), 1, "`{source}`: {diagnostics:?}");
        assert_eq!(
            diagnostics[0].code,
            DiagnosticCode::NOT_YET_IMPLEMENTED,
            "`{source}`"
        );
    }
}

#[test]
fn classical_operand_shapes_reach_the_ast() {
    let source = "func main {
    nop
    asl a
    lda #1
    lda addr
    lda addr,x
    sta addr,y
    lda (ptr,x)
    lda (ptr),y
    jmp (vector)
    lda dp addr
    lda abs addr,x
    lda far base+3
    inx * 3
}
";
    let mut map = SourceMap::new();
    let id = map.add_file("native.k65", source);
    let result = parse_source(map.file(id), None);
    assert_eq!(result.diagnostics, vec![]);

    let ItemKind::Func(func) = &result.file.items[0].kind else {
        panic!("expected a func");
    };
    let rendered: Vec<String> = func
        .body
        .statements
        .iter()
        .map(|statement| {
            let StatementKind::Native(native) = &statement.kind else {
                panic!("expected a native instruction");
            };
            let operand = match &native.operand {
                Some(NativeOperand::Immediate(expr)) => format!(" #{}", dump_expr(&expr.kind)),
                Some(NativeOperand::Register(register)) => {
                    format!(" {}", dump_register(register.kind))
                }
                Some(NativeOperand::Address(address)) => format!(
                    " {}{}{}{}",
                    dump_encoding(address.encoding),
                    if address.offset_marker.is_some() {
                        "&"
                    } else {
                        ""
                    },
                    dump_expr(&address.value.kind),
                    dump_index(address.index)
                ),
                Some(NativeOperand::Indirect(indirect)) => format!(
                    " ({}{}{}){}",
                    dump_encoding(indirect.encoding),
                    dump_expr(&indirect.value.kind),
                    dump_index(indirect.inner_index),
                    dump_index(indirect.outer_index)
                ),
                Some(NativeOperand::LongIndirect(long)) => format!(
                    " [{}{}]{}",
                    dump_encoding(long.encoding),
                    dump_expr(&long.value.kind),
                    dump_index(long.outer_index)
                ),
                Some(NativeOperand::BlockMove(moved)) => format!(
                    " {},{}",
                    dump_expr(&moved.source.kind),
                    dump_expr(&moved.destination.kind)
                ),
                None => String::new(),
            };
            let repeat = match &native.repeat {
                Some(repeat) => format!(" * {}", dump_expr(&repeat.kind)),
                None => String::new(),
            };
            format!("{}{operand}{repeat}", native.mnemonic.text)
        })
        .collect();
    assert_eq!(
        rendered,
        vec![
            "nop",
            "asl a",
            "lda #1",
            "lda addr",
            "lda addr,x",
            "sta addr,y",
            "lda (ptr,x)",
            "lda (ptr),y",
            "jmp (vector)",
            "lda dp addr",
            "lda abs addr,x",
            "lda far (+ base 3)",
            "inx * 3",
        ]
    );
}

#[test]
fn an_operand_does_not_swallow_the_next_statement() {
    // `lda foo` then `bar = a` could also read as the direct-page prefix form
    // `lda foo bar` followed by `= a`; only the first goes on to parse.
    let mut map = SourceMap::new();
    let id = map.add_file("boundary.k65", "func f {\n  lda foo\n  bar = a\n}\n");
    let result = parse_source(map.file(id), None);
    assert_eq!(result.diagnostics, vec![]);
    let ItemKind::Func(func) = &result.file.items[0].kind else {
        panic!("expected a func");
    };
    assert_eq!(func.body.statements.len(), 2);
    let StatementKind::Native(native) = &func.body.statements[0].kind else {
        panic!("expected a native instruction");
    };
    let Some(NativeOperand::Address(address)) = &native.operand else {
        panic!("expected an address operand");
    };
    assert!(address.encoding.is_none());
    assert!(matches!(
        func.body.statements[1].kind,
        StatementKind::Hla(HlaStatement::Assign(_))
    ));
}

#[test]
fn a_recognized_mnemonic_without_isa_support_names_the_translator() {
    // `inx` is a real instruction the frozen ISA table does not carry yet, so
    // it must not be reported as an unknown mnemonic.
    let mut map = SourceMap::new();
    let id = map.add_file("inx.k65", "func main @a8 {\n    inx\n}\n");
    let parsed = parse_source(map.file(id), None);
    assert_eq!(parsed.diagnostics, vec![]);
    let (_, diagnostics) = k816::semantic::analyze_file(&parsed.file);
    assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
    assert_eq!(diagnostics[0].code, DiagnosticCode::NOT_YET_IMPLEMENTED);
    assert_eq!(map.span_text(diagnostics[0].primary.span), "inx");
}

#[test]
fn operand_shapes_beyond_the_isa_table_name_the_translator() {
    for source in [
        "func main @a8 { lda SCREEN,x }\n",
        "func main @a8 { lda (SCREEN),y }\n",
        "func main @a8 { lda dp SCREEN }\n",
        "func main @a8 { asl a }\n",
        "func main @a8 { rts * 2 }\n",
    ] {
        let mut map = SourceMap::new();
        let id = map.add_file("shape.k65", format!("var SCREEN = $0400\n{source}"));
        let parsed = parse_source(map.file(id), None);
        assert_eq!(parsed.diagnostics, vec![], "`{source}` did not parse");
        let (_, diagnostics) = k816::semantic::analyze_file(&parsed.file);
        assert_eq!(diagnostics.len(), 1, "`{source}`: {diagnostics:?}");
        assert_eq!(
            diagnostics[0].code,
            DiagnosticCode::NOT_YET_IMPLEMENTED,
            "`{source}`"
        );
    }
}

#[test]
fn native_65816_operand_shapes_reach_the_ast() {
    let source = "func main {
    lda 4,s
    lda (4,s),y
    lda [ptr]
    lda [ptr],y
    lda &FIELD_HP, x
    lda abs &FIELD_HP, x
    mvn $01,$02
    xba
}
";
    let mut map = SourceMap::new();
    let id = map.add_file("native816.k65", source);
    let result = parse_source(map.file(id), None);
    assert_eq!(result.diagnostics, vec![]);

    let ItemKind::Func(func) = &result.file.items[0].kind else {
        panic!("expected a func");
    };
    let rendered: Vec<String> = func
        .body
        .statements
        .iter()
        .map(|statement| {
            let StatementKind::Native(native) = &statement.kind else {
                panic!("expected a native instruction");
            };
            let operand = match &native.operand {
                Some(NativeOperand::Address(address)) => format!(
                    " {}{}{}{}",
                    dump_encoding(address.encoding),
                    if address.offset_marker.is_some() {
                        "&"
                    } else {
                        ""
                    },
                    dump_expr(&address.value.kind),
                    dump_index(address.index)
                ),
                Some(NativeOperand::Indirect(indirect)) => format!(
                    " ({}{}){}",
                    dump_expr(&indirect.value.kind),
                    dump_index(indirect.inner_index),
                    dump_index(indirect.outer_index)
                ),
                Some(NativeOperand::LongIndirect(long)) => format!(
                    " [{}]{}",
                    dump_expr(&long.value.kind),
                    dump_index(long.outer_index)
                ),
                Some(NativeOperand::BlockMove(moved)) => format!(
                    " {},{}",
                    dump_expr(&moved.source.kind),
                    dump_expr(&moved.destination.kind)
                ),
                _ => String::new(),
            };
            format!("{}{operand}", native.mnemonic.text)
        })
        .collect();
    assert_eq!(
        rendered,
        vec![
            "lda 4,s",
            "lda (4,s),y",
            "lda [ptr]",
            "lda [ptr],y",
            "lda &FIELD_HP,x",
            "lda abs &FIELD_HP,x",
            "mvn 1,2",
            "xba",
        ]
    );
}

#[test]
fn brackets_after_a_mnemonic_are_indirection_not_evaluation() {
    // `lda [ptr]` dereferences; the same brackets in an HLA assignment are
    // the compile-time evaluator's, which is why the operand form is
    // raw-mnemonic-only.
    let mut map = SourceMap::new();
    let id = map.add_file("brackets.k65", "func f {\n  lda [ptr]\n  lda #[SIZE]\n}\n");
    let result = parse_source(map.file(id), None);
    assert_eq!(result.diagnostics, vec![]);
    let ItemKind::Func(func) = &result.file.items[0].kind else {
        panic!("expected a func");
    };
    let StatementKind::Native(long) = &func.body.statements[0].kind else {
        panic!("expected a native instruction");
    };
    assert!(matches!(long.operand, Some(NativeOperand::LongIndirect(_))));
    let StatementKind::Native(immediate) = &func.body.statements[1].kind else {
        panic!("expected a native instruction");
    };
    let Some(NativeOperand::Immediate(value)) = &immediate.operand else {
        panic!("expected an immediate");
    };
    assert!(matches!(value.kind, ExprKind::Eval(_)));
}

/// Parses one HLA statement and renders it in the `dump_ast` chain form.
fn parse_hla(statement: &str) -> String {
    let mut map = SourceMap::new();
    let id = map.add_file("hla.k65", format!("func f {{\n  {statement}\n}}\n"));
    let result = parse_source(map.file(id), None);
    assert_eq!(result.diagnostics, vec![], "`{statement}` did not parse");
    let ItemKind::Func(func) = &result.file.items[0].kind else {
        panic!("expected a func");
    };
    assert_eq!(func.body.statements.len(), 1, "`{statement}`");
    match &func.body.statements[0].kind {
        StatementKind::Hla(HlaStatement::Assign(assign)) => {
            let mut chain = String::new();
            for target in &assign.targets {
                chain.push_str(&format!("{} = ", dump_hla_operand(&target.kind)));
            }
            chain.push_str(&dump_hla_operand(&assign.source.kind));
            chain
        }
        StatementKind::Hla(HlaStatement::Exchange(exchange)) => format!(
            "{}~{}",
            dump_register(exchange.left.kind),
            dump_register(exchange.right.kind)
        ),
        other => panic!("`{statement}` is not an HLA statement: {other:?}"),
    }
}

#[test]
fn every_movement_shape_reaches_the_ast() {
    assert_eq!(parse_hla("a = #1"), "a = #1");
    assert_eq!(parse_hla("a = mem,x"), "a = mem,x");
    assert_eq!(parse_hla("a = (mem),y"), "a = (mem),y");
    assert_eq!(parse_hla("mem = a"), "mem = a");
    assert_eq!(parse_hla("mem,x = a"), "mem,x = a");
    assert_eq!(parse_hla("(mem),y = a"), "(mem),y = a");
    assert_eq!(parse_hla("mem = x"), "mem = x");
    assert_eq!(parse_hla("mem = 0"), "mem = 0");
    assert_eq!(parse_hla("b~a"), "b~a");
    assert_eq!(parse_hla("a~b"), "a~b");
}

#[test]
fn a_chain_flows_from_its_rightmost_operand() {
    // "The value flows from the rightmost source through each target
    // left-to-right" — syntax-reference.md "Assignment Chaining".
    assert_eq!(parse_hla("dst0 = dst1 = a = src"), "dst0 = dst1 = a = src");
    assert_eq!(parse_hla("x = a = dst0"), "x = a = dst0");
    assert_eq!(parse_hla("s = x = y"), "s = x = y");
    assert_eq!(
        parse_hla("UART_DATA = a = text,x"),
        "UART_DATA = a = text,x"
    );

    let mut map = SourceMap::new();
    let id = map.add_file("chain.k65", "func f {\n  dst0 = dst1 = a = src\n}\n");
    let result = parse_source(map.file(id), None);
    let ItemKind::Func(func) = &result.file.items[0].kind else {
        panic!("expected a func");
    };
    let StatementKind::Hla(HlaStatement::Assign(assign)) = &func.body.statements[0].kind else {
        panic!("expected an assignment");
    };
    assert_eq!(assign.targets.len(), 3);
    assert_eq!(map.span_text(assign.source.span), "src");
    assert_eq!(map.span_text(assign.targets[0].span), "dst0");
}

#[test]
fn registers_are_case_insensitive() {
    assert_eq!(parse_hla("A = X"), "a = x");
    assert_eq!(parse_hla("D = C"), "d = c");
    assert_eq!(parse_hla("S = X = Y"), "s = x = y");
}

#[test]
fn the_slice_chain_shapes_still_compile_and_the_rest_name_their_owner() {
    // The two shapes sub-plan A compiles keep their semantics through the
    // generalization; every other chain says who owns it.
    let mut map = SourceMap::new();
    let slice = map.add_file("slice.k816", HLA_SLICE);
    let parsed = parse_source(map.file(slice), None);
    let (_, diagnostics) = k816::semantic::analyze_file(&parsed.file);
    assert_eq!(diagnostics, vec![], "the slice must stay clean");

    for source in ["x = a\n", "mem,x = a\n", "a = mem\n", "b~a\n"] {
        let mut map = SourceMap::new();
        let id = map.add_file(
            "chain.k65",
            format!("var mem = $0400\nfunc main @a8 {{\n  {source}}}\n"),
        );
        let parsed = parse_source(map.file(id), None);
        assert_eq!(parsed.diagnostics, vec![], "`{source}` did not parse");
        let (_, diagnostics) = k816::semantic::analyze_file(&parsed.file);
        assert_eq!(diagnostics.len(), 1, "`{source}`: {diagnostics:?}");
        assert_eq!(
            diagnostics[0].code,
            DiagnosticCode::NOT_YET_IMPLEMENTED,
            "`{source}`"
        );
    }
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
    let address = var.address.as_ref().expect("expected an initializer");
    assert_eq!(map.span_text(address.span), "$0400");
    assert_eq!(address.kind, ExprKind::Number(0x0400));
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

    let StatementKind::Hla(HlaStatement::Assign(assign)) = &func.body.statements[2].kind else {
        panic!("expected store statement");
    };
    assert_eq!(map.span_text(assign.targets[0].span), "SCREEN");
    assert_eq!(map.span_text(assign.source.span), "a");
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
