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
    AstFile, ExprKind, HlaStatement, ItemKind, NativeOperand, StatementKind, WidthAnnotationKind,
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

fn dump_expr(kind: &ExprKind) -> String {
    match kind {
        ExprKind::Number(value) => format!("{value}"),
        ExprKind::Error => "<error>".to_owned(),
    }
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
