use super::*;

fn is_ident_named(expr: &Expr, expected: &str) -> bool {
    match expr {
        Expr::Ident(name) => name == expected,
        Expr::IdentSpanned { name, .. } => name == expected,
        _ => false,
    }
}

fn assert_add_one_from(expr: &Expr, previous: &str) {
    assert!(matches!(
        expr,
        Expr::Binary { op: ExprBinaryOp::Add, lhs, rhs }
            if is_ident_named(lhs.as_ref(), previous)
            && matches!(rhs.as_ref(), Expr::Number(1, NumFmt::Dec))
    ));
}

fn assert_negate_of(expr: &Expr, expected_inner: i64) {
    let Expr::Unary {
        op: ExprUnaryOp::Negate,
        expr: inner,
    } = expr
    else {
        panic!("expected Unary(Negate, ...), got {expr:?}");
    };
    assert!(
        matches!(inner.as_ref(), Expr::Number(n, _) if *n == expected_inner),
        "expected Negate of Number({expected_inner}), got Negate of {inner:?}",
    );
}

#[test]
fn parses_far_function_and_call() {
    let source = "far func target {\n nop\n}\nfunc main {\n call target\n}\n";
    let file = parse(SourceId(0), source).expect("parse");
    assert_eq!(file.items.len(), 2);
}

#[test]
fn emits_warning_for_empty_function_body() {
    let source = "func f {\n}\n";
    let parsed = parse_with_warnings(SourceId(0), source).expect("parse");
    assert_eq!(parsed.file.items.len(), 1);
    assert_eq!(parsed.warnings.len(), 1);
    assert_eq!(parsed.warnings[0].severity, crate::diag::Severity::Warning);
    assert!(parsed.warnings[0].message.contains("empty function body"));
}

#[test]
fn function_name_scan_skips_reserved_mnemonics() {
    let source = "func lda {\n  nop\n}\nfunc helper {\n  nop\n}\ninline xba {\n  nop\n}\n";
    let names = scan_declared_function_names(SourceId(0), source);
    assert!(names.contains("helper"));
    assert!(!names.contains("lda"));
    assert!(!names.contains("xba"));
}

#[test]
fn parses_expression_fragment() {
    let expr = parse_expression_fragment(SourceId(0), "0x10").expect("parse");
    assert!(matches!(expr.node, Expr::Number(16, _)));
}

#[test]
fn parses_expression_fragment_with_newline_padding() {
    let expr = parse_expression_fragment(SourceId(0), "\n0x10\n").expect("parse");
    assert!(matches!(expr.node, Expr::Number(16, _)));
}

#[test]
fn parses_expression_fragment_with_address_hint_suffix() {
    let expr = parse_expression_fragment(SourceId(0), "foo:byte:abs").expect("parse");
    assert!(matches!(
        expr.node,
        Expr::AddressHint {
            expr,
            hint: AddressHint::ForceAbsolute16,
        } if matches!(
            expr.as_ref(),
            Expr::TypedView {
                expr,
                width: DataWidth::Byte,
            } if is_ident_named(expr.as_ref(), "foo")
        )
    ));
}

#[test]
fn parses_unary_minus_on_number() {
    let expr = parse_expression_fragment(SourceId(0), "-1").expect("parse");
    assert_negate_of(&expr.node, 1);
}

#[test]
fn parses_binary_minus_with_unary_minus_rhs() {
    let expr = parse_expression_fragment(SourceId(0), "5 - -1").expect("parse");
    let Expr::Binary {
        op: ExprBinaryOp::Sub,
        lhs,
        rhs,
    } = &expr.node
    else {
        panic!("expected Binary(Sub, ...), got {:?}", expr.node);
    };
    assert!(matches!(lhs.as_ref(), Expr::Number(5, _)));
    assert_negate_of(rhs.as_ref(), 1);
}

#[test]
fn parses_repeated_unary_minus() {
    let expr = parse_expression_fragment(SourceId(0), "--1").expect("parse");
    let Expr::Unary {
        op: ExprUnaryOp::Negate,
        expr: inner,
    } = &expr.node
    else {
        panic!("expected outer Unary(Negate, ...), got {:?}", expr.node);
    };
    assert_negate_of(inner.as_ref(), 1);
}

#[test]
fn nested_symbolic_subscript_forms_produce_same_ident() {
    let source = "\
var TASKS[
    .sp :word
    .message[
        .from :byte
    ]
] = $4000
func test @a16 {
    lda #TASKS.message.from
    lda #TASKS[.message].from
    lda #TASKS.message[.from]
    lda #TASKS[.message][.from]
}
";
    let file = parse(SourceId(0), source).expect("parse");
    let Item::CodeBlock(block) = &file.items[1].node else {
        panic!("expected code block");
    };

    for (i, stmt) in block.body.iter().enumerate() {
        let Stmt::Instruction(instr) = &stmt.node else {
            panic!("stmt {i}: expected instruction");
        };
        let Some(crate::ast::Operand::Immediate { expr, .. }) = &instr.operand else {
            panic!(
                "stmt {i}: expected immediate operand, got {:?}",
                instr.operand
            );
        };
        assert!(
            is_ident_named(expr, "TASKS.message.from"),
            "stmt {i}: expected Ident(\"TASKS.message.from\"), got {expr:?}",
        );
    }
}

mod consts;
mod data;
mod diagnostics;
mod statements;
