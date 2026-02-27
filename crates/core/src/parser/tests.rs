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
fn parses_expression_fragment() {
    let expr = parse_expression_fragment(SourceId(0), "0x10").expect("parse");
    assert!(matches!(expr.node, Expr::Number(16, _)));
}

#[test]
fn parses_expression_fragment_with_newline_padding() {
    let expr = parse_expression_fragment(SourceId(0), "\n0x10\n").expect("parse");
    assert!(matches!(expr.node, Expr::Number(16, _)));
}

mod consts;
mod data;
mod diagnostics;
mod statements;
