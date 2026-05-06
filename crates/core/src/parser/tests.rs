use super::*;
use crate::ast::Operand;

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

fn instruction_operand_exprs(body: &str) -> Vec<Expr> {
    let source = format!("func main @a16 {{\n{body}\n}}\n");
    let file = parse(SourceId(0), &source).expect("parse");
    let Item::CodeBlock(block) = &file.items[0].node else {
        panic!("expected code block");
    };
    block
        .body
        .iter()
        .filter_map(|stmt| {
            let Stmt::Instruction(instr) = &stmt.node else {
                return None;
            };
            match instr.operand.as_ref()? {
                Operand::Immediate { expr, .. } | Operand::Value { expr, .. } => Some(expr.clone()),
                _ => None,
            }
        })
        .collect()
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
fn rejects_expression_fragment_with_removed_abs_suffix() {
    let result = parse_expression_fragment(SourceId(0), "foo:byte:abs");
    assert!(
        result.is_err(),
        "expected parse error after :abs suffix removal"
    );
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

#[test]
fn parses_repeat_element_and_member_expression_forms() {
    let exprs = instruction_operand_exprs(
        "\
  lda COMP[2]
  lda COMP[2].two
  lda COMP[2][.two]
  lda COMP[2].str[3]
  lda &&COMP[2]
",
    );
    assert_eq!(exprs.len(), 5);

    let expr = &exprs[0];
    assert!(matches!(
        expr,
        Expr::Index { base, index }
            if is_ident_named(base.as_ref(), "COMP")
                && matches!(index.as_ref(), Expr::Number(2, NumFmt::Dec))
    ));

    for expr in [&exprs[1], &exprs[2]] {
        assert!(matches!(
            expr,
            Expr::Member { base, field, .. }
                if field == "two"
                    && matches!(
                        base.as_ref(),
                        Expr::Index { base, index }
                            if is_ident_named(base.as_ref(), "COMP")
                                && matches!(index.as_ref(), Expr::Number(2, NumFmt::Dec))
                    )
        ));
    }

    let expr = &exprs[3];
    assert!(matches!(
        expr,
        Expr::Index { base, index }
            if matches!(
                base.as_ref(),
                Expr::Member { base, field, .. }
                    if field == "str"
                        && matches!(
                            base.as_ref(),
                            Expr::Index { base, index }
                                if is_ident_named(base.as_ref(), "COMP")
                                    && matches!(index.as_ref(), Expr::Number(2, NumFmt::Dec))
                        )
            )
            && matches!(index.as_ref(), Expr::Number(3, NumFmt::Dec))
    ));

    let expr = &exprs[4];
    assert!(matches!(
        expr,
        Expr::Unary { op: ExprUnaryOp::WordLittleEndian, expr }
            if matches!(
                expr.as_ref(),
                Expr::Index { base, index }
                    if is_ident_named(base.as_ref(), "COMP")
                        && matches!(index.as_ref(), Expr::Number(2, NumFmt::Dec))
            )
    ));
}

#[test]
fn keeps_existing_symbolic_subscript_expression_shapes() {
    let exprs = instruction_operand_exprs(
        "\
  lda COMP.two
  lda COMP[.two]
  lda COMP.str[2]
",
    );
    assert_eq!(exprs.len(), 3);

    for expr in [&exprs[0], &exprs[1]] {
        assert!(
            is_ident_named(expr, "COMP.two"),
            "expected flattened COMP.two ident, got {expr:?}",
        );
    }

    let expr = &exprs[2];
    assert!(matches!(
        expr,
        Expr::Index { base, index }
            if is_ident_named(base.as_ref(), "COMP.str")
                && matches!(index.as_ref(), Expr::Number(2, NumFmt::Dec))
    ));
}

mod consts;
mod data;
mod diagnostics;
mod statements;
