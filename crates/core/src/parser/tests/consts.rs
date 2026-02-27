use super::*;

#[test]
fn parses_const_declaration() {
    let source = "const LIMIT = $10\n";
    let file = parse(SourceId(0), source).expect("parse");
    assert_eq!(file.items.len(), 1);

    let Item::Const(const_decl) = &file.items[0].node else {
        panic!("expected const item");
    };
    assert_eq!(const_decl.name, "LIMIT");
    assert!(matches!(const_decl.initializer, Expr::Number(16, _)));
    assert!(const_decl.initializer_span.is_some());
}

#[test]
fn preprocesses_comma_separated_const_declarations() {
    let source = "const A = 1, B = 2\n";
    let file = parse(SourceId(0), source).expect("parse");
    assert_eq!(file.items.len(), 1);
    let Item::ConstGroup(consts) = &file.items[0].node else {
        panic!("expected const group");
    };
    assert_eq!(consts.len(), 2);
    assert_eq!(consts[0].name, "A");
    assert_eq!(consts[1].name, "B");
}

#[test]
fn preprocesses_const_group_with_omitted_initializers() {
    let source = "const A = 0, B, C\n";
    let file = parse(SourceId(0), source).expect("parse");
    let Item::ConstGroup(consts) = &file.items[0].node else {
        panic!("expected const group");
    };
    assert_eq!(consts.len(), 3);
    assert_eq!(consts[0].name, "A");
    assert!(matches!(consts[0].initializer, Expr::Number(0, _)));
    assert_eq!(consts[1].name, "B");
    assert_add_one_from(&consts[1].initializer, "A");
    assert_eq!(consts[2].name, "C");
    assert_add_one_from(&consts[2].initializer, "B");
}

#[test]
fn preprocesses_const_group_with_implicit_zero_first() {
    let source = "const A, B, C\n";
    let file = parse(SourceId(0), source).expect("parse");
    let Item::ConstGroup(consts) = &file.items[0].node else {
        panic!("expected const group");
    };
    assert_eq!(consts.len(), 3);
    assert!(matches!(consts[0].initializer, Expr::Number(0, _)));
    assert_add_one_from(&consts[1].initializer, "A");
    assert_add_one_from(&consts[2].initializer, "B");
}

#[test]
fn preprocesses_const_group_with_explicit_resets() {
    let source = "const A = 0, B, C = 10, D\n";
    let file = parse(SourceId(0), source).expect("parse");
    let Item::ConstGroup(consts) = &file.items[0].node else {
        panic!("expected const group");
    };
    assert_eq!(consts.len(), 4);
    assert_add_one_from(&consts[1].initializer, "A");
    assert!(matches!(consts[2].initializer, Expr::Number(10, _)));
    assert_add_one_from(&consts[3].initializer, "C");
}

#[test]
fn preprocesses_const_group_with_multiline_trailing_comma() {
    let source = "const A = 0,\n      B,\n      C,\n";
    let file = parse(SourceId(0), source).expect("parse");
    let Item::ConstGroup(consts) = &file.items[0].node else {
        panic!("expected const group");
    };
    assert_eq!(consts.len(), 3);
    assert!(matches!(consts[0].initializer, Expr::Number(0, _)));
    assert_add_one_from(&consts[1].initializer, "A");
    assert_add_one_from(&consts[2].initializer, "B");
}

#[test]
fn preprocesses_const_group_with_resolvable_identifier_seed() {
    let source = "const BASE = 10\nconst A = BASE, B, C\n";
    let file = parse(SourceId(0), source).expect("parse");
    assert_eq!(file.items.len(), 2);
    let Item::ConstGroup(consts) = &file.items[1].node else {
        panic!("expected const group");
    };
    assert_eq!(consts.len(), 3);
    assert!(is_ident_named(&consts[0].initializer, "BASE"));
    assert_add_one_from(&consts[1].initializer, "A");
    assert_add_one_from(&consts[2].initializer, "B");
}

#[test]
fn parses_top_level_evaluator_block_item() {
    let source = "[ A = 1 ]\n";
    let file = parse(SourceId(0), source).expect("parse");
    assert_eq!(file.items.len(), 1);
    let Item::EvaluatorBlock(block) = &file.items[0].node else {
        panic!("expected evaluator block item");
    };
    assert!(block.text.contains("A = 1"));
}

#[test]
fn parses_multiline_top_level_evaluator_block_item() {
    let source = "[\n  A = 1,\n  B = A + 2\n]\n";
    let file = parse(SourceId(0), source).expect("parse");
    assert_eq!(file.items.len(), 1);
    let Item::EvaluatorBlock(block) = &file.items[0].node else {
        panic!("expected evaluator block item");
    };
    assert!(block.text.contains("A = 1"));
    assert!(block.text.contains("B = A + 2"));
}

#[test]
fn rejects_const_statement_inside_code_block() {
    let source = "func main {\n  const LIMIT = 1\n}\n";
    let diagnostics = parse(SourceId(0), source).expect_err("expected parse error");
    assert!(
        diagnostics
            .iter()
            .any(|diag| diag.message.contains("unexpected 'const'")),
        "unexpected diagnostics: {diagnostics:#?}"
    );
}

#[test]
fn rejects_const_without_initializer() {
    let source = "const LIMIT\n";
    let diagnostics = parse(SourceId(0), source).expect_err("expected parse error");
    let end_of_name = source.find("LIMIT").expect("LIMIT offset") + "LIMIT".len();
    assert!(
        diagnostics.iter().any(|diag| diag
            .message
            .contains("const initializer can be omitted only in comma-separated groups")),
        "unexpected diagnostics: {diagnostics:#?}"
    );
    assert!(
        diagnostics
            .iter()
            .any(|diag| diag.primary.start == end_of_name && diag.primary.end == end_of_name),
        "expected zero-width error span at end of identifier, got: {diagnostics:#?}"
    );
}
