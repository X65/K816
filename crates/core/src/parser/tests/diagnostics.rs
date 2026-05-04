use super::*;
use crate::ast::{Instruction, Item, Operand};

#[test]
fn rejects_modifier_without_code_block() {
    let diagnostics = parse(SourceId(0), "far var x\n").expect_err("expected parse error");
    assert!(!diagnostics.is_empty());
}

#[test]
fn collects_multiple_errors_across_items() {
    let source = "far var first\nnaked var second\n";
    let diagnostics = parse(SourceId(0), source).expect_err("expected parse errors");
    assert_eq!(diagnostics.len(), 2, "expected exactly two diagnostics");
    assert!(diagnostics[0].message.contains("found 'var'"));
    assert!(diagnostics[1].message.contains("found 'var'"));
    assert_eq!(diagnostics[0].primary.start, 4);
    assert_eq!(diagnostics[0].primary.end, 7);
    assert_eq!(diagnostics[1].primary.start, 20);
    assert_eq!(diagnostics[1].primary.end, 23);
}

#[test]
fn recovers_at_block_boundary_and_continues_items() {
    let source = "func main {\n call\n}\nfar var trailing\n";
    let diagnostics = parse(SourceId(0), source).expect_err("expected parse errors");
    assert_eq!(diagnostics.len(), 2, "expected exactly two diagnostics");
    assert!(
        diagnostics
            .iter()
            .any(|diag| diag.message.starts_with("invalid syntax: expected")),
        "unexpected diagnostics: {diagnostics:#?}"
    );
}

#[test]
fn parse_error_messages_are_human_readable() {
    let diagnostics = parse(SourceId(0), "func main {\n call\n}\n").expect_err("expected errors");
    let message = &diagnostics[0].message;
    assert!(message.contains("expected"));
    assert!(!message.contains("TokenKind"));
    assert!(!message.contains("ExpectedFound"));
}

#[test]
fn function_call_followed_by_bare_number_parses_as_instruction() {
    // `lsrx 3` (where `lsrx` is a known inline function) must parse cleanly
    // as `Stmt::Instruction`, leaving the call-misuse diagnostic to the
    // emit phase where the function's signature is available. Previously
    // chumsky surfaced an opaque "expected '*', '+', '-', ':', ',', '='"
    // recovery message; the bare-call grammar's boundary check now lets the
    // instruction alternative win.
    let source = "\
inline lsrx (a, #n) -> a {
    lsr * n
}

func test {
    lsrx 3
}
";
    let file = parse(SourceId(0), source).expect("parse should succeed");
    let test_block = file
        .items
        .iter()
        .find_map(|item| match &item.node {
            Item::CodeBlock(block) if block.name == "test" => Some(block),
            _ => None,
        })
        .expect("expected `test` code block");
    let stmt = &test_block.body[0].node;
    assert!(
        matches!(
            stmt,
            Stmt::Instruction(Instruction { mnemonic, operand: Some(Operand::Value { .. }) })
                if mnemonic == "lsrx"
        ),
        "expected `lsrx 3` to parse as Stmt::Instruction with a Value operand; got {stmt:?}"
    );
}

#[test]
fn suggests_width_before_array_in_var_decl_errors() {
    let source = "var bar[20]:word = 0x2000\n";
    let diagnostics = parse(SourceId(0), source).expect_err("expected parse error");
    assert!(
        diagnostics.iter().any(|diag| {
            diag.message == "invalid syntax: unexpected ':'"
                && diag.supplements.iter().any(|supplement| {
                    matches!(
                        supplement,
                        crate::diag::Supplemental::Help(help)
                            if help.contains("`var bar:word[20]`")
                    )
                })
        }),
        "unexpected diagnostics: {diagnostics:#?}"
    );
}
