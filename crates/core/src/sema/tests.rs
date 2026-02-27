use super::*;
use crate::parser::parse;
use crate::span::SourceId;

#[test]
fn allocates_vars_monotonically_in_source_order() {
    let source = "var top\nfunc f {\n  var in_a\n  var in_b\n}\nvar tail\n";
    let file = parse(SourceId(0), source).expect("parse");
    let sema = analyze(&file).expect("analyze");

    assert_eq!(sema.vars.get("top").expect("top").address, 0);
    assert_eq!(sema.vars.get("top").expect("top").size, 1);
    assert_eq!(sema.vars.get("in_a").expect("in_a").address, 1);
    assert_eq!(sema.vars.get("in_a").expect("in_a").size, 1);
    assert_eq!(sema.vars.get("in_b").expect("in_b").address, 2);
    assert_eq!(sema.vars.get("in_b").expect("in_b").size, 1);
    assert_eq!(sema.vars.get("tail").expect("tail").address, 3);
    assert_eq!(sema.vars.get("tail").expect("tail").size, 1);
}

#[test]
fn explicit_var_address_resets_auto_allocator() {
    let source = "var first\nvar reset = 0x100\nvar next\n";
    let file = parse(SourceId(0), source).expect("parse");
    let sema = analyze(&file).expect("analyze");

    assert_eq!(sema.vars.get("first").expect("first").address, 0);
    assert_eq!(sema.vars.get("reset").expect("reset").address, 0x100);
    assert_eq!(sema.vars.get("next").expect("next").address, 0x101);
}

#[test]
fn consts_are_collected_and_used_in_var_initializers() {
    let source = "const BASE = 0x100\nconst NEXT = BASE + 3\nvar ptr = NEXT\nvar tail\n";
    let file = parse(SourceId(0), source).expect("parse");
    let sema = analyze(&file).expect("analyze");

    assert_eq!(
        sema.consts.get("BASE").expect("BASE").value,
        Number::Int(0x100)
    );
    assert_eq!(
        sema.consts.get("NEXT").expect("NEXT").value,
        Number::Int(0x103)
    );
    assert_eq!(sema.vars.get("ptr").expect("ptr").address, 0x103);
    assert_eq!(sema.vars.get("tail").expect("tail").address, 0x104);
}

#[test]
fn const_sequence_implicit_initializers_are_resolved() {
    let source = "const A = 0, B, C\nvar ptr = C\n";
    let file = parse(SourceId(0), source).expect("parse");
    let sema = analyze(&file).expect("analyze");

    assert_eq!(sema.consts.get("A").expect("A").value, Number::Int(0));
    assert_eq!(sema.consts.get("B").expect("B").value, Number::Int(1));
    assert_eq!(sema.consts.get("C").expect("C").value, Number::Int(2));
    assert_eq!(sema.vars.get("ptr").expect("ptr").address, 2);
}

#[test]
fn const_sequence_explicit_initializer_resets_increment_chain() {
    let source = "const A = 0, B, C = 10, D\nvar ptr = D\n";
    let file = parse(SourceId(0), source).expect("parse");
    let sema = analyze(&file).expect("analyze");

    assert_eq!(sema.consts.get("A").expect("A").value, Number::Int(0));
    assert_eq!(sema.consts.get("B").expect("B").value, Number::Int(1));
    assert_eq!(sema.consts.get("C").expect("C").value, Number::Int(10));
    assert_eq!(sema.consts.get("D").expect("D").value, Number::Int(11));
    assert_eq!(sema.vars.get("ptr").expect("ptr").address, 11);
}

#[test]
fn const_sequence_accepts_resolvable_identifier_seed() {
    let source = "const BASE = 10\nconst A = BASE, B, C\nvar ptr = C\n";
    let file = parse(SourceId(0), source).expect("parse");
    let sema = analyze(&file).expect("analyze");

    assert_eq!(
        sema.consts.get("BASE").expect("BASE").value,
        Number::Int(10)
    );
    assert_eq!(sema.consts.get("A").expect("A").value, Number::Int(10));
    assert_eq!(sema.consts.get("B").expect("B").value, Number::Int(11));
    assert_eq!(sema.consts.get("C").expect("C").value, Number::Int(12));
    assert_eq!(sema.vars.get("ptr").expect("ptr").address, 12);
}

#[test]
fn top_level_evaluator_constants_are_available_in_source_order() {
    let source = "[ A = 1, B = A + 2 ]\nconst C = B + 3\nvar ptr = C\n";
    let file = parse(SourceId(0), source).expect("parse");
    let sema = analyze(&file).expect("analyze");

    assert_eq!(sema.consts.get("A").expect("A").value, Number::Int(1));
    assert_eq!(sema.consts.get("B").expect("B").value, Number::Int(3));
    assert_eq!(sema.consts.get("C").expect("C").value, Number::Int(6));
    assert_eq!(sema.vars.get("ptr").expect("ptr").address, 6);
}

#[test]
fn top_level_evaluator_allows_in_block_mutation() {
    let source = "[ A = 1, B = ++A + A--, C = A ]\n";
    let file = parse(SourceId(0), source).expect("parse");
    let sema = analyze(&file).expect("analyze");

    assert_eq!(sema.consts.get("A").expect("A").value, Number::Int(1));
    assert_eq!(sema.consts.get("B").expect("B").value, Number::Int(4));
    assert_eq!(sema.consts.get("C").expect("C").value, Number::Int(1));
}

#[test]
fn top_level_evaluator_supports_array_literal_indexing() {
    let source = "[ arr = [10, 20, 30], A = arr[1] ]\n";
    let file = parse(SourceId(0), source).expect("parse");
    let sema = analyze(&file).expect("analyze");

    assert_eq!(sema.consts.get("A").expect("A").value, Number::Int(20));
    assert!(!sema.consts.contains_key("arr"));
}

#[test]
fn top_level_evaluator_supports_named_data_indexing() {
    let source = "data arr {\n  10 20 30\n}\n[ A = arr[1] ]\n";
    let file = parse(SourceId(0), source).expect("parse");
    let sema = analyze(&file).expect("analyze");

    assert_eq!(sema.consts.get("A").expect("A").value, Number::Int(20));
}

#[test]
fn top_level_evaluator_rejects_cross_item_reassignment() {
    let source = "[ A = 1 ]\n[ A = 2 ]\n";
    let file = parse(SourceId(0), source).expect("parse");
    let errors = analyze(&file).expect_err("must fail");

    assert!(errors.iter().any(|error| {
        error
            .message
            .contains("cannot reassign constant 'A' in a different evaluator block")
    }));
}

#[test]
fn top_level_evaluator_unexpected_token_points_to_token() {
    let source = "[\n  obj = { member: 42 },\n]\n";
    let file = parse(SourceId(0), source).expect("parse");
    let errors = analyze(&file).expect_err("must fail");

    let error = errors
        .iter()
        .find(|error| error.message.contains("unexpected token"))
        .expect("unexpected-token error");
    let brace = source.find('{').expect("brace");

    assert_eq!(error.primary.start, brace);
    assert_eq!(error.primary.end, brace + 1);
}

#[test]
fn var_initializer_requires_exact_integer_from_top_level_evaluator_constant() {
    let source = "[ F = 1.5 ]\nvar ptr = F\n";
    let file = parse(SourceId(0), source).expect("parse");
    let errors = analyze(&file).expect_err("must fail");

    assert!(errors.iter().any(|error| {
        error
            .message
            .contains("var initializer must be an exact integer value")
    }));
}

#[test]
fn var_array_length_requires_exact_integer_from_top_level_evaluator_constant() {
    let source = "[ N = 2.5 ]\nvar buf[N]\n";
    let file = parse(SourceId(0), source).expect("parse");
    let errors = analyze(&file).expect_err("must fail");

    assert!(errors.iter().any(|error| {
        error
            .message
            .contains("var array length must be an exact integer value")
    }));
}

#[test]
fn const_declarations_do_not_advance_var_allocator() {
    let source = "const C = 7\nvar first\nvar second\n";
    let file = parse(SourceId(0), source).expect("parse");
    let sema = analyze(&file).expect("analyze");

    assert_eq!(sema.vars.get("first").expect("first").address, 0);
    assert_eq!(sema.vars.get("second").expect("second").address, 1);
}

#[test]
fn duplicate_symbols_between_const_and_var_are_rejected() {
    let source = "const dup = 1\nvar dup\n";
    let file = parse(SourceId(0), source).expect("parse");
    let errors = analyze(&file).expect_err("must fail");

    assert!(
        errors
            .iter()
            .any(|error| error.message.contains("duplicate symbol 'dup'"))
    );
}

#[test]
fn const_initializer_must_be_a_constant_expression() {
    let source = "const A = B\n";
    let file = parse(SourceId(0), source).expect("parse");
    let errors = analyze(&file).expect_err("must fail");

    assert!(errors.iter().any(|error| {
        error
            .message
            .contains("const initializer 'B' must be a constant numeric expression")
    }));
}

#[test]
fn array_length_advances_allocator() {
    let source = "var header[4]\nvar next\n";
    let file = parse(SourceId(0), source).expect("parse");
    let sema = analyze(&file).expect("analyze");

    assert_eq!(sema.vars.get("header").expect("header").address, 0);
    assert_eq!(sema.vars.get("header").expect("header").size, 4);
    assert_eq!(sema.vars.get("next").expect("next").address, 4);
    assert_eq!(sema.vars.get("next").expect("next").size, 1);
}

#[test]
fn explicit_array_address_sets_next_auto_address_after_full_size() {
    let source = "var first\nvar tiles[3] = 0x100\nvar tail\n";
    let file = parse(SourceId(0), source).expect("parse");
    let sema = analyze(&file).expect("analyze");

    assert_eq!(sema.vars.get("first").expect("first").address, 0);
    assert_eq!(sema.vars.get("tiles").expect("tiles").address, 0x100);
    assert_eq!(sema.vars.get("tiles").expect("tiles").size, 3);
    assert_eq!(sema.vars.get("tail").expect("tail").address, 0x103);
}

#[test]
fn array_length_must_be_positive_literal() {
    let source = "var bad[0]\n";
    let file = parse(SourceId(0), source).expect("parse");
    let errors = analyze(&file).expect_err("must fail");

    assert!(
        errors
            .iter()
            .any(|error| error.message.contains("array length must be positive"))
    );
}

#[test]
fn duplicate_symbols_between_var_and_function_are_rejected() {
    let source = "var dup\nfunc dup {\n  nop\n}\n";
    let file = parse(SourceId(0), source).expect("parse");
    let errors = analyze(&file).expect_err("must fail");

    assert!(
        errors
            .iter()
            .any(|error| error.message.contains("duplicate symbol 'dup'"))
    );
}

#[test]
fn computes_symbolic_subscript_field_offsets_and_total_size() {
    let source =
        "var foo[\n  .field_w:word\n  .field_w2:word\n  .idx:byte\n  .string[4]:byte\n] = 0x1234\n";
    let file = parse(SourceId(0), source).expect("parse");
    let sema = analyze(&file).expect("analyze");

    let foo = sema.vars.get("foo").expect("foo");
    assert_eq!(foo.address, 0x1234);
    assert_eq!(foo.size, 9);
    let symbolic_subscript = foo.symbolic_subscript.as_ref().expect("symbolic subscript");
    let field_w = symbolic_subscript.fields.get("field_w").expect("field_w");
    let field_w2 = symbolic_subscript.fields.get("field_w2").expect("field_w2");
    let idx = symbolic_subscript.fields.get("idx").expect("idx");
    let string = symbolic_subscript.fields.get("string").expect("string");
    assert_eq!(field_w.offset, 0);
    assert_eq!(field_w2.offset, 2);
    assert_eq!(idx.offset, 4);
    assert_eq!(string.offset, 5);
}

#[test]
fn symbolic_subscript_array_requires_explicit_base_address() {
    let source = "var VIA[\n  .orb:byte\n  .ora:byte\n]\n";
    let file = parse(SourceId(0), source).expect("parse");
    let errors = analyze(&file).expect_err("must fail");
    assert!(errors.iter().any(|error| {
        error
            .message
            .contains("is missing a base address assignment")
    }));
}

#[test]
fn symbolic_subscript_rejects_duplicate_field_names() {
    let source = "var VIA[\n  .orb:byte\n  .orb:word\n] = 0x6000\n";
    let file = parse(SourceId(0), source).expect("parse");
    let errors = analyze(&file).expect_err("must fail");
    assert!(errors.iter().any(|error| {
        error
            .message
            .contains("duplicate symbolic subscript field '.orb'")
    }));
}

#[test]
fn symbolic_subscript_base_expression_must_be_constant() {
    let source = "var VIA[\n  .orb:byte\n] = base_addr\n";
    let file = parse(SourceId(0), source).expect("parse");
    let errors = analyze(&file).expect_err("must fail");
    assert!(errors.iter().any(|error| {
        error
            .message
            .contains("must be a constant numeric expression")
    }));
}

#[test]
fn symbolic_subscript_fields_use_default_var_width_when_type_is_omitted() {
    let source = "var baz:byte[\n  .a\n  .b\n  .len:word\n] = 0x2244\n";
    let file = parse(SourceId(0), source).expect("parse");
    let sema = analyze(&file).expect("analyze");

    let baz = sema.vars.get("baz").expect("baz");
    assert_eq!(baz.size, 4);
    let symbolic_subscript = baz.symbolic_subscript.as_ref().expect("symbolic subscript");
    assert_eq!(symbolic_subscript.fields.get("a").expect("a").offset, 0);
    assert_eq!(symbolic_subscript.fields.get("b").expect("b").offset, 1);
    assert_eq!(symbolic_subscript.fields.get("len").expect("len").offset, 2);
}

#[test]
fn symbolic_subscript_fields_default_to_byte_when_no_type_is_provided() {
    let source = "var foo[\n  .a\n  .b[2]\n  .w:word\n] = 0x1234\n";
    let file = parse(SourceId(0), source).expect("parse");
    let sema = analyze(&file).expect("analyze");

    let foo = sema.vars.get("foo").expect("foo");
    assert_eq!(foo.size, 5);
    let symbolic_subscript = foo.symbolic_subscript.as_ref().expect("symbolic subscript");
    let a = symbolic_subscript.fields.get("a").expect("a");
    let b = symbolic_subscript.fields.get("b").expect("b");
    let w = symbolic_subscript.fields.get("w").expect("w");
    assert_eq!(a.offset, 0);
    assert_eq!(b.offset, 1);
    assert_eq!(w.offset, 3);
    assert_eq!(a.data_width, DataWidth::Byte);
    assert_eq!(b.data_width, DataWidth::Byte);
    assert_eq!(w.data_width, DataWidth::Word);
}
