use super::*;

#[test]
fn parses_var_array_length() {
    let source = "var tiles[16]\n";
    let file = parse(SourceId(0), source).expect("parse");
    assert_eq!(file.items.len(), 1);

    let Item::Var(var) = &file.items[0].node else {
        panic!("expected var item");
    };

    assert!(matches!(var.array_len, Some(Expr::Number(16, _))));
    assert!(var.symbolic_subscript_fields.is_none());
    assert!(var.initializer.is_none());
}

#[test]
fn rejects_var_abs_suffix() {
    let source = "var dp:abs = $12\n";
    let result = parse(SourceId(0), source);
    assert!(
        result.is_err(),
        "expected parse error for removed :abs suffix on var"
    );
}

#[test]
fn rejects_var_data_width_with_abs_suffix() {
    let source = "var table:word:abs = $2000\n";
    let result = parse(SourceId(0), source);
    assert!(
        result.is_err(),
        "expected parse error for removed :abs suffix on var"
    );
}

#[test]
fn parses_symbolic_subscript_field_list_with_commas_and_trailing_separator() {
    let source = "var foo[\n  .field_w:word,\n  .idx:byte,\n  .string :byte [20],\n  .words:word[4],\n  .ptrs:far[2],\n  .scratch[3],\n] = 0x1234\n";
    let file = parse(SourceId(0), source).expect("parse");
    assert_eq!(file.items.len(), 1);

    let Item::Var(var) = &file.items[0].node else {
        panic!("expected var item");
    };
    assert!(var.array_len.is_none());
    assert!(matches!(var.initializer, Some(Expr::Number(0x1234, _))));
    let fields = var
        .symbolic_subscript_fields
        .as_ref()
        .expect("symbolic subscript field list");
    assert_eq!(fields.len(), 6);
    assert_eq!(fields[0].name, "field_w");
    assert_eq!(fields[1].name, "idx");
    assert_eq!(fields[2].name, "string");
    assert_eq!(fields[3].name, "words");
    assert_eq!(fields[4].name, "ptrs");
    assert_eq!(fields[5].name, "scratch");
    assert!(matches!(fields[0].data_width, Some(DataWidth::Word)));
    assert!(matches!(fields[1].data_width, Some(DataWidth::Byte)));
    assert!(matches!(fields[2].data_width, Some(DataWidth::Byte)));
    assert!(matches!(fields[2].count, Some(Expr::Number(20, _))));
    assert!(matches!(fields[3].data_width, Some(DataWidth::Word)));
    assert!(matches!(fields[3].count, Some(Expr::Number(4, _))));
    assert!(matches!(fields[4].data_width, Some(DataWidth::Far)));
    assert!(matches!(fields[4].count, Some(Expr::Number(2, _))));
    assert!(fields[5].data_width.is_none());
    assert!(matches!(fields[5].count, Some(Expr::Number(3, _))));
}

#[test]
fn parses_symbolic_subscript_field_list_with_leading_comments() {
    let source = "var regs[\n  // control registers\n  .ctrl:word\n  /* status byte */\n  .status:byte\n] = 0x2100\n";
    let file = parse(SourceId(0), source).expect("parse");
    assert_eq!(file.items.len(), 1);

    let Item::Var(var) = &file.items[0].node else {
        panic!("expected var item");
    };
    let fields = var
        .symbolic_subscript_fields
        .as_ref()
        .expect("symbolic subscript field list");
    assert_eq!(fields.len(), 2);
    assert_eq!(fields[0].name, "ctrl");
    assert_eq!(fields[1].name, "status");
    assert!(matches!(fields[0].data_width, Some(DataWidth::Word)));
    assert!(matches!(fields[1].data_width, Some(DataWidth::Byte)));
}

#[test]
fn parses_symbolic_subscript_forms() {
    let source = "func main {\n  a=foo[.idx]\n  a=foo.string[2]\n}\n";
    let file = parse(SourceId(0), source).expect("parse");
    let Item::CodeBlock(block) = &file.items[0].node else {
        panic!("expected code block");
    };
    assert_eq!(block.body.len(), 2);

    let Stmt::Hla(HlaStmt::RegisterAssign { rhs, .. }) = &block.body[0].node else {
        panic!("expected first HLA register assignment");
    };
    assert!(is_ident_named(&rhs.expr, "foo.idx"));

    let Stmt::Hla(HlaStmt::RegisterAssign { rhs, .. }) = &block.body[1].node else {
        panic!("expected second HLA register assignment");
    };
    assert!(matches!(
        &rhs.expr,
        Expr::Index { base, index }
            if is_ident_named(base.as_ref(), "foo.string")
            && matches!(index.as_ref(), Expr::Number(2, _))
    ));
}

#[test]
fn parses_symbolic_subscript_fields_with_default_var_width() {
    let source = "var baz:byte[\n  .a\n  .b\n  .len:word\n] = 0x2244\n";
    let file = parse(SourceId(0), source).expect("parse");
    let Item::Var(var) = &file.items[0].node else {
        panic!("expected var item");
    };
    assert!(matches!(var.data_width, Some(DataWidth::Byte)));
    let fields = var
        .symbolic_subscript_fields
        .as_ref()
        .expect("symbolic subscript field list");
    assert_eq!(fields.len(), 3);
    assert!(fields[0].data_width.is_none());
    assert!(fields[1].data_width.is_none());
    assert!(matches!(fields[2].data_width, Some(DataWidth::Word)));
}

#[test]
fn parses_top_level_abstract_var_layout() {
    let source = "abstract var SHAPE:byte[\n  .x\n  .y:word\n]\n";
    let file = parse(SourceId(0), source).expect("parse");
    let Item::Var(var) = &file.items[0].node else {
        panic!("expected var item");
    };
    assert!(var.is_abstract);
    assert!(matches!(var.data_width, Some(DataWidth::Byte)));
    assert!(var.initializer.is_none());
    assert_eq!(
        var.symbolic_subscript_fields
            .as_ref()
            .expect("fields")
            .len(),
        2
    );
}

#[test]
fn rejects_unsupported_symbolic_subscript_field_type_in_var_brackets() {
    let source = "var foo[\n  .a:dword\n] = 0x1234\n";
    let errors = parse(SourceId(0), source).expect_err("must fail");

    assert!(
        !errors.is_empty(),
        "expected parse errors for unsupported field type"
    );
}

#[test]
fn rejects_empty_symbolic_subscript_array_count_at_field_slice() {
    let source = "var foo[\n  .a:byte[]\n] = 0x1234\n";
    let errors = parse(SourceId(0), source).expect_err("must fail");

    assert!(
        !errors.is_empty(),
        "expected parse errors for empty array count"
    );
}

#[test]
fn rejects_old_typed_symbolic_subscript_count_order() {
    for source in [
        "var foo[\n  .string[20]:byte\n] = 0x1234\n",
        "var foo[\n  .string[20] :byte\n] = 0x1234\n",
    ] {
        let errors = parse(SourceId(0), source).expect_err("must fail");
        assert!(
            errors.iter().any(|diag| {
                diag.primary_label == "old field-count/type order"
                    && diag.supplements.iter().any(|supplement| {
                        matches!(
                            supplement,
                            crate::diag::Supplemental::Help(help)
                                if help.contains("`.string:byte[20]`")
                        )
                    })
            }),
            "unexpected diagnostics: {errors:#?}"
        );
    }
}

#[test]
fn parses_anonymous_data_block_with_converter_and_directives() {
    let source = "data {\n align 16\n address 0x1234\n nocross 0x100\n binary(\"tiles\", 3)\n}\n";
    let file = parse(SourceId(0), source).expect("parse");
    assert_eq!(file.items.len(), 1);

    let Item::DataBlock(block) = &file.items[0].node else {
        panic!("expected data block");
    };
    assert!(block.name.is_none());
    assert_eq!(block.entries.len(), 4);

    assert!(matches!(block.entries[0].node, DataEntry::Align(16)));
    assert!(matches!(block.entries[1].node, DataEntry::Address(0x1234)));
    assert!(matches!(block.entries[2].node, DataEntry::Nocross(0x100)));

    let DataEntry::Convert { kind, args } = &block.entries[3].node else {
        panic!("expected converter entry");
    };
    assert_eq!(kind, "binary");
    assert_eq!(
        args,
        &vec![DataArg::Str("tiles".to_string()), DataArg::Int(3)]
    );
}

#[test]
fn named_and_anonymous_data_blocks_produce_equivalent_ast() {
    let named = parse(
        SourceId(0),
        "data SOMEBLOCK {\n 0 1 2 3 4\n FOO: 5 6 7\n}\n",
    )
    .expect("parse named");
    let anon = parse(
        SourceId(0),
        "data {\n SOMEBLOCK: 0 1 2 3 4\n FOO: 5 6 7\n}\n",
    )
    .expect("parse anon");

    let Item::DataBlock(named_block) = &named.items[0].node else {
        panic!("expected named data block");
    };
    let Item::DataBlock(anon_block) = &anon.items[0].node else {
        panic!("expected anonymous data block");
    };

    assert_eq!(named_block.name.as_deref(), Some("SOMEBLOCK"));
    assert!(anon_block.name.is_none());

    // The anonymous form has the leading label `SOMEBLOCK:` as its first entry,
    // followed by the same body as the named form.
    assert!(matches!(
        anon_block.entries[0].node,
        DataEntry::Label(ref n) if n == "SOMEBLOCK"
    ));
    let anon_tail: Vec<_> = anon_block.entries.iter().skip(1).collect();
    assert_eq!(anon_tail.len(), named_block.entries.len());
}

#[test]
fn parses_named_data_block_entries() {
    let source = "data text {\n  segment INFO\n  \"Hello\"\n  $0D 'A' $00\n}\n";
    let file = parse(SourceId(0), source).expect("parse");
    let Item::DataBlock(block) = &file.items[0].node else {
        panic!("expected named data block");
    };
    assert_eq!(block.name.as_deref(), Some("text"));
    assert_eq!(block.entries.len(), 3);
    assert!(matches!(block.entries[0].node, DataEntry::Segment(_)));
    assert!(matches!(block.entries[1].node, DataEntry::String(_)));
    assert!(matches!(
        block.entries[2].node,
        DataEntry::Values {
            width: DataWidth::Byte,
            ..
        }
    ));
}

#[test]
fn parses_named_data_for_eval_range_entries() {
    let source = "data table {\n  for i=0..4 eval [ i * FACTOR ]\n  for j=4..0 eval [ j ]\n}\n";
    let file = parse(SourceId(0), source).expect("parse");
    let Item::DataBlock(block) = &file.items[0].node else {
        panic!("expected named data block");
    };
    assert_eq!(block.entries.len(), 2);

    let DataEntry::ForEvalRange(forward) = &block.entries[0].node else {
        panic!("expected forward for-eval entry");
    };
    assert_eq!(forward.iterator, "i");
    assert!(matches!(forward.start, Expr::Number(0, _)));
    assert!(matches!(forward.end, Expr::Number(4, _)));
    assert_eq!(forward.eval.trim(), "i * FACTOR");

    let DataEntry::ForEvalRange(reverse) = &block.entries[1].node else {
        panic!("expected reverse for-eval entry");
    };
    assert_eq!(reverse.iterator, "j");
    assert!(matches!(reverse.start, Expr::Number(4, _)));
    assert!(matches!(reverse.end, Expr::Number(0, _)));
    assert_eq!(reverse.eval.trim(), "j");
}

#[test]
fn data_block_recovers_from_unknown_entries() {
    let cases = [
        "data gfx {\n  foo\n}\nfunc main {\n  a=0\n}\n",
        "data gfx {\n  image sprites 0 0\n  tiles 8 0 4\n}\nfunc main {\n  a=0\n}\n",
        "data gfx {\n  image\n}\nfunc main {\n  a=0\n}\n",
    ];
    for source in cases {
        let (file, _diagnostics) = parse_lenient(SourceId(0), source);
        let file = file.expect("should produce AST despite errors");
        assert!(
            file.items
                .iter()
                .any(|i| matches!(&i.node, Item::DataBlock(b) if b.name.is_some())),
            "data block not found in AST for: {source:?}",
        );
    }
}

#[test]
fn parses_main_symbol_in_address_byte_entries() {
    let source = "data vectors {\n  &<main &>main\n}\nfunc main {\n  return\n}\n";
    let file = parse(SourceId(0), source).expect("parse");
    assert_eq!(file.items.len(), 2);

    let Item::DataBlock(block) = &file.items[0].node else {
        panic!("expected named data block");
    };
    assert_eq!(block.entries.len(), 1);

    let DataEntry::Values {
        width: DataWidth::Byte,
        values,
    } = &block.entries[0].node
    else {
        panic!("expected bytes entry");
    };
    assert_eq!(values.len(), 2);
    assert!(matches!(
        &values[0],
        Expr::Unary {
            op: ExprUnaryOp::LowByte,
            expr
        } if is_ident_named(expr.as_ref(), "main")
    ));
    assert!(matches!(
        &values[1],
        Expr::Unary {
            op: ExprUnaryOp::HighByte,
            expr
        } if is_ident_named(expr.as_ref(), "main")
    ));
}

#[test]
fn parses_packed_address_operators_in_bytes_entries() {
    let source = "data bytes {\n  &&ptr\n  &&&ptr\n}\n";
    let file = parse(SourceId(0), source).expect("parse");
    let Item::DataBlock(block) = &file.items[0].node else {
        panic!("expected named data block");
    };
    assert_eq!(block.entries.len(), 2);

    let DataEntry::Values {
        width: DataWidth::Byte,
        values: first,
    } = &block.entries[0].node
    else {
        panic!("expected bytes entry");
    };
    assert_eq!(first.len(), 1);
    assert!(matches!(
        &first[0],
        Expr::Unary {
            op: ExprUnaryOp::WordLittleEndian,
            expr
        } if is_ident_named(expr.as_ref(), "ptr")
    ));

    let DataEntry::Values {
        width: DataWidth::Byte,
        values: second,
    } = &block.entries[1].node
    else {
        panic!("expected bytes entry");
    };
    assert_eq!(second.len(), 1);
    assert!(matches!(
        &second[0],
        Expr::Unary {
            op: ExprUnaryOp::FarLittleEndian,
            expr
        } if is_ident_named(expr.as_ref(), "ptr")
    ));
}

#[test]
fn keeps_bracketed_eval_ident_in_data_entries() {
    let source = "data text_data {\n  evaluator [ SCALE = 2 ]\n  [SCALE]\n}\n";
    let file = parse(SourceId(0), source).expect("parse");
    let Item::DataBlock(block) = &file.items[0].node else {
        panic!("expected named data block");
    };
    assert_eq!(block.entries.len(), 2);

    let DataEntry::Values {
        width: DataWidth::Byte,
        values,
    } = &block.entries[1].node
    else {
        panic!("expected bytes entry");
    };
    assert_eq!(values.len(), 1);
    assert!(matches!(
        &values[0],
        Expr::Unary {
            op: ExprUnaryOp::EvalBracketed,
            expr
        } if is_ident_named(expr.as_ref(), "SCALE")
    ));
}
