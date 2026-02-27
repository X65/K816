use super::*;

#[test]
fn parses_address_statement_inside_code_block() {
    let source = "func f {\n address $4000\n nop\n}\n";
    let file = parse(SourceId(0), source).expect("parse");
    assert_eq!(file.items.len(), 1);

    let Item::CodeBlock(block) = &file.items[0].node else {
        panic!("expected code block");
    };
    assert_eq!(block.body.len(), 2);
    assert!(matches!(block.body[0].node, Stmt::Address(0x4000)));
}

#[test]
fn parses_segment_statement_inside_code_block() {
    let source = "func f {\n segment fixed_hi\n nop\n}\n";
    let file = parse(SourceId(0), source).expect("parse");
    assert_eq!(file.items.len(), 1);

    let Item::CodeBlock(block) = &file.items[0].node else {
        panic!("expected code block");
    };
    assert_eq!(block.body.len(), 2);
    let Stmt::Segment(segment) = &block.body[0].node else {
        panic!("expected segment statement");
    };
    assert_eq!(segment.name, "fixed_hi");
}

#[test]
fn parses_label_and_instruction_statements() {
    let source = "func main {\n loop:\n nop\n}\n";
    let file = parse(SourceId(0), source).expect("parse");
    assert_eq!(file.items.len(), 1);

    let Item::CodeBlock(block) = &file.items[0].node else {
        panic!("expected code block");
    };
    assert_eq!(block.body.len(), 2);

    let Stmt::Label(label) = &block.body[0].node else {
        panic!("expected label");
    };
    assert_eq!(label.name, "loop");

    let Stmt::Instruction(instr) = &block.body[1].node else {
        panic!("expected instruction");
    };
    assert_eq!(instr.mnemonic, "nop");
    assert!(instr.operand.is_none());
}

#[test]
fn parses_hla_statements_without_text_desugaring() {
    let source = "func main {\n  x = 0\n  {\n    { a&?UART_READY } n-?\n    UART_DATA = a = text,x\n    x++\n  } a?0 !=\n  API_OP = a = [$FF]\n}\n";
    let file = parse(SourceId(0), source).expect("parse");
    let Item::CodeBlock(block) = &file.items[0].node else {
        panic!("expected code block");
    };

    assert_eq!(block.body.len(), 7);
    assert!(matches!(
        block.body[0].node,
        Stmt::Hla(HlaStmt::RegisterAssign {
            register: HlaCpuRegister::X,
            ..
        })
    ));
    assert!(matches!(block.body[1].node, Stmt::Hla(HlaStmt::DoOpen)));
    assert!(matches!(
        block.body[2].node,
        Stmt::Hla(HlaStmt::WaitLoopWhileNFlagClear { .. })
    ));
    assert!(matches!(
        block.body[3].node,
        Stmt::Hla(HlaStmt::StoreFromA { .. })
    ));
    assert!(matches!(block.body[4].node, Stmt::Hla(HlaStmt::XIncrement)));
    assert!(matches!(
        block.body[5].node,
        Stmt::Hla(HlaStmt::DoClose { .. })
    ));
    assert!(matches!(
        block.body[6].node,
        Stmt::Hla(HlaStmt::StoreFromA { .. })
    ));
}

#[test]
fn parses_hla_do_close_with_n_flag_suffix() {
    let source = "func main {\n  {\n    a=READY\n  } n-?\n}\n";
    let file = parse(SourceId(0), source).expect("parse");
    let Item::CodeBlock(block) = &file.items[0].node else {
        panic!("expected code block");
    };

    assert!(matches!(block.body[0].node, Stmt::Hla(HlaStmt::DoOpen)));
    assert!(matches!(
        block.body[1].node,
        Stmt::Hla(HlaStmt::RegisterAssign { .. })
    ));
    assert!(matches!(
        block.body[2].node,
        Stmt::Hla(HlaStmt::DoCloseNFlagClear)
    ));
}

#[test]
fn parses_hla_do_close_with_n_plus_suffix() {
    let source = "func main {\n  {\n    a=READY\n  } n+?\n}\n";
    let file = parse(SourceId(0), source).expect("parse");
    let Item::CodeBlock(block) = &file.items[0].node else {
        panic!("expected code block");
    };

    assert!(matches!(block.body[0].node, Stmt::Hla(HlaStmt::DoOpen)));
    assert!(matches!(
        block.body[1].node,
        Stmt::Hla(HlaStmt::RegisterAssign { .. })
    ));
    assert!(matches!(
        block.body[2].node,
        Stmt::Hla(HlaStmt::DoCloseNFlagSet)
    ));
}

#[test]
fn parses_stack_shorthand_as_hla_nodes() {
    let source = "func main {\n  a!!\n  p??\n  flag!!\n  z??\n}\n";
    let file = parse(SourceId(0), source).expect("parse");
    let Item::CodeBlock(block) = &file.items[0].node else {
        panic!("expected code block");
    };
    assert_eq!(block.body.len(), 4);
    assert!(matches!(
        block.body[0].node,
        Stmt::Hla(HlaStmt::StackOp {
            target: HlaStackTarget::A,
            push: true
        })
    ));
    assert!(matches!(
        block.body[1].node,
        Stmt::Hla(HlaStmt::StackOp {
            target: HlaStackTarget::P,
            push: false
        })
    ));
    assert!(matches!(
        block.body[2].node,
        Stmt::Hla(HlaStmt::StackOp {
            target: HlaStackTarget::P,
            push: true
        })
    ));
    assert!(matches!(
        block.body[3].node,
        Stmt::Hla(HlaStmt::StackOp {
            target: HlaStackTarget::P,
            push: false
        })
    ));
}

#[test]
fn rejects_unknown_stack_shorthand_target() {
    let source = "func main {\n  foo!!\n}\n";
    let errors = parse(SourceId(0), source).expect_err("must fail");
    assert!(
        errors
            .iter()
            .any(|error| error.message.contains("unsupported stack shorthand")),
        "expected unsupported stack shorthand error, got: {errors:?}"
    );
}

#[test]
fn warns_for_inefficient_postfix_le_and_gt_without_condition_seed() {
    let source = "func main {\n  {\n    x++\n  } <=\n  {\n    y++\n  } >\n}\n";
    let parsed = parse_with_warnings(SourceId(0), source).expect("parse");
    assert_eq!(parsed.warnings.len(), 2);
    assert!(parsed.warnings[0].message.contains("`} <=`"));
    assert!(parsed.warnings[1].message.contains("`} >`"));
}

#[test]
fn does_not_warn_for_postfix_ops_with_condition_seed() {
    let source = "func main {\n  {\n    a?0\n  } <=\n}\n";
    let parsed = parse_with_warnings(SourceId(0), source).expect("parse");
    assert!(parsed.warnings.is_empty());
}

#[test]
fn parses_indexed_condition_seed_as_hla_node() {
    let source = "func main {\n  a?zp,x\n}\n";
    let file = parse(SourceId(0), source).expect("parse");
    let Item::CodeBlock(block) = &file.items[0].node else {
        panic!("expected code block");
    };
    let Stmt::Hla(HlaStmt::ConditionSeed { lhs, rhs }) = &block.body[0].node else {
        panic!("expected HLA condition seed");
    };
    assert_eq!(*lhs, HlaRegister::A);
    assert_eq!(rhs.index, Some(IndexRegister::X));
    assert_eq!(rhs.addr_mode, OperandAddrMode::Direct);
}

#[test]
fn does_not_warn_for_postfix_ops_with_indexed_condition_seed() {
    let source = "func main {\n  {\n    a?zp,x\n  } <=\n}\n";
    let parsed = parse_with_warnings(SourceId(0), source).expect("parse");
    assert!(parsed.warnings.is_empty());
}

#[test]
fn keeps_bracketed_eval_ident_in_register_assignments() {
    let source = "[ A = 20, B = 30 ]\nfunc main {\n  a=[A]\n  x=[B]\n}\n";
    let file = parse(SourceId(0), source).expect("parse");
    let Item::CodeBlock(block) = &file.items[1].node else {
        panic!("expected code block");
    };
    assert_eq!(block.body.len(), 2);

    let expect_bracketed_assign = |stmt: &Stmt, register: HlaCpuRegister, name: &str| {
        let Stmt::Hla(HlaStmt::RegisterAssign { register: dst, rhs }) = stmt else {
            panic!("expected HLA register assignment");
        };
        assert_eq!(*dst, register);
        assert!(matches!(
            &rhs.expr,
            Expr::Unary {
                op: ExprUnaryOp::EvalBracketed,
                expr
            } if is_ident_named(expr.as_ref(), name)
        ));
    };

    expect_bracketed_assign(&block.body[0].node, HlaCpuRegister::A, "A");
    expect_bracketed_assign(&block.body[1].node, HlaCpuRegister::X, "B");
}

#[test]
fn parses_operand_modes_with_y_and_indirect_forms() {
    let source = "var ptr = 0x20\nfunc main {\n  a=ptr,y\n  a=(ptr)\n  a=(ptr,x)\n  a=(ptr),y\n}\n";
    let file = parse(SourceId(0), source).expect("parse");
    assert_eq!(file.items.len(), 2);

    let Item::CodeBlock(block) = &file.items[1].node else {
        panic!("expected code block");
    };
    assert_eq!(block.body.len(), 4);

    let expect_mode = |stmt: &Stmt, mode: OperandAddrMode, index: Option<IndexRegister>| {
        let Stmt::Hla(HlaStmt::RegisterAssign { register, rhs }) = stmt else {
            panic!("expected HLA register assignment");
        };
        assert_eq!(*register, HlaCpuRegister::A);
        assert!(is_ident_named(&rhs.expr, "ptr"));
        assert_eq!(rhs.index, index);
        assert_eq!(rhs.addr_mode, mode);
    };

    expect_mode(
        &block.body[0].node,
        OperandAddrMode::Direct,
        Some(IndexRegister::Y),
    );
    expect_mode(&block.body[1].node, OperandAddrMode::Indirect, None);
    expect_mode(&block.body[2].node, OperandAddrMode::IndexedIndirectX, None);
    expect_mode(&block.body[3].node, OperandAddrMode::IndirectIndexedY, None);
}

#[test]
fn reports_unsupported_flag_shorthand_for_invalid_flag_goto() {
    let source = "func main {\n  s-? goto .skip\n.skip:\n  return\n}\n";
    let diagnostics = parse(SourceId(0), source).expect_err("expected parse errors");
    assert!(
        diagnostics
            .iter()
            .any(|diag| diag.message == "invalid syntax: unsupported flag shorthand 's-?'"),
        "unexpected diagnostics: {diagnostics:#?}"
    );
    assert!(diagnostics.iter().any(|diag| {
        diag.supplements.iter().any(|supplement| {
            matches!(
                supplement,
                crate::diag::Supplemental::Help(help)
                    if help == "expected one of c+, c-, d+, d-, i+, i-, or v-"
            )
        })
    }));
}

#[test]
fn invalid_flag_goto_parser_matches_signed_flag_form() {
    let source = "s-? goto .skip";
    let tokens = lex(SourceId(0), source).expect("lex");
    let end_offset = tokens.last().map(|token| token.span.end).unwrap_or(0);
    let token_stream = Stream::from_iter(tokens.into_iter().map(|token| {
        let span = (token.span.start..token.span.end).into();
        (token.kind, span)
    }))
    .map((end_offset..end_offset).into(), |(kind, span): (_, _)| {
        (kind, span)
    });

    let (_output, errors) = invalid_flag_goto_stmt_parser()
        .parse(token_stream)
        .into_output_errors();
    assert!(
        errors.iter().any(|error| matches!(
            error.reason(),
            RichReason::Custom(message)
                if message == "unsupported flag shorthand 's-?'"
        )),
        "unexpected parser errors: {errors:#?}"
    );
}

#[test]
fn keeps_flag_and_symbolic_branch_goto_forms_distinct() {
    let source =
        "func main {\n  c-? goto target\n  < goto target\n  v+ goto target\n  <<= goto target\n}\n";
    let parsed = parse(SourceId(0), source).expect("parse");
    let Item::CodeBlock(block) = &parsed.items[0].node else {
        panic!("expected code block");
    };

    let first = &block.body[0].node;
    let second = &block.body[1].node;
    let third = &block.body[2].node;
    let fourth = &block.body[3].node;

    assert!(matches!(
        first,
        Stmt::Hla(HlaStmt::BranchGoto {
            mnemonic,
            form: HlaBranchForm::FlagQuestion,
            ..
        }) if mnemonic == "bcc"
    ));
    assert!(matches!(
        second,
        Stmt::Hla(HlaStmt::BranchGoto {
            mnemonic,
            form: HlaBranchForm::Symbolic,
            ..
        }) if mnemonic == "bcc"
    ));
    assert!(matches!(
        third,
        Stmt::Hla(HlaStmt::BranchGoto {
            mnemonic,
            form: HlaBranchForm::FlagPlain,
            ..
        }) if mnemonic == "bvs"
    ));
    assert!(matches!(
        fourth,
        Stmt::Hla(HlaStmt::BranchGoto {
            mnemonic,
            form: HlaBranchForm::Symbolic,
            ..
        }) if mnemonic == "bvs"
    ));
}

#[test]
fn keeps_prefix_flag_and_symbolic_forms_distinct() {
    let source = "func main {\n  c-?{ a=1 }\n  <{ a=2 }\n  v+?{ a=3 }\n  >>={ a=4 }\n}\n";
    let parsed = parse(SourceId(0), source).expect("parse");
    let Item::CodeBlock(block) = &parsed.items[0].node else {
        panic!("expected code block");
    };

    let first = &block.body[0].node;
    let second = &block.body[1].node;
    let third = &block.body[2].node;
    let fourth = &block.body[3].node;

    assert!(matches!(
        first,
        Stmt::Hla(HlaStmt::PrefixConditional {
            skip_mnemonic,
            form: HlaBranchForm::FlagQuestion,
            ..
        }) if skip_mnemonic == "bcs"
    ));
    assert!(matches!(
        second,
        Stmt::Hla(HlaStmt::PrefixConditional {
            skip_mnemonic,
            form: HlaBranchForm::Symbolic,
            ..
        }) if skip_mnemonic == "bcs"
    ));
    assert!(matches!(
        third,
        Stmt::Hla(HlaStmt::PrefixConditional {
            skip_mnemonic,
            form: HlaBranchForm::FlagQuestion,
            ..
        }) if skip_mnemonic == "bvc"
    ));
    assert!(matches!(
        fourth,
        Stmt::Hla(HlaStmt::PrefixConditional {
            skip_mnemonic,
            form: HlaBranchForm::Symbolic,
            ..
        }) if skip_mnemonic == "bvs"
    ));
}
