use crate::ast::{
    CodeBlock, ConstDecl, Expr, File, HlaCondition, HlaRhs, HlaStmt, Instruction, Item,
    NamedDataBlock, NamedDataEntry, NamedDataForEvalRange, Operand, Stmt,
    SymbolicSubscriptFieldDecl, VarDecl,
};
use crate::diag::Diagnostic;
use crate::parser::parse_expression_fragment;
use crate::span::{SourceId, Span, Spanned};

pub fn expand_file(file: &File, source_id: SourceId) -> Result<File, Vec<Diagnostic>> {
    let mut diagnostics = Vec::new();
    let mut items = Vec::with_capacity(file.items.len());

    for item in &file.items {
        let expanded = expand_item(&item.node, item.span, source_id, &mut diagnostics);
        items.push(Spanned::new(expanded, item.span));
    }

    if diagnostics.is_empty() {
        Ok(File {
            mode_default: file.mode_default,
            items,
        })
    } else {
        Err(diagnostics)
    }
}

fn expand_item(
    item: &Item,
    span: Span,
    source_id: SourceId,
    diagnostics: &mut Vec<Diagnostic>,
) -> Item {
    match item {
        Item::Const(const_decl) => {
            Item::Const(expand_const(const_decl, span, source_id, diagnostics))
        }
        Item::EvaluatorBlock(block) => Item::EvaluatorBlock(block.clone()),
        Item::Var(var) => Item::Var(expand_var(var, span, source_id, diagnostics)),
        Item::CodeBlock(block) => Item::CodeBlock(expand_code_block(block, source_id, diagnostics)),
        Item::Statement(stmt) => Item::Statement(expand_stmt(stmt, span, source_id, diagnostics)),
        Item::Segment(segment) => Item::Segment(segment.clone()),
        Item::DataBlock(block) => Item::DataBlock(block.clone()),
        Item::NamedDataBlock(block) => {
            Item::NamedDataBlock(expand_named_data_block(block, source_id, diagnostics))
        }
    }
}

fn expand_code_block(
    block: &CodeBlock,
    source_id: SourceId,
    diagnostics: &mut Vec<Diagnostic>,
) -> CodeBlock {
    let mut body = Vec::with_capacity(block.body.len());
    for stmt in &block.body {
        body.push(Spanned::new(
            expand_stmt(&stmt.node, stmt.span, source_id, diagnostics),
            stmt.span,
        ));
    }

    CodeBlock {
        name: block.name.clone(),
        name_span: block.name_span,
        is_far: block.is_far,
        is_naked: block.is_naked,
        is_inline: block.is_inline,
        mode_contract: block.mode_contract,
        body,
    }
}

fn expand_stmt(
    stmt: &Stmt,
    span: Span,
    source_id: SourceId,
    diagnostics: &mut Vec<Diagnostic>,
) -> Stmt {
    match stmt {
        Stmt::Segment(segment) => Stmt::Segment(segment.clone()),
        Stmt::Var(var) => Stmt::Var(expand_var(var, span, source_id, diagnostics)),
        Stmt::Instruction(instruction) => Stmt::Instruction(expand_instruction(
            instruction,
            span,
            source_id,
            diagnostics,
        )),
        Stmt::DataBlock(block) => Stmt::DataBlock(block.clone()),
        Stmt::Address(value) => Stmt::Address(*value),
        Stmt::Align(value) => Stmt::Align(*value),
        Stmt::Nocross(value) => Stmt::Nocross(*value),
        Stmt::Label(label) => Stmt::Label(label.clone()),
        Stmt::Call(call) => Stmt::Call(call.clone()),
        Stmt::ModeSet { a_width, i_width } => Stmt::ModeSet {
            a_width: *a_width,
            i_width: *i_width,
        },
        Stmt::ModeScopedBlock {
            a_width,
            i_width,
            body,
        } => Stmt::ModeScopedBlock {
            a_width: *a_width,
            i_width: *i_width,
            body: body
                .iter()
                .map(|s| Spanned::new(expand_stmt(&s.node, s.span, source_id, diagnostics), s.span))
                .collect(),
        },
        Stmt::SwapAB => Stmt::SwapAB,
        Stmt::TransferChain(instrs) => Stmt::TransferChain(instrs.clone()),
        Stmt::Hla(stmt) => Stmt::Hla(expand_hla_stmt(stmt, span, source_id, diagnostics)),
        Stmt::Empty => Stmt::Empty,
    }
}

fn expand_named_data_block(
    block: &NamedDataBlock,
    source_id: SourceId,
    diagnostics: &mut Vec<Diagnostic>,
) -> NamedDataBlock {
    let mut entries = Vec::with_capacity(block.entries.len());
    for entry in &block.entries {
        entries.push(Spanned::new(
            expand_named_data_entry(&entry.node, entry.span, source_id, diagnostics),
            entry.span,
        ));
    }

    NamedDataBlock {
        name: block.name.clone(),
        name_span: block.name_span,
        entries,
    }
}

fn expand_named_data_entry(
    entry: &NamedDataEntry,
    span: Span,
    source_id: SourceId,
    diagnostics: &mut Vec<Diagnostic>,
) -> NamedDataEntry {
    match entry {
        NamedDataEntry::Segment(segment) => NamedDataEntry::Segment(segment.clone()),
        NamedDataEntry::Address(value) => NamedDataEntry::Address(*value),
        NamedDataEntry::Align(value) => NamedDataEntry::Align(*value),
        NamedDataEntry::Nocross(value) => NamedDataEntry::Nocross(*value),
        NamedDataEntry::Bytes(values) => NamedDataEntry::Bytes(
            values
                .iter()
                .map(|expr| expand_expr(expr, span, source_id, diagnostics))
                .collect(),
        ),
        NamedDataEntry::ForEvalRange(range) => {
            NamedDataEntry::ForEvalRange(NamedDataForEvalRange {
                iterator: range.iterator.clone(),
                start: expand_expr(&range.start, span, source_id, diagnostics),
                end: expand_expr(&range.end, span, source_id, diagnostics),
                eval: range.eval.clone(),
            })
        }
        NamedDataEntry::String(value) => NamedDataEntry::String(value.clone()),
    }
}

fn expand_hla_stmt(
    stmt: &HlaStmt,
    span: Span,
    source_id: SourceId,
    diagnostics: &mut Vec<Diagnostic>,
) -> HlaStmt {
    match stmt {
        HlaStmt::XAssignImmediate { rhs } => HlaStmt::XAssignImmediate {
            rhs: expand_expr(rhs, span, source_id, diagnostics),
        },
        HlaStmt::XIncrement => HlaStmt::XIncrement,
        HlaStmt::StoreFromA { dests, rhs } => HlaStmt::StoreFromA {
            dests: dests.clone(),
            rhs: expand_hla_rhs(rhs, span, source_id, diagnostics),
        },
        HlaStmt::WaitLoopWhileNFlagClear { symbol } => HlaStmt::WaitLoopWhileNFlagClear {
            symbol: symbol.clone(),
        },
        HlaStmt::ConditionSeed { lhs, rhs } => HlaStmt::ConditionSeed {
            lhs: *lhs,
            rhs: expand_expr(rhs, span, source_id, diagnostics),
        },
        HlaStmt::DoOpen => HlaStmt::DoOpen,
        HlaStmt::DoCloseNFlagClear => HlaStmt::DoCloseNFlagClear,
        HlaStmt::DoCloseNFlagSet => HlaStmt::DoCloseNFlagSet,
        HlaStmt::DoCloseWithOp { op } => HlaStmt::DoCloseWithOp { op: *op },
        HlaStmt::DoClose { condition } => HlaStmt::DoClose {
            condition: expand_hla_condition(condition, span, source_id, diagnostics),
        },
        HlaStmt::DoCloseAlways => HlaStmt::DoCloseAlways,
        HlaStmt::DoCloseNever => HlaStmt::DoCloseNever,
        HlaStmt::DoCloseBranch { mnemonic } => HlaStmt::DoCloseBranch {
            mnemonic: mnemonic.clone(),
        },
        HlaStmt::LoopBreak { mnemonic } => HlaStmt::LoopBreak {
            mnemonic: mnemonic.clone(),
        },
        HlaStmt::LoopRepeat { mnemonic } => HlaStmt::LoopRepeat {
            mnemonic: mnemonic.clone(),
        },
        HlaStmt::RepeatNop(n) => HlaStmt::RepeatNop(*n),
        HlaStmt::PrefixConditional {
            skip_mnemonic,
            body,
            else_body,
        } => {
            let expanded_body = body
                .iter()
                .map(|s| {
                    crate::span::Spanned::new(
                        expand_stmt(&s.node, s.span, source_id, diagnostics),
                        s.span,
                    )
                })
                .collect();
            let expanded_else = else_body.as_ref().map(|stmts| {
                stmts
                    .iter()
                    .map(|s| {
                        crate::span::Spanned::new(
                            expand_stmt(&s.node, s.span, source_id, diagnostics),
                            s.span,
                        )
                    })
                    .collect()
            });
            HlaStmt::PrefixConditional {
                skip_mnemonic: skip_mnemonic.clone(),
                body: expanded_body,
                else_body: expanded_else,
            }
        }
    }
}

fn expand_hla_rhs(
    rhs: &HlaRhs,
    span: Span,
    source_id: SourceId,
    diagnostics: &mut Vec<Diagnostic>,
) -> HlaRhs {
    match rhs {
        HlaRhs::Immediate(expr) => {
            HlaRhs::Immediate(expand_expr(expr, span, source_id, diagnostics))
        }
        HlaRhs::Value {
            expr,
            index,
            addr_mode,
        } => HlaRhs::Value {
            expr: expand_expr(expr, span, source_id, diagnostics),
            index: *index,
            addr_mode: *addr_mode,
        },
    }
}

fn expand_hla_condition(
    condition: &HlaCondition,
    span: Span,
    source_id: SourceId,
    diagnostics: &mut Vec<Diagnostic>,
) -> HlaCondition {
    HlaCondition {
        lhs: condition.lhs,
        op: condition.op,
        rhs: condition
            .rhs
            .as_ref()
            .map(|rhs| expand_expr(rhs, span, source_id, diagnostics)),
    }
}

fn expand_const(
    const_decl: &ConstDecl,
    span: Span,
    source_id: SourceId,
    diagnostics: &mut Vec<Diagnostic>,
) -> ConstDecl {
    ConstDecl {
        name: const_decl.name.clone(),
        initializer: expand_expr(
            &const_decl.initializer,
            const_decl.initializer_span.unwrap_or(span),
            source_id,
            diagnostics,
        ),
        initializer_span: const_decl.initializer_span,
    }
}

fn expand_var(
    var: &VarDecl,
    span: Span,
    source_id: SourceId,
    diagnostics: &mut Vec<Diagnostic>,
) -> VarDecl {
    VarDecl {
        name: var.name.clone(),
        data_width: var.data_width,
        array_len: var
            .array_len
            .as_ref()
            .map(|expr| expand_expr(expr, span, source_id, diagnostics)),
        symbolic_subscript_fields: var.symbolic_subscript_fields.as_ref().map(|fields| {
            fields
                .iter()
                .map(|field| SymbolicSubscriptFieldDecl {
                    name: field.name.clone(),
                    data_width: field.data_width,
                    count: field.count.as_ref().map(|count| {
                        expand_expr(
                            count,
                            field.count_span.unwrap_or(field.span),
                            source_id,
                            diagnostics,
                        )
                    }),
                    count_span: field.count_span,
                    span: field.span,
                })
                .collect()
        }),
        initializer: var.initializer.as_ref().map(|expr| {
            expand_expr(
                expr,
                var.initializer_span.unwrap_or(span),
                source_id,
                diagnostics,
            )
        }),
        initializer_span: var.initializer_span,
    }
}

fn expand_instruction(
    instruction: &Instruction,
    span: Span,
    source_id: SourceId,
    diagnostics: &mut Vec<Diagnostic>,
) -> Instruction {
    let operand = instruction.operand.as_ref().map(|operand| match operand {
        Operand::Immediate {
            expr,
            explicit_hash,
        } => Operand::Immediate {
            expr: expand_expr(expr, span, source_id, diagnostics),
            explicit_hash: *explicit_hash,
        },
        Operand::Value {
            expr,
            force_far,
            index,
            addr_mode,
        } => Operand::Value {
            expr: expand_expr(expr, span, source_id, diagnostics),
            force_far: *force_far,
            index: *index,
            addr_mode: *addr_mode,
        },
        Operand::Auto { expr } => Operand::Auto {
            expr: expand_expr(expr, span, source_id, diagnostics),
        },
    });

    Instruction {
        mnemonic: instruction.mnemonic.clone(),
        operand,
    }
}

fn expand_expr(
    expr: &Expr,
    span: Span,
    source_id: SourceId,
    diagnostics: &mut Vec<Diagnostic>,
) -> Expr {
    match expr {
        Expr::EvalText(input) => {
            let expanded = match k816_eval::expand(input) {
                Ok(expanded) => expanded,
                Err(_err) => return Expr::Number(0),
            };

            match parse_expression_fragment(source_id, &expanded) {
                Ok(expr) => expr.node,
                Err(_err) => Expr::Number(0),
            }
        }
        Expr::Number(_) | Expr::Ident(_) | Expr::IdentSpanned { .. } => expr.clone(),
        Expr::Index { base, index } => Expr::Index {
            base: Box::new(expand_expr(base, span, source_id, diagnostics)),
            index: Box::new(expand_expr(index, span, source_id, diagnostics)),
        },
        Expr::Binary { op, lhs, rhs } => Expr::Binary {
            op: *op,
            lhs: Box::new(expand_expr(lhs, span, source_id, diagnostics)),
            rhs: Box::new(expand_expr(rhs, span, source_id, diagnostics)),
        },
        Expr::Unary { op, expr } => Expr::Unary {
            op: *op,
            expr: Box::new(expand_expr(expr, span, source_id, diagnostics)),
        },
        Expr::TypedView { expr, width } => Expr::TypedView {
            expr: Box::new(expand_expr(expr, span, source_id, diagnostics)),
            width: *width,
        },
    }
}
