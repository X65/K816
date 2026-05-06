use crate::ast::{
    CodeBlock, ConstDecl, DataBlock, DataEntry, DataForEvalRange, Expr, File, HlaCondition, HlaRhs,
    HlaStmt, Instruction, Item, NumFmt, Operand, Stmt, SymbolicSubscriptFieldDecl, VarDecl,
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
            comments: file.comments.clone(),
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
        Item::Const(consts) => Item::Const(
            consts
                .iter()
                .map(|const_decl| expand_const(const_decl, span, source_id, diagnostics))
                .collect(),
        ),
        Item::EvaluatorBlock(block) => Item::EvaluatorBlock(block.clone()),
        Item::Var(var) => Item::Var(expand_var(var, span, source_id, diagnostics)),
        Item::CodeBlock(block) => Item::CodeBlock(expand_code_block(block, source_id, diagnostics)),
        Item::Statement(stmt) => Item::Statement(expand_stmt(stmt, span, source_id, diagnostics)),
        Item::Segment(segment) => Item::Segment(segment.clone()),
        Item::DataBlock(block) => Item::DataBlock(expand_data_block(block, source_id, diagnostics)),
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
        has_contract: block.has_contract,
        params: block.params.clone(),
        outputs: block.outputs.clone(),
        mode_contract: block.mode_contract,
        exit_contract: block.exit_contract,
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
        Stmt::DataBlock(block) => Stmt::DataBlock(expand_data_block(block, source_id, diagnostics)),
        Stmt::Address(value) => Stmt::Address(*value),
        Stmt::Align { boundary, offset } => Stmt::Align {
            boundary: *boundary,
            offset: *offset,
        },
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
        Stmt::Hla(stmt) => Stmt::Hla(expand_hla_stmt(stmt, span, source_id, diagnostics)),
        Stmt::Empty => Stmt::Empty,
    }
}

fn expand_data_block(
    block: &DataBlock,
    source_id: SourceId,
    diagnostics: &mut Vec<Diagnostic>,
) -> DataBlock {
    let mut entries = Vec::with_capacity(block.entries.len());
    for entry in &block.entries {
        entries.push(Spanned::new(
            expand_data_entry(&entry.node, entry.span, source_id, diagnostics),
            entry.span,
        ));
    }

    DataBlock {
        name: block.name.clone(),
        name_span: block.name_span,
        entries,
    }
}

fn expand_data_entry(
    entry: &DataEntry,
    span: Span,
    source_id: SourceId,
    diagnostics: &mut Vec<Diagnostic>,
) -> DataEntry {
    match entry {
        DataEntry::Segment(segment) => DataEntry::Segment(segment.clone()),
        DataEntry::Label(name) => DataEntry::Label(name.clone()),
        DataEntry::Address(value) => DataEntry::Address(*value),
        DataEntry::Align(value) => DataEntry::Align(*value),
        DataEntry::Nocross(value) => DataEntry::Nocross(*value),
        DataEntry::Values { width, values } => DataEntry::Values {
            width: *width,
            values: values
                .iter()
                .map(|expr| expand_expr(expr, span, source_id, diagnostics))
                .collect(),
        },
        DataEntry::ForEvalRange(range) => DataEntry::ForEvalRange(DataForEvalRange {
            iterator: range.iterator.clone(),
            start: expand_expr(&range.start, span, source_id, diagnostics),
            end: expand_expr(&range.end, span, source_id, diagnostics),
            eval: range.eval.clone(),
        }),
        DataEntry::String(value) => DataEntry::String(value.clone()),
        DataEntry::Repeat { count, body } => DataEntry::Repeat {
            count: *count,
            body: body
                .iter()
                .map(|e| {
                    crate::span::Spanned::new(
                        expand_data_entry(&e.node, e.span, source_id, diagnostics),
                        e.span,
                    )
                })
                .collect(),
        },
        DataEntry::Code(stmts) => DataEntry::Code(
            stmts
                .iter()
                .map(|s| {
                    crate::span::Spanned::new(
                        expand_stmt(&s.node, s.span, source_id, diagnostics),
                        s.span,
                    )
                })
                .collect(),
        ),
        DataEntry::Evaluator(text) => DataEntry::Evaluator(text.clone()),
        DataEntry::Charset(value) => DataEntry::Charset(value.clone()),
        DataEntry::Convert { kind, args } => DataEntry::Convert {
            kind: kind.clone(),
            args: args.clone(),
        },
    }
}

fn expand_hla_stmt(
    stmt: &HlaStmt,
    span: Span,
    source_id: SourceId,
    diagnostics: &mut Vec<Diagnostic>,
) -> HlaStmt {
    match stmt {
        HlaStmt::RegisterAssign { register, rhs } => HlaStmt::RegisterAssign {
            register: *register,
            rhs: expand_hla_operand_expr(rhs, span, source_id, diagnostics),
        },
        HlaStmt::RegisterStore { dest, src } => HlaStmt::RegisterStore {
            dest: expand_hla_operand_expr(dest, span, source_id, diagnostics),
            src: *src,
        },
        HlaStmt::MemStoreZero { dest } => HlaStmt::MemStoreZero {
            dest: expand_hla_operand_expr(dest, span, source_id, diagnostics),
        },
        HlaStmt::RegisterTransfer { dest, src } => HlaStmt::RegisterTransfer {
            dest: *dest,
            src: *src,
        },
        HlaStmt::AssignmentChain { idents, tail_expr } => HlaStmt::AssignmentChain {
            idents: idents.clone(),
            tail_expr: tail_expr
                .as_ref()
                .map(|expr| expand_hla_operand_expr(expr, span, source_id, diagnostics)),
        },
        HlaStmt::AccumulatorAlu { op, rhs } => HlaStmt::AccumulatorAlu {
            op: *op,
            rhs: expand_hla_operand_expr(rhs, span, source_id, diagnostics),
        },
        HlaStmt::AccumulatorBitTest { rhs } => HlaStmt::AccumulatorBitTest {
            rhs: expand_hla_operand_expr(rhs, span, source_id, diagnostics),
        },
        HlaStmt::IndexCompare { register, rhs } => HlaStmt::IndexCompare {
            register: *register,
            rhs: expand_hla_operand_expr(rhs, span, source_id, diagnostics),
        },
        HlaStmt::IncDec { op, target } => HlaStmt::IncDec {
            op: *op,
            target: match target {
                crate::ast::HlaIncDecTarget::Register(register) => {
                    crate::ast::HlaIncDecTarget::Register(*register)
                }
                crate::ast::HlaIncDecTarget::Address(address) => {
                    crate::ast::HlaIncDecTarget::Address(expand_hla_operand_expr(
                        address,
                        span,
                        source_id,
                        diagnostics,
                    ))
                }
            },
        },
        HlaStmt::ShiftRotate { op, target } => HlaStmt::ShiftRotate {
            op: *op,
            target: match target {
                crate::ast::HlaShiftTarget::Accumulator => crate::ast::HlaShiftTarget::Accumulator,
                crate::ast::HlaShiftTarget::Address(address) => {
                    crate::ast::HlaShiftTarget::Address(expand_hla_operand_expr(
                        address,
                        span,
                        source_id,
                        diagnostics,
                    ))
                }
            },
        },
        HlaStmt::FlagSet { flag, set } => HlaStmt::FlagSet {
            flag: *flag,
            set: *set,
        },
        HlaStmt::StackOp { target, push } => HlaStmt::StackOp {
            target: *target,
            push: *push,
        },
        HlaStmt::Goto {
            target,
            indirect,
            far,
        } => HlaStmt::Goto {
            target: expand_expr(target, span, source_id, diagnostics),
            indirect: *indirect,
            far: *far,
        },
        HlaStmt::BranchGoto {
            mnemonic,
            target,
            form,
        } => HlaStmt::BranchGoto {
            mnemonic: mnemonic.clone(),
            target: expand_expr(target, span, source_id, diagnostics),
            form: *form,
        },
        HlaStmt::Return { interrupt } => HlaStmt::Return {
            interrupt: *interrupt,
        },
        HlaStmt::XAssignImmediate { rhs } => HlaStmt::XAssignImmediate {
            rhs: expand_expr(rhs, span, source_id, diagnostics),
        },
        HlaStmt::XIncrement => HlaStmt::XIncrement,
        HlaStmt::StoreFromA {
            dests,
            rhs,
            load_start,
            store_end,
        } => HlaStmt::StoreFromA {
            dests: dests.clone(),
            rhs: expand_hla_rhs(rhs, span, source_id, diagnostics),
            load_start: *load_start,
            store_end: *store_end,
        },
        HlaStmt::WaitLoopWhileNFlagClear { symbol } => HlaStmt::WaitLoopWhileNFlagClear {
            symbol: symbol.clone(),
        },
        HlaStmt::ConditionSeed { lhs, rhs } => HlaStmt::ConditionSeed {
            lhs: *lhs,
            rhs: expand_hla_operand_expr(rhs, span, source_id, diagnostics),
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
        HlaStmt::NeverBlock { body } => {
            let expanded_body = body
                .iter()
                .map(|s| {
                    crate::span::Spanned::new(
                        expand_stmt(&s.node, s.span, source_id, diagnostics),
                        s.span,
                    )
                })
                .collect();
            HlaStmt::NeverBlock {
                body: expanded_body,
            }
        }
        HlaStmt::RepeatInstruction { mnemonic, count } => HlaStmt::RepeatInstruction {
            mnemonic: mnemonic.clone(),
            count: expand_expr(count, span, source_id, diagnostics),
        },
        HlaStmt::PrefixConditional {
            skip_mnemonic,
            form,
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
                form: *form,
                body: expanded_body,
                else_body: expanded_else,
            }
        }
    }
}

fn expand_hla_operand_expr(
    expr: &crate::ast::HlaOperandExpr,
    span: Span,
    source_id: SourceId,
    diagnostics: &mut Vec<Diagnostic>,
) -> crate::ast::HlaOperandExpr {
    crate::ast::HlaOperandExpr {
        expr: expand_expr(&expr.expr, span, source_id, diagnostics),
        index: expr.index,
        addr_mode: expr.addr_mode,
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
        seed_span: condition.seed_span,
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
        is_abstract: var.is_abstract,
        data_width: var.data_width,
        addr_mode_default: var.addr_mode_default,
        array_len: var
            .array_len
            .as_ref()
            .map(|expr| expand_expr(expr, span, source_id, diagnostics)),
        symbolic_subscript_fields: var
            .symbolic_subscript_fields
            .as_ref()
            .map(|fields| expand_symbolic_subscript_fields(fields, source_id, diagnostics)),
        alloc_count: var
            .alloc_count
            .as_ref()
            .map(|expr| expand_expr(expr, span, source_id, diagnostics)),
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
            addr_mode_override,
            index,
            addr_mode,
        } => Operand::Value {
            expr: expand_expr(expr, span, source_id, diagnostics),
            addr_mode_override: *addr_mode_override,
            index: *index,
            addr_mode: *addr_mode,
        },
        Operand::Auto { expr } => Operand::Auto {
            expr: expand_expr(expr, span, source_id, diagnostics),
        },
        Operand::BlockMove { src, dst } => Operand::BlockMove {
            src: expand_expr(src, span, source_id, diagnostics),
            dst: expand_expr(dst, span, source_id, diagnostics),
        },
        Operand::Register { reg, span } => Operand::Register {
            reg: *reg,
            span: *span,
        },
    });

    Instruction {
        mnemonic: instruction.mnemonic.clone(),
        operand,
    }
}

fn expand_symbolic_subscript_fields(
    fields: &[SymbolicSubscriptFieldDecl],
    source_id: SourceId,
    diagnostics: &mut Vec<Diagnostic>,
) -> Vec<SymbolicSubscriptFieldDecl> {
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
            nested_fields: field
                .nested_fields
                .as_ref()
                .map(|nested| expand_symbolic_subscript_fields(nested, source_id, diagnostics)),
            span: field.span,
        })
        .collect()
}

fn expand_expr(
    expr: &Expr,
    _span: Span,
    source_id: SourceId,
    _diagnostics: &mut Vec<Diagnostic>,
) -> Expr {
    match expr {
        Expr::EvalText(input) => {
            let expanded = match k816_eval::expand(input) {
                Ok(expanded) => expanded,
                Err(_err) => return Expr::Number(0, NumFmt::Dec),
            };

            match parse_expression_fragment(source_id, &expanded) {
                Ok(expr) => expr.node,
                Err(_err) => Expr::Number(0, NumFmt::Dec),
            }
        }
        Expr::Number(_, _) | Expr::Ident(_) | Expr::IdentSpanned { .. } => expr.clone(),
        Expr::Index { base, index } => Expr::Index {
            base: Box::new(expand_expr(base, _span, source_id, _diagnostics)),
            index: Box::new(expand_expr(index, _span, source_id, _diagnostics)),
        },
        Expr::Binary { op, lhs, rhs } => Expr::Binary {
            op: *op,
            lhs: Box::new(expand_expr(lhs, _span, source_id, _diagnostics)),
            rhs: Box::new(expand_expr(rhs, _span, source_id, _diagnostics)),
        },
        Expr::Unary { op, expr } => Expr::Unary {
            op: *op,
            expr: Box::new(expand_expr(expr, _span, source_id, _diagnostics)),
        },
        Expr::TypedView { expr, width } => Expr::TypedView {
            expr: Box::new(expand_expr(expr, _span, source_id, _diagnostics)),
            width: *width,
        },
        Expr::MetadataQuery { expr, query } => Expr::MetadataQuery {
            expr: Box::new(expand_expr(expr, _span, source_id, _diagnostics)),
            query: *query,
        },
    }
}
