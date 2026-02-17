use crate::ast::{
    Expr, File, HlaCondition, HlaStmt, Item, NamedDataBlock, NamedDataEntry, NumFmt, Stmt,
};
use crate::diag::Diagnostic;
use crate::span::Spanned;

pub fn normalize_file(file: &File) -> Result<File, Vec<Diagnostic>> {
    let mut items = Vec::with_capacity(file.items.len());
    for item in &file.items {
        items.push(Spanned::new(normalize_item(&item.node), item.span));
    }

    Ok(File {
        mode_default: file.mode_default,
        items,
        comments: file.comments.clone(),
    })
}

fn normalize_item(item: &Item) -> Item {
    match item {
        Item::Segment(segment) => Item::Segment(segment.clone()),
        Item::Const(const_decl) => Item::Const(const_decl.clone()),
        Item::EvaluatorBlock(block) => Item::EvaluatorBlock(block.clone()),
        Item::Var(var) => Item::Var(var.clone()),
        Item::DataBlock(block) => Item::DataBlock(block.clone()),
        Item::NamedDataBlock(block) => Item::NamedDataBlock(normalize_named_data_block(block)),
        Item::CodeBlock(block) => {
            let body = normalize_stmt_sequence(&block.body);
            Item::CodeBlock(crate::ast::CodeBlock {
                name: block.name.clone(),
                name_span: block.name_span,
                is_far: block.is_far,
                is_naked: block.is_naked,
                is_inline: block.is_inline,
                mode_contract: block.mode_contract,
                body,
            })
        }
        Item::Statement(stmt) => Item::Statement(normalize_stmt(stmt)),
    }
}

fn normalize_named_data_block(block: &NamedDataBlock) -> NamedDataBlock {
    let mut entries = Vec::with_capacity(block.entries.len());
    for entry in &block.entries {
        entries.push(Spanned::new(
            normalize_named_data_entry(&entry.node),
            entry.span,
        ));
    }

    NamedDataBlock {
        name: block.name.clone(),
        name_span: block.name_span,
        entries,
    }
}

fn normalize_named_data_entry(entry: &NamedDataEntry) -> NamedDataEntry {
    match entry {
        NamedDataEntry::Segment(segment) => NamedDataEntry::Segment(segment.clone()),
        NamedDataEntry::Label(name) => NamedDataEntry::Label(name.clone()),
        NamedDataEntry::Address(value) => NamedDataEntry::Address(*value),
        NamedDataEntry::Align(value) => NamedDataEntry::Align(*value),
        NamedDataEntry::Nocross(value) => NamedDataEntry::Nocross(*value),
        NamedDataEntry::Bytes(values) => NamedDataEntry::Bytes(values.clone()),
        NamedDataEntry::Words(values) => NamedDataEntry::Words(values.clone()),
        NamedDataEntry::Fars(values) => NamedDataEntry::Fars(values.clone()),
        NamedDataEntry::ForEvalRange(range) => NamedDataEntry::ForEvalRange(range.clone()),
        NamedDataEntry::String(value) => NamedDataEntry::String(value.clone()),
        NamedDataEntry::Repeat { count, body } => NamedDataEntry::Repeat {
            count: *count,
            body: body
                .iter()
                .map(|e| crate::span::Spanned::new(normalize_named_data_entry(&e.node), e.span))
                .collect(),
        },
        NamedDataEntry::Code(stmts) => NamedDataEntry::Code(normalize_stmt_sequence(stmts)),
        NamedDataEntry::Evaluator(text) => NamedDataEntry::Evaluator(text.clone()),
        NamedDataEntry::Charset(value) => NamedDataEntry::Charset(value.clone()),
    }
}

fn normalize_stmt(stmt: &Stmt) -> Stmt {
    match stmt {
        Stmt::Segment(segment) => Stmt::Segment(segment.clone()),
        Stmt::Label(label) => Stmt::Label(label.clone()),
        Stmt::Var(var) => Stmt::Var(var.clone()),
        Stmt::DataBlock(block) => Stmt::DataBlock(block.clone()),
        Stmt::Address(value) => Stmt::Address(*value),
        Stmt::Align { boundary, offset } => Stmt::Align {
            boundary: *boundary,
            offset: *offset,
        },
        Stmt::Nocross(value) => Stmt::Nocross(*value),
        Stmt::Instruction(instruction) => Stmt::Instruction(instruction.clone()),
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
            body: normalize_stmt_sequence(body),
        },
        Stmt::SwapAB => Stmt::SwapAB,
        Stmt::Empty => Stmt::Empty,
        Stmt::Hla(stmt) => Stmt::Hla(normalize_hla_stmt(stmt)),
    }
}

fn normalize_stmt_sequence(stmts: &[Spanned<Stmt>]) -> Vec<Spanned<Stmt>> {
    let mut out = Vec::with_capacity(stmts.len());
    let mut index = 0usize;

    while index < stmts.len() {
        let current = &stmts[index];
        if let Stmt::Hla(HlaStmt::ConditionSeed { lhs, rhs }) = &current.node {
            if let Some(next) = stmts.get(index + 1) {
                if rhs.index.is_none()
                    && rhs.addr_mode == crate::ast::OperandAddrMode::Direct
                    && let Stmt::Hla(HlaStmt::DoCloseWithOp { op }) = &next.node
                {
                    out.push(Spanned::new(
                        Stmt::Hla(HlaStmt::DoClose {
                            condition: normalize_condition(&HlaCondition {
                                lhs: *lhs,
                                op: *op,
                                rhs: Some(rhs.expr.clone()),
                                seed_span: Some(current.span),
                            }),
                        }),
                        next.span,
                    ));
                    index += 2;
                    continue;
                }
            }
        }

        out.push(Spanned::new(normalize_stmt(&current.node), current.span));
        index += 1;
    }

    out
}

fn normalize_hla_stmt(stmt: &HlaStmt) -> HlaStmt {
    match stmt {
        HlaStmt::RegisterAssign { register, rhs } => HlaStmt::RegisterAssign {
            register: *register,
            rhs: rhs.clone(),
        },
        HlaStmt::RegisterStore { dest, src } => HlaStmt::RegisterStore {
            dest: dest.clone(),
            src: *src,
        },
        HlaStmt::RegisterTransfer { dest, src } => HlaStmt::RegisterTransfer {
            dest: *dest,
            src: *src,
        },
        HlaStmt::AssignmentChain { idents, tail_expr } => HlaStmt::AssignmentChain {
            idents: idents.clone(),
            tail_expr: tail_expr.clone(),
        },
        HlaStmt::AccumulatorAlu { op, rhs } => HlaStmt::AccumulatorAlu {
            op: *op,
            rhs: rhs.clone(),
        },
        HlaStmt::AccumulatorBitTest { rhs } => HlaStmt::AccumulatorBitTest { rhs: rhs.clone() },
        HlaStmt::IndexCompare { register, rhs } => HlaStmt::IndexCompare {
            register: *register,
            rhs: rhs.clone(),
        },
        HlaStmt::IncDec { op, target } => HlaStmt::IncDec {
            op: *op,
            target: target.clone(),
        },
        HlaStmt::ShiftRotate { op, target } => HlaStmt::ShiftRotate {
            op: *op,
            target: target.clone(),
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
            target: target.clone(),
            indirect: *indirect,
            far: *far,
        },
        HlaStmt::BranchGoto { mnemonic, target } => HlaStmt::BranchGoto {
            mnemonic: mnemonic.clone(),
            target: target.clone(),
        },
        HlaStmt::Return { interrupt } => HlaStmt::Return {
            interrupt: *interrupt,
        },
        HlaStmt::XAssignImmediate { rhs } => HlaStmt::XAssignImmediate { rhs: rhs.clone() },
        HlaStmt::XIncrement => HlaStmt::XIncrement,
        HlaStmt::StoreFromA {
            dests,
            rhs,
            load_start,
            store_end,
        } => HlaStmt::StoreFromA {
            dests: dests.clone(),
            rhs: rhs.clone(),
            load_start: *load_start,
            store_end: *store_end,
        },
        HlaStmt::WaitLoopWhileNFlagClear { symbol } => HlaStmt::WaitLoopWhileNFlagClear {
            symbol: symbol.clone(),
        },
        HlaStmt::ConditionSeed { lhs, rhs } => HlaStmt::ConditionSeed {
            lhs: *lhs,
            rhs: rhs.clone(),
        },
        HlaStmt::DoOpen => HlaStmt::DoOpen,
        HlaStmt::DoCloseNFlagClear => HlaStmt::DoCloseNFlagClear,
        HlaStmt::DoCloseNFlagSet => HlaStmt::DoCloseNFlagSet,
        HlaStmt::DoCloseWithOp { op } => HlaStmt::DoCloseWithOp { op: *op },
        HlaStmt::DoClose { condition } => HlaStmt::DoClose {
            condition: normalize_condition(condition),
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
        HlaStmt::NeverBlock { body } => HlaStmt::NeverBlock {
            body: normalize_stmt_sequence(body),
        },
        HlaStmt::RepeatNop(n) => HlaStmt::RepeatNop(*n),
        HlaStmt::PrefixConditional {
            skip_mnemonic,
            body,
            else_body,
        } => HlaStmt::PrefixConditional {
            skip_mnemonic: skip_mnemonic.clone(),
            body: normalize_stmt_sequence(body),
            else_body: else_body
                .as_ref()
                .map(|stmts| normalize_stmt_sequence(stmts)),
        },
    }
}

fn normalize_condition(condition: &HlaCondition) -> HlaCondition {
    HlaCondition {
        lhs: condition.lhs,
        op: condition.op,
        rhs: Some(condition.rhs.clone().unwrap_or(Expr::Number(0, NumFmt::Dec))),
        seed_span: condition.seed_span,
    }
}
