use crate::ast::{Expr, File, HlaCondition, HlaStmt, Item, NamedDataBlock, NamedDataEntry, Stmt};
use crate::diag::Diagnostic;
use crate::span::Spanned;

pub fn normalize_file(file: &File) -> Result<File, Vec<Diagnostic>> {
    let mut items = Vec::with_capacity(file.items.len());
    for item in &file.items {
        items.push(Spanned::new(normalize_item(&item.node), item.span));
    }

    Ok(File { items })
}

fn normalize_item(item: &Item) -> Item {
    match item {
        Item::Segment(segment) => Item::Segment(segment.clone()),
        Item::Var(var) => Item::Var(var.clone()),
        Item::DataBlock(block) => Item::DataBlock(block.clone()),
        Item::NamedDataBlock(block) => Item::NamedDataBlock(normalize_named_data_block(block)),
        Item::CodeBlock(block) => {
            let body = normalize_stmt_sequence(&block.body);
            Item::CodeBlock(crate::ast::CodeBlock {
                name: block.name.clone(),
                name_span: block.name_span,
                kind: block.kind,
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
        NamedDataEntry::Address(value) => NamedDataEntry::Address(*value),
        NamedDataEntry::Align(value) => NamedDataEntry::Align(*value),
        NamedDataEntry::Nocross(value) => NamedDataEntry::Nocross(*value),
        NamedDataEntry::Bytes(values) => NamedDataEntry::Bytes(values.clone()),
        NamedDataEntry::String(value) => NamedDataEntry::String(value.clone()),
        NamedDataEntry::Convert { kind, args } => NamedDataEntry::Convert {
            kind: kind.clone(),
            args: args.clone(),
        },
        NamedDataEntry::Ignored => NamedDataEntry::Ignored,
    }
}

fn normalize_stmt(stmt: &Stmt) -> Stmt {
    match stmt {
        Stmt::Segment(segment) => Stmt::Segment(segment.clone()),
        Stmt::Label(label) => Stmt::Label(label.clone()),
        Stmt::Var(var) => Stmt::Var(var.clone()),
        Stmt::DataBlock(block) => Stmt::DataBlock(block.clone()),
        Stmt::Address(value) => Stmt::Address(*value),
        Stmt::Align(value) => Stmt::Align(*value),
        Stmt::Nocross(value) => Stmt::Nocross(*value),
        Stmt::Instruction(instruction) => Stmt::Instruction(instruction.clone()),
        Stmt::Call(call) => Stmt::Call(call.clone()),
        Stmt::Bytes(values) => Stmt::Bytes(values.clone()),
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
                if let Stmt::Hla(HlaStmt::DoCloseWithOp { op }) = &next.node {
                    out.push(Spanned::new(
                        Stmt::Hla(HlaStmt::DoClose {
                            condition: normalize_condition(&HlaCondition {
                                lhs: *lhs,
                                op: *op,
                                rhs: Some(rhs.clone()),
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
        HlaStmt::XAssignImmediate { rhs } => HlaStmt::XAssignImmediate { rhs: rhs.clone() },
        HlaStmt::XIncrement => HlaStmt::XIncrement,
        HlaStmt::StoreFromA { dest, rhs } => HlaStmt::StoreFromA {
            dest: dest.clone(),
            rhs: rhs.clone(),
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
        HlaStmt::RepeatNop(n) => HlaStmt::RepeatNop(*n),
        HlaStmt::PrefixConditional {
            skip_mnemonic,
            body,
        } => HlaStmt::PrefixConditional {
            skip_mnemonic: skip_mnemonic.clone(),
            body: normalize_stmt_sequence(body),
        },
    }
}

fn normalize_condition(condition: &HlaCondition) -> HlaCondition {
    HlaCondition {
        lhs: condition.lhs,
        op: condition.op,
        rhs: Some(condition.rhs.clone().unwrap_or(Expr::Number(0))),
    }
}
