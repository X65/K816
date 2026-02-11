use k816_core::ast::{
    BlockKind, DataArg, DataCommand, Expr, File, HlaCompareOp, HlaCondition, HlaRhs, HlaStmt,
    Instruction, Item, NamedDataEntry, Operand, OperandAddrMode, Stmt,
};

pub fn format_ast(file: &File) -> String {
    let mut out = String::new();
    for item in &file.items {
        format_item(&mut out, &item.node, 0);
    }
    out
}

fn format_item(out: &mut String, item: &Item, indent: usize) {
    match item {
        Item::Segment(segment) => line(out, indent, format!("segment {}", segment.name)),
        Item::Var(var) => line(out, indent, format_var(var)),
        Item::DataBlock(block) => {
            line(out, indent, "data {".to_string());
            for command in &block.commands {
                line(out, indent + 2, format_data_command(&command.node));
            }
            line(out, indent, "}".to_string());
        }
        Item::NamedDataBlock(block) => {
            line(out, indent, format!("data {} {{", block.name));
            for entry in &block.entries {
                line(out, indent + 2, format_named_data_entry(&entry.node));
            }
            line(out, indent, "}".to_string());
        }
        Item::CodeBlock(block) => {
            let mut header = String::new();
            if block.is_far {
                header.push_str("far ");
            }
            if block.is_naked {
                header.push_str("naked ");
            }
            if block.is_inline {
                header.push_str("inline ");
            }
            header.push_str(match block.kind {
                BlockKind::Main => "main",
                BlockKind::Func => "func",
            });
            if matches!(block.kind, BlockKind::Func) {
                header.push(' ');
                header.push_str(&block.name);
            }
            header.push_str(" {");
            line(out, indent, header);

            for stmt in &block.body {
                format_stmt(out, &stmt.node, indent + 2);
            }

            line(out, indent, "}".to_string());
        }
        Item::Statement(stmt) => format_stmt(out, stmt, indent),
    }
}

fn format_stmt(out: &mut String, stmt: &Stmt, indent: usize) {
    match stmt {
        Stmt::Segment(segment) => line(out, indent, format!("segment {}", segment.name)),
        Stmt::Label(label) => line(out, indent, format!("{}:", label.name)),
        Stmt::Var(var) => line(out, indent, format_var(var)),
        Stmt::DataBlock(block) => {
            line(out, indent, "data {".to_string());
            for command in &block.commands {
                line(out, indent + 2, format_data_command(&command.node));
            }
            line(out, indent, "}".to_string());
        }
        Stmt::Address(value) => line(out, indent, format!("address {value}")),
        Stmt::Align(value) => line(out, indent, format!("align {value}")),
        Stmt::Nocross(value) => line(out, indent, format!("nocross {value}")),
        Stmt::Instruction(instr) => line(out, indent, format_instruction(instr)),
        Stmt::Call(call) => line(out, indent, format!("call {}", call.target)),
        Stmt::Bytes(values) => {
            let values = values
                .iter()
                .map(format_expr)
                .collect::<Vec<_>>()
                .join(", ");
            line(out, indent, format!(".byte {values}"));
        }
        Stmt::Hla(stmt) => line(out, indent, format_hla_stmt(stmt)),
        Stmt::Empty => line(out, indent, String::new()),
    }
}

fn format_instruction(instr: &Instruction) -> String {
    match &instr.operand {
        None => instr.mnemonic.clone(),
        Some(Operand::Immediate(expr)) => format!("{} #{}", instr.mnemonic, format_expr(expr)),
        Some(Operand::Value {
            expr,
            force_far,
            index,
            addr_mode,
        }) => {
            let value = format_address_operand(expr, *index, *addr_mode);
            if *force_far {
                format!("{} far {}", instr.mnemonic, value)
            } else {
                format!("{} {}", instr.mnemonic, value)
            }
        }
    }
}

fn format_var(var: &k816_core::ast::VarDecl) -> String {
    let mut out = format!("var {}", var.name);
    if let Some(array_len) = &var.array_len {
        out.push('[');
        out.push_str(&format_expr(array_len));
        out.push(']');
    }
    if let Some(expr) = &var.initializer {
        out.push_str(" = ");
        out.push_str(&format_expr(expr));
    }
    out
}

fn format_data_command(command: &DataCommand) -> String {
    match command {
        DataCommand::Align(value) => format!("align {value}"),
        DataCommand::Address(value) => format!("address {value}"),
        DataCommand::Nocross(value) => format!("nocross {value}"),
        DataCommand::Convert { kind, args } => {
            let args = args
                .iter()
                .map(format_data_arg)
                .collect::<Vec<_>>()
                .join(", ");
            format!("{kind}({args})")
        }
        DataCommand::Ignored => "<ignored>".to_string(),
    }
}

fn format_data_arg(arg: &DataArg) -> String {
    match arg {
        DataArg::Int(value) => value.to_string(),
        DataArg::Str(value) => format!("\"{}\"", value.replace('"', "\\\"")),
    }
}

fn format_expr(expr: &Expr) -> String {
    match expr {
        Expr::Number(value) => value.to_string(),
        Expr::Ident(value) => value.clone(),
        Expr::EvalText(value) => format!("[{value}]"),
        Expr::Binary { op, lhs, rhs } => {
            let op = match op {
                k816_core::ast::ExprBinaryOp::Add => "+",
                k816_core::ast::ExprBinaryOp::Sub => "-",
            };
            format!("{} {op} {}", format_expr(lhs), format_expr(rhs))
        }
        Expr::Unary { op, expr } => {
            let op = match op {
                k816_core::ast::ExprUnaryOp::LowByte => "&<",
                k816_core::ast::ExprUnaryOp::HighByte => "&>",
            };
            format!("{op}{}", format_expr(expr))
        }
    }
}

fn format_named_data_entry(entry: &NamedDataEntry) -> String {
    match entry {
        NamedDataEntry::Segment(segment) => format!("segment {}", segment.name),
        NamedDataEntry::Address(value) => format!("address {value}"),
        NamedDataEntry::Align(value) => format!("align {value}"),
        NamedDataEntry::Nocross(value) => format!("nocross {value}"),
        NamedDataEntry::Bytes(values) => {
            values.iter().map(format_expr).collect::<Vec<_>>().join(" ")
        }
        NamedDataEntry::String(value) => format!("\"{}\"", value.replace('\"', "\\\"")),
        NamedDataEntry::Convert { kind, args } => {
            let args = args
                .iter()
                .map(format_data_arg)
                .collect::<Vec<_>>()
                .join(", ");
            format!("{kind}({args})")
        }
        NamedDataEntry::Ignored => "<ignored>".to_string(),
    }
}

fn format_hla_stmt(stmt: &HlaStmt) -> String {
    match stmt {
        HlaStmt::XAssignImmediate { rhs } => format!("x = #{}", format_expr(rhs)),
        HlaStmt::XIncrement => "x++".to_string(),
        HlaStmt::StoreFromA { dest, rhs } => format!("{dest} = a = {}", format_hla_rhs(rhs)),
        HlaStmt::WaitLoopWhileNFlagClear { symbol } => format!("{{ a&?{symbol} }} n-?"),
        HlaStmt::ConditionSeed { lhs, rhs } => {
            let lhs = match lhs {
                k816_core::ast::HlaRegister::A => "a",
            };
            format!("{lhs}?{}", format_expr(rhs))
        }
        HlaStmt::DoOpen => "{".to_string(),
        HlaStmt::DoCloseNFlagClear => "} n-?".to_string(),
        HlaStmt::DoCloseNFlagSet => "} n+?".to_string(),
        HlaStmt::DoCloseWithOp { op } => format!("}} {}", format_hla_op(*op)),
        HlaStmt::DoClose { condition } => format!("}} {}", format_hla_condition(condition)),
        HlaStmt::DoCloseAlways => "} always".to_string(),
        HlaStmt::DoCloseNever => "} never".to_string(),
    }
}

fn format_hla_rhs(rhs: &HlaRhs) -> String {
    match rhs {
        HlaRhs::Immediate(expr) => format!("#{}", format_expr(expr)),
        HlaRhs::Value {
            expr,
            index,
            addr_mode,
        } => format_address_operand(expr, *index, *addr_mode),
    }
}

fn format_hla_condition(condition: &HlaCondition) -> String {
    let lhs = match condition.lhs {
        k816_core::ast::HlaRegister::A => "a",
    };
    let op = format_hla_op(condition.op);
    match &condition.rhs {
        Some(rhs) => format!("{lhs}?{} {op}", format_expr(rhs)),
        None => format!("{lhs}?{op}"),
    }
}

fn format_hla_op(op: HlaCompareOp) -> &'static str {
    match op {
        HlaCompareOp::Eq => "==",
        HlaCompareOp::Ne => "!=",
        HlaCompareOp::Lt => "<",
        HlaCompareOp::Le => "<=",
        HlaCompareOp::Gt => ">",
        HlaCompareOp::Ge => ">=",
    }
}

fn line(out: &mut String, indent: usize, text: String) {
    out.push_str(&" ".repeat(indent));
    out.push_str(&text);
    out.push('\n');
}

fn format_address_operand(
    expr: &Expr,
    index: Option<k816_core::ast::IndexRegister>,
    addr_mode: OperandAddrMode,
) -> String {
    let expr = format_expr(expr);
    match addr_mode {
        OperandAddrMode::Direct => match index {
            None => expr,
            Some(k816_core::ast::IndexRegister::X) => format!("{expr},x"),
            Some(k816_core::ast::IndexRegister::Y) => format!("{expr},y"),
        },
        OperandAddrMode::Indirect => format!("({expr})"),
        OperandAddrMode::IndexedIndirectX => format!("({expr},x)"),
        OperandAddrMode::IndirectIndexedY => format!("({expr}),y"),
    }
}
