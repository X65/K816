use k816_core::ast::{
    BlockKind, DataArg, DataCommand, Expr, File, Instruction, Item, Operand, Stmt,
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
        Item::Bank(bank) => line(out, indent, format!("bank {}", bank.name)),
        Item::Var(var) => {
            if let Some(expr) = &var.initializer {
                line(
                    out,
                    indent,
                    format!("var {} = {}", var.name, format_expr(expr)),
                );
            } else {
                line(out, indent, format!("var {}", var.name));
            }
        }
        Item::DataBlock(block) => {
            line(out, indent, "data {".to_string());
            for command in &block.commands {
                line(out, indent + 2, format_data_command(&command.node));
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
        Stmt::Label(label) => line(out, indent, format!("{}:", label.name)),
        Stmt::Var(var) => {
            if let Some(expr) = &var.initializer {
                line(
                    out,
                    indent,
                    format!("var {} = {}", var.name, format_expr(expr)),
                );
            } else {
                line(out, indent, format!("var {}", var.name));
            }
        }
        Stmt::DataBlock(block) => {
            line(out, indent, "data {".to_string());
            for command in &block.commands {
                line(out, indent + 2, format_data_command(&command.node));
            }
            line(out, indent, "}".to_string());
        }
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
        Stmt::Empty => line(out, indent, String::new()),
    }
}

fn format_instruction(instr: &Instruction) -> String {
    match &instr.operand {
        None => instr.mnemonic.clone(),
        Some(Operand::Immediate(expr)) => format!("{} #{}", instr.mnemonic, format_expr(expr)),
        Some(Operand::Value { expr, force_far }) => {
            if *force_far {
                format!("{} far {}", instr.mnemonic, format_expr(expr))
            } else {
                format!("{} {}", instr.mnemonic, format_expr(expr))
            }
        }
    }
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
    }
}

fn line(out: &mut String, indent: usize, text: String) {
    out.push_str(&" ".repeat(indent));
    out.push_str(&text);
    out.push('\n');
}
