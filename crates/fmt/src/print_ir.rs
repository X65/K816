use k816_core::hir::{AddressValue, ByteRelocationKind, Op, OperandOp, Program};

pub fn format_ir(program: &Program) -> String {
    let mut out = String::new();
    for op in &program.ops {
        match &op.node {
            Op::SelectSegment(name) => out.push_str(&format!("segment {name}\n")),
            Op::FunctionStart(name) => out.push_str(&format!("function_start {name}\n")),
            Op::FunctionEnd => out.push_str("function_end\n"),
            Op::Label(name) => out.push_str(&format!("label {name}\n")),
            Op::Align(value) => out.push_str(&format!("align {value}\n")),
            Op::Address(value) => out.push_str(&format!("address {value}\n")),
            Op::Nocross(value) => out.push_str(&format!("nocross {value}\n")),
            Op::EmitBytes(bytes) => {
                let data = bytes
                    .iter()
                    .map(|byte| format!("{byte:02X}"))
                    .collect::<Vec<_>>()
                    .join(" ");
                out.push_str(&format!("emit {data}\n"));
            }
            Op::EmitRelocBytes { bytes, relocations } => {
                let data = bytes
                    .iter()
                    .map(|byte| format!("{byte:02X}"))
                    .collect::<Vec<_>>()
                    .join(" ");
                out.push_str(&format!("emit {data}\n"));
                for relocation in relocations {
                    let kind = match relocation.kind {
                        ByteRelocationKind::LowByte => "lo",
                        ByteRelocationKind::HighByte => "hi",
                    };
                    out.push_str(&format!(
                        "  reloc {kind} +{} {}\n",
                        relocation.offset, relocation.label
                    ));
                }
            }
            Op::Instruction(instr) => {
                out.push_str(&instr.mnemonic);
                if let Some(operand) = &instr.operand {
                    out.push(' ');
                    match operand {
                        OperandOp::Immediate(value) => out.push_str(&format!("#{value}")),
                        OperandOp::Address {
                            value,
                            force_far,
                            index_x,
                        } => {
                            if *force_far {
                                out.push_str("far ");
                            }
                            match value {
                                AddressValue::Literal(value) => out.push_str(&value.to_string()),
                                AddressValue::Label(name) => out.push_str(name),
                            }
                            if *index_x {
                                out.push_str(",x");
                            }
                        }
                    }
                }
                out.push('\n');
            }
        }
    }
    out
}
