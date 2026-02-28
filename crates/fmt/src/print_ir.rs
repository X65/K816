use k816_core::hir::{
    AddressOperandMode, AddressSizeHint, AddressValue, ByteRelocationKind, IndexRegister, Op,
    OperandOp, Program,
};

pub fn format_ir(program: &Program) -> String {
    let mut out = String::new();
    for op in &program.ops {
        match &op.node {
            Op::SelectSegment(name) => out.push_str(&format!("segment {name}\n")),
            Op::FunctionStart { name, .. } => out.push_str(&format!("function_start {name}\n")),
            Op::FunctionEnd => out.push_str("function_end\n"),
            Op::Label(name) => out.push_str(&format!("label {name}\n")),
            Op::Align { boundary, offset } => {
                if *offset == 0 {
                    out.push_str(&format!("align {boundary}\n"));
                } else {
                    out.push_str(&format!("align {boundary} + {offset}\n"));
                }
            }
            Op::Address(value) => out.push_str(&format!("address {value}\n")),
            Op::Nocross(value) => out.push_str(&format!("nocross {value}\n")),
            Op::Rep(mask) => out.push_str(&format!("rep #{mask:#04X}\n")),
            Op::Sep(mask) => out.push_str(&format!("sep #{mask:#04X}\n")),
            Op::FixedRep(mask) => out.push_str(&format!("fixed_rep #{mask:#04X}\n")),
            Op::FixedSep(mask) => out.push_str(&format!("fixed_sep #{mask:#04X}\n")),
            Op::DefineAbsoluteSymbol { name, address } => {
                out.push_str(&format!("abs_symbol {name} {address:#X}\n"));
            }
            Op::SetMode(_) => {
                out.push_str("set_mode\n");
            }
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
                        ByteRelocationKind::FullWord => "word",
                        ByteRelocationKind::FullLong => "far",
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
                        OperandOp::ImmediateByteRelocation { kind, label } => {
                            let prefix = match kind {
                                ByteRelocationKind::LowByte => "&<",
                                ByteRelocationKind::HighByte => "&>",
                                ByteRelocationKind::FullWord => "&&",
                                ByteRelocationKind::FullLong => "&&&",
                            };
                            out.push('#');
                            out.push_str(prefix);
                            out.push_str(label);
                        }
                        OperandOp::Address {
                            value,
                            size_hint,
                            mode,
                        } => {
                            let base = match value {
                                AddressValue::Literal(value) => value.to_string(),
                                AddressValue::Label(name) => name.clone(),
                                AddressValue::LabelOffset { label, addend } => {
                                    if *addend >= 0 {
                                        format!("{label}+{addend}")
                                    } else {
                                        format!("{label}{addend}")
                                    }
                                }
                            };
                            if *size_hint == AddressSizeHint::ForceAbsoluteLong {
                                out.push_str("far ");
                            }
                            let base = if *size_hint == AddressSizeHint::ForceAbsolute16 {
                                format!("{base}:abs")
                            } else {
                                base
                            };
                            let rendered = match mode {
                                AddressOperandMode::Direct {
                                    index: Some(IndexRegister::X),
                                } => format!("{base},x"),
                                AddressOperandMode::Direct {
                                    index: Some(IndexRegister::Y),
                                } => format!("{base},y"),
                                AddressOperandMode::Direct { index: None } => base,
                                AddressOperandMode::Indirect => format!("({base})"),
                                AddressOperandMode::IndexedIndirectX => format!("({base},x)"),
                                AddressOperandMode::IndirectIndexedY => format!("({base}),y"),
                            };
                            out.push_str(&rendered);
                        }
                        OperandOp::BlockMove { src, dst } => {
                            out.push_str(&format!("{src},{dst}"));
                        }
                    }
                }
                out.push('\n');
            }
        }
    }
    out
}
