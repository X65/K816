use indexmap::IndexMap;
use rustc_hash::FxHashMap;

use crate::diag::Diagnostic;
use crate::hir::{AddressValue, Op, OperandOp, Program};
use crate::isa65816::{OperandShape, operand_width, select_encoding};
use crate::span::Span;

#[derive(Debug, Clone)]
pub struct EmitOutput {
    pub banks: IndexMap<String, Vec<u8>>,
    pub listing: String,
}

#[derive(Debug)]
struct BankState {
    bytes: Vec<u8>,
    nocross_boundary: Option<u16>,
}

#[derive(Debug)]
struct Fixup {
    bank: String,
    offset: usize,
    width: usize,
    label: String,
    span: Span,
}

pub fn emit(program: &Program) -> Result<EmitOutput, Vec<Diagnostic>> {
    let mut diagnostics = Vec::new();
    let mut banks: IndexMap<String, BankState> = IndexMap::new();
    let mut labels: FxHashMap<String, (String, usize)> = FxHashMap::default();
    let mut fixups = Vec::new();
    let mut current_bank = "default".to_string();
    banks.entry(current_bank.clone()).or_insert(BankState {
        bytes: Vec::new(),
        nocross_boundary: None,
    });

    for op in &program.ops {
        match &op.node {
            Op::SelectBank(name) => {
                current_bank = name.clone();
                banks.entry(current_bank.clone()).or_insert(BankState {
                    bytes: Vec::new(),
                    nocross_boundary: None,
                });
            }
            Op::Label(name) => {
                let bank = banks
                    .get(&current_bank)
                    .expect("current bank must exist during emit");
                if labels
                    .insert(name.clone(), (current_bank.clone(), bank.bytes.len()))
                    .is_some()
                {
                    diagnostics.push(
                        Diagnostic::error(op.span, format!("duplicate label '{name}'"))
                            .with_hint("rename one of the labels"),
                    );
                }
            }
            Op::Align(align) => {
                if *align == 0 {
                    diagnostics.push(Diagnostic::error(op.span, "align value must be non-zero"));
                    continue;
                }
                let bank = banks
                    .get_mut(&current_bank)
                    .expect("current bank must exist during emit");
                while bank.bytes.len() % usize::from(*align) != 0 {
                    bank.bytes.push(0);
                }
            }
            Op::Address(address) => {
                let bank = banks
                    .get_mut(&current_bank)
                    .expect("current bank must exist during emit");
                let address = *address as usize;
                if bank.bytes.len() > address {
                    diagnostics.push(Diagnostic::error(
                        op.span,
                        format!(
                            "address {address:#X} is behind current position {:#X}",
                            bank.bytes.len()
                        ),
                    ));
                    continue;
                }
                while bank.bytes.len() < address {
                    bank.bytes.push(0);
                }
            }
            Op::Nocross(boundary) => {
                let bank = banks
                    .get_mut(&current_bank)
                    .expect("current bank must exist during emit");
                bank.nocross_boundary = Some(*boundary);
            }
            Op::EmitBytes(bytes) => {
                let bank = banks
                    .get_mut(&current_bank)
                    .expect("current bank must exist during emit");
                apply_nocross_if_needed(bank, bytes.len(), op.span, &mut diagnostics);
                bank.bytes.extend(bytes);
            }
            Op::Instruction(instruction) => {
                let bank = banks
                    .get_mut(&current_bank)
                    .expect("current bank must exist during emit");
                let operand_shape = match &instruction.operand {
                    None => OperandShape::None,
                    Some(OperandOp::Immediate(value)) => OperandShape::Immediate(*value),
                    Some(OperandOp::Address { value, force_far }) => match value {
                        AddressValue::Literal(literal) => OperandShape::Address {
                            literal: Some(*literal),
                            force_far: *force_far,
                        },
                        AddressValue::Label(_) => OperandShape::Address {
                            literal: None,
                            force_far: *force_far,
                        },
                    },
                };

                let encoding = match select_encoding(&instruction.mnemonic, operand_shape) {
                    Ok(encoding) => encoding,
                    Err(err) => {
                        diagnostics.push(Diagnostic::error(op.span, err.to_string()));
                        continue;
                    }
                };

                let width = operand_width(encoding.mode);
                apply_nocross_if_needed(bank, 1 + width, op.span, &mut diagnostics);
                bank.bytes.push(encoding.opcode);
                let operand_offset = bank.bytes.len();

                match &instruction.operand {
                    None => {}
                    Some(OperandOp::Immediate(value)) => {
                        bank.bytes.push(*value as u8);
                    }
                    Some(OperandOp::Address { value, .. }) => match value {
                        AddressValue::Literal(literal) => write_literal(
                            &mut bank.bytes,
                            *literal,
                            width,
                            op.span,
                            &mut diagnostics,
                        ),
                        AddressValue::Label(label) => {
                            bank.bytes.resize(bank.bytes.len() + width, 0);
                            fixups.push(Fixup {
                                bank: current_bank.clone(),
                                offset: operand_offset,
                                width,
                                label: label.clone(),
                                span: op.span,
                            });
                        }
                    },
                }
            }
        }
    }

    for fixup in fixups {
        let Some((label_bank, label_addr)) = labels.get(&fixup.label) else {
            diagnostics.push(Diagnostic::error(
                fixup.span,
                format!("undefined label '{}'", fixup.label),
            ));
            continue;
        };

        if fixup.width == 2 && label_bank != &fixup.bank {
            diagnostics.push(
                Diagnostic::error(
                    fixup.span,
                    format!(
                        "label '{}' is in bank '{label_bank}', but absolute reference is in bank '{}'",
                        fixup.label, fixup.bank
                    ),
                )
                .with_hint("use far function calls or far operand to cross banks"),
            );
            continue;
        }

        let value = *label_addr as u32;
        let bank = banks
            .get_mut(&fixup.bank)
            .expect("fixup bank should exist during patching");
        write_literal_at(
            &mut bank.bytes,
            fixup.offset,
            value,
            fixup.width,
            fixup.span,
            &mut diagnostics,
        );
    }

    if !diagnostics.is_empty() {
        return Err(diagnostics);
    }

    let skip_default_empty = banks.len() > 1
        && banks
            .get("default")
            .is_some_and(|state| state.bytes.is_empty());

    let mut listing_blocks = Vec::new();
    let mut output_banks = IndexMap::new();
    for (bank_name, state) in &banks {
        if skip_default_empty && bank_name == "default" {
            continue;
        }
        listing_blocks.push(format_listing_block(bank_name, &state.bytes));
        output_banks.insert(bank_name.clone(), state.bytes.clone());
    }

    Ok(EmitOutput {
        banks: output_banks,
        listing: listing_blocks.join("\n\n"),
    })
}

fn apply_nocross_if_needed(
    bank: &mut BankState,
    next_size: usize,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let Some(boundary) = bank.nocross_boundary.take() else {
        return;
    };
    let boundary = usize::from(boundary);
    if boundary == 0 {
        diagnostics.push(Diagnostic::error(span, "nocross value must be non-zero"));
        return;
    }
    if next_size > boundary {
        diagnostics.push(Diagnostic::error(
            span,
            format!("emit chunk size {next_size} exceeds nocross boundary {boundary}"),
        ));
        return;
    }

    let offset_in_window = bank.bytes.len() % boundary;
    if offset_in_window + next_size > boundary {
        let pad = boundary - offset_in_window;
        bank.bytes.resize(bank.bytes.len() + pad, 0);
    }
}

fn write_literal(
    bytes: &mut Vec<u8>,
    value: u32,
    width: usize,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) {
    match width {
        0 => {}
        1 => match u8::try_from(value) {
            Ok(value) => bytes.push(value),
            Err(_) => diagnostics.push(Diagnostic::error(
                span,
                "value does not fit in 8-bit operand",
            )),
        },
        2 => match u16::try_from(value) {
            Ok(value) => bytes.extend_from_slice(&value.to_le_bytes()),
            Err(_) => diagnostics.push(Diagnostic::error(
                span,
                "value does not fit in 16-bit operand",
            )),
        },
        3 => {
            if value > 0x00FF_FFFF {
                diagnostics.push(Diagnostic::error(
                    span,
                    "value does not fit in 24-bit operand",
                ));
            } else {
                let bytes24 = value.to_le_bytes();
                bytes.extend_from_slice(&bytes24[..3]);
            }
        }
        _ => diagnostics.push(Diagnostic::error(span, "unsupported operand width")),
    }
}

fn write_literal_at(
    bytes: &mut [u8],
    offset: usize,
    value: u32,
    width: usize,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let end = offset + width;
    if end > bytes.len() {
        diagnostics.push(Diagnostic::error(span, "internal fixup overflow"));
        return;
    }

    match width {
        0 => {}
        1 => match u8::try_from(value) {
            Ok(value) => bytes[offset] = value,
            Err(_) => diagnostics.push(Diagnostic::error(
                span,
                "label address does not fit in 8-bit operand",
            )),
        },
        2 => match u16::try_from(value) {
            Ok(value) => bytes[offset..end].copy_from_slice(&value.to_le_bytes()),
            Err(_) => diagnostics.push(Diagnostic::error(
                span,
                "label address does not fit in 16-bit operand",
            )),
        },
        3 => {
            if value > 0x00FF_FFFF {
                diagnostics.push(Diagnostic::error(
                    span,
                    "label address does not fit in 24-bit operand",
                ));
            } else {
                let bytes24 = value.to_le_bytes();
                bytes[offset..end].copy_from_slice(&bytes24[..3]);
            }
        }
        _ => diagnostics.push(Diagnostic::error(span, "unsupported operand width")),
    }
}

fn format_listing_block(bank_name: &str, bytes: &[u8]) -> String {
    let mut out = String::new();
    out.push_str(&format!("[{bank_name}]\n"));

    if bytes.is_empty() {
        out.push_str("000000: <empty>");
        return out;
    }

    for (index, chunk) in bytes.chunks(16).enumerate() {
        let address = index * 16;
        let hex = chunk
            .iter()
            .map(|byte| format!("{byte:02X}"))
            .collect::<Vec<_>>()
            .join(" ");
        out.push_str(&format!("{address:06X}: {hex}\n"));
    }

    if out.ends_with('\n') {
        out.pop();
    }
    out
}
