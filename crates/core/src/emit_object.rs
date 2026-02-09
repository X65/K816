use indexmap::IndexMap;
use rustc_hash::FxHashMap;

use k816_o65::{O65Object, Relocation, RelocationKind, Section, Symbol, SymbolDefinition};

use crate::diag::Diagnostic;
use crate::hir::{AddressValue, Op, OperandOp, Program};
use crate::isa65816::{OperandShape, operand_width, select_encoding};
use crate::span::Span;

#[derive(Debug, Clone)]
pub struct EmitObjectOutput {
    pub object: O65Object,
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
    kind: RelocationKind,
    label: String,
    span: Span,
}

pub fn emit_object(program: &Program) -> Result<EmitObjectOutput, Vec<Diagnostic>> {
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
                            .with_help("rename one of the labels"),
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
                while !bank.bytes.len().is_multiple_of(usize::from(*align)) {
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
                                kind: RelocationKind::Absolute,
                                label: label.clone(),
                                span: op.span,
                            });
                        }
                    },
                }
            }
        }
    }

    for fixup in &fixups {
        if !labels.contains_key(&fixup.label) {
            diagnostics.push(Diagnostic::error(
                fixup.span,
                format!("undefined label '{}'", fixup.label),
            ));
        }
    }

    if !diagnostics.is_empty() {
        return Err(diagnostics);
    }

    let skip_default_empty = banks.len() > 1
        && banks
            .get("default")
            .is_some_and(|state| state.bytes.is_empty());

    let mut listing_blocks = Vec::new();
    let mut sections = IndexMap::new();
    for (bank_name, state) in &banks {
        if skip_default_empty && bank_name == "default" {
            continue;
        }
        listing_blocks.push(format_listing_block(bank_name, &state.bytes));
        sections.insert(
            bank_name.clone(),
            Section {
                bytes: state.bytes.clone(),
            },
        );
    }

    let mut symbols = Vec::new();
    for (name, (bank, offset)) in &labels {
        if skip_default_empty && bank == "default" {
            continue;
        }
        symbols.push(Symbol {
            name: name.clone(),
            global: true,
            definition: Some(SymbolDefinition {
                section: bank.clone(),
                offset: *offset as u32,
            }),
        });
    }
    symbols.sort_by(|a, b| a.name.cmp(&b.name));

    let mut relocations = Vec::new();
    for fixup in &fixups {
        if skip_default_empty && fixup.bank == "default" {
            continue;
        }
        relocations.push(Relocation {
            section: fixup.bank.clone(),
            offset: fixup.offset as u32,
            width: fixup.width as u8,
            kind: fixup.kind,
            symbol: fixup.label.clone(),
        });
    }

    Ok(EmitObjectOutput {
        object: O65Object {
            sections,
            symbols,
            relocations,
            listing: listing_blocks.join("\n\n"),
        },
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

fn format_listing_block(bank_name: &str, bytes: &[u8]) -> String {
    let mut out = String::new();
    out.push_str(&format!("[{bank_name}]\n"));
    if bytes.is_empty() {
        out.push_str("(empty)\n");
        return out;
    }

    for (index, chunk) in bytes.chunks(16).enumerate() {
        let mut hex = String::new();
        for (i, byte) in chunk.iter().enumerate() {
            if i > 0 {
                hex.push(' ');
            }
            hex.push_str(&format!("{byte:02X}"));
        }
        let address = index * 16;
        out.push_str(&format!("{address:06X}: {hex}\n"));
    }

    out
}
