use indexmap::IndexMap;
use rustc_hash::FxHashMap;

use crate::diag::Diagnostic;
use crate::hir::{AddressValue, Op, OperandOp, Program};
use crate::isa65816::{AddressingMode, OperandShape, operand_width, select_encoding};
use crate::span::Span;

#[derive(Debug, Clone)]
pub struct EmitOutput {
    pub banks: IndexMap<String, Vec<u8>>,
    pub listing: String,
}

#[derive(Debug)]
struct SegmentState {
    bytes: Vec<u8>,
    nocross_boundary: Option<u16>,
}

#[derive(Debug)]
struct Fixup {
    segment: String,
    offset: usize,
    width: usize,
    mode: AddressingMode,
    label: String,
    span: Span,
}

pub fn emit(program: &Program) -> Result<EmitOutput, Vec<Diagnostic>> {
    let mut diagnostics = Vec::new();
    let mut segments: IndexMap<String, SegmentState> = IndexMap::new();
    let mut labels: FxHashMap<String, (String, usize)> = FxHashMap::default();
    let mut fixups = Vec::new();
    let mut current_segment = "default".to_string();
    segments
        .entry(current_segment.clone())
        .or_insert(SegmentState {
            bytes: Vec::new(),
            nocross_boundary: None,
        });

    for op in &program.ops {
        match &op.node {
            Op::SelectSegment(name) => {
                current_segment = name.clone();
                segments
                    .entry(current_segment.clone())
                    .or_insert(SegmentState {
                        bytes: Vec::new(),
                        nocross_boundary: None,
                    });
            }
            Op::Label(name) => {
                let segment = segments
                    .get(&current_segment)
                    .expect("current segment must exist during emit");
                if labels
                    .insert(name.clone(), (current_segment.clone(), segment.bytes.len()))
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
                let segment = segments
                    .get_mut(&current_segment)
                    .expect("current segment must exist during emit");
                while !segment.bytes.len().is_multiple_of(usize::from(*align)) {
                    segment.bytes.push(0);
                }
            }
            Op::Address(address) => {
                let segment = segments
                    .get_mut(&current_segment)
                    .expect("current segment must exist during emit");
                let address = *address as usize;
                if segment.bytes.len() > address {
                    diagnostics.push(Diagnostic::error(
                        op.span,
                        format!(
                            "address {address:#X} is behind current position {:#X}",
                            segment.bytes.len()
                        ),
                    ));
                    continue;
                }
                while segment.bytes.len() < address {
                    segment.bytes.push(0);
                }
            }
            Op::Nocross(boundary) => {
                let segment = segments
                    .get_mut(&current_segment)
                    .expect("current segment must exist during emit");
                segment.nocross_boundary = Some(*boundary);
            }
            Op::EmitBytes(bytes) => {
                let segment = segments
                    .get_mut(&current_segment)
                    .expect("current segment must exist during emit");
                apply_nocross_if_needed(segment, bytes.len(), op.span, &mut diagnostics);
                segment.bytes.extend(bytes);
            }
            Op::Instruction(instruction) => {
                let segment = segments
                    .get_mut(&current_segment)
                    .expect("current segment must exist during emit");
                let operand_shape = match &instruction.operand {
                    None => OperandShape::None,
                    Some(OperandOp::Immediate(value)) => OperandShape::Immediate(*value),
                    Some(OperandOp::Address {
                        value,
                        force_far,
                        index_x,
                    }) => match value {
                        AddressValue::Literal(literal) => OperandShape::Address {
                            literal: Some(*literal),
                            force_far: *force_far,
                            indexed_x: *index_x,
                        },
                        AddressValue::Label(_) => OperandShape::Address {
                            literal: None,
                            force_far: *force_far,
                            indexed_x: *index_x,
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
                apply_nocross_if_needed(segment, 1 + width, op.span, &mut diagnostics);
                segment.bytes.push(encoding.opcode);
                let operand_offset = segment.bytes.len();

                match &instruction.operand {
                    None => {}
                    Some(OperandOp::Immediate(value)) => {
                        segment.bytes.push(*value as u8);
                    }
                    Some(OperandOp::Address { value, .. }) => match value {
                        AddressValue::Literal(literal) => {
                            if encoding.mode == AddressingMode::Relative8 {
                                let site_addr = operand_offset;
                                write_relative_literal(
                                    &mut segment.bytes,
                                    *literal,
                                    site_addr,
                                    op.span,
                                    &mut diagnostics,
                                );
                            } else {
                                write_literal(
                                    &mut segment.bytes,
                                    *literal,
                                    width,
                                    op.span,
                                    &mut diagnostics,
                                );
                            }
                        }
                        AddressValue::Label(label) => {
                            segment.bytes.resize(segment.bytes.len() + width, 0);
                            fixups.push(Fixup {
                                segment: current_segment.clone(),
                                offset: operand_offset,
                                width,
                                mode: encoding.mode,
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
        let Some((label_segment, label_addr)) = labels.get(&fixup.label) else {
            diagnostics.push(Diagnostic::error(
                fixup.span,
                format!("undefined label '{}'", fixup.label),
            ));
            continue;
        };

        if fixup.mode == AddressingMode::Relative8 {
            if label_segment != &fixup.segment {
                diagnostics.push(
                    Diagnostic::error(
                        fixup.span,
                        format!(
                            "relative branch from segment '{}' to '{}' is not supported",
                            fixup.segment, label_segment
                        ),
                    )
                    .with_help("branch targets must stay in the same segment"),
                );
                continue;
            }

            let site_addr = fixup.offset;
            let delta = *label_addr as isize - (site_addr as isize + 1);
            if delta < i8::MIN as isize || delta > i8::MAX as isize {
                diagnostics.push(Diagnostic::error(
                    fixup.span,
                    format!(
                        "relative branch to '{}' out of range from {site_addr:#X}",
                        fixup.label
                    ),
                ));
                continue;
            }

            let segment = segments
                .get_mut(&fixup.segment)
                .expect("fixup segment should exist during patching");
            segment.bytes[fixup.offset] = delta as i8 as u8;
            continue;
        }

        if fixup.width == 2 && label_segment != &fixup.segment {
            diagnostics.push(
                Diagnostic::error(
                    fixup.span,
                    format!(
                        "label '{}' is in segment '{label_segment}', but absolute reference is in segment '{}'",
                        fixup.label, fixup.segment
                    ),
                )
                .with_help("use far function calls or far operand to cross segments"),
            );
            continue;
        }

        let value = *label_addr as u32;
        let segment = segments
            .get_mut(&fixup.segment)
            .expect("fixup segment should exist during patching");
        write_literal_at(
            &mut segment.bytes,
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

    let skip_default_empty = segments.len() > 1
        && segments
            .get("default")
            .is_some_and(|state| state.bytes.is_empty());

    let mut listing_blocks = Vec::new();
    let mut output_banks = IndexMap::new();
    for (segment_name, state) in &segments {
        if skip_default_empty && segment_name == "default" {
            continue;
        }
        listing_blocks.push(format_listing_block(segment_name, &state.bytes));
        output_banks.insert(segment_name.clone(), state.bytes.clone());
    }

    Ok(EmitOutput {
        banks: output_banks,
        listing: listing_blocks.join("\n\n"),
    })
}

fn apply_nocross_if_needed(
    segment: &mut SegmentState,
    next_size: usize,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let Some(boundary) = segment.nocross_boundary.take() else {
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

    let offset_in_window = segment.bytes.len() % boundary;
    if offset_in_window + next_size > boundary {
        let pad = boundary - offset_in_window;
        segment.bytes.resize(segment.bytes.len() + pad, 0);
    }
}

fn write_relative_literal(
    bytes: &mut Vec<u8>,
    target: u32,
    site_addr: usize,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let delta = target as i64 - (site_addr as i64 + 1);
    if delta < i8::MIN as i64 || delta > i8::MAX as i64 {
        diagnostics.push(Diagnostic::error(
            span,
            format!("relative branch target {target:#X} out of range"),
        ));
        return;
    }
    bytes.push(delta as i8 as u8);
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
