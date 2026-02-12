use indexmap::IndexMap;
use k816_isa65816::{
    AddressOperandMode as IsaAddressOperandMode, AddressingMode, IndexRegister as IsaIndexRegister,
    OperandShape, decode_instruction_with_mode, format_instruction, operand_width_for_mode,
    select_encoding,
};
use rustc_hash::FxHashMap;

use crate::diag::Diagnostic;
use crate::hir::{
    AddressOperandMode, AddressValue, ByteRelocationKind, IndexRegister, Op, OperandOp, Program,
};
use crate::span::Span;

#[derive(Debug, Clone)]
pub struct EmitOutput {
    pub segments: IndexMap<String, Vec<u8>>,
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
    kind: FixupKind,
    label: String,
    span: Span,
}

#[derive(Debug, Clone, Copy)]
enum FixupKind {
    Instruction { width: usize, mode: AddressingMode },
    ByteRelocation(ByteRelocationKind),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum UnknownWidthCause {
    Plp,
    Rti,
}

#[derive(Debug)]
struct FunctionInstructionSite {
    segment: String,
    function: String,
    offset: usize,
}

fn to_isa_address_mode(mode: AddressOperandMode) -> IsaAddressOperandMode {
    match mode {
        AddressOperandMode::Direct { index } => IsaAddressOperandMode::Direct {
            index: index.map(|index| match index {
                IndexRegister::X => IsaIndexRegister::X,
                IndexRegister::Y => IsaIndexRegister::Y,
            }),
        },
        AddressOperandMode::Indirect => IsaAddressOperandMode::Indirect,
        AddressOperandMode::IndexedIndirectX => IsaAddressOperandMode::IndexedIndirectX,
        AddressOperandMode::IndirectIndexedY => IsaAddressOperandMode::IndirectIndexedY,
    }
}

pub fn emit(program: &Program) -> Result<EmitOutput, Vec<Diagnostic>> {
    let mut diagnostics = Vec::new();
    let mut segments: IndexMap<String, SegmentState> = IndexMap::new();
    let mut labels: FxHashMap<String, (String, usize)> = FxHashMap::default();
    let mut fixups = Vec::new();
    let mut current_segment = "default".to_string();
    let mut current_function: Option<String> = None;
    let mut function_instruction_sites = Vec::new();
    let mut m_wide: Option<bool> = None;
    let mut x_wide: Option<bool> = None;
    let mut m_unknown_cause: Option<UnknownWidthCause> = None;
    let mut x_unknown_cause: Option<UnknownWidthCause> = None;
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
            Op::FunctionStart {
                name,
                mode_contract,
                is_entry,
            } => {
                current_function = Some(name.clone());
                let default = if *is_entry { Some(false) } else { None };
                m_wide = mode_contract
                    .a_width
                    .map(|w| w == crate::ast::RegWidth::W16)
                    .or(default);
                x_wide = mode_contract
                    .i_width
                    .map(|w| w == crate::ast::RegWidth::W16)
                    .or(default);
                m_unknown_cause = None;
                x_unknown_cause = None;
            }
            Op::FunctionEnd => {
                current_function = None;
                m_unknown_cause = None;
                x_unknown_cause = None;
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
            Op::Rep(mask) | Op::FixedRep(mask) => {
                // REP #mask => opcode 0xC2, 1-byte immediate
                if mask & 0x20 != 0 {
                    m_wide = Some(true);
                    m_unknown_cause = None;
                }
                if mask & 0x10 != 0 {
                    x_wide = Some(true);
                    x_unknown_cause = None;
                }
                let segment = segments
                    .get_mut(&current_segment)
                    .expect("current segment must exist during emit");
                if let Some(ref func_name) = current_function {
                    function_instruction_sites.push(FunctionInstructionSite {
                        segment: current_segment.clone(),
                        function: func_name.clone(),
                        offset: segment.bytes.len(),
                    });
                }
                segment.bytes.push(0xC2);
                segment.bytes.push(*mask);
            }
            Op::Sep(mask) | Op::FixedSep(mask) => {
                // SEP #mask => opcode 0xE2, 1-byte immediate
                if mask & 0x20 != 0 {
                    m_wide = Some(false);
                    m_unknown_cause = None;
                }
                if mask & 0x10 != 0 {
                    x_wide = Some(false);
                    x_unknown_cause = None;
                }
                let segment = segments
                    .get_mut(&current_segment)
                    .expect("current segment must exist during emit");
                if let Some(ref func_name) = current_function {
                    function_instruction_sites.push(FunctionInstructionSite {
                        segment: current_segment.clone(),
                        function: func_name.clone(),
                        offset: segment.bytes.len(),
                    });
                }
                segment.bytes.push(0xE2);
                segment.bytes.push(*mask);
            }
            Op::EmitBytes(bytes) => {
                let segment = segments
                    .get_mut(&current_segment)
                    .expect("current segment must exist during emit");
                apply_nocross_if_needed(segment, bytes.len(), op.span, &mut diagnostics);
                segment.bytes.extend(bytes);
            }
            Op::EmitRelocBytes { bytes, relocations } => {
                let segment = segments
                    .get_mut(&current_segment)
                    .expect("current segment must exist during emit");
                apply_nocross_if_needed(segment, bytes.len(), op.span, &mut diagnostics);
                let emit_offset = segment.bytes.len();
                segment.bytes.extend(bytes);
                for relocation in relocations {
                    let Ok(rel_offset) = usize::try_from(relocation.offset) else {
                        diagnostics.push(Diagnostic::error(
                            op.span,
                            "byte relocation offset does not fit in usize",
                        ));
                        continue;
                    };
                    if rel_offset >= bytes.len() {
                        diagnostics.push(Diagnostic::error(
                            op.span,
                            format!(
                                "byte relocation offset {rel_offset} is outside emitted byte range"
                            ),
                        ));
                        continue;
                    }
                    fixups.push(Fixup {
                        segment: current_segment.clone(),
                        offset: emit_offset + rel_offset,
                        kind: FixupKind::ByteRelocation(relocation.kind),
                        label: relocation.label.clone(),
                        span: op.span,
                    });
                }
            }
            Op::Instruction(instruction) => {
                let segment = segments
                    .get_mut(&current_segment)
                    .expect("current segment must exist during emit");
                let mnemonic = instruction.mnemonic.to_ascii_lowercase();
                let operand_shape = match &instruction.operand {
                    None => OperandShape::None,
                    Some(OperandOp::Immediate(value)) => OperandShape::Immediate(*value),
                    Some(OperandOp::Address {
                        value,
                        force_far,
                        mode,
                    }) => match value {
                        AddressValue::Literal(literal) => OperandShape::Address {
                            literal: Some(*literal),
                            force_far: *force_far,
                            mode: to_isa_address_mode(*mode),
                        },
                        AddressValue::Label(_) => OperandShape::Address {
                            literal: None,
                            force_far: *force_far,
                            mode: to_isa_address_mode(*mode),
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

                if encoding.mode == AddressingMode::ImmediateM && m_wide.is_none() {
                    let help = match m_unknown_cause {
                        Some(UnknownWidthCause::Plp) => {
                            "PLP restores processor flags from runtime stack; add @a8 or @a16 to re-establish accumulator width"
                        }
                        Some(UnknownWidthCause::Rti) => {
                            "RTI restores processor flags from runtime state; add @a8 or @a16 to re-establish accumulator width"
                        }
                        None => "add @a8 or @a16 to the enclosing function",
                    };
                    diagnostics.push(
                        Diagnostic::error(
                            op.span,
                            format!(
                                "`{}` uses a width-dependent immediate but accumulator width is unknown",
                                instruction.mnemonic,
                            ),
                        )
                        .with_help(help),
                    );
                    continue;
                }
                if encoding.mode == AddressingMode::ImmediateX && x_wide.is_none() {
                    let help = match x_unknown_cause {
                        Some(UnknownWidthCause::Plp) => {
                            "PLP restores processor flags from runtime stack; add @i8 or @i16 to re-establish index width"
                        }
                        Some(UnknownWidthCause::Rti) => {
                            "RTI restores processor flags from runtime state; add @i8 or @i16 to re-establish index width"
                        }
                        None => "add @i8 or @i16 to the enclosing function",
                    };
                    diagnostics.push(
                        Diagnostic::error(
                            op.span,
                            format!(
                                "`{}` uses a width-dependent immediate but index width is unknown",
                                instruction.mnemonic,
                            ),
                        )
                        .with_help(help),
                    );
                    continue;
                }

                let width = operand_width_for_mode(
                    encoding.mode,
                    m_wide.unwrap_or(false),
                    x_wide.unwrap_or(false),
                );
                apply_nocross_if_needed(segment, 1 + width, op.span, &mut diagnostics);
                let opcode_offset = segment.bytes.len();
                segment.bytes.push(encoding.opcode);
                let operand_offset = segment.bytes.len();

                if let Some(function) = &current_function {
                    function_instruction_sites.push(FunctionInstructionSite {
                        segment: current_segment.clone(),
                        function: function.clone(),
                        offset: opcode_offset,
                    });
                }

                match &instruction.operand {
                    None => {}
                    Some(OperandOp::Immediate(value)) => {
                        if let Ok(raw) = u32::try_from(*value) {
                            write_literal(
                                &mut segment.bytes,
                                raw,
                                width,
                                op.span,
                                &mut diagnostics,
                            );
                        } else {
                            diagnostics.push(Diagnostic::error(
                                op.span,
                                "immediate value cannot be negative",
                            ));
                        }
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
                            } else if encoding.mode == AddressingMode::Relative16 {
                                let site_addr = operand_offset;
                                write_relative16_literal(
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
                                kind: FixupKind::Instruction {
                                    width,
                                    mode: encoding.mode,
                                },
                                label: label.clone(),
                                span: op.span,
                            });
                        }
                    },
                }

                if mnemonic == "plp" || mnemonic == "rti" {
                    m_wide = None;
                    x_wide = None;
                    let cause = if mnemonic == "plp" {
                        UnknownWidthCause::Plp
                    } else {
                        UnknownWidthCause::Rti
                    };
                    m_unknown_cause = Some(cause);
                    x_unknown_cause = Some(cause);
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

        let FixupKind::Instruction { width, mode } = fixup.kind else {
            let value = *label_addr as u32;
            let segment = segments
                .get_mut(&fixup.segment)
                .expect("fixup segment should exist during patching");
            match fixup.kind {
                FixupKind::ByteRelocation(ByteRelocationKind::LowByte) => {
                    segment.bytes[fixup.offset] = (value & 0xFF) as u8;
                }
                FixupKind::ByteRelocation(ByteRelocationKind::HighByte) => {
                    segment.bytes[fixup.offset] = ((value >> 8) & 0xFF) as u8;
                }
                FixupKind::Instruction { .. } => unreachable!(),
            }
            continue;
        };

        if mode == AddressingMode::Relative8 {
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

        if mode == AddressingMode::Relative16 {
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
            let delta = *label_addr as i64 - (site_addr as i64 + 2);
            if delta < i16::MIN as i64 || delta > i16::MAX as i64 {
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
            let rel = (delta as i16).to_le_bytes();
            segment.bytes[fixup.offset..fixup.offset + 2].copy_from_slice(&rel);
            continue;
        }

        if width == 2 && mode != AddressingMode::Relative16 && label_segment != &fixup.segment {
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
            width,
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
    let mut output_segments = IndexMap::new();
    for (segment_name, state) in &segments {
        if skip_default_empty && segment_name == "default" {
            continue;
        }
        listing_blocks.push(format_listing_block(segment_name, &state.bytes));
        output_segments.insert(segment_name.clone(), state.bytes.clone());
    }
    listing_blocks.extend(format_disassembly_blocks(
        &segments,
        &function_instruction_sites,
        skip_default_empty,
    ));

    Ok(EmitOutput {
        segments: output_segments,
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

fn write_relative16_literal(
    bytes: &mut Vec<u8>,
    target: u32,
    site_addr: usize,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let delta = target as i64 - (site_addr as i64 + 2);
    if delta < i16::MIN as i64 || delta > i16::MAX as i64 {
        diagnostics.push(Diagnostic::error(
            span,
            format!("relative16 target {target:#X} out of range"),
        ));
        return;
    }
    bytes.extend_from_slice(&(delta as i16).to_le_bytes());
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

fn format_listing_block(segment_name: &str, bytes: &[u8]) -> String {
    let mut out = String::new();
    out.push_str(&format!("[{segment_name}]\n"));

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

fn format_disassembly_blocks(
    segments: &IndexMap<String, SegmentState>,
    sites: &[FunctionInstructionSite],
    skip_default_empty: bool,
) -> Vec<String> {
    let mut grouped: IndexMap<(String, String), Vec<usize>> = IndexMap::new();
    for site in sites {
        if skip_default_empty && site.segment == "default" {
            continue;
        }
        grouped
            .entry((site.segment.clone(), site.function.clone()))
            .or_default()
            .push(site.offset);
    }

    let mut blocks = Vec::new();
    for ((segment_name, function_name), offsets) in grouped {
        let Some(segment) = segments.get(&segment_name) else {
            continue;
        };
        blocks.push(format_disassembly_block(
            &segment_name,
            &function_name,
            &segment.bytes,
            &offsets,
        ));
    }

    blocks
}

fn format_disassembly_block(
    segment_name: &str,
    function_name: &str,
    bytes: &[u8],
    offsets: &[usize],
) -> String {
    let mut out = String::new();
    out.push_str(&format!("[disasm {segment_name}::{function_name}]\n"));

    let mut m_wide = false;
    let mut x_wide = false;

    for &offset in offsets {
        if offset >= bytes.len() {
            continue;
        }

        match decode_instruction_with_mode(&bytes[offset..], m_wide, x_wide) {
            Ok(decoded) => {
                // Track mode changes from REP/SEP
                if decoded.mnemonic == "rep" && !decoded.operand.is_empty() {
                    let mask = decoded.operand[0];
                    if mask & 0x20 != 0 {
                        m_wide = true;
                    }
                    if mask & 0x10 != 0 {
                        x_wide = true;
                    }
                } else if decoded.mnemonic == "sep" && !decoded.operand.is_empty() {
                    let mask = decoded.operand[0];
                    if mask & 0x20 != 0 {
                        m_wide = false;
                    }
                    if mask & 0x10 != 0 {
                        x_wide = false;
                    }
                }

                let len = decoded.len();
                let end = (offset + len).min(bytes.len());
                let raw = &bytes[offset..end];
                let hex = raw
                    .iter()
                    .map(|byte| format!("{byte:02X}"))
                    .collect::<Vec<_>>()
                    .join(" ");
                let text = format_instruction(&decoded, offset as u32);
                out.push_str(&format!("{offset:06X}: {hex:<11} {text}\n"));
            }
            Err(_) => {
                out.push_str(&format!("{offset:06X}: ??         <decode-error>\n"));
            }
        }
    }

    if out.ends_with('\n') {
        out.pop();
    }
    out
}
