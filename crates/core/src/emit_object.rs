use indexmap::IndexMap;
use rustc_hash::FxHashMap;

use k816_isa65816::{
    AddressOperandMode as IsaAddressOperandMode, AddressingMode, IndexRegister as IsaIndexRegister,
    OperandShape, operand_width_for_mode, select_encoding,
};
use k816_o65::{
    CallMetadata, DataStringFragment, FunctionDisassembly, FunctionMetadata, O65Object, Relocation,
    RelocationKind, Section, SectionChunk, SourceLocation, Symbol, SymbolDefinition,
};

use crate::diag::Diagnostic;
use crate::hir::{
    AddressOperandMode, AddressValue, ByteRelocationKind, IndexRegister, Op, OperandOp, Program,
};
use crate::span::{SourceMap, Span};

#[derive(Debug, Clone)]
pub struct EmitObjectOutput {
    pub object: O65Object,
}


#[derive(Debug)]
struct SegmentState {
    chunks: Vec<SectionChunk>,
    section_offset: u32,
    fixed_cursor: Option<u32>,
    nocross_boundary: Option<u16>,
}

#[derive(Debug)]
struct Fixup {
    segment: String,
    offset: u32,
    width: u8,
    kind: RelocationKind,
    label: String,
    span: Span,
    call_metadata: Option<CallMetadata>,
}

#[derive(Debug)]
struct FunctionInstructionSite {
    segment: String,
    function: String,
    offset: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum UnknownWidthCause {
    Plp,
    Rti,
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

pub fn emit_object(
    program: &Program,
    source_map: &SourceMap,
) -> Result<EmitObjectOutput, Vec<Diagnostic>> {
    let mut diagnostics = Vec::new();
    let mut segments: IndexMap<String, SegmentState> = IndexMap::new();
    let mut labels: FxHashMap<String, (String, u32, Span)> = FxHashMap::default();
    let mut absolute_symbols: FxHashMap<String, (u32, Span)> = FxHashMap::default();
    let mut fixups = Vec::new();
    let mut current_segment = "default".to_string();
    let mut current_function: Option<String> = None;
    let mut function_instruction_sites = Vec::new();
    let mut function_initial_modes: FxHashMap<(String, String), (bool, bool)> =
        FxHashMap::default();
    let mut data_string_fragments = Vec::new();
    let mut function_label_metadata: FxHashMap<String, FunctionMetadata> = FxHashMap::default();
    let mut m_wide: Option<bool> = None; // accumulator width: None=unknown, Some(true)=16-bit, Some(false)=8-bit
    let mut x_wide: Option<bool> = None; // index width: None=unknown, Some(true)=16-bit, Some(false)=8-bit
    let mut m_unknown_cause: Option<UnknownWidthCause> = None;
    let mut x_unknown_cause: Option<UnknownWidthCause> = None;

    segments
        .entry(current_segment.clone())
        .or_insert(SegmentState {
            chunks: Vec::new(),
            section_offset: 0,
            fixed_cursor: None,
            nocross_boundary: None,
        });

    for op in &program.ops {
        match &op.node {
            Op::SelectSegment(name) => {
                current_segment = name.clone();
                segments
                    .entry(current_segment.clone())
                    .or_insert(SegmentState {
                        chunks: Vec::new(),
                        section_offset: 0,
                        fixed_cursor: None,
                        nocross_boundary: None,
                    });
            }
            Op::FunctionStart {
                name,
                mode_contract,
                is_entry,
                is_far,
            } => {
                current_function = Some(name.clone());
                // Entry point (main) defaults to 8-bit (emulation mode).
                // Functions require explicit mode contract for width-dependent ops.
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
                function_initial_modes.insert(
                    (current_segment.clone(), name.clone()),
                    (m_wide.unwrap_or(false), x_wide.unwrap_or(false)),
                );
                function_label_metadata.insert(
                    name.clone(),
                    FunctionMetadata {
                        is_far: *is_far,
                        a_width: mode_contract
                            .a_width
                            .map(|w| w == crate::ast::RegWidth::W16),
                        i_width: mode_contract
                            .i_width
                            .map(|w| w == crate::ast::RegWidth::W16),
                    },
                );
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
                    .insert(
                        name.clone(),
                        (current_segment.clone(), segment.section_offset, op.span),
                    )
                    .is_some()
                    || absolute_symbols.contains_key(name)
                {
                    diagnostics.push(
                        Diagnostic::error(op.span, format!("duplicate label '{name}'"))
                            .with_help("rename one of the labels"),
                    );
                }
            }
            Op::DefineAbsoluteSymbol { name, address } => {
                if labels.contains_key(name)
                    || absolute_symbols
                        .insert(name.clone(), (*address, op.span))
                        .is_some()
                {
                    diagnostics.push(
                        Diagnostic::error(op.span, format!("duplicate symbol '{name}'"))
                            .with_help("rename one of the symbols"),
                    );
                }
            }
            Op::SetMode(mode_contract) => {
                m_wide = mode_contract
                    .a_width
                    .map(|w| w == crate::ast::RegWidth::W16);
                x_wide = mode_contract
                    .i_width
                    .map(|w| w == crate::ast::RegWidth::W16);
                m_unknown_cause = None;
                x_unknown_cause = None;
            }
            Op::Align { boundary, offset } => {
                if *boundary == 0 {
                    diagnostics.push(Diagnostic::error(op.span, "align value must be non-zero"));
                    continue;
                }

                let segment = segments
                    .get_mut(&current_segment)
                    .expect("current segment must exist during emit");

                let cursor = active_cursor(segment);
                let align = u32::from(*boundary);
                let offset = u32::from(*offset);
                let rem = (cursor.wrapping_sub(offset)) % align;
                let pad = if rem == 0 { 0 } else { align - rem };

                if pad > 0 {
                    emit_zeroes(segment, pad as usize, op.span, &mut diagnostics);
                }
            }
            Op::Address(address) => {
                let segment = segments
                    .get_mut(&current_segment)
                    .expect("current segment must exist during emit");
                segment.fixed_cursor = Some(*address);
            }
            Op::Nocross(boundary) => {
                let segment = segments
                    .get_mut(&current_segment)
                    .expect("current segment must exist during emit");
                segment.nocross_boundary = Some(*boundary);
            }
            Op::Rep(mask) | Op::FixedRep(mask) => {
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
                let opcode_offset = segment.section_offset;
                if let Some(function) = &current_function {
                    function_instruction_sites.push(FunctionInstructionSite {
                        segment: current_segment.clone(),
                        function: function.clone(),
                        offset: opcode_offset,
                    });
                }
                let bytes = [0xC2, *mask];
                append_bytes(segment, &bytes, op.span, &mut diagnostics);
            }
            Op::Sep(mask) | Op::FixedSep(mask) => {
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
                let opcode_offset = segment.section_offset;
                if let Some(function) = &current_function {
                    function_instruction_sites.push(FunctionInstructionSite {
                        segment: current_segment.clone(),
                        function: function.clone(),
                        offset: opcode_offset,
                    });
                }
                let bytes = [0xE2, *mask];
                append_bytes(segment, &bytes, op.span, &mut diagnostics);
            }
            Op::EmitBytes(bytes) => {
                let segment = segments
                    .get_mut(&current_segment)
                    .expect("current segment must exist during emit");

                apply_nocross_if_needed(segment, bytes.len(), op.span, &mut diagnostics);
                let emit_offset = segment.section_offset;
                if let Some(text) = string_literal_text_for_emit(bytes, source_map, op.span) {
                    data_string_fragments.push(DataStringFragment {
                        section: current_segment.clone(),
                        offset: emit_offset,
                        text,
                    });
                }
                append_bytes(segment, bytes, op.span, &mut diagnostics);
            }
            Op::EmitRelocBytes { bytes, relocations } => {
                let segment = segments
                    .get_mut(&current_segment)
                    .expect("current segment must exist during emit");

                apply_nocross_if_needed(segment, bytes.len(), op.span, &mut diagnostics);
                let emit_offset = segment.section_offset;
                append_bytes(segment, bytes, op.span, &mut diagnostics);
                for relocation in relocations {
                    if relocation.offset >= bytes.len() as u32 {
                        diagnostics.push(Diagnostic::error(
                            op.span,
                            format!(
                                "byte relocation offset {} is outside emitted byte range",
                                relocation.offset
                            ),
                        ));
                        continue;
                    }
                    let relocation_span =
                        relocation_span_for_label(source_map, op.span, &relocation.label);
                    let (width, kind) = match relocation.kind {
                        ByteRelocationKind::LowByte => (1, RelocationKind::LowByte),
                        ByteRelocationKind::HighByte => (1, RelocationKind::HighByte),
                        ByteRelocationKind::FullWord => (2, RelocationKind::Absolute),
                    };
                    fixups.push(Fixup {
                        segment: current_segment.clone(),
                        offset: emit_offset + relocation.offset,
                        width,
                        kind,
                        label: relocation.label.clone(),
                        span: relocation_span,
                        call_metadata: None,
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
                    Some(OperandOp::ImmediateByteRelocation { .. }) => OperandShape::Immediate(0),
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

                let opcode_offset = segment.section_offset;
                append_bytes(segment, &[encoding.opcode], op.span, &mut diagnostics);
                let operand_offset = segment.section_offset;

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
                            emit_literal(segment, raw, width, op.span, &mut diagnostics);
                        } else {
                            diagnostics.push(Diagnostic::error(
                                op.span,
                                "immediate value cannot be negative",
                            ));
                        }
                    }
                    Some(OperandOp::ImmediateByteRelocation { kind, label }) => {
                        if width == 0 {
                            diagnostics.push(Diagnostic::error(
                                op.span,
                                "immediate byte relocation requires a non-zero operand width",
                            ));
                            continue;
                        }
                        emit_zeroes(segment, width, op.span, &mut diagnostics);
                        let relocation_span = relocation_span_for_label(source_map, op.span, label);
                        let (reloc_width, reloc_kind) = match kind {
                            ByteRelocationKind::LowByte => (1, RelocationKind::LowByte),
                            ByteRelocationKind::HighByte => (1, RelocationKind::HighByte),
                            ByteRelocationKind::FullWord => (2, RelocationKind::Absolute),
                        };
                        fixups.push(Fixup {
                            segment: current_segment.clone(),
                            offset: operand_offset,
                            width: reloc_width,
                            kind: reloc_kind,
                            label: label.clone(),
                            span: relocation_span,
                            call_metadata: None,
                        });
                    }
                    Some(OperandOp::Address { value, .. }) => match value {
                        AddressValue::Literal(literal) => {
                            if matches!(
                                encoding.mode,
                                AddressingMode::Relative8 | AddressingMode::Relative16
                            ) {
                                diagnostics.push(Diagnostic::error(
                                    op.span,
                                    "relative branch operands must be labels, not numeric literals",
                                ));
                            } else {
                                emit_literal(segment, *literal, width, op.span, &mut diagnostics)
                            }
                        }
                        AddressValue::Label(label) => {
                            emit_zeroes(segment, width, op.span, &mut diagnostics);
                            let relocation_span =
                                relocation_span_for_label(source_map, op.span, label);
                            let is_call = matches!(mnemonic.as_str(), "jsr" | "jsl");
                            fixups.push(Fixup {
                                segment: current_segment.clone(),
                                offset: operand_offset,
                                width: width as u8,
                                kind: if matches!(
                                    encoding.mode,
                                    AddressingMode::Relative8 | AddressingMode::Relative16
                                ) {
                                    RelocationKind::Relative
                                } else {
                                    RelocationKind::Absolute
                                },
                                label: label.clone(),
                                span: relocation_span,
                                call_metadata: if is_call {
                                    Some(CallMetadata {
                                        caller_a_width: m_wide,
                                        caller_i_width: x_wide,
                                    })
                                } else {
                                    None
                                },
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

    if !diagnostics.is_empty() {
        return Err(diagnostics);
    }

    let skip_default_empty = segments.len() > 1
        && segments
            .get("default")
            .is_some_and(|state| state.chunks.is_empty());

    let mut listing_blocks = Vec::new();
    let mut sections = IndexMap::new();
    for (segment_name, state) in &segments {
        if skip_default_empty && segment_name == "default" {
            continue;
        }

        listing_blocks.push(format_listing_block(segment_name, &state.chunks));
        sections.insert(
            segment_name.clone(),
            Section {
                chunks: state.chunks.clone(),
            },
        );
    }

    let mut symbols = Vec::new();
    for (name, (segment, offset, span)) in &labels {
        if skip_default_empty && segment == "default" {
            continue;
        }
        symbols.push(Symbol {
            name: name.clone(),
            global: true,
            definition: Some(SymbolDefinition::Section {
                section: segment.clone(),
                offset: *offset,
                source: source_location_for_span(source_map, *span),
            }),
            function_metadata: function_label_metadata.get(name.as_str()).copied(),
        });
    }
    for (name, (address, span)) in &absolute_symbols {
        symbols.push(Symbol {
            name: name.clone(),
            global: true,
            definition: Some(SymbolDefinition::Absolute {
                address: *address,
                source: source_location_for_span(source_map, *span),
            }),
            function_metadata: None,
        });
    }
    symbols.sort_by(|a, b| a.name.cmp(&b.name));

    let mut relocations = Vec::new();
    for fixup in &fixups {
        if skip_default_empty && fixup.segment == "default" {
            continue;
        }
        relocations.push(Relocation {
            section: fixup.segment.clone(),
            offset: fixup.offset,
            width: fixup.width,
            kind: fixup.kind,
            symbol: fixup.label.clone(),
            source: source_location_for_span(source_map, fixup.span),
            call_metadata: fixup.call_metadata,
        });
    }

    let mut grouped_disassembly: IndexMap<(String, String), Vec<u32>> = IndexMap::new();
    for site in function_instruction_sites {
        if skip_default_empty && site.segment == "default" {
            continue;
        }
        grouped_disassembly
            .entry((site.segment, site.function))
            .or_default()
            .push(site.offset);
    }

    let function_disassembly = grouped_disassembly
        .into_iter()
        .map(|((section, function), instruction_offsets)| {
            let (m_wide, x_wide) = function_initial_modes
                .get(&(section.clone(), function.clone()))
                .copied()
                .unwrap_or((false, false));
            FunctionDisassembly {
                section,
                function,
                instruction_offsets,
                m_wide,
                x_wide,
            }
        })
        .collect();
    data_string_fragments.sort_by(|lhs, rhs| {
        lhs.section
            .cmp(&rhs.section)
            .then_with(|| lhs.offset.cmp(&rhs.offset))
    });

    Ok(EmitObjectOutput {
        object: O65Object {
            sections,
            symbols,
            relocations,
            function_disassembly,
            data_string_fragments,
            listing: listing_blocks.join("\n\n"),
        },
    })
}

fn active_cursor(segment: &SegmentState) -> u32 {
    segment.fixed_cursor.unwrap_or(segment.section_offset)
}

fn emit_zeroes(
    segment: &mut SegmentState,
    count: usize,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if count == 0 {
        return;
    }
    let bytes = vec![0_u8; count];
    append_bytes(segment, &bytes, span, diagnostics);
}

fn append_bytes(
    segment: &mut SegmentState,
    bytes: &[u8],
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if bytes.is_empty() {
        return;
    }

    let len = match u32::try_from(bytes.len()) {
        Ok(len) => len,
        Err(_) => {
            diagnostics.push(Diagnostic::error(
                span,
                "emitted bytes exceed 32-bit section offset range",
            ));
            return;
        }
    };

    let section_start = segment.section_offset;
    let fixed_start = segment.fixed_cursor;

    let can_extend_last = segment
        .chunks
        .last()
        .is_some_and(|last| can_extend_chunk(last, section_start, fixed_start));

    if can_extend_last {
        if let Some(last) = segment.chunks.last_mut() {
            last.bytes.extend_from_slice(bytes);
        }
    } else {
        segment.chunks.push(SectionChunk {
            offset: section_start,
            address: fixed_start,
            bytes: bytes.to_vec(),
        });
    }

    segment.section_offset = match segment.section_offset.checked_add(len) {
        Some(next) => next,
        None => {
            diagnostics.push(Diagnostic::error(
                span,
                "section offset overflow while appending bytes",
            ));
            return;
        }
    };

    if let Some(fixed_start) = segment.fixed_cursor {
        segment.fixed_cursor = match fixed_start.checked_add(len) {
            Some(next) => Some(next),
            None => {
                diagnostics.push(Diagnostic::error(
                    span,
                    "absolute address overflow while appending bytes",
                ));
                None
            }
        };
    }
}

fn can_extend_chunk(last: &SectionChunk, section_start: u32, fixed_start: Option<u32>) -> bool {
    let last_len = match u32::try_from(last.bytes.len()) {
        Ok(len) => len,
        Err(_) => return false,
    };

    let Some(last_section_end) = last.offset.checked_add(last_len) else {
        return false;
    };
    if last_section_end != section_start {
        return false;
    }

    match (last.address, fixed_start) {
        (None, None) => true,
        (Some(last_addr), Some(fixed_addr)) => {
            let Some(last_addr_end) = last_addr.checked_add(last_len) else {
                return false;
            };
            last_addr_end == fixed_addr
        }
        _ => false,
    }
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

    let cursor = active_cursor(segment) as usize;
    let offset_in_window = cursor % boundary;
    if offset_in_window + next_size > boundary {
        let pad = boundary - offset_in_window;
        emit_zeroes(segment, pad, span, diagnostics);
    }
}

fn emit_literal(
    segment: &mut SegmentState,
    value: u32,
    width: usize,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) {
    match width {
        0 => {}
        1 => match u8::try_from(value) {
            Ok(value) => append_bytes(segment, &[value], span, diagnostics),
            Err(_) => diagnostics.push(Diagnostic::error(
                span,
                "value does not fit in 8-bit operand",
            )),
        },
        2 => match u16::try_from(value) {
            Ok(value) => append_bytes(segment, &value.to_le_bytes(), span, diagnostics),
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
                append_bytes(segment, &bytes24[..3], span, diagnostics);
            }
        }
        _ => diagnostics.push(Diagnostic::error(span, "unsupported operand width")),
    }
}

fn format_listing_block(segment_name: &str, chunks: &[SectionChunk]) -> String {
    let mut out = String::new();
    out.push_str(&format!("[{segment_name}]\n"));

    if chunks.is_empty() {
        out.push_str("(empty)\n");
        return out;
    }

    for chunk in chunks {
        for (index, row) in chunk.bytes.chunks(16).enumerate() {
            let mut hex = String::new();
            for (i, byte) in row.iter().enumerate() {
                if i > 0 {
                    hex.push(' ');
                }
                hex.push_str(&format!("{byte:02X}"));
            }

            let address = chunk.offset + (index as u32 * 16);
            out.push_str(&format!("{address:06X}: {hex}\n"));
        }
    }

    out
}

fn source_location_for_span(source_map: &SourceMap, span: Span) -> Option<SourceLocation> {
    let file = source_map.get(span.source_id)?;
    let (line, column) = file.line_col(span.start);
    let end_offset = span.end.saturating_sub(1).max(span.start);
    let (end_line, end_column_inclusive) = file.line_col(end_offset);
    let column_end = if end_line == line {
        end_column_inclusive.saturating_add(1)
    } else {
        column.saturating_add(1)
    };
    let line_index = line.saturating_sub(1);
    let line_text = file
        .text
        .lines()
        .nth(line_index)
        .unwrap_or_default()
        .to_string();

    Some(SourceLocation {
        file: file.name.clone(),
        line: line as u32,
        column: column as u32,
        column_end: column_end as u32,
        line_text,
    })
}

fn relocation_span_for_label(source_map: &SourceMap, fallback: Span, label: &str) -> Span {
    let Some(file) = source_map.get(fallback.source_id) else {
        return fallback;
    };

    let text_len = file.text.len();
    let start = fallback.start.min(text_len);
    let end = fallback.end.min(text_len);
    if start >= end {
        return fallback;
    }

    let haystack = &file.text[start..end];
    for candidate in label_span_candidates(label) {
        if let Some(idx) = haystack.find(&candidate) {
            return Span::new(
                fallback.source_id,
                start.saturating_add(idx),
                start.saturating_add(idx).saturating_add(candidate.len()),
            );
        }
    }

    fallback
}

fn label_span_candidates(label: &str) -> Vec<String> {
    let mut candidates = vec![label.to_string()];
    if let Some(local_name) = label.split("::.").nth(1) {
        candidates.push(format!(".{local_name}"));
    }
    candidates
}

fn string_literal_text_for_emit(
    bytes: &[u8],
    source_map: &SourceMap,
    span: Span,
) -> Option<String> {
    let source = source_location_for_span(source_map, span)?;
    if !source.line_text.trim_start().starts_with('"') {
        return None;
    }

    std::str::from_utf8(bytes).ok().map(|text| text.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::ModeContract;
    use crate::hir::{
        AddressOperandMode, AddressValue, ByteRelocationKind, InstructionOp, Op, OperandOp, Program,
    };
    use crate::span::{SourceId, SourceMap, Span, Spanned};

    fn op(node: Op) -> Spanned<Op> {
        Spanned::new(node, Span::new(SourceId(0), 0, 0))
    }

    #[test]
    fn address_creates_sparse_chunk_without_zero_padding() {
        let mut source_map = SourceMap::default();
        source_map.add_source("test.k65", "func a {}\n");
        let program = Program {
            ops: vec![
                op(Op::Label("func_a".to_string())),
                op(Op::EmitBytes(vec![0xEA, 0x60])),
                op(Op::Address(0x4000)),
                op(Op::Label("func_b".to_string())),
                op(Op::EmitBytes(vec![0xEA, 0x60])),
            ],
        };

        let emitted = emit_object(&program, &source_map).expect("emit object");
        let section = emitted
            .object
            .sections
            .get("default")
            .expect("default section");

        assert_eq!(section.chunks.len(), 2);
        assert_eq!(section.chunks[0].offset, 0);
        assert_eq!(section.chunks[0].address, None);
        assert_eq!(section.chunks[0].bytes, vec![0xEA, 0x60]);

        assert_eq!(section.chunks[1].offset, 2);
        assert_eq!(section.chunks[1].address, Some(0x4000));
        assert_eq!(section.chunks[1].bytes, vec![0xEA, 0x60]);
    }

    #[test]
    fn labels_after_address_keep_compact_section_offsets() {
        let mut source_map = SourceMap::default();
        source_map.add_source("test.k65", "func a {}\n");
        let program = Program {
            ops: vec![
                op(Op::Label("func_a".to_string())),
                op(Op::EmitBytes(vec![0xEA, 0x60])),
                op(Op::Address(0x4000)),
                op(Op::Label("func_b".to_string())),
                op(Op::EmitBytes(vec![0xEA, 0x60])),
            ],
        };

        let emitted = emit_object(&program, &source_map).expect("emit object");
        let mut symbols = emitted.object.symbols;
        symbols.sort_by(|a, b| a.name.cmp(&b.name));

        assert_eq!(symbols.len(), 2);
        assert_eq!(symbols[0].name, "func_a");
        assert!(matches!(
            symbols[0].definition.as_ref(),
            Some(SymbolDefinition::Section { offset: 0, .. })
        ));
        assert_eq!(symbols[1].name, "func_b");
        assert!(matches!(
            symbols[1].definition.as_ref(),
            Some(SymbolDefinition::Section { offset: 2, .. })
        ));
    }

    #[test]
    fn keeps_unresolved_labels_for_linker() {
        let mut source_map = SourceMap::default();
        source_map.add_source("test.k65", "func a {}\n");
        let program = Program {
            ops: vec![op(Op::Instruction(InstructionOp {
                mnemonic: "lda".to_string(),
                operand: Some(OperandOp::Address {
                    value: AddressValue::Label("missing".to_string()),
                    force_far: false,
                    mode: AddressOperandMode::Direct { index: None },
                }),
            }))],
        };

        let emitted =
            emit_object(&program, &source_map).expect("unresolved labels deferred to linker");
        assert_eq!(emitted.object.symbols.len(), 0);
        assert_eq!(emitted.object.relocations.len(), 1);
        assert_eq!(emitted.object.relocations[0].symbol, "missing");
    }

    #[test]
    fn keeps_immediate_byte_relocations_for_linker() {
        let mut source_map = SourceMap::default();
        source_map.add_source("test.k65", "func main { a=&<missing }\n");
        let program = Program {
            ops: vec![
                op(Op::FunctionStart {
                    name: "main".to_string(),
                    mode_contract: ModeContract::default(),
                    is_entry: true,
                    is_far: false,
                }),
                op(Op::Instruction(InstructionOp {
                    mnemonic: "lda".to_string(),
                    operand: Some(OperandOp::ImmediateByteRelocation {
                        kind: ByteRelocationKind::LowByte,
                        label: "missing".to_string(),
                    }),
                })),
                op(Op::FunctionEnd),
            ],
        };

        let emitted =
            emit_object(&program, &source_map).expect("unresolved labels deferred to linker");
        assert_eq!(emitted.object.symbols.len(), 0);
        assert_eq!(emitted.object.relocations.len(), 1);
        assert_eq!(emitted.object.relocations[0].symbol, "missing");
        assert_eq!(emitted.object.relocations[0].kind, RelocationKind::LowByte);
        assert_eq!(emitted.object.relocations[0].width, 1);
    }

    #[test]
    fn emits_absolute_symbol_definitions() {
        let mut source_map = SourceMap::default();
        source_map.add_source("test.k65", "var foo = 0x1234\n");
        let program = Program {
            ops: vec![op(Op::DefineAbsoluteSymbol {
                name: "foo".to_string(),
                address: 0x1234,
            })],
        };

        let emitted = emit_object(&program, &source_map).expect("emit object");
        assert_eq!(emitted.object.symbols.len(), 1);
        assert!(matches!(
            emitted.object.symbols[0].definition.as_ref(),
            Some(SymbolDefinition::Absolute {
                address: 0x1234,
                ..
            })
        ));
    }
}
