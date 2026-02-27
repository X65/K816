use anyhow::{Context, Result, bail};
use k816_isa65816::{decode_instruction_with_mode, format_instruction};
use k816_o65::{O65Object, SourceLocation, SymbolDefinition};
use std::collections::HashMap;

use super::layout::MemoryState;
use super::render::resolve_reloc_site;
use super::{PlacedChunk, resolve_symbol_addr};

pub(super) fn format_section_listing(
    object_index: usize,
    segment: &str,
    chunks: &[PlacedChunk],
    memory_map: &HashMap<String, MemoryState>,
    include_object_index: bool,
) -> Result<String> {
    let mut out = String::new();
    out.push_str(&format!(
        "{}\n",
        format_section_header(object_index, segment, include_object_index)
    ));

    for chunk in chunks {
        let mem = memory_map.get(&chunk.memory_name).ok_or_else(|| {
            anyhow::anyhow!(
                "internal linker error: memory '{}' missing",
                chunk.memory_name
            )
        })?;

        let rel_start = (chunk.base_addr - mem.spec.start) as usize;
        let rel_end = rel_start + chunk.len as usize;
        let bytes = &mem.bytes[rel_start..rel_end];

        for (row_index, row) in bytes.chunks(16).enumerate() {
            let mut hex = String::new();
            for (i, byte) in row.iter().enumerate() {
                if i > 0 {
                    hex.push(' ');
                }
                hex.push_str(&format!("{byte:02X}"));
            }

            let address = chunk.base_addr + (row_index as u32 * 16);
            out.push_str(&format!("{address:06X}: {hex}\n"));
        }
    }

    Ok(out)
}

pub(super) fn format_data_symbol_listings(
    objects: &[O65Object],
    placed_by_section: &HashMap<(usize, String), Vec<PlacedChunk>>,
) -> Result<Vec<String>> {
    let mut blocks = Vec::new();

    for (object_index, object) in objects.iter().enumerate() {
        let mut data_symbols = object
            .symbols
            .iter()
            .filter(|symbol| is_named_data_symbol(symbol))
            .collect::<Vec<_>>();
        data_symbols.sort_by(|lhs, rhs| {
            let lhs_location = lhs
                .definition
                .as_ref()
                .and_then(symbol_source_location)
                .map(|source| (source.line, source.column))
                .unwrap_or((u32::MAX, u32::MAX));
            let rhs_location = rhs
                .definition
                .as_ref()
                .and_then(symbol_source_location)
                .map(|source| (source.line, source.column))
                .unwrap_or((u32::MAX, u32::MAX));

            lhs_location
                .cmp(&rhs_location)
                .then_with(|| lhs.name.cmp(&rhs.name))
        });

        for symbol in data_symbols {
            let definition = symbol.definition.as_ref().ok_or_else(|| {
                anyhow::anyhow!("internal linker error: data symbol without definition")
            })?;
            let SymbolDefinition::Section {
                section,
                offset,
                source: _,
            } = definition
            else {
                continue;
            };
            let key = (object_index, section.clone());
            let placements = placed_by_section.get(&key).ok_or_else(|| {
                anyhow::anyhow!(
                    "internal linker error: placements missing for data section '{}'",
                    section
                )
            })?;
            let byte_count = data_symbol_byte_count(object, symbol)?;

            blocks.push(format_data_symbol_listing_block(
                object_index,
                object,
                section,
                &symbol.name,
                *offset,
                byte_count,
                placements,
            )?);
        }
    }

    Ok(blocks)
}

fn is_named_data_symbol(symbol: &k816_o65::Symbol) -> bool {
    let Some(SymbolDefinition::Section {
        source: Some(source),
        ..
    }) = symbol.definition.as_ref()
    else {
        return false;
    };

    source.line_text.trim_start().starts_with("data ")
}

fn is_top_level_code_block_symbol(symbol: &k816_o65::Symbol) -> bool {
    let Some(SymbolDefinition::Section {
        source: Some(source),
        ..
    }) = symbol.definition.as_ref()
    else {
        return false;
    };

    let line = source.line_text.trim_start();
    if !line.contains('{') {
        return false;
    }

    let header = line.split('{').next().unwrap_or(line).trim();
    if header.is_empty() {
        return false;
    }

    let mut saw_func = false;
    let mut last_token = None;
    for token in header.split_whitespace() {
        if token == "func" {
            saw_func = true;
        }
        last_token = Some(token);
    }

    saw_func || last_token == Some("main")
}

fn data_symbol_byte_count(object: &O65Object, symbol: &k816_o65::Symbol) -> Result<u32> {
    let definition = symbol
        .definition
        .as_ref()
        .ok_or_else(|| anyhow::anyhow!("internal linker error: data symbol without definition"))?;
    let SymbolDefinition::Section {
        section: section_name,
        offset,
        source: Some(source),
    } = definition
    else {
        return Err(anyhow::anyhow!(
            "internal linker error: data symbol '{}' must be section-relative",
            symbol.name
        ));
    };
    let section = object.sections.get(section_name).ok_or_else(|| {
        anyhow::anyhow!(
            "data symbol '{}' references unknown section '{}'",
            symbol.name,
            section_name
        )
    })?;

    let mut section_end = 0_u32;
    for chunk in &section.chunks {
        let len = u32::try_from(chunk.bytes.len())
            .context("section chunk length for data listing does not fit in u32")?;
        let chunk_end = chunk
            .offset
            .checked_add(len)
            .ok_or_else(|| anyhow::anyhow!("section chunk range overflow in data listing"))?;
        section_end = section_end.max(chunk_end);
    }

    let current_key = (*offset, source.line, source.column, symbol.name.as_str());
    let mut next_key = (section_end, u32::MAX, u32::MAX, "\u{10FFFF}");
    for candidate in &object.symbols {
        let Some(candidate_definition) = candidate.definition.as_ref() else {
            continue;
        };
        let SymbolDefinition::Section {
            section: candidate_section,
            offset: candidate_offset,
            source: Some(candidate_source),
        } = candidate_definition
        else {
            continue;
        };
        if candidate_section != section_name {
            continue;
        }
        if !is_named_data_symbol(candidate) && !is_top_level_code_block_symbol(candidate) {
            continue;
        }

        let candidate_key = (
            *candidate_offset,
            candidate_source.line,
            candidate_source.column,
            candidate.name.as_str(),
        );
        if candidate_key > current_key && candidate_key < next_key {
            next_key = candidate_key;
        }
    }

    next_key.0.checked_sub(*offset).ok_or_else(|| {
        anyhow::anyhow!(
            "data symbol '{}' has invalid range in section '{}'",
            symbol.name,
            section_name
        )
    })
}

fn symbol_source_location(definition: &SymbolDefinition) -> Option<&SourceLocation> {
    match definition {
        SymbolDefinition::Section { source, .. } | SymbolDefinition::Absolute { source, .. } => {
            source.as_ref()
        }
    }
}

#[derive(Debug)]
enum DataSymbolPart {
    Literal { offset: u32, text: String },
    Bytes { offset: u32, len: u32 },
}

fn build_data_symbol_parts(
    object: &O65Object,
    section: &str,
    start_offset: u32,
    byte_count: u32,
) -> Result<Vec<DataSymbolPart>> {
    if byte_count == 0 {
        return Ok(vec![DataSymbolPart::Bytes {
            offset: start_offset,
            len: 0,
        }]);
    }

    let end_offset = start_offset
        .checked_add(byte_count)
        .ok_or_else(|| anyhow::anyhow!("data symbol range overflow"))?;
    let mut parts = Vec::new();
    let mut cursor = start_offset;
    let mut fragments = object
        .data_string_fragments
        .iter()
        .filter(|fragment| fragment.section == section)
        .collect::<Vec<_>>();
    fragments.sort_by_key(|fragment| fragment.offset);

    for fragment in fragments {
        if fragment.offset < start_offset || fragment.offset >= end_offset {
            continue;
        }
        if fragment.offset < cursor {
            continue;
        }

        if fragment.offset > cursor {
            parts.push(DataSymbolPart::Bytes {
                offset: cursor,
                len: fragment.offset - cursor,
            });
        }

        let text_len: u32 = fragment
            .text
            .len()
            .try_into()
            .context("data string fragment length does not fit in u32")?;
        let fragment_end = fragment
            .offset
            .checked_add(text_len)
            .ok_or_else(|| anyhow::anyhow!("data string fragment range overflow"))?;
        if fragment_end > end_offset {
            break;
        }

        parts.push(DataSymbolPart::Literal {
            offset: fragment.offset,
            text: fragment.text.clone(),
        });
        cursor = fragment_end;
    }

    if cursor < end_offset {
        parts.push(DataSymbolPart::Bytes {
            offset: cursor,
            len: end_offset - cursor,
        });
    }

    Ok(parts)
}

fn escape_data_literal_for_listing(text: &str) -> String {
    let mut escaped = String::with_capacity(text.len());
    for ch in text.chars() {
        match ch {
            '\\' => escaped.push_str("\\\\"),
            '"' => escaped.push_str("\\\""),
            '\n' => escaped.push_str("\\n"),
            '\r' => escaped.push_str("\\r"),
            '\t' => escaped.push_str("\\t"),
            _ => escaped.push(ch),
        }
    }
    escaped
}

fn format_data_symbol_listing_block(
    object_index: usize,
    object: &O65Object,
    section: &str,
    symbol: &str,
    start_offset: u32,
    byte_count: u32,
    placements: &[PlacedChunk],
) -> Result<String> {
    let parts = build_data_symbol_parts(object, section, start_offset, byte_count)?;

    let mut out = String::new();
    out.push_str(&format!("[data obj#{object_index} {section}::{symbol}]\n"));
    for part in parts {
        match part {
            DataSymbolPart::Literal { offset, text } => {
                let address = resolve_symbol_addr(placements, offset).ok_or_else(|| {
                    anyhow::anyhow!(
                        "data symbol '{}' offset {:#X} is outside section '{}'",
                        symbol,
                        offset,
                        section
                    )
                })?;
                out.push_str(&format!(
                    "{address:06X}: \"{}\"\n",
                    escape_data_literal_for_listing(&text)
                ));
            }
            DataSymbolPart::Bytes { offset, len } => {
                let address = resolve_symbol_addr(placements, offset).ok_or_else(|| {
                    anyhow::anyhow!(
                        "data symbol '{}' offset {:#X} is outside section '{}'",
                        symbol,
                        offset,
                        section
                    )
                })?;
                out.push_str(&format!("{address:06X}: <{len} bytes of data>\n"));
            }
        }
    }
    if out.ends_with('\n') {
        out.pop();
    }
    Ok(out)
}

pub(super) fn format_function_disassembly_listings(
    objects: &[O65Object],
    placed_by_section: &HashMap<(usize, String), Vec<PlacedChunk>>,
    memory_map: &HashMap<String, MemoryState>,
) -> Result<Vec<String>> {
    let mut blocks = Vec::new();
    for (object_index, object) in objects.iter().enumerate() {
        for function in &object.function_disassembly {
            if function.instruction_offsets.is_empty() {
                continue;
            }

            let key = (object_index, function.section.clone());
            let Some(placements) = placed_by_section.get(&key) else {
                continue;
            };

            blocks.push(format_function_disassembly_block(
                object_index,
                function,
                placements,
                memory_map,
            )?);
        }
    }

    Ok(blocks)
}

fn format_function_disassembly_block(
    object_index: usize,
    function: &k816_o65::FunctionDisassembly,
    placements: &[PlacedChunk],
    memory_map: &HashMap<String, MemoryState>,
) -> Result<String> {
    let mut out = String::new();
    out.push_str(&format!(
        "[disasm obj#{object_index} {}::{}]\n",
        function.section, function.function
    ));

    let mut m_wide = function.m_wide;
    let mut x_wide = function.x_wide;

    for offset in &function.instruction_offsets {
        let (memory_name, address) =
            resolve_reloc_site(placements, *offset, 1).ok_or_else(|| {
                anyhow::anyhow!(
                    "disassembly site {:#X} is outside section '{}'",
                    offset,
                    function.section
                )
            })?;

        let memory = memory_map.get(memory_name).ok_or_else(|| {
            anyhow::anyhow!(
                "internal linker error: memory '{}' missing for disassembly",
                memory_name
            )
        })?;

        if address < memory.spec.start {
            bail!("disassembly address underflows memory range");
        }

        let index = (address - memory.spec.start) as usize;
        let decode_slice = &memory.bytes[index..];
        let decoded =
            decode_instruction_with_mode(decode_slice, m_wide, x_wide).map_err(|err| {
                anyhow::anyhow!(
                    "failed to decode instruction at {address:#X} ({}::{}): {err}",
                    function.section,
                    function.function
                )
            })?;

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
        let end = index.saturating_add(len);
        if end > memory.bytes.len() {
            bail!(
                "decoded instruction at {address:#X} extends outside memory '{}'",
                memory_name
            );
        }

        let raw = &memory.bytes[index..end];
        let hex = raw
            .iter()
            .map(|byte| format!("{byte:02X}"))
            .collect::<Vec<_>>()
            .join(" ");
        let text = format_instruction(&decoded, address);
        out.push_str(&format!("{address:06X}: {hex:<11} {text}\n"));
    }

    if out.ends_with('\n') {
        out.pop();
    }

    Ok(out)
}

pub(super) fn format_empty_listing_block(
    object_index: usize,
    name: &str,
    include_object_index: bool,
) -> String {
    format!(
        "{}\n(empty)\n",
        format_section_header(object_index, name, include_object_index)
    )
}

fn format_section_header(object_index: usize, segment: &str, include_object_index: bool) -> String {
    if include_object_index {
        return format!("[obj#{object_index} {segment}]");
    }
    format!("[{segment}]")
}
