use super::*;

pub(super) fn validate_object(object: &O65Object) -> Result<()> {
    for (section_name, section) in &object.sections {
        validate_section(section_name, section)?;
    }

    for symbol in &object.symbols {
        let Some(definition) = &symbol.definition else {
            continue;
        };

        if let SymbolDefinition::Section {
            section, offset, ..
        } = definition
        {
            let section_data = object.sections.get(section).ok_or_else(|| {
                anyhow::anyhow!(
                    "symbol '{}' references unknown section '{}'",
                    symbol.name,
                    section
                )
            })?;

            if !section_contains_point(section_data, *offset)? {
                bail!(
                    "symbol '{}' offset {:#X} is outside section '{}'",
                    symbol.name,
                    offset,
                    section
                );
            }
        }
    }

    for reloc in &object.relocations {
        if reloc.width == 0 {
            bail!("relocation in section '{}' has zero width", reloc.section);
        }
        if matches!(
            reloc.kind,
            RelocationKind::LowByte | RelocationKind::HighByte
        ) && reloc.width != 1
        {
            bail!(
                "low/high-byte relocation in section '{}' must have width 1",
                reloc.section
            );
        }

        let section = object.sections.get(&reloc.section).ok_or_else(|| {
            anyhow::anyhow!("relocation references unknown section '{}'", reloc.section)
        })?;

        let end = reloc
            .offset
            .checked_add(u32::from(reloc.width))
            .ok_or_else(|| {
                anyhow::anyhow!("relocation range overflow in section '{}'", reloc.section)
            })?;

        if !section_contains_range(section, reloc.offset, end)? {
            bail!(
                "relocation site {:#X}..{:#X} is outside section '{}'",
                reloc.offset,
                end,
                reloc.section
            );
        }
    }

    for function in &object.function_disassembly {
        let section = object.sections.get(&function.section).ok_or_else(|| {
            anyhow::anyhow!(
                "function disassembly '{}' references unknown section '{}'",
                function.function,
                function.section
            )
        })?;

        for offset in &function.instruction_offsets {
            if !section_contains_instruction_start(section, *offset)? {
                bail!(
                    "function disassembly '{}' offset {:#X} is outside section '{}'",
                    function.function,
                    offset,
                    function.section
                );
            }
        }
    }

    for fragment in &object.data_string_fragments {
        let section = object.sections.get(&fragment.section).ok_or_else(|| {
            anyhow::anyhow!(
                "data string fragment at offset {:#X} references unknown section '{}'",
                fragment.offset,
                fragment.section
            )
        })?;

        let text_len: u32 = fragment
            .text
            .len()
            .try_into()
            .context("data string fragment length does not fit in u32")?;
        let end = fragment.offset.checked_add(text_len).ok_or_else(|| {
            anyhow::anyhow!(
                "data string fragment at offset {:#X} overflows section '{}'",
                fragment.offset,
                fragment.section
            )
        })?;

        if text_len == 0 {
            if !section_contains_point(section, fragment.offset)? {
                bail!(
                    "data string fragment at offset {:#X} is outside section '{}'",
                    fragment.offset,
                    fragment.section
                );
            }
        } else if !section_contains_range(section, fragment.offset, end)? {
            bail!(
                "data string fragment range {:#X}..{:#X} is outside section '{}'",
                fragment.offset,
                end,
                fragment.section
            );
        }
    }

    Ok(())
}

fn validate_section(section_name: &str, section: &Section) -> Result<()> {
    let mut prev_end: Option<u32> = None;

    for (index, chunk) in section.chunks.iter().enumerate() {
        if chunk.bytes.is_empty() {
            bail!(
                "section '{}' chunk {} at offset {:#X} is empty",
                section_name,
                index,
                chunk.offset
            );
        }

        let len =
            u32::try_from(chunk.bytes.len()).context("section chunk length does not fit in u32")?;
        let chunk_end = chunk.offset.checked_add(len).ok_or_else(|| {
            anyhow::anyhow!(
                "section '{}' chunk {} range overflows u32",
                section_name,
                index
            )
        })?;

        if let Some(prev_end) = prev_end
            && chunk.offset < prev_end {
                bail!(
                    "section '{}' has overlapping chunks around offset {:#X}",
                    section_name,
                    chunk.offset
                );
            }

        prev_end = Some(chunk_end);
    }

    Ok(())
}

fn section_contains_point(section: &Section, point: u32) -> Result<bool> {
    for chunk in &section.chunks {
        let end = chunk
            .offset
            .checked_add(
                u32::try_from(chunk.bytes.len())
                    .context("section chunk length does not fit in u32")?,
            )
            .ok_or_else(|| anyhow::anyhow!("section chunk range overflow"))?;

        if point >= chunk.offset && point <= end {
            return Ok(true);
        }
    }
    Ok(false)
}

fn section_contains_instruction_start(section: &Section, offset: u32) -> Result<bool> {
    for chunk in &section.chunks {
        let end = chunk
            .offset
            .checked_add(
                u32::try_from(chunk.bytes.len())
                    .context("section chunk length does not fit in u32")?,
            )
            .ok_or_else(|| anyhow::anyhow!("section chunk range overflow"))?;

        if offset >= chunk.offset && offset < end {
            return Ok(true);
        }
    }

    Ok(false)
}

fn section_contains_range(section: &Section, start: u32, end: u32) -> Result<bool> {
    for chunk in &section.chunks {
        let chunk_end = chunk
            .offset
            .checked_add(
                u32::try_from(chunk.bytes.len())
                    .context("section chunk length does not fit in u32")?,
            )
            .ok_or_else(|| anyhow::anyhow!("section chunk range overflow"))?;

        if start >= chunk.offset && end <= chunk_end {
            return Ok(true);
        }
    }
    Ok(false)
}
