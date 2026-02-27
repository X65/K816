mod anchors;

use anyhow::{Context, Result, bail};
use k816_o65::{
    CallMetadata, FunctionMetadata, O65Object, RelocationKind, SourceLocation, SymbolDefinition,
};
use std::collections::HashMap;

use self::anchors::{
    decorate_with_anchor, decorate_with_anchor_with_label, find_anchor_context,
    find_section_anchor_context,
};
use super::config::{
    LinkerConfig, MemoryArea, SymbolValue, select_segment_rule, validate_segment_rules,
};
use super::listing::{
    format_data_symbol_listings, format_empty_listing_block, format_function_disassembly_listings,
    format_section_listing,
};
use super::render::{collect_linked_runs, resolve_reloc_site, resolve_symbol_addr};
use super::types::{LinkRenderOptions, LinkedLayout, PlacedChunk};

#[derive(Debug, Clone)]
pub(super) struct MemoryState {
    pub(super) spec: MemoryArea,
    pub(super) bytes: Vec<u8>,
    pub(super) used: Vec<bool>,
    pub(super) cursor: u32,
}

#[derive(Debug, Clone)]
struct PlannedChunk {
    obj_idx: usize,
    segment: String,
    chunk_idx: usize,
    section_offset: u32,
    len: u32,
    memory_name: String,
    align: u32,
    start: Option<u32>,
    offset: Option<u32>,
    fixed_addr: Option<u32>,
}

#[derive(Debug, Clone)]
struct ResolvedSymbol {
    addr: u32,
    segment: String,
}

#[derive(Debug, Clone)]
struct AnchorContext {
    symbol_name: String,
    source: Option<SourceLocation>,
}

const ABSOLUTE_SYMBOL_SEGMENT: &str = "__absolute__";

pub fn link_objects(objects: &[O65Object], config: &LinkerConfig) -> Result<LinkedLayout> {
    link_objects_with_options(objects, config, LinkRenderOptions::plain())
}

pub fn link_objects_with_options(
    objects: &[O65Object],
    config: &LinkerConfig,
    options: LinkRenderOptions,
) -> Result<LinkedLayout> {
    if config.memory.is_empty() {
        bail!("linker config must contain at least one memory area");
    }
    validate_segment_rules(config)?;

    let mut memory_map: HashMap<String, MemoryState> = HashMap::new();
    for mem in &config.memory {
        let size = mem.size as usize;
        let fill = mem.fill.unwrap_or(0);
        memory_map.insert(
            mem.name.clone(),
            MemoryState {
                spec: mem.clone(),
                bytes: vec![fill; size],
                used: vec![false; size],
                cursor: mem.start,
            },
        );
    }

    let mut section_order = Vec::new();
    let mut placed_by_section: HashMap<(usize, String), Vec<PlacedChunk>> = HashMap::new();
    let mut planned = Vec::new();

    for (obj_idx, object) in objects.iter().enumerate() {
        for (segment, section) in &object.sections {
            section_order.push((obj_idx, segment.clone()));
            placed_by_section
                .entry((obj_idx, segment.clone()))
                .or_default();

            let rule = select_segment_rule(config, segment)?;
            let align = rule.align.unwrap_or(1);
            if align == 0 {
                bail!("segment rule id '{}' has align=0", rule.id);
            }

            for (chunk_idx, chunk) in section.chunks.iter().enumerate() {
                if chunk.bytes.is_empty() {
                    continue;
                }

                let len = u32::try_from(chunk.bytes.len()).context("section chunk is too large")?;
                planned.push(PlannedChunk {
                    obj_idx,
                    segment: segment.clone(),
                    chunk_idx,
                    section_offset: chunk.offset,
                    len,
                    memory_name: rule.load.clone(),
                    align,
                    start: rule.start,
                    offset: rule.offset,
                    fixed_addr: chunk.address,
                });
            }
        }
    }

    for chunk in planned.iter().filter(|chunk| chunk.fixed_addr.is_some()) {
        let base = chunk
            .fixed_addr
            .expect("filtered fixed chunk must have address");
        place_chunk(
            chunk,
            base,
            objects,
            &mut memory_map,
            &mut placed_by_section,
            true,
            options,
        )?;
    }

    for chunk in planned.iter().filter(|chunk| chunk.fixed_addr.is_none()) {
        let base = choose_reloc_base(chunk, objects, &mut memory_map, options)?;
        place_chunk(
            chunk,
            base,
            objects,
            &mut memory_map,
            &mut placed_by_section,
            false,
            options,
        )?;
    }

    for chunks in placed_by_section.values_mut() {
        chunks.sort_by_key(|chunk| chunk.section_offset);
    }

    let mut symbols: HashMap<String, ResolvedSymbol> = HashMap::new();
    for (obj_idx, object) in objects.iter().enumerate() {
        for symbol in &object.symbols {
            let Some(def) = &symbol.definition else {
                continue;
            };

            let resolved = match def {
                SymbolDefinition::Section {
                    section, offset, ..
                } => {
                    let section_key = (obj_idx, section.clone());
                    let placements = placed_by_section.get(&section_key).ok_or_else(|| {
                        anyhow::anyhow!(
                            "symbol '{}' references unknown section '{}'",
                            symbol.name,
                            section
                        )
                    })?;

                    let addr = resolve_symbol_addr(placements, *offset).ok_or_else(|| {
                        anyhow::anyhow!(
                            "symbol '{}' offset {:#X} is outside section '{}'",
                            symbol.name,
                            offset,
                            section
                        )
                    })?;

                    ResolvedSymbol {
                        addr,
                        segment: section.clone(),
                    }
                }
                SymbolDefinition::Absolute { address, .. } => ResolvedSymbol {
                    addr: *address,
                    segment: ABSOLUTE_SYMBOL_SEGMENT.to_string(),
                },
            };
            if symbols.insert(symbol.name.clone(), resolved).is_some() {
                bail!("duplicate global symbol '{}'", symbol.name);
            }
        }
    }

    for link_symbol in &config.symbols {
        let value = match &link_symbol.value {
            SymbolValue::Absolute(addr) => ResolvedSymbol {
                addr: *addr,
                segment: ABSOLUTE_SYMBOL_SEGMENT.to_string(),
            },
            SymbolValue::Import(name) => symbols.get(name).cloned().ok_or_else(|| {
                anyhow::anyhow!(
                    "link symbol '{}' imports undefined symbol '{}'",
                    link_symbol.name,
                    name
                )
            })?,
        };
        symbols.insert(link_symbol.name.clone(), value);
    }

    let mut function_metadata_map: HashMap<String, FunctionMetadata> = HashMap::new();
    for object in objects {
        for symbol in &object.symbols {
            if let Some(meta) = &symbol.function_metadata {
                function_metadata_map.insert(symbol.name.clone(), *meta);
            }
        }
    }

    validate_call_metadata(objects, &symbols, &function_metadata_map, options)?;

    for (obj_idx, object) in objects.iter().enumerate() {
        for reloc in &object.relocations {
            let section_key = (obj_idx, reloc.section.clone());
            let placements = placed_by_section.get(&section_key).ok_or_else(|| {
                anyhow::anyhow!("relocation references unknown section '{}'", reloc.section)
            })?;

            let (site_memory_name, site_addr) =
                resolve_reloc_site(placements, reloc.offset, reloc.width).ok_or_else(|| {
                    anyhow::anyhow!(
                        "relocation site {:#X}..{:#X} is outside section '{}'",
                        reloc.offset,
                        reloc.offset.saturating_add(u32::from(reloc.width)),
                        reloc.section
                    )
                })?;

            let target = symbols.get(&reloc.symbol).cloned().ok_or_else(|| {
                let detail = format!("undefined symbol '{}'", reloc.symbol);
                let label_message = format!("symbol '{}' referenced here", reloc.symbol);
                let anchor = reloc
                    .source
                    .as_ref()
                    .map(|source| AnchorContext {
                        symbol_name: reloc.symbol.clone(),
                        source: Some(source.clone()),
                    })
                    .or_else(|| {
                        find_section_anchor_context(objects, obj_idx, &reloc.section, reloc.offset)
                    });
                anyhow::anyhow!(
                    "{}",
                    decorate_with_anchor_with_label(
                        &detail,
                        anchor.as_ref(),
                        options,
                        Some(&label_message),
                        None,
                    )
                )
            })?;

            if reloc.kind == RelocationKind::Absolute
                && reloc.width == 2
                && target.segment != ABSOLUTE_SYMBOL_SEGMENT
                && reloc.section != target.segment
                && reloc.call_metadata.is_some()
            {
                bail!(
                    "16-bit relocation from segment '{}' to '{}' requires far addressing",
                    reloc.section,
                    target.segment
                );
            }

            let mem = memory_map.get_mut(site_memory_name).ok_or_else(|| {
                anyhow::anyhow!(
                    "internal linker error: memory '{}' missing",
                    site_memory_name
                )
            })?;

            if site_addr < mem.spec.start {
                bail!("relocation site underflows memory range");
            }

            let rel_idx = (site_addr - mem.spec.start) as usize;
            let end_idx = rel_idx.saturating_add(reloc.width as usize);
            if end_idx > mem.bytes.len() {
                bail!("relocation writes outside memory range");
            }

            let effective_addr = (target.addr as i64 + reloc.addend as i64) as u32;
            match reloc.kind {
                RelocationKind::Absolute => write_value(
                    &mut mem.bytes[rel_idx..end_idx],
                    effective_addr,
                    reloc.width,
                )?,
                RelocationKind::Relative => match reloc.width {
                    1 => {
                        let delta = effective_addr as i64 - (site_addr as i64 + 1);
                        if delta < i8::MIN as i64 || delta > i8::MAX as i64 {
                            bail!(
                                "relative relocation to '{}' out of range at {:#X}",
                                reloc.symbol,
                                site_addr
                            );
                        }
                        mem.bytes[rel_idx] = delta as i8 as u8;
                    }
                    2 => {
                        let delta = effective_addr as i64 - (site_addr as i64 + 2);
                        if delta < i16::MIN as i64 || delta > i16::MAX as i64 {
                            bail!(
                                "relative relocation to '{}' out of range at {:#X}",
                                reloc.symbol,
                                site_addr
                            );
                        }
                        mem.bytes[rel_idx..end_idx].copy_from_slice(&(delta as i16).to_le_bytes());
                    }
                    _ => bail!("relative relocation width must be 1 or 2 bytes"),
                },
                RelocationKind::LowByte => {
                    if reloc.width != 1 {
                        bail!("low-byte relocation width must be 1");
                    }
                    mem.bytes[rel_idx] = (effective_addr & 0xFF) as u8;
                }
                RelocationKind::HighByte => {
                    if reloc.width != 1 {
                        bail!("high-byte relocation width must be 1");
                    }
                    mem.bytes[rel_idx] = ((effective_addr >> 8) & 0xFF) as u8;
                }
            }
        }
    }

    let runs = collect_linked_runs(config, &memory_map)?;

    let mut listing_blocks = Vec::new();
    let include_object_index = objects.len() > 1;
    for key in &section_order {
        let object_index = key.0;
        let segment_name = &key.1;
        let Some(chunks) = placed_by_section.get(key) else {
            continue;
        };

        if chunks.is_empty() {
            listing_blocks.push(format_empty_listing_block(
                object_index,
                segment_name,
                include_object_index,
            ));
            continue;
        }

        listing_blocks.push(format_section_listing(
            object_index,
            segment_name,
            chunks,
            &memory_map,
            include_object_index,
        )?);
    }
    listing_blocks.extend(format_data_symbol_listings(objects, &placed_by_section)?);
    listing_blocks.extend(format_function_disassembly_listings(
        objects,
        &placed_by_section,
        &memory_map,
    )?);

    Ok(LinkedLayout {
        runs,
        listing: listing_blocks.join("\n\n"),
        symbols: symbols
            .into_iter()
            .map(|(name, rs)| (name, rs.addr))
            .collect(),
        section_placements: placed_by_section,
    })
}

fn validate_call_metadata(
    objects: &[O65Object],
    symbols: &HashMap<String, ResolvedSymbol>,
    function_metadata_map: &HashMap<String, FunctionMetadata>,
    options: LinkRenderOptions,
) -> Result<()> {
    let mut errors: Vec<String> = Vec::new();

    for (obj_idx, object) in objects.iter().enumerate() {
        for reloc in &object.relocations {
            let Some(call_meta) = &reloc.call_metadata else {
                continue;
            };

            if !symbols.contains_key(&reloc.symbol) {
                continue;
            }

            let Some(func_meta) = function_metadata_map.get(&reloc.symbol) else {
                continue;
            };

            let anchor = reloc
                .source
                .as_ref()
                .map(|source| AnchorContext {
                    symbol_name: reloc.symbol.clone(),
                    source: Some(source.clone()),
                })
                .or_else(|| {
                    find_section_anchor_context(objects, obj_idx, &reloc.section, reloc.offset)
                });

            if reloc.width == 2 && func_meta.is_far {
                let detail = format!("near call to far function '{}'", reloc.symbol);
                let help = format!("use `call far {}` instead", reloc.symbol);
                errors.push(decorate_with_anchor_with_label(
                    &detail,
                    anchor.as_ref(),
                    options,
                    Some(&detail),
                    Some(&help),
                ));
            } else if reloc.width == 3 && !func_meta.is_far {
                let detail = format!("far call to near function '{}'", reloc.symbol);
                let help = format!("use `call {}` instead", reloc.symbol);
                errors.push(decorate_with_anchor_with_label(
                    &detail,
                    anchor.as_ref(),
                    options,
                    Some(&detail),
                    Some(&help),
                ));
            }

            check_width_mismatch(
                call_meta,
                func_meta,
                &reloc.symbol,
                anchor.as_ref(),
                options,
                &mut errors,
            );
        }
    }

    if errors.is_empty() {
        Ok(())
    } else {
        bail!("{}", errors.join("\n\n"));
    }
}

fn check_width_mismatch(
    call_meta: &CallMetadata,
    func_meta: &FunctionMetadata,
    symbol: &str,
    anchor: Option<&AnchorContext>,
    options: LinkRenderOptions,
    errors: &mut Vec<String>,
) {
    fn width_name(wide: bool) -> &'static str {
        if wide { "16-bit" } else { "8-bit" }
    }

    if let (Some(caller_a), Some(callee_a)) = (call_meta.caller_a_width, func_meta.a_width)
        && caller_a != callee_a
    {
        let detail = format!(
            "accumulator width mismatch calling '{}': caller is {}, callee expects {}",
            symbol,
            width_name(caller_a),
            width_name(callee_a),
        );
        let label = format!(
            "caller A={}, callee '{}' expects A={}",
            width_name(caller_a),
            symbol,
            width_name(callee_a),
        );
        errors.push(decorate_with_anchor_with_label(
            &detail,
            anchor,
            options,
            Some(&label),
            None,
        ));
    }

    if let (Some(caller_i), Some(callee_i)) = (call_meta.caller_i_width, func_meta.i_width)
        && caller_i != callee_i
    {
        let detail = format!(
            "index register width mismatch calling '{}': caller is {}, callee expects {}",
            symbol,
            width_name(caller_i),
            width_name(callee_i),
        );
        let label = format!(
            "caller I={}, callee '{}' expects I={}",
            width_name(caller_i),
            symbol,
            width_name(callee_i),
        );
        errors.push(decorate_with_anchor_with_label(
            &detail,
            anchor,
            options,
            Some(&label),
            None,
        ));
    }
}

fn choose_reloc_base(
    chunk: &PlannedChunk,
    objects: &[O65Object],
    memory_map: &mut HashMap<String, MemoryState>,
    options: LinkRenderOptions,
) -> Result<u32> {
    let anchor = find_anchor_context(objects, chunk);
    let mem = memory_map
        .get_mut(&chunk.memory_name)
        .ok_or_else(|| anyhow::anyhow!("unknown memory area '{}'", chunk.memory_name))?;

    let mut base = if let Some(start) = chunk.start {
        start
    } else {
        mem.cursor.max(mem.spec.start)
    };

    base = align_up(base, chunk.align);

    if let Some(offset) = chunk.offset {
        base = base.checked_add(offset).ok_or_else(|| {
            anyhow::anyhow!("segment placement overflow in memory '{}'", mem.spec.name)
        })?;
    }

    if chunk.start.is_some() {
        return Ok(base);
    }

    find_first_fit(mem, base, chunk.len, chunk.align).ok_or_else(|| {
        let detail = format!(
            "no free range found for relocatable chunk in segment '{}' (len={:#X})",
            chunk.segment, chunk.len
        );
        anyhow::anyhow!(
            "{}",
            decorate_with_anchor(&detail, anchor.as_ref(), options)
        )
    })
}

fn find_first_fit(mem: &MemoryState, min_addr: u32, len: u32, align: u32) -> Option<u32> {
    if len == 0 {
        return Some(min_addr.max(mem.spec.start));
    }

    let mem_end = mem.spec.start.checked_add(mem.spec.size)?;
    let max_start = mem_end.checked_sub(len)?;

    let mut candidate = align_up(min_addr.max(mem.spec.start), align);
    while candidate <= max_start {
        let rel_start = (candidate - mem.spec.start) as usize;
        let rel_end = rel_start.checked_add(len as usize)?;

        if rel_end > mem.used.len() {
            return None;
        }

        let mut conflict_pos = None;
        for (idx, used) in mem.used[rel_start..rel_end].iter().enumerate() {
            if *used {
                conflict_pos = Some(idx);
                break;
            }
        }

        if let Some(conflict_pos) = conflict_pos {
            let next = mem
                .spec
                .start
                .checked_add((rel_start + conflict_pos + 1) as u32)?;
            candidate = align_up(next, align);
            continue;
        }

        return Some(candidate);
    }

    None
}

fn place_chunk(
    chunk: &PlannedChunk,
    base: u32,
    objects: &[O65Object],
    memory_map: &mut HashMap<String, MemoryState>,
    placed_by_section: &mut HashMap<(usize, String), Vec<PlacedChunk>>,
    fixed: bool,
    options: LinkRenderOptions,
) -> Result<()> {
    let anchor = find_anchor_context(objects, chunk);

    let section = objects
        .get(chunk.obj_idx)
        .and_then(|object| object.sections.get(&chunk.segment))
        .ok_or_else(|| {
            anyhow::anyhow!("internal linker error: missing section '{}'", chunk.segment)
        })?;
    let source_chunk = section.chunks.get(chunk.chunk_idx).ok_or_else(|| {
        anyhow::anyhow!(
            "internal linker error: missing chunk {} in section '{}'",
            chunk.chunk_idx,
            chunk.segment
        )
    })?;

    let mem = memory_map
        .get_mut(&chunk.memory_name)
        .ok_or_else(|| anyhow::anyhow!("unknown memory area '{}'", chunk.memory_name))?;

    let mem_end = mem
        .spec
        .start
        .checked_add(mem.spec.size)
        .ok_or_else(|| anyhow::anyhow!("memory '{}' range overflow", mem.spec.name))?;

    let end = base
        .checked_add(chunk.len)
        .ok_or_else(|| anyhow::anyhow!("placement overflow for segment '{}'", chunk.segment))?;

    if base < mem.spec.start || end > mem_end {
        let kind = if fixed { "fixed" } else { "relocatable" };
        let detail = format!(
            "{kind} chunk for segment '{}' at {:#X}..{:#X} is outside memory '{}' ({:#X}..{:#X})",
            chunk.segment, base, end, mem.spec.name, mem.spec.start, mem_end
        );
        bail!(
            "{}",
            decorate_with_anchor(&detail, anchor.as_ref(), options)
        );
    }

    let rel_start = (base - mem.spec.start) as usize;
    let rel_end = rel_start
        .checked_add(chunk.len as usize)
        .ok_or_else(|| anyhow::anyhow!("placement index overflow"))?;

    if rel_end > mem.bytes.len() {
        let detail = format!(
            "segment '{}' chunk placement exceeds memory '{}': start={:#X}, len={:#X}",
            chunk.segment, mem.spec.name, base, chunk.len
        );
        bail!(
            "{}",
            decorate_with_anchor(&detail, anchor.as_ref(), options)
        );
    }

    if let Some(conflict) = mem.used[rel_start..rel_end].iter().position(|used| *used) {
        let overlap_addr = base + conflict as u32;
        if fixed {
            let detail = format!(
                "fixed chunk for segment '{}' at {:#X}..{:#X} overlaps existing placement near {:#X} in memory '{}'",
                chunk.segment, base, end, overlap_addr, mem.spec.name
            );
            bail!(
                "{}",
                decorate_with_anchor(&detail, anchor.as_ref(), options)
            );
        }

        let detail = format!(
            "relocatable chunk for segment '{}' cannot be placed at {:#X}..{:#X}; overlap near {:#X} in memory '{}'",
            chunk.segment, base, end, overlap_addr, mem.spec.name
        );
        bail!(
            "{}",
            decorate_with_anchor(&detail, anchor.as_ref(), options)
        );
    }

    mem.bytes[rel_start..rel_end].copy_from_slice(&source_chunk.bytes);
    for used in &mut mem.used[rel_start..rel_end] {
        *used = true;
    }

    mem.cursor = mem.cursor.max(end);

    let key = (chunk.obj_idx, chunk.segment.clone());
    placed_by_section.entry(key).or_default().push(PlacedChunk {
        section_offset: chunk.section_offset,
        len: chunk.len,
        base_addr: base,
        memory_name: chunk.memory_name.clone(),
    });

    Ok(())
}

fn align_up(value: u32, align: u32) -> u32 {
    if align <= 1 {
        return value;
    }
    let rem = value % align;
    if rem == 0 {
        value
    } else {
        value + (align - rem)
    }
}

fn write_value(slot: &mut [u8], value: u32, width: u8) -> Result<()> {
    match width {
        1 => {
            let v = u8::try_from(value).context("absolute relocation does not fit in 8 bits")?;
            slot[0] = v;
        }
        2 => {
            let v = u16::try_from(value).context("absolute relocation does not fit in 16 bits")?;
            slot.copy_from_slice(&v.to_le_bytes());
        }
        3 => {
            if value > 0x00FF_FFFF {
                bail!("absolute relocation does not fit in 24 bits");
            }
            let bytes = value.to_le_bytes();
            slot.copy_from_slice(&bytes[..3]);
        }
        _ => bail!("unsupported relocation width: {width}"),
    }
    Ok(())
}
