use anyhow::{Context, Result, bail};
use ariadne::{Cache, Config, IndexType, Label, Report, ReportKind, Source};
use k816_isa65816::{decode_instruction_with_mode, format_instruction};
use k816_o65::{O65Object, RelocationKind, SourceLocation, SymbolDefinition};
use serde::Deserialize;
use std::collections::HashMap;
use std::fmt;
use std::path::Path;

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct LinkerConfig {
    #[serde(default = "default_format")]
    pub format: String,
    #[serde(default)]
    pub target: Option<String>,
    pub memory: Vec<MemoryArea>,
    #[serde(default)]
    pub segments: Vec<SegmentRule>,
    #[serde(default)]
    pub symbols: Vec<LinkSymbol>,
    #[serde(default)]
    pub output: OutputSpec,
    #[serde(default)]
    pub entry: Option<String>,
}

fn default_format() -> String {
    "o65-link".to_string()
}

fn default_output_kind() -> OutputKind {
    OutputKind::Xex
}

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct MemoryArea {
    pub name: String,
    pub start: u32,
    pub size: u32,
    pub kind: MemoryKind,
    #[serde(default)]
    pub fill: Option<u8>,
}

#[derive(Debug, Clone, Copy, Deserialize)]
pub enum MemoryKind {
    ReadOnly,
    ReadWrite,
}

#[derive(Debug, Clone, Deserialize)]
pub struct SegmentRule {
    pub id: String,
    pub load: String,
    #[serde(default)]
    pub run: Option<String>,
    #[serde(default)]
    pub align: Option<u32>,
    #[serde(default)]
    pub start: Option<u32>,
    #[serde(default)]
    pub offset: Option<u32>,
    #[serde(default)]
    pub optional: bool,
    #[serde(default)]
    pub segment: Option<String>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct LinkSymbol {
    pub name: String,
    pub value: SymbolValue,
}

#[derive(Debug, Clone, Deserialize)]
pub enum SymbolValue {
    Absolute(u32),
    Import(String),
}

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct OutputSpec {
    #[serde(default = "default_output_kind")]
    pub kind: OutputKind,
    #[serde(default)]
    pub file: Option<String>,
}

impl Default for OutputSpec {
    fn default() -> Self {
        Self {
            kind: default_output_kind(),
            file: None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
pub enum OutputKind {
    RawBinary,
    Xex,
}

#[derive(Debug, Clone)]
pub struct LinkOutput {
    pub bytes: Vec<u8>,
    pub kind: OutputKind,
    pub listing: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LinkRenderOptions {
    pub color: bool,
}

impl LinkRenderOptions {
    pub const fn plain() -> Self {
        Self { color: false }
    }

    pub const fn colored() -> Self {
        Self { color: true }
    }
}

#[derive(Debug, Clone)]
struct MemoryState {
    spec: MemoryArea,
    bytes: Vec<u8>,
    used: Vec<bool>,
    cursor: u32,
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
struct PlacedChunk {
    section_offset: u32,
    len: u32,
    base_addr: u32,
    memory_name: String,
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

pub fn load_config(path: &Path) -> Result<LinkerConfig> {
    let text = std::fs::read_to_string(path)
        .with_context(|| format!("failed to read linker config '{}'", path.display()))?;
    ron::from_str(&text)
        .with_context(|| format!("failed to parse linker config '{}'", path.display()))
}

pub fn default_stub_config() -> LinkerConfig {
    LinkerConfig {
        format: "o65-link".to_string(),
        target: Some("stub".to_string()),
        memory: vec![MemoryArea {
            name: "MAIN".to_string(),
            start: 0,
            size: 0x1_0000,
            kind: MemoryKind::ReadWrite,
            fill: Some(0),
        }],
        segments: vec![SegmentRule {
            id: "DEFAULT".to_string(),
            load: "MAIN".to_string(),
            run: None,
            align: Some(1),
            start: None,
            offset: None,
            optional: false,
            segment: None,
        }],
        symbols: Vec::new(),
        output: OutputSpec::default(),
        entry: None,
    }
}

pub fn link_objects(objects: &[O65Object], config: &LinkerConfig) -> Result<LinkOutput> {
    link_objects_with_options(objects, config, LinkRenderOptions::plain())
}

pub fn link_objects_with_options(
    objects: &[O65Object],
    config: &LinkerConfig,
    options: LinkRenderOptions,
) -> Result<LinkOutput> {
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
                        Some(&label_message)
                    )
                )
            })?;

            if reloc.kind == RelocationKind::Absolute
                && reloc.width == 2
                && target.segment != ABSOLUTE_SYMBOL_SEGMENT
                && reloc.section != target.segment
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

            match reloc.kind {
                RelocationKind::Absolute => {
                    write_value(&mut mem.bytes[rel_idx..end_idx], target.addr, reloc.width)?
                }
                RelocationKind::Relative => match reloc.width {
                    1 => {
                        let delta = target.addr as i64 - (site_addr as i64 + 1);
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
                        let delta = target.addr as i64 - (site_addr as i64 + 2);
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
                    mem.bytes[rel_idx] = (target.addr & 0xFF) as u8;
                }
                RelocationKind::HighByte => {
                    if reloc.width != 1 {
                        bail!("high-byte relocation width must be 1");
                    }
                    mem.bytes[rel_idx] = ((target.addr >> 8) & 0xFF) as u8;
                }
            }
        }
    }

    let (output_kind, bytes) = render_link_output(config, &memory_map)?;

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

    Ok(LinkOutput {
        bytes,
        kind: output_kind,
        listing: listing_blocks.join("\n\n"),
    })
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

fn find_anchor_context(objects: &[O65Object], chunk: &PlannedChunk) -> Option<AnchorContext> {
    find_section_anchor_context(objects, chunk.obj_idx, &chunk.segment, chunk.section_offset)
}

fn find_section_anchor_context(
    objects: &[O65Object],
    obj_idx: usize,
    section_name: &str,
    section_offset: u32,
) -> Option<AnchorContext> {
    let object = objects.get(obj_idx)?;
    let mut best_with_source: Option<(u32, AnchorContext)> = None;
    let mut best_without_source: Option<(u32, String)> = None;

    for symbol in &object.symbols {
        let Some(SymbolDefinition::Section {
            section,
            offset,
            source,
        }) = symbol.definition.as_ref()
        else {
            continue;
        };
        if section != section_name || *offset > section_offset {
            continue;
        }

        if let Some(source) = source {
            let candidate = AnchorContext {
                symbol_name: symbol.name.clone(),
                source: Some(source.clone()),
            };
            if best_with_source
                .as_ref()
                .is_none_or(|(best_offset, _)| *offset >= *best_offset)
            {
                best_with_source = Some((*offset, candidate));
            }
            continue;
        }

        if best_without_source
            .as_ref()
            .is_none_or(|(best_offset, _)| *offset >= *best_offset)
        {
            best_without_source = Some((*offset, symbol.name.clone()));
        }
    }

    if let Some((_, anchor)) = best_with_source {
        return Some(anchor);
    }

    best_without_source.map(|(_, symbol_name)| AnchorContext {
        symbol_name,
        source: None,
    })
}

fn decorate_with_anchor(
    message: &str,
    anchor: Option<&AnchorContext>,
    options: LinkRenderOptions,
) -> String {
    let label_message =
        anchor.map(|anchor| format!("function '{}' defined here", anchor.symbol_name));
    decorate_with_anchor_with_label(message, anchor, options, label_message.as_deref())
}

fn decorate_with_anchor_with_label(
    message: &str,
    anchor: Option<&AnchorContext>,
    options: LinkRenderOptions,
    label_message: Option<&str>,
) -> String {
    let Some(anchor) = anchor else {
        return message.to_string();
    };
    let label_message = label_message.unwrap_or("defined here");

    let Some(source) = &anchor.source else {
        return format!("{message}\n{label_message}");
    };

    let context = render_anchor_context(message, source, options, label_message);
    if context.is_empty() {
        format!(
            "{message}\n{label_message} at {}:{}:{}",
            source.file, source.line, source.column
        )
    } else {
        context
    }
}

fn render_anchor_context(
    message: &str,
    source: &SourceLocation,
    options: LinkRenderOptions,
    label_message: &str,
) -> String {
    let file_id = source.file.clone();
    let line_len = source.line_text.len();
    let mut start = source.column.saturating_sub(1) as usize;
    if start > line_len {
        start = line_len;
    }
    let mut end = source.column_end.saturating_sub(1) as usize;
    if end <= start {
        end = start.saturating_add(1);
    }
    if end > line_len {
        end = line_len;
    }
    if end <= start && line_len > start {
        end = start + 1;
    }

    let mut cache = SingleSourceCache {
        id: file_id.clone(),
        source: Source::from(source.line_text.clone()),
    };
    let mut output = Vec::new();

    let report = Report::build(ReportKind::Error, (file_id.clone(), start..end))
        .with_config(
            Config::default()
                .with_index_type(IndexType::Byte)
                .with_color(options.color),
        )
        .with_message(message.to_string())
        .with_label(
            Label::new((file_id.clone(), start..end)).with_message(label_message.to_string()),
        )
        .finish();

    if report.write(&mut cache, &mut output).is_ok() {
        String::from_utf8_lossy(&output).into_owned()
    } else {
        String::new()
    }
}

#[derive(Debug)]
struct SingleSourceCache {
    id: String,
    source: Source<String>,
}

impl Cache<String> for SingleSourceCache {
    type Storage = String;

    fn fetch(
        &mut self,
        id: &String,
    ) -> std::result::Result<&Source<Self::Storage>, impl fmt::Debug> {
        if id == &self.id {
            Ok::<_, String>(&self.source)
        } else {
            Err::<&Source<Self::Storage>, _>(format!("missing source for '{id}'"))
        }
    }

    fn display<'a>(&self, id: &'a String) -> Option<impl fmt::Display + 'a> {
        Some(id)
    }
}

fn resolve_symbol_addr(placements: &[PlacedChunk], offset: u32) -> Option<u32> {
    for chunk in placements {
        if offset == chunk.section_offset {
            return Some(chunk.base_addr);
        }
    }

    for chunk in placements {
        let end = chunk.section_offset.checked_add(chunk.len)?;
        if offset > chunk.section_offset && offset <= end {
            return chunk.base_addr.checked_add(offset - chunk.section_offset);
        }
    }

    None
}

fn resolve_reloc_site(placements: &[PlacedChunk], offset: u32, width: u8) -> Option<(&str, u32)> {
    if width == 0 {
        return None;
    }

    let end = offset.checked_add(u32::from(width))?;
    for chunk in placements {
        let chunk_end = chunk.section_offset.checked_add(chunk.len)?;
        if offset >= chunk.section_offset && end <= chunk_end {
            let addr = chunk.base_addr.checked_add(offset - chunk.section_offset)?;
            return Some((&chunk.memory_name, addr));
        }
    }

    None
}

fn used_runs(used: &[bool]) -> Vec<(usize, usize)> {
    let mut out = Vec::new();
    let mut index = 0usize;

    while index < used.len() {
        while index < used.len() && !used[index] {
            index += 1;
        }
        if index >= used.len() {
            break;
        }

        let start = index;
        while index < used.len() && used[index] {
            index += 1;
        }
        out.push((start, index));
    }

    out
}

fn render_link_output(
    config: &LinkerConfig,
    memory_map: &HashMap<String, MemoryState>,
) -> Result<(OutputKind, Vec<u8>)> {
    let bytes = match config.output.kind {
        OutputKind::RawBinary => render_raw_output(config, memory_map)?,
        OutputKind::Xex => render_xex_output(config, memory_map)?,
    };
    Ok((config.output.kind, bytes))
}

fn render_raw_output(
    config: &LinkerConfig,
    memory_map: &HashMap<String, MemoryState>,
) -> Result<Vec<u8>> {
    let mut used_states: Vec<(&str, &MemoryState)> = Vec::new();
    let mut used_names = Vec::new();

    for mem in &config.memory {
        let state = memory_map.get(&mem.name).ok_or_else(|| {
            anyhow::anyhow!("internal linker error: memory '{}' missing", mem.name)
        })?;
        if state.used.iter().any(|used| *used) {
            used_states.push((&mem.name, state));
            used_names.push(mem.name.clone());
        }
    }

    match used_states.as_slice() {
        [] => Ok(Vec::new()),
        [(name, state)] => {
            let runs = used_runs(&state.used);
            if runs.len() > 1 {
                bail!(
                    "raw binary output is ambiguous: data placed in multiple ranges in memory area '{}'",
                    name
                );
            }
            Ok(compact_memory_runs(state))
        }
        _ => bail!(
            "raw binary output is ambiguous: data placed in multiple memory areas ({})",
            used_names.join(", ")
        ),
    }
}

fn compact_memory_runs(state: &MemoryState) -> Vec<u8> {
    let mut compact = Vec::new();
    for (start, end) in used_runs(&state.used) {
        compact.extend_from_slice(&state.bytes[start..end]);
    }
    compact
}

fn render_xex_output(
    config: &LinkerConfig,
    memory_map: &HashMap<String, MemoryState>,
) -> Result<Vec<u8>> {
    let mut xex = Vec::new();
    let mut wrote_header = false;

    for mem in &config.memory {
        let state = memory_map.get(&mem.name).ok_or_else(|| {
            anyhow::anyhow!("internal linker error: memory '{}' missing", mem.name)
        })?;

        for (start, end) in used_runs(&state.used) {
            if !wrote_header {
                xex.extend_from_slice(&0xFFFFu16.to_le_bytes());
                wrote_header = true;
            }

            let run_start: u32 = start
                .try_into()
                .context("used run start does not fit in u32")?;
            let run_end_exclusive: u32 =
                end.try_into().context("used run end does not fit in u32")?;

            let start_addr = state
                .spec
                .start
                .checked_add(run_start)
                .ok_or_else(|| anyhow::anyhow!("xex run start address overflow"))?;
            let end_addr_exclusive = state
                .spec
                .start
                .checked_add(run_end_exclusive)
                .ok_or_else(|| anyhow::anyhow!("xex run end address overflow"))?;
            let end_addr = end_addr_exclusive
                .checked_sub(1)
                .ok_or_else(|| anyhow::anyhow!("xex run end underflow"))?;

            let start16 = u16::try_from(start_addr).with_context(|| {
                format!(
                    "xex output supports 16-bit load addresses, got start {start_addr:#X} in memory '{}'",
                    mem.name
                )
            })?;
            let end16 = u16::try_from(end_addr).with_context(|| {
                format!(
                    "xex output supports 16-bit load addresses, got end {end_addr:#X} in memory '{}'",
                    mem.name
                )
            })?;

            xex.extend_from_slice(&start16.to_le_bytes());
            xex.extend_from_slice(&end16.to_le_bytes());
            xex.extend_from_slice(&state.bytes[start..end]);
        }
    }

    Ok(xex)
}

fn format_section_listing(
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

fn format_data_symbol_listings(
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

fn format_function_disassembly_listings(
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

fn select_segment_rule<'a>(config: &'a LinkerConfig, segment: &str) -> Result<&'a SegmentRule> {
    if let Some(rule) = config
        .segments
        .iter()
        .find(|rule| rule.matches_named_segment(segment))
    {
        return Ok(rule);
    }

    if let Some(rule) = config
        .segments
        .iter()
        .find(|rule| rule.segment_name().is_none())
    {
        return Ok(rule);
    }

    bail!("no segment rule found for segment '{segment}'");
}

impl SegmentRule {
    fn segment_name(&self) -> Option<&str> {
        self.segment.as_deref()
    }

    fn matches_named_segment(&self, segment: &str) -> bool {
        self.segment_name()
            .is_some_and(|name| name.eq_ignore_ascii_case(segment))
    }
}

fn validate_segment_rules(config: &LinkerConfig) -> Result<()> {
    let mut seen_ids: HashMap<&str, usize> = HashMap::new();
    for (index, rule) in config.segments.iter().enumerate() {
        if rule.id.trim().is_empty() {
            bail!("segment rule id at index {index} must not be empty");
        }
        if let Some(previous) = seen_ids.insert(rule.id.as_str(), index) {
            bail!(
                "duplicate segment rule id '{}' at indices {} and {}",
                rule.id,
                previous,
                index
            );
        }
    }
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

fn format_empty_listing_block(
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

#[cfg(test)]
mod tests {
    use super::*;
    use indexmap::IndexMap;
    use k816_o65::{
        DataStringFragment, Relocation, RelocationKind, Section, SectionChunk, SourceLocation,
        Symbol, SymbolDefinition,
    };

    fn object_with_single_section(section: &str, bytes: Vec<u8>) -> O65Object {
        let mut sections = IndexMap::new();
        sections.insert(
            section.to_string(),
            Section {
                chunks: vec![SectionChunk {
                    offset: 0,
                    address: None,
                    bytes,
                }],
            },
        );
        O65Object {
            sections,
            symbols: Vec::new(),
            relocations: Vec::new(),
            function_disassembly: Vec::new(),
            data_string_fragments: Vec::new(),
            listing: String::new(),
        }
    }

    fn segment_rule(id: &str, segment: Option<&str>, start: Option<u32>) -> SegmentRule {
        SegmentRule {
            id: id.to_string(),
            load: "MAIN".to_string(),
            run: None,
            align: Some(1),
            start,
            offset: None,
            optional: false,
            segment: segment.map(std::string::ToString::to_string),
        }
    }

    fn symbol_with_source(
        name: &str,
        section: &str,
        offset: u32,
        line: u32,
        column: u32,
        line_text: &str,
    ) -> Symbol {
        Symbol {
            name: name.to_string(),
            global: true,
            definition: Some(SymbolDefinition::Section {
                section: section.to_string(),
                offset,
                source: Some(SourceLocation {
                    file: "fixture.k65".to_string(),
                    line,
                    column,
                    column_end: column + 1,
                    line_text: line_text.to_string(),
                }),
            }),
        }
    }

    #[test]
    fn links_absolute_relocation() {
        let mut sections = IndexMap::new();
        sections.insert(
            "default".to_string(),
            Section {
                chunks: vec![SectionChunk {
                    offset: 0,
                    address: None,
                    bytes: vec![0xEA, 0x00, 0x00],
                }],
            },
        );

        let object = O65Object {
            sections,
            symbols: vec![Symbol {
                name: "target".to_string(),
                global: true,
                definition: Some(SymbolDefinition::Section {
                    section: "default".to_string(),
                    offset: 2,
                    source: None,
                }),
            }],
            relocations: vec![Relocation {
                section: "default".to_string(),
                offset: 1,
                width: 1,
                kind: RelocationKind::Absolute,
                symbol: "target".to_string(),
                source: None,
            }],
            function_disassembly: Vec::new(),
            data_string_fragments: Vec::new(),
            listing: String::new(),
        };

        let mut config = default_stub_config();
        config.output = OutputSpec {
            kind: OutputKind::RawBinary,
            file: None,
        };
        let linked = link_objects(&[object], &config).expect("link");
        assert_eq!(linked.kind, OutputKind::RawBinary);
        assert_eq!(linked.bytes, vec![0xEA, 0x02, 0x00]);
    }

    #[test]
    fn links_16bit_relocation_to_absolute_symbol() {
        let mut sections = IndexMap::new();
        sections.insert(
            "default".to_string(),
            Section {
                chunks: vec![SectionChunk {
                    offset: 0,
                    address: None,
                    bytes: vec![0xAD, 0x00, 0x00],
                }],
            },
        );

        let object = O65Object {
            sections,
            symbols: vec![Symbol {
                name: "abs_target".to_string(),
                global: true,
                definition: Some(SymbolDefinition::Absolute {
                    address: 0x1234,
                    source: None,
                }),
            }],
            relocations: vec![Relocation {
                section: "default".to_string(),
                offset: 1,
                width: 2,
                kind: RelocationKind::Absolute,
                symbol: "abs_target".to_string(),
                source: None,
            }],
            function_disassembly: Vec::new(),
            data_string_fragments: Vec::new(),
            listing: String::new(),
        };

        let mut config = default_stub_config();
        config.output = OutputSpec {
            kind: OutputKind::RawBinary,
            file: None,
        };
        let linked = link_objects(&[object], &config).expect("link");
        assert_eq!(linked.bytes, vec![0xAD, 0x34, 0x12]);
    }

    #[test]
    fn default_stub_config_emits_xex() {
        let mut sections = IndexMap::new();
        sections.insert(
            "default".to_string(),
            Section {
                chunks: vec![SectionChunk {
                    offset: 0,
                    address: None,
                    bytes: vec![0xEA],
                }],
            },
        );

        let object = O65Object {
            sections,
            symbols: Vec::new(),
            relocations: Vec::new(),
            function_disassembly: Vec::new(),
            data_string_fragments: Vec::new(),
            listing: String::new(),
        };

        let linked = link_objects(&[object], &default_stub_config()).expect("link");
        assert_eq!(linked.kind, OutputKind::Xex);
        assert_eq!(linked.bytes, vec![0xFF, 0xFF, 0x00, 0x00, 0x00, 0x00, 0xEA]);
    }

    #[test]
    fn linker_config_defaults_output_kind_to_xex_when_unspecified() {
        let config: LinkerConfig = ron::from_str(
            r#"(
  format: "o65-link",
  target: Some("defaults"),
  memory: [
    (
      name: "MAIN",
      start: 0,
      size: 65536,
      kind: ReadWrite,
      fill: Some(0),
    ),
  ],
  segments: [
    (
      id: "DEFAULT",
      load: "MAIN",
      run: None,
      align: Some(1),
      start: None,
      offset: None,
      optional: false,
      segment: None,
    ),
  ],
  symbols: [],
  output: (
    file: None,
  ),
  entry: None,
)"#,
        )
        .expect("config should parse");

        assert_eq!(config.output.kind, OutputKind::Xex);
    }

    #[test]
    fn explicit_segment_selector_takes_precedence_over_default_rule() {
        let object = object_with_single_section("info", vec![0xAA]);

        let mut config = default_stub_config();
        config.segments = vec![
            segment_rule("DEFAULT", None, Some(0x0200)),
            segment_rule("INFO_EXPLICIT", Some("info"), Some(0x0300)),
        ];

        let linked = link_objects(&[object], &config).expect("link");
        assert!(linked.listing.contains("000300: AA"));
    }

    #[test]
    fn selectorless_rule_is_used_as_default_fallback() {
        let object = object_with_single_section("other", vec![0xBB]);

        let mut config = default_stub_config();
        config.segments = vec![segment_rule("DEFAULT", None, Some(0x0400))];

        let linked = link_objects(&[object], &config).expect("link");
        assert!(linked.listing.contains("000400: BB"));
    }

    #[test]
    fn does_not_match_segment_by_rule_id_when_selector_missing() {
        let object = object_with_single_section("info", vec![0xCC]);

        let mut config = default_stub_config();
        config.segments = vec![
            segment_rule("DEFAULT", None, Some(0x0200)),
            segment_rule("INFO", None, Some(0xFC00)),
        ];

        let linked = link_objects(&[object], &config).expect("link");
        assert!(linked.listing.contains("000200: CC"));
        assert!(!linked.listing.contains("00FC00: CC"));
    }

    #[test]
    fn fails_when_no_matching_selector_or_default_rule_exists() {
        let object = object_with_single_section("info", vec![0xDD]);

        let mut config = default_stub_config();
        config.segments = vec![segment_rule(
            "ONLY_DEFAULT_SEGMENT",
            Some("default"),
            Some(0x0200),
        )];

        let err = link_objects(&[object], &config).expect_err("expected missing rule");
        assert!(
            err.to_string()
                .contains("no segment rule found for segment 'info'")
        );
    }

    #[test]
    fn rejects_empty_segment_rule_id() {
        let object = object_with_single_section("default", vec![0xEE]);

        let mut config = default_stub_config();
        config.segments = vec![segment_rule("  ", None, Some(0x0200))];

        let err = link_objects(&[object], &config).expect_err("expected empty id error");
        assert!(
            err.to_string()
                .contains("segment rule id at index 0 must not be empty")
        );
    }

    #[test]
    fn rejects_duplicate_segment_rule_ids() {
        let object = object_with_single_section("default", vec![0xFF]);

        let mut config = default_stub_config();
        config.segments = vec![
            segment_rule("DUP", Some("default"), Some(0x0200)),
            segment_rule("DUP", Some("info"), Some(0x0300)),
        ];

        let err = link_objects(&[object], &config).expect_err("expected duplicate id error");
        assert!(err.to_string().contains("duplicate segment rule id 'DUP'"));
    }

    #[test]
    fn fixed_chunk_reserved_before_relocatable() {
        let mut sections = IndexMap::new();
        sections.insert(
            "default".to_string(),
            Section {
                chunks: vec![
                    SectionChunk {
                        offset: 0,
                        address: Some(0x20),
                        bytes: vec![0xAA],
                    },
                    SectionChunk {
                        offset: 1,
                        address: None,
                        bytes: vec![0xBB],
                    },
                ],
            },
        );

        let object = O65Object {
            sections,
            symbols: Vec::new(),
            relocations: Vec::new(),
            function_disassembly: Vec::new(),
            data_string_fragments: Vec::new(),
            listing: String::new(),
        };

        let mut config = default_stub_config();
        config.output = OutputSpec {
            kind: OutputKind::RawBinary,
            file: None,
        };
        let linked = link_objects(&[object], &config).expect("link");
        assert_eq!(linked.bytes, vec![0xAA, 0xBB]);
        assert!(linked.listing.contains("000020: AA"));
        assert!(linked.listing.contains("000021: BB"));
    }

    #[test]
    fn rejects_overlapping_fixed_chunks() {
        let mut sections = IndexMap::new();
        sections.insert(
            "default".to_string(),
            Section {
                chunks: vec![
                    SectionChunk {
                        offset: 0,
                        address: Some(0x30),
                        bytes: vec![0xAA, 0xBB],
                    },
                    SectionChunk {
                        offset: 2,
                        address: Some(0x31),
                        bytes: vec![0xCC],
                    },
                ],
            },
        );

        let object = O65Object {
            sections,
            symbols: Vec::new(),
            relocations: Vec::new(),
            function_disassembly: Vec::new(),
            data_string_fragments: Vec::new(),
            listing: String::new(),
        };

        let err = link_objects(&[object], &default_stub_config()).expect_err("expected overlap");
        assert!(err.to_string().contains("overlaps existing placement"));
    }

    #[test]
    fn rejects_out_of_bounds_fixed_chunk() {
        let mut sections = IndexMap::new();
        sections.insert(
            "default".to_string(),
            Section {
                chunks: vec![SectionChunk {
                    offset: 0,
                    address: Some(0x1_0000),
                    bytes: vec![0xAA],
                }],
            },
        );

        let object = O65Object {
            sections,
            symbols: Vec::new(),
            relocations: Vec::new(),
            function_disassembly: Vec::new(),
            data_string_fragments: Vec::new(),
            listing: String::new(),
        };

        let err =
            link_objects(&[object], &default_stub_config()).expect_err("expected range error");
        assert!(err.to_string().contains("outside memory"));
    }

    #[test]
    fn fails_when_no_free_range_for_relocatable_chunk() {
        let mut config = default_stub_config();
        config.memory[0].size = 2;

        let mut sections = IndexMap::new();
        sections.insert(
            "default".to_string(),
            Section {
                chunks: vec![
                    SectionChunk {
                        offset: 0,
                        address: Some(1),
                        bytes: vec![0xAA],
                    },
                    SectionChunk {
                        offset: 1,
                        address: None,
                        bytes: vec![0xBB, 0xCC],
                    },
                ],
            },
        );

        let object = O65Object {
            sections,
            symbols: Vec::new(),
            relocations: Vec::new(),
            function_disassembly: Vec::new(),
            data_string_fragments: Vec::new(),
            listing: String::new(),
        };

        let err = link_objects(&[object], &config).expect_err("expected no-fit error");
        assert!(err.to_string().contains("no free range found"));
    }

    #[test]
    fn raw_binary_rejects_multiple_used_memory_areas() {
        let mut config = default_stub_config();
        config.output = OutputSpec {
            kind: OutputKind::RawBinary,
            file: None,
        };
        config.memory.push(MemoryArea {
            name: "AUX".to_string(),
            start: 0x8000,
            size: 0x1000,
            kind: MemoryKind::ReadWrite,
            fill: Some(0),
        });
        config.segments = vec![
            SegmentRule {
                id: "DEFAULT".to_string(),
                load: "MAIN".to_string(),
                run: None,
                align: Some(1),
                start: None,
                offset: None,
                optional: false,
                segment: Some("default".to_string()),
            },
            SegmentRule {
                id: "AUX".to_string(),
                load: "AUX".to_string(),
                run: None,
                align: Some(1),
                start: None,
                offset: None,
                optional: false,
                segment: Some("aux".to_string()),
            },
        ];

        let mut sections = IndexMap::new();
        sections.insert(
            "default".to_string(),
            Section {
                chunks: vec![SectionChunk {
                    offset: 0,
                    address: None,
                    bytes: vec![0xAA],
                }],
            },
        );
        sections.insert(
            "aux".to_string(),
            Section {
                chunks: vec![SectionChunk {
                    offset: 0,
                    address: None,
                    bytes: vec![0xBB],
                }],
            },
        );

        let object = O65Object {
            sections,
            symbols: Vec::new(),
            relocations: Vec::new(),
            function_disassembly: Vec::new(),
            data_string_fragments: Vec::new(),
            listing: String::new(),
        };

        let err = link_objects(&[object], &config).expect_err("expected ambiguity error");
        assert!(err.to_string().contains("raw binary output is ambiguous"));
    }

    #[test]
    fn raw_binary_rejects_multiple_segment_runs_in_single_memory_area() {
        let mut config = default_stub_config();
        config.output = OutputSpec {
            kind: OutputKind::RawBinary,
            file: None,
        };
        config.segments = vec![
            SegmentRule {
                id: "DEFAULT".to_string(),
                load: "MAIN".to_string(),
                run: None,
                align: Some(1),
                start: Some(0x0200),
                offset: None,
                optional: false,
                segment: Some("default".to_string()),
            },
            SegmentRule {
                id: "INFO".to_string(),
                load: "MAIN".to_string(),
                run: None,
                align: Some(1),
                start: Some(0xFC00),
                offset: None,
                optional: false,
                segment: Some("info".to_string()),
            },
        ];

        let mut sections = IndexMap::new();
        sections.insert(
            "default".to_string(),
            Section {
                chunks: vec![SectionChunk {
                    offset: 0,
                    address: None,
                    bytes: vec![0xAA],
                }],
            },
        );
        sections.insert(
            "info".to_string(),
            Section {
                chunks: vec![SectionChunk {
                    offset: 0,
                    address: None,
                    bytes: vec![0xBB],
                }],
            },
        );

        let object = O65Object {
            sections,
            symbols: Vec::new(),
            relocations: Vec::new(),
            function_disassembly: Vec::new(),
            data_string_fragments: Vec::new(),
            listing: String::new(),
        };

        let err = link_objects(&[object], &config).expect_err("expected ambiguity error");
        assert!(err.to_string().contains("raw binary output is ambiguous"));
    }

    #[test]
    fn emits_xex_blocks_from_used_runs() {
        let mut config = default_stub_config();
        config.output = OutputSpec {
            kind: OutputKind::Xex,
            file: None,
        };

        let mut sections = IndexMap::new();
        sections.insert(
            "default".to_string(),
            Section {
                chunks: vec![
                    SectionChunk {
                        offset: 0,
                        address: Some(0x0200),
                        bytes: vec![0xAA],
                    },
                    SectionChunk {
                        offset: 1,
                        address: Some(0x0210),
                        bytes: vec![0xBB, 0xCC],
                    },
                ],
            },
        );

        let object = O65Object {
            sections,
            symbols: Vec::new(),
            relocations: Vec::new(),
            function_disassembly: Vec::new(),
            data_string_fragments: Vec::new(),
            listing: String::new(),
        };

        let linked = link_objects(&[object], &config).expect("link");
        assert_eq!(linked.kind, OutputKind::Xex);
        let xex = &linked.bytes;
        assert_eq!(
            xex,
            &vec![
                0xFF, 0xFF, // XEX header
                0x00, 0x02, 0x00, 0x02, 0xAA, // block at $0200
                0x10, 0x02, 0x11, 0x02, 0xBB, 0xCC, // block at $0210-$0211
            ]
        );
    }

    #[test]
    fn rejects_xex_addresses_above_16bit() {
        let mut config = default_stub_config();
        config.memory[0].start = 0x1_0000;
        config.memory[0].size = 0x100;
        config.output = OutputSpec {
            kind: OutputKind::Xex,
            file: None,
        };

        let mut sections = IndexMap::new();
        sections.insert(
            "default".to_string(),
            Section {
                chunks: vec![SectionChunk {
                    offset: 0,
                    address: Some(0x1_0000),
                    bytes: vec![0xAA],
                }],
            },
        );

        let object = O65Object {
            sections,
            symbols: Vec::new(),
            relocations: Vec::new(),
            function_disassembly: Vec::new(),
            data_string_fragments: Vec::new(),
            listing: String::new(),
        };

        let err = link_objects(&[object], &config).expect_err("expected xex address error");
        assert!(
            err.to_string()
                .contains("xex output supports 16-bit load addresses")
        );
    }

    #[test]
    fn listing_includes_named_data_symbol_addresses() {
        let mut object = object_with_single_section("default", vec![0xAA, 0xBB, 0xCC]);
        object.symbols = vec![symbol_with_source(
            "text",
            "default",
            1,
            1,
            1,
            "data text {",
        )];

        let linked = link_objects(&[object], &default_stub_config()).expect("link");
        assert!(linked.listing.contains("[data obj#0 default::text]"));
        assert!(linked.listing.contains("000001: <2 bytes of data>"));
    }

    #[test]
    fn listing_skips_non_data_symbols_by_source_line() {
        let mut object = object_with_single_section("default", vec![0xAA, 0xBB, 0xCC]);
        object.symbols = vec![
            symbol_with_source("main", "default", 0, 1, 1, "main {"),
            symbol_with_source("main::.loop", "default", 1, 2, 3, ".loop:"),
        ];

        let linked = link_objects(&[object], &default_stub_config()).expect("link");
        assert!(!linked.listing.contains("[data obj#0"));
    }

    #[test]
    fn listing_skips_symbols_without_source_metadata() {
        let mut object = object_with_single_section("default", vec![0xAA]);
        object.symbols = vec![Symbol {
            name: "text".to_string(),
            global: true,
            definition: Some(SymbolDefinition::Section {
                section: "default".to_string(),
                offset: 0,
                source: None,
            }),
        }];

        let linked = link_objects(&[object], &default_stub_config()).expect("link");
        assert!(!linked.listing.contains("[data obj#0"));
    }

    #[test]
    fn listing_orders_data_symbols_by_source_location_then_name() {
        let mut object = object_with_single_section("default", vec![0x10, 0x20, 0x30, 0x40]);
        object.symbols = vec![
            symbol_with_source("zeta", "default", 3, 20, 1, "data zeta {"),
            symbol_with_source("gamma", "default", 1, 10, 4, "data gamma {"),
            symbol_with_source("alpha", "default", 0, 10, 4, "data alpha {"),
            symbol_with_source("beta", "default", 2, 10, 6, "data beta {"),
        ];

        let linked = link_objects(&[object], &default_stub_config()).expect("link");
        let alpha_pos = linked
            .listing
            .find("[data obj#0 default::alpha]")
            .expect("alpha block");
        let gamma_pos = linked
            .listing
            .find("[data obj#0 default::gamma]")
            .expect("gamma block");
        let beta_pos = linked
            .listing
            .find("[data obj#0 default::beta]")
            .expect("beta block");
        let zeta_pos = linked
            .listing
            .find("[data obj#0 default::zeta]")
            .expect("zeta block");

        assert!(alpha_pos < gamma_pos);
        assert!(gamma_pos < beta_pos);
        assert!(beta_pos < zeta_pos);
    }

    #[test]
    fn listing_computes_data_length_until_next_top_level_code_block() {
        let mut object = object_with_single_section("default", vec![0x01, 0x02, 0x03, 0x04]);
        object.symbols = vec![
            symbol_with_source("bytes", "default", 0, 1, 1, "data bytes {"),
            symbol_with_source("here", "default", 3, 4, 1, "here:"),
            symbol_with_source("main", "default", 4, 6, 1, "main {"),
        ];

        let linked = link_objects(&[object], &default_stub_config()).expect("link");
        assert!(linked.listing.contains("000000: <4 bytes of data>"));
    }

    #[test]
    fn listing_renders_string_literal_parts_inside_data_symbol() {
        let mut object = object_with_single_section(
            "default",
            vec![
                b'H', b'e', b'l', b'l', b'o', b',', b' ', b'W', b'o', b'r', b'l', b'd', b'!', 0x0D,
                0x0A, 0x00, 0x60,
            ],
        );
        object.symbols = vec![
            symbol_with_source("text", "default", 0, 1, 1, "data text {"),
            symbol_with_source("main", "default", 16, 6, 1, "main {"),
        ];
        object.data_string_fragments = vec![DataStringFragment {
            section: "default".to_string(),
            offset: 0,
            text: "Hello, World!".to_string(),
        }];

        let linked = link_objects(&[object], &default_stub_config()).expect("link");
        assert!(linked.listing.contains("000000: \"Hello, World!\""));
        assert!(linked.listing.contains("00000D: <3 bytes of data>"));
    }
}
