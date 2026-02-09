use anyhow::{Context, Result, bail};
use ariadne::{Cache, Config, IndexType, Label, Report, ReportKind, Source};
use k816_isa65816::{decode_instruction, format_instruction};
use k816_o65::{O65Object, RelocationKind, SourceLocation};
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
    pub name: String,
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
    #[serde(default, rename = "bank")]
    pub legacy_bank: Option<String>,
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
            name: "DEFAULT".to_string(),
            load: "MAIN".to_string(),
            run: None,
            align: Some(1),
            start: None,
            offset: None,
            optional: false,
            segment: None,
            legacy_bank: None,
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
                bail!("segment '{}' has align=0", rule.name);
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
        let base = choose_reloc_base(chunk, &mut memory_map)?;
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

            let section_key = (obj_idx, def.section.clone());
            let placements = placed_by_section.get(&section_key).ok_or_else(|| {
                anyhow::anyhow!(
                    "symbol '{}' references unknown section '{}'",
                    symbol.name,
                    def.section
                )
            })?;

            let addr = resolve_symbol_addr(placements, def.offset).ok_or_else(|| {
                anyhow::anyhow!(
                    "symbol '{}' offset {:#X} is outside section '{}'",
                    symbol.name,
                    def.offset,
                    def.section
                )
            })?;

            let resolved = ResolvedSymbol {
                addr,
                segment: def.section.clone(),
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
                segment: "__absolute__".to_string(),
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

            let target = symbols
                .get(&reloc.symbol)
                .ok_or_else(|| anyhow::anyhow!("undefined symbol '{}'", reloc.symbol))?
                .clone();

            if reloc.kind == RelocationKind::Absolute
                && reloc.width == 2
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
            }
        }
    }

    let (output_kind, bytes) = render_link_output(config, &memory_map)?;

    let mut listing_blocks = Vec::new();
    for key in &section_order {
        let segment_name = &key.1;
        let Some(chunks) = placed_by_section.get(key) else {
            continue;
        };

        if chunks.is_empty() {
            listing_blocks.push(format_empty_listing_block(segment_name));
            continue;
        }

        listing_blocks.push(format_section_listing(segment_name, chunks, &memory_map)?);
    }
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
    memory_map: &mut HashMap<String, MemoryState>,
) -> Result<u32> {
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
        anyhow::anyhow!(
            "no free range found for relocatable chunk in segment '{}' (len={:#X})",
            chunk.segment,
            chunk.len
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
    let object = objects.get(chunk.obj_idx)?;
    let mut first_match_name: Option<String> = None;

    for symbol in &object.symbols {
        let Some(definition) = symbol.definition.as_ref() else {
            continue;
        };
        if definition.section != chunk.segment || definition.offset != chunk.section_offset {
            continue;
        }

        if definition.source.is_some() {
            return Some(AnchorContext {
                symbol_name: symbol.name.clone(),
                source: definition.source.clone(),
            });
        }

        if first_match_name.is_none() {
            first_match_name = Some(symbol.name.clone());
        }
    }

    first_match_name.map(|symbol_name| AnchorContext {
        symbol_name,
        source: None,
    })
}

fn decorate_with_anchor(
    message: &str,
    anchor: Option<&AnchorContext>,
    options: LinkRenderOptions,
) -> String {
    let Some(anchor) = anchor else {
        return message.to_string();
    };

    let Some(source) = &anchor.source else {
        return format!("{message}\nfunction '{}'", anchor.symbol_name);
    };

    let context = render_anchor_context(message, anchor, source, options);
    if context.is_empty() {
        format!(
            "{message}\nfunction '{}' at {}:{}:{}",
            anchor.symbol_name, source.file, source.line, source.column
        )
    } else {
        context
    }
}

fn render_anchor_context(
    message: &str,
    anchor: &AnchorContext,
    source: &SourceLocation,
    options: LinkRenderOptions,
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
            Label::new((file_id.clone(), start..end))
                .with_message(format!("function '{}' defined here", anchor.symbol_name)),
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
    segment: &str,
    chunks: &[PlacedChunk],
    memory_map: &HashMap<String, MemoryState>,
) -> Result<String> {
    let mut out = String::new();
    out.push_str(&format!("[{segment}]\n"));

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
        let decoded = decode_instruction(decode_slice).map_err(|err| {
            anyhow::anyhow!(
                "failed to decode instruction at {address:#X} ({}::{}): {err}",
                function.section,
                function.function
            )
        })?;
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
        .find(|rule| rule.matches_rule_name(segment))
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
        self.segment.as_deref().or(self.legacy_bank.as_deref())
    }

    fn matches_named_segment(&self, segment: &str) -> bool {
        self.segment_name()
            .is_some_and(|name| name.eq_ignore_ascii_case(segment))
    }

    fn matches_rule_name(&self, segment: &str) -> bool {
        self.segment_name().is_none() && self.name.eq_ignore_ascii_case(segment)
    }
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

fn format_empty_listing_block(name: &str) -> String {
    format!("[{name}]\n(empty)\n")
}

#[cfg(test)]
mod tests {
    use super::*;
    use indexmap::IndexMap;
    use k816_o65::{Relocation, RelocationKind, Section, SectionChunk, Symbol, SymbolDefinition};

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
                definition: Some(SymbolDefinition {
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
            }],
            function_disassembly: Vec::new(),
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
      name: "DEFAULT",
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
                name: "DEFAULT".to_string(),
                load: "MAIN".to_string(),
                run: None,
                align: Some(1),
                start: None,
                offset: None,
                optional: false,
                segment: Some("default".to_string()),
                legacy_bank: None,
            },
            SegmentRule {
                name: "AUX".to_string(),
                load: "AUX".to_string(),
                run: None,
                align: Some(1),
                start: None,
                offset: None,
                optional: false,
                segment: Some("aux".to_string()),
                legacy_bank: None,
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
                name: "DEFAULT".to_string(),
                load: "MAIN".to_string(),
                run: None,
                align: Some(1),
                start: Some(0x0200),
                offset: None,
                optional: false,
                segment: Some("default".to_string()),
                legacy_bank: None,
            },
            SegmentRule {
                name: "INFO".to_string(),
                load: "MAIN".to_string(),
                run: None,
                align: Some(1),
                start: Some(0xFC00),
                offset: None,
                optional: false,
                segment: Some("info".to_string()),
                legacy_bank: None,
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
            listing: String::new(),
        };

        let err = link_objects(&[object], &config).expect_err("expected xex address error");
        assert!(
            err.to_string()
                .contains("xex output supports 16-bit load addresses")
        );
    }
}
