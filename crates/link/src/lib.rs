use anyhow::{Context, Result, bail};
use indexmap::IndexMap;
use k816_o65::{O65Object, RelocationKind};
use serde::Deserialize;
use std::collections::HashMap;
use std::path::Path;

#[derive(Debug, Clone, Deserialize)]
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
    pub outputs: Vec<OutputSpec>,
    #[serde(default)]
    pub entry: Option<String>,
}

fn default_format() -> String {
    "o65-link".to_string()
}

#[derive(Debug, Clone, Deserialize)]
pub struct MemoryArea {
    pub name: String,
    pub start: u32,
    pub size: u32,
    pub kind: MemoryKind,
    #[serde(default)]
    pub fill: Option<u8>,
    #[serde(default)]
    pub out_file: Option<String>,
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
pub struct OutputSpec {
    pub name: String,
    pub kind: OutputKind,
    #[serde(default)]
    pub default_file: Option<String>,
}

#[derive(Debug, Clone, Deserialize)]
pub enum OutputKind {
    RawBinary,
}

#[derive(Debug, Clone)]
pub struct LinkOutput {
    pub binaries: IndexMap<String, Vec<u8>>,
    pub listing: String,
}

#[derive(Debug, Clone)]
struct MemoryState {
    spec: MemoryArea,
    bytes: Vec<u8>,
    used: Vec<bool>,
    cursor: u32,
    highest_used: usize,
}

#[derive(Debug, Clone)]
struct PlacedSection {
    segment: String,
    memory_name: String,
    base_addr: u32,
    len: usize,
}

#[derive(Debug, Clone)]
struct ResolvedSymbol {
    addr: u32,
    segment: String,
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
            out_file: Some("main.bin".to_string()),
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
        outputs: vec![OutputSpec {
            name: "main".to_string(),
            kind: OutputKind::RawBinary,
            default_file: Some("main.bin".to_string()),
        }],
        entry: None,
    }
}

pub fn link_objects(objects: &[O65Object], config: &LinkerConfig) -> Result<LinkOutput> {
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
                highest_used: 0,
            },
        );
    }

    let mut placed: HashMap<(usize, String), PlacedSection> = HashMap::new();
    let mut placement_order: Vec<(usize, String)> = Vec::new();
    for (obj_idx, object) in objects.iter().enumerate() {
        for (segment, section) in &object.sections {
            if section.bytes.is_empty() {
                placed.insert(
                    (obj_idx, segment.clone()),
                    PlacedSection {
                        segment: segment.clone(),
                        memory_name: select_segment_rule(config, segment)?.load.clone(),
                        base_addr: 0,
                        len: 0,
                    },
                );
                placement_order.push((obj_idx, segment.clone()));
                continue;
            }

            let rule = select_segment_rule(config, segment)?;
            let mem = memory_map
                .get_mut(&rule.load)
                .ok_or_else(|| anyhow::anyhow!("segment '{}' references unknown memory '{}'", rule.name, rule.load))?;

            let align = rule.align.unwrap_or(1);
            if align == 0 {
                bail!("segment '{}' has align=0", rule.name);
            }

            let mut base = if let Some(start) = rule.start {
                start
            } else {
                align_up(mem.cursor.max(mem.spec.start), align)
            };
            if let Some(offset) = rule.offset {
                base = base.saturating_add(offset);
            }

            if base < mem.spec.start {
                bail!(
                    "section '{}' placement underflows memory '{}'",
                    segment,
                    mem.spec.name
                );
            }

            let rel_start = (base - mem.spec.start) as usize;
            let rel_end = rel_start.saturating_add(section.bytes.len());
            if rel_end > mem.bytes.len() {
                bail!(
                    "section '{}' does not fit memory '{}' (start={base:#X}, len={:#X})",
                    segment,
                    mem.spec.name,
                    section.bytes.len()
                );
            }

            for idx in rel_start..rel_end {
                if mem.used[idx] {
                    bail!(
                        "section '{}' overlaps previously allocated bytes in memory '{}'",
                        segment,
                        mem.spec.name
                    );
                }
            }

            mem.bytes[rel_start..rel_end].copy_from_slice(&section.bytes);
            for idx in rel_start..rel_end {
                mem.used[idx] = true;
            }
            mem.highest_used = mem.highest_used.max(rel_end);
            mem.cursor = mem.cursor.max(base.saturating_add(section.bytes.len() as u32));

            placed.insert(
                (obj_idx, segment.clone()),
                PlacedSection {
                    segment: segment.clone(),
                    memory_name: mem.spec.name.clone(),
                    base_addr: base,
                    len: section.bytes.len(),
                },
            );
            placement_order.push((obj_idx, segment.clone()));
        }
    }

    let mut symbols: HashMap<String, ResolvedSymbol> = HashMap::new();
    for (obj_idx, object) in objects.iter().enumerate() {
        for symbol in &object.symbols {
            let Some(def) = &symbol.definition else {
                continue;
            };
            let placed_section = placed
                .get(&(obj_idx, def.section.clone()))
                .ok_or_else(|| anyhow::anyhow!("symbol '{}' references unknown section '{}'", symbol.name, def.section))?;

            if def.offset as usize > placed_section.len {
                bail!(
                    "symbol '{}' offset {:#X} is outside section '{}'",
                    symbol.name,
                    def.offset,
                    def.section
                );
            }

            let resolved = ResolvedSymbol {
                addr: placed_section.base_addr.saturating_add(def.offset),
                segment: placed_section.segment.clone(),
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
            SymbolValue::Import(name) => symbols
                .get(name)
                .cloned()
                .ok_or_else(|| anyhow::anyhow!("link symbol '{}' imports undefined symbol '{}'", link_symbol.name, name))?,
        };
        symbols.insert(link_symbol.name.clone(), value);
    }

    for (obj_idx, object) in objects.iter().enumerate() {
        for reloc in &object.relocations {
            let src_section = placed
                .get(&(obj_idx, reloc.section.clone()))
                .ok_or_else(|| anyhow::anyhow!("relocation references unknown section '{}'", reloc.section))?
                .clone();
            let target = symbols
                .get(&reloc.symbol)
                .ok_or_else(|| anyhow::anyhow!("undefined symbol '{}'", reloc.symbol))?
                .clone();

            if reloc.kind == RelocationKind::Absolute
                && reloc.width == 2
                && src_section.segment != target.segment
            {
                bail!(
                    "16-bit relocation from segment '{}' to '{}' requires far addressing",
                    src_section.segment,
                    target.segment
                );
            }

            let mem = memory_map
                .get_mut(&src_section.memory_name)
                .ok_or_else(|| anyhow::anyhow!("internal linker error: memory '{}' missing", src_section.memory_name))?;

            let site_addr = src_section.base_addr.saturating_add(reloc.offset);
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
                RelocationKind::Relative => {
                    if reloc.width != 1 {
                        bail!("relative relocation width must be 1 byte");
                    }
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
            }
        }
    }

    let mut binaries = IndexMap::new();
    for mem in &config.memory {
        let state = memory_map
            .get(&mem.name)
            .ok_or_else(|| anyhow::anyhow!("internal linker error: memory '{}' missing", mem.name))?;
        let out_name = mem
            .out_file
            .clone()
            .unwrap_or_else(|| format!("{}.bin", mem.name.to_ascii_lowercase()));
        let bytes = state.bytes[..state.highest_used].to_vec();
        binaries.insert(out_name, bytes);
    }

    let mut listing_blocks = Vec::new();
    for key in placement_order {
        let Some(section) = placed.get(&key) else {
            continue;
        };
        if section.len == 0 {
            listing_blocks.push(format_empty_listing_block(&section.segment));
            continue;
        }
        let mem = memory_map
            .get(&section.memory_name)
            .ok_or_else(|| anyhow::anyhow!("internal linker error: memory '{}' missing", section.memory_name))?;
        let rel_start = (section.base_addr - mem.spec.start) as usize;
        let rel_end = rel_start + section.len;
        listing_blocks.push(format_listing_block(
            &section.segment,
            &mem.bytes[rel_start..rel_end],
        ));
    }
    let listing = listing_blocks.join("\n\n");

    Ok(LinkOutput { binaries, listing })
}

fn select_segment_rule<'a>(config: &'a LinkerConfig, segment: &str) -> Result<&'a SegmentRule> {
    if let Some(rule) = config
        .segments
        .iter()
        .find(|rule| rule.segment_name() == Some(segment))
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
        self.segment
            .as_deref()
            .or(self.legacy_bank.as_deref())
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

fn format_listing_block(name: &str, bytes: &[u8]) -> String {
    let mut out = String::new();
    out.push_str(&format!("[{name}]\n"));
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

#[cfg(test)]
mod tests {
    use super::*;
    use k816_o65::{O65Object, Relocation, RelocationKind, Section, Symbol, SymbolDefinition};

    #[test]
    fn links_absolute_relocation() {
        let mut sections = IndexMap::new();
        sections.insert(
            "default".to_string(),
            Section {
                bytes: vec![0xEA, 0x00, 0x00],
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
                }),
            }],
            relocations: vec![Relocation {
                section: "default".to_string(),
                offset: 1,
                width: 1,
                kind: RelocationKind::Absolute,
                symbol: "target".to_string(),
            }],
            listing: String::new(),
        };

        let linked = link_objects(&[object], &default_stub_config()).expect("link");
        let main = linked.binaries.get("main.bin").expect("main output");
        assert_eq!(main, &vec![0xEA, 0x02, 0x00]);
    }
}
