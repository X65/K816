use anyhow::{Context, Result, bail};
use std::collections::HashMap;

use super::layout::MemoryState;
use super::{LinkedLayout, LinkedRun, LinkerConfig, OutputKind, PlacedChunk};

pub fn resolve_symbol_addr(placements: &[PlacedChunk], offset: u32) -> Option<u32> {
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

pub(super) fn resolve_reloc_site(
    placements: &[PlacedChunk],
    offset: u32,
    width: u8,
) -> Option<(&str, u32)> {
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

pub(super) fn collect_linked_runs(
    config: &LinkerConfig,
    memory_map: &HashMap<String, MemoryState>,
) -> Result<Vec<LinkedRun>> {
    let mut runs = Vec::new();
    for mem in &config.memory {
        let state = memory_map.get(&mem.name).ok_or_else(|| {
            anyhow::anyhow!("internal linker error: memory '{}' missing", mem.name)
        })?;
        for (start, end) in used_runs(&state.used) {
            let run_start: u32 = start
                .try_into()
                .context("used run start does not fit in u32")?;
            let start_addr = state
                .spec
                .start
                .checked_add(run_start)
                .ok_or_else(|| anyhow::anyhow!("linked run start address overflow"))?;
            runs.push(LinkedRun {
                memory_name: mem.name.clone(),
                start_addr,
                bytes: state.bytes[start..end].to_vec(),
            });
        }
    }
    Ok(runs)
}

pub fn render_linked_output(layout: &LinkedLayout, kind: OutputKind) -> Result<Vec<u8>> {
    match kind {
        OutputKind::RawBinary => render_raw_output(layout),
        OutputKind::Xex => render_xex_output(layout),
    }
}

fn render_raw_output(layout: &LinkedLayout) -> Result<Vec<u8>> {
    if layout.runs.is_empty() {
        return Ok(Vec::new());
    }

    let first_memory = layout
        .runs
        .first()
        .map(|run| run.memory_name.as_str())
        .unwrap_or_default();
    let multiple_memories = layout
        .runs
        .iter()
        .any(|run| run.memory_name.as_str() != first_memory);
    if multiple_memories {
        let mut used_names: Vec<String> = layout
            .runs
            .iter()
            .map(|run| run.memory_name.clone())
            .collect();
        used_names.sort();
        used_names.dedup();
        bail!(
            "raw binary output is ambiguous: data placed in multiple memory areas ({})",
            used_names.join(", ")
        );
    }

    if layout.runs.len() > 1 {
        bail!(
            "raw binary output is ambiguous: data placed in multiple ranges in memory area '{}'",
            first_memory
        );
    }

    Ok(layout
        .runs
        .first()
        .map(|run| run.bytes.clone())
        .unwrap_or_default())
}

fn render_xex_output(layout: &LinkedLayout) -> Result<Vec<u8>> {
    let mut xex = Vec::new();
    if layout.runs.is_empty() {
        return Ok(xex);
    }
    xex.extend_from_slice(&0xFFFFu16.to_le_bytes());

    for run in &layout.runs {
        let run_len: u32 = run
            .bytes
            .len()
            .try_into()
            .context("xex run length does not fit in u32")?;
        let end_addr_exclusive = run
            .start_addr
            .checked_add(run_len)
            .ok_or_else(|| anyhow::anyhow!("xex run end address overflow"))?;
        let end_addr = end_addr_exclusive
            .checked_sub(1)
            .ok_or_else(|| anyhow::anyhow!("xex run end underflow"))?;

        let start16 = u16::try_from(run.start_addr).with_context(|| {
            format!(
                "xex output supports 16-bit load addresses, got start {:#X} in memory '{}'",
                run.start_addr, run.memory_name
            )
        })?;
        let end16 = u16::try_from(end_addr).with_context(|| {
            format!(
                "xex output supports 16-bit load addresses, got end {end_addr:#X} in memory '{}'",
                run.memory_name
            )
        })?;

        xex.extend_from_slice(&start16.to_le_bytes());
        xex.extend_from_slice(&end16.to_le_bytes());
        xex.extend_from_slice(&run.bytes);
    }

    Ok(xex)
}
