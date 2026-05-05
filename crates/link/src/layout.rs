mod anchors;
mod dp;

use anyhow::{Context, Result};
use k816_o65::{
    CallMetadata, FunctionMetadata, O65Object, RelocationKind, SourceLocation, SymbolDefinition,
};
use std::collections::HashMap;

use self::anchors::{
    decorate_with_anchor, decorate_with_anchor_with_label, find_anchor_context,
    find_section_anchor_context, render_duplicate_symbol_error,
};
use super::config::{
    LinkerConfig, MemoryArea, SymbolValue, select_segment_rule_indexed, validate_segment_rules,
};
use super::listing::{
    format_data_symbol_listings, format_empty_listing_block, format_function_disassembly_listings,
    format_section_listing,
};
use super::render::{collect_linked_runs, resolve_reloc_site, resolve_symbol_addr};
use super::types::{
    LinkDiagnostic, LinkErrors, LinkRelated, LinkRenderOptions, LinkSeverity, LinkedLayout,
    PlacedChunk,
};

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
    rule_idx: usize,
    fixed_addr: Option<u32>,
}

#[derive(Debug, Clone)]
struct ResolvedSymbol {
    addr: u32,
    segment: String,
    source: Option<SourceLocation>,
}

#[derive(Debug, Clone)]
pub(super) struct AnchorContext {
    pub(super) symbol_name: String,
    pub(super) source: Option<SourceLocation>,
}

const ABSOLUTE_SYMBOL_SEGMENT: &str = "__absolute__";
pub(crate) const DP_SYMBOL_SEGMENT: &str = "__dp__";

pub fn link_objects(objects: &[O65Object], config: &LinkerConfig) -> Result<LinkedLayout> {
    link_objects_with_options(objects, config, LinkRenderOptions::plain())
}

/// Back-compat wrapper that pre-renders link diagnostics into a single
/// `anyhow::Error`. Migrating callers should switch to
/// [`link_objects_diagnostics`] and render via `k816_core::diag` instead.
pub fn link_objects_with_options(
    objects: &[O65Object],
    config: &LinkerConfig,
    options: LinkRenderOptions,
) -> Result<LinkedLayout> {
    match link_objects_diagnostics(objects, config) {
        Ok(layout) => Ok(layout),
        Err(LinkErrors(diags)) => {
            let rendered: Vec<String> = diags
                .iter()
                .map(|diag| render_link_diagnostic_legacy(diag, options))
                .collect();
            Err(anyhow::Error::msg(rendered.join("\n\n")))
        }
    }
}

/// Canonical entry point: link objects and emit structured diagnostics on
/// failure so each consumer (CLI, golden harness, LSP) can present them in
/// its own way without re-implementing per-error logic.
pub fn link_objects_diagnostics(
    objects: &[O65Object],
    config: &LinkerConfig,
) -> std::result::Result<LinkedLayout, LinkErrors> {
    if config.memory.is_empty() {
        return Err(LinkErrors(vec![anchorless_error(
            "linker config must contain at least one memory area",
        )]));
    }
    if let Err(err) = validate_segment_rules(config) {
        return Err(LinkErrors(vec![anchorless_error(err.to_string())]));
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

            let (rule_idx, rule) = match select_segment_rule_indexed(config, segment) {
                Ok(found) => found,
                Err(err) => {
                    return Err(LinkErrors(vec![anchorless_error(err.to_string())]));
                }
            };
            let align = rule.align.unwrap_or(1);

            for (chunk_idx, chunk) in section.chunks.iter().enumerate() {
                if chunk.bytes.is_empty() {
                    continue;
                }

                let len =
                    match u32::try_from(chunk.bytes.len()).context("section chunk is too large") {
                        Ok(len) => len,
                        Err(err) => {
                            return Err(LinkErrors(vec![anchorless_error(err.to_string())]));
                        }
                    };
                planned.push(PlannedChunk {
                    obj_idx,
                    segment: segment.clone(),
                    chunk_idx,
                    section_offset: chunk.offset,
                    len,
                    memory_name: rule.load.clone(),
                    align,
                    rule_idx,
                    fixed_addr: chunk.address,
                });
            }
        }
    }

    // Cross-phase error accumulator. Phases that previously short-circuited on
    // first error now push into this list and continue, so the user sees
    // *all* link problems in a single run instead of having later phases
    // hidden by an early bail. We bail at the very end if any errors were
    // recorded. Phases that produce non-recoverable internal-state corruption
    // (config validation, allocation overflows) still bail immediately above.
    let mut errors: Vec<LinkDiagnostic> = Vec::new();

    // Phase 3: place fixed-address chunks. On failure, still record a
    // placement entry so symbols defined in this chunk resolve to a nominal
    // address — without that, every reference to those symbols would cascade
    // into "undefined symbol" noise, drowning out the original overlap error.
    for chunk in planned.iter().filter(|chunk| chunk.fixed_addr.is_some()) {
        let base = chunk
            .fixed_addr
            .expect("filtered fixed chunk must have address");
        if let Err(diag) = place_chunk(
            chunk,
            base,
            objects,
            &mut memory_map,
            &mut placed_by_section,
            true,
        ) {
            errors.push(diag);
            record_unplaced_chunk(chunk, base, &mut placed_by_section);
        }
    }

    // Phase 4: place relocatable chunks. Group chunks by their owning segment
    // rule and process the rules in `config.segments` declaration order. This
    // gives each rule a single anchor address (from `start`, `offset`, or the
    // memory's high-water mark) and lays its contributing chunks out
    // independently above that anchor via first-fit. Chunks contributed by
    // multiple object files to one rule no longer all collide at the anchor;
    // smaller later chunks may fall into holes that earlier larger chunks
    // skipped past (e.g. around a fixed-address `at = ...` placement).
    let mut by_rule: Vec<Vec<usize>> = vec![Vec::new(); config.segments.len()];
    for (idx, chunk) in planned.iter().enumerate() {
        if chunk.fixed_addr.is_some() {
            continue;
        }
        by_rule[chunk.rule_idx].push(idx);
    }
    for bucket in by_rule.iter_mut() {
        bucket.sort_by_key(|&idx| {
            let c = &planned[idx];
            (c.obj_idx, c.section_offset, c.chunk_idx)
        });
    }

    for (rule_idx, rule) in config.segments.iter().enumerate() {
        let bucket = &by_rule[rule_idx];
        if bucket.is_empty() {
            continue;
        }

        let anchor = {
            let Some(mem) = memory_map.get(&rule.load) else {
                errors.push(anchorless_error(format!(
                    "segment rule '{}' references unknown memory area '{}'",
                    rule.id, rule.load
                )));
                for &chunk_idx in bucket {
                    record_unplaced_chunk(&planned[chunk_idx], 0, &mut placed_by_section);
                }
                continue;
            };
            if let Some(start) = rule.start {
                start
            } else if let Some(offset) = rule.offset {
                mem.spec.start.saturating_add(offset)
            } else {
                mem.cursor.max(mem.spec.start)
            }
        };

        for &chunk_idx in bucket {
            let chunk = &planned[chunk_idx];
            let mem = memory_map
                .get_mut(&rule.load)
                .expect("memory presence checked above");
            let base = match find_first_fit(mem, anchor, chunk.len, chunk.align) {
                Some(b) => b,
                None => {
                    errors.push(no_fit_diagnostic(rule, chunk, anchor, objects, mem));
                    record_unplaced_chunk(chunk, anchor, &mut placed_by_section);
                    continue;
                }
            };
            if let Err(diag) = place_chunk(
                chunk,
                base,
                objects,
                &mut memory_map,
                &mut placed_by_section,
                false,
            ) {
                errors.push(diag);
                record_unplaced_chunk(chunk, base, &mut placed_by_section);
            }
        }
    }

    for chunks in placed_by_section.values_mut() {
        chunks.sort_by_key(|chunk| chunk.section_offset);
    }

    // Phase 5a: allocate the DP pool. DP is segment-orthogonal — see
    // `crates/link/src/layout/dp.rs`. Failures still return the partial
    // allocation map so symbols that *did* fit resolve cleanly downstream.
    let (dp_alloc, dp_errors) = dp::allocate(objects);
    errors.extend(dp_errors);

    // Phase 5b: build the symbol table. Duplicates are accumulated, not fatal —
    // the first definition wins and processing continues so that downstream
    // relocation errors still surface.
    let mut symbols: HashMap<String, ResolvedSymbol> = HashMap::new();
    for (obj_idx, object) in objects.iter().enumerate() {
        for symbol in &object.symbols {
            let Some(def) = &symbol.definition else {
                continue;
            };

            let resolved = match def {
                SymbolDefinition::Section {
                    section,
                    offset,
                    source,
                } => {
                    let section_key = (obj_idx, section.clone());
                    let Some(placements) = placed_by_section.get(&section_key) else {
                        errors.push(anchorless_error(format!(
                            "symbol '{}' references unknown section '{}'",
                            symbol.name, section
                        )));
                        continue;
                    };
                    let Some(addr) = resolve_symbol_addr(placements, *offset) else {
                        errors.push(anchorless_error(format!(
                            "symbol '{}' offset {:#X} is outside section '{}'",
                            symbol.name, offset, section
                        )));
                        continue;
                    };
                    ResolvedSymbol {
                        addr,
                        segment: section.clone(),
                        source: source.clone(),
                    }
                }
                SymbolDefinition::Absolute { address, source } => ResolvedSymbol {
                    addr: *address,
                    segment: ABSOLUTE_SYMBOL_SEGMENT.to_string(),
                    source: source.clone(),
                },
                SymbolDefinition::DirectPageFixed { offset, source } => ResolvedSymbol {
                    addr: *offset as u32,
                    segment: DP_SYMBOL_SEGMENT.to_string(),
                    source: source.clone(),
                },
                SymbolDefinition::DirectPageAlloc { source, .. }
                | SymbolDefinition::DirectPageAllocAlias { source, .. } => {
                    let Some(resolved) = dp_alloc.symbols.get(&symbol.name) else {
                        // Allocator failed for this symbol (overflow). Diagnostic
                        // already pushed; skip to avoid cascading errors.
                        continue;
                    };
                    ResolvedSymbol {
                        addr: resolved.offset as u32,
                        segment: DP_SYMBOL_SEGMENT.to_string(),
                        source: source.clone(),
                    }
                }
            };

            if let Some(existing) = symbols.get(&symbol.name) {
                let new_source = match def {
                    SymbolDefinition::Section { source, .. }
                    | SymbolDefinition::Absolute { source, .. }
                    | SymbolDefinition::DirectPageFixed { source, .. }
                    | SymbolDefinition::DirectPageAlloc { source, .. }
                    | SymbolDefinition::DirectPageAllocAlias { source, .. } => source.as_ref(),
                };
                errors.push(duplicate_symbol_diagnostic(
                    &symbol.name,
                    existing.source.as_ref(),
                    new_source,
                ));
                continue;
            }
            symbols.insert(symbol.name.clone(), resolved);
        }
    }

    // Phase 6: resolve link-config-supplied symbols (imports / absolutes).
    for link_symbol in &config.symbols {
        let value = match &link_symbol.value {
            SymbolValue::Absolute(addr) => ResolvedSymbol {
                addr: *addr,
                segment: ABSOLUTE_SYMBOL_SEGMENT.to_string(),
                source: None,
            },
            SymbolValue::Import(name) => match symbols.get(name).cloned() {
                Some(value) => value,
                None => {
                    errors.push(anchorless_error(format!(
                        "link symbol '{}' imports undefined symbol '{}'",
                        link_symbol.name, name
                    )));
                    continue;
                }
            },
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

    // Phase 7: validate call metadata (already accumulating; just merge into
    // the cross-phase error list instead of bailing on it).
    let call_errors = collect_call_metadata_errors(objects, &symbols, &function_metadata_map);
    errors.extend(call_errors);

    // Phase 8: resolve relocations. Each relocation is processed
    // independently — undefined symbols, out-of-range relative jumps, and
    // bad widths all accumulate so the user sees every problem in one shot.
    for (obj_idx, object) in objects.iter().enumerate() {
        for reloc in &object.relocations {
            if let Err(diag) = resolve_one_relocation(
                obj_idx,
                object,
                objects,
                reloc,
                &placed_by_section,
                &symbols,
                &mut memory_map,
            ) {
                errors.push(diag);
            }
        }
    }

    if !errors.is_empty() {
        return Err(LinkErrors(errors));
    }

    let runs = match collect_linked_runs(config, &memory_map) {
        Ok(runs) => runs,
        Err(err) => {
            return Err(LinkErrors(vec![anchorless_error(err.to_string())]));
        }
    };

    let mut listing_blocks = Vec::new();
    let include_object_index = objects.len() > 1;

    // DP block: render the auto-allocated DP layout if any DP vars exist.
    // Pinned DP vars are listed in encounter order from the DP allocator
    // (which records them as it processes objects).
    if !dp_alloc.symbols.is_empty() {
        listing_blocks.push(format_dp_listing(&dp_alloc, objects));
    }

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

        match format_section_listing(
            object_index,
            segment_name,
            chunks,
            &memory_map,
            include_object_index,
        ) {
            Ok(block) => listing_blocks.push(block),
            Err(err) => {
                return Err(LinkErrors(vec![anchorless_error(err.to_string())]));
            }
        }
    }
    match format_data_symbol_listings(objects, &placed_by_section) {
        Ok(blocks) => listing_blocks.extend(blocks),
        Err(err) => {
            return Err(LinkErrors(vec![anchorless_error(err.to_string())]));
        }
    }
    match format_function_disassembly_listings(objects, &placed_by_section, &memory_map) {
        Ok(blocks) => listing_blocks.extend(blocks),
        Err(err) => {
            return Err(LinkErrors(vec![anchorless_error(err.to_string())]));
        }
    }

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

/// Render one structured `LinkDiagnostic` into the legacy single-string
/// presentation used by `link_objects_with_options`. Kept until every caller
/// migrates to the unified `k816_core::diag` renderer (then this and the
/// helpers in `anchors.rs` go away).
fn render_link_diagnostic_legacy(diag: &LinkDiagnostic, options: LinkRenderOptions) -> String {
    if let Some(related) = diag.related.first()
        && let Some(anchor) = &diag.anchor
        && diag.message.starts_with("duplicate global symbol")
    {
        // Mirror the original two-source ariadne report for duplicate symbols.
        let name = diag
            .message
            .trim_start_matches("duplicate global symbol '")
            .trim_end_matches('\'');
        return render_duplicate_symbol_error(name, Some(&related.anchor), Some(anchor), options);
    }

    let anchor_ctx = diag.anchor.as_ref().map(|src| AnchorContext {
        symbol_name: String::new(),
        source: Some(src.clone()),
    });

    decorate_with_anchor_with_label(
        &diag.message,
        anchor_ctx.as_ref(),
        options,
        diag.primary_label.as_deref(),
        diag.help.as_deref(),
        diag.note.as_deref(),
    )
}

/// Build an anchorless error diagnostic — used for internal/structural
/// failures (config validation, memory overflows) where there is no source
/// position to attribute to the user's code.
/// Renders a `[DP]` block summarising direct-page slot assignments for both
/// pinned (`var dp NAME = $X`) and auto-allocated (`var dp NAME`) symbols.
/// Listed in DP-offset order so the page layout reads top-to-bottom; intra-row
/// rendering keeps fields together with their parent.
fn format_dp_listing(dp_alloc: &dp::DpAllocations, objects: &[O65Object]) -> String {
    let mut entries: Vec<(u8, &str, Option<u8>)> = Vec::new();
    for object in objects {
        for symbol in &object.symbols {
            match &symbol.definition {
                Some(SymbolDefinition::DirectPageFixed { offset, .. }) => {
                    entries.push((*offset, symbol.name.as_str(), Some(1)));
                }
                Some(SymbolDefinition::DirectPageAlloc { size, .. }) => {
                    if let Some(resolved) = dp_alloc.symbols.get(&symbol.name) {
                        entries.push((resolved.offset, symbol.name.as_str(), Some(*size)));
                    }
                }
                Some(SymbolDefinition::DirectPageAllocAlias { .. }) => {
                    if let Some(resolved) = dp_alloc.symbols.get(&symbol.name) {
                        // Aliases have no independent size; render them as zero-width.
                        entries.push((resolved.offset, symbol.name.as_str(), None));
                    }
                }
                _ => {}
            }
        }
    }
    entries.sort_by(|a, b| a.0.cmp(&b.0).then_with(|| a.1.cmp(b.1)));
    let mut out = String::from("[DP]\n");
    for (offset, name, size) in entries {
        match size {
            Some(size) if size > 1 => {
                out.push_str(&format!("    ${offset:02X}..${:02X}: {name}\n", offset.saturating_add(size).saturating_sub(1)));
            }
            Some(_) => {
                out.push_str(&format!("    ${offset:02X}: {name}\n"));
            }
            None => {
                out.push_str(&format!("    ${offset:02X}: {name} (alias)\n"));
            }
        }
    }
    out
}

fn anchorless_error(message: impl Into<String>) -> LinkDiagnostic {
    LinkDiagnostic {
        severity: LinkSeverity::Error,
        message: message.into(),
        primary_label: None,
        anchor: None,
        help: None,
        note: None,
        related: Vec::new(),
    }
}

fn anchored_error(
    message: String,
    label: Option<String>,
    anchor: Option<SourceLocation>,
    help: Option<String>,
) -> LinkDiagnostic {
    anchored_error_rich(message, label, anchor, help, None)
}

fn anchored_error_rich(
    message: String,
    label: Option<String>,
    anchor: Option<SourceLocation>,
    help: Option<String>,
    note: Option<String>,
) -> LinkDiagnostic {
    LinkDiagnostic {
        severity: LinkSeverity::Error,
        message,
        primary_label: label,
        anchor,
        help,
        note,
        related: Vec::new(),
    }
}

fn duplicate_symbol_diagnostic(
    name: &str,
    first: Option<&SourceLocation>,
    second: Option<&SourceLocation>,
) -> LinkDiagnostic {
    let mut diag = LinkDiagnostic {
        severity: LinkSeverity::Error,
        message: format!("duplicate global symbol '{name}'"),
        primary_label: Some("duplicate definition".to_string()),
        anchor: second.cloned(),
        help: Some(format!(
            "rename one of the `{name}` definitions, gate it behind a different file so only one is in the link group, or move it into a `func` body to make it function-local"
        )),
        note: Some(
            "All top-level `const`, `var`, function, and label definitions live in one flat global namespace shared by every linked object; the linker rejects duplicates instead of silently picking one, since either resolution would change generated code.".to_string(),
        ),
        related: Vec::new(),
    };
    if let Some(first) = first {
        diag.related.push(LinkRelated {
            anchor: first.clone(),
            message: "first defined here".to_string(),
        });
    }
    diag
}

/// Record a chunk in `placed_by_section` without writing bytes or marking
/// memory as used. Used when `place_chunk` failed (overlap / out of range)
/// so that symbols defined inside the chunk can still resolve to a nominal
/// address — preventing a "1 overlap error → N undefined-symbol cascade
/// errors" amplification that would drown the real diagnostic.
fn record_unplaced_chunk(
    chunk: &PlannedChunk,
    base: u32,
    placed_by_section: &mut HashMap<(usize, String), Vec<PlacedChunk>>,
) {
    let key = (chunk.obj_idx, chunk.segment.clone());
    placed_by_section.entry(key).or_default().push(PlacedChunk {
        section_offset: chunk.section_offset,
        len: chunk.len,
        base_addr: base,
        memory_name: chunk.memory_name.clone(),
    });
}

/// Resolve a single relocation. Returns a `LinkDiagnostic` on failure so the
/// caller can accumulate it. Reasons for failure: missing section placement,
/// site outside section, undefined symbol, range overflow on relative jumps,
/// width mismatch on byte halves.
#[allow(clippy::too_many_arguments)]
fn resolve_one_relocation(
    obj_idx: usize,
    object: &O65Object,
    objects: &[O65Object],
    reloc: &k816_o65::Relocation,
    placed_by_section: &HashMap<(usize, String), Vec<PlacedChunk>>,
    symbols: &HashMap<String, ResolvedSymbol>,
    memory_map: &mut HashMap<String, MemoryState>,
) -> std::result::Result<(), LinkDiagnostic> {
    let _ = object;
    let section_key = (obj_idx, reloc.section.clone());
    let placements = placed_by_section.get(&section_key).ok_or_else(|| {
        anchorless_error(format!(
            "relocation references unknown section '{}'",
            reloc.section
        ))
    })?;

    let (site_memory_name, site_addr) = resolve_reloc_site(placements, reloc.offset, reloc.width)
        .ok_or_else(|| {
        anchorless_error(format!(
            "relocation site {:#X}..{:#X} is outside section '{}'",
            reloc.offset,
            reloc.offset.saturating_add(u32::from(reloc.width)),
            reloc.section
        ))
    })?;

    let target = symbols.get(&reloc.symbol).cloned().ok_or_else(|| {
        let anchor = reloc.source.clone().or_else(|| {
            find_section_anchor_context(objects, obj_idx, &reloc.section, reloc.offset)
                .and_then(|ctx| ctx.source)
        });
        anchored_error_rich(
            format!("undefined symbol '{}'", reloc.symbol),
            Some(format!("symbol '{}' referenced here", reloc.symbol)),
            anchor,
            Some(format!(
                "no `func`, `var`, `const`, or top-level label named '{}' is exported by any of the linked objects; check the spelling, declare/define the symbol in one of the source files, or add the missing source to the project so the linker sees it",
                reloc.symbol
            )),
            Some(
                "Symbol resolution happens at link time across every `.o65` in the project. Missing-symbol errors usually mean a source file was not compiled into the link group, or an `inline` import / cross-unit reference was renamed.".to_string(),
            ),
        )
    })?;

    if reloc.kind == RelocationKind::Absolute
        && reloc.width == 2
        && target.segment != ABSOLUTE_SYMBOL_SEGMENT
        && reloc.section != target.segment
        && reloc.call_metadata.is_some()
    {
        return Err(anchored_error(
            format!(
                "16-bit relocation from segment '{}' to '{}' requires far addressing",
                reloc.section, target.segment
            ),
            None,
            reloc.source.clone(),
            None,
        ));
    }

    let mem = memory_map.get_mut(site_memory_name).ok_or_else(|| {
        anchorless_error(format!(
            "internal linker error: memory '{}' missing",
            site_memory_name
        ))
    })?;

    if site_addr < mem.spec.start {
        return Err(anchorless_error("relocation site underflows memory range"));
    }

    let rel_idx = (site_addr - mem.spec.start) as usize;
    let end_idx = rel_idx.saturating_add(reloc.width as usize);
    if end_idx > mem.bytes.len() {
        return Err(anchorless_error("relocation writes outside memory range"));
    }

    let effective_addr = (target.addr as i64 + reloc.addend as i64) as u32;
    match reloc.kind {
        RelocationKind::Absolute => {
            if let Err(err) = write_value(
                &mut mem.bytes[rel_idx..end_idx],
                effective_addr,
                reloc.width,
            ) {
                return Err(anchored_error(
                    err.to_string(),
                    None,
                    reloc.source.clone(),
                    None,
                ));
            }
        }
        RelocationKind::Relative => match reloc.width {
            1 => {
                let delta = effective_addr as i64 - (site_addr as i64 + 1);
                if delta < i8::MIN as i64 || delta > i8::MAX as i64 {
                    return Err(anchored_error(
                        format!(
                            "relative relocation to '{}' out of range at {:#X}",
                            reloc.symbol, site_addr
                        ),
                        None,
                        reloc.source.clone(),
                        None,
                    ));
                }
                mem.bytes[rel_idx] = delta as i8 as u8;
            }
            2 => {
                let delta = effective_addr as i64 - (site_addr as i64 + 2);
                if delta < i16::MIN as i64 || delta > i16::MAX as i64 {
                    return Err(anchored_error(
                        format!(
                            "relative relocation to '{}' out of range at {:#X}",
                            reloc.symbol, site_addr
                        ),
                        None,
                        reloc.source.clone(),
                        None,
                    ));
                }
                mem.bytes[rel_idx..end_idx].copy_from_slice(&(delta as i16).to_le_bytes());
            }
            _ => {
                return Err(anchorless_error(
                    "relative relocation width must be 1 or 2 bytes",
                ));
            }
        },
        RelocationKind::LowByte => {
            if reloc.width != 1 {
                return Err(anchorless_error("low-byte relocation width must be 1"));
            }
            mem.bytes[rel_idx] = (effective_addr & 0xFF) as u8;
        }
        RelocationKind::HighByte => {
            if reloc.width != 1 {
                return Err(anchorless_error("high-byte relocation width must be 1"));
            }
            mem.bytes[rel_idx] = ((effective_addr >> 8) & 0xFF) as u8;
        }
    }
    Ok(())
}

/// Like [`validate_call_metadata`] but returns the accumulated error list
/// directly instead of bailing — the caller merges it into the cross-phase
/// error accumulator so call-metadata problems show up alongside undefined
/// symbols, overlaps, etc.
fn collect_call_metadata_errors(
    objects: &[O65Object],
    symbols: &HashMap<String, ResolvedSymbol>,
    function_metadata_map: &HashMap<String, FunctionMetadata>,
) -> Vec<LinkDiagnostic> {
    let mut errors: Vec<LinkDiagnostic> = Vec::new();
    validate_call_metadata_inner(objects, symbols, function_metadata_map, &mut errors);
    errors
}

fn validate_call_metadata_inner(
    objects: &[O65Object],
    symbols: &HashMap<String, ResolvedSymbol>,
    function_metadata_map: &HashMap<String, FunctionMetadata>,
    errors: &mut Vec<LinkDiagnostic>,
) {
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

            let anchor = reloc.source.clone().or_else(|| {
                find_section_anchor_context(objects, obj_idx, &reloc.section, reloc.offset)
                    .and_then(|ctx| ctx.source)
            });

            if reloc.width == 2 && func_meta.is_far {
                let detail = format!("near call to far function '{}'", reloc.symbol);
                let help = format!("use `call far {}` instead", reloc.symbol);
                errors.push(anchored_error(
                    detail.clone(),
                    Some(detail),
                    anchor.clone(),
                    Some(help),
                ));
            } else if reloc.width == 3 && !func_meta.is_far {
                let detail = format!("far call to near function '{}'", reloc.symbol);
                let help = format!("use `call {}` instead", reloc.symbol);
                errors.push(anchored_error(
                    detail.clone(),
                    Some(detail),
                    anchor.clone(),
                    Some(help),
                ));
            }

            check_width_mismatch(call_meta, func_meta, &reloc.symbol, anchor.as_ref(), errors);
        }
    }
}

fn check_width_mismatch(
    call_meta: &CallMetadata,
    func_meta: &FunctionMetadata,
    symbol: &str,
    anchor: Option<&SourceLocation>,
    errors: &mut Vec<LinkDiagnostic>,
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
        let mode_tag = if callee_a { "@a16" } else { "@a8" };
        let help = format!(
            "switch the accumulator to {mode_tag} (matching '{symbol}'s declared width) before this call, or change the callee's `@a8`/`@a16` annotation if it should be the other width"
        );
        let note = "The W65C816's `m` flag picks 8- vs 16-bit accumulator opcodes; the linker reads each function's declared `@a8`/`@a16` and rejects calls where the caller's runtime mode would silently encode against the wrong width.".to_string();
        errors.push(anchored_error_rich(
            detail,
            Some(label),
            anchor.cloned(),
            Some(help),
            Some(note),
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
        let mode_tag = if callee_i { "@i16" } else { "@i8" };
        let help = format!(
            "switch the index registers to {mode_tag} (matching '{symbol}'s declared width) before this call, or change the callee's `@i8`/`@i16` annotation if it should be the other width"
        );
        let note = "The W65C816's `x` flag picks 8- vs 16-bit X/Y opcodes; the linker reads each function's declared `@i8`/`@i16` and rejects calls where the caller's runtime mode would silently encode against the wrong width.".to_string();
        errors.push(anchored_error_rich(
            detail,
            Some(label),
            anchor.cloned(),
            Some(help),
            Some(note),
        ));
    }
}

fn no_fit_diagnostic(
    rule: &super::config::SegmentRule,
    chunk: &PlannedChunk,
    anchor: u32,
    objects: &[O65Object],
    mem: &MemoryState,
) -> LinkDiagnostic {
    let anchor_ctx = find_anchor_context(objects, chunk);
    let detail = format!(
        "no free range found for relocatable chunk in segment '{}' (len={:#X})",
        chunk.segment, chunk.len
    );
    let help = format!(
        "memory '{}' has no contiguous {:#X}-byte gap at or above {:#X} that satisfies alignment {:#X}; reduce the segment '{}', raise the size of '{}' in your `.ld.ron`, or move existing fixed (`at = ...`) placements out of the way",
        mem.spec.name,
        chunk.len,
        anchor,
        chunk.align.max(1),
        rule.id,
        mem.spec.name
    );
    let note = "The linker scans each `MEMORY` region for a free, aligned span large enough to hold the chunk, starting at the segment's anchor (`start`, `offset`, or the high-water mark of previous segments). Fixed placements (`at = ...`) reserve their range up front, so adding or growing them shrinks the relocatable budget for everything else placed in the same memory.".to_string();
    anchored_error_rich(
        detail,
        anchor_ctx
            .as_ref()
            .map(|a| format!("function '{}' defined here", a.symbol_name)),
        anchor_ctx.and_then(|a| a.source),
        Some(help),
        Some(note),
    )
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
) -> std::result::Result<(), LinkDiagnostic> {
    let anchor = find_anchor_context(objects, chunk);

    let section = objects
        .get(chunk.obj_idx)
        .and_then(|object| object.sections.get(&chunk.segment))
        .ok_or_else(|| {
            anchorless_error(format!(
                "internal linker error: missing section '{}'",
                chunk.segment
            ))
        })?;
    let source_chunk = section.chunks.get(chunk.chunk_idx).ok_or_else(|| {
        anchorless_error(format!(
            "internal linker error: missing chunk {} in section '{}'",
            chunk.chunk_idx, chunk.segment
        ))
    })?;

    let mem = memory_map
        .get_mut(&chunk.memory_name)
        .ok_or_else(|| anchorless_error(format!("unknown memory area '{}'", chunk.memory_name)))?;

    let mem_end =
        mem.spec.start.checked_add(mem.spec.size).ok_or_else(|| {
            anchorless_error(format!("memory '{}' range overflow", mem.spec.name))
        })?;

    let end = base.checked_add(chunk.len).ok_or_else(|| {
        anchorless_error(format!(
            "placement overflow for segment '{}'",
            chunk.segment
        ))
    })?;

    if base < mem.spec.start || end > mem_end {
        let kind = if fixed { "fixed" } else { "relocatable" };
        let detail = format!(
            "{kind} chunk for segment '{}' at {:#X}..{:#X} is outside memory '{}' ({:#X}..{:#X})",
            chunk.segment, base, end, mem.spec.name, mem.spec.start, mem_end
        );
        let help = if fixed {
            format!(
                "memory '{}' covers {:#X}..{:#X}; the requested fixed placement at {:#X}..{:#X} sits outside it — relax or move the fixed `at` constraint, grow '{}' in your `.ld.ron`, or place this segment in a memory area that contains the requested address",
                mem.spec.name, mem.spec.start, mem_end, base, end, mem.spec.name
            )
        } else {
            format!(
                "memory '{}' covers {:#X}..{:#X}; the relocatable placement at {:#X}..{:#X} would extend past the end — grow '{}' in your `.ld.ron`, shrink the segment, or assign it to a larger memory area",
                mem.spec.name, mem.spec.start, mem_end, base, end, mem.spec.name
            )
        };
        let note = "Memory ranges in your `.ld.ron` are hard bounds: the linker never extends them, even by a single byte. Width, alignment, and any `offset = ...` add to the chunk's footprint, so a chunk that just barely fits in source can spill out after alignment.".to_string();
        return Err(anchored_error_rich(
            detail,
            anchor
                .as_ref()
                .map(|a| format!("function '{}' defined here", a.symbol_name)),
            anchor.and_then(|a| a.source),
            Some(help),
            Some(note),
        ));
    }

    let rel_start = (base - mem.spec.start) as usize;
    let rel_end = rel_start
        .checked_add(chunk.len as usize)
        .ok_or_else(|| anchorless_error("placement index overflow"))?;

    if rel_end > mem.bytes.len() {
        let detail = format!(
            "segment '{}' chunk placement exceeds memory '{}': start={:#X}, len={:#X}",
            chunk.segment, mem.spec.name, base, chunk.len
        );
        return Err(anchored_error(
            detail,
            anchor
                .as_ref()
                .map(|a| format!("function '{}' defined here", a.symbol_name)),
            anchor.and_then(|a| a.source),
            None,
        ));
    }

    if let Some(conflict) = mem.used[rel_start..rel_end].iter().position(|used| *used) {
        let overlap_addr = base + conflict as u32;
        if fixed {
            let detail = format!(
                "fixed chunk for segment '{}' at {:#X}..{:#X} overlaps existing placement near {:#X} in memory '{}'",
                chunk.segment, base, end, overlap_addr, mem.spec.name
            );
            let help = format!(
                "move the fixed placement of segment '{}' to a free range in '{}', or relax the `at = ...` constraint so the linker can place it automatically around the existing chunk near {:#X}",
                chunk.segment, mem.spec.name, overlap_addr
            );
            let note = "The linker treats `at = ...` placements as immovable: when two pieces of code are pinned to overlapping addresses, neither can move out of the way, so the only fix is to change the address (or remove one pin). Relocatable placements are only laid down after every fixed range is reserved.".to_string();
            return Err(anchored_error_rich(
                detail,
                anchor
                    .as_ref()
                    .map(|a| format!("function '{}' defined here", a.symbol_name)),
                anchor.and_then(|a| a.source),
                Some(help),
                Some(note),
            ));
        }

        let detail = format!(
            "relocatable chunk for segment '{}' cannot be placed at {:#X}..{:#X}; overlap near {:#X} in memory '{}'",
            chunk.segment, base, end, overlap_addr, mem.spec.name
        );
        let help = format!(
            "the linker tried to relocate segment '{}' but every candidate range it considered above {:#X} in '{}' was already taken; reduce the segment, raise the size of '{}' in your `.ld.ron`, or move blocking fixed (`at = ...`) placements out of the way",
            chunk.segment, base, mem.spec.name, mem.spec.name
        );
        let note = "Relocatable chunks are placed after fixed (`at = ...`) chunks reserve their ranges. If a relocatable chunk cannot find any free aligned span large enough, the layout fails — even when the total free bytes would be sufficient if defragmented.".to_string();
        return Err(anchored_error_rich(
            detail,
            anchor
                .as_ref()
                .map(|a| format!("function '{}' defined here", a.symbol_name)),
            anchor.and_then(|a| a.source),
            Some(help),
            Some(note),
        ));
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
                anyhow::bail!("absolute relocation does not fit in 24 bits");
            }
            let bytes = value.to_le_bytes();
            slot.copy_from_slice(&bytes[..3]);
        }
        _ => anyhow::bail!("unsupported relocation width: {width}"),
    }
    Ok(())
}

#[allow(dead_code)]
fn unused_decorate_helpers_keep_compile_happy() {
    // These helpers (`decorate_with_anchor`, `decorate_with_anchor_with_label`)
    // are still used by the legacy renderer. Reference them so dead-code
    // warnings stay quiet if a future caller drops the legacy path.
    let _ = decorate_with_anchor;
    let _ = decorate_with_anchor_with_label;
}
