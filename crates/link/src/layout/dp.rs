//! Direct-page (DP) allocator. DP is a 256-byte logical pool decoupled from
//! segments and memory areas — the runtime sets the D register at any 16-bit
//! base via `tcd`/`pld`, so the linker only tracks 8-bit offsets within the
//! page. `var dp NAME` requests a slot of size N; `var dp NAME = $X` pins a
//! specific offset (multiple pinned vars may intentionally share an offset).
//! Symbolic-subscript field aliases ride along with their parent's slot.

use std::collections::HashMap;

use k816_o65::{O65Object, SourceLocation, SymbolDefinition};

use crate::types::{LinkDiagnostic, LinkSeverity};

#[derive(Debug, Clone)]
pub(super) struct DpResolved {
    pub(super) offset: u8,
    /// Source location of the original declaration. Surfaces via the link
    /// layout when LSP/CLI rendering wants to anchor a hover or warning at the
    /// var declaration site.
    #[allow(dead_code)]
    pub(super) source: Option<SourceLocation>,
}

#[derive(Debug, Clone, Default)]
pub(super) struct DpAllocations {
    /// Final 8-bit offset for every DP-class symbol (pinned + auto + alias).
    pub(super) symbols: HashMap<String, DpResolved>,
    /// Auto-allocated symbols in placement order, for stable listing output.
    pub(super) listing_order: Vec<(String, u8, u8)>,
}

/// Walks every input object's symbol table, pins fixed DP slots, runs first-fit
/// allocation in input order, and resolves field aliases. Returns both the
/// (possibly partial) allocation map and any overflow diagnostics so callers
/// can report the failures while still resolving the symbols that *did* fit —
/// avoiding cascade "undefined symbol" noise for vars allocated before the
/// failing one.
pub(super) fn allocate(objects: &[O65Object]) -> (DpAllocations, Vec<LinkDiagnostic>) {
    let mut errors = Vec::new();
    let mut alloc = DpAllocations::default();
    let mut used: [bool; 256] = [false; 256];

    // Pin pass — mark every DirectPageFixed slot as used. Multiple `var dp foo
    // = $X` declarations may share an offset (intentional aliasing); we just
    // mark the bitmap and record each name.
    for object in objects {
        for symbol in &object.symbols {
            let Some(SymbolDefinition::DirectPageFixed { offset, source }) = &symbol.definition
            else {
                continue;
            };
            used[*offset as usize] = true;
            alloc.symbols.insert(
                symbol.name.clone(),
                DpResolved {
                    offset: *offset,
                    source: source.clone(),
                },
            );
        }
    }

    // Allocate pass — first-fit, following source order *within* each object
    // (so two DP vars declared in the same file get DP offsets in declaration
    // order, not alphabetical) and link-input order *between* objects.
    for object in objects {
        let mut alloc_requests: Vec<(&String, u8, &Option<SourceLocation>)> = object
            .symbols
            .iter()
            .filter_map(|symbol| match &symbol.definition {
                Some(SymbolDefinition::DirectPageAlloc { size, source }) => {
                    Some((&symbol.name, *size, source))
                }
                _ => None,
            })
            .collect();
        alloc_requests.sort_by_key(|(name, _, source)| {
            (
                source
                    .as_ref()
                    .map(|s| (s.line, s.column))
                    .unwrap_or((u32::MAX, u32::MAX)),
                (*name).clone(),
            )
        });
        for (name, size, source) in alloc_requests {
            let size = size as usize;
            if size == 0 {
                alloc.symbols.insert(
                    name.clone(),
                    DpResolved {
                        offset: 0,
                        source: source.clone(),
                    },
                );
                continue;
            }
            match find_first_fit(&used, size) {
                Some(start) => {
                    for slot in &mut used[start..start + size] {
                        *slot = true;
                    }
                    let offset = start as u8;
                    alloc.symbols.insert(
                        name.clone(),
                        DpResolved {
                            offset,
                            source: source.clone(),
                        },
                    );
                    alloc
                        .listing_order
                        .push((name.clone(), offset, size as u8));
                }
                None => {
                    errors.push(dp_overflow_diagnostic(
                        name,
                        size,
                        bytes_used(&used),
                        source.clone(),
                    ));
                }
            }
        }
    }

    // Alias pass — resolve aliases to (parent_offset + field_offset). Done last
    // so all parents have their final offsets, regardless of object order.
    for object in objects {
        for symbol in &object.symbols {
            let Some(SymbolDefinition::DirectPageAllocAlias {
                parent,
                field_offset,
                source,
            }) = &symbol.definition
            else {
                continue;
            };
            let Some(parent_resolved) = alloc.symbols.get(parent) else {
                // Parent missing — likely already errored elsewhere (overflow,
                // unknown name). Skip silently rather than cascade.
                continue;
            };
            let combined = parent_resolved.offset.saturating_add(*field_offset);
            alloc.symbols.insert(
                symbol.name.clone(),
                DpResolved {
                    offset: combined,
                    source: source.clone(),
                },
            );
        }
    }

    (alloc, errors)
}

fn find_first_fit(used: &[bool; 256], size: usize) -> Option<usize> {
    if size > 256 {
        return None;
    }
    let mut run = 0_usize;
    for (i, &occupied) in used.iter().enumerate() {
        if occupied {
            run = 0;
            continue;
        }
        run += 1;
        if run >= size {
            return Some(i + 1 - size);
        }
    }
    None
}

fn bytes_used(used: &[bool; 256]) -> usize {
    used.iter().filter(|b| **b).count()
}

fn dp_overflow_diagnostic(
    name: &str,
    requested: usize,
    used_bytes: usize,
    source: Option<SourceLocation>,
) -> LinkDiagnostic {
    let free = 256 - used_bytes;
    LinkDiagnostic {
        severity: LinkSeverity::Error,
        message: format!(
            "DP pool exhausted while allocating '{name}' ({requested} bytes requested, {free} bytes free out of 256)"
        ),
        primary_label: Some("no DP slot available".to_string()),
        anchor: source,
        help: Some(format!(
            "consolidate DP usage by moving non-hot vars to absolute storage (`var {name}` instead of `var dp {name}`), or pin selected DP vars to specific offsets to free contiguous runs"
        )),
        note: Some(
            "DP is a 256-byte logical pool. The runtime decides where it physically lives via `tcd`/`pld`; the assembler only tracks 8-bit offsets within the page, so total DP allocation cannot exceed 256 bytes per link group.".to_string(),
        ),
        related: Vec::new(),
    }
}
