# Per-Instruction Address Mapping for Hovers (and future DAP breakpoints)

## Context

Currently, hovers show addresses only for named symbols (functions, labels) via `resolved_addresses: HashMap<String, u32>` populated from the linker's symbol table. The user wants **per-instruction granularity** — hovering over ANY instruction should show its absolute memory address. This replaces the symbol-level approach with span-matched instruction-level address resolution, all in memory (no O65 format changes).

The linker is the authority on where instructions are placed (especially with BFS section sorting), so we must use the linker's section placement data to resolve section-relative offsets to absolute addresses.

This also lays the groundwork for DAP breakpoint resolution (translating source lines to memory addresses).

## Architecture

```
emit_object() ──→ Vec<AddressableSite> { segment, offset, span }
                    │ stored per document in LSP
                    ↓
try_link_workspace() ──→ LinkOutput.section_placements
                    │ resolve each site: (obj_idx, segment, offset) → address
                    ↓
DocumentState.resolved_sites: Vec<(Span, u32)>
                    │ sorted by span.start, binary-searchable
                    ↓
hover() ──→ address_at_offset(cursor_byte_offset) → Option<u32>
```

## Changes

### 1. `crates/core/src/emit_object.rs` — capture instruction spans

New public struct (alongside existing `EmitObjectOutput`):

```rust
#[derive(Debug, Clone)]
pub struct AddressableSite {
    pub segment: String,
    pub offset: u32,
    pub span: Span,
}
```

Extend `EmitObjectOutput`:

```rust
pub struct EmitObjectOutput {
    pub object: O65Object,
    pub addressable_sites: Vec<AddressableSite>,
}
```

In `emit_object()`, add `let mut addressable_sites = Vec::new();` and push a site for each byte-generating op:

| Op | What to capture |
| --- | --- |
| `Op::Label(_)` | `(current_segment, segment.section_offset, op.span)` |
| `Op::Instruction(_)` | `(current_segment, opcode_offset, op.span)` — opcode_offset already computed |
| `Op::Rep(_)` / `Op::FixedRep(_)` | `(current_segment, opcode_offset, op.span)` |
| `Op::Sep(_)` / `Op::FixedSep(_)` | `(current_segment, opcode_offset, op.span)` |
| `Op::EmitBytes(_)` | `(current_segment, segment.section_offset, op.span)` — before appending |
| `Op::EmitRelocBytes { .. }` | `(current_segment, segment.section_offset, op.span)` — before appending |

Not captured (no meaningful address): `SelectSegment`, `FunctionStart/End`, `SetMode`, `Align`, `Address`, `Nocross`, `DefineAbsoluteSymbol`.

Note: `Op::Address` directives (which can appear inside functions and data blocks to place code at fixed addresses) are NOT captured as sites — they don't emit bytes. However, they create new `PlacedChunk` entries in the linker, so subsequent instructions captured at their section offsets will be correctly resolved to the fixed addresses via `resolve_symbol_addr`'s multi-chunk handling.

### 2. `crates/core/src/driver.rs` — pass through pipeline

Extend `CompileObjectOutput`:

```rust
pub struct CompileObjectOutput {
    pub object: O65Object,
    pub addressable_sites: Vec<AddressableSite>,
    pub warnings: Vec<Diagnostic>,
    pub rendered_warnings: String,
}
```

In `compile_source_to_object_with_fs_and_options()` (line 204), pass `emit_output.addressable_sites` through:

```rust
Ok(CompileObjectOutput {
    object: emit_output.object,
    addressable_sites: emit_output.addressable_sites,
    warnings,
    rendered_warnings,
})
```

Re-export `AddressableSite` from `crates/core/src/lib.rs`.

### 3. `crates/link/src/lib.rs` — expose section placements

Make public:

- `PlacedChunk` struct (fields: `section_offset`, `len`, `base_addr`, `memory_name`)
- `resolve_symbol_addr(placements: &[PlacedChunk], offset: u32) -> Option<u32>` function

Add to `LinkOutput`:

```rust
pub struct LinkOutput {
    pub bytes: Vec<u8>,
    pub kind: OutputKind,
    pub listing: String,
    pub symbols: HashMap<String, u32>,             // keep for CLI usage
    pub section_placements: HashMap<(usize, String), Vec<PlacedChunk>>,  // NEW
}
```

Populate by cloning `placed_by_section` into the output (line ~607):

```rust
section_placements: placed_by_section,   // move instead of drop
```

### 4. `crates/lsp/src/lib.rs` — build address map, update hover

#### DocumentState changes

```rust
struct DocumentState {
    // ... existing fields ...
    object: Option<k816_o65::O65Object>,
    addressable_sites: Vec<k816_core::emit_object::AddressableSite>,  // NEW
    resolved_sites: Vec<(k816_core::span::Span, u32)>,                // NEW (sorted by span.start)
}
```

Helper method:

```rust
impl DocumentState {
    fn address_at_offset(&self, offset: usize) -> Option<u32> {
        // Binary search: find last span whose start <= offset, check if offset < span.end
        let idx = self.resolved_sites.partition_point(|(span, _)| span.start <= offset);
        if idx > 0 {
            let (span, addr) = &self.resolved_sites[idx - 1];
            if offset < span.end { return Some(*addr); }
        }
        None
    }
}
```

#### ServerState changes

- **Remove** `resolved_addresses: HashMap<String, u32>` field and its initialization

#### `analyze_document()` changes (free function, line 1068)

Return `addressable_sites` alongside the O65 object:

```rust
fn analyze_document(
    source_name: &str,
    source_text: &str,
) -> (DocumentAnalysis, Option<k816_o65::O65Object>, Vec<AddressableSite>) {
    match k816_core::compile_source_to_object_with_options(...) {
        Ok(output) => (... , Some(output.object), output.addressable_sites),
        Err(error) => (... , None, Vec::new()),
    };
    // ...
}
```

Update all 3 call sites of `analyze_document` to handle the new tuple element and store in `DocumentState.addressable_sites`.

#### `try_link_workspace()` changes

Track URI → obj_idx mapping, then resolve sites after linking:

```rust
fn try_link_workspace(&mut self) {
    // Collect (uri, object) pairs — track index correlation
    let doc_entries: Vec<(Uri, &O65Object)> = self.documents.iter()
        .filter_map(|(uri, doc)| doc.object.as_ref().map(|obj| (uri.clone(), obj)))
        .collect();

    if doc_entries.is_empty() {
        // Clear all resolved_sites
        for doc in self.documents.values_mut() { doc.resolved_sites.clear(); }
        return;
    }

    let objects: Vec<O65Object> = doc_entries.iter().map(|(_, obj)| (*obj).clone()).collect();

    match k816_link::link_objects_with_options(&objects, &self.linker_config, ...) {
        Ok(output) => {
            // Resolve each document's addressable_sites using linker placements
            for (obj_idx, (uri, _)) in doc_entries.iter().enumerate() {
                if let Some(doc) = self.documents.get_mut(uri) {
                    let mut sites = Vec::new();
                    for site in &doc.addressable_sites {
                        let key = (obj_idx, site.segment.clone());
                        if let Some(placements) = output.section_placements.get(&key) {
                            if let Some(addr) = k816_link::resolve_symbol_addr(placements, site.offset) {
                                sites.push((site.span, addr));
                            }
                        }
                    }
                    sites.sort_by_key(|(span, _)| span.start);
                    doc.resolved_sites = sites;
                }
            }
        }
        Err(_) => {
            for doc in self.documents.values_mut() { doc.resolved_sites.clear(); }
        }
    }
}
```

#### `hover()` changes (line 532)

After building hover content (whether from symbol lookup or builtin text), also check the address map:

```rust
fn hover(&self, uri: &Uri, position: Position) -> Option<Hover> {
    let doc = self.documents.get(uri)?;
    let offset = doc.line_index.to_offset(&doc.text, position)?;
    // ... existing token extraction and symbol lookup ...

    // Try symbol hover (existing logic)
    if let Some(definitions) = self.symbols.get(&canonical) ... {
        let contents = hover_contents_for_symbol(&canonical, definition, self);
        // Append address from resolved_sites if not already shown
        // (address_at_offset uses the cursor position, not the symbol definition)
        ...
    }

    // Try builtin hover (existing logic)
    // After getting builtin text, also check for instruction address
    if let Some(addr) = doc.address_at_offset(offset) {
        // Append "- address: `$XXXX`" to hover content
    }
}
```

#### `hover_contents_for_symbol()` changes (line 1622)

Replace `state.resolved_addresses.get(canonical)` lookups (lines 1641, 1660) with span-based lookup:

```rust
// For functions (was: state.resolved_addresses.get(canonical)):
if let Some(doc) = state.documents.get(&symbol.uri) {
    if let Some(addr) = doc.address_at_offset(symbol.selection.start) {
        lines.push(format!("- address: `{}`", format_address(addr)));
    }
}

// For fallback symbols (labels, data blocks — was: state.resolved_addresses.get(canonical)):
if let Some(doc) = state.documents.get(&symbol.uri) {
    if let Some(addr) = doc.address_at_offset(symbol.selection.start) {
        text.push_str(&format!("\n- address: `{}`", format_address(addr)));
    }
}
```

Variables keep `VarMeta.address` (set during semantic analysis, not linking).

For absolute symbols (`DefineAbsoluteSymbol`): check the O65 object's symbol definitions for `SymbolDefinition::Absolute { address, .. }` as a fallback.

## Files to modify

| File | Change |
| --- | --- |
| `crates/core/src/emit_object.rs` | Add `AddressableSite`, capture spans for byte-generating ops |
| `crates/core/src/driver.rs` | Add `addressable_sites` to `CompileObjectOutput`, pass through |
| `crates/core/src/lib.rs` | Re-export `AddressableSite` |
| `crates/link/src/lib.rs` | Make `PlacedChunk` + `resolve_symbol_addr` public, add `section_placements` to `LinkOutput` |
| `crates/lsp/src/lib.rs` | Add `addressable_sites` + `resolved_sites` to `DocumentState`, remove `resolved_addresses` from `ServerState`, update `try_link_workspace()`, update `hover()` + `hover_contents_for_symbol()` |

## Verification

1. `cargo build` — all crates compile
2. `cargo test` — existing tests pass
3. Open a K65 file in VSCode with k816 LSP running
4. Hover over a function name → see address (same as before, from span lookup)
5. Hover over an instruction opcode (`lda`, `sta`) → see address (NEW)
6. Hover over a label → see address
7. Compare addresses against listing output to verify correctness
