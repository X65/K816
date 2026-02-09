# K816 Linker RON Format (`.k816ld.ron`)

This document describes the linker configuration format used by:

- `k816 link ... -T path/to/link.k816ld.ron`
- internal linker APIs in `crates/link`

The format is RON and deserializes into `LinkerConfig`.

## Minimal Example

```ron
(
  format: "o65-link",
  target: Some("atari-demo"),
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
    kind: Xex,
    file: Some("game.xex"),
  ),
  entry: None,
)
```

## Top-Level Schema

```ron
(
  format: String,            // optional, default: "o65-link"
  target: Option<String>,    // optional
  memory: [MemoryArea],      // required, non-empty
  segments: [SegmentRule],   // optional, default: []
  symbols: [LinkSymbol],     // optional, default: []
  output: OutputSpec,        // optional, default: (kind: Xex, file: None)
  entry: Option<String>,     // optional
)
```

## Field Reference

### `format`

- Type: `String`
- Default: `"o65-link"`
- Notes: currently parsed as metadata; there is no strict runtime validation against a fixed set.

### `target`

- Type: `Option<String>`
- Notes: currently parsed as metadata.

### `memory`

- Type: array of `MemoryArea`
- Required: yes, and must contain at least one item.

`MemoryArea`:

```ron
(
  name: String,          // memory area name (referenced by segments[*].load)
  start: u32,            // inclusive base address
  size: u32,             // size in bytes
  kind: ReadOnly | ReadWrite,
  fill: Option<u8>,      // default byte for unused range; if None, defaults to 0
)
```

Notes:

- `kind` is parsed but not currently used to enforce write protection.
- Placement range is `[start, start + size)`.

### `segments`

- Type: array of `SegmentRule`
- Default: `[]`

`SegmentRule`:

```ron
(
  name: String,            // rule name (diagnostics / readability)
  load: String,            // target memory area name
  run: Option<String>,     // currently parsed but not used
  align: Option<u32>,      // default when omitted: 1
  start: Option<u32>,      // placement base
  offset: Option<u32>,     // added after start/cursor and alignment
  optional: bool,          // currently parsed but not used
  segment: Option<String>, // canonical segment selector
  bank: Option<String>,    // legacy alias for segment
)
```

Segment-rule selection for an object section named `S`:

1. First rule where `segment == Some(S)` (or legacy `bank == Some(S)`).
2. Otherwise, first rule with no `segment`/`bank` set (fallback/default rule).
3. Otherwise, link fails with `no segment rule found for segment 'S'`.

Placement behavior:

- `align` of `0` is invalid.
- Relocatable chunks are first-fit placed in `load` memory.
- If `start` is set, the linker uses that aligned base directly (plus `offset` if present) instead of searching.
- Source chunks with explicit fixed addresses are placed before relocatable chunks and must fit/no-overlap.

### `symbols`

- Type: array of `LinkSymbol`
- Default: `[]`

`LinkSymbol`:

```ron
(
  name: String,
  value: Absolute(u32) | Import(String),
)
```

Semantics:

- `Absolute(ADDR)` defines a linker symbol with a fixed numeric value.
- `Import("name")` aliases another resolved symbol.
- Link symbols are applied after object symbols and can override same-name object symbols.

### `output`

- Type: `OutputSpec`
- Default: `(kind: Xex, file: None)`

`OutputSpec`:

```ron
(
  kind: RawBinary | Xex, // default: Xex
  file: Option<String>,  // optional output path hint
)
```

`RawBinary` semantics:

- Produces raw bytes without container headers.
- Requires all placed data to be in exactly one **contiguous** used range of one memory area.
- If data lands in multiple memory areas or multiple discontiguous ranges, linking fails with an ambiguity error.

`Xex` semantics:

- Emits Atari XEX blocks from each used memory run.
- File starts with `0xFFFF` header when there is at least one run.
- Each block stores `start`/`end` as little-endian 16-bit addresses plus block data.
- Run addresses must fit in 16-bit (`0x0000..=0xFFFF`), otherwise linking fails.

### `entry`

- Type: `Option<String>`
- Notes: currently parsed as metadata.

## CLI Output Path Resolution

`k816` also supports an explicit format override:

- `--output-format raw`
- `--output-format xex`

Format precedence is:

1. `--output-format` switch (if provided)
2. Output file extension (`.bin` => `RawBinary`, `.xex` => `Xex`)
3. `output.kind` from config (or its default if omitted)

When using `k816 link`:

1. `-o/--output` has highest priority.
2. Else `output.file` from config is used.
3. Else command fails with:
   `output file must be provided via linker config output.file or -o`

If `output.file` is relative, it is resolved relative to the linker config file directory.

When using direct build mode (`k816 input.k65` or `k816 -T config.ron input.k65`):

1. Config selection order:
   - Explicit `-T/--config` path if provided.
   - Else adjacent `<input>.k816ld.ron` if present.
   - Else adjacent `<input>.k816.ron` if present.
   - Else built-in stub config.
2. If selected config has `output.file`, that path is used.
3. Else default output path is `<input-stem>.bin` for `RawBinary` or `<input-stem>.xex` for `Xex`.

## Common Patterns

### Default XEX output by omitting `output`

```ron
(
  memory: [ (name: "MAIN", start: 0, size: 65536, kind: ReadWrite, fill: Some(0)) ],
  segments: [ (name: "DEFAULT", load: "MAIN", run: None, align: Some(1), start: None, offset: None, optional: false, segment: None) ],
)
```

Equivalent output defaults:

- `kind: Xex`
- `file: None`

### Explicit raw binary output

```ron
output: (
  kind: RawBinary,
  file: Some("game.bin"),
)
```

## Compatibility Notes

- `segment` is the canonical selector in segment rules.
- `bank` is accepted as a legacy alias for backward compatibility.
