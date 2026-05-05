# K816 Linker RON Format (`.ld.ron`)

This document describes the linker configuration format used by:

- `k816 link ... -T path/to/link.ld.ron`
- internal linker APIs in `crates/link`

The format is RON and deserializes into `LinkerConfig`.

## Compatibility Scope

- `format: "o65-link"` identifies k816 linker-config metadata.
- k816 object files use o65 magic/header framing, but carry a k816-defined
  payload model.
- As of February 13, 2026, interoperability with external o65
  compilers/linkers is not guaranteed.
- This document describes k816 behavior/contracts, not the canonical o65
  specification.

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
  id: String,              // rule id (diagnostics / referencing)
  load: String,            // target memory area name
  run: Option<String>,     // currently parsed but not used
  align: Option<u32>,      // default when omitted: 1
  start: Option<u32>,      // segment anchor: absolute address inside `load`
  offset: Option<u32>,     // segment anchor: bytes from `load.start`
  optional: bool,          // currently parsed but not used
  segment: Option<String>, // segment selector
)
```

A segment rule describes the placement of one logical *segment* — the
aggregate of all section chunks (potentially from multiple object files)
that route to this rule. Exactly one of three anchor modes applies:

- `start: Some(X)` — the segment is anchored at the absolute address `X`
  within `load`.
- `offset: Some(N)` — the segment is anchored at `load.start + N`.
- Neither — the segment is anchored immediately after the previous rule's
  highest placed byte in the same `load` memory, in `segments` declaration
  order. The first unanchored rule in a memory area starts at `load.start`.

`start` and `offset` are mutually exclusive; specifying both is rejected at
config load.

Segment-rule selection for an object section named `S`:

1. First rule where `segment == Some(S)`.
2. Otherwise, first rule with no `segment` set (fallback/default rule).
3. Otherwise, link fails with `no segment rule found for segment 'S'`.

Validation (rejected at config load with a clear diagnostic):

- `id` must be non-empty.
- `id` values must be unique across `segments`.
- `align`, when set, must be non-zero.
- `load` must reference a defined memory area.
- A rule must not set both `start` and `offset`.
- `start`, when set, must lie inside `[load.start, load.start + load.size)`.
- `offset`, when set, must be less than `load.size`.

Placement behavior:

- Relocatable chunks belonging to one rule are processed in compile order
  (`obj_idx`, then in-section order). Each chunk is independently
  first-fit-placed at or above the segment's anchor — i.e. the search
  slides past existing fixed-address (`at = ...`) occupants. Smaller
  later chunks may fall into holes that earlier larger chunks skipped.
- Rules are processed in `segments` declaration order, so an unanchored
  rule sees the high-water mark left by previous rules in the same `load`
  memory.
- Source chunks with explicit fixed addresses are placed before any
  relocatable chunks and must fit without overlap.

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

## Direct-page (DP) variables — invisible to RON

DP-class variables (`var dp NAME` / `var dp NAME = $X`) are deliberately **not**
configurable through `.ld.ron`. The 65816 direct page is runtime-relocatable
(any 16-bit base via `tcd`/`pld`) and lives in an addressing space disjoint
from the program-bank and data-bank spaces, so the linker treats DP as a
**256-byte logical pool** rather than a memory region:

- There is no `MemoryKind::DirectPage`, no DP segment rule, no `__dp__`
  section. Adding entries to `memory:` or `segments:` for DP has no effect.
- DP allocations emit zero bytes into the output binary. The user is
  responsible for setting the `D` register at runtime and ensuring whatever
  bytes back the DP window are actually present in RAM.
- Auto-allocation is by **first-fit** in declaration order within each
  compilation unit, then in **link-input order** between units. Pinned
  slots (`var dp NAME = $X`) reserve their byte range so the auto-allocator
  skips it. Multiple pinned vars at the same offset are allowed by design
  (intentional aliasing).
- Total DP usage across all linked objects must fit in 256 bytes. Overflow
  produces a rich diagnostic at link time pointing at the failing
  declaration.
- The linker emits a `[DP]` block at the top of the listing showing every
  resolved DP slot (pinned + auto + symbolic-subscript-field aliases).

`var far NAME` is the other special case: FAR vars must always carry an
explicit `= <addr>` initializer because the assembler has no notion of
which 64K bank to place an unaddressed FAR symbol in. FAR placement
therefore needs no linker-config help either.

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
   - Else adjacent `<input>.ld.ron` if present.
   - Else built-in stub config.
2. If selected config has `output.file`, that path is used.
3. Else default output path is `<input-stem>.bin` for `RawBinary` or `<input-stem>.xex` for `Xex`.

## Common Patterns

### Default XEX output by omitting `output`

```ron
(
  memory: [ (name: "MAIN", start: 0, size: 65536, kind: ReadWrite, fill: Some(0)) ],
  segments: [ (id: "DEFAULT", load: "MAIN", run: None, align: Some(1), start: None, offset: None, optional: false, segment: None) ],
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

## Cross-Unit Function Call Validation

When linking multiple compilation units, the linker validates calling convention
and register width consistency at each call site.

### Calling Convention Mismatch

The linker checks that near calls (`JSR`, 2-byte operand) target near functions
(`func`) and far calls (`JSL`, 3-byte operand) target far functions
(`far func`). Mismatches produce errors:

```
error: near call to far function 'lib_init': use `call far lib_init` instead
error: far call to near function 'app_start': use `call app_start` instead
```

### Register Width Mismatch

Within a single compilation unit, the compiler automatically inserts `REP`/`SEP`
instructions when the caller's accumulator or index register width differs from
the callee's declared contract (`@a8`, `@a16`, `@i8`, `@i16`).

Across compilation units, the compiler cannot see the callee's contract and
assumes a match. The linker verifies this assumption by comparing the caller's
register width state at each call site against the callee's declared contract.
When both are specified and they differ, the linker reports an error:

```
error: accumulator width mismatch calling 'app_init': caller is 8-bit, callee expects 16-bit
error: index register width mismatch calling 'app_init': caller is 8-bit, callee expects 16-bit
```

### Metadata Encoding

Function metadata (calling convention, register width contract) is stored in the
o65 object file alongside symbol definitions. Call-site metadata (caller's
register width state) is stored alongside relocations. Both are encoded under
payload version 7 and are backward-compatible — older objects without metadata
are linked without validation.
