# K65 Dialect - Symbolic Subscripts for Arrays (Field-List Overlays)

## Goal

Implement Pawn-style "symbolic subscripts for arrays" in the K65 dialect of the K816 compiler, as a compact way to define a packed, typed field layout over a fixed memory address.

This feature must:

- avoid introducing an explicit `record/struct` block syntax
- unify with existing K65 access style (dot access, brackets)
- be overlay-only: `var` does not emit data; `data{}` is the only way to emit bytes

## User-Facing Syntax

### Overlay declaration (only valid form)

```k65
var foo[
  .field_w   :word
  .field_w2  :word
  .idx       :byte
  .string[20]:byte
] = 0x1234
```

Meaning:

- `foo` is a symbol bound to base address `0x1234`
- the field list defines a packed layout (offset table)
- no bytes are emitted for `foo`

### Field declaration rules

Field item grammar:

- `.name :type`
- `.name[count] :type`

Where:

- `name` is an identifier
- `count` is a compile-time constant integer expression (prefer literal in v1)
- `type` is one of:

  - `:byte` (1 byte)
  - `:word` (2 bytes, little-endian)

Separators:

- fields separated by newline or comma
- trailing separator allowed

### Default width declaration form

Declaring a width on the symbol itself is also allowed, as a catch-all for declaring default width of fields without repeating `:byte` or `:word`:

```k65
var baz:byte[
  .a
  .b
  .c
  .len :word
] = 0x2244
```

This should keep the current form of defining a length of symbol:

```k65
var bay:byte[20] = 0x1010
var bax:word[20] = 0x2020
```

### Access rules

Dot sugar (preferred):

```k65
foo.field_w = a = 0xBEEF
foo.idx = a = 7
foo.string[i] = a = 'A'
```

Optional explicit symbolic indexing (equivalent to dot):

```k65
foo[.field_w] = a = 0xBEEF
foo[.string][i] = a = 'A'
```

Hard errors:

- numeric/expression indexing on the aggregate itself:

  - `foo[0]`, `foo[i]`, `foo[expr]` are invalid
- unknown field name:

  - `foo.nope` or `foo[.nope]`

### Access via views

Accessing fields via :byte and :word views should be allowed:

```k65
var foo[
  .field_w  :word
  .field_b1 :byte
  .field_b2 :byte
] = 0x1234

@a8
foo.field_w:byte = a = 0x12
@a16
foo.field_b1:word = a = 0x1234
```

### Address expression constraints

The RHS of `= <addr>` must be a compile-time constant expression:

- integer literals: `0x1234`
- symbols/labels: `MMIO_BASE`
- constant arithmetic: `MMIO_BASE + 0x10`, `LABEL - 2`

Runtime expressions are rejected.

## Semantics

### Layout (packed)

Packed, sequential offsets, no padding/alignment.

- `offset(first) = 0`
- `offset(next) = offset(prev) + size(prev)`
- `size(field) = width(type) * count`, with `count = 1` if omitted
- `width(byte)=1`, `width(word)=2`

Example above:

- `.field_w`   offset 0 size 2
- `.field_w2`  offset 2 size 2
- `.idx`       offset 4 size 1
- `.string`    offset 5 size 20
  Total size 25 bytes (useful for diagnostics, not allocation).

### Lowering model

`foo.field` and `foo[.field]` lower to:

- base address expression of `foo` + constant byte offset(field)

If a field is an array (`[count]`), the field access yields an lvalue slice:

- base = foo + offset(field)
- element width from type
- index operations (`foo.string[i]`) behave like normal array indexing on that slice

## Diagnostics

Required diagnostics:

- missing `= <addr>` in a field-list `var` declaration
- non-constant RHS address expression
- duplicate field names in the same list
- unknown field access (with suggestions)
- invalid indexing:

  - numeric index on aggregate (`foo[1]`)
  - expression index on aggregate (`foo[i]`)
- invalid/negative/zero array counts (decide: count must be >= 1)

## Implementation Plan

### 1. Parser (K65 dialect gated)

- Extend `var` parsing to accept:

  - identifier
  - field-list block: `[...]`
  - required `= <const_expr>`
- Parse field items:

  - `.ident`
  - optional `[const_expr]`
  - `:byte` or `:word`

Notes:

- Ensure `.` inside the field list is unambiguous with other grammar.
- Keep this construct dialect-scoped (K65) so core K816 syntax does not change unless desired.

### 2. AST / HIR nodes

Add a new declaration kind (names illustrative):

- `VarDecl::OverlayFieldList { name, fields, base_addr_expr }`

Field representation:

- `FieldDecl { name, ty, count_opt }`

### 3. Semantic analysis

- Validate all diagnostics listed above
- Resolve field layout into a `FieldLayout` table:

  - `name -> { offset_bytes, ty, count }`
- Compute total size (for debug/diagnostics)

### 4. Name resolution and typing

- `foo` binds as an address-valued symbol (overlay base)
- `foo.field` binds as an lvalue at `foo + offset`
- array field yields an lvalue slice type

### 5. Lowering to IR

Lower accesses into address computations:

- `addr = base(foo) + const_offset`
- For arrays: `addr + (index * elem_width)`

Ensure this integrates with existing K65 addressing modes and any `far`/bank rules if present.

### 6. Codegen

- Byte field load/store uses byte ops
- Word field load/store uses little-endian word ops
- Array fields use element width for indexing and ops

### 7. Tests (golden)

Add fixtures covering:

- parsing of multi-line field lists, commas, trailing separators
- correct offset calculation and access lowering
- errors:

  - `foo[1]`
  - unknown field
  - duplicate fields
  - missing `=`
  - non-constant base expression
- codegen sanity:

  - store to `foo.idx`
  - store/load to `foo.field_w`
  - indexed store to `foo.string[i]`

Prefer tests that assert emitted opcodes/IR and diagnostics span accuracy.

## Non-Goals

- No data allocation or initialization syntax for `var` (no `{ ... }` initializers)
- No explicit `record/struct { ... }` syntax
- No padding/alignment rules (packed only)
- No multi-dimensional symbolic aggregates in v1 (only scalar fields and fixed-size 1D array fields)

## Example (MMIO overlay)

```k65
var VIA[
  .orb :byte
  .ora :byte
  .ddrb:byte
  .ddra:byte
] = 0x6000

var VIA2:byte[
  .orb
  .ora
  .ddrb
  .ddra
] = 0x6010

VIA.ddra = 0xFF
VIA.ora  = 0x55
VIA2.orb = 0xAA
```
