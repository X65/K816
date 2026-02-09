# O65 Object + Linker Migration Plan

## Summary

Migrate K816 from direct final-binary emission to a traditional 2-stage toolchain:

1. `compile` -> relocatable `.o65` object output
2. `link` -> final binary output from one or more `.o65` objects

This plan reflects current repository state as of latest CLI changes.

## Source of Truth

- `o65` format specification:
  - <https://raw.githubusercontent.com/fachat/o65/refs/heads/main/fileformat.adoc>
- `ld65` configuration model reference (section 5):
  - <https://cc65.github.io/doc/ld65.html#s5>

## Latest Findings (Current State)

### Implemented already

- CLI has explicit subcommands:
  - `k816 compile <input> ...`
  - `k816 link <object> ...`
- Running `k816 <input>` (no subcommand) now performs a two-step fallback flow (compile then link).
- CLI input source extension is enforced as `.k65`.
- Help usage is now command-first:
  - `k816 [COMMAND] [INPUT]`

### Important caveat

- Current `compile`/`link` split is **interface-level only**.
- `compile` currently writes a custom object-bundle directory (`manifest.txt`, `listing.lst`, bank `.bin` chunks), not real `o65`.
- `link` currently reads that custom bundle and writes final output files.
- Core emission still resolves and emits flat bytes per bank (`crates/core/src/emit.rs`); it does not emit relocations/object records yet.
- Current golden harness compares compile-stage bank bytes; it must be moved to compare **linked** output produced by compile+link pass.

## Target Architecture

## 1. Compiler output boundary

- Replace compile output contract from:
  - `banks: IndexMap<String, Vec<u8>>`
- To:
  - relocatable object model suitable for `.o65`
  - section data + symbol table + relocation table + metadata

Compiler responsibilities:

- Parse/lower/sema
- Encode instructions/data into section-relative bytes
- Emit symbol definitions/references
- Emit relocation records (no final absolute placement)

## 2. Linker responsibilities

- Load one or more `.o65` objects
- Resolve global/local symbols
- Apply linker script placement (memory regions, banks, entrypoint, fixed addresses)
- Apply relocations after section placement
- Emit final binary image(s) and optional map/listing artifacts

## 3. Configuration model

- Use native Rust linker script in **RON** format (`.k816ld.ron`) for placement policy.
- Design is inspired by `ld65` config model section 5:
  - `MEMORY` -> memory areas with start/size/type/fill/file
  - `SEGMENTS` -> section placement, load/run memories, align/start/offset
  - `FILES`/`FORMATS` -> output mapping and format behavior
  - `SYMBOLS` -> linker-defined symbols
  - `FEATURES` -> optional linker features enabled per output
- Do not depend on vendor `.nut` scripting for Rust implementation.
- Preserve intent parity with vendor where practical:
  - bank ranges
  - placement policies
  - fixed-address constraints
  - entrypoint rules

Reference used: `ld65` docs section 5 (<https://cc65.github.io/doc/ld65.html#s5>).

## 4. `ld65`-Inspired RON Schema (Initial)

Proposed top-level config shape:

```ron
LinkerConfig(
  format: "o65-link",
  target: "c64",
  memory: [
    MemoryArea(
      name: "RAM",
      start: 0x0801,
      size: 0x7FFF,
      kind: ReadWrite,
      fill: Some(0x00),
      out_file: Some("main.bin"),
    ),
    MemoryArea(
      name: "ROM0",
      start: 0xF000,
      size: 0x1000,
      kind: ReadOnly,
      fill: Some(0xFF),
      out_file: Some("bank0.bin"),
    ),
  ],
  segments: [
    SegmentRule(
      name: "CODE",
      load: "RAM",
      run: None,
      align: Some(1),
      start: None,
      offset: None,
      optional: false,
      bank: Some("main"),
    ),
    SegmentRule(
      name: "FIXED_FN",
      load: "ROM0",
      run: None,
      align: Some(1),
      start: Some(0xF400),
      offset: None,
      optional: false,
      bank: Some("bank0"),
    ),
  ],
  symbols: [
    LinkSymbol(name: "__STACKTOP", value: Absolute(0xCFFF)),
    LinkSymbol(name: "__ENTRY", value: Import("main")),
  ],
  outputs: [
    OutputSpec(name: "main", kind: RawBinary, default_file: "a.out"),
  ],
  entry: Some("__ENTRY"),
)
```

### Mapping Notes

- Current K816 logical bank names map to config `bank` field in `SegmentRule`.
- Function/data fixed address requests map to `start` constraints in segment placement.
- Compile stage emits relocatable references; link stage resolves addresses using segment placement decisions.
- `run` is reserved for future support (load/run split), but modelled now for compatibility with `ld65` style.

### Explicit non-goals for phase 1

- Full `ld65` expression language parity.
- Every `ld65` feature switch and output format.
- Script-like imperative behavior.

## O65 Scope (Phase 1)

Implement a minimal relocatable subset of `o65` required for current language features:

- object header required for relocatable mode
- text/data (and bss if needed by current lowering)
- symbol table entries for defined/undefined symbols
- relocation records for:
  - absolute 16-bit references
  - absolute long 24-bit references
  - relative branch references

Defer full optional extension coverage until phase 2.

## Migration Phases

## Phase A: Stabilize CLI contract (mostly done)

- Keep:
  - `compile` subcommand
  - `link` subcommand
  - fallback `k816 <input>` compile+link
- Change output semantics:
  - `compile` output must become `.o65` file(s), not custom bundle dir

## Phase B: Introduce object crate and types

- Add crate: `crates/o65`
- Define:
  - `O65Object`
  - `Section`
  - `Symbol`
  - `Relocation`
  - serializer/deserializer
- Add roundtrip tests (write->read->compare) for deterministic objects.

## Phase C: Refactor core emitter to relocatable backend

- Replace current final-byte patching backend in `crates/core/src/emit.rs`.
- New backend emits:
  - section-relative bytes
  - unresolved symbol references
  - relocation entries
- Keep diagnostics and listing behavior deterministic.

## Phase D: Build linker crate

- Add crate: `crates/link`
- Implement:
  - object ingestion (`.o65`)
  - symbol resolution across objects
  - placement engine from `.k816ld.ron`
  - relocation application
  - final image writer(s)
  - config validation with actionable diagnostics:
    - unknown memory/segment references
    - overlapping fixed `start` constraints
    - out-of-range load/run mappings
    - unsupported phase-1 fields

## Phase E: Wire CLI to real pipeline

- `k816 compile input.k65 -o out.o65`
- `k816 link out.o65 -T target.k816ld.ron -o out.bin`
- fallback `k816 input.k65` should internally perform equivalent two-step flow using temporary `.o65` representation.

## Phase F: Tests and fixtures

- Add compile-only fixture class:
  - source -> expected `.o65` structural assertions
- Add link-only fixture class:
  - object(s) + script -> expected `.bin`
- Add end-to-end fixture class:
  - source + script -> expected `.bin`/`.lst`
- Migrate golden harness to run:
  - `compile` (source -> `.o65`)
  - `link` (`.o65` + stub linker config -> final `.bin`/`.lst`)
  - compare against expected linked artifacts
- Keep current tests passing during transition; migrate old golden binary fixtures gradually.

### Golden Harness Migration (Required)

- Harness source of truth becomes **link output**, not compiler bank buffer output.
- Introduce deterministic stub config for golden fixtures:
  - e.g. `tests/golden/link.stub.k816ld.ron`
  - fixed memory layout
  - deterministic fill bytes
  - deterministic section ordering/placement policy
  - explicit entry symbol policy
- Golden fixture execution path:
  1. compile fixture source to temporary `.o65`
  2. link temporary `.o65` with stub config
  3. assert final `.bin` byte-for-byte
  4. assert `.lst`/map outputs where relevant
- Multi-bank fixtures should compare each linked output file deterministically (stable naming from linker config).
- Remove direct dependence of golden assertions on `CompileOutput.banks` once linker pipeline is active.

## Technical Decisions (Locked)

- Source extension remains `.k65`.
- CLI remains command-first in usage/help.
- Separate compile and link subcommands remain first-class.
- No-subcommand mode remains compile+link fallback for user convenience.

## Open Work Items

- Implement real `.o65` writer/reader.
- Replace custom object-bundle format in CLI.
- Introduce RON linker config parser and validator (`.k816ld.ron`) inspired by `ld65` section 5.
- Move fixed-address handling from compile-time cursor behavior to link-time placement constraints.
- Rework cross-bank reference checks to occur at link-time using final section placement.

## Acceptance Criteria

- `compile` produces valid relocatable `.o65` object(s).
- `link` consumes `.o65` and emits deterministic final binary output.
- Symbol resolution/relocations are applied at link stage, not compile stage.
- Golden tests execute compile+link with stub `.k816ld.ron` and compare linked binaries deterministically.
- Existing CLI ergonomics remain:
  - `k816 compile ...`
  - `k816 link ...`
  - `k816 input.k65` fallback.
