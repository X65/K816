# o65 + XEX Execution Plan

## Goal

1. Compile stage emits real o65-framed object files (instead of the current custom top-level magic format).
2. Link stage can emit Atari 8-bit `.xex` output from one or more object files.
3. Keep current raw-binary linker behavior working for existing tests/configs.

## Scope

- In scope:
  - `crates/o65`: migrate on-disk object envelope to o65 header + payload.
  - `crates/link`: add `OutputKind::Xex`, render XEX blocks from used memory runs.
  - `tests`: add linker and CLI tests for XEX output and keep existing tests green.
- Out of scope:
  - RUNAD/INITAD vector emission.
  - Full external-toolchain parity for every o65 feature.

## Implementation Steps

1. **o65 object envelope**
   - Replace custom file magic with standard o65 magic prefix (`0x01 0x00 'o' '6' '5'`).
   - Add fixed o65 header fields and options terminator.
   - Store the current K816 object model as payload in the text segment.
   - Keep current object model validation (`sections`, `symbols`, `relocations`) unchanged.

2. **Link output pipeline**
   - Extend `OutputKind` with `Xex`.
   - Add output rendering helpers:
     - Legacy memory-based raw `.bin` fallback (current behavior).
     - Config-driven outputs list when `outputs` is non-empty.
   - Add XEX serializer:
     - First block starts with `0xFFFF`.
     - One block per contiguous used run.
     - Address fields are 16-bit little-endian inclusive ranges.
     - Reject addresses outside 16-bit range.

3. **Tests**
   - Linker unit tests:
     - XEX sparse blocks serialization.
     - XEX address range validation.
   - CLI integration test:
     - `compile` + `link -T` with `OutputKind::Xex` produces expected `.xex` file.
   - Keep existing link and CLI tests unchanged where possible.

4. **Verification**
   - Run:
     - `cargo test -p k816-o65`
     - `cargo test -p k816-link`
     - `cargo test --test cli`
   - If failures appear, adjust with minimal, behavior-preserving fixes.

## Acceptance Criteria

1. `k816 compile input.k65 -o out.o65` writes o65-header-framed files.
2. `k816 link obj.o65 -T cfg.k816ld.ron` supports `OutputKind::Xex`.
3. XEX output is block-based from used ranges and begins with `0xFFFF`.
4. Existing raw-binary workflows remain passing.
