# Single Output Linker Migration Plan

## Goal

Enforce explicit single-output linking:

1. Remove legacy `outputs: []` fallback behavior.
2. Remove `memory.out_file` support.
3. Require exactly one linker output spec in config (`output`).
4. Require output filename from config (`output.file`) or CLI `-o`.
5. Keep listing output optional and CLI-controlled only.

## Scope

- In scope:
  - `crates/link`: config schema + output rendering changes.
  - `src/main.rs`: CLI output/listing path handling changes.
  - `tests/cli.rs`, `tests/golden/src/harness.rs`, linker unit tests.
  - in-repo `.k816ld.ron` config migrations.
- Out of scope:
  - multiple output artifacts per link invocation.
  - config-driven listing file paths.

## Implementation

1. **Linker schema**
   - Change `LinkerConfig.outputs: Vec<OutputSpec>` to `LinkerConfig.output: OutputSpec`.
   - Remove `MemoryArea.out_file`.
   - Change `OutputSpec.default_file` to `OutputSpec.file`.
   - Keep `OutputKind::{RawBinary, Xex}`.

2. **Linker output model**
   - Change `LinkOutput` from map-of-binaries to single artifact:
     - `bytes: Vec<u8>`
     - `kind: OutputKind`
     - `listing: String`
   - Remove `render_legacy_raw_outputs`.
   - For `RawBinary`:
     - emit compact bytes for one used memory area.
     - if more than one memory area has used bytes, return error.
   - For `Xex`:
     - keep existing block serializer.

3. **CLI behavior**
   - `link -o` means exact output file path.
   - Resolve final output path:
     - `-o` wins.
     - otherwise use `config.output.file` (relative to config file directory).
     - otherwise error.
   - Add optional listing flag in both forms:
     - `--listing` (auto `<output_stem>.lst`)
     - `--listing <PATH>`
   - Do not emit listing unless flag is present.
   - Keep implicit `k816 input.k65` mode by passing an internal output path.

4. **Config migration**
   - Update:
     - `examples/hello_uart.k816ld.ron`
     - `tests/golden/link.stub.k816ld.ron`
     - `tests/golden/fixtures/link-no-fit/link.k816ld.ron`
   - Replace `outputs: []` + `out_file` with single `output`.

5. **Tests**
   - Update linker unit tests for single-output `LinkOutput`.
   - Add/adjust CLI tests for:
     - required output file semantics,
     - config path resolution,
     - listing optional behavior.
   - Update golden harness to compare `LinkOutput.bytes`.

## Acceptance Criteria

1. No legacy fallback path remains in linker.
2. No multi-output config path exists.
3. Link fails when neither config output file nor `-o` is provided.
4. Listing file is produced only when requested by CLI.
5. All relevant tests pass.
