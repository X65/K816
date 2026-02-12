# Multi-Unit Golden Inputs + Cross-Unit Symbol Linking (Funcs + Vars)

## Summary

Implement multi-source golden fixtures and cross-compilation-unit symbol resolution with these defaults:

1. Apply permissive unresolved-symbol handling to all object-producing flows (`k816 compile`, project build, golden harness).
2. Keep strict early undefined-label diagnostics for single-file shortcut builds and LSP.
3. Use implicit unresolved-symbol linkage (no new `extern/import` syntax).
4. Support `var` exports across units, including symbolic field aliases (for example `regs.ctrl`).
5. Use fixture layout `input.*.k65` (lexicographic order), while keeping existing `input.k65` behavior.

## Public API / Type Changes

1. Add object-compilation APIs in `k816-core` for link-oriented mode (permissive unresolved symbols), without breaking existing strict APIs.
2. Add an object-emission option type (for example `EmitObjectOptions`) with `allow_undefined_symbols`.
3. Extend HIR with an absolute symbol op (for example `Op::DefineAbsoluteSymbol { name, address }`).
4. Extend `k816-o65` symbol definitions to represent both section-relative and absolute definitions (enum form), and bump payload version while keeping decode support for older payload versions.

## Implementation P lan

1. Golden harness input discovery:
Use existing single-source behavior when `input.k65`/`input.k816` exists.
Add multi-source mode for `input.*.k65`/`input.*.k816` files in fixture root, sorted lexicographically.
Error on ambiguous layouts (single-input file plus multi-input pattern together).
2. Golden harness compile/link pipeline:
Compile each source file independently to object in deterministic order using link-oriented object compile mode.
Aggregate warnings in compile order.
Link all produced objects together.
Keep existing config precedence: `fixture/link.ld.ron`, then adjacent `input.ld.ron` only for single-source fixtures, else stub config.
3. Cross-unit unresolved symbol support:
In object emission, allow unresolved label fixups when `allow_undefined_symbols=true`.
Keep current strict behavior when `allow_undefined_symbols=false`.
4. Export `var` symbols:
During lowering, emit absolute-symbol ops for every `var` base name.
For symbolic subscript vars, also emit alias symbols `<var>.<field>` at `base + field.offset`.
5. Emit object symbols:
Convert code/data labels to section-relative symbol definitions.
Convert lowered var exports to absolute symbol definitions.
Keep duplicate-symbol diagnostics across both symbol kinds.
6. Linker symbol resolution:
Resolve section-relative symbols via placements (current behavior).
Resolve absolute symbols directly to fixed addresses.
Preserve undefined-symbol link failures for unresolved imports.
Bypass section-mismatch far-addressing guard for absolute symbols (still enforce relocation width/range).
7. CLI wiring:
Use link-oriented object compile mode in `k816 compile` and project build object compilation.
Keep strict compile path for single-file shortcut build and LSP analysis to retain early span diagnostics.
8. Fixture refactor:
Split `tests/golden/fixtures/unit-bare-exports/input.k65` into:
`input.10-vars.k65`, `input.20-test.k65`, `input.30-main.k65`.
Make cross-unit references explicit in fixture content:
`main` calls `test`; `test` references exported vars/field aliases (`foo`, `regs.ctrl`).
Regenerate `expected.bin` and `expected.lst` to match multi-object link output.

## Test Cases and Scenarios

1. Core unit tests:
Verify lowering emits absolute var symbols and field aliases.
Verify strict vs permissive unresolved-symbol behavior in object emission.
2. O65 tests:
Roundtrip encode/decode for absolute and section-relative symbols.
Backward decode coverage for prior payload versions.
3. Linker tests:
Absolute symbol relocation resolves correctly.
16-bit relocation to absolute symbols does not fail due to segment mismatch.
Undefined symbols still fail at link with clear error.
4. CLI integration tests:
Compile multiple files to `.o65` then link; confirm cross-unit function and var alias references resolve.
Project build with multiple `src/**/*.k65` files resolves cross-unit symbols.
Single-file shortcut still reports undefined labels at compile stage.
5. Golden tests:
Updated `unit-bare-exports` passes with multi-input harness mode.
Add one negative multi-input fixture for unresolved cross-unit symbol link failure.

## Assumptions and Defaults

1. No new language syntax (`extern/import`) in this change.
2. Cross-unit var support covers base names and symbolic field aliases, not full cross-unit symbolic indexed semantics (for example `regs.string[2]` stays out of scope).
3. Multi-source fixture order is filename lexicographic; numeric prefixes are used to lock ordering.
4. Existing fixtures remain valid without changes unless they opt into `input.*.k65`.
