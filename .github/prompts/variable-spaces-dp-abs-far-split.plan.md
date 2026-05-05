# Plan: Decouple DP / ABS / FAR variable address spaces

## Context

The fixture [tests/golden/fixtures/link-dp/input.k65](../../tests/golden/fixtures/link-dp/input.k65) declares two `var dp NAME:far` variables and emits `lda [name],y`. With the linker config placing the only segment at `$0200`, the build fails:

```
Error: absolute relocation does not fit in 8 bits
   ╭─[ tests/golden/fixtures/link-dp/input.k65:6:10 ]
 6 │     lda [kstrncpy_src],y
   │          ──────┬─────  
```

Root cause traced in [crates/core/src/sema/vars.rs:52-84](../../crates/core/src/sema/vars.rs#L52) — `eval_var_placement` writes every un-addressed `var` into the *current segment* via a per-segment cursor (DP-storage vars included). The linker resolves the symbol to `segment_start + offset = $0200`, then the encoder's 1-byte DP relocation can't hold the value.

This is 6502-era thinking: zero page was a fixed memory region shared with everything else. On 65816 the **direct page is runtime-relocatable** (any 16-bit base via `tcd`/`pld`) and lives in an addressing space disjoint from program-bank and data-bank. DP, ABS, and FAR are three orthogonal address spaces that the compiler must track independently.

Three variable storage classes (parsed today as `addr_mode_default: Option<ForceAddrMode>` on `VarDecl`, [crates/core/src/ast/mod.rs:124-136](../../crates/core/src/ast/mod.rs#L124); fully orthogonal to the `:byte`/`:word`/`:far` *data width* — `var dp foo:far` is a 3-byte FAR pointer stored in a DP slot):

| class | syntax              | address width | auto-allocator                            | initializer required? |
|-------|---------------------|--------------:|-------------------------------------------|-----------------------|
| DP    | `var dp NAME[:w]`   | 1 byte        | global 256-byte logical pool (link-time)  | no                    |
| ABS   | `var NAME[:w]`      | 2 bytes       | per-segment cursor (today)                | no                    |
| FAR   | `var far NAME[:w]`  | 3 bytes       | **none — must have explicit address**     | **yes**               |

**Critical architectural fact:** there is no `__dp__` section, no `MemoryKind::DirectPage`, no DP segment rule. The runtime — not the linker — decides where DP physically lives. The linker just tracks symbol-name → 8-bit DP offset (0..255) so that relocations resolve correctly. Output binaries contain zero bytes for DP vars; the user is responsible for `tcd` at runtime and for having backing RAM under whatever D points at.

## Approach

### 1. AST/parser

No changes. The existing `addr_mode_default: Option<ForceAddrMode>` and `data_width: Option<DataWidth>` already cover the syntax surface.

### 2. Sema — split placement by storage class

Files: [crates/core/src/sema/model.rs](../../crates/core/src/sema/model.rs), [crates/core/src/sema/vars.rs](../../crates/core/src/sema/vars.rs), [crates/core/src/sema/analysis.rs](../../crates/core/src/sema/analysis.rs), [crates/core/src/sema/tests.rs](../../crates/core/src/sema/tests.rs).

Replace the current `VarPlacement` (model.rs:99-114):

```rust
pub enum VarPlacement {
    Fixed { address: u32 },                              // any space, explicit init
    AllocatedAbs { segment: String, offset: u32 },       // existing per-segment cursor
    AllocatedDp,                                         // request a DP slot at link time
}
```

Notes:

- `AllocatedDp` carries no offset — the linker picks the final 8-bit address. `VarMeta.size` already records how many DP bytes the var needs.
- `Fixed` covers all explicit-address paths regardless of storage class. The linker still needs to know the storage class to pin DP slots; that comes from `VarMeta.addr_mode_default`, which we already carry.
- `compile_time_address()` returns `Some(address)` only for `Fixed`; both `AllocatedAbs` and `AllocatedDp` are linker-resolved.

In `eval_var_placement` (vars.rs:52-84):

1. Branch on `var.addr_mode_default`:
   - `Some(ForceAddrMode::AbsoluteLong)` (FAR) without initializer → emit a rich diagnostic and return `None` (rejection rule below).
   - `Some(ForceAddrMode::DirectPage)` without initializer → return `VarPlacement::AllocatedDp`. Maintain a per-unit DP usage counter for diagnostic purposes; reject if unit-local DP usage > 256.
   - Else → keep the existing per-segment cursor path returning `VarPlacement::AllocatedAbs`.
2. With initializer, return `Fixed { address }` regardless of class. For `Fixed` DP vars, validate `address ≤ 0xFF` and emit a diagnostic if not (DP slot must fit in a byte). For `Fixed` FAR vars, the address may span the 24-bit space; no extra check.

Keep ABS allocation behaviour unchanged so existing fixtures don't shift their layouts unnecessarily.

### 3. Object format — new symbol kinds for DP

Files: [crates/o65/src/model.rs](../../crates/o65/src/model.rs), [crates/o65/src/encode.rs](../../crates/o65/src/encode.rs), [crates/o65/src/decode.rs](../../crates/o65/src/decode.rs).

Extend `SymbolDefinition`:

```rust
pub enum SymbolDefinition {
    Section { section: String, offset: u32, source: Option<SourceLocation> },
    Absolute { address: u32, source: Option<SourceLocation> },
    DirectPageFixed { offset: u8, source: Option<SourceLocation> },  // var dp foo = $42
    DirectPageAlloc { size: u8, source: Option<SourceLocation> },    // var dp foo (auto)
}
```

Two new variants make storage-class intent first-class in the object so the linker can pin slots and auto-allocate without sniffing addresses. Wire encode/decode for both.

### 4. Lowering — emit DP symbols instead of section symbols

File: [crates/core/src/lower.rs](../../crates/core/src/lower.rs) (`emit_var_absolute_symbols` at lines 2978-3017, plus `resolve_operand_ident` around 6347-6354).

Replace the single `Allocated` branch with a match on the new `VarPlacement` variants:

- `Fixed { address }` + DP class → emit a new `Op::DefineDpFixedSymbol { name, offset: address as u8 }`.
- `Fixed { address }` + non-DP → existing `Op::DefineAbsoluteSymbol`.
- `AllocatedAbs { segment, offset }` → existing path: `SelectSegment` + `DefineSectionSymbol` + `EmitBytes(zero_fill)`.
- `AllocatedDp` → new `Op::DefineDpAllocSymbol { name, size: var.size as u8 }`. **No `EmitBytes`** — DP allocations write nothing.

Add the matching HIR `Op` variants and the `emit_object` translation that produces `SymbolDefinition::DirectPageAlloc` / `DirectPageFixed` records.

For symbolic subscript fields (`var dp gpio:[ddr:byte, data:byte]`), keep the existing field-symbol expansion: emit a `DirectPageAlloc` for `gpio` (size = total subscript size) and field-alias symbols anchored to the parent so the linker resolves `gpio.ddr = dp_base + 0`, `gpio.data = dp_base + 1`. Concrete encoding to be picked during TDD step 6 below — prefer the option that requires no two-pass linking.

### 5. Linker — global DP allocator

Files: [crates/link/src/layout.rs](../../crates/link/src/layout.rs), new module `crates/link/src/layout/dp.rs`.

Add a pre-pass before relocation resolution:

1. **Pin pass.** Scan every object's symbol table; for each `DirectPageFixed { offset }` mark the corresponding bits in a 256-bit usage bitmap. Non-DP `Absolute` symbols at addresses 0..255 do not occupy DP slots — they live in data-bank space, which is logically distinct.

2. **Allocate pass.** Iterate `DirectPageAlloc` requests in **link-input order**. For each, find first-fit free run of `size` bytes in the bitmap; mark used; record the assigned 8-bit offset on a `dp_assignments: HashMap<String, u8>` map keyed by symbol name.

3. **Resolution.** When the existing `resolve_one_relocation` path looks up a symbol whose definition is `DirectPageFixed { offset }`, return `offset as u32` as the resolved value. For `DirectPageAlloc`, return `dp_assignments[name] as u32`. Existing `write_value(width=1)` then succeeds.

4. **Diagnostics.**
   - DP overflow: emit a rich diagnostic listing total bytes requested, available bytes, and the failing var's source span. Help: "consolidate DP usage by giving non-hot vars `:abs` storage class, or pin selected DP vars to specific offsets to free contiguous runs."
   - DP fixed-offset out-of-range (already in sema): "`var dp NAME = $X` requires `$X ≤ $FF` because DP slots are 1-byte offsets within the direct page."
   - DP fixed collision is *allowed*. Two `var dp foo = $42` and `var dp bar = $42` succeed; the bitmap just sets bit $42 twice. The auto-allocator skips $42.

User-pinned overrides: document that `var dp foo = $42` is the way to pin a specific slot; auto-allocated vars fill the rest in input order.

### 6. Linker config

No changes needed. DP is invisible to RON — no new `MemoryKind`, no segment rule, no `default_stub_config` edits.

### 7. Encoder / ISA

No changes. The encoder already produces width=1 relocations for DP-mode opcodes when `addr_mode_default == DirectPage` (or when use-site `dp` override forces it). The post-fix linker just resolves those relocations to actual u8 values.

### 8. Listing output

File: [crates/link/src/listing.rs](../../crates/link/src/listing.rs).

Add a `[DP]` block at the top of the listing showing each DP symbol and its assigned offset (`$00..$FF`). Format consistent with the existing `[default]` segment block. This makes `expected.lst` regression-checkable against DP layout changes.

### 9. LSP

Files: [crates/lsp/src/hover.rs](../../crates/lsp/src/hover.rs), [crates/lsp/src/lib.rs](../../crates/lsp/src/lib.rs).

Hover for DP vars currently shows `<linker-allocated>` because `compile_time_address()` returns `None`. Use the live link layout (`last_link_layout`) — same path used by `k816/queryMemoryMap` — to surface the assigned DP offset (e.g. `dp $42`). Falls back to `<DP-pool>` when no link state is available.

Update the existing LSP test in [crates/lsp/src/tests.rs](../../crates/lsp/src/tests.rs) (around line 2216 / 2270) — the "absolute relocation does not fit in 8 bits" expectation will flip to "no diagnostic" once DP placement is fixed.

### 10. Tests (TDD order)

New golden fixtures under [tests/golden/fixtures/](../../tests/golden/fixtures/):

1. **`link-dp/`** — the existing failing fixture. Bless after step 5 (sema + lowering + linker land). Asserts `[kstrncpy_src],y` resolves correctly when the only segment starts at `$0200`. The existing `link.ld.ron` needs no change.
2. **`link-dp-mixed/`** — multi-input: two `.k65` files each with `var dp` declarations, asserting input-order placement (file 1's vars get lower DP offsets).
3. **`link-dp-fixed-and-auto/`** — mix of `var dp foo = $10` (pinned) and `var dp bar` (auto). Asserts the auto-allocator skips $10 and that pinning `var dp baz = $10` again is allowed (intentional aliasing).
4. **`link-dp-overflow-err/`** — request >256 bytes of auto-allocated DP. Asserts the rich overflow diagnostic.
5. **`var-far-no-addr-err/`** — `var far foo` without initializer. Asserts the rejection diagnostic with help text directing the user to provide an explicit address.
6. **`link-dp-fixed-out-of-range-err/`** — `var dp foo = $0100`. Asserts the "DP fixed offset must fit in 1 byte" diagnostic.
7. **`syntax-symbolic-subscripts-dp/`** — `var dp gpio:[ddr:byte, data:byte, *32]` to verify field aliases resolve to `dp_base + field_offset`.

TDD order:

1. Write fixture (1) — currently failing → expected to fail with the new diagnostic shape after sema split.
2. Implement sema split + FAR rejection → fixture (5) and (6) pass.
3. Implement lowering + new HIR ops + o65 encode/decode → object emits DP symbols.
4. Implement linker DP allocator (pin + allocate + resolve) → fixture (1) passes.
5. Add fixtures (2), (3) → bless.
6. Add fixture (7) → may require subscript-field DP handling refinements; iterate.
7. Add fixture (4) → may require diagnostic phrasing iteration.
8. Re-run full golden harness; bless any expected.lst diffs in fixtures that mix `var dp` with bare `var` (offset shifts in ABS cursor when DP vars no longer consume segment bytes).

Run after each step:

```
cargo test -p k816-golden-tests
cargo run -p k816-golden-tests --bin bless -- --case fixture:<name>
cargo test -p k816-golden-tests
```

Likely fixtures that will need re-blessing (mix `var dp` with bare `var` in same segment cursor):

- [tests/golden/fixtures/syntax-addrmode-indirect/](../../tests/golden/fixtures/syntax-addrmode-indirect/) (verify; may pass byte-for-byte if no bare `var` in same file)
- [tests/golden/fixtures/syntax-addrmode-indirect-err-ptr/](../../tests/golden/fixtures/syntax-addrmode-indirect-err-ptr/) (error fixture; only checks diagnostics)
- [tests/golden/fixtures/syntax-addrmode-indirect-multi/](../../tests/golden/fixtures/syntax-addrmode-indirect-multi/) (multi-unit DP)

Re-bless any that drift; review each diff by eye per CLAUDE.md.

### 11. Documentation

- [docs/syntax-reference.md](../../docs/syntax-reference.md): new section "Variable storage classes" covering DP / ABS / FAR with the orthogonal data-width axis. Spell out:
  - DP is a 256-byte logical pool; `tcd` controls runtime placement; the linker only tracks 8-bit offsets.
  - ABS lives in the segment chosen at declaration; per-segment cursor.
  - FAR requires an explicit address; the assembler does not auto-allocate banks.
  - `var dp foo = $X` pins a slot; collisions with other pinned DP vars are allowed.
- [docs/linker-ron-format.md](../../docs/linker-ron-format.md): clarify that DP is invisible to the linker config — there is no `MemoryKind::DirectPage`, no DP segment. Mention the link-input ordering rule for auto-allocated DP vars.

## Critical files to be modified

- [crates/core/src/sema/model.rs](../../crates/core/src/sema/model.rs) — `VarPlacement` split, `compile_time_address` adjustment.
- [crates/core/src/sema/vars.rs](../../crates/core/src/sema/vars.rs) — DP cursor, FAR rejection, fixed-DP range check, auto-DP routing.
- [crates/core/src/sema/analysis.rs](../../crates/core/src/sema/analysis.rs) — wire DP cursor through `collect_var` calls.
- [crates/core/src/sema/tests.rs](../../crates/core/src/sema/tests.rs) — new placement helpers + tests.
- [crates/core/src/lower.rs](../../crates/core/src/lower.rs) — `emit_var_absolute_symbols` split, new `Op::DefineDp{Fixed,Alloc}Symbol`, `resolve_operand_ident` arm split.
- [crates/core/src/hir/mod.rs](../../crates/core/src/hir/mod.rs) — new `Op` variants for DP symbol declarations.
- [crates/core/src/emit_object.rs](../../crates/core/src/emit_object.rs) — translate new HIR ops into `SymbolDefinition::DirectPage*`.
- [crates/o65/src/model.rs](../../crates/o65/src/model.rs) — `SymbolDefinition` variants.
- [crates/o65/src/encode.rs](../../crates/o65/src/encode.rs), [crates/o65/src/decode.rs](../../crates/o65/src/decode.rs) — wire encode/decode for new variants.
- [crates/link/src/layout.rs](../../crates/link/src/layout.rs) — DP pre-pass (pin + allocate + assignments map); resolution path.
- [crates/link/src/layout/dp.rs](../../crates/link/src/layout/dp.rs) — new module with `DpAllocator`, `Bitmap256`, first-fit logic.
- [crates/link/src/listing.rs](../../crates/link/src/listing.rs) — `[DP]` block.
- [crates/lsp/src/hover.rs](../../crates/lsp/src/hover.rs) — DP-aware hover address.
- [crates/lsp/src/tests.rs](../../crates/lsp/src/tests.rs) — flip overflow expectations.
- [docs/syntax-reference.md](../../docs/syntax-reference.md), [docs/linker-ron-format.md](../../docs/linker-ron-format.md) — documentation.

## Existing functions/utilities reused

- `Diagnostic::error(...).with_primary_label(...).with_help(...).with_note(...)` builder in [crates/core/src/diag.rs](../../crates/core/src/diag.rs) — match the rich-diagnostic style mandated by CLAUDE.md.
- `FunctionMeta::signature_call_form` in [crates/core/src/sema/model.rs](../../crates/core/src/sema/model.rs) — for any contract-related diagnostic surfaces touched.
- The existing per-segment cursor logic in [crates/core/src/sema/vars.rs](../../crates/core/src/sema/vars.rs) stays in place for ABS allocation.
- The existing relocation-write path in [crates/link/src/layout.rs:1098-1110](../../crates/link/src/layout.rs#L1098) needs no change — once DP symbols resolve to u8 values, `write_value(width=1)` works.
- Anchor/source-location rendering in [crates/link/src/layout/anchors.rs](../../crates/link/src/layout/anchors.rs) for DP overflow diagnostic.

## Verification

End-to-end:

```
cargo build
cargo clippy --all-targets --all-features
cargo test
cargo test -p k816-golden-tests
cargo run -p k816-golden-tests --bin bless -- --case fixture:link-dp
cargo test -p k816-golden-tests link_dp
```

Each new fixture's `expected.err` (for error cases) and `expected.bin` + `expected.lst` (for success cases) is reviewed by eye after blessing.

Manual smoke: examples that use `var dp` (e.g. [examples/hello_uart.k65](../../examples/hello_uart.k65) if applicable) — build with `cargo run` and visually inspect the listing for sensible DP offsets.

Cross-tool: open the project in VS Code with the LSP; hover a `var dp` var and confirm the DP offset shows after a build.
