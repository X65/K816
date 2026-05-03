# Split addressing-mode (prefix) from data-length (suffix); remove `:abs` and complete the prefix family

## Context

The fixture [tests/golden/fixtures/syntax-all-addrmodes/input.k65](tests/golden/fixtures/syntax-all-addrmodes/input.k65) exposes a grammar conflation: K65 uses the suffix slot (`:byte`, `:word`, `:far`, `:abs`) for two unrelated concerns — *element size / register-width validation* and *operand address-encoding*. The collision shows up as four concrete asymmetries:

1. `:far` is overloaded: on a `var` it declares 3-byte storage; at a use-site `:far` *could* read as either "force AbsoluteLong addressing" or "treat result as 24-bit data".
2. `:abs` exists as a suffix but `abs` does not exist as an operand prefix; conversely `far` exists as a prefix but is dragged into the data-width axis as a suffix.
3. There is no way to force direct-page addressing (no `:dp` suffix or `dp` prefix); `:byte` is misread as "force zero-page" but it is a data-width concept.
4. The two concerns track different things: *register width / data length* (a property of the value) vs. *address encoding length* (a property of the operand expression).

**Resolution direction:** addressing-mode selection is **prefix-only** (`dp`, `abs`, `far`) and the suffix slot is **data-length-only** (`:byte`, `:word`, `:far`). The two axes become orthogonal and cleanly compose. Worked examples:

- `lda far x0:far` → prefix `far` forces AbsoluteLong on the address; suffix `:far` says the value at that address is 24-bit. Sema rejects it because no register holds 3 bytes. The error comes from the *data* side, not the addressing side.
- `lda far x0:byte` → AbsoluteLong load of a 1-byte value. Compiles in `@a8`, errors in `@a16` because the byte width disagrees with the 16-bit accumulator.

## Recommended design

### Two orthogonal axes

| Axis | Spelling | Slot | Meaning |
|---|---|---|---|
| **Address-encoding** | `dp`, `abs`, `far` | **prefix** before the operand expression | Force DirectPage / Absolute (16-bit) / AbsoluteLong (24-bit) encoding for this one operand. |
| **Data-length / register-width** | `:byte`, `:word`, `:far` | **suffix** on the operand expression (or on a `var` declaration) | Element size on declarations; typed view at use-site. Drives register-width validation against current `@a8`/`@a16`/`@i8`/`@i16` mode. `:far` always rejects direct register load (no register holds 3 bytes). |

The two are independent and compose freely. Both operand spellings remain optional; the existing auto-shrink default (page-0 → DirectPage, in-bank → Absolute, cross-bank → AbsoluteLong) continues when no prefix is given.

### Removals

- **`:abs` suffix is removed** — both as a `var`-declaration suffix and as an expression suffix. The use-site auto-shrink behavior (page-0 default to DirectPage when an opcode supports it) stays.
- The `force_far: bool` field on `Operand::Value` is replaced by an explicit override enum.

### Additions

- **`dp`, `abs`, `far` operand prefixes** parsed alongside the existing `far` prefix. `far` stays a real keyword token. `dp` and `abs` are recognized as **contextual identifiers in the operand-prefix slot only** so they do not reserve `abs` globally (the evaluator already treats `abs` as a function name).
- **`dp`, `abs`, `far` declaration prefixes on `var`** — `var abs dpa = $0012` declares `dpa` as a memory label whose plain references default to Absolute (16-bit) encoding. Variables are addressable labels, so the same prefix family applies. The default is a per-var fallback only — operand-level prefixes still override it. `dp` and `abs` use the same contextual-identifier lookahead as the operand position so `var abs = $12` (var literally named `abs`) still parses as a non-prefixed declaration.
- **`:far` use-site suffix** is now a first-class typed view (today it parses but is hard-rejected at lowering through a special-case path). It joins `:byte`/`:word` in the standard data-length validation path — no separate diagnostic.

### Worked combinations

| Source | Today | After |
|---|---|---|
| `lda far x0` | AbsoluteLong | unchanged |
| `lda x0:abs` | force Absolute | parse error (`:abs` removed) |
| `lda abs x0` | parse error | force Absolute |
| `lda dp x0` | parse error | force DirectPage (sema-error if no DP form or value > $FF) |
| `lda far x0:far` | sema error "Cannot directly load/store :far" | sema error "value is 3 bytes; no register can hold it" (different message, same outcome) |
| `lda far x0:byte` | parse OK, then "Cannot directly load/store :far" if x0 is `:far`; otherwise data-width-mismatch path | AbsoluteLong load + 1-byte data; OK in `@a8`, mismatch in `@a16` |
| `var p:far = $300000; lda p` | sema error | sema error (same — 3-byte slot can't go into a register) |
| `var dp_full:abs = $12; lda dp_full` | force Absolute via declaration | rewrite declaration as `var abs dp_full = $12`; `lda dp_full` then defaults to Absolute via the var prefix |
| `var table:word:abs = $2000` | typed-view + force Absolute | rewrite as `var abs table:word = $2000` (or drop the prefix — `$2000` already requires Absolute) |

### Behavior diff summary

- **Breaking**: `:abs` suffix removed everywhere. Existing source/fixtures using `var X:abs = ...` migrate to `var abs X = ...`. Use-site `lda X:abs` migrates to `lda abs X` (or drop it if a per-var default has been declared).
- **New**: `lda x:far` for non-`:far` `x` no longer uses a special diagnostic path; it routes through standard data-width validation. For LDA family this still errors (3-byte data, no register), with a clearer message.
- **New**: `lda x0:far` and `lda far x0:byte` style combinations are now expressible.
- **New**: `dp` and `far` declaration prefixes also work (`var dp counter = $80`, `var far handler = $030000`) and are useful when the natural auto-pick would not match the desired encoding.

## Files to modify

Critical files (with the role each plays):

- [crates/core/src/lexer.rs](crates/core/src/lexer.rs) — no new keyword tokens; `dp` and `abs` parse as `Ident` and are recognized contextually in the operand-prefix parser. `far` keyword unchanged.
- [crates/core/src/ast/mod.rs](crates/core/src/ast/mod.rs)
  - Remove `AddressHint` enum and remove `Expr::AddressHint`.
  - Replace `addr_hint` field on `VarDecl` with `addr_mode_default: Option<ForceAddrMode>`. Set by the var-declaration prefix.
  - Replace `Operand::Value.force_far: bool` with `addr_mode_override: Option<ForceAddrMode>` where `ForceAddrMode = { DirectPage, Absolute, AbsoluteLong }`.
  - `DataWidth` keeps all three variants `Byte`, `Word`, `Far` (unchanged).
- [crates/core/src/parser/data.rs](crates/core/src/parser/data.rs)
  - Delete `address_hint_parser` and remove its call from the suffix chain.
  - `data_width_parser` keeps `:byte`, `:word`, `TokenKind::Far` arms.
- [crates/core/src/parser/data/vars.rs](crates/core/src/parser/data/vars.rs) — drop the suffix-side `addr_hint` collection. Add an optional addressing-mode prefix (`dp`/`abs`/`far`) consumed between `var` and the variable name; the lookahead requires an Ident immediately after, so `var abs = $12` (var literally named `abs`) still parses as a non-prefixed declaration.
- [crates/core/src/parser/expr.rs](crates/core/src/parser/expr.rs) — drop `ExprSuffix::AddressHint` and the "address-hint must come last" rule. Suffix chain now collects only `TypedView`.
- [crates/core/src/parser/stmt.rs](crates/core/src/parser/stmt.rs) — replace the single `just(TokenKind::Far).or_not()` prefix in `direct_operand` with a parser that accepts `dp` (Ident match), `abs` (Ident match), or `far` (TokenKind::Far) before the expression, producing the matching `ForceAddrMode`. Apply the same prefix slot to indexed and stack-relative forms where addressing-mode selection is meaningful (consult [crates/core/src/parser/operands.rs](crates/core/src/parser/operands.rs) for which operand families need it — at minimum the Direct-with-optional-index family).
- [crates/core/src/parser/operands.rs](crates/core/src/parser/operands.rs) — propagate `addr_mode_override` through helper constructors. Today's `force_far` lives on the Direct-family operand only; the new override applies to the same family.
- [crates/core/src/sema/model.rs](crates/core/src/sema/model.rs) — replace `addr_hint` on `VarMeta` with `addr_mode_default: Option<ForceAddrMode>`. `data_width` keeps all three variants.
- [crates/core/src/sema/vars.rs](crates/core/src/sema/vars.rs) — copy `addr_mode_default` from `VarDecl` to `VarMeta`. Element size still derives from `data_width` (`:far` → 3 bytes).
- [crates/core/src/lower.rs](crates/core/src/lower.rs)
  - Replace `address_size_hint_for_expr(expr, sema, force_far: bool)` with a function that consults the operand override first; if absent, falls back to the var's `addr_mode_default` resolved through the expression's base ident.
  - Keep `expr_data_width` and `data_width_to_reg_width`. Refine the diagnostic at lines 3415–3434:
    - For `:byte`/`:word` mismatching the current mode, keep the existing register-width-mismatch message.
    - For `:far` (data is 3 bytes), produce: *"`<name>` is a 3-byte value; no register can hold it directly. Use partial views (`(<name>+0):byte`, `(<name>+1):word`) or indirect-long (`[<name>]`)."*
- [crates/core/src/hir/mod.rs](crates/core/src/hir/mod.rs) — `AddressSizeHint` gains `ForceDirectPage`.
- [crates/core/src/emit_object.rs](crates/core/src/emit_object.rs) — `to_isa_address_size_hint` bridge gains the new variant.
- [crates/isa65816/src/lib.rs](crates/isa65816/src/lib.rs) — addressing-mode selector honors `ForceDirectPage`. Emit a clear encoding error when the instruction has no DirectPage form, or when the value is not in `$00..=$FF` (and after applying the current `D` (direct page) register offset, where applicable — note today `D` is treated as 0 by the encoder; preserve that simplification unless we already track it).
- [crates/fmt/src/print_ir.rs](crates/fmt/src/print_ir.rs), [crates/lsp/src/hover.rs](crates/lsp/src/hover.rs) — pretty-print the new prefixes; drop `:abs` rendering. Hover for `:far` symbol unchanged in semantics; update label wording.
- [docs/syntax-reference.md](docs/syntax-reference.md) — restructure the *Typed Width* and *65816 Addressing Modes in K65* sections around the two-axis split. Replace examples that use `:abs` with `abs` prefix forms. Document `dp` and the new use-site `:far`. Add a worked-combinations table.

## Preserved behavior: automatic far-calling for `far func` / `far naked`

The `far` keyword has three independent roles in K65 today, and only one of them is touched by this redesign:

1. **Top-level declaration prefix** — `far func name { ... }`, `far naked name { ... }`, and `far` blocks in data segments. **Not changed.** A `far func` continues to lower its body terminator to `RTL`, and call sites continue to auto-promote: a bare `name()` / `call name` / `goto name` to a `far func` still emits `JSL` / `JML` without requiring an explicit `far` at the call site. Cross-unit `call far lib_init` syntax also stays.
2. **Address-encoding operand prefix** — `lda far x0`, `jmp far target`. **Subsumed by the new prefix family** (`dp`, `abs`, `far`). The prefix slot is shared but each prefix maps to its own `ForceAddrMode` variant; `far` here continues to mean AbsoluteLong encoding for one operand.
3. **Data emission keyword** — `far value1 value2 ...` inside data segments emits 24-bit little-endian values. **Not changed.**

Implementation note: roles (1) and (3) live in different parsers from the operand prefix in `direct_operand`. Touching `direct_operand` to add `dp`/`abs` does not affect the function/data parsers. The auto-far-call promotion lives in [crates/core/src/lower.rs](crates/core/src/lower.rs) (call-site mnemonic selection consults `FunctionMeta` for the `far` flag) and in [crates/core/src/sema/](crates/core/src/sema/) — neither code path reads the operand `addr_mode_override`, so they remain intact. Add regression coverage:

- `regression-far-func-autocall/` — calling a `far func` from a near caller still emits `JSL` without an explicit prefix.
- `regression-far-naked-autocall/` — same for `far naked`.
- `regression-far-data-block/` — `far` data segments still emit 24-bit values.

## Migration checklist (existing source)

Search the workspace for current `:abs` usage and rewrite each:

- `tests/golden/fixtures/emit-addr-direct/` — primary `:abs` fixture per [AGENTS.md](AGENTS.md) line 161; rewrite inputs to use the `abs` prefix; re-bless.
- `examples/` and `tests/golden/fixtures/**/input.k65`, `input.*.k65` — grep for `:abs` and `:abs:` and rewrite.
- [docs/syntax-reference.md](docs/syntax-reference.md) inline examples.
- VS Code extension instruction docs at [editors/vscode-k816/resources/instructions-description.json](editors/vscode-k816/resources/instructions-description.json) if any examples show `:abs`.

## Verification

1. **Update the user-visible fixture.** Edit [tests/golden/fixtures/syntax-all-addrmodes/input.k65](tests/golden/fixtures/syntax-all-addrmodes/input.k65) to add a 3.5.17 Direct example using the `dp` prefix (`lda dp x0`), and to demonstrate the new combinations (`lda far x0:byte`). Bless via `cargo test -p k816-golden-tests --features golden-bless syntax_all_addrmodes`.
2. **New positive fixtures**:
   - `prefix-dp/` — `lda dp x` succeeds when x is in page 0.
   - `prefix-abs/` — `lda abs x0` byte-equivalent to today's `lda x0:abs`.
   - `prefix-far/` — `lda far x0` unchanged (regression coverage).
   - `prefix-suffix-combo/` — `lda far x0:byte` succeeds in `@a8`.
3. **New error fixtures**:
   - `prefix-dp-no-dp-form/` — instruction without a DP form rejected.
   - `prefix-dp-out-of-range/` — `lda dp $1234` rejected.
   - `prefix-abs-suffix-far/` — `lda far x0:far` produces the new "3-byte value" diagnostic.
   - `prefix-far-suffix-byte-a16/` — `lda far x0:byte` rejected in `@a16` with the data-width-mismatch message.
   - `parse-abs-suffix-removed/` — `var x:abs = 0` and `lda x:abs` both produce a parse error pointing to the `abs` prefix migration.
4. **Migrated fixtures**: rewrite [tests/golden/fixtures/emit-addr-direct/](tests/golden/fixtures/emit-addr-direct/) and any other `:abs` users; bless and check expected diffs are only the suffix→prefix swap.
5. **Existing `:far` error fixture** [tests/golden/fixtures/syntax-mode-typed-width-errors-far/](tests/golden/fixtures/syntax-mode-typed-width-errors-far/) — update `expected.err` to match the new "3-byte value" message wording.
6. **Full suite + bless**:
   - `cargo test`
   - `cargo clippy --all-targets --all-features`
   - `cargo test -p k816-golden-tests --features golden-bless` then re-run `cargo test -p k816-golden-tests` for reproducibility.
7. **LSP smoke test** — `tests/lsp_cli.rs`; verify hover renders the new prefix forms and that `:abs`-related metadata is gone.
8. **Doc round-trip** — re-render syntax-reference examples to confirm the new model is internally consistent (no leftover `:abs`).
