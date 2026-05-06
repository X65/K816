# Compound Repeat Element Accessors

## Summary
Implement compile-time element access for `* N` repeat variables so `COMP[i]` means `COMP + i * COMP:sizeof`, and `COMP[i].field` adds the symbolic field offset. The selected bounds policy is strict: constant indices must be `0 <= i < N`.

## Key Changes
- Extend variable metadata with `repeat_count` on `VarMeta` and `ExternalVarClass`; set it from `alloc_count`, defaulting to `1`. Keep `:sizeof` returning `element_size`, not total allocation size.
- Extend expression syntax for field access after non-ident expressions:
  - Keep existing flattened names for `COMP.field` and `COMP[.field]`.
  - Add an AST member expression for forms like `COMP[2].two`, `COMP[2][.two]`, and `COMP[2].str[3]`.
  - Update expression visitors/formatting/LSP field resolution for the new AST node.
- Add a lowering helper that decomposes indexed repeat accesses into `{base symbol, addend}`:
  - `COMP[i]` -> `COMP + i * COMP.element_size`
  - `COMP[i].two` -> `COMP + i * COMP.element_size + COMP.two:offsetof`
  - `COMP[i].str[j]` -> `COMP + i * COMP.element_size + COMP.str:offsetof + j * sizeof(str element)`
  - Works for fixed, allocated ABS, allocated DP, and cross-unit repeat vars using existing literal or `LabelOffset` relocation paths.
- Extend address-of recognition so native operands like these are immediate address loads:
  - `lda &&COMP + COMP:sizeof`
  - `lda &&COMP + 2*COMP:sizeof`
  - `lda &&COMP[1]`
  - `lda &&COMP[2].two`
  This should replace the current literal-only addend handling with constant-expression addends.
- Preserve existing behavior where non-repeat byte-offset indexing remains generic arithmetic; numeric aggregate indexing without `* N` remains an error with improved help explaining that aggregate numeric indexing requires a repeated var.

## Diagnostics And Docs
- Add rich diagnostics for:
  - non-constant repeat indices
  - negative indices
  - out-of-range indices (`index >= repeat_count`)
  - indexing a non-repeated compound aggregate
  - field access after a repeated non-compound var (`STR[1].field`)
  - unknown fields after element indexing, with nearest-field suggestion
  - abstract var address/index use
- Update `docs/syntax-reference.md` and `docs/symbolic-subscripts.md`:
  - Clarify `[N]` on declarations reserves bytes, while `* N` creates repeat elements addressable by `NAME[i]`.
  - Document equivalence: `&&COMP[i] == &&COMP + i*COMP:sizeof`.
  - Document compound forms: `COMP[i].field`, `COMP[i][.field]`, and `COMP[i].array_field[j]`.
  - State bounds and constant-index requirements.

## Test Plan
- Parser/unit tests:
  - Parse `COMP[2]`, `COMP[2].two`, `COMP[2][.two]`, `COMP[2].str[3]`, `&&COMP[2]`.
  - Verify existing `COMP.two`, `COMP[.two]`, and `COMP.str[2]` AST behavior remains compatible.
- Lowering/unit tests:
  - Fixed compound repeat emits literal or address-of relocation addends matching `i * element_size`.
  - Allocated ABS and DP repeats use existing `LabelOffset` relocations with correct addends and address-size hints.
  - Cross-unit repeat metadata propagates enough to lower `COMP[2].field` from another file.
- Golden fixtures:
  - Refresh/expand `syntax-element-index` for fixed `COMP[...] * 10`, `STR:byte * 10`, and `WORDS:word * 4`, with mode changes so byte/word typed accesses validate.
  - Add allocated ABS, allocated DP, and cross-unit positive fixtures.
  - Add negative fixtures for no-repeat aggregate index, nonconstant index, negative index, out-of-range index, unknown field after element index, member access on scalar repeat, and abstract var indexing.
- Verification:
  - Bless selected fixtures with `cargo run -p k816-golden-tests --bin bless -- --case fixture:<name>`.
  - Run `cargo test -p k816-core`.
  - Run `cargo test -p k816-golden-tests`.
  - Run `cargo clippy --all-targets --all-features`.

## Assumptions
- No new object format version is needed; existing relocation addends are sufficient.
- Field-array indexing semantics (`COMP.str[i]`) remain unchanged except where combined with a repeated aggregate base.
- Runtime-variable indices are out of scope for this bracket form; users must use indexed addressing or explicit pointer arithmetic for runtime indexing.
