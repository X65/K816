# Implement `abstract var` Layout Declarations

## Summary

Add `abstract var NAME[...]` as a top-level, storage-free variant of symbolic-subscript `var`. It reuses existing packed field layout semantics and metadata queries, but emits no bytes, no HIR symbol, and no linker-visible object symbol.

Accepted shape:

```k65
abstract var DEV_HANDLER[
    .open  :word
    .close :word
]
```

Allowed queries:

```k65
DEV_HANDLER:sizeof
DEV_HANDLER.open:offsetof
DEV_HANDLER.open:sizeof
```

## Key Changes

- Treat `abstract` as a contextual keyword only before top-level `var`, not a globally reserved token.
- Add `is_abstract` to `VarDecl`; normal vars default to `false`.
- Add an abstract placement/classification to semantic metadata, or equivalent explicit flag, so abstract layouts:
  - participate in duplicate-symbol checks,
  - carry `element_size`, `data_width`, and symbolic field metadata,
  - are propagated across compilation units for `:sizeof` / `:offsetof`,
  - are excluded from addressable-symbol scans and HIR/object emission.
- Restrict abstract declarations during sema with rich diagnostics:
  - must have a symbolic field list,
  - may use declaration-level data width shorthand, e.g. `abstract var POS:byte[...]`,
  - must not have `dp` / `abs` / `far` storage prefix,
  - must not have an initializer,
  - must not have `* count`,
  - must not be an array-only var like `abstract var buf[16]`.
- Update lowering so value/address uses of abstract vars fail early with helpful diagnostics:
  - `lda DEV_HANDLER`
  - `lda DEV_HANDLER.open`
  - `ldx &&DEV_HANDLER`
  - cross-unit uses of the same forms.
  Help should point users to `:sizeof` / `:offsetof`, or to declaring a real `var` / `data` symbol when they need storage.

## Implementation Notes

- Parser: add an `abstract var` top-level item path around the existing `var_decl_parser`, then set `is_abstract = true`; do not accept it through statement parsing.
- Sema: compute abstract var layout through the existing symbolic-subscript layout evaluator, then skip placement/cursor allocation entirely.
- Driver/external collection: propagate abstract layout metadata across source files, but mark it non-addressable so it cannot influence operand encoding or `&&` address validation.
- Lower/object emission: skip `emit_var_absolute_symbols` for abstract vars and add a shared diagnostic path for abstract names where a memory address is required.
- Formatter/LSP/docs: preserve formatting of `abstract var` regions; show hover as an abstract variable/layout with fields and size, not with an address or storage class; document the feature in the metadata-query / symbolic-subscript section.

## Golden Tests

Positive fixtures:

- `syntax-abstract-var-sizeof-offsets`: same-file `abstract var` with nested fields; verify `:sizeof`, `:offsetof`, field `:sizeof`, arithmetic use, and no extra reserved bytes in output/listing.
- `syntax-abstract-var-default-field-width`: `abstract var NAME:byte[...]` verifies declaration-level field-width shorthand still works.
- `syntax-abstract-var-cross-unit`: layout in `input.10-layout.k65`, metadata queries in `input.20-use.k65`.

Negative fixtures with `expected.err` reviewed by eye:

- `syntax-abstract-var-err-no-fields`: `abstract var plain:word`.
- `syntax-abstract-var-err-array`: `abstract var buf[16]`.
- `syntax-abstract-var-err-initializer`: `abstract var SHAPE[...] = $4000`.
- `syntax-abstract-var-err-count`: `abstract var SHAPE[...] * 4`.
- `syntax-abstract-var-err-storage-prefix`: `abstract var dp/abs/far SHAPE[...]`.
- `syntax-abstract-var-err-address-use`: bare operand, field operand, and `&&` address-of use.
- `syntax-abstract-var-err-cross-unit-address-use`: same address-use failures when the abstract layout comes from another input file.

Also add focused Rust tests for parser/sema/lower behavior where golden coverage would be too indirect.

## Verification

Run:

```sh
cargo test -p k816-golden-tests --features golden-bless syntax_abstract_var
cargo test -p k816-golden-tests
cargo test
cargo clippy --all-targets --all-features
```

For new error fixtures, regenerate `expected.err`, then read the rendered diagnostics manually to confirm each has a specific primary label, actionable help, and no default `"here"` label.

## Assumptions

- `abstract var` is top-level language syntax.
- `abstract var NAME:byte[...]` is accepted and keeps the current default-field-width behavior.
- Abstract vars are not addressable, even though they occupy the normal symbol namespace for duplicate-name protection.
- This feature does not introduce types, typed instantiation, or storage templates.
