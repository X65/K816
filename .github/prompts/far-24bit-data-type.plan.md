# Add `far` as 24-bit data type: data prefix, variable width, subscripts, views

## Context

The 65c816 CPU has a 24-bit address bus (16MB, 256 banks of 64KB). Cross-bank "far" addressing uses 3-byte little-endian addresses. The K816 compiler already supports far functions (`far func`), far calls (`call far`) using JSL/RTL, and the linker handles `width: 3` relocations. This change adds comprehensive 24-bit `far` support, paralleling how `byte` (8-bit) and `word` (16-bit) already work:

1. **`far` data prefix** — emit 3-byte values in `data {}` blocks (like `word` for 2-byte)
2. **`DataWidth::Far`** — 3-byte variable declarations (`var ptr:far = $100`)
3. **`:far` symbolic subscript fields** — 3-byte fields in subscript layouts
4. **`:far` typed views** — explicit width annotation on expressions

Direct register load/store of `:far` values is an **error** — the 65816 has no 24-bit register mode, so the programmer must use explicit `:byte` or `:word` views for partial access.

## Part A: `far` data prefix in named data blocks

### A1. AST — add `Fars` variant

**File:** `crates/core/src/ast/mod.rs` (line ~311, after `Words`)

```rust
Fars(Vec<Expr>),
```

### A2. HIR — add `FullLong` relocation kind

**File:** `crates/core/src/hir/mod.rs` (line ~65, after `FullWord`)

```rust
FullLong,
```

### A3. Parser — add `far` entry parsing

**File:** `crates/core/src/parser/mod.rs`

In both `named_data_entry_parser()` (~line 765) and `named_data_entry_flat_parser()` (~line 1016):

Clone from `word_value`/`word_entry`, matching `"far"` and mapping to `NamedDataEntry::Fars`. Define `far_value` and `far_entry` before `word_value`/`word_entry` (since those clone `address_byte_expr`/`undef_byte`). Wire into both choice chains: `far_entry` before `word_entry` before `bytes_entry`.

### A4. Lowering — `evaluate_far_exprs()`

**File:** `crates/core/src/lower.rs`

Add `NamedDataEntry::Fars` case in `lower_named_data_entry()` (~line 509, after `Words`).

Create `evaluate_far_exprs()` modeled on `evaluate_word_exprs()` (line 2195):

- `Vec::with_capacity(values.len() * 3)` — 3 bytes per value
- Numeric: validate `0..=0xFFFFFF`, emit 3 LE bytes
- Symbol: push 3 zero placeholder bytes, `ByteRelocationKind::FullLong`
- `&<`/`&>` unary: push 3 zero placeholders (single-byte relocation)
- `?`: emit 3 zero bytes
- Error: "far address literal out of range: {number}"

### A5. Emit (single-file) — handle `FullLong`

**File:** `crates/core/src/emit.rs` (~line 816, after `FullWord` arm)

```rust
ByteRelocationKind::FullLong => {
    if offset + 2 >= bytes.len() {
        diagnostics.push(Diagnostic::error(span, "internal fixup overflow"));
        return;
    }
    let le = value.to_le_bytes();
    bytes[offset] = le[0];
    bytes[offset + 1] = le[1];
    bytes[offset + 2] = le[2];
}
```

### A6. Emit (object) — handle `FullLong`

**File:** `crates/core/src/emit_object.rs` (~line 314, after `FullWord`)

```rust
ByteRelocationKind::FullLong => (3, RelocationKind::Absolute),
```

Linker already handles `width: 3` (`crates/link/src/lib.rs:1861`).

### A7. Pass-through in transforms

Add `Fars` arm in each file, identical pattern to `Words`:

- **`crates/core/src/normalize_hla.rs`** (~line 65): clone pass-through
- **`crates/core/src/eval_expand.rs`** (~line 162): expand_expr map
- **`crates/core/src/sema/mod.rs`** (~line 306): collect values, validate `0..=0xFFFFFF`
- **`crates/fmt/src/print_ast.rs`** (~line 284): format as `"far ..."`
- **`crates/fmt/src/print_ir.rs`** (~line 51, 69): `FullLong => "far"` display

## Part B: `DataWidth::Far` — variables, subscripts, views

### B1. AST — add `Far` to `DataWidth` enum

**File:** `crates/core/src/ast/mod.rs` (line ~7)

```rust
pub enum DataWidth {
    Byte,
    Word,
    Far,
}
```

### B2. Parser — add `:far` recognition

**`data_width_parser()`** (line ~1167): add match arm

```rust
TokenKind::Ident(value) if value.eq_ignore_ascii_case("far") => DataWidth::Far,
```

**`parse_symbolic_subscript_field_entry()`** (line ~1567): add else-if branch

```rust
} else if type_name.eq_ignore_ascii_case("far") {
    Some(DataWidth::Far)
```

### B3. Semantic analysis — 3-byte element sizes

**File:** `crates/core/src/sema/mod.rs`

Variable element size (~line 591):

```rust
Some(DataWidth::Far) => 3,
```

Field element size (~line 764):

```rust
DataWidth::Far => 3_u32,
```

### B4. Lowering — register access error + index scaling

**File:** `crates/core/src/lower.rs`

**`data_width_to_reg_width()`** (line ~1581): change return type to `Option<RegWidth>`:

```rust
fn data_width_to_reg_width(width: DataWidth) -> Option<RegWidth> {
    match width {
        DataWidth::Byte => Some(RegWidth::W8),
        DataWidth::Word => Some(RegWidth::W16),
        DataWidth::Far => None,  // no 24-bit register mode
    }
}
```

**Caller at line ~1408**: when `data_width_to_reg_width` returns `None`, emit a diagnostic error:

```
"Cannot directly load/store :far value — use explicit :byte or :word view"
```

with help: `"e.g. {name}:word for low 16 bits, ({name}+2):byte for bank byte"`

**`data_width_name()`** (line ~1588): add `DataWidth::Far => "far"`

**Typed access diagnostics** (~line 1481): add `DataWidth::Far` case in the message formatting (though the error at line 1408 should fire first).

**Index scaling** — two locations (~lines 2486, 2983):

```rust
DataWidth::Far => 3_i64,
```

### B5. Formatter — print `:far`

**File:** `crates/fmt/src/print_ast.rs`

Three match locations (~lines 164, 182, 264): add `DataWidth::Far => "far"` / `":far"`.

## Part C: Golden tests

### C1. `tests/golden/fixtures/syntax-data-fars/`

Data block with `far` prefix — numeric and symbolic values, verifying 3-byte LE emission and relocation.

### C2. `tests/golden/fixtures/syntax-var-far/` (or extend existing fixture)

Variable declaration with `:far` width, symbolic subscript with `:far` fields, `:far` typed views.

### C3. Error test for direct register access of `:far`

Test that `lda some_var:far` produces the expected error diagnostic.

## Part D: Documentation

**File:** `docs/syntax-reference.md`

- Add `### far (24-bit Far Address Entries)` section after `word` section (~line 363)
- Update variable declaration docs to mention `:far` alongside `:byte`/`:word`

**File:** `docs/symbolic-subscripts.md`

- Add `:far` to field type examples

## Verification

1. `cargo build` — compiles cleanly
2. `cargo test` — all existing 331 tests pass + new golden tests
3. Data prefix test: 3-byte LE emission + relocation verified in listing
4. Var/subscript test: `:far` layout with 3-byte elements
5. Error test: direct `lda`/`sta` on `:far` values produces diagnostic
