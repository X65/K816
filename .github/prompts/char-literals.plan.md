# Add Character Literal Support (`lda #'s'`)

## Context

k816 currently supports numeric literals (`$73`, `115`, `0b1110011`) and double-quoted strings in data blocks, but has no way to use a single character as an immediate numeric value. Adding character literal syntax (`'s'`) lets users write `lda #'s'` instead of `lda #$73`, which is more readable when working with ASCII character data.

## Approach

Character literals are just syntactic sugar for their ASCII numeric value. The simplest implementation maps them directly to `Number(i64)` tokens at the lexer level — no AST, parser, evaluator, or lowering changes needed.

Two independent lexers must be updated:

### 1. Main assembler lexer — `crates/core/src/lexer.rs`

Add a second `#[regex]` attribute on the existing `Number(i64)` variant (logos supports multiple patterns per variant):

```rust
#[regex(r"%[01]+|0b[01]+|0x[0-9a-fA-F]+|\$[0-9a-fA-F]+|[0-9]+", parse_number)]
#[regex(r"'([^'\\]|\\.)'", parse_char)]
Number(i64),
```

Add `parse_char` callback (after `parse_string`, ~line 228):

- Strip surrounding `'` quotes
- Handle same escape sequences as `parse_string`: `\n`, `\r`, `\t`, `\\`, `\'`
- Return the character's value as `i64`

### 2. Expression evaluator lexer — `crates/eval/src/expr.rs`

In `next_token()` (~line 315), add a match arm before the `_` fallback:

```rust
'\'' => self.lex_char_literal(),
```

Add `lex_char_literal()` method (after `lex_base_prefixed_binary`, ~line 413):

- Consume opening `'`
- Handle escape sequences (same set: `\n`, `\r`, `\t`, `\\`, `\'`)
- Consume closing `'` (error if missing)
- Return `TokenKind::Number(Number::Int(value))`

### 3. Tests

**Unit test** in `crates/eval/src/expr.rs` — add to existing `mod tests`:

- Basic: `'A'` == 65, `'0'` == 48
- Escape: `'\n'` == 10, `'\\'` == 92, `'\''` == 39
- In expression: `'A' + 1` == 66

**Golden test fixture** — create `tests/golden/fixtures/syntax-char-literals/`:

- `input.k65`: function using `lda #'A'` and data block with char literals
- `expected.bin` and `expected.lst`: generated via `cargo test --features golden-bless` (or the bless binary)

## Files to modify

| File | Change |
|------|--------|
| `crates/core/src/lexer.rs` | Add `#[regex]` + `parse_char` callback |
| `crates/eval/src/expr.rs` | Add `'\''` arm + `lex_char_literal()` method + unit tests |
| `tests/golden/fixtures/syntax-char-literals/input.k65` | New fixture (create) |
| `tests/golden/fixtures/syntax-char-literals/expected.bin` | New fixture (bless) |
| `tests/golden/fixtures/syntax-char-literals/expected.lst` | New fixture (bless) |

## Verification

1. `cargo test -p k816-eval` — runs unit tests including new char literal tests
2. `cargo test -p k816-core` — ensures lexer/parser integration works
3. `cargo test -p golden` — runs golden test with the new fixture
4. If golden test fails because expected files don't exist yet, bless them with `cargo test -p golden --features golden-bless`
