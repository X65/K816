# Add auto-incrementing constants to `const` declarations

## Context

K816 currently requires explicit values for every constant (`const A = 0, B = 1, C = 2`). This extends `const` so that bindings in a comma-separated group can omit `= expr` and auto-increment from the previous binding. No new keyword is introduced.

**Rules:**

- Omitted initializer is allowed in comma-separated groups (`const A = 0, B, C`).
- The first binding may also omit `= expr` only when it is part of a list (`const A, B, C` implies `A = 0`). A standalone `const FOO` remains a parse error.
- Auto-increment works when the previous binding is compile-time resolvable under existing const-expression semantics (`Expr::Number`, resolvable identifiers, and existing arithmetic forms).
- Explicit initializer resets the sequence (`const A = 0, B, C = 10, D` implies `D = 11`).
- If the previous binding is not compile-time resolvable, omitting `= expr` is a compilation error.

**Syntax:**

```k65
const A = 0, B, C              // A=0, B=1, C=2
const A = 10, B, C             // A=10, B=11, C=12
const A, B, C                  // A=0, B=1, C=2
const A = 0, B, C = 10, D      // A=0, B=1, C=10, D=11
const BASE = 10
const A = BASE, B, C           // A=10, B=11, C=12 (BASE is compile-time resolvable)

// multi-line with comma continuation (trailing comma allowed):
const SPRITE_X    = 0,
      SPRITE_Y,
      SPRITE_TILE,
      SPRITE_ATTR,
```

## Approach

Parser-driven desugaring:

1. **Parser** — parses const groups with optional `= expr` per binding and immediately desugars omitted values into concrete expressions.
2. **Sema** — evaluates the resulting concrete const expressions as today.

No AST shape changes and no new keyword; by sema time every `ConstDecl` still has a concrete initializer `Expr`.

**Auto-increment desugaring algorithm** (in parser, applied to each const group):

- `const A, ...` desugars first omitted binding to `A = 0` only when the group has at least one more binding.
- Any omitted binding `NAME` after previous binding `PREV` desugars to `NAME = PREV + 1`.
- Explicit initializer bindings are kept as-is and become the new predecessor for following omitted bindings.
- Single-binding `const NAME` (no `=`, no comma tail) is a parse error.

## Files to modify

### 1. Parser — `crates/core/src/parser/mod.rs`

- Add/adjust const binding parser so `= expr` is optional per binding.
- Desugar omitted initializers inside `const_decl_item_parser()` into concrete `Expr` values.
- Keep `const FOO` rejected with a targeted diagnostic.
- After each comma in a const group, eat optional `TokenKind::Newline` for multi-line continuation.

```rust
let const_sep = just(TokenKind::Comma).then_ignore(just(TokenKind::Newline).repeated());

const_decl_group_binding_parser(source_id)
    .separated_by(const_sep)
    .allow_trailing()
    .collect::<Vec<_>>()
```

### 2. Parser tests — `crates/core/src/parser/mod.rs`

Add/adjust tests for:

- `const A = 0, B, C`
- `const A, B, C`
- `const A = 0, B, C = 10, D`
- `const BASE = 10` then `const A = BASE, B, C`
- multiline trailing-comma form
- rejection of `const FOO`
- rejection of `const A = UNKNOWN, B, C`

### 3. Sema tests — `crates/core/src/sema/mod.rs`

Add tests confirming evaluated values for implicit and reset cases.

### 4. Documentation — `docs/syntax-reference.md`

Update the "Constant Declaration" section with:

- shorthand forms
- explicit note that standalone `const FOO` remains invalid

### 5. Golden tests

Add:

- **`tests/golden/fixtures/syntax-const-sequence/`** — success case:
  - Auto-increment from explicit start value
  - List-leading implicit zero (`const A, B, C`)
  - Mixed explicit and auto values with reset
  - Multi-line comma continuation
  - Const members used in instructions/var expressions
- **`tests/golden/fixtures/syntax-const-err-missing-initializer/`** — error case update:
  - Keep/adjust to assert standalone `const FOO` is still rejected
- Update `tests/golden/fixtures/syntax-fixture-matrix.md`

## Verification

1. `cargo test` — all existing tests pass.
2. `cargo test -p k816-golden-tests`
3. Verify parser/sema tests cover:
   - list-leading implicit zero
   - mid-list explicit reset
   - standalone `const FOO` rejection
   - resolvable-identifier continuation (`const BASE = 10` then `const A = BASE, B, C`)
