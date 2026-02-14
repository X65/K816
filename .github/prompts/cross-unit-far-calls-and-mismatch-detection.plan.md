# Plan: Cross-Unit Far Calls and Calling Convention/Width Mismatch Detection

## Context

Cross-compilation-unit near calls (JSR) work after the previous change. Three gaps remain:

1. **`call far` syntax** is not supported by the parser — `call far lib_init` fails with a parse error.
2. **Calling convention mismatch** (near call to far function, or far call to near function) is not detected.
3. **Register width mismatch** (caller's A/I width differs from callee's declared contract) is not detected for cross-unit calls. Within a single unit the compiler inserts REP/SEP automatically; across units it assumes a match but the linker must verify.

## Implementation

### Step 1: Extend o65 `Symbol` with `FunctionMetadata`

File: [o65/src/lib.rs](crates/o65/src/lib.rs)

Add a struct and optional field on `Symbol`:

```rust
#[derive(Debug, Clone, Copy, Default)]
pub struct FunctionMetadata {
    pub is_far: bool,
    /// None = unspecified, Some(false) = 8-bit, Some(true) = 16-bit
    pub a_width: Option<bool>,
    pub i_width: Option<bool>,
}

pub struct Symbol {
    // ...existing fields...
    pub function_metadata: Option<FunctionMetadata>,
}
```

Bump `PAYLOAD_VERSION` to 7. Encode/decode: after the existing symbol fields, write a `has_function_metadata` byte. If present, write `is_far` (u8), `a_width` (u8: 0=unset, 1=8-bit, 2=16-bit), `i_width` (u8). Decode conditionally on `version >= 7`.

Update all `Symbol` construction sites (tests, emit_object, linker helpers) to include `function_metadata: None` by default.

### Step 2: Extend o65 `Relocation` with `CallMetadata`

File: [o65/src/lib.rs](crates/o65/src/lib.rs)

```rust
#[derive(Debug, Clone, Copy, Default)]
pub struct CallMetadata {
    /// Caller's accumulator width at the call site.
    pub caller_a_width: Option<bool>,
    /// Caller's index register width at the call site.
    pub caller_i_width: Option<bool>,
}

pub struct Relocation {
    // ...existing fields...
    pub call_metadata: Option<CallMetadata>,
}
```

Encode/decode after existing relocation fields (under `version >= 7`). Same pattern: `has_call_metadata` byte, then two width bytes.

Update all `Relocation` construction sites to include `call_metadata: None`.

### Step 3: Extend AST `CallStmt` with `is_far`

File: [ast/mod.rs](crates/core/src/ast/mod.rs)

```rust
pub struct CallStmt {
    pub target: String,
    pub is_far: bool,
}
```

### Step 4: Update parser for `call far <ident>` syntax

File: [parser/mod.rs](crates/core/src/parser/mod.rs)

Modify `call_stmt` (line ~1450) to optionally consume `TokenKind::Far` after `TokenKind::Call`:

```rust
let call_stmt = just(TokenKind::Call)
    .ignore_then(just(TokenKind::Far).or_not())
    .then(ident_parser())
    .map(|(far, target)| Stmt::Call(CallStmt { target, is_far: far.is_some() }));
```

Update `far_call_stmt` (line ~2355) in the HLA parser to also set `is_far: true`.

### Step 5: Add `is_far` to HIR `Op::FunctionStart`

File: [hir/mod.rs](crates/core/src/hir.rs) (wherever `Op::FunctionStart` is defined)

Add `is_far: bool` field to `Op::FunctionStart { name, mode_contract, is_entry, is_far }`.

### Step 6: Update lowering to propagate `is_far` and caller mode

File: [lower.rs](crates/core/src/lower.rs)

- When emitting `Op::FunctionStart` for a CodeBlock, set `is_far` from `ast.is_far`.
- In the `Stmt::Call` handler for cross-unit calls (line ~696), use `call.is_far` instead of hardcoded `false`:
  ```rust
  } else if ctx.options.allow_undefined_functions {
      lower_call_with_contract(&target, call.is_far, None, None, span, ctx, ops);
  }
  ```

### Step 7: Emit `FunctionMetadata` and `CallMetadata` in emit_object

File: [emit_object.rs](crates/core/src/emit_object.rs)

- Track a map `function_label_metadata: HashMap<String, FunctionMetadata>` during emit.
- When processing `Op::FunctionStart`, record `{ is_far, a_width: mode_contract.a_width mapped to bool, i_width: ... }` keyed by function name.
- When building the final symbol list, attach `function_metadata` from this map to matching symbols.
- When building relocations from fixups, for fixups that originate from call instructions (need to track this), attach `CallMetadata` with the caller's `m_wide`/`x_wide` state at the call site.

To identify call-site fixups: Add a `call_metadata: Option<CallMetadata>` field to the `Fixup` struct. In the `Op::Instruction` handler, when the mnemonic is `jsr` or `jsl`, propagate the current `m_wide`/`x_wide` state into the fixup as `CallMetadata`.

### Step 8: Add linker validation for calling convention and width mismatches

File: [link/src/lib.rs](crates/link/src/lib.rs)

After resolving all relocations (line ~410 area), add a new validation pass:

For each relocation that has `call_metadata`:
1. Look up the target symbol in the resolved symbol table.
2. Find the symbol's `FunctionMetadata` from the original o65 objects.
3. **Far/near mismatch**: If relocation width is 2 (near JSR) but `function_metadata.is_far == true` → error. If width is 3 (far JSL) but `is_far == false` → error.
4. **Register width mismatch**: Compare `call_metadata.caller_a_width` with `function_metadata.a_width`. If both are `Some` and they differ → error. Same for `i_width`.

Collect all mismatch errors and report them with source context using the existing `decorate_with_anchor_with_label` mechanism.

Build a lookup map `symbol_name -> FunctionMetadata` from all input objects for efficient lookup.

### Step 9: Update test fixtures

**link-multi-func/** — success case:
- Existing `expected.err` was already deleted in previous work.
- After parser change, `call far lib_init` will parse correctly.
- Delete and re-bless `expected.bin` and `expected.lst` (content will change because lib_init is now JSL/RTL).

**link-multi-func-err/** — error case:
- Delete current `expected.err` (old parse error).
- Create empty `expected.err`.
- Bless to populate with new linker errors about:
  - register width mismatch calling `app_init` (caller a=8-bit, callee expects a=16-bit; same for i)
  - near call to far function `lib_init`
  - far call to near function `app_start`

## Key Files

- [crates/o65/src/lib.rs](crates/o65/src/lib.rs) — o65 object format: Symbol, Relocation, FunctionMetadata, CallMetadata, encode/decode, PAYLOAD_VERSION bump
- [crates/core/src/ast/mod.rs](crates/core/src/ast/mod.rs) — AST: CallStmt.is_far
- [crates/core/src/parser/mod.rs](crates/core/src/parser/mod.rs) — Parser: `call far <ident>` syntax
- [crates/core/src/hir.rs](crates/core/src/hir.rs) — HIR: Op::FunctionStart.is_far
- [crates/core/src/lower.rs](crates/core/src/lower.rs) — Lowering: propagate is_far, caller mode
- [crates/core/src/emit_object.rs](crates/core/src/emit_object.rs) — Emit: attach FunctionMetadata to symbols, CallMetadata to relocations
- [crates/link/src/lib.rs](crates/link/src/lib.rs) — Linker: validate far/near and register width mismatches
- [tests/golden/fixtures/link-multi-func/](tests/golden/fixtures/link-multi-func/) — Success test
- [tests/golden/fixtures/link-multi-func-err/](tests/golden/fixtures/link-multi-func-err/) — Error test

## Verification

1. `cargo test -p k816-o65` — o65 roundtrip tests pass with new metadata fields
2. `cargo test -p k816-core` — parser/lower/emit tests pass
3. `cargo test -p k816-golden-tests --features golden-bless` — bless both fixtures
4. `cargo test -p k816-golden-tests` — golden tests pass
5. `cargo test --workspace` — all tests pass
