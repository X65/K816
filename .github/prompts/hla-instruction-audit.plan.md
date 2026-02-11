# Plan: Audit & Fix K65 HLA Instruction Lowering

## Context

The K65 HLA documentation (`vendor/doc/docs/instructions.md`) describes HLA syntax forms for all 56 standard 6502 opcodes. The compiler handles many correctly but has bugs and missing implementations. This plan audits each instruction section, fixes bugs, implements missing HLA forms, and ensures test coverage.

## Bugs Found

### Bug 1: `a?mem` generates CMP immediate instead of CMP memory
- **Files**: `crates/core/src/parser/mod.rs:1796-1814`, `crates/core/src/lower.rs:605-672`
- **Issue**: `hla_condition_seed_stmt_parser` creates ConditionSeed for plain `a?mem`, lowering always generates `CMP #value` (immediate). For variables at e.g. 0x40, it produces `CMP #$40` instead of `CMP $0040`.
- **Fix**: Use `expr_is_address_like()` in parser to route address-like exprs to CMP Instruction (not ConditionSeed). Keep ConditionSeed only for literals/constants.
- **Test update**: `syntax-arith-ops` expected.lst/.bin (CMP line changes from `C9 40` to `CD 40 00`)

### Bug 2: `goto (mem)` generates JMP absolute instead of JMP indirect
- **Files**: `crates/core/src/parser/mod.rs:1564-1581`
- **Issue**: `flow_stmt_parser` strips parentheses from `goto (expr)` but uses `Direct` addr_mode. Should use `Indirect`.
- **Fix**: Distinguish parenthesized vs plain form, set `OperandAddrMode::Indirect` for `goto (expr)`.
- **Test update**: `syntax-goto-call-and-far` expected (JMP opcode changes from 4C to 6C)

### Bug 3: `*N` generates single NOP instead of N NOPs
- **Files**: `crates/core/src/parser/mod.rs:1630-1638`
- **Issue**: Parser consumes count but discards it, always generating single NOP.
- **Fix**: Generate N NOP instructions from `*N` syntax.
- **Test update**: `syntax-nop-shortcuts` expected (should have 7 NOPs: 1 + 5 + 1)

## Branch Form Mappings (derived from docs)

### Goto → Branch mnemonic
| Goto form | Mnemonic | When |
|-----------|----------|------|
| `< goto` | bcc | C=0 |
| `>= goto` | bcs | C=1 |
| `== goto` | beq | Z=1 |
| `!= goto` | bne | Z=0 |
| `<0 goto` | bmi | N=1 |
| `>=0 goto` | bpl | N=0 |
| `<<= goto` | bvs | V=1 |
| `>>= goto` | bvc | V=0 |
| `c-? goto` | bcc | C=0 |
| `c+? goto` | bcs | C=1 |
| `z+? goto` | beq | Z=1 |
| `z-? goto` | bne | Z=0 |
| `n+? goto` | bmi | N=1 |
| `n-? goto` | bpl | N=0 |
| `v+ goto` | bvs | V=1 |
| `v- goto` | bvc | V=0 |

### Postfix `}` → Branch mnemonic (loop back)
| Postfix | Mnemonic | Loop while |
|---------|----------|-----------|
| `} <` | bcc | C=0 |
| `} >=` | bcs | C=1 |
| `} ==` | beq | Z=1 |
| `} !=` | bne | Z=0 |
| `} <0` | bmi | N=1 |
| `} >=0` | bpl | N=0 |
| `} <<=` | bvs | V=1 |
| `} >>=` | bvc | V=0 |
| `} c-?` | bcc | C=0 |
| `} c+?` | bcs | C=1 |
| `} z+?` | beq | Z=1 |
| `} z-?` | bne | Z=0 |
| `} n+?` | bmi | N=1 |
| `} n-?` | bpl | N=0 |
| `} v+` | bvs | V=1 |
| `} v-` | bvc | V=0 |

Already implemented: `} <`, `} >=`, `} ==`, `} !=`, `} <=`, `} >`, `} n-?`, `} n+?`

### Prefix → Skip branch mnemonic (branch OVER the block)
| Prefix | Skip branch | Execute when |
|--------|-------------|-------------|
| `>={ }` | bcc | C=1 |
| `<{ }` | bcs | C=0 |
| `=={ }` | bne | Z=1 |
| `!={ }` | beq | Z=0 |
| `<0{ }` | bpl | N=1 |
| `>=0{ }` | bmi | N=0 |
| `<<={ }` | bvc | V=1 |
| `>>={ }` | bvs | V=0 |
| `c+?{ }` | bcc | C=1 |
| `c-?{ }` | bcs | C=0 |
| `z+?{ }` | bne | Z=1 |
| `z-?{ }` | beq | Z=0 |
| `n+?{ }` | bpl | N=1 |
| `n-?{ }` | bmi | N=0 |
| `v+?{ }` | bvc | V=1 |
| `v-?{ }` | bvs | V=0 |

## Implementation Steps

### Step 1: Fix Bug 1 - `a?mem` CMP addressing
1. In `hla_condition_seed_stmt_parser`: when `expr_is_address_like(parsed.expr)` is true AND index/addr_mode is plain Direct, generate `instruction_stmt("cmp", Some(Operand::Value {...}))` instead of ConditionSeed
2. Update `syntax-arith-ops` expected files
3. Run `cargo test -p k816-golden-tests`

### Step 2: Fix Bug 2 - `goto (mem)` indirect JMP
1. In `flow_stmt_parser`: separate the parenthesized form from the plain form, set `addr_mode: Indirect`
2. Update `syntax-goto-call-and-far` expected files
3. Run tests

### Step 3: Fix Bug 3 - `*N` multiple NOPs
1. In `nop_stmt_parser`: capture count, return multiple NOP statements via `Stmt::Bytes` of repeated `0xEA` or emit multiple instructions
2. Actually, since the parser returns a single Stmt, we'll need a different approach - perhaps a new `Stmt` variant or emit the count through the existing pipeline. Simplest: generate a `Stmt::Bytes(vec![Expr::Number(0xEA); n])`.
3. Update `syntax-nop-shortcuts` expected files
4. Run tests

### Step 4: Add postfix loop closes for all flag/condition forms
1. Extend `hla_do_close_suffix` parser to recognize:
   - `c-?`, `c+?` → new close forms
   - `z+?`, `z-?` → new close forms
   - `v+`, `v-` (no `?`) → new close forms
   - `<0`, `>=0`, `<<=`, `>>=` → new close forms
2. Add new HlaStmt variants or map directly to branch instructions
3. Add lowering in `lower_hla_stmt`
4. Create test fixture `syntax-postfix-flag-closes`
5. Run tests

### Step 5: Add missing goto forms
1. Extend `flow_stmt_parser` `branch_goto_stmt` to handle:
   - `<0 goto` → bmi, `>=0 goto` → bpl
   - `<<= goto` → bvs, `>>= goto` → bvc
   - `c-? goto` → bcc, `c+? goto` → bcs
   - `z+? goto` → beq, `z-? goto` → bne
   - `n+? goto` → bmi, `n-? goto` → bpl
   - `v+ goto` → bvs, `v- goto` → bvc
2. Create test fixture `syntax-branch-goto-extended`
3. Run tests

### Step 6: Add prefix conditional blocks
1. Add HlaStmt variants: `PrefixOpen { skip_branch: String }` and let DoClose handle it
2. Create parser for prefix form: `condition { ... }` → emits skip branch over block
3. Lowering: emit skip branch to fresh label, emit block, emit label
4. Remove prefix forms from `discard_stmt_parser`
5. Update `syntax-branch-prefix` test fixture
6. Run tests

### Step 7: Add missing test coverage
1. Create `syntax-cpx-cpy-memory` fixture for `x?mem`, `y?mem`
2. Verify all STA/STX/STY addressing modes have coverage
3. Any remaining gaps
4. Run full `cargo test`

## Files to Modify

| File | Changes |
|------|---------|
| `crates/core/src/parser/mod.rs` | Bug fixes + new parsers for goto/postfix/prefix forms |
| `crates/core/src/ast/mod.rs` | New HlaStmt variants for prefix conditionals + flag-based closes |
| `crates/core/src/lower.rs` | Lowering for new HLA forms |
| `crates/core/src/normalize_hla.rs` | Handle new HlaStmt variants |
| `tests/golden/fixtures/syntax-arith-ops/` | Update expected (CMP bug fix) |
| `tests/golden/fixtures/syntax-goto-call-and-far/` | Update expected (JMP indirect fix) |
| `tests/golden/fixtures/syntax-nop-shortcuts/` | Update expected (NOP count fix) |
| `tests/golden/fixtures/syntax-branch-prefix/` | Rewrite expected (prefix conditionals) |
| `tests/golden/fixtures/syntax-postfix-flag-closes/` | New fixture |
| `tests/golden/fixtures/syntax-branch-goto-extended/` | New fixture |
| `tests/golden/fixtures/syntax-cpx-cpy-memory/` | New fixture |

## Verification

```bash
cargo test -p k816-golden-tests   # Golden tests
cargo test -p k816-core            # Unit tests
cargo test                         # All tests
```

Each step runs tests incrementally before proceeding to the next.
