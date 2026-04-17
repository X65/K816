# Plan: Fill in missing HLA coverage — PHX/PLX/… shorthand, STZ lowering, 65816 addressing modes

## Context

While writing the 65C02/65816 doc update (sibling plan `syntax-reference-65c02-65816-extensions.plan.md`) I verified three concrete gaps in the compiler that force users into raw mnemonics for things the HLA grammar should express naturally:

1. **Stack shorthand for X/Y/B/D registers does not exist** — [`crates/core/src/parser/registers.rs:18-38`](crates/core/src/parser/registers.rs#L18-L38) collapses `x!!`, `y!!`, `b!!`, `d!!` into `HlaStackTarget::P` (→ PHP) because `x/y/b/d` match the status-flag alias list. There is no way to express PHX/PHY/PLX/PLY/PHB/PHD/PLB/PLD from HLA; the user must drop to raw mnemonics.
2. **`mem=0` does not synthesise STZ** — lowering emits LDA #0 + STA mem where STZ would be both shorter (saves 1 byte per occurrence on zp, 1 cycle) and clobber-free (no A-register damage, so contract checks against `@a` after `mem=0` stay clean). STZ supports DirectPage / DirectPageX / Absolute / AbsoluteX.
3. **Four 65816 addressing modes are rejected by the HLA operand grammar** — [`crates/core/src/ast/mod.rs:205-210`](crates/core/src/ast/mod.rs#L205-L210) only has `Direct | Indirect | IndexedIndirectX | IndirectIndexedY`, and [`crates/core/src/parser/operands.rs:30-57`](crates/core/src/parser/operands.rs#L30-L57) explicitly errors on `(expr,S)`, `(expr,S),Y` and never accepts `[expr]` or `[expr],Y`. The underlying ISA (`crates/isa65816/src/lib.rs`) has opcodes for all four, so this is a grammar-coverage gap only.

Goal: close each of the three gaps with a minimal, well-tested change, keeping existing HLA behavior stable. Prioritise in order 1 → 2 → 3 because #1 is a real ambiguity (users are silently getting the wrong opcode), #2 is an optimisation, #3 is an expressiveness gap.

## Gap 1: PHX/PHY/PLX/PLY/PHB/PHD/PLB/PLD shorthand

### Problem

`parse_stack_target` groups `a` → A, and every status-related letter (including `x`, `y`, `b`, `d`) → P. So `x!!` currently emits PHP, not PHX. This is a *silent miscompile* for any user who assumes `x!!` ≡ `phx` (natural inference given `a!!` ≡ `pha`).

### Fix

1. Extend `HlaStackTarget` in [`ast/mod.rs:298-302`](crates/core/src/ast/mod.rs#L298-L302) to:

   ```rust
   pub enum HlaStackTarget { A, X, Y, B, D, P }
   ```

   (No K — PHK has no PLK, and the symmetry breaks; K stays as raw mnemonic.)

2. Rework `parse_stack_target` in [`parser/registers.rs:18-38`](crates/core/src/parser/registers.rs#L18-L38) so that names matching a pushable register (`a`, `x`, `y`, `b`, `d`) map to the corresponding register variant. Only pure status-flag names (`p`, `flag`, `n`, `v`, `o`, `m`, `i`, `z`, `c`) map to `P`.

3. Extend the lowering table in [`lower.rs:3791-3795`](crates/core/src/lower.rs#L3791-L3795):

   ```rust
   (A, true)  => "pha", (A, false) => "pla",
   (X, true)  => "phx", (X, false) => "plx",
   (Y, true)  => "phy", (Y, false) => "ply",
   (B, true)  => "phb", (B, false) => "plb",
   (D, true)  => "phd", (D, false) => "pld",
   (P, true)  => "php", (P, false) => "plp",
   ```

4. Update the `sema::functions` clobber/width tracker so that PHX/PHY pushes follow the X-flag width (`@i8`/`@i16`) and PHB/PHD follow DB/DP widths (both 8-bit and 16-bit respectively — PHD always pushes 16 bits, PHB always 8). Cross-check against the existing width propagation logic in [`crates/isa65816/src/lib.rs:500-530`](crates/isa65816/src/lib.rs#L500-L530) which already has these classifications for raw mnemonics.

### Migration / ambiguity

- `c!!` currently means PHP. After the fix, `c` is **not** listed as a pushable register (C is the 16-bit accumulator; there is no PHC). Keep `c!!` → PHP for backwards compatibility — but add a warning suggesting `flag!!` or `p!!` to disambiguate, because relying on "c is a status-flag alias" is surprising.
- `x!!` / `y!!` / `b!!` / `d!!` **change meaning**. This is a breaking change for any existing code that relied on them pushing P. Audit with:

  ```
  rg '(^|[^a-z])[xybd]!!' --glob '*.k65'
  rg '(^|[^a-z])[xybd]\?\?' --glob '*.k65'
  ```

  If any hits exist outside `tests/golden/fixtures`, alert before landing. If hits inside fixtures exist, update them to `p!!` / `flag!!` as appropriate.
- The existing test suite covers `a!!/a??` and the flag-alias set — extend [`parser/tests/statements.rs:140-161`](crates/core/src/parser/tests/statements.rs#L140-L161) with round-trip tests for `x!!`, `y!!`, `b!!`, `d!!` and their `??` counterparts.

### Files touched

- `crates/core/src/ast/mod.rs`
- `crates/core/src/parser/registers.rs`
- `crates/core/src/parser/operations.rs` (error message in `suffix`)
- `crates/core/src/lower.rs`
- `crates/core/src/sema/functions.rs` (width/clobber tracking) — only if contract checks break
- `crates/core/src/parser/tests/statements.rs` (new tests)
- Any affected golden fixtures under `tests/golden/fixtures/` — regenerate snapshots with `cargo insta review` after the code change.

## Gap 2: synthesise STZ from `mem=0`

### Problem

The HLA chain `mem=0` today lowers to:

```
lda #0
sta mem
```

STZ supports zp, zp,X, abs, abs,X (see STZ entries in `crates/isa65816/src/lib.rs`: lines 245, 262, 304, 306). It writes 8 or 16 bits according to the M flag, so it is a drop-in substitute that (a) saves a byte and 2 cycles, (b) preserves A.

### Fix

Add a **peephole** step (not a parser change) in [`crates/core/src/peephole.rs`](crates/core/src/peephole.rs) that recognises the emitted sequence and rewrites it. Doing it at peephole is safer than lowering because:

- Chain assignments `dst0=dst1=a=0` already correctly emit `LDA #0; STA dst1; STA dst0` — the peephole can replace each `STA mem` that is reached immediately after `LDA #0` (with no intervening A clobber) with `STZ mem`, and drop the `LDA #0` once all subsequent `STA`s that consumed it have been rewritten.
- We keep `mem=a=0` behavior intact when the user actually wants A = 0 afterwards (chain has a trailing use of A). Detect this: only drop the `LDA #0` when A is dead after the sequence.

Concrete steps:

1. Identify the stored zero by pattern-matching `lda #imm` where `imm` evaluates to 0 at compile time (constant-folded).
2. For each contiguous `sta mem` following the `lda`, if the addressing mode is one of `{DirectPage, DirectPageX, Absolute, AbsoluteX}`, replace with `stz mem`.
3. If A becomes dead (no register-read between the sequence and the next `LDA` / contract exit), remove the `LDA #0`. Otherwise keep it so the chain's tail value is preserved.

Respect M-flag: in `@a16` mode STZ writes 16 bits. The sequence `lda #0; sta word_mem` (also 16-bit in `@a16`) is safely replaced 1-for-1 — byte count and semantics both match.

### Verification

- Write a new golden fixture `tests/golden/fixtures/peephole-stz-zero/input.k65` exercising zp, zp+X, abs, abs+X, chained stores, `@a8`, `@a16`, and a case where A is read afterwards (LDA #0 must *not* be dropped).
- Run `cargo test` and `cargo insta review`.

### Files touched

- `crates/core/src/peephole.rs`
- `tests/golden/fixtures/peephole-stz-zero/` (new)

## Gap 3: [zp], [zp],Y, zp,S, (zp,S),Y in the HLA operand grammar

### Problem

Four 65816 addressing modes are in the ISA opcode table but unreachable from HLA. Users who need long-indirect or stack-relative addressing (common in 65816 native code, especially around bank crossings and reentrant subroutines) must drop to raw mnemonics.

### Fix

1. Extend `OperandAddrMode` in [`ast/mod.rs:205-210`](crates/core/src/ast/mod.rs#L205-L210):

   ```rust
   pub enum OperandAddrMode {
       Direct,
       Indirect,
       IndirectLong,            // [expr]
       IndexedIndirectX,
       IndirectIndexedY,
       IndirectLongIndexedY,    // [expr],y
       StackRelative,           // expr,s
       StackRelativeIndirectY,  // (expr,s),y
   }
   ```

2. Parser additions in [`parser/operands.rs`](crates/core/src/parser/operands.rs):

   - **`[expr]` and `[expr],Y`** — currently `[` is already a lexer token (`TokenKind::LBracket`) and the post-lex coalescing pass in `crates/core/src/lexer.rs:296-314` reserves bracketed *top-level* runs for the evaluator. In an operand position after a mnemonic, `[...]` is unambiguous because the evaluator's coalescing pass only fires at statement/item top level, not inside operand parsing. Add a `long_indirect` branch to `operand_expr_parser`:

     ```
     `[` expr `]`              → IndirectLong
     `[` expr `]` `,` `y`      → IndirectLongIndexedY
     ```

     Verify by checking that `lda [ptr]` and `lda [ptr],y` lex as `Ident Mnemonic LBracket Ident RBracket …` rather than colliding with evaluator coalescing. If the coalescer does intercept, gate it off inside operand parsing (it already uses a two-pass model, so the fix is a single branch check).

   - **`expr,S`** — add `S` to the set of accepted index-register trailers; map it to `StackRelative`. [`parser/registers.rs:40-54`](crates/core/src/parser/registers.rs#L40-L54) already returns `IndexRegister::S` — just don't reject it at the operand level as it does today.

   - **`(expr,S),Y`** — extend the parenthesised operand branch to accept `(expr,S)` followed by `,y` and map to `StackRelativeIndirectY`. Keep the existing rejections for `(expr,Y)`, `(expr),X`, `(expr),S`.

3. Lowering in [`lower.rs`](crates/core/src/lower.rs): map each new `OperandAddrMode` to the corresponding opcode lookup in the ISA crate. Identify the relevant `op!` entries — e.g. `lda [dp]` uses the DirectPageIndirectLong mode already defined in `isa65816/src/lib.rs`; just wire the address-mode selection to pick it.

4. `:far` interaction: existing docs say long-indirect should be a companion to `:far` typed variables. Document in `docs/syntax-reference.md` that `lda [ptr]` now works directly; keep the existing `:far` typed-view path unchanged.

### Which mnemonics inherit the new modes

From the ISA crate, these modes exist for (at least): LDA, STA, ADC, SBC, AND, ORA, EOR, CMP for `[zp]`, `[zp],y`, `expr,s`, `(expr,s),y`. The lowering path in `lower.rs` should fail cleanly (diagnostic, not panic) when a mnemonic is paired with an unsupported addressing mode — extend the existing addressing-mode-to-opcode resolver accordingly.

### Verification

- New golden fixtures under `tests/golden/fixtures/`:
  - `syntax-long-indirect/` — `lda [ptr]`, `sta [ptr]`, `lda [ptr],y`, `sta [ptr],y` in `@a8` and `@a16`.
  - `syntax-stack-relative/` — `lda 4,s`, `sta 4,s`, `lda (4,s),y`, `sta (4,s),y`.
- Unit tests in `parser/tests/statements.rs` asserting the AST mode for each new form, plus rejection tests for the illegal combinations (`(expr,S)` without trailing `,Y`, `(expr),S`, `[expr],X`, etc.).
- Check the emitted opcodes against the ISA table manually for one example each, then rely on golden snapshots for the rest.

### Files touched

- `crates/core/src/ast/mod.rs`
- `crates/core/src/parser/operands.rs`
- `crates/core/src/parser/registers.rs` (only if the index-register gate needs adjustment)
- `crates/core/src/lower.rs`
- `crates/core/src/sema/functions.rs` (if clobber/flow logic is mode-aware)
- `tests/golden/fixtures/syntax-long-indirect/`, `tests/golden/fixtures/syntax-stack-relative/` (new)
- `crates/core/src/parser/tests/statements.rs` (new cases)
- `docs/syntax-reference.md` (update the addressing-mode bullets once land-order allows; the sibling doc plan adds a "limitations" note that becomes stale here — replace it with the new supported forms)

## Rollout and ordering

- Land Gap 1 first behind no flag. It is a semantic-change bugfix — the current behavior is silently wrong and extremely unlikely to be relied upon deliberately. One release note line: *"`x!!`/`y!!`/`b!!`/`d!!` now emit PHX/PHY/PHB/PHD respectively; previously they emitted PHP. Use `p!!`/`flag!!` if you meant PHP."*
- Land Gap 2 second. Pure optimisation — no behavior change visible to the user beyond smaller/faster output and preserved A register after `mem=0`. Add a note that code which previously read A after `mem=0` (expecting 0) should assign `a=0` explicitly or chain `mem=a=0`.
- Land Gap 3 third. Additive — no existing valid program changes meaning.

## Verification summary

Per gap, above. Repo-wide:

1. `cargo test --workspace` — all unit tests pass.
2. `cargo insta review` — review new / changed golden snapshots.
3. `rg '(^|[^a-z])[xybd](!!|\?\?)' -g '*.k65'` — confirm zero hits in user code outside the tests we intentionally update.
4. Build one of the 65816 reset-vector examples and objdump — verify the new opcodes appear where expected:

   ```
   cargo run -p k816 -- compile tests/golden/fixtures/example-reset-vector/input.k65
   ```

5. After all three gaps land, update `docs/syntax-reference.md` in one follow-up pass to reflect the new HLA shorthand (part of the sibling doc plan).
