# Plan: Add missing 65C02 / 65816 instructions to `docs/syntax-reference.md`

## Context

`docs/syntax-reference.md` was written around the standard 6502 ISA and documents the 56 classic mnemonics (ADC…TYA). After earlier revisions it also contains a `### 65816 Transfer Instructions` section covering TCD, TCS, TDC, TSC, TXY, TYX, and XBA (7 mnemonics), plus a "Complete Register Transfer Table". The `k816` compiler's ISA crate (`crates/isa65816/src/lib.rs`) recognises **92** unique mnemonics total, so **29 instructions** remain undocumented: BRA, BRL, COP, JML, JSL, MVN, MVP, PEA, PEI, PER, PHB, PHD, PHK, PHX, PHY, PLB, PLD, PLX, PLY, REP, RTL, SEP, STP, STZ, TRB, TSB, WAI, WDM, XCE. Several are already referenced implicitly elsewhere (e.g. `far func` emits `RTL`/`JSL`, `@a16`/`@i16` synthesise `REP`/`SEP`), but there is no per-instruction reference that a reader can grep for.

Goal: extend the per-instruction reference so that every mnemonic the ISA crate accepts has a greppable `### \`MNEMONIC\`` heading. The 65816 addressing modes that the HLA operand grammar supports are already documented in the existing `### 65816 Addressing Modes in K65` subsection (added after the revalidation pass); this plan does not touch that subsection.

## What I verified in the source (revalidated)

Observations below reflect the **current** state of the codebase after the sibling plans `hla-stack-stz-addr-modes.plan.md` and `mem-zero-store-direct-stz.plan.md` landed. Two original observations ("HLA stack shorthand is A/P-only" and "HLA operand addressing modes don't expose `[zp]`/stack-relative") were invalidated by those plans and have been pruned from this list.

1. **ISA coverage** `crates/isa65816/src/lib.rs` OPCODE_TABLE contains the 36 extra mnemonics: `bra brl cop jml jsl mvn mvp pea pei per phb phd phk phx phy plb pld plx ply rep rtl sep stp stz trb tsb txy tyx wai wdm xba xce tcd tcs tdc tsc`, plus the DirectPage/DirectPageX/Absolute/AbsoluteX variants of `stz`, `tsb`, `trb`. The exhaustive `syntax-all-256-opcodes` golden fixture proves every opcode emits correctly. Note: PHK has no matching PLK (you cannot pull into the program-bank register); BBR/BBS/RMB/SMB (WDC 65C02 bit operations) are intentionally not in the K65 ISA.
2. **HLA register transfers** [`crates/core/src/parser/registers.rs:78-94`](crates/core/src/parser/registers.rs#L78-L94) maps `y=x→txy`, `x=y→tyx`, `d=c→tcd`, `s=c→tcs`, `c=d→tdc`, `c=s→tsc`. `b><a→xba` lowering unchanged.
3. **STZ synthesis** STZ is synthesised through **two** paths:
   - Direct `mem = 0` form parses to `HlaStmt::MemStoreZero` (see [`parser/operations.rs store_stmt_parser`](crates/core/src/parser/operations.rs)) and lowers to a single `stz mem` with no A-clobber. Rejected at parse time for non-zero constants and at lower time for unsupported addressing modes (fixture `syntax-mem-zero-store`).
   - Chain form `mem = a = 0` goes through the existing `LDA #0; STA mem` lowering, then [`peephole.rs stz_rewrite`](crates/core/src/peephole.rs) rewrites each supported `STA` to `STZ` and drops the leading `LDA #0` when A is dead afterwards (fixture `peephole-stz-zero`).
4. **Compiler-implicit emissions** `far func` emits `RTL` ([`lower.rs:1638`](crates/core/src/lower.rs#L1638)); `far label` as goto → `JML`; bare `far`/`call far` → `JSL`; loop `break`/`repeat` → `BRA`; `@a8/@a16/@i8/@i16` annotations → `REP`/`SEP`.
5. **Block-move syntax** Two-operand form `mvn src,dst` / `mvp src,dst` is parsed in `stmt.rs` via the `BlockMove` trailer. Used in `syntax-native-native` and `syntax-all-256-opcodes`.
6. **`far` operand prefix** `stmt.rs` accepts `far` before an operand expression, producing `AbsoluteLong` encoding.

## Edits to `/home/smoku/devel/X65/k816/docs/syntax-reference.md`

### (a) New bulk section: "65C02 / 65816 Instruction Extensions"

Insert a single new top-level section immediately **after** the last existing instruction entry in the per-instruction reference (grep for `^### \`TYA\`` and place the new `## 65C02 / 65816 Instruction Extensions` heading below that entry's trailing `---` separator). The doc grows by extending the reference rather than interleaving new mnemonics between old ones.

The new section has four subsections. Each instruction follows the same format used today (title with mnemonic + short name, HLA-syntax table when applicable, followed by the addressing-mode listing). For instructions that are compiler-implicit, note that explicitly instead of inventing HLA syntax.

**Subsection 1 — 65C02 additions**

Document (in this order, each as its own `### …` block):

- `BRA` branch always — raw mnemonic `bra label` (Relative8). Note it is also what the compiler emits for unconditional `break`/`repeat` in loop constructs.
- `PHX` push X — HLA `x!!` or raw mnemonic `phx` (Implied). Width follows `@i8/@i16`.
- `PHY` push Y — HLA `y!!` or raw mnemonic `phy` (Implied).
- `PLX` pull X — HLA `x??` or raw mnemonic `plx` (Implied).
- `PLY` pull Y — HLA `y??` or raw mnemonic `ply` (Implied).
- `STZ` store zero — raw mnemonic, addressing `stz zp`, `stz zp,x`, `stz abs`, `stz abs,x`. Cross-reference the existing "Zero-Store Shortcut" section: `mem = 0` lowers directly to STZ, and `mem = a = 0` chains get STZ via peephole rewrite.
- `TSB` test and set bits — raw mnemonic `tsb zp` / `tsb abs`.
- `TRB` test and reset bits — raw mnemonic `trb zp` / `trb abs`.
- `WAI` wait for interrupt — raw mnemonic `wai` (Implied).
- `STP` stop — raw mnemonic `stp` (Implied).

**Subsection 2 — 65816 bank-register push/pull and mode switch**

`TCD`, `TCS`, `TDC`, `TSC`, `TXY`, `TYX`, and `XBA` already have dedicated entries under the existing `### 65816 Transfer Instructions` section — **do not duplicate**. New entries to add in this subsection:

- `PHB` / `PLB` — HLA `b!!` / `b??` or raw mnemonic (Implied). Cross-reference the `example-reset-vector` fixture that uses `plb`.
- `PHD` / `PLD` — HLA `d!!` / `d??` or raw mnemonic (Implied).
- `PHK` — raw mnemonic only (Implied). No PLK pair exists on the 65816, so there is no HLA `k!!` / `k??` shorthand.
- `XCE` exchange carry and emulation — raw mnemonic `xce` (Implied). Cross-reference the reset-vector example in fixtures.

**Subsection 3 — 65816 control flow, mode and stack**

- `BRL` branch long — raw mnemonic `brl label` (Relative16).
- `JML` jump long — raw mnemonic `jml addr` (AbsoluteLong). Note: `far goto label` lowers to `JML`.
- `JSL` jump subroutine long — raw mnemonic `jsl addr` (AbsoluteLong). Cross-reference the existing "Far Calls" section: `far func_name` and `call far name` lower to `JSL`.
- `RTL` return long — raw mnemonic `rtl` (Implied). Emitted automatically at the end of a `far func`; also usable directly in `naked far` blocks (fixture `syntax-data-fars`).
- `REP` reset P bits — raw mnemonic `rep #mask` (Immediate8). Note that `@a16`/`@i16` annotations synthesise `REP #$20` / `REP #$10`.
- `SEP` set P bits — raw mnemonic `sep #mask` (Immediate8). `@a8`/`@i8` synthesise `SEP #$20` / `SEP #$10`.
- `PEA` push effective absolute — raw mnemonic `pea #addr` (Immediate16).
- `PEI` push effective indirect — raw mnemonic `pei (zp)` (DirectPageIndirect).
- `PER` push effective PC-relative — raw mnemonic `per label` (Relative16).
- `COP` coprocessor trap — raw mnemonic `cop #imm` (Immediate8).
- `WDM` reserved — raw mnemonic `wdm #imm` (Immediate8). Note "reserved by WDC, no-op on current cores".

**Subsection 4 — 65816 block moves**

- `MVN` block move negative — `mvn src_bank, dst_bank` (BlockMove). The compiler parses two comma-separated operands; fixture `syntax-native-native` shows `mvn 0,0`.
- `MVP` block move positive — `mvp src_bank, dst_bank` (BlockMove).

### (b) Address-mode documentation — already done

The "65816 Addressing Modes in K65" subsection (anchor: `### 65816 Addressing Modes in K65`, follows the "Address Modes" footnotes) already documents the new modes after the revalidation pass: `expr,S`, `(expr,S),Y`, `far expr`, `[expr]`, `[expr],Y`, plus the raw-mnemonic bare-number-literal caveat. **No edits to this subsection are needed.** The original plan draft predicted these modes would remain unsupported — in practice they were implemented in `hla-stack-stz-addr-modes`, and the subsection reflects that.

### (c) Small touch-ups in already-existing sections

- In the `### Registers` section (grep for `^### Registers` within the instruction reference), add a bullet noting that `b><a` is the HLA shorthand for `XBA`; we already mention the swap but don't tie it to the mnemonic name, which breaks grepping.
- In the `### Far Calls` section (grep for `^### Far Calls`), add one sentence stating that the emitted opcodes are `JSL` / `RTL` / `JML` (currently the doc describes behaviour but never names the mnemonics).
- In the `### \`func\`` section (grep for "Function headers can also carry an optional call contract"), cross-link `@a8`/`@a16`/`@i8`/`@i16` to the new REP/SEP entries in subsection 3.

No other sections need edits.

## Files touched

- `/home/smoku/devel/X65/k816/docs/syntax-reference.md` — the only file modified.

## What to reuse (do not invent syntax)

- Register transfer HLA forms: [`crates/core/src/parser/registers.rs:78-94`](crates/core/src/parser/registers.rs#L78-L94) `resolve_transfer` is the authority — document exactly what it accepts.
- Stack shorthand `!!`/`??` mapping: [`crates/core/src/parser/registers.rs:18-47`](crates/core/src/parser/registers.rs#L18-L47) `parse_stack_target` is the authority. `a!!`→PHA, `x!!`→PHX, `y!!`→PHY, `b!!`→PHB, `d!!`→PHD; pull forms use `??`. Flag aliases (`p`, `flag`, `n`, `v`, `o`, `m`, `i`, `z`, `c`) collapse to PHP/PLP. PHK/PLK have no HLA shorthand.
- Block-move two-operand syntax: `crates/core/src/parser/stmt.rs` — grep for `CommaTrailer::BlockMoveDst`.
- Examples of real usage for the new entries (reference them or lift short snippets):
  - `tests/golden/fixtures/syntax-flag-and-stack-shorthand/input.k65` → HLA `x!!`/`y!!`/`b!!`/`d!!` plus flag-alias fallbacks
  - `tests/golden/fixtures/syntax-mode-push-pull-width/input.k65` → raw `phx`/`plx`/`phy`/`ply`
  - `tests/golden/fixtures/syntax-mode-store-acc-width/input.k65` → STZ/TRB/TSB
  - `tests/golden/fixtures/syntax-mem-zero-store/input.k65` → `mem = 0` → STZ shortcut
  - `tests/golden/fixtures/peephole-stz-zero/input.k65` → chain form `mem = a = 0` peephole rewrite
  - `tests/golden/fixtures/example-reset-vector/input.k65` → XCE/PLB
  - `tests/golden/fixtures/link-multi-func/input.20-func.k65` → XBA
  - `tests/golden/fixtures/syntax-data-fars/input.k65` → RTL
  - `tests/golden/fixtures/syntax-native-native/input.k65` → MVN/MVP
  - `tests/golden/fixtures/syntax-all-256-opcodes/input.k65` → every opcode + addressing mode in 8-bit mode
  - `tests/golden/fixtures/syntax-all-256-opcodes-16bit/input.k65` → same in 16-bit native mode

## Verification

1. Run `cargo test -p k816` (or whichever crate has the doc/golden tests) to confirm nothing else consumes `docs/syntax-reference.md` programmatically. A grep of the repo for the filename (there's a Markdown link to it from `README.md` only) shows no test checks its contents.
2. Eyeball the rendered Markdown:
   - `mdcat docs/syntax-reference.md | less` or open in VS Code preview.
   - Confirm the new `### …` headings follow the existing tone/format (same table layout, same "N Z C I D V" block style for raw-mnemonic entries).
3. Sanity-check: diff the mnemonic names in `OPCODE_TABLE` against the per-instruction headings in the doc. After implementing, every one of the 92 unique mnemonics (56 classic 6502 + 7 existing 65816 transfers + 29 added by this plan) should have a greppable `^###[#]* \`MNEMONIC\`` heading. Inline one-liner:

   ```sh
   comm -23 \
     <(grep -oP 'op!\("\K[^"]+' crates/isa65816/src/lib.rs | tr '[:lower:]' '[:upper:]' | sort -u) \
     <(grep -oP "^###[#]* \`\K[A-Z]{3,4}" docs/syntax-reference.md | sort -u)
   ```

   Empty output = every mnemonic documented.
4. For each new mnemonic, confirm the claimed addressing modes match the `OPCODE_TABLE` entries in `crates/isa65816/src/lib.rs`. (There are 4 STZ opcodes, 2 each for TSB/TRB; the others are Implied / Immediate / Relative.)
5. Build one of the fixture programs that exercises the new mnemonics and confirm the doc's examples compile: `cargo run -p k816 -- compile tests/golden/fixtures/syntax-mode-store-acc-width/input.k65` should still succeed (read-only verification that the doc examples are realistic).
