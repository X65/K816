# Grammar Alignment Plan: compiler.inc vs Rust Compiler

## Context

The file `vendor/src/compiler.inc` defines the grammar for the original C++ K65 compiler. The Rust rewrite in `crates/core/` has diverged from this grammar — adding 65816-specific features (mode tracking, REP/SEP), redesigning syntax (modifiers instead of separate section types), and omitting some original features.

This plan defines the implementation work for aligning the Rust compiler to the grammar.

**Decisions:**

- `segment` replaces `bank` — intentional, no backward compatibility alias
- Preprocessor (#if/#elif/#else/#endif) — skipped, not implementing
- `evalfunc` — skipped, not implementing
- Illegal 6502 opcodes (ANC/DCP/ISC/SAX/AXS) — skipped, we only support 65c816
- Image/tile processing — skipped, not extending yet
- PHP/PLP via `<flag>!!`/`<flag>??` — already implemented (`parser/mod.rs:2282-2305`)

---

## 1. IMPLEMENTATION TASKS

### 1.1 Add `O`/`o` as overflow flag alias

**Grammar lines 259-260**: `Flag -> O → "V"`, `Flag -> o → "v"`.
**Current**: Only `v+?`/`v-?` work for overflow. `O`/`o` not recognized as flag letter.
**Change**: In flag parsing, accept `O`/`o` and map to `V`/`v`.
**Files**: `crates/core/src/parser/mod.rs`

### 1.2 Implement `far goto`

**Grammar line 156**: `far goto Arg` emits a far jump (JML).
**Current**: `far call` is supported, but `far goto` is not.
**Change**: Parse `far goto <expr>` → emit JML instruction. Add to flow statement parser.
**Files**: `crates/core/src/parser/mod.rs`, `crates/core/src/ast/mod.rs`

### 1.3 Add `never {` block opener

**Grammar line 200**: `never {` emits JMP over the block body — code assembled but never executed.
**Current**: `} never` (close without branch) is supported, but `never {` (skip-over opener) is not.
**Change**: Parse as HLA construct that emits JMP over block body. Add `HlaStmt::NeverBlock` variant.
**Files**: `crates/core/src/parser/mod.rs`, `crates/core/src/ast/mod.rs`, `crates/core/src/lower.rs`

### 1.4 Add `align <expr> + <expr>` offset form

**Grammar line 135**: `align Number + Number` — align to boundary with offset.
**Current**: Only `align <expr>` (no offset form).
**Change**: Extend align parsing to accept `+ offset` suffix. Carry offset through to HIR.
**Files**: `crates/core/src/parser/mod.rs`, `crates/core/src/ast/mod.rs`, `crates/core/src/hir/mod.rs`, `crates/core/src/lower.rs`

### 1.5 Add `?` undefined byte in data blocks

**Grammar line 74**: `?` emits `.undef` opcode — a placeholder/undefined byte.
**Current**: Not supported.
**Change**: Parse `?` in data context → emit a zero byte or reserved marker.
**Files**: `crates/core/src/parser/mod.rs`, `crates/core/src/data_blocks.rs`

### 1.6 Extend chained assignments (MovSeq)

**Grammar lines 183, 190-191**: `Arg = Arg = Arg = ...` chains with right-to-left evaluation.
**Current**: Only simple two-operand assignments. Transfer chains exist but are register-to-register only.
**Change**: Parse `a = b = c = d` chains. Lower to sequence of load/store/transfer instructions via `opmovseq()` equivalent.
**Files**: `crates/core/src/parser/mod.rs`, `crates/core/src/ast/mod.rs`, `crates/core/src/lower.rs`

### 1.7 Data section `repeat` construct

**Grammar line 75**: `repeat Number { DataList }` — repeats data block N times.
**Current**: `repeat` exists only in code blocks (HLA loop repeat), not in data blocks.
**Change**: Parse `repeat <N> { <data> }` in data context. Lower by repeating the data block N times.
**Files**: `crates/core/src/parser/mod.rs`, `crates/core/src/ast/mod.rs`, `crates/core/src/data_blocks.rs`

### 1.8 Data section `code` blocks

**Grammar lines 76-77**: `code { Sequence }` and `NoCross code { Sequence }` — embed code instructions within data sections.
**Current**: Not supported. Data blocks only contain data commands.
**Change**: Parse `code { <instructions> }` inside data sections. Lower by emitting instructions inline within data output.
**Files**: `crates/core/src/parser/mod.rs`, `crates/core/src/ast/mod.rs`, `crates/core/src/data_blocks.rs`, `crates/core/src/lower.rs`

### 1.9 Labels inside data sections

**Grammar line 96**: `Ident :` — labels within data sections for address references.
**Current**: Not supported in data blocks.
**Change**: Parse `name:` labels within data blocks. Emit as Label ops in data context.
**Files**: `crates/core/src/parser/mod.rs`, `crates/core/src/data_blocks.rs`

### 1.10 Data section `evaluator` directive

**Grammar line 93**: `evaluator [ EvalCode ]` — sets an evaluator for subsequent data processing.
**Current**: Not supported as a data-block command.
**Change**: Parse `evaluator [ code ]` in data context. Precompile and attach evaluator to data processing.
**Files**: `crates/core/src/parser/mod.rs`, `crates/core/src/ast/mod.rs`, `crates/core/src/data_blocks.rs`

---

## 2. REFERENCE: Intentional Divergences (Rust-only features to keep)

These are documented as intentional improvements over the original grammar:

- 65816 mode tracking system (@a8/@a16/@i8/@i16)
- Mode-scoped blocks and contracts
- `a >< b` swap (XBA)
- Variable type annotations and symbolic subscripts
- Character literals
- `$FF` and `%1010` number notations
- Wait loop construct
- Transfer chains with mode awareness
- Named data blocks
- Unified function modifier syntax (vs separate section types)
- `segment` replaces `bank` (no alias)

## 3. REFERENCE: Skipped Grammar Features

Not implementing:

- Preprocessor conditional compilation (#if/#elif/#else/#endif, #error, #warn)
- `evalfunc` user-defined evaluator functions
- Illegal 6502 opcodes (ANC, DCP, ISC, SAX, AXS)
- Image/tile processing (positioning, tiling, direction, wave, colormode)

## 4. REFERENCE: Already Matching Features

These grammar features are already fully supported:

- PHP/PLP via `<flag>!!`/`<flag>??` syntax (parser/mod.rs:2282-2305)
- All standard 65816 instructions and addressing modes
- ArithOp (+/&/^/-/|) for A register → ADC/AND/EOR/SBC/ORA
- BIT instruction (`a & ?<operand>`)
- Compare on X/Y (`x ? <operand>` / `y ? <operand>`)
- PostfixOp (<</>>/<<</***/++/--) → ASL/LSR/ROL/ROR/INC/DEC
- All BranchOp conditions (symbolic and flag-based)
- Flag set/clear (c+/c-/d+/d-/i+/i-)
- A!!/A?? (PHA/PLA)
- `return` (RTS), `return_i` (RTI)
- `goto`/`goto (indirect)`/conditional `goto`
- `call`/`far call`
- Labels, `break`, `repeat` in code
- `{ } <condition>` do-while loops with all close variants (always/never/flag/op)
- If-else HLA blocks
- Data blocks with bytes, strings, align, address, nocross, for-eval
- Constants, variables, evaluator blocks
- Nop padding (`*`/`* N`/`%`)

---

## 5. VERIFICATION

- Run existing test suite after each task: `cargo test` in workspace root
- For each new feature, add parser round-trip tests in `crates/core/src/parser/` test module
- Test with existing K65 source files from `vendor/` to verify compatibility
- Check `crates/fmt/src/print_ast.rs` — formatter must be updated for any new AST nodes
- Check `crates/lsp/src/lib.rs` — LSP server may need updates for new constructs
