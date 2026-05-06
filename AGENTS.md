# Project Guidelines

## Project Identity

k816 is a Rust implementation of a K65-style high-level assembler for the WDC 65816 microprocessor. The syntax reference is cross-checked against the reference compiler grammar in `vendor/`. Treat `vendor/` as a behavior and syntax reference, not as a runtime dependency of the Rust workspace.

## Code Style

Follow standard Rust conventions with `rustfmt` for formatting and `cargo clippy` for style guidance. Apply clippy suggestions to new and modified code. Example: [crates/core/src/lib.rs](crates/core/src/lib.rs) uses clean module declarations and public exports.

## Reuse First

- Default behavior: search the existing codebase for matching patterns, utilities, and functions before introducing new ones.
- Prefer extending or reusing existing implementations when behavior aligns, instead of creating parallel code paths.
- When touching related code, refactor opportunistically to reduce duplication if it can be done safely and clearly.

## Architecture

Multi-crate workspace for separation of concerns: `core` handles lexing/parsing/AST/HIR/semantics/lowering/encoding/emit (depends on `eval` and `assets`); `eval` for compile-time expression evaluation; `fmt` for pretty-printing; `assets` for data converters; `isa65816` for instruction definitions and encoding/decoding tables; `o65` for object format; `link` for RON-based linking and layout; `lsp` for editor analysis/protocol support. Design emphasizes high-level assembly with C-like syntax, compile-time evaluation, structured blocks, deterministic output, explicit far functions, project-aware builds, and 24-bit addressing. Reference: [Cargo.toml](Cargo.toml).

Repository Layout:

- `vendor/` contains the source code of a reference K65 compiler written in C. It is kept for reference purposes only and has no bearing on the Rust implementation.
- `crates/` contains the Rust workspace crates listed above.
- `docs/` contains documentation for k816's syntax and linker configuration.
- `docs/misc/w65c816s.md` is the WDC W65C816S datasheet — the canonical reference for CPU behavior (instruction semantics, addressing modes, status flags, cycle counts, native/emulation mode, MX/E flag effects). Consult this file when researching 65816 processor behavior or verifying ISA details before changing instruction encoding, mode handling, or hover metadata.
- `examples/` contains example K65 programs.
- `tests/golden/` contains golden test fixtures for regression testing.

## Methodology

The compiler accepts two source styles that can be mixed in a single file:

- the native 65816 mnemonic syntax (`lda`, `inx`, `pha`, ...)
- the high-level K65-style syntax (`a = ...`, `x++`, `a!!`, ...)

Equivalent native/HLA constructs produce equivalent machine output, but they stay distinct in AST:

- native mnemonics are represented as `Stmt::Instruction`
- HLA sugar is represented as `Stmt::Hla(...)` variants (register assignments/transfers, stack shorthand, flow shorthand, one-letter operators, nop shorthand, chains, etc.)

Convergence happens in lowering (AST -> HIR), not in parsing.

Current frontend/backend flow:

Core compile pipeline in `k816_core` (see `crates/core/src/driver.rs`):

1. Parse with warnings (`parse_with_warnings`) into AST.
2. Expand evaluator-backed expression fragments (`eval_expand::expand_file`).
3. Normalize HLA constructs into canonical HLA AST forms (`normalize_hla::normalize_file`).
4. Run semantic analysis (`sema::analyze` / `sema::analyze_with_external_consts`) to build validated symbol/function/const/var metadata.
5. Lower AST + semantic model into operation-level HIR (`lower::lower`), translating HLA nodes and native instructions through the same instruction/mode validation path.
6. Optimize mode ops with `eliminate_dead_mode_ops` and `fold_mode_ops`.
7. Run peephole cleanup (`peephole_optimize`) on the HIR stream.
8. Emit relocatable object/chunk metadata via `emit_object`.

Link/render orchestration (CLI, golden harness, and project build flows using `k816_link`):

1. Link object(s) into canonical placed layout (symbols/placements/listing), format-agnostic.
2. Render final container format (`raw`/`xex`) only at output write time.

`k816_core` now exposes both single-source compilation and multi-source compilation APIs (`compile_sources`, `compile_sources_all_or_nothing`). Project builds, the golden harness, and LSP workspace analysis all use the multi-source path so constants can be shared across compilation units before link time.

- AST is syntax-oriented: spans, comments, numeric literal formats, and source-level constructs for diagnostics/formatting are preserved.
- Semantic analysis resolves symbols, const values, variable layouts, and function mode contracts before lowering.
- HIR (`hir::Program`) is operation-oriented: segments, labels, instructions, emitted bytes, mode ops, and relocatable byte fixups.
- Width/mode-sensitive diagnostics (typed loads/stores, `:far` direct access rejection, etc.) are enforced during lowering for both native and HLA forms.
- Core compile backend is single-path object/chunk emission (`emit_object`-style), regardless of whether the caller compiled one source or many.
- Linker output is layout-first; `raw`/`xex` are render targets selected at final write time, not compile backends.
- Emission handles instruction selection/encoding and fixup materialization; linker placement and symbol resolution are handled in the `link` crate from RON config.
- Structured top-level blocks (`func`, `data`, named data blocks) remain the units used by lowering and downstream placement/link flows.

## Build and Test

- Build: `cargo build`
- Test: `cargo test`
- Lint: `cargo clippy --all-targets --all-features`
- Golden tests: `cargo test -p k816-golden-tests` for reproducible output validation against fixtures. Uses Rust edition 2024 with dependencies like `chumsky` for parsing, `clap` for CLI. Reference: [Cargo.toml](Cargo.toml).
- Golden fixture binaries: Use [tests/golden/link.stub.ld.ron](tests/golden/link.stub.ld.ron) as the linker config (`-T` flag) to generate `expected.bin` files in raw binary format for golden test fixtures.
- Always create `expected.lst` alongside every `expected.bin` so the listing is regression-checked together with the binary. The bless flow only refreshes `expected.lst` when the file already exists, so `touch expected.lst` in the fixture directory before blessing a new success fixture.
- CLI/integration coverage lives in `tests/cli.rs` and `tests/lsp_cli.rs`; these cover project init/build/run/clean flows plus LSP startup and editor features.
- Golden bless through `cargo test` (feature-gated):
  - Regenerate all golden outputs: `cargo test -p k816-golden-tests --features golden-bless`
  - Regenerate selected cases via test filter: `cargo test -p k816-golden-tests --features golden-bless syntax_doc_example_dli`
  - Validate after blessing: `cargo test -p k816-golden-tests`
- Golden bless CLI (secondary, script-friendly):
  - Regenerate all golden outputs: `cargo run -p k816-golden-tests --bin bless --`
  - Regenerate only error fixtures (`expected.err`): `cargo run -p k816-golden-tests --bin bless -- --err-only`
  - Regenerate a single fixture: `cargo run -p k816-golden-tests --bin bless -- --case fixture:<name>`
  - List discoverable cases: `cargo run -p k816-golden-tests --bin bless -- --list`

## Testing Approach

- Follow Test-Driven Development (TDD): write or update tests first, then implement behavior to satisfy them.
- Ensure implementations are thoroughly testable through `tests/golden/` fixtures.
- Cover positive desired paths, expected and handled errors, and edge cases in golden fixtures.
- Golden fixtures may use either a single `input.k65` / `input.k816` file or multi-input naming such as `input.10-main.k65`, `input.20-const.k65`; do not mix the single-input and multi-input forms in one fixture.

## Project Conventions

- **When in doubt about 65816 ISA semantics — instruction behavior, addressing modes, opcode tables, status-flag effects, cycle counts, MX/E mode interactions, reset state, or any CPU-level quirk — consult [docs/misc/w65c816s.md](docs/misc/w65c816s.md) (the bundled WDC W65C816S datasheet) before relying on memory or assumption.** Cite the relevant section or table (e.g. "Table 5-5", "Note 2", "§7.10 Switching Modes") in code comments, diagnostic notes, or PR descriptions when the datasheet's specific wording is what justifies a non-obvious choice. Do not invent CPU semantics.
- No "compat", "legacy", or "deprecated" shims; features are first-class or removed.
- `segment` directive only for output segments; no `bank` alias.
- Error on unknown directives; no silent ignores.
- Syntax: C-style comments, variables declared with `var NAME` (DP/ABS/FAR storage class prefixes optional), constants declared with `const NAME = expr` (no brackets, no memory location), labels (global `label:`, local `.label:`), numeric literals (decimal, hex `$` or `0x`, binary `%` or `0b`). Example: [examples/hello_uart.k65](examples/hello_uart.k65) and [docs/syntax-reference.md](docs/syntax-reference.md).
- `[expr]` is a compile-time eval fragment: a post-lex pass in [crates/core/src/parser/preprocess.rs](crates/core/src/parser/preprocess.rs) coalesces the bracketed text into a single `TokenKind::Eval(String)`, which `expr_parser` and the data-block `eval_bytes_entry` re-parse via the evaluator. Two contexts keep brackets raw (`LBracket`/`RBracket`) instead: symbolic-subscript field declarations after `var` (e.g. `var FOO[i]:byte = ...`), and long/24-bit indirect operand brackets immediately after a CPU mnemonic (e.g. `lda [addr]`). The `is_var_bracket` / `is_operand_bracket` predicates in `preprocess.rs` are the source of truth for which context applies.
- `const` is for compile-time *values* (immediates, expressions, indices, `var` initializers). It does **not** name a memory location. In native instruction-operand position, bare consts and const-rooted expressions are illegal — the user must write `#NAME` for an immediate, or declare a `var` for memory access. Native `Operand::Value` (parsed from `lda EXPR` without `#`) is always address-mode; the only exception is the `&&label` / `&&&label` address-of shapes, which produce immediate relocations by design (see `is_address_of_expr` and `value_operand_uses_immediate` in [crates/core/src/lower.rs](crates/core/src/lower.rs)). The address-operand path (`lower_address_operand` → `eval_address_expr`) tags expression provenance as `Address` (label/var-rooted), `PureLiteral` (only number literals), or `ConstTainted` (any const reference, no label/var); the first two are accepted, the third is rejected with a diagnostic. HLA shorthand (`a = NAME`) keeps its auto-determine semantics via `Operand::Auto`/`is_immediate_expression`. Preserve this separation when extending operand evaluation: do not reintroduce const-aware auto-routing in native `Operand::Value` lowering.
- Stack-relative addressing modes (`,s` and `(byte,s),y`) **invert** the address-vs-const rule because the operand is a byte offset from SP, not a memory address. Numeric literals and consts (and const-tainted expressions) are valid offsets; vars, labels, functions, and address-rooted expressions are rejected with an "address used as stack offset" diagnostic. The mode-aware branches live in `resolve_operand_ident` and `lower_address_operand` (gated by `is_stack_relative_mode` in [crates/core/src/lower.rs](crates/core/src/lower.rs)). Keep this asymmetry intact: extending stack-relative semantics should not let a `var`/label slip through as an offset.
- Project-mode CLI commands use `k816.toml` at the project root, compile `src/**/*.k65` into `target/obj/**/*.o65`, link to `target/<package>.{xex|bin}`, and optionally run the artifact via `[run].runner`.
- LSP workspace discovery is project-aware: when `k816.toml` exists, it indexes `.k65` sources under both `src/` and `tests/`.

## Integration Points

Linker uses RON format for configuration (`.ld.ron` files and project `link.ron`). `vendor/` contains the reference C compiler used for syntax/grammar cross-checking only; there is no Rust build or runtime integration with that code. Reference: [docs/linker-ron-format.md](docs/linker-ron-format.md).

## Diagnostics Style

**Rich diagnostics are first-class output.** Diagnostic quality is part of the compiler's contract, on equal footing with generating correct assembly. A diagnostic that reports only *what* is wrong without telling the user *what to do* or *why the language behaves that way* is incomplete work — the same way a half-encoded instruction is incomplete work. Hold new code to this bar; raise existing code when you touch it. The exemplar lives in [crates/core/src/emit_object.rs](crates/core/src/emit_object.rs) (`build_encode_diagnostic`, `enrich_invalid_indirect`) and the rendered output in [tests/golden/fixtures/emit-err-indirect-out-of-range-literal/expected.err](tests/golden/fixtures/emit-err-indirect-out-of-range-literal/expected.err).

Builder API in [crates/core/src/diag.rs](crates/core/src/diag.rs): `Diagnostic::error(span, msg)` then `.with_primary_label(...)`, `.with_help(...)`, `.with_note(...)`, `.with_label(other_span, ...)`, `.with_optional_help(...)`. The LSP layer reads `Supplemental::Help` separately, so any enrichment you add flows to editors as well as CLI.

Concrete rules:

- **Keep primary messages concise and focused on what is wrong.** Do not fold remediation into the message.
- **Remediation goes in `with_help(...)`. This is mandatory** — never fold "how to fix" wording into the primary `message`, even when it seems shorter or more convenient. Ariadne renders the help slot as its own labelled `Help:` line; the LSP surfaces it separately.
- **Never ship the default `"here"` primary label.** The primary label defaults to `"here"` in [crates/core/src/diag.rs](crates/core/src/diag.rs); always call `.with_primary_label(...)` with a phrase that *names what kind of thing* the span points at — `"indirect operand"`, `"forced direct-page operand"`, `"call site"`, `"missing initializer"`, `"clobbered register"`. The label should still make sense if the surrounding error text is hidden.
- **Use `with_note(...)` for architectural / language quirks.** When the user will hit the same wall again unless they understand *why* the language behaves a certain way, add a note. Good notes explain W65C816 quirks (`lda` has no `(abs)` form), the const-vs-address split, the stack-relative inversion, MX-flag-driven encoding, etc. Bad notes restate the message.
- **Use secondary `with_label(other_span, ...)` to point at the *other* end of a relationship.** Duplicate definitions, contract vs call site, fixed-chunk overlaps, live-after-clobber — the user should see both endpoints in one Ariadne report. The duplicate-symbol path in `link_diag` is the model.
- **Build help text from real values, not generic phrasing.** When a value, range, or symbol is known at diagnostic time, inline it: `value $1234 exceeds the direct-page range (0x00..=0xFF)` beats `value out of range`. The `DirectPageOutOfRange` arm in [crates/core/src/emit_object.rs](crates/core/src/emit_object.rs) shows the pattern: pull the literal/symbol off the operand and embed it.
- **For contract/call diagnostics, show the exact call form.** Use `FunctionMeta::signature_call_form(name)` from `crates/core/src/sema/model.rs` to produce forms like `sub(a, #b) -> a`, `dispatch`, or `produce() -> a` rather than vague directives ("match the declaration", "repeat the inputs").
- **When raising existing diagnostics, refresh the golden fixture.** Run `cargo run -p k816-golden-tests --bin bless -- --err-only` to regenerate `expected.err` files, then `cargo test -p k816-golden-tests` to validate. Read each regenerated `expected.err` by eye — the diff *is* the user-visible change you are shipping; lint cannot judge whether help text is helpful.

### Enriching chumsky-`Rich` parser errors

Two channels exist in [crates/core/src/parser/diagnostics.rs](crates/core/src/parser/diagnostics.rs). They are **not** parallel options — one is the preferred path, the other is a fallback that should shrink over time.

**Path 1 (preferred): tag at the emit site.** When the K816 parser code itself constructs the error via `Rich::custom(...)`, append inline tags to the message — `; label: ...`, `; hint: ...`, `; note: ...`. Order does not matter; the adapter's `parse_rich_custom` peels them off and routes them to `with_primary_label`/`with_help`/`with_note`. The local code knows exactly what construct it was trying to parse and what the user wrote, so the resulting label/help/note are precise. Examples: the `unexpected '{token}'` recovery in `crates/core/src/parser/control.rs`, the `data` block ident recovery in `crates/core/src/parser/data.rs`, the const-missing-initializer error in `crates/core/src/parser/data/consts.rs`.

**Path 2 (fallback, treat as technical debt): context detection in the adapter.** When the error comes from chumsky's automatic `ExpectedFound` machinery — i.e. a `just(...)`/`or(...)`/`then(...)` combinator deep in the grammar simply doesn't match and chumsky synthesises an "expected X, found Y" message — the parser code never gets a chance to attach tags. The only intercept point is the adapter, after the fact, where we have the source text and can re-detect the situation by inspecting bytes around `error_offset`. Add a function that returns `Option<ContextEnrichment>` and wire it into the `or_else` chain in `rich_error_to_diagnostic`. Templates: `detect_double_colon_typo`, `detect_abs_suffix_typo`, `detect_symbolic_subscript_context`. Detectors must be conservative — a false negative just falls back to the un-enriched message, but a false positive attaches misleading help to an unrelated diagnostic.

**The direction of travel is Path 2 → Path 1.** Path 2 exists because chumsky has many fallible combinators and we have not (yet) wrapped each one with a `try_map`/`recover_with` that emits a tagged `Rich::custom`. As you touch parser code, prefer to migrate sites: replace a chumsky combinator chain that bubbles its failure up to an `ExpectedFound` with a `validate`/`try_map` recovery that emits a tagged custom error directly. Each migration shrinks the surface that Path 2 is responsible for. Do not add new context detectors when the matching emit site is realistically reachable — that bakes in the workaround.

Token-kind-specific enrichment for unexpected tokens (e.g. stray `}`, operator at start-of-statement) lives in `unexpected_token_enrichment` — extend it for new token-kind cases instead of building parallel detectors.

## Recent Implementation Notes

- Multi-source compilation and shared external constants were implemented.
- Implementation path:
  - Driver APIs: `crates/core/src/driver.rs`, `crates/core/src/lib.rs`
  - Partial/external-const semantic analysis: `crates/core/src/sema/analysis.rs`, `crates/core/src/sema/mod.rs`, `crates/core/src/sema/consts.rs`
  - CLI project build flow: `src/main.rs`
  - Golden harness multi-input compilation: `tests/golden/src/harness.rs`
  - LSP workspace-wide compilation: `crates/lsp/src/workspace.rs`, `crates/lsp/src/analysis.rs`
- Semantics:
  - `compile_sources` returns per-file results so callers like the LSP can keep diagnostics isolated per document.
  - `compile_sources_all_or_nothing` is used by project builds and the golden harness when a single failure should stop the whole pipeline.
  - Cross-unit constants are collected to a fixed point before full semantic analysis so later files can depend on earlier constant definitions.
- Constraints:
  - Shared external const propagation is for compile-time constants only; symbol resolution for code/data labels still happens in the linker.
  - Project builds discover `.k65` files recursively under `src/`; the LSP additionally discovers `.k65` files under `tests/`.

- Linker duplicate-symbol diagnostics were improved for multi-object layouts.
- Implementation path:
  - Duplicate-symbol detection and anchor lookup: `crates/link/src/layout.rs`, `crates/link/src/layout/anchors.rs`
  - Regression fixtures: `tests/golden/fixtures/link-multi-const-err/`, `tests/golden/fixtures/link-multi-func-err/`
- Semantics:
  - Duplicate global symbol failures now render both the duplicate definition and the original definition when source locations are available.
  - Anchor rendering is source-aware across multiple input objects, which makes multi-file linker errors much easier to diagnose.

- Stack-relative addressing support was implemented for native instructions.
- Implementation path:
  - Parser/register handling: `crates/core/src/parser/operands.rs`, `crates/core/src/parser/registers.rs`
  - AST/HIR/object lowering: `crates/core/src/ast/mod.rs`, `crates/core/src/lower.rs`, `crates/core/src/hir/mod.rs`, `crates/core/src/emit_object.rs`
  - ISA tables and formatting: `crates/isa65816/src/lib.rs`, `crates/fmt/src/print_ir.rs`, `crates/lsp/src/hover.rs`
  - Golden coverage: `tests/golden/fixtures/syntax-native-native/`
- Semantics:
  - `,s` and `(sr,s),y` addressing forms now map through the normal operand validation and encoding path.
  - Stack-relative index handling is carried through parsing, lowering, object emission, formatter output, and hover text.

- Direct-page auto-shrink with `:abs` overrides was implemented for address operands.
- Implementation path:
  - Syntax/reference docs: `docs/syntax-reference.md`
  - AST additions for declaration/use-site address hints: `crates/core/src/ast/mod.rs`
  - Parser support for `var name:abs` and `expr:...:abs`: `crates/core/src/parser/data.rs`, `crates/core/src/parser/data/vars.rs`, `crates/core/src/parser/expr.rs`
  - Semantic propagation of var-level defaults: `crates/core/src/sema/model.rs`, `crates/core/src/sema/vars.rs`
  - Lowering/immediate classification updates so `:abs` keeps operands address-based: `crates/core/src/lower.rs`
  - HIR/object/encoder address-size hint flow: `crates/core/src/hir/mod.rs`, `crates/core/src/emit_object.rs`, `crates/isa65816/src/lib.rs`
  - Pretty-printing/tests: `crates/fmt/src/print_ir.rs`, `tests/golden/fixtures/emit-addr-direct/`
- Semantics:
  - Eligible page-0 direct operands now default to direct-page encodings when an opcode exists.
  - Use-site `expr:abs` forces 16-bit absolute-family encoding and has highest precedence.
  - Declaration-level `var name:abs = ...` makes plain references inherit that 16-bit absolute preference.
  - `:abs` is an address-encoding preference only; it does not change variable data width.
- Constraints:
  - Declaration-level `:abs` is currently supported only on top-level `var` declarations.
  - Symbolic-subscript field declarations do not yet support `.field:...:abs`.
  - `:far` continues to mean 24-bit data width / long-address preference where already supported; `:word` still means data width only.

- VS Code LM tools were added for Copilot agent mode:
  - `k816_lookup_instruction`
  - `k816_query_memory_map`
- Extension implementation path:
  - Manifest/tool contributions and activation events: `editors/vscode-k816/package.json`
  - Tool runtime registration and formatting: `editors/vscode-k816/src/extension.ts`
  - Bundled instruction reference data: `editors/vscode-k816/resources/instructions-description.json`
  - Extension docs: `editors/vscode-k816/README.md`
- LSP implementation path:
  - New custom request `k816/queryMemoryMap`: `crates/lsp/src/lib.rs`
  - LSP request/response types:
    - `QueryMemoryMapParams { memory_name?: string, detail?: "summary" | "runs" }`
    - `QueryMemoryMapResult { status, reason?, memories, runs }`
  - Link state retention (`last_link_layout`, `last_link_error`) and pending-change flush before query are in `crates/lsp/src/lib.rs`.
- Constraints:
  - Tools are read-only (no build/run side effects).
  - Requires VS Code `^1.109.0` and Copilot chat/agent availability.
  - Instruction lookup uses bundled static JSON (no external fetch).
  - Memory map output depends on current in-memory LSP link state; returns `unavailable` with reason when link data is absent/failed.

- LSP feature batch for extension features 7-17 was implemented (references/rename, completion/docs, semantic tokens, diagnostics freshness updates, inlay hints, hover metadata, code lens, folding ranges, signature help).
- Implementation path:
  - Core LSP capability wiring, handlers, and feature logic: `crates/lsp/src/lib.rs`
  - Bundled instruction hover metadata (flags/cycles): `crates/lsp/resources/instruction-metadata.json`
  - LSP integration coverage for new requests/capabilities: `tests/lsp_cli.rs`
  - VS Code code-lens command wiring: `editors/vscode-k816/src/extension.ts`, `editors/vscode-k816/package.json`
- Constraints:
  - Rename is safe symbol-only (resolved canonical symbols only); no fuzzy text rename.
  - References include declaration sites when requested by LSP context.
  - Folding ranges are brace-structure based; `#if ... #endif` preprocessor folding is not implemented yet.
  - Signature help currently covers evaluator built-ins only (e.g. `sin`, `cos`, `clamp`), not macro signatures.
  - Opcode hover cycle/flag details come from bundled metadata and are best-effort for covered instructions.

- DAP debug enhancements were added for features 18/19/20/23 (disassembly, memory read/write, register view, inline debug values).
- Implementation path:
  - Extension DAP proxy + inline values provider: `editors/vscode-k816/src/extension.ts`
  - Extension settings/docs updates: `editors/vscode-k816/package.json`, `editors/vscode-k816/README.md`
  - LSP custom request for inline symbol resolution: `k816/resolveInlineSymbols` in `crates/lsp/src/lib.rs`
  - Emulator DAP/backend wiring (external repo): `/home/smoku/devel/X65/devel/emu/src/dap.cc`, `/home/smoku/devel/X65/devel/emu/src/common/webapi.h`, `/home/smoku/devel/X65/devel/emu/src/x65.c`
- Constraints:
  - Scope intentionally excludes feature 21 (conditional breakpoints) and 22 (data breakpoints/watchpoints).
  - Inline symbol memory reads are limited to `read_size_hint` 1 or 2; larger symbols currently render address-only inline text.
  - Inline values depend on a stopped `k816` debug session and current LSP symbol/address state.

- DP / ABS / FAR variable address spaces were decoupled.
- Implementation path:
  - Sema placement split: `crates/core/src/sema/model.rs` (`VarPlacement::{Fixed, AllocatedAbs, AllocatedDp}`), `crates/core/src/sema/vars.rs` (DP cursor, FAR-no-addr rejection, fixed-DP range check)
  - HIR ops: `crates/core/src/hir/mod.rs` adds `DefineDpFixedSymbol`, `DefineDpAllocSymbol`, `DefineDpAllocAlias`
  - Lowering: `crates/core/src/lower.rs` (`emit_var_absolute_symbols`, dotted-name DP hint inheritance in `address_size_hint_for_operand`)
  - Object format: `crates/o65/src/model.rs` adds `SymbolDefinition::{DirectPageFixed, DirectPageAlloc, DirectPageAllocAlias}` (payload version 9); `crates/o65/src/codec.rs` round-trips them
  - Linker DP allocator: `crates/link/src/layout/dp.rs` (first-fit 256-byte bitmap, pin + allocate + alias passes), wired into `crates/link/src/layout.rs`
  - Listing: `[DP]` block in `crates/link/src/layout.rs` (`format_dp_listing`)
  - LSP hover: `crates/lsp/src/hover.rs` surfaces DP storage class and offset
  - Docs: `docs/syntax-reference.md` (storage classes), `docs/linker-ron-format.md` (DP is invisible to RON)
  - Coverage: `tests/golden/fixtures/link-dp/` (the bug fixture), `link-dp-mixed/` (multi-unit), `link-dp-fixed-and-auto/` (pinned + auto, intentional aliasing), `link-dp-overflow-err/` (>256 bytes), `link-dp-fixed-out-of-range-err/` (`var dp = $0100`), `var-far-no-addr-err/`, `syntax-symbolic-subscripts-dp/`
- Semantics:
  - DP-class vars (`var dp NAME`) live in a 256-byte logical pool managed at link time, fully decoupled from segments and `MemoryKind`. Auto-allocation is first-fit in declaration order within a unit, link-input order between units.
  - FAR-class vars (`var far NAME`) require an explicit `= <addr>` initializer; the assembler does not auto-allocate banks.
  - ABS-class vars (`var NAME` / `var abs NAME`) keep the existing per-segment cursor behaviour.
  - Fixed-DP collisions (`var dp foo = $42` and `var dp bar = $42`) are intentionally allowed for typed-view aliasing.
  - Symbolic-subscript fields on a DP parent inherit the DP encoding hint via dotted-name lookup in `address_size_hint_for_operand` and resolve to `parent_dp_offset + field_offset`.
- Constraints:
  - DP pool overflow (>256 bytes total across all linked objects) is rejected with a rich link-time diagnostic.
  - DP fixed offsets must be `≤ $FF` — pinned offsets that don't fit in a byte are rejected at sema.
  - DP is invisible to RON: there is no `MemoryKind::DirectPage`, no DP segment rule, no `__dp__` section.
