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

- No "compat", "legacy", or "deprecated" shims; features are first-class or removed.
- `segment` directive only for output segments; no `bank` alias.
- Error on unknown directives; no silent ignores.
- Syntax: C-style comments, variables with `var`, constants in `[...]`, labels (global `label:`, local `.label:`), numeric literals (decimal, hex `$` or `0x`, binary `%` or `0b`). Example: [examples/hello_uart.k65](examples/hello_uart.k65) and [docs/syntax-reference.md](docs/syntax-reference.md).
- Project-mode CLI commands use `k816.toml` at the project root, compile `src/**/*.k65` into `target/obj/**/*.o65`, link to `target/<package>.{xex|bin}`, and optionally run the artifact via `[run].runner`.
- LSP workspace discovery is project-aware: when `k816.toml` exists, it indexes `.k65` sources under both `src/` and `tests/`.

## Integration Points

Linker uses RON format for configuration (`.ld.ron` files and project `link.ron`). `vendor/` contains the reference C compiler used for syntax/grammar cross-checking only; there is no Rust build or runtime integration with that code. Reference: [docs/linker-ron-format.md](docs/linker-ron-format.md).

## Diagnostics Style

- Keep primary error messages concise and focused on what is wrong.
- Put remediation guidance in Ariadne help text using `Diagnostic::with_help(...)`. **This is mandatory** — never fold "how to fix" wording into the primary error `message`, even in situations where it seems shorter or more convenient. The help slot exists specifically for this; the LSP layer reads `Supplemental::Help` separately, and Ariadne renders it as its own labelled line.
- Avoid embedding "how to fix" details directly in the main error message when help text can carry that context.
- For contract/call diagnostics, the help line should show the exact call syntax for the target function rather than vague directives ("match the declaration", "repeat the inputs"). Use `FunctionMeta::signature_call_form(name)` from `crates/core/src/sema/model.rs` to produce forms like `sub(a, #b) -> a`, `dispatch`, or `produce() -> a`.

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
