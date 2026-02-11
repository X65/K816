---
title: "k816 LSP Integration"
status: "plan"
owner: "agent"
last_updated: 2026-02-11
prerequisites:
  - "k816 project-mode build structure is in place (k816.toml, src/** discovery, etc.)"
scope:
  - "Add an LSP server for .k65 to the Rust-based k816 toolchain"
  - "Diagnostics + basic IDE features (definition/hover/completion/symbols/formatting)"
non_goals:
  - "Full rust-analyzer-level semantic model"
  - "Advanced refactors (rename/extract), typechecking beyond what k816 already has"
  - "Editor extension packaging (VSCode marketplace) - provide config snippets only"
---

# Goal

Provide a first-class Language Server Protocol (LSP) server for the K65 language (.k65) backed by k816's existing front-end (lexer/parser/HIR/diagnostics), aligned with the new project-mode layout.

Target: `k816 lsp` runs as a stdio LSP server usable by VSCode, Neovim, etc.

# Crate selection (Rust)

## Recommended baseline (sync, minimal surface)

- `lsp-server` (transport + JSON-RPC/LSP scaffolding, synchronous loop)
- `lsp-types` (LSP data types)
- `text-size` + `line-index` (byte offsets <-> line/col and UTF-16 mapping)
- `walkdir` (workspace discovery: src/**, tests/**)
- `notify` (optional: file watching for non-open files / external edits)
- logging:
  - either `log` + `env_logger`, or `tracing` + `tracing-subscriber`
- error plumbing:
  - `thiserror` (library errors)
  - `anyhow` (binary-level error context)

## Optional alternative (async framework)

- `tower-lsp-server` if an async/tokio-first LSP architecture is preferred.

Decision default: start with `lsp-server` because it fits a compiler-style “single dispatch loop + worker analysis” model and avoids forcing async across k816.

# Repo / crate layout

Add a new workspace crate:

- `crates/lsp/`
  - depends on `core` (AST/HIR/diagnostics), `fmt` (formatting), and project-mode manifest reader (wherever `k816.toml` lives)
  - exposes `run_stdio_server()` and request handlers

Wire into main binary:

- `k816 lsp` subcommand (in the top-level CLI crate)
  - calls `k816_lsp::run_stdio_server()`

# LSP capabilities (MVP)

Implement these first:

- Lifecycle:
  - `initialize`, `initialized`, `shutdown`, `exit`
- Text sync:
  - FULL sync first (simplifies edits); upgrade to incremental later
- Diagnostics:
  - publish on `didOpen`, `didChange`, `didSave`
- Navigation:
  - `textDocument/definition`
  - `textDocument/hover`
- Symbols:
  - `textDocument/documentSymbol`
- Completion:
  - opcode/directive keywords
  - in-scope labels/symbols (from k816 symbol resolution)
- Formatting:
  - `textDocument/formatting` via `fmt` crate (whole-document TextEdit)

Defer (phase 2+):

- semantic tokens
- references/rename
- code actions / quick-fixes

# Workspace model (project-mode aware)

## Root detection

- Determine workspace root by searching upward from CWD for `k816.toml`
- Parse manifest and apply the same source discovery rules as `k816 build`:
  - sources: `src/**` with `.k65`
  - tests: `tests/**` with `.k65`

## Source index

Maintain:

- `FileId` per URI/path
- `SourceMap`: URI <-> FileId <-> path
- `SymbolIndex`: global index for “goto definition” and “document symbols”
- module/import graph as defined by k816 module system (reuse `core`)

# Text and position mapping

## Document store

Keep an in-memory map:

- `documents: HashMap<Url, DocumentState>`

Where `DocumentState` holds:

- `version: i32`
- `text: String` (MVP)
- `line_index: LineIndex`
- `last_parse: ParseResult` (AST/HIR handles as appropriate)
- `last_diags: Vec<Diagnostic>`

## Offset conversions

- Internally, k816 diagnostics/spans should remain byte-based offsets (UTF-8).
- Convert to LSP `Range` at the boundary using `line-index`:
  - offset (byte) -> (line, col_utf8) via `LineIndex`
  - then to wide encoding (UTF-16) for LSP `Position.character`

# Diagnostics pipeline

## Sources of diagnostics

- lexer errors
- parser errors
- name resolution / module import errors
- basic semantic errors k816 already reports
- (optional) formatting warnings later

## Publishing strategy

- On `didOpen` / `didChange` / `didSave`:
  1) update stored text + recompute `LineIndex`
  2) run analysis (parse + resolve) for the document (and dependent docs if needed)
  3) convert k816 diagnostics -> LSP diagnostics
  4) `textDocument/publishDiagnostics`

## Debounce / responsiveness

- On rapid `didChange`, debounce analysis (e.g. 150-300ms) and cancel stale work.
- Keep the LSP message loop responsive at all times.

# Request handling

## textDocument/definition

- Use `core` name resolution results to map “symbol under cursor” -> definition site span
- Return `Location` (uri + range)
- If multiple results exist (overloads/macros), return all locations

## textDocument/hover

- For labels/symbols: show resolved symbol name, kind, module, and any doc comment if k816 supports it
- For opcodes/directives: show short builtin help text (hardcoded table)

## completion

- Always offer opcode/directive keywords
- If cursor is in operand/identifier context:
  - offer labels/symbols visible in scope
  - (optional) offer imported `pub` symbols from modules

## documentSymbol

- Provide a hierarchical symbol tree if k816 has nested scopes, else flat list:
  - functions/labels/macros/modules/consts as appropriate

## formatting

- Run `fmt` on the current document text and return a single whole-document `TextEdit`

# Concurrency model

Recommended:

- Main thread: LSP IO loop (recv requests, send responses)
- Worker thread(s): analysis tasks (parse/resolve/index)
- Communication:
  - job queue (crossbeam or std channels)
  - results channel back to main loop for publishing diagnostics and answering pending requests

MVP can start single-threaded; add worker thread as soon as diagnostics feel slow.

# Testing

## Unit tests

- position mapping (byte offsets <-> LSP positions) including non-ASCII
- diagnostic conversion correctness
- symbol lookup for definition/hover on sample fixtures

## Integration tests (preferred)

- spawn `k816 lsp` as a subprocess
- drive it using an LSP client test harness crate (or minimal handcrafted JSON-RPC)
- verify:
  - initialize handshake
  - didOpen -> diagnostics published
  - definition/hover returns expected locations

# Phased implementation

## Phase 1: MVP server + diagnostics

- add crates/lsp
- implement `k816 lsp` subcommand
- FULL text sync
- diagnostics publish on open/change/save

## Phase 2: navigation + symbols + formatting

- goto definition
- hover
- documentSymbol
- formatting

## Phase 3: completions + workspace robustness

- smarter completions by context
- workspace scanning and optional file watching (`notify`)
- debounce + background analysis

# Acceptance criteria

- `k816 lsp` can be configured in an editor and completes a full initialize/shutdown lifecycle.
- On opening a `.k65` file, parse/name-resolution diagnostics appear in the editor.
- “Go to definition” works for at least local labels and imported module symbols in a project-mode workspace.
- Formatting returns stable edits (idempotent formatting on repeated invocation).

Crate research references: `lsp-server` describes itself as a synchronous, crossbeam-channel based language server scaffold. ([docs.rs][4])
`lsp-types` provides LSP types, supports LSP 3.16, and has a `proposed` feature for 3.17 features. ([docs.rs][5])
`line-index` (rust-analyzer) provides UTF-8 offset mapping plus conversions to “wide” encodings (UTF-16), suitable for LSP position mapping. ([rust-lang.github.io][6])
`tower-lsp` offers a higher-level `LanguageServer` trait for async LSP servers; `tower-lsp-server` describes the `LanguageServer` trait + `LspService` + `Server` architecture. ([docs.rs][7])
`notify` is a cross-platform filesystem notification library and exposes a `Watcher` trait with platform backends. ([docs.rs][8])

[4]: https://docs.rs/crate/lsp-server/latest "lsp-server 0.7.9 - Docs.rs"
[5]: https://docs.rs/crate/lsp-types/latest "lsp-types 0.97.0 - Docs.rs"
[6]: https://rust-lang.github.io/rust-analyzer/line_index/struct.LineIndex.html "LineIndex in line_index - Rust"
[7]: https://docs.rs/tower-lsp/latest/tower_lsp/trait.LanguageServer.html "LanguageServer in tower_lsp - Rust"
[8]: https://docs.rs/notify/latest/notify/trait.Watcher.html?utm_source=chatgpt.com "Watcher in notify - Rust"
