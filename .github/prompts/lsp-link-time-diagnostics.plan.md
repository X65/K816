# Plan: Surface link-time errors in the k816 LSP

## Context

The k816 LSP at `crates/lsp/` currently surfaces only per-file compile diagnostics. Link-time errors (undefined symbols, range overflows, section overlaps, far-call width mismatches, duplicate symbols, …) are produced exclusively by `k816_link::link_objects_with_options` ([crates/link/src/layout.rs:64](crates/link/src/layout.rs#L64)) and are invisible in the editor.

Concretely, the fixture [tests/golden/fixtures/syntax-addrmode-indirect/input.k65](tests/golden/fixtures/syntax-addrmode-indirect/input.k65) references symbols (`shell_str_ptr`, `shell_tokenize_exit`) that are never defined. Per-file `compile_source` legitimately succeeds — these become relocation entries on the assumption another translation unit defines them ([driver.rs:107](crates/core/src/driver.rs#L107)) — and `cargo test fixture_fixture_syntax_addrmode_indirect` fails only at link time. The LSP already runs the linker via `try_link_workspace` ([crates/lsp/src/workspace.rs:354](crates/lsp/src/workspace.rs#L354)), but it stashes errors into `last_link_error: Option<String>` ([crates/lsp/src/types.rs:274](crates/lsp/src/types.rs#L274)) and never publishes them.

The root cause is structural: the linker emits a single pre-rendered ariadne string via `anyhow::bail!("{}", errors.join("\n\n"))` ([crates/link/src/layout.rs:305](crates/link/src/layout.rs#L305)), with no per-source attribution exposed in the return type. To surface link errors in the editor, the linker's output must become structured.

**Hard constraint:** the LSP and the actual compiler must consume the same link-error production path. No LSP-specific simulation, no parallel logic. One producer, multiple consumers.

**Outcome:** open a file like `syntax-addrmode-indirect/input.k65` in the IDE and see red squiggles on `shell_str_ptr` and `shell_tokenize_exit`, with hover messages identical to what `cargo test` prints today.

### Companion bug: cross-file consts reported "unknown identifier" in fixture-style layouts

While auditing this plan, a second LSP-only bug surfaced that this work must also fix to make the manual smoke check meaningful.

**Symptom.** Open [tests/golden/fixtures/link-multi-const/input.10-main.k65](tests/golden/fixtures/link-multi-const/input.10-main.k65). Line 9's `lda #BAR` shows "unknown identifier 'BAR'" — but `const BAR = 1337` is defined right next to it in [input.20-const.k65](tests/golden/fixtures/link-multi-const/input.20-const.k65), and `cargo test fixture_fixture_link_multi_const` happily passes.

**Root cause.** `ServerState::initialize_workspace` calls `discover_workspace_sources` ([crates/lsp/src/workspace.rs:21](crates/lsp/src/workspace.rs#L21)), which recurses through `src/` and `tests/` and treats every discovered `.k65` file as part of one compilation unit. Inside this repo that pulls in **every** golden fixture under `tests/golden/fixtures/`, including unrelated programs that happen to share names. `collect_external_consts_from_parsed` ([crates/core/src/driver.rs:336](crates/core/src/driver.rs#L336)) treats divergent values for the same name as ambiguous and **drops the symbol entirely** from the externals map. Verified at HEAD:

- `tests/golden/fixtures/link-multi-const/input.20-const.k65` defines `const BAR = 1337`
- `tests/golden/fixtures/link-multi-const-err/input.20-const.k65` defines `const BAR = 84`

`BAR` is therefore omitted from `WorkspaceExternals.consts`. The per-file compile of `input.10-main.k65` then sees `BAR` as unknown. (Spot-confirmed by passing only the two `link-multi-const/` files into `compile_sources_with_externals` directly — `BAR` resolves cleanly under that scope.)

The same pathology affects any future workspace that contains multiple independent programs side-by-side; this is not specific to fixture trees.

**Why this belongs in the same plan.** The link-time-diagnostics work in steps 1–9 below is built on `compile_sources_with_externals` returning the same view of the world the linker sees. If that view glues every fixture into one ill-formed pseudo-program, the new link diagnostics will be just as wrong as the current "unknown identifier 'BAR'" — and the manual smoke check on [input.20-const.k65](tests/golden/fixtures/link-multi-const/input.20-const.k65) cannot validate the work end-to-end. Fixing partitioning unblocks the rest.

## Approach

Refactor the linker to emit `Vec<LinkDiagnostic>` carrying source-anchored info. Both CLI and LSP convert these into `k816_core::diag::Diagnostic` (via a single adapter) and render with the existing `k816_core::diag::render_diagnostics` (CLI) or convert to LSP `Diagnostic` via the existing `diagnostic_to_lsp` ([crates/lsp/src/convert.rs:38](crates/lsp/src/convert.rs#L38)) (LSP). One producer, one adapter, two presentation paths that are themselves already shared with compile diagnostics.

Why a new `LinkDiagnostic` type rather than passing a `SourceMap` into the linker:
- The linker operates on `O65Object` and never holds source text. Forcing it to take a `SourceMap` couples it to the compiler frontend, breaks every existing caller signature, and makes a standalone `k816 link <objs>` operate on text it has no business reading.
- Every error site already has a `SourceLocation { file, line, column, column_end, line_text }` available via `Relocation.source` ([crates/o65/src/model.rs:90](crates/o65/src/model.rs#L90)) or `find_section_anchor_context` ([crates/link/src/layout/anchors.rs](crates/link/src/layout/anchors.rs)). The linker's natural unit is `(path, line, col)`, not `Span`.
- A boundary adapter mirrors how `compile_sources` already produces structured diagnostics that each consumer translates as needed.

## Concrete changes

### 0. Workspace partitioning (companion fix)

This step is independent of the structured-link-diagnostic refactor (steps 1–9) and lands first.

The LSP must stop pretending that "every `.k65` file under `src/` and `tests/`" is a single compilation unit. Instead, partition discovered sources into units that match what the linker would actually link together.

**Strategy.** Two-tier, in order of precedence:

a. **Manifest-anchored unit.** When the workspace root has a `k816.toml` ([crates/project/src/lib.rs:6](crates/project/src/lib.rs#L6)), that manifest defines the canonical unit — discover sources only under `src/` per the existing `discover_sources_in_folders` contract, *not* the entire `tests/` tree. The fixture trees under `tests/golden/fixtures/` are not part of the manifest's program; they should never have been pulled in.

b. **Directory-bucket fallback.** When no manifest is present, partition discovered sources by their *parent directory*. Each directory becomes a self-contained unit. This is how the fixture layout already organizes one program per `tests/golden/fixtures/<name>/`. Files outside any recognized directory (e.g. ad-hoc files at the workspace root) form their own per-file unit.

The current single-bucket behavior is a degenerate case of (b) and should disappear once partitioning lands.

**Concrete changes.**

- `crates/lsp/src/types.rs` — add `compilation_units: Vec<CompilationUnit>` to `ServerState`, where `CompilationUnit { uris: Vec<Uri> }` enumerates the documents that should be compiled together. Backed by an `IndexMap<UnitId, Vec<Uri>>` if it simplifies lookups; either is fine.
- `crates/lsp/src/project.rs` — add `partition_workspace_into_units(root: &Path, manifest: Option<&ProjectManifest>, discovered: &[PathBuf]) -> Vec<CompilationUnit>`. Implements the manifest-first / directory-fallback rules above. Pure function, deterministic ordering, unit-testable.
- `crates/lsp/src/workspace.rs` `analyze_all_documents` ([workspace.rs:154](crates/lsp/src/workspace.rs#L154)) — loop over `state.compilation_units`. For each unit, build the `LinkCompileInput` slice from that unit's URIs only, call `collect_workspace_externals` and `compile_sources_with_externals` per unit, then fan out per-doc analysis exactly as today. A document open but not in any unit (rare; e.g. user dragged a stray file in) is analyzed as a singleton unit.
- `try_link_workspace` ([workspace.rs:354](crates/lsp/src/workspace.rs#L354)), once step 7 lands, runs **once per unit**. Each unit gets independent link diagnostics; cross-unit collisions become impossible by construction. Unit-IDed link state replaces the singleton `last_link_diagnostics` field — change `last_link_diagnostics: Vec<k816_link::LinkDiagnostic>` (step 7) to `last_link_diagnostics_per_unit: IndexMap<UnitId, Vec<k816_link::LinkDiagnostic>>`. `query_memory_map` then asks "for the unit owning *this* URI, was there a link error?".

**Tests.**

- `crates/lsp/src/tests.rs` (or a new `partition_tests.rs`):
  - `partition_groups_by_parent_directory` — feed three files in two directories; assert two units.
  - `partition_honors_manifest_root` — when `k816.toml` present, only `src/` files form the unit; `tests/` files are excluded.
  - `cross_file_const_visible_within_unit` — open `link-multi-const/input.10-main.k65` and `input.20-const.k65`; assert no `unknown identifier 'BAR'` diagnostic.
  - `cross_file_const_invisible_across_units` — open `link-multi-const/input.10-main.k65` plus `link-multi-const-err/input.20-const.k65` (different `BAR`); assert the `BAR` reference in the first file does **not** silently pick up the unrelated definition. (Today's bug is that `BAR` becomes ambiguous and disappears; the desired behavior is that the second file isn't even visible to the first because they're separate units.)

**Out of scope here.** Manifest schema growth (e.g. multi-program manifests). The current single-program manifest plus directory-fallback is sufficient to fix the observed bug and to keep developing inside this repo without spurious squiggles.

### 1. New types in the link crate

`crates/link/src/types.rs` — add:

```rust
pub struct LinkDiagnostic {
    pub severity: LinkSeverity,             // Error / Warning
    pub message: String,                    // e.g. "undefined symbol 'foo'"
    pub primary_label: Option<String>,      // e.g. "symbol 'foo' referenced here"
    pub anchor: Option<SourceLocation>,     // None for anchorless internals
    pub help: Option<String>,
    pub related: Vec<LinkRelated>,          // for "first defined here" etc.
}
pub struct LinkRelated { pub anchor: SourceLocation, pub message: String }
pub enum LinkSeverity { Error, Warning }

pub struct LinkErrors(pub Vec<LinkDiagnostic>);  // impls std::error::Error
```

`anchor: Option<SourceLocation>` cleanly handles the few sites without source info (unknown sections, missing memory regions); these stay anchorless and the renderer falls back to message-only.

`crates/link/src/lib.rs` — re-export `LinkDiagnostic`, `LinkRelated`, `LinkSeverity`, `LinkErrors`, and the new entry point below.

### 2. Canonical linker entry point

`crates/link/src/layout.rs`:

```rust
pub fn link_objects_diagnostics(
    objects: &[O65Object],
    config: &LinkerConfig,
) -> Result<LinkedLayout, LinkErrors>;
```

Replace `Vec<String>` accumulator at [layout.rs:134](crates/link/src/layout.rs#L134) with `Vec<LinkDiagnostic>`. Each error site that currently calls `decorate_with_anchor[_with_label]` or `format!` becomes a `LinkDiagnostic` constructor:

- Undefined symbol [layout.rs:404-424](crates/link/src/layout.rs#L404-L424) — `anchor` from `reloc.source` or `find_section_anchor_context`, `primary_label = Some("symbol 'X' referenced here")`.
- Site outside section [layout.rs:394-402](crates/link/src/layout.rs#L394-L402) — anchorless.
- 16-bit reloc requires far [layout.rs:432-435](crates/link/src/layout.rs#L432-L435) — anchor from reloc.
- Range overflow [layout.rs:464-484](crates/link/src/layout.rs#L464-L484) — anchor from reloc.
- Width mismatch [layout.rs:487-497](crates/link/src/layout.rs#L487-L497) — anchor from reloc.
- Duplicate symbol [layout.rs:235-240](crates/link/src/layout.rs#L235-L240) — `anchor` = second definition, `related[0]` = first definition.
- Section placement / call-metadata errors — same pattern.

Internal helpers (`place_chunk`, `choose_reloc_base`, `resolve_one_relocation`, `collect_call_metadata_errors`, `validate_call_metadata_inner`, `check_width_mismatch`) change return type from `Result<T, String>` / `anyhow::Result<T>` to `Result<T, LinkDiagnostic>`.

`find_anchor_context` and `find_section_anchor_context` ([crates/link/src/layout/anchors.rs](crates/link/src/layout/anchors.rs)) keep their signatures — they still return `Option<AnchorContext>` for internal use; only the rendering helpers (`decorate_with_anchor*`, `render_anchor_context`, `MultiSourceCache`, `SingleSourceCache`, `render_duplicate_symbol_error`) are deleted, since rendering moves to `k816_core::diag` per the user's choice to unify.

`link_objects_with_options` is **kept** as a thin wrapper for back-compat during the migration: calls `link_objects_diagnostics`, converts to `Vec<k816_core::diag::Diagnostic>` via the adapter (step 3), renders via `k816_core::diag::render_diagnostics_with_options`, returns `anyhow::Error::msg(rendered)`. After all callers move, it can be deleted in a follow-up.

### 3. Adapter: `LinkDiagnostic` → `k816_core::diag::Diagnostic`

`crates/core/src/diag.rs` (or new `crates/core/src/link_diag.rs`):

```rust
pub fn link_diagnostic_to_diagnostic(
    link_diag: &k816_link::LinkDiagnostic,
    source_map: &SourceMap,
    resolve_path: &dyn Fn(&str) -> Option<SourceId>,
) -> Option<Diagnostic>;
```

- Resolves `anchor.file → SourceId` via the closure.
- Converts `(line, column..column_end)` to byte offsets using `SourceFile::line_starts` (already present on `SourceFile`).
- Builds `Diagnostic::error(span, message).with_primary_label(label).with_optional_help(help)`, adds related `LabelledSpan`s.
- Returns `None` only when an anchorless diagnostic cannot be mapped to any source (callers decide the fallback).

Re-export from `crates/core/src/lib.rs`.

### 4. Driver entry that exposes the SourceMap

The CLI and golden harness need the SourceMap that `compile_sources` builds internally so they can resolve link diagnostics against it. Add to `crates/core/src/driver.rs`:

```rust
pub fn compile_sources_keeping_map(
    sources: &[LinkCompileInput<'_>],
    options: CompileRenderOptions,
) -> (Vec<Result<CompileObjectOutput, CompileError>>, SourceMap, Vec<SourceId>);

pub fn compile_sources_all_or_nothing_keeping_map(
    sources: &[LinkCompileInput<'_>],
    options: CompileRenderOptions,
) -> Result<(Vec<CompileObjectOutput>, SourceMap, Vec<SourceId>), CompileError>;
```

These reuse the existing `assign_source_ids` + `compile_sources_with_externals_inner` flow, simply not discarding the SourceMap. The `path → SourceId` closure for the adapter is `|path| source_ids.iter().zip(sources.iter()).find_map(|(id, input)| (input.name == path).then_some(*id))`.

### 5. CLI migration (chosen: unified rendering)

Both CLI link sites switch to the structured-diagnostic flow and render via `k816_core::diag::render_diagnostics_with_options`:

- `single_file_build_command` ([src/main.rs:639-659](src/main.rs#L639-L659)): swap `compile_source_file` for a keep-map variant; on link `Err(LinkErrors)`, convert to `Vec<Diagnostic>` via the adapter, render with `k816_core::diag::render_diagnostics_with_options(&map, &diags, opts)`, wrap as `RenderedDiagnosticError(rendered)` so the existing `print_error` ([src/main.rs:285](src/main.rs#L285)) handles it identically to compile errors.
- `project_build_internal` ([src/main.rs:902](src/main.rs#L902), link call at [src/main.rs:977](src/main.rs#L977)): same pattern; SourceMap comes from `compile_sources_all_or_nothing_keeping_map`.

Anchorless link diagnostics (no `anchor`, e.g. "memory region missing") render as message-only via `k816_core::diag` — match the existing anchorless branch in `decorate_with_anchor` to keep semantics.

### 6. Harness migration

`tests/golden/src/harness.rs` `compile_and_link` ([tests/golden/src/harness.rs:511](tests/golden/src/harness.rs#L511)): switch to `compile_sources_all_or_nothing_keeping_map` + `link_objects_diagnostics` + adapter + `k816_core::diag::render_diagnostics_with_options`. Same pattern as CLI; one shared `render_link_errors_to_diagnostic_error(...)` helper in `crates/core` (or inlined) keeps the two callers in lockstep.

Bless step: since rendering moves from the link-crate ariadne config to `k816_core::diag`, byte-level differences in `tests/golden/fixtures/*/expected.err` are likely (label colors, padding, trailing newlines). Run the existing `bless` binary ([tests/golden/src/bin/bless.rs](tests/golden/src/bin/bless.rs)) once after step 5 lands, review the diffs, commit. The CLI assertion in [tests/cli.rs:108](tests/cli.rs#L108) only checks `contains("undefined symbol 'missing'")` — that remains valid.

### 7. LSP integration

`crates/lsp/src/workspace.rs` `try_link_workspace` ([workspace.rs:354-406](crates/lsp/src/workspace.rs#L354-L406)):

1. Build a `path → URI` lookup from the existing `doc_uris` and `source_names` (workspace.rs:156-166) — these are exactly the strings the linker stores in `SourceLocation.file`.
2. Call `link_objects_diagnostics(&objects, &self.linker_config)` (replaces line 373).
3. **On Ok(layout)**: clear any previously-appended link diagnostics on every doc; populate `resolved_sites` exactly as today.
4. **On Err(LinkErrors(link_diags))**:
   - For each `LinkDiagnostic`, look up its `anchor.file` in the path→URI map. For matched docs, build a per-doc `SourceMap` on the fly (`add_source(name, &doc.text)`), call `link_diagnostic_to_diagnostic`, and append the resulting `Diagnostic` to that doc's `analysis.diagnostics` (per chosen LSP attach point).
   - Unmatched diagnostics (anchor in a non-open file or anchorless) are logged via `eprintln!` and dropped from per-doc lists.
   - Clear `resolved_sites` on every doc.

Since the user chose **append into `analysis.diagnostics`**, link entries naturally flow through the existing `lsp_diagnostics` ([workspace.rs:480-502](crates/lsp/src/workspace.rs#L480-L502)) without merge logic. Critical invariant: `try_link_workspace` is called **only** from `rebuild_symbol_index` ([workspace.rs:286-303](crates/lsp/src/workspace.rs#L286-L303)), which is called only from paths that first run `analyze_all_documents` (which rewrites `doc.analysis = new_analysis` at line 202, naturally clearing prior link entries). Audit `rebuild_symbol_index` callers to confirm — if any path calls it without a fresh compile, add an explicit "remove link-origin entries" pass guarded by an internal tag (e.g. a sentinel `Diagnostic::primary_label` prefix) before appending.

`crates/lsp/src/types.rs`:
- Remove `last_link_error: Option<String>` ([types.rs:274](crates/lsp/src/types.rs#L274)).
- Add `last_link_diagnostics: Vec<k816_link::LinkDiagnostic>` to `ServerState` so `query_memory_map` can derive a `reason`.

`crates/lsp/src/workspace.rs::query_memory_map` ([workspace.rs:413](crates/lsp/src/workspace.rs#L413)): replace the `last_link_error` lookup with a small helper that renders `last_link_diagnostics` via the same `k816_core::diag` path used by the CLI, returning `Some("link errors prevent memory map: …")` when non-empty.

`crates/lsp/src/dedup_diagnostics` ([crates/lsp/src/convert.rs:121](crates/lsp/src/convert.rs#L121)): unchanged. Its key `(severity, message, start, end)` already disambiguates compile vs link reports in practice (different messages or different spans). The rare collision case (compile and link both flagging the exact same byte range with the exact same message) is acceptable — dropping a duplicate squiggle is the right behavior.

### 8. Cross-file diagnostic broadcast

`crates/lsp/src/server.rs`: add

```rust
fn publish_all_open_diagnostics(&self) -> Result<()>;
```

Walk every open document, call `state.lsp_diagnostics(uri)`, publish. Replace the single-URI publishes that follow analysis cycles:

- `did_open` ([server.rs:188](crates/lsp/src/server.rs#L188)): publish the new URI immediately, then broadcast.
- `did_change` debounced flush (around [server.rs:288-291](crates/lsp/src/server.rs#L288-L291)): broadcast instead of single-URI publish.
- `did_save` ([server.rs:237](crates/lsp/src/server.rs#L237)): broadcast.
- `did_close` ([server.rs:247](crates/lsp/src/server.rs#L247)): publish empty for the closed URI, then broadcast (closing a doc may resurface link errors elsewhere).
- Filesystem-event handler ([server.rs:141-149](crates/lsp/src/server.rs#L141-L149)): broadcast (FS-event scope under-reports cross-file impacts).

Cost: O(N_open) publishes per analysis cycle — standard practice for LSPs.

### 9. Tests

`crates/lsp/src/tests.rs` — add two tests modeled on `resolves_cross_file_function_definition` ([crates/lsp/src/tests.rs:206](crates/lsp/src/tests.rs#L206)):

- **`link_undefined_symbol_appears_as_diagnostic`**: open one document referencing `missing`; assert `lsp_diagnostics(&uri)` contains an Error with `message.contains("undefined symbol 'missing'")` and `range` covering the `missing` token.
- **`cross_file_link_diagnostic_invalidates_when_other_file_changes`**: open two documents, `main.k65` referencing `app_init` defined in `extra.k65`; assert no diagnostic; close `extra.k65`; assert the diagnostic appears on `main.k65`; reopen; assert it disappears.

A fixture `tests/golden/fixtures/syntax-addrmode-indirect/` already exercises the CLI end of this path; no new golden fixture needed unless we want a positive-control case for "cross-file link error on file B when A is removed."

## Order of operations

Each step keeps the workspace green.

0. Land workspace partitioning (companion fix, section 0). `partition_workspace_into_units` + per-unit `analyze_all_documents` loop + the four new partition tests. After this step the existing `unknown identifier 'BAR'` regression on [tests/golden/fixtures/link-multi-const/input.10-main.k65](tests/golden/fixtures/link-multi-const/input.10-main.k65) is gone, and the rest of the plan's link-error work runs against the correct unit granularity. `cargo test` passes.
1. Add `LinkDiagnostic` types in `crates/link/src/types.rs`. No callers. `cargo test` passes.
2. Add `link_diagnostic_to_diagnostic` adapter in `crates/core/src/diag.rs` with unit tests for line-range → byte-offset resolution. `cargo test` passes.
3. Add `compile_sources_keeping_map` / `compile_sources_all_or_nothing_keeping_map` in `crates/core/src/driver.rs`. No callers. `cargo test` passes.
4. Refactor `crates/link/src/layout.rs` and prune rendering helpers from `crates/link/src/layout/anchors.rs`. Add `link_objects_diagnostics`. Make `link_objects_with_options` a thin wrapper that produces the same byte-for-byte string today (use the link-crate's existing renderer in this transitional wrapper to keep step 4 local). `cargo test -p k816-link` passes.
5. Migrate CLI ([src/main.rs](src/main.rs)) and harness ([tests/golden/src/harness.rs](tests/golden/src/harness.rs)) to `link_objects_diagnostics` + adapter + `k816_core::diag` renderer. **Run `cargo run --bin bless` once**, review `expected.err` diffs, commit. `cargo test` passes.
6. Once steps 4–5 settle, retire `link_objects_with_options` and the residual link-crate rendering helpers in a small follow-up commit (no behavior change). Optional within this plan's scope.
7. LSP migration: types changes ([crates/lsp/src/types.rs](crates/lsp/src/types.rs)), `try_link_workspace` rewrite ([crates/lsp/src/workspace.rs](crates/lsp/src/workspace.rs)), `query_memory_map` update, `publish_all_open_diagnostics` helper in [crates/lsp/src/server.rs](crates/lsp/src/server.rs), broadcast call sites. `cargo test -p k816-lsp` passes.
8. Add the two new LSP tests. `cargo test` passes.

## Files to modify

- `crates/lsp/src/project.rs` — `partition_workspace_into_units` (companion fix, section 0).
- `crates/lsp/src/types.rs` — `compilation_units` field on `ServerState`; `last_link_diagnostics_per_unit` once link integration lands.
- `crates/link/src/types.rs` — `LinkDiagnostic`, `LinkErrors`, `LinkRelated`, `LinkSeverity`.
- `crates/link/src/layout.rs` — accumulator switch, `link_objects_diagnostics`, `link_objects_with_options` becomes a wrapper.
- `crates/link/src/layout/anchors.rs` — prune rendering helpers, keep `find_*_anchor_context`.
- `crates/link/src/lib.rs` — re-exports.
- `crates/core/src/diag.rs` — `link_diagnostic_to_diagnostic` adapter.
- `crates/core/src/driver.rs` — `compile_sources_keeping_map`, `compile_sources_all_or_nothing_keeping_map`.
- `crates/core/src/lib.rs` — re-exports.
- `src/main.rs` — `single_file_build_command` and `project_build_internal` link sites.
- `tests/golden/src/harness.rs` — `compile_and_link` migrates to structured flow.
- `crates/lsp/src/types.rs` — drop `last_link_error`, add `last_link_diagnostics`.
- `crates/lsp/src/workspace.rs` — `try_link_workspace`, `query_memory_map`.
- `crates/lsp/src/server.rs` — `publish_all_open_diagnostics`, broadcast call sites.
- `crates/lsp/src/tests.rs` — two new tests.
- `tests/golden/fixtures/*/expected.err` — bless after step 5.

## Verification

```sh
# Linker only
cargo test -p k816-link

# Golden fixtures (most likely to flag rendering deltas)
cargo test -p k816-tests-golden

# CLI integration
cargo test --test cli

# LSP unit tests, including the two new ones
cargo test -p k816-lsp

# Full workspace
cargo test
```

Manual LSP smoke check:

1. Open [tests/golden/fixtures/link-multi-const/input.10-main.k65](tests/golden/fixtures/link-multi-const/input.10-main.k65). Expect **no** "unknown identifier 'BAR'" diagnostic on line 9 — `BAR` is defined in the sibling [input.20-const.k65](tests/golden/fixtures/link-multi-const/input.20-const.k65) and the partitioning fix (section 0) makes that visible. This is the regression the companion fix targets; if the squiggle is still there, the rest of the smoke check is moot.
2. Open [tests/golden/fixtures/syntax-addrmode-indirect/input.k65](tests/golden/fixtures/syntax-addrmode-indirect/input.k65) in VS Code with the `editors/vscode-k816/` extension running against a `cargo run --bin k816 -- lsp` server.
3. Expect red squiggles on `shell_str_ptr` (lines 2 and 9) and `shell_tokenize_exit` (line 12), with hover messages matching the `cargo test` output verbatim.
4. Open a multi-file example where one file defines `app_init` and another references it. Close (or delete) the defining file — the squiggle on the consuming file appears. Re-open it — the squiggle disappears. This validates step 8 broadcast.
5. With a link error active, invoke the `k816/queryMemoryMap` LSP request — assert `reason` is non-empty and references the underlying diagnostic (no longer the dropped `last_link_error` string).
