---
title: "k816 Project Build System (Cargo/Dune-style)"
status: "approved-proposal"
owner: "agent"
last_updated: 2026-02-11
scope:
  - k816 CLI: project-mode commands (init/build/run/clean)
  - project layout conventions
  - minimal manifest format (k816.toml)
non_goals:
  - package registry (crates.io/opam-like)
  - complex build graphs / user-defined rules
  - IDE integration beyond stable CLI outputs
---

# Goal

Add a simple, convention-first build system to `k816` for a predefined project structure,
inspired by Cargo/Dune ergonomics and build2 auto-discovery,
while keeping k816’s existing `compile`/`link` pipeline (o65 objects + linker config).

# User-facing UX

## Commands

Implement these top-level commands in `k816`:

- `k816 init <name>`
  - Scaffold a new project directory with default layout and config files.
- `k816 build`
  - Compile + link the default target.
- `k816 run [-- <args...>]`
  - Build then invoke configured runner (emulator/flash tool).
  - `--` forwards args to runner.
- `k816 clean`
  - Remove build output directories.

## Output & exit behavior

- Clear, deterministic logs:
  - One line per compiled unit.
  - One line for link step.
  - For `run`, print invoked command line.
- Exit codes:
  - `0` on success.
  - Non-zero on compilation/link failures or runner failures.
- Paths printed should be stable and machine-parsable where possible.

# Project Structure (Conventions)

The build system assumes the following layout:

```

myproj/
  k816.toml
  link.ron                 # optional (default link script)
  src/
    main.k65               # default executable entry
    **/*.k65               # modules (auto-discovered)
  assets/                  # optional
  target/
````

Rules:

- Default executable entry: `src/main.k65`.
- All `src/**` files with extension `.k65` are discovered automatically.

# Manifest: `k816.toml`

## Minimal schema

Implement a minimal manifest parser:

```toml
[package]
name = "myproj"
version = "0.1.0"

[link]
script = "link.ron"        # if missing: use link.ron if present, else internal default

[run]
runner = "x65emu"          # optional; if missing, `k816 run` errors with actionable message
args = []                  # default runner args (optional)
````

Validation rules:

- `package.name` required.
- `package.version` optional (default "0.1.0").
- `link.script` optional; resolution:

  1. if set in manifest, use it
  2. else if `link.ron` exists, use it
  3. else use built-in default link config (internal)
- `run.runner` optional; required for `k816 run`.

# Build Semantics

## Output directories

Use Cargo-like target layout:

- Objects:

  - `target/obj/...`
- Final linked artifact:

  - `target/<package.name>.<ext>` where `<ext>` is derived from output format (`raw`/`xex`).

Keep file placement deterministic and stable.

## Source discovery

- Compile set for build:

  - all files under `src/**` with extensions: `.k65`
- No manual file lists in the manifest.

## Compilation and linking pipeline

- Reuse existing commands internally:

  - compile each translation unit to relocatable `.o65`
  - link the produced objects into final binary

Implementation guidance:

- `k816 build` behaves like:

  1. discover sources in `src/**`
  2. compile -> `.o65` (into target tree)
  3. link -> final output artifact

# Running

## Runner selection

`k816 run`:

- If `[run].runner` is set:

  - execute `<runner> <run.args...> <artifact> -- <user args...>` (exact forwarding rules below)
- If not set:

  - error with message: “No runner configured. Set [run].runner in k816.toml.”

## Arg forwarding

- `k816 run -- foo bar` forwards `foo bar` to the runner after `--`.
- `run.args` from manifest are inserted before forwarded args.
- The linked artifact path is always passed to the runner.

Example invocation layout:

- runner command line:

  - `<runner> <run.args...> <artifact> <forwarded args...>`

# Cleaning

`k816 clean`:

- remove `target/` directory for the project root (or only the k816-managed subdirectories).
- be safe: never delete outside the project directory.

# Implementation Plan

## Phase 0: Project detection

- Define “project root”:

  - current directory (or nearest parent) containing `k816.toml`
- If not found:

  - for `init`: create it
  - for other commands: error with “No k816.toml found (run k816 init).”

## Phase 1: `init`

- Create directory `<name>/` (or initialize current dir if `name="."` is supported)
- Write:

  - `k816.toml` minimal template
  - `src/main.k65` minimal hello program (or a minimal empty entry if appropriate)
  - `link.ron` default (optional if you prefer built-in)
  - `.gitignore` containing `target/`
- Ensure idempotency: refuse to overwrite existing files unless `--force` (optional).

## Phase 2: `build`

- Parse manifest.
- Resolve profile.
- Discover sources.
- Compile each `.k65` in `src/**`:

  - determine object output path
  - perform incremental rebuild checks (see Phase 4) or just rebuild all initially
- Link objects using:

  - manifest-resolved link script (or default)
  - manifest output format override if you already support it at top-level (`--output-format`)
- Write final artifact into `target/`.

## Phase 3: `run`

- Require runner configured.
- Call `build` first.
- Invoke runner with artifact and args.
- Propagate exit code.

# Acceptance Criteria

- `k816 init hello` produces a buildable project.
- `k816 build` from project root generates:

  - `target/debug/<name>.*` and object files under `target/debug/obj/...`
- `k816 run` executes the artifact via configured runner and forwards args correctly.
- `k816 clean` removes build outputs.
- No file lists in config; adding a new `src/foo.k65` is picked up automatically.

# Notes / Guardrails

- Keep configuration intentionally minimal.
- Prefer conventions over extra manifest options.
- Preserve existing `compile/link` commands for low-level usage; “project mode” is additive.
- Focus on a single-project workflow first; multi-package/workspace support can come later if needed.
- Avoid complex build graph features (custom rules, user-defined targets) for now; keep it simple and focused on the common case.
