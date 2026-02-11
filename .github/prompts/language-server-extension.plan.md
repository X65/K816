---
title: "VS Code Extension for k816 LSP (.k65)"
status: "plan"
owner: "agent"
last_updated: 2026-02-12
prerequisites:
  - "k816 repo builds a working `k816` binary on developer machine"
  - "`k816 lsp` command mode exists (or is implemented in this plan) and speaks LSP over stdio"
scope:
  - "Implement the simplest possible VS Code extension that starts `k816 lsp` and wires it via `vscode-languageclient`"
  - "Register `.k65` as a VS Code language id (`k65`) so the extension auto-activates"
  - "Provide basic user settings: path to `k816`, extra args, env (RUST_LOG), and a restart command"
non_goals:
  - "Bundling/shiping the `k816` binary inside the extension (assume `k816` is installed / on PATH)"
  - "Syntax highlighting beyond minimal language configuration (TextMate grammar optional later)"
  - "Marketplace publishing (local run + optional `.vsix` packaging only)"
---

# Goal

Deliver a working VS Code extension that:

- activates on `.k65` files,
- launches `k816 lsp` as a stdio language server,
- connects using LSP and enables diagnostics/hover/definition/completion/etc. as provided by the Rust server.

This plan is intentionally minimal and should be developed alongside the Rust `k816 lsp` command mode so the end-to-end loop works.

# Repository layout

Add a new folder in the `k816` repo:

- `editors/vscode-k816/`
  - `package.json`
  - `src/extension.ts`
  - `language-configuration.json`
  - `tsconfig.json`
  - `.vscode/launch.json`
  - `README.md`
  - (optional later) `syntaxes/k65.tmLanguage.json`

Keep it as a standalone Node/TypeScript extension project.

# Technical choices

## VS Code client library

Use the official Node client SDK:

- `vscode-languageclient` (Node flavor import path: `vscode-languageclient/node`)

Pin a stable major version compatible with the repo’s Node/TypeScript setup (start with current stable `9.x` unless constraints force otherwise).

## Transport

- stdio only
- command: `k816`
- args: `["lsp"]`

## Activation

- `onLanguage:k65`

## Language id

- language id: `k65`
- file extension: `.k65`
- minimal `language-configuration.json` for brackets/comments/indentation rules

# Work items

## Phase 0 - Align Rust `k816 lsp` behavior (must land early)

Implement/verify in Rust:

1) `k816 lsp` runs an LSP server over stdio
   - stdin: JSON-RPC frames
   - stdout: JSON-RPC frames
   - stderr: logs only (never log to stdout)

2) Robust lifecycle
   - supports `initialize`, `initialized`, `shutdown`, `exit`
   - survives multi-root inputs (workspaceFolders) even if you only use the first folder for now

3) Root selection
   - use LSP `initialize.rootUri` / `workspaceFolders` as authoritative
   - do not rely on current working directory

4) Minimal diagnostics
   - publish on didOpen/didChange/didSave

Acceptance for Phase 0:

- Can run `k816 lsp` manually and complete initialize/shutdown with a generic LSP client.

## Phase 1 - Scaffold the extension project

1) Scaffold via generator

- use the VS Code extension generator (TypeScript)
- select a minimal extension template (no webviews, no extra UI)

1) Add dependencies

- `vscode-languageclient`
- dev: `typescript`, `eslint` (optional), `@types/node`

1) Add scripts

- `compile` (tsc)
- `watch` (tsc -w)
- `lint` (optional)
- `package` (optional: vsce)

Acceptance for Phase 1:

- `F5` launches Extension Development Host without runtime errors.

## Phase 2 - Register `.k65` language

Edit `package.json`:

1) contributes.languages

- id: `k65`
- extensions: [`.k65`]
- aliases: ["K65", "k65"]
- configuration: `./language-configuration.json`

1) activationEvents

- `onLanguage:k65`

Create `language-configuration.json`:

- lineComment: `;` (if that matches K65 expectations)
- brackets: `[]`, `{}`, `()`
- autoClosingPairs: same
- surroundingPairs: same

Acceptance for Phase 2:

- Opening a `.k65` file shows language mode `K65` and activates the extension.

## Phase 3 - Start the language client and spawn `k816 lsp`

Implement `src/extension.ts`:

1) Configuration keys (contributes.configuration)

- `k816.server.path` (string, default: `"k816"`)
- `k816.server.args` (array of strings, default: `["lsp"]`)
- `k816.server.env` (object map string->string, default empty)
- `k816.trace.server` (off/messages/verbose - optional passthrough)

1) ServerOptions

- command: from `k816.server.path`
- args: from `k816.server.args`
- env: merge `process.env` + `k816.server.env`
- transport: stdio

1) ClientOptions

- `documentSelector: [{ language: "k65" }]`
- `outputChannel`: dedicated output channel ("k816 LSP")
- optionally set `revealOutputChannelOn` to show errors only

1) Start/stop lifecycle

- `activate()` starts the client
- `deactivate()` stops the client (return the promise)

1) Basic failure UX

- If spawn fails (ENOENT), show a single actionable error:
  - “k816 not found. Set k816.server.path or ensure k816 is on PATH.”
- Do not spam repeated popups; use a “once per session” guard.

Acceptance for Phase 3:

- Opening a `.k65` file starts `k816 lsp`.
- Diagnostics published by the server appear in the editor Problems panel.

## Phase 4 - Add a restart command

1) Contribute a command:

- `k816.restartServer`

1) Implement:

- stop client if running
- start it again
- log actions to the output channel

Acceptance for Phase 4:

- Running “K816: Restart Language Server” reliably restarts the process.

## Phase 5 - Developer workflow polish (minimal)

1) `.vscode/launch.json`

- Extension Development Host configuration
- optional env: `RUST_LOG=k816_lsp=info`

1) README

- how to install dependencies
- how to run/debug (F5)
- how to set `k816.server.path`
- minimal troubleshooting: output channel + server logs on stderr

1) Optional `.vsix` packaging (local only)

- add `@vscode/vsce` instructions
- `vsce package` produces a local installable artifact

Acceptance for Phase 5:

- Another developer can run the extension in dev host from a clean checkout.

# Testing

## Manual smoke test (required)

- Open a folder containing `k816.toml`
- Open a `.k65` file with a known syntax error
- Verify:
  - language mode is K65
  - server starts (output channel shows startup)
  - diagnostics appear
  - go-to-definition/hover work if the Rust server implements them

## Automated smoke test (optional, simple)

- Use VS Code extension test runner to:
  - open a fixture workspace
  - open a `.k65` document
  - wait for diagnostics to appear via `vscode.languages.getDiagnostics(uri)`
- Keep it minimal; do not build a full integration harness.

# Deliverables checklist

- `editors/vscode-k816/package.json` with language contribution + configuration + activation + commands
- `editors/vscode-k816/src/extension.ts` spawning `k816 lsp` via stdio LanguageClient
- `editors/vscode-k816/language-configuration.json`
- `editors/vscode-k816/README.md`
- `.vscode/launch.json` for debugging
- Rust side: `k816 lsp` command mode stable enough for VS Code client

# Acceptance criteria

- Opening a `.k65` file in VS Code automatically starts `k816 lsp`.
- Diagnostics from `k816 lsp` show up without manual steps.
- Restart command works.
- No stdout logging corruption (LSP stream remains valid).

# References (copy/paste)

- VS Code Language Server Extension Guide:
  `https://code.visualstudio.com/api/language-extensions/language-server-extension-guide`
- VS Code “Your First Extension” (generator usage):
  `https://code.visualstudio.com/api/get-started/your-first-extension`
- VS Code Publishing / vsce tool:
  `https://code.visualstudio.com/api/working-with-extensions/publishing-extension`
- vscode-languageserver-node (client library home):
  `https://github.com/microsoft/vscode-languageserver-node`
