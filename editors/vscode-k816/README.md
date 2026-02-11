# k816 VS Code Extension

Minimal VS Code client for the `k816 lsp` language server.

## Prerequisites

- `k816` binary is installed and callable from your shell (or configure `k816.server.path`).
- Node.js and npm installed.
- VS Code installed.

## Development

```sh
cd editors/vscode-k816
npm install
npm run compile
```

Open `editors/vscode-k816` in VS Code and press `F5` to launch an Extension Development Host.

## Settings

- `k816.server.path`: executable path (default: `k816`)
- `k816.server.args`: startup args (default: `["lsp"]`)
- `k816.server.env`: extra env map merged with `process.env` (for example `{ "RUST_LOG": "k816_lsp=info" }`)
- `k816.trace.server`: `off`, `messages`, or `verbose`

## Command

- `K816: Restart Language Server` (`k816.restartServer`)

## Troubleshooting

- Open the output channel `k816 LSP` to inspect client logs.
- Server logs are emitted on the server stderr stream.
- If startup fails with executable not found, set `k816.server.path`.

## Optional local packaging

```sh
npm run package
```

This runs `vsce package` and creates a local `.vsix` artifact.
