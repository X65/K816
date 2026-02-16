# k816 Language Support

VS Code extension for the K65 assembly language targeting the WDC 65C816 processor.

Provides syntax highlighting, language server integration, and debugging support
for the [K816](https://github.com/X65/K816) toolchain.

## Features

### Editor

- Syntax highlighting for K65 assembly (`.k65` files)
- Document outline and breadcrumb navigation (functions, labels, data blocks)
- Code formatting (Format Document, Format on Save)
- Hover information with symbol details
- Live diagnostics (errors and warnings as you type)
- LSP server status in the status bar

### Build

- **F7** to build the project (`k816 build`)
- Task provider with `build`, `clean`, and `run` tasks (Terminal > Run Task)
- Custom tasks via `tasks.json` for any k816 command (`compile`, `link`, `fmt`, etc.)

### Debugging

- Launch and debug programs via the emulator's DAP interface
- Source-level breakpoints with automatic address resolution
- Configurable emulator path and arguments

## Prerequisites

- The `k816` binary installed and on your PATH (or set `k816.server.path`)
- For debugging: the emulator binary (or set `k816.debugger.path`)

## Getting Started

1. Install the extension (`.vsix` or from the marketplace)
2. Open a folder containing `.k65` source files
3. The language server starts automatically
4. Press **F7** to build, or **F5** to debug

## Settings

### Language Server

| Setting | Default | Description |
|---------|---------|-------------|
| `k816.server.path` | `k816` | Path to the k816 executable |
| `k816.server.args` | `["lsp"]` | Arguments for the language server |
| `k816.server.env` | `{}` | Extra environment variables |
| `k816.trace.server` | `off` | Trace level: `off`, `messages`, `verbose` |

### Debugger

| Setting | Default | Description |
|---------|---------|-------------|
| `k816.debugger.path` | `emu` | Path to the emulator executable |
| `k816.debugger.args` | `["--dap"]` | Arguments for the debug adapter |
| `k816.debugger.env` | `{}` | Extra environment variables |
| `k816.trace.debugger` | `off` | DAP trace level: `off`, `messages`, `verbose` |

## Commands

| Command | Keybinding | Description |
|---------|------------|-------------|
| K816: Build | F7 | Build the project |
| K816: Restart Language Server | | Restart the LSP server |

## Debug Configuration

Add to `.vscode/launch.json`:

```json
{
    "type": "k816",
    "request": "launch",
    "name": "Debug K65 Program",
    "program": "${workspaceFolder}/target/output.xex"
}
```

| Property | Required | Description |
|----------|----------|-------------|
| `program` | yes | Path to the binary (`.xex`) to debug |
| `stopOnEntry` | no | Pause on the first instruction (default: `false`) |

## Troubleshooting

- Check the **k816 LSP** output channel for server logs
- Check the **k816 DAP** output channel for debugger communication (when tracing is enabled)
- If the server fails to start, verify `k816.server.path` points to a valid executable
- Click the k816 status bar item to restart the language server

## Development

```sh
cd editors/vscode-k816
npm install
npm run compile
```

Press **F5** in VS Code to launch an Extension Development Host.

```sh
npm run package
```

Creates a local `.vsix` artifact.

## License

0BSD
