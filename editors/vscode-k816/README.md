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

### Requirements

This extension requires that the `k816` binary is installed and available on your PATH, or that you configure the `k816.server.path` setting to point to the executable. The extension will automatically start the language server when you open a folder containing `.k65` files.

### Build and Run

- **F7** to build the project (`k816 build`)
- **Ctrl+F5** to build and run the project without debugging (`k816 run`;
  requires `[run].runner` in `k816.toml`)
- Task provider with `build`, `clean`, and `run` tasks (Terminal > Run Task)
- Custom tasks via `tasks.json` for any k816 command (`compile`, `link`, `fmt`, etc.)

### Debugging

- **F5** to launch and debug the project via the emulator's DAP interface
- Source-level breakpoints with automatic address resolution
- Disassembly view support (instruction pointer + disassemble request)
- Memory inspector support (read and write memory when adapter supports it)
- Inline debug values for registers and resolved symbol addresses/values
- Configurable emulator path and arguments

### Language Model Tools

- `#k816_lookup_instruction`: look up bundled 65816 mnemonic descriptions
- `#k816_query_memory_map`: query current linker memory-map usage from the
  workspace language server state

Example prompts in Copilot chat or agent mode:

- `Use #k816_lookup_instruction for ADC`
- `Use #k816_query_memory_map with detail=runs`
- `Use #k816_query_memory_map for memory_name=MAIN`

## Prerequisites

- VS Code `1.109.0` or newer
- The `k816` binary installed and on your PATH (or set `k816.server.path`)
- For debugging: the emulator binary (or set `k816.debugger.path`)
- For language model tools: GitHub Copilot chat/agent features enabled

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
| `k816.debugger.inlineValues` | `true` | Enable inline debug values while stopped |
| `k816.trace.debugger` | `off` | DAP trace level: `off`, `messages`, `verbose` |

## Commands

| Command | Keybinding | Description |
|---------|------------|-------------|
| K816: Build | F7 | Build the project |
| K816: Run (without debugging) | Ctrl+F5 | Build and run via the configured runner |
| K816: Restart Language Server | | Restart the LSP server |

## Debug Configuration

A minimal `.vscode/launch.json` is enough — the extension auto-resolves the
binary path by running `k816 metadata` in the workspace root and using the
reported `artifact.path`:

```json
{
    "type": "k816",
    "request": "launch",
    "name": "Debug K65 Program"
}
```

To override auto-resolution (e.g. to debug a different artifact), set
`program` explicitly:

```json
{
    "type": "k816",
    "request": "launch",
    "name": "Debug K65 Program",
    "program": "${workspaceFolder}/path/to/custom.xex"
}
```

| Property | Required | Description |
|----------|----------|-------------|
| `program` | no | Path to the binary (`.xex` or `.bin`) to debug. If omitted, resolved via `k816 metadata`. |
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
