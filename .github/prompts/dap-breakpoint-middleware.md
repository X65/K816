# DAP Breakpoint Middleware — Source Lines → Memory Addresses

## Context

The emu DAP server treats "line numbers" in `setBreakpoints` as raw 24-bit memory addresses and requires a `sourceReference` integer to group breakpoints per file. It knows nothing about source code. VSCode, on the other hand, sends breakpoints as source file paths + line numbers. The extension must become a DAP **proxy** — intercepting `setBreakpoints` requests from VSCode, translating source lines to memory addresses via the k816 LSP (which already has per-instruction address resolution via `resolved_sites`), and forwarding modified requests to emu.

The per-instruction address mapping infrastructure (from the previous plan) is already implemented and working: `DocumentState.resolved_sites: Vec<(Span, u32)>` is populated after linking and supports O(log n) lookup via `address_at_offset()`.

## Architecture

```
VSCode                      Extension (proxy)                    Emu
  │                              │                                │
  │ setBreakpoints               │                                │
  │ source: {path: "main.k65"}   │                                │
  │ lines: [10, 25]              │                                │
  ├─────────────────────────────►│                                │
  │                              │ k816/resolveAddresses          │
  │                              │ uri, lines:[9,24] (0-indexed)  │
  │                              ├───► LSP ───► [0x8000, 0x8015]  │
  │                              │                                │
  │                              │ setBreakpoints                 │
  │                              │ source: {sourceReference: 1}   │
  │                              │ lines: [0x8000, 0x8015]        │
  │                              ├───────────────────────────────►│
  │                              │                                │
  │                              │◄───────────────────────────────┤
  │                              │ response: verified, id=addr    │
  │                              │                                │
  │◄─────────────────────────────┤                                │
  │ response: verified,          │                                │
  │   line=10, source=main.k65   │                                │
```

## Changes

### 1. `crates/lsp/src/lib.rs` — custom `k816/resolveAddresses` request

Add a new LSP request that translates source line numbers to memory addresses. This reuses the existing `resolved_sites` and `LineIndex` infrastructure.

**Request/response types** (add `Serialize` import from serde):

```rust
#[derive(Debug, Deserialize)]
struct ResolveAddressesParams {
    uri: String,
    lines: Vec<u32>,  // 0-indexed line numbers
}

#[derive(Debug, Serialize)]
struct ResolveAddressesResult {
    addresses: Vec<Option<u32>>,  // parallel array; null = no addressable site on this line
}
```

**Dispatch** — add to `handle_request()` match:

```rust
"k816/resolveAddresses" => self.on_resolve_addresses(request),
```

**Handler** on `Server`:

```rust
fn on_resolve_addresses(&mut self, request: Request) -> Result<()> {
    let params: ResolveAddressesParams = serde_json::from_value(request.params)?;
    let uri = Uri::from_str(&params.uri)?;
    self.ensure_document_fresh(&uri)?;
    let result = self.state.resolve_addresses_for_lines(&uri, &params.lines);
    self.send_result(request.id, &result)
}
```

**Resolution logic** on `ServerState` — for each line, find the first `resolved_site` whose span starts on that line:

```rust
fn resolve_addresses_for_lines(&self, uri: &Uri, lines: &[u32]) -> ResolveAddressesResult {
    let Some(doc) = self.documents.get(uri) else {
        return ResolveAddressesResult { addresses: lines.iter().map(|_| None).collect() };
    };
    let addresses = lines.iter().map(|&line| {
        let line = line as usize;
        if line >= doc.line_index.line_starts.len() {
            return None;
        }
        let line_start = doc.line_index.line_starts[line];
        let line_end = if line + 1 < doc.line_index.line_starts.len() {
            doc.line_index.line_starts[line + 1]
        } else {
            doc.text.len()
        };
        // Binary search: first site whose span.start >= line_start
        let idx = doc.resolved_sites.partition_point(|(span, _)| span.start < line_start);
        if idx < doc.resolved_sites.len() {
            let (span, addr) = &doc.resolved_sites[idx];
            if span.start < line_end {
                return Some(*addr);
            }
        }
        None
    }).collect();
    ResolveAddressesResult { addresses }
}
```

Note: `line_starts` is accessible because `resolve_addresses_for_lines` is in the same module as `LineIndex`.

### 2. `editors/vscode-k816/src/extension.ts` — DAP proxy

Replace `DebugAdapterExecutable` with `DebugAdapterInlineImplementation` wrapping a proxy that spawns emu, intercepts `setBreakpoints`, and translates via LSP.

#### 2a. Import `child_process`

```typescript
import { ChildProcess, spawn } from "child_process";
```

#### 2b. `K816DebugAdapterProxy` class

Implements `vscode.DebugAdapter`. Core responsibilities:

1. **Spawn emu** as child process with DAP over stdio
2. **DAP wire framing** — Content-Length headers (same as LSP wire protocol)
3. **Intercept `setBreakpoints`** — resolve lines via LSP, translate, forward
4. **Translate responses** — re-attach original source/line info
5. **Pass-through** all other messages

```typescript
class K816DebugAdapterProxy implements vscode.DebugAdapter {
  private _onDidSendMessage = new vscode.EventEmitter<vscode.DebugProtocolMessage>();
  readonly onDidSendMessage = this._onDidSendMessage.event;

  private process: ChildProcess | null = null;
  private rawBuffer = Buffer.alloc(0);
  private contentLength = -1;

  private nextSourceRef = 1;
  private sourceRefByPath = new Map<string, number>();

  // Pending setBreakpoints translations keyed by request seq
  private pendingBp = new Map<number, {
    originalSource: Record<string, unknown>;
    entries: Array<{ originalLine: number; resolved: boolean; address?: number }>;
  }>();

  constructor(
    private command: string,
    private args: string[],
    private env: Record<string, string>,
  ) {}

  handleMessage(message: any): void { ... }
  dispose(): void { ... }

  // --- internals ---
  private startProcess(): void { ... }        // spawn + wire stdout/stderr
  private sendToEmu(msg: any): void { ... }   // Content-Length framed write to stdin
  private onData(data: Buffer): void { ... }  // parse Content-Length frames from stdout
  private onEmuMessage(msg: any): void { ... } // translate responses, fire event

  private async handleSetBreakpoints(request: any): Promise<void> { ... }
  private getSourceRef(path: string): number { ... }
}
```

**`handleMessage`** — entry point from VSCode:

```typescript
handleMessage(message: any): void {
  if (!this.process) this.startProcess();

  if (message.type === "request" && message.command === "setBreakpoints") {
    this.handleSetBreakpoints(message).catch(() => {
      // Fallback: forward unmodified (emu will reject without sourceReference)
      this.sendToEmu(message);
    });
    return;
  }

  this.sendToEmu(message);
}
```

**`handleSetBreakpoints`** — the core translation:

```typescript
private async handleSetBreakpoints(request: any): Promise<void> {
  const source = request.arguments?.source ?? {};
  const lines: number[] = request.arguments?.lines ?? [];
  const filePath: string | undefined = source.path;

  if (!filePath || !client) {
    this.sendToEmu(request);  // can't resolve, pass through
    return;
  }

  const uri = vscode.Uri.file(filePath).toString();
  // DAP lines are 1-indexed; LSP lines are 0-indexed
  const lspLines = lines.map((l: number) => l - 1);

  const result = await client.sendRequest<{ addresses: (number | null)[] }>(
    "k816/resolveAddresses",
    { uri, lines: lspLines },
  );

  const sourceRef = this.getSourceRef(filePath);
  const entries: Array<{ originalLine: number; resolved: boolean; address?: number }> = [];
  const resolvedLines: number[] = [];

  for (let i = 0; i < lines.length; i++) {
    const addr = result.addresses[i];
    if (addr !== null && addr !== undefined) {
      entries.push({ originalLine: lines[i], resolved: true, address: addr });
      resolvedLines.push(addr);
    } else {
      entries.push({ originalLine: lines[i], resolved: false });
    }
  }

  this.pendingBp.set(request.seq, { originalSource: source, entries });

  // Forward with addresses instead of lines, sourceReference instead of path
  this.sendToEmu({
    ...request,
    arguments: {
      ...request.arguments,
      source: { sourceReference: sourceRef },
      lines: resolvedLines,
      breakpoints: resolvedLines.map((addr) => ({ line: addr })),
    },
  });
}
```

**`onEmuMessage`** — translate `setBreakpoints` responses back:

```typescript
private onEmuMessage(msg: any): void {
  if (msg.type === "response" && msg.command === "setBreakpoints") {
    const pending = this.pendingBp.get(msg.request_seq);
    if (pending) {
      this.pendingBp.delete(msg.request_seq);

      // Rebuild breakpoints array in original order
      const translated: any[] = [];
      let emuIdx = 0;
      for (const entry of pending.entries) {
        if (entry.resolved) {
          const emuBp = msg.body?.breakpoints?.[emuIdx++] ?? {};
          translated.push({
            ...emuBp,
            source: pending.originalSource,
            line: entry.originalLine,
          });
        } else {
          translated.push({
            verified: false,
            line: entry.originalLine,
            source: pending.originalSource,
            message: "Cannot resolve memory address for this line",
          });
        }
      }
      msg.body = { breakpoints: translated };
    }
  }

  this._onDidSendMessage.fire(msg);
}
```

**DAP stdio framing** — identical to LSP wire protocol:

```typescript
private sendToEmu(msg: any): void {
  const json = JSON.stringify(msg);
  const header = `Content-Length: ${Buffer.byteLength(json)}\r\n\r\n`;
  this.process?.stdin?.write(header + json);
}

private onData(data: Buffer): void {
  this.rawBuffer = Buffer.concat([this.rawBuffer, data]);
  while (true) {
    if (this.contentLength < 0) {
      const idx = this.rawBuffer.indexOf("\r\n\r\n");
      if (idx < 0) break;
      const header = this.rawBuffer.subarray(0, idx).toString();
      const match = header.match(/Content-Length:\s*(\d+)/i);
      if (!match) break;
      this.contentLength = parseInt(match[1], 10);
      this.rawBuffer = this.rawBuffer.subarray(idx + 4);
    }
    if (this.rawBuffer.length < this.contentLength) break;
    const body = this.rawBuffer.subarray(0, this.contentLength).toString();
    this.rawBuffer = this.rawBuffer.subarray(this.contentLength);
    this.contentLength = -1;
    this.onEmuMessage(JSON.parse(body));
  }
}
```

#### 2c. Update `K816DebugAdapterFactory`

Change from `DebugAdapterExecutable` to inline proxy:

```typescript
class K816DebugAdapterFactory implements vscode.DebugAdapterDescriptorFactory {
  createDebugAdapterDescriptor(
    session: vscode.DebugSession,
  ): vscode.ProviderResult<vscode.DebugAdapterDescriptor> {
    const config = vscode.workspace.getConfiguration("k816");
    const command = config.get<string>("debugger.path", "emu");
    const configuredArgs = config.get<string[]>("debugger.args", ["--dap"]);
    const program = session.configuration.program;
    const execArgs = program ? [...configuredArgs, program] : configuredArgs;
    const env = resolveDebuggerEnv(config);

    const proxy = new K816DebugAdapterProxy(command, execArgs, env);
    return new vscode.DebugAdapterInlineImplementation(proxy);
  }
}
```

## Files to modify

| File | Change |
| --- | --- |
| `crates/lsp/src/lib.rs` | Add `k816/resolveAddresses` custom request: types, dispatch, handler, resolution logic |
| `editors/vscode-k816/src/extension.ts` | Add `K816DebugAdapterProxy` class, change factory to use inline implementation |

## Key details

- **Line indexing**: DAP uses 1-indexed lines, LSP/LineIndex uses 0-indexed. Subtract 1 when calling LSP.
- **sourceReference**: Emu requires this integer to group breakpoints per file. Proxy assigns a monotonic counter per file path.
- **Unresolved lines**: Lines without addressable instructions (comments, empty lines, directives) get `verified: false` in the response without being sent to emu.
- **Empty breakpoints**: When VSCode sends `setBreakpoints` with empty lines (to clear), proxy still forwards with the sourceReference to clear emu's breakpoints for that file.
- **Process lifecycle**: Proxy spawns emu in `handleMessage` on first call. `dispose()` kills the process. Process exit/error fires DAP `terminated` event.
- **Existing tracker**: `K816DebugAdapterTrackerFactory` stays for logging. It sees original VSCode requests and translated responses.

## Verification

1. `cargo build` — LSP compiles with new request handler
2. `cargo test` — existing tests pass
3. `cd editors/vscode-k816 && npm run compile` — TypeScript compiles
4. Manual test: Set a breakpoint in a .k65 file on an instruction line → emu receives the memory address → breakpoint verified
5. Set a breakpoint on a comment/empty line → shows as unverified
6. Check DAP trace output (`k816.trace.debugger: verbose`) to see translated messages
