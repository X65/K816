import { type ChildProcess, spawn } from "node:child_process";
import * as vscode from "vscode";
import {
  LanguageClient,
  type LanguageClientOptions,
  RevealOutputChannelOn,
  type ServerOptions,
  Trace,
} from "vscode-languageclient/node";

let client: LanguageClient | undefined;
let outputChannel: vscode.OutputChannel | undefined;
let spawnNotFoundShown = false;

export async function activate(
  context: vscode.ExtensionContext,
): Promise<void> {
  const channel = ensureOutputChannel();
  context.subscriptions.push(channel);

  context.subscriptions.push(
    vscode.commands.registerCommand("k816.restartServer", async () => {
      outputChannel?.appendLine("Restarting language server...");
      await stopClient();
      await startClient();
    }),
    vscode.commands.registerCommand("k816.build", () => {
      const config = vscode.workspace.getConfiguration("k816");
      const command = config.get<string>("server.path", "k816");
      const folder = vscode.workspace.workspaceFolders?.[0];
      const task = new vscode.Task(
        { type: "k816" },
        folder ?? vscode.TaskScope.Workspace,
        "build",
        "k816",
        new vscode.ShellExecution(command, ["build"], {
          cwd: folder?.uri.fsPath,
        }),
      );
      task.group = vscode.TaskGroup.Build;
      task.presentationOptions.reveal = vscode.TaskRevealKind.Always;
      vscode.tasks.executeTask(task);
    }),
  );

  context.subscriptions.push(
    vscode.debug.registerDebugAdapterDescriptorFactory(
      "k816",
      new K816DebugAdapterFactory(),
    ),
    vscode.debug.registerDebugConfigurationProvider(
      "k816",
      new K816DebugConfigProvider(),
    ),
    vscode.debug.registerDebugAdapterTrackerFactory(
      "k816",
      new K816DebugAdapterTrackerFactory(),
    ),
  );

  await startClient();
}

export function deactivate(): Thenable<void> | undefined {
  return stopClient();
}

async function startClient(): Promise<void> {
  if (client) {
    return;
  }

  const channel = ensureOutputChannel();
  const config = vscode.workspace.getConfiguration("k816");
  const command = config.get<string>("server.path", "k816");
  const args = config.get<string[]>("server.args", ["lsp"]);
  const env = resolveServerEnv(config);
  const trace = traceLevel(config.get<string>("trace.server", "off"));

  const serverOptions: ServerOptions = {
    command,
    args,
    options: { env },
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ language: "k65" }],
    outputChannel: channel,
    revealOutputChannelOn: RevealOutputChannelOn.Error,
  };

  const nextClient = new LanguageClient(
    "k816-lsp",
    "k816 Language Server",
    serverOptions,
    clientOptions,
  );

  try {
    await nextClient.start();
    void nextClient.setTrace(trace);
    client = nextClient;
    channel.appendLine(`Started: ${command} ${args.join(" ")}`.trim());
  } catch (error) {
    channel.appendLine(`Failed to start server: ${formatError(error)}`);
    if (isSpawnNotFound(error) && !spawnNotFoundShown) {
      spawnNotFoundShown = true;
      void vscode.window.showErrorMessage(
        "k816 not found. Set k816.server.path or ensure k816 is on PATH.",
      );
    }
  }
}

async function stopClient(): Promise<void> {
  if (!client) {
    return;
  }

  const running = client;
  client = undefined;
  await running.stop();
  outputChannel?.appendLine("Stopped language server.");
}

function ensureOutputChannel(): vscode.OutputChannel {
  if (!outputChannel) {
    outputChannel = vscode.window.createOutputChannel("k816 LSP");
  }
  return outputChannel;
}

function resolveServerEnv(
  config: vscode.WorkspaceConfiguration,
): NodeJS.ProcessEnv {
  const rawEnv = config.get<Record<string, unknown>>("server.env", {});
  const extraEnv: Record<string, string> = {};
  for (const [key, value] of Object.entries(rawEnv)) {
    if (typeof value === "string") {
      extraEnv[key] = value;
    }
  }
  return {
    ...process.env,
    ...extraEnv,
  };
}

function resolveDebuggerEnv(
  config: vscode.WorkspaceConfiguration,
): Record<string, string> {
  const base: Record<string, string> = {};
  for (const [key, value] of Object.entries(process.env)) {
    if (value !== undefined) {
      base[key] = value;
    }
  }
  const rawEnv = config.get<Record<string, unknown>>("debugger.env", {});
  for (const [key, value] of Object.entries(rawEnv)) {
    if (typeof value === "string") {
      base[key] = value;
    }
  }
  return base;
}

function traceLevel(value: string): Trace {
  switch (value) {
    case "messages":
      return Trace.Messages;
    case "verbose":
      return Trace.Verbose;
    default:
      return Trace.Off;
  }
}

function isSpawnNotFound(error: unknown): boolean {
  if (typeof error === "object" && error !== null && "code" in error) {
    return (error as { code?: string }).code === "ENOENT";
  }
  return formatError(error).includes("ENOENT");
}

function formatError(error: unknown): string {
  if (error instanceof Error) {
    return error.message;
  }
  return String(error);
}

class K816DebugAdapterProxy implements vscode.DebugAdapter {
  private _onDidSendMessage =
    new vscode.EventEmitter<vscode.DebugProtocolMessage>();
  readonly onDidSendMessage = this._onDidSendMessage.event;

  private process: ChildProcess | null = null;
  private rawBuffer = Buffer.alloc(0);
  private contentLength = -1;

  private nextSourceRef = 1;
  private sourceRefByPath = new Map<string, number>();

  private pendingBp = new Map<
    number,
    {
      originalSource: Record<string, unknown>;
      entries: Array<{
        originalLine: number;
        resolved: boolean;
        address?: number;
      }>;
    }
  >();

  constructor(
    private command: string,
    private args: string[],
    private env: Record<string, string>,
  ) {}

  handleMessage(message: any): void {
    if (!this.process) this.startProcess();

    if (
      message.type === "request" &&
      message.command === "setBreakpoints"
    ) {
      this.handleSetBreakpoints(message).catch(() => {
        this.sendToEmu(message);
      });
      return;
    }

    this.sendToEmu(message);
  }

  dispose(): void {
    if (this.process) {
      this.process.kill();
      this.process = null;
    }
    this._onDidSendMessage.dispose();
  }

  private startProcess(): void {
    this.process = spawn(this.command, this.args, {
      stdio: ["pipe", "pipe", "pipe"],
      env: this.env,
    });

    this.process.stdout?.on("data", (data: Buffer) => this.onData(data));

    this.process.stderr?.on("data", (data: Buffer) => {
      outputChannel?.appendLine(`[emu stderr] ${data.toString()}`);
    });

    this.process.on("error", (err) => {
      outputChannel?.appendLine(`[emu error] ${err.message}`);
      this._onDidSendMessage.fire({
        type: "event",
        event: "terminated",
        seq: 0,
      } as any);
    });

    this.process.on("exit", (code, signal) => {
      outputChannel?.appendLine(
        `[emu exit] code=${code} signal=${signal}`,
      );
      this._onDidSendMessage.fire({
        type: "event",
        event: "terminated",
        seq: 0,
      } as any);
    });
  }

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
      const body = this.rawBuffer
        .subarray(0, this.contentLength)
        .toString();
      this.rawBuffer = this.rawBuffer.subarray(this.contentLength);
      this.contentLength = -1;
      this.onEmuMessage(JSON.parse(body));
    }
  }

  private onEmuMessage(msg: any): void {
    if (
      msg.type === "response" &&
      msg.command === "setBreakpoints"
    ) {
      const pending = this.pendingBp.get(msg.request_seq);
      if (pending) {
        this.pendingBp.delete(msg.request_seq);

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
              message:
                "Cannot resolve memory address for this line",
            });
          }
        }
        msg.body = { breakpoints: translated };
      }
    }

    this._onDidSendMessage.fire(msg);
  }

  private async handleSetBreakpoints(request: any): Promise<void> {
    const source = request.arguments?.source ?? {};
    const lines: number[] = request.arguments?.lines ?? [];
    const filePath: string | undefined = source.path;

    if (!filePath || !client) {
      this.sendToEmu(request);
      return;
    }

    const uri = vscode.Uri.file(filePath).toString();
    const lspLines = lines.map((l: number) => l - 1);

    const result = await client.sendRequest<{
      addresses: (number | null)[];
    }>("k816/resolveAddresses", { uri, lines: lspLines });

    const sourceRef = this.getSourceRef(filePath);
    const entries: Array<{
      originalLine: number;
      resolved: boolean;
      address?: number;
    }> = [];
    const resolvedLines: number[] = [];

    for (let i = 0; i < lines.length; i++) {
      const addr = result.addresses[i];
      if (addr !== null && addr !== undefined) {
        entries.push({
          originalLine: lines[i],
          resolved: true,
          address: addr,
        });
        resolvedLines.push(addr);
      } else {
        entries.push({ originalLine: lines[i], resolved: false });
      }
    }

    this.pendingBp.set(request.seq, {
      originalSource: source,
      entries,
    });

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

  private getSourceRef(path: string): number {
    let ref = this.sourceRefByPath.get(path);
    if (ref === undefined) {
      ref = this.nextSourceRef++;
      this.sourceRefByPath.set(path, ref);
    }
    return ref;
  }
}

class K816DebugAdapterFactory
  implements vscode.DebugAdapterDescriptorFactory
{
  createDebugAdapterDescriptor(
    session: vscode.DebugSession,
  ): vscode.ProviderResult<vscode.DebugAdapterDescriptor> {
    const config = vscode.workspace.getConfiguration("k816");
    const command = config.get<string>("debugger.path", "emu");
    const configuredArgs = config.get<string[]>("debugger.args", [
      "--dap",
    ]);
    const program = session.configuration.program;
    const execArgs = program
      ? [...configuredArgs, program]
      : configuredArgs;
    const env = resolveDebuggerEnv(config);

    const proxy = new K816DebugAdapterProxy(command, execArgs, env);
    return new vscode.DebugAdapterInlineImplementation(proxy);
  }
}

class K816DebugAdapterTrackerFactory
  implements vscode.DebugAdapterTrackerFactory
{
  private channel: vscode.OutputChannel | undefined;

  createDebugAdapterTracker(
    _session: vscode.DebugSession,
  ): vscode.ProviderResult<vscode.DebugAdapterTracker> {
    const level = vscode.workspace
      .getConfiguration("k816")
      .get<string>("trace.debugger", "off");
    if (level === "off") {
      return undefined;
    }
    const verbose = level === "verbose";
    if (!this.channel) {
      this.channel = vscode.window.createOutputChannel("k816 DAP");
    }
    const ch = this.channel;
    ch.show(true);
    return {
      onWillReceiveMessage(msg: unknown) {
        ch.appendLine(
          `[VSCode → emu] ${verbose ? JSON.stringify(msg, undefined, 2) : JSON.stringify(msg)}`,
        );
      },
      onDidSendMessage(msg: unknown) {
        ch.appendLine(
          `[emu → VSCode] ${verbose ? JSON.stringify(msg, undefined, 2) : JSON.stringify(msg)}`,
        );
      },
      onError(error: Error) {
        ch.appendLine(`[error] ${error.message}`);
      },
      onExit(code: number | undefined, signal: string | undefined) {
        ch.appendLine(`[exit] code=${code} signal=${signal}`);
      },
    };
  }
}

class K816DebugConfigProvider implements vscode.DebugConfigurationProvider {
  async resolveDebugConfiguration(
    _folder: vscode.WorkspaceFolder | undefined,
    config: vscode.DebugConfiguration,
  ): Promise<vscode.DebugConfiguration | undefined> {
    if (!config.type && !config.request && !config.name) {
      const editor = vscode.window.activeTextEditor;
      if (editor && editor.document.languageId === "k65") {
        config.type = "k816";
        config.request = "launch";
        config.name = "Debug K65 Program";
      }
    }
    if (config.type === "k816" && !config.program) {
      const uris = await vscode.window.showOpenDialog({
        canSelectFiles: true,
        canSelectMany: false,
        title: "Select program binary to debug",
        filters: { "XEX binaries": ["xex"], "All files": ["*"] },
      });
      if (!uris || uris.length === 0) {
        return undefined;
      }
      config.program = uris[0].fsPath;
    }
    return config;
  }
}
