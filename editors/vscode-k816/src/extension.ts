import { type ChildProcess, spawn } from "node:child_process";
import { readFileSync } from "node:fs";
import * as path from "node:path";
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
let lspStatusItem: vscode.StatusBarItem | undefined;
let fileInfoItem: vscode.StatusBarItem | undefined;

const LOOKUP_INSTRUCTION_TOOL = "k816_lookup_instruction";
const QUERY_MEMORY_MAP_TOOL = "k816_query_memory_map";
const QUERY_MEMORY_MAP_METHOD = "k816/queryMemoryMap";
const RESOLVE_ADDRESSES_METHOD = "k816/resolveAddresses";
const RESOLVE_INLINE_SYMBOLS_METHOD = "k816/resolveInlineSymbols";
const INSTRUCTION_DATA_FILE = "resources/instructions-description.json";
const INLINE_VALUE_CACHE_TTL_MS = 150;

type MemoryMapDetail = "summary" | "runs";

interface LookupInstructionInput {
  mnemonic: string;
}

interface QueryMemoryMapInput {
  memory_name?: string;
  detail?: MemoryMapDetail;
}

interface QueryMemoryMapMemoryRow {
  name: string;
  start: number;
  size: number;
  kind: string;
  used: number;
  free: number;
  utilization_percent: number;
}

interface QueryMemoryMapRunRow {
  memory_name: string;
  start: number;
  end: number;
  size: number;
}

interface QueryMemoryMapResult {
  status: "ok" | "unavailable";
  reason?: string;
  memories: QueryMemoryMapMemoryRow[];
  runs: QueryMemoryMapRunRow[];
}

interface ResolveAddressesResult {
  addresses: Array<number | null>;
}

interface InlineSymbolRow {
  name: string;
  category: string;
  start_line: number;
  start_character: number;
  end_line: number;
  end_character: number;
  address: number;
  read_size_hint?: number;
}

interface ResolveInlineSymbolsResult {
  symbols: InlineSymbolRow[];
}

export async function activate(
  context: vscode.ExtensionContext,
): Promise<void> {
  const channel = ensureOutputChannel();
  context.subscriptions.push(channel);

  lspStatusItem = vscode.window.createStatusBarItem(
    vscode.StatusBarAlignment.Left,
    100,
  );
  lspStatusItem.command = "k816.restartServer";
  setLspStatus("stopped");
  context.subscriptions.push(lspStatusItem);

  fileInfoItem = vscode.window.createStatusBarItem(
    vscode.StatusBarAlignment.Right,
    100,
  );
  context.subscriptions.push(fileInfoItem);

  context.subscriptions.push(
    vscode.window.onDidChangeActiveTextEditor((editor) =>
      updateFileInfo(editor),
    ),
    vscode.window.onDidChangeTextEditorSelection((e) =>
      updateFileInfo(e.textEditor),
    ),
  );
  updateFileInfo(vscode.window.activeTextEditor);
  registerLanguageModelTools(context);

  const taskProvider = new K816TaskProvider();
  context.subscriptions.push(
    vscode.commands.registerCommand("k816.restartServer", async () => {
      outputChannel?.appendLine("Restarting language server...");
      await stopClient();
      await startClient();
    }),
    vscode.commands.registerCommand("k816.showCodeLensInfo", async () => {}),
    vscode.commands.registerCommand("k816.build", async () => {
      const tasks = await vscode.tasks.fetchTasks({ type: "k816" });
      const buildTask = tasks.find(
        (t) => t.group === vscode.TaskGroup.Build,
      );
      if (buildTask) {
        vscode.tasks.executeTask(buildTask);
      }
    }),
    vscode.tasks.registerTaskProvider("k816", taskProvider),
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
    vscode.languages.registerInlineValuesProvider(
      { language: "k65" },
      new K816InlineValuesProvider(),
    ),
  );

  await startClient();
}

export function deactivate(): Thenable<void> | undefined {
  return stopClient();
}

function registerLanguageModelTools(
  context: vscode.ExtensionContext,
): void {
  const descriptions = loadInstructionDescriptions(context);

  context.subscriptions.push(
    vscode.lm.registerTool<LookupInstructionInput>(LOOKUP_INSTRUCTION_TOOL, {
      prepareInvocation(options) {
        const mnemonic = normalizeMnemonic(options.input.mnemonic);
        return {
          invocationMessage: mnemonic
            ? `Looking up ${mnemonic} in the 65816 instruction reference`
            : "Looking up 65816 instruction information",
        };
      },
      invoke(options) {
        const mnemonic = normalizeMnemonic(options.input.mnemonic);
        if (!mnemonic) {
          return toolTextResult(
            "Missing `mnemonic`. Provide a 65816 opcode name like `LDA`, `ADC`, or `BRA`.",
          );
        }

        const description = descriptions[mnemonic];
        if (!description) {
          return toolTextResult(
            `No entry found for \`${mnemonic}\` in the bundled 65816 instruction reference.`,
          );
        }

        return toolTextResult(`### ${mnemonic}\n${description}`);
      },
    }),
    vscode.lm.registerTool<QueryMemoryMapInput>(QUERY_MEMORY_MAP_TOOL, {
      prepareInvocation(options) {
        const detail = normalizeMemoryMapDetail(options.input.detail);
        const memoryName = normalizeMemoryName(options.input.memory_name);
        const detailText = detail === "runs" ? "with run details" : "summary";
        return {
          invocationMessage: memoryName
            ? `Querying memory map for ${memoryName} (${detailText})`
            : `Querying workspace memory map (${detailText})`,
        };
      },
      async invoke(options) {
        await startClient();
        if (!client) {
          return toolTextResult(
            "k816 language server is not running. Set `k816.server.path` to a valid k816 binary and retry.",
          );
        }

        const input: QueryMemoryMapInput = {
          memory_name: normalizeMemoryName(options.input.memory_name),
          detail: normalizeMemoryMapDetail(options.input.detail),
        };

        try {
          const result = await client.sendRequest<QueryMemoryMapResult>(
            QUERY_MEMORY_MAP_METHOD,
            input,
          );
          return toolTextResult(formatMemoryMapResult(result));
        } catch (error) {
          if (isUnsupportedRequestError(error)) {
            return toolTextResult(
              "The connected k816 language server does not support `k816/queryMemoryMap` yet. Update the k816 toolchain and restart the language server.",
            );
          }
          return toolTextResult(
            `Failed to query memory map from k816 language server: ${formatError(error)}`,
          );
        }
      },
    }),
  );
}

function loadInstructionDescriptions(
  context: vscode.ExtensionContext,
): Record<string, string> {
  const filePath = path.join(context.extensionPath, INSTRUCTION_DATA_FILE);
  try {
    const raw = readFileSync(filePath, "utf8");
    const parsed = JSON.parse(raw) as Record<string, unknown>;
    const descriptions: Record<string, string> = {};
    for (const [key, value] of Object.entries(parsed)) {
      if (typeof value === "string") {
        descriptions[key.toUpperCase()] = value;
      }
    }
    return descriptions;
  } catch (error) {
    ensureOutputChannel().appendLine(
      `Failed to load instruction data from '${filePath}': ${formatError(error)}`,
    );
    return {};
  }
}

function normalizeMnemonic(value: string | undefined): string | undefined {
  if (typeof value !== "string") {
    return undefined;
  }
  const normalized = value.trim().toUpperCase();
  return normalized.length > 0 ? normalized : undefined;
}

function normalizeMemoryName(value: string | undefined): string | undefined {
  if (typeof value !== "string") {
    return undefined;
  }
  const normalized = value.trim();
  return normalized.length > 0 ? normalized : undefined;
}

function normalizeMemoryMapDetail(value: string | undefined): MemoryMapDetail {
  return value === "runs" ? "runs" : "summary";
}

function formatMemoryMapResult(result: QueryMemoryMapResult): string {
  if (result.status !== "ok") {
    return result.reason ?? "Memory map is currently unavailable.";
  }

  if (result.memories.length === 0) {
    return "No memory areas matched the query.";
  }

  const lines: string[] = [
    "### Memory Areas",
    "",
    "| Name | Kind | Start | Size | Used | Free | Utilization |",
    "| --- | --- | ---: | ---: | ---: | ---: | ---: |",
  ];

  for (const memory of result.memories) {
    lines.push(
      `| ${memory.name} | ${memory.kind} | ${formatHex(memory.start, 6)} | ${formatByteCount(memory.size)} | ${formatByteCount(memory.used)} | ${formatByteCount(memory.free)} | ${memory.utilization_percent.toFixed(2)}% |`,
    );
  }

  if (result.runs.length > 0) {
    lines.push("", "### Linked Runs", "", "| Memory | Range | Size |", "| --- | --- | ---: |");
    for (const run of result.runs) {
      lines.push(
        `| ${run.memory_name} | ${formatHex(run.start, 6)}-${formatHex(run.end, 6)} | ${formatByteCount(run.size)} |`,
      );
    }
  }

  return lines.join("\n");
}

function formatByteCount(value: number): string {
  return `${value} (${formatHex(value)})`;
}

function formatHex(value: number, width = 0): string {
  const hex = Math.max(0, value).toString(16).toUpperCase();
  return `$${hex.padStart(width, "0")}`;
}

function toolTextResult(text: string): vscode.LanguageModelToolResult {
  return new vscode.LanguageModelToolResult([new vscode.LanguageModelTextPart(text)]);
}

function isUnsupportedRequestError(error: unknown): boolean {
  const message = formatError(error).toLowerCase();
  return message.includes("unsupported request") || message.includes("-32601");
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

  setLspStatus("starting");

  try {
    await nextClient.start();
    void nextClient.setTrace(trace);
    client = nextClient;
    channel.appendLine(`Started: ${command} ${args.join(" ")}`.trim());
    setLspStatus("running");
  } catch (error) {
    channel.appendLine(`Failed to start server: ${formatError(error)}`);
    setLspStatus("error", formatError(error));
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
  setLspStatus("stopped");
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

type LspStatus = "starting" | "running" | "stopped" | "error";

function setLspStatus(status: LspStatus, detail?: string): void {
  if (!lspStatusItem) return;
  const config = vscode.workspace.getConfiguration("k816");
  const command = config.get<string>("server.path", "k816");
  switch (status) {
    case "starting":
      lspStatusItem.text = "$(loading~spin) k816";
      lspStatusItem.tooltip = `k816 Language Server: starting\n${command}`;
      lspStatusItem.backgroundColor = undefined;
      break;
    case "running":
      lspStatusItem.text = "$(check) k816";
      lspStatusItem.tooltip = `k816 Language Server: running\n${command}`;
      lspStatusItem.backgroundColor = undefined;
      break;
    case "error":
      lspStatusItem.text = "$(error) k816";
      lspStatusItem.tooltip = `k816 Language Server: error\n${detail ?? command}`;
      lspStatusItem.backgroundColor = new vscode.ThemeColor(
        "statusBarItem.errorBackground",
      );
      break;
    case "stopped":
      lspStatusItem.text = "$(circle-slash) k816";
      lspStatusItem.tooltip = "k816 Language Server: stopped\nClick to restart";
      lspStatusItem.backgroundColor = undefined;
      break;
  }
  lspStatusItem.show();
}

let fileInfoPending = false;

function updateFileInfo(editor: vscode.TextEditor | undefined): void {
  if (!fileInfoItem) return;
  if (!editor || editor.document.languageId !== "k65") {
    fileInfoItem.hide();
    return;
  }

  fileInfoItem.text = "$(file-code) K65";
  fileInfoItem.tooltip = editor.document.fileName;
  fileInfoItem.show();

  if (!client || fileInfoPending) return;

  const line = editor.selection.active.line;
  const uri = editor.document.uri.toString();

  fileInfoPending = true;
  client
    .sendRequest<ResolveAddressesResult>(RESOLVE_ADDRESSES_METHOD, {
      uri,
      lines: [line],
    })
    .then((result) => {
      const addr = result.addresses[0];
      if (addr !== null && addr !== undefined && fileInfoItem) {
        const hex = addr.toString(16).toUpperCase().padStart(4, "0");
        fileInfoItem.text = `$(file-code) K65  $${hex}`;
      }
    })
    .catch(() => {})
    .finally(() => {
      fileInfoPending = false;
    });
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

interface K816TaskDefinition extends vscode.TaskDefinition {
  task: string;
}

class K816TaskProvider implements vscode.TaskProvider {
  static readonly type = "k816";

  provideTasks(): vscode.Task[] {
    const config = vscode.workspace.getConfiguration("k816");
    const command = config.get<string>("server.path", "k816");
    const folder = vscode.workspace.workspaceFolders?.[0];
    const scope = folder ?? vscode.TaskScope.Workspace;
    const cwd = folder?.uri.fsPath;

    const build = this.createTask(scope, command, "build", cwd);
    build.group = vscode.TaskGroup.Build;
    build.presentationOptions.reveal = vscode.TaskRevealKind.Always;

    const clean = this.createTask(scope, command, "clean", cwd);
    clean.group = vscode.TaskGroup.Clean;

    const run = this.createTask(scope, command, "run", cwd);

    return [build, clean, run];
  }

  resolveTask(task: vscode.Task): vscode.Task | undefined {
    const definition = task.definition as K816TaskDefinition;
    if (definition.type !== K816TaskProvider.type || !definition.task) {
      return undefined;
    }
    const config = vscode.workspace.getConfiguration("k816");
    const command = config.get<string>("server.path", "k816");
    const folder = vscode.workspace.workspaceFolders?.[0];
    const cwd = folder?.uri.fsPath;
    return this.createTask(
      task.scope ?? vscode.TaskScope.Workspace,
      command,
      definition.task,
      cwd,
    );
  }

  private createTask(
    scope: vscode.WorkspaceFolder | vscode.TaskScope,
    command: string,
    taskName: string,
    cwd: string | undefined,
  ): vscode.Task {
    const definition: K816TaskDefinition = {
      type: K816TaskProvider.type,
      task: taskName,
    };
    return new vscode.Task(
      definition,
      scope,
      taskName,
      "k816",
      new vscode.ShellExecution(command, [taskName], { cwd }),
    );
  }
}

interface ReadMemoryResponse {
  address?: string;
  data?: string;
  unreadableBytes?: number;
}

class K816InlineValuesProvider implements vscode.InlineValuesProvider {
  private symbolCache = new Map<
    string,
    { timestamp: number; rows: InlineSymbolRow[] }
  >();
  private memoryCache = new Map<
    string,
    { timestamp: number; bytes: Uint8Array }
  >();

  async provideInlineValues(
    document: vscode.TextDocument,
    viewPort: vscode.Range,
    context: vscode.InlineValueContext,
    token: vscode.CancellationToken,
  ): Promise<vscode.InlineValue[]> {
    if (
      !vscode.workspace
        .getConfiguration("k816")
        .get<boolean>("debugger.inlineValues", true)
    ) {
      return [];
    }

    const session = vscode.debug.activeDebugSession;
    if (!session || session.type !== "k816") {
      return [];
    }

    await startClient();
    if (!client) {
      return [];
    }

    const startLine = Math.max(0, viewPort.start.line);
    const endLine = Math.max(startLine, viewPort.end.line);
    const rows = await this.resolveInlineSymbols(
      document,
      startLine,
      endLine,
      token,
    );
    if (token.isCancellationRequested) {
      return [];
    }

    const values: vscode.InlineValue[] = [];
    const stopLine = context.stoppedLocation.start.line;
    const evalRange = new vscode.Range(stopLine, 0, stopLine, 0);
    for (const expression of [
      "reg.A",
      "reg.B",
      "reg.C",
      "reg.X",
      "reg.Y",
      "reg.S",
      "reg.PC",
      "reg.P",
      "reg.D",
      "reg.DBR",
      "reg.PBR",
    ]) {
      values.push(
        new vscode.InlineValueEvaluatableExpression(evalRange, expression),
      );
    }

    for (const row of rows) {
      if (token.isCancellationRequested) {
        return values;
      }
      const range = new vscode.Range(
        row.start_line,
        row.start_character,
        row.end_line,
        row.end_character,
      );
      const addressText = formatHex(row.address, 6);
      let text = `${row.name}: ${addressText}`;
      if (row.read_size_hint === 1 || row.read_size_hint === 2) {
        const bytes = await this.readMemoryValue(
          session,
          context.frameId,
          row.address,
          row.read_size_hint,
          token,
        );
        if (bytes && bytes.length >= row.read_size_hint) {
          const formatted =
            row.read_size_hint === 1
              ? `$${bytes[0].toString(16).toUpperCase().padStart(2, "0")}`
              : `$${((bytes[1] << 8) | bytes[0]).toString(16).toUpperCase().padStart(4, "0")}`;
          text = `${row.name}: ${formatted} @ ${addressText}`;
        }
      }
      values.push(new vscode.InlineValueText(range, text));
    }

    return values;
  }

  private async resolveInlineSymbols(
    document: vscode.TextDocument,
    startLine: number,
    endLine: number,
    token: vscode.CancellationToken,
  ): Promise<InlineSymbolRow[]> {
    if (!client) {
      return [];
    }
    const cacheKey = `${document.uri.toString()}:${document.version}:${startLine}:${endLine}`;
    const now = Date.now();
    const cached = this.symbolCache.get(cacheKey);
    if (cached && now - cached.timestamp <= INLINE_VALUE_CACHE_TTL_MS) {
      return cached.rows;
    }

    try {
      const result = await client.sendRequest<ResolveInlineSymbolsResult>(
        RESOLVE_INLINE_SYMBOLS_METHOD,
        {
          uri: document.uri.toString(),
          start_line: startLine,
          end_line: endLine,
        },
      );
      if (token.isCancellationRequested) {
        return [];
      }
      const rows = Array.isArray(result.symbols) ? result.symbols : [];
      this.symbolCache.set(cacheKey, { timestamp: now, rows });
      return rows;
    } catch (error) {
      if (!isUnsupportedRequestError(error)) {
        outputChannel?.appendLine(
          `[inline values] resolveInlineSymbols failed: ${formatError(error)}`,
        );
      }
      return [];
    }
  }

  private async readMemoryValue(
    session: vscode.DebugSession,
    frameId: number,
    address: number,
    count: number,
    token: vscode.CancellationToken,
  ): Promise<Uint8Array | undefined> {
    const cacheKey = `${session.id}:${frameId}:${address}:${count}`;
    const now = Date.now();
    const cached = this.memoryCache.get(cacheKey);
    if (cached && now - cached.timestamp <= INLINE_VALUE_CACHE_TTL_MS) {
      return cached.bytes;
    }

    try {
      const response = (await session.customRequest("readMemory", {
        memoryReference: `0x${address.toString(16).toUpperCase()}`,
        count,
      })) as ReadMemoryResponse;
      if (token.isCancellationRequested || typeof response?.data !== "string") {
        return undefined;
      }
      const decoded = Buffer.from(response.data, "base64");
      const bytes = Uint8Array.from(decoded.subarray(0, count));
      this.memoryCache.set(cacheKey, { timestamp: now, bytes });
      return bytes;
    } catch {
      return undefined;
    }
  }
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
    const lines = this.extractBreakpointLines(request.arguments);
    const filePath: string | undefined = source.path;

    if (!filePath || !client) {
      this.sendToEmu(request);
      return;
    }

    const uri = vscode.Uri.file(filePath).toString();
    const lspLines = lines.map((l: number) => l - 1);

    const result = await client.sendRequest<ResolveAddressesResult>(
      RESOLVE_ADDRESSES_METHOD,
      { uri, lines: lspLines },
    );

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

  private extractBreakpointLines(args: any): number[] {
    if (Array.isArray(args?.breakpoints)) {
      return args.breakpoints
        .map((bp: any) => bp?.line)
        .filter((line: unknown): line is number =>
          typeof line === "number" && Number.isFinite(line),
        );
    }

    if (Array.isArray(args?.lines)) {
      return args.lines.filter(
        (line: unknown): line is number =>
          typeof line === "number" && Number.isFinite(line),
      );
    }

    return [];
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
