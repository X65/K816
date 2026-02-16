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

class K816DebugAdapterFactory implements vscode.DebugAdapterDescriptorFactory {
  createDebugAdapterDescriptor(
    _session: vscode.DebugSession,
  ): vscode.ProviderResult<vscode.DebugAdapterDescriptor> {
    const config = vscode.workspace.getConfiguration("k816");
    const command = config.get<string>("debugger.path", "emu");
    const args = config.get<string[]>("debugger.args", ["--dap"]);
    const env = resolveDebuggerEnv(config);
    return new vscode.DebugAdapterExecutable(command, args, { env });
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
