# Plan: Fix VSCode extension F5 / Ctrl+F5 behavior (GitHub issue #3)

## Context

GitHub issue [#3](https://github.com/X65/K816/issues/3) ("Ctrl+F5 Vs F5 - VSC behavior") reports that pressing F5 in the k816 VSCode extension does not reliably start a debug session. The root cause is that the extension ships a debug-configuration template whose `program` field is hardcoded to `${workspaceFolder}/target/output.xex` — a path the build system never produces (actual output is `target/{package.name}.{xex|bin}` from `k816.toml`). A user who accepts the default snippet via "Add Configuration" gets a broken launch.json and F5 fails with file-not-found.

The issue also asks for a Ctrl+F5 shortcut to launch the emulator without a debug session — a common "just run it" flow that currently requires a terminal.

The fix has four parts:

1. Remove the misleading `program` default from the debug snippet/template.
2. Add a `k816 metadata` CLI subcommand that dumps project config (including the resolved artifact path) as JSON — the authoritative source of truth that the extension can parse.
3. Have the extension's debug-config resolver call `k816 metadata` to auto-fill `program` when it's not explicitly set.
4. Add a `k816.run` command bound to Ctrl+F5 that delegates to the existing `k816 run` task (which honors `[run].runner` from `k816.toml`).

F7 (the existing `k816.build` keybinding) is already correct and is not touched.

## Files to modify

- [src/main.rs](src/main.rs) — new `Metadata` subcommand + handler
- [editors/vscode-k816/package.json](editors/vscode-k816/package.json) — debug config templates, commands, keybindings
- [editors/vscode-k816/src/extension.ts](editors/vscode-k816/src/extension.ts) — new `k816.run` command, updated debug config resolver
- [editors/vscode-k816/README.md](editors/vscode-k816/README.md) — document the `program` override

No new dependencies on either side (`serde_json` is already a workspace dep — see [Cargo.toml:47](Cargo.toml#L47) — and the extension needs no new npm packages).

## Changes

### 1. Rust CLI — `k816 metadata` subcommand

**Add variant to `Commands` enum** ([src/main.rs:169-187](src/main.rs#L169-L187)):

```rust
/// Dump resolved project metadata as JSON (for editor integration).
Metadata,
```

**Dispatch in `run()`** ([src/main.rs:280-310](src/main.rs#L280-L310)):

```rust
Some(Commands::Metadata) => {
    link_options.validate_for_project_build_or_run("metadata")?;
    output_option.validate_absent_for_subcommand("metadata")?;
    metadata_command(link_options)
}
```

**New handler `metadata_command`** near the existing `project_build_command` ([src/main.rs:796-799](src/main.rs#L796-L799)). It reuses the existing helpers — no duplication of resolution logic:

```rust
fn metadata_command(link_options: LinkPhaseOptions) -> anyhow::Result<()> {
    let project_root = resolve_project_root()?;
    let manifest = load_project_manifest(&project_root)?;
    let target_dir = project_root.join("target");

    let resolved_config = resolve_project_link_config_path(&project_root, &manifest, &link_options);
    let link_config = if let Some(path) = resolved_config.as_deref() {
        k816_link::load_config(path)?
    } else {
        k816_link::default_stub_config()
    };
    let output_kind = resolve_output_kind(link_config.output.kind, None, link_options.output_format);
    let ext = output_extension(output_kind);
    let artifact_path = target_dir.join(format!("{}.{}", manifest.package.name, ext));

    let metadata = serde_json::json!({
        "project_root": project_root,
        "package": { "name": manifest.package.name },
        "target_dir": target_dir,
        "artifact": {
            "path": artifact_path,
            "kind": match output_kind {
                k816_link::OutputKind::Xex => "xex",
                k816_link::OutputKind::RawBinary => "bin",
            },
        },
        "run": {
            "runner": manifest.run.runner,
            "args": manifest.run.args,
        },
    });
    println!("{}", serde_json::to_string_pretty(&metadata)?);
    Ok(())
}
```

Key points:

- Uses the **same resolution pipeline** as `project_build_internal` ([src/main.rs:952-964](src/main.rs#L952-L964)) so the reported path is guaranteed to match what a build would produce.
- Does **not** invoke the compiler or linker — pure metadata.
- Writes to stdout only; any error goes through the existing `anyhow` error path (stderr + nonzero exit).
- `ProjectManifest.run` is already a public-ish field inside this binary (defined at [src/main.rs:653-658](src/main.rs#L653-L658)); exposing it in the JSON lets future editor features use `[run].runner` without re-parsing TOML.

### 2. `package.json` — remove misleading defaults, add command + keybinding

- **Remove `program` from the initial debug config** ([editors/vscode-k816/package.json:58-65](editors/vscode-k816/package.json#L58-L65)). Keep `type`/`request`/`name` only.
- **Remove `program` from the configuration snippet** ([editors/vscode-k816/package.json:66-77](editors/vscode-k816/package.json#L66-L77)).
- **Drop `"required": ["program"]`** ([editors/vscode-k816/package.json:42-44](editors/vscode-k816/package.json#L42-L44)) so launch.json can omit `program` and rely on metadata resolution. `program` remains a **supported, optional** override — documented in the README (see §4 below).
- **Add command** to `contributes.commands` (after line 171):

  ```json
  { "command": "k816.run", "title": "K816: Run (without debugging)" }
  ```

- **Add keybinding** to `contributes.keybindings` (after line 250):

  ```json
  {
    "command": "k816.run",
    "key": "ctrl+f5",
    "when": "editorTextFocus && editorLangId == k65"
  }
  ```

### 3. `extension.ts` — register `k816.run`, auto-resolve program via `k816 metadata`

**New helper** near the existing `formatError` function ([editors/vscode-k816/src/extension.ts:600-605](editors/vscode-k816/src/extension.ts#L600-L605)):

```ts
interface K816Metadata {
  project_root: string;
  package: { name: string };
  target_dir: string;
  artifact: { path: string; kind: "xex" | "bin" };
  run: { runner: string | null; args: string[] };
}

async function fetchProjectMetadata(
  folder: vscode.WorkspaceFolder,
): Promise<K816Metadata | undefined> {
  const config = vscode.workspace.getConfiguration("k816");
  const command = config.get<string>("server.path", "k816");
  const { spawn } = await import("node:child_process");
  return new Promise((resolve) => {
    const proc = spawn(command, ["metadata"], { cwd: folder.uri.fsPath });
    let stdout = "";
    proc.stdout.on("data", (chunk) => (stdout += chunk.toString()));
    proc.on("error", () => resolve(undefined));
    proc.on("close", (code) => {
      if (code !== 0) return resolve(undefined);
      try {
        resolve(JSON.parse(stdout) as K816Metadata);
      } catch {
        resolve(undefined);
      }
    });
  });
}
```

**Register `k816.run`** in `activate()` alongside the existing `k816.build` handler ([editors/vscode-k816/src/extension.ts:121-129](editors/vscode-k816/src/extension.ts#L121-L129)). The existing `K816TaskProvider` ([editors/vscode-k816/src/extension.ts:611-669](editors/vscode-k816/src/extension.ts#L611-L669)) already creates a `run` task that shell-executes `k816 run`. Fetch it by `definition.task === "run"` (it has no `TaskGroup.Run` assigned):

```ts
vscode.commands.registerCommand("k816.run", async () => {
  const tasks = await vscode.tasks.fetchTasks({ type: "k816" });
  const runTask = tasks.find(
    (t) => (t.definition as K816TaskDefinition).task === "run",
  );
  if (runTask) {
    vscode.tasks.executeTask(runTask);
  }
}),
```

This is intentionally parallel to the existing `k816.build` handler. `k816 run` already builds and invokes `[run].runner` — all path logic stays in the Rust CLI.

**Update `K816DebugConfigProvider.resolveDebugConfiguration`** ([editors/vscode-k816/src/extension.ts:1150-1177](editors/vscode-k816/src/extension.ts#L1150-L1177)) — call `k816 metadata` before the picker fallback, and widen the picker filter to accept `.bin` too:

```ts
if (config.type === "k816" && !config.program) {
  const folder = vscode.workspace.workspaceFolders?.[0];
  if (folder) {
    const meta = await fetchProjectMetadata(folder);
    if (meta?.artifact?.path) {
      config.program = meta.artifact.path;
    }
  }
  if (!config.program) {
    const uris = await vscode.window.showOpenDialog({
      canSelectFiles: true,
      canSelectMany: false,
      title: "Select program binary to debug",
      filters: { "K816 binaries": ["xex", "bin"], "All files": ["*"] },
    });
    if (!uris || uris.length === 0) {
      return undefined;
    }
    config.program = uris[0].fsPath;
  }
}
```

Note: when `program` **is** set in `launch.json`, metadata is not consulted — the user override wins.

### 4. README — document the `program` override

Add a short "Debugging" section to [editors/vscode-k816/README.md](editors/vscode-k816/README.md) explaining:

- F5 starts a debug session. When `launch.json` is absent or its k816 config has no `program`, the extension auto-resolves the binary by invoking `k816 metadata` in the workspace root and using `artifact.path`.
- Users can override auto-resolution by setting `program` explicitly in `launch.json`:

  ```json
  {
    "type": "k816",
    "request": "launch",
    "name": "Debug K65 Program",
    "program": "${workspaceFolder}/path/to/custom.xex"
  }
  ```

- F7 runs `k816 build`.
- Ctrl+F5 runs `k816 run` (requires `[run].runner` in `k816.toml`).

## Verification

Manual test in an Extension Development Host:

1. Rebuild the Rust CLI: `cargo build --release` from the repo root. Confirm `target/release/k816 metadata` produces JSON when run inside a k816 project directory.
2. From `/home/smoku/devel/X65/k816`, open `editors/vscode-k816/` in VSCode and press F5 (its own `.vscode/launch.json` launches the dev host).
3. In the dev host, open a k816 project (create one with `k816 init demo` if none is handy) and open a `.k65` file.
4. **`k816 metadata` sanity**: run `k816 metadata` in the integrated terminal. Confirm it prints JSON with `artifact.path` ending in `target/demo.xex`.
5. **F7**: press F7. Terminal runs `k816 build`; `target/demo.xex` exists afterward. (No behavior change expected — sanity check only.)
6. **F5, no launch.json**: delete `.vscode/launch.json` if present. Press F5. Expected: no file picker; DAP session starts with `emu --dap <workspace>/target/demo.xex`. Enable `k816.trace.debugger: "messages"` to confirm initialize in the "k816 DAP" output channel.
7. **F5, template snippet**: run "Debug: Add Configuration" → "K816: Launch". Confirm the inserted snippet contains no `program` line. Press F5. Same auto-resolve path is used.
8. **F5, explicit program override**: add `"program": "${workspaceFolder}/some/other.xex"` to `launch.json`. Confirm the override wins (no `k816 metadata` call — check with a trace) and auto-resolve is skipped.
9. **F5, broken project**: remove `k816.toml`. Press F5. Expected: `k816 metadata` fails, resolver falls back to the file picker (existing behavior).
10. **Ctrl+F5, runner configured**: set `[run] runner = "emu"` in `k816.toml`. Press Ctrl+F5. Expected: `k816 run` task runs in the terminal, builds, then spawns `emu target/demo.xex`. No DAP session in Run/Debug view.
11. **Ctrl+F5, no runner**: remove `[run].runner`. Press Ctrl+F5. Expected: `k816 run` errors with "No runner configured. Set [run].runner in k816.toml." in the task terminal. This is correct behavior — no extra extension handling.
