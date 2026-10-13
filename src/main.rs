/*****************************************************************************/
/*                                                                           */
/*                                  main.rs                                  */
/*                                                                           */
/*              CLI composition and diagnostic rendering for k816            */
/*                                                                           */
/*                                                                           */
/* Copyright (c) 2026 Tomasz Sterna (smokku)                                 */
/*                                                                           */
/* Permission to use, copy, modify, and/or distribute this software for any  */
/* purpose with or without fee is hereby granted.                            */
/*                                                                           */
/* THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES  */
/* WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF          */
/* MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR   */
/* ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES    */
/* WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN     */
/* ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF   */
/* OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.            */
/*                                                                           */
/*****************************************************************************/

//! The `k816` executable: command-line parsing, pipeline composition, and
//! diagnostic *rendering*. All compiler logic lives in the library; this
//! file may not create diagnostics, only display them.

mod render;

use std::path::{Path, PathBuf};
use std::process::ExitCode;

use k816::source::SourceMap;
use k816::{Options, compile_to_ca65};

const VERSION: &str = "2.19";

fn main() -> ExitCode {
    let mut args = std::env::args();
    let prog_name = args
        .next()
        .and_then(|arg| {
            Path::new(&arg)
                .file_name()
                .map(|name| name.to_string_lossy().into_owned())
        })
        .unwrap_or_else(|| String::from("k816"));

    let args: Vec<String> = args.collect();
    if args.iter().any(|arg| arg == "-V" || arg == "--version") {
        eprintln!(
            "{} V{} - {}",
            prog_name,
            VERSION,
            option_env!("BUILD_ID").unwrap_or("N/A")
        );
        return ExitCode::SUCCESS;
    }

    match args.first().map(String::as_str) {
        Some("compile") => compile(&prog_name, &args[1..]),
        Some("emit-ir") => emit_ir(&prog_name, &args[1..]),
        Some("lsp") => lsp(&prog_name, &args[1..]),
        Some("fmt") => fmt(&prog_name, &args[1..]),
        Some("bind") => bind(&prog_name, &args[1..]),
        _ => {
            usage(&prog_name);
            ExitCode::FAILURE
        }
    }
}

fn usage(prog_name: &str) {
    eprintln!("Usage: {prog_name} <command> [options]");
    eprintln!();
    eprintln!("Commands:");
    eprintln!("  compile [--no-opt] [-o OUT.s] FILE   compile K816 source to ca65 assembly");
    eprintln!("  emit-ir [--after-opt] [--explain] FILE");
    eprintln!("                                       dump the lowered operation stream, and");
    eprintln!("                                       with --explain what the optimizer did");
    eprintln!("  fmt [--check] FILE...                normalize whitespace: indentation, trailing");
    eprintln!(
        "                                       space, and the final newline. --check writes"
    );
    eprintln!("                                       nothing and exits non-zero if a file would");
    eprintln!("                                       change");
    eprintln!("  bind [--for FILE] [--all] [--layouts [--flat]] [-o OUT.k816] DEBUGFILE");
    eprintln!("                                       write K816 declarations for the symbols");
    eprintln!("                                       ca65 modules provide, read from the debug");
    eprintln!(
        "                                       file ld65 wrote (`cl65 -g -Wl --dbgfile,F`)."
    );
    eprintln!("                                       --for FILE: only what the module built from");
    eprintln!("                                       FILE imports; --all: every export, used or");
    eprintln!("                                       not; --layouts: every `.struct` as a field");
    eprintln!("                                       layout instead, --flat as one `var` per");
    eprintln!("                                       member of a `.struct` at an `.org`");
    eprintln!("  lsp [--strict-k65] [--entry AXES]    run the language server over stdio. The");
    eprintln!("                                       same options a build states, so an editor");
    eprintln!("                                       and a build agree; a client may also send");
    eprintln!("                                       them as `strict-k65` and `entry` in its");
    eprintln!("                                       initializationOptions or configuration");
    eprintln!();
    eprintln!("Options:");
    eprintln!("  --create-dep FILE                    write a make dependency file for what");
    eprintln!("                                       compile read: the source, every unit it");
    eprintln!("                                       imports, and every file a `binary`");
    eprintln!("                                       includes. --create-full-dep is the same");
    eprintln!("                                       list, as K816 has no include path");
    eprintln!("  --dep-target NAME                    the name on the left of the `:`, when the");
    eprintln!("                                       file the build tracks is not the -o one");
    eprintln!("  --message-format FORMAT              diagnostic output of compile and emit-ir:");
    eprintln!("                                       human, compact, json");
    eprintln!("  --strict-k65                         report every construct the original K65");
    eprintln!("                                       language does not have. Extensions are");
    eprintln!("                                       errors, and a construct that means");
    eprintln!("                                       something else there is a warning");
    eprintln!("  --entry AXES                         the state the machine is in when main");
    eprintln!("                                       gets control: native or emulation, a8 or");
    eprintln!("                                       a16, i8 or i16, comma-separated in that");
    eprintln!("                                       order and each optional. Default");
    eprintln!("                                       native,a8,i8 - what the startup stub in");
    eprintln!("                                       examples/fullstack establishes");
    eprintln!("  -V, --version                        print the version number and exit");
}

/// Which subcommand's command line is being read.
///
/// The arms a command accepts are a property of the command, and this is
/// what says so in one place: `lsp` takes the two options that describe the
/// *project* — `--strict-k65` and `--entry` — because an editor and a build
/// have to agree about them, and none of the options that describe one
/// compilation, because it compiles nothing.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Command {
    /// `compile [--no-opt] [-o OUT] FILE`
    Compile,
    /// `emit-ir [--after-opt] [--explain] FILE`
    EmitIr,
    /// `lsp [--strict-k65] [--entry AXES]`
    Lsp,
    /// `fmt [--check] FILE...`
    Fmt,
    /// `bind [--for FILE] [--all] [--layouts [--flat]] [-o OUT] DEBUGFILE`
    Bind,
}

impl Command {
    /// The optimizer toggle this command takes, and the value it sets.
    fn opt_flag(self) -> Option<(&'static str, bool)> {
        match self {
            Self::Compile => Some(("--no-opt", false)),
            Self::EmitIr => Some(("--after-opt", true)),
            Self::Lsp | Self::Fmt | Self::Bind => None,
        }
    }

    /// Whether the command reads source files named on the command line.
    ///
    /// The LSP's documents arrive over the protocol, so it takes no input
    /// file and no `-o`. Which *other* units a document belongs with is no
    /// longer a command-line question for anyone: a source names the units
    /// it may see, and the server reads the same lines the build does.
    fn takes_files(self) -> bool {
        !matches!(self, Self::Lsp)
    }

    /// Whether the command reads *several* files rather than one.
    fn takes_many(self) -> bool {
        matches!(self, Self::Fmt)
    }

    /// Whether the command can report what it read as a make dependency file.
    ///
    /// `compile` only: a dependency is a statement about what makes an
    /// *output* stale, and it is the only command that produces one a build
    /// system tracks.
    fn takes_deps(self) -> bool {
        matches!(self, Self::Compile)
    }
}

struct CommandLine {
    /// The files named on the command line, in order. `compile` and
    /// `emit-ir` take one; `fmt` takes any number; `lsp` takes none.
    inputs: Vec<PathBuf>,
    /// `fmt --check`: report what would change and write nothing.
    check: bool,
    /// `bind`'s own options: which module's imports, every export or only
    /// the used ones, and layouts instead of symbols.
    bind: k816::bind::Request,
    /// Print what the optimizer did, after the dump (`emit-ir --explain`).
    explain: bool,
    output: Option<PathBuf>,
    options: Options,
    message_format: render::MessageFormat,
    /// `--create-dep` / `--create-full-dep`: where to write a make dependency
    /// file. Both spellings are accepted and both produce the same list,
    /// because K816 has no include path to separate a system header from a
    /// local one — see [`write_dependencies`].
    dep_files: Vec<PathBuf>,
    /// `--dep-target`: the name on the left of the `:`, when the file a build
    /// tracks is not the one `-o` names. `cl65` passes the object here,
    /// because what goes stale is the object and not the intermediate
    /// assembly it threw away.
    dep_target: Option<PathBuf>,
}

/// Parses one subcommand's command line.
///
/// Every command reads `--strict-k65`, `--entry` and `--message-format`
/// through these same arms, which is what makes a refused `--entry` render
/// identically whichever command was asked for it — and, for the LSP, what
/// makes the editor's configuration and the build's flags one vocabulary
/// (`plans/07-lsp-tooling.md` decision 1).
fn parse_command_line(
    prog_name: &str,
    args: &[String],
    command: Command,
) -> Result<CommandLine, ()> {
    let opt_flag = command.opt_flag();
    let mut inputs: Vec<PathBuf> = Vec::new();
    let mut output: Option<PathBuf> = None;
    let mut explain = false;
    let mut check = false;
    let mut bind = k816::bind::Request::default();
    let mut dep_files: Vec<PathBuf> = Vec::new();
    let mut dep_target: Option<PathBuf> = None;
    let mut options = Options {
        // The toggle names the value it sets, so the starting value is the
        // other one; a command with no toggle keeps the default.
        optimize: opt_flag.is_none_or(|(_, enables)| !enables),
        ..Options::default()
    };
    let mut message_format = render::MessageFormat::default();

    let mut iter = args.iter();
    while let Some(arg) = iter.next() {
        match arg.as_str() {
            "-o" if command.takes_files() => match iter.next() {
                Some(path) => output = Some(PathBuf::from(path)),
                None => {
                    render::render_tool_error(prog_name, "`-o` needs a file name", message_format);
                    return Err(());
                }
            },
            "--message-format" => match iter.next() {
                Some(text) => set_message_format(prog_name, text, &mut message_format)?,
                None => {
                    render::render_tool_error(
                        prog_name,
                        "`--message-format` needs a value",
                        message_format,
                    );
                    return Err(());
                }
            },
            _ if arg.starts_with("--message-format=") => {
                let text = &arg["--message-format=".len()..];
                set_message_format(prog_name, text, &mut message_format)?;
            }
            "--create-dep" | "--create-full-dep" if command.takes_deps() => match iter.next() {
                Some(path) => dep_files.push(PathBuf::from(path)),
                None => {
                    render::render_tool_error(
                        prog_name,
                        &format!("`{arg}` needs a file name"),
                        message_format,
                    );
                    return Err(());
                }
            },
            "--dep-target" if command.takes_deps() => match iter.next() {
                Some(path) => dep_target = Some(PathBuf::from(path)),
                None => {
                    render::render_tool_error(
                        prog_name,
                        "`--dep-target` needs a name",
                        message_format,
                    );
                    return Err(());
                }
            },
            "--strict-k65" => options.strict_k65 = true,
            "--entry" => match iter.next() {
                Some(text) => set_entry(prog_name, text, &mut options, message_format)?,
                None => {
                    render::render_tool_error(
                        prog_name,
                        "`--entry` needs a value (`native,a8,i8` or any part of it)",
                        message_format,
                    );
                    return Err(());
                }
            },
            _ if arg.starts_with("--entry=") => {
                let text = &arg["--entry=".len()..];
                set_entry(prog_name, text, &mut options, message_format)?;
            }
            "--explain" if command == Command::EmitIr => explain = true,
            "--check" if command == Command::Fmt => check = true,
            "--for" if command == Command::Bind => match iter.next() {
                Some(path) => bind.for_module = Some(PathBuf::from(path)),
                None => {
                    render::render_tool_error(
                        prog_name,
                        "`--for` needs the source file of a module",
                        message_format,
                    );
                    return Err(());
                }
            },
            _ if command == Command::Bind && arg.starts_with("--for=") => {
                bind.for_module = Some(PathBuf::from(&arg["--for=".len()..]));
            }
            "--all" if command == Command::Bind => bind.all = true,
            "--layouts" if command == Command::Bind => bind.layouts = true,
            "--flat" if command == Command::Bind => bind.flat = true,
            flag if opt_flag.is_some_and(|(name, _)| name == flag) => {
                options.optimize = opt_flag.expect("matched the toggle's own name").1;
            }
            _ if arg.starts_with('-') => {
                render::render_tool_error(
                    prog_name,
                    &format!("unknown option `{arg}`"),
                    message_format,
                );
                return Err(());
            }
            _ if command.takes_files() && (command.takes_many() || inputs.is_empty()) => {
                inputs.push(PathBuf::from(arg));
            }
            _ if command.takes_files() => {
                render::render_tool_error(prog_name, "more than one input file", message_format);
                return Err(());
            }
            _ => {
                render::render_tool_error(
                    prog_name,
                    &format!("`lsp` takes no input file, but `{arg}` was given"),
                    message_format,
                );
                return Err(());
            }
        }
    }
    if command.takes_files() && inputs.is_empty() {
        render::render_tool_error(prog_name, "no input file", message_format);
        return Err(());
    }
    Ok(CommandLine {
        inputs,
        check,
        bind,
        explain,
        output,
        options,
        message_format,
        dep_files,
        dep_target,
    })
}

/// Applies one `--message-format` value, reporting unknown ones in whatever
/// format is in effect so far.
/// Reads a `--entry` value into `options`, or reports why it could not be.
///
/// The reading is `abi::EntryEnvironment::parse`'s, in the library, because
/// what it enforces is a fact about the machine and not a convention of this
/// CLI: `emulation` forces both registers to eight bits, so `emulation,a16`
/// names no machine. `main.rs` only decides where the sentence goes.
fn set_entry(
    prog_name: &str,
    text: &str,
    options: &mut Options,
    message_format: render::MessageFormat,
) -> Result<(), ()> {
    match k816::abi::EntryEnvironment::parse(text) {
        Ok(environment) => {
            options.entry = environment;
            Ok(())
        }
        Err(refusal) => {
            render::render_coded_tool_error(
                prog_name,
                k816::abi::EntrySpelling::CODE,
                &format!("`--entry`: {refusal}"),
                message_format,
            );
            Err(())
        }
    }
}

fn set_message_format(
    prog_name: &str,
    text: &str,
    message_format: &mut render::MessageFormat,
) -> Result<(), ()> {
    match render::MessageFormat::parse(text) {
        Some(format) => {
            *message_format = format;
            Ok(())
        }
        None => {
            render::render_tool_error(
                prog_name,
                &format!("unknown message format `{text}` (human, compact, json)"),
                *message_format,
            );
            Err(())
        }
    }
}

fn load(
    prog_name: &str,
    map: &mut SourceMap,
    input: &Path,
    format: render::MessageFormat,
) -> Result<k816::source::FileId, ()> {
    match std::fs::read_to_string(input) {
        Ok(text) => Ok(map.add_file_at(input.to_string_lossy(), input, text)),
        Err(error) => {
            render::render_tool_error(
                prog_name,
                &format!("cannot read {}: {error}", input.display()),
                format,
            );
            Err(())
        }
    }
}

/// Builds the whole program: the entry file, the units named beside it, and
/// everything they `import`.
///
/// One routine for `compile` and `emit-ir`, so that the program the IR dump
/// describes is the program the compiler compiles.
///
/// A named file that cannot be read is fatal and says so; an *imported* one
/// is not reported here at all. The loader adds nothing where it cannot
/// read, and the compiler then reports `K816-E0166` at the string that named
/// it — which points at the line rather than at a path in a tool message, and
/// is the same sentence the editor shows.
fn load_program(
    prog_name: &str,
    map: &mut SourceMap,
    input: &Path,
    format: render::MessageFormat,
) -> Result<k816::source::FileId, ()> {
    let file = load(prog_name, map, input, format)?;
    k816::load_imports(map, &mut |path| std::fs::read_to_string(path));
    Ok(file)
}

fn lsp(prog_name: &str, args: &[String]) -> ExitCode {
    let Ok(command) = parse_command_line(prog_name, args, Command::Lsp) else {
        return ExitCode::FAILURE;
    };
    match k816::lsp::run_stdio_server(command.options) {
        Ok(()) => ExitCode::SUCCESS,
        Err(error) => {
            eprintln!("{prog_name}: LSP server error: {error}");
            ExitCode::FAILURE
        }
    }
}

/// `k816 fmt [--check] FILE...`
///
/// The same function the editor's `textDocument/formatting` calls, because a
/// formatter reachable only through an editor cannot run in CI and cannot
/// format the corpus for its own gate (`plans/07-lsp-tooling.md` decision 16).
fn fmt(prog_name: &str, args: &[String]) -> ExitCode {
    let Ok(command) = parse_command_line(prog_name, args, Command::Fmt) else {
        return ExitCode::FAILURE;
    };

    let mut changed = 0usize;
    let mut failed = false;
    for input in &command.inputs {
        let mut map = SourceMap::new();
        let Ok(file) = load(prog_name, &mut map, input, command.message_format) else {
            return ExitCode::FAILURE;
        };
        let source = map.file(file);
        let parse = k816::parser::parse_source(source, None);
        let Some(formatted) = k816::parser::format::format(source, &parse.tree) else {
            // A program the grammar could not read is not formatted, because
            // formatting one is guessing. The diagnostics already say why,
            // and this says which file was left alone.
            render::render_tool_error(
                prog_name,
                &format!(
                    "{}: not formatted — its parse has an error, and formatting a program                      the grammar could not read would be guessing",
                    input.display()
                ),
                command.message_format,
            );
            failed = true;
            continue;
        };
        if formatted == source.text() {
            continue;
        }
        changed += 1;
        if command.check {
            eprintln!("{}: would be reformatted", input.display());
            continue;
        }
        if let Err(error) = std::fs::write(input, formatted) {
            render::render_tool_error(
                prog_name,
                &format!("cannot write {}: {error}", input.display()),
                command.message_format,
            );
            return ExitCode::FAILURE;
        }
    }

    if failed || (command.check && changed > 0) {
        return ExitCode::FAILURE;
    }
    ExitCode::SUCCESS
}

/// `k816 bind [--for FILE] [--all] [--layouts [--flat]] [-o OUT] DEBUGFILE`
///
/// The K816 side of a program's ca65 modules, written from what ld65
/// recorded about them. The reading and the writing are the library's
/// (`k816::bind`); this reads the files and chooses where the text goes.
fn bind(prog_name: &str, args: &[String]) -> ExitCode {
    let Ok(mut command) = parse_command_line(prog_name, args, Command::Bind) else {
        return ExitCode::FAILURE;
    };
    let input = command.inputs[0].clone();
    let text = match std::fs::read_to_string(&input) {
        Ok(text) => text,
        Err(error) => {
            render::render_tool_error(
                prog_name,
                &format!("cannot read {}: {error}", input.display()),
                command.message_format,
            );
            return ExitCode::FAILURE;
        }
    };
    let database = match k816::bind::Database::parse(&text) {
        Ok(database) => database,
        Err(error) => {
            render::render_tool_error(
                prog_name,
                &format!("{}: {error}", input.display()),
                command.message_format,
            );
            return ExitCode::FAILURE;
        }
    };
    command.bind.output = command.output.clone();
    command.bind.invocation = format!("{prog_name} bind {}", args.join(" "));
    let generated = match k816::bind::bind(&database, &command.bind, &|path| {
        std::fs::read_to_string(path).ok()
    }) {
        Ok(generated) => generated,
        Err(error) => {
            render::render_tool_error(prog_name, &error, command.message_format);
            return ExitCode::FAILURE;
        }
    };
    match command.output {
        Some(output) => {
            if let Err(error) = std::fs::write(&output, generated) {
                render::render_tool_error(
                    prog_name,
                    &format!("cannot write {}: {error}", output.display()),
                    command.message_format,
                );
                return ExitCode::FAILURE;
            }
        }
        None => print!("{generated}"),
    }
    ExitCode::SUCCESS
}

fn compile(prog_name: &str, args: &[String]) -> ExitCode {
    let Ok(command) = parse_command_line(prog_name, args, Command::Compile) else {
        return ExitCode::FAILURE;
    };
    let input = command.inputs[0].clone();
    let mut map = SourceMap::new();
    let Ok(file) = load_program(prog_name, &mut map, &input, command.message_format) else {
        return ExitCode::FAILURE;
    };

    let result = compile_to_ca65(&map, file, &command.options);
    render::render_diagnostics(&map, &result.diagnostics, command.message_format);

    let Some(assembly) = result.assembly else {
        return ExitCode::FAILURE;
    };
    let output = command.output.unwrap_or_else(|| input.with_extension("s"));
    if let Err(error) = std::fs::write(&output, assembly) {
        render::render_tool_error(
            prog_name,
            &format!("cannot write {}: {error}", output.display()),
            command.message_format,
        );
        return ExitCode::FAILURE;
    }

    // After the output, and only on success: a dependency file that outlived
    // a failed compilation would tell the next build the target is current.
    let target = command.dep_target.as_ref().unwrap_or(&output);
    let mut inputs = vec![input];
    inputs.extend(result.imports);
    inputs.extend(result.binaries);
    for path in &command.dep_files {
        if let Err(error) = write_dependencies(path, target, &inputs) {
            render::render_tool_error(
                prog_name,
                &format!("cannot write {}: {error}", path.display()),
                command.message_format,
            );
            return ExitCode::FAILURE;
        }
    }
    ExitCode::SUCCESS
}

/// Writes a make dependency file: what `target` is built from.
///
/// The shape is cc65's, because a build system that already reads cc65's
/// output must not need a second parser: the target, a colon and a tab, the
/// inputs separated by spaces, then a blank line and a *phony* rule naming
/// each input with no prerequisites. The phony half is what keeps `make` from
/// stopping when a file named here is deleted rather than edited — it is the
/// same trick `gcc -MP` plays, and `cc65` plays it too.
///
/// **A K816 compilation reads what the command line named, what those files
/// `import` transitively, and the files a `binary` includes.** The whole
/// import closure and not only the direct edges: a unit two imports away is
/// not *visible*, but its body decided a contract this compilation used, so
/// a build that rebuilt only on direct imports would ship a stale object.
///
/// There is still no include path and no implicit prelude, which is why
/// `--create-dep` and `--create-full-dep` produce the same list: cc65
/// distinguishes them by whether system headers are listed, and K816 has no
/// system headers to leave out.
fn write_dependencies(path: &Path, target: &Path, inputs: &[PathBuf]) -> std::io::Result<()> {
    fn escaped(path: &Path) -> String {
        // Only spaces, as cc65 escapes them: a make prerequisite is
        // space-separated and everything else in a path is literal.
        path.to_string_lossy().replace(' ', "\\ ")
    }

    let list: Vec<String> = inputs.iter().map(|input| escaped(input)).collect();
    let list = list.join(" ");
    std::fs::write(path, format!("{}:\t{list}\n\n{list}:\n\n", escaped(target)))
}

fn emit_ir(prog_name: &str, args: &[String]) -> ExitCode {
    let Ok(command) = parse_command_line(prog_name, args, Command::EmitIr) else {
        return ExitCode::FAILURE;
    };
    let input = command.inputs[0].clone();
    let mut map = SourceMap::new();
    let Ok(file) = load_program(prog_name, &mut map, &input, command.message_format) else {
        return ExitCode::FAILURE;
    };

    let result = k816::emit_ir(&map, file, &command.options);
    render::render_diagnostics(&map, &result.diagnostics, command.message_format);

    let Some(ir) = result.ir else {
        return ExitCode::FAILURE;
    };
    print!("{ir}");
    if command.explain {
        match result.explanation {
            Some(explanation) => print!("\n{explanation}"),
            // `--explain` without `--after-opt`: there is no optimizer run to
            // explain, and saying so beats printing nothing.
            None => println!("\noptimizer: not run (add --after-opt)"),
        }
    }
    ExitCode::SUCCESS
}
