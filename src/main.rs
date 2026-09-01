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
    eprintln!("  lsp [--strict-k65] [--entry AXES]    run the language server over stdio. The");
    eprintln!("                                       same options a build states, so an editor");
    eprintln!("                                       and a build agree; a client may also send");
    eprintln!("                                       them as `strict-k65` and `entry` in its");
    eprintln!("                                       initializationOptions or configuration");
    eprintln!();
    eprintln!("Options:");
    eprintln!("  --unit PATH                          a sibling unit whose names this one may");
    eprintln!("                                       use; repeatable. K816 derives the");
    eprintln!("                                       imports and exports from them");
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
    /// `compile [--no-opt] [-o OUT] [--unit PATH] FILE`
    Compile,
    /// `emit-ir [--after-opt] [--explain] [--unit PATH] FILE`
    EmitIr,
    /// `lsp [--strict-k65] [--entry AXES]`
    Lsp,
}

impl Command {
    /// The optimizer toggle this command takes, and the value it sets.
    fn opt_flag(self) -> Option<(&'static str, bool)> {
        match self {
            Self::Compile => Some(("--no-opt", false)),
            Self::EmitIr => Some(("--after-opt", true)),
            Self::Lsp => None,
        }
    }

    /// Whether the command reads source files named on the command line.
    ///
    /// The LSP's documents arrive over the protocol, so it takes no input
    /// file, no `-o`, and no `--unit`: naming a unit there would be a second
    /// way to describe the program, and decision 2 keeps the program the set
    /// of open documents until sub-plan I lands a project file.
    fn takes_files(self) -> bool {
        !matches!(self, Self::Lsp)
    }
}

struct CommandLine {
    /// The file to compile, and `None` for a command that reads none.
    input: Option<PathBuf>,
    /// Print what the optimizer did, after the dump (`emit-ir --explain`).
    explain: bool,
    /// Sibling units, whose definitions this unit may refer to. K65 has no
    /// import syntax, so naming the other units is how a workspace is
    /// described until sub-plan I gives the driver a project surface.
    units: Vec<PathBuf>,
    output: Option<PathBuf>,
    options: Options,
    message_format: render::MessageFormat,
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
    let mut input: Option<PathBuf> = None;
    let mut units: Vec<PathBuf> = Vec::new();
    let mut output: Option<PathBuf> = None;
    let mut explain = false;
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
            "--unit" if command.takes_files() => match iter.next() {
                Some(path) => units.push(PathBuf::from(path)),
                None => {
                    render::render_tool_error(
                        prog_name,
                        "`--unit` needs a file name",
                        message_format,
                    );
                    return Err(());
                }
            },
            _ if command.takes_files() && arg.starts_with("--unit=") => {
                units.push(PathBuf::from(&arg["--unit=".len()..]));
            }
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
            _ if command.takes_files() && input.is_none() => input = Some(PathBuf::from(arg)),
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
    if command.takes_files() && input.is_none() {
        render::render_tool_error(prog_name, "no input file", message_format);
        return Err(());
    }
    Ok(CommandLine {
        input,
        explain,
        units,
        output,
        options,
        message_format,
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
        Ok(text) => Ok(map.add_file(input.to_string_lossy(), text)),
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

fn compile(prog_name: &str, args: &[String]) -> ExitCode {
    let Ok(command) = parse_command_line(prog_name, args, Command::Compile) else {
        return ExitCode::FAILURE;
    };
    let input = command.input.clone().expect("compile requires an input");
    let mut map = SourceMap::new();
    let Ok(file) = load(prog_name, &mut map, &input, command.message_format) else {
        return ExitCode::FAILURE;
    };
    // The sibling units go into the same map, which is the whole workspace
    // as far as name resolution is concerned. Only `input` is emitted.
    for unit in &command.units {
        if load(prog_name, &mut map, unit, command.message_format).is_err() {
            return ExitCode::FAILURE;
        }
    }

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
    ExitCode::SUCCESS
}

fn emit_ir(prog_name: &str, args: &[String]) -> ExitCode {
    let Ok(command) = parse_command_line(prog_name, args, Command::EmitIr) else {
        return ExitCode::FAILURE;
    };
    let input = command.input.clone().expect("emit-ir requires an input");
    let mut map = SourceMap::new();
    let Ok(file) = load(prog_name, &mut map, &input, command.message_format) else {
        return ExitCode::FAILURE;
    };
    // The sibling units go into the same map, which is the whole workspace
    // as far as name resolution is concerned. Only `input` is emitted.
    for unit in &command.units {
        if load(prog_name, &mut map, unit, command.message_format).is_err() {
            return ExitCode::FAILURE;
        }
    }

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
