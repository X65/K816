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
        Some("lsp") => match k816::lsp::run_stdio_server() {
            Ok(()) => ExitCode::SUCCESS,
            Err(error) => {
                eprintln!("{prog_name}: LSP server error: {error}");
                ExitCode::FAILURE
            }
        },
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
    eprintln!("  emit-ir [--after-opt] FILE           dump the lowered operation stream");
    eprintln!("  lsp                                  run the language server over stdio");
    eprintln!();
    eprintln!("Options:");
    eprintln!("  --message-format FORMAT              diagnostic output: human, compact, json");
    eprintln!("  -V, --version                        print the version number and exit");
}

struct CommandLine {
    input: PathBuf,
    output: Option<PathBuf>,
    options: Options,
    message_format: render::MessageFormat,
}

/// Parses `FILE`, `-o OUT`, and the optimizer toggle named by `opt_flag`
/// (with the polarity given by `opt_flag_enables`).
fn parse_command_line(
    prog_name: &str,
    args: &[String],
    opt_flag: &str,
    opt_flag_enables: bool,
) -> Result<CommandLine, ()> {
    let mut input: Option<PathBuf> = None;
    let mut output: Option<PathBuf> = None;
    let mut options = Options {
        optimize: !opt_flag_enables,
    };
    let mut message_format = render::MessageFormat::default();

    let mut parse_format = |prog_name: &str, text: &str| match render::MessageFormat::parse(text) {
        Some(format) => {
            message_format = format;
            Ok(())
        }
        None => {
            eprintln!("{prog_name}: unknown message format `{text}` (human, compact, json)");
            Err(())
        }
    };

    let mut iter = args.iter();
    while let Some(arg) = iter.next() {
        match arg.as_str() {
            "-o" => match iter.next() {
                Some(path) => output = Some(PathBuf::from(path)),
                None => {
                    eprintln!("{prog_name}: `-o` needs a file name");
                    return Err(());
                }
            },
            "--message-format" => match iter.next() {
                Some(format) => parse_format(prog_name, format)?,
                None => {
                    eprintln!("{prog_name}: `--message-format` needs a value");
                    return Err(());
                }
            },
            _ if arg.starts_with("--message-format=") => {
                parse_format(prog_name, &arg["--message-format=".len()..])?;
            }
            flag if flag == opt_flag => options.optimize = opt_flag_enables,
            _ if arg.starts_with('-') => {
                eprintln!("{prog_name}: unknown option `{arg}`");
                return Err(());
            }
            _ if input.is_none() => input = Some(PathBuf::from(arg)),
            _ => {
                eprintln!("{prog_name}: more than one input file");
                return Err(());
            }
        }
    }
    let Some(input) = input else {
        eprintln!("{prog_name}: no input file");
        return Err(());
    };
    Ok(CommandLine {
        input,
        output,
        options,
        message_format,
    })
}

fn load(prog_name: &str, map: &mut SourceMap, input: &Path) -> Result<k816::source::FileId, ()> {
    match std::fs::read_to_string(input) {
        Ok(text) => Ok(map.add_file(input.to_string_lossy(), text)),
        Err(error) => {
            eprintln!("{prog_name}: cannot read {}: {error}", input.display());
            Err(())
        }
    }
}

fn compile(prog_name: &str, args: &[String]) -> ExitCode {
    let Ok(command) = parse_command_line(prog_name, args, "--no-opt", false) else {
        return ExitCode::FAILURE;
    };
    let mut map = SourceMap::new();
    let Ok(file) = load(prog_name, &mut map, &command.input) else {
        return ExitCode::FAILURE;
    };

    let result = compile_to_ca65(&map, file, &command.options);
    render::render_diagnostics(&map, &result.diagnostics, command.message_format);

    let Some(assembly) = result.assembly else {
        return ExitCode::FAILURE;
    };
    let output = command
        .output
        .unwrap_or_else(|| command.input.with_extension("s"));
    if let Err(error) = std::fs::write(&output, assembly) {
        eprintln!("{prog_name}: cannot write {}: {error}", output.display());
        return ExitCode::FAILURE;
    }
    ExitCode::SUCCESS
}

fn emit_ir(prog_name: &str, args: &[String]) -> ExitCode {
    let Ok(command) = parse_command_line(prog_name, args, "--after-opt", true) else {
        return ExitCode::FAILURE;
    };
    let mut map = SourceMap::new();
    let Ok(file) = load(prog_name, &mut map, &command.input) else {
        return ExitCode::FAILURE;
    };

    let result = k816::emit_ir(&map, file, &command.options);
    render::render_diagnostics(&map, &result.diagnostics, command.message_format);

    let Some(ir) = result.ir else {
        return ExitCode::FAILURE;
    };
    print!("{ir}");
    ExitCode::SUCCESS
}
