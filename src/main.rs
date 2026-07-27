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
    eprintln!("  compile [-o OUT.s] FILE      compile K816 source to ca65 assembly");
    eprintln!();
    eprintln!("Options:");
    eprintln!("  -V, --version                print the version number and exit");
}

fn compile(prog_name: &str, args: &[String]) -> ExitCode {
    let mut input: Option<PathBuf> = None;
    let mut output: Option<PathBuf> = None;
    let options = Options::default();

    let mut iter = args.iter();
    while let Some(arg) = iter.next() {
        match arg.as_str() {
            "-o" => match iter.next() {
                Some(path) => output = Some(PathBuf::from(path)),
                None => {
                    eprintln!("{prog_name}: `-o` needs a file name");
                    return ExitCode::FAILURE;
                }
            },
            _ if arg.starts_with('-') => {
                eprintln!("{prog_name}: unknown option `{arg}`");
                return ExitCode::FAILURE;
            }
            _ if input.is_none() => input = Some(PathBuf::from(arg)),
            _ => {
                eprintln!("{prog_name}: more than one input file");
                return ExitCode::FAILURE;
            }
        }
    }
    let Some(input) = input else {
        eprintln!("{prog_name}: no input file");
        return ExitCode::FAILURE;
    };

    let mut map = SourceMap::new();
    let text = match std::fs::read_to_string(&input) {
        Ok(text) => text,
        Err(error) => {
            eprintln!("{prog_name}: cannot read {}: {error}", input.display());
            return ExitCode::FAILURE;
        }
    };
    let file = map.add_file(input.to_string_lossy(), text);

    let result = compile_to_ca65(&map, file, &options);
    render::render_diagnostics(&map, &result.diagnostics);

    let Some(assembly) = result.assembly else {
        return ExitCode::FAILURE;
    };
    let output = output.unwrap_or_else(|| input.with_extension("s"));
    if let Err(error) = std::fs::write(&output, assembly) {
        eprintln!("{prog_name}: cannot write {}: {error}", output.display());
        return ExitCode::FAILURE;
    }
    ExitCode::SUCCESS
}
