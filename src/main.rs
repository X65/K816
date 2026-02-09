use std::path::{Path, PathBuf};

use clap::{CommandFactory, Parser};

#[derive(Debug, Parser)]
#[command(
    name = "k816",
    version,
    about = "High-level assembler for the WDC 65816",
    long_about = None,
    after_help = "Examples:\n  k816 path/to/input.k65\n  k816 --help"
)]
struct Cli {
    /// Input source file.
    #[arg(value_name = "INPUT")]
    input: Option<PathBuf>,
}

fn main() {
    if let Err(err) = run() {
        eprintln!("{err}");
        std::process::exit(1);
    }
}

fn run() -> anyhow::Result<()> {
    let cli = Cli::parse();

    let Some(input_path) = cli.input else {
        print_banner();
        println!();
        let mut command = Cli::command();
        command.print_help()?;
        println!();
        return Ok(());
    };

    compile_input(input_path)
}

fn print_banner() {
    println!("K816 HLA, version {}.", env!("CARGO_PKG_VERSION"));
    println!("Port of K65 by Krzysztof Kluczek.");
    println!("License: 0BSD - free to use, copy, modify, and distribute.");
    println!("Provided AS IS, without warranty or liability.");
}

fn compile_input(input_path: PathBuf) -> anyhow::Result<()> {
    let source = std::fs::read_to_string(&input_path)?;
    let output = k816_core::compile_source(&input_path.display().to_string(), &source)
        .map_err(|error| anyhow::anyhow!(error.rendered))?;

    let stem = input_path
        .file_stem()
        .and_then(|stem| stem.to_str())
        .unwrap_or("out");
    let parent = input_path.parent().unwrap_or(Path::new("."));

    if output.banks.len() == 1 {
        let (_, bytes) = output
            .banks
            .first()
            .expect("a single bank should be present");
        let bin_path = parent.join(format!("{stem}.bin"));
        std::fs::write(bin_path, bytes)?;
    } else {
        for (bank, bytes) in &output.banks {
            let bin_path = parent.join(format!("{stem}.{bank}.bin"));
            std::fs::write(bin_path, bytes)?;
        }
    }

    let listing_path = parent.join(format!("{stem}.lst"));
    std::fs::write(listing_path, output.listing)?;
    Ok(())
}
