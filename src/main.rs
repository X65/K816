use std::path::{Path, PathBuf};
use std::{env, io::IsTerminal};

use clap::{CommandFactory, Parser, Subcommand};

#[derive(Debug, Parser)]
#[command(
    name = "k816",
    version,
    about = "High-level assembler for the WDC 65816",
    long_about = None,
    override_usage = "k816 [COMMAND] [INPUT]",
    after_help = "Examples:\n  k816 path/to/input.k65\n  k816 compile path/to/input.k65\n  k816 link path/to/input.o65 -T link.k816ld.ron\n  k816 --help"
)]
struct Cli {
    /// Optional explicit subcommand.
    #[command(subcommand)]
    command: Option<Commands>,

    /// Input source file.
    #[arg(value_name = "INPUT")]
    input: Option<PathBuf>,
}

#[derive(Debug, Subcommand)]
enum Commands {
    /// Compile source file into relocatable o65 object file.
    Compile(CompileArgs),
    /// Link one or more o65 object files into final binary output.
    Link(LinkArgs),
}

#[derive(Debug, Parser)]
struct CompileArgs {
    /// Input source file (.k65).
    #[arg(value_name = "INPUT")]
    input: PathBuf,
    /// Output object file path.
    #[arg(short = 'o', long = "output", value_name = "OBJECT_FILE")]
    output: Option<PathBuf>,
}

#[derive(Debug, Parser)]
struct LinkArgs {
    /// Input object files (.o65).
    #[arg(value_name = "OBJECT_FILE", required = true)]
    objects: Vec<PathBuf>,
    /// Linker config file in RON format.
    #[arg(short = 'T', long = "config", value_name = "CONFIG")]
    config: Option<PathBuf>,
    /// Output base path (without extension) for emitted binaries/listing.
    #[arg(short = 'o', long = "output", value_name = "OUT_BASE")]
    output: Option<PathBuf>,
}

#[derive(Debug, Clone)]
struct OutputBase {
    parent: PathBuf,
    stem: String,
}

fn main() {
    if let Err(err) = run() {
        eprintln!("{err}");
        std::process::exit(1);
    }
}

fn run() -> anyhow::Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Some(Commands::Compile(args)) => compile_command(args),
        Some(Commands::Link(args)) => link_command(args),
        None => {
            let Some(input_path) = cli.input else {
                print_banner();
                println!();
                let mut command = Cli::command();
                command.print_help()?;
                println!();
                return Ok(());
            };
            build_command(input_path)
        }
    }
}

fn print_banner() {
    println!("K816 HLA, version {}.", env!("CARGO_PKG_VERSION"));
    println!("Port of K65 by Krzysztof Kluczek.");
    println!("License: 0BSD - free to use, copy, modify, and distribute.");
    println!("Provided AS IS, without warranty or liability.");
}

fn compile_source_file(input_path: &Path) -> anyhow::Result<k816_o65::O65Object> {
    let is_k65 = input_path
        .extension()
        .and_then(|ext| ext.to_str())
        .is_some_and(|ext| ext.eq_ignore_ascii_case("k65"));
    if !is_k65 {
        anyhow::bail!(
            "invalid input extension for '{}': expected .k65 source file",
            input_path.display()
        );
    }

    let source = std::fs::read_to_string(input_path)?;
    let output = k816_core::compile_source_to_object_with_options(
        &input_path.display().to_string(),
        &source,
        k816_core::CompileRenderOptions {
            color: stderr_supports_color(),
        },
    )
    .map_err(|error| anyhow::anyhow!(error.rendered))?;
    Ok(output.object)
}

fn stderr_supports_color() -> bool {
    if env::var_os("NO_COLOR").is_some() {
        return false;
    }

    if let Some(force) = env::var_os("CLICOLOR_FORCE") {
        return force != "0";
    }

    if let Some(choice) = env::var_os("CLICOLOR") {
        if choice == "0" {
            return false;
        }
    }

    std::io::stderr().is_terminal()
}

fn default_object_path_for_input(input_path: &Path) -> PathBuf {
    let stem = input_path
        .file_stem()
        .and_then(|stem| stem.to_str())
        .unwrap_or("out");
    let parent = input_path.parent().unwrap_or(Path::new("."));
    parent.join(format!("{stem}.o65"))
}

fn output_base_from_input(input_path: &Path) -> OutputBase {
    let stem = input_path
        .file_stem()
        .and_then(|stem| stem.to_str())
        .unwrap_or("out")
        .to_string();
    let parent = input_path.parent().unwrap_or(Path::new(".")).to_path_buf();
    OutputBase { parent, stem }
}

fn output_base_from_path(path: &Path) -> OutputBase {
    let stem = path
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or("out")
        .to_string();
    let parent = path.parent().unwrap_or(Path::new(".")).to_path_buf();
    OutputBase { parent, stem }
}

fn output_base_from_object_path(object_path: &Path) -> OutputBase {
    let stem = object_path
        .file_stem()
        .and_then(|stem| stem.to_str())
        .unwrap_or("out")
        .to_string();
    let parent = object_path.parent().unwrap_or(Path::new(".")).to_path_buf();
    OutputBase { parent, stem }
}

fn write_link_output(
    base: &OutputBase,
    linked: &k816_link::LinkOutput,
    keep_config_names: bool,
) -> anyhow::Result<()> {
    if linked.binaries.is_empty() {
        anyhow::bail!("linker produced no output binaries");
    }

    if linked.binaries.len() == 1 && !keep_config_names {
        let (_, bytes) = linked
            .binaries
            .first()
            .expect("single output should be present");
        let path = base.parent.join(format!("{}.bin", base.stem));
        std::fs::write(path, bytes)?;
    } else {
        for (name, bytes) in &linked.binaries {
            let path = if keep_config_names {
                base.parent.join(format!("{}.{}", base.stem, name))
            } else {
                base.parent
                    .join(format!("{}.{}", base.stem, sanitize_filename(name)))
            };
            std::fs::write(path, bytes)?;
        }
    }

    let listing_path = base.parent.join(format!("{}.lst", base.stem));
    std::fs::write(listing_path, &linked.listing)?;
    Ok(())
}

fn sanitize_filename(name: &str) -> String {
    name.chars()
        .map(|ch| {
            if ch.is_ascii_alphanumeric() || ch == '.' || ch == '_' || ch == '-' {
                ch
            } else {
                '_'
            }
        })
        .collect()
}

fn compile_command(args: CompileArgs) -> anyhow::Result<()> {
    let object = compile_source_file(&args.input)?;
    let out_path = args
        .output
        .unwrap_or_else(|| default_object_path_for_input(&args.input));
    k816_o65::write_object(&out_path, &object)?;
    Ok(())
}

fn link_command(args: LinkArgs) -> anyhow::Result<()> {
    let mut objects = Vec::with_capacity(args.objects.len());
    for object_path in &args.objects {
        objects.push(k816_o65::read_object(object_path)?);
    }

    let config = if let Some(config_path) = args.config {
        k816_link::load_config(&config_path)?
    } else {
        k816_link::default_stub_config()
    };
    let linked = k816_link::link_objects_with_options(
        &objects,
        &config,
        k816_link::LinkRenderOptions {
            color: stderr_supports_color(),
        },
    )?;

    let base = args
        .output
        .map(|path| output_base_from_path(&path))
        .unwrap_or_else(|| output_base_from_object_path(&args.objects[0]));
    write_link_output(&base, &linked, true)
}

fn build_command(input_path: PathBuf) -> anyhow::Result<()> {
    let object = compile_source_file(&input_path)?;
    let config = k816_link::default_stub_config();
    let linked = k816_link::link_objects_with_options(
        &[object],
        &config,
        k816_link::LinkRenderOptions {
            color: stderr_supports_color(),
        },
    )?;
    let base = output_base_from_input(&input_path);
    write_link_output(&base, &linked, false)
}
