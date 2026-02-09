use std::path::{Path, PathBuf};

use clap::{CommandFactory, Parser, Subcommand};
use indexmap::IndexMap;

#[derive(Debug, Parser)]
#[command(
    name = "k816",
    version,
    about = "High-level assembler for the WDC 65816",
    long_about = None,
    override_usage = "k816 [COMMAND] [INPUT]",
    after_help = "Examples:\n  k816 path/to/input.k65\n  k816 compile path/to/input.k65\n  k816 link path/to/input.k816obj\n  k816 --help"
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
    /// Compile source file into relocatable object bundle.
    Compile(CompileArgs),
    /// Link relocatable object bundle into final binary output.
    Link(LinkArgs),
}

#[derive(Debug, Parser)]
struct CompileArgs {
    /// Input source file.
    #[arg(value_name = "INPUT")]
    input: PathBuf,
    /// Output object bundle directory.
    #[arg(short = 'o', long = "output", value_name = "OBJECT_DIR")]
    output: Option<PathBuf>,
}

#[derive(Debug, Parser)]
struct LinkArgs {
    /// Input object bundle directory.
    #[arg(value_name = "OBJECT_DIR")]
    object: PathBuf,
    /// Output base path (without extension).
    #[arg(short = 'o', long = "output", value_name = "OUT_BASE")]
    output: Option<PathBuf>,
}

#[derive(Debug, Clone)]
struct ObjectBundle {
    banks: IndexMap<String, Vec<u8>>,
    listing: String,
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

fn compile_source_file(input_path: &Path) -> anyhow::Result<ObjectBundle> {
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

    let source = std::fs::read_to_string(&input_path)?;
    let output = k816_core::compile_source(&input_path.display().to_string(), &source)
        .map_err(|error| anyhow::anyhow!(error.rendered))?;
    Ok(ObjectBundle {
        banks: output.banks,
        listing: output.listing,
    })
}

fn default_object_dir_for_input(input_path: &Path) -> PathBuf {
    let stem = input_path
        .file_stem()
        .and_then(|stem| stem.to_str())
        .unwrap_or("out");
    let parent = input_path.parent().unwrap_or(Path::new("."));
    parent.join(format!("{stem}.k816obj"))
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

fn output_base_from_object_dir(object_dir: &Path) -> OutputBase {
    let raw = object_dir
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or("out");
    let stem = raw
        .strip_suffix(".k816obj")
        .unwrap_or(raw)
        .to_string();
    let parent = object_dir.parent().unwrap_or(Path::new(".")).to_path_buf();
    OutputBase { parent, stem }
}

fn write_object_bundle(dir: &Path, bundle: &ObjectBundle) -> anyhow::Result<()> {
    std::fs::create_dir_all(dir)?;

    let listing_name = "listing.lst";
    std::fs::write(dir.join(listing_name), &bundle.listing)?;

    let mut manifest = String::new();
    manifest.push_str("K816OBJ1\n");
    manifest.push_str(&format!("listing\t{listing_name}\n"));

    for (idx, (bank, bytes)) in bundle.banks.iter().enumerate() {
        let bank_file = format!("bank_{idx:04}.bin");
        std::fs::write(dir.join(&bank_file), bytes)?;
        manifest.push_str(&format!("bank\t{bank}\t{bank_file}\n"));
    }

    std::fs::write(dir.join("manifest.txt"), manifest)?;
    Ok(())
}

fn read_object_bundle(dir: &Path) -> anyhow::Result<ObjectBundle> {
    let manifest_path = dir.join("manifest.txt");
    let manifest = std::fs::read_to_string(&manifest_path)?;
    let mut lines = manifest.lines();

    let Some(magic) = lines.next() else {
        anyhow::bail!("object manifest is empty: {}", manifest_path.display());
    };
    if magic != "K816OBJ1" {
        anyhow::bail!("unsupported object manifest version in {}", manifest_path.display());
    }

    let mut listing_name: Option<String> = None;
    let mut banks: IndexMap<String, Vec<u8>> = IndexMap::new();

    for line in lines {
        let parts: Vec<&str> = line.split('\t').collect();
        if parts.is_empty() {
            continue;
        }
        match parts[0] {
            "listing" if parts.len() == 2 => {
                listing_name = Some(parts[1].to_string());
            }
            "bank" if parts.len() == 3 => {
                let bank = parts[1].to_string();
                let bytes = std::fs::read(dir.join(parts[2]))?;
                banks.insert(bank, bytes);
            }
            _ => anyhow::bail!("invalid manifest line '{}'", line),
        }
    }

    let Some(listing_name) = listing_name else {
        anyhow::bail!("manifest is missing listing entry");
    };
    let listing = std::fs::read_to_string(dir.join(listing_name))?;
    Ok(ObjectBundle { banks, listing })
}

fn write_link_output(base: &OutputBase, bundle: &ObjectBundle) -> anyhow::Result<()> {
    if bundle.banks.len() == 1 {
        let (_, bytes) = bundle
            .banks
            .first()
            .expect("a single bank should be present");
        let bin_path = base.parent.join(format!("{}.bin", base.stem));
        std::fs::write(bin_path, bytes)?;
    } else {
        for (bank, bytes) in &bundle.banks {
            let bin_path = base.parent.join(format!("{}.{}.bin", base.stem, bank));
            std::fs::write(bin_path, bytes)?;
        }
    }

    let listing_path = base.parent.join(format!("{}.lst", base.stem));
    std::fs::write(listing_path, &bundle.listing)?;
    Ok(())
}

fn compile_command(args: CompileArgs) -> anyhow::Result<()> {
    let bundle = compile_source_file(&args.input)?;
    let out_dir = args
        .output
        .unwrap_or_else(|| default_object_dir_for_input(&args.input));
    write_object_bundle(&out_dir, &bundle)
}

fn link_command(args: LinkArgs) -> anyhow::Result<()> {
    let bundle = read_object_bundle(&args.object)?;
    let base = args
        .output
        .map(|path| output_base_from_path(&path))
        .unwrap_or_else(|| output_base_from_object_dir(&args.object));
    write_link_output(&base, &bundle)
}

fn build_command(input_path: PathBuf) -> anyhow::Result<()> {
    let bundle = compile_source_file(&input_path)?;
    let base = output_base_from_input(&input_path);
    write_link_output(&base, &bundle)
}
