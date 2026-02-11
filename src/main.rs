use std::path::{Path, PathBuf};
use std::{env, io::IsTerminal};

use clap::{Args, CommandFactory, Parser, Subcommand, ValueEnum};

#[derive(Debug, Parser)]
#[command(
    name = "k816",
    version,
    about = "High-level assembler for the WDC 65816",
    long_about = None,
    override_usage = "k816 [COMMAND] [INPUT]",
    after_help = "Examples:\n  k816 path/to/input.k65\n  k816 -T path/to/link.ld.ron path/to/input.k65\n  k816 compile path/to/input.k65\n  k816 link path/to/input.o65 -T link.ld.ron\n  k816 --help"
)]
struct Cli {
    /// Optional explicit subcommand.
    #[command(subcommand)]
    command: Option<Commands>,

    #[command(flatten)]
    compile_options: CompilePhaseOptions,

    #[command(flatten)]
    link_options: LinkPhaseOptions,

    #[command(flatten)]
    output_option: OutputOption,

    /// Input source file.
    #[arg(value_name = "INPUT")]
    input: Option<PathBuf>,
}

#[derive(Debug, Clone, Default, Args)]
struct CompilePhaseOptions {}

impl CompilePhaseOptions {
    fn validate_for_link_subcommand(&self) -> anyhow::Result<()> {
        Ok(())
    }
}

#[derive(Debug, Clone, Default, Args)]
struct OutputOption {
    /// Output file path.
    /// In `compile`, this is the object output path.
    /// In `link` and shortcut mode, this is the linked artifact path.
    #[arg(
        short = 'o',
        long = "output",
        value_name = "OUTPUT_FILE",
        global = true
    )]
    output: Option<PathBuf>,
}

#[derive(Debug, Clone, Default, Args)]
struct LinkPhaseOptions {
    /// Linker config file in RON format.
    #[arg(short = 'T', long = "config", value_name = "CONFIG", global = true)]
    config: Option<PathBuf>,

    /// Output binary format override (`raw` or `xex`).
    #[arg(
        long = "output-format",
        value_name = "FORMAT",
        value_enum,
        global = true
    )]
    output_format: Option<CliOutputFormat>,

    /// Optional listing output path for linked output.
    /// When passed without value, writes <output>.lst.
    #[arg(
        long = "listing",
        value_name = "LISTING_FILE",
        num_args = 0..=1,
        default_missing_value = "__auto__",
        global = true
    )]
    listing: Option<String>,
}

impl LinkPhaseOptions {
    fn validate_for_compile_subcommand(&self) -> anyhow::Result<()> {
        let mut used = Vec::new();
        if self.config.is_some() {
            used.push("--config");
        }
        if self.output_format.is_some() {
            used.push("--output-format");
        }
        if self.listing.is_some() {
            used.push("--listing");
        }
        if used.is_empty() {
            return Ok(());
        }

        let verb = if used.len() == 1 { "is" } else { "are" };
        let plural = if used.len() == 1 { "" } else { "s" };
        anyhow::bail!(
            "{} {} link-only option{}; use `k816 <input.k65> ...` for compile+link shortcut or `k816 link ...`",
            used.join(", "),
            verb,
            plural
        );
    }
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
}

#[derive(Debug, Parser)]
struct LinkArgs {
    /// Input object files (.o65).
    #[arg(value_name = "OBJECT_FILE", required = true)]
    objects: Vec<PathBuf>,
}

#[derive(Debug, Clone, Copy, ValueEnum)]
enum CliOutputFormat {
    Raw,
    Xex,
}

impl CliOutputFormat {
    fn output_kind(self) -> k816_link::OutputKind {
        match self {
            Self::Raw => k816_link::OutputKind::RawBinary,
            Self::Xex => k816_link::OutputKind::Xex,
        }
    }
}

const LISTING_AUTO_SENTINEL: &str = "__auto__";

fn main() {
    if let Err(err) = run() {
        eprintln!("{err}");
        std::process::exit(1);
    }
}

fn run() -> anyhow::Result<()> {
    let Cli {
        command,
        compile_options,
        link_options,
        output_option,
        input,
    } = Cli::parse();

    match command {
        Some(Commands::Compile(args)) => {
            link_options.validate_for_compile_subcommand()?;
            compile_command(args, compile_options, output_option.output)
        }
        Some(Commands::Link(args)) => {
            compile_options.validate_for_link_subcommand()?;
            link_command(args, link_options, output_option.output)
        }
        None => {
            let Some(input_path) = input else {
                print_banner();
                println!();
                let mut command = Cli::command();
                command.print_help()?;
                println!();
                return Ok(());
            };
            build_command(
                input_path,
                compile_options,
                link_options,
                output_option.output,
            )
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
    if !output.rendered_warnings.trim().is_empty() {
        eprintln!("{}", output.rendered_warnings.trim_end());
    }
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

fn default_build_output_path(input_path: &Path, kind: k816_link::OutputKind) -> PathBuf {
    let stem = input_path
        .file_stem()
        .and_then(|stem| stem.to_str())
        .unwrap_or("out");
    let ext = match kind {
        k816_link::OutputKind::RawBinary => "bin",
        k816_link::OutputKind::Xex => "xex",
    };
    let parent = input_path.parent().unwrap_or(Path::new("."));
    parent.join(format!("{stem}.{ext}"))
}

fn resolve_config_output_path(
    config: &k816_link::LinkerConfig,
    config_path: Option<&Path>,
) -> Option<PathBuf> {
    let file = config.output.file.as_deref()?;
    let file_path = PathBuf::from(file);
    if file_path.is_absolute() {
        return Some(file_path);
    }

    let base = config_path.and_then(Path::parent).unwrap_or(Path::new("."));
    Some(base.join(file_path))
}

fn discover_adjacent_config_path(input_path: &Path) -> Option<PathBuf> {
    let path = input_path.with_extension("ld.ron");
    if path.exists() {
        return Some(path);
    }

    None
}

fn output_kind_from_extension(path: &Path) -> Option<k816_link::OutputKind> {
    let ext = path.extension()?.to_str()?;
    if ext.eq_ignore_ascii_case("bin") {
        return Some(k816_link::OutputKind::RawBinary);
    }
    if ext.eq_ignore_ascii_case("xex") {
        return Some(k816_link::OutputKind::Xex);
    }
    None
}

fn resolve_output_kind(
    configured_kind: k816_link::OutputKind,
    output_path: Option<&Path>,
    output_format: Option<CliOutputFormat>,
) -> k816_link::OutputKind {
    if let Some(format) = output_format {
        return format.output_kind();
    }

    output_path
        .and_then(output_kind_from_extension)
        .unwrap_or(configured_kind)
}

fn resolve_listing_path(output_path: &Path, listing_arg: Option<&str>) -> Option<PathBuf> {
    match listing_arg {
        None => None,
        Some(LISTING_AUTO_SENTINEL) => Some(output_path.with_extension("lst")),
        Some(path) => Some(PathBuf::from(path)),
    }
}

fn write_link_output(
    output_path: &Path,
    listing_path: Option<&Path>,
    linked: &k816_link::LinkOutput,
) -> anyhow::Result<()> {
    std::fs::write(output_path, &linked.bytes)?;
    if let Some(path) = listing_path {
        std::fs::write(path, &linked.listing)?;
    }
    Ok(())
}

fn compile_command(
    args: CompileArgs,
    _options: CompilePhaseOptions,
    output: Option<PathBuf>,
) -> anyhow::Result<()> {
    let object = compile_source_file(&args.input)?;
    let out_path = output.unwrap_or_else(|| default_object_path_for_input(&args.input));
    k816_o65::write_object(&out_path, &object)?;
    Ok(())
}

fn link_command(
    args: LinkArgs,
    options: LinkPhaseOptions,
    output: Option<PathBuf>,
) -> anyhow::Result<()> {
    let mut objects = Vec::with_capacity(args.objects.len());
    for object_path in &args.objects {
        objects.push(k816_o65::read_object(object_path)?);
    }

    let mut config = if let Some(path) = options.config.as_deref() {
        k816_link::load_config(path)?
    } else {
        k816_link::default_stub_config()
    };

    let output_path = if let Some(path) = output {
        path
    } else if let Some(path) = resolve_config_output_path(&config, options.config.as_deref()) {
        path
    } else {
        anyhow::bail!("output file must be provided via linker config output.file or -o");
    };

    config.output.kind = resolve_output_kind(
        config.output.kind,
        Some(&output_path),
        options.output_format,
    );

    let linked = k816_link::link_objects_with_options(
        &objects,
        &config,
        k816_link::LinkRenderOptions {
            color: stderr_supports_color(),
        },
    )?;

    let listing_path = resolve_listing_path(&output_path, options.listing.as_deref());
    write_link_output(&output_path, listing_path.as_deref(), &linked)
}

fn build_command(
    input_path: PathBuf,
    _compile_options: CompilePhaseOptions,
    link_options: LinkPhaseOptions,
    output: Option<PathBuf>,
) -> anyhow::Result<()> {
    let object = compile_source_file(&input_path)?;
    let resolved_config_path = link_options
        .config
        .or_else(|| discover_adjacent_config_path(&input_path));
    let mut config = if let Some(path) = resolved_config_path.as_deref() {
        k816_link::load_config(path)?
    } else {
        k816_link::default_stub_config()
    };
    let config_output_path = resolve_config_output_path(&config, resolved_config_path.as_deref());
    let output_kind = resolve_output_kind(
        config.output.kind,
        config_output_path.as_deref(),
        link_options.output_format,
    );
    config.output.kind = output_kind;
    let linked = k816_link::link_objects_with_options(
        &[object],
        &config,
        k816_link::LinkRenderOptions {
            color: stderr_supports_color(),
        },
    )?;
    let output_path = output
        .or(config_output_path)
        .unwrap_or_else(|| default_build_output_path(&input_path, linked.kind));
    let listing_path = resolve_listing_path(&output_path, link_options.listing.as_deref());
    write_link_output(&output_path, listing_path.as_deref(), &linked)
}
