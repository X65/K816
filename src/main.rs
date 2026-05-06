use std::ffi::OsString;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::{env, fmt, io::IsTerminal};

use anyhow::Context;
use clap::{Args, CommandFactory, Parser, Subcommand, ValueEnum};
use k816_project::{
    ListingOption, PROJECT_DEFAULT_LINK_SCRIPT, PROJECT_MANIFEST, PROJECT_SRC_DIR,
    discover_sources, load_project_manifest, resolve_project_link_config_path,
    validate_package_name,
};

const LISTING_AUTO_SENTINEL: &str = "__auto__";
const PROJECT_MAIN_SOURCE: &str = "main.k65";
const PROJECT_TARGET_DIR: &str = "target";
const PROJECT_OBJECT_DIR: &str = "obj";

#[derive(Debug, Parser)]
#[command(
    name = "k816",
    version,
    about = "High-level assembler for the WDC 65816",
    long_about = None,
    override_usage = "k816 [COMMAND] [INPUT]",
    after_help = "Examples:\n  k816 path/to/input.k65\n  k816 -T path/to/link.ld.ron path/to/input.k65\n  k816 compile path/to/input.k65\n  k816 link path/to/input.o65 -T link.ld.ron\n  k816 init hello\n  k816 build\n  k816 run -- --fast\n  k816 clean\n  k816 fmt src/*.k65\n  k816 fmt --check src/*.k65\n  k816 lsp\n  k816 --help"
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

impl OutputOption {
    fn validate_absent_for_subcommand(&self, subcommand: &str) -> anyhow::Result<()> {
        if self.output.is_none() {
            return Ok(());
        }

        anyhow::bail!(
            "-o/--output is not supported with `k816 {subcommand}`; use `k816 link` for explicit output paths"
        );
    }
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
        default_missing_value = LISTING_AUTO_SENTINEL,
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

    fn validate_for_project_build_or_run(&self, subcommand: &str) -> anyhow::Result<()> {
        if self.listing.is_some() {
            anyhow::bail!(
                "--listing is not supported with `k816 {subcommand}`; set listing in [link] section of k816.toml instead"
            );
        }
        Ok(())
    }

    fn validate_absent_for_subcommand(&self, subcommand: &str) -> anyhow::Result<()> {
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
            "{} {} not supported with `k816 {subcommand}` command{}",
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
    /// Scaffold a new project directory.
    Init(InitArgs),
    /// Build the current k816 project.
    Build,
    /// Build and run the current k816 project.
    Run(RunArgs),
    /// Remove project build outputs.
    Clean,
    /// Run the Language Server Protocol server over stdio.
    Lsp,
    /// Format source files.
    Fmt(FmtArgs),
    /// Dump resolved project metadata as JSON (for editor integration).
    Metadata,
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

#[derive(Debug, Args)]
struct InitArgs {
    /// Project directory name to create (use '.' for current directory).
    #[arg(value_name = "NAME")]
    name: PathBuf,

    /// Overwrite managed files if they already exist.
    #[arg(long)]
    force: bool,
}

#[derive(Debug, Args)]
struct RunArgs {
    /// Additional args forwarded to the configured runner.
    #[arg(value_name = "ARGS", trailing_var_arg = true)]
    forwarded_args: Vec<String>,
}

#[derive(Debug, Parser)]
struct FmtArgs {
    /// Input source files (.k65).
    #[arg(value_name = "INPUT", required = true)]
    input: Vec<PathBuf>,

    /// Check formatting without writing changes; exit 1 if any file would change.
    #[arg(long)]
    check: bool,
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

#[derive(Debug)]
struct ExitCodeError(i32);

impl fmt::Display for ExitCodeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "process exited with status {}", self.0)
    }
}

impl std::error::Error for ExitCodeError {}

#[derive(Debug)]
struct RenderedDiagnosticError(String);

impl fmt::Display for RenderedDiagnosticError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

impl std::error::Error for RenderedDiagnosticError {}

fn main() {
    match run() {
        Ok(()) => {}
        Err(err) => {
            if let Some(code) = err.downcast_ref::<ExitCodeError>() {
                std::process::exit(code.0);
            }
            print_error(&err);
            std::process::exit(1);
        }
    }
}

fn print_error(err: &anyhow::Error) {
    if let Some(rendered) = err.downcast_ref::<RenderedDiagnosticError>() {
        eprintln!("{}", rendered.0.trim_end());
        return;
    }

    eprintln!("Error: {err}");
    let causes: Vec<_> = err.chain().skip(1).collect();
    if causes.is_empty() {
        return;
    }
    eprintln!();
    eprintln!("Caused by:");
    for (i, cause) in causes.iter().enumerate() {
        let text = cause.to_string();
        let mut lines = text.lines();
        if let Some(first) = lines.next() {
            eprintln!("  {i}: {first}");
        }
        for line in lines {
            eprintln!("     {line}");
        }
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
        Some(Commands::Init(args)) => {
            link_options.validate_absent_for_subcommand("init")?;
            output_option.validate_absent_for_subcommand("init")?;
            init_command(args)
        }
        Some(Commands::Build) => {
            link_options.validate_for_project_build_or_run("build")?;
            output_option.validate_absent_for_subcommand("build")?;
            project_build_command(link_options)
        }
        Some(Commands::Run(args)) => {
            link_options.validate_for_project_build_or_run("run")?;
            output_option.validate_absent_for_subcommand("run")?;
            project_run_command(args, link_options)
        }
        Some(Commands::Clean) => {
            link_options.validate_absent_for_subcommand("clean")?;
            output_option.validate_absent_for_subcommand("clean")?;
            project_clean_command()
        }
        Some(Commands::Lsp) => {
            link_options.validate_absent_for_subcommand("lsp")?;
            output_option.validate_absent_for_subcommand("lsp")?;
            k816_lsp::run_stdio_server()
        }
        Some(Commands::Fmt(args)) => {
            link_options.validate_absent_for_subcommand("fmt")?;
            output_option.validate_absent_for_subcommand("fmt")?;
            fmt_command(args)
        }
        Some(Commands::Metadata) => {
            link_options.validate_for_project_build_or_run("metadata")?;
            output_option.validate_absent_for_subcommand("metadata")?;
            metadata_command(link_options)
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
            single_file_build_command(
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

fn read_source_file(path: &Path) -> anyhow::Result<String> {
    std::fs::read_to_string(path).with_context(|| format!("failed to read '{}'", path.display()))
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

    let source = read_source_file(input_path)?;
    let output = k816_core::compile_source(
        &input_path.display().to_string(),
        &source,
        auto_compile_render(),
    )
    .map_err(|error| anyhow::Error::new(RenderedDiagnosticError(error.rendered)))?;
    if !output.rendered_warnings.trim().is_empty() {
        eprintln!("{}", output.rendered_warnings.trim_end());
    }
    Ok(output.object)
}

/// Render the linker's structured `LinkErrors` through the same machinery the
/// compiler uses for its diagnostics. The resulting string, wrapped in
/// `RenderedDiagnosticError`, flows through `print_error` indistinguishably
/// from a compile failure.
fn link_errors_to_rendered(
    link_errors: &k816_link::LinkErrors,
    source_map: &k816_core::span::SourceMap,
    source_ids: &[k816_core::span::SourceId],
    source_names: &[String],
) -> RenderedDiagnosticError {
    let resolve = |path: &str| -> Option<k816_core::span::SourceId> {
        source_ids
            .iter()
            .zip(source_names.iter())
            .find(|(_, name)| name.as_str() == path)
            .map(|(id, _)| *id)
    };
    let rendered = k816_core::render_link_errors(
        link_errors,
        source_map,
        &resolve,
        k816_core::diag::RenderOptions {
            color: stderr_supports_color(),
        },
    );
    RenderedDiagnosticError(rendered)
}

fn stderr_supports_color() -> bool {
    if env::var_os("NO_COLOR").is_some() {
        return false;
    }

    if let Some(force) = env::var_os("CLICOLOR_FORCE") {
        return force != "0";
    }

    if let Some(choice) = env::var_os("CLICOLOR")
        && choice == "0"
    {
        return false;
    }

    std::io::stderr().is_terminal()
}

fn auto_compile_render() -> k816_core::CompileRenderOptions {
    k816_core::CompileRenderOptions {
        color: stderr_supports_color(),
    }
}

fn auto_link_render() -> k816_link::LinkRenderOptions {
    k816_link::LinkRenderOptions {
        color: stderr_supports_color(),
    }
}

fn load_link_config(path: Option<&Path>) -> anyhow::Result<k816_link::LinkerConfig> {
    match path {
        Some(path) => k816_link::load_config(path),
        None => Ok(k816_link::default_stub_config()),
    }
}

/// Bundles a loaded linker config with its source path and resolved `output.file`,
/// and exposes a single point that applies the output-kind precedence rules
/// (CLI `--format` override > `output_path` extension > config `output.kind`).
struct LinkPhaseContext {
    config: k816_link::LinkerConfig,
    config_output_path: Option<PathBuf>,
}

impl LinkPhaseContext {
    fn load(config_path: Option<&Path>) -> anyhow::Result<Self> {
        let config = load_link_config(config_path)?;
        let config_output_path = resolve_config_output_path(&config, config_path);
        Ok(Self {
            config,
            config_output_path,
        })
    }

    fn apply_output_kind(
        &mut self,
        hint: Option<&Path>,
        format: Option<CliOutputFormat>,
    ) -> k816_link::OutputKind {
        let kind = resolve_output_kind(self.config.output.kind, hint, format);
        self.config.output.kind = kind;
        kind
    }
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
    linked: &k816_link::LinkedLayout,
    output_kind: k816_link::OutputKind,
) -> anyhow::Result<()> {
    let bytes = k816_link::render_linked_output(linked, output_kind)?;
    std::fs::write(output_path, &bytes)?;
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

fn fmt_command(args: FmtArgs) -> anyhow::Result<()> {
    let mut unformatted_count = 0u32;
    for path in &args.input {
        let source = read_source_file(path)?;
        let formatted = k816_fmt::format_source(&source);
        if formatted == source {
            continue;
        }
        if args.check {
            println!("{}", path.display());
            unformatted_count += 1;
        } else {
            std::fs::write(path, &formatted)
                .with_context(|| format!("failed to write '{}'", path.display()))?;
        }
    }
    if args.check && unformatted_count > 0 {
        anyhow::bail!(
            "{} file{} would be reformatted",
            unformatted_count,
            if unformatted_count == 1 { "" } else { "s" }
        );
    }
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

    let mut ctx = LinkPhaseContext::load(options.config.as_deref())?;

    let output_path = output
        .or_else(|| ctx.config_output_path.clone())
        .ok_or_else(|| {
            anyhow::anyhow!("output file must be provided via linker config output.file or -o")
        })?;

    let output_kind = ctx.apply_output_kind(Some(&output_path), options.output_format);

    let linked = k816_link::link_objects_with_options(&objects, &ctx.config, auto_link_render())?;

    let listing_path = resolve_listing_path(&output_path, options.listing.as_deref());
    write_link_output(&output_path, listing_path.as_deref(), &linked, output_kind)
}

fn single_file_build_command(
    input_path: PathBuf,
    _compile_options: CompilePhaseOptions,
    link_options: LinkPhaseOptions,
    output: Option<PathBuf>,
) -> anyhow::Result<()> {
    let source = read_source_file(&input_path)?;
    let source_name = input_path.display().to_string();
    let compile_inputs = k816_core::LinkCompileInput::from_pairs(std::iter::once((
        source_name.as_str(),
        source.as_str(),
    )));
    let (compile_outputs, source_map, source_ids) =
        k816_core::compile_sources_all_or_nothing_keeping_map(
            &compile_inputs,
            auto_compile_render(),
        )
        .map_err(|error| anyhow::Error::new(RenderedDiagnosticError(error.rendered)))?;

    let mut objects = Vec::with_capacity(compile_outputs.len());
    for output in compile_outputs {
        if !output.rendered_warnings.trim().is_empty() {
            eprintln!("{}", output.rendered_warnings.trim_end());
        }
        objects.push(output.object);
    }

    let resolved_config_path = link_options
        .config
        .or_else(|| discover_adjacent_config_path(&input_path));
    let mut ctx = LinkPhaseContext::load(resolved_config_path.as_deref())?;
    let config_output_path = ctx.config_output_path.clone();
    let output_kind =
        ctx.apply_output_kind(config_output_path.as_deref(), link_options.output_format);
    let linked = k816_link::link_objects_diagnostics(&objects, &ctx.config).map_err(|errors| {
        anyhow::Error::new(link_errors_to_rendered(
            &errors,
            &source_map,
            &source_ids,
            std::slice::from_ref(&source_name),
        ))
    })?;
    let output_path = output
        .or(config_output_path)
        .unwrap_or_else(|| default_build_output_path(&input_path, output_kind));
    let listing_path = resolve_listing_path(&output_path, link_options.listing.as_deref());
    write_link_output(&output_path, listing_path.as_deref(), &linked, output_kind)
}

struct ProjectBuildResult {
    project_root: PathBuf,
    artifact_path: PathBuf,
}

fn init_command(args: InitArgs) -> anyhow::Result<()> {
    let cwd = env::current_dir().context("failed to resolve current directory")?;
    let project_root = if args.name == Path::new(".") {
        cwd.clone()
    } else if args.name.is_absolute() {
        args.name.clone()
    } else {
        cwd.join(&args.name)
    };

    if project_root.exists() && !project_root.is_dir() {
        anyhow::bail!(
            "project path '{}' exists and is not a directory",
            project_root.display()
        );
    }

    std::fs::create_dir_all(&project_root)
        .with_context(|| format!("failed to create project dir '{}'", project_root.display()))?;

    let package_name = project_root
        .file_name()
        .and_then(|name| name.to_str())
        .map(str::to_string)
        .ok_or_else(|| anyhow::anyhow!("failed to infer package name from project path"))?;
    validate_package_name(&package_name)?;

    let manifest_path = project_root.join(PROJECT_MANIFEST);
    let source_path = project_root.join(PROJECT_SRC_DIR).join(PROJECT_MAIN_SOURCE);
    let link_path = project_root.join(PROJECT_DEFAULT_LINK_SCRIPT);
    let gitignore_path = project_root.join(".gitignore");

    let managed_files = [&manifest_path, &source_path, &link_path, &gitignore_path];
    if !args.force
        && let Some(existing) = managed_files.iter().find(|path| path.exists())
    {
        anyhow::bail!(
            "refusing to overwrite existing file '{}'; use --force to overwrite",
            existing.display()
        );
    }

    write_text_file(&manifest_path, &render_manifest_template(&package_name))?;
    write_text_file(&source_path, default_main_source_template())?;
    write_text_file(&link_path, &render_link_script_template(&package_name))?;
    write_text_file(&gitignore_path, "target/\n")?;

    println!("create {}", display_project_path(&cwd, &project_root));
    println!(
        "write {}",
        display_project_path(&project_root, &manifest_path)
    );
    println!(
        "write {}",
        display_project_path(&project_root, &source_path)
    );
    println!("write {}", display_project_path(&project_root, &link_path));
    println!(
        "write {}",
        display_project_path(&project_root, &gitignore_path)
    );
    Ok(())
}

fn write_text_file(path: &Path, contents: &str) -> anyhow::Result<()> {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)
            .with_context(|| format!("failed to create directory '{}'", parent.display()))?;
    }
    std::fs::write(path, contents).with_context(|| format!("failed to write '{}'", path.display()))
}

fn render_manifest_template(package_name: &str) -> String {
    format!(
        "[package]\nname = \"{package_name}\"\nversion = \"0.1.0\"\n\n[link]\nscript = \"link.ron\"\n#listing = true\n"
    )
}

fn default_main_source_template() -> &'static str {
    "func main {\n  nop\n}\n"
}

fn render_link_script_template(package_name: &str) -> String {
    format!(
        r#"( 
  format: "o65-link",
  target: Some("{package_name}"),
  memory: [
    (
      name: "MAIN",
      start: 0,
      size: 65536,
      kind: ReadWrite,
      fill: Some(0),
    ),
  ],
  segments: [
    (
      id: "DEFAULT",
      load: "MAIN",
      run: None,
      align: Some(1),
      start: None,
      offset: None,
      optional: false,
      segment: None,
    ),
  ],
  symbols: [],
  output: (
    kind: Xex,
    file: None,
  ),
  entry: None,
)
"#
    )
}

fn project_build_command(link_options: LinkPhaseOptions) -> anyhow::Result<()> {
    let _ = project_build_internal(&link_options)?;
    Ok(())
}

fn metadata_command(link_options: LinkPhaseOptions) -> anyhow::Result<()> {
    let project_root = resolve_project_root()?;
    let manifest = load_project_manifest(&project_root)?;
    let target_dir = project_root.join(PROJECT_TARGET_DIR);

    let resolved_config =
        resolve_project_link_config_path(&project_root, &manifest, link_options.config.as_deref());
    let mut ctx = LinkPhaseContext::load(resolved_config.as_deref())?;
    let output_kind = ctx.apply_output_kind(None, link_options.output_format);
    let ext = output_extension(output_kind);
    let artifact_path = target_dir.join(format!("{}.{}", manifest.package.name, ext));

    let metadata = serde_json::json!({
        "project_root": project_root,
        "package": { "name": manifest.package.name },
        "target_dir": target_dir,
        "artifact": {
            "path": artifact_path,
            "kind": ext,
        },
        "run": {
            "runner": manifest.run.runner,
            "args": manifest.run.args,
        },
    });
    println!("{}", serde_json::to_string_pretty(&metadata)?);
    Ok(())
}

fn project_run_command(args: RunArgs, link_options: LinkPhaseOptions) -> anyhow::Result<()> {
    let run_manifest = load_project_manifest(&resolve_project_root()?)?.run;
    let Some(runner) = run_manifest.runner.as_deref() else {
        anyhow::bail!("No runner configured. Set [run].runner in k816.toml.");
    };

    let build = project_build_internal(&link_options)?;

    let artifact_arg = build
        .artifact_path
        .strip_prefix(&build.project_root)
        .unwrap_or(&build.artifact_path)
        .to_path_buf();

    let mut printable_args =
        Vec::with_capacity(1 + run_manifest.args.len() + args.forwarded_args.len() + 1);
    printable_args.push(OsString::from(runner));
    for arg in &run_manifest.args {
        printable_args.push(OsString::from(arg));
    }
    printable_args.push(artifact_arg.as_os_str().to_os_string());
    for arg in &args.forwarded_args {
        printable_args.push(OsString::from(arg));
    }
    println!("run {}", render_command_line(&printable_args));

    let mut command = Command::new(runner);
    command.current_dir(&build.project_root);
    command.args(&run_manifest.args);
    command.arg(&artifact_arg);
    command.args(&args.forwarded_args);

    let status = command
        .status()
        .with_context(|| format!("failed to execute runner '{runner}'"))?;
    if status.success() {
        return Ok(());
    }

    let exit_code = status.code().unwrap_or(1);
    Err(anyhow::Error::new(ExitCodeError(exit_code)))
}

fn render_command_line(parts: &[OsString]) -> String {
    parts
        .iter()
        .map(|part| {
            let text = part.to_string_lossy();
            if text.chars().all(is_unquoted_cli_char) {
                text.into_owned()
            } else {
                format!("{text:?}")
            }
        })
        .collect::<Vec<_>>()
        .join(" ")
}

fn is_unquoted_cli_char(ch: char) -> bool {
    ch.is_ascii_alphanumeric() || matches!(ch, '/' | '.' | '_' | '-' | ':' | '=' | '+' | ',')
}

fn project_clean_command() -> anyhow::Result<()> {
    let project_root = resolve_project_root()?;
    let target_dir = project_root.join(PROJECT_TARGET_DIR);
    if !target_dir.exists() {
        return Ok(());
    }

    let metadata = std::fs::symlink_metadata(&target_dir)
        .with_context(|| format!("failed to inspect '{}'", target_dir.display()))?;
    if metadata.file_type().is_symlink() {
        anyhow::bail!(
            "refusing to clean symlinked target directory '{}'",
            display_project_path(&project_root, &target_dir)
        );
    }

    println!("clean {}", display_project_path(&project_root, &target_dir));
    std::fs::remove_dir_all(&target_dir)
        .with_context(|| format!("failed to clean '{}'", target_dir.display()))
}

fn project_build_internal(link_options: &LinkPhaseOptions) -> anyhow::Result<ProjectBuildResult> {
    let project_root = resolve_project_root()?;
    let manifest = load_project_manifest(&project_root)?;

    let source_root = project_root.join(PROJECT_SRC_DIR);
    if !source_root.is_dir() {
        anyhow::bail!(
            "project source directory '{}' does not exist",
            display_project_path(&project_root, &source_root)
        );
    }

    let mut sources = discover_sources(&source_root)?;
    if sources.is_empty() {
        anyhow::bail!("no .k65 sources found under '{}'", PROJECT_SRC_DIR);
    }
    sources.sort();

    let target_dir = project_root.join(PROJECT_TARGET_DIR);
    let object_root = target_dir.join(PROJECT_OBJECT_DIR);
    std::fs::create_dir_all(&object_root)
        .with_context(|| format!("failed to create '{}'", object_root.display()))?;

    let mut loaded_sources = Vec::with_capacity(sources.len());
    for source in &sources {
        let object_path = object_path_for_source(&source_root, &object_root, source)?;
        if let Some(parent) = object_path.parent() {
            std::fs::create_dir_all(parent)
                .with_context(|| format!("failed to create '{}'", parent.display()))?;
        }
        println!(
            "compile {} -> {}",
            display_project_path(&project_root, source),
            display_project_path(&project_root, &object_path)
        );
        let source_text = read_source_file(source)?;
        loaded_sources.push((object_path, source.display().to_string(), source_text));
    }

    let compile_inputs = k816_core::LinkCompileInput::from_pairs(loaded_sources.iter().map(
        |(_object_path, source_name, source_text)| (source_name.as_str(), source_text.as_str()),
    ));
    let (compile_outputs, source_map, source_ids) =
        k816_core::compile_sources_all_or_nothing_keeping_map(
            &compile_inputs,
            auto_compile_render(),
        )
        .map_err(|error| anyhow::Error::new(RenderedDiagnosticError(error.rendered)))?;

    let source_names: Vec<String> = loaded_sources
        .iter()
        .map(|(_, source_name, _)| source_name.clone())
        .collect();

    let mut objects = Vec::with_capacity(compile_outputs.len());
    for ((object_path, _source_name, _source_text), output) in
        loaded_sources.into_iter().zip(compile_outputs)
    {
        if !output.rendered_warnings.trim().is_empty() {
            eprintln!("{}", output.rendered_warnings.trim_end());
        }
        k816_o65::write_object(&object_path, &output.object)?;
        objects.push(output.object);
    }

    let resolved_config =
        resolve_project_link_config_path(&project_root, &manifest, link_options.config.as_deref());
    let mut ctx = LinkPhaseContext::load(resolved_config.as_deref())?;
    let output_kind = ctx.apply_output_kind(None, link_options.output_format);

    let artifact_path = target_dir.join(format!(
        "{}.{}",
        manifest.package.name,
        output_extension(output_kind)
    ));
    let objects = k816_link::bfs_reorder(objects)?;

    println!(
        "link {} -> {}",
        objects.len(),
        display_project_path(&project_root, &artifact_path)
    );

    let linked = k816_link::link_objects_diagnostics(&objects, &ctx.config).map_err(|errors| {
        anyhow::Error::new(link_errors_to_rendered(
            &errors,
            &source_map,
            &source_ids,
            &source_names,
        ))
    })?;
    let listing_path = match &manifest.link.listing {
        ListingOption::Bool(false) => None,
        ListingOption::Bool(true) => Some(artifact_path.with_extension("lst")),
        ListingOption::Path(path) => Some(project_root.join(path)),
    };
    write_link_output(
        &artifact_path,
        listing_path.as_deref(),
        &linked,
        output_kind,
    )?;

    Ok(ProjectBuildResult {
        project_root,
        artifact_path,
    })
}

fn output_extension(kind: k816_link::OutputKind) -> &'static str {
    match kind {
        k816_link::OutputKind::RawBinary => "bin",
        k816_link::OutputKind::Xex => "xex",
    }
}

fn object_path_for_source(
    source_root: &Path,
    object_root: &Path,
    source: &Path,
) -> anyhow::Result<PathBuf> {
    let rel = source.strip_prefix(source_root).with_context(|| {
        format!(
            "source '{}' is outside '{}'",
            source.display(),
            source_root.display()
        )
    })?;
    let mut rel_object = rel.to_path_buf();
    rel_object.set_extension("o65");
    Ok(object_root.join(rel_object))
}

fn resolve_project_root() -> anyhow::Result<PathBuf> {
    let cwd = env::current_dir().context("failed to resolve current directory")?;
    find_project_root(&cwd).ok_or_else(|| anyhow::anyhow!("No k816.toml found (run k816 init)."))
}

fn find_project_root(start: &Path) -> Option<PathBuf> {
    for dir in start.ancestors() {
        let manifest = dir.join(PROJECT_MANIFEST);
        if manifest.is_file() {
            return Some(dir.to_path_buf());
        }
    }
    None
}

fn display_project_path(project_root: &Path, path: &Path) -> String {
    path.strip_prefix(project_root)
        .unwrap_or(path)
        .display()
        .to_string()
}
