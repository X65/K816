use std::ffi::OsString;
use std::path::{Component, Path, PathBuf};
use std::process::Command;
use std::{env, fmt, io::IsTerminal};

use anyhow::Context;
use clap::{Args, CommandFactory, Parser, Subcommand, ValueEnum};
use serde::Deserialize;

const LISTING_AUTO_SENTINEL: &str = "__auto__";
const PROJECT_MANIFEST: &str = "k816.toml";
const PROJECT_DEFAULT_LINK_SCRIPT: &str = "link.ron";
const PROJECT_SRC_DIR: &str = "src";
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
    after_help = "Examples:\n  k816 path/to/input.k65\n  k816 -T path/to/link.ld.ron path/to/input.k65\n  k816 compile path/to/input.k65\n  k816 link path/to/input.o65 -T link.ld.ron\n  k816 init hello\n  k816 build\n  k816 run -- --fast\n  k816 clean\n  k816 lsp\n  k816 --help"
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
                "--listing is not supported with `k816 {subcommand}`; use `k816 link` for listing output"
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

fn main() {
    match run() {
        Ok(()) => {}
        Err(err) => {
            if let Some(code) = err.downcast_ref::<ExitCodeError>() {
                std::process::exit(code.0);
            }
            eprintln!("{err}");
            std::process::exit(1);
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

fn compile_source_file_strict(input_path: &Path) -> anyhow::Result<k816_o65::O65Object> {
    compile_source_file_internal(input_path, false)
}

fn compile_source_file_for_link(input_path: &Path) -> anyhow::Result<k816_o65::O65Object> {
    compile_source_file_internal(input_path, true)
}

fn compile_source_file_internal(
    input_path: &Path,
    allow_undefined_symbols: bool,
) -> anyhow::Result<k816_o65::O65Object> {
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
    let render = k816_core::CompileRenderOptions {
        color: stderr_supports_color(),
    };
    let output = if allow_undefined_symbols {
        k816_core::compile_source_to_object_for_link_with_options(
            &input_path.display().to_string(),
            &source,
            render,
        )
    } else {
        k816_core::compile_source_to_object_with_options(
            &input_path.display().to_string(),
            &source,
            render,
        )
    }
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

    if let Some(choice) = env::var_os("CLICOLOR")
        && choice == "0"
    {
        return false;
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
    let object = compile_source_file_for_link(&args.input)?;
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

fn single_file_build_command(
    input_path: PathBuf,
    _compile_options: CompilePhaseOptions,
    link_options: LinkPhaseOptions,
    output: Option<PathBuf>,
) -> anyhow::Result<()> {
    let object = compile_source_file_strict(&input_path)?;
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

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct ProjectManifest {
    package: ProjectPackage,
    #[serde(default)]
    link: ProjectLink,
    #[serde(default)]
    run: ProjectRun,
}

impl ProjectManifest {
    fn validate(&self) -> anyhow::Result<()> {
        validate_package_name(&self.package.name)?;
        if self.package.version.trim().is_empty() {
            anyhow::bail!("package.version must not be empty");
        }
        Ok(())
    }
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct ProjectPackage {
    name: String,
    #[serde(default = "default_manifest_version")]
    version: String,
}

fn default_manifest_version() -> String {
    "0.1.0".to_string()
}

#[derive(Debug, Default, Deserialize)]
#[serde(default, deny_unknown_fields)]
struct ProjectLink {
    script: Option<PathBuf>,
}

#[derive(Debug, Clone, Default, Deserialize)]
#[serde(default, deny_unknown_fields)]
struct ProjectRun {
    runner: Option<String>,
    args: Vec<String>,
}

struct ProjectBuildResult {
    project_root: PathBuf,
    artifact_path: PathBuf,
}

fn validate_package_name(name: &str) -> anyhow::Result<()> {
    if name.trim().is_empty() {
        anyhow::bail!("package.name is required in k816.toml");
    }

    let mut components = Path::new(name).components();
    match (components.next(), components.next()) {
        (Some(Component::Normal(_)), None) => Ok(()),
        _ => anyhow::bail!("package.name '{}' must be a single path component", name),
    }
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
        "[package]\nname = \"{package_name}\"\nversion = \"0.1.0\"\n\n[link]\nscript = \"link.ron\"\n"
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

    let mut objects = Vec::with_capacity(sources.len());
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
        let object = compile_source_file_for_link(source)?;
        k816_o65::write_object(&object_path, &object)?;
        objects.push(object);
    }

    let resolved_config = resolve_project_link_config_path(&project_root, &manifest, link_options);
    let mut config = if let Some(path) = resolved_config.as_deref() {
        k816_link::load_config(path)?
    } else {
        k816_link::default_stub_config()
    };
    config.output.kind = resolve_output_kind(config.output.kind, None, link_options.output_format);

    let artifact_path = target_dir.join(format!(
        "{}.{}",
        manifest.package.name,
        output_extension(config.output.kind)
    ));
    let objects = k816_link::bfs_reorder(objects)?;

    println!(
        "link {} -> {}",
        objects.len(),
        display_project_path(&project_root, &artifact_path)
    );

    let linked = k816_link::link_objects_with_options(
        &objects,
        &config,
        k816_link::LinkRenderOptions {
            color: stderr_supports_color(),
        },
    )?;
    write_link_output(&artifact_path, None, &linked)?;

    Ok(ProjectBuildResult {
        project_root,
        artifact_path,
    })
}

fn resolve_project_link_config_path(
    project_root: &Path,
    manifest: &ProjectManifest,
    link_options: &LinkPhaseOptions,
) -> Option<PathBuf> {
    if let Some(path) = link_options.config.as_deref() {
        return Some(path.to_path_buf());
    }

    if let Some(script) = manifest.link.script.as_deref() {
        if script.is_absolute() {
            return Some(script.to_path_buf());
        }
        return Some(project_root.join(script));
    }

    let script = project_root.join(PROJECT_DEFAULT_LINK_SCRIPT);
    if script.exists() {
        return Some(script);
    }

    None
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

fn discover_sources(source_root: &Path) -> anyhow::Result<Vec<PathBuf>> {
    let mut out = Vec::new();
    discover_sources_rec(source_root, &mut out)?;
    Ok(out)
}

fn discover_sources_rec(dir: &Path, out: &mut Vec<PathBuf>) -> anyhow::Result<()> {
    let mut entries = std::fs::read_dir(dir)
        .with_context(|| format!("failed to read source directory '{}'", dir.display()))?
        .collect::<Result<Vec<_>, _>>()
        .with_context(|| format!("failed to read source directory '{}'", dir.display()))?;
    entries.sort_by_key(|entry| entry.path());

    for entry in entries {
        let path = entry.path();
        let file_type = entry
            .file_type()
            .with_context(|| format!("failed to inspect '{}'", path.display()))?;
        if file_type.is_dir() {
            discover_sources_rec(&path, out)?;
            continue;
        }
        if file_type.is_file() && is_k65_source_path(&path) {
            out.push(path);
        }
    }

    Ok(())
}

fn is_k65_source_path(path: &Path) -> bool {
    path.extension()
        .and_then(|ext| ext.to_str())
        .is_some_and(|ext| ext.eq_ignore_ascii_case("k65"))
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

fn load_project_manifest(project_root: &Path) -> anyhow::Result<ProjectManifest> {
    let manifest_path = project_root.join(PROJECT_MANIFEST);
    let text = std::fs::read_to_string(&manifest_path)
        .with_context(|| format!("failed to read '{}'", manifest_path.display()))?;
    let manifest: ProjectManifest = toml::from_str(&text)
        .with_context(|| format!("failed to parse '{}'", manifest_path.display()))?;
    manifest.validate()?;
    Ok(manifest)
}

fn display_project_path(project_root: &Path, path: &Path) -> String {
    path.strip_prefix(project_root)
        .unwrap_or(path)
        .display()
        .to_string()
}
