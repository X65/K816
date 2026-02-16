use std::path::{Path, PathBuf};

use anyhow::{Context, Result, anyhow};

struct PipelineOutput {
    linked: k816_link::LinkedLayout,
    warnings: String,
}

#[derive(Clone, Debug)]
struct InputSource {
    path: PathBuf,
    source_name: String,
}

struct FixturePathRewriter {
    repo_root: PathBuf,
    repo_root_text: String,
    repo_root_unix_text: String,
    repo_root_windows_text: String,
}

impl FixturePathRewriter {
    fn new(manifest_dir: &Path) -> Result<Self> {
        let repo_root = manifest_dir
            .ancestors()
            .nth(2)
            .ok_or_else(|| {
                anyhow!(
                    "failed to resolve repository root from '{}'",
                    manifest_dir.display()
                )
            })?
            .to_path_buf();
        let repo_root_text = repo_root.display().to_string();
        let repo_root_unix_text = repo_root_text.replace('\\', "/");
        let repo_root_windows_text = repo_root_text.replace('/', "\\");

        Ok(Self {
            repo_root,
            repo_root_text,
            repo_root_unix_text,
            repo_root_windows_text,
        })
    }

    fn source_name(&self, path: &Path) -> String {
        self.repo_relative_path(path)
            .unwrap_or_else(|| self.normalize(path.display().to_string()))
    }

    fn normalize(&self, text: impl AsRef<str>) -> String {
        let mut normalized = text.as_ref().to_string();
        normalized = normalized.replace(&format!("{}/", self.repo_root_text), "");
        if self.repo_root_unix_text != self.repo_root_text {
            normalized = normalized.replace(&format!("{}/", self.repo_root_unix_text), "");
        }
        if self.repo_root_windows_text != self.repo_root_text
            && self.repo_root_windows_text != self.repo_root_unix_text
        {
            normalized = normalized.replace(&format!("{}\\", self.repo_root_windows_text), "");
        }
        normalized = normalized.replace(&self.repo_root_text, "");
        if self.repo_root_unix_text != self.repo_root_text {
            normalized = normalized.replace(&self.repo_root_unix_text, "");
        }
        if self.repo_root_windows_text != self.repo_root_text
            && self.repo_root_windows_text != self.repo_root_unix_text
        {
            normalized = normalized.replace(&self.repo_root_windows_text, "");
        }
        normalized
    }

    fn repo_relative_path(&self, path: &Path) -> Option<String> {
        if let Ok(relative) = path.strip_prefix(&self.repo_root) {
            return Some(self.relative_path_text(relative));
        }

        let canonical = path.canonicalize().ok()?;
        let relative = canonical.strip_prefix(&self.repo_root).ok()?;
        Some(self.relative_path_text(relative))
    }

    fn relative_path_text(&self, relative: &Path) -> String {
        relative.to_string_lossy().replace('\\', "/")
    }
}

#[derive(Clone, Copy)]
enum ExpectedBinary {
    Bin,
    Xex,
}

impl ExpectedBinary {
    fn label(self) -> &'static str {
        match self {
            Self::Bin => "expected.bin",
            Self::Xex => "expected.xex",
        }
    }

    fn kind(self) -> k816_link::OutputKind {
        match self {
            Self::Bin => k816_link::OutputKind::RawBinary,
            Self::Xex => k816_link::OutputKind::Xex,
        }
    }

    fn path(self, fixture_dir: &Path) -> std::path::PathBuf {
        fixture_dir.join(self.label())
    }
}

fn detect_expected_binary(fixture_dir: &Path) -> Result<Option<ExpectedBinary>> {
    let has_bin = fixture_dir.join("expected.bin").exists();
    let has_xex = fixture_dir.join("expected.xex").exists();

    if has_bin && has_xex {
        return Err(anyhow!(
            "fixture '{}' must not contain both expected.bin and expected.xex",
            fixture_dir.display()
        ));
    }

    Ok(if has_xex {
        Some(ExpectedBinary::Xex)
    } else if has_bin {
        Some(ExpectedBinary::Bin)
    } else {
        None
    })
}

#[derive(Debug, Clone, Copy, Default)]
pub struct BlessOptions {
    pub errors_only: bool,
}

#[derive(Debug, Default)]
pub struct BlessSummary {
    pub processed_cases: usize,
    pub skipped_cases: usize,
    pub updated_files: Vec<PathBuf>,
}

pub fn discover_cases() -> Result<Vec<String>> {
    let manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let fixtures_dir = manifest_dir.join("fixtures");
    let examples_fixtures_dir = manifest_dir.join("examples");

    let mut cases = Vec::new();
    if fixtures_dir.exists() {
        let mut fixture_cases = std::fs::read_dir(&fixtures_dir)
            .with_context(|| format!("failed to read fixtures dir '{}'", fixtures_dir.display()))?
            .filter_map(|entry| entry.ok())
            .map(|entry| entry.path())
            .filter(|path| path.is_dir())
            .filter_map(|path| {
                path.file_name()
                    .and_then(|name| name.to_str().map(str::to_string))
            })
            .map(|name| format!("fixture:{name}"))
            .collect::<Vec<_>>();
        fixture_cases.sort();
        cases.extend(fixture_cases);
    }

    if examples_fixtures_dir.exists() {
        let mut example_cases = std::fs::read_dir(&examples_fixtures_dir)
            .with_context(|| {
                format!(
                    "failed to read examples fixture dir '{}'",
                    examples_fixtures_dir.display()
                )
            })?
            .filter_map(|entry| entry.ok())
            .map(|entry| entry.path())
            .filter(|path| path.is_dir())
            .filter_map(|path| {
                path.file_name()
                    .and_then(|name| name.to_str().map(str::to_string))
            })
            .map(|name| format!("example:{name}"))
            .collect::<Vec<_>>();
        example_cases.sort();
        cases.extend(example_cases);
    }

    cases.sort();
    Ok(cases)
}

pub fn bless_cases(cases: &[String], options: BlessOptions) -> Result<BlessSummary> {
    let manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let path_rewriter = FixturePathRewriter::new(manifest_dir)?;
    let mut summary = BlessSummary::default();

    for case in cases {
        let updated = bless_case_internal(case, manifest_dir, &path_rewriter, options)?;
        summary.processed_cases += 1;
        if updated.is_empty() {
            summary.skipped_cases += 1;
        } else {
            summary.updated_files.extend(updated);
        }
    }

    Ok(summary)
}

pub fn run_case(case: &str) -> Result<()> {
    let manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let path_rewriter = FixturePathRewriter::new(manifest_dir)?;
    if cfg!(feature = "golden-bless") {
        let _updated = bless_case_internal(
            case,
            manifest_dir,
            &path_rewriter,
            BlessOptions { errors_only: false },
        )?;
        return Ok(());
    }

    let (fixture_dir, inputs, allow_adjacent_config) =
        resolve_paths(manifest_dir, &path_rewriter, case)?;

    let expected_binary = detect_expected_binary(&fixture_dir)?;
    let expected_error_path = fixture_dir.join("expected.err");
    let expected_listing_path = fixture_dir.join("expected.lst");
    if expected_error_path.exists() {
        let expected_error = read_non_empty_text(&expected_error_path, "expected.err")?;

        let pipeline_result = compile_and_link(
            &fixture_dir,
            &inputs,
            allow_adjacent_config,
            expected_binary.map(ExpectedBinary::kind),
        );

        if let Some(expected_binary) = expected_binary {
            if contains_error_marker(&expected_error) {
                return Err(anyhow!(
                    "fixture '{}' has both {} and expected.err, but expected.err must contain warnings only (no error diagnostics)",
                    fixture_dir.display(),
                    expected_binary.label()
                ));
            }

            let pipeline = pipeline_result?;
            let warnings = path_rewriter.normalize(&pipeline.warnings);
            if warnings.trim().is_empty() {
                return Err(anyhow!(
                    "fixture '{}' expected warnings in '{}', but compiler produced no warnings",
                    fixture_dir.display(),
                    expected_error_path.display()
                ));
            }
            similar_asserts::assert_eq!(expected_error.trim_end(), warnings.trim_end());
            if !warnings.trim().is_empty() {
                println!("{}", warnings.trim_end());
            }
            compare_binary_output(&fixture_dir, expected_binary, &pipeline.linked)?;
            compare_listing(&fixture_dir, &pipeline.linked.listing)?;
            return Ok(());
        }

        let err = match pipeline_result {
            Ok(_) => {
                return Err(anyhow!(
                    "fixture '{}' expected a link error, but compile+link succeeded",
                    fixture_dir.display()
                ));
            }
            Err(err) => err,
        };

        if expected_listing_path.exists() {
            return Err(anyhow!(
                "fixture '{}' has '{}', but listing comparison is only supported for successful link outputs",
                fixture_dir.display(),
                expected_listing_path.display()
            ));
        }

        let rendered_error = path_rewriter.normalize(err.to_string());
        similar_asserts::assert_eq!(expected_error.trim_end(), rendered_error.trim_end());
        return Ok(());
    }

    let Some(expected_binary) = expected_binary else {
        return Err(anyhow!(
            "fixture '{}' must provide either expected.bin or expected.xex",
            fixture_dir.display()
        ));
    };
    let pipeline = compile_and_link(
        &fixture_dir,
        &inputs,
        allow_adjacent_config,
        Some(expected_binary.kind()),
    )?;
    compare_binary_output(&fixture_dir, expected_binary, &pipeline.linked)?;
    compare_listing(&fixture_dir, &pipeline.linked.listing)?;
    Ok(())
}

fn bless_case_internal(
    case: &str,
    manifest_dir: &Path,
    path_rewriter: &FixturePathRewriter,
    options: BlessOptions,
) -> Result<Vec<PathBuf>> {
    let (fixture_dir, inputs, allow_adjacent_config) =
        resolve_paths(manifest_dir, path_rewriter, case)?;

    let expected_binary = detect_expected_binary(&fixture_dir)?;
    let expected_error_path = fixture_dir.join("expected.err");
    let expected_listing_path = fixture_dir.join("expected.lst");
    let mut updated = Vec::new();

    if options.errors_only && !expected_error_path.exists() {
        return Ok(updated);
    }

    if expected_error_path.exists() {
        let pipeline_result = compile_and_link(
            &fixture_dir,
            &inputs,
            allow_adjacent_config,
            expected_binary.map(ExpectedBinary::kind),
        );

        if let Some(expected_binary) = expected_binary {
            let pipeline = pipeline_result?;
            let warnings = path_rewriter.normalize(&pipeline.warnings);
            if warnings.trim().is_empty() {
                return Err(anyhow!(
                    "fixture '{}' expected warnings in '{}', but compiler produced no warnings",
                    fixture_dir.display(),
                    expected_error_path.display()
                ));
            }

            if write_text_if_changed(&expected_error_path, &warnings)? {
                updated.push(expected_error_path.clone());
            }
            if options.errors_only {
                return Ok(updated);
            }

            let expected_binary_path = expected_binary.path(&fixture_dir);
            let rendered =
                k816_link::render_linked_output(&pipeline.linked, expected_binary.kind())?;
            if write_bytes_if_changed(&expected_binary_path, &rendered)? {
                updated.push(expected_binary_path);
            }
            if expected_listing_path.exists()
                && write_text_if_changed(&expected_listing_path, &pipeline.linked.listing)?
            {
                updated.push(expected_listing_path);
            }
            return Ok(updated);
        }

        let err = match pipeline_result {
            Ok(_) => {
                return Err(anyhow!(
                    "fixture '{}' expected a link error, but compile+link succeeded",
                    fixture_dir.display()
                ));
            }
            Err(err) => err,
        };

        let rendered_error = path_rewriter.normalize(err.to_string());
        if write_text_if_changed(&expected_error_path, &rendered_error)? {
            updated.push(expected_error_path);
        }
        return Ok(updated);
    }

    if options.errors_only {
        return Ok(updated);
    }

    let Some(expected_binary) = expected_binary else {
        return Err(anyhow!(
            "fixture '{}' must provide either expected.bin or expected.xex",
            fixture_dir.display()
        ));
    };

    let pipeline = compile_and_link(
        &fixture_dir,
        &inputs,
        allow_adjacent_config,
        Some(expected_binary.kind()),
    )?;
    let expected_binary_path = expected_binary.path(&fixture_dir);
    let rendered = k816_link::render_linked_output(&pipeline.linked, expected_binary.kind())?;
    if write_bytes_if_changed(&expected_binary_path, &rendered)? {
        updated.push(expected_binary_path);
    }
    if expected_listing_path.exists()
        && write_text_if_changed(&expected_listing_path, &pipeline.linked.listing)?
    {
        updated.push(expected_listing_path);
    }
    Ok(updated)
}

fn resolve_paths(
    manifest_dir: &Path,
    path_rewriter: &FixturePathRewriter,
    case: &str,
) -> Result<(PathBuf, Vec<InputSource>, bool)> {
    if let Some(name) = case.strip_prefix("fixture:") {
        let fixture_dir = manifest_dir.join("fixtures").join(name);
        let input_paths = resolve_fixture_inputs(&fixture_dir)?;
        let allow_adjacent_config = input_paths.len() == 1;
        let inputs = input_paths
            .into_iter()
            .map(|path| InputSource {
                source_name: path_rewriter.source_name(&path),
                path,
            })
            .collect();
        return Ok((fixture_dir, inputs, allow_adjacent_config));
    }

    if let Some(stem) = case.strip_prefix("example:") {
        let fixture_dir = manifest_dir.join("examples").join(stem);
        let input_path = path_rewriter
            .repo_root
            .join("examples")
            .join(format!("{stem}.k65"));
        let input = InputSource {
            source_name: path_rewriter.source_name(&input_path),
            path: input_path,
        };
        return Ok((fixture_dir, vec![input], true));
    }

    Err(anyhow!(
        "unknown golden case '{case}', expected prefixes 'fixture:' or 'example:'"
    ))
}

fn resolve_fixture_inputs(fixture_dir: &Path) -> Result<Vec<PathBuf>> {
    let input_k65 = fixture_dir.join("input.k65");
    let input_k816 = fixture_dir.join("input.k816");
    let has_input_k65 = input_k65.is_file();
    let has_input_k816 = input_k816.is_file();

    if has_input_k65 && has_input_k816 {
        return Err(anyhow!(
            "fixture '{}' must not contain both input.k65 and input.k816",
            fixture_dir.display()
        ));
    }

    let mut multi_inputs = std::fs::read_dir(fixture_dir)
        .with_context(|| format!("failed to read fixture dir '{}'", fixture_dir.display()))?
        .filter_map(|entry| entry.ok())
        .map(|entry| entry.path())
        .filter(|path| path.is_file())
        .filter(|path| {
            let Some(file_name) = path.file_name().and_then(|name| name.to_str()) else {
                return false;
            };
            if file_name == "input.k65" || file_name == "input.k816" {
                return false;
            }
            if !file_name.starts_with("input.") {
                return false;
            }
            path.extension()
                .and_then(|ext| ext.to_str())
                .is_some_and(|ext| {
                    ext.eq_ignore_ascii_case("k65") || ext.eq_ignore_ascii_case("k816")
                })
        })
        .collect::<Vec<_>>();
    multi_inputs.sort();

    if (has_input_k65 || has_input_k816) && !multi_inputs.is_empty() {
        return Err(anyhow!(
            "fixture '{}' mixes input.k65/input.k816 with input.*.k65/input.*.k816 files",
            fixture_dir.display()
        ));
    }

    if has_input_k65 {
        return Ok(vec![input_k65]);
    }
    if has_input_k816 {
        return Ok(vec![input_k816]);
    }
    if !multi_inputs.is_empty() {
        return Ok(multi_inputs);
    }

    Err(anyhow!(
        "fixture '{}' must provide input.k65, input.k816, or one or more input.*.k65/input.*.k816 files",
        fixture_dir.display()
    ))
}

fn compile_and_link(
    fixture_dir: &Path,
    inputs: &[InputSource],
    allow_adjacent_config: bool,
    expected_output_kind: Option<k816_link::OutputKind>,
) -> Result<PipelineOutput> {
    let mut objects = Vec::with_capacity(inputs.len());
    let mut warnings = String::new();
    for input in inputs {
        let source = std::fs::read_to_string(&input.path)
            .with_context(|| format!("failed to read fixture input '{}'", input.path.display()))?;
        let compiled = k816_core::compile_source_to_object_for_link(&input.source_name, &source)
            .map_err(|error| anyhow!("{}", error.rendered))?;
        if !compiled.rendered_warnings.trim().is_empty() {
            if !warnings.is_empty() && !warnings.ends_with('\n') {
                warnings.push('\n');
            }
            warnings.push_str(compiled.rendered_warnings.trim_end());
            warnings.push('\n');
        }
        objects.push(compiled.object);
    }

    let local_config_path = fixture_dir.join("link.ld.ron");
    let source_config_path = if allow_adjacent_config && inputs.len() == 1 {
        Some(inputs[0].path.with_extension("ld.ron"))
    } else {
        None
    };
    let config_path = if local_config_path.exists() {
        local_config_path
    } else if source_config_path
        .as_ref()
        .is_some_and(|path| path.exists())
    {
        source_config_path.expect("checked above")
    } else {
        Path::new(env!("CARGO_MANIFEST_DIR")).join("link.stub.ld.ron")
    };
    let mut config = k816_link::load_config(&config_path)?;
    if let Some(kind) = expected_output_kind {
        config.output.kind = kind;
    }
    let linked = k816_link::link_objects(&objects, &config)?;
    Ok(PipelineOutput { linked, warnings })
}

fn compare_binary_output(
    fixture_dir: &Path,
    expected_binary: ExpectedBinary,
    linked: &k816_link::LinkedLayout,
) -> Result<()> {
    let expected_path = expected_binary.path(fixture_dir);
    let expected = read_non_empty_bytes(&expected_path, expected_binary.label())?;
    let rendered = k816_link::render_linked_output(linked, expected_binary.kind())?;
    if rendered != expected {
        return Err(anyhow!(
            "{} mismatch in fixture '{}': expected {} bytes, got {} bytes",
            expected_binary.label(),
            fixture_dir.display(),
            expected.len(),
            rendered.len(),
        ));
    }
    Ok(())
}

fn compare_listing(fixture_dir: &Path, listing: &str) -> Result<()> {
    let expected_path = fixture_dir.join("expected.lst");
    if !expected_path.exists() {
        return Ok(());
    }

    let expected = std::fs::read_to_string(&expected_path)
        .with_context(|| format!("failed to read '{}'", expected_path.display()))?;

    similar_asserts::assert_eq!(expected.trim_end(), listing.trim_end());
    Ok(())
}

fn read_non_empty_text(path: &Path, label: &str) -> Result<String> {
    let text = std::fs::read_to_string(path)
        .with_context(|| format!("failed to read '{}'", path.display()))?;
    if text.trim().is_empty() {
        return Err(anyhow!("{} '{}' must not be empty", label, path.display()));
    }
    Ok(text)
}

fn read_non_empty_bytes(path: &Path, label: &str) -> Result<Vec<u8>> {
    let bytes =
        std::fs::read(path).with_context(|| format!("failed to read '{}'", path.display()))?;
    if bytes.is_empty() {
        return Err(anyhow!("{} '{}' must not be empty", label, path.display()));
    }
    Ok(bytes)
}

fn write_text_if_changed(path: &Path, text: &str) -> Result<bool> {
    let mut normalized = text.to_string();
    if !normalized.ends_with('\n') {
        normalized.push('\n');
    }
    if std::fs::read_to_string(path).ok().as_deref() == Some(normalized.as_str()) {
        return Ok(false);
    }
    std::fs::write(path, normalized)
        .with_context(|| format!("failed to write '{}'", path.display()))?;
    Ok(true)
}

fn write_bytes_if_changed(path: &Path, bytes: &[u8]) -> Result<bool> {
    if std::fs::read(path).ok().as_deref() == Some(bytes) {
        return Ok(false);
    }
    std::fs::write(path, bytes).with_context(|| format!("failed to write '{}'", path.display()))?;
    Ok(true)
}

fn contains_error_marker(text: &str) -> bool {
    text.lines().any(|line| {
        let line = line.trim_start();
        line.starts_with("Error:") || line.starts_with("error:")
    })
}
