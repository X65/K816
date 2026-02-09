use std::path::Path;

use anyhow::{Context, Result, anyhow};

struct PipelineOutput {
    linked: k816_link::LinkOutput,
    warnings: String,
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

pub fn run_case(case: &str) -> Result<()> {
    let manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let (fixture_dir, input_path, source_name) = resolve_paths(manifest_dir, case)?;

    let source = std::fs::read_to_string(&input_path)
        .with_context(|| format!("failed to read fixture input '{}'", input_path.display()))?;

    let expected_binary = detect_expected_binary(&fixture_dir)?;
    let expected_error_path = fixture_dir.join("expected.err");
    let expected_listing_path = fixture_dir.join("expected.lst");
    if expected_error_path.exists() {
        let expected_error = read_non_empty_text(&expected_error_path, "expected.err")?;

        let pipeline_result = compile_and_link(
            &fixture_dir,
            &input_path,
            &source_name,
            &source,
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
            if pipeline.warnings.trim().is_empty() {
                return Err(anyhow!(
                    "fixture '{}' expected warnings in '{}', but compiler produced no warnings",
                    fixture_dir.display(),
                    expected_error_path.display()
                ));
            }
            similar_asserts::assert_eq!(expected_error.trim_end(), pipeline.warnings.trim_end());
            if !pipeline.warnings.trim().is_empty() {
                println!("{}", pipeline.warnings.trim_end());
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

        similar_asserts::assert_eq!(expected_error.trim_end(), err.to_string().trim_end());
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
        &input_path,
        &source_name,
        &source,
        Some(expected_binary.kind()),
    )?;
    compare_binary_output(&fixture_dir, expected_binary, &pipeline.linked)?;
    compare_listing(&fixture_dir, &pipeline.linked.listing)?;
    Ok(())
}

fn resolve_paths(
    manifest_dir: &Path,
    case: &str,
) -> Result<(std::path::PathBuf, std::path::PathBuf, String)> {
    if let Some(name) = case.strip_prefix("fixture:") {
        let fixture_dir = manifest_dir.join("fixtures").join(name);
        let input_k65 = fixture_dir.join("input.k65");
        let input_path = if input_k65.exists() {
            input_k65
        } else {
            fixture_dir.join("input.k816")
        };
        return Ok((
            fixture_dir,
            input_path.clone(),
            input_path.display().to_string(),
        ));
    }

    if let Some(stem) = case.strip_prefix("example:") {
        let fixture_dir = manifest_dir.join("examples").join(stem);
        let input_path = manifest_dir
            .join("../../examples")
            .join(format!("{stem}.k65"));
        return Ok((fixture_dir, input_path, format!("examples/{stem}.k65")));
    }

    Err(anyhow!(
        "unknown golden case '{case}', expected prefixes 'fixture:' or 'example:'"
    ))
}

fn compile_and_link(
    fixture_dir: &Path,
    input_path: &Path,
    source_name: &str,
    source: &str,
    expected_output_kind: Option<k816_link::OutputKind>,
) -> Result<PipelineOutput> {
    let compiled = k816_core::compile_source_to_object(source_name, source)
        .map_err(|error| anyhow!("{}", error.rendered))?;
    let warnings = compiled.rendered_warnings.clone();

    let local_config_path = fixture_dir.join("link.k816ld.ron");
    let source_config_path = input_path.with_extension("k816ld.ron");
    let config_path = if local_config_path.exists() {
        local_config_path
    } else if source_config_path.exists() {
        source_config_path
    } else {
        Path::new(env!("CARGO_MANIFEST_DIR")).join("link.stub.k816ld.ron")
    };
    let mut config = k816_link::load_config(&config_path)?;
    if let Some(kind) = expected_output_kind {
        config.output.kind = kind;
    }
    let linked = k816_link::link_objects(&[compiled.object], &config)?;
    Ok(PipelineOutput { linked, warnings })
}

fn compare_binary_output(
    fixture_dir: &Path,
    expected_binary: ExpectedBinary,
    linked: &k816_link::LinkOutput,
) -> Result<()> {
    if linked.kind != expected_binary.kind() {
        return Err(anyhow!(
            "linked output kind mismatch in fixture '{}': expected {:?}, got {:?}",
            fixture_dir.display(),
            expected_binary.kind(),
            linked.kind
        ));
    }

    let expected_path = expected_binary.path(fixture_dir);
    let expected = read_non_empty_bytes(&expected_path, expected_binary.label())?;
    if linked.bytes != expected {
        return Err(anyhow!(
            "{} mismatch in fixture '{}': expected {} bytes, got {} bytes",
            expected_binary.label(),
            fixture_dir.display(),
            expected.len(),
            linked.bytes.len(),
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
    let bytes = std::fs::read(path).with_context(|| format!("failed to read '{}'", path.display()))?;
    if bytes.is_empty() {
        return Err(anyhow!("{} '{}' must not be empty", label, path.display()));
    }
    Ok(bytes)
}

fn contains_error_marker(text: &str) -> bool {
    text.lines().any(|line| {
        let line = line.trim_start();
        line.starts_with("Error:") || line.starts_with("error:")
    })
}
