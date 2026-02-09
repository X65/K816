use std::path::Path;

use anyhow::{Context, Result, anyhow};

pub fn run_case(case: &str) -> Result<()> {
    let manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let (fixture_dir, input_path, source_name) = resolve_paths(manifest_dir, case)?;

    let source = std::fs::read_to_string(&input_path)
        .with_context(|| format!("failed to read fixture input '{}'", input_path.display()))?;

    let expected_error_path = fixture_dir.join("expected.err");
    if expected_error_path.exists() {
        let expected_error = std::fs::read_to_string(&expected_error_path)
            .with_context(|| format!("failed to read '{}'", expected_error_path.display()))?;

        let pipeline_result = compile_and_link(&fixture_dir, &input_path, &source_name, &source);
        let err = match pipeline_result {
            Ok(_) => {
                return Err(anyhow!(
                    "fixture '{}' expected a link error, but compile+link succeeded",
                    fixture_dir.display()
                ));
            }
            Err(err) => err,
        };

        similar_asserts::assert_eq!(expected_error.trim_end(), err.to_string().trim_end());
        return Ok(());
    }

    let linked = compile_and_link(&fixture_dir, &input_path, &source_name, &source)?;
    compare_binaries(&fixture_dir, &linked.bytes)?;
    compare_listing(&fixture_dir, &linked.listing)?;
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
) -> Result<k816_link::LinkOutput> {
    let compiled = k816_core::compile_source_to_object(source_name, source)
        .map_err(|error| anyhow!("{}", error.rendered))?;

    let local_config_path = fixture_dir.join("link.k816ld.ron");
    let source_config_path = input_path.with_extension("k816ld.ron");
    let config_path = if local_config_path.exists() {
        local_config_path
    } else if source_config_path.exists() {
        source_config_path
    } else {
        Path::new(env!("CARGO_MANIFEST_DIR")).join("link.stub.k816ld.ron")
    };
    let config = k816_link::load_config(&config_path)?;
    k816_link::link_objects(&[compiled.object], &config)
}

fn compare_binaries(fixture_dir: &Path, bytes: &[u8]) -> Result<()> {
    let expected_path = fixture_dir.join("expected.bin");
    let expected = std::fs::read(&expected_path)
        .with_context(|| format!("failed to read '{}'", expected_path.display()))?;
    if bytes != expected {
        return Err(anyhow!(
            "binary mismatch in fixture '{}': expected {} bytes, got {} bytes",
            fixture_dir.display(),
            expected.len(),
            bytes.len(),
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
