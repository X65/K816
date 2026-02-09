use std::path::Path;

use anyhow::{Context, Result, anyhow};

pub fn run_fixture(name: &str) -> Result<()> {
    let fixture_dir = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("fixtures")
        .join(name);

    let input_path = fixture_dir.join("input.k65");
    let input_path = if input_path.exists() {
        input_path
    } else {
        fixture_dir.join("input.k816")
    };
    let source = std::fs::read_to_string(&input_path)
        .with_context(|| format!("failed to read fixture input '{}'", input_path.display()))?;

    let compiled = k816_core::compile_source_to_object(&input_path.display().to_string(), &source)
        .map_err(|error| anyhow!("{}", error.rendered))?;

    let config_path = Path::new(env!("CARGO_MANIFEST_DIR")).join("link.stub.k816ld.ron");
    let config = k816_link::load_config(&config_path)?;
    let linked = k816_link::link_objects(&[compiled.object], &config)?;

    compare_binaries(&fixture_dir, &linked.binaries)?;
    compare_listing(&fixture_dir, &linked.listing)?;
    Ok(())
}

fn compare_binaries(fixture_dir: &Path, banks: &indexmap::IndexMap<String, Vec<u8>>) -> Result<()> {
    let mut expected_per_bank = Vec::new();
    for entry in std::fs::read_dir(fixture_dir)? {
        let entry = entry?;
        let path = entry.path();
        let Some(file_name) = path.file_name().and_then(|name| name.to_str()) else {
            continue;
        };
        if file_name.starts_with("expected.")
            && file_name.ends_with(".bin")
            && file_name != "expected.bin"
        {
            let bank = file_name
                .trim_start_matches("expected.")
                .trim_end_matches(".bin")
                .to_string();
            expected_per_bank.push((bank, path));
        }
    }

    if expected_per_bank.is_empty() {
        let expected_path = fixture_dir.join("expected.bin");
        let expected = std::fs::read(&expected_path)
            .with_context(|| format!("failed to read '{}'", expected_path.display()))?;
        let (_, actual) = banks
            .first()
            .ok_or_else(|| anyhow!("compiler emitted no banks"))?;
        if *actual != expected {
            return Err(anyhow!(
                "binary mismatch in fixture '{}': expected {} bytes, got {} bytes",
                fixture_dir.display(),
                expected.len(),
                actual.len(),
            ));
        }
        return Ok(());
    }

    for (bank, path) in expected_per_bank {
        let expected =
            std::fs::read(&path).with_context(|| format!("failed to read '{}'", path.display()))?;
        let actual = banks
            .get(&bank)
            .ok_or_else(|| anyhow!("missing output bank '{bank}'"))?;
        if *actual != expected {
            return Err(anyhow!(
                "binary mismatch for bank '{bank}': expected {} bytes, got {} bytes",
                expected.len(),
                actual.len()
            ));
        }
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
