use std::collections::HashMap;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};

fn main() {
    let manifest_dir = PathBuf::from(
        env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR should be set by Cargo"),
    );
    let fixtures_dir = manifest_dir.join("fixtures");
    println!("cargo:rerun-if-changed={}", fixtures_dir.display());

    let fixture_names = discover_fixture_names(&fixtures_dir);
    let generated = generate_tests(&fixture_names);

    let out_dir =
        PathBuf::from(env::var("OUT_DIR").expect("OUT_DIR should be set by Cargo build script"));
    let out_file = out_dir.join("generated_golden_tests.rs");
    fs::write(&out_file, generated).expect("failed to write generated golden tests");
}

fn discover_fixture_names(fixtures_dir: &Path) -> Vec<String> {
    let mut names = Vec::new();
    let entries = fs::read_dir(fixtures_dir).unwrap_or_else(|err| {
        panic!(
            "failed to read fixtures dir '{}': {err}",
            fixtures_dir.display()
        )
    });

    for entry in entries {
        let entry = entry.expect("failed to read fixture entry");
        let path = entry.path();
        println!("cargo:rerun-if-changed={}", path.display());

        if !path.is_dir() {
            continue;
        }

        let name = entry
            .file_name()
            .into_string()
            .unwrap_or_else(|_| panic!("fixture directory name must be valid UTF-8"));
        names.push(name);

        let fixture_files = fs::read_dir(&path)
            .unwrap_or_else(|err| panic!("failed to read '{}': {err}", path.display()));
        for fixture_file in fixture_files {
            let fixture_file = fixture_file.expect("failed to read fixture file entry");
            println!("cargo:rerun-if-changed={}", fixture_file.path().display());
        }
    }

    names.sort();
    names
}

fn generate_tests(fixture_names: &[String]) -> String {
    let mut out = String::new();
    out.push_str("use k816_golden_tests::harness::run_fixture;\n\n");

    if fixture_names.is_empty() {
        out.push_str("#[test]\nfn no_fixtures_found() {\n    panic!(\"no fixtures found under tests/golden/fixtures\");\n}\n");
        return out;
    }

    let mut used_names: HashMap<String, usize> = HashMap::new();
    for fixture_name in fixture_names {
        let base_name = format!("fixture_{}", sanitize_ident(fixture_name));
        let count = used_names.entry(base_name.clone()).or_insert(0);
        let function_name = if *count == 0 {
            base_name
        } else {
            format!("{}_{}", base_name, count)
        };
        *count += 1;

        out.push_str(&format!(
            "#[test]\nfn {function_name}() {{\n    run_fixture({fixture_name:?})\n        .unwrap_or_else(|err| panic!(\"fixture {{}} failed:\\n{{err:#}}\", {fixture_name:?}));\n}}\n\n"
        ));
    }

    out
}

fn sanitize_ident(input: &str) -> String {
    let mut ident = String::new();
    for ch in input.chars() {
        if ch.is_ascii_alphanumeric() {
            ident.push(ch.to_ascii_lowercase());
        } else {
            ident.push('_');
        }
    }

    while ident.contains("__") {
        ident = ident.replace("__", "_");
    }
    ident = ident.trim_matches('_').to_string();

    if ident.is_empty() {
        return "fixture".to_string();
    }
    if ident
        .chars()
        .next()
        .expect("non-empty ident")
        .is_ascii_digit()
    {
        return format!("_{ident}");
    }

    ident
}
