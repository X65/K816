use std::path::{Path, PathBuf};

use anyhow::{Result, anyhow, bail};
use k816_golden_tests::harness::{BlessOptions, bless_cases, discover_cases};

fn main() {
    if let Err(error) = run() {
        eprintln!("{error:#}");
        std::process::exit(1);
    }
}

fn run() -> Result<()> {
    let mut errors_only = false;
    let mut list_only = false;
    let mut cases: Vec<String> = Vec::new();

    let args = std::env::args().skip(1).collect::<Vec<_>>();
    let mut index = 0usize;
    while index < args.len() {
        match args[index].as_str() {
            "--errors-only" | "--err-only" => {
                errors_only = true;
                index += 1;
            }
            "--list" => {
                list_only = true;
                index += 1;
            }
            "--case" => {
                let Some(value) = args.get(index + 1) else {
                    bail!("--case requires a value");
                };
                cases.push(normalize_case_arg(value));
                index += 2;
            }
            "--help" | "-h" => {
                print_usage();
                return Ok(());
            }
            unknown => {
                return Err(anyhow!(
                    "unknown argument '{unknown}'. Use --help to view usage."
                ));
            }
        }
    }

    if list_only {
        for case in discover_cases()? {
            println!("{case}");
        }
        return Ok(());
    }

    if cases.is_empty() {
        cases = discover_cases()?;
    } else {
        cases.sort();
        cases.dedup();
    }

    let summary = bless_cases(&cases, BlessOptions { errors_only })?;
    let repo_root = workspace_root()?;
    println!(
        "Processed {} case(s), updated {} file(s).",
        summary.processed_cases,
        summary.updated_files.len()
    );
    for path in summary.updated_files {
        println!("updated {}", display_repo_relative(&repo_root, &path));
    }
    Ok(())
}

fn normalize_case_arg(raw: &str) -> String {
    if raw.starts_with("fixture:") || raw.starts_with("example:") {
        raw.to_string()
    } else {
        format!("fixture:{raw}")
    }
}

fn workspace_root() -> Result<PathBuf> {
    let manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    manifest_dir
        .ancestors()
        .nth(2)
        .map(Path::to_path_buf)
        .ok_or_else(|| {
            anyhow!(
                "failed to resolve workspace root from '{}'",
                manifest_dir.display()
            )
        })
}

fn display_repo_relative(repo_root: &Path, path: &Path) -> String {
    if let Ok(relative) = path.strip_prefix(repo_root) {
        return relative.to_string_lossy().replace('\\', "/");
    }
    path.display().to_string()
}

fn print_usage() {
    println!("Usage: cargo run -p k816-golden-tests --bin bless -- [OPTIONS]");
    println!();
    println!("Options:");
    println!("  --errors-only, --err-only   Regenerate only expected.err fixtures");
    println!("  --case <CASE>               Case name (fixture:<name> or example:<name>)");
    println!("                              Bare names default to fixture:<name>");
    println!("  --list                      List discoverable cases and exit");
    println!("  -h, --help                  Show this help");
}
