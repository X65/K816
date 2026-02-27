use std::fs;
use std::path::{Path, PathBuf};
use std::str::FromStr;

use anyhow::{Context, Result};
use lsp_types::Uri;
use serde::Deserialize;
use walkdir::WalkDir;

use super::{PROJECT_MANIFEST, PROJECT_SRC_DIR, PROJECT_TESTS_DIR};

#[derive(Debug, Deserialize)]
pub(super) struct ProjectManifest {
    pub package: ProjectPackage,
    #[serde(default)]
    pub link: ProjectLink,
}

#[derive(Debug, Default, Deserialize)]
#[serde(default)]
pub(super) struct ProjectLink {
    pub script: Option<PathBuf>,
}

#[derive(Debug, Deserialize)]
pub(super) struct ProjectPackage {
    pub name: String,
}

pub(super) fn discover_workspace_sources(root: &Path) -> Result<Vec<PathBuf>> {
    let manifest_path = root.join(PROJECT_MANIFEST);
    if manifest_path.is_file() {
        let text = fs::read_to_string(&manifest_path)
            .with_context(|| format!("failed to read '{}'", manifest_path.display()))?;
        let manifest: ProjectManifest = toml::from_str(&text)
            .with_context(|| format!("failed to parse '{}'", manifest_path.display()))?;
        if manifest.package.name.trim().is_empty() {
            anyhow::bail!(
                "package.name in '{}' must not be empty",
                manifest_path.display()
            );
        }
    }

    let mut files = Vec::new();
    for folder in [PROJECT_SRC_DIR, PROJECT_TESTS_DIR] {
        let dir = root.join(folder);
        if !dir.is_dir() {
            continue;
        }
        for entry in WalkDir::new(&dir).follow_links(false) {
            let entry = match entry {
                Ok(entry) => entry,
                Err(_) => continue,
            };
            if !entry.file_type().is_file() {
                continue;
            }
            if is_k65_source_path(entry.path()) {
                files.push(entry.path().to_path_buf());
            }
        }
    }

    files.sort();
    files.dedup();
    Ok(files)
}

fn is_k65_source_path(path: &Path) -> bool {
    path.extension()
        .and_then(|ext| ext.to_str())
        .is_some_and(|ext| ext.eq_ignore_ascii_case("k65"))
}

pub(super) fn uri_to_file_path(uri: &Uri) -> Option<PathBuf> {
    let parsed = url::Url::parse(uri.as_str()).ok()?;
    parsed.to_file_path().ok()
}

pub(super) fn uri_from_file_path(path: &Path) -> Result<Uri> {
    let url = url::Url::from_file_path(path)
        .map_err(|()| anyhow::anyhow!("path '{}' cannot be represented as URI", path.display()))?;
    Uri::from_str(url.as_str()).map_err(|error| anyhow::anyhow!("invalid URI '{}': {error}", url))
}
