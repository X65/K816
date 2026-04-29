use std::path::{Path, PathBuf};
use std::str::FromStr;

use anyhow::Result;
use lsp_types::Uri;

use super::{PROJECT_SRC_DIR, PROJECT_TESTS_DIR};

pub(super) fn discover_workspace_sources(root: &Path) -> Result<Vec<PathBuf>> {
    let manifest_path = root.join(k816_project::PROJECT_MANIFEST);
    if manifest_path.is_file() {
        k816_project::load_project_manifest(root)?;
    }
    k816_project::discover_sources_in_folders(root, &[PROJECT_SRC_DIR, PROJECT_TESTS_DIR])
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
