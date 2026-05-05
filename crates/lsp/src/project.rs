use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::str::FromStr;

use anyhow::Result;
use lsp_types::Uri;

use super::linker_config::{LinkerConfigKey, key_for_source};
use super::{PROJECT_SRC_DIR, PROJECT_TESTS_DIR};

/// A group of source URIs the LSP should compile and link together. Mirrors
/// what the actual compiler/linker would consider one program; partitioning
/// keeps unrelated fixtures from being glued into a single ill-formed pseudo-
/// program (which silently dropped cross-file consts as ambiguous).
#[derive(Debug, Clone)]
pub(super) struct CompilationUnit {
    pub(super) uris: Vec<Uri>,
    /// Which linker config this unit links against. Manifest-mode workspaces
    /// always use `LinkerConfigKey::Workspace`; manifest-less workspaces
    /// resolve per source via [`key_for_source`].
    pub(super) config_key: LinkerConfigKey,
}

pub(super) fn discover_workspace_sources(root: &Path) -> Result<Vec<PathBuf>> {
    let manifest_path = root.join(k816_project::PROJECT_MANIFEST);
    if manifest_path.is_file() {
        k816_project::load_project_manifest(root)?;
        // With a manifest the project is a single program rooted at `src/`;
        // `tests/` holds golden fixtures that are independent programs and
        // must not bleed into the canonical unit.
        return k816_project::discover_sources_in_folders(root, &[PROJECT_SRC_DIR]);
    }
    k816_project::discover_sources_in_folders(root, &[PROJECT_SRC_DIR, PROJECT_TESTS_DIR])
}

/// Partition the discovered source files into compilation units the LSP can
/// compile + link together as a coherent program. Manifest-first, directory
/// bucket fallback. Pure and deterministic so it's straightforward to test.
///
/// Manifest workspaces always produce a single unit bound to the workspace
/// linker config — per-subtree `link.ld.ron` files are deliberately ignored
/// inside a manifest project (a project asserts "this is one program"). In
/// manifest-less workspaces, files are bucketed by `(nearest config, parent
/// directory)` so a fixture tree carrying its own `link.ld.ron` (e.g.
/// `tests/golden/fixtures/link-dp/`) links under the right config and a
/// stray `link.ld.ron` doesn't merge unrelated dirs.
pub(super) fn partition_workspace_into_units(
    root: &Path,
    has_manifest: bool,
    discovered: &[PathBuf],
) -> Vec<CompilationUnit> {
    if discovered.is_empty() {
        return Vec::new();
    }

    if has_manifest {
        let mut uris: Vec<Uri> = Vec::with_capacity(discovered.len());
        for path in discovered {
            if let Ok(uri) = uri_from_file_path(path) {
                uris.push(uri);
            }
        }
        if uris.is_empty() {
            return Vec::new();
        }
        return vec![CompilationUnit {
            uris,
            config_key: LinkerConfigKey::Workspace,
        }];
    }

    // BTreeMap keyed by (config_key, parent dir) keeps unit ordering
    // deterministic. Two adjacent dirs sharing the same nearest config still
    // form separate units (current behaviour) — only `.ld.ron`-bearing dirs
    // introduce new boundaries, never merge old ones.
    let mut by_bucket: BTreeMap<(LinkerConfigKey, PathBuf), Vec<Uri>> = BTreeMap::new();
    let mut adhoc_units: Vec<CompilationUnit> = Vec::new();

    for path in discovered {
        let Ok(uri) = uri_from_file_path(path) else {
            continue;
        };
        let key = key_for_source(root, path);
        let parent = path.parent().map(Path::to_path_buf);
        match parent {
            Some(parent) if parent == root => {
                // Files directly at the workspace root form their own
                // singleton units — no obvious sibling to group with.
                adhoc_units.push(CompilationUnit {
                    uris: vec![uri],
                    config_key: key,
                });
            }
            Some(parent) => {
                by_bucket.entry((key, parent)).or_default().push(uri);
            }
            None => {
                adhoc_units.push(CompilationUnit {
                    uris: vec![uri],
                    config_key: key,
                });
            }
        }
    }

    let mut units: Vec<CompilationUnit> = by_bucket
        .into_iter()
        .map(|((config_key, _parent), uris)| CompilationUnit { uris, config_key })
        .collect();
    units.extend(adhoc_units);
    units
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
