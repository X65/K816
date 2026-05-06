use std::collections::HashMap;
use std::path::{Path, PathBuf};

use k816_link::{LinkerConfig, default_stub_config};

/// Filenames the per-source resolver looks for, in priority order.
const NEAREST_CONFIG_FILES: &[&str] = &["link.ld.ron", "link.ron"];

/// Identifies which linker config a compilation unit should use.
///
/// `Workspace` is the fallback resolved by [`super::workspace`]'s
/// `load_workspace_linker_config` (manifest script → `<root>/link.ron` →
/// stub). `Path` points at a `link.ld.ron` / `link.ron` discovered by walking
/// up from a source file. The `Workspace` variant orders below `Path` so
/// `BTreeMap` ordering is deterministic and locality-grouped.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub(super) enum LinkerConfigKey {
    Path(PathBuf),
    Workspace,
}

#[derive(Debug, Default)]
pub(super) struct LinkerConfigCache {
    by_path: HashMap<PathBuf, LinkerConfig>,
    workspace: Option<LinkerConfig>,
}

impl LinkerConfigCache {
    pub(super) fn new() -> Self {
        Self::default()
    }

    /// Set the workspace-fallback config (manifest script / `<root>/link.ron`
    /// / stub). Called by `load_workspace_linker_config` at startup and
    /// whenever the manifest changes on disk.
    pub(super) fn set_workspace(&mut self, config: LinkerConfig) {
        self.workspace = Some(config);
    }

    /// Lazy load + cache a path-keyed config. Returns the workspace fallback
    /// (or a stub if the workspace slot is also empty) when loading fails so
    /// linking always has a config to work with.
    pub(super) fn resolve(&mut self, key: &LinkerConfigKey) -> LinkerConfig {
        match key {
            LinkerConfigKey::Workspace => {
                self.workspace.clone().unwrap_or_else(default_stub_config)
            }
            LinkerConfigKey::Path(path) => {
                if !self.by_path.contains_key(path) {
                    match k816_link::load_config(path) {
                        Ok(cfg) => {
                            self.by_path.insert(path.clone(), cfg);
                        }
                        Err(error) => {
                            log::warn!(
                                "failed to load linker config '{}': {error}",
                                path.display()
                            );
                        }
                    }
                }
                self.by_path
                    .get(path)
                    .cloned()
                    .unwrap_or_else(|| self.workspace.clone().unwrap_or_else(default_stub_config))
            }
        }
    }

    /// Read-only lookup. Returns `None` for path keys that haven't been
    /// loaded yet — used by `query_memory_map` to find the config that
    /// produced the captured layout without mutating cache state.
    pub(super) fn get(&self, key: &LinkerConfigKey) -> Option<&LinkerConfig> {
        match key {
            LinkerConfigKey::Workspace => self.workspace.as_ref(),
            LinkerConfigKey::Path(path) => self.by_path.get(path),
        }
    }

    pub(super) fn invalidate(&mut self, key: &LinkerConfigKey) {
        match key {
            LinkerConfigKey::Workspace => {
                self.workspace = None;
            }
            LinkerConfigKey::Path(path) => {
                self.by_path.remove(path);
            }
        }
    }
}

/// Walk up from `source_path.parent()` to (and including) `root`, looking
/// for `link.ld.ron` (preferred) then `link.ron` at each level. Returns the
/// first match. Stops at `root` — never escapes the workspace, even if a
/// matching file lives further up the filesystem.
pub(super) fn discover_linker_config_for(root: &Path, source_path: &Path) -> Option<PathBuf> {
    let canonical_root = canonical_or_logical(root);

    let start = source_path.parent()?.to_path_buf();
    if !canonical_or_logical(&start).starts_with(&canonical_root) {
        return None;
    }

    let mut current = start;
    loop {
        for filename in NEAREST_CONFIG_FILES {
            let candidate = current.join(filename);
            if candidate.is_file() {
                return Some(candidate);
            }
        }
        if canonical_or_logical(&current) == canonical_root {
            return None;
        }
        let parent = current.parent().map(Path::to_path_buf)?;
        if !canonical_or_logical(&parent).starts_with(&canonical_root) {
            return None;
        }
        current = parent;
    }
}

pub(super) fn key_for_source(root: &Path, source_path: &Path) -> LinkerConfigKey {
    match discover_linker_config_for(root, source_path) {
        Some(path) => LinkerConfigKey::Path(canonical_or_logical(&path)),
        None => LinkerConfigKey::Workspace,
    }
}

/// Returns true if `name` is a recognised linker config filename. Used by
/// the watcher and event dispatcher to decide whether a non-`.k65` event is
/// worth surfacing.
pub(super) fn is_linker_config_filename(name: &str) -> bool {
    NEAREST_CONFIG_FILES.contains(&name)
}

fn canonical_or_logical(path: &Path) -> PathBuf {
    std::fs::canonicalize(path).unwrap_or_else(|_| path.to_path_buf())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::tempdir;

    #[test]
    fn discover_walks_up_to_nearest_link_ld_ron() {
        let root = tempdir().unwrap();
        let foo = root.path().join("foo");
        let bar = foo.join("bar");
        fs::create_dir_all(&bar).unwrap();
        let cfg = foo.join("link.ld.ron");
        fs::write(&cfg, "()").unwrap();
        let source = bar.join("baz.k65");
        fs::write(&source, "").unwrap();

        let found = discover_linker_config_for(root.path(), &source);
        assert_eq!(
            found.map(|p| canonical_or_logical(&p)),
            Some(canonical_or_logical(&cfg))
        );
    }

    #[test]
    fn discover_prefers_link_ld_ron_over_link_ron_at_same_level() {
        let root = tempdir().unwrap();
        let dir = root.path().join("here");
        fs::create_dir_all(&dir).unwrap();
        let preferred = dir.join("link.ld.ron");
        let other = dir.join("link.ron");
        fs::write(&preferred, "()").unwrap();
        fs::write(&other, "()").unwrap();
        let source = dir.join("input.k65");
        fs::write(&source, "").unwrap();

        let found = discover_linker_config_for(root.path(), &source);
        assert_eq!(
            found.map(|p| canonical_or_logical(&p)),
            Some(canonical_or_logical(&preferred))
        );
    }

    #[test]
    fn discover_falls_back_to_link_ron_when_no_ld_ron() {
        let root = tempdir().unwrap();
        let dir = root.path().join("here");
        fs::create_dir_all(&dir).unwrap();
        let plain = dir.join("link.ron");
        fs::write(&plain, "()").unwrap();
        let source = dir.join("input.k65");
        fs::write(&source, "").unwrap();

        let found = discover_linker_config_for(root.path(), &source);
        assert_eq!(
            found.map(|p| canonical_or_logical(&p)),
            Some(canonical_or_logical(&plain))
        );
    }

    #[test]
    fn discover_stops_at_root() {
        // Place a config above the workspace root: it must not be picked.
        let outer = tempdir().unwrap();
        let outside_cfg = outer.path().join("link.ld.ron");
        fs::write(&outside_cfg, "()").unwrap();
        let root = outer.path().join("workspace");
        let dir = root.join("dir");
        fs::create_dir_all(&dir).unwrap();
        let source = dir.join("input.k65");
        fs::write(&source, "").unwrap();

        let found = discover_linker_config_for(&root, &source);
        assert!(
            found.is_none(),
            "found unexpected outside config: {found:?}"
        );
    }

    #[test]
    fn discover_returns_none_when_walk_exhausts() {
        let root = tempdir().unwrap();
        let dir = root.path().join("dir");
        fs::create_dir_all(&dir).unwrap();
        let source = dir.join("input.k65");
        fs::write(&source, "").unwrap();
        assert!(discover_linker_config_for(root.path(), &source).is_none());
    }

    #[test]
    fn key_for_source_falls_back_to_workspace() {
        let root = tempdir().unwrap();
        let dir = root.path().join("dir");
        fs::create_dir_all(&dir).unwrap();
        let source = dir.join("input.k65");
        fs::write(&source, "").unwrap();
        assert_eq!(
            key_for_source(root.path(), &source),
            LinkerConfigKey::Workspace
        );
    }

    #[test]
    fn cache_invalidate_clears_path_entry() {
        let root = tempdir().unwrap();
        let cfg_path = root.path().join("link.ld.ron");
        fs::write(
            &cfg_path,
            "(format:\"o65-link\", memory:[(name:\"MAIN\", start:0, size:65536, kind:ReadWrite)])",
        )
        .unwrap();

        let mut cache = LinkerConfigCache::new();
        let key = LinkerConfigKey::Path(canonical_or_logical(&cfg_path));
        let _ = cache.resolve(&key);
        assert!(cache.get(&key).is_some());
        cache.invalidate(&key);
        assert!(cache.get(&key).is_none());
    }
}
