use std::path::{Component, Path, PathBuf};

use anyhow::Context;
use serde::Deserialize;

pub const PROJECT_MANIFEST: &str = "k816.toml";
pub const PROJECT_DEFAULT_LINK_SCRIPT: &str = "link.ron";
pub const PROJECT_SRC_DIR: &str = "src";
pub const PROJECT_TESTS_DIR: &str = "tests";

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ProjectManifest {
    pub package: ProjectPackage,
    #[serde(default)]
    pub link: ProjectLink,
    #[serde(default)]
    pub run: ProjectRun,
}

impl ProjectManifest {
    pub fn validate(&self) -> anyhow::Result<()> {
        validate_package_name(&self.package.name)?;
        if self.package.version.trim().is_empty() {
            anyhow::bail!("package.version must not be empty");
        }
        Ok(())
    }
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ProjectPackage {
    pub name: String,
    #[serde(default = "default_manifest_version")]
    pub version: String,
}

fn default_manifest_version() -> String {
    "0.1.0".to_string()
}

#[derive(Debug, Default, Deserialize)]
#[serde(default, deny_unknown_fields)]
pub struct ProjectLink {
    pub script: Option<PathBuf>,
    #[serde(default)]
    pub listing: ListingOption,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(untagged)]
pub enum ListingOption {
    Path(String),
    Bool(bool),
}

impl Default for ListingOption {
    fn default() -> Self {
        Self::Bool(false)
    }
}

#[derive(Debug, Clone, Default, Deserialize)]
#[serde(default, deny_unknown_fields)]
pub struct ProjectRun {
    pub runner: Option<String>,
    pub args: Vec<String>,
}

pub fn validate_package_name(name: &str) -> anyhow::Result<()> {
    if name.trim().is_empty() {
        anyhow::bail!("package.name is required in k816.toml");
    }

    let mut components = Path::new(name).components();
    match (components.next(), components.next()) {
        (Some(Component::Normal(_)), None) => Ok(()),
        _ => anyhow::bail!("package.name '{}' must be a single path component", name),
    }
}

pub fn load_project_manifest(project_root: &Path) -> anyhow::Result<ProjectManifest> {
    let manifest_path = project_root.join(PROJECT_MANIFEST);
    let text = std::fs::read_to_string(&manifest_path)
        .with_context(|| format!("failed to read '{}'", manifest_path.display()))?;
    let manifest: ProjectManifest = toml::from_str(&text)
        .with_context(|| format!("failed to parse '{}'", manifest_path.display()))?;
    manifest.validate()?;
    Ok(manifest)
}

pub fn parse_project_manifest(text: &str) -> anyhow::Result<ProjectManifest> {
    let manifest: ProjectManifest = toml::from_str(text)?;
    manifest.validate()?;
    Ok(manifest)
}

pub fn resolve_project_link_config_path(
    project_root: &Path,
    manifest: &ProjectManifest,
    override_path: Option<&Path>,
) -> Option<PathBuf> {
    if let Some(path) = override_path {
        return Some(path.to_path_buf());
    }

    if let Some(script) = manifest.link.script.as_deref() {
        if script.is_absolute() {
            return Some(script.to_path_buf());
        }
        return Some(project_root.join(script));
    }

    let script = project_root.join(PROJECT_DEFAULT_LINK_SCRIPT);
    if script.exists() {
        return Some(script);
    }

    None
}

pub fn is_k65_source_path(path: &Path) -> bool {
    path.extension()
        .and_then(|ext| ext.to_str())
        .is_some_and(|ext| ext.eq_ignore_ascii_case("k65"))
}

pub fn discover_sources(source_root: &Path) -> anyhow::Result<Vec<PathBuf>> {
    let mut out = Vec::new();
    discover_sources_rec(source_root, &mut out)?;
    Ok(out)
}

pub fn discover_sources_in_folders(
    project_root: &Path,
    folders: &[&str],
) -> anyhow::Result<Vec<PathBuf>> {
    let mut files = Vec::new();
    for folder in folders {
        let dir = project_root.join(folder);
        if !dir.is_dir() {
            continue;
        }
        files.extend(discover_sources(&dir)?);
    }
    files.sort();
    files.dedup();
    Ok(files)
}

fn discover_sources_rec(dir: &Path, out: &mut Vec<PathBuf>) -> anyhow::Result<()> {
    let mut entries = std::fs::read_dir(dir)
        .with_context(|| format!("failed to read source directory '{}'", dir.display()))?
        .collect::<Result<Vec<_>, _>>()
        .with_context(|| format!("failed to read source directory '{}'", dir.display()))?;
    entries.sort_by_key(|entry| entry.path());

    for entry in entries {
        let path = entry.path();
        let file_type = entry
            .file_type()
            .with_context(|| format!("failed to inspect '{}'", path.display()))?;
        if file_type.is_dir() {
            discover_sources_rec(&path, out)?;
            continue;
        }
        if file_type.is_file() && is_k65_source_path(&path) {
            out.push(path);
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn validates_manifest_package_name() {
        let err = parse_project_manifest("[package]\nname = \"../bad\"\n")
            .expect_err("invalid package name");
        assert!(
            err.to_string().contains("single path component"),
            "unexpected error: {err:#}"
        );
    }

    #[test]
    fn detects_k65_sources_case_insensitively() {
        assert!(is_k65_source_path(Path::new("src/main.k65")));
        assert!(is_k65_source_path(Path::new("src/main.K65")));
        assert!(!is_k65_source_path(Path::new("src/main.asm")));
    }

    #[test]
    fn discovers_sources_in_sorted_order() {
        let root = tempfile::tempdir().expect("tempdir");
        std::fs::create_dir_all(root.path().join("src/nested")).expect("mkdir");
        std::fs::write(root.path().join("src/z.k65"), "").expect("write");
        std::fs::write(root.path().join("src/a.k65"), "").expect("write");
        std::fs::write(root.path().join("src/nested/b.k65"), "").expect("write");
        std::fs::write(root.path().join("src/ignore.txt"), "").expect("write");

        let files = discover_sources(&root.path().join("src")).expect("discover");
        let names = files
            .iter()
            .map(|path| {
                path.strip_prefix(root.path())
                    .unwrap()
                    .display()
                    .to_string()
            })
            .collect::<Vec<_>>();
        assert_eq!(names, vec!["src/a.k65", "src/nested/b.k65", "src/z.k65"]);
    }

    #[test]
    fn resolves_link_script_precedence() {
        let root = tempfile::tempdir().expect("tempdir");
        std::fs::write(root.path().join(PROJECT_DEFAULT_LINK_SCRIPT), "").expect("write");
        let manifest = parse_project_manifest(
            "[package]\nname = \"demo\"\n\n[link]\nscript = \"custom.ron\"\n",
        )
        .expect("manifest");

        let override_path = Path::new("override.ron");
        assert_eq!(
            resolve_project_link_config_path(root.path(), &manifest, Some(override_path)),
            Some(override_path.to_path_buf())
        );
        assert_eq!(
            resolve_project_link_config_path(root.path(), &manifest, None),
            Some(root.path().join("custom.ron"))
        );

        let manifest = parse_project_manifest("[package]\nname = \"demo\"\n").expect("manifest");
        assert_eq!(
            resolve_project_link_config_path(root.path(), &manifest, None),
            Some(root.path().join(PROJECT_DEFAULT_LINK_SCRIPT))
        );
    }
}
