use std::path::{Path, PathBuf};

use anyhow::{Context, Result};
use crossbeam_channel::{Receiver, Sender, unbounded};
use notify::event::{EventKind, ModifyKind, RemoveKind};
use notify::{Error, Event, RecommendedWatcher, RecursiveMode, Watcher};

use super::linker_config::is_linker_config_filename;
use super::{PROJECT_MANIFEST, PROJECT_SRC_DIR, PROJECT_TESTS_DIR};

#[derive(Debug, Clone)]
pub(super) enum WorkspaceFsEvent {
    Changed(PathBuf),
    Removed(PathBuf),
}

pub(super) struct FileWatcher {
    _watcher: RecommendedWatcher,
    receiver: Receiver<WorkspaceFsEvent>,
}

impl FileWatcher {
    pub(super) fn start(root: &Path) -> Result<Option<Self>> {
        let src_dir = root.join(PROJECT_SRC_DIR);
        let tests_dir = root.join(PROJECT_TESTS_DIR);
        let watch_dirs: Vec<PathBuf> = [src_dir, tests_dir]
            .into_iter()
            .filter(|path| path.is_dir())
            .collect();

        // The source watches still target src/ + tests/ to keep the event
        // volume sane for project-mode workspaces. The workspace root is
        // additionally watched non-recursively so manifest / workspace-level
        // config files (k816.toml, link.ron) are picked up. Per-directory
        // link.ld.ron files inside src/ or tests/ already flow through the
        // recursive watches above; outside those, ad-hoc fixture trees are
        // watched on demand by the recursive root watch added below.
        if watch_dirs.is_empty() && !root.is_dir() {
            return Ok(None);
        }

        let (tx, rx) = unbounded::<WorkspaceFsEvent>();
        let mut watcher = notify::recommended_watcher(move |res: Result<Event, Error>| {
            if let Ok(event) = res {
                dispatch_event(&tx, event);
            }
        })
        .context("failed to create filesystem watcher")?;

        for dir in &watch_dirs {
            watcher
                .watch(dir, RecursiveMode::Recursive)
                .with_context(|| format!("failed to watch '{}'", dir.display()))?;
        }

        // Watch the workspace root recursively too so per-fixture
        // link.ld.ron files (and ad-hoc sources outside src/ + tests/) are
        // visible. The dispatch filter below ensures only relevant paths
        // turn into events. Best-effort: failures here downgrade to "config
        // changes won't be hot-reloaded" without aborting startup.
        if root.is_dir()
            && let Err(error) = watcher.watch(root, RecursiveMode::Recursive)
        {
            log::warn!(
                "failed to watch workspace root '{}' for config-file changes: {error}",
                root.display(),
            );
        }

        Ok(Some(Self {
            _watcher: watcher,
            receiver: rx,
        }))
    }

    pub(super) fn receiver(&self) -> &Receiver<WorkspaceFsEvent> {
        &self.receiver
    }
}

fn dispatch_event(sender: &Sender<WorkspaceFsEvent>, event: Event) {
    let (is_removal, is_relevant) = classify(&event.kind);
    if !is_relevant {
        return;
    }
    for path in event.paths {
        if !is_workspace_relevant_path(&path) {
            continue;
        }
        let msg = if is_removal {
            WorkspaceFsEvent::Removed(path)
        } else {
            WorkspaceFsEvent::Changed(path)
        };
        let _ = sender.send(msg);
    }
}

fn is_workspace_relevant_path(path: &Path) -> bool {
    if k816_project::is_k65_source_path(path) {
        return true;
    }
    let Some(name) = path.file_name().and_then(|n| n.to_str()) else {
        return false;
    };
    is_linker_config_filename(name) || name == PROJECT_MANIFEST
}

fn classify(kind: &EventKind) -> (bool, bool) {
    match kind {
        EventKind::Create(_) => (false, true),
        EventKind::Modify(ModifyKind::Data(_) | ModifyKind::Any | ModifyKind::Name(_)) => {
            (false, true)
        }
        EventKind::Remove(RemoveKind::File | RemoveKind::Any) => (true, true),
        _ => (false, false),
    }
}
