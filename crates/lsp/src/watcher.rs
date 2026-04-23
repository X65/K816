use std::path::{Path, PathBuf};

use anyhow::{Context, Result};
use crossbeam_channel::{Receiver, Sender, unbounded};
use notify::event::{EventKind, ModifyKind, RemoveKind};
use notify::{Error, Event, RecommendedWatcher, RecursiveMode, Watcher};

use super::{PROJECT_SRC_DIR, PROJECT_TESTS_DIR};

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

        if watch_dirs.is_empty() {
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
        if !is_k65_source(&path) {
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

fn is_k65_source(path: &Path) -> bool {
    path.extension()
        .and_then(|ext| ext.to_str())
        .is_some_and(|ext| ext.eq_ignore_ascii_case("k65"))
}
