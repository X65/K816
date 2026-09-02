/*****************************************************************************/
/*                                                                           */
/*                                 imports.rs                                */
/*                                                                           */
/*          Which file a path names, and which file that path is             */
/*                                                                           */
/*                                                                           */
/* Copyright (c) 2026 Tomasz Sterna (smokku)                                 */
/*                                                                           */
/* Permission to use, copy, modify, and/or distribute this software for any  */
/* purpose with or without fee is hereby granted.                            */
/*                                                                           */
/* THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES  */
/* WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF          */
/* MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR   */
/* ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES    */
/* WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN     */
/* ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF   */
/* OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.            */
/*                                                                           */
/*****************************************************************************/

//! Path arithmetic for `import`, in one place.
//!
//! Two questions, and both have to be answered the same way twice — once by
//! the loader that opens files and once by [`crate::linkage`], which decides
//! which unit an `import` edge points at. If they disagree, a unit is loaded
//! that no edge reaches, or an edge reaches nothing and a program that builds
//! reports a missing import. So neither grows its own rule; both call this.
//!
//! # Which file does the string name
//!
//! The importing file's directory, joined with the path as written. The rule
//! `binary` already uses (`plans/06-compatibility.md` decision 8), and the
//! parser records the answer on the AST node because it is the only stage
//! that knows which file the line came from.
//!
//! # Which file is that
//!
//! A path is a *name* for a file and not the file itself, and one file has
//! many names: `lib.k816`, `./lib.k816`, `../dir/lib.k816` from a
//! subdirectory. Before `import` there was one way to name a unit and no
//! reason to care; now an entry may import a sibling that imports it back,
//! and loading that file twice would hand one program two copies of every
//! definition it holds — reported, absurdly, as `K816-E0102`, a name defined
//! by more than one unit.
//!
//! [`identity`] is the comparison key, and it is **purely lexical**: the path
//! made absolute against the working directory, with `.` dropped and `..`
//! folded into the component before it. It touches the filesystem not at all.
//!
//! That is a deliberate choice against `std::fs::canonicalize`, which is the
//! obvious answer and is wrong here for two reasons. It fails for a file that
//! is not on disk — an in-memory map, an editor buffer that has never been
//! saved — so the key would have to fall back to something else, and a key
//! computed two ways is two keys: a file gets one identity before it exists
//! and a different one after, so saving a new file in a symlinked directory
//! turns one unit into two and earns a duplicate-symbol error about a name
//! declared once. And it resolves symlinks, which is more than is wanted:
//! whether two spellings are the same *file* is a question about the paths a
//! program wrote, and a program that writes both `link/x.k816` and
//! `real/x.k816` has said something the compiler should not quietly undo.
//!
//! The cost is the other side of that last point: two spellings that reach
//! one file through a symlink are two units here, and the duplicate-symbol
//! error says so. That failure is visible and is about something the source
//! actually wrote; the one canonicalize would produce is neither.

use std::path::{Component, Path, PathBuf};

/// The file `written` names, read from a source whose own path is
/// `importer`.
///
/// The importing file's *path*, not its name. An editor names a document by
/// URI, and joining `unit-b.k816` onto `file:///unit-a.k816` as though
/// `file:` were a directory answers a different question from the one the
/// build answers about the same two files.
pub fn resolve(importer: &Path, written: &str) -> PathBuf {
    importer.parent().unwrap_or(Path::new("")).join(written)
}

/// The comparison key for `path`: which file it is, rather than what it is
/// called.
///
/// A pure function of the path and the working directory. See the module
/// documentation for why it does not consult the filesystem.
pub fn identity(path: &Path) -> PathBuf {
    let absolute = std::path::absolute(path).unwrap_or_else(|_| path.to_path_buf());
    normalize(&absolute)
}

/// `path` with `.` dropped and `..` folded into the component before it.
///
/// Purely lexical: `a/../b` is `b` here even where `a` is a symlink pointing
/// somewhere `b` is not.
fn normalize(path: &Path) -> PathBuf {
    let mut kept: Vec<Component<'_>> = Vec::new();
    for component in path.components() {
        match component {
            Component::CurDir => {}
            Component::ParentDir => {
                // A `..` cancels the component before it only when that one
                // names a directory. Above the root, or above a relative
                // path's own leading `..`, there is nothing to cancel and
                // the `..` is kept: dropping it would make `../lib` and
                // `lib` one file, and `../../lib` and `lib` too.
                match kept.last() {
                    Some(Component::Normal(_)) => {
                        kept.pop();
                    }
                    Some(Component::RootDir) => {}
                    _ => kept.push(component),
                }
            }
            other => kept.push(other),
        }
    }
    let mut out = PathBuf::new();
    for component in kept {
        out.push(component);
    }
    if out.as_os_str().is_empty() {
        out.push(".");
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_path_resolves_against_the_importing_files_directory() {
        assert_eq!(
            resolve(Path::new("sub/dir/unit.k816"), "lib.k816"),
            Path::new("sub/dir/lib.k816")
        );
        assert_eq!(
            resolve(Path::new("sub/dir/unit.k816"), "../shared/defs.k65"),
            Path::new("sub/dir/../shared/defs.k65")
        );
        // A source with no directory of its own resolves against the working
        // directory, which is what joining onto an empty prefix does.
        assert_eq!(
            resolve(Path::new("unit.k816"), "lib.k816"),
            Path::new("lib.k816")
        );
    }

    #[test]
    fn normalization_folds_the_spellings_of_one_file_together() {
        for (written, expected) in [
            ("lib.k816", "lib.k816"),
            ("./lib.k816", "lib.k816"),
            ("sub/../lib.k816", "lib.k816"),
            ("sub/dir/../../lib.k816", "lib.k816"),
            ("./sub/./dir/lib.k816", "sub/dir/lib.k816"),
        ] {
            assert_eq!(
                normalize(Path::new(written)),
                Path::new(expected),
                "{written}"
            );
        }
    }

    /// Identity does not change when the file appears.
    ///
    /// The property that makes it usable at all in an editor, where a new
    /// file is imported before it is saved and saved a keystroke later. A
    /// key that consulted the filesystem would answer differently on either
    /// side of that, and one unit would become two.
    #[test]
    fn identity_does_not_depend_on_whether_the_file_exists() {
        // `CARGO_TARGET_TMPDIR` is an integration-test variable, so a unit
        // test builds its own scratch directory under the crate's target.
        let dir =
            std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("target/tmp/identity-stability");
        std::fs::create_dir_all(&dir).expect("create the work dir");
        let file = dir.join("appears.k816");
        let _ = std::fs::remove_file(&file);

        let before = identity(&file);
        std::fs::write(&file, "func f @a8 @i8 {\n    nop\n}\n").expect("create the file");
        assert_eq!(
            identity(&file),
            before,
            "creating the file moved its identity"
        );

        // And a symlinked route to it is a different spelling, which the
        // module documentation says is deliberate: it is a thing the source
        // wrote, not a thing to be quietly undone.
        assert_ne!(
            identity(std::path::Path::new("/tmp/link/appears.k816")),
            before
        );
    }

    #[test]
    fn normalization_keeps_a_parent_it_cannot_cancel() {
        // `../lib.k816` and `lib.k816` are different files, and a fold that
        // dropped the `..` would make one program of two.
        assert_eq!(
            normalize(Path::new("../lib.k816")),
            Path::new("../lib.k816")
        );
        assert_eq!(
            normalize(Path::new("../../lib.k816")),
            Path::new("../../lib.k816")
        );
        assert_eq!(normalize(Path::new("")), Path::new("."));
        // `..` above the root has nowhere to go and is dropped, which is
        // what the filesystem itself does.
        assert_eq!(normalize(Path::new("/../lib.k816")), Path::new("/lib.k816"));
    }
}
