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
//! [`identity`] is the comparison key. `std::fs::canonicalize` where the file
//! is on disk, which resolves `.`, `..`, symlinks and the working directory
//! in one step; otherwise the lexical normalisation of the same path, which
//! is what an in-memory map and an unsaved editor buffer get. The fallback is
//! not a weaker canonicalize — it cannot see symlinks — and it does not need
//! to be: a map whose files are not on disk has whatever names its builder
//! gave them, and two of those are the same file only if they are spelled the
//! same way after `.` and `..` are folded out.

use std::path::{Component, Path, PathBuf};

/// The file `written` names, read from a source whose own name is `importer`.
pub fn resolve(importer: &str, written: &str) -> PathBuf {
    Path::new(importer)
        .parent()
        .unwrap_or(Path::new(""))
        .join(written)
}

/// The comparison key for `path`: which file it is, rather than what it is
/// called.
pub fn identity(path: &Path) -> PathBuf {
    std::fs::canonicalize(path).unwrap_or_else(|_| normalize(path))
}

/// `path` with `.` dropped and `..` folded into the component before it.
///
/// Purely lexical: `a/../b` is `b` here even where `a` is a symlink pointing
/// elsewhere, which is why this is the fallback rather than the rule.
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
            resolve("sub/dir/unit.k816", "lib.k816"),
            Path::new("sub/dir/lib.k816")
        );
        assert_eq!(
            resolve("sub/dir/unit.k816", "../shared/defs.k65"),
            Path::new("sub/dir/../shared/defs.k65")
        );
        // A source with no directory of its own resolves against the working
        // directory, which is what joining onto an empty prefix does.
        assert_eq!(resolve("unit.k816", "lib.k816"), Path::new("lib.k816"));
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
