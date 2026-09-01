/*****************************************************************************/
/*                                                                           */
/*                                 source.rs                                 */
/*                                                                           */
/*         Source files, byte-offset spans, and location translation         */
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

//! Source text management: files, byte-offset spans, and location translation.
//!
//! Every CST, AST, and lowered operation carries a [`Span`] with exact byte
//! offsets into an unmodified source file. Source text is never preprocessed
//! or rewritten before parsing. Synthetic constructs (compiler-inserted nodes
//! and operations) keep a zero-width span at the source position they were
//! derived from and are marked via [`Provenance::Synthetic`].

/// Identifies one source file inside a [`SourceMap`].
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileId(u32);

impl FileId {
    pub fn index(self) -> usize {
        self.0 as usize
    }
}

/// A byte-offset range inside one source file.
///
/// `start` and `end` are byte offsets into the file's UTF-8 text with
/// `start <= end`. A zero-width span (`start == end`) marks a point location,
/// used for EOF diagnostics and synthetic nodes.
///
/// Ordered by file, then start, then end — source order within a file, which
/// is what makes a span usable as a map key for the reference index the
/// editor reads (`plans/07-lsp-tooling.md` decision 11).
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
    pub file: FileId,
    pub start: u32,
    pub end: u32,
}

impl Span {
    pub fn new(file: FileId, start: u32, end: u32) -> Self {
        debug_assert!(start <= end);
        Self { file, start, end }
    }

    /// A zero-width span at `offset`, for point locations and synthetic nodes.
    pub fn empty_at(file: FileId, offset: u32) -> Self {
        Self::new(file, offset, offset)
    }

    pub fn len(self) -> u32 {
        self.end - self.start
    }

    pub fn is_empty(self) -> bool {
        self.start == self.end
    }

    /// Whether `offset` falls inside this span. The file is the caller's to
    /// check: two spans of different files never overlap however their
    /// offsets compare.
    pub fn contains(self, offset: u32) -> bool {
        (self.start..self.end).contains(&offset)
    }

    /// The smallest span covering both `self` and `other` (same file).
    pub fn join(self, other: Span) -> Span {
        debug_assert_eq!(self.file, other.file);
        Span::new(
            self.file,
            self.start.min(other.start),
            self.end.max(other.end),
        )
    }
}

/// Whether a node or operation was written by the user or synthesized by the
/// compiler. Synthetic entities keep an origin [`Span`] pointing at the source
/// they were derived from.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Provenance {
    /// Present verbatim in user source at its span.
    Source,
    /// Inserted by the compiler; the span is the (possibly zero-width) origin.
    Synthetic,
}

/// A zero-based line/column location. `column` counts bytes within the line.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct LineCol {
    pub line: u32,
    pub column: u32,
}

/// One source file: its name, unmodified text, and a line-start index.
#[derive(Debug)]
pub struct SourceFile {
    id: FileId,
    name: String,
    text: String,
    /// Byte offset of the first byte of every line. `line_starts[0] == 0`.
    line_starts: Vec<u32>,
}

impl SourceFile {
    fn new(id: FileId, name: String, text: String) -> Self {
        assert!(
            u32::try_from(text.len()).is_ok(),
            "source file larger than 4 GiB: {name}"
        );
        let mut line_starts = vec![0];
        for (offset, byte) in text.bytes().enumerate() {
            if byte == b'\n' {
                line_starts.push(offset as u32 + 1);
            }
        }
        Self {
            id,
            name,
            text,
            line_starts,
        }
    }

    pub fn id(&self) -> FileId {
        self.id
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn text(&self) -> &str {
        &self.text
    }

    /// A span covering the entire file.
    pub fn span(&self) -> Span {
        Span::new(self.id, 0, self.text.len() as u32)
    }

    /// A zero-width span at end of file.
    pub fn end_span(&self) -> Span {
        Span::empty_at(self.id, self.text.len() as u32)
    }

    /// How many lines the file has. A file ending in a newline has an empty
    /// final line, which is where an editor's cursor legitimately sits.
    pub fn line_count(&self) -> u32 {
        self.line_starts.len() as u32
    }

    /// Translates a byte offset into a zero-based line/column pair.
    ///
    /// Offsets past the end of the file clamp to the final position. The
    /// column counts bytes from the line start, so multi-byte UTF-8 characters
    /// advance it by their encoded length.
    pub fn line_col(&self, offset: u32) -> LineCol {
        let offset = offset.min(self.text.len() as u32);
        let line = self
            .line_starts
            .partition_point(|&start| start <= offset)
            .saturating_sub(1);
        LineCol {
            line: line as u32,
            column: offset - self.line_starts[line],
        }
    }

    /// Translates a zero-based line/column pair back into a byte offset.
    ///
    /// The inverse of [`Self::line_col`], and clamped at both ends: a line
    /// past the last one gives the end of the file, and a column past the end
    /// of its line gives that line's end. An editor's cursor legitimately
    /// sits one position past the last character, and a client that has seen
    /// a newer version of a document than the server holds sends positions
    /// that do not exist here — neither is an error, and neither may panic
    /// (`plans/07-lsp-tooling.md` decision 18).
    ///
    /// A column that falls inside a multi-byte character rounds *down* to
    /// that character's first byte, so the result is always a character
    /// boundary and `&text[..offset]` never panics.
    pub fn offset(&self, location: LineCol) -> u32 {
        let Some(&start) = self.line_starts.get(location.line as usize) else {
            return self.text.len() as u32;
        };
        let line = self.line_text(location.line);
        let column = (location.column as usize).min(line.len());
        // `floor_char_boundary` is unstable; walking back off a continuation
        // byte is the same answer.
        let mut column = column;
        while column > 0 && !line.is_char_boundary(column) {
            column -= 1;
        }
        start + column as u32
    }

    /// The text of a zero-based line, without its trailing newline.
    pub fn line_text(&self, line: u32) -> &str {
        let line = line as usize;
        let start = self.line_starts.get(line).copied().unwrap_or(0) as usize;
        let end = self
            .line_starts
            .get(line + 1)
            .map(|&next| next as usize)
            .unwrap_or(self.text.len());
        self.text[start..end].trim_end_matches(['\n', '\r'])
    }

    /// The source text covered by `span`, which must belong to this file.
    pub fn span_text(&self, span: Span) -> &str {
        debug_assert_eq!(span.file, self.id);
        &self.text[span.start as usize..span.end as usize]
    }
}

/// Owns all source files of a compilation and translates spans to locations.
#[derive(Debug, Default)]
pub struct SourceMap {
    files: Vec<SourceFile>,
}

impl SourceMap {
    pub fn new() -> Self {
        Self::default()
    }

    /// Registers source text under `name` and returns its new [`FileId`].
    pub fn add_file(&mut self, name: impl Into<String>, text: impl Into<String>) -> FileId {
        let id = FileId(u32::try_from(self.files.len()).expect("too many source files"));
        self.files
            .push(SourceFile::new(id, name.into(), text.into()));
        id
    }

    /// Replaces the text of an already registered file, keeping its
    /// [`FileId`].
    ///
    /// For a live document store, whose file identities have to survive an
    /// edit: every span taken from the previous text is stale afterwards,
    /// which is what re-analysis is for. The name is unchanged, because it is
    /// what identifies the document to the editor.
    pub fn replace_file(&mut self, id: FileId, text: impl Into<String>) {
        let name = std::mem::take(&mut self.files[id.index()].name);
        self.files[id.index()] = SourceFile::new(id, name, text.into());
    }

    pub fn file(&self, id: FileId) -> &SourceFile {
        &self.files[id.index()]
    }

    pub fn files(&self) -> impl Iterator<Item = &SourceFile> {
        self.files.iter()
    }

    pub fn line_col(&self, file: FileId, offset: u32) -> LineCol {
        self.file(file).line_col(offset)
    }

    pub fn span_text(&self, span: Span) -> &str {
        self.file(span.file).span_text(span)
    }
}
