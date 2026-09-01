/*****************************************************************************/
/*                                                                           */
/*                                 convert.rs                                */
/*                                                                           */
/*        Positions, ranges and URIs, in both directions, in one place       */
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

//! Position, range and URI conversion — both directions, in one module.
//!
//! K816 counts **bytes**; the protocol counts **UTF-16 code units** on a
//! zero-based line. The conversion is where an incremental server goes wrong
//! silently: a byte-offset mistake on a line holding `ż` corrupts the
//! document without producing a diagnostic, because the server simply ends up
//! holding different text than the editor does. So both directions live here,
//! composed out of [`SourceFile::line_col`] and [`SourceFile::offset`] rather
//! than out of a third piece of arithmetic, and both are tested on one-, two-
//! and four-byte characters (`plans/07-lsp-tooling.md` decision 13).
//!
//! Nothing here reports an error. A position past the end of a line, a line
//! past the end of a file, and a position inside a character all have a
//! defined answer, because a client that has seen a newer version of a
//! document than the server holds sends every one of them and a panic in a
//! request handler kills the editor's session (decision 18).

use lsp_types::{Location, Position, Range, Uri};

use crate::source::{FileId, LineCol, SourceFile, SourceMap, Span};

/// Converts a byte [`Span`] into an LSP range (UTF-16 columns).
pub fn span_to_range(map: &SourceMap, span: Span) -> Range {
    Range::new(
        position(map, span.file, span.start),
        position(map, span.file, span.end),
    )
}

/// Converts a byte offset in `file` into an LSP position.
pub fn position(map: &SourceMap, file: FileId, offset: u32) -> Position {
    position_in(map.file(file), offset)
}

/// Converts a byte offset into an LSP position within one file.
pub fn position_in(source: &SourceFile, offset: u32) -> Position {
    let location = source.line_col(offset);
    let line_text = source.line_text(location.line);
    // Rounded down to a character boundary for the same reason `offset_in`
    // rounds: an offset that splits a character has to have *an* answer, and
    // slicing on a continuation byte would panic instead of giving one.
    let mut byte_column = (location.column as usize).min(line_text.len());
    while byte_column > 0 && !line_text.is_char_boundary(byte_column) {
        byte_column -= 1;
    }
    let utf16_column: usize = line_text[..byte_column].chars().map(char::len_utf16).sum();
    Position::new(location.line, utf16_column as u32)
}

/// Converts an LSP position into a byte offset in `source`.
///
/// The inverse of [`position_in`], clamped the way the protocol's own
/// tolerances require: a line past the last one is the end of the file, a
/// column past the end of its line is that line's end, and a column that
/// falls *inside* a character — the second UTF-16 unit of a four-byte one —
/// rounds down to that character's first byte, so the result is always a
/// character boundary.
pub fn offset_in(source: &SourceFile, position: Position) -> u32 {
    if position.line >= source.line_count() {
        return source.text().len() as u32;
    }
    let line = source.line_text(position.line);
    let mut utf16 = 0u32;
    for (index, character) in line.char_indices() {
        let next = utf16 + character.len_utf16() as u32;
        if position.character < next {
            return source.offset(LineCol {
                line: position.line,
                column: index as u32,
            });
        }
        utf16 = next;
    }
    source.offset(LineCol {
        line: position.line,
        column: line.len() as u32,
    })
}

/// Converts an LSP range into a byte [`Span`] in `source`.
///
/// A reversed range — which the protocol does not forbid a client from
/// sending — is read as the range it covers rather than refused, because
/// there is no way to report a refusal from inside a `didChange`.
pub fn range_to_span(source: &SourceFile, range: Range) -> Span {
    let start = offset_in(source, range.start);
    let end = offset_in(source, range.end);
    Span::new(source.id(), start.min(end), start.max(end))
}

/// The URI of a file, which is the name the document was opened under.
///
/// `None` for a name that is not a URI at all — a workspace built from paths
/// by a test, for instance. A file the client cannot be told about is one it
/// never told the server about.
pub fn uri(map: &SourceMap, file: FileId) -> Option<Uri> {
    map.file(file).name().parse().ok()
}

/// A protocol location for a span: its file's URI and its UTF-16 range.
pub fn location(map: &SourceMap, span: Span) -> Option<Location> {
    Some(Location::new(
        uri(map, span.file)?,
        span_to_range(map, span),
    ))
}
