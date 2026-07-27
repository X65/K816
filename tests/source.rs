//! Exact span and location tests for `source.rs` (master plan Phase 1).

use k816::source::{FileId, LineCol, SourceMap, Span};

fn line_col(line: u32, column: u32) -> LineCol {
    LineCol { line, column }
}

#[test]
fn ascii_offsets_translate_to_exact_lines_and_columns() {
    let mut map = SourceMap::new();
    let id = map.add_file("main.k816", "var SCREEN = $0400\n\nfunc main {\n}\n");
    let file = map.file(id);

    assert_eq!(file.line_col(0), line_col(0, 0));
    assert_eq!(file.line_col(4), line_col(0, 4)); // start of `SCREEN`
    assert_eq!(file.line_col(18), line_col(0, 18)); // the `\n` itself
    assert_eq!(file.line_col(19), line_col(1, 0)); // empty line
    assert_eq!(file.line_col(20), line_col(2, 0)); // `func`
    assert_eq!(file.line_col(25), line_col(2, 5)); // `main`
    assert_eq!(file.line_col(32), line_col(3, 0)); // closing `}`

    let name_span = Span::new(id, 4, 10);
    assert_eq!(file.span_text(name_span), "SCREEN");
    assert_eq!(name_span.len(), 6);
    assert!(!name_span.is_empty());
}

#[test]
fn multibyte_utf8_advances_columns_by_encoded_length() {
    let mut map = SourceMap::new();
    // "żółć" is 4 characters, 8 bytes; the comment starts at byte 0.
    let id = map.add_file("uni.k816", "// żółć\nvar A = 1\n");
    let file = map.file(id);

    assert_eq!(file.line_col(3), line_col(0, 3)); // start of `ż`
    assert_eq!(file.line_col(5), line_col(0, 5)); // start of `ó` (after 2-byte `ż`)
    assert_eq!(file.line_col(11), line_col(0, 11)); // end of `żółć`
    assert_eq!(file.line_col(12), line_col(1, 0)); // `var` on line 1

    let word = Span::new(id, 3, 11);
    assert_eq!(file.span_text(word), "żółć");
    assert_eq!(file.line_text(0), "// żółć");
}

#[test]
fn spans_stay_attached_to_their_own_file() {
    let mut map = SourceMap::new();
    let first = map.add_file("one.k816", "var A = 1\n");
    let second = map.add_file("two.k816", "func main {\n}\n");

    assert_ne!(first, second);
    assert_eq!(map.file(first).name(), "one.k816");
    assert_eq!(map.file(second).name(), "two.k816");

    // Identical offsets resolve inside each file independently.
    let a = Span::new(first, 4, 5);
    let f = Span::new(second, 0, 4);
    assert_eq!(map.span_text(a), "A");
    assert_eq!(map.span_text(f), "func");
    assert_eq!(map.line_col(first, 4), line_col(0, 4));
    assert_eq!(map.line_col(second, 12), line_col(1, 0));
}

#[test]
fn eof_locations_are_exact_and_clamped() {
    let mut map = SourceMap::new();
    let id = map.add_file("eof.k816", "rts"); // no trailing newline
    let file = map.file(id);

    let eof = file.end_span();
    assert!(eof.is_empty());
    assert_eq!(eof.start, 3);
    assert_eq!(file.line_col(eof.start), line_col(0, 3));

    // Offsets past the end clamp instead of panicking.
    assert_eq!(file.line_col(1000), line_col(0, 3));

    // A file ending in a newline places EOF on the following empty line.
    let id = map.add_file("nl.k816", "rts\n");
    assert_eq!(map.file(id).line_col(4), line_col(1, 0));

    // An empty file has a single valid location.
    let id = map.add_file("empty.k816", "");
    assert_eq!(map.file(id).line_col(0), line_col(0, 0));
    assert_eq!(map.file(id).span().len(), 0);
}

#[test]
fn zero_width_synthetic_spans_keep_their_origin() {
    let mut map = SourceMap::new();
    let id = map.add_file("syn.k816", "func main {\n}\n");
    let file = map.file(id);

    // A compiler-inserted RTS would anchor at the closing brace.
    let origin = Span::empty_at(id, 12);
    assert!(origin.is_empty());
    assert_eq!(origin.len(), 0);
    assert_eq!(file.line_col(origin.start), line_col(1, 0));
    assert_eq!(file.span_text(origin), "");

    // Joining a synthetic point with a real span covers both.
    let brace = Span::new(id, 12, 13);
    assert_eq!(origin.join(brace), Span::new(id, 12, 13));
}

#[test]
fn line_text_strips_line_endings() {
    let mut map = SourceMap::new();
    let id = map.add_file("crlf.k816", "lda #1\r\nsta $0400\r\n");
    let file = map.file(id);

    assert_eq!(file.line_text(0), "lda #1");
    assert_eq!(file.line_text(1), "sta $0400");
    assert_eq!(file.line_col(8), line_col(1, 0));
}

#[test]
fn file_ids_are_stable_indices() {
    let mut map = SourceMap::new();
    let a = map.add_file("a", "");
    let b = map.add_file("b", "");
    assert_eq!(a.index(), 0);
    assert_eq!(b.index(), 1);
    let collected: Vec<FileId> = map.files().map(|f| f.id()).collect();
    assert_eq!(collected, vec![a, b]);
}
