use std::ops::Range;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SourceId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub source_id: SourceId,
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(source_id: SourceId, start: usize, end: usize) -> Self {
        Self {
            source_id,
            start,
            end,
        }
    }

    pub fn as_range(&self) -> Range<usize> {
        self.start..self.end
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(node: T, span: Span) -> Self {
        Self { node, span }
    }

    pub fn map<U>(self, mapper: impl FnOnce(T) -> U) -> Spanned<U> {
        Spanned {
            node: mapper(self.node),
            span: self.span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct SourceFile {
    pub name: String,
    pub text: String,
    line_starts: Vec<usize>,
}

impl SourceFile {
    fn new(name: impl Into<String>, text: impl Into<String>) -> Self {
        let text = text.into();
        let mut line_starts = vec![0];
        for (offset, ch) in text.char_indices() {
            if ch == '\n' {
                line_starts.push(offset + 1);
            }
        }

        Self {
            name: name.into(),
            text,
            line_starts,
        }
    }

    pub fn line_col(&self, offset: usize) -> (usize, usize) {
        let line_idx = self
            .line_starts
            .partition_point(|line_start| *line_start <= offset)
            .saturating_sub(1);
        let line_start = self.line_starts[line_idx];
        let line = line_idx + 1;
        let col = offset.saturating_sub(line_start) + 1;
        (line, col)
    }

    pub fn slice(&self, span: Span) -> &str {
        &self.text[span.start..span.end]
    }
}

#[derive(Debug, Clone, Default)]
pub struct SourceMap {
    files: Vec<SourceFile>,
}

impl SourceMap {
    pub fn add_source(&mut self, name: impl Into<String>, text: impl Into<String>) -> SourceId {
        let id = SourceId(self.files.len() as u32);
        self.files.push(SourceFile::new(name, text));
        id
    }

    pub fn get(&self, source_id: SourceId) -> Option<&SourceFile> {
        self.files.get(source_id.0 as usize)
    }

    pub fn must_get(&self, source_id: SourceId) -> &SourceFile {
        self.get(source_id)
            .expect("source id should exist in source map")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn maps_line_and_column() {
        let mut map = SourceMap::default();
        let source_id = map.add_source("test", "first\nsecond\nthird");
        let file = map.must_get(source_id);
        assert_eq!(file.line_col(0), (1, 1));
        assert_eq!(file.line_col(7), (2, 2));
        assert_eq!(file.line_col(14), (3, 2));
    }
}
