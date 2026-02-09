#[derive(Debug, Clone, Default)]
pub struct Doc {
    chunks: Vec<String>,
}

impl Doc {
    pub fn text(mut self, chunk: impl Into<String>) -> Self {
        self.chunks.push(chunk.into());
        self
    }

    pub fn line(mut self) -> Self {
        self.chunks.push("\n".to_string());
        self
    }

    pub fn render(self) -> String {
        self.chunks.concat()
    }
}
