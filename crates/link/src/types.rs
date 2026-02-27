use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct LinkedRun {
    pub memory_name: String,
    pub start_addr: u32,
    pub bytes: Vec<u8>,
}

#[derive(Debug, Clone)]
pub struct LinkedLayout {
    pub runs: Vec<LinkedRun>,
    pub listing: String,
    pub symbols: HashMap<String, u32>,
    pub section_placements: HashMap<(usize, String), Vec<PlacedChunk>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LinkRenderOptions {
    pub color: bool,
}

impl LinkRenderOptions {
    pub const fn plain() -> Self {
        Self { color: false }
    }

    pub const fn colored() -> Self {
        Self { color: true }
    }
}

#[derive(Debug, Clone)]
pub struct PlacedChunk {
    pub section_offset: u32,
    pub len: u32,
    pub base_addr: u32,
    pub memory_name: String,
}
