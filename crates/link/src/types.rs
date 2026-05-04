use std::collections::HashMap;
use std::fmt;

use k816_o65::SourceLocation;

#[derive(Debug, Clone)]
pub struct LinkedRun {
    pub memory_name: String,
    pub start_addr: u32,
    pub bytes: Vec<u8>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LinkSeverity {
    Error,
    Warning,
}

/// One structured link-time diagnostic. Deliberately decoupled from any
/// renderer: the linker emits these, then CLI/LSP each translate to their
/// presentation. `anchor: None` covers the few sites that have no source
/// attribution (missing memory regions, unknown sections); the renderer falls
/// back to message-only.
#[derive(Debug, Clone)]
pub struct LinkDiagnostic {
    pub severity: LinkSeverity,
    pub message: String,
    pub primary_label: Option<String>,
    pub anchor: Option<SourceLocation>,
    pub help: Option<String>,
    pub note: Option<String>,
    pub related: Vec<LinkRelated>,
}

#[derive(Debug, Clone)]
pub struct LinkRelated {
    pub anchor: SourceLocation,
    pub message: String,
}

/// Carries a batch of `LinkDiagnostic`s as a `std::error::Error` so the new
/// linker entry point can use idiomatic `Result<_, LinkErrors>` while still
/// being compatible with `anyhow::Error`-bearing callers via `From`.
#[derive(Debug, Clone)]
pub struct LinkErrors(pub Vec<LinkDiagnostic>);

impl fmt::Display for LinkErrors {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, diag) in self.0.iter().enumerate() {
            if i > 0 {
                writeln!(f)?;
            }
            f.write_str(&diag.message)?;
        }
        Ok(())
    }
}

impl std::error::Error for LinkErrors {}

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
