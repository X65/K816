use std::path::Path;

use thiserror::Error;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub source_id: u32,
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Arg {
    Str(String),
    Int(i64),
    Array(Vec<Arg>),
}

pub struct ConvertRequest<'a> {
    pub kind: &'a str,
    pub args: &'a [Arg],
    pub span: Span,
    pub fs: &'a dyn AssetFS,
}

pub trait DataConverter: Send + Sync {
    fn kind(&self) -> &'static str;
    fn convert(&self, req: ConvertRequest<'_>) -> Result<Vec<u8>, ConvertError>;
}

pub trait AssetFS: Send + Sync {
    fn read(&self, path: &Path) -> Result<Vec<u8>, ConvertError>;
}

#[derive(Debug, Default)]
pub struct StdAssetFS;

impl AssetFS for StdAssetFS {
    fn read(&self, path: &Path) -> Result<Vec<u8>, ConvertError> {
        std::fs::read(path).map_err(|source| ConvertError::Io {
            path: path.display().to_string(),
            source,
        })
    }
}

#[derive(Debug, Error)]
pub enum ConvertError {
    #[error("missing argument {index}")]
    MissingArg { index: usize },
    #[error("invalid argument {index}: {message}")]
    InvalidArg { index: usize, message: String },
    #[error("converter '{kind}' failed: {message}")]
    Message { kind: String, message: String },
    #[error("failed to read '{path}'")]
    Io {
        path: String,
        #[source]
        source: std::io::Error,
    },
    #[error("image decode error: {message}")]
    ImageDecode { message: String },
}
