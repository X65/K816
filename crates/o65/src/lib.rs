use anyhow::{Context, Result, bail};
use indexmap::IndexMap;

mod codec;
mod io;
mod model;
mod validate;

#[cfg(test)]
mod tests;

#[cfg(test)]
use self::codec::O65_MAGIC;
pub use self::codec::{decode_object, encode_object};
pub use self::io::{read_object, write_object};
pub use self::model::{
    CallMetadata, DataStringFragment, FunctionDisassembly, FunctionMetadata, O65Object, Relocation,
    RelocationKind, Section, SectionChunk, SourceLocation, Symbol, SymbolDefinition,
};
use self::validate::validate_object;
