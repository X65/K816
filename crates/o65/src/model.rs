use super::*;

#[derive(Debug, Clone, Default)]
pub struct O65Object {
    pub sections: IndexMap<String, Section>,
    pub symbols: Vec<Symbol>,
    pub relocations: Vec<Relocation>,
    pub function_disassembly: Vec<FunctionDisassembly>,
    pub data_string_fragments: Vec<DataStringFragment>,
    pub listing: String,
}

#[derive(Debug, Clone, Default)]
pub struct FunctionDisassembly {
    pub section: String,
    pub function: String,
    pub instruction_offsets: Vec<u32>,
    /// Initial accumulator width for disassembly: true = 16-bit, false = 8-bit.
    pub m_wide: bool,
    /// Initial index register width for disassembly: true = 16-bit, false = 8-bit.
    pub x_wide: bool,
}

#[derive(Debug, Clone, Default)]
pub struct DataStringFragment {
    pub section: String,
    pub offset: u32,
    pub text: String,
}

#[derive(Debug, Clone, Default)]
pub struct Section {
    pub chunks: Vec<SectionChunk>,
}

#[derive(Debug, Clone, Default)]
pub struct SectionChunk {
    pub offset: u32,
    pub address: Option<u32>,
    pub bytes: Vec<u8>,
}

#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub global: bool,
    pub definition: Option<SymbolDefinition>,
    pub function_metadata: Option<FunctionMetadata>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FunctionMetadata {
    pub is_far: bool,
    /// None = unspecified, Some(false) = 8-bit, Some(true) = 16-bit.
    pub a_width: Option<bool>,
    /// None = unspecified, Some(false) = 8-bit, Some(true) = 16-bit.
    pub i_width: Option<bool>,
}

#[derive(Debug, Clone)]
pub enum SymbolDefinition {
    Section {
        section: String,
        offset: u32,
        source: Option<SourceLocation>,
    },
    Absolute {
        address: u32,
        source: Option<SourceLocation>,
    },
}

#[derive(Debug, Clone)]
pub struct SourceLocation {
    pub file: String,
    pub line: u32,
    pub column: u32,
    pub column_end: u32,
    pub line_text: String,
}

#[derive(Debug, Clone)]
pub struct Relocation {
    pub section: String,
    pub offset: u32,
    pub width: u8,
    pub kind: RelocationKind,
    pub symbol: String,
    pub addend: i32,
    pub source: Option<SourceLocation>,
    pub call_metadata: Option<CallMetadata>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CallMetadata {
    /// Caller's accumulator width at the call site.
    /// None = unknown, Some(false) = 8-bit, Some(true) = 16-bit.
    pub caller_a_width: Option<bool>,
    /// Caller's index register width at the call site.
    /// None = unknown, Some(false) = 8-bit, Some(true) = 16-bit.
    pub caller_i_width: Option<bool>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RelocationKind {
    Absolute,
    Relative,
    LowByte,
    HighByte,
}
