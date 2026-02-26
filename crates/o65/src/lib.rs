use anyhow::{Context, Result, bail};
use indexmap::IndexMap;

const O65_MAGIC: &[u8; 5] = b"\x01\x00o65";
const O65_MODE_RELOCATABLE: u16 = 0x0000;
const PAYLOAD_VERSION: u16 = 8;

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

pub fn write_object(path: &std::path::Path, object: &O65Object) -> Result<()> {
    let bytes = encode_object(object)?;
    std::fs::write(path, bytes).with_context(|| format!("failed to write '{}'", path.display()))
}

pub fn read_object(path: &std::path::Path) -> Result<O65Object> {
    let bytes =
        std::fs::read(path).with_context(|| format!("failed to read '{}'", path.display()))?;
    decode_object(&bytes).with_context(|| format!("failed to decode '{}'", path.display()))
}

pub fn encode_object(object: &O65Object) -> Result<Vec<u8>> {
    validate_object(object)?;
    let payload = encode_payload(object)?;
    let text_len: u16 = payload
        .len()
        .try_into()
        .context("encoded object payload exceeds o65 text segment limit (64 KiB)")?;

    let mut out = Vec::with_capacity(26 + payload.len());
    out.extend_from_slice(O65_MAGIC);
    write_u16(&mut out, O65_MODE_RELOCATABLE);

    // Standard o65 header fields. We keep memory bases/sizes at zero because
    // placement is resolved by the linker from our relocatable payload.
    write_u16(&mut out, 0); // tbase
    write_u16(&mut out, text_len); // tlen
    write_u16(&mut out, 0); // dbase
    write_u16(&mut out, 0); // dlen
    write_u16(&mut out, 0); // bbase
    write_u16(&mut out, 0); // blen
    write_u16(&mut out, 0); // zbase
    write_u16(&mut out, 0); // zlen
    write_u16(&mut out, 0); // stack

    // End of o65 options list.
    out.push(0);
    out.extend_from_slice(&payload);

    Ok(out)
}

fn encode_payload(object: &O65Object) -> Result<Vec<u8>> {
    let mut out = Vec::new();
    write_u16(&mut out, PAYLOAD_VERSION);
    write_u32(&mut out, object.sections.len() as u32);
    write_u32(&mut out, object.symbols.len() as u32);
    write_u32(&mut out, object.relocations.len() as u32);
    write_u32(&mut out, object.function_disassembly.len() as u32);
    write_u32(&mut out, object.data_string_fragments.len() as u32);
    write_string(&mut out, &object.listing)?;

    for (name, section) in &object.sections {
        write_string(&mut out, name)?;
        write_u32(&mut out, section.chunks.len() as u32);
        for chunk in &section.chunks {
            write_u32(&mut out, chunk.offset);
            match chunk.address {
                Some(address) => {
                    out.push(1);
                    write_u32(&mut out, address);
                }
                None => out.push(0),
            }
            write_bytes(&mut out, &chunk.bytes)?;
        }
    }

    for symbol in &object.symbols {
        write_string(&mut out, &symbol.name)?;
        out.push(symbol.global as u8);
        match &symbol.definition {
            Some(SymbolDefinition::Section {
                section,
                offset,
                source,
            }) => {
                out.push(1);
                out.push(0);
                write_string(&mut out, section)?;
                write_u32(&mut out, *offset);
                match source {
                    Some(source) => {
                        out.push(1);
                        write_string(&mut out, &source.file)?;
                        write_u32(&mut out, source.line);
                        write_u32(&mut out, source.column);
                        write_u32(&mut out, source.column_end);
                        write_string(&mut out, &source.line_text)?;
                    }
                    None => out.push(0),
                }
            }
            Some(SymbolDefinition::Absolute { address, source }) => {
                out.push(1);
                out.push(1);
                write_u32(&mut out, *address);
                match source {
                    Some(source) => {
                        out.push(1);
                        write_string(&mut out, &source.file)?;
                        write_u32(&mut out, source.line);
                        write_u32(&mut out, source.column);
                        write_u32(&mut out, source.column_end);
                        write_string(&mut out, &source.line_text)?;
                    }
                    None => out.push(0),
                }
            }
            None => out.push(0),
        }
        match &symbol.function_metadata {
            Some(meta) => {
                out.push(1);
                out.push(meta.is_far as u8);
                out.push(encode_optional_width(meta.a_width));
                out.push(encode_optional_width(meta.i_width));
            }
            None => out.push(0),
        }
    }

    for reloc in &object.relocations {
        write_string(&mut out, &reloc.section)?;
        write_u32(&mut out, reloc.offset);
        out.push(reloc.width);
        out.push(match reloc.kind {
            RelocationKind::Absolute => 0,
            RelocationKind::Relative => 1,
            RelocationKind::LowByte => 2,
            RelocationKind::HighByte => 3,
        });
        write_string(&mut out, &reloc.symbol)?;
        match &reloc.source {
            Some(source) => {
                out.push(1);
                write_string(&mut out, &source.file)?;
                write_u32(&mut out, source.line);
                write_u32(&mut out, source.column);
                write_u32(&mut out, source.column_end);
                write_string(&mut out, &source.line_text)?;
            }
            None => out.push(0),
        }
        match &reloc.call_metadata {
            Some(meta) => {
                out.push(1);
                out.push(encode_optional_width(meta.caller_a_width));
                out.push(encode_optional_width(meta.caller_i_width));
            }
            None => out.push(0),
        }
        write_i32(&mut out, reloc.addend);
    }

    for function in &object.function_disassembly {
        write_string(&mut out, &function.section)?;
        write_string(&mut out, &function.function)?;
        let mode_flags: u8 =
            (if function.m_wide { 0x20 } else { 0 }) | (if function.x_wide { 0x10 } else { 0 });
        out.push(mode_flags);
        write_u32(&mut out, function.instruction_offsets.len() as u32);
        for offset in &function.instruction_offsets {
            write_u32(&mut out, *offset);
        }
    }

    for fragment in &object.data_string_fragments {
        write_string(&mut out, &fragment.section)?;
        write_u32(&mut out, fragment.offset);
        write_string(&mut out, &fragment.text)?;
    }

    Ok(out)
}

pub fn decode_object(bytes: &[u8]) -> Result<O65Object> {
    let mut rd = Reader::new(bytes);
    let magic = rd.read_exact(5)?;
    if magic != O65_MAGIC {
        bail!("invalid object magic");
    }

    let _mode = rd.read_u16()?;
    let _tbase = rd.read_u16()?;
    let text_len = rd.read_u16()?;
    let _dbase = rd.read_u16()?;
    let data_len = rd.read_u16()?;
    let _bbase = rd.read_u16()?;
    let _blen = rd.read_u16()?;
    let _zbase = rd.read_u16()?;
    let _zlen = rd.read_u16()?;
    let _stack = rd.read_u16()?;

    loop {
        let option_len = rd.read_u8()?;
        if option_len == 0 {
            break;
        }
        if option_len == 1 {
            bail!("invalid o65 option length: 1");
        }
        rd.read_exact(usize::from(option_len - 1))?;
    }

    let mut payload = rd.read_exact(usize::from(text_len))?.to_vec();
    if data_len > 0 {
        payload.extend_from_slice(rd.read_exact(usize::from(data_len))?);
    }
    if !rd.is_eof() {
        bail!("object has trailing bytes");
    }

    let object = decode_payload(&payload)?;
    validate_object(&object)?;
    Ok(object)
}

fn decode_payload(payload: &[u8]) -> Result<O65Object> {
    let mut rd = Reader::new(payload);
    let version = rd.read_u16()?;
    if !(1..=PAYLOAD_VERSION).contains(&version) {
        bail!("unsupported object payload version: {version}");
    }

    let section_count = rd.read_u32()? as usize;
    let symbol_count = rd.read_u32()? as usize;
    let reloc_count = rd.read_u32()? as usize;
    let function_disassembly_count = if version >= 2 {
        rd.read_u32()? as usize
    } else {
        0
    };
    let data_string_fragment_count = if version >= 3 {
        rd.read_u32()? as usize
    } else {
        0
    };
    let listing = rd.read_string()?;

    let mut sections = IndexMap::new();
    for _ in 0..section_count {
        let name = rd.read_string()?;
        let chunk_count = rd.read_u32()? as usize;
        let mut chunks = Vec::with_capacity(chunk_count);
        for _ in 0..chunk_count {
            let offset = rd.read_u32()?;
            let has_address = rd.read_u8()? != 0;
            let address = if has_address {
                Some(rd.read_u32()?)
            } else {
                None
            };
            let bytes = rd.read_bytes()?;
            chunks.push(SectionChunk {
                offset,
                address,
                bytes,
            });
        }
        sections.insert(name, Section { chunks });
    }

    let mut symbols = Vec::with_capacity(symbol_count);
    for _ in 0..symbol_count {
        let name = rd.read_string()?;
        let global = rd.read_u8()? != 0;
        let has_definition = rd.read_u8()? != 0;
        let definition = if has_definition {
            let source = |rd: &mut Reader<'_>| -> Result<Option<SourceLocation>> {
                let has_source = rd.read_u8()? != 0;
                if has_source {
                    Ok(Some(SourceLocation {
                        file: rd.read_string()?,
                        line: rd.read_u32()?,
                        column: rd.read_u32()?,
                        column_end: rd.read_u32()?,
                        line_text: rd.read_string()?,
                    }))
                } else {
                    Ok(None)
                }
            };

            if version >= 5 {
                match rd.read_u8()? {
                    0 => Some(SymbolDefinition::Section {
                        section: rd.read_string()?,
                        offset: rd.read_u32()?,
                        source: source(&mut rd)?,
                    }),
                    1 => Some(SymbolDefinition::Absolute {
                        address: rd.read_u32()?,
                        source: source(&mut rd)?,
                    }),
                    other => bail!("invalid symbol definition kind: {other}"),
                }
            } else {
                Some(SymbolDefinition::Section {
                    section: rd.read_string()?,
                    offset: rd.read_u32()?,
                    source: source(&mut rd)?,
                })
            }
        } else {
            None
        };
        let function_metadata = if version >= 7 {
            let has_meta = rd.read_u8()? != 0;
            if has_meta {
                Some(FunctionMetadata {
                    is_far: rd.read_u8()? != 0,
                    a_width: decode_optional_width(rd.read_u8()?)?,
                    i_width: decode_optional_width(rd.read_u8()?)?,
                })
            } else {
                None
            }
        } else {
            None
        };
        symbols.push(Symbol {
            name,
            global,
            definition,
            function_metadata,
        });
    }

    let mut relocations = Vec::with_capacity(reloc_count);
    for _ in 0..reloc_count {
        let section = rd.read_string()?;
        let offset = rd.read_u32()?;
        let width = rd.read_u8()?;
        let kind = match rd.read_u8()? {
            0 => RelocationKind::Absolute,
            1 => RelocationKind::Relative,
            2 => RelocationKind::LowByte,
            3 => RelocationKind::HighByte,
            other => bail!("invalid relocation kind: {other}"),
        };
        let symbol = rd.read_string()?;
        let source = if version >= 6 {
            let has_source = rd.read_u8()? != 0;
            if has_source {
                Some(SourceLocation {
                    file: rd.read_string()?,
                    line: rd.read_u32()?,
                    column: rd.read_u32()?,
                    column_end: rd.read_u32()?,
                    line_text: rd.read_string()?,
                })
            } else {
                None
            }
        } else {
            None
        };
        let call_metadata = if version >= 7 {
            let has_meta = rd.read_u8()? != 0;
            if has_meta {
                Some(CallMetadata {
                    caller_a_width: decode_optional_width(rd.read_u8()?)?,
                    caller_i_width: decode_optional_width(rd.read_u8()?)?,
                })
            } else {
                None
            }
        } else {
            None
        };
        let addend = if version >= 8 { rd.read_i32()? } else { 0 };
        relocations.push(Relocation {
            section,
            offset,
            width,
            kind,
            symbol,
            addend,
            source,
            call_metadata,
        });
    }

    let mut function_disassembly = Vec::with_capacity(function_disassembly_count);
    for _ in 0..function_disassembly_count {
        let section = rd.read_string()?;
        let function = rd.read_string()?;
        let (m_wide, x_wide) = if version >= 4 {
            let flags = rd.read_u8()?;
            (flags & 0x20 != 0, flags & 0x10 != 0)
        } else {
            (false, false)
        };
        let offset_count = rd.read_u32()? as usize;
        let mut instruction_offsets = Vec::with_capacity(offset_count);
        for _ in 0..offset_count {
            instruction_offsets.push(rd.read_u32()?);
        }
        function_disassembly.push(FunctionDisassembly {
            section,
            function,
            instruction_offsets,
            m_wide,
            x_wide,
        });
    }

    let mut data_string_fragments = Vec::with_capacity(data_string_fragment_count);
    for _ in 0..data_string_fragment_count {
        let section = rd.read_string()?;
        let offset = rd.read_u32()?;
        let text = rd.read_string()?;
        data_string_fragments.push(DataStringFragment {
            section,
            offset,
            text,
        });
    }

    if !rd.is_eof() {
        bail!("payload has trailing bytes");
    }

    Ok(O65Object {
        sections,
        symbols,
        relocations,
        function_disassembly,
        data_string_fragments,
        listing,
    })
}

fn validate_object(object: &O65Object) -> Result<()> {
    for (section_name, section) in &object.sections {
        validate_section(section_name, section)?;
    }

    for symbol in &object.symbols {
        let Some(definition) = &symbol.definition else {
            continue;
        };

        if let SymbolDefinition::Section {
            section, offset, ..
        } = definition
        {
            let section_data = object.sections.get(section).ok_or_else(|| {
                anyhow::anyhow!(
                    "symbol '{}' references unknown section '{}'",
                    symbol.name,
                    section
                )
            })?;

            if !section_contains_point(section_data, *offset)? {
                bail!(
                    "symbol '{}' offset {:#X} is outside section '{}'",
                    symbol.name,
                    offset,
                    section
                );
            }
        }
    }

    for reloc in &object.relocations {
        if reloc.width == 0 {
            bail!("relocation in section '{}' has zero width", reloc.section);
        }
        if matches!(
            reloc.kind,
            RelocationKind::LowByte | RelocationKind::HighByte
        ) && reloc.width != 1
        {
            bail!(
                "low/high-byte relocation in section '{}' must have width 1",
                reloc.section
            );
        }

        let section = object.sections.get(&reloc.section).ok_or_else(|| {
            anyhow::anyhow!("relocation references unknown section '{}'", reloc.section)
        })?;

        let end = reloc
            .offset
            .checked_add(u32::from(reloc.width))
            .ok_or_else(|| {
                anyhow::anyhow!("relocation range overflow in section '{}'", reloc.section)
            })?;

        if !section_contains_range(section, reloc.offset, end)? {
            bail!(
                "relocation site {:#X}..{:#X} is outside section '{}'",
                reloc.offset,
                end,
                reloc.section
            );
        }
    }

    for function in &object.function_disassembly {
        let section = object.sections.get(&function.section).ok_or_else(|| {
            anyhow::anyhow!(
                "function disassembly '{}' references unknown section '{}'",
                function.function,
                function.section
            )
        })?;

        for offset in &function.instruction_offsets {
            if !section_contains_instruction_start(section, *offset)? {
                bail!(
                    "function disassembly '{}' offset {:#X} is outside section '{}'",
                    function.function,
                    offset,
                    function.section
                );
            }
        }
    }

    for fragment in &object.data_string_fragments {
        let section = object.sections.get(&fragment.section).ok_or_else(|| {
            anyhow::anyhow!(
                "data string fragment at offset {:#X} references unknown section '{}'",
                fragment.offset,
                fragment.section
            )
        })?;

        let text_len: u32 = fragment
            .text
            .len()
            .try_into()
            .context("data string fragment length does not fit in u32")?;
        let end = fragment.offset.checked_add(text_len).ok_or_else(|| {
            anyhow::anyhow!(
                "data string fragment at offset {:#X} overflows section '{}'",
                fragment.offset,
                fragment.section
            )
        })?;

        if text_len == 0 {
            if !section_contains_point(section, fragment.offset)? {
                bail!(
                    "data string fragment at offset {:#X} is outside section '{}'",
                    fragment.offset,
                    fragment.section
                );
            }
        } else if !section_contains_range(section, fragment.offset, end)? {
            bail!(
                "data string fragment range {:#X}..{:#X} is outside section '{}'",
                fragment.offset,
                end,
                fragment.section
            );
        }
    }

    Ok(())
}

fn validate_section(section_name: &str, section: &Section) -> Result<()> {
    let mut prev_end: Option<u32> = None;

    for (index, chunk) in section.chunks.iter().enumerate() {
        if chunk.bytes.is_empty() {
            bail!(
                "section '{}' chunk {} at offset {:#X} is empty",
                section_name,
                index,
                chunk.offset
            );
        }

        let len =
            u32::try_from(chunk.bytes.len()).context("section chunk length does not fit in u32")?;
        let chunk_end = chunk.offset.checked_add(len).ok_or_else(|| {
            anyhow::anyhow!(
                "section '{}' chunk {} range overflows u32",
                section_name,
                index
            )
        })?;

        if let Some(prev_end) = prev_end {
            if chunk.offset < prev_end {
                bail!(
                    "section '{}' has overlapping chunks around offset {:#X}",
                    section_name,
                    chunk.offset
                );
            }
        }

        prev_end = Some(chunk_end);
    }

    Ok(())
}

fn section_contains_point(section: &Section, point: u32) -> Result<bool> {
    for chunk in &section.chunks {
        let end = chunk
            .offset
            .checked_add(
                u32::try_from(chunk.bytes.len())
                    .context("section chunk length does not fit in u32")?,
            )
            .ok_or_else(|| anyhow::anyhow!("section chunk range overflow"))?;

        if point >= chunk.offset && point <= end {
            return Ok(true);
        }
    }
    Ok(false)
}

fn section_contains_instruction_start(section: &Section, offset: u32) -> Result<bool> {
    for chunk in &section.chunks {
        let end = chunk
            .offset
            .checked_add(
                u32::try_from(chunk.bytes.len())
                    .context("section chunk length does not fit in u32")?,
            )
            .ok_or_else(|| anyhow::anyhow!("section chunk range overflow"))?;

        if offset >= chunk.offset && offset < end {
            return Ok(true);
        }
    }

    Ok(false)
}

fn section_contains_range(section: &Section, start: u32, end: u32) -> Result<bool> {
    for chunk in &section.chunks {
        let chunk_end = chunk
            .offset
            .checked_add(
                u32::try_from(chunk.bytes.len())
                    .context("section chunk length does not fit in u32")?,
            )
            .ok_or_else(|| anyhow::anyhow!("section chunk range overflow"))?;

        if start >= chunk.offset && end <= chunk_end {
            return Ok(true);
        }
    }
    Ok(false)
}

fn encode_optional_width(width: Option<bool>) -> u8 {
    match width {
        None => 0,
        Some(false) => 1, // 8-bit
        Some(true) => 2,  // 16-bit
    }
}

fn decode_optional_width(byte: u8) -> Result<Option<bool>> {
    match byte {
        0 => Ok(None),
        1 => Ok(Some(false)), // 8-bit
        2 => Ok(Some(true)),  // 16-bit
        other => bail!("invalid optional width value: {other}"),
    }
}

fn write_u16(out: &mut Vec<u8>, value: u16) {
    out.extend_from_slice(&value.to_le_bytes());
}

fn write_u32(out: &mut Vec<u8>, value: u32) {
    out.extend_from_slice(&value.to_le_bytes());
}

fn write_i32(out: &mut Vec<u8>, value: i32) {
    out.extend_from_slice(&value.to_le_bytes());
}

fn write_string(out: &mut Vec<u8>, value: &str) -> Result<()> {
    let bytes = value.as_bytes();
    let len: u32 = bytes
        .len()
        .try_into()
        .context("string too long for object encoding")?;
    write_u32(out, len);
    out.extend_from_slice(bytes);
    Ok(())
}

fn write_bytes(out: &mut Vec<u8>, value: &[u8]) -> Result<()> {
    let len: u32 = value
        .len()
        .try_into()
        .context("byte array too long for object encoding")?;
    write_u32(out, len);
    out.extend_from_slice(value);
    Ok(())
}

struct Reader<'a> {
    bytes: &'a [u8],
    pos: usize,
}

impl<'a> Reader<'a> {
    fn new(bytes: &'a [u8]) -> Self {
        Self { bytes, pos: 0 }
    }

    fn read_u8(&mut self) -> Result<u8> {
        let bytes = self.read_exact(1)?;
        Ok(bytes[0])
    }

    fn read_u16(&mut self) -> Result<u16> {
        let bytes = self.read_exact(2)?;
        Ok(u16::from_le_bytes([bytes[0], bytes[1]]))
    }

    fn read_u32(&mut self) -> Result<u32> {
        let bytes = self.read_exact(4)?;
        Ok(u32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]))
    }

    fn read_i32(&mut self) -> Result<i32> {
        let bytes = self.read_exact(4)?;
        Ok(i32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]))
    }

    fn read_string(&mut self) -> Result<String> {
        let bytes = self.read_bytes()?;
        let value = std::str::from_utf8(&bytes).context("invalid utf-8 in object")?;
        Ok(value.to_string())
    }

    fn read_bytes(&mut self) -> Result<Vec<u8>> {
        let len = self.read_u32()? as usize;
        Ok(self.read_exact(len)?.to_vec())
    }

    fn read_exact(&mut self, len: usize) -> Result<&'a [u8]> {
        let end = self.pos.saturating_add(len);
        if end > self.bytes.len() {
            bail!("unexpected EOF");
        }
        let slice = &self.bytes[self.pos..end];
        self.pos = end;
        Ok(slice)
    }

    fn is_eof(&self) -> bool {
        self.pos == self.bytes.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn encoded_object_uses_o65_magic() {
        let bytes = encode_object(&O65Object::default()).expect("encode");
        assert_eq!(&bytes[..5], O65_MAGIC);
    }

    #[test]
    fn rejects_invalid_magic() {
        let err = decode_object(b"K816O65").expect_err("expected magic error");
        assert!(err.to_string().contains("invalid object magic"));
    }

    #[test]
    fn object_roundtrip() {
        let mut sections = IndexMap::new();
        sections.insert(
            "default".to_string(),
            Section {
                chunks: vec![SectionChunk {
                    offset: 0,
                    address: None,
                    bytes: vec![0xEA, 0x00, 0x00],
                }],
            },
        );
        let object = O65Object {
            sections,
            symbols: vec![Symbol {
                name: "target".to_string(),
                global: true,
                definition: Some(SymbolDefinition::Section {
                    section: "default".to_string(),
                    offset: 2,
                    source: None,
                }),
                function_metadata: None,
            }],
            relocations: vec![Relocation {
                section: "default".to_string(),
                offset: 1,
                width: 1,
                kind: RelocationKind::Absolute,
                symbol: "target".to_string(),
                addend: 0,
                source: None,
                call_metadata: None,
            }],
            function_disassembly: vec![FunctionDisassembly {
                section: "default".to_string(),
                function: "main".to_string(),
                instruction_offsets: vec![0],
                m_wide: false,
                x_wide: false,
            }],
            data_string_fragments: vec![DataStringFragment {
                section: "default".to_string(),
                offset: 0,
                text: "EA".to_string(),
            }],
            listing: "[default]\n000000: EA 00 00\n".to_string(),
        };

        let bytes = encode_object(&object).expect("encode");
        let decoded = decode_object(&bytes).expect("decode");
        assert_eq!(decoded.sections["default"].chunks.len(), 1);
        assert_eq!(
            decoded.sections["default"].chunks[0].bytes,
            vec![0xEA, 0x00, 0x00]
        );
        assert_eq!(decoded.symbols.len(), 1);
        assert_eq!(decoded.relocations.len(), 1);
        assert_eq!(decoded.function_disassembly.len(), 1);
        assert_eq!(decoded.data_string_fragments.len(), 1);
        assert_eq!(decoded.listing, object.listing);
    }

    #[test]
    fn object_roundtrip_with_absolute_symbol() {
        let object = O65Object {
            sections: IndexMap::new(),
            symbols: vec![Symbol {
                name: "foo".to_string(),
                global: true,
                definition: Some(SymbolDefinition::Absolute {
                    address: 0x1234,
                    source: None,
                }),
                function_metadata: None,
            }],
            relocations: Vec::new(),
            function_disassembly: Vec::new(),
            data_string_fragments: Vec::new(),
            listing: String::new(),
        };

        let bytes = encode_object(&object).expect("encode");
        let decoded = decode_object(&bytes).expect("decode");
        assert_eq!(decoded.symbols.len(), 1);
        assert!(matches!(
            decoded.symbols[0].definition.as_ref(),
            Some(SymbolDefinition::Absolute {
                address: 0x1234,
                ..
            })
        ));
    }

    #[test]
    fn rejects_overlapping_chunks() {
        let mut sections = IndexMap::new();
        sections.insert(
            "default".to_string(),
            Section {
                chunks: vec![
                    SectionChunk {
                        offset: 0,
                        address: None,
                        bytes: vec![0xEA, 0x60],
                    },
                    SectionChunk {
                        offset: 1,
                        address: None,
                        bytes: vec![0xEA],
                    },
                ],
            },
        );

        let object = O65Object {
            sections,
            symbols: Vec::new(),
            relocations: Vec::new(),
            function_disassembly: Vec::new(),
            data_string_fragments: Vec::new(),
            listing: String::new(),
        };

        let err = encode_object(&object).expect_err("expected validation error");
        assert!(err.to_string().contains("overlapping chunks"));
    }

    #[test]
    fn rejects_relocation_outside_chunk_data() {
        let mut sections = IndexMap::new();
        sections.insert(
            "default".to_string(),
            Section {
                chunks: vec![SectionChunk {
                    offset: 0,
                    address: None,
                    bytes: vec![0xEA],
                }],
            },
        );

        let object = O65Object {
            sections,
            symbols: Vec::new(),
            relocations: vec![Relocation {
                section: "default".to_string(),
                offset: 1,
                width: 1,
                kind: RelocationKind::Absolute,
                symbol: "target".to_string(),
                addend: 0,
                source: None,
                call_metadata: None,
            }],
            function_disassembly: Vec::new(),
            data_string_fragments: Vec::new(),
            listing: String::new(),
        };

        let err = encode_object(&object).expect_err("expected validation error");
        assert!(err.to_string().contains("relocation site"));
    }
}
