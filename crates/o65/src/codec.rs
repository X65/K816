use super::*;

pub(super) const O65_MAGIC: &[u8; 5] = b"\x01\x00o65";
const O65_MODE_RELOCATABLE: u16 = 0x0000;
const PAYLOAD_VERSION: u16 = 8;

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
