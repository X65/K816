use anyhow::{Context, Result, bail};
use indexmap::IndexMap;

const MAGIC: &[u8; 8] = b"K816O65\0";
const VERSION: u16 = 1;

#[derive(Debug, Clone, Default)]
pub struct O65Object {
    pub sections: IndexMap<String, Section>,
    pub symbols: Vec<Symbol>,
    pub relocations: Vec<Relocation>,
    pub listing: String,
}

#[derive(Debug, Clone, Default)]
pub struct Section {
    pub bytes: Vec<u8>,
}

#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub global: bool,
    pub definition: Option<SymbolDefinition>,
}

#[derive(Debug, Clone)]
pub struct SymbolDefinition {
    pub section: String,
    pub offset: u32,
}

#[derive(Debug, Clone)]
pub struct Relocation {
    pub section: String,
    pub offset: u32,
    pub width: u8,
    pub kind: RelocationKind,
    pub symbol: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RelocationKind {
    Absolute,
    Relative,
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
    let mut out = Vec::new();
    out.extend_from_slice(MAGIC);
    write_u16(&mut out, VERSION);
    write_u32(&mut out, object.sections.len() as u32);
    write_u32(&mut out, object.symbols.len() as u32);
    write_u32(&mut out, object.relocations.len() as u32);
    write_string(&mut out, &object.listing)?;

    for (name, section) in &object.sections {
        write_string(&mut out, name)?;
        write_bytes(&mut out, &section.bytes)?;
    }

    for symbol in &object.symbols {
        write_string(&mut out, &symbol.name)?;
        out.push(symbol.global as u8);
        match &symbol.definition {
            Some(definition) => {
                out.push(1);
                write_string(&mut out, &definition.section)?;
                write_u32(&mut out, definition.offset);
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
        });
        write_string(&mut out, &reloc.symbol)?;
    }

    Ok(out)
}

pub fn decode_object(bytes: &[u8]) -> Result<O65Object> {
    let mut rd = Reader::new(bytes);
    let magic = rd.read_exact(8)?;
    if magic != MAGIC {
        bail!("invalid object magic");
    }
    let version = rd.read_u16()?;
    if version != VERSION {
        bail!("unsupported object version: {version}");
    }

    let section_count = rd.read_u32()? as usize;
    let symbol_count = rd.read_u32()? as usize;
    let reloc_count = rd.read_u32()? as usize;
    let listing = rd.read_string()?;

    let mut sections = IndexMap::new();
    for _ in 0..section_count {
        let name = rd.read_string()?;
        let section_bytes = rd.read_bytes()?;
        sections.insert(
            name,
            Section {
                bytes: section_bytes,
            },
        );
    }

    let mut symbols = Vec::with_capacity(symbol_count);
    for _ in 0..symbol_count {
        let name = rd.read_string()?;
        let global = rd.read_u8()? != 0;
        let has_definition = rd.read_u8()? != 0;
        let definition = if has_definition {
            Some(SymbolDefinition {
                section: rd.read_string()?,
                offset: rd.read_u32()?,
            })
        } else {
            None
        };
        symbols.push(Symbol {
            name,
            global,
            definition,
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
            other => bail!("invalid relocation kind: {other}"),
        };
        let symbol = rd.read_string()?;
        relocations.push(Relocation {
            section,
            offset,
            width,
            kind,
            symbol,
        });
    }

    if !rd.is_eof() {
        bail!("object has trailing bytes");
    }

    Ok(O65Object {
        sections,
        symbols,
        relocations,
        listing,
    })
}

fn write_u16(out: &mut Vec<u8>, value: u16) {
    out.extend_from_slice(&value.to_le_bytes());
}

fn write_u32(out: &mut Vec<u8>, value: u32) {
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
    fn object_roundtrip() {
        let mut sections = IndexMap::new();
        sections.insert(
            "default".to_string(),
            Section {
                bytes: vec![0xEA, 0x00, 0x00],
            },
        );
        let object = O65Object {
            sections,
            symbols: vec![Symbol {
                name: "target".to_string(),
                global: true,
                definition: Some(SymbolDefinition {
                    section: "default".to_string(),
                    offset: 2,
                }),
            }],
            relocations: vec![Relocation {
                section: "default".to_string(),
                offset: 1,
                width: 1,
                kind: RelocationKind::Absolute,
                symbol: "target".to_string(),
            }],
            listing: "[default]\n000000: EA 00 00\n".to_string(),
        };

        let bytes = encode_object(&object).expect("encode");
        let decoded = decode_object(&bytes).expect("decode");
        assert_eq!(decoded.sections["default"].bytes, vec![0xEA, 0x00, 0x00]);
        assert_eq!(decoded.symbols.len(), 1);
        assert_eq!(decoded.relocations.len(), 1);
        assert_eq!(decoded.listing, object.listing);
    }
}
