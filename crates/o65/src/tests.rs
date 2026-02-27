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
