use super::*;

#[test]
fn links_absolute_relocation() {
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
        function_disassembly: Vec::new(),
        data_string_fragments: Vec::new(),
        listing: String::new(),
    };

    let mut config = default_stub_config();
    config.output = OutputSpec {
        kind: OutputKind::RawBinary,
        file: None,
    };
    let linked = link_objects(&[object], &config).expect("link");
    assert_eq!(render(&linked, config.output.kind), vec![0xEA, 0x02, 0x00]);
}

#[test]
fn links_16bit_relocation_to_absolute_symbol() {
    let mut sections = IndexMap::new();
    sections.insert(
        "default".to_string(),
        Section {
            chunks: vec![SectionChunk {
                offset: 0,
                address: None,
                bytes: vec![0xAD, 0x00, 0x00],
            }],
        },
    );

    let object = O65Object {
        sections,
        symbols: vec![Symbol {
            name: "abs_target".to_string(),
            global: true,
            definition: Some(SymbolDefinition::Absolute {
                address: 0x1234,
                source: None,
            }),
            function_metadata: None,
        }],
        relocations: vec![Relocation {
            section: "default".to_string(),
            offset: 1,
            width: 2,
            kind: RelocationKind::Absolute,
            symbol: "abs_target".to_string(),
            addend: 0,
            source: None,
            call_metadata: None,
        }],
        function_disassembly: Vec::new(),
        data_string_fragments: Vec::new(),
        listing: String::new(),
    };

    let mut config = default_stub_config();
    config.output = OutputSpec {
        kind: OutputKind::RawBinary,
        file: None,
    };
    let linked = link_objects(&[object], &config).expect("link");
    assert_eq!(render(&linked, config.output.kind), vec![0xAD, 0x34, 0x12]);
}

#[test]
fn fixed_chunk_reserved_before_relocatable() {
    let mut sections = IndexMap::new();
    sections.insert(
        "default".to_string(),
        Section {
            chunks: vec![
                SectionChunk {
                    offset: 0,
                    address: Some(0x20),
                    bytes: vec![0xAA],
                },
                SectionChunk {
                    offset: 1,
                    address: None,
                    bytes: vec![0xBB],
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

    let mut config = default_stub_config();
    config.output = OutputSpec {
        kind: OutputKind::RawBinary,
        file: None,
    };
    let linked = link_objects(&[object], &config).expect("link");
    assert_eq!(render(&linked, config.output.kind), vec![0xAA, 0xBB]);
    assert!(linked.listing.contains("000020: AA"));
    assert!(linked.listing.contains("000021: BB"));
}

#[test]
fn rejects_overlapping_fixed_chunks() {
    let mut sections = IndexMap::new();
    sections.insert(
        "default".to_string(),
        Section {
            chunks: vec![
                SectionChunk {
                    offset: 0,
                    address: Some(0x30),
                    bytes: vec![0xAA, 0xBB],
                },
                SectionChunk {
                    offset: 2,
                    address: Some(0x31),
                    bytes: vec![0xCC],
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

    let err = link_objects(&[object], &default_stub_config()).expect_err("expected overlap");
    assert!(err.to_string().contains("overlaps existing placement"));
}

#[test]
fn rejects_out_of_bounds_fixed_chunk() {
    let mut sections = IndexMap::new();
    sections.insert(
        "default".to_string(),
        Section {
            chunks: vec![SectionChunk {
                offset: 0,
                address: Some(0x1_0000),
                bytes: vec![0xAA],
            }],
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

    let err = link_objects(&[object], &default_stub_config()).expect_err("expected range error");
    assert!(err.to_string().contains("outside memory"));
}

#[test]
fn fails_when_no_free_range_for_relocatable_chunk() {
    let mut config = default_stub_config();
    config.memory[0].size = 2;

    let mut sections = IndexMap::new();
    sections.insert(
        "default".to_string(),
        Section {
            chunks: vec![
                SectionChunk {
                    offset: 0,
                    address: Some(1),
                    bytes: vec![0xAA],
                },
                SectionChunk {
                    offset: 1,
                    address: None,
                    bytes: vec![0xBB, 0xCC],
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

    let err = link_objects(&[object], &config).expect_err("expected no-fit error");
    assert!(err.to_string().contains("no free range found"));
}
