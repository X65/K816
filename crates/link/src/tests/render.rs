use super::*;

#[test]
fn raw_binary_rejects_multiple_used_memory_areas() {
    let mut config = default_stub_config();
    config.output = OutputSpec {
        kind: OutputKind::RawBinary,
        file: None,
    };
    config.memory.push(MemoryArea {
        name: "AUX".to_string(),
        start: 0x8000,
        size: 0x1000,
        kind: MemoryKind::ReadWrite,
        fill: Some(0),
    });
    config.segments = vec![
        SegmentRule {
            id: "DEFAULT".to_string(),
            load: "MAIN".to_string(),
            run: None,
            align: Some(1),
            start: None,
            offset: None,
            optional: false,
            segment: Some("default".to_string()),
        },
        SegmentRule {
            id: "AUX".to_string(),
            load: "AUX".to_string(),
            run: None,
            align: Some(1),
            start: None,
            offset: None,
            optional: false,
            segment: Some("aux".to_string()),
        },
    ];

    let mut sections = IndexMap::new();
    sections.insert(
        "default".to_string(),
        Section {
            chunks: vec![SectionChunk {
                offset: 0,
                address: None,
                bytes: vec![0xAA],
            }],
        },
    );
    sections.insert(
        "aux".to_string(),
        Section {
            chunks: vec![SectionChunk {
                offset: 0,
                address: None,
                bytes: vec![0xBB],
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

    let linked = link_objects(&[object], &config).expect("link");
    let err =
        render_linked_output(&linked, OutputKind::RawBinary).expect_err("expected ambiguity error");
    assert!(err.to_string().contains("raw binary output is ambiguous"));
}

#[test]
fn raw_binary_rejects_multiple_segment_runs_in_single_memory_area() {
    let mut config = default_stub_config();
    config.output = OutputSpec {
        kind: OutputKind::RawBinary,
        file: None,
    };
    config.segments = vec![
        SegmentRule {
            id: "DEFAULT".to_string(),
            load: "MAIN".to_string(),
            run: None,
            align: Some(1),
            start: Some(0x0200),
            offset: None,
            optional: false,
            segment: Some("default".to_string()),
        },
        SegmentRule {
            id: "INFO".to_string(),
            load: "MAIN".to_string(),
            run: None,
            align: Some(1),
            start: Some(0xFC00),
            offset: None,
            optional: false,
            segment: Some("info".to_string()),
        },
    ];

    let mut sections = IndexMap::new();
    sections.insert(
        "default".to_string(),
        Section {
            chunks: vec![SectionChunk {
                offset: 0,
                address: None,
                bytes: vec![0xAA],
            }],
        },
    );
    sections.insert(
        "info".to_string(),
        Section {
            chunks: vec![SectionChunk {
                offset: 0,
                address: None,
                bytes: vec![0xBB],
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

    let linked = link_objects(&[object], &config).expect("link");
    let err =
        render_linked_output(&linked, OutputKind::RawBinary).expect_err("expected ambiguity error");
    assert!(err.to_string().contains("raw binary output is ambiguous"));
}

#[test]
fn emits_xex_blocks_from_used_runs() {
    let mut config = default_stub_config();
    config.output = OutputSpec {
        kind: OutputKind::Xex,
        file: None,
    };

    let mut sections = IndexMap::new();
    sections.insert(
        "default".to_string(),
        Section {
            chunks: vec![
                SectionChunk {
                    offset: 0,
                    address: Some(0x0200),
                    bytes: vec![0xAA],
                },
                SectionChunk {
                    offset: 1,
                    address: Some(0x0210),
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

    let linked = link_objects(&[object], &config).expect("link");
    let xex = render(&linked, config.output.kind);
    assert_eq!(
        xex,
        vec![
            0xFF, 0xFF, 0x00, 0x02, 0x00, 0x02, 0xAA, 0x10, 0x02, 0x11, 0x02, 0xBB, 0xCC,
        ]
    );
}

#[test]
fn rejects_xex_addresses_above_16bit() {
    let mut config = default_stub_config();
    config.memory[0].start = 0x1_0000;
    config.memory[0].size = 0x100;
    config.output = OutputSpec {
        kind: OutputKind::Xex,
        file: None,
    };

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

    let linked = link_objects(&[object], &config).expect("link");
    let err =
        render_linked_output(&linked, OutputKind::Xex).expect_err("expected xex address error");
    assert!(
        err.to_string()
            .contains("xex output supports 16-bit load addresses")
    );
}
