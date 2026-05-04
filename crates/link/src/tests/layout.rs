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
fn pinned_segment_lays_multi_source_chunks_sequentially() {
    // Two objects each contribute a chunk to the `default` segment under a
    // single rule pinned at 0x200. Without the fix, the second chunk would
    // get the same `start = 0x200` and collide with the first.
    let obj_a = object_with_single_section("default", vec![0xAA; 0x40]);
    let obj_b = object_with_single_section("default", vec![0xBB; 0x20]);

    let mut config = default_stub_config();
    config.segments[0].start = Some(0x200);
    config.output = OutputSpec {
        kind: OutputKind::RawBinary,
        file: None,
    };

    let linked = link_objects(&[obj_a, obj_b], &config).expect("link");
    let placements_a = linked
        .section_placements
        .get(&(0_usize, "default".to_string()))
        .expect("obj 0 placements");
    let placements_b = linked
        .section_placements
        .get(&(1_usize, "default".to_string()))
        .expect("obj 1 placements");
    assert_eq!(placements_a.len(), 1);
    assert_eq!(placements_b.len(), 1);
    assert_eq!(placements_a[0].base_addr, 0x200);
    assert_eq!(placements_b[0].base_addr, 0x240);
}

#[test]
fn pinned_segment_chunks_first_fit_around_fixed_chunk() {
    // Rule pinned at 0x200; one source emits a small chunk (0x80 bytes)
    // and a larger chunk (0x100 bytes). A second object pins a fixed-
    // address chunk at 0x300..0x320. The small chunk fits in 0x200..0x280
    // (the hole before the fixed chunk), while the larger chunk slides
    // past the fixed chunk to 0x320+.
    let mut sections_a = IndexMap::new();
    sections_a.insert(
        "default".to_string(),
        Section {
            chunks: vec![
                SectionChunk {
                    offset: 0,
                    address: None,
                    bytes: vec![0xAA; 0x80],
                },
                SectionChunk {
                    offset: 0x80,
                    address: None,
                    bytes: vec![0xBB; 0x100],
                },
            ],
        },
    );
    let obj_a = O65Object {
        sections: sections_a,
        symbols: Vec::new(),
        relocations: Vec::new(),
        function_disassembly: Vec::new(),
        data_string_fragments: Vec::new(),
        listing: String::new(),
    };

    let mut sections_b = IndexMap::new();
    sections_b.insert(
        "default".to_string(),
        Section {
            chunks: vec![SectionChunk {
                offset: 0,
                address: Some(0x300),
                bytes: vec![0xCC; 0x20],
            }],
        },
    );
    let obj_b = O65Object {
        sections: sections_b,
        symbols: Vec::new(),
        relocations: Vec::new(),
        function_disassembly: Vec::new(),
        data_string_fragments: Vec::new(),
        listing: String::new(),
    };

    let mut config = default_stub_config();
    config.segments[0].start = Some(0x200);
    config.output = OutputSpec {
        kind: OutputKind::RawBinary,
        file: None,
    };

    let linked = link_objects(&[obj_a, obj_b], &config).expect("link");
    let placements = linked
        .section_placements
        .get(&(0_usize, "default".to_string()))
        .expect("obj 0 placements");
    assert_eq!(placements.len(), 2);
    let small = placements
        .iter()
        .find(|p| p.len == 0x80)
        .expect("small chunk");
    let big = placements
        .iter()
        .find(|p| p.len == 0x100)
        .expect("big chunk");
    assert_eq!(
        small.base_addr, 0x200,
        "small fills hole before fixed chunk"
    );
    assert_eq!(big.base_addr, 0x320, "big slides past fixed chunk");
}

#[test]
fn unanchored_segment_anchors_after_previous_segment() {
    // Two rules: DEFAULT pinned at 0x200, RODATA unanchored. RODATA chunks
    // must place immediately after DEFAULT's last byte (modulo alignment).
    let mut sections = IndexMap::new();
    sections.insert(
        "default".to_string(),
        Section {
            chunks: vec![SectionChunk {
                offset: 0,
                address: None,
                bytes: vec![0xAA; 0x40],
            }],
        },
    );
    sections.insert(
        "RODATA".to_string(),
        Section {
            chunks: vec![SectionChunk {
                offset: 0,
                address: None,
                bytes: vec![0xBB; 0x10],
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

    let mut config = default_stub_config();
    // First rule: pinned default at 0x200.
    config.segments[0].start = Some(0x200);
    config.segments[0].segment = Some("default".to_string());
    // Second rule: unanchored RODATA.
    config.segments.push(SegmentRule {
        id: "RODATA".to_string(),
        load: "MAIN".to_string(),
        run: None,
        align: Some(1),
        start: None,
        offset: None,
        optional: false,
        segment: Some("RODATA".to_string()),
    });

    let linked = link_objects(&[object], &config).expect("link");
    let default_p = linked
        .section_placements
        .get(&(0_usize, "default".to_string()))
        .expect("default placements");
    let rodata_p = linked
        .section_placements
        .get(&(0_usize, "RODATA".to_string()))
        .expect("RODATA placements");
    assert_eq!(default_p[0].base_addr, 0x200);
    assert_eq!(rodata_p[0].base_addr, 0x240);
}

#[test]
fn offset_anchor_resolves_to_memory_start_plus_offset() {
    let object = object_with_single_section("default", vec![0xAA; 0x10]);
    let mut config = default_stub_config();
    config.memory[0].start = 0x1000;
    config.memory[0].size = 0x1000;
    config.segments[0].offset = Some(0x80);

    let linked = link_objects(&[object], &config).expect("link");
    let placements = linked
        .section_placements
        .get(&(0_usize, "default".to_string()))
        .expect("default placements");
    assert_eq!(placements[0].base_addr, 0x1080);
}

#[test]
fn rejects_rule_with_both_start_and_offset() {
    let mut config = default_stub_config();
    config.segments[0].start = Some(0x200);
    config.segments[0].offset = Some(0x10);

    let object = object_with_single_section("default", vec![0xAA; 4]);
    let err = link_objects(&[object], &config).expect_err("expected validation error");
    let msg = err.to_string();
    assert!(
        msg.contains("both 'start' and 'offset'"),
        "expected start/offset XOR diagnostic, got: {msg}"
    );
}

#[test]
fn rejects_start_outside_memory_area() {
    let mut config = default_stub_config();
    config.segments[0].start = Some(0x1_0000);

    let object = object_with_single_section("default", vec![0xAA; 4]);
    let err = link_objects(&[object], &config).expect_err("expected validation error");
    let msg = err.to_string();
    assert!(
        msg.contains("outside memory area"),
        "expected start out-of-range diagnostic, got: {msg}"
    );
}

#[test]
fn rejects_offset_outside_memory_area() {
    let mut config = default_stub_config();
    config.segments[0].offset = Some(config.memory[0].size);

    let object = object_with_single_section("default", vec![0xAA; 4]);
    let err = link_objects(&[object], &config).expect_err("expected validation error");
    let msg = err.to_string();
    assert!(
        msg.contains("outside memory area"),
        "expected offset out-of-range diagnostic, got: {msg}"
    );
}

#[test]
fn rejects_rule_load_referencing_unknown_memory() {
    let mut config = default_stub_config();
    config.segments[0].load = "MISSING".to_string();

    let object = object_with_single_section("default", vec![0xAA; 4]);
    let err = link_objects(&[object], &config).expect_err("expected validation error");
    let msg = err.to_string();
    assert!(
        msg.contains("unknown memory area"),
        "expected unknown-memory diagnostic, got: {msg}"
    );
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
