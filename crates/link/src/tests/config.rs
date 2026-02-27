use super::*;

#[test]
fn default_stub_config_emits_xex() {
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
        relocations: Vec::new(),
        function_disassembly: Vec::new(),
        data_string_fragments: Vec::new(),
        listing: String::new(),
    };

    let config = default_stub_config();
    let linked = link_objects(&[object], &config).expect("link");
    assert_eq!(
        render(&linked, config.output.kind),
        vec![0xFF, 0xFF, 0x00, 0x00, 0x00, 0x00, 0xEA]
    );
}

#[test]
fn linker_config_defaults_output_kind_to_xex_when_unspecified() {
    let config: LinkerConfig = ron::from_str(
        r#"(
  format: "o65-link",
  target: Some("defaults"),
  memory: [
    (
      name: "MAIN",
      start: 0,
      size: 65536,
      kind: ReadWrite,
      fill: Some(0),
    ),
  ],
  segments: [
    (
      id: "DEFAULT",
      load: "MAIN",
      run: None,
      align: Some(1),
      start: None,
      offset: None,
      optional: false,
      segment: None,
    ),
  ],
  symbols: [],
  output: (
    file: None,
  ),
  entry: None,
)"#,
    )
    .expect("config should parse");

    assert_eq!(config.output.kind, OutputKind::Xex);
}

#[test]
fn explicit_segment_selector_takes_precedence_over_default_rule() {
    let object = object_with_single_section("info", vec![0xAA]);

    let mut config = default_stub_config();
    config.segments = vec![
        segment_rule("DEFAULT", None, Some(0x0200)),
        segment_rule("INFO_EXPLICIT", Some("info"), Some(0x0300)),
    ];

    let linked = link_objects(&[object], &config).expect("link");
    assert!(linked.listing.contains("000300: AA"));
}

#[test]
fn selectorless_rule_is_used_as_default_fallback() {
    let object = object_with_single_section("other", vec![0xBB]);

    let mut config = default_stub_config();
    config.segments = vec![segment_rule("DEFAULT", None, Some(0x0400))];

    let linked = link_objects(&[object], &config).expect("link");
    assert!(linked.listing.contains("000400: BB"));
}

#[test]
fn does_not_match_segment_by_rule_id_when_selector_missing() {
    let object = object_with_single_section("info", vec![0xCC]);

    let mut config = default_stub_config();
    config.segments = vec![
        segment_rule("DEFAULT", None, Some(0x0200)),
        segment_rule("INFO", None, Some(0xFC00)),
    ];

    let linked = link_objects(&[object], &config).expect("link");
    assert!(linked.listing.contains("000200: CC"));
    assert!(!linked.listing.contains("00FC00: CC"));
}

#[test]
fn fails_when_no_matching_selector_or_default_rule_exists() {
    let object = object_with_single_section("info", vec![0xDD]);

    let mut config = default_stub_config();
    config.segments = vec![segment_rule(
        "ONLY_DEFAULT_SEGMENT",
        Some("default"),
        Some(0x0200),
    )];

    let err = link_objects(&[object], &config).expect_err("expected missing rule");
    assert!(
        err.to_string()
            .contains("no segment rule found for segment 'info'")
    );
}

#[test]
fn rejects_empty_segment_rule_id() {
    let object = object_with_single_section("default", vec![0xEE]);

    let mut config = default_stub_config();
    config.segments = vec![segment_rule("  ", None, Some(0x0200))];

    let err = link_objects(&[object], &config).expect_err("expected empty id error");
    assert!(
        err.to_string()
            .contains("segment rule id at index 0 must not be empty")
    );
}

#[test]
fn rejects_duplicate_segment_rule_ids() {
    let object = object_with_single_section("default", vec![0xFF]);

    let mut config = default_stub_config();
    config.segments = vec![
        segment_rule("DUP", Some("default"), Some(0x0200)),
        segment_rule("DUP", Some("info"), Some(0x0300)),
    ];

    let err = link_objects(&[object], &config).expect_err("expected duplicate id error");
    assert!(err.to_string().contains("duplicate segment rule id 'DUP'"));
}
