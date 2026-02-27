use super::*;

#[test]
fn listing_includes_named_data_symbol_addresses() {
    let mut object = object_with_single_section("default", vec![0xAA, 0xBB, 0xCC]);
    object.symbols = vec![symbol_with_source(
        "text",
        "default",
        1,
        1,
        1,
        "data text {",
    )];

    let linked = link_objects(&[object], &default_stub_config()).expect("link");
    assert!(linked.listing.contains("[data obj#0 default::text]"));
    assert!(linked.listing.contains("000001: <2 bytes of data>"));
}

#[test]
fn listing_skips_non_data_symbols_by_source_line() {
    let mut object = object_with_single_section("default", vec![0xAA, 0xBB, 0xCC]);
    object.symbols = vec![
        symbol_with_source("main", "default", 0, 1, 1, "func main {"),
        symbol_with_source("main::.loop", "default", 1, 2, 3, ".loop:"),
    ];

    let linked = link_objects(&[object], &default_stub_config()).expect("link");
    assert!(!linked.listing.contains("[data obj#0"));
}

#[test]
fn listing_skips_symbols_without_source_metadata() {
    let mut object = object_with_single_section("default", vec![0xAA]);
    object.symbols = vec![Symbol {
        name: "text".to_string(),
        global: true,
        definition: Some(SymbolDefinition::Section {
            section: "default".to_string(),
            offset: 0,
            source: None,
        }),
        function_metadata: None,
    }];

    let linked = link_objects(&[object], &default_stub_config()).expect("link");
    assert!(!linked.listing.contains("[data obj#0"));
}

#[test]
fn listing_orders_data_symbols_by_source_location_then_name() {
    let mut object = object_with_single_section("default", vec![0x10, 0x20, 0x30, 0x40]);
    object.symbols = vec![
        symbol_with_source("zeta", "default", 3, 20, 1, "data zeta {"),
        symbol_with_source("gamma", "default", 1, 10, 4, "data gamma {"),
        symbol_with_source("alpha", "default", 0, 10, 4, "data alpha {"),
        symbol_with_source("beta", "default", 2, 10, 6, "data beta {"),
    ];

    let linked = link_objects(&[object], &default_stub_config()).expect("link");
    let alpha_pos = linked
        .listing
        .find("[data obj#0 default::alpha]")
        .expect("alpha block");
    let gamma_pos = linked
        .listing
        .find("[data obj#0 default::gamma]")
        .expect("gamma block");
    let beta_pos = linked
        .listing
        .find("[data obj#0 default::beta]")
        .expect("beta block");
    let zeta_pos = linked
        .listing
        .find("[data obj#0 default::zeta]")
        .expect("zeta block");

    assert!(alpha_pos < gamma_pos);
    assert!(gamma_pos < beta_pos);
    assert!(beta_pos < zeta_pos);
}

#[test]
fn listing_computes_data_length_until_next_top_level_code_block() {
    let mut object = object_with_single_section("default", vec![0x01, 0x02, 0x03, 0x04]);
    object.symbols = vec![
        symbol_with_source("bytes", "default", 0, 1, 1, "data bytes {"),
        symbol_with_source("here", "default", 3, 4, 1, "here:"),
        symbol_with_source("main", "default", 4, 6, 1, "func main {"),
    ];

    let linked = link_objects(&[object], &default_stub_config()).expect("link");
    assert!(linked.listing.contains("000000: <4 bytes of data>"));
}

#[test]
fn listing_renders_string_literal_parts_inside_data_symbol() {
    let mut object = object_with_single_section(
        "default",
        vec![
            b'H', b'e', b'l', b'l', b'o', b',', b' ', b'W', b'o', b'r', b'l', b'd', b'!', 0x0D,
            0x0A, 0x00, 0x60,
        ],
    );
    object.symbols = vec![
        symbol_with_source("text", "default", 0, 1, 1, "data text {"),
        symbol_with_source("main", "default", 16, 6, 1, "func main {"),
    ];
    object.data_string_fragments = vec![DataStringFragment {
        section: "default".to_string(),
        offset: 0,
        text: "Hello, World!".to_string(),
    }];

    let linked = link_objects(&[object], &default_stub_config()).expect("link");
    assert!(linked.listing.contains("000000: \"Hello, World!\""));
    assert!(linked.listing.contains("00000D: <3 bytes of data>"));
}
