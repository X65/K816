use super::*;
use indexmap::IndexMap;
use k816_o65::O65Object;
use k816_o65::{
    DataStringFragment, Relocation, RelocationKind, Section, SectionChunk, SourceLocation, Symbol,
    SymbolDefinition,
};

fn object_with_single_section(section: &str, bytes: Vec<u8>) -> O65Object {
    let mut sections = IndexMap::new();
    sections.insert(
        section.to_string(),
        Section {
            chunks: vec![SectionChunk {
                offset: 0,
                address: None,
                bytes,
            }],
        },
    );
    O65Object {
        sections,
        symbols: Vec::new(),
        relocations: Vec::new(),
        function_disassembly: Vec::new(),
        data_string_fragments: Vec::new(),
        listing: String::new(),
    }
}

fn segment_rule(id: &str, segment: Option<&str>, start: Option<u32>) -> SegmentRule {
    SegmentRule {
        id: id.to_string(),
        load: "MAIN".to_string(),
        run: None,
        align: Some(1),
        start,
        offset: None,
        optional: false,
        segment: segment.map(std::string::ToString::to_string),
    }
}

fn symbol_with_source(
    name: &str,
    section: &str,
    offset: u32,
    line: u32,
    column: u32,
    line_text: &str,
) -> Symbol {
    Symbol {
        name: name.to_string(),
        global: true,
        definition: Some(SymbolDefinition::Section {
            section: section.to_string(),
            offset,
            source: Some(SourceLocation {
                file: "fixture.k65".to_string(),
                line,
                column,
                column_end: column + 1,
                line_text: line_text.to_string(),
            }),
        }),
        function_metadata: None,
    }
}

fn render(layout: &LinkedLayout, kind: OutputKind) -> Vec<u8> {
    render_linked_output(layout, kind).expect("render")
}

mod config;
mod layout;
mod listing;
mod render;
