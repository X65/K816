use ariadne::{Cache, Color, Config, IndexType, Label, Report, ReportKind, Source};
use k816_o65::{O65Object, SourceLocation, SymbolDefinition};
use std::fmt;

use super::{AnchorContext, PlannedChunk};
use crate::types::LinkRenderOptions;

pub(super) fn render_duplicate_symbol_error(
    name: &str,
    first: Option<&SourceLocation>,
    second: Option<&SourceLocation>,
    options: LinkRenderOptions,
) -> String {
    let message = format!("duplicate global symbol '{name}'");

    let (Some(second_src), Some(first_src)) = (second, first) else {
        return message;
    };

    let mut sources = MultiSourceCache::new();
    let first_id = first_src.file.clone();
    let second_id = second_src.file.clone();

    let first_line_prefix = "\n".repeat(first_src.line.saturating_sub(1) as usize);
    let first_padded = format!("{first_line_prefix}{}", first_src.line_text);
    let first_byte_offset = first_line_prefix.len();
    sources.insert(first_id.clone(), Source::from(first_padded));

    let second_line_prefix = "\n".repeat(second_src.line.saturating_sub(1) as usize);
    let second_padded = format!("{second_line_prefix}{}", second_src.line_text);
    let second_byte_offset = second_line_prefix.len();
    sources.insert(second_id.clone(), Source::from(second_padded));

    let first_start = clamp_span(first_src, first_byte_offset);
    let second_start = clamp_span(second_src, second_byte_offset);

    let mut output = Vec::new();
    let report = Report::build(ReportKind::Error, (second_id.clone(), second_start.clone()))
        .with_config(
            Config::default()
                .with_index_type(IndexType::Byte)
                .with_color(options.color),
        )
        .with_message(&message)
        .with_label(
            Label::new((second_id, second_start))
                .with_color(Color::Red)
                .with_priority(100)
                .with_order(0)
                .with_message("duplicate definition"),
        )
        .with_label(
            Label::new((first_id, first_start))
                .with_color(Color::Blue)
                .with_order(1)
                .with_message("first defined here"),
        )
        .finish();

    if report.write(&mut sources, &mut output).is_ok() {
        return String::from_utf8_lossy(&output).into_owned();
    }

    message
}

fn clamp_span(src: &SourceLocation, byte_offset: usize) -> std::ops::Range<usize> {
    let line_len = src.line_text.len();
    let mut start = src.column.saturating_sub(1) as usize;
    if start > line_len {
        start = line_len;
    }
    let mut end = src.column_end.saturating_sub(1) as usize;
    if end <= start {
        end = start.saturating_add(1);
    }
    if end > line_len {
        end = line_len;
    }
    if end <= start && line_len > start {
        end = start + 1;
    }
    (byte_offset + start)..(byte_offset + end)
}

#[derive(Debug)]
struct MultiSourceCache {
    sources: Vec<(String, Source<String>)>,
}

impl MultiSourceCache {
    fn new() -> Self {
        Self {
            sources: Vec::new(),
        }
    }

    fn insert(&mut self, id: String, source: Source<String>) {
        if !self
            .sources
            .iter()
            .any(|(existing_id, _)| existing_id == &id)
        {
            self.sources.push((id, source));
        }
    }
}

impl Cache<String> for MultiSourceCache {
    type Storage = String;

    fn fetch(
        &mut self,
        id: &String,
    ) -> std::result::Result<&Source<Self::Storage>, impl fmt::Debug> {
        self.sources
            .iter()
            .find(|(existing_id, _)| existing_id == id)
            .map(|(_, source)| source)
            .ok_or_else(|| format!("missing source for '{id}'"))
    }

    fn display<'a>(&self, id: &'a String) -> Option<impl fmt::Display + 'a> {
        Some(id)
    }
}

pub(super) fn find_anchor_context(
    objects: &[O65Object],
    chunk: &PlannedChunk,
) -> Option<AnchorContext> {
    find_section_anchor_context(objects, chunk.obj_idx, &chunk.segment, chunk.section_offset)
}

pub(super) fn find_section_anchor_context(
    objects: &[O65Object],
    obj_idx: usize,
    section_name: &str,
    section_offset: u32,
) -> Option<AnchorContext> {
    let object = objects.get(obj_idx)?;
    let mut best_with_source: Option<(u32, AnchorContext)> = None;
    let mut best_without_source: Option<(u32, String)> = None;

    for symbol in &object.symbols {
        let Some(SymbolDefinition::Section {
            section,
            offset,
            source,
        }) = symbol.definition.as_ref()
        else {
            continue;
        };
        if section != section_name || *offset > section_offset {
            continue;
        }

        if let Some(source) = source {
            let candidate = AnchorContext {
                symbol_name: symbol.name.clone(),
                source: Some(source.clone()),
            };
            if best_with_source
                .as_ref()
                .is_none_or(|(best_offset, _)| *offset >= *best_offset)
            {
                best_with_source = Some((*offset, candidate));
            }
            continue;
        }

        if best_without_source
            .as_ref()
            .is_none_or(|(best_offset, _)| *offset >= *best_offset)
        {
            best_without_source = Some((*offset, symbol.name.clone()));
        }
    }

    if let Some((_, anchor)) = best_with_source {
        return Some(anchor);
    }

    best_without_source.map(|(_, symbol_name)| AnchorContext {
        symbol_name,
        source: None,
    })
}

pub(super) fn decorate_with_anchor(
    message: &str,
    anchor: Option<&AnchorContext>,
    options: LinkRenderOptions,
) -> String {
    let label_message =
        anchor.map(|anchor| format!("function '{}' defined here", anchor.symbol_name));
    decorate_with_anchor_with_label(message, anchor, options, label_message.as_deref(), None)
}

pub(super) fn decorate_with_anchor_with_label(
    message: &str,
    anchor: Option<&AnchorContext>,
    options: LinkRenderOptions,
    label_message: Option<&str>,
    help: Option<&str>,
) -> String {
    let Some(anchor) = anchor else {
        return match help {
            Some(help) => format!("{message}\n\n Help: {help}"),
            None => message.to_string(),
        };
    };
    let label_message = label_message.unwrap_or("defined here");

    let Some(source) = &anchor.source else {
        return format!("{message}\n{label_message}");
    };

    let context = render_anchor_context(message, source, options, label_message, help);
    if context.is_empty() {
        format!(
            "{message}\n{label_message} at {}:{}:{}",
            source.file, source.line, source.column
        )
    } else {
        context
    }
}

fn render_anchor_context(
    message: &str,
    source: &SourceLocation,
    options: LinkRenderOptions,
    label_message: &str,
    help: Option<&str>,
) -> String {
    let file_id = source.file.clone();
    let line_len = source.line_text.len();
    let mut start = source.column.saturating_sub(1) as usize;
    if start > line_len {
        start = line_len;
    }
    let mut end = source.column_end.saturating_sub(1) as usize;
    if end <= start {
        end = start.saturating_add(1);
    }
    if end > line_len {
        end = line_len;
    }
    if end <= start && line_len > start {
        end = start + 1;
    }

    let line_prefix = "\n".repeat(source.line.saturating_sub(1) as usize);
    let padded_source = format!("{line_prefix}{}", source.line_text);
    let byte_offset = line_prefix.len();

    let mut cache = SingleSourceCache {
        id: file_id.clone(),
        source: Source::from(padded_source),
    };
    let mut output = Vec::new();

    let mut report = Report::build(
        ReportKind::Error,
        (file_id.clone(), byte_offset + start..byte_offset + end),
    )
    .with_config(
        Config::default()
            .with_index_type(IndexType::Byte)
            .with_color(options.color),
    )
    .with_message(message.to_string())
    .with_label(
        Label::new((file_id.clone(), byte_offset + start..byte_offset + end))
            .with_message(label_message.to_string()),
    );
    if let Some(help) = help {
        report = report.with_help(help.to_string());
    }
    let report = report.finish();

    if report.write(&mut cache, &mut output).is_ok() {
        String::from_utf8_lossy(&output).into_owned()
    } else {
        String::new()
    }
}

#[derive(Debug)]
struct SingleSourceCache {
    id: String,
    source: Source<String>,
}

impl Cache<String> for SingleSourceCache {
    type Storage = String;

    fn fetch(
        &mut self,
        id: &String,
    ) -> std::result::Result<&Source<Self::Storage>, impl fmt::Debug> {
        if id == &self.id {
            Ok::<_, String>(&self.source)
        } else {
            Err::<&Source<Self::Storage>, _>(format!("missing source for '{id}'"))
        }
    }

    fn display<'a>(&self, id: &'a String) -> Option<impl fmt::Display + 'a> {
        Some(id)
    }
}
