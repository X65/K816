use anyhow::{Context, Result, bail};
use serde::Deserialize;
use std::collections::HashMap;
use std::path::Path;

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct LinkerConfig {
    #[serde(default = "default_format")]
    pub format: String,
    #[serde(default)]
    pub target: Option<String>,
    pub memory: Vec<MemoryArea>,
    #[serde(default)]
    pub segments: Vec<SegmentRule>,
    #[serde(default)]
    pub symbols: Vec<LinkSymbol>,
    #[serde(default)]
    pub output: OutputSpec,
    #[serde(default)]
    pub entry: Option<String>,
}

fn default_format() -> String {
    "o65-link".to_string()
}

fn default_output_kind() -> OutputKind {
    OutputKind::Xex
}

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct MemoryArea {
    pub name: String,
    pub start: u32,
    pub size: u32,
    pub kind: MemoryKind,
    #[serde(default)]
    pub fill: Option<u8>,
}

#[derive(Debug, Clone, Copy, Deserialize)]
pub enum MemoryKind {
    ReadOnly,
    ReadWrite,
}

#[derive(Debug, Clone, Deserialize)]
pub struct SegmentRule {
    pub id: String,
    pub load: String,
    #[serde(default)]
    pub run: Option<String>,
    #[serde(default)]
    pub align: Option<u32>,
    #[serde(default)]
    pub start: Option<u32>,
    #[serde(default)]
    pub offset: Option<u32>,
    #[serde(default)]
    pub optional: bool,
    #[serde(default)]
    pub segment: Option<String>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct LinkSymbol {
    pub name: String,
    pub value: SymbolValue,
}

#[derive(Debug, Clone, Deserialize)]
pub enum SymbolValue {
    Absolute(u32),
    Import(String),
}

#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct OutputSpec {
    #[serde(default = "default_output_kind")]
    pub kind: OutputKind,
    #[serde(default)]
    pub file: Option<String>,
}

impl Default for OutputSpec {
    fn default() -> Self {
        Self {
            kind: default_output_kind(),
            file: None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
pub enum OutputKind {
    RawBinary,
    Xex,
}

pub fn load_config(path: &Path) -> Result<LinkerConfig> {
    let text = std::fs::read_to_string(path)
        .with_context(|| format!("failed to read linker config '{}'", path.display()))?;
    ron::from_str(&text)
        .with_context(|| format!("failed to parse linker config '{}'", path.display()))
}

pub fn default_stub_config() -> LinkerConfig {
    LinkerConfig {
        format: "o65-link".to_string(),
        target: Some("stub".to_string()),
        memory: vec![MemoryArea {
            name: "MAIN".to_string(),
            start: 0,
            size: 0x1_0000,
            kind: MemoryKind::ReadWrite,
            fill: Some(0),
        }],
        segments: vec![SegmentRule {
            id: "DEFAULT".to_string(),
            load: "MAIN".to_string(),
            run: None,
            align: Some(1),
            start: None,
            offset: None,
            optional: false,
            segment: None,
        }],
        symbols: Vec::new(),
        output: OutputSpec::default(),
        entry: None,
    }
}

pub(super) fn select_segment_rule<'a>(
    config: &'a LinkerConfig,
    segment: &str,
) -> Result<&'a SegmentRule> {
    if let Some(rule) = config
        .segments
        .iter()
        .find(|rule| rule.matches_named_segment(segment))
    {
        return Ok(rule);
    }

    if let Some(rule) = config
        .segments
        .iter()
        .find(|rule| rule.segment_name().is_none())
    {
        return Ok(rule);
    }

    bail!("no segment rule found for segment '{segment}'");
}

impl SegmentRule {
    fn segment_name(&self) -> Option<&str> {
        self.segment.as_deref()
    }

    fn matches_named_segment(&self, segment: &str) -> bool {
        self.segment_name()
            .is_some_and(|name| name.eq_ignore_ascii_case(segment))
    }
}

pub(super) fn validate_segment_rules(config: &LinkerConfig) -> Result<()> {
    let mut seen_ids: HashMap<&str, usize> = HashMap::new();
    for (index, rule) in config.segments.iter().enumerate() {
        if rule.id.trim().is_empty() {
            bail!("segment rule id at index {index} must not be empty");
        }
        if let Some(previous) = seen_ids.insert(rule.id.as_str(), index) {
            bail!(
                "duplicate segment rule id '{}' at indices {} and {}",
                rule.id,
                previous,
                index
            );
        }
    }
    Ok(())
}
