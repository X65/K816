use anyhow::bail;

use super::{LinkedLayout, LinkerConfig, MemoryKind};

#[derive(Debug, Clone)]
pub struct MemoryUsage {
    pub memories: Vec<MemoryUsageMemory>,
    pub runs: Vec<MemoryUsageRun>,
}

#[derive(Debug, Clone)]
pub struct MemoryUsageMemory {
    pub name: String,
    pub start: u32,
    pub size: u32,
    pub kind: MemoryKind,
    pub used: u32,
    pub free: u32,
    pub utilization_percent: f64,
}

#[derive(Debug, Clone)]
pub struct MemoryUsageRun {
    pub memory_name: String,
    pub start: u32,
    pub end: u32,
    pub size: u32,
}

pub fn memory_usage(
    config: &LinkerConfig,
    layout: &LinkedLayout,
    memory_name: Option<&str>,
    include_runs: bool,
) -> anyhow::Result<MemoryUsage> {
    let filter = memory_name.map(str::trim).filter(|name| !name.is_empty());
    let filtered_specs = config
        .memory
        .iter()
        .filter(|memory| match filter {
            Some(name) => memory.name.eq_ignore_ascii_case(name),
            None => true,
        })
        .collect::<Vec<_>>();

    if filtered_specs.is_empty()
        && let Some(name) = filter
    {
        bail!("memory area '{name}' was not found in linker config");
    }

    let mut memories = Vec::with_capacity(filtered_specs.len());
    for spec in filtered_specs {
        let used = layout
            .runs
            .iter()
            .filter(|run| run.memory_name == spec.name)
            .fold(0u64, |acc, run| acc.saturating_add(run.bytes.len() as u64));
        let size = u64::from(spec.size);
        let free = size.saturating_sub(used);
        let utilization_percent = if size == 0 {
            0.0
        } else {
            ((used as f64) * 100.0) / (size as f64)
        };

        memories.push(MemoryUsageMemory {
            name: spec.name.clone(),
            start: spec.start,
            size: spec.size,
            kind: spec.kind,
            used: used.min(u64::from(u32::MAX)) as u32,
            free: free.min(u64::from(u32::MAX)) as u32,
            utilization_percent,
        });
    }

    let runs = if include_runs {
        let mut rows = Vec::new();
        for run in &layout.runs {
            if let Some(name) = filter
                && !run.memory_name.eq_ignore_ascii_case(name)
            {
                continue;
            }
            let size = run.bytes.len().min(u32::MAX as usize) as u32;
            let end = if size == 0 {
                run.start_addr
            } else {
                run.start_addr.saturating_add(size.saturating_sub(1))
            };
            rows.push(MemoryUsageRun {
                memory_name: run.memory_name.clone(),
                start: run.start_addr,
                end,
                size,
            });
        }
        rows.sort_by_key(|run| (run.memory_name.clone(), run.start));
        rows
    } else {
        Vec::new()
    };

    Ok(MemoryUsage { memories, runs })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{LinkedRun, MemoryArea, OutputKind, OutputSpec};

    fn config() -> LinkerConfig {
        LinkerConfig {
            format: "o65-link".to_string(),
            target: None,
            memory: vec![
                MemoryArea {
                    name: "ROM".to_string(),
                    start: 0x8000,
                    size: 0x100,
                    kind: MemoryKind::ReadOnly,
                    fill: None,
                },
                MemoryArea {
                    name: "RAM".to_string(),
                    start: 0,
                    size: 0,
                    kind: MemoryKind::ReadWrite,
                    fill: None,
                },
            ],
            segments: Vec::new(),
            symbols: Vec::new(),
            output: OutputSpec {
                kind: OutputKind::Xex,
                file: None,
            },
            entry: None,
        }
    }

    fn layout() -> LinkedLayout {
        LinkedLayout {
            runs: vec![
                LinkedRun {
                    memory_name: "ROM".to_string(),
                    start_addr: 0x8000,
                    bytes: vec![1, 2, 3],
                },
                LinkedRun {
                    memory_name: "ROM".to_string(),
                    start_addr: 0x8010,
                    bytes: vec![4],
                },
            ],
            listing: String::new(),
            symbols: Default::default(),
            section_placements: Default::default(),
        }
    }

    #[test]
    fn reports_summary_without_runs() {
        let usage = memory_usage(&config(), &layout(), None, false).expect("usage");
        assert_eq!(usage.memories.len(), 2);
        assert!(usage.runs.is_empty());
        assert_eq!(usage.memories[0].name, "ROM");
        assert_eq!(usage.memories[0].used, 4);
        assert_eq!(usage.memories[0].free, 0xFC);
        assert_eq!(usage.memories[1].name, "RAM");
        assert_eq!(usage.memories[1].utilization_percent, 0.0);
    }

    #[test]
    fn includes_run_details_when_requested() {
        let usage = memory_usage(&config(), &layout(), None, true).expect("usage");
        assert_eq!(usage.runs.len(), 2);
        assert_eq!(usage.runs[0].start, 0x8000);
        assert_eq!(usage.runs[0].end, 0x8002);
        assert_eq!(usage.runs[0].size, 3);
    }

    #[test]
    fn filters_by_memory_name_case_insensitively() {
        let usage = memory_usage(&config(), &layout(), Some("rom"), true).expect("usage");
        assert_eq!(usage.memories.len(), 1);
        assert_eq!(usage.memories[0].name, "ROM");
        assert_eq!(usage.runs.len(), 2);
    }

    #[test]
    fn reports_unknown_memory_filter() {
        let err =
            memory_usage(&config(), &layout(), Some("VRAM"), false).expect_err("unknown memory");
        assert!(
            err.to_string()
                .contains("memory area 'VRAM' was not found in linker config"),
            "unexpected error: {err:#}"
        );
    }
}
