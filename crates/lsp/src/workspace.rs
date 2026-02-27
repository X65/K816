use std::fs;
use std::path::PathBuf;

use anyhow::{Context, Result};
use lsp_types::{Diagnostic, Uri};

use super::project::{
    ProjectManifest, discover_workspace_sources, uri_from_file_path, uri_to_file_path,
};
use super::protocol::{
    QueryMemoryMapDetail, QueryMemoryMapMemory, QueryMemoryMapParams, QueryMemoryMapResult,
    QueryMemoryMapRun, QueryMemoryMapStatus, memory_kind_label,
};
use super::{
    ByteRange, DocumentState, LineIndex, PROJECT_MANIFEST, ServerState, SymbolLocation,
    SymbolOccurrence, analyze_document, canonical_symbol, diagnostic_to_lsp,
    extract_unknown_identifier_name,
};

impl ServerState {
    pub(super) fn initialize_workspace(&mut self) -> Result<()> {
        self.load_linker_config();
        let sources = discover_workspace_sources(&self.workspace_root)?;
        eprintln!(
            "k816-lsp: workspace '{}', discovered {} source file(s)",
            self.workspace_root.display(),
            sources.len(),
        );
        for path in sources {
            if let Err(error) = self.load_from_disk(path.clone()) {
                eprintln!("k816-lsp: skipping '{}': {error}", path.display());
            }
        }
        self.rebuild_symbol_index();
        eprintln!(
            "k816-lsp: indexed {} unique symbol(s) across {} document(s)",
            self.symbols.len(),
            self.documents.len(),
        );
        Ok(())
    }

    pub(super) fn load_linker_config(&mut self) {
        let manifest_path = self.workspace_root.join(PROJECT_MANIFEST);
        if manifest_path.is_file()
            && let Ok(text) = fs::read_to_string(&manifest_path)
            && let Ok(manifest) = toml::from_str::<ProjectManifest>(&text)
            && let Some(script) = manifest.link.script
        {
            let config_path = if script.is_absolute() {
                script
            } else {
                self.workspace_root.join(script)
            };
            if let Ok(config) = k816_link::load_config(&config_path) {
                eprintln!(
                    "k816-lsp: loaded linker config from '{}'",
                    config_path.display()
                );
                self.linker_config = config;
                return;
            }
        }

        let default_script = self.workspace_root.join("link.ron");
        if default_script.is_file()
            && let Ok(config) = k816_link::load_config(&default_script)
        {
            eprintln!(
                "k816-lsp: loaded linker config from '{}'",
                default_script.display()
            );
            self.linker_config = config;
            return;
        }

        eprintln!("k816-lsp: using default stub linker config");
    }

    pub(super) fn load_from_disk(&mut self, path: PathBuf) -> Result<()> {
        let text = fs::read_to_string(&path)
            .with_context(|| format!("failed to read source '{}'", path.display()))?;
        let uri = uri_from_file_path(&path)?;
        let file_id = self
            .source_index
            .ensure_file_id(uri.clone(), Some(path.clone()));
        let source_name = path.display().to_string();
        eprintln!("k816-lsp: analyzing '{source_name}'");
        let (analysis, object, addressable_sites) = analyze_document(&source_name, &text);
        self.documents.insert(
            uri.clone(),
            DocumentState {
                _file_id: file_id,
                uri,
                path: Some(path),
                version: 0,
                open: false,
                line_index: LineIndex::new(&text),
                text,
                analysis,
                object,
                addressable_sites,
                resolved_sites: Vec::new(),
            },
        );
        Ok(())
    }

    pub(super) fn upsert_document(
        &mut self,
        uri: Uri,
        text: String,
        version: i32,
        open: bool,
    ) -> Result<()> {
        self.upsert_document_without_analysis(uri.clone(), text, version, open);
        self.analyze_document(&uri);
        Ok(())
    }

    pub(super) fn upsert_document_without_analysis(
        &mut self,
        uri: Uri,
        text: String,
        version: i32,
        open: bool,
    ) {
        let path = uri_to_file_path(&uri);
        let file_id = self.source_index.ensure_file_id(uri.clone(), path.clone());
        let prev = self.documents.remove(&uri);
        let analysis = prev
            .as_ref()
            .map(|doc| doc.analysis.clone())
            .unwrap_or_default();
        let object = prev.as_ref().and_then(|doc| doc.object.clone());
        let addressable_sites = prev
            .as_ref()
            .map(|doc| doc.addressable_sites.clone())
            .unwrap_or_default();
        let resolved_sites = prev.map(|doc| doc.resolved_sites).unwrap_or_default();

        self.documents.insert(
            uri.clone(),
            DocumentState {
                _file_id: file_id,
                uri,
                path,
                version,
                open,
                line_index: LineIndex::new(&text),
                text,
                analysis,
                object,
                addressable_sites,
                resolved_sites,
            },
        );
    }

    pub(super) fn analyze_document(&mut self, uri: &Uri) {
        let Some(doc) = self.documents.get_mut(uri) else {
            return;
        };
        let source_name = doc
            .path
            .as_ref()
            .map(|path| path.display().to_string())
            .unwrap_or_else(|| doc.uri.to_string());
        eprintln!("k816-lsp: analyzing '{source_name}'");
        let (mut new_analysis, object, addressable_sites) =
            analyze_document(&source_name, &doc.text);
        if new_analysis.symbols.is_empty() && !doc.analysis.symbols.is_empty() {
            new_analysis.symbols = doc.analysis.symbols.clone();
            new_analysis.scopes = doc.analysis.scopes.clone();
        }
        doc.analysis = new_analysis;
        doc.object = object;
        doc.addressable_sites = addressable_sites;
        self.rebuild_symbol_index();
    }

    pub(super) fn close_document(&mut self, uri: &Uri) {
        if let Some(path) = self.documents.get(uri).and_then(|doc| doc.path.clone())
            && let Ok(text) = fs::read_to_string(&path)
        {
            let file_id = self
                .source_index
                .ensure_file_id(uri.clone(), Some(path.clone()));
            let (analysis, object, addressable_sites) =
                analyze_document(&path.display().to_string(), &text);
            self.documents.insert(
                uri.clone(),
                DocumentState {
                    _file_id: file_id,
                    uri: uri.clone(),
                    path: Some(path),
                    version: 0,
                    open: false,
                    line_index: LineIndex::new(&text),
                    text,
                    analysis,
                    object,
                    addressable_sites,
                    resolved_sites: Vec::new(),
                },
            );
            self.rebuild_symbol_index();
            return;
        }

        self.documents.remove(uri);
        self.rebuild_symbol_index();
    }

    pub(super) fn rebuild_symbol_index(&mut self) {
        self.symbols.clear();
        for doc in self.documents.values() {
            for symbol in &doc.analysis.symbols {
                self.symbols
                    .entry(symbol.canonical.clone())
                    .or_default()
                    .push(SymbolLocation {
                        uri: doc.uri.clone(),
                        name: symbol.name.clone(),
                        category: symbol.category,
                        selection: symbol.selection.clone(),
                    });
            }
        }
        self.rebuild_occurrence_index();
        self.try_link_workspace();
    }

    pub(super) fn rebuild_occurrence_index(&mut self) {
        self.symbol_occurrences.clear();

        for doc in self.documents.values() {
            for symbol in &doc.analysis.symbols {
                self.symbol_occurrences
                    .entry(symbol.canonical.clone())
                    .or_default()
                    .push(SymbolOccurrence {
                        uri: doc.uri.clone(),
                        range: symbol.selection.clone(),
                        is_declaration: true,
                    });
            }

            let (tokens, _) =
                k816_core::lexer::lex_lenient(k816_core::span::SourceId(0), &doc.text);
            for token in tokens {
                let k816_core::lexer::TokenKind::Ident(name) = token.kind else {
                    continue;
                };
                let scope = doc.analysis.scope_at_offset(token.span.start);
                let canonical = canonical_symbol(&name, scope);
                if !self.symbols.contains_key(&canonical) {
                    continue;
                }
                let token_range = ByteRange {
                    start: token.span.start,
                    end: token.span.end,
                };
                let already_decl = doc.analysis.symbols.iter().any(|symbol| {
                    symbol.canonical == canonical
                        && symbol.selection.start == token_range.start
                        && symbol.selection.end == token_range.end
                });
                if already_decl {
                    continue;
                }
                self.symbol_occurrences
                    .entry(canonical.clone())
                    .or_default()
                    .push(SymbolOccurrence {
                        uri: doc.uri.clone(),
                        range: token_range,
                        is_declaration: false,
                    });
            }
        }
    }

    pub(super) fn try_link_workspace(&mut self) {
        let doc_entries: Vec<(Uri, k816_o65::O65Object)> = self
            .documents
            .iter()
            .filter_map(|(uri, doc)| doc.object.as_ref().map(|obj| (uri.clone(), obj.clone())))
            .collect();

        if doc_entries.is_empty() {
            for doc in self.documents.values_mut() {
                doc.resolved_sites.clear();
            }
            self.last_link_layout = None;
            self.last_link_error = Some("no compiled objects are available to link".to_string());
            return;
        }

        let objects: Vec<k816_o65::O65Object> =
            doc_entries.iter().map(|(_, obj)| obj.clone()).collect();

        match k816_link::link_objects_with_options(
            &objects,
            &self.linker_config,
            k816_link::LinkRenderOptions::plain(),
        ) {
            Ok(output) => {
                self.last_link_layout = Some(output.clone());
                self.last_link_error = None;
                for (obj_idx, (uri, _)) in doc_entries.iter().enumerate() {
                    if let Some(doc) = self.documents.get_mut(uri) {
                        let mut sites = Vec::new();
                        for site in &doc.addressable_sites {
                            let key = (obj_idx, site.segment.clone());
                            if let Some(placements) = output.section_placements.get(&key)
                                && let Some(addr) =
                                    k816_link::resolve_symbol_addr(placements, site.offset)
                            {
                                sites.push((site.span, addr, site.size));
                            }
                        }
                        sites.sort_by_key(|(span, _, _)| span.start);
                        doc.resolved_sites = sites;
                    }
                }
            }
            Err(error) => {
                self.last_link_layout = None;
                self.last_link_error = Some(error.to_string());
                for doc in self.documents.values_mut() {
                    doc.resolved_sites.clear();
                }
            }
        }
    }

    pub(super) fn query_memory_map(&self, params: &QueryMemoryMapParams) -> QueryMemoryMapResult {
        let Some(layout) = self.last_link_layout.as_ref() else {
            return QueryMemoryMapResult {
                status: QueryMemoryMapStatus::Unavailable,
                reason: self
                    .last_link_error
                    .clone()
                    .or_else(|| Some("memory map is unavailable".to_string())),
                memories: Vec::new(),
                runs: Vec::new(),
            };
        };

        let filter = params
            .memory_name
            .as_deref()
            .map(str::trim)
            .filter(|name| !name.is_empty());

        let filtered_specs: Vec<&k816_link::MemoryArea> = self
            .linker_config
            .memory
            .iter()
            .filter(|memory| match filter {
                Some(name) => memory.name.eq_ignore_ascii_case(name),
                None => true,
            })
            .collect();

        if filtered_specs.is_empty() {
            return QueryMemoryMapResult {
                status: QueryMemoryMapStatus::Unavailable,
                reason: filter
                    .map(|name| format!("memory area '{name}' was not found in linker config")),
                memories: Vec::new(),
                runs: Vec::new(),
            };
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

            memories.push(QueryMemoryMapMemory {
                name: spec.name.clone(),
                start: spec.start,
                size: spec.size,
                kind: memory_kind_label(spec.kind).to_string(),
                used: used.min(u64::from(u32::MAX)) as u32,
                free: free.min(u64::from(u32::MAX)) as u32,
                utilization_percent,
            });
        }

        let runs = if params.detail == QueryMemoryMapDetail::Runs {
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
                rows.push(QueryMemoryMapRun {
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

        QueryMemoryMapResult {
            status: QueryMemoryMapStatus::Ok,
            reason: None,
            memories,
            runs,
        }
    }

    pub(super) fn lsp_diagnostics(&self, uri: &Uri) -> Vec<Diagnostic> {
        let Some(doc) = self.documents.get(uri) else {
            return Vec::new();
        };
        doc.analysis
            .diagnostics
            .iter()
            .filter(|diag| !self.is_cross_module_false_positive(diag))
            .map(|diag| diagnostic_to_lsp(diag, &doc.uri, &doc.line_index, &doc.text))
            .collect()
    }

    pub(super) fn is_cross_module_false_positive(
        &self,
        diag: &k816_core::diag::Diagnostic,
    ) -> bool {
        if diag.severity != k816_core::diag::Severity::Error {
            return false;
        }
        if let Some(name) = extract_unknown_identifier_name(&diag.message) {
            if self.symbols.contains_key(&name) {
                return true;
            }
            let suffix = format!("::{name}");
            return self.symbols.keys().any(|symbol| symbol.ends_with(&suffix));
        }
        false
    }
}
