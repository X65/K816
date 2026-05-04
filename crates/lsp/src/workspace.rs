use std::fs;
use std::path::PathBuf;

use anyhow::{Context, Result};
use lsp_types::{Diagnostic, Uri};

use super::project::{
    CompilationUnit, discover_workspace_sources, partition_workspace_into_units, uri_from_file_path,
    uri_to_file_path,
};
use super::protocol::{
    QueryMemoryMapDetail, QueryMemoryMapMemory, QueryMemoryMapParams, QueryMemoryMapResult,
    QueryMemoryMapRun, QueryMemoryMapStatus, memory_kind_label,
};
use super::watcher::WorkspaceFsEvent;
use super::{
    ByteRange, DocumentAnalysis, DocumentState, ForeignSource, LineIndex, PROJECT_MANIFEST,
    ServerState, SymbolLocation, SymbolOccurrence, analyze_document, diagnostic_to_lsp,
};

impl ServerState {
    pub(super) fn initialize_workspace(&mut self) -> Result<()> {
        self.load_linker_config();
        let sources = discover_workspace_sources(&self.workspace_root)?;
        log::info!(
            "workspace '{}', discovered {} source file(s)",
            self.workspace_root.display(),
            sources.len(),
        );
        for path in sources {
            if let Err(error) = self.load_from_disk(path.clone()) {
                log::warn!("skipping '{}': {error}", path.display());
            }
        }
        self.analyze_all_documents();
        log::info!(
            "indexed {} unique symbol(s) across {} document(s)",
            self.symbols.len(),
            self.documents.len(),
        );
        Ok(())
    }

    pub(super) fn load_linker_config(&mut self) {
        let manifest_path = self.workspace_root.join(PROJECT_MANIFEST);
        if manifest_path.is_file()
            && let Ok(manifest) = k816_project::load_project_manifest(&self.workspace_root)
            && let Some(script) = manifest.link.script
        {
            let config_path = if script.is_absolute() {
                script
            } else {
                self.workspace_root.join(script)
            };
            if let Ok(config) = k816_link::load_config(&config_path) {
                log::info!("loaded linker config from '{}'", config_path.display());
                self.linker_config = config;
                return;
            }
        }

        let default_script = self.workspace_root.join("link.ron");
        if default_script.is_file()
            && let Ok(config) = k816_link::load_config(&default_script)
        {
            log::info!("loaded linker config from '{}'", default_script.display());
            self.linker_config = config;
            return;
        }

        log::info!("using default stub linker config");
    }

    pub(super) fn load_from_disk(&mut self, path: PathBuf) -> Result<()> {
        let text = fs::read_to_string(&path)
            .with_context(|| format!("failed to read source '{}'", path.display()))?;
        let uri = uri_from_file_path(&path)?;
        let file_id = self
            .source_index
            .ensure_file_id(uri.clone(), Some(path.clone()));
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
                analysis: DocumentAnalysis::default(),
                object: None,
                addressable_sites: Vec::new(),
                resolved_sites: Vec::new(),
                source_id_uris: Vec::new(),
                unit_id: None,
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
        self.upsert_document_without_analysis(uri, text, version, open);
        self.analyze_all_documents();
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
        let source_id_uris = prev
            .as_ref()
            .map(|doc| doc.source_id_uris.clone())
            .unwrap_or_default();
        let unit_id = prev.as_ref().and_then(|doc| doc.unit_id);
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
                source_id_uris,
                unit_id,
            },
        );
    }

    pub(super) fn analyze_all_documents(&mut self) {
        self.recompute_compilation_units();

        let total_sources: usize = self
            .compilation_units
            .iter()
            .map(|unit| unit.uris.len())
            .sum();
        log::info!(
            "compiling {} source(s) across {} unit(s)",
            total_sources,
            self.compilation_units.len(),
        );

        let units: Vec<CompilationUnit> = self.compilation_units.clone();
        for (unit_id, unit) in units.iter().enumerate() {
            self.analyze_unit(unit_id, unit);
        }

        self.rebuild_symbol_index();
    }

    /// Rebuild `compilation_units` from the current document set. Called from
    /// `analyze_all_documents` so a doc open/close/reload reshapes units before
    /// we recompile. A document the partition logic doesn't recognize (e.g. an
    /// open ad-hoc file outside `src/` or `tests/`) becomes its own singleton
    /// unit so it still gets analyzed.
    fn recompute_compilation_units(&mut self) {
        let manifest_path = self.workspace_root.join(PROJECT_MANIFEST);
        let has_manifest = manifest_path.is_file();

        let mut paths: Vec<PathBuf> = self
            .documents
            .values()
            .filter_map(|doc| doc.path.clone())
            .collect();
        paths.sort();
        paths.dedup();

        let mut units = partition_workspace_into_units(&self.workspace_root, has_manifest, &paths);

        // Any document whose path didn't make it into a unit (e.g. pathless
        // virtual URI, or a manifest project's `tests/` file the user opened
        // directly) gets a singleton unit so analysis still runs against it.
        let mut covered: std::collections::HashSet<Uri> = std::collections::HashSet::new();
        for unit in &units {
            for uri in &unit.uris {
                covered.insert(uri.clone());
            }
        }
        for uri in self.documents.keys() {
            if !covered.contains(uri) {
                units.push(CompilationUnit {
                    uris: vec![uri.clone()],
                });
            }
        }

        self.compilation_units = units;
    }

    /// Compile and analyze every document in a single compilation unit. Each
    /// unit is its own externals scope so unrelated programs (e.g. sibling
    /// fixture trees) cannot collide into ambiguous-symbol drops.
    fn analyze_unit(&mut self, unit_id: usize, unit: &CompilationUnit) {
        let doc_uris: Vec<Uri> = unit
            .uris
            .iter()
            .filter(|uri| self.documents.contains_key(uri))
            .cloned()
            .collect();
        if doc_uris.is_empty() {
            return;
        }
        let source_names: Vec<String> = doc_uris
            .iter()
            .map(|uri| {
                let doc = &self.documents[uri];
                doc.path
                    .as_ref()
                    .map(|path| path.display().to_string())
                    .unwrap_or_else(|| doc.uri.to_string())
            })
            .collect();
        let sources = k816_core::LinkCompileInput::from_pairs(
            doc_uris
                .iter()
                .zip(source_names.iter())
                .map(|(uri, name)| (name.as_str(), self.documents[uri].text.as_str())),
        );

        let externals = k816_core::collect_workspace_externals(&sources);
        let compile_results = k816_core::compile_sources_with_externals(
            &sources,
            &externals,
            k816_core::CompileRenderOptions::plain(),
        );

        for (i, uri) in doc_uris.iter().enumerate() {
            let doc = self.documents.get_mut(uri).unwrap();

            let compile_ref = compile_results[i].as_ref();
            let (mut new_analysis, object, addressable_sites) = analyze_document(
                &source_names[i],
                &doc.text,
                Some(compile_ref),
                Some(&externals),
            );
            if new_analysis.symbols.is_empty() && !doc.analysis.symbols.is_empty() {
                new_analysis.symbols = doc.analysis.symbols.clone();
                new_analysis.scopes = doc.analysis.scopes.clone();
            }
            doc.analysis = new_analysis;
            doc.object = object;
            doc.addressable_sites = addressable_sites;
            doc.source_id_uris = doc_uris.clone();
            doc.unit_id = Some(unit_id);
        }
    }

    /// Apply a batch of external filesystem events. Returns the URIs of open
    /// documents whose diagnostics should be re-published. Events for open
    /// documents are ignored — the editor owns the in-memory text.
    pub(super) fn apply_fs_events(&mut self, events: Vec<WorkspaceFsEvent>) -> Vec<Uri> {
        let mut dirty = false;
        for event in events {
            match event {
                WorkspaceFsEvent::Changed(path) => {
                    let Ok(uri) = uri_from_file_path(&path) else {
                        continue;
                    };
                    if self.documents.get(&uri).is_some_and(|doc| doc.open) {
                        continue;
                    }
                    if let Err(error) = self.load_from_disk(path.clone()) {
                        log::warn!("failed to reload '{}': {error}", path.display());
                        continue;
                    }
                    dirty = true;
                }
                WorkspaceFsEvent::Removed(path) => {
                    let Ok(uri) = uri_from_file_path(&path) else {
                        continue;
                    };
                    if self.documents.get(&uri).is_some_and(|doc| doc.open) {
                        continue;
                    }
                    if self.documents.remove(&uri).is_some() {
                        dirty = true;
                    }
                }
            }
        }

        if !dirty {
            return Vec::new();
        }

        self.analyze_all_documents();
        self.documents
            .iter()
            .filter_map(|(uri, doc)| if doc.open { Some(uri.clone()) } else { None })
            .collect()
    }

    pub(super) fn close_document(&mut self, uri: &Uri) {
        if let Some(path) = self.documents.get(uri).and_then(|doc| doc.path.clone())
            && let Ok(text) = fs::read_to_string(&path)
        {
            let file_id = self
                .source_index
                .ensure_file_id(uri.clone(), Some(path.clone()));
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
                    analysis: DocumentAnalysis::default(),
                    object: None,
                    addressable_sites: Vec::new(),
                    resolved_sites: Vec::new(),
                    source_id_uris: Vec::new(),
                    unit_id: None,
                },
            );
            self.analyze_all_documents();
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
                let canonical = doc.analysis.canonical_at_offset(&name, token.span.start);
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
        // Reset per-doc resolved sites; per-unit linking re-populates them.
        for doc in self.documents.values_mut() {
            doc.resolved_sites.clear();
        }
        self.last_link_layout = None;
        self.last_link_diagnostics_per_unit.clear();

        let units = self.compilation_units.clone();
        for (unit_id, unit) in units.iter().enumerate() {
            self.try_link_unit(unit_id, unit);
        }
    }

    fn try_link_unit(&mut self, unit_id: usize, unit: &CompilationUnit) {
        // Walk URIs in unit order so the (obj_idx → URI) mapping the linker
        // returns can be inverted via the same vector.
        let doc_entries: Vec<(Uri, k816_o65::O65Object)> = unit
            .uris
            .iter()
            .filter_map(|uri| {
                self.documents
                    .get(uri)
                    .and_then(|doc| doc.object.as_ref().map(|obj| (uri.clone(), obj.clone())))
            })
            .collect();

        if doc_entries.is_empty() {
            return;
        }

        let objects: Vec<k816_o65::O65Object> =
            doc_entries.iter().map(|(_, obj)| obj.clone()).collect();

        match k816_link::link_objects_diagnostics(&objects, &self.linker_config) {
            Ok(output) => {
                // The first successful unit's layout backs `query_memory_map`.
                // A future plan iteration can return per-unit results once a
                // user-facing way to identify the unit lands; today the LSP
                // exposes a single layout, so we keep the first-wins policy.
                if self.last_link_layout.is_none() {
                    self.last_link_layout = Some(output.clone());
                }
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
            Err(k816_link::LinkErrors(link_diags)) => {
                // Build a (path → URI) lookup so `LinkDiagnostic.anchor.file`
                // (the `source_name` the per-doc compile used, i.e. the file
                // path) maps back to its document for diagnostic placement.
                let path_to_uri: std::collections::HashMap<String, Uri> = unit
                    .uris
                    .iter()
                    .filter_map(|uri| {
                        self.documents.get(uri).map(|doc| {
                            let name = doc
                                .path
                                .as_ref()
                                .map(|path| path.display().to_string())
                                .unwrap_or_else(|| doc.uri.to_string());
                            (name, uri.clone())
                        })
                    })
                    .collect();

                for link_diag in &link_diags {
                    let Some(anchor) = link_diag.anchor.as_ref() else {
                        log::warn!(
                            "dropping anchorless link diagnostic: {}",
                            link_diag.message
                        );
                        continue;
                    };
                    let Some(target_uri) = path_to_uri.get(&anchor.file) else {
                        log::warn!(
                            "link diagnostic anchored at unknown file '{}'",
                            anchor.file
                        );
                        continue;
                    };
                    let Some(target_doc) = self.documents.get_mut(target_uri) else {
                        continue;
                    };

                    // Build a one-shot SourceMap over the target document so
                    // `link_diagnostic_to_diagnostic` can convert (line,col)
                    // into byte offsets on the LIVE in-memory text.
                    let mut local_map = k816_core::span::SourceMap::default();
                    let local_id = local_map.add_source(anchor.file.clone(), &target_doc.text);
                    let resolve = |path: &str| -> Option<k816_core::span::SourceId> {
                        if path == anchor.file { Some(local_id) } else { None }
                    };
                    if let Some(diag) = k816_core::link_diagnostic_to_diagnostic(
                        link_diag,
                        &local_map,
                        &resolve,
                    ) {
                        // Re-anchor to source_id 0 so it composes with the
                        // doc's own diagnostics (which use the per-doc map
                        // built inside `analyze_document`).
                        let primary = k816_core::span::Span::new(
                            k816_core::span::SourceId(0),
                            diag.primary.start,
                            diag.primary.end,
                        );
                        let mut rebound =
                            k816_core::diag::Diagnostic::error(primary, diag.message)
                                .with_primary_label(diag.primary_label);
                        for label in diag.labels {
                            let span = k816_core::span::Span::new(
                                k816_core::span::SourceId(0),
                                label.span.start,
                                label.span.end,
                            );
                            rebound = rebound.with_label(span, label.message);
                        }
                        for supp in diag.supplements {
                            match supp {
                                k816_core::diag::Supplemental::Help(help) => {
                                    rebound = rebound.with_help(help);
                                }
                                k816_core::diag::Supplemental::Note(note) => {
                                    rebound = rebound.with_note(note);
                                }
                                k816_core::diag::Supplemental::InlineOrigin { span, label } => {
                                    let span = k816_core::span::Span::new(
                                        k816_core::span::SourceId(0),
                                        span.start,
                                        span.end,
                                    );
                                    rebound = rebound.with_inline_origin(span, label);
                                }
                            }
                        }
                        target_doc.analysis.diagnostics.push(rebound);
                    }
                }

                self.last_link_diagnostics_per_unit
                    .insert(unit_id, link_diags);
            }
        }
    }

    /// Build a one-line reason string for `query_memory_map` when no link
    /// layout is available — points the caller at why the memory map is
    /// missing without forcing them to render the structured diagnostics.
    fn summarize_link_failure(&self) -> String {
        let total: usize = self
            .last_link_diagnostics_per_unit
            .values()
            .map(|diags| diags.len())
            .sum();
        if total == 0 {
            return "memory map is unavailable".to_string();
        }
        let first = self
            .last_link_diagnostics_per_unit
            .values()
            .flat_map(|diags| diags.iter())
            .next();
        match first {
            Some(diag) if total == 1 => {
                format!("link errors prevent memory map: {}", diag.message)
            }
            Some(diag) => format!(
                "link errors prevent memory map ({total} total): {}",
                diag.message
            ),
            None => "memory map is unavailable".to_string(),
        }
    }

    pub(super) fn query_memory_map(&self, params: &QueryMemoryMapParams) -> QueryMemoryMapResult {
        let Some(layout) = self.last_link_layout.as_ref() else {
            return QueryMemoryMapResult {
                status: QueryMemoryMapStatus::Unavailable,
                reason: Some(self.summarize_link_failure()),
                memories: Vec::new(),
                runs: Vec::new(),
            };
        };

        let include_runs = params.detail == QueryMemoryMapDetail::Runs;
        let usage = match k816_link::memory_usage(
            &self.linker_config,
            layout,
            params.memory_name.as_deref(),
            include_runs,
        ) {
            Ok(usage) => usage,
            Err(error) => {
                return QueryMemoryMapResult {
                    status: QueryMemoryMapStatus::Unavailable,
                    reason: Some(error.to_string()),
                    memories: Vec::new(),
                    runs: Vec::new(),
                };
            }
        };

        if usage.memories.is_empty() {
            return QueryMemoryMapResult {
                status: QueryMemoryMapStatus::Unavailable,
                reason: None,
                memories: Vec::new(),
                runs: Vec::new(),
            };
        }

        let memories = usage
            .memories
            .into_iter()
            .map(|memory| QueryMemoryMapMemory {
                name: memory.name,
                start: memory.start,
                size: memory.size,
                kind: memory_kind_label(memory.kind).to_string(),
                used: memory.used,
                free: memory.free,
                utilization_percent: memory.utilization_percent,
            })
            .collect();
        let runs = usage
            .runs
            .into_iter()
            .map(|run| QueryMemoryMapRun {
                memory_name: run.memory_name,
                start: run.start,
                end: run.end,
                size: run.size,
            })
            .collect();

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
        let foreign_sources: Vec<Option<ForeignSource<'_>>> = doc
            .source_id_uris
            .iter()
            .map(|src_uri| {
                self.documents.get(src_uri).map(|src_doc| ForeignSource {
                    uri: src_doc.uri.clone(),
                    line_index: &src_doc.line_index,
                    text: &src_doc.text,
                })
            })
            .collect();
        doc.analysis
            .diagnostics
            .iter()
            .map(|diag| {
                diagnostic_to_lsp(diag, &doc.uri, &doc.line_index, &doc.text, &foreign_sources)
            })
            .collect()
    }
}
