use std::collections::HashMap;

use lsp_types::{
    GotoDefinitionResponse, Location, Position, PrepareRenameResponse, Range, ReferenceContext,
    TextEdit, Uri, WorkspaceEdit,
};

use super::text::{
    QualifiedSegment, TokenMatch, cumulative_field_key, resolve_field_from_ast,
    resolve_qualified_segment, token_at_offset,
};
use super::{
    ByteRange, ServerState, SymbolCategory, byte_range_to_lsp, canonical_symbol,
    document_symbols::document_symbols_from_ast, is_valid_symbol_name,
};

impl ServerState {
    /// Convert a byte range living in `uri`'s document to an LSP `Range`,
    /// looking up the document's `LineIndex` and text in one step. Returns
    /// `None` if the URI is not tracked.
    pub(super) fn convert_doc_range(&self, uri: &Uri, range: &ByteRange) -> Option<Range> {
        let doc = self.documents.get(uri)?;
        Some(byte_range_to_lsp(range, &doc.line_index, &doc.text))
    }

    /// Build LSP `Location`s for every definition of `canonical` recorded in
    /// the cross-file symbol index. Skips definitions whose owning document is
    /// no longer tracked.
    pub(super) fn definition_locations(&self, canonical: &str) -> Vec<Location> {
        let Some(definitions) = self.symbols.get(canonical) else {
            return Vec::new();
        };
        definitions
            .iter()
            .filter_map(|definition| {
                let range = self.convert_doc_range(&definition.uri, &definition.selection)?;
                Some(Location::new(definition.uri.clone(), range))
            })
            .collect()
    }

    /// Build LSP `Location`s for every recorded occurrence of `canonical`,
    /// optionally filtering out declaration sites (used for "find references"
    /// when the editor doesn't want declarations included).
    pub(super) fn occurrence_locations(
        &self,
        canonical: &str,
        include_declaration: bool,
    ) -> Vec<Location> {
        let Some(occurrences) = self.symbol_occurrences.get(canonical) else {
            return Vec::new();
        };
        occurrences
            .iter()
            .filter(|occurrence| include_declaration || !occurrence.is_declaration)
            .filter_map(|occurrence| {
                let range = self.convert_doc_range(&occurrence.uri, &occurrence.range)?;
                Some(Location::new(occurrence.uri.clone(), range))
            })
            .collect()
    }

    pub(super) fn definition(
        &self,
        uri: &Uri,
        position: Position,
    ) -> Option<GotoDefinitionResponse> {
        let doc = self.documents.get(uri)?;
        let offset = doc.line_index.to_offset(&doc.text, position)?;
        let token = token_at_offset(&doc.text, offset)?;
        let scope = doc.analysis.scope_at_offset(offset);
        let canonical = canonical_symbol(&token.text, scope);

        let locations = self.definition_locations(&canonical);
        if !locations.is_empty() {
            return Some(GotoDefinitionResponse::Array(locations));
        }

        // Standalone subscript field (e.g. `.from` inside brackets): resolve
        // via AST to get the full qualified path (var + field key).
        if token.text.starts_with('.')
            && let Some(resolved) = doc
                .analysis
                .ast
                .as_ref()
                .and_then(|ast| resolve_field_from_ast(ast, offset))
        {
            let segment_name = &token.text[1..];
            let field_key = cumulative_field_key(&resolved.field_key, segment_name)
                .unwrap_or(&resolved.field_key);
            return self.definition_for_subscript_field(Some(&resolved.var_name), field_key);
        }

        // Qualified subscript field access: VAR.field1.field2... (position-aware per segment)
        if let Some(seg) = resolve_qualified_segment(&token.text, token.start, offset) {
            match seg {
                QualifiedSegment::Var { name, .. } => {
                    let var_canonical = canonical_symbol(name, scope);
                    let locations = self.definition_locations(&var_canonical);
                    if !locations.is_empty() {
                        return Some(GotoDefinitionResponse::Array(locations));
                    }
                }
                QualifiedSegment::Field {
                    var_name,
                    field_key,
                    ..
                } => {
                    return self.definition_for_subscript_field(Some(var_name), field_key);
                }
            }
        }

        None
    }

    pub(super) fn references(
        &self,
        uri: &Uri,
        position: Position,
        context: &ReferenceContext,
    ) -> Option<Vec<Location>> {
        let (canonical, _, _) = self.symbol_at_position(uri, position)?;
        Some(self.occurrence_locations(&canonical, context.include_declaration))
    }

    pub(super) fn prepare_rename(
        &self,
        uri: &Uri,
        position: Position,
    ) -> Option<PrepareRenameResponse> {
        let (canonical, token, category) = self.symbol_at_position(uri, position)?;
        if matches!(
            category,
            SymbolCategory::Segment | SymbolCategory::DataBlock
        ) && canonical.is_empty()
        {
            return None;
        }
        let range = self.convert_doc_range(
            uri,
            &ByteRange {
                start: token.start,
                end: token.end,
            },
        )?;
        Some(PrepareRenameResponse::Range(range))
    }

    #[allow(clippy::mutable_key_type)]
    pub(super) fn rename(
        &self,
        uri: &Uri,
        position: Position,
        new_name: &str,
    ) -> Option<WorkspaceEdit> {
        let (canonical, token, _category) = self.symbol_at_position(uri, position)?;
        let new_name = new_name.trim();
        if new_name.is_empty() {
            return None;
        }
        if !is_valid_symbol_name(new_name) {
            return None;
        }
        if token.text.starts_with('.') && !new_name.starts_with('.') {
            return None;
        }
        if !token.text.starts_with('.') && new_name.starts_with('.') {
            return None;
        }

        let mut changes: HashMap<Uri, Vec<TextEdit>> = HashMap::new();
        for occurrence in self.symbol_occurrences.get(&canonical)? {
            let Some(range) = self.convert_doc_range(&occurrence.uri, &occurrence.range) else {
                continue;
            };
            changes
                .entry(occurrence.uri.clone())
                .or_default()
                .push(TextEdit {
                    range,
                    new_text: new_name.to_string(),
                });
        }

        if changes.is_empty() {
            return None;
        }

        Some(WorkspaceEdit {
            changes: Some(changes),
            document_changes: None,
            change_annotations: None,
        })
    }

    pub(super) fn symbol_at_position(
        &self,
        uri: &Uri,
        position: Position,
    ) -> Option<(String, TokenMatch, SymbolCategory)> {
        let doc = self.documents.get(uri)?;
        let offset = doc.line_index.to_offset(&doc.text, position)?;
        let token = token_at_offset(&doc.text, offset)?;
        let scope = doc.analysis.scope_at_offset(offset);
        let canonical = canonical_symbol(&token.text, scope);
        let category = self
            .symbols
            .get(&canonical)
            .and_then(|defs| defs.first())
            .map(|def| def.category)?;
        Some((canonical, token, category))
    }

    pub(super) fn document_symbols(&self, uri: &Uri) -> Option<lsp_types::DocumentSymbolResponse> {
        let doc = self.documents.get(uri)?;
        let ast = doc.analysis.ast.as_ref()?;
        let symbols = document_symbols_from_ast(ast, &doc.line_index, &doc.text);
        Some(lsp_types::DocumentSymbolResponse::Nested(symbols))
    }

    fn definition_for_subscript_field(
        &self,
        var_name: Option<&str>,
        field_key: &str,
    ) -> Option<GotoDefinitionResponse> {
        for (doc_uri, doc_state) in &self.documents {
            let Some(ast) = doc_state.analysis.ast.as_ref() else {
                continue;
            };
            for item in &ast.items {
                let var = match &item.node {
                    k816_core::ast::Item::Var(v) => v,
                    _ => continue,
                };
                // If var_name is given (qualified access), match exactly; otherwise search all vars.
                if let Some(name) = var_name
                    && var.name != name
                {
                    continue;
                }
                if let Some(fields) = &var.symbolic_subscript_fields
                    && let Some(span) = find_field_span(fields, field_key, "")
                    && let Some(range) =
                        self.convert_doc_range(doc_uri, &ByteRange::from_span(span))
                {
                    return Some(GotoDefinitionResponse::Array(vec![Location::new(
                        doc_uri.clone(),
                        range,
                    )]));
                }
            }
        }
        None
    }
}

/// Recursively find a field declaration span by field key (e.g. "mode" or "nested.child").
fn find_field_span(
    fields: &[k816_core::ast::SymbolicSubscriptFieldDecl],
    key: &str,
    prefix: &str,
) -> Option<k816_core::span::Span> {
    for field in fields {
        let qualified = if prefix.is_empty() {
            field.name.clone()
        } else {
            format!("{prefix}.{}", field.name)
        };
        if qualified == key {
            return Some(field.span);
        }
        if let Some(nested) = &field.nested_fields
            && let Some(span) = find_field_span(nested, key, &qualified)
        {
            return Some(span);
        }
    }
    None
}
