use std::collections::HashMap;

use lsp_types::{
    GotoDefinitionResponse, Location, Position, PrepareRenameResponse, ReferenceContext, TextEdit,
    Uri, WorkspaceEdit,
};

use super::text::{TokenMatch, token_at_offset};
use super::{
    ByteRange, ServerState, SymbolCategory, byte_range_to_lsp, canonical_symbol,
    document_symbols::document_symbols_from_ast, is_valid_symbol_name,
};

impl ServerState {
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
        let definitions = self.symbols.get(&canonical)?;

        let mut locations = Vec::new();
        for definition in definitions {
            let Some(def_doc) = self.documents.get(&definition.uri) else {
                continue;
            };
            let range =
                byte_range_to_lsp(&definition.selection, &def_doc.line_index, &def_doc.text);
            locations.push(Location::new(definition.uri.clone(), range));
        }

        if locations.is_empty() {
            None
        } else {
            Some(GotoDefinitionResponse::Array(locations))
        }
    }

    pub(super) fn references(
        &self,
        uri: &Uri,
        position: Position,
        context: &ReferenceContext,
    ) -> Option<Vec<Location>> {
        let (canonical, _, _) = self.symbol_at_position(uri, position)?;
        let occurrences = self.symbol_occurrences.get(&canonical)?;

        let mut locations = Vec::new();
        for occurrence in occurrences {
            if occurrence.is_declaration && !context.include_declaration {
                continue;
            }
            let Some(doc) = self.documents.get(&occurrence.uri) else {
                continue;
            };
            let range = byte_range_to_lsp(&occurrence.range, &doc.line_index, &doc.text);
            locations.push(Location::new(occurrence.uri.clone(), range));
        }
        Some(locations)
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
        let doc = self.documents.get(uri)?;
        let range = byte_range_to_lsp(
            &ByteRange {
                start: token.start,
                end: token.end,
            },
            &doc.line_index,
            &doc.text,
        );
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
            let Some(doc) = self.documents.get(&occurrence.uri) else {
                continue;
            };
            let edit = TextEdit {
                range: byte_range_to_lsp(&occurrence.range, &doc.line_index, &doc.text),
                new_text: new_name.to_string(),
            };
            changes
                .entry(occurrence.uri.clone())
                .or_default()
                .push(edit);
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
}
