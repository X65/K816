use std::collections::BTreeSet;

use lsp_types::Uri;

use super::protocol::{
    ResolveAddressesResult, ResolveInlineSymbol, ResolveInlineSymbolsParams,
    ResolveInlineSymbolsResult, symbol_category_label,
};
use super::text::token_matches_in_range;
use super::{ServerState, SymbolCategory, SymbolLocation, canonical_symbol};

impl ServerState {
    pub(super) fn resolve_addresses_for_lines(
        &self,
        uri: &Uri,
        lines: &[u32],
    ) -> ResolveAddressesResult {
        let Some(doc) = self.documents.get(uri) else {
            return ResolveAddressesResult {
                addresses: lines.iter().map(|_| None).collect(),
            };
        };
        let addresses = lines
            .iter()
            .map(|&line| {
                let line = line as usize;
                if line >= doc.line_index.line_starts.len() {
                    return None;
                }
                let line_start = doc.line_index.line_starts[line];
                let line_end = if line + 1 < doc.line_index.line_starts.len() {
                    doc.line_index.line_starts[line + 1]
                } else {
                    doc.text.len()
                };
                let idx = doc
                    .resolved_sites
                    .partition_point(|(span, _, _)| span.start < line_start);
                if idx < doc.resolved_sites.len() {
                    let (span, addr, _) = &doc.resolved_sites[idx];
                    if span.start < line_end {
                        return Some(*addr);
                    }
                }
                None
            })
            .collect();
        ResolveAddressesResult { addresses }
    }

    pub(super) fn resolve_inline_symbols(
        &self,
        uri: &Uri,
        params: &ResolveInlineSymbolsParams,
    ) -> ResolveInlineSymbolsResult {
        let Some(doc) = self.documents.get(uri) else {
            return ResolveInlineSymbolsResult {
                symbols: Vec::new(),
            };
        };
        if doc.line_index.line_starts.is_empty() {
            return ResolveInlineSymbolsResult {
                symbols: Vec::new(),
            };
        }

        let line_count = doc.line_index.line_starts.len();
        let mut start_line = params.start_line.min(params.end_line) as usize;
        let mut end_line = params.start_line.max(params.end_line) as usize;
        if start_line >= line_count {
            return ResolveInlineSymbolsResult {
                symbols: Vec::new(),
            };
        }
        end_line = end_line.min(line_count.saturating_sub(1));
        start_line = start_line.min(end_line);

        let start_offset = doc.line_index.line_starts[start_line];
        let end_offset = if end_line + 1 < line_count {
            doc.line_index.line_starts[end_line + 1]
        } else {
            doc.text.len()
        };

        let mut seen = BTreeSet::new();
        let mut symbols = Vec::new();

        for token in token_matches_in_range(&doc.text, start_offset, end_offset) {
            let scope = doc.analysis.scope_at_offset(token.start);
            let canonical = canonical_symbol(&token.text, scope);
            let Some(definition) = self.symbols.get(&canonical).and_then(|defs| defs.first())
            else {
                continue;
            };
            let Some((address, read_size_hint)) =
                self.inline_symbol_address(definition, &canonical)
            else {
                continue;
            };

            if !seen.insert((token.start, token.end, canonical.clone())) {
                continue;
            }

            let start = doc.line_index.to_position(&doc.text, token.start);
            let end = doc.line_index.to_position(&doc.text, token.end);

            symbols.push(ResolveInlineSymbol {
                name: token.text,
                category: symbol_category_label(definition.category).to_string(),
                start_line: start.line,
                start_character: start.character,
                end_line: end.line,
                end_character: end.character,
                address,
                read_size_hint,
            });
        }

        symbols.sort_by_key(|row| (row.start_line, row.start_character, row.name.clone()));
        ResolveInlineSymbolsResult { symbols }
    }

    pub(super) fn inline_symbol_address(
        &self,
        symbol: &SymbolLocation,
        canonical: &str,
    ) -> Option<(u32, Option<u32>)> {
        let doc = self.documents.get(&symbol.uri)?;

        if let Some(meta) = doc.analysis.semantic.vars.get(canonical) {
            let read_size_hint = match meta.size {
                1 | 2 => Some(meta.size),
                _ => None,
            };
            return Some((meta.address, read_size_hint));
        }

        if let Some((addr, _)) = doc.address_at_offset(symbol.selection.start) {
            return Some((addr, None));
        }

        if let Some((_, addr, _)) = doc.resolved_sites.iter().find(|(span, _, _)| {
            span.start >= symbol.selection.start && span.start < symbol.selection.end
        }) {
            return Some((*addr, None));
        }

        match symbol.category {
            SymbolCategory::Label => {
                let idx = doc
                    .resolved_sites
                    .partition_point(|(span, _, _)| span.start < symbol.selection.start);
                doc.resolved_sites
                    .get(idx)
                    .map(|(_, addr, _)| (*addr, None))
            }
            SymbolCategory::Function => {
                let scope = doc
                    .analysis
                    .scopes
                    .iter()
                    .find(|scope| scope.name == symbol.name)?;
                doc.resolved_sites
                    .iter()
                    .find(|(span, _, _)| {
                        span.start >= scope.range.start && span.start < scope.range.end
                    })
                    .map(|(_, addr, _)| (*addr, None))
            }
            _ => None,
        }
    }
}
