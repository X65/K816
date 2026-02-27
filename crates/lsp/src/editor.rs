use std::collections::BTreeSet;

use lsp_types::{
    CodeLens, Command, CompletionItem, CompletionItemKind, CompletionItemLabelDetails,
    CompletionResponse, FoldingRange, FoldingRangeKind, Hover, HoverContents, InlayHint,
    InlayHintKind, InlayHintLabel, InsertTextFormat, MarkupContent, MarkupKind, Position, Range,
    SemanticTokens, SignatureHelp, SignatureInformation, TextEdit, Uri,
};

use super::hover::{
    builtin_hover_text, directive_keywords, evaluator_signature, format_address,
    format_address_range, hover_contents_for_numeric_literal, hover_contents_for_symbol,
    is_register_name, opcode_keywords, register_keywords,
};
use super::text::{
    evaluator_call_at_offset, in_symbol_completion_context, numeric_literal_at_offset,
    token_at_offset, token_prefix_at_offset,
};
use super::{
    ByteRange, INSTRUCTION_DESCRIPTIONS, ServerState, SymbolCategory, byte_range_to_lsp,
    canonical_symbol, completion_kind_for_symbol, semantic_token_modifier_bit,
    semantic_token_type_for_category, semantic_token_type_index,
};

impl ServerState {
    pub(super) fn hover(&self, uri: &Uri, position: Position) -> Option<Hover> {
        let doc = self.documents.get(uri)?;
        let offset = doc.line_index.to_offset(&doc.text, position)?;

        if let Some((range, value)) = numeric_literal_at_offset(&doc.text, offset) {
            return Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: hover_contents_for_numeric_literal(value),
                }),
                range: Some(byte_range_to_lsp(&range, &doc.line_index, &doc.text)),
            });
        }

        if let Some(token) = token_at_offset(&doc.text, offset) {
            let token_range = byte_range_to_lsp(
                &ByteRange {
                    start: token.start,
                    end: token.end,
                },
                &doc.line_index,
                &doc.text,
            );

            let scope = doc.analysis.scope_at_offset(offset);
            let canonical = canonical_symbol(&token.text, scope);
            if let Some(definitions) = self.symbols.get(&canonical)
                && let Some(definition) = definitions.first()
            {
                let contents = hover_contents_for_symbol(&canonical, definition, self);
                return Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: contents,
                    }),
                    range: Some(token_range),
                });
            }

            if let Some(mut text) = builtin_hover_text(&token.text) {
                if let Some((addr, size)) = doc.address_at_offset(offset) {
                    text.push_str(&format!(
                        "\n- address: `{}`",
                        format_address_range(addr, size)
                    ));
                }
                return Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: text,
                    }),
                    range: Some(token_range),
                });
            }

            if let Some((addr, size)) = doc.address_at_offset(offset) {
                return Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: format!("address: `{}`", format_address_range(addr, size)),
                    }),
                    range: Some(token_range),
                });
            }
        }

        doc.address_at_offset(offset).map(|(addr, size)| Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: format!("address: `{}`", format_address_range(addr, size)),
            }),
            range: None,
        })
    }

    pub(super) fn completion(&self, uri: &Uri, position: Position) -> Option<CompletionResponse> {
        let doc = self.documents.get(uri)?;
        let offset = doc.line_index.to_offset(&doc.text, position)?;
        let prefix = token_prefix_at_offset(&doc.text, offset).to_ascii_lowercase();
        let scope = doc.analysis.scope_at_offset(offset);
        let allow_symbol_completions = in_symbol_completion_context(&doc.text, offset);

        let mut candidates: Vec<(String, CompletionItemKind, &'static str, Option<String>)> =
            opcode_keywords()
                .into_iter()
                .map(|mnemonic| {
                    let doc = INSTRUCTION_DESCRIPTIONS
                        .get(&mnemonic.to_ascii_uppercase())
                        .cloned();
                    (mnemonic, CompletionItemKind::KEYWORD, "opcode", doc)
                })
                .collect();
        candidates.extend(directive_keywords().iter().map(|directive| {
            (
                (*directive).to_string(),
                CompletionItemKind::KEYWORD,
                "directive",
                None,
            )
        }));
        candidates.extend(register_keywords().iter().map(|(name, doc)| {
            (
                (*name).to_string(),
                CompletionItemKind::VARIABLE,
                "register",
                Some((*doc).to_string()),
            )
        }));

        if allow_symbol_completions {
            let mut visible_symbols: BTreeSet<(String, SymbolCategory)> = BTreeSet::new();
            for entries in self.symbols.values() {
                for entry in entries {
                    if entry.name.starts_with('.') {
                        continue;
                    }
                    visible_symbols.insert((entry.name.clone(), entry.category));
                }
            }

            if let Some(scope) = scope {
                for symbol in &doc.analysis.symbols {
                    if symbol.name.starts_with('.') && symbol.scope.as_deref() == Some(scope) {
                        visible_symbols.insert((symbol.name.clone(), symbol.category));
                    }
                }
            }

            candidates.extend(visible_symbols.into_iter().map(|(name, category)| {
                (
                    name,
                    completion_kind_for_symbol(category),
                    category.detail(),
                    None,
                )
            }));
        }

        let mut seen = BTreeSet::new();
        let items = candidates
            .into_iter()
            .filter(|(label, _, _, _)| {
                if prefix.is_empty() {
                    return true;
                }
                label.to_ascii_lowercase().starts_with(&prefix)
            })
            .filter(|(label, _, _, _)| seen.insert(label.clone()))
            .map(|(label, kind, detail, documentation)| CompletionItem {
                label,
                kind: Some(kind),
                detail: Some(detail.to_string()),
                insert_text_format: Some(InsertTextFormat::PLAIN_TEXT),
                label_details: Some(CompletionItemLabelDetails {
                    detail: None,
                    description: Some(detail.to_string()),
                }),
                documentation: documentation.map(|text| {
                    lsp_types::Documentation::MarkupContent(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: text,
                    })
                }),
                ..CompletionItem::default()
            })
            .collect::<Vec<_>>();

        Some(CompletionResponse::Array(items))
    }

    pub(super) fn formatting(&self, uri: &Uri) -> Vec<TextEdit> {
        let Some(doc) = self.documents.get(uri) else {
            return Vec::new();
        };

        let formatted = k816_fmt::format_source(&doc.text);
        if formatted == doc.text {
            return Vec::new();
        }

        let end = doc.line_index.to_position(&doc.text, doc.text.len());
        vec![TextEdit {
            range: Range {
                start: Position {
                    line: 0,
                    character: 0,
                },
                end,
            },
            new_text: formatted,
        }]
    }

    pub(super) fn semantic_tokens(&self, uri: &Uri) -> Option<SemanticTokens> {
        let doc = self.documents.get(uri)?;
        let (tokens, _) = k816_core::lexer::lex_lenient(k816_core::span::SourceId(0), &doc.text);
        let mut semantic_rows = Vec::new();

        for token in tokens {
            let span = ByteRange::from_span(token.span);
            if span.start >= span.end {
                continue;
            }
            let start = doc.line_index.to_position(&doc.text, span.start);
            let end = doc.line_index.to_position(&doc.text, span.end);
            if start.line != end.line {
                continue;
            }
            let len = end.character.saturating_sub(start.character);
            if len == 0 {
                continue;
            }

            let mut modifiers = 0u32;
            let token_type = match &token.kind {
                k816_core::lexer::TokenKind::LineComment(_)
                | k816_core::lexer::TokenKind::BlockComment(_) => {
                    semantic_token_type_index("comment")
                }
                k816_core::lexer::TokenKind::Number(_) => semantic_token_type_index("number"),
                k816_core::lexer::TokenKind::ModeA8
                | k816_core::lexer::TokenKind::ModeA16
                | k816_core::lexer::TokenKind::ModeI8
                | k816_core::lexer::TokenKind::ModeI16 => {
                    modifiers |= semantic_token_modifier_bit("mode");
                    semantic_token_type_index("keyword")
                }
                k816_core::lexer::TokenKind::Hash
                | k816_core::lexer::TokenKind::Eq
                | k816_core::lexer::TokenKind::EqEq
                | k816_core::lexer::TokenKind::BangEq
                | k816_core::lexer::TokenKind::Lt
                | k816_core::lexer::TokenKind::LtEq
                | k816_core::lexer::TokenKind::Gt
                | k816_core::lexer::TokenKind::GtEq
                | k816_core::lexer::TokenKind::Plus
                | k816_core::lexer::TokenKind::Minus
                | k816_core::lexer::TokenKind::Star
                | k816_core::lexer::TokenKind::Percent
                | k816_core::lexer::TokenKind::Amp
                | k816_core::lexer::TokenKind::Pipe
                | k816_core::lexer::TokenKind::Caret
                | k816_core::lexer::TokenKind::Question
                | k816_core::lexer::TokenKind::Bang => semantic_token_type_index("operator"),
                k816_core::lexer::TokenKind::Ident(name) => {
                    let lower = name.to_ascii_lowercase();
                    if directive_keywords().iter().any(|d| *d == lower) {
                        semantic_token_type_index("keyword")
                    } else if is_register_name(&lower) {
                        semantic_token_type_index("parameter")
                    } else if opcode_keywords().iter().any(|op| op == &lower) {
                        semantic_token_type_index("keyword")
                    } else {
                        let scope = doc.analysis.scope_at_offset(span.start);
                        let canonical = canonical_symbol(name, scope);
                        if let Some(defs) = self.symbols.get(&canonical) {
                            modifiers |= semantic_token_modifier_bit("resolved");
                            if name.starts_with('.') {
                                modifiers |= semantic_token_modifier_bit("local");
                            } else {
                                modifiers |= semantic_token_modifier_bit("global");
                            }
                            let is_decl = doc.analysis.symbols.iter().any(|symbol| {
                                symbol.canonical == canonical
                                    && symbol.selection.start == span.start
                                    && symbol.selection.end == span.end
                            });
                            if is_decl {
                                modifiers |= semantic_token_modifier_bit("declaration");
                            }
                            semantic_token_type_for_category(
                                defs.first()
                                    .map(|d| d.category)
                                    .unwrap_or(SymbolCategory::Label),
                            )
                        } else {
                            modifiers |= semantic_token_modifier_bit("unresolved");
                            semantic_token_type_index("variable")
                        }
                    }
                }
                _ => continue,
            };

            semantic_rows.push((start, len, token_type, modifiers));
        }

        semantic_rows.sort_by_key(|(start, _, _, _)| (start.line, start.character));
        let mut data = Vec::with_capacity(semantic_rows.len());
        let mut prev_line = 0u32;
        let mut prev_start = 0u32;
        for (start, len, token_type, modifiers) in semantic_rows {
            let delta_line = start.line.saturating_sub(prev_line);
            let delta_start = if delta_line == 0 {
                start.character.saturating_sub(prev_start)
            } else {
                start.character
            };
            data.push(lsp_types::SemanticToken {
                delta_line,
                delta_start,
                length: len,
                token_type,
                token_modifiers_bitset: modifiers,
            });
            prev_line = start.line;
            prev_start = start.character;
        }

        Some(SemanticTokens {
            result_id: None,
            data,
        })
    }

    pub(super) fn inlay_hints(&self, uri: &Uri, range: Range) -> Option<Vec<InlayHint>> {
        let doc = self.documents.get(uri)?;
        let start_offset = doc.line_index.to_offset(&doc.text, range.start)?;
        let end_offset = doc.line_index.to_offset(&doc.text, range.end)?;
        let mut hints = Vec::new();

        for symbol in &doc.analysis.symbols {
            if !matches!(
                symbol.category,
                SymbolCategory::Function | SymbolCategory::Label
            ) {
                continue;
            }
            if symbol.selection.end < start_offset || symbol.selection.start > end_offset {
                continue;
            }
            if let Some((addr, _size)) = doc.address_at_offset(symbol.selection.start) {
                let pos = doc.line_index.to_position(&doc.text, symbol.selection.end);
                hints.push(InlayHint {
                    position: pos,
                    label: InlayHintLabel::String(format!(" = {}", format_address(addr))),
                    kind: Some(InlayHintKind::TYPE),
                    text_edits: None,
                    tooltip: None,
                    padding_left: Some(true),
                    padding_right: Some(false),
                    data: None,
                });
            }
        }

        for (span, _addr, size) in &doc.resolved_sites {
            if *size == 0 {
                continue;
            }
            if span.end < start_offset || span.start > end_offset {
                continue;
            }
            let pos = doc.line_index.to_position(&doc.text, span.end);
            hints.push(InlayHint {
                position: pos,
                label: InlayHintLabel::String(format!(" {}b", size)),
                kind: Some(InlayHintKind::PARAMETER),
                text_edits: None,
                tooltip: None,
                padding_left: Some(true),
                padding_right: Some(false),
                data: None,
            });
        }

        for symbol in &doc.analysis.symbols {
            if symbol.category != SymbolCategory::Function {
                continue;
            }
            if symbol.selection.end < start_offset || symbol.selection.start > end_offset {
                continue;
            }
            let Some(meta) = doc.analysis.semantic.functions.get(&symbol.canonical) else {
                continue;
            };
            let mut parts = Vec::new();
            if let Some(width) = meta.mode_contract.a_width {
                parts.push(format!(
                    "a{}",
                    if width == k816_core::ast::RegWidth::W16 {
                        "16"
                    } else {
                        "8"
                    }
                ));
            }
            if let Some(width) = meta.mode_contract.i_width {
                parts.push(format!(
                    "i{}",
                    if width == k816_core::ast::RegWidth::W16 {
                        "16"
                    } else {
                        "8"
                    }
                ));
            }
            if parts.is_empty() {
                continue;
            }
            let pos = doc.line_index.to_position(&doc.text, symbol.selection.end);
            hints.push(InlayHint {
                position: pos,
                label: InlayHintLabel::String(format!(" [{}]", parts.join(","))),
                kind: Some(InlayHintKind::TYPE),
                text_edits: None,
                tooltip: None,
                padding_left: Some(true),
                padding_right: Some(false),
                data: None,
            });
        }

        Some(hints)
    }

    pub(super) fn code_lenses(&self, uri: &Uri) -> Option<Vec<CodeLens>> {
        let doc = self.documents.get(uri)?;
        let mut lenses = Vec::new();

        for symbol in &doc.analysis.symbols {
            if matches!(
                symbol.category,
                SymbolCategory::Function | SymbolCategory::Label
            ) {
                let usage_count = self
                    .symbol_occurrences
                    .get(&symbol.canonical)
                    .map(|occ| occ.iter().filter(|o| !o.is_declaration).count())
                    .unwrap_or(0);
                let pos = doc
                    .line_index
                    .to_position(&doc.text, symbol.selection.start);
                let range = Range {
                    start: pos,
                    end: pos,
                };
                lenses.push(CodeLens {
                    range,
                    command: Some(Command {
                        title: if usage_count == 1 {
                            "1 reference".to_string()
                        } else {
                            format!("{usage_count} references")
                        },
                        command: "k816.showCodeLensInfo".to_string(),
                        arguments: Some(vec![serde_json::Value::String(symbol.canonical.clone())]),
                    }),
                    data: None,
                });
            }

            if matches!(
                symbol.category,
                SymbolCategory::Function | SymbolCategory::Segment
            ) {
                let addr = if symbol.category == SymbolCategory::Function {
                    doc.address_at_offset(symbol.selection.start)
                        .map(|(addr, _)| addr)
                } else {
                    doc.resolved_sites
                        .iter()
                        .find(|(span, _, _)| span.start >= symbol.selection.start)
                        .map(|(_, addr, _)| *addr)
                };
                if let Some(addr) = addr {
                    let pos = doc
                        .line_index
                        .to_position(&doc.text, symbol.selection.start);
                    let range = Range {
                        start: pos,
                        end: pos,
                    };
                    lenses.push(CodeLens {
                        range,
                        command: Some(Command {
                            title: format!("address {}", format_address(addr)),
                            command: "k816.showCodeLensInfo".to_string(),
                            arguments: Some(vec![serde_json::Value::String(
                                symbol.canonical.clone(),
                            )]),
                        }),
                        data: None,
                    });
                }
            }
        }

        Some(lenses)
    }

    pub(super) fn folding_ranges(&self, uri: &Uri) -> Option<Vec<FoldingRange>> {
        let doc = self.documents.get(uri)?;
        let (tokens, _) = k816_core::lexer::lex_lenient(k816_core::span::SourceId(0), &doc.text);
        let mut stack = Vec::new();
        let mut ranges = Vec::new();

        for token in tokens {
            match token.kind {
                k816_core::lexer::TokenKind::LBrace => stack.push(token.span.start),
                k816_core::lexer::TokenKind::RBrace => {
                    let Some(start_offset) = stack.pop() else {
                        continue;
                    };
                    let start = doc.line_index.to_position(&doc.text, start_offset);
                    let end = doc.line_index.to_position(&doc.text, token.span.end);
                    if end.line <= start.line {
                        continue;
                    }
                    ranges.push(FoldingRange {
                        start_line: start.line,
                        start_character: None,
                        end_line: end.line.saturating_sub(1),
                        end_character: None,
                        kind: Some(FoldingRangeKind::Region),
                        collapsed_text: None,
                    });
                }
                _ => {}
            }
        }

        ranges.sort_by_key(|range| (range.start_line, range.end_line));
        ranges.dedup_by_key(|range| (range.start_line, range.end_line));
        Some(ranges)
    }

    pub(super) fn signature_help(&self, uri: &Uri, position: Position) -> Option<SignatureHelp> {
        let doc = self.documents.get(uri)?;
        let offset = doc.line_index.to_offset(&doc.text, position)?;
        let (function_name, active_param) = evaluator_call_at_offset(&doc.text, offset)?;
        let signature = evaluator_signature(function_name.as_str())?;

        Some(SignatureHelp {
            signatures: vec![SignatureInformation {
                label: signature.label.to_string(),
                documentation: Some(lsp_types::Documentation::MarkupContent(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: signature.documentation.to_string(),
                })),
                parameters: Some(
                    signature
                        .parameters
                        .iter()
                        .map(|param| lsp_types::ParameterInformation {
                            label: lsp_types::ParameterLabel::Simple((*param).to_string()),
                            documentation: None,
                        })
                        .collect(),
                ),
                active_parameter: None,
            }],
            active_signature: Some(0),
            active_parameter: Some(
                active_param.min(signature.parameters.len().saturating_sub(1) as u32),
            ),
        })
    }
}
