use std::collections::HashSet;

use lsp_types::{CompletionItemKind, SemanticTokenModifier, SemanticTokenType};

use super::{ByteRange, DocumentAnalysis, ScopeRange, SemanticInfo, SymbolCategory, SymbolDef};

pub(super) fn analyze_document(
    source_name: &str,
    source_text: &str,
    compile_result: Option<Result<&k816_core::CompileObjectOutput, &k816_core::CompileError>>,
    externals: Option<&k816_core::WorkspaceExternals>,
) -> (
    DocumentAnalysis,
    Option<k816_o65::O65Object>,
    Vec<k816_core::AddressableSite>,
) {
    let (mut diagnostics, compile_failed, object, addressable_sites, frontend) =
        if let Some(result) = compile_result {
            match result {
                Ok(output) => (
                    output.warnings.clone(),
                    false,
                    Some(output.object.clone()),
                    output.addressable_sites.clone(),
                    Some(output.frontend.clone()),
                ),
                Err(error) => (error.diagnostics.clone(), true, None, Vec::new(), None),
            }
        } else {
            match k816_core::compile_source(
                source_name,
                source_text,
                k816_core::CompileRenderOptions::plain(),
            ) {
                Ok(output) => (
                    output.warnings,
                    false,
                    Some(output.object),
                    output.addressable_sites,
                    Some(output.frontend),
                ),
                Err(error) => (error.diagnostics, true, None, Vec::new(), None),
            }
        };

    let mut source_map = k816_core::span::SourceMap::default();
    let source_id = source_map.add_source(source_name, source_text);

    let (symbols, scopes, semantic, ast) = if let Some(frontend) = frontend {
        let mut symbol_collection = collect_symbols(&frontend.parsed);
        let semantic = semantic_info_from_model(&frontend.semantic);
        filter_symbols_to_semantic_model(&mut symbol_collection, &semantic);
        (
            symbol_collection.symbols,
            symbol_collection.scopes,
            semantic,
            Some(frontend.parsed),
        )
    } else {
        let mut ast = None;
        let mut semantic = SemanticInfo::default();
        let external_function_names = externals.map(|e| &e.function_names);
        let (parsed_file, parse_diagnostics) = k816_core::parser::parse_lenient_and_externals(
            source_id,
            source_text,
            external_function_names,
        );
        if compile_failed {
            diagnostics.extend(parse_diagnostics);
        }

        let (symbols, scopes) = if let Some(parsed_file) = parsed_file {
            let mut symbol_collection = collect_symbols(&parsed_file);
            ast = Some(parsed_file.clone());

            if let Ok(expanded) = k816_core::eval_expand::expand_file(&parsed_file, source_id)
                && let Ok(normalized) = k816_core::normalize_hla::normalize_file(&expanded)
            {
                let analysis_externals = k816_core::sema::AnalysisExternals {
                    consts: externals.map(|e| &e.consts),
                    vars: externals.map(|e| &e.vars),
                };
                let (model, _sema_diagnostics) =
                    k816_core::sema::analyze_partial(&normalized, analysis_externals);
                for (name, meta) in model.functions {
                    semantic.functions.insert(name, meta);
                }
                for (name, meta) in model.consts {
                    semantic.consts.insert(name, meta);
                }
                for (name, meta) in model.vars {
                    semantic.vars.insert(name, meta);
                }
            }
            filter_symbols_to_semantic_model(&mut symbol_collection, &semantic);
            (symbol_collection.symbols, symbol_collection.scopes)
        } else {
            let fallback = scan_tokens_for_symbols(source_id, source_text);
            (fallback.symbols, fallback.scopes)
        };
        (symbols, scopes, semantic, ast)
    };

    super::dedup_diagnostics(&mut diagnostics);

    (
        DocumentAnalysis {
            diagnostics,
            symbols,
            scopes,
            semantic,
            ast,
        },
        object,
        addressable_sites,
    )
}

fn semantic_info_from_model(model: &k816_core::sema::SemanticModel) -> SemanticInfo {
    SemanticInfo {
        functions: model
            .functions
            .iter()
            .map(|(name, meta)| (name.clone(), meta.clone()))
            .collect(),
        consts: model
            .consts
            .iter()
            .map(|(name, meta)| (name.clone(), *meta))
            .collect(),
        vars: model
            .vars
            .iter()
            .map(|(name, meta)| (name.clone(), meta.clone()))
            .collect(),
    }
}

fn filter_symbols_to_semantic_model(symbols: &mut SymbolCollection, semantic: &SemanticInfo) {
    let valid_function_scopes = semantic
        .functions
        .keys()
        .map(String::as_str)
        .collect::<HashSet<_>>();
    let mut seen_scope_names = HashSet::new();
    let valid_scopes = symbols
        .scopes
        .iter()
        .filter(|scope| {
            valid_function_scopes.contains(scope.name.as_str())
                && seen_scope_names.insert(scope.name.clone())
        })
        .cloned()
        .collect::<Vec<_>>();
    let mut accepted_seen = HashSet::new();

    symbols.symbols.retain(|symbol| match symbol.category {
        SymbolCategory::Function => {
            semantic.functions.contains_key(&symbol.canonical)
                && accepted_seen.insert((symbol.category, symbol.canonical.clone()))
        }
        SymbolCategory::Constant => {
            semantic.consts.contains_key(&symbol.canonical)
                && accepted_seen.insert((symbol.category, symbol.canonical.clone()))
        }
        SymbolCategory::Variable => {
            semantic.vars.contains_key(&symbol.canonical)
                && accepted_seen.insert((symbol.category, symbol.canonical.clone()))
        }
        SymbolCategory::Label => symbol.scope.as_deref().is_none_or(|scope| {
            valid_scopes.iter().any(|valid| {
                valid.name == scope
                    && symbol.selection.start >= valid.range.start
                    && symbol.selection.end <= valid.range.end
            })
        }),
        SymbolCategory::DataBlock | SymbolCategory::Segment => true,
    });
    symbols.scopes = valid_scopes;
}

#[derive(Debug, Default)]
struct SymbolCollection {
    symbols: Vec<SymbolDef>,
    scopes: Vec<ScopeRange>,
}

fn collect_symbols(file: &k816_core::ast::File) -> SymbolCollection {
    let mut out = SymbolCollection::default();
    for item in &file.items {
        match &item.node {
            k816_core::ast::Item::CodeBlock(block) => {
                let range = ByteRange::from_span(item.span);
                let selection = block
                    .name_span
                    .map(ByteRange::from_span)
                    .unwrap_or_else(|| range.clone());
                out.symbols.push(SymbolDef {
                    canonical: block.name.clone(),
                    name: block.name.clone(),
                    category: SymbolCategory::Function,
                    selection,
                    scope: None,
                });
                out.scopes.push(ScopeRange {
                    name: block.name.clone(),
                    range,
                });
                for stmt in &block.body {
                    collect_stmt_symbols(
                        &stmt.node,
                        stmt.span,
                        Some(&block.name),
                        &mut out.symbols,
                    );
                }
            }
            k816_core::ast::Item::Var(var) => {
                let range = ByteRange::from_span(item.span);
                out.symbols.push(SymbolDef {
                    canonical: var.name.clone(),
                    name: var.name.clone(),
                    category: SymbolCategory::Variable,
                    selection: range,
                    scope: None,
                });
            }
            k816_core::ast::Item::Const(const_decl) => {
                let range = ByteRange::from_span(item.span);
                out.symbols.push(SymbolDef {
                    canonical: const_decl.name.clone(),
                    name: const_decl.name.clone(),
                    category: SymbolCategory::Constant,
                    selection: range,
                    scope: None,
                });
            }
            k816_core::ast::Item::ConstGroup(consts) => {
                let range = ByteRange::from_span(item.span);
                for const_decl in consts {
                    out.symbols.push(SymbolDef {
                        canonical: const_decl.name.clone(),
                        name: const_decl.name.clone(),
                        category: SymbolCategory::Constant,
                        selection: range.clone(),
                        scope: None,
                    });
                }
            }
            k816_core::ast::Item::Segment(segment) => {
                let range = ByteRange::from_span(item.span);
                out.symbols.push(SymbolDef {
                    canonical: segment.name.clone(),
                    name: segment.name.clone(),
                    category: SymbolCategory::Segment,
                    selection: range,
                    scope: None,
                });
            }
            k816_core::ast::Item::Statement(stmt) => {
                collect_stmt_symbols(stmt, item.span, None, &mut out.symbols);
            }
            k816_core::ast::Item::EvaluatorBlock(_) => {}
            k816_core::ast::Item::DataBlock(block) => {
                if let (Some(name), Some(name_span)) = (&block.name, block.name_span) {
                    out.symbols.push(SymbolDef {
                        canonical: name.clone(),
                        name: name.clone(),
                        category: SymbolCategory::DataBlock,
                        selection: ByteRange::from_span(name_span),
                        scope: None,
                    });
                }
            }
        }
    }
    out
}

fn collect_stmt_symbols(
    stmt: &k816_core::ast::Stmt,
    span: k816_core::span::Span,
    scope: Option<&str>,
    out: &mut Vec<SymbolDef>,
) {
    match stmt {
        k816_core::ast::Stmt::Label(label) => {
            let range = ByteRange::from_span(span);
            let canonical = canonical_symbol(&label.name, scope);
            out.push(SymbolDef {
                canonical,
                name: label.name.clone(),
                category: SymbolCategory::Label,
                selection: range,
                scope: scope.map(str::to_string),
            });
        }
        k816_core::ast::Stmt::Var(var) => {
            let range = ByteRange::from_span(span);
            out.push(SymbolDef {
                canonical: var.name.clone(),
                name: var.name.clone(),
                category: SymbolCategory::Variable,
                selection: range,
                scope: scope.map(str::to_string),
            });
        }
        k816_core::ast::Stmt::ModeScopedBlock { body, .. } => {
            for stmt in body {
                collect_stmt_symbols(&stmt.node, stmt.span, scope, out);
            }
        }
        k816_core::ast::Stmt::Hla(k816_core::ast::HlaStmt::PrefixConditional { body, .. }) => {
            for stmt in body {
                collect_stmt_symbols(&stmt.node, stmt.span, scope, out);
            }
        }
        _ => {}
    }
}

fn scan_tokens_for_symbols(
    source_id: k816_core::span::SourceId,
    source_text: &str,
) -> SymbolCollection {
    use k816_core::lexer::{TokenKind, lex_lenient};

    let (tokens, _) = lex_lenient(source_id, source_text);
    let mut out = SymbolCollection::default();

    let mut i = 0;
    while i < tokens.len() {
        let token = &tokens[i];
        match &token.kind {
            TokenKind::Func => {
                if let Some(name_token) = tokens.get(i + 1)
                    && let TokenKind::Ident(name) = &name_token.kind
                {
                    let selection = ByteRange::from_span(name_token.span);
                    let scope_range = find_brace_scope(&tokens, i + 2);
                    out.symbols.push(SymbolDef {
                        canonical: name.clone(),
                        name: name.clone(),
                        category: SymbolCategory::Function,
                        selection,
                        scope: None,
                    });
                    if let Some(scope_range) = scope_range {
                        out.scopes.push(ScopeRange {
                            name: name.clone(),
                            range: scope_range,
                        });
                    }
                    i += 2;
                    continue;
                }
            }
            TokenKind::Far | TokenKind::Naked | TokenKind::Inline => {
                let mut j = i + 1;
                while j < tokens.len() {
                    match &tokens[j].kind {
                        TokenKind::Far | TokenKind::Naked | TokenKind::Inline | TokenKind::Func => {
                            j += 1;
                        }
                        _ => break,
                    }
                }
                if let Some(name_token) = tokens.get(j)
                    && let TokenKind::Ident(name) = &name_token.kind
                {
                    let selection = ByteRange::from_span(name_token.span);
                    let scope_range = find_brace_scope(&tokens, j + 1);
                    out.symbols.push(SymbolDef {
                        canonical: name.clone(),
                        name: name.clone(),
                        category: SymbolCategory::Function,
                        selection,
                        scope: None,
                    });
                    if let Some(scope_range) = scope_range {
                        out.scopes.push(ScopeRange {
                            name: name.clone(),
                            range: scope_range,
                        });
                    }
                    i = j + 1;
                    continue;
                }
            }
            TokenKind::Var => {
                if let Some(name_token) = tokens.get(i + 1)
                    && let TokenKind::Ident(name) = &name_token.kind
                {
                    out.symbols.push(SymbolDef {
                        canonical: name.clone(),
                        name: name.clone(),
                        category: SymbolCategory::Variable,
                        selection: ByteRange::from_span(name_token.span),
                        scope: None,
                    });
                    i += 2;
                    continue;
                }
            }
            TokenKind::Const => {
                if let Some(name_token) = tokens.get(i + 1)
                    && let TokenKind::Ident(name) = &name_token.kind
                {
                    out.symbols.push(SymbolDef {
                        canonical: name.clone(),
                        name: name.clone(),
                        category: SymbolCategory::Constant,
                        selection: ByteRange::from_span(name_token.span),
                        scope: None,
                    });
                    i += 2;
                    continue;
                }
            }
            TokenKind::Data => {
                if let Some(name_token) = tokens.get(i + 1)
                    && let TokenKind::Ident(name) = &name_token.kind
                {
                    out.symbols.push(SymbolDef {
                        canonical: name.clone(),
                        name: name.clone(),
                        category: SymbolCategory::DataBlock,
                        selection: ByteRange::from_span(name_token.span),
                        scope: None,
                    });
                    i += 2;
                    continue;
                }
            }
            TokenKind::Segment => {
                if let Some(name_token) = tokens.get(i + 1)
                    && let TokenKind::Ident(name) = &name_token.kind
                {
                    out.symbols.push(SymbolDef {
                        canonical: name.clone(),
                        name: name.clone(),
                        category: SymbolCategory::Segment,
                        selection: ByteRange::from_span(name_token.span),
                        scope: None,
                    });
                    i += 2;
                    continue;
                }
            }
            _ => {}
        }
        i += 1;
    }
    out
}

fn find_brace_scope(tokens: &[k816_core::lexer::Token], start: usize) -> Option<ByteRange> {
    use k816_core::lexer::TokenKind;

    let mut j = start;
    while j < tokens.len() {
        match &tokens[j].kind {
            TokenKind::ModeA8
            | TokenKind::ModeA16
            | TokenKind::ModeI8
            | TokenKind::ModeI16
            | TokenKind::Newline => {
                j += 1;
            }
            _ => break,
        }
    }
    if j >= tokens.len() || tokens[j].kind != TokenKind::LBrace {
        return None;
    }
    let block_start = tokens[j].span.start;
    let mut depth = 1usize;
    j += 1;
    while j < tokens.len() && depth > 0 {
        match &tokens[j].kind {
            TokenKind::LBrace => depth += 1,
            TokenKind::RBrace => depth -= 1,
            _ => {}
        }
        j += 1;
    }
    let block_end = if j > 0 {
        tokens[j - 1].span.end
    } else {
        block_start
    };
    Some(ByteRange {
        start: block_start,
        end: block_end,
    })
}

pub(super) fn canonical_symbol(name: &str, scope: Option<&str>) -> String {
    if name.starts_with('.')
        && let Some(scope) = scope
    {
        return format!("{scope}::{name}");
    }
    name.to_string()
}

pub(super) fn completion_kind_for_symbol(category: SymbolCategory) -> CompletionItemKind {
    match category {
        SymbolCategory::Function => CompletionItemKind::FUNCTION,
        SymbolCategory::Constant => CompletionItemKind::CONSTANT,
        SymbolCategory::Variable => CompletionItemKind::VARIABLE,
        SymbolCategory::Label => CompletionItemKind::CONSTANT,
        SymbolCategory::DataBlock => CompletionItemKind::VALUE,
        SymbolCategory::Segment => CompletionItemKind::MODULE,
    }
}

pub(super) fn semantic_token_type_for_category(category: SymbolCategory) -> u32 {
    match category {
        SymbolCategory::Function => semantic_token_type_index("function"),
        SymbolCategory::Constant => semantic_token_type_index("constant"),
        SymbolCategory::Variable => semantic_token_type_index("variable"),
        SymbolCategory::Label => semantic_token_type_index("label"),
        SymbolCategory::DataBlock => semantic_token_type_index("data"),
        SymbolCategory::Segment => semantic_token_type_index("segment"),
    }
}

pub(super) fn semantic_token_type_index(name: &str) -> u32 {
    semantic_token_legend_type_names()
        .iter()
        .position(|entry| *entry == name)
        .unwrap_or(0) as u32
}

pub(super) fn semantic_token_modifier_bit(name: &str) -> u32 {
    let idx = semantic_token_legend_modifier_names()
        .iter()
        .position(|entry| *entry == name)
        .unwrap_or(0) as u32;
    1 << idx
}

pub(super) fn semantic_token_legend_types() -> Vec<SemanticTokenType> {
    semantic_token_legend_type_names()
        .iter()
        .map(|name| SemanticTokenType::new(name))
        .collect()
}

pub(super) fn semantic_token_legend_modifiers() -> Vec<SemanticTokenModifier> {
    semantic_token_legend_modifier_names()
        .iter()
        .map(|name| SemanticTokenModifier::new(name))
        .collect()
}

fn semantic_token_legend_type_names() -> &'static [&'static str] {
    &[
        "keyword",
        "function",
        "constant",
        "variable",
        "label",
        "segment",
        "data",
        "number",
        "operator",
        "parameter",
        "comment",
    ]
}

fn semantic_token_legend_modifier_names() -> &'static [&'static str] {
    &[
        "declaration",
        "local",
        "global",
        "resolved",
        "unresolved",
        "mode",
    ]
}
