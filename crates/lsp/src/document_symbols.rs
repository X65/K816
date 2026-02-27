use lsp_types::DocumentSymbol;

use super::{ByteRange, LineIndex, SymbolCategory, byte_range_to_lsp};

pub(super) fn document_symbols_from_ast(
    file: &k816_core::ast::File,
    line_index: &LineIndex,
    text: &str,
) -> Vec<DocumentSymbol> {
    let mut symbols = Vec::new();
    for item in &file.items {
        match &item.node {
            k816_core::ast::Item::CodeBlock(block) => {
                let children = stmt_document_symbols(&block.body, line_index, text);
                let selection = block.name_span.unwrap_or(item.span);
                symbols.push(make_document_symbol(
                    block.name.clone(),
                    SymbolCategory::Function,
                    item.span,
                    selection,
                    if children.is_empty() {
                        None
                    } else {
                        Some(children)
                    },
                    line_index,
                    text,
                ));
            }
            k816_core::ast::Item::Var(var) => symbols.push(make_document_symbol(
                var.name.clone(),
                SymbolCategory::Variable,
                item.span,
                item.span,
                None,
                line_index,
                text,
            )),
            k816_core::ast::Item::Const(const_decl) => symbols.push(make_document_symbol(
                const_decl.name.clone(),
                SymbolCategory::Constant,
                item.span,
                item.span,
                None,
                line_index,
                text,
            )),
            k816_core::ast::Item::ConstGroup(consts) => {
                for const_decl in consts {
                    symbols.push(make_document_symbol(
                        const_decl.name.clone(),
                        SymbolCategory::Constant,
                        item.span,
                        item.span,
                        None,
                        line_index,
                        text,
                    ));
                }
            }
            k816_core::ast::Item::NamedDataBlock(block) => symbols.push(make_document_symbol(
                block.name.clone(),
                SymbolCategory::DataBlock,
                item.span,
                block.name_span,
                None,
                line_index,
                text,
            )),
            k816_core::ast::Item::Segment(segment) => symbols.push(make_document_symbol(
                segment.name.clone(),
                SymbolCategory::Segment,
                item.span,
                item.span,
                None,
                line_index,
                text,
            )),
            k816_core::ast::Item::Statement(stmt) => {
                if let Some(symbol) = stmt_to_document_symbol(stmt, item.span, line_index, text) {
                    symbols.push(symbol);
                }
            }
            k816_core::ast::Item::EvaluatorBlock(_) => {}
            k816_core::ast::Item::DataBlock(_) => {}
        }
    }
    symbols
}

fn stmt_document_symbols(
    statements: &[k816_core::span::Spanned<k816_core::ast::Stmt>],
    line_index: &LineIndex,
    text: &str,
) -> Vec<DocumentSymbol> {
    statements
        .iter()
        .filter_map(|stmt| stmt_to_document_symbol(&stmt.node, stmt.span, line_index, text))
        .collect()
}

fn stmt_to_document_symbol(
    stmt: &k816_core::ast::Stmt,
    span: k816_core::span::Span,
    line_index: &LineIndex,
    text: &str,
) -> Option<DocumentSymbol> {
    match stmt {
        k816_core::ast::Stmt::Label(label) => Some(make_document_symbol(
            label.name.clone(),
            SymbolCategory::Label,
            span,
            span,
            None,
            line_index,
            text,
        )),
        k816_core::ast::Stmt::Var(var) => Some(make_document_symbol(
            var.name.clone(),
            SymbolCategory::Variable,
            span,
            span,
            None,
            line_index,
            text,
        )),
        k816_core::ast::Stmt::ModeScopedBlock { body, .. } => {
            let children = stmt_document_symbols(body, line_index, text);
            Some(make_document_symbol(
                "@mode".to_string(),
                SymbolCategory::Segment,
                span,
                span,
                if children.is_empty() {
                    None
                } else {
                    Some(children)
                },
                line_index,
                text,
            ))
        }
        k816_core::ast::Stmt::Hla(k816_core::ast::HlaStmt::PrefixConditional { body, .. }) => {
            let children = stmt_document_symbols(body, line_index, text);
            Some(make_document_symbol(
                "prefix-conditional".to_string(),
                SymbolCategory::Segment,
                span,
                span,
                if children.is_empty() {
                    None
                } else {
                    Some(children)
                },
                line_index,
                text,
            ))
        }
        _ => None,
    }
}

#[allow(deprecated)]
fn make_document_symbol(
    name: String,
    category: SymbolCategory,
    range: k816_core::span::Span,
    selection: k816_core::span::Span,
    children: Option<Vec<DocumentSymbol>>,
    line_index: &LineIndex,
    text: &str,
) -> DocumentSymbol {
    DocumentSymbol {
        name,
        detail: Some(category.detail().to_string()),
        kind: category.symbol_kind(),
        tags: None,
        deprecated: None,
        range: byte_range_to_lsp(&ByteRange::from_span(range), line_index, text),
        selection_range: byte_range_to_lsp(&ByteRange::from_span(selection), line_index, text),
        children,
    }
}
