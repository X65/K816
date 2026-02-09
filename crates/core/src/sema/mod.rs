use indexmap::IndexMap;

use crate::ast::{CodeBlock, File, Item};
use crate::diag::Diagnostic;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FunctionMeta {
    pub is_far: bool,
    pub is_naked: bool,
    pub is_inline: bool,
}

#[derive(Debug, Clone, Default)]
pub struct SemanticModel {
    pub functions: IndexMap<String, FunctionMeta>,
}

pub fn analyze(file: &File) -> Result<SemanticModel, Vec<Diagnostic>> {
    let mut model = SemanticModel::default();
    let mut diagnostics = Vec::new();

    for item in &file.items {
        if let Item::CodeBlock(block) = &item.node {
            collect_function(block, item.span, &mut model, &mut diagnostics);
        }
    }

    if diagnostics.is_empty() {
        Ok(model)
    } else {
        Err(diagnostics)
    }
}

fn collect_function(
    block: &CodeBlock,
    span: crate::span::Span,
    model: &mut SemanticModel,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if let Some(existing) = model.functions.get(&block.name) {
        let _ = existing;
        diagnostics.push(
            Diagnostic::error(span, format!("duplicate code block name '{}'", block.name))
                .with_hint("rename one of the blocks to keep symbols unique"),
        );
        return;
    }

    model.functions.insert(
        block.name.clone(),
        FunctionMeta {
            is_far: block.is_far,
            is_naked: block.is_naked,
            is_inline: block.is_inline,
        },
    );
}
