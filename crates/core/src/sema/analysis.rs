use std::collections::HashSet;

use super::*;

pub fn analyze(file: &File) -> Result<SemanticModel, Vec<Diagnostic>> {
    analyze_with_external_consts(file, None)
}

pub(crate) fn analyze_with_external_consts(
    file: &File,
    external_consts: Option<&IndexMap<String, ConstMeta>>,
) -> Result<SemanticModel, Vec<Diagnostic>> {
    let (model, diagnostics) = analyze_partial(file, external_consts);
    if diagnostics.is_empty() {
        Ok(model)
    } else {
        Err(diagnostics)
    }
}

pub fn analyze_partial(
    file: &File,
    external_consts: Option<&IndexMap<String, ConstMeta>>,
) -> (SemanticModel, Vec<Diagnostic>) {
    let mut model = SemanticModel::default();
    let mut diagnostics = Vec::new();
    let mut next_auto_addr = 0_u32;
    let mut evaluator_context = EvalContext::default();

    let mut external_names = HashSet::new();
    if let Some(ext) = external_consts {
        for (name, meta) in ext {
            model.consts.insert(name.clone(), *meta);
            evaluator_context.set(name.clone(), meta.value);
            external_names.insert(name.clone());
        }
    }

    for item in &file.items {
        match &item.node {
            Item::Const(const_decl) => collect_const(
                const_decl,
                item.span,
                &mut model,
                &mut evaluator_context,
                &external_names,
                &mut diagnostics,
            ),
            Item::ConstGroup(consts) => {
                for const_decl in consts {
                    collect_const(
                        const_decl,
                        item.span,
                        &mut model,
                        &mut evaluator_context,
                        &external_names,
                        &mut diagnostics,
                    );
                }
            }
            Item::EvaluatorBlock(block) => collect_evaluator_block(
                block,
                item.span,
                &mut model,
                &mut evaluator_context,
                &external_names,
                &mut diagnostics,
            ),
            Item::Statement(Stmt::Var(var)) => collect_var(
                var,
                item.span,
                &mut next_auto_addr,
                &mut model,
                &external_names,
                &mut diagnostics,
            ),
            Item::Var(var) => collect_var(
                var,
                item.span,
                &mut next_auto_addr,
                &mut model,
                &external_names,
                &mut diagnostics,
            ),
            Item::CodeBlock(block) => {
                collect_function(
                    block,
                    item.span,
                    &file.mode_default,
                    &mut model,
                    &mut diagnostics,
                );
                for stmt in &block.body {
                    if let Stmt::Var(var) = &stmt.node {
                        collect_var(
                            var,
                            stmt.span,
                            &mut next_auto_addr,
                            &mut model,
                            &external_names,
                            &mut diagnostics,
                        );
                    }
                }
            }
            Item::NamedDataBlock(block) => {
                for entry in &block.entries {
                    if let NamedDataEntry::Evaluator(text) = &entry.node {
                        let eval_block = EvaluatorBlock { text: text.clone() };
                        collect_evaluator_block(
                            &eval_block,
                            entry.span,
                            &mut model,
                            &mut evaluator_context,
                            &external_names,
                            &mut diagnostics,
                        );
                    }
                }
                collect_named_data_block_array(block, &model.consts, &mut evaluator_context)
            }
            _ => {}
        }
    }

    (model, diagnostics)
}
