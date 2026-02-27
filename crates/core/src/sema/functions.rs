use super::*;

pub(super) fn collect_function(
    block: &CodeBlock,
    span: Span,
    module_default: &ModeContract,
    model: &mut SemanticModel,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if !is_symbol_available(&block.name, model) {
        diagnostics.push(
            Diagnostic::error(span, format!("duplicate symbol '{}'", block.name))
                .with_help("rename one of the blocks to keep symbols unique"),
        );
        return;
    }

    // Merge: function-level explicit annotations take priority, module default fills gaps.
    let merged_contract = ModeContract {
        a_width: block.mode_contract.a_width.or(module_default.a_width),
        i_width: block.mode_contract.i_width.or(module_default.i_width),
    };

    model.functions.insert(
        block.name.clone(),
        FunctionMeta {
            is_far: block.is_far,
            is_naked: block.is_naked,
            is_inline: block.is_inline,
            mode_contract: merged_contract,
        },
    );
}
