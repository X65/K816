use crate::ast::{CodeBlock, File, HlaCompareOp, HlaStmt, Item, Stmt};
use crate::diag::Diagnostic;

pub(super) fn collect_parser_warnings(file: &File) -> Vec<Diagnostic> {
    let mut warnings = Vec::new();

    for item in &file.items {
        let Item::CodeBlock(block) = &item.node else {
            continue;
        };

        collect_hla_postfix_efficiency_warnings(block, &mut warnings);

        if block.name == "main" || !block.body.is_empty() {
            continue;
        }

        let span = block.name_span.unwrap_or(item.span);
        warnings.push(
            Diagnostic::warning(span, format!("empty function body '{}'", block.name))
                .with_help("add at least one statement or remove the function"),
        );
    }

    warnings
}

fn collect_hla_postfix_efficiency_warnings(block: &CodeBlock, warnings: &mut Vec<Diagnostic>) {
    for (index, stmt) in block.body.iter().enumerate() {
        let Stmt::Hla(HlaStmt::DoCloseWithOp { op }) = &stmt.node else {
            continue;
        };

        let has_condition_seed = index > 0
            && matches!(
                block.body[index - 1].node,
                Stmt::Hla(HlaStmt::ConditionSeed { .. })
            );
        if has_condition_seed {
            continue;
        }

        match op {
            HlaCompareOp::Le => warnings.push(
                Diagnostic::warning(
                    stmt.span,
                    "postfix `} <=` expands to two branch instructions and may be inefficient",
                )
                .with_help("expansion is `BCC target` + `BEQ target`"),
            ),
            HlaCompareOp::Gt => warnings.push(
                Diagnostic::warning(
                    stmt.span,
                    "postfix `} >` expands to three branch instructions and may be inefficient",
                )
                .with_help("expansion is `BEQ skip` + `BCC skip` + `BRA target`"),
            ),
            _ => {}
        }
    }
}
