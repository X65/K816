use super::*;
use crate::ast::{ContractParam, HlaStmt};
use crate::span::Spanned;
use k816_isa65816::is_mnemonic;
use rustc_hash::FxHashMap;
use std::collections::HashSet;

const RESERVED_HLA_HEADS: &[&str] = &[
    "always", "break", "call", "continue", "far", "flag", "for", "goto", "if", "image", "loop",
    "never", "repeat", "return", "return_i", "segment", "var", "while",
];

fn is_reserved_name(name: &str) -> bool {
    is_mnemonic(name)
        || RESERVED_HLA_HEADS
            .iter()
            .any(|keyword| keyword.eq_ignore_ascii_case(name))
}

fn collect_label_names(stmts: &[Spanned<Stmt>], labels: &mut FxHashMap<String, String>) {
    for stmt in stmts {
        match &stmt.node {
            Stmt::Label(label) => {
                labels
                    .entry(label.name.clone())
                    .or_insert_with(|| label.name.clone());
                if let Some(local) = label.name.strip_prefix('.') {
                    labels
                        .entry(local.to_string())
                        .or_insert_with(|| label.name.clone());
                }
            }
            Stmt::ModeScopedBlock { body, .. } => collect_label_names(body, labels),
            Stmt::Hla(HlaStmt::NeverBlock { body }) => collect_label_names(body, labels),
            Stmt::Hla(HlaStmt::PrefixConditional {
                body, else_body, ..
            }) => {
                collect_label_names(body, labels);
                if let Some(else_body) = else_body {
                    collect_label_names(else_body, labels);
                }
            }
            _ => {}
        }
    }
}

fn validate_inline_param_names(
    block: &CodeBlock,
    model: &SemanticModel,
    diagnostics: &mut Vec<Diagnostic>,
    span: Span,
) {
    let mut seen = HashSet::new();
    let mut labels = FxHashMap::default();
    collect_label_names(&block.body, &mut labels);

    for param in &block.params {
        let (param_name, display_name, allows_var_shadow) = match param {
            ContractParam::Register(reg) => {
                let name = match reg {
                    crate::ast::RegName::A => "a",
                    crate::ast::RegName::X => "x",
                    crate::ast::RegName::Y => "y",
                };
                (name.to_string(), name.to_string(), false)
            }
            ContractParam::Immediate(param) => {
                (param.name.clone(), format!("#{}", param.name), false)
            }
            ContractParam::Alias(name) => (name.clone(), name.clone(), true),
        };

        if !seen.insert(param_name.clone()) {
            diagnostics.push(
                Diagnostic::error(
                    span,
                    format!(
                        "inline contract parameter '{display_name}' is declared more than once"
                    ),
                )
                .with_help(
                    "rename the duplicate parameter so every contract input has a unique name",
                ),
            );
            continue;
        }

        if block.name == param_name || model.functions.contains_key(&param_name) {
            diagnostics.push(
                Diagnostic::error(
                    span,
                    format!("inline contract parameter '{display_name}' may not shadow a function"),
                )
                .with_help("rename the parameter to avoid colliding with a function name"),
            );
            continue;
        }

        if let Some(label_name) = labels.get(&param_name) {
            diagnostics.push(
                Diagnostic::error(
                    span,
                    format!(
                        "inline contract parameter '{display_name}' may not shadow label '{label_name}'"
                    ),
                )
                .with_help("rename the parameter or the label so both remain distinct"),
            );
            continue;
        }

        if !allows_var_shadow && model.vars.contains_key(&param_name) {
            diagnostics.push(
                Diagnostic::error(
                    span,
                    format!("inline immediate parameter '{display_name}' may not shadow a variable"),
                )
                .with_help("rename the parameter or use an alias parameter if you intend to shadow a variable"),
            );
        }
    }
}

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

    if is_reserved_name(&block.name) {
        diagnostics.push(
            Diagnostic::error(span, format!("function name '{}' is reserved", block.name))
                .with_help("choose a name that does not collide with a mnemonic or HLA keyword"),
        );
        return;
    }

    if block.is_naked && (!block.outputs.is_empty() || block.exit_contract.is_some()) {
        diagnostics.push(
            Diagnostic::error(
                span,
                format!(
                    "naked function '{}' may not declare an exit contract",
                    block.name
                ),
            )
            .with_help(
                "remove the `-> ...` clause; control does not return to the caller implicitly from a naked function",
            ),
        );
        return;
    }

    if !block.is_inline {
        for param in &block.params {
            match param {
                ContractParam::Register(_) => {}
                ContractParam::Immediate(param) => diagnostics.push(
                    Diagnostic::error(
                        span,
                        format!(
                            "function '{}' uses inline-only immediate parameter '#{}'",
                            block.name, param.name
                        ),
                    )
                    .with_help(
                        "move this contract to an 'inline' function or use register parameters",
                    ),
                ),
                ContractParam::Alias(name) => diagnostics.push(
                    Diagnostic::error(
                        span,
                        format!(
                            "function '{}' uses inline-only variable alias parameter '{}'",
                            block.name, name
                        ),
                    )
                    .with_help(
                        "move this contract to an 'inline' function or use register parameters",
                    ),
                ),
            }
        }
    } else {
        validate_inline_param_names(block, model, diagnostics, span);
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
            has_contract: block.has_contract,
            params: block.params.clone(),
            outputs: block.outputs.clone(),
            mode_contract: merged_contract,
            exit_contract: block.exit_contract.map(|exit| ModeContract {
                a_width: exit.a_width.or(merged_contract.a_width),
                i_width: exit.i_width.or(merged_contract.i_width),
            }),
        },
    );
}
