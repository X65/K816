use std::collections::{HashMap, HashSet};

use super::*;
use crate::ast::{HlaStmt, Instruction, NumFmt, Operand, OperandAddrMode};

pub fn analyze(file: &File) -> Result<SemanticModel, Vec<Diagnostic>> {
    analyze_with_externals(file, AnalysisExternals::default())
}

pub(crate) fn analyze_with_externals(
    file: &File,
    externals: AnalysisExternals<'_>,
) -> Result<SemanticModel, Vec<Diagnostic>> {
    let (model, diagnostics) = analyze_partial(file, externals);
    if diagnostics.is_empty() {
        Ok(model)
    } else {
        Err(diagnostics)
    }
}

pub fn analyze_partial(
    file: &File,
    externals: AnalysisExternals<'_>,
) -> (SemanticModel, Vec<Diagnostic>) {
    let mut model = SemanticModel::default();
    let mut diagnostics = Vec::new();
    // Per-segment bump cursors. Each segment starts at offset 0 and advances
    // as vars are collected within it. The lexically most recent
    // `Item::Segment` / `Stmt::Segment` directive sets `current_segment`.
    let mut cursors: HashMap<String, u32> = HashMap::new();
    let mut current_segment = crate::DEFAULT_SEGMENT.to_string();
    let mut evaluator_context = EvalContext::default();

    let mut external_names = HashSet::new();
    if let Some(ext) = externals.consts {
        for (name, meta) in ext {
            model.consts.insert(name.clone(), *meta);
            evaluator_context.set(name.clone(), meta.value);
            external_names.insert(name.clone());
        }
    }
    if let Some(ext) = externals.vars {
        for (name, meta) in ext {
            model.vars.insert(name.clone(), meta.clone());
            external_names.insert(name.clone());
        }
    }
    if let Some(ext) = externals.external_var_classes {
        for (name, class) in ext {
            // Skip names that are already a compile-time-resolvable var or
            // const in this unit; those take precedence and have addresses.
            if model.vars.contains_key(name) || model.consts.contains_key(name) {
                continue;
            }
            model
                .external_var_classes
                .insert(name.clone(), class.clone());
            external_names.insert(name.clone());
        }
    }

    for item in &file.items {
        match &item.node {
            Item::Const(consts) => {
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
                &mut cursors,
                &current_segment,
                &mut model,
                &external_names,
                &mut diagnostics,
            ),
            Item::Var(var) => collect_var(
                var,
                item.span,
                &mut cursors,
                &current_segment,
                &mut model,
                &external_names,
                &mut diagnostics,
            ),
            Item::Segment(decl) => {
                current_segment = decl.name.clone();
            }
            Item::Statement(Stmt::Segment(decl)) => {
                current_segment = decl.name.clone();
            }
            Item::CodeBlock(block) => {
                collect_function(
                    block,
                    item.span,
                    &file.mode_default,
                    &mut model,
                    &mut diagnostics,
                );
                // Track segment shifts inside the function body so any nested
                // `var` declarations land in the right segment cursor. The
                // outer `current_segment` is not perturbed — function-body
                // segment directives are scoped to the body for var emission.
                let mut body_segment = current_segment.clone();
                for stmt in &block.body {
                    match &stmt.node {
                        Stmt::Segment(decl) => {
                            body_segment = decl.name.clone();
                        }
                        Stmt::Var(var) => {
                            collect_var(
                                var,
                                stmt.span,
                                &mut cursors,
                                &body_segment,
                                &mut model,
                                &external_names,
                                &mut diagnostics,
                            );
                        }
                        _ => {}
                    }
                }
            }
            Item::DataBlock(block) => {
                collect_data_block_labels(
                    block,
                    item.span,
                    &mut model,
                    &external_names,
                    &mut diagnostics,
                );
                for entry in &block.entries {
                    if let DataEntry::Evaluator(text) = &entry.node {
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
                let ctx = ConstEvalCtx::new(&model.consts, &model.vars);
                collect_data_block_array(block, &ctx, &mut evaluator_context)
            }
            _ => {}
        }
    }

    // Second pass: walk function bodies for call-site validation that needs
    // the fully-built `model.functions` map. Catches `lsrx 3` (instruction-form
    // call to a known function with a bare operand) early, before lowering —
    // so the LSP surfaces it even when other parts of the file fail to compile
    // and the strict `compile_source_inner` pipeline never reaches `emit`.
    for item in &file.items {
        if let Item::CodeBlock(block) = &item.node {
            for stmt in &block.body {
                check_function_call_misuse(&stmt.node, stmt.span, &model, &mut diagnostics);
            }
        }
    }

    (model, diagnostics)
}

/// Walk one statement (recursively, for the wrappers that nest other
/// statements) looking for `Stmt::Instruction(name, Some(operand))` whose
/// `name` resolves to a declared function. That shape can never encode a CPU
/// instruction (the encoder would just say `unknown mnemonic`), and the user
/// almost certainly forgot the `#` prefix on an immediate or the parens of
/// the explicit call form.
fn check_function_call_misuse(
    stmt: &Stmt,
    span: Span,
    model: &SemanticModel,
    diagnostics: &mut Vec<Diagnostic>,
) {
    match stmt {
        Stmt::Instruction(Instruction {
            mnemonic,
            operand: Some(operand),
        }) => {
            let Some(meta) = model.functions.get(mnemonic) else {
                return;
            };
            let kind_phrase = format!("{}function", meta.kind_prefix_with_article());
            let mut diag = Diagnostic::error(
                span,
                format!(
                    "'{mnemonic}' is {kind_phrase} and cannot be invoked as a CPU instruction with this operand"
                ),
            )
            .with_primary_label("expected a call form".to_string());
            if let Some(value) = bare_immediate_hint(operand) {
                diag = diag.with_help(format!(
                    "did you mean `{mnemonic} #{value}`? — pass {value} as an immediate, or use the explicit call form `{sig}`",
                    sig = meta.signature_call_form(mnemonic),
                ));
            } else {
                diag = diag.with_help(format!(
                    "call it as `{sig}`, or `{mnemonic} #imm[, ...]` for the instruction-style call form",
                    sig = meta.signature_call_form(mnemonic),
                ));
            }
            diag = diag.with_note(
                "K65 distinguishes between CPU mnemonics and user-defined functions/inline macros. \
                 Inline macros declared with `#name` parameters require `#` at every call site, \
                 mirroring 65816 instruction syntax (e.g. `lda #5` vs `lda 5`).".to_string(),
            );
            diagnostics.push(diag);
        }
        Stmt::ModeScopedBlock { body, .. } => {
            for stmt in body {
                check_function_call_misuse(&stmt.node, stmt.span, model, diagnostics);
            }
        }
        Stmt::Hla(HlaStmt::PrefixConditional { body, .. }) => {
            for stmt in body {
                check_function_call_misuse(&stmt.node, stmt.span, model, diagnostics);
            }
        }
        _ => {}
    }
}

/// Extract a render-ready value string when `operand` is a bare numeric
/// literal in `Direct` mode without any addressing prefix or index — the
/// shape produced by `mnemonic 3`. Used so the diagnostic can suggest the
/// exact `mnemonic #3` immediate replacement.
fn bare_immediate_hint(operand: &Operand) -> Option<String> {
    let Operand::Value {
        expr,
        addr_mode_override: None,
        index: None,
        addr_mode: OperandAddrMode::Direct,
    } = operand
    else {
        return None;
    };
    match expr {
        Expr::Number(value, NumFmt::Hex(_)) => Some(format!("0x{value:X}")),
        Expr::Number(value, _) => Some(value.to_string()),
        _ => None,
    }
}
