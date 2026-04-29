//! Exit-mode summarisation for code blocks with checked contracts.
//!
//! Splits the cache, in-progress visit set, and the per-file inputs out of
//! `compute_exit_mode_summary`'s parameter list and into an `ExitModeAnalyzer`,
//! so the recursive call site reads as `self.summarize(&dep, diagnostics)`
//! instead of forwarding eight arguments.

use k816_assets::AssetFS;
use rustc_hash::FxHashMap;

use crate::ast::{CodeBlock, File, HlaStmt, Item, RegName, Stmt};
use crate::diag::Diagnostic;
use crate::sema::SemanticModel;
use crate::span::Spanned;

use super::{
    ExitModeSummary, ExitWidth, LowerContext, collect_label_declared_modes, collect_label_depths,
    entry_mode_variants, format_contract_mode, format_inferred_exit_width, infer_exit_width,
    lower_stmt,
};

pub(super) fn build_exit_mode_summaries(
    file: &File,
    sema: &SemanticModel,
    fs: &dyn AssetFS,
    diagnostics: &mut Vec<Diagnostic>,
) -> FxHashMap<String, ExitModeSummary> {
    let mut analyzer = ExitModeAnalyzer::from_file(file, sema, fs);
    let names: Vec<String> = analyzer.blocks.keys().cloned().collect();
    for name in &names {
        analyzer.summarize(name, diagnostics);
    }
    analyzer.cache
}

struct ExitModeAnalyzer<'a> {
    blocks: FxHashMap<String, &'a CodeBlock>,
    inline_bodies: FxHashMap<String, &'a CodeBlock>,
    sema: &'a SemanticModel,
    fs: &'a dyn AssetFS,
    cache: FxHashMap<String, ExitModeSummary>,
    visiting: Vec<String>,
}

impl<'a> ExitModeAnalyzer<'a> {
    fn from_file(file: &'a File, sema: &'a SemanticModel, fs: &'a dyn AssetFS) -> Self {
        let blocks: FxHashMap<String, &'a CodeBlock> = file
            .items
            .iter()
            .filter_map(|item| match &item.node {
                Item::CodeBlock(block) => Some((block.name.clone(), block)),
                _ => None,
            })
            .collect();
        let inline_bodies: FxHashMap<String, &'a CodeBlock> = file
            .items
            .iter()
            .filter_map(|item| match &item.node {
                Item::CodeBlock(block) if block.is_inline => Some((block.name.clone(), block)),
                _ => None,
            })
            .collect();
        Self {
            blocks,
            inline_bodies,
            sema,
            fs,
            cache: FxHashMap::default(),
            visiting: Vec::new(),
        }
    }

    fn summarize(&mut self, name: &str, diagnostics: &mut Vec<Diagnostic>) -> ExitModeSummary {
        if let Some(summary) = self.cache.get(name).copied() {
            return summary;
        }

        let Some(block) = self.blocks.get(name).copied() else {
            let summary = ExitModeSummary {
                a_width: ExitWidth::Unknown,
                i_width: ExitWidth::Unknown,
                is_naked: false,
            };
            self.cache.insert(name.to_string(), summary);
            return summary;
        };

        if self.visiting.iter().any(|entry| entry == name) {
            if let Some(span) = block.name_span {
                diagnostics.push(
                    Diagnostic::error(
                        span,
                        format!(
                            "cannot infer exit mode for recursive checked contract call cycle involving '{name}'"
                        ),
                    )
                    .with_help("remove the checked call cycle or use `call foo` on at least one edge"),
                );
            }
            let summary = ExitModeSummary {
                a_width: ExitWidth::Unknown,
                i_width: ExitWidth::Unknown,
                is_naked: block.is_naked,
            };
            self.cache.insert(name.to_string(), summary);
            return summary;
        }

        let mut deps = Vec::new();
        collect_checked_call_dependencies(&block.body, self.sema, &mut deps);

        self.visiting.push(name.to_string());
        for dep in deps {
            if dep != name {
                self.summarize(&dep, diagnostics);
            } else if let Some(span) = block.name_span {
                diagnostics.push(
                    Diagnostic::error(
                        span,
                        format!(
                            "cannot infer exit mode for recursive checked contract call cycle involving '{name}'"
                        ),
                    )
                    .with_help("remove the checked self-call or use `call foo` to keep caller mode"),
                );
            }
        }
        self.visiting.pop();

        let effective_contract = self
            .sema
            .functions
            .get(name)
            .map(|meta| meta.mode_contract)
            .unwrap_or(block.mode_contract);
        let is_entry = block.name == "main";
        let mut variant_modes = Vec::new();
        let mut divergent = false;
        for initial_mode in entry_mode_variants(is_entry, effective_contract) {
            let mut ctx = LowerContext {
                is_far: block.is_far,
                mode: initial_mode,
                ..LowerContext::default()
            };
            ctx.label_depths = collect_label_depths(&block.body, Some(name), 0, diagnostics);
            ctx.label_declared_modes =
                collect_label_declared_modes(&block.body, Some(name), initial_mode, diagnostics);

            let mut dummy_diagnostics = Vec::new();
            let mut dummy_ops = Vec::new();
            let mut dummy_segment = crate::DEFAULT_SEGMENT.to_string();
            for stmt in &block.body {
                lower_stmt(
                    &stmt.node,
                    stmt.span,
                    Some(name),
                    self.sema,
                    self.fs,
                    &self.inline_bodies,
                    &self.cache,
                    &mut dummy_segment,
                    &mut ctx,
                    &mut dummy_diagnostics,
                    &mut dummy_ops,
                );
            }

            if !block.is_naked && ctx.reachable {
                ctx.return_modes.push(ctx.mode);
            }

            let mut return_modes = ctx.return_modes.into_iter();
            let inferred = return_modes.next().unwrap_or_default();
            if return_modes.any(|mode| mode != inferred) {
                divergent = true;
            }
            variant_modes.push((initial_mode, inferred));
        }

        if divergent && let Some(span) = block.name_span {
            diagnostics.push(
                Diagnostic::error(
                    span,
                    format!("function '{name}' has inconsistent exit mode across reachable returns"),
                )
                .with_help(
                    "ensure every reachable return exits with the same @a*/@i* widths or use `call foo` at the caller",
                ),
            );
        }

        let summary = ExitModeSummary {
            a_width: infer_exit_width(&variant_modes, RegName::A),
            i_width: infer_exit_width(&variant_modes, RegName::X),
            is_naked: block.is_naked,
        };

        if let Some(exit_contract) = block.exit_contract
            && let Some(span) = block.name_span
        {
            for (register, expected, actual) in [
                (RegName::A, exit_contract.a_width, summary.a_width),
                (RegName::X, exit_contract.i_width, summary.i_width),
            ] {
                if let Some(expected) = expected
                    && actual != ExitWidth::Fixed(expected)
                {
                    diagnostics.push(
                        Diagnostic::error(
                            span,
                            format!(
                                "function '{name}' exit contract requires {} but inferred exit mode is {}",
                                format_contract_mode(Some(expected), register),
                                format_inferred_exit_width(actual, register),
                            ),
                        )
                        .with_help(
                            "change the body so all reachable returns end in the declared mode or update the `->` contract",
                        ),
                    );
                }
            }
        }

        self.cache.insert(name.to_string(), summary);
        summary
    }
}

fn collect_checked_call_dependencies(
    stmts: &[Spanned<Stmt>],
    sema: &SemanticModel,
    out: &mut Vec<String>,
) {
    for stmt in stmts {
        match &stmt.node {
            Stmt::Instruction(instruction)
                if instruction.operand.is_none()
                    && sema
                        .functions
                        .get(&instruction.mnemonic)
                        .is_some_and(|meta| meta.has_contract) =>
            {
                if !out.iter().any(|name| name == &instruction.mnemonic) {
                    out.push(instruction.mnemonic.clone());
                }
            }
            Stmt::Call(call)
                if call.is_bare
                    && sema
                        .functions
                        .get(&call.target)
                        .is_some_and(|meta| meta.has_contract) =>
            {
                if !out.iter().any(|name| name == &call.target) {
                    out.push(call.target.clone());
                }
            }
            Stmt::ModeScopedBlock { body, .. } => {
                collect_checked_call_dependencies(body, sema, out)
            }
            Stmt::Hla(HlaStmt::NeverBlock { body }) => {
                collect_checked_call_dependencies(body, sema, out);
            }
            Stmt::Hla(HlaStmt::PrefixConditional {
                body, else_body, ..
            }) => {
                collect_checked_call_dependencies(body, sema, out);
                if let Some(else_body) = else_body {
                    collect_checked_call_dependencies(else_body, sema, out);
                }
            }
            _ => {}
        }
    }
}
