use k816_assets::AssetFS;
use k816_eval::{EvalContext, EvalError as EvaluatorError, Number};
use rustc_hash::FxHashMap;

use crate::ast::{
    DataWidth, Expr, ExprBinaryOp, ExprUnaryOp, File, HlaCompareOp, HlaCondition, HlaRegister,
    HlaRhs, HlaStmt, Instruction, Item, NamedDataBlock, NamedDataEntry, Operand, OperandAddrMode,
    RegWidth, Stmt,
};
use crate::data_blocks::lower_data_block;
use crate::diag::Diagnostic;
use crate::hir::{
    AddressOperandMode, AddressValue, ByteRelocation, ByteRelocationKind, IndexRegister,
    InstructionOp, Op, OperandOp, Program,
};
use crate::sema::SemanticModel;
use crate::span::{Span, Spanned};

/// Tracked CPU register width state during lowering.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
struct ModeState {
    a_width: Option<RegWidth>,
    i_width: Option<RegWidth>,
}

#[derive(Debug, Clone)]
struct ModeFrame {
    entry_mode: ModeState,
}

#[derive(Debug)]
struct LowerContext {
    next_label: usize,
    do_loop_targets: Vec<(String, usize)>,
    break_targets: Vec<String>,
    mode: ModeState,
    mode_frames: Vec<ModeFrame>,
    label_entry_modes: FxHashMap<String, ModeState>,
    label_declared_modes: FxHashMap<String, ModeState>,
    label_fixed_masks: FxHashMap<String, u8>,
    label_depths: FxHashMap<String, usize>,
    reachable: bool,
}

struct EvaluatedBytes {
    bytes: Vec<u8>,
    relocations: Vec<ByteRelocation>,
}

impl LowerContext {
    fn next_label_id(&mut self) -> usize {
        let id = self.next_label;
        self.next_label += 1;
        id
    }

    fn frame_depth(&self) -> usize {
        self.mode_frames.len()
    }
}

impl Default for LowerContext {
    fn default() -> Self {
        Self {
            next_label: 0,
            do_loop_targets: Vec::new(),
            break_targets: Vec::new(),
            mode: ModeState::default(),
            mode_frames: Vec::new(),
            label_entry_modes: FxHashMap::default(),
            label_declared_modes: FxHashMap::default(),
            label_fixed_masks: FxHashMap::default(),
            label_depths: FxHashMap::default(),
            reachable: true,
        }
    }
}

fn initial_mode_for_block(is_entry: bool, mode_contract: crate::ast::ModeContract) -> ModeState {
    if is_entry {
        ModeState {
            a_width: Some(mode_contract.a_width.unwrap_or(RegWidth::W8)),
            i_width: Some(mode_contract.i_width.unwrap_or(RegWidth::W8)),
        }
    } else {
        ModeState {
            a_width: mode_contract.a_width,
            i_width: mode_contract.i_width,
        }
    }
}

pub fn lower(
    file: &File,
    sema: &SemanticModel,
    fs: &dyn AssetFS,
) -> Result<Program, Vec<Diagnostic>> {
    let mut diagnostics = Vec::new();
    let mut ops = Vec::new();
    let mut top_level_ctx = LowerContext {
        ..Default::default()
    };
    let mut current_segment = "default".to_string();

    for item in &file.items {
        match &item.node {
            Item::Segment(segment) => {
                ops.push(Spanned::new(
                    Op::SelectSegment(segment.name.clone()),
                    item.span,
                ));
                current_segment = segment.name.clone();
            }
            Item::DataBlock(block) => match lower_data_block(block, fs) {
                Ok(mut lowered) => ops.append(&mut lowered),
                Err(mut errs) => diagnostics.append(&mut errs),
            },
            Item::NamedDataBlock(block) => {
                lower_named_data_block(
                    block,
                    sema,
                    fs,
                    &current_segment,
                    &mut diagnostics,
                    &mut ops,
                );
            }
            Item::CodeBlock(block) => {
                let mut block_ctx = LowerContext::default();
                let scope = block.name.clone();
                block_ctx.label_depths =
                    collect_label_depths(&block.body, Some(scope.as_str()), 0, &mut diagnostics);
                let label_span = block.name_span.unwrap_or(item.span);
                let mut block_segment = current_segment.clone();
                // Allow `segment ...` at the top of a code block to control where the
                // function/main entry label is emitted.
                let mut body_start = 0usize;
                while body_start < block.body.len() {
                    let stmt = &block.body[body_start];
                    match &stmt.node {
                        Stmt::Segment(segment) => {
                            ops.push(Spanned::new(
                                Op::SelectSegment(segment.name.clone()),
                                stmt.span,
                            ));
                            block_segment = segment.name.clone();
                            body_start += 1;
                        }
                        _ => break,
                    }
                }

                // Use sema's merged contract (includes module defaults).
                let effective_contract = sema
                    .functions
                    .get(&block.name)
                    .map(|meta| meta.mode_contract)
                    .unwrap_or(block.mode_contract);
                let is_entry = block.name == "main";
                let initial_mode = initial_mode_for_block(is_entry, effective_contract);
                block_ctx.mode = initial_mode;
                block_ctx.label_declared_modes = collect_label_declared_modes(
                    &block.body[body_start..],
                    Some(scope.as_str()),
                    initial_mode,
                    &mut diagnostics,
                );

                ops.push(Spanned::new(
                    Op::FunctionStart {
                        name: scope.clone(),
                        mode_contract: effective_contract,
                        is_entry,
                        is_far: block.is_far,
                    },
                    label_span,
                ));
                ops.push(Spanned::new(Op::Label(scope.clone()), label_span));
                // For entry points, emit mode setup at function entry since
                // there is no call site to bridge from. The CPU defaults to
                // 8-bit mode, so only emit Rep for 16-bit contracts.
                if is_entry {
                    if effective_contract.a_width == Some(RegWidth::W16) {
                        ops.push(Spanned::new(Op::Rep(0x20), label_span));
                    }
                    if effective_contract.i_width == Some(RegWidth::W16) {
                        ops.push(Spanned::new(Op::Rep(0x10), label_span));
                    }
                }
                for stmt in block.body.iter().skip(body_start) {
                    lower_stmt(
                        &stmt.node,
                        stmt.span,
                        Some(scope.as_str()),
                        sema,
                        fs,
                        &mut block_segment,
                        &mut block_ctx,
                        &mut diagnostics,
                        &mut ops,
                    );
                }

                if !block_ctx.do_loop_targets.is_empty() {
                    diagnostics.push(
                        Diagnostic::error(item.span, "unterminated HLA do/while loop")
                            .with_help("close every '{' HLA loop with a '} a?...' condition"),
                    );
                }

                if !block.is_naked {
                    let mnemonic = if block.is_far { "rtl" } else { "rts" };
                    ops.push(Spanned::new(
                        Op::Instruction(InstructionOp {
                            mnemonic: mnemonic.to_string(),
                            operand: None,
                        }),
                        item.span,
                    ));
                }
                ops.push(Spanned::new(Op::FunctionEnd, item.span));
                if block_segment != current_segment {
                    ops.push(Spanned::new(
                        Op::SelectSegment(current_segment.clone()),
                        item.span,
                    ));
                }

                if is_entry && block.is_far {
                    diagnostics.push(
                        Diagnostic::error(item.span, "'func main' cannot be declared far")
                            .with_help("remove 'far' from 'func main'"),
                    );
                }
            }
            Item::Statement(stmt) => {
                lower_stmt(
                    stmt,
                    item.span,
                    None,
                    sema,
                    fs,
                    &mut current_segment,
                    &mut top_level_ctx,
                    &mut diagnostics,
                    &mut ops,
                );
            }
            Item::EvaluatorBlock(_) => {}
            Item::Const(_) => {}
            Item::Var(var) => {
                emit_var_absolute_symbols(var, item.span, sema, &mut diagnostics, &mut ops);
            }
        }
    }

    if diagnostics.is_empty() {
        Ok(Program { ops })
    } else {
        Err(diagnostics)
    }
}

fn collect_label_depths(
    stmts: &[Spanned<Stmt>],
    scope: Option<&str>,
    depth: usize,
    diagnostics: &mut Vec<Diagnostic>,
) -> FxHashMap<String, usize> {
    let mut out = FxHashMap::default();
    collect_label_depths_into(stmts, scope, depth, diagnostics, &mut out);
    out
}

fn collect_label_declared_modes(
    stmts: &[Spanned<Stmt>],
    scope: Option<&str>,
    initial_mode: ModeState,
    diagnostics: &mut Vec<Diagnostic>,
) -> FxHashMap<String, ModeState> {
    let mut out = FxHashMap::default();
    let mut mode = initial_mode;
    collect_label_declared_modes_into(stmts, scope, &mut mode, diagnostics, &mut out);
    out
}

fn collect_label_depths_into(
    stmts: &[Spanned<Stmt>],
    scope: Option<&str>,
    depth: usize,
    diagnostics: &mut Vec<Diagnostic>,
    out: &mut FxHashMap<String, usize>,
) {
    for stmt in stmts {
        match &stmt.node {
            Stmt::Label(label) => {
                if let Some(name) = resolve_symbol(&label.name, scope, stmt.span, diagnostics) {
                    out.entry(name).or_insert(depth);
                }
            }
            Stmt::ModeScopedBlock { body, .. } => {
                collect_label_depths_into(body, scope, depth + 1, diagnostics, out);
            }
            Stmt::Hla(HlaStmt::NeverBlock { body }) => {
                collect_label_depths_into(body, scope, depth, diagnostics, out);
            }
            Stmt::Hla(HlaStmt::PrefixConditional {
                body, else_body, ..
            }) => {
                collect_label_depths_into(body, scope, depth, diagnostics, out);
                if let Some(else_body) = else_body {
                    collect_label_depths_into(else_body, scope, depth, diagnostics, out);
                }
            }
            _ => {}
        }
    }
}

fn collect_label_declared_modes_into(
    stmts: &[Spanned<Stmt>],
    scope: Option<&str>,
    mode: &mut ModeState,
    diagnostics: &mut Vec<Diagnostic>,
    out: &mut FxHashMap<String, ModeState>,
) {
    for stmt in stmts {
        match &stmt.node {
            Stmt::Label(label) => {
                if let Some(name) = resolve_symbol(&label.name, scope, stmt.span, diagnostics) {
                    out.entry(name).or_insert(*mode);
                }
            }
            Stmt::ModeSet { a_width, i_width } => {
                if let Some(a) = a_width {
                    mode.a_width = Some(*a);
                }
                if let Some(i) = i_width {
                    mode.i_width = Some(*i);
                }
            }
            Stmt::ModeScopedBlock {
                a_width,
                i_width,
                body,
            } => {
                let saved = *mode;
                if let Some(a) = a_width {
                    mode.a_width = Some(*a);
                }
                if let Some(i) = i_width {
                    mode.i_width = Some(*i);
                }
                collect_label_declared_modes_into(body, scope, mode, diagnostics, out);
                *mode = saved;
            }
            Stmt::Instruction(instruction) => {
                let mnemonic = instruction.mnemonic.to_ascii_lowercase();
                if mnemonic == "plp" || mnemonic == "rti" {
                    *mode = ModeState::default();
                }
            }
            Stmt::Hla(HlaStmt::NeverBlock { body }) => {
                let mut inner_mode = *mode;
                collect_label_declared_modes_into(body, scope, &mut inner_mode, diagnostics, out);
            }
            Stmt::Hla(HlaStmt::PrefixConditional {
                body, else_body, ..
            }) => {
                let mut inner_mode = *mode;
                collect_label_declared_modes_into(body, scope, &mut inner_mode, diagnostics, out);
                if let Some(else_body) = else_body {
                    let mut else_mode = *mode;
                    collect_label_declared_modes_into(
                        else_body,
                        scope,
                        &mut else_mode,
                        diagnostics,
                        out,
                    );
                }
            }
            _ => {}
        }
    }
}

fn lower_named_data_block(
    block: &NamedDataBlock,
    sema: &SemanticModel,
    fs: &dyn AssetFS,
    outer_segment: &str,
    diagnostics: &mut Vec<Diagnostic>,
    ops: &mut Vec<Spanned<Op>>,
) {
    let Some(name) = resolve_symbol(&block.name, None, block.name_span, diagnostics) else {
        return;
    };
    let mut label_emitted = false;
    let mut block_segment = outer_segment.to_string();
    let mut charset: Option<String> = None;

    for entry in &block.entries {
        if !label_emitted
            && matches!(
                entry.node,
                NamedDataEntry::Segment(_)
                    | NamedDataEntry::Address(_)
                    | NamedDataEntry::Align(_)
                    | NamedDataEntry::Nocross(_)
            )
        {
            lower_named_data_entry(
                &entry.node,
                entry.span,
                sema,
                fs,
                &mut block_segment,
                &mut charset,
                diagnostics,
                ops,
            );
            continue;
        }

        if !label_emitted {
            ops.push(Spanned::new(Op::Label(name.clone()), block.name_span));
            label_emitted = true;
        }

        lower_named_data_entry(
            &entry.node,
            entry.span,
            sema,
            fs,
            &mut block_segment,
            &mut charset,
            diagnostics,
            ops,
        );
    }

    if !label_emitted {
        ops.push(Spanned::new(Op::Label(name), block.name_span));
    }
    if block_segment != outer_segment {
        ops.push(Spanned::new(
            Op::SelectSegment(outer_segment.to_string()),
            block.name_span,
        ));
    }
}

fn lower_named_data_entry(
    entry: &NamedDataEntry,
    span: Span,
    sema: &SemanticModel,
    fs: &dyn AssetFS,
    current_segment: &mut String,
    charset: &mut Option<String>,
    diagnostics: &mut Vec<Diagnostic>,
    ops: &mut Vec<Spanned<Op>>,
) {
    match entry {
        NamedDataEntry::Segment(segment) => {
            ops.push(Spanned::new(Op::SelectSegment(segment.name.clone()), span));
            *current_segment = segment.name.clone();
        }
        NamedDataEntry::Label(name) => {
            if let Some(resolved) = resolve_symbol(name, None, span, diagnostics) {
                ops.push(Spanned::new(Op::Label(resolved), span));
            }
        }
        NamedDataEntry::Address(value) => {
            ops.push(Spanned::new(Op::Address(*value), span));
        }
        NamedDataEntry::Align(value) => {
            ops.push(Spanned::new(
                Op::Align {
                    boundary: *value,
                    offset: 0,
                },
                span,
            ));
        }
        NamedDataEntry::Nocross(value) => {
            ops.push(Spanned::new(Op::Nocross(*value), span));
        }
        NamedDataEntry::Bytes(values) => {
            if let Some(evaluated) = evaluate_byte_exprs(values, None, sema, span, diagnostics) {
                let op = if evaluated.relocations.is_empty() {
                    Op::EmitBytes(evaluated.bytes)
                } else {
                    Op::EmitRelocBytes {
                        bytes: evaluated.bytes,
                        relocations: evaluated.relocations,
                    }
                };
                ops.push(Spanned::new(op, span));
            }
        }
        NamedDataEntry::Words(values) => {
            if let Some(evaluated) = evaluate_word_exprs(values, None, sema, span, diagnostics) {
                let op = if evaluated.relocations.is_empty() {
                    Op::EmitBytes(evaluated.bytes)
                } else {
                    Op::EmitRelocBytes {
                        bytes: evaluated.bytes,
                        relocations: evaluated.relocations,
                    }
                };
                ops.push(Spanned::new(op, span));
            }
        }
        NamedDataEntry::Fars(values) => {
            if let Some(evaluated) = evaluate_far_exprs(values, None, sema, span, diagnostics) {
                let op = if evaluated.relocations.is_empty() {
                    Op::EmitBytes(evaluated.bytes)
                } else {
                    Op::EmitRelocBytes {
                        bytes: evaluated.bytes,
                        relocations: evaluated.relocations,
                    }
                };
                ops.push(Spanned::new(op, span));
            }
        }
        NamedDataEntry::ForEvalRange(range) => {
            let Some(start) = eval_to_number_strict(&range.start, sema, span, diagnostics) else {
                return;
            };
            let Some(end) = eval_to_number_strict(&range.end, sema, span, diagnostics) else {
                return;
            };

            let mut context = EvalContext::default();
            for (name, constant) in &sema.consts {
                context.set(name.clone(), constant.value);
            }

            let mut bytes = Vec::new();
            let step = if start <= end { 1_i64 } else { -1_i64 };
            let mut current = start;

            loop {
                context.set(range.iterator.as_str(), Number::Int(current));
                let outcome = match k816_eval::evaluate_with_context(&range.eval, &mut context) {
                    Ok(value) => value,
                    Err(error) => {
                        diagnostics.push(map_named_data_for_eval_error(error, span));
                        return;
                    }
                };
                let value = outcome.value;

                let Some(integer) = value.to_i64_exact() else {
                    diagnostics.push(
                        Diagnostic::error(
                            span,
                            format!(
                                "data for-eval expression must evaluate to an exact integer, got {value}"
                            ),
                        )
                        .with_help("use an expression that resolves to an integer byte value"),
                    );
                    return;
                };

                let Ok(byte) = u8::try_from(integer) else {
                    diagnostics.push(Diagnostic::error(
                        span,
                        format!("byte literal out of range: {integer}"),
                    ));
                    return;
                };
                bytes.push(byte);

                if current == end {
                    break;
                }
                let Some(next) = current.checked_add(step) else {
                    diagnostics.push(Diagnostic::error(
                        span,
                        "for-eval range overflow while iterating",
                    ));
                    return;
                };
                current = next;
            }

            ops.push(Spanned::new(Op::EmitBytes(bytes), span));
        }
        NamedDataEntry::String(value) => {
            let bytes = if let &mut Some(ref cs) = charset {
                value
                    .chars()
                    .map(|ch| cs.find(ch).map(|idx| idx as u8).unwrap_or(0))
                    .collect()
            } else {
                value.as_bytes().to_vec()
            };
            ops.push(Spanned::new(Op::EmitBytes(bytes), span));
        }
        NamedDataEntry::Repeat { count, body } => {
            for _ in 0..*count {
                for entry in body {
                    lower_named_data_entry(
                        &entry.node,
                        entry.span,
                        sema,
                        fs,
                        current_segment,
                        charset,
                        diagnostics,
                        ops,
                    );
                }
            }
        }
        NamedDataEntry::Code(stmts) => {
            let mode_contract = crate::ast::ModeContract {
                a_width: Some(RegWidth::W8),
                i_width: Some(RegWidth::W8),
            };
            ops.push(Spanned::new(Op::SetMode(mode_contract), span));
            let mut code_ctx = LowerContext::default();
            code_ctx.mode = ModeState {
                a_width: Some(RegWidth::W8),
                i_width: Some(RegWidth::W8),
            };
            let mut code_segment = current_segment.clone();
            for stmt in stmts {
                lower_stmt(
                    &stmt.node,
                    stmt.span,
                    None,
                    sema,
                    fs,
                    &mut code_segment,
                    &mut code_ctx,
                    diagnostics,
                    ops,
                );
            }
        }
        NamedDataEntry::Evaluator(_text) => {
            // Evaluator blocks are handled at the sema level; no lowering needed.
        }
        NamedDataEntry::Charset(value) => {
            *charset = Some(value.clone());
        }
    }
}

fn lower_stmt(
    stmt: &Stmt,
    span: Span,
    scope: Option<&str>,
    sema: &SemanticModel,
    fs: &dyn AssetFS,
    current_segment: &mut String,
    ctx: &mut LowerContext,
    diagnostics: &mut Vec<Diagnostic>,
    ops: &mut Vec<Spanned<Op>>,
) {
    match stmt {
        Stmt::Segment(segment) => {
            ops.push(Spanned::new(Op::SelectSegment(segment.name.clone()), span));
            *current_segment = segment.name.clone();
        }
        Stmt::Label(label) => {
            if let Some(resolved) = resolve_symbol(&label.name, scope, span, diagnostics) {
                let declared_mode = ctx.label_declared_modes.get(&resolved).copied();
                let mut fixed_mask = ctx.label_fixed_masks.get(&resolved).copied().unwrap_or(0);
                if let Some(declared) = declared_mode {
                    if ctx.reachable {
                        if let Some(a) = declared.a_width {
                            if ctx.mode.a_width != Some(a) {
                                fixed_mask |= 0x20;
                            }
                        }
                        if let Some(i) = declared.i_width {
                            if ctx.mode.i_width != Some(i) {
                                fixed_mask |= 0x10;
                            }
                        }
                    }
                }

                if let Some(incoming_mode) = ctx.label_entry_modes.get(&resolved).copied() {
                    let incoming_mode = apply_declared_label_mode(incoming_mode, declared_mode);
                    if ctx.reachable && ctx.mode != incoming_mode {
                        diagnostics.push(mode_mismatch_diagnostic(
                            span,
                            &resolved,
                            ctx.mode,
                            incoming_mode,
                        ));
                    }
                    ctx.mode = incoming_mode;
                    ctx.reachable = true;
                } else if ctx.reachable {
                    ctx.mode = apply_declared_label_mode(ctx.mode, declared_mode);
                    ctx.label_entry_modes.insert(resolved.clone(), ctx.mode);
                }

                ops.push(Spanned::new(Op::Label(resolved.clone()), span));

                if fixed_mask != 0 {
                    let fixed_a = if fixed_mask & 0x20 != 0 {
                        declared_mode.and_then(|mode| mode.a_width)
                    } else {
                        None
                    };
                    let fixed_i = if fixed_mask & 0x10 != 0 {
                        declared_mode.and_then(|mode| mode.i_width)
                    } else {
                        None
                    };
                    emit_fixed_mode_set(fixed_a, fixed_i, span, ops);
                    if let Some(a) = fixed_a {
                        ctx.mode.a_width = Some(a);
                    }
                    if let Some(i) = fixed_i {
                        ctx.mode.i_width = Some(i);
                    }
                    ctx.label_fixed_masks.insert(resolved, fixed_mask);
                }
            }
        }
        Stmt::Instruction(instruction) => {
            if instruction.operand.is_none() {
                if let Some(meta) = sema.functions.get(&instruction.mnemonic) {
                    let Some(target) =
                        resolve_symbol(&instruction.mnemonic, scope, span, diagnostics)
                    else {
                        return;
                    };
                    lower_call_with_contract(
                        &target,
                        meta.is_far,
                        meta.mode_contract.a_width,
                        meta.mode_contract.i_width,
                        span,
                        ctx,
                        ops,
                    );
                    return;
                }
            }
            lower_instruction_stmt(instruction, scope, sema, span, ctx, diagnostics, ops);
        }
        Stmt::Call(call) => {
            let Some(target) = resolve_symbol(&call.target, scope, span, diagnostics) else {
                return;
            };

            if let Some(meta) = sema.functions.get(&call.target) {
                lower_call_with_contract(
                    &target,
                    meta.is_far,
                    meta.mode_contract.a_width,
                    meta.mode_contract.i_width,
                    span,
                    ctx,
                    ops,
                );
            } else {
                // Unknown function: emit JSR or JSL based on `call far`.
                // The linker will resolve or report the missing symbol.
                lower_call_with_contract(&target, call.is_far, None, None, span, ctx, ops);
            }
        }
        Stmt::DataBlock(block) => match lower_data_block(block, fs) {
            Ok(mut lowered) => ops.append(&mut lowered),
            Err(mut errs) => diagnostics.append(&mut errs),
        },
        Stmt::Address(address) => {
            ops.push(Spanned::new(Op::Address(*address), span));
        }
        Stmt::Align { boundary, offset } => {
            ops.push(Spanned::new(
                Op::Align {
                    boundary: *boundary,
                    offset: *offset,
                },
                span,
            ));
        }
        Stmt::Nocross(nocross) => {
            ops.push(Spanned::new(Op::Nocross(*nocross), span));
        }
        Stmt::Hla(HlaStmt::NeverBlock { body }) => {
            let Some(skip_label) = fresh_local_label("never_end", ctx, scope, span, diagnostics)
            else {
                return;
            };
            // Emit JMP over the block body
            emit_branch_to_label("jmp", &skip_label, scope, sema, span, ctx, diagnostics, ops);
            let saved_reachable = ctx.reachable;
            ctx.reachable = false;
            for s in body {
                lower_stmt(
                    &s.node,
                    s.span,
                    scope,
                    sema,
                    fs,
                    current_segment,
                    ctx,
                    diagnostics,
                    ops,
                );
            }
            ctx.reachable = saved_reachable;
            ops.push(Spanned::new(Op::Label(skip_label), span));
        }
        Stmt::Hla(HlaStmt::PrefixConditional {
            skip_mnemonic,
            body,
            else_body,
        }) => {
            if let Some(else_body) = else_body {
                // if-else form: condition skips to else, then-body jumps over else
                let Some(else_label) = fresh_local_label("else", ctx, scope, span, diagnostics)
                else {
                    return;
                };
                let Some(end_label) = fresh_local_label("endif", ctx, scope, span, diagnostics)
                else {
                    return;
                };
                emit_branch_to_label(
                    skip_mnemonic,
                    &else_label,
                    scope,
                    sema,
                    span,
                    ctx,
                    diagnostics,
                    ops,
                );
                for s in body {
                    lower_stmt(
                        &s.node,
                        s.span,
                        scope,
                        sema,
                        fs,
                        current_segment,
                        ctx,
                        diagnostics,
                        ops,
                    );
                }
                emit_branch_to_label("bra", &end_label, scope, sema, span, ctx, diagnostics, ops);
                if let Some(incoming_mode) = ctx.label_entry_modes.get(&else_label).copied() {
                    if ctx.reachable && ctx.mode != incoming_mode {
                        diagnostics.push(mode_mismatch_diagnostic(
                            span,
                            &else_label,
                            ctx.mode,
                            incoming_mode,
                        ));
                    }
                    ctx.mode = incoming_mode;
                    ctx.reachable = true;
                } else if ctx.reachable {
                    ctx.label_entry_modes.insert(else_label.clone(), ctx.mode);
                }
                ops.push(Spanned::new(Op::Label(else_label), span));
                for s in else_body {
                    lower_stmt(
                        &s.node,
                        s.span,
                        scope,
                        sema,
                        fs,
                        current_segment,
                        ctx,
                        diagnostics,
                        ops,
                    );
                }
                if let Some(incoming_mode) = ctx.label_entry_modes.get(&end_label).copied() {
                    if ctx.reachable && ctx.mode != incoming_mode {
                        diagnostics.push(mode_mismatch_diagnostic(
                            span,
                            &end_label,
                            ctx.mode,
                            incoming_mode,
                        ));
                    }
                    ctx.mode = incoming_mode;
                    ctx.reachable = true;
                } else if ctx.reachable {
                    ctx.label_entry_modes.insert(end_label.clone(), ctx.mode);
                }
                ops.push(Spanned::new(Op::Label(end_label), span));
            } else {
                // Simple skip form (no else)
                let Some(skip_label) =
                    fresh_local_label("prefix_skip", ctx, scope, span, diagnostics)
                else {
                    return;
                };
                emit_branch_to_label(
                    skip_mnemonic,
                    &skip_label,
                    scope,
                    sema,
                    span,
                    ctx,
                    diagnostics,
                    ops,
                );
                for s in body {
                    lower_stmt(
                        &s.node,
                        s.span,
                        scope,
                        sema,
                        fs,
                        current_segment,
                        ctx,
                        diagnostics,
                        ops,
                    );
                }
                if let Some(incoming_mode) = ctx.label_entry_modes.get(&skip_label).copied() {
                    if ctx.reachable && ctx.mode != incoming_mode {
                        diagnostics.push(mode_mismatch_diagnostic(
                            span,
                            &skip_label,
                            ctx.mode,
                            incoming_mode,
                        ));
                    }
                    ctx.mode = incoming_mode;
                    ctx.reachable = true;
                } else if ctx.reachable {
                    ctx.label_entry_modes.insert(skip_label.clone(), ctx.mode);
                }
                ops.push(Spanned::new(Op::Label(skip_label), span));
            }
        }
        Stmt::Hla(stmt) => {
            lower_hla_stmt(stmt, span, scope, sema, ctx, diagnostics, ops);
        }
        Stmt::ModeSet { a_width, i_width } => {
            ctx.mode = lower_mode_contract_transition(ctx.mode, *a_width, *i_width, span, ops);
        }
        Stmt::ModeScopedBlock {
            a_width,
            i_width,
            body,
        } => {
            let saved_mode = ctx.mode;
            ctx.mode_frames.push(ModeFrame {
                entry_mode: saved_mode,
            });
            ctx.mode = lower_mode_contract_transition(ctx.mode, *a_width, *i_width, span, ops);
            // Lower body
            for s in body {
                lower_stmt(
                    &s.node,
                    s.span,
                    scope,
                    sema,
                    fs,
                    current_segment,
                    ctx,
                    diagnostics,
                    ops,
                );
            }
            let _ = ctx.mode_frames.pop();
            if ctx.reachable {
                // Restore mode when control falls out of the block naturally.
                lower_mode_restore(ctx.mode, saved_mode, span, ops);
                ctx.mode = saved_mode;
            }
        }
        Stmt::SwapAB => {
            ops.push(Spanned::new(
                Op::Instruction(InstructionOp {
                    mnemonic: "xba".to_string(),
                    operand: None,
                }),
                span,
            ));
        }
        Stmt::TransferChain(instructions) => {
            for instr in instructions {
                lower_instruction_and_push(instr, scope, sema, span, diagnostics, ops);
            }
        }
        Stmt::Var(var) => {
            emit_var_absolute_symbols(var, span, sema, diagnostics, ops);
        }
        Stmt::Empty => {}
    }
}

fn emit_var_absolute_symbols(
    var: &crate::ast::VarDecl,
    span: Span,
    sema: &SemanticModel,
    diagnostics: &mut Vec<Diagnostic>,
    ops: &mut Vec<Spanned<Op>>,
) {
    let Some(meta) = sema.vars.get(&var.name) else {
        return;
    };

    ops.push(Spanned::new(
        Op::DefineAbsoluteSymbol {
            name: var.name.clone(),
            address: meta.address,
        },
        span,
    ));

    let Some(symbolic_subscript) = meta.symbolic_subscript.as_ref() else {
        return;
    };

    for (field_name, field_meta) in &symbolic_subscript.fields {
        let Some(address) = meta.address.checked_add(field_meta.offset) else {
            diagnostics.push(Diagnostic::error(
                span,
                format!(
                    "symbolic subscript field '{}.{}' address overflows address space",
                    var.name, field_name
                ),
            ));
            continue;
        };
        ops.push(Spanned::new(
            Op::DefineAbsoluteSymbol {
                name: format!("{}.{}", var.name, field_name),
                address,
            },
            span,
        ));
    }
}

fn lower_mode_set(
    a_width: Option<RegWidth>,
    i_width: Option<RegWidth>,
    span: Span,
    ops: &mut Vec<Spanned<Op>>,
) {
    // @a16 => rep(0x20), @a8 => sep(0x20)
    // @i16 => rep(0x10), @i8 => sep(0x10)
    match a_width {
        Some(RegWidth::W16) => ops.push(Spanned::new(Op::Rep(0x20), span)),
        Some(RegWidth::W8) => ops.push(Spanned::new(Op::Sep(0x20), span)),
        None => {}
    }
    match i_width {
        Some(RegWidth::W16) => ops.push(Spanned::new(Op::Rep(0x10), span)),
        Some(RegWidth::W8) => ops.push(Spanned::new(Op::Sep(0x10), span)),
        None => {}
    }
}

fn emit_fixed_mode_set(
    a_width: Option<RegWidth>,
    i_width: Option<RegWidth>,
    span: Span,
    ops: &mut Vec<Spanned<Op>>,
) {
    let mut rep_mask = 0u8;
    let mut sep_mask = 0u8;
    match a_width {
        Some(RegWidth::W16) => rep_mask |= 0x20,
        Some(RegWidth::W8) => sep_mask |= 0x20,
        None => {}
    }
    match i_width {
        Some(RegWidth::W16) => rep_mask |= 0x10,
        Some(RegWidth::W8) => sep_mask |= 0x10,
        None => {}
    }
    if rep_mask != 0 {
        ops.push(Spanned::new(Op::FixedRep(rep_mask), span));
    }
    if sep_mask != 0 {
        ops.push(Spanned::new(Op::FixedSep(sep_mask), span));
    }
}

fn lower_mode_contract_transition(
    current: ModeState,
    a_width: Option<RegWidth>,
    i_width: Option<RegWidth>,
    span: Span,
    ops: &mut Vec<Spanned<Op>>,
) -> ModeState {
    let mut next = current;

    let a_delta = if let Some(target) = a_width {
        if current.a_width != Some(target) {
            Some(target)
        } else {
            None
        }
    } else {
        None
    };
    let i_delta = if let Some(target) = i_width {
        if current.i_width != Some(target) {
            Some(target)
        } else {
            None
        }
    } else {
        None
    };

    lower_mode_set(a_delta, i_delta, span, ops);

    if let Some(target) = a_width {
        next.a_width = Some(target);
    }
    if let Some(target) = i_width {
        next.i_width = Some(target);
    }
    next
}

/// Emit REP/SEP ops to restore from `current` mode back to `saved` mode.
/// Only emits for dimensions where both states are known and differ.
fn lower_mode_restore(
    current: ModeState,
    saved: ModeState,
    span: Span,
    ops: &mut Vec<Spanned<Op>>,
) {
    // Restore A width
    match (saved.a_width, current.a_width) {
        (Some(RegWidth::W16), Some(RegWidth::W8)) => {
            ops.push(Spanned::new(Op::Rep(0x20), span));
        }
        (Some(RegWidth::W8), Some(RegWidth::W16)) => {
            ops.push(Spanned::new(Op::Sep(0x20), span));
        }
        _ => {}
    }
    // Restore index width
    match (saved.i_width, current.i_width) {
        (Some(RegWidth::W16), Some(RegWidth::W8)) => {
            ops.push(Spanned::new(Op::Rep(0x10), span));
        }
        (Some(RegWidth::W8), Some(RegWidth::W16)) => {
            ops.push(Spanned::new(Op::Sep(0x10), span));
        }
        _ => {}
    }
}

fn lower_call_with_contract(
    target: &str,
    is_far: bool,
    a_width: Option<RegWidth>,
    i_width: Option<RegWidth>,
    span: Span,
    ctx: &mut LowerContext,
    ops: &mut Vec<Spanned<Op>>,
) {
    let saved_mode = ctx.mode;
    ctx.mode = lower_mode_contract_transition(ctx.mode, a_width, i_width, span, ops);

    let mnemonic = if is_far { "jsl" } else { "jsr" };
    ops.push(Spanned::new(
        Op::Instruction(InstructionOp {
            mnemonic: mnemonic.to_string(),
            operand: Some(OperandOp::Address {
                value: AddressValue::Label(target.to_string()),
                force_far: is_far,
                mode: AddressOperandMode::Direct { index: None },
            }),
        }),
        span,
    ));

    lower_mode_restore(ctx.mode, saved_mode, span, ops);
    ctx.mode = saved_mode;
}

fn lower_instruction_stmt(
    instruction: &Instruction,
    scope: Option<&str>,
    sema: &SemanticModel,
    span: Span,
    ctx: &mut LowerContext,
    diagnostics: &mut Vec<Diagnostic>,
    ops: &mut Vec<Spanned<Op>>,
) {
    let mnemonic = instruction.mnemonic.to_ascii_lowercase();

    validate_instruction_width_rules(instruction, sema, ctx.mode, span, diagnostics);

    let target_label = instruction_jump_target_label(instruction, scope, span, diagnostics);
    let is_unconditional_jump = matches!(mnemonic.as_str(), "jmp" | "jml" | "bra" | "brl");
    let is_conditional_branch = matches!(
        mnemonic.as_str(),
        "bcc" | "bcs" | "beq" | "bmi" | "bne" | "bpl" | "bvc" | "bvs"
    );
    let is_return = matches!(mnemonic.as_str(), "rts" | "rtl" | "rti");

    if is_unconditional_jump || is_return {
        let exit_frames = if is_return {
            ctx.frame_depth()
        } else if let Some(target) = target_label.as_deref() {
            match ctx.label_depths.get(target).copied() {
                Some(target_depth) if target_depth > ctx.frame_depth() => {
                    diagnostics.push(
                        Diagnostic::error(span, "illegal jump into a deeper block scope")
                            .with_help("jump targets cannot enter nested @a*/@i* blocks"),
                    );
                    0
                }
                Some(target_depth) => ctx.frame_depth().saturating_sub(target_depth),
                None => ctx.frame_depth(),
            }
        } else {
            ctx.frame_depth()
        };

        let mode_after_jump = emit_mode_restores_for_exited_frames(
            ctx.mode,
            &ctx.mode_frames,
            exit_frames,
            span,
            ops,
        );

        if let Some(target) = target_label.as_deref() {
            record_jump_target_mode(ctx, target, mode_after_jump, span, diagnostics);
        }

        if lower_instruction_and_push(instruction, scope, sema, span, diagnostics, ops) {
            ctx.mode = mode_after_jump;
            ctx.reachable = false;
            if mnemonic == "plp" || mnemonic == "rti" {
                ctx.mode = ModeState::default();
            }
        }
        return;
    }

    if is_conditional_branch {
        if let Some(target) = target_label.as_deref() {
            record_jump_target_mode(ctx, target, ctx.mode, span, diagnostics);
        }
    }

    if lower_instruction_and_push(instruction, scope, sema, span, diagnostics, ops)
        && (mnemonic == "plp" || mnemonic == "rti")
    {
        ctx.mode = ModeState::default();
    }
}

fn emit_mode_restores_for_exited_frames(
    current: ModeState,
    frames: &[ModeFrame],
    exit_count: usize,
    span: Span,
    ops: &mut Vec<Spanned<Op>>,
) -> ModeState {
    let mut mode = current;
    for frame in frames.iter().rev().take(exit_count) {
        lower_mode_restore(mode, frame.entry_mode, span, ops);
        mode = frame.entry_mode;
    }
    mode
}

fn instruction_jump_target_label(
    instruction: &Instruction,
    scope: Option<&str>,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<String> {
    let expr = match instruction.operand.as_ref()? {
        Operand::Value {
            expr,
            addr_mode,
            index,
            ..
        } => {
            if *addr_mode != OperandAddrMode::Direct || index.is_some() {
                return None;
            }
            expr
        }
        Operand::Auto { expr } => expr,
        Operand::Immediate { .. } => return None,
    };

    let Some(name) = expr_ident_name(expr) else {
        return None;
    };

    resolve_symbol(name, scope, span, diagnostics)
}

fn record_label_entry_mode(
    ctx: &mut LowerContext,
    label: &str,
    mode: ModeState,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if let Some(existing) = ctx.label_entry_modes.get(label).copied() {
        if existing != mode {
            diagnostics.push(mode_mismatch_diagnostic(span, label, existing, mode));
        }
    } else {
        ctx.label_entry_modes.insert(label.to_string(), mode);
    }
}

fn apply_declared_label_mode(mode: ModeState, declared: Option<ModeState>) -> ModeState {
    let Some(declared) = declared else {
        return mode;
    };
    let mut out = mode;
    if let Some(a) = declared.a_width {
        out.a_width = Some(a);
    }
    if let Some(i) = declared.i_width {
        out.i_width = Some(i);
    }
    out
}

fn record_jump_target_mode(
    ctx: &mut LowerContext,
    label: &str,
    incoming_mode: ModeState,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let declared = ctx.label_declared_modes.get(label).copied();
    let mut effective_mode = apply_declared_label_mode(incoming_mode, declared);

    if let Some(declared) = declared {
        let mut fixed_mask = ctx.label_fixed_masks.get(label).copied().unwrap_or(0);
        if let Some(a) = declared.a_width {
            if incoming_mode.a_width != Some(a) {
                fixed_mask |= 0x20;
            }
            effective_mode.a_width = Some(a);
        }
        if let Some(i) = declared.i_width {
            if incoming_mode.i_width != Some(i) {
                fixed_mask |= 0x10;
            }
            effective_mode.i_width = Some(i);
        }
        if fixed_mask != 0 {
            ctx.label_fixed_masks.insert(label.to_string(), fixed_mask);
        }
    }

    record_label_entry_mode(ctx, label, effective_mode, span, diagnostics);
}

fn mode_mismatch_diagnostic(span: Span, label: &str, lhs: ModeState, rhs: ModeState) -> Diagnostic {
    Diagnostic::error(
        span,
        format!(
            "mode mismatch at label {label}: incoming edges have different modes ({} vs {})",
            format_mode_state(lhs),
            format_mode_state(rhs),
        ),
    )
    .with_help("ensure all incoming paths use the same @a*/@i* mode state")
}

fn format_mode_state(mode: ModeState) -> String {
    let a = match mode.a_width {
        Some(RegWidth::W8) => "a8",
        Some(RegWidth::W16) => "a16",
        None => "a?",
    };
    let i = match mode.i_width {
        Some(RegWidth::W8) => "i8",
        Some(RegWidth::W16) => "i16",
        None => "i?",
    };
    format!("{a}/{i}")
}

fn validate_instruction_width_rules(
    instruction: &Instruction,
    sema: &SemanticModel,
    mode: ModeState,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let mnemonic = instruction.mnemonic.to_ascii_lowercase();
    let Some(operand) = instruction.operand.as_ref() else {
        return;
    };

    let is_immediate = match operand {
        Operand::Immediate { .. } => true,
        Operand::Auto { expr } => is_immediate_expression(expr, sema),
        Operand::Value { .. } => false,
    };

    let imm_expr = match operand {
        Operand::Immediate { expr, .. } | Operand::Auto { expr } if is_immediate => Some(expr),
        _ => None,
    };

    if let Some(expr) = imm_expr {
        let reg_mode = match mnemonic.as_str() {
            "lda" => mode.a_width,
            "ldx" | "ldy" => mode.i_width,
            _ => None,
        };
        if let Some(reg_width) = reg_mode {
            if let Some(value) = eval_to_number_strict(expr, sema, span, diagnostics) {
                if !value_fits_reg_width(value, reg_width) {
                    diagnostics.push(immediate_width_error(
                        span,
                        value,
                        reg_width,
                        mnemonic.as_str(),
                    ));
                }
            }
        }
    }

    let addr_expr = match operand {
        Operand::Value { expr, .. } => Some(expr),
        Operand::Auto { expr } if !is_immediate => Some(expr),
        _ => None,
    };
    let Some(expr) = addr_expr else {
        return;
    };

    let Some(data_width) = expr_data_width(expr, sema) else {
        return;
    };
    let Some(required_width) = data_width_to_reg_width(data_width) else {
        let var_name = base_ident(expr);
        let msg = if let Some(name) = var_name {
            format!(
                "Cannot directly load/store :far value '{name}' — use explicit :byte or :word view"
            )
        } else {
            "Cannot directly load/store :far value — use explicit :byte or :word view".to_string()
        };
        let help = if let Some(name) = var_name {
            format!("e.g. {name}:word for low 16 bits, ({name}+2):byte for bank byte")
        } else {
            "e.g. name:word for low 16 bits, (name+2):byte for bank byte".to_string()
        };
        diagnostics.push(Diagnostic::error(span, msg).with_help(help));
        return;
    };
    let var_name = base_ident(expr);

    match mnemonic.as_str() {
        "lda" => validate_mode_for_typed_access(
            span,
            mode.a_width,
            required_width,
            "load",
            "a",
            var_name,
            data_width,
            diagnostics,
        ),
        "ldx" | "ldy" => validate_mode_for_typed_access(
            span,
            mode.i_width,
            required_width,
            "load",
            "i",
            var_name,
            data_width,
            diagnostics,
        ),
        "sta" => validate_mode_for_typed_access(
            span,
            mode.a_width,
            required_width,
            "store",
            "a",
            var_name,
            data_width,
            diagnostics,
        ),
        "stx" | "sty" => validate_mode_for_typed_access(
            span,
            mode.i_width,
            required_width,
            "store",
            "i",
            var_name,
            data_width,
            diagnostics,
        ),
        _ => {}
    }
}

fn validate_mode_for_typed_access(
    span: Span,
    actual: Option<RegWidth>,
    required: RegWidth,
    action: &str,
    axis: &str,
    var_name: Option<&str>,
    data_width: DataWidth,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let Some(actual) = actual else {
        return;
    };
    if actual == required {
        return;
    }

    let required_tag = match (axis, required) {
        ("a", RegWidth::W8) => "@a8",
        ("a", RegWidth::W16) => "@a16",
        ("i", RegWidth::W8) => "@i8",
        ("i", RegWidth::W16) => "@i16",
        _ => "@?",
    };

    let (message, help) = if action == "store" && axis == "a" && data_width == DataWidth::Word {
        if let Some(name) = var_name {
            (
                format!("Store to var {name}:word requires @a16."),
                Some(format!(
                    "for byte stores, use explicit view ({name}:byte / ({name}+1):byte)"
                )),
            )
        } else {
            (
                "Store to word-typed value requires @a16.".to_string(),
                Some("for byte stores, use an explicit :byte view".to_string()),
            )
        }
    } else if action == "store" && axis == "a" && data_width == DataWidth::Byte {
        if let Some(name) = var_name {
            (format!("Store to var {name}:byte requires @a8."), None)
        } else {
            ("Store to byte-typed value requires @a8.".to_string(), None)
        }
    } else if action == "load" {
        (
            format!(
                "Load from {}-typed value requires {required_tag}.",
                data_width_name(data_width)
            ),
            None,
        )
    } else {
        (
            format!(
                "{} to {}-typed value requires {required_tag}.",
                capitalize(action),
                data_width_name(data_width)
            ),
            None,
        )
    };

    let diagnostic = if let Some(help) = help {
        Diagnostic::error(span, message).with_help(help)
    } else {
        Diagnostic::error(span, message)
    };
    diagnostics.push(diagnostic);
}

fn expr_data_width(expr: &Expr, sema: &SemanticModel) -> Option<DataWidth> {
    match expr {
        Expr::TypedView { width, .. } => Some(*width),
        Expr::Ident(name) => sema
            .vars
            .get(name)
            .and_then(|var| var.data_width)
            .or_else(|| symbolic_subscript_field_width(name, sema)),
        Expr::IdentSpanned { name, .. } => sema
            .vars
            .get(name)
            .and_then(|var| var.data_width)
            .or_else(|| symbolic_subscript_field_width(name, sema)),
        Expr::Index { base, .. } => expr_data_width(base, sema),
        _ => None,
    }
}

fn base_ident(expr: &Expr) -> Option<&str> {
    match expr {
        Expr::Ident(name) => Some(name.as_str()),
        Expr::IdentSpanned { name, .. } => Some(name.as_str()),
        Expr::Index { base, .. } => base_ident(base),
        Expr::TypedView { expr, .. } => base_ident(expr),
        _ => None,
    }
}

fn expr_ident_name(expr: &Expr) -> Option<&str> {
    match expr {
        Expr::Ident(name) => Some(name.as_str()),
        Expr::IdentSpanned { name, .. } => Some(name.as_str()),
        _ => None,
    }
}

fn expr_ident_span(expr: &Expr, fallback: Span) -> Option<Span> {
    match expr {
        Expr::IdentSpanned { start, end, .. } => Some(Span::new(fallback.source_id, *start, *end)),
        _ => None,
    }
}

fn symbolic_subscript_field_width(name: &str, sema: &SemanticModel) -> Option<DataWidth> {
    let (base_name, field_name) = name.split_once('.')?;
    let base = sema.vars.get(base_name)?;
    let symbolic_subscript = base.symbolic_subscript.as_ref()?;
    symbolic_subscript
        .fields
        .get(field_name)
        .map(|field| field.data_width)
}

fn data_width_to_reg_width(width: DataWidth) -> Option<RegWidth> {
    match width {
        DataWidth::Byte => Some(RegWidth::W8),
        DataWidth::Word => Some(RegWidth::W16),
        DataWidth::Far => None,
    }
}

fn data_width_name(width: DataWidth) -> &'static str {
    match width {
        DataWidth::Byte => "byte",
        DataWidth::Word => "word",
        DataWidth::Far => "far",
    }
}

fn capitalize(text: &str) -> String {
    let mut chars = text.chars();
    let Some(first) = chars.next() else {
        return String::new();
    };
    first.to_uppercase().chain(chars).collect()
}

fn value_fits_reg_width(value: i64, width: RegWidth) -> bool {
    if value < 0 {
        return false;
    }
    match width {
        RegWidth::W8 => value <= i64::from(u8::MAX),
        RegWidth::W16 => value <= i64::from(u16::MAX),
    }
}

fn immediate_width_error(span: Span, value: i64, width: RegWidth, mnemonic: &str) -> Diagnostic {
    let value_text = if value < 0 {
        value.to_string()
    } else {
        format!("0x{value:X}")
    };
    match (mnemonic, width) {
        ("lda", RegWidth::W8) => Diagnostic::error(
            span,
            format!("Immediate {value_text} does not fit in @a8; use @a16 or split into bytes."),
        ),
        ("ldx" | "ldy", RegWidth::W8) => Diagnostic::error(
            span,
            format!("Immediate {value_text} does not fit in @i8; use @i16 or split into bytes."),
        ),
        _ => Diagnostic::error(
            span,
            format!("Immediate {value_text} does not fit in selected width."),
        ),
    }
}

fn lower_hla_stmt(
    stmt: &HlaStmt,
    span: Span,
    scope: Option<&str>,
    sema: &SemanticModel,
    ctx: &mut LowerContext,
    diagnostics: &mut Vec<Diagnostic>,
    ops: &mut Vec<Spanned<Op>>,
) {
    match stmt {
        HlaStmt::XAssignImmediate { rhs } => {
            let instruction = Instruction {
                mnemonic: "ldx".to_string(),
                operand: Some(Operand::Immediate {
                    expr: rhs.clone(),
                    explicit_hash: false,
                }),
            };
            lower_instruction_and_push(&instruction, scope, sema, span, diagnostics, ops);
        }
        HlaStmt::XIncrement => {
            let instruction = Instruction {
                mnemonic: "inx".to_string(),
                operand: None,
            };
            lower_instruction_and_push(&instruction, scope, sema, span, diagnostics, ops);
        }
        HlaStmt::StoreFromA { dests, rhs } => {
            let lda_instruction = Instruction {
                mnemonic: "lda".to_string(),
                operand: Some(match rhs {
                    HlaRhs::Immediate(expr) => Operand::Immediate {
                        expr: expr.clone(),
                        explicit_hash: false,
                    },
                    HlaRhs::Value {
                        expr,
                        index,
                        addr_mode,
                    } => {
                        if *addr_mode == OperandAddrMode::Direct && index.is_none() {
                            Operand::Auto { expr: expr.clone() }
                        } else {
                            Operand::Value {
                                expr: expr.clone(),
                                force_far: false,
                                index: *index,
                                addr_mode: *addr_mode,
                            }
                        }
                    }
                }),
            };
            if !lower_instruction_and_push(&lda_instruction, scope, sema, span, diagnostics, ops) {
                return;
            }

            // Store to each destination in reverse order (innermost first)
            for dest in dests.iter().rev() {
                let sta_instruction = Instruction {
                    mnemonic: "sta".to_string(),
                    operand: Some(Operand::Value {
                        expr: Expr::Ident(dest.clone()),
                        force_far: false,
                        index: None,
                        addr_mode: OperandAddrMode::Direct,
                    }),
                };
                lower_instruction_and_push(&sta_instruction, scope, sema, span, diagnostics, ops);
            }
        }
        HlaStmt::WaitLoopWhileNFlagClear { symbol } => {
            let Some(wait_label) = fresh_local_label("wait", ctx, scope, span, diagnostics) else {
                return;
            };
            ops.push(Spanned::new(Op::Label(wait_label.clone()), span));

            let bit_instruction = Instruction {
                mnemonic: "bit".to_string(),
                operand: Some(Operand::Value {
                    expr: Expr::Ident(symbol.clone()),
                    force_far: false,
                    index: None,
                    addr_mode: OperandAddrMode::Direct,
                }),
            };
            if !lower_instruction_and_push(&bit_instruction, scope, sema, span, diagnostics, ops) {
                return;
            }

            emit_branch_to_label("bpl", &wait_label, scope, sema, span, ctx, diagnostics, ops);
        }
        HlaStmt::ConditionSeed { .. } => {
            if let HlaStmt::ConditionSeed { rhs, .. } = stmt {
                let instruction = Instruction {
                    mnemonic: "cmp".to_string(),
                    operand: Some(Operand::Auto { expr: rhs.clone() }),
                };
                let _ =
                    lower_instruction_and_push(&instruction, scope, sema, span, diagnostics, ops);
            }
        }
        HlaStmt::DoOpen => {
            let Some(loop_label) = fresh_local_label("loop", ctx, scope, span, diagnostics) else {
                return;
            };
            let break_depth = ctx.break_targets.len();
            ctx.do_loop_targets.push((loop_label.clone(), break_depth));
            ops.push(Spanned::new(Op::Label(loop_label), span));
        }
        HlaStmt::DoCloseNFlagClear => {
            let Some((loop_target, break_depth)) = ctx.do_loop_targets.pop() else {
                diagnostics.push(
                    Diagnostic::error(span, "HLA do/while close without matching '{'")
                        .with_help("open loop with a standalone '{' line before the condition"),
                );
                return;
            };
            emit_branch_to_label(
                "bpl",
                &loop_target,
                scope,
                sema,
                span,
                ctx,
                diagnostics,
                ops,
            );
            resolve_break_labels(break_depth, ctx, span, ops);
        }
        HlaStmt::DoCloseNFlagSet => {
            let Some((loop_target, break_depth)) = ctx.do_loop_targets.pop() else {
                diagnostics.push(
                    Diagnostic::error(span, "HLA do/while close without matching '{'")
                        .with_help("open loop with a standalone '{' line before the condition"),
                );
                return;
            };
            emit_branch_to_label(
                "bmi",
                &loop_target,
                scope,
                sema,
                span,
                ctx,
                diagnostics,
                ops,
            );
            resolve_break_labels(break_depth, ctx, span, ops);
        }
        HlaStmt::DoCloseWithOp { op } => {
            let Some((loop_target, break_depth)) = ctx.do_loop_targets.pop() else {
                diagnostics.push(
                    Diagnostic::error(span, "HLA do/while close without matching '{'")
                        .with_help("open loop with a standalone '{' line before the condition"),
                );
                return;
            };
            lower_hla_postfix_close_op_branch(
                *op,
                &loop_target,
                span,
                scope,
                sema,
                ctx,
                diagnostics,
                ops,
            );
            resolve_break_labels(break_depth, ctx, span, ops);
        }
        HlaStmt::DoClose { condition } => {
            let Some((loop_target, break_depth)) = ctx.do_loop_targets.pop() else {
                diagnostics.push(
                    Diagnostic::error(span, "HLA do/while close without matching '{'")
                        .with_help("open loop with a standalone '{' line before the condition"),
                );
                return;
            };
            lower_hla_condition_branch(
                condition,
                &loop_target,
                span,
                scope,
                sema,
                ctx,
                diagnostics,
                ops,
            );
            resolve_break_labels(break_depth, ctx, span, ops);
        }
        HlaStmt::DoCloseAlways => {
            let Some((loop_target, break_depth)) = ctx.do_loop_targets.pop() else {
                diagnostics.push(
                    Diagnostic::error(span, "HLA do/while close without matching '{'")
                        .with_help("open loop with a standalone '{' line before the condition"),
                );
                return;
            };
            emit_branch_to_label(
                "bra",
                &loop_target,
                scope,
                sema,
                span,
                ctx,
                diagnostics,
                ops,
            );
            resolve_break_labels(break_depth, ctx, span, ops);
        }
        HlaStmt::DoCloseNever => {
            let Some((_loop_target, break_depth)) = ctx.do_loop_targets.pop() else {
                diagnostics.push(
                    Diagnostic::error(span, "HLA do/while close without matching '{'")
                        .with_help("open loop with a standalone '{' line before the condition"),
                );
                return;
            };
            resolve_break_labels(break_depth, ctx, span, ops);
        }
        HlaStmt::DoCloseBranch { mnemonic } => {
            let Some((loop_target, break_depth)) = ctx.do_loop_targets.pop() else {
                diagnostics.push(
                    Diagnostic::error(span, "HLA do/while close without matching '{'")
                        .with_help("open loop with a standalone '{' line before the condition"),
                );
                return;
            };
            emit_branch_to_label(
                mnemonic,
                &loop_target,
                scope,
                sema,
                span,
                ctx,
                diagnostics,
                ops,
            );
            resolve_break_labels(break_depth, ctx, span, ops);
        }
        HlaStmt::LoopBreak { mnemonic } => {
            if ctx.do_loop_targets.is_empty() {
                diagnostics.push(
                    Diagnostic::error(span, "'break' outside loop")
                        .with_help("'break' can only be used inside a '{' ... '}' loop"),
                );
                return;
            }
            let Some(break_label) = fresh_local_label("break", ctx, scope, span, diagnostics)
            else {
                return;
            };
            ctx.break_targets.push(break_label.clone());
            emit_branch_to_label(
                mnemonic,
                &break_label,
                scope,
                sema,
                span,
                ctx,
                diagnostics,
                ops,
            );
        }
        HlaStmt::LoopRepeat { mnemonic } => {
            let Some((loop_target, _)) = ctx.do_loop_targets.last() else {
                diagnostics.push(
                    Diagnostic::error(span, "'repeat' outside loop")
                        .with_help("'repeat' can only be used inside a '{' ... '}' loop"),
                );
                return;
            };
            let loop_target = loop_target.clone();
            emit_branch_to_label(
                mnemonic,
                &loop_target,
                scope,
                sema,
                span,
                ctx,
                diagnostics,
                ops,
            );
        }
        HlaStmt::RepeatNop(count) => {
            let nop = Instruction {
                mnemonic: "nop".to_string(),
                operand: None,
            };
            for _ in 0..*count {
                lower_instruction_and_push(&nop, scope, sema, span, diagnostics, ops);
            }
        }
        HlaStmt::NeverBlock { .. } | HlaStmt::PrefixConditional { .. } => {
            // Handled in lower_stmt directly (needs fs and current_segment parameters)
        }
    }
}

fn lower_hla_postfix_close_op_branch(
    op: HlaCompareOp,
    loop_target: &str,
    span: Span,
    scope: Option<&str>,
    sema: &SemanticModel,
    ctx: &mut LowerContext,
    diagnostics: &mut Vec<Diagnostic>,
    ops: &mut Vec<Spanned<Op>>,
) {
    match op {
        HlaCompareOp::Eq => {
            emit_branch_to_label("beq", loop_target, scope, sema, span, ctx, diagnostics, ops);
        }
        HlaCompareOp::Ne => {
            emit_branch_to_label("bne", loop_target, scope, sema, span, ctx, diagnostics, ops);
        }
        HlaCompareOp::Lt => {
            emit_branch_to_label("bcc", loop_target, scope, sema, span, ctx, diagnostics, ops);
        }
        HlaCompareOp::Ge => {
            emit_branch_to_label("bcs", loop_target, scope, sema, span, ctx, diagnostics, ops);
        }
        HlaCompareOp::Le => {
            emit_branch_to_label("bcc", loop_target, scope, sema, span, ctx, diagnostics, ops);
            emit_branch_to_label("beq", loop_target, scope, sema, span, ctx, diagnostics, ops);
        }
        HlaCompareOp::Gt => {
            let Some(skip_label) =
                fresh_local_label("postfix_gt_skip", ctx, scope, span, diagnostics)
            else {
                return;
            };
            emit_branch_to_label("beq", &skip_label, scope, sema, span, ctx, diagnostics, ops);
            emit_branch_to_label("bcc", &skip_label, scope, sema, span, ctx, diagnostics, ops);
            emit_branch_to_label("bra", loop_target, scope, sema, span, ctx, diagnostics, ops);
            ops.push(Spanned::new(Op::Label(skip_label), span));
        }
    }
}

fn lower_hla_condition_branch(
    condition: &HlaCondition,
    loop_target: &str,
    span: Span,
    scope: Option<&str>,
    sema: &SemanticModel,
    ctx: &mut LowerContext,
    diagnostics: &mut Vec<Diagnostic>,
    ops: &mut Vec<Spanned<Op>>,
) {
    if condition.lhs != HlaRegister::A {
        diagnostics.push(Diagnostic::error(
            span,
            "only accumulator 'a' is currently supported in HLA loop conditions",
        ));
        return;
    }

    let rhs = condition.rhs.as_ref().unwrap_or(&Expr::Number(0));
    let Some(rhs_number) = eval_to_number(rhs, scope, sema, span, diagnostics) else {
        return;
    };

    let compare_instruction = Instruction {
        mnemonic: "cmp".to_string(),
        operand: Some(Operand::Immediate {
            expr: Expr::Number(rhs_number),
            explicit_hash: false,
        }),
    };
    if !lower_instruction_and_push(&compare_instruction, scope, sema, span, diagnostics, ops) {
        return;
    }

    match condition.op {
        HlaCompareOp::Eq => {
            emit_branch_to_label("beq", loop_target, scope, sema, span, ctx, diagnostics, ops);
        }
        HlaCompareOp::Ne => {
            emit_branch_to_label("bne", loop_target, scope, sema, span, ctx, diagnostics, ops);
        }
        HlaCompareOp::Ge => {
            let branch = if rhs_number == 0 { "bpl" } else { "bcs" };
            emit_branch_to_label(
                branch,
                loop_target,
                scope,
                sema,
                span,
                ctx,
                diagnostics,
                ops,
            );
        }
        HlaCompareOp::Lt => {
            let branch = if rhs_number == 0 { "bmi" } else { "bcc" };
            emit_branch_to_label(
                branch,
                loop_target,
                scope,
                sema,
                span,
                ctx,
                diagnostics,
                ops,
            );
        }
        HlaCompareOp::Le => {
            if rhs_number == 0 {
                emit_branch_to_label("bmi", loop_target, scope, sema, span, ctx, diagnostics, ops);
                emit_branch_to_label("beq", loop_target, scope, sema, span, ctx, diagnostics, ops);
            } else {
                emit_branch_to_label("bcc", loop_target, scope, sema, span, ctx, diagnostics, ops);
                emit_branch_to_label("beq", loop_target, scope, sema, span, ctx, diagnostics, ops);
            }
        }
        HlaCompareOp::Gt => {
            let Some(skip_label) = fresh_local_label("cond_skip", ctx, scope, span, diagnostics)
            else {
                return;
            };

            emit_branch_to_label("beq", &skip_label, scope, sema, span, ctx, diagnostics, ops);
            let branch = if rhs_number == 0 { "bpl" } else { "bcs" };
            emit_branch_to_label(
                branch,
                loop_target,
                scope,
                sema,
                span,
                ctx,
                diagnostics,
                ops,
            );
            ops.push(Spanned::new(Op::Label(skip_label), span));
        }
    }
}

fn emit_branch_to_label(
    mnemonic: &str,
    target: &str,
    scope: Option<&str>,
    sema: &SemanticModel,
    span: Span,
    ctx: &mut LowerContext,
    diagnostics: &mut Vec<Diagnostic>,
    ops: &mut Vec<Spanned<Op>>,
) {
    let instruction = Instruction {
        mnemonic: mnemonic.to_string(),
        operand: Some(Operand::Value {
            expr: Expr::Ident(target.to_string()),
            force_far: false,
            index: None,
            addr_mode: OperandAddrMode::Direct,
        }),
    };
    lower_instruction_stmt(&instruction, scope, sema, span, ctx, diagnostics, ops);
}

fn resolve_break_labels(
    break_depth: usize,
    ctx: &mut LowerContext,
    span: Span,
    ops: &mut Vec<Spanned<Op>>,
) {
    while ctx.break_targets.len() > break_depth {
        let label = ctx.break_targets.pop().unwrap();
        ops.push(Spanned::new(Op::Label(label), span));
    }
}

fn fresh_local_label(
    family: &str,
    ctx: &mut LowerContext,
    scope: Option<&str>,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<String> {
    let raw = format!(".__k816_{family}_{}", ctx.next_label_id());
    resolve_symbol(&raw, scope, span, diagnostics)
}

fn evaluate_byte_exprs(
    values: &[Expr],
    scope: Option<&str>,
    sema: &SemanticModel,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<EvaluatedBytes> {
    let mut bytes = Vec::with_capacity(values.len());
    let mut relocations = Vec::new();

    for (index, value) in values.iter().enumerate() {
        if let Some(number) = eval_to_number_strict(value, sema, span, diagnostics) {
            match u8::try_from(number) {
                Ok(byte) => bytes.push(byte),
                Err(_) => {
                    diagnostics.push(Diagnostic::error(
                        span,
                        format!("byte literal out of range: {number}"),
                    ));
                    return None;
                }
            }
            continue;
        }

        if matches!(
            value,
            Expr::Unary {
                op: ExprUnaryOp::LowByte | ExprUnaryOp::HighByte,
                ..
            }
        ) {
            if let Some((kind, label)) =
                resolve_symbolic_byte_relocation(value, scope, sema, span, diagnostics)
            {
                bytes.push(0);
                relocations.push(ByteRelocation {
                    offset: u32::try_from(index).expect("byte expression index should fit in u32"),
                    kind,
                    label,
                });
                continue;
            }

            diagnostics.push(
                Diagnostic::error(
                    span,
                    "low/high-byte expression must be a constant or plain symbol reference",
                )
                .with_help("supported symbolic forms are '&<label' and '&>label'"),
            );
            return None;
        }

        let Some(number) = eval_to_number(value, scope, sema, span, diagnostics) else {
            return None;
        };
        match u8::try_from(number) {
            Ok(byte) => bytes.push(byte),
            Err(_) => {
                diagnostics.push(Diagnostic::error(
                    span,
                    format!("byte literal out of range: {number}"),
                ));
                return None;
            }
        }
    }

    Some(EvaluatedBytes { bytes, relocations })
}

fn evaluate_word_exprs(
    values: &[Expr],
    scope: Option<&str>,
    sema: &SemanticModel,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<EvaluatedBytes> {
    let mut bytes = Vec::with_capacity(values.len() * 2);
    let mut relocations = Vec::new();

    for value in values {
        // Try to resolve as a compile-time number (literal, const, var address, expr).
        if let Some(number) = eval_to_number_strict(value, sema, span, diagnostics) {
            match u16::try_from(number) {
                Ok(word) => bytes.extend_from_slice(&word.to_le_bytes()),
                Err(_) => {
                    diagnostics.push(Diagnostic::error(
                        span,
                        format!("word literal out of range: {number}"),
                    ));
                    return None;
                }
            }
            continue;
        }

        // &< / &> unary: emit single-byte relocation (same as in bytes context).
        if matches!(
            value,
            Expr::Unary {
                op: ExprUnaryOp::LowByte | ExprUnaryOp::HighByte,
                ..
            }
        ) {
            if let Some((kind, label)) =
                resolve_symbolic_byte_relocation(value, scope, sema, span, diagnostics)
            {
                let offset =
                    u32::try_from(bytes.len()).expect("word expression offset should fit in u32");
                bytes.extend_from_slice(&[0, 0]);
                relocations.push(ByteRelocation {
                    offset,
                    kind,
                    label,
                });
                continue;
            }

            diagnostics.push(
                Diagnostic::error(
                    span,
                    "low/high-byte expression must be a constant or plain symbol reference",
                )
                .with_help("supported symbolic forms are '&<label' and '&>label'"),
            );
            return None;
        }

        // Bare ident that is a label: emit a full 16-bit word relocation.
        if let Some(symbol) = expr_ident_name(value) {
            if !sema.vars.contains_key(symbol) && !sema.consts.contains_key(symbol) {
                match resolve_symbolic_subscript_name(symbol, sema, span, diagnostics) {
                    Ok(Some(_)) | Err(()) => {}
                    Ok(None) => {
                        if let Some(resolved) = resolve_symbol(symbol, scope, span, diagnostics) {
                            let offset = u32::try_from(bytes.len())
                                .expect("word expression offset should fit in u32");
                            bytes.extend_from_slice(&[0, 0]);
                            relocations.push(ByteRelocation {
                                offset,
                                kind: ByteRelocationKind::FullWord,
                                label: resolved,
                            });
                            continue;
                        }
                        return None;
                    }
                }
            }
        }

        // Fallback: try general numeric evaluation.
        let Some(number) = eval_to_number(value, scope, sema, span, diagnostics) else {
            return None;
        };
        match u16::try_from(number) {
            Ok(word) => bytes.extend_from_slice(&word.to_le_bytes()),
            Err(_) => {
                diagnostics.push(Diagnostic::error(
                    span,
                    format!("word literal out of range: {number}"),
                ));
                return None;
            }
        }
    }

    Some(EvaluatedBytes { bytes, relocations })
}

fn evaluate_far_exprs(
    values: &[Expr],
    scope: Option<&str>,
    sema: &SemanticModel,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<EvaluatedBytes> {
    let mut bytes = Vec::with_capacity(values.len() * 3);
    let mut relocations = Vec::new();

    for value in values {
        // Try to resolve as a compile-time number (literal, const, var address, expr).
        if let Some(number) = eval_to_number_strict(value, sema, span, diagnostics) {
            if !(0..=0xFFFFFF).contains(&number) {
                diagnostics.push(Diagnostic::error(
                    span,
                    format!("far address literal out of range: {number}"),
                ));
                return None;
            }
            let le = (number as u32).to_le_bytes();
            bytes.extend_from_slice(&le[..3]);
            continue;
        }

        // &< / &> unary: emit single-byte relocation (same as in bytes context).
        if matches!(
            value,
            Expr::Unary {
                op: ExprUnaryOp::LowByte | ExprUnaryOp::HighByte,
                ..
            }
        ) {
            if let Some((kind, label)) =
                resolve_symbolic_byte_relocation(value, scope, sema, span, diagnostics)
            {
                let offset =
                    u32::try_from(bytes.len()).expect("far expression offset should fit in u32");
                bytes.extend_from_slice(&[0, 0, 0]);
                relocations.push(ByteRelocation {
                    offset,
                    kind,
                    label,
                });
                continue;
            }

            diagnostics.push(
                Diagnostic::error(
                    span,
                    "low/high-byte expression must be a constant or plain symbol reference",
                )
                .with_help("supported symbolic forms are '&<label' and '&>label'"),
            );
            return None;
        }

        // Bare ident that is a label: emit a full 24-bit far relocation.
        if let Some(symbol) = expr_ident_name(value) {
            if !sema.vars.contains_key(symbol) && !sema.consts.contains_key(symbol) {
                match resolve_symbolic_subscript_name(symbol, sema, span, diagnostics) {
                    Ok(Some(_)) | Err(()) => {}
                    Ok(None) => {
                        if let Some(resolved) = resolve_symbol(symbol, scope, span, diagnostics) {
                            let offset = u32::try_from(bytes.len())
                                .expect("far expression offset should fit in u32");
                            bytes.extend_from_slice(&[0, 0, 0]);
                            relocations.push(ByteRelocation {
                                offset,
                                kind: ByteRelocationKind::FullLong,
                                label: resolved,
                            });
                            continue;
                        }
                        return None;
                    }
                }
            }
        }

        // Fallback: try general numeric evaluation.
        let Some(number) = eval_to_number(value, scope, sema, span, diagnostics) else {
            return None;
        };
        if !(0..=0xFFFFFF).contains(&number) {
            diagnostics.push(Diagnostic::error(
                span,
                format!("far address literal out of range: {number}"),
            ));
            return None;
        }
        let le = (number as u32).to_le_bytes();
        bytes.extend_from_slice(&le[..3]);
    }

    Some(EvaluatedBytes { bytes, relocations })
}

fn eval_to_number_strict(
    expr: &Expr,
    sema: &SemanticModel,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<i64> {
    match expr {
        Expr::Number(value) => Some(*value),
        Expr::Ident(name) | Expr::IdentSpanned { name, .. } => {
            let ident_span = expr_ident_span(expr, span).unwrap_or(span);
            if let Some(var) = sema.vars.get(name) {
                return Some(i64::from(var.address));
            }
            if let Some(constant) = sema.consts.get(name) {
                return constant_to_exact_i64(
                    name,
                    constant.value,
                    ident_span,
                    diagnostics,
                    "numeric expression",
                );
            }
            match resolve_symbolic_subscript_name(name, sema, ident_span, diagnostics) {
                Ok(Some(ResolvedSymbolicSubscriptName::Aggregate { address, .. }))
                | Ok(Some(ResolvedSymbolicSubscriptName::Field { address, .. })) => {
                    Some(i64::from(address))
                }
                Ok(None) | Err(()) => None,
            }
        }
        Expr::EvalText(_) => {
            diagnostics.push(Diagnostic::error(
                span,
                "internal error: eval text should be expanded before lowering",
            ));
            None
        }
        Expr::Index { base, index } => eval_index_expr_strict(base, index, sema, span, diagnostics),
        Expr::Binary { op, lhs, rhs } => {
            let lhs = eval_to_number_strict(lhs, sema, span, diagnostics)?;
            let rhs = eval_to_number_strict(rhs, sema, span, diagnostics)?;
            match op {
                ExprBinaryOp::Add => lhs.checked_add(rhs),
                ExprBinaryOp::Sub => lhs.checked_sub(rhs),
            }
            .or_else(|| {
                diagnostics.push(Diagnostic::error(span, "arithmetic overflow"));
                None
            })
        }
        Expr::Unary { op, expr } => {
            let value = eval_to_number_strict(expr, sema, span, diagnostics)?;
            match op {
                ExprUnaryOp::LowByte => Some(value & 0xFF),
                ExprUnaryOp::HighByte => Some((value >> 8) & 0xFF),
            }
        }
        Expr::TypedView { expr, .. } => eval_to_number_strict(expr, sema, span, diagnostics),
    }
}

fn map_named_data_for_eval_error(error: EvaluatorError, span: Span) -> Diagnostic {
    match error {
        EvaluatorError::UnknownIdentifier { name, .. } => Diagnostic::error(
            span,
            format!("unknown identifier '{name}' in data for-eval expression"),
        )
        .with_help("define the identifier earlier or use the loop variable declared after 'for'"),
        EvaluatorError::UnknownFunction { name } => {
            Diagnostic::error(span, format!("unknown evaluator function '{name}'"))
        }
        EvaluatorError::DeferredFunction { name, reason } => Diagnostic::error(
            span,
            format!("evaluator function '{name}' is not supported in data for-eval expressions"),
        )
        .with_help(reason),
        EvaluatorError::BadArity {
            name,
            expected,
            got,
        } => Diagnostic::error(
            span,
            format!("function '{name}' expected {expected} arguments, got {got}"),
        ),
        EvaluatorError::InvalidAssignmentTarget => Diagnostic::error(
            span,
            "invalid assignment target in data for-eval expression",
        )
        .with_help("assign only to identifiers (for example: `NAME = expr`)"),
        EvaluatorError::IntegerRequired { op } => Diagnostic::error(
            span,
            format!("operator '{op}' requires exact integer operands"),
        ),
        EvaluatorError::DivisionByZero => Diagnostic::error(span, "division by zero"),
        EvaluatorError::Overflow => Diagnostic::error(span, "arithmetic overflow"),
        EvaluatorError::UnexpectedToken { column, token } => Diagnostic::error(
            span,
            format!("unexpected token {token} in data for-eval expression at column {column}"),
        ),
        EvaluatorError::UnexpectedEof => {
            Diagnostic::error(span, "unexpected end of data for-eval expression")
        }
        EvaluatorError::InvalidNumber { literal } => Diagnostic::error(
            span,
            format!("invalid number literal '{literal}' in data for-eval expression"),
        ),
        EvaluatorError::ArrayLiteralInNumericContext => Diagnostic::error(
            span,
            "array literal can only be assigned to an identifier or indexed",
        ),
    }
}

fn constant_to_exact_i64(
    name: &str,
    value: k816_eval::Number,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
    context: &str,
) -> Option<i64> {
    if let Some(integer) = value.to_i64_exact() {
        return Some(integer);
    }

    diagnostics.push(
        Diagnostic::error(
            span,
            format!("constant '{name}' must be an exact integer in this {context}"),
        )
        .with_help("remove fractional parts before using this constant in integer-only contexts"),
    );
    None
}

fn eval_index_expr_strict(
    base: &Expr,
    index: &Expr,
    sema: &SemanticModel,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<i64> {
    let symbolic_subscript_base_name = if let Some(name) = base_ident(base) {
        match resolve_symbolic_subscript_name(name, sema, span, diagnostics) {
            Ok(found) => found,
            Err(()) => return None,
        }
    } else {
        None
    };

    if let Some(resolved) = symbolic_subscript_base_name {
        match resolved {
            ResolvedSymbolicSubscriptName::Aggregate { base, .. } => {
                diagnostics.push(invalid_symbolic_subscript_aggregate_index_diagnostic(
                    &base, index, sema, span,
                ));
                return None;
            }
            ResolvedSymbolicSubscriptName::Field {
                base,
                field,
                address,
                data_width,
                count,
            } => {
                if count <= 1 {
                    diagnostics.push(Diagnostic::error(
                        span,
                        format!(
                            "symbolic subscript field '{}.{}' is not an array",
                            base, field
                        ),
                    ));
                    return None;
                }

                let Some(index_value) = eval_to_number_strict(index, sema, span, diagnostics)
                else {
                    diagnostics.push(Diagnostic::error(
                        span,
                        "symbolic subscript array index must be a constant numeric expression",
                    ));
                    return None;
                };
                if index_value < 0 {
                    diagnostics.push(Diagnostic::error(
                        span,
                        format!("index must be non-negative, found {index_value}"),
                    ));
                    return None;
                }
                let scale = match data_width {
                    DataWidth::Byte => 1_i64,
                    DataWidth::Word => 2_i64,
                    DataWidth::Far => 3_i64,
                };
                let byte_offset = index_value.checked_mul(scale).or_else(|| {
                    diagnostics.push(Diagnostic::error(span, "arithmetic overflow"));
                    None
                })?;
                return i64::from(address).checked_add(byte_offset).or_else(|| {
                    diagnostics.push(Diagnostic::error(span, "arithmetic overflow"));
                    None
                });
            }
        }
    }

    let base_value = eval_to_number_strict(base, sema, span, diagnostics)?;
    let index_value = eval_to_number_strict(index, sema, span, diagnostics)?;
    base_value.checked_add(index_value).or_else(|| {
        diagnostics.push(Diagnostic::error(span, "arithmetic overflow"));
        None
    })
}

fn resolve_symbolic_byte_relocation(
    expr: &Expr,
    scope: Option<&str>,
    sema: &SemanticModel,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<(ByteRelocationKind, String)> {
    let Expr::Unary { op, expr } = expr else {
        return None;
    };

    let kind = match op {
        ExprUnaryOp::LowByte => ByteRelocationKind::LowByte,
        ExprUnaryOp::HighByte => ByteRelocationKind::HighByte,
    };

    let Some(symbol) = expr_ident_name(expr.as_ref()) else {
        return None;
    };

    if sema.vars.contains_key(symbol) || sema.consts.contains_key(symbol) {
        return None;
    }

    match resolve_symbolic_subscript_name(symbol, sema, span, diagnostics) {
        Ok(Some(_)) | Err(()) => return None,
        Ok(None) => {}
    }

    let resolved = resolve_symbol(symbol, scope, span, diagnostics)?;
    Some((kind, resolved))
}

/// Determines whether an expression should be treated as an immediate value
/// (compile-time constant) rather than a memory address. Uses the semantic model
/// for deterministic classification instead of naming-convention heuristics.
fn is_immediate_expression(expr: &Expr, sema: &SemanticModel) -> bool {
    match expr {
        Expr::Number(_) => true,
        Expr::Ident(name) | Expr::IdentSpanned { name, .. } => sema.consts.contains_key(name),
        Expr::Binary { lhs, rhs, .. } => {
            is_immediate_expression(lhs, sema) && is_immediate_expression(rhs, sema)
        }
        Expr::Unary { .. } => true,
        Expr::Index { .. } | Expr::EvalText(_) => false,
        Expr::TypedView { expr, .. } => is_immediate_expression(expr, sema),
    }
}

#[derive(Debug, Clone)]
enum ResolvedSymbolicSubscriptName {
    Aggregate {
        base: String,
        address: u32,
    },
    Field {
        base: String,
        field: String,
        address: u32,
        data_width: DataWidth,
        count: u32,
    },
}

fn resolve_symbolic_subscript_name(
    name: &str,
    sema: &SemanticModel,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) -> Result<Option<ResolvedSymbolicSubscriptName>, ()> {
    if let Some(var) = sema.vars.get(name) {
        if var.symbolic_subscript.is_some() {
            return Ok(Some(ResolvedSymbolicSubscriptName::Aggregate {
                base: name.to_string(),
                address: var.address,
            }));
        }
        return Ok(None);
    }

    let Some((base_name, field_name)) = name.split_once('.') else {
        return Ok(None);
    };

    let Some(base_var) = sema.vars.get(base_name) else {
        return Ok(None);
    };
    let Some(symbolic_subscript) = base_var.symbolic_subscript.as_ref() else {
        return Ok(None);
    };

    let Some(field_meta) = symbolic_subscript.fields.get(field_name) else {
        let mut diagnostic = Diagnostic::error(
            span,
            format!("unknown symbolic subscript field '.{field_name}' on '{base_name}'"),
        );
        if let Some(suggestion) = suggest_symbolic_subscript_field(field_name, symbolic_subscript) {
            diagnostic = diagnostic.with_help(format!("did you mean '.{suggestion}'?"));
        }
        diagnostics.push(diagnostic);
        return Err(());
    };

    let Some(address) = base_var.address.checked_add(field_meta.offset) else {
        diagnostics.push(Diagnostic::error(
            span,
            format!("symbolic subscript field '{name}' address overflows address space"),
        ));
        return Err(());
    };

    Ok(Some(ResolvedSymbolicSubscriptName::Field {
        base: base_name.to_string(),
        field: field_name.to_string(),
        address,
        data_width: field_meta.data_width,
        count: field_meta.count,
    }))
}

fn suggest_symbolic_subscript_field<'a>(
    requested: &str,
    symbolic_subscript: &'a crate::sema::SymbolicSubscriptMeta,
) -> Option<&'a str> {
    symbolic_subscript
        .fields
        .keys()
        .map(|candidate| (candidate.as_str(), levenshtein(requested, candidate)))
        .min_by_key(|(_, distance)| *distance)
        .and_then(|(candidate, distance)| (distance <= 3).then_some(candidate))
}

fn invalid_symbolic_subscript_aggregate_index_diagnostic(
    base: &str,
    index: &Expr,
    sema: &SemanticModel,
    span: Span,
) -> Diagnostic {
    let mut diagnostic = Diagnostic::error(
        span,
        format!("invalid index on symbolic subscript array '{base}': use '.field' or '[.field]'"),
    );

    if let Some(requested) = expr_ident_name(index)
        && let Some(symbolic_subscript) = sema
            .vars
            .get(base)
            .and_then(|var| var.symbolic_subscript.as_ref())
        && let Some(suggestion) = suggest_symbolic_subscript_field(requested, symbolic_subscript)
    {
        diagnostic = diagnostic.with_help(format!("did you mean '.{suggestion}'?"));
        return diagnostic;
    }

    diagnostic.with_help("only named field access is allowed on symbolic subscript arrays")
}

fn levenshtein(lhs: &str, rhs: &str) -> usize {
    if lhs.is_empty() {
        return rhs.chars().count();
    }
    if rhs.is_empty() {
        return lhs.chars().count();
    }

    let lhs_chars = lhs.chars().collect::<Vec<_>>();
    let rhs_chars = rhs.chars().collect::<Vec<_>>();
    let mut costs = (0..=rhs_chars.len()).collect::<Vec<_>>();

    for (i, lhs_char) in lhs_chars.iter().enumerate() {
        let mut prev = costs[0];
        costs[0] = i + 1;
        for (j, rhs_char) in rhs_chars.iter().enumerate() {
            let saved = costs[j + 1];
            let replace = if lhs_char == rhs_char { prev } else { prev + 1 };
            let insert = costs[j] + 1;
            let delete = costs[j + 1] + 1;
            costs[j + 1] = replace.min(insert).min(delete);
            prev = saved;
        }
    }

    costs[rhs_chars.len()]
}

fn lower_instruction_and_push(
    instruction: &Instruction,
    scope: Option<&str>,
    sema: &SemanticModel,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
    ops: &mut Vec<Spanned<Op>>,
) -> bool {
    if let Some(op) = lower_instruction(instruction, scope, sema, span, diagnostics) {
        ops.push(Spanned::new(Op::Instruction(op), span));
        true
    } else {
        false
    }
}

fn lower_instruction(
    instruction: &Instruction,
    scope: Option<&str>,
    sema: &SemanticModel,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<InstructionOp> {
    let operand = match &instruction.operand {
        None => None,
        Some(Operand::Immediate {
            expr,
            explicit_hash,
        }) => {
            let eval_span = immediate_expr_span(expr, span, *explicit_hash);
            if let Some((kind, label)) =
                resolve_symbolic_byte_relocation(expr, scope, sema, eval_span, diagnostics)
            {
                Some(OperandOp::ImmediateByteRelocation { kind, label })
            } else {
                let value = eval_to_number(expr, scope, sema, eval_span, diagnostics)?;
                Some(OperandOp::Immediate(value))
            }
        }
        Some(Operand::Value {
            expr,
            force_far,
            index,
            addr_mode,
        }) => {
            let mode = lower_operand_mode(*addr_mode, *index);
            lower_address_operand(expr, scope, sema, span, diagnostics, *force_far, mode)
        }
        Some(Operand::Auto { expr }) => {
            if is_immediate_expression(expr, sema) {
                let eval_span = immediate_expr_span(expr, span, false);
                if let Some((kind, label)) =
                    resolve_symbolic_byte_relocation(expr, scope, sema, eval_span, diagnostics)
                {
                    Some(OperandOp::ImmediateByteRelocation { kind, label })
                } else {
                    let value = eval_to_number(expr, scope, sema, eval_span, diagnostics)?;
                    Some(OperandOp::Immediate(value))
                }
            } else {
                let mode = lower_operand_mode(OperandAddrMode::Direct, None);
                lower_address_operand(expr, scope, sema, span, diagnostics, false, mode)
            }
        }
    };

    Some(InstructionOp {
        mnemonic: instruction.mnemonic.clone(),
        operand,
    })
}

fn immediate_expr_span(expr: &Expr, instruction_span: Span, explicit_hash: bool) -> Span {
    match expr {
        Expr::IdentSpanned { start, end, .. } => {
            Span::new(instruction_span.source_id, *start, *end)
        }
        Expr::Ident(name) if explicit_hash => {
            let ident_start = instruction_span.end.saturating_sub(name.len());
            Span::new(
                instruction_span.source_id,
                ident_start,
                instruction_span.end,
            )
        }
        Expr::Unary {
            op: ExprUnaryOp::LowByte | ExprUnaryOp::HighByte,
            expr,
        } => match expr.as_ref() {
            Expr::IdentSpanned { start, end, .. } => {
                Span::new(instruction_span.source_id, *start, *end)
            }
            Expr::Ident(name) => {
                let ident_start = instruction_span.end.saturating_sub(name.len());
                Span::new(
                    instruction_span.source_id,
                    ident_start,
                    instruction_span.end,
                )
            }
            _ => instruction_span,
        },
        _ => instruction_span,
    }
}

fn lower_address_operand(
    expr: &Expr,
    scope: Option<&str>,
    sema: &SemanticModel,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
    force_far: bool,
    mode: AddressOperandMode,
) -> Option<OperandOp> {
    match expr {
        Expr::Number(value) => {
            let address = u32::try_from(*value).map_err(|_| {
                Diagnostic::error(span, format!("address cannot be negative: {value}"))
            });
            match address {
                Ok(address) => Some(OperandOp::Address {
                    value: AddressValue::Literal(address),
                    force_far,
                    mode,
                }),
                Err(diag) => {
                    diagnostics.push(diag);
                    None
                }
            }
        }
        Expr::Ident(name) => {
            resolve_operand_ident(name, scope, sema, span, diagnostics, force_far, mode)
        }
        Expr::IdentSpanned { name, .. } => {
            resolve_operand_ident(name, scope, sema, span, diagnostics, force_far, mode)
        }
        // Typed views should preserve unresolved labels for relocation-aware object flows.
        Expr::TypedView { expr, .. } => {
            lower_address_operand(expr, scope, sema, span, diagnostics, force_far, mode)
        }
        Expr::Index { .. } | Expr::Binary { .. } | Expr::Unary { .. } => {
            let Some(value) = eval_to_number(expr, scope, sema, span, diagnostics) else {
                return None;
            };
            let Ok(address) = u32::try_from(value) else {
                diagnostics.push(Diagnostic::error(
                    span,
                    format!("address cannot be negative: {value}"),
                ));
                return None;
            };
            Some(OperandOp::Address {
                value: AddressValue::Literal(address),
                force_far,
                mode,
            })
        }
        Expr::EvalText(_) => {
            diagnostics.push(Diagnostic::error(
                span,
                "internal error: eval text should be expanded before lowering",
            ));
            None
        }
    }
}

fn eval_to_number(
    expr: &Expr,
    scope: Option<&str>,
    sema: &SemanticModel,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<i64> {
    match expr {
        Expr::Number(value) => Some(*value),
        Expr::Ident(name) | Expr::IdentSpanned { name, .. } => {
            let ident_span = expr_ident_span(expr, span).unwrap_or(span);
            if let Some(var) = sema.vars.get(name) {
                return Some(i64::from(var.address));
            }
            if let Some(constant) = sema.consts.get(name) {
                return constant_to_exact_i64(
                    name,
                    constant.value,
                    ident_span,
                    diagnostics,
                    "numeric expression",
                );
            }
            match resolve_symbolic_subscript_name(name, sema, ident_span, diagnostics) {
                Ok(Some(ResolvedSymbolicSubscriptName::Aggregate { address, .. }))
                | Ok(Some(ResolvedSymbolicSubscriptName::Field { address, .. })) => {
                    return Some(i64::from(address));
                }
                Err(()) => return None,
                Ok(None) => {}
            }

            let _ = resolve_symbol(name, scope, ident_span, diagnostics)?;
            diagnostics.push(Diagnostic::error(
                ident_span,
                format!("unknown identifier '{name}' in numeric expression"),
            ));
            None
        }
        Expr::EvalText(_) => {
            diagnostics.push(Diagnostic::error(
                span,
                "internal error: eval text should be expanded before lowering",
            ));
            None
        }
        Expr::Index { base, index } => eval_index_expr(base, index, scope, sema, span, diagnostics),
        Expr::Binary { op, lhs, rhs } => {
            let lhs = eval_to_number(lhs, scope, sema, span, diagnostics)?;
            let rhs = eval_to_number(rhs, scope, sema, span, diagnostics)?;
            match op {
                ExprBinaryOp::Add => lhs.checked_add(rhs),
                ExprBinaryOp::Sub => lhs.checked_sub(rhs),
            }
            .or_else(|| {
                diagnostics.push(Diagnostic::error(span, "arithmetic overflow"));
                None
            })
        }
        Expr::Unary { op, expr } => {
            let value = eval_to_number(expr, scope, sema, span, diagnostics)?;
            match op {
                ExprUnaryOp::LowByte => Some(value & 0xFF),
                ExprUnaryOp::HighByte => Some((value >> 8) & 0xFF),
            }
        }
        Expr::TypedView { expr, .. } => eval_to_number(expr, scope, sema, span, diagnostics),
    }
}

fn eval_index_expr(
    base: &Expr,
    index: &Expr,
    scope: Option<&str>,
    sema: &SemanticModel,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<i64> {
    if let Some(name) = base_ident(base) {
        match resolve_symbolic_subscript_name(name, sema, span, diagnostics) {
            Ok(Some(ResolvedSymbolicSubscriptName::Aggregate { base, .. })) => {
                diagnostics.push(invalid_symbolic_subscript_aggregate_index_diagnostic(
                    &base, index, sema, span,
                ));
                return None;
            }
            Ok(Some(ResolvedSymbolicSubscriptName::Field {
                base,
                field,
                address,
                data_width,
                count,
            })) => {
                if count <= 1 {
                    diagnostics.push(Diagnostic::error(
                        span,
                        format!(
                            "symbolic subscript field '{}.{}' is not an array",
                            base, field
                        ),
                    ));
                    return None;
                }

                let Some(index_value) = eval_to_number_strict(index, sema, span, diagnostics)
                else {
                    diagnostics.push(Diagnostic::error(
                        span,
                        "symbolic subscript array index must be a constant numeric expression",
                    ));
                    return None;
                };
                if index_value < 0 {
                    diagnostics.push(Diagnostic::error(
                        span,
                        format!("index must be non-negative, found {index_value}"),
                    ));
                    return None;
                }
                let scale = match data_width {
                    DataWidth::Byte => 1_i64,
                    DataWidth::Word => 2_i64,
                    DataWidth::Far => 3_i64,
                };
                let byte_offset = index_value.checked_mul(scale).or_else(|| {
                    diagnostics.push(Diagnostic::error(span, "arithmetic overflow"));
                    None
                })?;
                return i64::from(address).checked_add(byte_offset).or_else(|| {
                    diagnostics.push(Diagnostic::error(span, "arithmetic overflow"));
                    None
                });
            }
            Ok(None) => {}
            Err(()) => return None,
        }
    }

    let base_value = eval_to_number(base, scope, sema, span, diagnostics)?;
    let index_value = eval_to_number(index, scope, sema, span, diagnostics)?;
    base_value.checked_add(index_value).or_else(|| {
        diagnostics.push(Diagnostic::error(span, "arithmetic overflow"));
        None
    })
}

fn lower_operand_mode(
    addr_mode: OperandAddrMode,
    index: Option<crate::ast::IndexRegister>,
) -> AddressOperandMode {
    match addr_mode {
        OperandAddrMode::Direct => AddressOperandMode::Direct {
            index: index.map(lower_index_register),
        },
        OperandAddrMode::Indirect => AddressOperandMode::Indirect,
        OperandAddrMode::IndexedIndirectX => AddressOperandMode::IndexedIndirectX,
        OperandAddrMode::IndirectIndexedY => AddressOperandMode::IndirectIndexedY,
    }
}

fn lower_index_register(index: crate::ast::IndexRegister) -> IndexRegister {
    match index {
        crate::ast::IndexRegister::X => IndexRegister::X,
        crate::ast::IndexRegister::Y => IndexRegister::Y,
    }
}

fn resolve_operand_ident(
    name: &str,
    scope: Option<&str>,
    sema: &SemanticModel,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
    force_far: bool,
    mode: AddressOperandMode,
) -> Option<OperandOp> {
    if name.starts_with('.') {
        let resolved = resolve_symbol(name, scope, span, diagnostics)?;
        return Some(OperandOp::Address {
            value: AddressValue::Label(resolved),
            force_far,
            mode,
        });
    }

    if let Some(var) = sema.vars.get(name) {
        return Some(OperandOp::Address {
            value: AddressValue::Literal(var.address),
            force_far,
            mode,
        });
    }
    if let Some(constant) = sema.consts.get(name) {
        let Some(value) = constant_to_exact_i64(
            name,
            constant.value,
            span,
            diagnostics,
            "address expression",
        ) else {
            return None;
        };
        let Ok(address) = u32::try_from(value) else {
            diagnostics.push(Diagnostic::error(
                span,
                format!("address cannot be negative: {value} (from const '{name}')"),
            ));
            return None;
        };
        return Some(OperandOp::Address {
            value: AddressValue::Literal(address),
            force_far,
            mode,
        });
    }

    match resolve_symbolic_subscript_name(name, sema, span, diagnostics) {
        Ok(Some(ResolvedSymbolicSubscriptName::Aggregate { address, .. }))
        | Ok(Some(ResolvedSymbolicSubscriptName::Field { address, .. })) => {
            return Some(OperandOp::Address {
                value: AddressValue::Literal(address),
                force_far,
                mode,
            });
        }
        Ok(None) => {}
        Err(()) => return None,
    }

    if sema.functions.contains_key(name) {
        return Some(OperandOp::Address {
            value: AddressValue::Label(name.to_string()),
            force_far,
            mode,
        });
    }

    Some(OperandOp::Address {
        value: AddressValue::Label(name.to_string()),
        force_far,
        mode,
    })
}

fn resolve_symbol(
    symbol: &str,
    scope: Option<&str>,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<String> {
    if let Some(local) = symbol.strip_prefix('.') {
        let Some(scope) = scope else {
            diagnostics.push(
                Diagnostic::error(span, "local labels require function/main scope")
                    .with_label(span, format!("label '.{local}' cannot be resolved here")),
            );
            return None;
        };
        return Some(format!("{scope}::.{}", local));
    }
    Some(symbol.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hir::{AddressOperandMode, AddressValue, ByteRelocationKind, OperandOp};
    use crate::parser::parse;
    use crate::sema::analyze;
    use crate::span::SourceId;
    use k816_assets::StdAssetFS;

    #[test]
    fn resolves_var_operand_to_literal_address() {
        let source = "var target = 0x1234\nfunc main {\n  lda target\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs).expect("lower");

        let operand = program
            .ops
            .iter()
            .find_map(|op| match &op.node {
                Op::Instruction(instruction) if instruction.mnemonic == "lda" => {
                    instruction.operand.as_ref()
                }
                _ => None,
            })
            .expect("lda operand");

        match operand {
            OperandOp::Address {
                value,
                force_far,
                mode,
            } => {
                assert!(!force_far);
                assert_eq!(*mode, AddressOperandMode::Direct { index: None });
                assert!(matches!(value, AddressValue::Literal(0x1234)));
            }
            _ => panic!("expected address operand"),
        }
    }

    #[test]
    fn resolves_const_operand_to_immediate_value() {
        let source = "const LIMIT = 0x34\nfunc main {\n  lda #LIMIT\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs).expect("lower");

        let operand = program
            .ops
            .iter()
            .find_map(|op| match &op.node {
                Op::Instruction(instruction) if instruction.mnemonic == "lda" => {
                    instruction.operand.as_ref()
                }
                _ => None,
            })
            .expect("lda operand");

        assert!(matches!(operand, OperandOp::Immediate(0x34)));
    }

    #[test]
    fn resolves_top_level_evaluator_constant_to_immediate_value() {
        let source = "[ LIMIT = 0x34 ]\nfunc main {\n  lda #LIMIT\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs).expect("lower");

        let operand = program
            .ops
            .iter()
            .find_map(|op| match &op.node {
                Op::Instruction(instruction) if instruction.mnemonic == "lda" => {
                    instruction.operand.as_ref()
                }
                _ => None,
            })
            .expect("lda operand");

        assert!(matches!(operand, OperandOp::Immediate(0x34)));
    }

    #[test]
    fn lowers_immediate_address_byte_symbols_to_relocations() {
        let source = "data displist {\n  0\n}\nfunc dli {\n  rti\n}\nfunc main {\n  a=&<displist\n  a=&>displist\n  a=&<dli\n  a=&>dli\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs).expect("lower");

        let lda_operands = program
            .ops
            .iter()
            .filter_map(|op| match &op.node {
                Op::Instruction(instruction) if instruction.mnemonic == "lda" => {
                    instruction.operand.as_ref()
                }
                _ => None,
            })
            .collect::<Vec<_>>();
        assert_eq!(lda_operands.len(), 4);

        assert!(matches!(
            lda_operands[0],
            OperandOp::ImmediateByteRelocation {
                kind: ByteRelocationKind::LowByte,
                label
            } if label == "displist"
        ));
        assert!(matches!(
            lda_operands[1],
            OperandOp::ImmediateByteRelocation {
                kind: ByteRelocationKind::HighByte,
                label
            } if label == "displist"
        ));
        assert!(matches!(
            lda_operands[2],
            OperandOp::ImmediateByteRelocation {
                kind: ByteRelocationKind::LowByte,
                label
            } if label == "dli"
        ));
        assert!(matches!(
            lda_operands[3],
            OperandOp::ImmediateByteRelocation {
                kind: ByteRelocationKind::HighByte,
                label
            } if label == "dli"
        ));
    }

    #[test]
    fn reports_unknown_identifier_in_numeric_expression() {
        let source = "func main {\n  lda #MISSING\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let errors = lower(&file, &sema, &fs).expect_err("must fail");

        let missing_start = source.find("MISSING").expect("MISSING start");
        let missing_end = missing_start + "MISSING".len();

        assert!(errors.iter().any(|error| {
            error
                .message
                .contains("unknown identifier 'MISSING' in numeric expression")
        }));
        assert!(errors.iter().any(|error| {
            error.primary.start == missing_start && error.primary.end == missing_end
        }));
    }

    #[test]
    fn keeps_unresolved_identifier_as_label_operand() {
        let source = "func main {\n  lda missing\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs).expect("lower");
        let operand = program
            .ops
            .iter()
            .find_map(|op| match &op.node {
                Op::Instruction(instruction) if instruction.mnemonic == "lda" => {
                    instruction.operand.as_ref()
                }
                _ => None,
            })
            .expect("lda operand");
        assert!(matches!(
            operand,
            OperandOp::Address {
                value: AddressValue::Label(name),
                ..
            } if name == "missing"
        ));
    }

    #[test]
    fn keeps_unresolved_typed_view_identifier_as_label_operand() {
        let source = "func test @a8 {\n  lda regs.status:byte\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs).expect("lower");
        let operand = program
            .ops
            .iter()
            .find_map(|op| match &op.node {
                Op::Instruction(instruction) if instruction.mnemonic == "lda" => {
                    instruction.operand.as_ref()
                }
                _ => None,
            })
            .expect("lda operand");
        assert!(matches!(
            operand,
            OperandOp::Address {
                value: AddressValue::Label(name),
                ..
            } if name == "regs.status"
        ));
    }

    #[test]
    fn emits_absolute_symbols_for_vars_and_symbolic_fields() {
        let source = "var foo = 0x1234\nvar regs[\n  .ctrl:word\n  .status:byte\n] = 0x2100\nfunc main {\n  nop\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs).expect("lower");

        let mut symbols = std::collections::BTreeMap::new();
        for op in &program.ops {
            let Op::DefineAbsoluteSymbol { name, address } = &op.node else {
                continue;
            };
            symbols.insert(name.clone(), *address);
        }

        assert_eq!(symbols.get("foo"), Some(&0x1234));
        assert_eq!(symbols.get("regs"), Some(&0x2100));
        assert_eq!(symbols.get("regs.ctrl"), Some(&0x2100));
        assert_eq!(symbols.get("regs.status"), Some(&0x2102));
    }

    #[test]
    fn resolves_symbolic_subscript_field_accesses_to_literal_addresses() {
        let source = "var foo[\n  .field_w:byte\n  .idx:byte\n  .string[4]:byte\n] = 0x1234\nfunc main {\n  lda foo.field_w\n  sta foo[.idx]\n  lda foo.string[2]\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs).expect("lower");

        let mut lda_literals = Vec::new();
        let mut sta_literals = Vec::new();
        for op in &program.ops {
            let Op::Instruction(instruction) = &op.node else {
                continue;
            };
            let Some(OperandOp::Address {
                value: AddressValue::Literal(value),
                ..
            }) = instruction.operand.as_ref()
            else {
                continue;
            };

            match instruction.mnemonic.as_str() {
                "lda" => lda_literals.push(*value),
                "sta" => sta_literals.push(*value),
                _ => {}
            }
        }

        assert_eq!(lda_literals, vec![0x1234, 0x1238]);
        assert_eq!(sta_literals, vec![0x1235]);
    }

    #[test]
    fn rejects_numeric_indexing_on_symbolic_subscript_aggregate() {
        let source = "var foo[\n  .idx:byte\n] = 0x1234\nfunc main {\n  lda foo[1]\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let errors = lower(&file, &sema, &fs).expect_err("must fail");

        assert!(errors.iter().any(|error| {
            error
                .message
                .contains("invalid index on symbolic subscript array 'foo'")
        }));
    }

    #[test]
    fn suggests_field_for_symbolic_subscript_aggregate_expression_index() {
        let source =
            "var foo[\n  .idx:byte\n  .status:byte\n] = 0x1234\nfunc main {\n  lda foo[idx]\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let errors = lower(&file, &sema, &fs).expect_err("must fail");

        assert!(errors.iter().any(|error| {
            error
                .message
                .contains("invalid index on symbolic subscript array 'foo'")
        }));
        assert!(errors.iter().any(|error| {
            error.supplements.iter().any(|supplement| {
                matches!(
                    supplement,
                    crate::diag::Supplemental::Help(help) if help.contains("did you mean '.idx'")
                )
            })
        }));
    }

    #[test]
    fn keeps_generic_help_for_symbolic_subscript_aggregate_index_without_similar_field() {
        let source = "var foo[\n  .idx:byte\n  .status:byte\n] = 0x1234\nfunc main {\n  lda foo[buffer]\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let errors = lower(&file, &sema, &fs).expect_err("must fail");

        assert!(errors.iter().any(|error| {
            error.supplements.iter().any(|supplement| {
                matches!(
                    supplement,
                    crate::diag::Supplemental::Help(help)
                        if help.contains("only named field access is allowed on symbolic subscript arrays")
                )
            })
        }));
        assert!(!errors.iter().any(|error| {
            error.supplements.iter().any(|supplement| {
                matches!(
                    supplement,
                    crate::diag::Supplemental::Help(help) if help.contains("did you mean '.")
                )
            })
        }));
    }

    #[test]
    fn reports_unknown_symbolic_subscript_field_with_suggestion() {
        let source =
            "var foo[\n  .idx:byte\n  .status:byte\n] = 0x1234\nfunc main {\n  lda foo.idz\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let errors = lower(&file, &sema, &fs).expect_err("must fail");

        assert!(errors.iter().any(|error| {
            error
                .message
                .contains("unknown symbolic subscript field '.idz'")
        }));
        assert!(errors.iter().any(|error| {
            error.supplements.iter().any(|supplement| {
                matches!(
                    supplement,
                    crate::diag::Supplemental::Help(help) if help.contains("did you mean '.idx'")
                )
            })
        }));
    }

    #[test]
    fn named_data_label_is_emitted_after_leading_segment_directive() {
        let source = "data info {\n  segment INFO\n  \"A\"\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs).expect("lower");

        let segment_index = program
            .ops
            .iter()
            .position(|op| matches!(&op.node, Op::SelectSegment(name) if name == "INFO"))
            .expect("segment select for INFO");
        let label_index = program
            .ops
            .iter()
            .position(|op| matches!(&op.node, Op::Label(name) if name == "info"))
            .expect("label for info");
        let emit_index = program
            .ops
            .iter()
            .position(|op| matches!(&op.node, Op::EmitBytes(bytes) if bytes == b"A"))
            .expect("emit for data payload");

        assert!(segment_index < label_index);
        assert!(label_index < emit_index);
    }

    #[test]
    fn named_data_label_is_emitted_after_leading_align_directive() {
        let source = "data bytes {\n  align 16\n  1\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs).expect("lower");

        let align_index = program
            .ops
            .iter()
            .position(|op| matches!(&op.node, Op::Align { boundary: 16, .. }))
            .expect("align directive");
        let label_index = program
            .ops
            .iter()
            .position(|op| matches!(&op.node, Op::Label(name) if name == "bytes"))
            .expect("label for bytes");
        let emit_index = program
            .ops
            .iter()
            .position(|op| matches!(&op.node, Op::EmitBytes(bytes) if bytes == &[1]))
            .expect("data byte emission");

        assert!(align_index < label_index);
        assert!(label_index < emit_index);
    }

    #[test]
    fn lowers_named_data_for_eval_range_entries() {
        let source = "[ FACTOR = 3 ]\ndata table {\n  for i=0..4 eval [ i * FACTOR ]\n  for j=4..0 eval [ j ]\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs).expect("lower");

        let emitted = program
            .ops
            .iter()
            .filter_map(|op| match &op.node {
                Op::EmitBytes(bytes) => Some(bytes.clone()),
                _ => None,
            })
            .collect::<Vec<_>>();

        assert_eq!(emitted.len(), 2);
        assert_eq!(emitted[0], vec![0, 3, 6, 9, 12]);
        assert_eq!(emitted[1], vec![4, 3, 2, 1, 0]);
    }

    #[test]
    fn lowers_wait_loop_bit_pattern_to_bit_and_bpl() {
        let source = "var ready = 0x1234\nfunc main {\n  { a&?ready } n-?\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs).expect("lower");

        let mnemonics = program
            .ops
            .iter()
            .filter_map(|op| match &op.node {
                Op::Instruction(instruction) => Some(instruction.mnemonic.as_str()),
                _ => None,
            })
            .collect::<Vec<_>>();

        assert!(mnemonics.windows(2).any(|pair| pair == ["bit", "bpl"]));
        assert!(!mnemonics.contains(&"lda"));

        let bit_operand = program
            .ops
            .iter()
            .find_map(|op| match &op.node {
                Op::Instruction(instruction) if instruction.mnemonic == "bit" => {
                    instruction.operand.as_ref()
                }
                _ => None,
            })
            .expect("bit operand");

        assert!(matches!(
            bit_operand,
            OperandOp::Address {
                value: AddressValue::Literal(0x1234),
                force_far: false,
                mode: AddressOperandMode::Direct { index: None }
            }
        ));
    }

    #[test]
    fn lowers_postfix_n_flag_close_without_cmp() {
        let source = "var ready = 0x1234\nfunc main {\n  {\n    a=ready\n  } n-?\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs).expect("lower");

        let mnemonics = program
            .ops
            .iter()
            .filter_map(|op| match &op.node {
                Op::Instruction(instruction) => Some(instruction.mnemonic.as_str()),
                _ => None,
            })
            .collect::<Vec<_>>();

        assert!(mnemonics.windows(2).any(|pair| pair == ["lda", "bpl"]));
        assert!(!mnemonics.contains(&"cmp"));
    }

    #[test]
    fn lowers_postfix_le_to_two_direct_branches() {
        let source = "func main {\n  {\n    x++\n  } <=\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs).expect("lower");

        let mnemonics = program
            .ops
            .iter()
            .filter_map(|op| match &op.node {
                Op::Instruction(instruction) => Some(instruction.mnemonic.as_str()),
                _ => None,
            })
            .collect::<Vec<_>>();

        assert!(mnemonics.windows(2).any(|pair| pair == ["bcc", "beq"]));
        assert!(!mnemonics.contains(&"cmp"));
    }

    #[test]
    fn lowers_postfix_gt_to_three_branch_sequence() {
        let source = "func main {\n  {\n    x++\n  } >\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs).expect("lower");

        let mnemonics = program
            .ops
            .iter()
            .filter_map(|op| match &op.node {
                Op::Instruction(instruction) => Some(instruction.mnemonic.as_str()),
                _ => None,
            })
            .collect::<Vec<_>>();

        assert!(mnemonics.windows(3).any(|seq| seq == ["beq", "bcc", "bra"]));
        assert!(!mnemonics.contains(&"cmp"));
    }

    #[test]
    fn segment_inside_named_data_block_restores_outer_segment() {
        let source =
            "segment fixed_lo\ndata vectors {\n  segment fixed_hi\n  1\n}\ndata tail {\n  2\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs).expect("lower");

        let fixed_hi_index = program
            .ops
            .iter()
            .position(|op| matches!(&op.node, Op::SelectSegment(name) if name == "fixed_hi"))
            .expect("fixed_hi segment select");
        let fixed_lo_indices = program
            .ops
            .iter()
            .enumerate()
            .filter_map(|(idx, op)| {
                matches!(&op.node, Op::SelectSegment(name) if name == "fixed_lo").then_some(idx)
            })
            .collect::<Vec<_>>();
        assert!(
            fixed_lo_indices.len() >= 2,
            "expected top-level + restore selects"
        );
        assert!(fixed_lo_indices[1] > fixed_hi_index);

        let tail_label_index = program
            .ops
            .iter()
            .position(|op| matches!(&op.node, Op::Label(name) if name == "tail"))
            .expect("tail label");
        assert!(fixed_lo_indices[1] < tail_label_index);
    }

    #[test]
    fn segment_inside_code_block_restores_outer_segment() {
        let source =
            "segment fixed_lo\nfunc main {\n  segment fixed_hi\n  nop\n}\ndata tail {\n  2\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs).expect("lower");

        let fixed_hi_index = program
            .ops
            .iter()
            .position(|op| matches!(&op.node, Op::SelectSegment(name) if name == "fixed_hi"))
            .expect("fixed_hi segment select");
        let function_end_index = program
            .ops
            .iter()
            .position(|op| matches!(&op.node, Op::FunctionEnd))
            .expect("function end");
        let restore_index = program
            .ops
            .iter()
            .enumerate()
            .filter_map(|(idx, op)| {
                matches!(&op.node, Op::SelectSegment(name) if name == "fixed_lo").then_some(idx)
            })
            .find(|idx| *idx > function_end_index)
            .expect("restore segment fixed_lo after function");
        let tail_label_index = program
            .ops
            .iter()
            .position(|op| matches!(&op.node, Op::Label(name) if name == "tail"))
            .expect("tail label");

        assert!(fixed_hi_index < function_end_index);
        assert!(function_end_index < restore_index);
        assert!(restore_index < tail_label_index);
    }

    #[test]
    fn segment_inside_func_block_restores_outer_segment() {
        let source =
            "segment fixed_lo\nfunc worker {\n  segment fixed_hi\n  nop\n}\ndata tail {\n  2\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs).expect("lower");

        let fixed_hi_index = program
            .ops
            .iter()
            .position(|op| matches!(&op.node, Op::SelectSegment(name) if name == "fixed_hi"))
            .expect("fixed_hi segment select");
        let function_end_index = program
            .ops
            .iter()
            .position(|op| matches!(&op.node, Op::FunctionEnd))
            .expect("function end");
        let restore_index = program
            .ops
            .iter()
            .enumerate()
            .filter_map(|(idx, op)| {
                matches!(&op.node, Op::SelectSegment(name) if name == "fixed_lo").then_some(idx)
            })
            .find(|idx| *idx > function_end_index)
            .expect("restore segment fixed_lo after function");
        let tail_label_index = program
            .ops
            .iter()
            .position(|op| matches!(&op.node, Op::Label(name) if name == "tail"))
            .expect("tail label");

        assert!(fixed_hi_index < function_end_index);
        assert!(function_end_index < restore_index);
        assert!(restore_index < tail_label_index);
    }

    #[test]
    fn segment_inside_naked_block_restores_outer_segment() {
        let source =
            "segment fixed_lo\nnaked worker {\n  segment fixed_hi\n  nop\n}\ndata tail {\n  2\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs).expect("lower");

        let fixed_hi_index = program
            .ops
            .iter()
            .position(|op| matches!(&op.node, Op::SelectSegment(name) if name == "fixed_hi"))
            .expect("fixed_hi segment select");
        let function_end_index = program
            .ops
            .iter()
            .position(|op| matches!(&op.node, Op::FunctionEnd))
            .expect("function end");
        let restore_index = program
            .ops
            .iter()
            .enumerate()
            .filter_map(|(idx, op)| {
                matches!(&op.node, Op::SelectSegment(name) if name == "fixed_lo").then_some(idx)
            })
            .find(|idx| *idx > function_end_index)
            .expect("restore segment fixed_lo after naked block");
        let tail_label_index = program
            .ops
            .iter()
            .position(|op| matches!(&op.node, Op::Label(name) if name == "tail"))
            .expect("tail label");

        assert!(fixed_hi_index < function_end_index);
        assert!(function_end_index < restore_index);
        assert!(restore_index < tail_label_index);
    }
}
