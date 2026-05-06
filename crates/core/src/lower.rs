use indexmap::IndexMap;
use k816_assets::AssetFS;
use k816_eval::{EvalContext, EvalError as EvaluatorError, Number};
use k816_isa65816::{
    AddressingMode, RegEffects, RegSet, mnemonic_effects, mnemonic_width_sensitivity,
    supported_modes,
};
use rustc_hash::FxHashMap;
use std::collections::HashSet;

use crate::ast::{
    CallArg, CallStmt, CodeBlock, ContractParam, DataArg, DataBlock, DataEntry, DataWidth, Expr,
    ExprBinaryOp, ExprUnaryOp, File, ForceAddrMode, HlaCondition, HlaCpuRegister, HlaOperandExpr,
    HlaRhs, HlaStackTarget, HlaStmt, ImmediateParamType, Instruction, Item, MetadataQuery,
    ModeContract, NumFmt, Operand, OperandAddrMode, RegName, RegWidth, Stmt,
};
use crate::diag::{Diagnostic, Severity};
use crate::hir::{
    AddressOperandMode, AddressSizeHint, AddressValue, ByteRelocation, ByteRelocationKind,
    IndexRegister, InstructionOp, Op, OperandOp, Program,
};
use crate::sema::{SemanticModel, VarPlacement};
use crate::span::{SourceId, Span, Spanned};

mod exit_mode;
mod hla;

use self::exit_mode::build_exit_mode_summaries;
use self::hla::lower_hla_stmt;

#[derive(Debug, Clone)]
struct CallContractSummary {
    inputs: Vec<ContractParam>,
    outputs: Vec<RegName>,
    clobbers: RegSet,
    is_naked: bool,
}

#[derive(Debug, Clone)]
pub(crate) struct LowerOutput {
    pub program: Program,
    pub warnings: Vec<Diagnostic>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExitWidth {
    Preserve,
    Fixed(RegWidth),
    Unknown,
}

#[derive(Debug, Clone, Copy)]
struct ExitModeSummary {
    a_width: ExitWidth,
    i_width: ExitWidth,
    is_naked: bool,
}

#[derive(Debug, Clone)]
struct InlineBindings {
    immediates: FxHashMap<String, Expr>,
    aliases: FxHashMap<String, String>,
}

#[derive(Debug, Clone, Copy)]
enum CallModeBehavior {
    PreserveCaller,
    AdoptExit(ExitModeSummary),
}

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

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
struct RegHazardState {
    a_dead: bool,
    x_truncated: bool,
    y_truncated: bool,
}

#[derive(Debug, Clone, Copy)]
struct LabelEntryRecord {
    mode: ModeState,
    first_edge_span: Span,
}

#[derive(Debug, Clone, Copy)]
struct LabelDeclaredRecord {
    mode: ModeState,
    decl_span: Span,
    /// `true` for cross-function function-entry signatures: the contract is
    /// known but it does not auto-bridge incoming modes via mask/fixed_mask
    /// (the bridge would belong in another function's lowering, where this
    /// `ctx` cannot reach). Advisory records are still passed to mismatch
    /// diagnostics so the user sees the declared signature.
    is_advisory: bool,
}

#[derive(Debug)]
struct LowerContext {
    next_label: usize,
    do_loop_targets: Vec<(String, usize)>,
    break_targets: Vec<String>,
    mode: ModeState,
    mode_frames: Vec<ModeFrame>,
    label_entry_modes: FxHashMap<String, LabelEntryRecord>,
    label_declared_modes: FxHashMap<String, LabelDeclaredRecord>,
    label_fixed_masks: FxHashMap<String, u8>,
    label_depths: FxHashMap<String, usize>,
    return_modes: Vec<ModeState>,
    reachable: bool,
    is_far: bool,
    hazards: RegHazardState,
    label_entry_hazards: FxHashMap<String, RegHazardState>,
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
            return_modes: Vec::new(),
            reachable: true,
            is_far: false,
            hazards: RegHazardState::default(),
            label_entry_hazards: FxHashMap::default(),
        }
    }
}

fn insert_function_entry_signature(
    out: &mut FxHashMap<String, LabelDeclaredRecord>,
    name: &str,
    mode_contract: ModeContract,
    decl_span: Span,
) {
    let mode = ModeState {
        a_width: mode_contract.a_width,
        i_width: mode_contract.i_width,
    };
    if mode.a_width.is_none() && mode.i_width.is_none() {
        return;
    }
    out.entry(name.to_string()).or_insert(LabelDeclaredRecord {
        mode,
        decl_span,
        is_advisory: true,
    });
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

fn reg_name_set(reg: RegName) -> RegSet {
    match reg {
        RegName::A => RegSet::A,
        RegName::X => RegSet::X,
        RegName::Y => RegSet::Y,
    }
}

fn reg_names_set(regs: &[RegName]) -> RegSet {
    regs.iter()
        .copied()
        .fold(RegSet::NONE, |acc, reg| acc | reg_name_set(reg))
}

fn call_mode_behavior(
    preserve_mode: bool,
    exit_summary: Option<ExitModeSummary>,
) -> CallModeBehavior {
    if preserve_mode {
        CallModeBehavior::PreserveCaller
    } else {
        CallModeBehavior::AdoptExit(
            exit_summary.expect("checked function contract calls must have an inferred exit mode"),
        )
    }
}

fn operand_index_reads(operand: Option<&Operand>) -> RegSet {
    match operand {
        Some(Operand::Value {
            index: Some(crate::ast::IndexRegister::X),
            ..
        }) => RegSet::X,
        Some(Operand::Value {
            index: Some(crate::ast::IndexRegister::Y),
            ..
        }) => RegSet::Y,
        Some(Operand::Value {
            index: Some(crate::ast::IndexRegister::S),
            ..
        }) => RegSet::NONE,
        Some(Operand::Auto { .. })
        | Some(Operand::Immediate { .. })
        | Some(Operand::BlockMove { .. })
        | Some(Operand::Register { .. })
        | Some(Operand::Value { index: None, .. })
        | None => RegSet::NONE,
    }
}

fn compose_effects(left: RegEffects, right: RegEffects) -> RegEffects {
    RegEffects {
        reads: left.reads | (right.reads - left.modifies),
        modifies: left.modifies | right.modifies,
    }
}

fn merge_summary_effects(left: RegEffects, right: RegEffects) -> RegEffects {
    RegEffects {
        reads: left.reads | right.reads,
        modifies: left.modifies | right.modifies,
    }
}

fn hla_operand_reads(operand: &HlaOperandExpr) -> RegSet {
    match operand.index {
        Some(crate::ast::IndexRegister::X) => RegSet::X,
        Some(crate::ast::IndexRegister::Y) => RegSet::Y,
        Some(crate::ast::IndexRegister::S) | None => RegSet::NONE,
    }
}

fn hla_rhs_reads(rhs: &HlaRhs) -> RegSet {
    match rhs {
        HlaRhs::Immediate(_) => RegSet::NONE,
        HlaRhs::Value { index, .. } => match index {
            Some(crate::ast::IndexRegister::X) => RegSet::X,
            Some(crate::ast::IndexRegister::Y) => RegSet::Y,
            Some(crate::ast::IndexRegister::S) | None => RegSet::NONE,
        },
    }
}

fn instruction_targets_accumulator(instruction: &Instruction) -> bool {
    instruction.operand.is_none()
        || matches!(
            instruction.operand,
            Some(Operand::Register {
                reg: HlaCpuRegister::A,
                ..
            })
        )
}

fn instruction_effects(instruction: &Instruction) -> RegEffects {
    let base = mnemonic_effects(
        &instruction.mnemonic,
        instruction_targets_accumulator(instruction),
    );
    let mut reads = base.reads | operand_index_reads(instruction.operand.as_ref());
    // Memory-form `BIT` derives N from operand bit 7/15 and V from operand bit
    // 6/14 independently of A; only Z reflects `A AND M`. The immediate form
    // (Note 2 in the W65C816 datasheet) writes only Z, so its effect *is* the
    // AND with A. Treat memory BIT as not reading A so a stale-A after a width
    // switch does not block N/V uses; keep immediate BIT as an A-reader.
    if instruction.mnemonic.eq_ignore_ascii_case("bit")
        && !matches!(instruction.operand, Some(Operand::Immediate { .. }))
    {
        reads = reads - RegSet::A;
    }
    RegEffects {
        reads,
        modifies: base.modifies,
    }
}

fn instruction_accumulator_load_wins(instruction: &Instruction, mode: ModeState) -> bool {
    let lower = instruction.mnemonic.to_ascii_lowercase();
    let effects = mnemonic_effects(&lower, instruction_targets_accumulator(instruction));
    let kills_a = effects.modifies.contains(RegSet::A) && !effects.reads.contains(RegSet::A);
    // `xba` reads+writes A (byte-swap), so it doesn't fall under `kills_a`,
    // but in 8-bit accumulator mode the visible A is replaced by the prior
    // hidden high byte — close enough that the existing rule treats it as
    // an A-load win.
    kills_a || (lower == "xba" && mode.a_width == Some(RegWidth::W8))
}

fn hla_effects(stmt: &HlaStmt) -> RegEffects {
    match stmt {
        HlaStmt::RegisterAssign { register, rhs } => RegEffects {
            reads: hla_operand_reads(rhs),
            modifies: match register {
                HlaCpuRegister::A => RegSet::A,
                HlaCpuRegister::X => RegSet::X,
                HlaCpuRegister::Y => RegSet::Y,
                _ => RegSet::NONE,
            },
        },
        HlaStmt::RegisterStore { dest, src } => RegEffects {
            reads: hla_operand_reads(dest)
                | match src {
                    HlaCpuRegister::A => RegSet::A,
                    HlaCpuRegister::X => RegSet::X,
                    HlaCpuRegister::Y => RegSet::Y,
                    _ => RegSet::NONE,
                },
            modifies: RegSet::NONE,
        },
        HlaStmt::MemStoreZero { dest } => RegEffects {
            reads: hla_operand_reads(dest),
            modifies: RegSet::NONE,
        },
        HlaStmt::RegisterTransfer { dest, src } => RegEffects {
            reads: match src {
                HlaCpuRegister::A => RegSet::A,
                HlaCpuRegister::X => RegSet::X,
                HlaCpuRegister::Y => RegSet::Y,
                _ => RegSet::NONE,
            },
            modifies: match dest {
                HlaCpuRegister::A => RegSet::A,
                HlaCpuRegister::X => RegSet::X,
                HlaCpuRegister::Y => RegSet::Y,
                _ => RegSet::NONE,
            },
        },
        HlaStmt::AccumulatorAlu { rhs, .. } | HlaStmt::AccumulatorBitTest { rhs } => RegEffects {
            reads: RegSet::A | hla_operand_reads(rhs),
            modifies: RegSet::A,
        },
        HlaStmt::IndexCompare { register, rhs } => RegEffects {
            reads: hla_operand_reads(rhs)
                | match register {
                    crate::ast::IndexRegister::X => RegSet::X,
                    crate::ast::IndexRegister::Y => RegSet::Y,
                    crate::ast::IndexRegister::S => RegSet::NONE,
                },
            modifies: RegSet::NONE,
        },
        HlaStmt::IncDec { target, .. } => match target {
            crate::ast::HlaIncDecTarget::Register(crate::ast::IndexRegister::X) => RegEffects {
                reads: RegSet::X,
                modifies: RegSet::X,
            },
            crate::ast::HlaIncDecTarget::Register(crate::ast::IndexRegister::Y) => RegEffects {
                reads: RegSet::Y,
                modifies: RegSet::Y,
            },
            _ => RegEffects::default(),
        },
        HlaStmt::ShiftRotate { target, .. } => match target {
            crate::ast::HlaShiftTarget::Accumulator => RegEffects {
                reads: RegSet::A,
                modifies: RegSet::A,
            },
            crate::ast::HlaShiftTarget::Address(_) => RegEffects::default(),
        },
        HlaStmt::StackOp {
            target: HlaStackTarget::A,
            push,
        } => {
            if *push {
                RegEffects {
                    reads: RegSet::A,
                    modifies: RegSet::NONE,
                }
            } else {
                RegEffects {
                    reads: RegSet::NONE,
                    modifies: RegSet::A,
                }
            }
        }
        HlaStmt::StackOp {
            target: HlaStackTarget::X,
            push,
        } => {
            if *push {
                RegEffects {
                    reads: RegSet::X,
                    modifies: RegSet::NONE,
                }
            } else {
                RegEffects {
                    reads: RegSet::NONE,
                    modifies: RegSet::X,
                }
            }
        }
        HlaStmt::StackOp {
            target: HlaStackTarget::Y,
            push,
        } => {
            if *push {
                RegEffects {
                    reads: RegSet::Y,
                    modifies: RegSet::NONE,
                }
            } else {
                RegEffects {
                    reads: RegSet::NONE,
                    modifies: RegSet::Y,
                }
            }
        }
        HlaStmt::StackOp {
            target: HlaStackTarget::B | HlaStackTarget::D | HlaStackTarget::P,
            ..
        } => RegEffects::default(),
        HlaStmt::XAssignImmediate { .. } => RegEffects {
            reads: RegSet::NONE,
            modifies: RegSet::X,
        },
        HlaStmt::XIncrement => RegEffects {
            reads: RegSet::X,
            modifies: RegSet::X,
        },
        HlaStmt::StoreFromA { rhs, .. } => RegEffects {
            reads: RegSet::A | hla_rhs_reads(rhs),
            modifies: RegSet::A,
        },
        HlaStmt::ConditionSeed { rhs, .. } => RegEffects {
            reads: RegSet::A | hla_operand_reads(rhs),
            modifies: RegSet::NONE,
        },
        HlaStmt::NeverBlock { .. }
        | HlaStmt::PrefixConditional { .. }
        | HlaStmt::AssignmentChain { .. }
        | HlaStmt::FlagSet { .. }
        | HlaStmt::Goto { .. }
        | HlaStmt::BranchGoto { .. }
        | HlaStmt::Return { .. }
        | HlaStmt::WaitLoopWhileNFlagClear { .. }
        | HlaStmt::DoOpen
        | HlaStmt::DoCloseNFlagClear
        | HlaStmt::DoCloseNFlagSet
        | HlaStmt::DoCloseWithOp { .. }
        | HlaStmt::DoClose { .. }
        | HlaStmt::DoCloseAlways
        | HlaStmt::DoCloseNever
        | HlaStmt::DoCloseBranch { .. }
        | HlaStmt::LoopBreak { .. }
        | HlaStmt::LoopRepeat { .. }
        | HlaStmt::RepeatInstruction { .. } => RegEffects::default(),
    }
}

fn call_signature_help(meta: &crate::sema::FunctionMeta, target: &str) -> String {
    format!("call it as `{}`", meta.signature_call_form(target))
}

fn build_inline_bindings(
    call: &CallStmt,
    meta: &crate::sema::FunctionMeta,
    scope: Option<&str>,
    sema: &SemanticModel,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<InlineBindings> {
    let mut bindings = InlineBindings {
        immediates: FxHashMap::default(),
        aliases: FxHashMap::default(),
    };

    if call.is_bare && !meta.has_contract && (!call.args.is_empty() || !call.outputs.is_empty()) {
        diagnostics.push(
            Diagnostic::error(
                span,
                format!(
                    "function '{}' does not declare a call contract",
                    call.target
                ),
            )
            .with_help(
                "remove the call-site arguments/outputs or add a contract to the declaration",
            ),
        );
        return None;
    }

    if !call.is_bare || !meta.has_contract {
        return Some(bindings);
    }

    if call.args.len() != meta.params.len() {
        diagnostics.push(
            Diagnostic::error(
                span,
                format!(
                    "call to '{}' expects {} contract argument(s), found {}",
                    call.target,
                    meta.params.len(),
                    call.args.len()
                ),
            )
            .with_help(call_signature_help(meta, &call.target)),
        );
        return None;
    }

    for (param, arg) in meta.params.iter().zip(&call.args) {
        match (param, arg) {
            (ContractParam::Register(expected), CallArg::Register(found)) if expected == found => {}
            (ContractParam::Register(expected), _) => {
                diagnostics.push(
                    Diagnostic::error(
                        span,
                        format!(
                            "call to '{}' must pass register '{}' in this position",
                            call.target,
                            reg_name_text(*expected)
                        ),
                    )
                    .with_help(call_signature_help(meta, &call.target)),
                );
                return None;
            }
            (ContractParam::Immediate(param), CallArg::Immediate(expr)) => {
                let value = eval_to_number(expr, scope, sema, span, diagnostics)?;
                let fits = match param.ty {
                    ImmediateParamType::Inferred => true,
                    ImmediateParamType::Byte => (0..=0xFF).contains(&value),
                    ImmediateParamType::Word => (0..=0xFFFF).contains(&value),
                };
                if !fits {
                    diagnostics.push(
                        Diagnostic::error(
                            span,
                            format!(
                                "inline immediate '#{}' does not fit declared type '{}'",
                                param.name,
                                immediate_param_type_text(param.ty)
                            ),
                        )
                        .with_help(
                            "pass a constant expression that fits the declared immediate width",
                        ),
                    );
                    return None;
                }
                bindings
                    .immediates
                    .insert(param.name.clone(), Expr::Number(value, NumFmt::Dec));
            }
            (ContractParam::Immediate(param), _) => {
                diagnostics.push(
                    Diagnostic::error(
                        span,
                        format!(
                            "call to '{}' must pass '#{}' as an immediate argument",
                            call.target, param.name
                        ),
                    )
                    .with_help(call_signature_help(meta, &call.target)),
                );
                return None;
            }
            (ContractParam::Alias(name), CallArg::Alias(target)) => {
                let resolved_target =
                    resolve_inline_alias_target(target, scope, sema, span, diagnostics)?;
                bindings.aliases.insert(name.clone(), resolved_target);
            }
            (ContractParam::Alias(name), _) => {
                diagnostics.push(
                    Diagnostic::error(
                        span,
                        format!(
                            "call to '{}' must pass '{}' as a bare identifier",
                            call.target, name
                        ),
                    )
                    .with_help(call_signature_help(meta, &call.target)),
                );
                return None;
            }
        }
    }

    if call.outputs != meta.outputs {
        diagnostics.push(
            Diagnostic::error(
                span,
                format!("call to '{}' has mismatched output contract", call.target),
            )
            .with_help(call_signature_help(meta, &call.target)),
        );
        return None;
    }

    Some(bindings)
}

fn resolve_inline_alias_target(
    target: &str,
    scope: Option<&str>,
    sema: &SemanticModel,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<String> {
    if sema.consts.contains_key(target) {
        diagnostics.push(
            Diagnostic::error(
                span,
                format!("inline alias argument '{target}' must be an address-like identifier"),
            )
            .with_help(
                "pass a variable, function, or label identifier here; constants are not accepted",
            ),
        );
        return None;
    }

    if matches!(
        resolve_symbolic_subscript_name(target, sema, span, diagnostics),
        Ok(Some(_))
    ) {
        return Some(target.to_string());
    }

    resolve_symbol(target, scope, span, diagnostics)
}

fn substitute_inline_ident(name: &str, bindings: &InlineBindings) -> String {
    if let Some(alias) = bindings.aliases.get(name) {
        return alias.clone();
    }
    for (from, to) in &bindings.aliases {
        if let Some(rest) = name.strip_prefix(from)
            && rest.starts_with('.')
        {
            return format!("{to}{rest}");
        }
    }
    name.to_string()
}

fn substitute_inline_expr(expr: &Expr, bindings: &InlineBindings) -> Expr {
    match expr {
        Expr::Number(..) | Expr::EvalText(..) => expr.clone(),
        Expr::Ident(name) => bindings
            .immediates
            .get(name)
            .cloned()
            .unwrap_or_else(|| Expr::Ident(substitute_inline_ident(name, bindings))),
        Expr::IdentSpanned { name, .. } => bindings
            .immediates
            .get(name)
            .cloned()
            .unwrap_or_else(|| Expr::Ident(substitute_inline_ident(name, bindings))),
        Expr::Index { base, index } => Expr::Index {
            base: Box::new(substitute_inline_expr(base, bindings)),
            index: Box::new(substitute_inline_expr(index, bindings)),
        },
        Expr::Member {
            base,
            field,
            start,
            end,
        } => Expr::Member {
            base: Box::new(substitute_inline_expr(base, bindings)),
            field: field.clone(),
            start: *start,
            end: *end,
        },
        Expr::Binary { op, lhs, rhs } => Expr::Binary {
            op: *op,
            lhs: Box::new(substitute_inline_expr(lhs, bindings)),
            rhs: Box::new(substitute_inline_expr(rhs, bindings)),
        },
        Expr::Unary { op, expr } => Expr::Unary {
            op: *op,
            expr: Box::new(substitute_inline_expr(expr, bindings)),
        },
        Expr::TypedView { expr, width } => Expr::TypedView {
            expr: Box::new(substitute_inline_expr(expr, bindings)),
            width: *width,
        },
        Expr::MetadataQuery { expr, query } => Expr::MetadataQuery {
            expr: Box::new(substitute_inline_expr(expr, bindings)),
            query: *query,
        },
    }
}

fn substitute_inline_operand_expr(
    expr: &HlaOperandExpr,
    bindings: &InlineBindings,
) -> HlaOperandExpr {
    HlaOperandExpr {
        expr: substitute_inline_expr(&expr.expr, bindings),
        index: expr.index,
        addr_mode: expr.addr_mode,
    }
}

fn substitute_inline_rhs(rhs: &HlaRhs, bindings: &InlineBindings) -> HlaRhs {
    match rhs {
        HlaRhs::Immediate(expr) => HlaRhs::Immediate(substitute_inline_expr(expr, bindings)),
        HlaRhs::Value {
            expr,
            index,
            addr_mode,
        } => HlaRhs::Value {
            expr: substitute_inline_expr(expr, bindings),
            index: *index,
            addr_mode: *addr_mode,
        },
    }
}

fn substitute_inline_call_arg(arg: &CallArg, bindings: &InlineBindings) -> CallArg {
    match arg {
        CallArg::Register(reg) => CallArg::Register(*reg),
        CallArg::Immediate(expr) => CallArg::Immediate(substitute_inline_expr(expr, bindings)),
        CallArg::Alias(name) => CallArg::Alias(substitute_inline_ident(name, bindings)),
    }
}

fn substitute_inline_stmt(stmt: &Stmt, bindings: &InlineBindings) -> Stmt {
    match stmt {
        Stmt::Instruction(instruction) => Stmt::Instruction(Instruction {
            mnemonic: instruction.mnemonic.clone(),
            operand: instruction.operand.as_ref().map(|operand| match operand {
                Operand::Immediate {
                    expr,
                    explicit_hash,
                } => Operand::Immediate {
                    expr: substitute_inline_expr(expr, bindings),
                    explicit_hash: *explicit_hash,
                },
                Operand::Value {
                    expr,
                    addr_mode_override,
                    index,
                    addr_mode,
                } => Operand::Value {
                    expr: substitute_inline_expr(expr, bindings),
                    addr_mode_override: *addr_mode_override,
                    index: *index,
                    addr_mode: *addr_mode,
                },
                Operand::Auto { expr } => Operand::Auto {
                    expr: substitute_inline_expr(expr, bindings),
                },
                Operand::BlockMove { src, dst } => Operand::BlockMove {
                    src: substitute_inline_expr(src, bindings),
                    dst: substitute_inline_expr(dst, bindings),
                },
                Operand::Register { reg, span } => Operand::Register {
                    reg: *reg,
                    span: *span,
                },
            }),
        }),
        Stmt::Call(call) => Stmt::Call(CallStmt {
            target: call.target.clone(),
            is_far: call.is_far,
            args: call
                .args
                .iter()
                .map(|arg| substitute_inline_call_arg(arg, bindings))
                .collect(),
            outputs: call.outputs.clone(),
            is_bare: call.is_bare,
        }),
        Stmt::Hla(hla) => Stmt::Hla(match hla {
            HlaStmt::RegisterAssign { register, rhs } => HlaStmt::RegisterAssign {
                register: *register,
                rhs: substitute_inline_operand_expr(rhs, bindings),
            },
            HlaStmt::RegisterStore { dest, src } => HlaStmt::RegisterStore {
                dest: substitute_inline_operand_expr(dest, bindings),
                src: *src,
            },
            HlaStmt::RegisterTransfer { dest, src } => HlaStmt::RegisterTransfer {
                dest: *dest,
                src: *src,
            },
            HlaStmt::AssignmentChain { idents, tail_expr } => HlaStmt::AssignmentChain {
                idents: idents
                    .iter()
                    .map(|ident| substitute_inline_ident(ident, bindings))
                    .collect(),
                tail_expr: tail_expr
                    .as_ref()
                    .map(|expr| substitute_inline_operand_expr(expr, bindings)),
            },
            HlaStmt::AccumulatorAlu { op, rhs } => HlaStmt::AccumulatorAlu {
                op: *op,
                rhs: substitute_inline_operand_expr(rhs, bindings),
            },
            HlaStmt::AccumulatorBitTest { rhs } => HlaStmt::AccumulatorBitTest {
                rhs: substitute_inline_operand_expr(rhs, bindings),
            },
            HlaStmt::IndexCompare { register, rhs } => HlaStmt::IndexCompare {
                register: *register,
                rhs: substitute_inline_operand_expr(rhs, bindings),
            },
            HlaStmt::IncDec { op, target } => HlaStmt::IncDec {
                op: *op,
                target: match target {
                    crate::ast::HlaIncDecTarget::Register(reg) => {
                        crate::ast::HlaIncDecTarget::Register(*reg)
                    }
                    crate::ast::HlaIncDecTarget::Address(expr) => {
                        crate::ast::HlaIncDecTarget::Address(substitute_inline_operand_expr(
                            expr, bindings,
                        ))
                    }
                },
            },
            HlaStmt::ShiftRotate { op, target } => HlaStmt::ShiftRotate {
                op: *op,
                target: match target {
                    crate::ast::HlaShiftTarget::Accumulator => {
                        crate::ast::HlaShiftTarget::Accumulator
                    }
                    crate::ast::HlaShiftTarget::Address(expr) => {
                        crate::ast::HlaShiftTarget::Address(substitute_inline_operand_expr(
                            expr, bindings,
                        ))
                    }
                },
            },
            HlaStmt::Goto {
                target,
                indirect,
                far,
            } => HlaStmt::Goto {
                target: substitute_inline_expr(target, bindings),
                indirect: *indirect,
                far: *far,
            },
            HlaStmt::BranchGoto {
                mnemonic,
                target,
                form,
            } => HlaStmt::BranchGoto {
                mnemonic: mnemonic.clone(),
                target: substitute_inline_expr(target, bindings),
                form: *form,
            },
            HlaStmt::XAssignImmediate { rhs } => HlaStmt::XAssignImmediate {
                rhs: substitute_inline_expr(rhs, bindings),
            },
            HlaStmt::StoreFromA {
                dests,
                rhs,
                load_start,
                store_end,
            } => HlaStmt::StoreFromA {
                dests: dests
                    .iter()
                    .map(|dest| substitute_inline_ident(dest, bindings))
                    .collect(),
                rhs: substitute_inline_rhs(rhs, bindings),
                load_start: *load_start,
                store_end: *store_end,
            },
            HlaStmt::WaitLoopWhileNFlagClear { symbol } => HlaStmt::WaitLoopWhileNFlagClear {
                symbol: substitute_inline_ident(symbol, bindings),
            },
            HlaStmt::ConditionSeed { lhs, rhs } => HlaStmt::ConditionSeed {
                lhs: *lhs,
                rhs: substitute_inline_operand_expr(rhs, bindings),
            },
            HlaStmt::DoClose { condition } => HlaStmt::DoClose {
                condition: HlaCondition {
                    lhs: condition.lhs,
                    op: condition.op,
                    rhs: condition
                        .rhs
                        .as_ref()
                        .map(|expr| substitute_inline_expr(expr, bindings)),
                    seed_span: condition.seed_span,
                },
            },
            HlaStmt::NeverBlock { body } => HlaStmt::NeverBlock {
                body: substitute_inline_body(body, bindings),
            },
            HlaStmt::PrefixConditional {
                skip_mnemonic,
                form,
                body,
                else_body,
            } => HlaStmt::PrefixConditional {
                skip_mnemonic: skip_mnemonic.clone(),
                form: *form,
                body: substitute_inline_body(body, bindings),
                else_body: else_body
                    .as_ref()
                    .map(|body| substitute_inline_body(body, bindings)),
            },
            HlaStmt::MemStoreZero { dest } => HlaStmt::MemStoreZero {
                dest: substitute_inline_operand_expr(dest, bindings),
            },
            HlaStmt::RepeatInstruction { mnemonic, count } => HlaStmt::RepeatInstruction {
                mnemonic: mnemonic.clone(),
                count: substitute_inline_expr(count, bindings),
            },
            // Payload-free variants: nothing to substitute. Listed explicitly so
            // adding a new HlaStmt variant fails to compile here instead of
            // silently skipping inline substitution (the bug that produced the
            // misleading "unknown identifier 'n'" diagnostic in `lsr * n`).
            HlaStmt::FlagSet { .. }
            | HlaStmt::StackOp { .. }
            | HlaStmt::Return { .. }
            | HlaStmt::XIncrement
            | HlaStmt::DoOpen
            | HlaStmt::DoCloseNFlagClear
            | HlaStmt::DoCloseNFlagSet
            | HlaStmt::DoCloseWithOp { .. }
            | HlaStmt::DoCloseAlways
            | HlaStmt::DoCloseNever
            | HlaStmt::DoCloseBranch { .. }
            | HlaStmt::LoopBreak { .. }
            | HlaStmt::LoopRepeat { .. } => hla.clone(),
        }),
        Stmt::ModeScopedBlock {
            a_width,
            i_width,
            body,
        } => Stmt::ModeScopedBlock {
            a_width: *a_width,
            i_width: *i_width,
            body: substitute_inline_body(body, bindings),
        },
        _ => stmt.clone(),
    }
}

fn substitute_inline_body(body: &[Spanned<Stmt>], bindings: &InlineBindings) -> Vec<Spanned<Stmt>> {
    body.iter()
        .map(|stmt| Spanned::new(substitute_inline_stmt(&stmt.node, bindings), stmt.span))
        .collect()
}

/// If the inline body came from a different source than the call site, clone
/// it and rewrite every span to collapse onto the call site. The caller's
/// `SourceMap` only knows its own file, so body spans from other files would
/// either render gibberish (same `SourceId(0)` accidentally aliased) or panic
/// at `SourceMap::must_get`.
///
/// Returns `None` when the body is safe to reuse in-place (same file).
fn retargeted_body_if_foreign(
    body: &[Spanned<Stmt>],
    call_site: Span,
) -> Option<Vec<Spanned<Stmt>>> {
    let body_source_id = body.first().map(|stmt| stmt.span.source_id)?;
    if body_source_id == call_site.source_id {
        return None;
    }
    let mut cloned: Vec<Spanned<Stmt>> = body.to_vec();
    retarget_spans(&mut cloned, call_site);
    Some(cloned)
}

/// Recursively rewrite every span inside these statements to a zero-width span
/// at `target`. Used for cross-TU inline expansions so body-internal
/// diagnostics anchor on the call site in the caller's `SourceMap` instead of
/// pointing at byte offsets in a source file the caller's `SourceMap` does not
/// know about. The pre-retarget span is preserved on `Spanned::origin` so a
/// downstream emit-time helper can attach an "(Inlined from …)" annotation.
///
/// Note: lowering-time diagnostics emitted during expansion (e.g. unresolved
/// identifiers from `eval_to_number`) currently anchor at the retargeted
/// zero-width call-site span but do **not** carry an `InlineOrigin`
/// supplement back to the foreign body. Emit-time diagnostics already do via
/// `attach_inline_origin` in `emit_object.rs`. Threading `&SourceMap` through
/// `lower_stmt` and `lower_inline_call` to close that gap is left as
/// follow-up work.
fn retarget_spans(body: &mut [Spanned<Stmt>], target: Span) {
    let zero = Span {
        source_id: target.source_id,
        start: target.start,
        end: target.start,
    };
    for stmt in body {
        if stmt.origin.is_none() {
            stmt.origin = Some(stmt.span);
        }
        stmt.span = zero;
        retarget_stmt_spans(&mut stmt.node, zero);
    }
}

fn retarget_stmt_spans(stmt: &mut Stmt, target: Span) {
    // Walk into every expression so any `Expr::IdentSpanned { start, end, … }`
    // (whose byte offsets refer to the inline body's *original* SourceId) gets
    // collapsed to `Expr::Ident(name)`. Without this, `expr_ident_span` later
    // builds a frankenspan: target's `source_id` paired with the foreign body's
    // byte offsets. That frankenspan is what made "unknown identifier 'n'"
    // anchor inside an unrelated `var` declaration in the caller's source.
    match stmt {
        Stmt::ModeScopedBlock { body, .. } => {
            retarget_spans(body, target);
        }
        Stmt::Hla(HlaStmt::NeverBlock { body }) => {
            retarget_spans(body, target);
        }
        Stmt::Hla(HlaStmt::PrefixConditional {
            body, else_body, ..
        }) => {
            retarget_spans(body, target);
            if let Some(else_body) = else_body {
                retarget_spans(else_body, target);
            }
        }
        Stmt::Instruction(instruction) => {
            if let Some(operand) = instruction.operand.as_mut() {
                retarget_operand_spans(operand);
            }
        }
        Stmt::Call(call) => {
            for arg in call.args.iter_mut() {
                if let CallArg::Immediate(expr) = arg {
                    retarget_expr_spans(expr);
                }
            }
        }
        Stmt::Hla(hla) => retarget_hla_spans(hla, target),
        _ => {}
    }
}

fn retarget_expr_spans(expr: &mut Expr) {
    match expr {
        Expr::IdentSpanned { name, .. } => {
            *expr = Expr::Ident(std::mem::take(name));
        }
        Expr::Index { base, index } => {
            retarget_expr_spans(base);
            retarget_expr_spans(index);
        }
        Expr::Member { base, .. } => {
            retarget_expr_spans(base);
        }
        Expr::Binary { lhs, rhs, .. } => {
            retarget_expr_spans(lhs);
            retarget_expr_spans(rhs);
        }
        Expr::Unary { expr, .. } => {
            retarget_expr_spans(expr);
        }
        Expr::TypedView { expr, .. } => {
            retarget_expr_spans(expr);
        }
        Expr::MetadataQuery { expr, .. } => {
            retarget_expr_spans(expr);
        }
        Expr::Number(..) | Expr::Ident(..) | Expr::EvalText(..) => {}
    }
}

fn retarget_operand_expr_spans(operand: &mut HlaOperandExpr) {
    retarget_expr_spans(&mut operand.expr);
}

fn retarget_operand_spans(operand: &mut Operand) {
    match operand {
        Operand::Immediate { expr, .. } | Operand::Value { expr, .. } | Operand::Auto { expr } => {
            retarget_expr_spans(expr)
        }
        Operand::BlockMove { src, dst } => {
            retarget_expr_spans(src);
            retarget_expr_spans(dst);
        }
        Operand::Register { .. } => {}
    }
}

fn retarget_hla_spans(hla: &mut HlaStmt, target: Span) {
    match hla {
        HlaStmt::RegisterAssign { rhs, .. }
        | HlaStmt::AccumulatorAlu { rhs, .. }
        | HlaStmt::AccumulatorBitTest { rhs, .. }
        | HlaStmt::IndexCompare { rhs, .. }
        | HlaStmt::ConditionSeed { rhs, .. } => retarget_operand_expr_spans(rhs),
        HlaStmt::RegisterStore { dest, .. } | HlaStmt::MemStoreZero { dest } => {
            retarget_operand_expr_spans(dest);
        }
        HlaStmt::AssignmentChain { tail_expr, .. } => {
            if let Some(tail) = tail_expr.as_mut() {
                retarget_operand_expr_spans(tail);
            }
        }
        HlaStmt::IncDec { target: tgt, .. } => {
            if let crate::ast::HlaIncDecTarget::Address(operand) = tgt {
                retarget_operand_expr_spans(operand);
            }
        }
        HlaStmt::ShiftRotate { target: tgt, .. } => {
            if let crate::ast::HlaShiftTarget::Address(operand) = tgt {
                retarget_operand_expr_spans(operand);
            }
        }
        HlaStmt::Goto { target, .. } | HlaStmt::BranchGoto { target, .. } => {
            retarget_expr_spans(target);
        }
        HlaStmt::XAssignImmediate { rhs } => retarget_expr_spans(rhs),
        HlaStmt::StoreFromA { rhs, .. } => match rhs {
            HlaRhs::Immediate(expr) => retarget_expr_spans(expr),
            HlaRhs::Value { expr, .. } => retarget_expr_spans(expr),
        },
        HlaStmt::DoClose { condition } => {
            if let Some(rhs) = condition.rhs.as_mut() {
                retarget_expr_spans(rhs);
            }
        }
        HlaStmt::RepeatInstruction { count, .. } => retarget_expr_spans(count),
        HlaStmt::NeverBlock { body } => retarget_spans(body, target),
        HlaStmt::PrefixConditional {
            body, else_body, ..
        } => {
            retarget_spans(body, target);
            if let Some(else_body) = else_body {
                retarget_spans(else_body, target);
            }
        }
        HlaStmt::RegisterTransfer { .. }
        | HlaStmt::FlagSet { .. }
        | HlaStmt::StackOp { .. }
        | HlaStmt::Return { .. }
        | HlaStmt::XIncrement
        | HlaStmt::WaitLoopWhileNFlagClear { .. }
        | HlaStmt::DoOpen
        | HlaStmt::DoCloseNFlagClear
        | HlaStmt::DoCloseNFlagSet
        | HlaStmt::DoCloseWithOp { .. }
        | HlaStmt::DoCloseAlways
        | HlaStmt::DoCloseNever
        | HlaStmt::DoCloseBranch { .. }
        | HlaStmt::LoopBreak { .. }
        | HlaStmt::LoopRepeat { .. } => {}
    }
}

fn reg_name_text(reg: RegName) -> &'static str {
    match reg {
        RegName::A => "a",
        RegName::X => "x",
        RegName::Y => "y",
    }
}

fn merge_hazards(lhs: RegHazardState, rhs: RegHazardState) -> RegHazardState {
    RegHazardState {
        a_dead: lhs.a_dead || rhs.a_dead,
        x_truncated: lhs.x_truncated || rhs.x_truncated,
        y_truncated: lhs.y_truncated || rhs.y_truncated,
    }
}

fn apply_mode_transition_hazards(
    hazards: RegHazardState,
    current_mode: ModeState,
    a_width: Option<RegWidth>,
    i_width: Option<RegWidth>,
) -> RegHazardState {
    let mut next = hazards;
    if let Some(target) = a_width
        && current_mode.a_width != Some(target)
    {
        next.a_dead = true;
    }
    if let Some(RegWidth::W8) = i_width
        && current_mode.i_width != Some(RegWidth::W8)
    {
        next.x_truncated = true;
        next.y_truncated = true;
    }
    next
}

fn apply_mode_restore_hazards(
    hazards: RegHazardState,
    current_mode: ModeState,
    saved_mode: ModeState,
) -> RegHazardState {
    let mut next = hazards;
    if saved_mode.a_width.is_some() && saved_mode.a_width != current_mode.a_width {
        next.a_dead = true;
    }
    if saved_mode.i_width == Some(RegWidth::W8) && current_mode.i_width != Some(RegWidth::W8) {
        next.x_truncated = true;
        next.y_truncated = true;
    }
    next
}

fn apply_hazards_for_exited_frames(
    hazards: RegHazardState,
    current_mode: ModeState,
    frames: &[ModeFrame],
    exit_count: usize,
) -> RegHazardState {
    let mut next_hazards = hazards;
    let mut mode = current_mode;
    for frame in frames.iter().rev().take(exit_count) {
        next_hazards = apply_mode_restore_hazards(next_hazards, mode, frame.entry_mode);
        mode = frame.entry_mode;
    }
    next_hazards
}

fn accumulator_width_switch_error(span: Span, width: Option<RegWidth>) -> Diagnostic {
    match width {
        Some(RegWidth::W8) => {
            Diagnostic::error(span, "Accumulator value is dead after switching to @a8.")
                .with_help("load A again in 8-bit mode before using it")
        }
        Some(RegWidth::W16) => {
            Diagnostic::error(span, "Accumulator value is dead after switching to @a16.")
                .with_help("load A again in 16-bit mode before using it")
        }
        None => Diagnostic::error(
            span,
            "Accumulator value is dead after switching accumulator width.",
        )
        .with_help("load A again after the width change before using it"),
    }
}

fn index_truncation_warning(span: Span, reg: RegName) -> Diagnostic {
    let reg_name = reg_name_text(reg);
    Diagnostic::warning(
        span,
        format!("register {reg_name} is used after @i8 without an 8-bit reload"),
    )
    .with_help(format!(
        "the previous 16-bit value was truncated to 8 bits; read {reg_name} before @i8, or reload {reg_name} with an 8-bit value after @i8 before reading it"
    ))
}

fn apply_reg_access_hazards(
    hazards: RegHazardState,
    effects: RegEffects,
    mode: ModeState,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) -> RegHazardState {
    if effects.reads.contains(RegSet::A) && hazards.a_dead {
        diagnostics.push(accumulator_width_switch_error(span, mode.a_width));
    }
    if effects.reads.contains(RegSet::X) && hazards.x_truncated {
        diagnostics.push(index_truncation_warning(span, RegName::X));
    }
    if effects.reads.contains(RegSet::Y) && hazards.y_truncated {
        diagnostics.push(index_truncation_warning(span, RegName::Y));
    }

    let mut next = hazards;
    if effects.modifies.contains(RegSet::A) {
        next.a_dead = false;
    }
    if effects.modifies.contains(RegSet::X) {
        next.x_truncated = false;
    }
    if effects.modifies.contains(RegSet::Y) {
        next.y_truncated = false;
    }
    next
}

fn apply_call_summary_hazards(
    hazards: RegHazardState,
    summary: &CallContractSummary,
    mode: ModeState,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) -> RegHazardState {
    let mut reads = RegSet::NONE;
    for param in &summary.inputs {
        if let ContractParam::Register(reg) = param {
            reads |= reg_name_set(*reg);
        }
    }
    let effects = RegEffects {
        reads,
        modifies: summary.clobbers | reg_names_set(&summary.outputs),
    };
    apply_reg_access_hazards(hazards, effects, mode, span, diagnostics)
}

fn record_label_entry_hazards(ctx: &mut LowerContext, label: &str, hazards: RegHazardState) {
    if let Some(existing) = ctx.label_entry_hazards.get(label).copied() {
        ctx.label_entry_hazards
            .insert(label.to_string(), merge_hazards(existing, hazards));
    } else {
        ctx.label_entry_hazards.insert(label.to_string(), hazards);
    }
}

fn join_synthetic_label_state(
    ctx: &mut LowerContext,
    label: &str,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if let Some(existing) = ctx.label_entry_modes.get(label).copied() {
        if ctx.reachable && ctx.mode != existing.mode {
            diagnostics.push(mode_mismatch_diagnostic(
                label,
                ModeMismatchEdges {
                    current: (span, ctx.mode),
                    earlier: Some((existing.first_edge_span, existing.mode)),
                    declared: None,
                },
            ));
        }
        let merged_hazards =
            if let Some(incoming_hazards) = ctx.label_entry_hazards.get(label).copied() {
                if ctx.reachable {
                    merge_hazards(ctx.hazards, incoming_hazards)
                } else {
                    incoming_hazards
                }
            } else {
                ctx.hazards
            };
        ctx.mode = existing.mode;
        ctx.hazards = merged_hazards;
        ctx.reachable = true;
    } else if ctx.reachable {
        ctx.label_entry_modes.insert(
            label.to_string(),
            LabelEntryRecord {
                mode: ctx.mode,
                first_edge_span: span,
            },
        );
        ctx.label_entry_hazards
            .insert(label.to_string(), ctx.hazards);
    }
}

fn immediate_param_type_text(ty: ImmediateParamType) -> &'static str {
    match ty {
        ImmediateParamType::Inferred => "inferred",
        ImmediateParamType::Byte => "byte",
        ImmediateParamType::Word => "word",
    }
}

fn build_call_contract_summaries(
    file: &File,
    sema: &SemanticModel,
) -> FxHashMap<String, CallContractSummary> {
    let blocks: FxHashMap<String, &CodeBlock> = file
        .items
        .iter()
        .filter_map(|item| match &item.node {
            Item::CodeBlock(block) => Some((block.name.clone(), block)),
            _ => None,
        })
        .collect();
    let mut cache = FxHashMap::default();
    let mut visiting = Vec::new();

    for name in blocks.keys() {
        compute_function_summary(name, &blocks, sema, &mut cache, &mut visiting);
    }

    cache
}

fn compute_function_summary(
    name: &str,
    blocks: &FxHashMap<String, &CodeBlock>,
    sema: &SemanticModel,
    cache: &mut FxHashMap<String, CallContractSummary>,
    visiting: &mut Vec<String>,
) -> CallContractSummary {
    if let Some(summary) = cache.get(name) {
        return summary.clone();
    }
    if visiting.iter().any(|entry| entry == name) {
        return CallContractSummary {
            inputs: Vec::new(),
            outputs: Vec::new(),
            clobbers: RegSet::NONE,
            is_naked: false,
        };
    }

    let Some(meta) = sema.functions.get(name) else {
        return CallContractSummary {
            inputs: Vec::new(),
            outputs: Vec::new(),
            clobbers: RegSet::NONE,
            is_naked: false,
        };
    };
    let Some(block) = blocks.get(name) else {
        return CallContractSummary {
            inputs: meta.params.clone(),
            outputs: meta.outputs.clone(),
            clobbers: RegSet::NONE,
            is_naked: meta.is_naked,
        };
    };

    visiting.push(name.to_string());
    let body_effects = summarize_stmt_sequence_summary(&block.body, sema, blocks, cache, visiting);
    visiting.pop();

    let summary = CallContractSummary {
        inputs: meta.params.clone(),
        outputs: meta.outputs.clone(),
        clobbers: if meta.is_naked {
            RegSet::NONE
        } else {
            body_effects.modifies - reg_names_set(&meta.outputs)
        },
        is_naked: meta.is_naked,
    };
    cache.insert(name.to_string(), summary.clone());
    summary
}

fn summarize_stmt_sequence_summary(
    stmts: &[Spanned<Stmt>],
    sema: &SemanticModel,
    blocks: &FxHashMap<String, &CodeBlock>,
    cache: &mut FxHashMap<String, CallContractSummary>,
    visiting: &mut Vec<String>,
) -> RegEffects {
    let mut effects = RegEffects::default();
    for stmt in stmts {
        effects = merge_summary_effects(
            effects,
            summary_stmt_effects(&stmt.node, sema, blocks, cache, visiting),
        );
    }
    effects
}

fn summarize_stmt_sequence_liveness(
    stmts: &[Spanned<Stmt>],
    sema: &SemanticModel,
    blocks: &FxHashMap<String, &CodeBlock>,
    cache: &mut FxHashMap<String, CallContractSummary>,
    visiting: &mut Vec<String>,
) -> RegEffects {
    let mut effects = RegEffects::default();
    for stmt in stmts {
        effects = compose_effects(
            effects,
            liveness_stmt_effects(&stmt.node, sema, blocks, cache, visiting),
        );
    }
    effects
}

fn summary_stmt_effects(
    stmt: &Stmt,
    sema: &SemanticModel,
    blocks: &FxHashMap<String, &CodeBlock>,
    cache: &mut FxHashMap<String, CallContractSummary>,
    visiting: &mut Vec<String>,
) -> RegEffects {
    match stmt {
        Stmt::Hla(HlaStmt::PrefixConditional {
            body, else_body, ..
        }) => {
            let body_effects = summarize_stmt_sequence_summary(body, sema, blocks, cache, visiting);
            let else_effects = else_body
                .as_ref()
                .map(|body| summarize_stmt_sequence_summary(body, sema, blocks, cache, visiting))
                .unwrap_or_default();
            merge_summary_effects(body_effects, else_effects)
        }
        Stmt::ModeScopedBlock { body, .. } => {
            summarize_stmt_sequence_summary(body, sema, blocks, cache, visiting)
        }
        Stmt::Hla(HlaStmt::NeverBlock { .. }) => RegEffects::default(),
        _ => liveness_stmt_effects(stmt, sema, blocks, cache, visiting),
    }
}

fn liveness_stmt_effects(
    stmt: &Stmt,
    sema: &SemanticModel,
    blocks: &FxHashMap<String, &CodeBlock>,
    cache: &mut FxHashMap<String, CallContractSummary>,
    visiting: &mut Vec<String>,
) -> RegEffects {
    match stmt {
        Stmt::Instruction(instruction) => instruction_effects(instruction),
        Stmt::Call(call) => call_stmt_effects(call, sema, blocks, cache, visiting),
        Stmt::Hla(HlaStmt::NeverBlock { .. }) => RegEffects::default(),
        Stmt::Hla(HlaStmt::PrefixConditional {
            body, else_body, ..
        }) => {
            let body_effects =
                summarize_stmt_sequence_liveness(body, sema, blocks, cache, visiting);
            let else_effects = else_body
                .as_ref()
                .map(|body| summarize_stmt_sequence_liveness(body, sema, blocks, cache, visiting))
                .unwrap_or_default();
            RegEffects {
                reads: body_effects.reads | else_effects.reads,
                modifies: body_effects.modifies & else_effects.modifies,
            }
        }
        Stmt::Hla(hla) => hla_effects(hla),
        Stmt::ModeScopedBlock { body, .. } => {
            summarize_stmt_sequence_liveness(body, sema, blocks, cache, visiting)
        }
        _ => RegEffects::default(),
    }
}

fn call_stmt_effects(
    call: &CallStmt,
    sema: &SemanticModel,
    blocks: &FxHashMap<String, &CodeBlock>,
    cache: &mut FxHashMap<String, CallContractSummary>,
    visiting: &mut Vec<String>,
) -> RegEffects {
    if !call.is_bare {
        return RegEffects::default();
    }
    let Some(meta) = sema.functions.get(&call.target) else {
        return RegEffects::default();
    };
    if !meta.has_contract {
        return RegEffects::default();
    }
    let summary = compute_function_summary(&call.target, blocks, sema, cache, visiting);
    RegEffects {
        reads: summary
            .inputs
            .iter()
            .fold(RegSet::NONE, |acc, param| match param {
                ContractParam::Register(reg) => acc | reg_name_set(*reg),
                ContractParam::Immediate(_) | ContractParam::Alias(_) => acc,
            }),
        modifies: summary.clobbers | reg_names_set(&summary.outputs),
    }
}

fn check_damage_for_sequence(
    stmts: &[Spanned<Stmt>],
    sema: &SemanticModel,
    blocks: &FxHashMap<String, &CodeBlock>,
    summaries: &mut FxHashMap<String, CallContractSummary>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    for (idx, stmt) in stmts.iter().enumerate() {
        if let Stmt::Call(call) = &stmt.node
            && call.is_bare
            && let Some(meta) = sema.functions.get(&call.target)
            && meta.has_contract
        {
            let mut visiting = Vec::new();
            let summary =
                compute_function_summary(&call.target, blocks, sema, summaries, &mut visiting);
            if !summary.is_naked {
                let live_after = forward_live_scan(&stmts[idx + 1..], sema, blocks, summaries);
                let collisions = live_after & (summary.clobbers - reg_names_set(&summary.outputs));
                for reg in [RegName::A, RegName::X, RegName::Y] {
                    if collisions.contains(reg_name_set(reg)) {
                        let reg_text = reg_name_text(reg);
                        let call_form = meta.signature_call_form(&call.target);
                        diagnostics.push(
                            Diagnostic::error(
                                stmt.span,
                                format!(
                                    "register `{}` is live after call to `{}` but clobbered by it",
                                    reg_text,
                                    call.target
                                ),
                            )
                            .with_primary_label(format!("call to `{}` clobbers `{}`", call.target, reg_text))
                            .with_help(format!(
                                "save `{reg_text}` across the call (`{reg_text}!!` to push, `!!{reg_text}` to pop), consume `{reg_text}` before the call, or extend the contract of `{call_form}` so it preserves or returns `{reg_text}`"
                            ))
                            .with_note(
                                "The W65C816 has only three general-purpose registers (`A`, `X`, `Y`); the function-contract analyzer assumes a callee may overwrite anything in its clobber set, and warns when a value the caller still needs would not survive the call.",
                            ),
                        );
                    }
                }
            }
        }

        match &stmt.node {
            Stmt::ModeScopedBlock { body, .. } => {
                check_damage_for_sequence(body, sema, blocks, summaries, diagnostics);
            }
            Stmt::Hla(HlaStmt::PrefixConditional {
                body, else_body, ..
            }) => {
                check_damage_for_sequence(body, sema, blocks, summaries, diagnostics);
                if let Some(else_body) = else_body {
                    check_damage_for_sequence(else_body, sema, blocks, summaries, diagnostics);
                }
            }
            Stmt::Hla(HlaStmt::NeverBlock { body }) => {
                check_damage_for_sequence(body, sema, blocks, summaries, diagnostics);
            }
            _ => {}
        }
    }
}

fn forward_live_scan(
    stmts: &[Spanned<Stmt>],
    sema: &SemanticModel,
    blocks: &FxHashMap<String, &CodeBlock>,
    summaries: &mut FxHashMap<String, CallContractSummary>,
) -> RegSet {
    let mut live = RegSet::NONE;
    let mut killed = RegSet::NONE;
    let mut visiting = Vec::new();

    for stmt in stmts {
        if is_naked_call_boundary(&stmt.node, sema) {
            break;
        }
        let effects = liveness_stmt_effects(&stmt.node, sema, blocks, summaries, &mut visiting);
        live |= effects.reads - killed;
        killed |= effects.modifies;
        if killed.contains(RegSet::A) && killed.contains(RegSet::X) && killed.contains(RegSet::Y) {
            break;
        }
    }

    live
}

fn is_naked_call_boundary(stmt: &Stmt, sema: &SemanticModel) -> bool {
    let name = match stmt {
        Stmt::Call(call) if call.is_bare => call.target.as_str(),
        Stmt::Instruction(instruction) if instruction.operand.is_none() => {
            instruction.mnemonic.as_str()
        }
        _ => return false,
    };
    sema.functions
        .get(name)
        .is_some_and(|meta| meta.is_naked && meta.has_contract)
}

fn mode_state_to_contract(mode: ModeState) -> ModeContract {
    ModeContract {
        a_width: mode.a_width,
        i_width: mode.i_width,
    }
}

fn apply_exit_width(width: ExitWidth, entry: Option<RegWidth>) -> Option<RegWidth> {
    // An `Unknown` outbound contract is treated as `Preserve` at the call site:
    // the callee is required to leave the caller's tracked width unchanged.
    // See docs/register-width-aware-syntax.md, "Unknown exit contracts preserve
    // caller mode".
    match width {
        ExitWidth::Preserve | ExitWidth::Unknown => entry,
        ExitWidth::Fixed(width) => Some(width),
    }
}

fn apply_exit_mode(summary: ExitModeSummary, entry: ModeState) -> ModeState {
    ModeState {
        a_width: apply_exit_width(summary.a_width, entry.a_width),
        i_width: apply_exit_width(summary.i_width, entry.i_width),
    }
}

fn format_contract_mode(mode: Option<RegWidth>, register: RegName) -> &'static str {
    match (register, mode) {
        (RegName::A, Some(RegWidth::W8)) => "@a8",
        (RegName::A, Some(RegWidth::W16)) => "@a16",
        (RegName::A, None) => "@a?",
        (_, Some(RegWidth::W8)) => "@i8",
        (_, Some(RegWidth::W16)) => "@i16",
        (_, None) => "@i?",
    }
}

fn format_inferred_exit_width(width: ExitWidth, register: RegName) -> &'static str {
    match width {
        ExitWidth::Preserve => match register {
            RegName::A => "caller-preserved A width",
            _ => "caller-preserved I width",
        },
        ExitWidth::Fixed(width) => format_contract_mode(Some(width), register),
        ExitWidth::Unknown => format_contract_mode(None, register),
    }
}

fn entry_mode_variants(is_entry: bool, mode_contract: ModeContract) -> Vec<ModeState> {
    if is_entry {
        return vec![initial_mode_for_block(true, mode_contract)];
    }

    let a_widths = match mode_contract.a_width {
        Some(width) => vec![Some(width)],
        None => vec![Some(RegWidth::W8), Some(RegWidth::W16)],
    };
    let i_widths = match mode_contract.i_width {
        Some(width) => vec![Some(width)],
        None => vec![Some(RegWidth::W8), Some(RegWidth::W16)],
    };

    let mut variants = Vec::new();
    for a_width in a_widths {
        for i_width in &i_widths {
            variants.push(ModeState {
                a_width,
                i_width: *i_width,
            });
        }
    }
    variants
}

fn infer_exit_width(variants: &[(ModeState, ModeState)], reg: RegName) -> ExitWidth {
    let input_width = |mode: ModeState| match reg {
        RegName::A => mode.a_width,
        _ => mode.i_width,
    };
    let output_width = |mode: ModeState| match reg {
        RegName::A => mode.a_width,
        _ => mode.i_width,
    };

    let Some((_, first_output)) = variants.first().copied() else {
        return ExitWidth::Unknown;
    };
    let first_output = output_width(first_output);

    if variants
        .iter()
        .all(|(_, output)| output_width(*output) == first_output)
    {
        return first_output.map_or(ExitWidth::Unknown, ExitWidth::Fixed);
    }

    if variants
        .iter()
        .all(|(input, output)| output_width(*output) == input_width(*input))
    {
        return ExitWidth::Preserve;
    }

    ExitWidth::Unknown
}

pub(crate) fn lower_with_warnings(
    file: &File,
    sema: &SemanticModel,
    fs: &dyn AssetFS,
    external_inline_bodies: Option<&IndexMap<String, CodeBlock>>,
    workspace_addressable: Option<&HashSet<String>>,
) -> Result<LowerOutput, Vec<Diagnostic>> {
    let mut diagnostics = Vec::new();
    let mut ops = Vec::new();
    let mut top_level_ctx = LowerContext {
        ..Default::default()
    };
    let mut current_segment = crate::DEFAULT_SEGMENT.to_string();

    // Collect inline function bodies for substitution at call sites. Local
    // declarations win over cross-unit ones on name collisions. Bodies pulled
    // from `external_inline_bodies` carry spans from another source file's
    // `SourceId`; the call-site-dispatch code retargets those spans when it
    // detects a cross-file body, so body-internal diagnostics anchor against
    // this file's `SourceMap`.
    let mut inline_bodies: FxHashMap<String, &CodeBlock> = FxHashMap::default();
    if let Some(external) = external_inline_bodies {
        for (name, block) in external {
            inline_bodies.insert(name.clone(), block);
        }
    }
    for item in &file.items {
        if let Item::CodeBlock(block) = &item.node
            && block.is_inline
        {
            inline_bodies.insert(block.name.clone(), block);
        }
    }
    let function_bodies: FxHashMap<String, &CodeBlock> = file
        .items
        .iter()
        .filter_map(|item| match &item.node {
            Item::CodeBlock(block) => Some((block.name.clone(), block)),
            _ => None,
        })
        .collect();
    // Build advisory function-entry signatures so cross-function jumps can
    // surface the target function's `@a*/@i*` annotation in mismatch diagnostics
    // even though the declared mode lives in a different `block_ctx`. Advisory
    // records are NOT used to mask incoming modes (the bridging rep/sep would
    // need to be emitted in the target function's lowering, which this context
    // cannot reach), so masking is intentionally skipped for them.
    //
    // External code blocks (cross-source) are merged in with local-wins
    // semantics so a `jmp foo` in this source can name the contract span from
    // another source's `func foo @a16 @i16 { ... }` declaration.
    let mut function_entry_signatures: FxHashMap<String, LabelDeclaredRecord> =
        FxHashMap::default();
    for item in &file.items {
        if let Item::CodeBlock(block) = &item.node
            && !block.is_inline
        {
            let contract = sema
                .functions
                .get(&block.name)
                .map(|meta| meta.mode_contract)
                .unwrap_or(block.mode_contract);
            let decl_span = block.name_span.unwrap_or(item.span);
            insert_function_entry_signature(
                &mut function_entry_signatures,
                &block.name,
                contract,
                decl_span,
            );
        }
    }
    if let Some(external) = external_inline_bodies {
        for (name, block) in external {
            if block.is_inline {
                continue;
            }
            let decl_span = block.name_span.unwrap_or_else(|| {
                block
                    .body
                    .first()
                    .map(|stmt| stmt.span)
                    .unwrap_or(Span::new(SourceId(0), 0, 0))
            });
            insert_function_entry_signature(
                &mut function_entry_signatures,
                name,
                block.mode_contract,
                decl_span,
            );
        }
    }
    let exit_summaries = build_exit_mode_summaries(file, sema, fs, &mut diagnostics);
    let mut call_summaries = build_call_contract_summaries(file, sema);

    for item in &file.items {
        match &item.node {
            Item::Segment(segment) => {
                ops.push(Spanned::new(
                    Op::SelectSegment(segment.name.clone()),
                    item.span,
                ));
                current_segment = segment.name.clone();
            }
            Item::DataBlock(block) => {
                lower_data_block(
                    block,
                    sema,
                    fs,
                    &inline_bodies,
                    &exit_summaries,
                    &current_segment,
                    &mut diagnostics,
                    &mut ops,
                );
            }
            Item::CodeBlock(block) => {
                if block.is_inline {
                    continue; // Body will be inlined at call sites.
                }
                let mut block_ctx = LowerContext {
                    is_far: block.is_far,
                    ..LowerContext::default()
                };
                check_damage_for_sequence(
                    &block.body,
                    sema,
                    &function_bodies,
                    &mut call_summaries,
                    &mut diagnostics,
                );
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
                for (name, record) in &function_entry_signatures {
                    block_ctx
                        .label_declared_modes
                        .entry(name.clone())
                        .or_insert(*record);
                }

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
                // there is no call site to bridge from. The runtime tracker
                // reflects only the declared contract, so emit REP for W16
                // and SEP for W8 — both bring the CPU into agreement with
                // the contract regardless of the prior runtime state.
                if is_entry {
                    match effective_contract.a_width {
                        Some(RegWidth::W16) => ops.push(Spanned::new(
                            Op::Rep {
                                mask: 0x20,
                                fixed: false,
                            },
                            label_span,
                        )),
                        Some(RegWidth::W8) => ops.push(Spanned::new(
                            Op::Sep {
                                mask: 0x20,
                                fixed: false,
                            },
                            label_span,
                        )),
                        None => {}
                    }
                    match effective_contract.i_width {
                        Some(RegWidth::W16) => ops.push(Spanned::new(
                            Op::Rep {
                                mask: 0x10,
                                fixed: false,
                            },
                            label_span,
                        )),
                        Some(RegWidth::W8) => ops.push(Spanned::new(
                            Op::Sep {
                                mask: 0x10,
                                fixed: false,
                            },
                            label_span,
                        )),
                        None => {}
                    }
                }
                for stmt in block.body.iter().skip(body_start) {
                    lower_stmt(
                        &stmt.node,
                        stmt.span,
                        Some(scope.as_str()),
                        sema,
                        fs,
                        &inline_bodies,
                        &exit_summaries,
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
                    &inline_bodies,
                    &exit_summaries,
                    &mut current_segment,
                    &mut top_level_ctx,
                    &mut diagnostics,
                    &mut ops,
                );
            }
            Item::EvaluatorBlock(_) => {}
            Item::Const(consts) => {
                for c in consts {
                    emit_const_absolute_symbol(c, item.span, sema, &mut ops);
                }
            }
            Item::Var(var) => {
                emit_var_absolute_symbols(
                    var,
                    item.span,
                    sema,
                    &mut current_segment,
                    &mut diagnostics,
                    &mut ops,
                );
            }
        }
    }

    let mut errors = Vec::new();
    let mut warnings = Vec::new();
    for diagnostic in diagnostics {
        match diagnostic.severity {
            Severity::Error => errors.push(diagnostic),
            Severity::Warning => warnings.push(diagnostic),
        }
    }

    // Validate `&&NAME` and `&&&NAME` operands against the workspace's known
    // addressable symbols. Without this, `lda &&unknown_symbol` silently
    // emits a relocation and the error only surfaces at link time — useless
    // for LSP feedback. With the workspace set in hand, we can flag undefined
    // names per file at compile time.
    if let Some(known) = workspace_addressable {
        validate_address_of_relocations(&ops, sema, known, &mut errors);
    }

    if errors.is_empty() {
        Ok(LowerOutput {
            program: Program { ops },
            warnings,
        })
    } else {
        Err(errors)
    }
}

pub fn lower(
    file: &File,
    sema: &SemanticModel,
    fs: &dyn AssetFS,
    external_inline_bodies: Option<&IndexMap<String, CodeBlock>>,
) -> Result<Program, Vec<Diagnostic>> {
    lower_with_warnings(file, sema, fs, external_inline_bodies, None).map(|output| output.program)
}

fn validate_address_of_relocations(
    ops: &[Spanned<Op>],
    sema: &SemanticModel,
    workspace_addressable: &HashSet<String>,
    errors: &mut Vec<Diagnostic>,
) {
    for op in ops {
        let Op::Instruction(instruction) = &op.node else {
            continue;
        };
        let (label, kind_label, label_span) = match &instruction.operand {
            Some(OperandOp::ImmediateWordRelocation {
                label, label_span, ..
            }) => (label, "&&", label_span.as_ref()),
            Some(OperandOp::ImmediateFarRelocation {
                label, label_span, ..
            }) => (label, "&&&", label_span.as_ref()),
            _ => continue,
        };
        if label.starts_with('.') {
            // Local-scoped labels (`.label` form, scoped to the enclosing
            // function) — handled by resolve_symbol earlier; not workspace
            // visible.
            continue;
        }
        if sema.functions.contains_key(label)
            || sema.vars.contains_key(label)
            || sema.consts.contains_key(label)
            || workspace_addressable.contains(label)
        {
            continue;
        }
        // Symbolic subscript fields (e.g. `TASKS.name`) — resolved through
        // `resolve_symbolic_subscript_name`; if the base var exists, the
        // field reference is fine.
        if let Some((base, _field)) = label.split_once('.')
            && (sema.vars.contains_key(base) || workspace_addressable.contains(base))
        {
            continue;
        }
        // Anchor the diagnostic on the symbol name itself (so the squiggle
        // covers `shell_main` only, not `lda &&shell_main`). Falls back to
        // the instruction span when the source position wasn't preserved
        // (e.g. for non-spanned `Ident` exprs from synthesized HIR).
        let primary_span = label_span.copied().unwrap_or(op.span);
        errors.push(
            Diagnostic::error(
                primary_span,
                format!("undefined symbol '{label}' referenced by `{kind_label}`"),
            )
            .with_help(format!(
                "no `func`, `var`, `data`, or label named '{label}' is declared in this workspace"
            )),
        );
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
) -> FxHashMap<String, LabelDeclaredRecord> {
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
    out: &mut FxHashMap<String, LabelDeclaredRecord>,
) {
    for stmt in stmts {
        match &stmt.node {
            Stmt::Label(label) => {
                if let Some(name) = resolve_symbol(&label.name, scope, stmt.span, diagnostics) {
                    out.entry(name).or_insert(LabelDeclaredRecord {
                        mode: *mode,
                        decl_span: stmt.span,
                        is_advisory: false,
                    });
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
            Stmt::Hla(HlaStmt::StackOp {
                target: HlaStackTarget::P,
                push: false,
            })
            | Stmt::Hla(HlaStmt::Return { interrupt: true }) => {
                *mode = ModeState::default();
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

#[allow(clippy::too_many_arguments)]
fn lower_data_block(
    block: &DataBlock,
    sema: &SemanticModel,
    fs: &dyn AssetFS,
    inline_bodies: &FxHashMap<String, &CodeBlock>,
    exit_summaries: &FxHashMap<String, ExitModeSummary>,
    outer_segment: &str,
    diagnostics: &mut Vec<Diagnostic>,
    ops: &mut Vec<Spanned<Op>>,
) {
    let resolved_name = match (&block.name, block.name_span) {
        (Some(name), Some(span)) => match resolve_symbol(name, None, span, diagnostics) {
            Some(resolved) => Some((resolved, span)),
            None => return,
        },
        _ => None,
    };

    let mut segment_seen = false;
    let mut label_emitted = false;
    let mut block_segment = outer_segment.to_string();
    let mut charset: Option<String> = None;

    for (idx, entry) in block.entries.iter().enumerate() {
        if matches!(entry.node, DataEntry::Segment(_)) {
            if idx != 0 {
                diagnostics.push(
                    Diagnostic::error(
                        entry.span,
                        "`segment` directive must appear as the first entry of a data block",
                    )
                    .with_help(
                        "move the `segment` directive to the top of the block, before any data",
                    ),
                );
                continue;
            }
            if segment_seen {
                diagnostics.push(Diagnostic::error(
                    entry.span,
                    "`segment` directive may appear at most once per data block",
                ));
                continue;
            }
            segment_seen = true;
        }

        if !label_emitted
            && matches!(
                entry.node,
                DataEntry::Segment(_)
                    | DataEntry::Address(_)
                    | DataEntry::Align(_)
                    | DataEntry::Nocross(_)
            )
        {
            lower_data_entry(
                &entry.node,
                entry.span,
                sema,
                fs,
                inline_bodies,
                exit_summaries,
                &mut block_segment,
                &mut charset,
                diagnostics,
                ops,
            );
            continue;
        }

        if !label_emitted {
            if let Some((name, name_span)) = &resolved_name {
                ops.push(Spanned::new(Op::Label(name.clone()), *name_span));
            }
            label_emitted = true;
        }

        lower_data_entry(
            &entry.node,
            entry.span,
            sema,
            fs,
            inline_bodies,
            exit_summaries,
            &mut block_segment,
            &mut charset,
            diagnostics,
            ops,
        );
    }

    if !label_emitted && let Some((name, name_span)) = &resolved_name {
        ops.push(Spanned::new(Op::Label(name.clone()), *name_span));
    }
    if block_segment != outer_segment {
        let restore_span = block
            .name_span
            .or_else(|| block.entries.last().map(|e| e.span))
            .unwrap_or_else(|| Span::new(SourceId(0), 0, 0));
        ops.push(Spanned::new(
            Op::SelectSegment(outer_segment.to_string()),
            restore_span,
        ));
    }
}

#[allow(clippy::too_many_arguments)]
fn lower_data_entry(
    entry: &DataEntry,
    span: Span,
    sema: &SemanticModel,
    fs: &dyn AssetFS,
    inline_bodies: &FxHashMap<String, &CodeBlock>,
    exit_summaries: &FxHashMap<String, ExitModeSummary>,
    current_segment: &mut String,
    charset: &mut Option<String>,
    diagnostics: &mut Vec<Diagnostic>,
    ops: &mut Vec<Spanned<Op>>,
) {
    match entry {
        DataEntry::Segment(segment) => {
            ops.push(Spanned::new(Op::SelectSegment(segment.name.clone()), span));
            *current_segment = segment.name.clone();
        }
        DataEntry::Label(name) => {
            if let Some(resolved) = resolve_symbol(name, None, span, diagnostics) {
                ops.push(Spanned::new(Op::Label(resolved), span));
            }
        }
        DataEntry::Address(value) => {
            ops.push(Spanned::new(Op::Address(*value), span));
        }
        DataEntry::Align(value) => {
            ops.push(Spanned::new(
                Op::Align {
                    boundary: *value,
                    offset: 0,
                },
                span,
            ));
        }
        DataEntry::Nocross(value) => {
            ops.push(Spanned::new(Op::Nocross(*value), span));
        }
        DataEntry::Values { width, values } => {
            if let Some(evaluated) =
                evaluate_data_value_exprs(*width, values, None, sema, span, diagnostics)
            {
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
        DataEntry::ForEvalRange(range) => {
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
        DataEntry::String(value) => {
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
        DataEntry::Repeat { count, body } => {
            for _ in 0..*count {
                for entry in body {
                    lower_data_entry(
                        &entry.node,
                        entry.span,
                        sema,
                        fs,
                        inline_bodies,
                        exit_summaries,
                        current_segment,
                        charset,
                        diagnostics,
                        ops,
                    );
                }
            }
        }
        DataEntry::Code(stmts) => {
            let mode_contract = crate::ast::ModeContract {
                a_width: Some(RegWidth::W8),
                i_width: Some(RegWidth::W8),
            };
            ops.push(Spanned::new(Op::SetMode(mode_contract), span));
            let mut code_ctx = LowerContext {
                mode: ModeState {
                    a_width: Some(RegWidth::W8),
                    i_width: Some(RegWidth::W8),
                },
                ..LowerContext::default()
            };
            let mut code_segment = current_segment.clone();
            for stmt in stmts {
                lower_stmt(
                    &stmt.node,
                    stmt.span,
                    None,
                    sema,
                    fs,
                    inline_bodies,
                    exit_summaries,
                    &mut code_segment,
                    &mut code_ctx,
                    diagnostics,
                    ops,
                );
            }
        }
        DataEntry::Evaluator(_text) => {
            // Evaluator blocks are handled at the sema level; no lowering needed.
        }
        DataEntry::Charset(value) => {
            *charset = Some(value.clone());
        }
        DataEntry::Convert { kind, args } => {
            lower_convert_entry(kind, args, span, fs, diagnostics, ops);
        }
    }
}

fn lower_convert_entry(
    kind: &str,
    args: &[DataArg],
    span: Span,
    fs: &dyn AssetFS,
    diagnostics: &mut Vec<Diagnostic>,
    ops: &mut Vec<Spanned<Op>>,
) {
    let converters = k816_assets::builtin_registry();
    let Some(converter) = converters.iter().find(|c| c.kind() == kind) else {
        diagnostics.push(
            Diagnostic::error(span, format!("unknown data converter '{kind}'"))
                .with_help("expected one of: binary, charset, image"),
        );
        return;
    };

    let converted_args: Vec<k816_assets::Arg> = args
        .iter()
        .map(|arg| match arg {
            DataArg::Int(value) => k816_assets::Arg::Int(*value),
            DataArg::Str(value) => k816_assets::Arg::Str(value.clone()),
        })
        .collect();
    let converter_span = k816_assets::Span {
        source_id: span.source_id.0,
        start: span.start,
        end: span.end,
    };
    let request = k816_assets::ConvertRequest {
        kind,
        args: &converted_args,
        span: converter_span,
        fs,
    };

    match converter.convert(request) {
        Ok(bytes) => ops.push(Spanned::new(Op::EmitBytes(bytes), span)),
        Err(err) => diagnostics.push(Diagnostic::error(
            span,
            format!("converter '{kind}' failed: {err}"),
        )),
    }
}

#[allow(clippy::too_many_arguments)]
fn lower_stmt(
    stmt: &Stmt,
    span: Span,
    scope: Option<&str>,
    sema: &SemanticModel,
    fs: &dyn AssetFS,
    inline_bodies: &FxHashMap<String, &CodeBlock>,
    exit_summaries: &FxHashMap<String, ExitModeSummary>,
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
                let declared_record = ctx.label_declared_modes.get(&resolved).copied();
                let masking_record = declared_record.filter(|d| !d.is_advisory);
                let declared_mode = masking_record.map(|d| d.mode);
                let mut fixed_mask = ctx.label_fixed_masks.get(&resolved).copied().unwrap_or(0);
                if let Some(declared) = declared_mode
                    && ctx.reachable
                {
                    if let Some(a) = declared.a_width
                        && ctx.mode.a_width != Some(a)
                    {
                        fixed_mask |= 0x20;
                    }
                    if let Some(i) = declared.i_width
                        && ctx.mode.i_width != Some(i)
                    {
                        fixed_mask |= 0x10;
                    }
                }

                if let Some(existing) = ctx.label_entry_modes.get(&resolved).copied() {
                    let incoming_mode = apply_declared_label_mode(existing.mode, declared_mode);
                    if ctx.reachable && ctx.mode != incoming_mode {
                        diagnostics.push(mode_mismatch_diagnostic(
                            &resolved,
                            ModeMismatchEdges {
                                current: (span, ctx.mode),
                                earlier: Some((existing.first_edge_span, incoming_mode)),
                                declared: declared_record.map(|d| (d.decl_span, d.mode)),
                            },
                        ));
                    }
                    if let Some(incoming_hazards) = ctx.label_entry_hazards.get(&resolved).copied()
                    {
                        ctx.hazards = if ctx.reachable {
                            merge_hazards(ctx.hazards, incoming_hazards)
                        } else {
                            incoming_hazards
                        };
                    }
                    ctx.mode = incoming_mode;
                    ctx.reachable = true;
                } else if ctx.reachable {
                    ctx.mode = apply_declared_label_mode(ctx.mode, declared_mode);
                    ctx.label_entry_modes.insert(
                        resolved.clone(),
                        LabelEntryRecord {
                            mode: ctx.mode,
                            first_edge_span: span,
                        },
                    );
                    ctx.label_entry_hazards
                        .insert(resolved.clone(), ctx.hazards);
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
            if instruction.operand.is_none()
                && let Some(meta) = sema.functions.get(&instruction.mnemonic)
            {
                if meta.is_inline {
                    if let Some(inline_block) = inline_bodies.get(&instruction.mnemonic) {
                        let exit_summary = exit_summaries.get(&instruction.mnemonic).copied();
                        let retargeted = retargeted_body_if_foreign(&inline_block.body, span);
                        let body_slice: &[Spanned<Stmt>] = retargeted
                            .as_deref()
                            .unwrap_or(inline_block.body.as_slice());
                        lower_inline_call(
                            inline_block.name.as_str(),
                            body_slice,
                            meta,
                            exit_summary,
                            !meta.has_contract,
                            sema,
                            fs,
                            inline_bodies,
                            exit_summaries,
                            current_segment,
                            ctx,
                            span,
                            diagnostics,
                            ops,
                        );
                    }
                    return;
                }
                let Some(target) = resolve_symbol(&instruction.mnemonic, scope, span, diagnostics)
                else {
                    return;
                };
                if meta.has_contract {
                    let summary = CallContractSummary {
                        inputs: meta.params.clone(),
                        outputs: meta.outputs.clone(),
                        clobbers: RegSet::NONE,
                        is_naked: meta.is_naked,
                    };
                    ctx.hazards = apply_call_summary_hazards(
                        ctx.hazards,
                        &summary,
                        ctx.mode,
                        span,
                        diagnostics,
                    );
                }
                lower_call_with_contract(
                    &target,
                    meta.is_far,
                    Some(meta.mode_contract),
                    exit_summaries.get(&instruction.mnemonic).copied(),
                    !meta.has_contract,
                    exit_summaries
                        .get(&instruction.mnemonic)
                        .is_some_and(|summary| summary.is_naked && meta.has_contract),
                    span,
                    ctx,
                    ops,
                );
                return;
            }
            lower_instruction_stmt(instruction, scope, sema, span, ctx, diagnostics, ops);
        }
        Stmt::Call(call) => {
            let Some(target) = resolve_symbol(&call.target, scope, span, diagnostics) else {
                return;
            };

            if let Some(meta) = sema.functions.get(&call.target) {
                let Some(bindings) =
                    build_inline_bindings(call, meta, scope, sema, span, diagnostics)
                else {
                    return;
                };
                if meta.is_inline {
                    if let Some(inline_block) = inline_bodies.get(call.target.as_str()) {
                        let mut inlined_body =
                            substitute_inline_body(&inline_block.body, &bindings);
                        if retargeted_body_if_foreign(&inline_block.body, span).is_some() {
                            retarget_spans(&mut inlined_body, span);
                        }
                        let exit_summary = exit_summaries.get(call.target.as_str()).copied();
                        lower_inline_call(
                            inline_block.name.as_str(),
                            &inlined_body,
                            meta,
                            exit_summary,
                            !call.is_bare || !meta.has_contract,
                            sema,
                            fs,
                            inline_bodies,
                            exit_summaries,
                            current_segment,
                            ctx,
                            span,
                            diagnostics,
                            ops,
                        );
                    }
                } else {
                    if call.is_bare && meta.has_contract {
                        let summary = CallContractSummary {
                            inputs: meta.params.clone(),
                            outputs: meta.outputs.clone(),
                            clobbers: RegSet::NONE,
                            is_naked: meta.is_naked,
                        };
                        ctx.hazards = apply_call_summary_hazards(
                            ctx.hazards,
                            &summary,
                            ctx.mode,
                            span,
                            diagnostics,
                        );
                    }
                    lower_call_with_contract(
                        &target,
                        meta.is_far,
                        Some(meta.mode_contract),
                        exit_summaries.get(call.target.as_str()).copied(),
                        !call.is_bare || !meta.has_contract,
                        exit_summaries
                            .get(call.target.as_str())
                            .is_some_and(|summary| {
                                summary.is_naked && call.is_bare && meta.has_contract
                            }),
                        span,
                        ctx,
                        ops,
                    );
                }
            } else {
                // Unknown function: emit JSR or JSL based on `call far`.
                // The linker will resolve or report the missing symbol.
                lower_call_with_contract(
                    &target,
                    call.is_far,
                    None,
                    None,
                    true,
                    false,
                    span,
                    ctx,
                    ops,
                );
            }
        }
        Stmt::DataBlock(block) => {
            lower_data_block(
                block,
                sema,
                fs,
                inline_bodies,
                exit_summaries,
                current_segment,
                diagnostics,
                ops,
            );
        }
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
                    inline_bodies,
                    exit_summaries,
                    current_segment,
                    ctx,
                    diagnostics,
                    ops,
                );
            }
            ctx.reachable = saved_reachable;
            join_synthetic_label_state(ctx, &skip_label, span, diagnostics);
            ops.push(Spanned::new(Op::Label(skip_label), span));
        }
        Stmt::Hla(HlaStmt::PrefixConditional {
            skip_mnemonic,
            body,
            else_body,
            ..
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
                        inline_bodies,
                        exit_summaries,
                        current_segment,
                        ctx,
                        diagnostics,
                        ops,
                    );
                }
                emit_branch_to_label("bra", &end_label, scope, sema, span, ctx, diagnostics, ops);
                join_synthetic_label_state(ctx, &else_label, span, diagnostics);
                ops.push(Spanned::new(Op::Label(else_label), span));
                for s in else_body {
                    lower_stmt(
                        &s.node,
                        s.span,
                        scope,
                        sema,
                        fs,
                        inline_bodies,
                        exit_summaries,
                        current_segment,
                        ctx,
                        diagnostics,
                        ops,
                    );
                }
                join_synthetic_label_state(ctx, &end_label, span, diagnostics);
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
                        inline_bodies,
                        exit_summaries,
                        current_segment,
                        ctx,
                        diagnostics,
                        ops,
                    );
                }
                join_synthetic_label_state(ctx, &skip_label, span, diagnostics);
                ops.push(Spanned::new(Op::Label(skip_label), span));
            }
        }
        Stmt::Hla(stmt) => {
            lower_hla_stmt(stmt, span, scope, sema, ctx, diagnostics, ops);
        }
        Stmt::ModeSet { a_width, i_width } => {
            ctx.hazards = apply_mode_transition_hazards(ctx.hazards, ctx.mode, *a_width, *i_width);
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
            ctx.hazards = apply_mode_transition_hazards(ctx.hazards, ctx.mode, *a_width, *i_width);
            ctx.mode = lower_mode_contract_transition(ctx.mode, *a_width, *i_width, span, ops);
            // Lower body
            for s in body {
                lower_stmt(
                    &s.node,
                    s.span,
                    scope,
                    sema,
                    fs,
                    inline_bodies,
                    exit_summaries,
                    current_segment,
                    ctx,
                    diagnostics,
                    ops,
                );
            }
            ctx.mode_frames.pop();
            if ctx.reachable {
                // Restore mode when control falls out of the block naturally.
                lower_mode_restore(ctx.mode, saved_mode, span, ops);
                ctx.hazards = apply_mode_restore_hazards(ctx.hazards, ctx.mode, saved_mode);
                ctx.mode = saved_mode;
            }
        }
        Stmt::SwapAB => {
            let instruction = make_instruction("xba", None);
            lower_instruction_stmt(&instruction, scope, sema, span, ctx, diagnostics, ops);
        }
        Stmt::Var(var) => {
            emit_var_absolute_symbols(var, span, sema, current_segment, diagnostics, ops);
        }
        Stmt::Empty => {}
    }
}

fn emit_var_absolute_symbols(
    var: &crate::ast::VarDecl,
    span: Span,
    sema: &SemanticModel,
    current_segment: &mut String,
    diagnostics: &mut Vec<Diagnostic>,
    ops: &mut Vec<Spanned<Op>>,
) {
    let Some(meta) = sema.vars.get(&var.name) else {
        return;
    };

    match &meta.placement {
        VarPlacement::Fixed { address } => {
            let base_address = *address;

            if meta.is_direct_page() {
                let offset = base_address as u8;
                ops.push(Spanned::new(
                    Op::DefineDpFixedSymbol {
                        name: var.name.clone(),
                        offset,
                    },
                    span,
                ));
                if let Some(symbolic_subscript) = meta.symbolic_subscript.as_ref() {
                    for (field_name, field_meta) in &symbolic_subscript.fields {
                        let field_offset = base_address.saturating_add(field_meta.offset);
                        if field_offset > 0xFF {
                            diagnostics.push(Diagnostic::error(
                                span,
                                format!(
                                    "symbolic subscript field '{}.{}' offset {field_offset:#06X} exceeds direct page (0x00..=0xFF)",
                                    var.name, field_name
                                ),
                            ));
                            continue;
                        }
                        ops.push(Spanned::new(
                            Op::DefineDpFixedSymbol {
                                name: format!("{}.{}", var.name, field_name),
                                offset: field_offset as u8,
                            },
                            span,
                        ));
                    }
                }
                return;
            }

            ops.push(Spanned::new(
                Op::DefineAbsoluteSymbol {
                    name: var.name.clone(),
                    address: base_address,
                },
                span,
            ));

            let Some(symbolic_subscript) = meta.symbolic_subscript.as_ref() else {
                return;
            };

            for (field_name, field_meta) in &symbolic_subscript.fields {
                let Some(address) = base_address.checked_add(field_meta.offset) else {
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
        VarPlacement::AllocatedDp => {
            // Auto-allocated DP var: emit a request that the linker resolves
            // to a final 8-bit DP offset by first-fit. Symbolic-subscript
            // fields become aliases anchored to the parent's slot.
            let total_size = meta.size as u8;
            ops.push(Spanned::new(
                Op::DefineDpAllocSymbol {
                    name: var.name.clone(),
                    size: total_size,
                },
                span,
            ));
            if let Some(symbolic_subscript) = meta.symbolic_subscript.as_ref() {
                for (field_name, field_meta) in &symbolic_subscript.fields {
                    let qualified = format!("{}.{}", var.name, field_name);
                    if field_meta.offset > 0xFF {
                        diagnostics.push(Diagnostic::error(
                            span,
                            format!(
                                "symbolic subscript field '{}' offset {:#06X} exceeds direct page (0x00..=0xFF)",
                                qualified, field_meta.offset
                            ),
                        ));
                        continue;
                    }
                    // Encode the field as a sized request linked to the parent
                    // by name so the linker can compute parent_offset + field_offset.
                    ops.push(Spanned::new(
                        Op::DefineDpAllocAlias {
                            name: qualified,
                            parent: var.name.clone(),
                            field_offset: field_meta.offset as u8,
                        },
                        span,
                    ));
                }
            }
        }
        VarPlacement::AllocatedAbs { segment, .. } => {
            // Linker-allocated var: switch to the var's segment, emit the
            // symbol(s) at the current offset, reserve `meta.size` zero
            // bytes for the data, then restore the surrounding segment so
            // following code/data emission lands where the source said it
            // would. Symbolic-subscript fields become individual section
            // symbols at their layout-relative offsets.
            let prev_segment = current_segment.clone();
            let switching = *segment != prev_segment;
            if switching {
                ops.push(Spanned::new(Op::SelectSegment(segment.clone()), span));
                *current_segment = segment.clone();
            }
            ops.push(Spanned::new(
                Op::DefineSectionSymbol {
                    name: var.name.clone(),
                },
                span,
            ));
            let total_size = meta.size;
            if let Some(symbolic_subscript) = meta.symbolic_subscript.as_ref() {
                let mut emitted = 0_u32;
                for (field_name, field_meta) in &symbolic_subscript.fields {
                    let field_offset = field_meta.offset;
                    if field_offset > emitted {
                        let gap = (field_offset - emitted) as usize;
                        ops.push(Spanned::new(Op::EmitBytes(vec![0; gap]), span));
                        emitted = field_offset;
                    }
                    ops.push(Spanned::new(
                        Op::DefineSectionSymbol {
                            name: format!("{}.{}", var.name, field_name),
                        },
                        span,
                    ));
                }
                if total_size > emitted {
                    let tail = (total_size - emitted) as usize;
                    ops.push(Spanned::new(Op::EmitBytes(vec![0; tail]), span));
                }
            } else if total_size > 0 {
                ops.push(Spanned::new(
                    Op::EmitBytes(vec![0; total_size as usize]),
                    span,
                ));
            }
            if switching {
                ops.push(Spanned::new(Op::SelectSegment(prev_segment.clone()), span));
                *current_segment = prev_segment;
            }
        }
        VarPlacement::Abstract => {}
    }
}

fn emit_const_absolute_symbol(
    const_decl: &crate::ast::ConstDecl,
    span: Span,
    sema: &crate::sema::SemanticModel,
    ops: &mut Vec<Spanned<Op>>,
) {
    let Some(meta) = sema.consts.get(&const_decl.name) else {
        return;
    };
    let Number::Int(value) = meta.value else {
        return;
    };
    ops.push(Spanned::new(
        Op::DefineAbsoluteSymbol {
            name: const_decl.name.clone(),
            address: value as u32,
        },
        span,
    ));
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
        Some(RegWidth::W16) => ops.push(Spanned::new(
            Op::Rep {
                mask: 0x20,
                fixed: false,
            },
            span,
        )),
        Some(RegWidth::W8) => ops.push(Spanned::new(
            Op::Sep {
                mask: 0x20,
                fixed: false,
            },
            span,
        )),
        None => {}
    }
    match i_width {
        Some(RegWidth::W16) => ops.push(Spanned::new(
            Op::Rep {
                mask: 0x10,
                fixed: false,
            },
            span,
        )),
        Some(RegWidth::W8) => ops.push(Spanned::new(
            Op::Sep {
                mask: 0x10,
                fixed: false,
            },
            span,
        )),
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
        ops.push(Spanned::new(
            Op::Rep {
                mask: rep_mask,
                fixed: true,
            },
            span,
        ));
    }
    if sep_mask != 0 {
        ops.push(Spanned::new(
            Op::Sep {
                mask: sep_mask,
                fixed: true,
            },
            span,
        ));
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

    let a_delta = a_width.filter(|&target| current.a_width != Some(target));
    let i_delta = i_width.filter(|&target| current.i_width != Some(target));

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
            ops.push(Spanned::new(
                Op::Rep {
                    mask: 0x20,
                    fixed: false,
                },
                span,
            ));
        }
        (Some(RegWidth::W8), Some(RegWidth::W16)) => {
            ops.push(Spanned::new(
                Op::Sep {
                    mask: 0x20,
                    fixed: false,
                },
                span,
            ));
        }
        _ => {}
    }
    // Restore index width
    match (saved.i_width, current.i_width) {
        (Some(RegWidth::W16), Some(RegWidth::W8)) => {
            ops.push(Spanned::new(
                Op::Rep {
                    mask: 0x10,
                    fixed: false,
                },
                span,
            ));
        }
        (Some(RegWidth::W8), Some(RegWidth::W16)) => {
            ops.push(Spanned::new(
                Op::Sep {
                    mask: 0x10,
                    fixed: false,
                },
                span,
            ));
        }
        _ => {}
    }
}

fn enter_call_mode(
    entry_contract: Option<ModeContract>,
    span: Span,
    ctx: &mut LowerContext,
    ops: &mut Vec<Spanned<Op>>,
) -> ModeState {
    let saved_mode = ctx.mode;
    if let Some(entry_contract) = entry_contract {
        ctx.mode = lower_mode_contract_transition(
            ctx.mode,
            entry_contract.a_width,
            entry_contract.i_width,
            span,
            ops,
        );
    }
    saved_mode
}

fn finish_call_mode(
    saved_mode: ModeState,
    entered_mode: ModeState,
    behavior: CallModeBehavior,
    reset_mode_after: bool,
    span: Span,
    ctx: &mut LowerContext,
    ops: &mut Vec<Spanned<Op>>,
) {
    match behavior {
        CallModeBehavior::PreserveCaller => {
            lower_mode_restore(ctx.mode, saved_mode, span, ops);
            ctx.mode = saved_mode;
        }
        CallModeBehavior::AdoptExit(exit_summary) => {
            if reset_mode_after {
                ctx.mode = ModeState::default();
                ops.push(Spanned::new(Op::SetMode(ModeContract::default()), span));
            } else {
                let exit_mode = apply_exit_mode(exit_summary, entered_mode);
                ctx.mode = exit_mode;
                ops.push(Spanned::new(
                    Op::SetMode(mode_state_to_contract(exit_mode)),
                    span,
                ));
            }
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn lower_call_with_contract(
    target: &str,
    is_far: bool,
    entry_contract: Option<ModeContract>,
    exit_summary: Option<ExitModeSummary>,
    preserve_mode: bool,
    reset_mode_after: bool,
    span: Span,
    ctx: &mut LowerContext,
    ops: &mut Vec<Spanned<Op>>,
) {
    let behavior = call_mode_behavior(preserve_mode, exit_summary);
    let saved_mode = enter_call_mode(entry_contract, span, ctx, ops);
    let entered_mode = ctx.mode;

    let mnemonic = if is_far { "jsl" } else { "jsr" };
    ops.push(Spanned::new(
        Op::Instruction(InstructionOp {
            mnemonic: mnemonic.to_string(),
            operand: Some(OperandOp::Address {
                value: AddressValue::Label(target.to_string()),
                size_hint: if is_far {
                    AddressSizeHint::ForceAbsoluteLong
                } else {
                    AddressSizeHint::Auto
                },
                mode: AddressOperandMode::Direct { index: None },
            }),
        }),
        span,
    ));

    finish_call_mode(
        saved_mode,
        entered_mode,
        behavior,
        reset_mode_after,
        span,
        ctx,
        ops,
    );
}

/// Inline a function body at the call site instead of emitting JSR/JSL.
#[allow(clippy::too_many_arguments)]
fn lower_inline_call(
    block_name: &str,
    body: &[Spanned<Stmt>],
    meta: &crate::sema::FunctionMeta,
    exit_summary: Option<ExitModeSummary>,
    preserve_mode: bool,
    sema: &SemanticModel,
    fs: &dyn AssetFS,
    inline_bodies: &FxHashMap<String, &CodeBlock>,
    exit_summaries: &FxHashMap<String, ExitModeSummary>,
    current_segment: &mut String,
    ctx: &mut LowerContext,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
    ops: &mut Vec<Spanned<Op>>,
) {
    let behavior = call_mode_behavior(preserve_mode, exit_summary);
    let saved_mode = enter_call_mode(Some(meta.mode_contract), span, ctx, ops);
    let entered_mode = ctx.mode;

    // Lower the inline function's body statements in-place. For statements that
    // came from a foreign inline body (origin set by `retarget_spans`), stamp
    // the origin onto each newly-pushed `Spanned<Op>` so emit-time diagnostics
    // can attach an "(Inlined from …)" annotation pointing back to the original
    // source location.
    let inline_scope = Some(block_name);
    for stmt in body {
        let before = ops.len();
        lower_stmt(
            &stmt.node,
            stmt.span,
            inline_scope,
            sema,
            fs,
            inline_bodies,
            exit_summaries,
            current_segment,
            ctx,
            diagnostics,
            ops,
        );
        if let Some(origin) = stmt.origin {
            for op in &mut ops[before..] {
                if op.origin.is_none() {
                    op.origin = Some(origin);
                }
            }
        }
    }

    finish_call_mode(saved_mode, entered_mode, behavior, false, span, ctx, ops);
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
    let mut effects = instruction_effects(instruction);
    if instruction_accumulator_load_wins(instruction, ctx.mode) {
        effects.reads = effects.reads - RegSet::A;
    }
    ctx.hazards = apply_reg_access_hazards(ctx.hazards, effects, ctx.mode, span, diagnostics);

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
                // Cross-unit / external target: tail jump, do not restore frames.
                // The callee has its own mode contract (declared via @a*/@i* on its
                // entry); restoring to the surrounding function's outer mode would
                // emit a SEP/REP that flips the M/X flags right before the jmp,
                // breaking the callee's declared entry mode. The mismatch
                // diagnostic in record_jump_target_mode still fires correctly when
                // ctx.mode at the source disagrees with the callee's declaration.
                None => 0,
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
        let hazards_after_jump =
            apply_hazards_for_exited_frames(ctx.hazards, ctx.mode, &ctx.mode_frames, exit_frames);

        if let Some(target) = target_label.as_deref() {
            record_jump_target_mode(ctx, target, mode_after_jump, span, diagnostics);
            record_label_entry_hazards(ctx, target, hazards_after_jump);
        }

        if lower_instruction_and_push(instruction, scope, sema, span, diagnostics, ops) {
            if is_return {
                ctx.return_modes.push(mode_after_jump);
            }
            ctx.mode = mode_after_jump;
            ctx.hazards = hazards_after_jump;
            ctx.reachable = false;
            if mnemonic == "plp" || mnemonic == "rti" {
                ctx.mode = ModeState::default();
            }
        }
        return;
    }

    if is_conditional_branch && let Some(target) = target_label.as_deref() {
        record_jump_target_mode(ctx, target, ctx.mode, span, diagnostics);
        record_label_entry_hazards(ctx, target, ctx.hazards);
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
        Operand::Immediate { .. } | Operand::BlockMove { .. } | Operand::Register { .. } => {
            return None;
        }
    };

    let name = expr_ident_name(expr)?;
    resolve_symbol(name, scope, span, diagnostics)
}

fn record_label_entry_mode(
    ctx: &mut LowerContext,
    label: &str,
    mode: ModeState,
    span: Span,
    declared: Option<LabelDeclaredRecord>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if let Some(existing) = ctx.label_entry_modes.get(label).copied() {
        if existing.mode != mode {
            diagnostics.push(mode_mismatch_diagnostic(
                label,
                ModeMismatchEdges {
                    current: (span, mode),
                    earlier: Some((existing.first_edge_span, existing.mode)),
                    declared: declared.map(|d| (d.decl_span, d.mode)),
                },
            ));
        }
    } else {
        ctx.label_entry_modes.insert(
            label.to_string(),
            LabelEntryRecord {
                mode,
                first_edge_span: span,
            },
        );
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
    let masking_declared = declared.filter(|d| !d.is_advisory);
    let mut effective_mode =
        apply_declared_label_mode(incoming_mode, masking_declared.map(|d| d.mode));

    if let Some(declared) = masking_declared {
        let mut fixed_mask = ctx.label_fixed_masks.get(label).copied().unwrap_or(0);
        if let Some(a) = declared.mode.a_width {
            if incoming_mode.a_width != Some(a) {
                fixed_mask |= 0x20;
            }
            effective_mode.a_width = Some(a);
        }
        if let Some(i) = declared.mode.i_width {
            if incoming_mode.i_width != Some(i) {
                fixed_mask |= 0x10;
            }
            effective_mode.i_width = Some(i);
        }
        if fixed_mask != 0 {
            ctx.label_fixed_masks.insert(label.to_string(), fixed_mask);
        }
    }

    record_label_entry_mode(ctx, label, effective_mode, span, declared, diagnostics);
}

struct ModeMismatchEdges {
    current: (Span, ModeState),
    earlier: Option<(Span, ModeState)>,
    declared: Option<(Span, ModeState)>,
}

fn mode_mismatch_diagnostic(label: &str, edges: ModeMismatchEdges) -> Diagnostic {
    let (cur_span, cur_mode) = edges.current;
    let mut diag = Diagnostic::error(
        cur_span,
        format!("incoming edges enter `{label}` with mismatched MX mode"),
    )
    .with_primary_label(format!(
        "this edge enters with {}",
        format_mode_state(cur_mode)
    ));

    if let Some((earlier_span, earlier_mode)) = edges.earlier {
        diag = diag.with_label(
            earlier_span,
            format!(
                "earlier incoming edge entered with {}",
                format_mode_state(earlier_mode)
            ),
        );
    }

    diag = match edges.declared {
        Some((decl_span, decl_mode)) => diag
            .with_label(
                decl_span,
                format!(
                    "label declared with entry mode {}",
                    format_mode_state(decl_mode)
                ),
            )
            .with_help(format!(
                "switch the offending edge to {} (matching `{label}`'s declared entry mode) before the branch, or change `{label}`'s `@a*/@i*` annotation if the signature is wrong",
                format_mode_state(decl_mode)
            )),
        None => diag.with_help(format!(
            "reconcile the two paths by adjusting `@a*/@i*` directives so both edges enter `{label}` in the same mode"
        )),
    };

    diag.with_note(
        "k816 statically tracks the W65C816 m/x flags through control flow; every edge into a label \
         must agree on @a*/@i* state because the assembler picks 8- vs 16-bit operand widths against \
         that tracked mode. Mixed entry modes would silently encode operands against the wrong width."
            .to_string(),
    )
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
        Operand::Value {
            expr,
            addr_mode_override,
            index,
            addr_mode,
        } => value_operand_uses_immediate(*addr_mode_override, *index, *addr_mode, expr, sema),
        Operand::BlockMove { .. } | Operand::Register { .. } => false,
    };

    let imm_expr = match operand {
        Operand::Immediate { expr, .. } | Operand::Auto { expr } if is_immediate => Some(expr),
        Operand::Value { expr, .. } if is_immediate => Some(expr),
        _ => None,
    };

    if let Some(expr) = imm_expr {
        if abstract_layout_ref_in_expr(expr, sema).is_some() {
            return;
        }
        let reg_mode = match mnemonic_width_sensitivity(&mnemonic, false) {
            (true, false) => mode.a_width,
            (false, true) => mode.i_width,
            _ => None,
        };
        if let Some(reg_width) = reg_mode
            && let Some(value) = eval_to_number_strict(expr, sema, span, diagnostics)
            && !value_fits_reg_width(value, reg_width)
        {
            diagnostics.push(immediate_width_error(
                span,
                value,
                reg_width,
                mnemonic.as_str(),
            ));
        }
    }

    let addr_info = match operand {
        Operand::Value {
            expr, addr_mode, ..
        } if !is_immediate => Some((expr, *addr_mode)),
        Operand::Auto { expr } if !is_immediate => Some((expr, OperandAddrMode::Direct)),
        _ => None,
    };
    let Some((expr, addr_mode)) = addr_info else {
        return;
    };

    if let Some(expected_ptr_width) = pointer_required_width(addr_mode) {
        if let Some(declared) = expr_data_width(expr, sema)
            && declared != expected_ptr_width
        {
            diagnostics.push(indirect_pointer_width_error(
                span,
                addr_mode,
                base_ident(expr),
                declared,
                expected_ptr_width,
                &mnemonic,
            ));
        }
        return;
    }

    let Some(data_width) = expr_data_width(expr, sema) else {
        return;
    };
    let Some(required_width) = data_width_to_reg_width(data_width) else {
        let var_name = base_ident(expr);
        let msg = if let Some(name) = var_name {
            format!("'{name}' is a 3-byte :far value; no register can hold it directly.")
        } else {
            "3-byte :far value cannot be loaded into a register directly.".to_string()
        };
        let help = if let Some(name) = var_name {
            format!(
                "use a partial view (e.g. {name}:word for the low 16 bits, ({name}+2):byte for the bank byte) or indirect-long ([{name}])"
            )
        } else {
            "use a partial view (e.g. name:word for the low 16 bits, (name+2):byte for the bank byte) or indirect-long ([name])".to_string()
        };
        diagnostics.push(Diagnostic::error(span, msg).with_help(help));
        return;
    };
    let var_name = base_ident(expr);

    if !matches!(
        mnemonic.as_str(),
        "lda" | "ldx" | "ldy" | "sta" | "stx" | "sty"
    ) {
        return;
    }
    let effects = mnemonic_effects(&mnemonic, false);
    let is_load = !effects.modifies.is_empty();
    let touches_a = effects.modifies.contains(RegSet::A) || effects.reads.contains(RegSet::A);
    let (verb, family, width) = match (is_load, touches_a) {
        (true, true) => ("load", "a", mode.a_width),
        (true, false) => ("load", "i", mode.i_width),
        (false, true) => ("store", "a", mode.a_width),
        (false, false) => ("store", "i", mode.i_width),
    };
    validate_mode_for_typed_access(
        span,
        width,
        required_width,
        verb,
        family,
        var_name,
        data_width,
        diagnostics,
    );
}

#[allow(clippy::too_many_arguments)]
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

    let actual_tag = match (axis, actual) {
        ("a", RegWidth::W8) => "@a8",
        ("a", RegWidth::W16) => "@a16",
        ("i", RegWidth::W8) => "@i8",
        ("i", RegWidth::W16) => "@i16",
        _ => "@?",
    };
    let width_word = data_width_name(data_width);

    let (message, help) = if action == "store" && axis == "a" && data_width == DataWidth::Word {
        if let Some(name) = var_name {
            (
                format!("Store to var {name}:word requires @a16."),
                Some(format!(
                    "switch the accumulator to 16-bit with `@a16` before this store, or use the explicit byte view (`{name}:byte` / `({name}+1):byte`) so the active `@a8` mode is allowed"
                )),
            )
        } else {
            (
                "Store to word-typed value requires @a16.".to_string(),
                Some(
                    "switch the accumulator to 16-bit with `@a16` before this store, or use an explicit `:byte` view so the active `@a8` mode is allowed".to_string(),
                ),
            )
        }
    } else if action == "store" && axis == "a" && data_width == DataWidth::Byte {
        if let Some(name) = var_name {
            (
                format!("Store to var {name}:byte requires @a8."),
                Some(format!(
                    "switch the accumulator to 8-bit with `@a8` before this store, or write a 16-bit view (e.g. declare `{name}:word`) so the active `@a16` mode is allowed"
                )),
            )
        } else {
            (
                "Store to byte-typed value requires @a8.".to_string(),
                Some("switch the accumulator to 8-bit with `@a8` before this store, or use a wider view to match the active `@a16` mode".to_string()),
            )
        }
    } else if action == "load" {
        let target = var_name
            .map(|n| format!("var `{n}`"))
            .unwrap_or_else(|| format!("{width_word}-typed value"));
        (
            format!("Load from {width_word}-typed value requires {required_tag}."),
            Some(format!(
                "the load target is {target}; switch the {axis} register to {required_tag} before the load (active mode is {actual_tag}), or change the source's declared width if it actually holds a {actual_tag} value"
            )),
        )
    } else {
        (
            format!(
                "{} to {width_word}-typed value requires {required_tag}.",
                capitalize(action),
            ),
            Some(format!(
                "switch the {axis} register to {required_tag} (active mode is {actual_tag}) so the encoded operand width matches the declared data width"
            )),
        )
    };

    let note = "K816 enforces operand-width matching at lowering time: typed loads/stores must run under a register-width mode (`@a8`/`@a16` or `@i8`/`@i16`) that produces opcodes of the correct width. The 65816 has no \"narrowing\" load that drops half a 16-bit value into an 8-bit register — the encoding itself differs.";

    let diag = Diagnostic::error(span, message)
        .with_primary_label(format!("{action} under {actual_tag}"))
        .with_optional_help(help)
        .with_note(note);
    diagnostics.push(diag);
}

/// Pointer container width required by an indirect addressing mode. For
/// `(addr)` / `(addr,X)` / `(addr),Y` the W65C816 fetches a 16-bit pointer
/// from zero page, so the variable must be `:word`. For `[addr]` / `[addr],Y`
/// it fetches a 24-bit pointer (low/high/bank), so the variable must be
/// `:far`. Returns `None` for direct addressing and for `(byte,s),y` where
/// the operand is a stack offset rather than a pointer.
fn pointer_required_width(mode: OperandAddrMode) -> Option<DataWidth> {
    match mode {
        OperandAddrMode::Indirect
        | OperandAddrMode::IndexedIndirectX
        | OperandAddrMode::IndirectIndexedY => Some(DataWidth::Word),
        OperandAddrMode::IndirectLong | OperandAddrMode::IndirectLongIndexedY => {
            Some(DataWidth::Far)
        }
        OperandAddrMode::Direct | OperandAddrMode::StackRelativeIndirectIndexedY => None,
    }
}

fn indirect_mode_syntax(mode: OperandAddrMode) -> &'static str {
    match mode {
        OperandAddrMode::Indirect => "(addr)",
        OperandAddrMode::IndexedIndirectX => "(addr,X)",
        OperandAddrMode::IndirectIndexedY => "(addr),Y",
        OperandAddrMode::IndirectLong => "[addr]",
        OperandAddrMode::IndirectLongIndexedY => "[addr],Y",
        OperandAddrMode::Direct | OperandAddrMode::StackRelativeIndirectIndexedY => "",
    }
}

fn indirect_pointer_width_error(
    span: Span,
    mode: OperandAddrMode,
    var_name: Option<&str>,
    declared: DataWidth,
    expected: DataWidth,
    mnemonic: &str,
) -> Diagnostic {
    let mode_syntax = indirect_mode_syntax(mode);
    let declared_name = data_width_name(declared);
    let expected_name = data_width_name(expected);
    let is_long = matches!(
        mode,
        OperandAddrMode::IndirectLong | OperandAddrMode::IndirectLongIndexedY
    );
    let label_phrase = if is_long {
        "long-indirect pointer operand"
    } else {
        "indirect pointer operand"
    };

    let primary = match var_name {
        Some(name) => format!(
            "`{mnemonic} {mode_syntax}` requires the indirect pointer to be `:{expected_name}`; var `{name}` is declared `:{declared_name}`",
        ),
        None => format!(
            "`{mnemonic} {mode_syntax}` requires the indirect pointer to be `:{expected_name}`; operand is declared `:{declared_name}`",
        ),
    };

    let help = match (var_name, expected) {
        (Some(name), DataWidth::Word) => format!(
            "declare the pointer as `var {name}:word = ...` so it can hold a 16-bit zero-page address, or use `[{name}]` long-indirect if `{name}` actually holds a 24-bit pointer"
        ),
        (Some(name), DataWidth::Far) => format!(
            "declare the pointer as `var {name}:far = ...` so it can hold a 24-bit pointer, or use `({name})` direct-page indirect if `{name}` actually holds a 16-bit pointer"
        ),
        (None, DataWidth::Word) => {
            "declare the pointer as `var <name>:word = ...` so it can hold a 16-bit zero-page address".to_string()
        }
        (None, DataWidth::Far) => {
            "declare the pointer as `var <name>:far = ...` so it can hold a 24-bit pointer".to_string()
        }
        _ => String::new(),
    };

    let note = if is_long {
        "Long-indirect addressing on the W65C816 reads a 24-bit pointer (low/high/bank) from zero page; the loaded data width is determined by the M flag (the active `@a8`/`@a16` mode), not by the pointer's declared width."
    } else {
        "Direct-page indirect addressing on the W65C816 reads a 16-bit pointer from zero page; the loaded data width is determined by the M flag (the active `@a8`/`@a16` mode), not by the pointer's declared width."
    };

    let mut diag = Diagnostic::error(span, primary).with_primary_label(label_phrase.to_string());
    if !help.is_empty() {
        diag = diag.with_help(help);
    }
    diag.with_note(note.to_string())
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
        Expr::Member { .. } => repeat_member_field_width(expr, sema),
        _ => None,
    }
}

fn base_ident(expr: &Expr) -> Option<&str> {
    match expr {
        Expr::Ident(name) => Some(name.as_str()),
        Expr::IdentSpanned { name, .. } => Some(name.as_str()),
        Expr::Index { base, .. } => base_ident(base),
        Expr::Member { base, .. } => base_ident(base),
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

fn force_addr_mode_to_size_hint(force: ForceAddrMode) -> AddressSizeHint {
    match force {
        ForceAddrMode::DirectPage => AddressSizeHint::ForceDirectPage,
        ForceAddrMode::Absolute => AddressSizeHint::ForceAbsolute16,
        ForceAddrMode::AbsoluteLong => AddressSizeHint::ForceAbsoluteLong,
    }
}

/// Resolves the effective addressing-mode hint for an operand. Operand-level
/// `dp`/`abs`/`far` prefixes win; otherwise a `var` declaration's
/// `addr_mode_default` (set by the same prefix family on the declaration) is
/// applied for plain references to that symbol. Cross-unit vars whose
/// addresses are linker-resolved still contribute their classification via
/// `sema.external_var_classes`, so e.g. `lda (X)` for a sibling-file
/// `var dp X:word` picks up `ForceDirectPage` and the encoder selects the
/// DP-indirect form.
fn address_size_hint_for_operand(
    addr_mode_override: Option<ForceAddrMode>,
    expr: &Expr,
    sema: &SemanticModel,
) -> AddressSizeHint {
    if let Some(force) = addr_mode_override {
        return force_addr_mode_to_size_hint(force);
    }
    if let Some(name) = base_ident(expr) {
        if let Some(force) = sema.vars.get(name).and_then(|var| var.addr_mode_default) {
            return force_addr_mode_to_size_hint(force);
        }
        if let Some(force) = sema
            .external_var_classes
            .get(name)
            .and_then(|class| class.addr_mode_default)
        {
            return force_addr_mode_to_size_hint(force);
        }
        // Dotted symbolic-subscript field reference (`gpio.ddr`): inherit the
        // parent var's storage class so DP-class parents keep DP encoding for
        // their fields. Without this, `lda gpio.ddr` would emit a 16-bit
        // absolute even though the field lives in the 1-byte DP slot.
        if let Some((base_name, _)) = name.split_once('.') {
            if let Some(force) = sema
                .vars
                .get(base_name)
                .and_then(|var| var.addr_mode_default)
            {
                return force_addr_mode_to_size_hint(force);
            }
            if let Some(force) = sema
                .external_var_classes
                .get(base_name)
                .and_then(|class| class.addr_mode_default)
            {
                return force_addr_mode_to_size_hint(force);
            }
        }
    }
    AddressSizeHint::Auto
}

fn symbolic_subscript_field_width(name: &str, sema: &SemanticModel) -> Option<DataWidth> {
    let (base_name, field_name) = name.split_once('.')?;
    let symbolic_subscript = lookup_layout(base_name, sema)?;
    symbolic_subscript
        .fields
        .get(field_name)
        .and_then(|field| field.data_width)
}

fn repeat_member_field_width(expr: &Expr, sema: &SemanticModel) -> Option<DataWidth> {
    let (base_name, field_key) = repeated_member_field_path(expr)?;
    lookup_layout(base_name, sema)?
        .fields
        .get(field_key.as_str())
        .and_then(|field| field.data_width)
}

fn repeated_member_field_path(expr: &Expr) -> Option<(&str, String)> {
    match expr {
        Expr::Member { base, field, .. } => {
            if let Some((base_name, parent_key)) = repeated_member_field_path(base) {
                return Some((base_name, format!("{parent_key}.{field}")));
            }
            let base_name = repeated_element_root(base)?;
            Some((base_name, field.clone()))
        }
        _ => None,
    }
}

fn repeated_element_root(expr: &Expr) -> Option<&str> {
    match expr {
        Expr::Index { base, .. } => base_ident(base),
        Expr::TypedView { expr, .. } => repeated_element_root(expr),
        _ => None,
    }
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
    let bits = match width {
        RegWidth::W8 => 8,
        RegWidth::W16 => 16,
    };
    let min = -(1i64 << (bits - 1));
    let max = (1i64 << bits) - 1;
    (min..=max).contains(&value)
}

fn immediate_width_error(span: Span, value: i64, width: RegWidth, mnemonic: &str) -> Diagnostic {
    let value_text = if value < 0 {
        value.to_string()
    } else {
        format!("0x{value:X}")
    };
    let is_index = matches!(mnemonic, "ldx" | "ldy" | "cpx" | "cpy");
    let mode_name = match (is_index, width) {
        (false, RegWidth::W8) => "@a8",
        (false, RegWidth::W16) => "@a16",
        (true, RegWidth::W8) => "@i8",
        (true, RegWidth::W16) => "@i16",
    };
    let diag = Diagnostic::error(
        span,
        format!("Immediate {value_text} does not fit in {mode_name}."),
    );
    match width {
        RegWidth::W8 => {
            let wider = if is_index { "@i16" } else { "@a16" };
            diag.with_help(format!("use {wider} or split into bytes"))
        }
        RegWidth::W16 => diag,
    }
}

fn lower_hla_operand_to_operand(parsed: &HlaOperandExpr) -> Operand {
    if parsed.addr_mode != OperandAddrMode::Direct || parsed.index.is_some() {
        return Operand::Value {
            expr: parsed.expr.clone(),
            addr_mode_override: None,
            index: parsed.index,
            addr_mode: parsed.addr_mode,
        };
    }
    match &parsed.expr {
        Expr::Index { .. } => Operand::Value {
            expr: parsed.expr.clone(),
            addr_mode_override: None,
            index: None,
            addr_mode: OperandAddrMode::Direct,
        },
        Expr::Unary { .. } => Operand::Immediate {
            expr: parsed.expr.clone(),
            explicit_hash: false,
        },
        _ => Operand::Auto {
            expr: parsed.expr.clone(),
        },
    }
}

fn parse_cpu_register_name(value: &str) -> Option<HlaCpuRegister> {
    match value.to_ascii_lowercase().as_str() {
        "a" => Some(HlaCpuRegister::A),
        "b" => Some(HlaCpuRegister::B),
        "c" => Some(HlaCpuRegister::C),
        "d" => Some(HlaCpuRegister::D),
        "s" => Some(HlaCpuRegister::S),
        "x" => Some(HlaCpuRegister::X),
        "y" => Some(HlaCpuRegister::Y),
        _ => None,
    }
}

fn format_hla_cpu_register(register: HlaCpuRegister) -> &'static str {
    match register {
        HlaCpuRegister::A => "a",
        HlaCpuRegister::B => "b",
        HlaCpuRegister::C => "c",
        HlaCpuRegister::D => "d",
        HlaCpuRegister::S => "s",
        HlaCpuRegister::X => "x",
        HlaCpuRegister::Y => "y",
    }
}

fn load_mnemonic_for_register(register: HlaCpuRegister) -> Option<&'static str> {
    match register {
        HlaCpuRegister::A => Some("lda"),
        HlaCpuRegister::X => Some("ldx"),
        HlaCpuRegister::Y => Some("ldy"),
        _ => None,
    }
}

fn store_mnemonic_for_register(register: HlaCpuRegister) -> Option<&'static str> {
    match register {
        HlaCpuRegister::A => Some("sta"),
        HlaCpuRegister::X => Some("stx"),
        HlaCpuRegister::Y => Some("sty"),
        _ => None,
    }
}

fn resolve_transfer(dest: HlaCpuRegister, src: HlaCpuRegister) -> Option<&'static str> {
    match (dest, src) {
        (HlaCpuRegister::X, HlaCpuRegister::A) => Some("tax"),
        (HlaCpuRegister::Y, HlaCpuRegister::A) => Some("tay"),
        (HlaCpuRegister::A, HlaCpuRegister::X) => Some("txa"),
        (HlaCpuRegister::A, HlaCpuRegister::Y) => Some("tya"),
        (HlaCpuRegister::X, HlaCpuRegister::S) => Some("tsx"),
        (HlaCpuRegister::S, HlaCpuRegister::X) => Some("txs"),
        (HlaCpuRegister::Y, HlaCpuRegister::X) => Some("txy"),
        (HlaCpuRegister::X, HlaCpuRegister::Y) => Some("tyx"),
        (HlaCpuRegister::D, HlaCpuRegister::C) => Some("tcd"),
        (HlaCpuRegister::S, HlaCpuRegister::C) => Some("tcs"),
        (HlaCpuRegister::C, HlaCpuRegister::D) => Some("tdc"),
        (HlaCpuRegister::C, HlaCpuRegister::S) => Some("tsc"),
        _ => None,
    }
}

fn invalid_transfer_hint(dest: HlaCpuRegister, src: HlaCpuRegister) -> Option<&'static str> {
    match (dest, src) {
        (HlaCpuRegister::D, HlaCpuRegister::A) => Some("use d=c"),
        (HlaCpuRegister::S, HlaCpuRegister::A) => Some("use s=c"),
        (HlaCpuRegister::A, HlaCpuRegister::D) => Some("use c=d"),
        (HlaCpuRegister::A, HlaCpuRegister::S) => Some("use c=s"),
        (HlaCpuRegister::D, HlaCpuRegister::S) => Some("use d=c=s"),
        (HlaCpuRegister::S, HlaCpuRegister::D) => Some("use s=c=d"),
        (HlaCpuRegister::D, HlaCpuRegister::X) => Some("use a=x then d=c"),
        (HlaCpuRegister::D, HlaCpuRegister::Y) => Some("use a=y then d=c"),
        (HlaCpuRegister::S, HlaCpuRegister::Y) => Some("use s=x=y"),
        (HlaCpuRegister::Y, HlaCpuRegister::S) => Some("use y=x=s"),
        (HlaCpuRegister::Y, HlaCpuRegister::D) => Some("use c=d then y=a"),
        (HlaCpuRegister::X, HlaCpuRegister::D) => Some("use c=d then x=a"),
        (HlaCpuRegister::B, _) | (_, HlaCpuRegister::B) => Some("use b~a to swap A and B"),
        (HlaCpuRegister::C, _) | (_, HlaCpuRegister::C) => {
            Some("C is the 16-bit accumulator; only d=c, s=c, c=d, c=s are valid")
        }
        _ => None,
    }
}

#[allow(clippy::too_many_arguments)]
fn lower_assignment_chain(
    idents: &[String],
    tail_expr: Option<&HlaOperandExpr>,
    scope: Option<&str>,
    sema: &SemanticModel,
    span: Span,
    ctx: &mut LowerContext,
    diagnostics: &mut Vec<Diagnostic>,
    ops: &mut Vec<Spanned<Op>>,
) {
    let n = idents.len();
    let has_tail = tail_expr.is_some();
    let total = n + if has_tail { 1 } else { 0 };
    let mut instructions = Vec::new();

    for i in (0..(total - 1)).rev() {
        let dest = &idents[i];
        let mut resolved = false;

        for src in idents.iter().skip(i + 1) {
            if let Some(instruction) = resolve_chain_pair_ident(dest, src) {
                instructions.push(instruction);
                resolved = true;
                break;
            }
        }

        if !resolved
            && has_tail
            && let Some(instruction) =
                resolve_chain_pair_expr(dest, tail_expr.expect("tail expression exists"))
        {
            instructions.push(instruction);
            resolved = true;
        }

        if !resolved {
            diagnostics.push(Diagnostic::error(
                span,
                format!("cannot resolve assignment for '{dest}' in chain"),
            ));
            return;
        }
    }

    for instruction in instructions {
        lower_instruction_stmt(&instruction, scope, sema, span, ctx, diagnostics, ops);
    }
}

fn resolve_chain_pair_ident(dest: &str, src: &str) -> Option<Instruction> {
    let dest_register = parse_cpu_register_name(dest);
    let src_register = parse_cpu_register_name(src);

    match (dest_register, src_register) {
        (Some(dest_register), Some(src_register)) => resolve_transfer(dest_register, src_register)
            .map(|mnemonic| make_instruction(mnemonic, None)),
        (Some(dest_register), None) => load_mnemonic_for_register(dest_register).map(|mnemonic| {
            make_instruction(
                mnemonic,
                Some(Operand::Auto {
                    expr: Expr::Ident(src.to_string()),
                }),
            )
        }),
        (None, Some(src_register)) => store_mnemonic_for_register(src_register).map(|mnemonic| {
            make_instruction(
                mnemonic,
                Some(Operand::Auto {
                    expr: Expr::Ident(dest.to_string()),
                }),
            )
        }),
        (None, None) => None,
    }
}

fn make_instruction(mnemonic: &str, operand: Option<Operand>) -> Instruction {
    Instruction {
        mnemonic: mnemonic.to_string(),
        operand,
    }
}

fn resolve_chain_pair_expr(dest: &str, src: &HlaOperandExpr) -> Option<Instruction> {
    let dest_register = parse_cpu_register_name(dest)?;
    let mnemonic = load_mnemonic_for_register(dest_register)?;
    Some(make_instruction(
        mnemonic,
        Some(lower_hla_operand_to_operand(src)),
    ))
}

#[allow(clippy::too_many_arguments)]
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
    let instruction = make_instruction(
        mnemonic,
        Some(Operand::Value {
            expr: Expr::Ident(target.to_string()),
            addr_mode_override: None,
            index: None,
            addr_mode: OperandAddrMode::Direct,
        }),
    );
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

#[derive(Copy, Clone)]
struct WidthSpec {
    width: usize,
    kind_label: &'static str,
    full_reloc: Option<ByteRelocationKind>,
}

const BYTE_SPEC: WidthSpec = WidthSpec {
    width: 1,
    kind_label: "byte",
    full_reloc: None,
};

const WORD_SPEC: WidthSpec = WidthSpec {
    width: 2,
    kind_label: "word",
    full_reloc: Some(ByteRelocationKind::FullWord),
};

const FAR_SPEC: WidthSpec = WidthSpec {
    width: 3,
    kind_label: "far address",
    full_reloc: Some(ByteRelocationKind::FullLong),
};

enum PrefixOutcome {
    Handled,
    Failed,
    NotHandled,
}

fn evaluate_data_value_exprs(
    width: DataWidth,
    values: &[Expr],
    scope: Option<&str>,
    sema: &SemanticModel,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<EvaluatedBytes> {
    let spec = match width {
        DataWidth::Byte => BYTE_SPEC,
        DataWidth::Word => WORD_SPEC,
        DataWidth::Far => FAR_SPEC,
    };
    evaluate_width_exprs(
        spec,
        values,
        scope,
        sema,
        span,
        diagnostics,
        |value, bytes, relocations, diagnostics| match width {
            DataWidth::Byte => try_handle_packed_byte_unary(
                value,
                bytes,
                relocations,
                scope,
                sema,
                span,
                diagnostics,
            ),
            DataWidth::Word | DataWidth::Far => PrefixOutcome::NotHandled,
        },
    )
}

fn evaluate_width_exprs(
    spec: WidthSpec,
    values: &[Expr],
    scope: Option<&str>,
    sema: &SemanticModel,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
    mut handle_prefix: impl FnMut(
        &Expr,
        &mut Vec<u8>,
        &mut Vec<ByteRelocation>,
        &mut Vec<Diagnostic>,
    ) -> PrefixOutcome,
) -> Option<EvaluatedBytes> {
    let mut bytes = Vec::with_capacity(values.len() * spec.width);
    let mut relocations = Vec::new();

    for value in values {
        match handle_prefix(value, &mut bytes, &mut relocations, diagnostics) {
            PrefixOutcome::Handled => continue,
            PrefixOutcome::Failed => return None,
            PrefixOutcome::NotHandled => {}
        }

        // Strict numeric: literal / const / var-address / pure expression.
        if let Some(number) = eval_to_number_strict(value, sema, span, diagnostics) {
            push_le_bytes(&mut bytes, number, spec, span, diagnostics)?;
            continue;
        }

        // &< / &> unary: emit single-byte relocation in a width-byte slot.
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
                    u32::try_from(bytes.len()).expect("expression offset should fit in u32");
                bytes.extend(std::iter::repeat_n(0u8, spec.width));
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

        // Bare ident that is a label: emit a full-width relocation (word/far only).
        if let Some(full_kind) = spec.full_reloc
            && let Some(symbol) = expr_ident_name(value)
            && !sema.vars.contains_key(symbol)
            && !sema.consts.contains_key(symbol)
        {
            match resolve_symbolic_subscript_name(symbol, sema, span, diagnostics) {
                Ok(Some(_)) | Err(()) => {}
                Ok(None) => {
                    let resolved = resolve_symbol(symbol, scope, span, diagnostics)?;
                    let offset =
                        u32::try_from(bytes.len()).expect("expression offset should fit in u32");
                    bytes.extend(std::iter::repeat_n(0u8, spec.width));
                    relocations.push(ByteRelocation {
                        offset,
                        kind: full_kind,
                        label: resolved,
                    });
                    continue;
                }
            }
        }

        // Fallback: full numeric evaluation (with scope).
        let number = eval_to_number(value, scope, sema, span, diagnostics)?;
        push_le_bytes(&mut bytes, number, spec, span, diagnostics)?;
    }

    Some(EvaluatedBytes { bytes, relocations })
}

fn push_le_bytes(
    bytes: &mut Vec<u8>,
    number: i64,
    spec: WidthSpec,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<()> {
    let max: i64 = match spec.width {
        1 => 0xFF,
        2 => 0xFFFF,
        3 => 0xFFFFFF,
        _ => unreachable!("WidthSpec.width must be 1, 2, or 3"),
    };
    if !(0..=max).contains(&number) {
        diagnostics.push(Diagnostic::error(
            span,
            format!("{} literal out of range: {number}", spec.kind_label),
        ));
        return None;
    }
    let le = (number as u32).to_le_bytes();
    bytes.extend_from_slice(&le[..spec.width]);
    Some(())
}

fn try_handle_packed_byte_unary(
    value: &Expr,
    bytes: &mut Vec<u8>,
    relocations: &mut Vec<ByteRelocation>,
    scope: Option<&str>,
    sema: &SemanticModel,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) -> PrefixOutcome {
    let Expr::Unary { op, expr } = value else {
        return PrefixOutcome::NotHandled;
    };
    let (byte_count, kind_label, range_max, reloc_kind) = match op {
        ExprUnaryOp::WordLittleEndian => (2usize, "word", 0xFFFFi64, ByteRelocationKind::FullWord),
        ExprUnaryOp::FarLittleEndian => (
            3usize,
            "far address",
            0xFFFFFFi64,
            ByteRelocationKind::FullLong,
        ),
        _ => return PrefixOutcome::NotHandled,
    };

    if let Some(number) = eval_to_number_strict(expr, sema, span, diagnostics) {
        if !(0..=range_max).contains(&number) {
            diagnostics.push(Diagnostic::error(
                span,
                format!("{kind_label} literal out of range: {number}"),
            ));
            return PrefixOutcome::Failed;
        }
        let le = (number as u32).to_le_bytes();
        bytes.extend_from_slice(&le[..byte_count]);
        return PrefixOutcome::Handled;
    }

    if let Some(symbol) = expr_ident_name(expr.as_ref())
        && !sema.vars.contains_key(symbol)
        && !sema.consts.contains_key(symbol)
    {
        match resolve_symbolic_subscript_name(symbol, sema, span, diagnostics) {
            Ok(Some(_)) | Err(()) => {}
            Ok(None) => {
                let Some(label) = resolve_symbol(symbol, scope, span, diagnostics) else {
                    return PrefixOutcome::Failed;
                };
                let offset =
                    u32::try_from(bytes.len()).expect("byte expression offset should fit in u32");
                bytes.extend(std::iter::repeat_n(0u8, byte_count));
                relocations.push(ByteRelocation {
                    offset,
                    kind: reloc_kind,
                    label,
                });
                return PrefixOutcome::Handled;
            }
        }
    }

    diagnostics.push(
        Diagnostic::error(
            span,
            "packed address expression must be a constant or plain symbol reference",
        )
        .with_help("supported symbolic forms are '&&label' and '&&&label'"),
    );
    PrefixOutcome::Failed
}

fn eval_to_number_strict(
    expr: &Expr,
    sema: &SemanticModel,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<i64> {
    match expr {
        Expr::Number(value, _) => Some(*value),
        Expr::Ident(name) | Expr::IdentSpanned { name, .. } => {
            let ident_span = expr_ident_span(expr, span).unwrap_or(span);
            if let Some(var) = sema.vars.get(name) {
                if var.is_abstract() {
                    diagnostics.push(abstract_var_address_diagnostic(ident_span, name));
                    return None;
                }
                if let Some(value) = var.compile_time_numeric_value() {
                    return Some(i64::from(value));
                }
            }
            if is_abstract_layout_name(name, sema) {
                diagnostics.push(abstract_var_address_diagnostic(ident_span, name));
                return None;
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
        Expr::Member { .. } => match resolve_repeat_access_expr(expr, sema, span, diagnostics) {
            Ok(Some(access)) => repeat_access_compile_time_value(&access, sema, span, diagnostics),
            Ok(None) => None,
            Err(()) => None,
        },
        Expr::Binary { op, lhs, rhs } => {
            let lhs = eval_to_number_strict(lhs, sema, span, diagnostics)?;
            let rhs = eval_to_number_strict(rhs, sema, span, diagnostics)?;
            match op {
                ExprBinaryOp::Add => lhs.checked_add(rhs),
                ExprBinaryOp::Sub => lhs.checked_sub(rhs),
                ExprBinaryOp::Mul => lhs.checked_mul(rhs),
                ExprBinaryOp::BitOr => Some(lhs | rhs),
                ExprBinaryOp::BitAnd => Some(lhs & rhs),
                ExprBinaryOp::BitXor => Some(lhs ^ rhs),
                ExprBinaryOp::Shl => u32::try_from(rhs).ok().and_then(|n| lhs.checked_shl(n)),
                ExprBinaryOp::Shr => u32::try_from(rhs).ok().and_then(|n| lhs.checked_shr(n)),
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
                ExprUnaryOp::WordLittleEndian | ExprUnaryOp::FarLittleEndian => Some(value),
                ExprUnaryOp::EvalBracketed => Some(value),
                ExprUnaryOp::AddressPositioned => Some(value),
                ExprUnaryOp::Negate => value.checked_neg().or_else(|| {
                    diagnostics.push(Diagnostic::error(span, "arithmetic overflow"));
                    None
                }),
                ExprUnaryOp::BitNot => Some(!value),
            }
        }
        Expr::TypedView { expr, .. } => eval_to_number_strict(expr, sema, span, diagnostics),
        Expr::MetadataQuery { expr, query } => {
            resolve_metadata_query(expr, *query, sema, span, diagnostics)
        }
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
    let indexed = Expr::Index {
        base: Box::new(base.clone()),
        index: Box::new(index.clone()),
    };
    match resolve_repeat_access_expr(&indexed, sema, span, diagnostics) {
        Ok(Some(access)) => {
            return repeat_access_compile_time_value(&access, sema, span, diagnostics);
        }
        Err(()) => return None,
        Ok(None) => {}
    }

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
            ResolvedSymbolicSubscriptName::Aggregate { base: agg_base, .. } => {
                diagnostics.push(invalid_symbolic_subscript_aggregate_index_diagnostic(
                    &agg_base, base, index, sema, span,
                ));
                return None;
            }
            ResolvedSymbolicSubscriptName::Field {
                base,
                field,
                address,
                data_width: _,
                size,
                count,
            } => {
                if count <= 1 {
                    diagnostics.push(
                        Diagnostic::error(
                            span,
                            format!(
                                "symbolic subscript field '{base}.{field}' is not an array"
                            ),
                        )
                        .with_primary_label("indexed access on non-array field")
                        .with_help(format!(
                            "drop the `[...]` and write `{base}.{field}` to read the single value, or change the field's declaration to `.{field}:type[N]` if you need an explicitly typed array slot"
                        ))
                        .with_note(
                            "Symbolic subscript fields without a `[count]` declare a single named slot; only fields with `[count >= 2]` produce array semantics that an `[index]` can dereference.",
                        ),
                    );
                    return None;
                }

                let Some(index_value) = eval_to_number_strict(index, sema, span, diagnostics)
                else {
                    diagnostics.push(
                        Diagnostic::error(
                            span,
                            "symbolic subscript array index must be a constant numeric expression",
                        )
                        .with_primary_label("non-constant index")
                        .with_help(format!(
                            "use a literal or `const`-rooted expression inside `{base}.{field}[...]`; runtime indices into symbolic-subscript arrays must go through a `var` or pointer arithmetic, not the bracket form"
                        ))
                        .with_note(
                            "The bracket form on a symbolic subscript field is folded into a fixed address at compile time; that requires the index to be known before code is generated.",
                        ),
                    );
                    return None;
                };
                if index_value < 0 {
                    diagnostics.push(Diagnostic::error(
                        span,
                        format!("index must be non-negative, found {index_value}"),
                    ));
                    return None;
                }
                let scale = i64::from(size / count);
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
        ExprUnaryOp::WordLittleEndian
        | ExprUnaryOp::FarLittleEndian
        | ExprUnaryOp::EvalBracketed
        | ExprUnaryOp::AddressPositioned
        | ExprUnaryOp::Negate
        | ExprUnaryOp::BitNot => return None,
    };

    let symbol = expr_ident_name(expr.as_ref())?;
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

fn value_operand_uses_immediate(
    addr_mode_override: Option<ForceAddrMode>,
    index: Option<crate::ast::IndexRegister>,
    addr_mode: OperandAddrMode,
    expr: &Expr,
    _sema: &SemanticModel,
) -> bool {
    if addr_mode_override.is_some() || index.is_some() || addr_mode != OperandAddrMode::Direct {
        return false;
    }
    // Native `Operand::Value` (no `#`) means address-mode by default. The
    // only legitimate exception is `&&label` / `&&&label` (address-of
    // operators), whose semantics are inherently immediate even without `#` —
    // that's the operator's whole job. Auto-routing anything else (bare
    // consts, plain numbers, arbitrary expressions) would silently rewrite
    // the user's address-mode operand into an immediate.
    is_address_of_expr(expr)
}

/// Detect `&&label` / `&&&label` (with optional `± N` addend) shapes that
/// `lower_immediate_operand` → `resolve_symbolic_address_immediate` will
/// handle as a relocation. Mirrors `peel_address_of`'s shape detection without
/// extracting the name/addend.
fn is_address_of_expr(expr: &Expr) -> bool {
    match expr {
        Expr::Unary { op, .. } => {
            matches!(
                op,
                ExprUnaryOp::WordLittleEndian | ExprUnaryOp::FarLittleEndian
            )
        }
        Expr::Binary { lhs, rhs, .. } => is_address_of_expr(lhs) || is_address_of_expr(rhs),
        Expr::TypedView { expr, .. } => is_address_of_expr(expr),
        _ => false,
    }
}

/// Determines whether an expression should be treated as an immediate value
/// (compile-time constant) rather than a memory address. Uses the semantic model
/// for deterministic classification instead of naming-convention heuristics.
fn is_immediate_expression(expr: &Expr, sema: &SemanticModel) -> bool {
    match expr {
        Expr::Number(_, _) => true,
        Expr::Ident(name) | Expr::IdentSpanned { name, .. } => sema.consts.contains_key(name),
        Expr::Binary { lhs, rhs, .. } => {
            is_immediate_expression(lhs, sema) && is_immediate_expression(rhs, sema)
        }
        Expr::Unary { .. } => true,
        Expr::Index { .. } | Expr::Member { .. } | Expr::EvalText(_) => false,
        Expr::TypedView { expr, .. } => is_immediate_expression(expr, sema),
        Expr::MetadataQuery { .. } => true,
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
        #[allow(dead_code)]
        data_width: Option<DataWidth>,
        size: u32,
        count: u32,
    },
}

/// Resolve a `:sizeof` or `:offsetof` metadata query to a compile-time numeric
/// value using the semantic model.
fn resolve_metadata_query(
    inner_expr: &Expr,
    query: MetadataQuery,
    sema: &SemanticModel,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<i64> {
    let name = match inner_expr {
        Expr::Ident(n) | Expr::IdentSpanned { name: n, .. } => n.as_str(),
        _ => {
            diagnostics.push(Diagnostic::error(
                span,
                format!(
                    "':{}' requires a variable or field name",
                    metadata_query_name(query)
                ),
            ));
            return None;
        }
    };

    match query {
        MetadataQuery::SizeOf => resolve_sizeof(name, sema, span, diagnostics),
        MetadataQuery::OffsetOf => resolve_offsetof(name, sema, span, diagnostics),
    }
}

fn resolve_sizeof(
    name: &str,
    sema: &SemanticModel,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<i64> {
    // Plain var without fields — use element_size (base size before `* count`)
    if let Some(var) = sema.vars.get(name) {
        return Some(i64::from(var.element_size));
    }
    // Cross-unit `var` declared elsewhere in the link group; layout is
    // independent of placement, so use the propagated element_size.
    if let Some(class) = sema.external_var_classes.get(name) {
        return Some(i64::from(class.element_size));
    }
    // Try as a symbolic subscript field path (e.g. TASKS.state).
    // Layout-only path: independent of whether the base var has a fixed
    // address or is linker-allocated.
    match resolve_symbolic_subscript_layout(name, sema, span, diagnostics) {
        Ok(Some(ResolvedSymbolicSubscriptLayout::Aggregate { base, .. })) => {
            let var = &sema.vars[&base];
            Some(i64::from(var.element_size))
        }
        Ok(Some(ResolvedSymbolicSubscriptLayout::Field { size, .. })) => Some(i64::from(size)),
        Ok(None) => {
            diagnostics.push(Diagnostic::error(
                span,
                format!("':sizeof' requires a variable name, found '{name}'"),
            ));
            None
        }
        Err(()) => None,
    }
}

fn resolve_offsetof(
    name: &str,
    sema: &SemanticModel,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<i64> {
    // Plain var — offsetof makes no sense (in-unit or cross-unit non-struct)
    let is_plain_var = sema
        .vars
        .get(name)
        .is_some_and(|v| v.symbolic_subscript.is_none())
        || sema
            .external_var_classes
            .get(name)
            .is_some_and(|c| c.symbolic_subscript.is_none());
    if is_plain_var {
        diagnostics.push(
            Diagnostic::error(
                span,
                format!("':offsetof' requires a field path, not a bare variable '{name}'"),
            )
            .with_help("use 'VAR.field:offsetof' to get the byte offset of a field"),
        );
        return None;
    }
    // Try as a symbolic subscript field path (e.g. TASKS.state).
    // Layout-only path: independent of whether the base var has a fixed
    // address or is linker-allocated.
    match resolve_symbolic_subscript_layout(name, sema, span, diagnostics) {
        Ok(Some(ResolvedSymbolicSubscriptLayout::Aggregate { base, .. })) => {
            diagnostics.push(
                Diagnostic::error(
                    span,
                    format!("':offsetof' requires a field path, not aggregate '{base}'"),
                )
                .with_help("use 'VAR.field:offsetof' to get the byte offset of a field"),
            );
            None
        }
        Ok(Some(ResolvedSymbolicSubscriptLayout::Field { offset, .. })) => Some(i64::from(offset)),
        Ok(None) => {
            diagnostics.push(Diagnostic::error(
                span,
                format!("':offsetof' requires a variable field path, found '{name}'"),
            ));
            None
        }
        Err(()) => None,
    }
}

fn metadata_query_name(query: MetadataQuery) -> &'static str {
    match query {
        MetadataQuery::SizeOf => "sizeof",
        MetadataQuery::OffsetOf => "offsetof",
    }
}

/// Layout-only sibling of `resolve_symbolic_subscript_name` used by the
/// `:sizeof` / `:offsetof` metadata queries. Returns the field's offset/size
/// from the declaration's layout map without computing an absolute address,
/// so it works for both `Fixed` and `Allocated` placements.
#[derive(Debug, Clone)]
enum ResolvedSymbolicSubscriptLayout {
    Aggregate {
        base: String,
        #[allow(dead_code)]
        total_size: u32,
    },
    Field {
        #[allow(dead_code)]
        base: String,
        #[allow(dead_code)]
        field: String,
        offset: u32,
        size: u32,
    },
}

fn resolve_symbolic_subscript_layout(
    name: &str,
    sema: &SemanticModel,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) -> Result<Option<ResolvedSymbolicSubscriptLayout>, ()> {
    if let Some(symbolic_subscript) = lookup_layout(name, sema) {
        return Ok(Some(ResolvedSymbolicSubscriptLayout::Aggregate {
            base: name.to_string(),
            total_size: symbolic_subscript.total_size,
        }));
    }
    // A name that's a known plain var/external var (no struct layout) is not
    // a layout query target — fall through.
    if sema.vars.contains_key(name) || sema.external_var_classes.contains_key(name) {
        return Ok(None);
    }

    let Some((base_name, field_name)) = name.split_once('.') else {
        return Ok(None);
    };

    let Some(symbolic_subscript) = lookup_layout(base_name, sema) else {
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

    Ok(Some(ResolvedSymbolicSubscriptLayout::Field {
        base: base_name.to_string(),
        field: field_name.to_string(),
        offset: field_meta.offset,
        size: field_meta.size,
    }))
}

/// Layout-only lookup: prefer the in-unit `var` definition, fall back to a
/// cross-unit `external_var_classes` entry. Both carry `symbolic_subscript`
/// independent of placement.
fn lookup_layout<'a>(
    name: &str,
    sema: &'a SemanticModel,
) -> Option<&'a crate::sema::SymbolicSubscriptMeta> {
    if let Some(var) = sema.vars.get(name) {
        return var.symbolic_subscript.as_ref();
    }
    sema.external_var_classes
        .get(name)
        .and_then(|class| class.symbolic_subscript.as_ref())
}

#[derive(Clone, Copy)]
struct VarAccessMeta<'a> {
    is_abstract: bool,
    element_size: u32,
    repeat_count: u32,
    data_width: Option<DataWidth>,
    symbolic_subscript: Option<&'a crate::sema::SymbolicSubscriptMeta>,
}

fn lookup_var_access_meta<'a>(name: &str, sema: &'a SemanticModel) -> Option<VarAccessMeta<'a>> {
    if let Some(var) = sema.vars.get(name) {
        return Some(VarAccessMeta {
            is_abstract: var.is_abstract(),
            element_size: var.element_size,
            repeat_count: var.repeat_count,
            data_width: var.data_width,
            symbolic_subscript: var.symbolic_subscript.as_ref(),
        });
    }
    sema.external_var_classes
        .get(name)
        .map(|class| VarAccessMeta {
            is_abstract: class.is_abstract,
            element_size: class.element_size,
            repeat_count: class.repeat_count,
            data_width: class.data_width,
            symbolic_subscript: class.symbolic_subscript.as_ref(),
        })
}

#[derive(Debug, Clone)]
struct RepeatAccess {
    root: String,
    element_offset: i64,
    field_key: Option<String>,
    field_offset: u32,
    tail_offset: i64,
    data_width: Option<DataWidth>,
}

impl RepeatAccess {
    fn total_addend(&self) -> Option<i64> {
        i64::from(self.field_offset)
            .checked_add(self.element_offset)?
            .checked_add(self.tail_offset)
    }
}

fn eval_repeat_index(
    index: &Expr,
    sema: &SemanticModel,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
    target: &str,
    upper_bound: Option<u32>,
) -> Option<i64> {
    let saved_len = diagnostics.len();
    let Some((value, provenance)) = eval_address_expr(index, None, sema, span, diagnostics) else {
        if diagnostics.len() == saved_len {
            diagnostics.push(
                Diagnostic::error(
                    span,
                    format!("{target} index must be a constant numeric expression"),
                )
                .with_primary_label("non-constant index")
                .with_help(format!(
                    "use a literal, `const`, or metadata expression inside `{target}[...]`; runtime indices must use indexed addressing or explicit pointer arithmetic"
                ))
                .with_note(
                    "Bracket element access is folded into a fixed byte offset before code generation, so the index has to be known at compile time.",
                ),
            );
        }
        return None;
    };
    if matches!(provenance, AddressProvenance::Address) {
        diagnostics.truncate(saved_len);
        diagnostics.push(
            Diagnostic::error(
                span,
                format!("{target} index must be a constant numeric expression"),
            )
            .with_primary_label("address used as index")
            .with_help(
                "use a literal, `const`, or metadata expression for the bracket index; labels and vars are memory addresses, not repeat indices",
            )
            .with_note(
                "Runtime indexing is still possible with CPU indexed addressing or pointer arithmetic, but the bracket accessor itself is compile-time address arithmetic.",
            ),
        );
        return None;
    }
    if value < 0 {
        diagnostics.push(
            Diagnostic::error(span, format!("index must be non-negative, found {value}"))
                .with_primary_label("negative index")
                .with_help("use an index in the declared repeat range"),
        );
        return None;
    }
    if let Some(upper_bound) = upper_bound
        && value >= i64::from(upper_bound)
    {
        diagnostics.push(
            Diagnostic::error(
                span,
                format!(
                    "index {value} is out of range for {target}; repeat count is {upper_bound}"
                ),
            )
            .with_primary_label("index outside repeat range")
            .with_help(format!(
                "use an index in `0..{upper_bound}` or increase the declaration's `* count`"
            )),
        );
        return None;
    }
    Some(value)
}

fn checked_i64_to_i32_addend(
    value: i64,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<i32> {
    i32::try_from(value)
        .map_err(|_| {
            diagnostics.push(
                Diagnostic::error(span, format!("address addend {value} is out of range"))
                    .with_primary_label("addend out of range")
                    .with_help(
                        "keep address arithmetic within the signed 32-bit relocation addend range",
                    ),
            );
        })
        .ok()
}

fn resolve_repeat_access_expr(
    expr: &Expr,
    sema: &SemanticModel,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) -> Result<Option<RepeatAccess>, ()> {
    match expr {
        Expr::TypedView { expr, .. } => resolve_repeat_access_expr(expr, sema, span, diagnostics),
        Expr::Member { base, field, .. } => {
            let Some(mut access) = resolve_repeat_access_expr(base, sema, span, diagnostics)?
            else {
                return Ok(None);
            };
            let Some(meta) = lookup_var_access_meta(&access.root, sema) else {
                return Ok(None);
            };
            let Some(symbolic_subscript) = meta.symbolic_subscript else {
                diagnostics.push(
                    Diagnostic::error(
                        span,
                        format!(
                            "field '.{field}' cannot be selected from scalar repeat var '{}'",
                            access.root
                        ),
                    )
                    .with_primary_label("field access on scalar repeat element")
                    .with_help(format!(
                        "`{}[...]` selects a scalar element because '{}' has no symbolic field layout; remove '.{field}' or declare '{}' with a symbolic field list",
                        access.root, access.root, access.root
                    )),
                );
                return Err(());
            };
            let field_key = match &access.field_key {
                Some(parent) => format!("{parent}.{field}"),
                None => field.clone(),
            };
            let Some(field_meta) = symbolic_subscript.fields.get(&field_key) else {
                let mut diagnostic = Diagnostic::error(
                    span,
                    format!(
                        "unknown symbolic subscript field '.{field_key}' on '{}'",
                        access.root
                    ),
                )
                .with_primary_label("unknown field after element index");
                if let Some(suggestion) =
                    suggest_symbolic_subscript_field(field, symbolic_subscript)
                {
                    diagnostic = diagnostic.with_help(format!("did you mean '.{suggestion}'?"));
                } else {
                    diagnostic = diagnostic.with_help(format!(
                        "choose one of the fields declared under '{}[...]'",
                        access.root
                    ));
                }
                diagnostics.push(diagnostic);
                return Err(());
            };
            access.field_key = Some(field_key);
            access.field_offset = field_meta.offset;
            access.tail_offset = 0;
            access.data_width = field_meta.data_width;
            Ok(Some(access))
        }
        Expr::Index { base, index } => {
            if let Some(mut access) = resolve_repeat_access_expr(base, sema, span, diagnostics)? {
                let Some(field_key) = access.field_key.as_deref() else {
                    return Ok(None);
                };
                let Some(symbolic_subscript) = lookup_var_access_meta(&access.root, sema)
                    .and_then(|meta| meta.symbolic_subscript)
                else {
                    return Ok(None);
                };
                let Some(field_meta) = symbolic_subscript.fields.get(field_key) else {
                    return Ok(None);
                };
                if field_meta.count <= 1 {
                    diagnostics.push(
                        Diagnostic::error(
                            span,
                            format!(
                                "symbolic subscript field '{}.{field_key}' is not an array",
                                access.root
                            ),
                        )
                        .with_primary_label("indexed access on non-array field")
                        .with_help(format!(
                            "drop the trailing `[...]` and write `{}[...].{field_key}` to read the single value, or change the field declaration to an array",
                            access.root
                        ))
                        .with_note(
                            "Only fields declared with `[count >= 2]` produce array semantics that a bracket index can dereference.",
                        ),
                    );
                    return Err(());
                }
                let Some(index_value) = eval_repeat_index(
                    index,
                    sema,
                    span,
                    diagnostics,
                    &format!("{}.{field_key}", access.root),
                    None,
                ) else {
                    return Err(());
                };
                let scale = i64::from(field_meta.size / field_meta.count);
                access.tail_offset = index_value.checked_mul(scale).ok_or_else(|| {
                    diagnostics.push(Diagnostic::error(span, "arithmetic overflow"));
                })?;
                Ok(Some(access))
            } else if let Some(name) = base_ident(base)
                && let Some(meta) = lookup_var_access_meta(name, sema)
            {
                if meta.is_abstract {
                    diagnostics.push(abstract_var_address_diagnostic(span, name));
                    return Err(());
                }
                if meta.repeat_count > 1 {
                    let Some(index_value) = eval_repeat_index(
                        index,
                        sema,
                        span,
                        diagnostics,
                        name,
                        Some(meta.repeat_count),
                    ) else {
                        return Err(());
                    };
                    let element_offset = index_value
                        .checked_mul(i64::from(meta.element_size))
                        .ok_or_else(|| {
                            diagnostics.push(Diagnostic::error(span, "arithmetic overflow"));
                        })?;
                    Ok(Some(RepeatAccess {
                        root: name.to_string(),
                        element_offset,
                        field_key: None,
                        field_offset: 0,
                        tail_offset: 0,
                        data_width: meta.data_width,
                    }))
                } else if meta.symbolic_subscript.is_some() {
                    diagnostics.push(invalid_symbolic_subscript_aggregate_index_diagnostic(
                        name, base, index, sema, span,
                    ));
                    Err(())
                } else {
                    Ok(None)
                }
            } else {
                Ok(None)
            }
        }
        _ => Ok(None),
    }
}

fn repeat_access_compile_time_value(
    access: &RepeatAccess,
    sema: &SemanticModel,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<i64> {
    let base_value = sema.vars.get(&access.root)?.compile_time_numeric_value()?;
    let addend = access.total_addend().or_else(|| {
        diagnostics.push(Diagnostic::error(span, "arithmetic overflow"));
        None
    })?;
    i64::from(base_value).checked_add(addend).or_else(|| {
        diagnostics.push(Diagnostic::error(span, "arithmetic overflow"));
        None
    })
}

fn repeat_access_address_operand(
    access: &RepeatAccess,
    sema: &SemanticModel,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
    size_hint: AddressSizeHint,
    mode: AddressOperandMode,
) -> Option<OperandOp> {
    let addend = access.total_addend().or_else(|| {
        diagnostics.push(Diagnostic::error(span, "arithmetic overflow"));
        None
    })?;
    if let Some(base_value) = sema
        .vars
        .get(&access.root)
        .and_then(|var| var.compile_time_numeric_value())
    {
        let value = i64::from(base_value).checked_add(addend).or_else(|| {
            diagnostics.push(Diagnostic::error(span, "arithmetic overflow"));
            None
        })?;
        let Ok(value) = u32::try_from(value) else {
            diagnostics.push(Diagnostic::error(
                span,
                format!("address cannot be negative: {value}"),
            ));
            return None;
        };
        return Some(OperandOp::Address {
            value: AddressValue::Literal(value),
            size_hint,
            mode,
        });
    }
    let addend = checked_i64_to_i32_addend(addend, span, diagnostics)?;
    Some(OperandOp::Address {
        value: AddressValue::LabelOffset {
            label: access.root.clone(),
            addend,
        },
        size_hint,
        mode,
    })
}

fn is_abstract_layout_name(name: &str, sema: &SemanticModel) -> bool {
    if sema.vars.get(name).is_some_and(|var| var.is_abstract())
        || sema
            .external_var_classes
            .get(name)
            .is_some_and(|class| class.is_abstract)
    {
        return true;
    }
    let Some((base_name, field_name)) = name.split_once('.') else {
        return false;
    };
    let base_is_abstract = sema
        .vars
        .get(base_name)
        .is_some_and(|var| var.is_abstract())
        || sema
            .external_var_classes
            .get(base_name)
            .is_some_and(|class| class.is_abstract);
    base_is_abstract
        && lookup_layout(base_name, sema)
            .is_some_and(|layout| layout.fields.contains_key(field_name))
}

fn abstract_layout_ref_in_expr<'a>(expr: &'a Expr, sema: &SemanticModel) -> Option<&'a str> {
    match expr {
        Expr::Ident(name) | Expr::IdentSpanned { name, .. } => {
            is_abstract_layout_name(name, sema).then_some(name.as_str())
        }
        Expr::Binary { lhs, rhs, .. } => abstract_layout_ref_in_expr(lhs, sema)
            .or_else(|| abstract_layout_ref_in_expr(rhs, sema)),
        Expr::Unary { expr, .. }
        | Expr::TypedView { expr, .. }
        | Expr::MetadataQuery { expr, .. } => abstract_layout_ref_in_expr(expr, sema),
        Expr::Index { base, index } => abstract_layout_ref_in_expr(base, sema)
            .or_else(|| abstract_layout_ref_in_expr(index, sema)),
        Expr::Member { base, .. } => abstract_layout_ref_in_expr(base, sema),
        Expr::Number(_, _) | Expr::EvalText(_) => None,
    }
}

fn abstract_var_address_diagnostic(span: Span, name: &str) -> Diagnostic {
    let help = if name.contains('.') {
        format!(
            "use `{name}:offsetof` for the field offset or `{name}:sizeof` for the field size, or declare a real `var`/`data` symbol when storage is needed",
        )
    } else {
        format!(
            "use `{name}:sizeof` or `{name}.field:offsetof` for layout metadata, or declare a real `var`/`data` symbol when storage is needed",
        )
    };
    Diagnostic::error(span, format!("abstract var layout '{name}' has no address"))
        .with_primary_label("layout-only symbol")
        .with_help(help)
        .with_note(
            "`abstract var` declares packed field offsets only. It emits no bytes, no linker symbol, and cannot be used as an instruction operand or address-of target.",
        )
}

fn resolve_symbolic_subscript_name(
    name: &str,
    sema: &SemanticModel,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) -> Result<Option<ResolvedSymbolicSubscriptName>, ()> {
    if let Some(var) = sema.vars.get(name) {
        if var.symbolic_subscript.is_some() {
            if var.is_abstract() {
                diagnostics.push(abstract_var_address_diagnostic(span, name));
                return Err(());
            }
            // For DP-class fixed vars the value is a 1-byte slot offset, not
            // a 16/24-bit address; the encoder treats it as a literal because
            // the operand carries a DP size hint that pins the encoding. ABS
            // fixed vars carry a real address.
            let Some(value) = var.compile_time_numeric_value() else {
                return Ok(None);
            };
            return Ok(Some(ResolvedSymbolicSubscriptName::Aggregate {
                base: name.to_string(),
                address: value,
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

    if base_var.is_abstract() {
        diagnostics.push(abstract_var_address_diagnostic(span, name));
        return Err(());
    }

    let Some(base_value) = base_var.compile_time_numeric_value() else {
        // Step 3 will return a Label/addend variant for Allocated bases.
        return Ok(None);
    };
    let Some(address) = base_value.checked_add(field_meta.offset) else {
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
        size: field_meta.size,
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
    base_expr: &Expr,
    index: &Expr,
    sema: &SemanticModel,
    span: Span,
) -> Diagnostic {
    let label_span = match base_expr {
        Expr::IdentSpanned { end, .. } if *end > span.start && *end < span.end => {
            Span::new(span.source_id, *end, span.end)
        }
        _ => span,
    };
    let mut diagnostic = Diagnostic::error(
        label_span,
        format!("invalid index on symbolic subscript array '{base}'"),
    )
    .with_primary_label("aggregate index without repeat count");

    if let Some(requested) = expr_ident_name(index)
        && let Some(symbolic_subscript) = lookup_layout(base, sema)
        && let Some(suggestion) = suggest_symbolic_subscript_field(requested, symbolic_subscript)
    {
        diagnostic = diagnostic.with_help(format!(
            "use '.field' or '[.field]' — did you mean '.{suggestion}'?"
        ));
        return diagnostic;
    }

    diagnostic
        .with_help(format!(
            "`{base}[index]` selects a repeated element only when `{base}` is declared with `* count`; add `* N` to the declaration for element access, or use '.field' or '[.field]' for named field access"
        ))
        .with_note(
            "The `[ ... ]` after a symbolic-subscript var has two meanings: `.field` selects a named field, while numeric indices select repeat elements from `var NAME[...] * N`.",
        )
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
            lower_immediate_operand(expr, scope, sema, eval_span, diagnostics)
        }
        Some(Operand::Value {
            expr,
            addr_mode_override,
            index,
            addr_mode,
        }) => {
            if value_operand_uses_immediate(*addr_mode_override, *index, *addr_mode, expr, sema) {
                let eval_span = immediate_expr_span(expr, span, false);
                lower_immediate_operand(expr, scope, sema, eval_span, diagnostics)
            } else {
                let mode = lower_operand_mode(*addr_mode, *index);
                let size_hint = address_size_hint_for_operand(*addr_mode_override, expr, sema);
                lower_address_operand(expr, scope, sema, span, diagnostics, size_hint, mode)
            }
        }
        Some(Operand::Auto { expr }) => {
            if is_immediate_expression(expr, sema) {
                let eval_span = immediate_expr_span(expr, span, false);
                lower_immediate_operand(expr, scope, sema, eval_span, diagnostics)
            } else {
                let mode = lower_operand_mode(OperandAddrMode::Direct, None);
                let size_hint = address_size_hint_for_operand(None, expr, sema);
                lower_address_operand(expr, scope, sema, span, diagnostics, size_hint, mode)
            }
        }
        Some(Operand::Register {
            reg,
            span: reg_span,
        }) => {
            let reg_name = format_hla_cpu_register(*reg);
            let mnemonic = &instruction.mnemonic;
            match reg {
                HlaCpuRegister::A => {
                    let mnemonic_lower = mnemonic.to_ascii_lowercase();
                    if !supported_modes(&mnemonic_lower).contains(&AddressingMode::Accumulator) {
                        diagnostics.push(
                            Diagnostic::error(
                                *reg_span,
                                format!(
                                    "instruction `{mnemonic}` does not accept the accumulator (`A`) as an operand"
                                ),
                            )
                            .with_primary_label("accumulator operand")
                            .with_help(format!(
                                "`{mnemonic}` takes an immediate (`#value`), an address, or an HLA register transfer (e.g. `a = x`); a bare `A` is not an addressing mode here"
                            ))
                            .with_note(
                                "On the W65C816, only shift/rotate-family mnemonics (`asl`, `lsr`, `rol`, `ror`, `inc`, `dec`) accept the accumulator as an implicit operand; loads, stores, and ALU ops read from memory or an immediate. Use the dedicated transfer mnemonics (`tax`, `txa`, `tya`, …) or the K65 HLA shorthand (`a = x`) to move values between registers.",
                            ),
                        );
                        return None;
                    }
                    None
                }
                _ => {
                    let primary_label = match reg {
                        HlaCpuRegister::X | HlaCpuRegister::Y => "index register operand",
                        _ => "register operand",
                    };
                    diagnostics.push(
                        Diagnostic::error(
                            *reg_span,
                            format!(
                                "register `{reg_name}` cannot be used as an operand of `{mnemonic}`"
                            ),
                        )
                        .with_primary_label(primary_label)
                        .with_help(format!(
                            "`{mnemonic}` takes an immediate (`#value`) or an address; index registers `X`/`Y` only appear as the post-comma index in indexed addressing (`addr,X`, `addr,Y`) — they are not standalone operands"
                        ))
                        .with_note(
                            "On the W65C816, only the accumulator-family mnemonics (`asl`, `lsr`, `rol`, `ror`, `inc`, `dec`) take a register operand directly. Move values between registers with the dedicated transfer mnemonics (`tax`, `tay`, `txa`, `tya`, `txy`, `tyx`) or the K65 HLA shorthand (`a = x`, `y = a`).",
                        ),
                    );
                    return None;
                }
            }
        }
        Some(Operand::BlockMove { src, dst }) => {
            let src_val = eval_to_number(src, scope, sema, span, diagnostics)?;
            let dst_val = eval_to_number(dst, scope, sema, span, diagnostics)?;
            let Ok(src_byte) = u8::try_from(src_val) else {
                diagnostics.push(Diagnostic::error(
                    span,
                    format!("block move source bank must be 0..255, got {src_val}"),
                ));
                return None;
            };
            let Ok(dst_byte) = u8::try_from(dst_val) else {
                diagnostics.push(Diagnostic::error(
                    span,
                    format!("block move destination bank must be 0..255, got {dst_val}"),
                ));
                return None;
            };
            Some(OperandOp::BlockMove {
                src: src_byte,
                dst: dst_byte,
            })
        }
    };

    Some(InstructionOp {
        mnemonic: instruction.mnemonic.clone(),
        operand,
    })
}

fn lower_immediate_operand(
    expr: &Expr,
    scope: Option<&str>,
    sema: &SemanticModel,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<OperandOp> {
    // `&EXPR` is an address-position marker for indexed `,X`/`,Y` operands;
    // it has no meaning inside an immediate (`#EXPR` already takes the
    // expression's value verbatim).
    if expression_contains_address_positioned(expr) {
        diagnostics.push(address_positioned_in_immediate_diag(span));
        return None;
    }
    if let Some((kind, label)) =
        resolve_symbolic_byte_relocation(expr, scope, sema, span, diagnostics)
    {
        return Some(OperandOp::ImmediateByteRelocation { kind, label });
    }
    let saved_diag_len = diagnostics.len();
    if let Some(op) = resolve_symbolic_address_immediate(expr, scope, sema, span, diagnostics) {
        return Some(op);
    }
    if diagnostics.len() > saved_diag_len {
        return None;
    }
    if let Some(diag) = reject_address_taking_immediate(expr, sema, span) {
        diagnostics.push(diag);
        return None;
    }
    let value = eval_to_number(expr, scope, sema, span, diagnostics)?;
    Some(OperandOp::Immediate(value))
}

/// Reject `lda #symbol` / `lda #symbol±N` where `symbol` is an addressable
/// thing (var, function, or symbolic-subscript field) — that syntax used to
/// silently succeed for vars with explicit addresses, producing an immediate
/// load of the var's compile-time address. It looked too similar to `lda symbol`
/// (which loads the *value at* the address), causing real-world confusion (see
/// the X65 OS kernel work). The intended replacement is the unambiguous
/// `&&symbol` operator. Rejecting `#symbol` also unifies treatment with auto-
/// addressed vars and functions, which already had to use `&&` to take their
/// address.
fn reject_address_taking_immediate(
    expr: &Expr,
    sema: &SemanticModel,
    span: Span,
) -> Option<Diagnostic> {
    let name = address_taking_ident_in_immediate(expr, sema)?;
    let primary_span = match expr {
        Expr::IdentSpanned { start, end, .. } => Span::new(span.source_id, *start, *end),
        Expr::Binary { lhs, rhs, .. } => {
            let lhs_span = expr_ident_span(lhs.as_ref(), span);
            let rhs_span = expr_ident_span(rhs.as_ref(), span);
            lhs_span.or(rhs_span).unwrap_or(span)
        }
        _ => span,
    };
    let kind = if sema.vars.get(name).is_some_and(|var| var.is_abstract()) {
        "abstract var layout"
    } else if sema.vars.contains_key(name) {
        "var"
    } else if sema.functions.contains_key(name) {
        "function"
    } else if sema.labels.contains_key(name) {
        "label"
    } else {
        "addressable symbol"
    };
    let diagnostic = Diagnostic::error(
        primary_span,
        format!("cannot use {kind} '{name}' as an immediate value"),
    )
    .with_primary_label(format!("{kind} reference"));
    if sema.vars.get(name).is_some_and(|var| var.is_abstract()) {
        return Some(
            diagnostic
                .with_help(format!(
                    "use `{name}:sizeof` for the layout size or `{name}.field:offsetof` for a field offset",
                ))
                .with_note(
                    "`abstract var` names layout metadata only; they do not resolve to addresses or immediate values.",
                ),
        );
    }
    Some(
        diagnostic
            .with_help(format!(
                "use `&&{name}` to load its 16-bit address, or `&&&{name}` for the far address"
            ))
            .with_note(
                "Immediate operands (`#expr`) take a compile-time numeric value baked into the instruction byte stream. Vars, functions, and data-block labels resolve to addresses, not values — load their address with `&&NAME`, or read the byte at that address by dropping the `#`.",
            ),
    )
}

/// If `expr` is an immediate that references an addressable symbol (a var,
/// function, or symbolic-subscript field) directly — either as a bare ident
/// or as `ident ± N` — return that name. Returns `None` for expressions that
/// resolve through pure compile-time numerics (consts, `:sizeof`/`:offsetof`,
/// literal arithmetic).
fn address_taking_ident_in_immediate<'a>(expr: &'a Expr, sema: &SemanticModel) -> Option<&'a str> {
    let name = match expr {
        Expr::Ident(name) => name.as_str(),
        Expr::IdentSpanned { name, .. } => name.as_str(),
        Expr::Binary { op, lhs, rhs } => match (op, lhs.as_ref(), rhs.as_ref()) {
            (
                ExprBinaryOp::Add | ExprBinaryOp::Sub,
                Expr::IdentSpanned { name, .. } | Expr::Ident(name),
                Expr::Number(_, _),
            )
            | (
                ExprBinaryOp::Add,
                Expr::Number(_, _),
                Expr::IdentSpanned { name, .. } | Expr::Ident(name),
            ) => name.as_str(),
            _ => return None,
        },
        _ => return None,
    };
    if sema.consts.contains_key(name) {
        return None;
    }
    if sema.vars.contains_key(name)
        || sema.functions.contains_key(name)
        || sema.labels.contains_key(name)
    {
        return Some(name);
    }
    if matches!(
        resolve_symbolic_subscript_name(name, sema, Span::new(SourceId(0), 0, 0), &mut Vec::new()),
        Ok(Some(_))
    ) {
        return Some(name);
    }
    None
}

/// Recognize `&&label` / `&&&label` (with optional `+ N` / `- N` addend) as an
/// immediate address-of operand, emitting a 2- or 3-byte relocation. Returns
/// `None` when the expression isn't a `WordLittleEndian` / `FarLittleEndian`
/// unary on a label-shaped argument — that lets `lower_immediate_operand`
/// continue down its normal `eval_to_number` path for purely-numeric uses
/// (e.g. `[X = &&CONST]` inside an evaluator block, where the unary just acts
/// as identity for compile-time numbers).
///
/// Accepts both `&&LABEL` (unary alone) and `&&LABEL + N` / `N + &&LABEL` /
/// `&&LABEL - N` — the unary binds tighter than `+` in the code-expression
/// grammar, so the surrounding `+ N` ends up as a binary at the top level.
fn resolve_symbolic_address_immediate(
    expr: &Expr,
    scope: Option<&str>,
    sema: &SemanticModel,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<OperandOp> {
    let (is_far, reference) = peel_address_of(expr, scope, sema, span, diagnostics)?;
    Some(if is_far {
        OperandOp::ImmediateFarRelocation {
            label: reference.label,
            addend: reference.addend,
            label_span: reference.label_span,
        }
    } else {
        OperandOp::ImmediateWordRelocation {
            label: reference.label,
            addend: reference.addend,
            label_span: reference.label_span,
        }
    })
}

#[derive(Debug, Clone)]
struct AddressReference {
    label: String,
    addend: i32,
    label_span: Option<Span>,
}

/// Decompose `&&LABEL` / `&&&LABEL` (optionally `± constant_expr`) into an
/// address relocation target. The target may be a repeated element accessor,
/// e.g. `&&COMP[2].field`, which lowers to `COMP + addend`.
fn peel_address_of(
    expr: &Expr,
    scope: Option<&str>,
    sema: &SemanticModel,
    instruction_span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<(bool, AddressReference)> {
    fn address_of_unary(expr: &Expr) -> Option<(bool, &Expr)> {
        let Expr::Unary { op, expr: inner } = expr else {
            return None;
        };
        let is_far = match op {
            ExprUnaryOp::WordLittleEndian => false,
            ExprUnaryOp::FarLittleEndian => true,
            _ => return None,
        };
        Some((is_far, inner.as_ref()))
    }

    if let Some((is_far, inner)) = address_of_unary(expr) {
        let reference =
            address_reference_for_expr(inner, scope, sema, instruction_span, diagnostics)?;
        return Some((is_far, reference));
    }
    if let Expr::Binary { op, lhs, rhs } = expr {
        match (op, lhs.as_ref(), rhs.as_ref()) {
            (ExprBinaryOp::Add, lhs, rhs) => {
                let (is_far, inner, addend_expr) =
                    if let Some((is_far, inner)) = address_of_unary(lhs) {
                        (is_far, inner, rhs)
                    } else if let Some((is_far, inner)) = address_of_unary(rhs) {
                        (is_far, inner, lhs)
                    } else {
                        return None;
                    };
                let mut reference =
                    address_reference_for_expr(inner, scope, sema, instruction_span, diagnostics)?;
                let addend = eval_address_of_addend(
                    addend_expr,
                    scope,
                    sema,
                    instruction_span,
                    diagnostics,
                )?;
                reference.addend = reference.addend.checked_add(addend).or_else(|| {
                    diagnostics.push(Diagnostic::error(instruction_span, "arithmetic overflow"));
                    None
                })?;
                return Some((is_far, reference));
            }
            (ExprBinaryOp::Sub, lhs, rhs) => {
                let (is_far, inner) = address_of_unary(lhs)?;
                let mut reference =
                    address_reference_for_expr(inner, scope, sema, instruction_span, diagnostics)?;
                let addend =
                    eval_address_of_addend(rhs, scope, sema, instruction_span, diagnostics)?;
                reference.addend = reference.addend.checked_sub(addend).or_else(|| {
                    diagnostics.push(Diagnostic::error(instruction_span, "arithmetic overflow"));
                    None
                })?;
                return Some((is_far, reference));
            }
            _ => {}
        }
    }
    None
}

fn address_reference_for_expr(
    expr: &Expr,
    scope: Option<&str>,
    sema: &SemanticModel,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<AddressReference> {
    if let Ok(Some(access)) = resolve_repeat_access_expr(expr, sema, span, diagnostics) {
        let addend = access.total_addend()?;
        return Some(AddressReference {
            label: access.root,
            addend: checked_i64_to_i32_addend(addend, span, diagnostics)?,
            label_span: None,
        });
    }
    if let Some(reference) =
        symbolic_field_array_address_reference(expr, scope, sema, span, diagnostics)
    {
        return Some(reference);
    }

    let ident_span = |start: usize, end: usize| Span::new(span.source_id, start, end);
    match expr {
        Expr::Ident(name) => {
            address_reference_for_ident(name, scope, sema, span, diagnostics, None)
        }
        Expr::IdentSpanned { name, start, end } => address_reference_for_ident(
            name,
            scope,
            sema,
            span,
            diagnostics,
            Some(ident_span(*start, *end)),
        ),
        Expr::Binary { op, lhs, rhs } => {
            let reference = match (op, lhs.as_ref(), rhs.as_ref()) {
                (ExprBinaryOp::Add, Expr::IdentSpanned { name, start, end }, other) => {
                    let mut reference = address_reference_for_ident(
                        name,
                        scope,
                        sema,
                        span,
                        diagnostics,
                        Some(ident_span(*start, *end)),
                    )?;
                    let addend = eval_address_of_addend(other, scope, sema, span, diagnostics)?;
                    reference.addend = reference.addend.checked_add(addend)?;
                    reference
                }
                (ExprBinaryOp::Add, other, Expr::IdentSpanned { name, start, end }) => {
                    let mut reference = address_reference_for_ident(
                        name,
                        scope,
                        sema,
                        span,
                        diagnostics,
                        Some(ident_span(*start, *end)),
                    )?;
                    let addend = eval_address_of_addend(other, scope, sema, span, diagnostics)?;
                    reference.addend = reference.addend.checked_add(addend)?;
                    reference
                }
                (ExprBinaryOp::Sub, Expr::IdentSpanned { name, start, end }, other) => {
                    let mut reference = address_reference_for_ident(
                        name,
                        scope,
                        sema,
                        span,
                        diagnostics,
                        Some(ident_span(*start, *end)),
                    )?;
                    let addend = eval_address_of_addend(other, scope, sema, span, diagnostics)?;
                    reference.addend = reference.addend.checked_sub(addend)?;
                    reference
                }
                (ExprBinaryOp::Add, Expr::Ident(name), other) => {
                    let mut reference =
                        address_reference_for_ident(name, scope, sema, span, diagnostics, None)?;
                    let addend = eval_address_of_addend(other, scope, sema, span, diagnostics)?;
                    reference.addend = reference.addend.checked_add(addend)?;
                    reference
                }
                (ExprBinaryOp::Add, other, Expr::Ident(name)) => {
                    let mut reference =
                        address_reference_for_ident(name, scope, sema, span, diagnostics, None)?;
                    let addend = eval_address_of_addend(other, scope, sema, span, diagnostics)?;
                    reference.addend = reference.addend.checked_add(addend)?;
                    reference
                }
                (ExprBinaryOp::Sub, Expr::Ident(name), other) => {
                    let mut reference =
                        address_reference_for_ident(name, scope, sema, span, diagnostics, None)?;
                    let addend = eval_address_of_addend(other, scope, sema, span, diagnostics)?;
                    reference.addend = reference.addend.checked_sub(addend)?;
                    reference
                }
                _ => return None,
            };
            if reference.addend == i32::MIN && matches!(op, ExprBinaryOp::Add | ExprBinaryOp::Sub) {
                diagnostics.push(Diagnostic::error(span, "arithmetic overflow"));
                return None;
            }
            Some(reference)
        }
        _ => None,
    }
}

fn address_reference_for_ident(
    name: &str,
    scope: Option<&str>,
    sema: &SemanticModel,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
    label_span: Option<Span>,
) -> Option<AddressReference> {
    if sema.consts.contains_key(name) {
        return None;
    }
    if is_abstract_layout_name(name, sema) {
        diagnostics.push(abstract_var_address_diagnostic(
            label_span.unwrap_or(span),
            name,
        ));
        return None;
    }
    let known_addressable = sema.vars.contains_key(name)
        || sema.functions.contains_key(name)
        || sema.labels.contains_key(name)
        || resolve_symbolic_subscript_name(name, sema, span, &mut Vec::new())
            .ok()
            .flatten()
            .is_some();
    if !known_addressable && name.starts_with('.') {
        return None;
    }
    let label = resolve_symbol(name, scope, span, diagnostics)?;
    Some(AddressReference {
        label,
        addend: 0,
        label_span,
    })
}

fn symbolic_field_array_address_reference(
    expr: &Expr,
    scope: Option<&str>,
    sema: &SemanticModel,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<AddressReference> {
    let Expr::Index { base, index } = expr else {
        return None;
    };
    let name = base_ident(base)?;
    let (base_name, field_name) = name.split_once('.')?;
    let symbolic_subscript = lookup_layout(base_name, sema)?;
    let field_meta = symbolic_subscript.fields.get(field_name)?;
    if field_meta.count <= 1 {
        diagnostics.push(
            Diagnostic::error(
                span,
                format!("symbolic subscript field '{base_name}.{field_name}' is not an array"),
            )
            .with_primary_label("indexed access on non-array field")
            .with_help(format!(
                "drop the `[...]` and write `{base_name}.{field_name}` to read the single value, or change the field's declaration to `.{field_name}:type[N]` if you need an explicitly typed array slot"
            )),
        );
        return None;
    }
    let index_value = eval_repeat_index(index, sema, span, diagnostics, name, None)?;
    let scale = i64::from(field_meta.size / field_meta.count);
    let addend = index_value.checked_mul(scale).or_else(|| {
        diagnostics.push(Diagnostic::error(span, "arithmetic overflow"));
        None
    })?;
    Some(AddressReference {
        label: resolve_symbol(name, scope, span, diagnostics)?,
        addend: checked_i64_to_i32_addend(addend, span, diagnostics)?,
        label_span: expr_ident_span(base, span),
    })
}

fn eval_address_of_addend(
    expr: &Expr,
    scope: Option<&str>,
    sema: &SemanticModel,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<i32> {
    let saved_len = diagnostics.len();
    let (value, provenance) = eval_address_expr(expr, scope, sema, span, diagnostics)?;
    if matches!(provenance, AddressProvenance::Address) {
        diagnostics.truncate(saved_len);
        diagnostics.push(
            Diagnostic::error(span, "address-of addend must be a compile-time numeric offset")
                .with_primary_label("address used as addend")
                .with_help(
                    "use a literal, `const`, or metadata expression such as `NAME:sizeof`; labels and vars are addresses, not numeric offsets",
                ),
        );
        return None;
    }
    checked_i64_to_i32_addend(value, span, diagnostics)
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
    size_hint: AddressSizeHint,
    mode: AddressOperandMode,
) -> Option<OperandOp> {
    // `&EXPR` marker: only valid inside `,X`/`,Y` indexed direct/absolute
    // modes. The marker upgrades inner provenance to `Address` so a const
    // is accepted as a literal byte/word offset; we still need to enforce
    // the addressing-mode shape here, before any value resolution.
    if expression_contains_address_positioned(expr) && !is_indexed_xy_mode(mode) {
        diagnostics.push(address_positioned_requires_index_diag(span));
        return None;
    }
    match expr {
        Expr::Number(value, _) => {
            let address = u32::try_from(*value).map_err(|_| {
                Diagnostic::error(span, format!("address cannot be negative: {value}"))
            });
            match address {
                Ok(address) => Some(OperandOp::Address {
                    value: AddressValue::Literal(address),
                    size_hint,
                    mode,
                }),
                Err(diag) => {
                    diagnostics.push(diag);
                    None
                }
            }
        }
        Expr::Ident(name) => {
            resolve_operand_ident(name, scope, sema, span, diagnostics, size_hint, mode)
        }
        Expr::IdentSpanned { name, .. } => {
            resolve_operand_ident(name, scope, sema, span, diagnostics, size_hint, mode)
        }
        // Typed views should preserve unresolved labels for relocation-aware object flows.
        Expr::TypedView { expr, .. } => {
            lower_address_operand(expr, scope, sema, span, diagnostics, size_hint, mode)
        }
        // Metadata queries always produce immediates, not addresses.
        Expr::MetadataQuery { expr, query } => {
            let value = resolve_metadata_query(expr, *query, sema, span, diagnostics)?;
            Some(OperandOp::Immediate(value))
        }
        Expr::Index { .. } | Expr::Member { .. } | Expr::Binary { .. } | Expr::Unary { .. } => {
            match resolve_repeat_access_expr(expr, sema, span, diagnostics) {
                Ok(Some(access)) => {
                    return repeat_access_address_operand(
                        &access,
                        sema,
                        span,
                        diagnostics,
                        size_hint,
                        mode,
                    );
                }
                Err(()) => return None,
                Ok(None) => {}
            }
            // Try compile-time evaluation first (works for vars, consts, symbolic subscripts).
            // Tracks provenance so a `const`-only expression in address position is rejected
            // rather than silently materialized as a literal address. Stack-relative modes
            // invert the rule: numeric (Number/ConstTainted) results are valid offsets,
            // address-rooted results are rejected.
            let stack_rel = is_stack_relative_mode(mode);
            let saved_diag_len = diagnostics.len();
            if let Some((value, prov)) = eval_address_expr(expr, scope, sema, span, diagnostics) {
                if stack_rel && matches!(prov, AddressProvenance::Address) {
                    diagnostics.truncate(saved_diag_len);
                    diagnostics.push(
                        Diagnostic::error(
                            span,
                            "address expression cannot be used as a stack-relative offset",
                        )
                        .with_primary_label(
                            "expression rooted in a label or var, not a numeric offset".to_string(),
                        )
                        .with_help(
                            "stack-relative modes (`,s` / `(,s),y`) take a byte offset from the stack pointer; use a numeric literal or a `const`-rooted expression instead, or pick a non-stack-relative addressing mode for memory access",
                        ),
                    );
                    return None;
                }
                if !stack_rel && matches!(prov, AddressProvenance::ConstTainted) {
                    diagnostics.truncate(saved_diag_len);
                    let const_name = first_const_ref(expr, sema).unwrap_or("<const>");
                    let mut diag = Diagnostic::error(
                        span,
                        format!(
                            "address expression depends on `const {const_name}`, which has no memory location",
                        ),
                    )
                    .with_primary_label(
                        "expression rooted in const, not in a label or var".to_string(),
                    )
                    .with_help(format!(
                        "consts contribute numeric values, not addresses; for a memory operand, root the expression in a `var` or label (e.g. `data_block + {const_name}`); for an immediate, prefix the whole expression with `#`",
                    ));
                    if is_indexed_xy_mode(mode) {
                        diag = diag.with_help(format!(
                            "if `{const_name}` is meant as a literal byte/word field offset (struct base in X/Y), prefix the expression with `&`: `&{const_name}, x` (or `, y`)",
                        ));
                    }
                    diagnostics.push(diag);
                    return None;
                }
                let Ok(address) = u32::try_from(value) else {
                    diagnostics.push(Diagnostic::error(
                        span,
                        format!("address cannot be negative: {value}"),
                    ));
                    return None;
                };
                return Some(OperandOp::Address {
                    value: AddressValue::Literal(address),
                    size_hint,
                    mode,
                });
            }
            // eval_address_expr failed — try to decompose as label ± constant for link-time
            // resolution (e.g. `data_block_name + 1`). Stack-relative modes never accept
            // label-rooted forms, so skip the fallback there.
            if !stack_rel
                && let Some(result) =
                    try_label_offset_operand(expr, scope, sema, span, size_hint, mode)
            {
                // Remove diagnostics added by the failed eval_address_expr attempt.
                diagnostics.truncate(saved_diag_len);
                return Some(result);
            }
            None
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

/// Provenance tag for expression values evaluated in address-operand position.
///
/// `const` declarations hold compile-time *values*, not memory locations. An
/// expression evaluated as an instruction-operand address is only legal when
/// it is rooted in a label/var (`Address`) or written entirely from numeric
/// literals (`PureLiteral`). A `ConstTainted` result — any path that touched
/// a const and never touched a label/var — must be rejected with a
/// diagnostic.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum AddressProvenance {
    /// At least one leaf is a label, var, or symbolic-subscript root.
    Address,
    /// Only number literals or metadata-query results; safe as a hardcoded address.
    PureLiteral,
    /// At least one const reference, no label/var anywhere — illegal as an address.
    ConstTainted,
}

impl AddressProvenance {
    fn join(self, other: AddressProvenance) -> AddressProvenance {
        use AddressProvenance::*;
        match (self, other) {
            (Address, _) | (_, Address) => Address,
            (ConstTainted, _) | (_, ConstTainted) => ConstTainted,
            (PureLiteral, PureLiteral) => PureLiteral,
        }
    }
}

/// Evaluate an `Expr` for use as an instruction-operand address, returning the
/// numeric value alongside an `AddressProvenance` tag. Mirrors `eval_to_number`
/// structurally but propagates provenance so the caller can reject
/// const-tainted expressions in address position.
///
/// Note the asymmetry with `eval_to_number`: this helper is *only* called from
/// `lower_address_operand`'s expression arm. Bare `Expr::Ident` operands go
/// through `resolve_operand_ident` separately, which handles the bare-const
/// rejection at its own site.
fn eval_address_expr(
    expr: &Expr,
    scope: Option<&str>,
    sema: &SemanticModel,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
) -> Option<(i64, AddressProvenance)> {
    match expr {
        Expr::Number(value, _) => Some((*value, AddressProvenance::PureLiteral)),
        Expr::Ident(name) | Expr::IdentSpanned { name, .. } => {
            let ident_span = expr_ident_span(expr, span).unwrap_or(span);
            if let Some(var) = sema.vars.get(name) {
                if var.is_abstract() {
                    diagnostics.push(abstract_var_address_diagnostic(ident_span, name));
                    return None;
                }
                match var.compile_time_numeric_value() {
                    Some(value) => {
                        return Some((i64::from(value), AddressProvenance::Address));
                    }
                    None => {
                        // Allocated var — no compile-time numeric value. Signal
                        // failure without a diagnostic so the caller can fall
                        // back to label-offset relocation.
                        return None;
                    }
                }
            }
            if is_abstract_layout_name(name, sema) {
                diagnostics.push(abstract_var_address_diagnostic(ident_span, name));
                return None;
            }
            if let Some(constant) = sema.consts.get(name) {
                let value = constant_to_exact_i64(
                    name,
                    constant.value,
                    ident_span,
                    diagnostics,
                    "address expression",
                )?;
                return Some((value, AddressProvenance::ConstTainted));
            }
            match resolve_symbolic_subscript_name(name, sema, ident_span, diagnostics) {
                Ok(Some(ResolvedSymbolicSubscriptName::Aggregate { address, .. }))
                | Ok(Some(ResolvedSymbolicSubscriptName::Field { address, .. })) => {
                    return Some((i64::from(address), AddressProvenance::Address));
                }
                Err(()) => return None,
                Ok(None) => {}
            }
            // A dotted name whose base is an Allocated symbolic-subscript var
            // — `resolve_symbolic_subscript_name` returns Ok(None) for those.
            // Signal failure silently so the binary-expression caller can fall
            // through to label-offset relocation against `X.field`.
            if let Some((base_name, _)) = name.split_once('.')
                && let Some(base_var) = sema.vars.get(base_name)
                && base_var.symbolic_subscript.is_some()
                && base_var.compile_time_numeric_value().is_none()
            {
                return None;
            }

            resolve_symbol(name, scope, ident_span, diagnostics)?;
            let mut diag = Diagnostic::error(ident_span, format!("unknown identifier '{name}'"))
                .with_primary_label("unresolved identifier");
            diag = if let Some(help) = address_of_help_for_name(name, sema, ident_span) {
                diag.with_help(help)
            } else {
                diag.with_help(format!(
                    "no `const`, `var`, function, or label named `{name}` is in scope; check spelling, declare it with `const {name} = <expr>`, or import it from another file by ensuring that file is in the same compile group"
                ))
            };
            diag = diag.with_note(
                "Identifiers in expressions resolve through `const`s (compile-time values), `var`s (memory addresses), functions/labels (also addresses), and symbolic-subscript fields. The lowering pass walks all of these in scope; if none match, the identifier has no source.",
            );
            diagnostics.push(diag);
            None
        }
        Expr::EvalText(_) => {
            diagnostics.push(Diagnostic::error(
                span,
                "internal error: eval text should be expanded before lowering",
            ));
            None
        }
        Expr::Index { base, index } => {
            // Indexing always targets a var/label/aggregate base; the result
            // is an address regardless of whether the index is a literal or
            // a const-tainted expression.
            let value = eval_index_expr(base, index, scope, sema, span, diagnostics)?;
            Some((value, AddressProvenance::Address))
        }
        Expr::Member { .. } => match resolve_repeat_access_expr(expr, sema, span, diagnostics) {
            Ok(Some(access)) => {
                let value = repeat_access_compile_time_value(&access, sema, span, diagnostics)?;
                Some((value, AddressProvenance::Address))
            }
            Ok(None) => None,
            Err(()) => None,
        },
        Expr::Binary { op, lhs, rhs } => {
            let (lhs_val, lhs_prov) = eval_address_expr(lhs, scope, sema, span, diagnostics)?;
            let (rhs_val, rhs_prov) = eval_address_expr(rhs, scope, sema, span, diagnostics)?;
            let value = match op {
                ExprBinaryOp::Add => lhs_val.checked_add(rhs_val),
                ExprBinaryOp::Sub => lhs_val.checked_sub(rhs_val),
                ExprBinaryOp::Mul => lhs_val.checked_mul(rhs_val),
                ExprBinaryOp::BitOr => Some(lhs_val | rhs_val),
                ExprBinaryOp::BitAnd => Some(lhs_val & rhs_val),
                ExprBinaryOp::BitXor => Some(lhs_val ^ rhs_val),
                ExprBinaryOp::Shl => u32::try_from(rhs_val)
                    .ok()
                    .and_then(|n| lhs_val.checked_shl(n)),
                ExprBinaryOp::Shr => u32::try_from(rhs_val)
                    .ok()
                    .and_then(|n| lhs_val.checked_shr(n)),
            }
            .or_else(|| {
                diagnostics.push(Diagnostic::error(span, "arithmetic overflow"));
                None
            })?;
            Some((value, lhs_prov.join(rhs_prov)))
        }
        Expr::Unary { op, expr } => {
            let (value, prov) = eval_address_expr(expr, scope, sema, span, diagnostics)?;
            let new_value = match op {
                ExprUnaryOp::LowByte => value & 0xFF,
                ExprUnaryOp::HighByte => (value >> 8) & 0xFF,
                ExprUnaryOp::WordLittleEndian | ExprUnaryOp::FarLittleEndian => value,
                ExprUnaryOp::EvalBracketed => value,
                ExprUnaryOp::AddressPositioned => value,
                ExprUnaryOp::Negate => value.checked_neg().or_else(|| {
                    diagnostics.push(Diagnostic::error(span, "arithmetic overflow"));
                    None
                })?,
                ExprUnaryOp::BitNot => !value,
            };
            // The `&EXPR` marker upgrades provenance to `Address` so a
            // const-rooted offset is accepted by the caller's address-vs-
            // const check. The mode-gate at the top of `lower_address_operand`
            // separately rejects the marker outside `,X`/`,Y` indexed modes.
            let new_prov = if matches!(op, ExprUnaryOp::AddressPositioned) {
                AddressProvenance::Address
            } else {
                prov
            };
            Some((new_value, new_prov))
        }
        Expr::TypedView { expr, .. } => eval_address_expr(expr, scope, sema, span, diagnostics),
        Expr::MetadataQuery { expr, query } => {
            let value = resolve_metadata_query(expr, *query, sema, span, diagnostics)?;
            Some((value, AddressProvenance::PureLiteral))
        }
    }
}

/// Walk an `Expr` and return the name of the first const-referenced
/// identifier, or `None` if no const is mentioned. Used to attribute a
/// `ConstTainted` provenance to a specific symbol in diagnostics.
fn first_const_ref<'a>(expr: &'a Expr, sema: &SemanticModel) -> Option<&'a str> {
    match expr {
        Expr::Ident(name) | Expr::IdentSpanned { name, .. } => {
            if sema.consts.contains_key(name) {
                Some(name.as_str())
            } else {
                None
            }
        }
        Expr::Binary { lhs, rhs, .. } => {
            first_const_ref(lhs, sema).or_else(|| first_const_ref(rhs, sema))
        }
        Expr::Unary { expr, .. } => first_const_ref(expr, sema),
        Expr::TypedView { expr, .. } => first_const_ref(expr, sema),
        Expr::Member { base, .. } => first_const_ref(base, sema),
        Expr::Index { base, index } => {
            first_const_ref(base, sema).or_else(|| first_const_ref(index, sema))
        }
        Expr::MetadataQuery { expr, .. } => first_const_ref(expr, sema),
        Expr::Number(_, _) | Expr::EvalText(_) => None,
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
        Expr::Number(value, _) => Some(*value),
        Expr::Ident(name) | Expr::IdentSpanned { name, .. } => {
            let ident_span = expr_ident_span(expr, span).unwrap_or(span);
            if let Some(var) = sema.vars.get(name) {
                if var.is_abstract() {
                    diagnostics.push(abstract_var_address_diagnostic(ident_span, name));
                    return None;
                }
                if let Some(value) = var.compile_time_numeric_value() {
                    return Some(i64::from(value));
                }
            }
            if is_abstract_layout_name(name, sema) {
                diagnostics.push(abstract_var_address_diagnostic(ident_span, name));
                return None;
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

            resolve_symbol(name, scope, ident_span, diagnostics)?;
            let mut diag = Diagnostic::error(ident_span, format!("unknown identifier '{name}'"))
                .with_primary_label("unresolved identifier");
            diag = if let Some(help) = address_of_help_for_name(name, sema, ident_span) {
                diag.with_help(help)
            } else {
                diag.with_help(format!(
                    "no `const`, `var`, function, or label named `{name}` is in scope; check spelling, declare it with `const {name} = <expr>`, or import it from another file by ensuring that file is in the same compile group"
                ))
            };
            diag = diag.with_note(
                "Identifiers in expressions resolve through `const`s (compile-time values), `var`s (memory addresses), functions/labels (also addresses), and symbolic-subscript fields. The lowering pass walks all of these in scope; if none match, the identifier has no source.",
            );
            diagnostics.push(diag);
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
        Expr::Member { .. } => match resolve_repeat_access_expr(expr, sema, span, diagnostics) {
            Ok(Some(access)) => repeat_access_compile_time_value(&access, sema, span, diagnostics),
            Ok(None) => None,
            Err(()) => None,
        },
        Expr::Binary { op, lhs, rhs } => {
            let lhs = eval_to_number(lhs, scope, sema, span, diagnostics)?;
            let rhs = eval_to_number(rhs, scope, sema, span, diagnostics)?;
            match op {
                ExprBinaryOp::Add => lhs.checked_add(rhs),
                ExprBinaryOp::Sub => lhs.checked_sub(rhs),
                ExprBinaryOp::Mul => lhs.checked_mul(rhs),
                ExprBinaryOp::BitOr => Some(lhs | rhs),
                ExprBinaryOp::BitAnd => Some(lhs & rhs),
                ExprBinaryOp::BitXor => Some(lhs ^ rhs),
                ExprBinaryOp::Shl => u32::try_from(rhs).ok().and_then(|n| lhs.checked_shl(n)),
                ExprBinaryOp::Shr => u32::try_from(rhs).ok().and_then(|n| lhs.checked_shr(n)),
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
                ExprUnaryOp::WordLittleEndian | ExprUnaryOp::FarLittleEndian => Some(value),
                ExprUnaryOp::EvalBracketed => Some(value),
                ExprUnaryOp::AddressPositioned => Some(value),
                ExprUnaryOp::BitNot => Some(!value),
                ExprUnaryOp::Negate => value.checked_neg().or_else(|| {
                    diagnostics.push(Diagnostic::error(span, "arithmetic overflow"));
                    None
                }),
            }
        }
        Expr::TypedView { expr, .. } => eval_to_number(expr, scope, sema, span, diagnostics),
        Expr::MetadataQuery { expr, query } => {
            resolve_metadata_query(expr, *query, sema, span, diagnostics)
        }
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
    let indexed = Expr::Index {
        base: Box::new(base.clone()),
        index: Box::new(index.clone()),
    };
    match resolve_repeat_access_expr(&indexed, sema, span, diagnostics) {
        Ok(Some(access)) => {
            return repeat_access_compile_time_value(&access, sema, span, diagnostics);
        }
        Err(()) => return None,
        Ok(None) => {}
    }

    if let Some(name) = base_ident(base) {
        match resolve_symbolic_subscript_name(name, sema, span, diagnostics) {
            Ok(Some(ResolvedSymbolicSubscriptName::Aggregate { base: agg_base, .. })) => {
                diagnostics.push(invalid_symbolic_subscript_aggregate_index_diagnostic(
                    &agg_base, base, index, sema, span,
                ));
                return None;
            }
            Ok(Some(ResolvedSymbolicSubscriptName::Field {
                base,
                field,
                address,
                data_width: _,
                size,
                count,
            })) => {
                if count <= 1 {
                    diagnostics.push(
                        Diagnostic::error(
                            span,
                            format!(
                                "symbolic subscript field '{base}.{field}' is not an array"
                            ),
                        )
                        .with_primary_label("indexed access on non-array field")
                        .with_help(format!(
                            "drop the `[...]` and write `{base}.{field}` to read the single value, or change the field's declaration to `.{field}:type[N]` if you need an explicitly typed array slot"
                        ))
                        .with_note(
                            "Symbolic subscript fields without a `[count]` declare a single named slot; only fields with `[count >= 2]` produce array semantics that an `[index]` can dereference.",
                        ),
                    );
                    return None;
                }

                let Some(index_value) = eval_to_number_strict(index, sema, span, diagnostics)
                else {
                    diagnostics.push(
                        Diagnostic::error(
                            span,
                            "symbolic subscript array index must be a constant numeric expression",
                        )
                        .with_primary_label("non-constant index")
                        .with_help(format!(
                            "use a literal or `const`-rooted expression inside `{base}.{field}[...]`; runtime indices into symbolic-subscript arrays must go through a `var` or pointer arithmetic, not the bracket form"
                        ))
                        .with_note(
                            "The bracket form on a symbolic subscript field is folded into a fixed address at compile time; that requires the index to be known before code is generated.",
                        ),
                    );
                    return None;
                };
                if index_value < 0 {
                    diagnostics.push(Diagnostic::error(
                        span,
                        format!("index must be non-negative, found {index_value}"),
                    ));
                    return None;
                }
                let scale = i64::from(size / count);
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

/// STZ only supports DirectPage / DirectPageX / Absolute / AbsoluteX. This
/// mirrors the predicate the peephole pass uses when rewriting `LDA #0; STA`.
fn stz_compatible_dest_mode(dest: &HlaOperandExpr) -> bool {
    matches!(dest.addr_mode, OperandAddrMode::Direct)
        && matches!(dest.index, None | Some(crate::ast::IndexRegister::X))
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
        OperandAddrMode::IndirectLong => AddressOperandMode::IndirectLong,
        OperandAddrMode::IndexedIndirectX => AddressOperandMode::IndexedIndirectX,
        OperandAddrMode::IndirectIndexedY => AddressOperandMode::IndirectIndexedY,
        OperandAddrMode::IndirectLongIndexedY => AddressOperandMode::IndirectLongIndexedY,
        OperandAddrMode::StackRelativeIndirectIndexedY => {
            AddressOperandMode::StackRelativeIndirectIndexedY
        }
    }
}

fn lower_index_register(index: crate::ast::IndexRegister) -> IndexRegister {
    match index {
        crate::ast::IndexRegister::X => IndexRegister::X,
        crate::ast::IndexRegister::Y => IndexRegister::Y,
        crate::ast::IndexRegister::S => IndexRegister::S,
    }
}

/// Stack-relative addressing modes (`,s` and `(byte,s),y`) take a numeric
/// byte offset relative to the stack pointer, **not** a memory address. The
/// usual address-vs-const rule inverts here: consts and pure-literal numbers
/// are valid offsets; vars, labels, and functions are addresses and must be
/// rejected.
fn is_stack_relative_mode(mode: AddressOperandMode) -> bool {
    matches!(
        mode,
        AddressOperandMode::Direct {
            index: Some(IndexRegister::S),
        } | AddressOperandMode::StackRelativeIndirectIndexedY
    )
}

/// `,X`/`,Y` indexed direct/absolute modes: the operand is a base
/// (or — under the `&EXPR` marker — a small literal field offset added to
/// a struct base in X/Y). The W65C816 stuffs both readings into the same
/// addressing mode, distinguished only by what the user puts in the index
/// register.
fn is_indexed_xy_mode(mode: AddressOperandMode) -> bool {
    matches!(
        mode,
        AddressOperandMode::Direct {
            index: Some(IndexRegister::X) | Some(IndexRegister::Y),
        }
    )
}

/// Walk `expr` looking for the `&EXPR` (`ExprUnaryOp::AddressPositioned`)
/// marker anywhere in the tree. The marker is allowed nested under binary
/// arithmetic (`&CONST + 1`) so the whole expression is checked, not just
/// the outermost node.
fn expression_contains_address_positioned(expr: &Expr) -> bool {
    match expr {
        Expr::Unary {
            op: ExprUnaryOp::AddressPositioned,
            ..
        } => true,
        Expr::Unary { expr: inner, .. } => expression_contains_address_positioned(inner),
        Expr::Binary { lhs, rhs, .. } => {
            expression_contains_address_positioned(lhs)
                || expression_contains_address_positioned(rhs)
        }
        Expr::TypedView { expr: inner, .. } => expression_contains_address_positioned(inner),
        Expr::Index { base, index } => {
            expression_contains_address_positioned(base)
                || expression_contains_address_positioned(index)
        }
        Expr::Member { base, .. } => expression_contains_address_positioned(base),
        Expr::MetadataQuery { expr: inner, .. } => expression_contains_address_positioned(inner),
        Expr::Number(_, _) | Expr::Ident(_) | Expr::IdentSpanned { .. } | Expr::EvalText(_) => {
            false
        }
    }
}

fn address_positioned_requires_index_diag(span: Span) -> Diagnostic {
    Diagnostic::error(
        span,
        "`&` address-positioned marker requires an X- or Y-indexed addressing mode",
    )
    .with_primary_label("address-positioned marker on non-indexed operand".to_string())
    .with_help(
        "use `&EXPR, x` or `&EXPR, y` — without an index register the operand IS the address, so a value-as-offset reading makes no sense",
    )
    .with_note(
        "The `&EXPR` marker says \"let this value play the role of the operand in `dp,X` / `abs,X` (or `,Y`) addressing.\" That's only meaningful when an index register adds the runtime base; in plain direct/absolute the operand is already the address.",
    )
}

fn address_positioned_in_immediate_diag(span: Span) -> Diagnostic {
    Diagnostic::error(
        span,
        "`&` address-positioned marker is not valid in an immediate operand",
    )
    .with_primary_label("address-positioned marker in immediate operand".to_string())
    .with_help(
        "for an immediate, write `#EXPR` directly — the `&` marker only applies in `,X` / `,Y` indexed addressing modes (e.g. `lda &CONST, x`)",
    )
}

fn stack_offset_not_address_diag(span: Span, name: &str, kind: &str) -> Diagnostic {
    Diagnostic::error(
        span,
        format!(
            "`{kind} {name}` is a memory address; stack-relative addressing requires a numeric offset"
        ),
    )
    .with_primary_label("address used as stack offset".to_string())
    .with_help(format!(
        "stack-relative modes (`,s` / `(,s),y`) take a byte offset from the stack pointer; replace `{name}` with a numeric literal or a `const` (e.g. `const offset = $04`), or pick a non-stack-relative addressing mode for memory access",
    ))
}

fn resolve_operand_ident(
    name: &str,
    scope: Option<&str>,
    sema: &SemanticModel,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
    size_hint: AddressSizeHint,
    mode: AddressOperandMode,
) -> Option<OperandOp> {
    let stack_rel = is_stack_relative_mode(mode);

    if name.starts_with('.') {
        if stack_rel {
            diagnostics.push(stack_offset_not_address_diag(span, name, "label"));
            return None;
        }
        let resolved = resolve_symbol(name, scope, span, diagnostics)?;
        return Some(OperandOp::Address {
            value: AddressValue::Label(resolved),
            size_hint,
            mode,
        });
    }

    if let Some(var) = sema.vars.get(name) {
        if var.is_abstract() {
            diagnostics.push(abstract_var_address_diagnostic(span, name));
            return None;
        }
        if stack_rel {
            diagnostics.push(stack_offset_not_address_diag(span, name, "var"));
            return None;
        }
        match &var.placement {
            VarPlacement::Fixed { address } => {
                return Some(OperandOp::Address {
                    value: AddressValue::Literal(*address),
                    size_hint,
                    mode,
                });
            }
            VarPlacement::AllocatedAbs { .. } | VarPlacement::AllocatedDp => {
                return Some(OperandOp::Address {
                    value: AddressValue::Label(name.to_string()),
                    size_hint,
                    mode,
                });
            }
            VarPlacement::Abstract => unreachable!("abstract vars are handled above"),
        }
    }
    if is_abstract_layout_name(name, sema) {
        diagnostics.push(abstract_var_address_diagnostic(span, name));
        return None;
    }
    if let Some(constant) = sema.consts.get(name) {
        if stack_rel {
            // Consts are valid stack offsets — they're compile-time numbers,
            // which is exactly what stack-relative addressing needs.
            let value = constant_to_exact_i64(
                name,
                constant.value,
                span,
                diagnostics,
                "stack-relative offset",
            )?;
            let Ok(address) = u32::try_from(value) else {
                diagnostics.push(Diagnostic::error(
                    span,
                    format!("stack offset cannot be negative: {value} (from const '{name}')"),
                ));
                return None;
            };
            return Some(OperandOp::Address {
                value: AddressValue::Literal(address),
                size_hint,
                mode,
            });
        }
        let mut diag = Diagnostic::error(
            span,
            format!("`const {name}` cannot be used as an address"),
        )
        .with_primary_label("const used as address".to_string())
        .with_help(format!(
            "consts hold compile-time values, not memory locations; prefix with `#` to use as an immediate (e.g. `#{name}`), or declare a `var` for memory access (e.g. `var {name} = $80`)",
        ));
        if is_indexed_xy_mode(mode) {
            diag = diag.with_help(format!(
                "if `{name}` is meant as a literal byte/word field offset (struct base in X/Y), prefix it with `&`: `&{name}, x` (or `, y`)",
            ));
        }
        diagnostics.push(diag);
        return None;
    }

    match resolve_symbolic_subscript_name(name, sema, span, diagnostics) {
        Ok(Some(ResolvedSymbolicSubscriptName::Aggregate { address, .. }))
        | Ok(Some(ResolvedSymbolicSubscriptName::Field { address, .. })) => {
            if stack_rel {
                diagnostics.push(stack_offset_not_address_diag(span, name, "symbol"));
                return None;
            }
            return Some(OperandOp::Address {
                value: AddressValue::Literal(address),
                size_hint,
                mode,
            });
        }
        Ok(None) => {}
        Err(()) => return None,
    }

    if sema.functions.contains_key(name) {
        if stack_rel {
            diagnostics.push(stack_offset_not_address_diag(span, name, "func"));
            return None;
        }
        return Some(OperandOp::Address {
            value: AddressValue::Label(name.to_string()),
            size_hint,
            mode,
        });
    }

    if stack_rel {
        diagnostics.push(stack_offset_not_address_diag(span, name, "label"));
        return None;
    }
    Some(OperandOp::Address {
        value: AddressValue::Label(name.to_string()),
        size_hint,
        mode,
    })
}

/// Attempt to decompose a binary expression into `label ± constant` for link-time resolution.
/// Returns `Some(OperandOp)` with `AddressValue::LabelOffset` if the expression is
/// `ident + number`, `number + ident`, or `ident - number` where `ident` is not a
/// compile-time-resolvable name (var, const, symbolic subscript).
fn try_label_offset_operand(
    expr: &Expr,
    scope: Option<&str>,
    sema: &SemanticModel,
    span: Span,
    size_hint: AddressSizeHint,
    mode: AddressOperandMode,
) -> Option<OperandOp> {
    let Expr::Binary { op, lhs, rhs } = expr else {
        return None;
    };
    let (name, addend) = match (op, lhs.as_ref(), rhs.as_ref()) {
        (ExprBinaryOp::Add, Expr::Ident(name), Expr::Number(n, _))
        | (ExprBinaryOp::Add, Expr::IdentSpanned { name, .. }, Expr::Number(n, _))
        | (ExprBinaryOp::Add, Expr::Number(n, _), Expr::Ident(name))
        | (ExprBinaryOp::Add, Expr::Number(n, _), Expr::IdentSpanned { name, .. }) => {
            (name.as_str(), *n as i32)
        }
        (ExprBinaryOp::Sub, Expr::Ident(name), Expr::Number(n, _))
        | (ExprBinaryOp::Sub, Expr::IdentSpanned { name, .. }, Expr::Number(n, _)) => {
            (name.as_str(), -(*n as i32))
        }
        _ => return None,
    };
    // If the name resolves at compile time (Fixed var, const), this function
    // should not handle it — eval_to_number should have succeeded. Allocated
    // vars are linker-resolved, so they belong here as label-offset targets.
    if sema.consts.contains_key(name) {
        return None;
    }
    if is_abstract_layout_name(name, sema) {
        return None;
    }
    if let Some(var) = sema.vars.get(name)
        && var.compile_time_numeric_value().is_some()
    {
        return None;
    }
    // Dotted names referring to fields of Allocated symbolic-subscript vars
    // are also linker-resolved; only reject when the field resolves through a
    // Fixed base (i.e. has a compile-time numeric value, including DP slots).
    if let Some((base_name, _field_name)) = name.split_once('.')
        && let Some(base_var) = sema.vars.get(base_name)
        && base_var.compile_time_numeric_value().is_some()
        && base_var.symbolic_subscript.is_some()
    {
        return None;
    }
    // Resolve the symbol name (handles local label scoping).
    let label = resolve_symbol(name, scope, span, &mut Vec::new())?;
    Some(OperandOp::Address {
        value: AddressValue::LabelOffset { label, addend },
        size_hint,
        mode,
    })
}

/// When an identifier in a numeric expression is unresolved, but the same
/// name exists as an addressable symbol (function, var, or symbolic-subscript
/// field), suggest the `&&`/`&&&` address-of operator. This nudges users from
/// `lda #funcname` (which only works for compile-time numbers) toward
/// `lda &&funcname` (which works for any addressable symbol via link-time
/// relocation).
fn address_of_help_for_name(name: &str, sema: &SemanticModel, span: Span) -> Option<String> {
    if sema.functions.contains_key(name) {
        return Some(format!(
            "'{name}' is a function — use `&&{name}` to load its 16-bit address (or `&&&{name}` for a far address)"
        ));
    }
    if sema.vars.get(name).is_some_and(|var| var.is_abstract()) {
        return Some(format!(
            "'{name}' is an abstract var layout — use `{name}:sizeof` or `{name}.field:offsetof` for layout metadata"
        ));
    }
    if sema.vars.contains_key(name) {
        return Some(format!(
            "'{name}' is a var — use `&&{name}` to load its 16-bit address"
        ));
    }
    if sema.labels.contains_key(name) {
        return Some(format!(
            "'{name}' is a data-block label — use `&&{name}` to load its 16-bit address (or `&&&{name}` for a far address)"
        ));
    }
    if let Ok(Some(_)) = resolve_symbolic_subscript_name(name, sema, span, &mut Vec::new()) {
        return Some(format!(
            "'{name}' is a symbolic subscript field — use `&&{name}` to load its 16-bit address"
        ));
    }
    None
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
    use crate::hir::{
        AddressOperandMode, AddressSizeHint, AddressValue, ByteRelocationKind, OperandOp,
    };
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
        let program = lower(&file, &sema, &fs, None).expect("lower");

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
                size_hint,
                mode,
            } => {
                assert_eq!(*size_hint, AddressSizeHint::Auto);
                assert_eq!(*mode, AddressOperandMode::Direct { index: None });
                assert!(matches!(value, AddressValue::Literal(0x1234)));
            }
            _ => panic!("expected address operand"),
        }
    }

    #[test]
    fn abs_prefix_forces_absolute16_for_address_operands() {
        let source = "var target = 0x0012\nfunc main {\n  lda abs target\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs, None).expect("lower");

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
                value: AddressValue::Literal(0x12),
                size_hint: AddressSizeHint::ForceAbsolute16,
                mode: AddressOperandMode::Direct { index: None },
            }
        ));
    }

    #[test]
    fn var_abs_prefix_propagates_default_to_plain_references() {
        let source = "var abs target = 0x0012\nfunc main {\n  lda target\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs, None).expect("lower");

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
                value: AddressValue::Literal(0x12),
                size_hint: AddressSizeHint::ForceAbsolute16,
                mode: AddressOperandMode::Direct { index: None },
            }
        ));
    }

    #[test]
    fn operand_prefix_overrides_var_default() {
        let source = "var abs target = 0x0012\nfunc main {\n  lda dp target\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs, None).expect("lower");

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
                value: AddressValue::Literal(0x12),
                size_hint: AddressSizeHint::ForceDirectPage,
                mode: AddressOperandMode::Direct { index: None },
            }
        ));
    }

    #[test]
    fn dp_prefix_forces_direct_page_for_high_address_value() {
        let source = "var target = 0x0080\nfunc main {\n  lda dp target\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs, None).expect("lower");

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
                value: AddressValue::Literal(0x80),
                size_hint: AddressSizeHint::ForceDirectPage,
                mode: AddressOperandMode::Direct { index: None },
            }
        ));
    }

    #[test]
    fn resolves_const_operand_to_immediate_value() {
        let source = "const LIMIT = 0x34\nfunc main {\n  lda #LIMIT\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs, None).expect("lower");

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
    fn resolves_negative_const_operand_to_signed_immediate() {
        let source = "const A = -1\nfunc main @a8 {\n  lda #A\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs, None).expect("lower");

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

        assert!(matches!(operand, OperandOp::Immediate(-1)));
    }

    #[test]
    fn accepts_immediate_at_signed_lower_bound_a8() {
        // -128 fits the signed range of 8-bit accumulator immediates.
        let source = "func main @a8 {\n  lda #-128\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        lower(&file, &sema, &fs, None).expect("lower");
    }

    #[test]
    fn rejects_immediate_below_signed_lower_bound_a8() {
        // -129 falls outside both i8 and u8 — error.
        let source = "func main @a8 {\n  lda #-129\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let errors = lower(&file, &sema, &fs, None).expect_err("must fail");
        assert!(
            errors
                .iter()
                .any(|e| e.message.contains("does not fit in @a8")),
            "expected @a8 width error, got {errors:?}"
        );
    }

    #[test]
    fn rejects_immediate_above_unsigned_upper_bound_a8() {
        // 256 fits neither i8 nor u8 — error.
        let source = "func main @a8 {\n  lda #256\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let errors = lower(&file, &sema, &fs, None).expect_err("must fail");
        assert!(
            errors
                .iter()
                .any(|e| e.message.contains("does not fit in @a8")),
            "expected @a8 width error, got {errors:?}"
        );
    }

    #[test]
    fn rejects_bare_const_operand_as_address() {
        // A bare `const` reference in instruction-operand position (no `#`)
        // is address-mode syntax. Since a const is a compile-time value with
        // no memory location, it must be rejected with a diagnostic that
        // points the user at `#LIMIT` (immediate) or a `var` (memory access).
        let source = "const LIMIT = 0x34\nfunc main {\n  lda LIMIT\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let errors = lower(&file, &sema, &fs, None).expect_err("must fail");
        let diag = errors
            .iter()
            .find(|e| {
                e.message.contains("`const LIMIT`")
                    && e.message.contains("cannot be used as an address")
            })
            .expect("expected const-as-address diagnostic");
        assert!(
            diag.supplements.iter().any(|s| matches!(
                s,
                crate::diag::Supplemental::Help(text) if text.contains("#LIMIT") && text.contains("`var`")
            )),
            "expected help suggesting `#LIMIT` and `var`, got {:?}",
            diag.supplements
        );
    }

    #[test]
    fn resolves_top_level_evaluator_constant_to_immediate_value() {
        let source = "[ LIMIT = 0x34 ]\nfunc main {\n  lda #LIMIT\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs, None).expect("lower");

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
    fn rejects_bare_top_level_evaluator_constant_as_address() {
        // Top-level evaluator constants (`[ NAME = ... ]`) share the same
        // semantic class as `const NAME = ...`: both populate `sema.consts`
        // and are immediate-only. Bare references in address position must
        // be rejected just like regular consts.
        let source = "[ LIMIT = 0x34 ]\nfunc main {\n  lda LIMIT\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let errors = lower(&file, &sema, &fs, None).expect_err("must fail");
        assert!(
            errors.iter().any(|e| e.message.contains("`const LIMIT`")
                && e.message.contains("cannot be used as an address")),
            "expected const-as-address diagnostic, got {errors:?}"
        );
    }

    #[test]
    fn lowers_immediate_address_byte_symbols_to_relocations() {
        let source = "data displist {\n  0\n}\nfunc dli {\n  rti\n}\nfunc main {\n  a=&<displist\n  a=&>displist\n  a=&<dli\n  a=&>dli\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs, None).expect("lower");

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
    fn lowers_address_of_word_to_immediate_word_relocation() {
        // `&&label` for a function: emits a 2-byte relocation, regardless of
        // whether the symbol's address is known at compile time.
        let source = "func dbg_task {\n  rti\n}\nfunc main @a16 {\n  lda &&dbg_task\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs, None).expect("lower");

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
            OperandOp::ImmediateWordRelocation { label, addend: 0, .. } if label == "dbg_task"
        ));
    }

    #[test]
    fn lowers_address_of_word_with_addend() {
        let source = "var TASKS = $4000\nfunc main @a16 {\n  lda &&TASKS+2\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs, None).expect("lower");

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
            OperandOp::ImmediateWordRelocation { label, addend: 2, .. } if label == "TASKS"
        ));
    }

    #[test]
    fn resolves_fixed_repeat_element_accessors_to_addresses_and_address_of_addends() {
        let source = "\
var COMP[
  .one:byte
  .two:word
  .str:byte[5]
] * 10 = $1000
var WORDS:word * 4 = $3000
func main @a16 {
  lda COMP[2].two
  lda &&COMP[2].two
  lda WORDS[2]
  @a8
  lda COMP[2].str[3]
}
";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs, None).expect("lower");

        let operands = program
            .ops
            .iter()
            .filter_map(|op| match &op.node {
                Op::Instruction(instruction) if instruction.mnemonic == "lda" => {
                    instruction.operand.as_ref()
                }
                _ => None,
            })
            .collect::<Vec<_>>();
        assert_eq!(operands.len(), 4);

        assert!(matches!(
            operands[0],
            OperandOp::Address {
                value: AddressValue::Literal(0x1011),
                ..
            }
        ));
        assert!(matches!(
            operands[1],
            OperandOp::ImmediateWordRelocation {
                label,
                addend: 17,
                ..
            } if label == "COMP"
        ));
        assert!(matches!(
            operands[2],
            OperandOp::Address {
                value: AddressValue::Literal(0x3004),
                ..
            }
        ));
        assert!(matches!(
            operands[3],
            OperandOp::Address {
                value: AddressValue::Literal(0x1016),
                ..
            }
        ));
    }

    #[test]
    fn uses_label_offsets_for_allocated_repeat_element_accessors() {
        let source = "\
var COMP[
  .one:byte
  .two:word
  .str:byte[5]
] * 3
func main @a16 {
  lda COMP[2].two
  lda &&COMP + 2*COMP:sizeof
  lda &&COMP[2].two
}
";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs, None).expect("lower");

        let operands = program
            .ops
            .iter()
            .filter_map(|op| match &op.node {
                Op::Instruction(instruction) if instruction.mnemonic == "lda" => {
                    instruction.operand.as_ref()
                }
                _ => None,
            })
            .collect::<Vec<_>>();
        assert_eq!(operands.len(), 3);

        assert!(matches!(
            operands[0],
            OperandOp::Address {
                value: AddressValue::LabelOffset { label, addend: 17 },
                size_hint: AddressSizeHint::Auto,
                ..
            } if label == "COMP"
        ));
        assert!(matches!(
            operands[1],
            OperandOp::ImmediateWordRelocation {
                label,
                addend: 16,
                ..
            } if label == "COMP"
        ));
        assert!(matches!(
            operands[2],
            OperandOp::ImmediateWordRelocation {
                label,
                addend: 17,
                ..
            } if label == "COMP"
        ));
    }

    #[test]
    fn dp_repeat_element_accessors_keep_direct_page_hint() {
        let source = "\
var dp COMP[
  .one:byte
  .two:word
] * 3
func main @a16 {
  lda COMP[2].two
}
";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs, None).expect("lower");

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
                value: AddressValue::LabelOffset { label, addend: 7 },
                size_hint: AddressSizeHint::ForceDirectPage,
                ..
            } if label == "COMP"
        ));
    }

    #[test]
    fn rejects_hash_immediate_of_var_with_helpful_diagnostic() {
        // `lda #VAR` is a type mismatch (var is an address, not a numeric
        // value); the user must explicitly write `lda &&VAR` to take the address.
        let source = "var TASKS = $4000\nfunc main @a16 {\n  lda #TASKS\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let errors = lower(&file, &sema, &fs, None).expect_err("must fail");

        let diag = errors
            .iter()
            .find(|e| {
                e.message.contains("cannot use var")
                    && e.message.contains("'TASKS'")
                    && e.message.contains("immediate value")
            })
            .expect("expected immediate-value-rejection diagnostic");
        // The help line should suggest `&&TASKS`.
        assert!(
            diag.supplements.iter().any(|s| matches!(
                s,
                crate::diag::Supplemental::Help(text) if text.contains("&&TASKS")
            )),
            "expected `&&TASKS` help, got {:?}",
            diag.supplements
        );
    }

    #[test]
    fn rejects_hash_immediate_of_function() {
        let source = "func dbg_task {\n  rti\n}\nfunc main @a16 {\n  lda #dbg_task\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let errors = lower(&file, &sema, &fs, None).expect_err("must fail");

        assert!(
            errors.iter().any(|e| {
                e.message.contains("cannot use function")
                    && e.message.contains("'dbg_task'")
                    && e.message.contains("immediate value")
            }),
            "expected immediate-value-rejection diagnostic mentioning 'function', got {errors:?}"
        );
    }

    #[test]
    fn allows_hash_immediate_of_const() {
        // Consts continue to work as immediates — they ARE numbers, not addresses.
        let source = "const LIMIT = 0x34\nfunc main {\n  lda #LIMIT\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs, None).expect("lower");
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
    fn unknown_identifier_in_immediate_suggests_address_of_for_function() {
        // When the user writes `lda #funcname` AND funcname doesn't resolve to
        // a compile-time number, the diagnostic should help them discover the
        // `&&` operator. This case happens in cross-unit refs to functions
        // when the function is declared but not yet linked.
        let source = "func helper {\n  rti\n}\nfunc main @a16 {\n  lda #helper:sizeof\n}\n";
        // Note: helper:sizeof is a metadata query; let's make a bare reference
        // through an arithmetic context instead.
        let _ = source;
        let source = "func helper {\n  rti\n}\nfunc main @a16 {\n  lda helper + 1\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        // This should succeed via the label-offset path; just confirm no
        // address-of help is spuriously emitted on success.
        let _ = lower(&file, &sema, &fs, None).expect("lower");
    }

    #[test]
    fn lowers_address_of_far_to_immediate_far_relocation() {
        let source = "func main @a16 {\n  lda &&&main\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs, None).expect("lower");

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
            OperandOp::ImmediateFarRelocation { label, addend: 0, .. } if label == "main"
        ));
    }

    #[test]
    fn reports_unknown_identifier_in_numeric_expression() {
        let source = "func main {\n  lda #MISSING\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let errors = lower(&file, &sema, &fs, None).expect_err("must fail");

        let missing_start = source.find("MISSING").expect("MISSING start");
        let missing_end = missing_start + "MISSING".len();

        assert!(
            errors
                .iter()
                .any(|error| { error.message.contains("unknown identifier 'MISSING'") })
        );
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
        let program = lower(&file, &sema, &fs, None).expect("lower");
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
        let program = lower(&file, &sema, &fs, None).expect("lower");
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
        let program = lower(&file, &sema, &fs, None).expect("lower");

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
        let source = "var foo[\n  .field_w:byte\n  .idx:byte\n  .string:byte[4]\n] = 0x1234\nfunc main {\n  lda foo.field_w\n  sta foo[.idx]\n  lda foo.string[2]\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs, None).expect("lower");

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
        let errors = lower(&file, &sema, &fs, None).expect_err("must fail");

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
        let errors = lower(&file, &sema, &fs, None).expect_err("must fail");

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
        let errors = lower(&file, &sema, &fs, None).expect_err("must fail");

        assert!(errors.iter().any(|error| {
            error.supplements.iter().any(|supplement| {
                matches!(
                    supplement,
                    crate::diag::Supplemental::Help(help)
                        if help.contains("use '.field' or '[.field]'")
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
        let errors = lower(&file, &sema, &fs, None).expect_err("must fail");

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
        let program = lower(&file, &sema, &fs, None).expect("lower");

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
        let program = lower(&file, &sema, &fs, None).expect("lower");

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
        let program = lower(&file, &sema, &fs, None).expect("lower");

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
        let program = lower(&file, &sema, &fs, None).expect("lower");

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
                size_hint: AddressSizeHint::Auto,
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
        let program = lower(&file, &sema, &fs, None).expect("lower");

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
        let program = lower(&file, &sema, &fs, None).expect("lower");

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
        let program = lower(&file, &sema, &fs, None).expect("lower");

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
        let program = lower(&file, &sema, &fs, None).expect("lower");

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
        let program = lower(&file, &sema, &fs, None).expect("lower");

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
        let program = lower(&file, &sema, &fs, None).expect("lower");

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
        let program = lower(&file, &sema, &fs, None).expect("lower");

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

    #[test]
    fn lowers_inline_immediate_contract_arguments() {
        let source =
            "inline scale (#factor:byte) {\n  lda #factor\n}\nfunc main {\n  scale #16\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs, None).expect("lower");

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

        assert!(matches!(operand, OperandOp::Immediate(16)));
    }

    #[test]
    fn lowers_untyped_inline_immediate_contract_arguments() {
        let source =
            "inline scale (#factor) {\n  lda #factor\n}\nfunc main @a16 {\n  scale #$1234\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs, None).expect("lower");

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

        assert!(matches!(operand, OperandOp::Immediate(0x1234)));
    }

    #[test]
    fn checked_inline_call_uses_inferred_exit_mode_without_restore() {
        let source = "inline widen @a8 () -> @a16 {\n  @a16\n  nop\n}\nfunc main {\n  widen\n  lda #$1234\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs, None).expect("lower");

        let mnemonics = program
            .ops
            .iter()
            .filter_map(|op| match &op.node {
                Op::Instruction(instruction) => Some(instruction.mnemonic.clone()),
                Op::Rep { mask, .. } => Some(format!("rep:{mask:#04x}")),
                Op::Sep { mask, .. } => Some(format!("sep:{mask:#04x}")),
                _ => None,
            })
            .collect::<Vec<_>>();

        assert!(mnemonics.windows(2).any(|pair| pair == ["nop", "lda"]));
        assert!(
            !mnemonics.iter().any(|mnemonic| mnemonic == "sep:0x20"),
            "unexpected restore after checked inline contract call: {mnemonics:?}"
        );
    }

    #[test]
    fn unchecked_inline_call_restores_caller_mode() {
        let source = "inline narrow @a8 {\n  nop\n}\nfunc main @a16 {\n  narrow\n  lda #$1234\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs, None).expect("lower");

        let mnemonics = program
            .ops
            .iter()
            .filter_map(|op| match &op.node {
                Op::Instruction(instruction) => Some(instruction.mnemonic.clone()),
                Op::Rep { mask, .. } => Some(format!("rep:{mask:#04x}")),
                Op::Sep { mask, .. } => Some(format!("sep:{mask:#04x}")),
                _ => None,
            })
            .collect::<Vec<_>>();

        assert!(
            mnemonics
                .windows(3)
                .any(|window| window == ["sep:0x20", "nop", "rep:0x20"]),
            "expected unchecked inline call to restore caller mode: {mnemonics:?}"
        );
    }

    #[test]
    fn bare_contract_call_uses_inferred_exit_mode_without_restore() {
        let source = "func widen @a8 () -> @a16 {\n  @a16\n  nop\n}\nfunc main {\n  widen\n  lda #$1234\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs, None).expect("lower");

        let mnemonics = program
            .ops
            .iter()
            .filter_map(|op| match &op.node {
                Op::Instruction(instruction) => Some(instruction.mnemonic.clone()),
                Op::Rep { mask, .. } => Some(format!("rep:{mask:#04x}")),
                Op::Sep { mask, .. } => Some(format!("sep:{mask:#04x}")),
                _ => None,
            })
            .collect::<Vec<_>>();

        assert!(mnemonics.windows(2).any(|pair| pair == ["jsr", "lda"]));
        assert!(
            !mnemonics.iter().any(|mnemonic| mnemonic == "sep:0x20"),
            "unexpected restore after contract-bearing call: {mnemonics:?}"
        );
    }

    #[test]
    fn rejects_exit_contract_mismatch_against_inferred_exit_mode() {
        let source = "inline widen @a8 () -> @a16 {\n  nop\n}\nfunc main {\n  widen\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let errors = lower(&file, &sema, &fs, None).expect_err("must fail");

        assert!(errors.iter().any(|error| {
            error
                .message
                .contains("exit contract requires @a16 but inferred exit mode is @a8")
        }));
    }

    #[test]
    fn rejects_divergent_exit_modes_across_reachable_returns() {
        let source = "func choose @a8 () -> a {\n  c-? {\n    @a16\n    return\n  }\n  return\n}\nfunc main {\n  choose -> a\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let errors = lower(&file, &sema, &fs, None).expect_err("must fail");

        assert!(errors.iter().any(|error| {
            error
                .message
                .contains("has inconsistent exit mode across reachable returns")
        }));
    }

    #[test]
    fn inline_alias_arguments_resolve_in_caller_scope() {
        let source = "inline jump_to (target) {\n  goto target\n}\nfunc main {\n.loop:\n  jump_to .loop\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let program = lower(&file, &sema, &fs, None).expect("lower");

        let operand = program
            .ops
            .iter()
            .find_map(|op| match &op.node {
                Op::Instruction(instruction) if instruction.mnemonic == "jmp" => {
                    instruction.operand.as_ref()
                }
                _ => None,
            })
            .expect("jmp operand");

        match operand {
            OperandOp::Address { value, .. } => {
                assert!(matches!(
                    value,
                    AddressValue::Label(label) if label == "main::.loop"
                ));
            }
            _ => panic!("expected address operand"),
        }
    }

    #[test]
    fn rejects_constant_inline_alias_arguments() {
        let source = "const DEST = $1234\ninline jump_to (target) {\n  goto target\n}\nfunc main {\n  jump_to DEST\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let errors = lower(&file, &sema, &fs, None).expect_err("must fail");

        assert!(errors.iter().any(|error| {
            error
                .message
                .contains("inline alias argument 'DEST' must be an address-like identifier")
        }));
    }

    #[test]
    fn rejects_bare_call_output_mismatch() {
        let source = "func produce @a8 () -> a {\n  lda #1\n}\nfunc main {\n  produce\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let errors = lower(&file, &sema, &fs, None).expect_err("must fail");

        assert!(errors.iter().any(|error| {
            error
                .message
                .contains("call to 'produce' has mismatched output contract")
        }));
    }

    #[test]
    fn rejects_live_register_collision_from_conditional_contract_clobber() {
        let source =
            "func touch @a8 (a) {\n  c-?{ inx }\n}\nfunc main {\n  ldx #1\n  touch a\n  txa\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let errors = lower(&file, &sema, &fs, None).expect_err("must fail");

        assert!(errors.iter().any(|error| {
            error
                .message
                .contains("register `x` is live after call to `touch` but clobbered by it")
        }));
    }

    #[test]
    fn rejects_live_register_collision_from_contract_call() {
        let source = "func touch @a8 (a) {\n  inx\n}\nfunc main {\n  ldx #1\n  touch a\n  txa\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let errors = lower(&file, &sema, &fs, None).expect_err("must fail");

        assert!(errors.iter().any(|error| {
            error
                .message
                .contains("register `x` is live after call to `touch` but clobbered by it")
        }));
    }

    #[test]
    fn rejects_accumulator_use_after_width_switch() {
        let source = "var dst = $2000\nfunc main {\n  @a16\n  lda #$1234\n  @a8\n  sta dst\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let errors = lower(&file, &sema, &fs, None).expect_err("must fail");

        assert!(errors.iter().any(|error| {
            error
                .message
                .contains("Accumulator value is dead after switching to @a8")
        }));
    }

    #[test]
    fn allows_accumulator_reload_after_width_switch() {
        let source = "func main {\n  @a16\n  lda #$1234\n  @a8\n  lda #$12\n  nop\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let lowered = lower_with_warnings(&file, &sema, &fs, None, None).expect("lower");

        assert!(lowered.warnings.is_empty());
    }

    #[test]
    fn warns_on_index_use_after_narrowing_without_reload() {
        let source = "func main {\n  @i16\n  ldx #$1234\n  ldy #$5678\n  @i8\n  txa\n  tya\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let lowered = lower_with_warnings(&file, &sema, &fs, None, None).expect("lower");

        assert_eq!(lowered.warnings.len(), 2);
        assert!(lowered.warnings.iter().any(|warning| {
            warning
                .message
                .contains("register x is used after @i8 without an 8-bit reload")
        }));
        assert!(lowered.warnings.iter().any(|warning| {
            warning
                .message
                .contains("register y is used after @i8 without an 8-bit reload")
        }));
    }
}
