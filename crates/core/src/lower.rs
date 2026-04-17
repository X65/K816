use indexmap::IndexMap;
use k816_assets::AssetFS;
use k816_eval::{EvalContext, EvalError as EvaluatorError, Number};
use k816_isa65816::{RegEffects, RegSet, mnemonic_effects};
use rustc_hash::FxHashMap;

use crate::ast::{
    AddressHint, CallArg, CallStmt, CodeBlock, ContractParam, DataWidth, Expr, ExprBinaryOp,
    ExprUnaryOp, File, HlaAluOp, HlaCompareOp, HlaCondition, HlaCpuRegister, HlaFlag, HlaIncDecOp,
    HlaIncDecTarget, HlaOperandExpr, HlaRegister, HlaRhs, HlaShiftOp, HlaShiftTarget,
    HlaStackTarget, HlaStmt, ImmediateParamType, Instruction, Item, MetadataQuery, ModeContract,
    NamedDataBlock, NamedDataEntry, NumFmt, Operand, OperandAddrMode, RegName, RegWidth, Stmt,
};
use crate::data_blocks::lower_data_block;
use crate::diag::{Diagnostic, Severity};
use crate::hir::{
    AddressOperandMode, AddressSizeHint, AddressValue, ByteRelocation, ByteRelocationKind,
    IndexRegister, InstructionOp, Op, OperandOp, Program,
};
use crate::sema::SemanticModel;
use crate::span::{Span, Spanned};

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

fn instruction_effects(instruction: &Instruction) -> RegEffects {
    let base = mnemonic_effects(&instruction.mnemonic, instruction.operand.is_none());
    RegEffects {
        reads: base.reads | operand_index_reads(instruction.operand.as_ref()),
        modifies: base.modifies,
    }
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
        | HlaStmt::RepeatNop(_) => RegEffects::default(),
    }
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
            .with_help(format!(
                "call it as `{}`",
                meta.signature_call_form(&call.target)
            )),
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
                    .with_help(format!(
                        "call it as `{}`",
                        meta.signature_call_form(&call.target)
                    )),
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
                    .with_help(format!(
                        "call it as `{}`",
                        meta.signature_call_form(&call.target)
                    )),
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
                    .with_help(format!(
                        "call it as `{}`",
                        meta.signature_call_form(&call.target)
                    )),
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
            .with_help(format!(
                "call it as `{}`",
                meta.signature_call_form(&call.target)
            )),
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
        Expr::AddressHint { expr, hint } => Expr::AddressHint {
            expr: Box::new(substitute_inline_expr(expr, bindings)),
            hint: *hint,
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
                    force_far,
                    index,
                    addr_mode,
                } => Operand::Value {
                    expr: substitute_inline_expr(expr, bindings),
                    force_far: *force_far,
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
            _ => hla.clone(),
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
/// know about.
fn retarget_spans(body: &mut [Spanned<Stmt>], target: Span) {
    let zero = Span {
        source_id: target.source_id,
        start: target.start,
        end: target.start,
    };
    for stmt in body {
        stmt.span = zero;
        retarget_stmt_spans(&mut stmt.node, zero);
    }
}

fn retarget_stmt_spans(stmt: &mut Stmt, target: Span) {
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
        _ => {}
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
    if let Some(incoming_mode) = ctx.label_entry_modes.get(label).copied() {
        if ctx.reachable && ctx.mode != incoming_mode {
            diagnostics.push(mode_mismatch_diagnostic(
                span,
                label,
                ctx.mode,
                incoming_mode,
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
        ctx.mode = incoming_mode;
        ctx.hazards = merged_hazards;
        ctx.reachable = true;
    } else if ctx.reachable {
        ctx.label_entry_modes.insert(label.to_string(), ctx.mode);
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
        let _ = compute_function_summary(name, &blocks, sema, &mut cache, &mut visiting);
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
                        diagnostics.push(
                            Diagnostic::error(
                                stmt.span,
                                format!(
                                    "register {} is live after call to '{}' but clobbered by it",
                                    reg_name_text(reg),
                                    call.target
                                ),
                            )
                            .with_help("save the register, consume the output immediately, or adjust the function contract"),
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
    match width {
        ExitWidth::Preserve => entry,
        ExitWidth::Fixed(width) => Some(width),
        ExitWidth::Unknown => None,
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

fn build_exit_mode_summaries(
    file: &File,
    sema: &SemanticModel,
    fs: &dyn AssetFS,
    diagnostics: &mut Vec<Diagnostic>,
) -> FxHashMap<String, ExitModeSummary> {
    let blocks: FxHashMap<String, &CodeBlock> = file
        .items
        .iter()
        .filter_map(|item| match &item.node {
            Item::CodeBlock(block) => Some((block.name.clone(), block)),
            _ => None,
        })
        .collect();
    let inline_bodies: FxHashMap<String, &CodeBlock> = file
        .items
        .iter()
        .filter_map(|item| match &item.node {
            Item::CodeBlock(block) if block.is_inline => Some((block.name.clone(), block)),
            _ => None,
        })
        .collect();

    let mut cache = FxHashMap::default();
    let mut visiting = Vec::new();

    for name in blocks.keys() {
        let _ = compute_exit_mode_summary(
            name,
            &blocks,
            &inline_bodies,
            sema,
            fs,
            &mut cache,
            &mut visiting,
            diagnostics,
        );
    }

    cache
}

#[allow(clippy::too_many_arguments)]
fn compute_exit_mode_summary<'a>(
    name: &str,
    blocks: &FxHashMap<String, &'a CodeBlock>,
    inline_bodies: &FxHashMap<String, &'a CodeBlock>,
    sema: &SemanticModel,
    fs: &dyn AssetFS,
    cache: &mut FxHashMap<String, ExitModeSummary>,
    visiting: &mut Vec<String>,
    diagnostics: &mut Vec<Diagnostic>,
) -> ExitModeSummary {
    if let Some(summary) = cache.get(name).copied() {
        return summary;
    }

    let Some(block) = blocks.get(name).copied() else {
        let summary = ExitModeSummary {
            a_width: ExitWidth::Unknown,
            i_width: ExitWidth::Unknown,
            is_naked: false,
        };
        cache.insert(name.to_string(), summary);
        return summary;
    };

    if visiting.iter().any(|entry| entry == name) {
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
        cache.insert(name.to_string(), summary);
        return summary;
    }

    let mut deps = Vec::new();
    collect_checked_call_dependencies(&block.body, sema, &mut deps);

    visiting.push(name.to_string());
    for dep in deps {
        if dep != name {
            let _ = compute_exit_mode_summary(
                &dep,
                blocks,
                inline_bodies,
                sema,
                fs,
                cache,
                visiting,
                diagnostics,
            );
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
    visiting.pop();

    let effective_contract = sema
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
        let mut dummy_segment = "default".to_string();
        for stmt in &block.body {
            lower_stmt(
                &stmt.node,
                stmt.span,
                Some(name),
                sema,
                fs,
                inline_bodies,
                cache,
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
                format!(
                    "function '{name}' has inconsistent exit mode across reachable returns"
                ),
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

    cache.insert(name.to_string(), summary);
    summary
}

pub(crate) fn lower_with_warnings(
    file: &File,
    sema: &SemanticModel,
    fs: &dyn AssetFS,
    external_inline_bodies: Option<&IndexMap<String, CodeBlock>>,
) -> Result<LowerOutput, Vec<Diagnostic>> {
    let mut diagnostics = Vec::new();
    let mut ops = Vec::new();
    let mut top_level_ctx = LowerContext {
        ..Default::default()
    };
    let mut current_segment = "default".to_string();

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
            Item::DataBlock(block) => match lower_data_block(block, fs) {
                Ok(mut lowered) => ops.append(&mut lowered),
                Err(mut errs) => diagnostics.append(&mut errs),
            },
            Item::NamedDataBlock(block) => {
                lower_named_data_block(
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
            Item::Const(c) => {
                emit_const_absolute_symbol(c, item.span, sema, &mut ops);
            }
            Item::ConstGroup(consts) => {
                for c in consts {
                    emit_const_absolute_symbol(c, item.span, sema, &mut ops);
                }
            }
            Item::Var(var) => {
                emit_var_absolute_symbols(var, item.span, sema, &mut diagnostics, &mut ops);
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
    lower_with_warnings(file, sema, fs, external_inline_bodies).map(|output| output.program)
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
fn lower_named_data_block(
    block: &NamedDataBlock,
    sema: &SemanticModel,
    fs: &dyn AssetFS,
    inline_bodies: &FxHashMap<String, &CodeBlock>,
    exit_summaries: &FxHashMap<String, ExitModeSummary>,
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
            ops.push(Spanned::new(Op::Label(name.clone()), block.name_span));
            label_emitted = true;
        }

        lower_named_data_entry(
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

#[allow(clippy::too_many_arguments)]
fn lower_named_data_entry(
    entry: &NamedDataEntry,
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
        NamedDataEntry::Code(stmts) => {
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
        NamedDataEntry::Evaluator(_text) => {
            // Evaluator blocks are handled at the sema level; no lowering needed.
        }
        NamedDataEntry::Charset(value) => {
            *charset = Some(value.clone());
        }
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
                let declared_mode = ctx.label_declared_modes.get(&resolved).copied();
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
                    ctx.label_entry_modes.insert(resolved.clone(), ctx.mode);
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
            let _ = ctx.mode_frames.pop();
            if ctx.reachable {
                // Restore mode when control falls out of the block naturally.
                lower_mode_restore(ctx.mode, saved_mode, span, ops);
                ctx.hazards = apply_mode_restore_hazards(ctx.hazards, ctx.mode, saved_mode);
                ctx.mode = saved_mode;
            }
        }
        Stmt::SwapAB => {
            let instruction = Instruction {
                mnemonic: "xba".to_string(),
                operand: None,
            };
            lower_instruction_stmt(&instruction, scope, sema, span, ctx, diagnostics, ops);
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

    // Lower the inline function's body statements in-place.
    let inline_scope = Some(block_name);
    for stmt in body {
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
    let effects = instruction_effects(instruction);
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
        Operand::Immediate { .. } | Operand::BlockMove { .. } => return None,
    };

    let name = expr_ident_name(expr)?;
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
        Operand::Value {
            expr,
            force_far,
            index,
            addr_mode,
        } => value_operand_uses_immediate(*force_far, *index, *addr_mode, expr, sema),
        Operand::BlockMove { .. } => false,
    };

    let imm_expr = match operand {
        Operand::Immediate { expr, .. } | Operand::Auto { expr } if is_immediate => Some(expr),
        Operand::Value { expr, .. } if is_immediate => Some(expr),
        _ => None,
    };

    if let Some(expr) = imm_expr {
        let reg_mode = match mnemonic.as_str() {
            "lda" | "cmp" | "and" | "ora" | "eor" | "adc" | "sbc" | "bit" => mode.a_width,
            "ldx" | "ldy" | "cpx" | "cpy" => mode.i_width,
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

    let addr_expr = match operand {
        Operand::Value { expr, .. } if !is_immediate => Some(expr),
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
            format!("Cannot directly load/store :far value '{name}'.")
        } else {
            "Cannot directly load/store :far value.".to_string()
        };
        let help = if let Some(name) = var_name {
            format!(
                "use explicit :byte or :word view, e.g. {name}:word for low 16 bits, ({name}+2):byte for bank byte"
            )
        } else {
            "use explicit :byte or :word view, e.g. name:word for low 16 bits, (name+2):byte for bank byte".to_string()
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
        Expr::AddressHint { expr, .. } => expr_data_width(expr, sema),
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
        Expr::AddressHint { expr, .. } => base_ident(expr),
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

fn expr_address_hint(expr: &Expr) -> Option<AddressHint> {
    match expr {
        Expr::AddressHint { expr: inner, hint } => expr_address_hint(inner).or(Some(*hint)),
        Expr::TypedView { expr, .. } => expr_address_hint(expr),
        _ => None,
    }
}

fn effective_address_hint(expr: &Expr, sema: &SemanticModel) -> Option<AddressHint> {
    expr_address_hint(expr).or_else(|| {
        base_ident(expr)
            .and_then(|name| sema.vars.get(name))
            .and_then(|var| var.addr_hint)
    })
}

fn address_size_hint_for_expr(
    expr: &Expr,
    sema: &SemanticModel,
    force_far: bool,
) -> AddressSizeHint {
    if force_far {
        AddressSizeHint::ForceAbsoluteLong
    } else if effective_address_hint(expr, sema).is_some() {
        AddressSizeHint::ForceAbsolute16
    } else {
        AddressSizeHint::Auto
    }
}

fn symbolic_subscript_field_width(name: &str, sema: &SemanticModel) -> Option<DataWidth> {
    let (base_name, field_name) = name.split_once('.')?;
    let base = sema.vars.get(base_name)?;
    let symbolic_subscript = base.symbolic_subscript.as_ref()?;
    symbolic_subscript
        .fields
        .get(field_name)
        .and_then(|field| field.data_width)
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
            force_far: false,
            index: parsed.index,
            addr_mode: parsed.addr_mode,
        };
    }
    match &parsed.expr {
        Expr::Index { .. } => Operand::Value {
            expr: parsed.expr.clone(),
            force_far: false,
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
        (HlaCpuRegister::B, _) | (_, HlaCpuRegister::B) => Some("use b><a to swap A and B"),
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
            .map(|mnemonic| Instruction {
                mnemonic: mnemonic.to_string(),
                operand: None,
            }),
        (Some(dest_register), None) => {
            load_mnemonic_for_register(dest_register).map(|mnemonic| Instruction {
                mnemonic: mnemonic.to_string(),
                operand: Some(Operand::Auto {
                    expr: Expr::Ident(src.to_string()),
                }),
            })
        }
        (None, Some(src_register)) => {
            store_mnemonic_for_register(src_register).map(|mnemonic| Instruction {
                mnemonic: mnemonic.to_string(),
                operand: Some(Operand::Auto {
                    expr: Expr::Ident(dest.to_string()),
                }),
            })
        }
        (None, None) => None,
    }
}

fn resolve_chain_pair_expr(dest: &str, src: &HlaOperandExpr) -> Option<Instruction> {
    let dest_register = parse_cpu_register_name(dest)?;
    let mnemonic = load_mnemonic_for_register(dest_register)?;
    Some(Instruction {
        mnemonic: mnemonic.to_string(),
        operand: Some(lower_hla_operand_to_operand(src)),
    })
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
        HlaStmt::RegisterAssign { register, rhs } => {
            let Some(mnemonic) = load_mnemonic_for_register(*register) else {
                match register {
                    HlaCpuRegister::C => diagnostics.push(Diagnostic::error(
                        span,
                        "C is the 16-bit accumulator; use a=expr for loads",
                    )),
                    _ => diagnostics.push(Diagnostic::error(
                        span,
                        format!(
                            "cannot load register '{}'",
                            format_hla_cpu_register(*register)
                        ),
                    )),
                }
                return;
            };
            let instruction = Instruction {
                mnemonic: mnemonic.to_string(),
                operand: Some(lower_hla_operand_to_operand(rhs)),
            };
            lower_instruction_stmt(&instruction, scope, sema, span, ctx, diagnostics, ops);
        }
        HlaStmt::RegisterStore { dest, src } => {
            let Some(mnemonic) = store_mnemonic_for_register(*src) else {
                diagnostics.push(Diagnostic::error(
                    span,
                    format!(
                        "cannot store register '{}' with assignment syntax",
                        format_hla_cpu_register(*src)
                    ),
                ));
                return;
            };
            let instruction = Instruction {
                mnemonic: mnemonic.to_string(),
                operand: Some(Operand::Value {
                    expr: dest.expr.clone(),
                    force_far: false,
                    index: dest.index,
                    addr_mode: dest.addr_mode,
                }),
            };
            lower_instruction_stmt(&instruction, scope, sema, span, ctx, diagnostics, ops);
        }
        HlaStmt::MemStoreZero { dest } => {
            if !stz_compatible_dest_mode(dest) {
                diagnostics.push(Diagnostic::error(
                    span,
                    "'mem = 0' requires zp, zp,X, abs, or abs,X addressing — \
                     use 'mem = a = 0' for other modes",
                ));
                return;
            }
            let instruction = Instruction {
                mnemonic: "stz".to_string(),
                operand: Some(Operand::Value {
                    expr: dest.expr.clone(),
                    force_far: false,
                    index: dest.index,
                    addr_mode: dest.addr_mode,
                }),
            };
            lower_instruction_stmt(&instruction, scope, sema, span, ctx, diagnostics, ops);
        }
        HlaStmt::RegisterTransfer { dest, src } => {
            let Some(mnemonic) = resolve_transfer(*dest, *src) else {
                let msg = format!(
                    "transfer '{}' to '{}' is not directly supported",
                    format_hla_cpu_register(*src).to_ascii_uppercase(),
                    format_hla_cpu_register(*dest).to_ascii_uppercase(),
                );
                let diagnostic = match invalid_transfer_hint(*dest, *src) {
                    Some(hint) => Diagnostic::error(span, msg).with_help(hint),
                    None => Diagnostic::error(span, msg),
                };
                diagnostics.push(diagnostic);
                return;
            };
            let instruction = Instruction {
                mnemonic: mnemonic.to_string(),
                operand: None,
            };
            lower_instruction_stmt(&instruction, scope, sema, span, ctx, diagnostics, ops);
        }
        HlaStmt::AssignmentChain { idents, tail_expr } => {
            lower_assignment_chain(
                idents,
                tail_expr.as_ref(),
                scope,
                sema,
                span,
                ctx,
                diagnostics,
                ops,
            );
        }
        HlaStmt::AccumulatorAlu { op, rhs } => {
            let mnemonic = match op {
                HlaAluOp::Add => "adc",
                HlaAluOp::Sub => "sbc",
                HlaAluOp::And => "and",
                HlaAluOp::Or => "ora",
                HlaAluOp::Xor => "eor",
            };
            let instruction = Instruction {
                mnemonic: mnemonic.to_string(),
                operand: Some(lower_hla_operand_to_operand(rhs)),
            };
            lower_instruction_stmt(&instruction, scope, sema, span, ctx, diagnostics, ops);
        }
        HlaStmt::AccumulatorBitTest { rhs } => {
            let instruction = Instruction {
                mnemonic: "bit".to_string(),
                operand: Some(lower_hla_operand_to_operand(rhs)),
            };
            lower_instruction_stmt(&instruction, scope, sema, span, ctx, diagnostics, ops);
        }
        HlaStmt::IndexCompare { register, rhs } => {
            let mnemonic = match register {
                crate::ast::IndexRegister::X => "cpx",
                crate::ast::IndexRegister::Y => "cpy",
                crate::ast::IndexRegister::S => {
                    diagnostics.push(Diagnostic::error(span, "cannot compare S register"));
                    return;
                }
            };
            let instruction = Instruction {
                mnemonic: mnemonic.to_string(),
                operand: Some(lower_hla_operand_to_operand(rhs)),
            };
            lower_instruction_stmt(&instruction, scope, sema, span, ctx, diagnostics, ops);
        }
        HlaStmt::IncDec { op, target } => {
            let mnemonic = match op {
                HlaIncDecOp::Inc => "inc",
                HlaIncDecOp::Dec => "dec",
            };
            match target {
                HlaIncDecTarget::Register(register) => {
                    let mnemonic = match (op, register) {
                        (HlaIncDecOp::Inc, crate::ast::IndexRegister::X) => "inx",
                        (HlaIncDecOp::Inc, crate::ast::IndexRegister::Y) => "iny",
                        (HlaIncDecOp::Dec, crate::ast::IndexRegister::X) => "dex",
                        (HlaIncDecOp::Dec, crate::ast::IndexRegister::Y) => "dey",
                        (_, crate::ast::IndexRegister::S) => {
                            diagnostics.push(Diagnostic::error(
                                span,
                                "cannot increment/decrement S register",
                            ));
                            return;
                        }
                    };
                    let instruction = Instruction {
                        mnemonic: mnemonic.to_string(),
                        operand: None,
                    };
                    lower_instruction_stmt(&instruction, scope, sema, span, ctx, diagnostics, ops);
                }
                HlaIncDecTarget::Address(address) => {
                    let instruction = Instruction {
                        mnemonic: mnemonic.to_string(),
                        operand: Some(Operand::Value {
                            expr: address.expr.clone(),
                            force_far: false,
                            index: address.index,
                            addr_mode: address.addr_mode,
                        }),
                    };
                    lower_instruction_stmt(&instruction, scope, sema, span, ctx, diagnostics, ops);
                }
            }
        }
        HlaStmt::ShiftRotate { op, target } => {
            let mnemonic = match op {
                HlaShiftOp::Asl => "asl",
                HlaShiftOp::Lsr => "lsr",
                HlaShiftOp::Rol => "rol",
                HlaShiftOp::Ror => "ror",
            };
            let instruction = match target {
                HlaShiftTarget::Accumulator => Instruction {
                    mnemonic: mnemonic.to_string(),
                    operand: None,
                },
                HlaShiftTarget::Address(address) => Instruction {
                    mnemonic: mnemonic.to_string(),
                    operand: Some(Operand::Value {
                        expr: address.expr.clone(),
                        force_far: false,
                        index: address.index,
                        addr_mode: address.addr_mode,
                    }),
                },
            };
            lower_instruction_stmt(&instruction, scope, sema, span, ctx, diagnostics, ops);
        }
        HlaStmt::FlagSet { flag, set } => {
            let mnemonic = match (flag, set) {
                (HlaFlag::Carry, true) => Some("sec"),
                (HlaFlag::Carry, false) => Some("clc"),
                (HlaFlag::Decimal, true) => Some("sed"),
                (HlaFlag::Decimal, false) => Some("cld"),
                (HlaFlag::Interrupt, true) => Some("sei"),
                (HlaFlag::Interrupt, false) => Some("cli"),
                (HlaFlag::Overflow, false) => Some("clv"),
                (HlaFlag::Overflow, true) => None,
            };
            let Some(mnemonic) = mnemonic else {
                diagnostics.push(Diagnostic::error(
                    span,
                    "overflow flag cannot be explicitly set with shorthand",
                ));
                return;
            };
            let instruction = Instruction {
                mnemonic: mnemonic.to_string(),
                operand: None,
            };
            lower_instruction_stmt(&instruction, scope, sema, span, ctx, diagnostics, ops);
        }
        HlaStmt::StackOp { target, push } => {
            let mnemonic = match (target, push) {
                (HlaStackTarget::A, true) => "pha",
                (HlaStackTarget::A, false) => "pla",
                (HlaStackTarget::X, true) => "phx",
                (HlaStackTarget::X, false) => "plx",
                (HlaStackTarget::Y, true) => "phy",
                (HlaStackTarget::Y, false) => "ply",
                (HlaStackTarget::B, true) => "phb",
                (HlaStackTarget::B, false) => "plb",
                (HlaStackTarget::D, true) => "phd",
                (HlaStackTarget::D, false) => "pld",
                (HlaStackTarget::P, true) => "php",
                (HlaStackTarget::P, false) => "plp",
            };
            let instruction = Instruction {
                mnemonic: mnemonic.to_string(),
                operand: None,
            };
            lower_instruction_stmt(&instruction, scope, sema, span, ctx, diagnostics, ops);
        }
        HlaStmt::Goto {
            target,
            indirect,
            far,
        } => {
            let instruction = Instruction {
                mnemonic: if *far { "jml" } else { "jmp" }.to_string(),
                operand: Some(Operand::Value {
                    expr: target.clone(),
                    force_far: *far,
                    index: None,
                    addr_mode: if *indirect {
                        OperandAddrMode::Indirect
                    } else {
                        OperandAddrMode::Direct
                    },
                }),
            };
            lower_instruction_stmt(&instruction, scope, sema, span, ctx, diagnostics, ops);
        }
        HlaStmt::BranchGoto {
            mnemonic, target, ..
        } => {
            let instruction = Instruction {
                mnemonic: mnemonic.clone(),
                operand: Some(Operand::Value {
                    expr: target.clone(),
                    force_far: false,
                    index: None,
                    addr_mode: OperandAddrMode::Direct,
                }),
            };
            lower_instruction_stmt(&instruction, scope, sema, span, ctx, diagnostics, ops);
        }
        HlaStmt::Return { interrupt } => {
            let mnemonic = if *interrupt {
                "rti"
            } else if ctx.is_far {
                "rtl"
            } else {
                "rts"
            };
            let instruction = Instruction {
                mnemonic: mnemonic.to_string(),
                operand: None,
            };
            lower_instruction_stmt(&instruction, scope, sema, span, ctx, diagnostics, ops);
        }
        HlaStmt::XAssignImmediate { rhs } => {
            let instruction = Instruction {
                mnemonic: "ldx".to_string(),
                operand: Some(Operand::Immediate {
                    expr: rhs.clone(),
                    explicit_hash: false,
                }),
            };
            lower_instruction_stmt(&instruction, scope, sema, span, ctx, diagnostics, ops);
        }
        HlaStmt::XIncrement => {
            let instruction = Instruction {
                mnemonic: "inx".to_string(),
                operand: None,
            };
            lower_instruction_stmt(&instruction, scope, sema, span, ctx, diagnostics, ops);
        }
        HlaStmt::StoreFromA {
            dests,
            rhs,
            load_start,
            store_end,
        } => {
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
                    } => lower_hla_operand_to_operand(&HlaOperandExpr {
                        expr: expr.clone(),
                        index: *index,
                        addr_mode: *addr_mode,
                    }),
                }),
            };
            let lda_span = load_start
                .map(|start| Span::new(span.source_id, start, span.end))
                .unwrap_or(span);
            let ops_len_before_lda = ops.len();
            lower_instruction_stmt(
                &lda_instruction,
                scope,
                sema,
                lda_span,
                ctx,
                diagnostics,
                ops,
            );
            if ops.len() == ops_len_before_lda {
                return;
            }

            // Store to each destination in reverse order (innermost first)
            let sta_span = store_end
                .map(|end| Span::new(span.source_id, span.start, end))
                .unwrap_or(span);
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
                lower_instruction_stmt(
                    &sta_instruction,
                    scope,
                    sema,
                    sta_span,
                    ctx,
                    diagnostics,
                    ops,
                );
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
            let ops_len_before_bit = ops.len();
            lower_instruction_stmt(&bit_instruction, scope, sema, span, ctx, diagnostics, ops);
            if ops.len() == ops_len_before_bit {
                return;
            }

            emit_branch_to_label("bpl", &wait_label, scope, sema, span, ctx, diagnostics, ops);
        }
        HlaStmt::ConditionSeed { .. } => {
            if let HlaStmt::ConditionSeed { rhs, .. } = stmt {
                let instruction = Instruction {
                    mnemonic: "cmp".to_string(),
                    operand: Some(lower_hla_operand_to_operand(rhs)),
                };
                lower_instruction_stmt(&instruction, scope, sema, span, ctx, diagnostics, ops);
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
                lower_instruction_stmt(&nop, scope, sema, span, ctx, diagnostics, ops);
            }
        }
        HlaStmt::NeverBlock { .. } | HlaStmt::PrefixConditional { .. } => {
            // Handled in lower_stmt directly (needs fs and current_segment parameters)
        }
    }
}

#[allow(clippy::too_many_arguments)]
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

#[allow(clippy::too_many_arguments)]
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

    let rhs = condition
        .rhs
        .as_ref()
        .unwrap_or(&Expr::Number(0, NumFmt::Dec));
    let Some(rhs_number) = eval_to_number(rhs, scope, sema, span, diagnostics) else {
        return;
    };

    let cmp_span = condition.seed_span.unwrap_or(span);
    let compare_instruction = Instruction {
        mnemonic: "cmp".to_string(),
        operand: Some(Operand::Immediate {
            expr: Expr::Number(rhs_number, NumFmt::Dec),
            explicit_hash: false,
        }),
    };
    let ops_len_before_cmp = ops.len();
    lower_instruction_stmt(
        &compare_instruction,
        scope,
        sema,
        cmp_span,
        ctx,
        diagnostics,
        ops,
    );
    if ops.len() == ops_len_before_cmp {
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

    for value in values {
        if let Expr::Unary { op, expr } = value {
            match op {
                ExprUnaryOp::WordLittleEndian | ExprUnaryOp::FarLittleEndian => {
                    let byte_count = match op {
                        ExprUnaryOp::WordLittleEndian => 2usize,
                        ExprUnaryOp::FarLittleEndian => 3usize,
                        _ => unreachable!(),
                    };
                    if let Some(number) = eval_to_number_strict(expr, sema, span, diagnostics) {
                        let range_ok = match op {
                            ExprUnaryOp::WordLittleEndian => (0..=0xFFFF).contains(&number),
                            ExprUnaryOp::FarLittleEndian => (0..=0xFFFFFF).contains(&number),
                            _ => false,
                        };
                        if !range_ok {
                            let kind = match op {
                                ExprUnaryOp::WordLittleEndian => "word",
                                ExprUnaryOp::FarLittleEndian => "far address",
                                _ => unreachable!(),
                            };
                            diagnostics.push(Diagnostic::error(
                                span,
                                format!("{kind} literal out of range: {number}"),
                            ));
                            return None;
                        }
                        let le = (number as u32).to_le_bytes();
                        bytes.extend_from_slice(&le[..byte_count]);
                        continue;
                    }

                    if let Some(symbol) = expr_ident_name(expr.as_ref())
                        && !sema.vars.contains_key(symbol)
                        && !sema.consts.contains_key(symbol)
                    {
                        match resolve_symbolic_subscript_name(symbol, sema, span, diagnostics) {
                            Ok(Some(_)) | Err(()) => {}
                            Ok(None) => {
                                let label = resolve_symbol(symbol, scope, span, diagnostics)?;
                                let kind = match op {
                                    ExprUnaryOp::WordLittleEndian => ByteRelocationKind::FullWord,
                                    ExprUnaryOp::FarLittleEndian => ByteRelocationKind::FullLong,
                                    _ => unreachable!(),
                                };
                                let offset = u32::try_from(bytes.len())
                                    .expect("byte expression offset should fit in u32");
                                bytes.extend(std::iter::repeat_n(0, byte_count));
                                relocations.push(ByteRelocation {
                                    offset,
                                    kind,
                                    label,
                                });
                                continue;
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
                    return None;
                }
                ExprUnaryOp::LowByte | ExprUnaryOp::HighByte | ExprUnaryOp::EvalBracketed => {}
            }
        }

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
                let offset =
                    u32::try_from(bytes.len()).expect("byte expression offset should fit in u32");
                bytes.push(0);
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

        let number = eval_to_number(value, scope, sema, span, diagnostics)?;
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
        if let Some(symbol) = expr_ident_name(value)
            && !sema.vars.contains_key(symbol)
            && !sema.consts.contains_key(symbol)
        {
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

        // Fallback: try general numeric evaluation.
        let number = eval_to_number(value, scope, sema, span, diagnostics)?;
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
        if let Some(symbol) = expr_ident_name(value)
            && !sema.vars.contains_key(symbol)
            && !sema.consts.contains_key(symbol)
        {
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

        // Fallback: try general numeric evaluation.
        let number = eval_to_number(value, scope, sema, span, diagnostics)?;
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
        Expr::Number(value, _) => Some(*value),
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
                ExprBinaryOp::Mul => lhs.checked_mul(rhs),
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
            }
        }
        Expr::TypedView { expr, .. } | Expr::AddressHint { expr, .. } => {
            eval_to_number_strict(expr, sema, span, diagnostics)
        }
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
        | ExprUnaryOp::EvalBracketed => return None,
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
    force_far: bool,
    index: Option<crate::ast::IndexRegister>,
    addr_mode: OperandAddrMode,
    expr: &Expr,
    sema: &SemanticModel,
) -> bool {
    if force_far || index.is_some() || addr_mode != OperandAddrMode::Direct {
        return false;
    }
    if effective_address_hint(expr, sema).is_some() {
        return false;
    }
    is_immediate_expression(expr, sema)
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
        Expr::Index { .. } | Expr::EvalText(_) => false,
        Expr::TypedView { expr, .. } | Expr::AddressHint { expr, .. } => {
            is_immediate_expression(expr, sema)
        }
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
    // Try as a symbolic subscript field path (e.g. TASKS.state)
    match resolve_symbolic_subscript_name(name, sema, span, diagnostics) {
        Ok(Some(ResolvedSymbolicSubscriptName::Aggregate { base, .. })) => {
            let var = &sema.vars[&base];
            Some(i64::from(var.element_size))
        }
        Ok(Some(ResolvedSymbolicSubscriptName::Field { size, .. })) => Some(i64::from(size)),
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
    // Plain var — offsetof makes no sense
    if sema.vars.contains_key(name) {
        diagnostics.push(
            Diagnostic::error(
                span,
                format!("':offsetof' requires a field path, not a bare variable '{name}'"),
            )
            .with_help("use 'VAR.field:offsetof' to get the byte offset of a field"),
        );
        return None;
    }
    // Try as a symbolic subscript field path (e.g. TASKS.state)
    match resolve_symbolic_subscript_name(name, sema, span, diagnostics) {
        Ok(Some(ResolvedSymbolicSubscriptName::Aggregate { base, .. })) => {
            diagnostics.push(
                Diagnostic::error(
                    span,
                    format!("':offsetof' requires a field path, not aggregate '{base}'"),
                )
                .with_help("use 'VAR.field:offsetof' to get the byte offset of a field"),
            );
            None
        }
        Ok(Some(ResolvedSymbolicSubscriptName::Field { base, field, .. })) => {
            // Look up the field's offset from the base var's symbolic subscript
            let var = &sema.vars[&base];
            let ss = var.symbolic_subscript.as_ref().unwrap();
            let field_meta = &ss.fields[&field];
            Some(i64::from(field_meta.offset))
        }
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
    );

    if let Some(requested) = expr_ident_name(index)
        && let Some(symbolic_subscript) = sema
            .vars
            .get(base)
            .and_then(|var| var.symbolic_subscript.as_ref())
        && let Some(suggestion) = suggest_symbolic_subscript_field(requested, symbolic_subscript)
    {
        diagnostic = diagnostic.with_help(format!(
            "use '.field' or '[.field]' — did you mean '.{suggestion}'?"
        ));
        return diagnostic;
    }

    diagnostic.with_help("use '.field' or '[.field]' for named field access")
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
            force_far,
            index,
            addr_mode,
        }) => {
            if value_operand_uses_immediate(*force_far, *index, *addr_mode, expr, sema) {
                let eval_span = immediate_expr_span(expr, span, false);
                lower_immediate_operand(expr, scope, sema, eval_span, diagnostics)
            } else {
                let mode = lower_operand_mode(*addr_mode, *index);
                let size_hint = address_size_hint_for_expr(expr, sema, *force_far);
                lower_address_operand(expr, scope, sema, span, diagnostics, size_hint, mode)
            }
        }
        Some(Operand::Auto { expr }) => {
            if effective_address_hint(expr, sema).is_none() && is_immediate_expression(expr, sema) {
                let eval_span = immediate_expr_span(expr, span, false);
                lower_immediate_operand(expr, scope, sema, eval_span, diagnostics)
            } else {
                let mode = lower_operand_mode(OperandAddrMode::Direct, None);
                let size_hint = address_size_hint_for_expr(expr, sema, false);
                lower_address_operand(expr, scope, sema, span, diagnostics, size_hint, mode)
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
    if let Some((kind, label)) =
        resolve_symbolic_byte_relocation(expr, scope, sema, span, diagnostics)
    {
        return Some(OperandOp::ImmediateByteRelocation { kind, label });
    }
    let value = eval_to_number(expr, scope, sema, span, diagnostics)?;
    Some(OperandOp::Immediate(value))
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
        Expr::AddressHint { expr, .. } => {
            lower_address_operand(expr, scope, sema, span, diagnostics, size_hint, mode)
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
        Expr::Index { .. } | Expr::Binary { .. } | Expr::Unary { .. } => {
            // Try compile-time evaluation first (works for vars, consts, symbolic subscripts).
            let saved_diag_len = diagnostics.len();
            if let Some(value) = eval_to_number(expr, scope, sema, span, diagnostics) {
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
            // eval_to_number failed — try to decompose as label ± constant for link-time
            // resolution (e.g. `data_block_name + 1`).
            if let Some(result) = try_label_offset_operand(expr, scope, sema, span, size_hint, mode)
            {
                // Remove diagnostics added by the failed eval_to_number attempt.
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
                format!("unknown identifier '{name}'"),
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
                ExprBinaryOp::Mul => lhs.checked_mul(rhs),
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
            }
        }
        Expr::TypedView { expr, .. } | Expr::AddressHint { expr, .. } => {
            eval_to_number(expr, scope, sema, span, diagnostics)
        }
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

fn resolve_operand_ident(
    name: &str,
    scope: Option<&str>,
    sema: &SemanticModel,
    span: Span,
    diagnostics: &mut Vec<Diagnostic>,
    size_hint: AddressSizeHint,
    mode: AddressOperandMode,
) -> Option<OperandOp> {
    if name.starts_with('.') {
        let resolved = resolve_symbol(name, scope, span, diagnostics)?;
        return Some(OperandOp::Address {
            value: AddressValue::Label(resolved),
            size_hint,
            mode,
        });
    }

    if let Some(var) = sema.vars.get(name) {
        return Some(OperandOp::Address {
            value: AddressValue::Literal(var.address),
            size_hint,
            mode,
        });
    }
    if let Some(constant) = sema.consts.get(name) {
        let value = constant_to_exact_i64(
            name,
            constant.value,
            span,
            diagnostics,
            "address expression",
        )?;
        let Ok(address) = u32::try_from(value) else {
            diagnostics.push(Diagnostic::error(
                span,
                format!("address cannot be negative: {value} (from const '{name}')"),
            ));
            return None;
        };
        return Some(OperandOp::Address {
            value: AddressValue::Literal(address),
            size_hint,
            mode,
        });
    }

    match resolve_symbolic_subscript_name(name, sema, span, diagnostics) {
        Ok(Some(ResolvedSymbolicSubscriptName::Aggregate { address, .. }))
        | Ok(Some(ResolvedSymbolicSubscriptName::Field { address, .. })) => {
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
        return Some(OperandOp::Address {
            value: AddressValue::Label(name.to_string()),
            size_hint,
            mode,
        });
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
    // If the name resolves at compile time (var, const, symbolic subscript), this
    // function should not handle it — eval_to_number should have succeeded.
    if sema.vars.contains_key(name) || sema.consts.contains_key(name) {
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
    fn inherits_var_absolute16_hint_for_address_operands() {
        let source = "var target:abs = 0x0012\nfunc main {\n  lda target\n}\n";
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
    fn var_absolute16_hint_prevents_hla_immediate_classification() {
        let source = "var target:abs = 0x0012\nfunc main {\n  a = target\n}\n";
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
    fn resolves_bare_const_operand_to_immediate_value() {
        let source = "const LIMIT = 0x34\nfunc main {\n  lda LIMIT\n}\n";
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
    fn resolves_bare_top_level_evaluator_constant_to_immediate_value() {
        let source = "[ LIMIT = 0x34 ]\nfunc main {\n  lda LIMIT\n}\n";
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
        let source = "var foo[\n  .field_w:byte\n  .idx:byte\n  .string[4]:byte\n] = 0x1234\nfunc main {\n  lda foo.field_w\n  sta foo[.idx]\n  lda foo.string[2]\n}\n";
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
                Op::Rep(mask) => Some(format!("rep:{mask:#04x}")),
                Op::Sep(mask) => Some(format!("sep:{mask:#04x}")),
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
                Op::Rep(mask) => Some(format!("rep:{mask:#04x}")),
                Op::Sep(mask) => Some(format!("sep:{mask:#04x}")),
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
                Op::Rep(mask) => Some(format!("rep:{mask:#04x}")),
                Op::Sep(mask) => Some(format!("sep:{mask:#04x}")),
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
                .contains("register x is live after call to 'touch' but clobbered by it")
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
                .contains("register x is live after call to 'touch' but clobbered by it")
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
        let lowered = lower_with_warnings(&file, &sema, &fs, None).expect("lower");

        assert!(lowered.warnings.is_empty());
    }

    #[test]
    fn warns_on_index_use_after_narrowing_without_reload() {
        let source = "func main {\n  @i16\n  ldx #$1234\n  ldy #$5678\n  @i8\n  txa\n  tya\n}\n";
        let file = parse(SourceId(0), source).expect("parse");
        let sema = analyze(&file).expect("analyze");
        let fs = StdAssetFS;
        let lowered = lower_with_warnings(&file, &sema, &fs, None).expect("lower");

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
