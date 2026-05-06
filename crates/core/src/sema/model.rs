use super::*;
use crate::ast::{ContractParam, ForceAddrMode, RegName};

#[derive(Debug, Clone)]
pub struct FunctionMeta {
    pub is_far: bool,
    pub is_naked: bool,
    pub is_inline: bool,
    pub has_contract: bool,
    pub params: Vec<ContractParam>,
    pub outputs: Vec<RegName>,
    pub mode_contract: ModeContract,
    pub exit_contract: Option<ModeContract>,
}

impl FunctionMeta {
    /// Pretty-prints the function's call signature in source form, e.g.
    /// `sub(a, #b) -> a`, `dispatch`, `produce() -> a`. Used by diagnostic
    /// help text so users always see the exact call form instead of vague
    /// "match the declaration" wording.
    pub fn signature_call_form(&self, name: &str) -> String {
        if !self.has_contract {
            return name.to_string();
        }
        let params = self
            .params
            .iter()
            .map(format_contract_param)
            .collect::<Vec<_>>()
            .join(", ");
        let outputs = self
            .outputs
            .iter()
            .map(|reg| reg_name_text(*reg).to_string())
            .collect::<Vec<_>>()
            .join(", ");
        match (params.as_str(), outputs.as_str()) {
            ("", "") => format!("{name}()"),
            (p, "") => format!("{name}({p})"),
            ("", o) => format!("{name}() -> {o}"),
            (p, o) => format!("{name}({p}) -> {o}"),
        }
    }

    /// English description of the function kind with article, e.g.
    /// `"a naked "`, `"an inline "`, `"a "`. Suitable for interpolating into
    /// diagnostic sentences that continue with `"function"`.
    pub fn kind_prefix_with_article(&self) -> &'static str {
        if self.is_naked {
            "a naked "
        } else if self.is_inline {
            "an inline "
        } else {
            "a "
        }
    }
}

fn format_contract_param(param: &ContractParam) -> String {
    match param {
        ContractParam::Register(reg) => reg_name_text(*reg).to_string(),
        ContractParam::Immediate(p) => format!("#{}", p.name),
        ContractParam::Alias(name) => name.clone(),
    }
}

fn reg_name_text(reg: RegName) -> &'static str {
    match reg {
        RegName::A => "a",
        RegName::X => "x",
        RegName::Y => "y",
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SymbolicSubscriptFieldMeta {
    pub offset: u32,
    pub size: u32,
    /// `None` for composite (nested) fields.
    pub data_width: Option<DataWidth>,
    pub count: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SymbolicSubscriptMeta {
    pub fields: IndexMap<String, SymbolicSubscriptFieldMeta>,
    pub total_size: u32,
}

/// Where a `var` lives in memory.
///
/// The variant tracks both the storage class (DP / ABS / FAR) and whether the
/// address is compile-time-known.
///
/// - `Fixed` covers any var with an explicit initializer (`var X = $1234`).
///   The storage class still matters for slot-pinning in the DP allocator and
///   is recovered from `VarMeta.addr_mode_default`.
/// - `AllocatedAbs` is an ABS-class var with no initializer. Sema picks an
///   offset within the var's segment via a per-segment cursor; the linker
///   resolves the segment's base.
/// - `AllocatedDp` is a DP-class var with no initializer. The linker picks
///   the final 8-bit DP offset by first-fit allocation across all input
///   objects in link-input order; sema only carries `VarMeta.size` for the
///   request.
/// - `Abstract` is a layout-only `abstract var`: it has field/size metadata
///   but no address, allocation request, or linker-visible symbol.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VarPlacement {
    Fixed { address: u32 },
    AllocatedAbs { segment: String, offset: u32 },
    AllocatedDp,
    Abstract,
}

impl VarPlacement {
    /// Returns the var's compile-time address, or `None` for linker-allocated
    /// vars whose address is only known after layout.
    pub fn compile_time_address(&self) -> Option<u32> {
        match self {
            VarPlacement::Fixed { address } => Some(*address),
            VarPlacement::AllocatedAbs { .. }
            | VarPlacement::AllocatedDp
            | VarPlacement::Abstract => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VarMeta {
    pub placement: VarPlacement,
    /// Total allocation size in bytes (element_size × alloc_count).
    pub size: u32,
    /// Base element size before `* count` multiplication.
    pub element_size: u32,
    /// Number of repeated elements requested by `* count`. Defaults to 1.
    pub repeat_count: u32,
    pub data_width: Option<DataWidth>,
    /// Default address-encoding for plain references to this var. Set by the
    /// `dp`/`abs`/`far` prefix on the `var` declaration. Operand-level prefixes
    /// override this per call site.
    pub addr_mode_default: Option<ForceAddrMode>,
    pub symbolic_subscript: Option<SymbolicSubscriptMeta>,
}

impl VarMeta {
    /// Convenience accessor — returns the compile-time address when the var
    /// is `Fixed`, `None` when it's `Allocated`.
    pub fn compile_time_address(&self) -> Option<u32> {
        self.placement.compile_time_address()
    }

    pub fn is_abstract(&self) -> bool {
        matches!(self.placement, VarPlacement::Abstract)
    }
}

/// Classification of a cross-unit `var` whose address is *not* known at
/// compile time (the declaration has no explicit `= <addr>` initializer, so
/// the address is auto-allocated within the declaring file and resolved by
/// the linker). Carries the addressing-mode information lowering needs to
/// pick the right encoding at the call site, plus the layout-only metadata
/// needed by `:sizeof` / `:offsetof` queries — but not the declaring file's
/// per-file auto-allocated address, which is meaningless across units.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExternalVarClass {
    pub is_abstract: bool,
    pub data_width: Option<DataWidth>,
    pub addr_mode_default: Option<ForceAddrMode>,
    /// Base element size in bytes (before any `* count` multiplier). Used by
    /// `:sizeof` on a bare cross-unit var name.
    pub element_size: u32,
    /// Number of repeated elements requested by `* count`. Defaults to 1.
    pub repeat_count: u32,
    /// Field layout when the cross-unit var is a symbolic-subscript struct.
    /// Populated for `var X[ .a:byte .b:word ... ]` style declarations and
    /// consumed by `:offsetof` / `:sizeof` on dotted field paths.
    pub symbolic_subscript: Option<SymbolicSubscriptMeta>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ConstMeta {
    pub value: Number,
}

#[derive(Debug, Clone, Copy)]
pub struct LabelMeta {
    pub defined_at: Span,
}

#[derive(Debug, Clone, Default)]
pub struct SemanticModel {
    pub functions: IndexMap<String, FunctionMeta>,
    pub vars: IndexMap<String, VarMeta>,
    pub consts: IndexMap<String, ConstMeta>,
    pub labels: IndexMap<String, LabelMeta>,
    /// Cross-unit `var` classifications threaded in from the workspace.
    /// Stored separately from `vars` so callers cannot accidentally read an
    /// (absent) compile-time address from these entries — only the
    /// addressing-mode classification is shared across files.
    pub external_var_classes: IndexMap<String, ExternalVarClass>,
}

/// Cross-unit symbols seeded into a per-file `analyze_partial` so that the
/// per-file pass can see consts/vars declared in other translation units in
/// the same link group. Used by `driver::compile_sources` and the LSP.
#[derive(Default, Clone, Copy)]
pub struct AnalysisExternals<'a> {
    pub consts: Option<&'a IndexMap<String, ConstMeta>>,
    /// Cross-unit vars whose address is *resolvable at compile time* — i.e. the
    /// declaration carried an explicit initializer (`var X = $1234`). Vars with
    /// per-file auto-allocated addresses must NOT be threaded here, since their
    /// `address` field is meaningless across translation units; bare references
    /// to such names already fall through `lower::resolve_operand_ident` to a
    /// label-relocation path.
    pub vars: Option<&'a IndexMap<String, VarMeta>>,
    /// Cross-unit vars whose addresses are linker-resolved (no explicit
    /// initializer). Threaded as classifications only — `data_width` and
    /// `addr_mode_default` — so lowering can upgrade a label-relocation's
    /// `size_hint` to match the declaring file's `dp`/`abs`/`far` prefix
    /// without leaking the per-file auto-allocated address.
    pub external_var_classes: Option<&'a IndexMap<String, ExternalVarClass>>,
}
