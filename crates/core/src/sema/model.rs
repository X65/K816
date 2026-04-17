use super::*;
use crate::ast::{AddressHint, ContractParam, RegName};

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VarMeta {
    pub address: u32,
    /// Total allocation size in bytes (element_size × alloc_count).
    pub size: u32,
    /// Base element size before `* count` multiplication.
    pub element_size: u32,
    pub data_width: Option<DataWidth>,
    pub addr_hint: Option<AddressHint>,
    pub symbolic_subscript: Option<SymbolicSubscriptMeta>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ConstMeta {
    pub value: Number,
}

#[derive(Debug, Clone, Default)]
pub struct SemanticModel {
    pub functions: IndexMap<String, FunctionMeta>,
    pub vars: IndexMap<String, VarMeta>,
    pub consts: IndexMap<String, ConstMeta>,
}
