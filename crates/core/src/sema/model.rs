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
