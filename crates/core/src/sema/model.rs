use super::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FunctionMeta {
    pub is_far: bool,
    pub is_naked: bool,
    pub is_inline: bool,
    pub mode_contract: ModeContract,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SymbolicSubscriptFieldMeta {
    pub offset: u32,
    pub size: u32,
    pub data_width: DataWidth,
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
    pub size: u32,
    pub data_width: Option<DataWidth>,
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
