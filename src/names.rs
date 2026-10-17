/*****************************************************************************/
/*                                                                           */
/*                                  names.rs                                 */
/*                                                                           */
/*        Every name a program writes, and the slot each one sits in         */
/*                                                                           */
/*                                                                           */
/* Copyright (c) 2026 Tomasz Sterna (smokku)                                 */
/*                                                                           */
/* Permission to use, copy, modify, and/or distribute this software for any  */
/* purpose with or without fee is hereby granted.                            */
/*                                                                           */
/* THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES  */
/* WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF          */
/* MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR   */
/* ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES    */
/* WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN     */
/* ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF   */
/* OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.            */
/*                                                                           */
/*****************************************************************************/

//! The inventory of names: every [`Name`] an AST holds, with the role of the
//! slot it sits in.
//!
//! It exists for one gate. The reference index the editor reads is filled at
//! the point a name *resolves*, and those points are scattered across five
//! modules — so "is every resolution site recorded?" cannot be answered by
//! counting them. It can be answered from the other side: walk every name a
//! program writes and require each one to be a declaration, or to have an
//! index entry, or to sit under a diagnostic, or to be synthetic
//! (`plans/07-lsp-tooling.md` decision 11,
//! `memory/inventory-from-the-enumerable-side`).
//!
//! Deliberately not in `src/ast.rs`: [`NameRole`] is a fact about grammar
//! *positions*, not a language construct, and `src/ast.rs`'s enums are read
//! by `tests/compat.rs` as the list of constructs the K65 manifest has to
//! classify.

use crate::ast::*;

/// What a [`Name`] is doing in the slot it occupies.
///
/// The classification is structural — a property of the grammar position, not
/// a judgement about the program — which is what lets a gate walk every
/// `Name` in a file and know which ones a reference index has to explain
/// (`plans/07-lsp-tooling.md` decision 11).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NameRole {
    /// A declaring occurrence: of a symbol, or of an evaluator binding.
    Declaration,
    /// A use that resolves through the unit's symbol table.
    Symbol,
    /// A `.field` step, which resolves through a layout rather than the
    /// symbol table — a field has no `SymbolId` and no scope of its own.
    Field,
    /// A name in the compile-time evaluator's language, which resolves
    /// against the evaluator's own environment first and the unit's symbols
    /// only after that.
    Evaluator,
    /// A slot whose vocabulary the language fixes and no declaration can
    /// extend: a mnemonic, a flag letter in a `->` clause, a segment name.
    Fixed,
}

/// Visits every [`Name`] in `file`, with the role of the slot it sits in.
///
/// Exhaustive by construction: every match below lists its variants, so a
/// construct added later cannot carry a name this walk silently skips. That
/// is the whole point — the inventory has to come from the side that can be
/// enumerated, not from a list someone keeps by hand
/// (`memory/inventory-from-the-enumerable-side`).
pub fn visit_names(file: &AstFile, visit: &mut impl FnMut(&Name, NameRole)) {
    for item in &file.items {
        item_names(item, visit);
    }
}

/// Visits every [`Name`] in one body, with the role of the slot it sits in.
pub fn visit_block(block: &Block, visit: &mut impl FnMut(&Name, NameRole)) {
    block_names(block, visit);
}

fn item_names(item: &Item, visit: &mut impl FnMut(&Name, NameRole)) {
    match &item.kind {
        ItemKind::Var(declaration) => var_names(declaration, visit),
        ItemKind::Const(declaration) => {
            for entry in &declaration.entries {
                visit(&entry.name, NameRole::Declaration);
                if let Some(value) = &entry.value {
                    expr_names(value, visit);
                }
            }
        }
        ItemKind::Segment(name) => visit(name, NameRole::Fixed),
        ItemKind::Data(data) => data_names(data, visit),
        ItemKind::EvalBlock(expr) => expr_names(expr, visit),
        ItemKind::EvalFunc(function) => {
            visit(&function.name, NameRole::Declaration);
            for parameter in &function.parameters {
                visit(parameter, NameRole::Declaration);
            }
            eval_names(&function.body, visit);
        }
        ItemKind::Preprocessor(directive) => preprocessor_names(directive, visit),
        ItemKind::Binary(declaration) => visit(&declaration.name, NameRole::Declaration),
        ItemKind::Reserved(reserved) => reserved_names(reserved, visit),
        ItemKind::ModuleWidth(_) => {}
        // A path is not a name. Nothing renames a file, and an editor that
        // offered to would be wrong about what the string means.
        ItemKind::Import(_) => {}
        ItemKind::Extern(declaration) => visit(
            crate::ast::extern_name(&declaration.kind),
            NameRole::Declaration,
        ),
        ItemKind::Func(function) => {
            visit(&function.name, NameRole::Declaration);
            if let Some(contract) = &function.contract {
                contract_names(contract, visit);
            }
            block_names(&function.body, visit);
        }
        ItemKind::Error => {}
    }
}

fn var_names(declaration: &VarDeclaration, visit: &mut impl FnMut(&Name, NameRole)) {
    visit(&declaration.name, NameRole::Declaration);
    for expr in [&declaration.size, &declaration.count, &declaration.address]
        .into_iter()
        .flatten()
    {
        expr_names(expr, visit);
    }
    field_declaration_names(&declaration.fields, visit);
}

fn field_declaration_names(fields: &[FieldDeclaration], visit: &mut impl FnMut(&Name, NameRole)) {
    for field in fields {
        visit(&field.name, NameRole::Declaration);
        if let Some(size) = &field.size {
            expr_names(size, visit);
        }
        field_declaration_names(&field.fields, visit);
    }
}

fn contract_names(contract: &CallContract, visit: &mut impl FnMut(&Name, NameRole)) {
    for parameter in contract.inputs.iter().flatten() {
        match parameter {
            ContractParameter::Register(_) => {}
            ContractParameter::ImmediateAlias { name, .. } => {
                visit(name, NameRole::Declaration);
            }
            ContractParameter::Alias(name) => visit(name, NameRole::Declaration),
        }
    }
    if let Some(outputs) = &contract.outputs {
        output_names(&outputs.outputs, visit);
    }
}

fn output_names(outputs: &[ContractOutput], visit: &mut impl FnMut(&Name, NameRole)) {
    for output in outputs {
        match output {
            ContractOutput::Register(_) => {}
            // A flag letter names one of `c z n v` and nothing a declaration
            // can add to.
            ContractOutput::Flag(name) => visit(name, NameRole::Fixed),
        }
    }
}

fn block_names(block: &Block, visit: &mut impl FnMut(&Name, NameRole)) {
    for statement in &block.statements {
        statement_names(statement, visit);
    }
}

fn statement_names(statement: &Statement, visit: &mut impl FnMut(&Name, NameRole)) {
    match &statement.kind {
        StatementKind::Label(label) => visit(&label.name, NameRole::Declaration),
        StatementKind::Native(native) => {
            visit(&native.mnemonic, NameRole::Fixed);
            if let Some(operand) = &native.operand {
                native_operand_names(operand, visit);
            }
            if let Some(repeat) = &native.repeat {
                expr_names(repeat, visit);
            }
        }
        StatementKind::Hla(hla) => hla_names(hla, visit),
        StatementKind::WidthBlock(width) => block_names(&width.body, visit),
        StatementKind::ModeSet(_) => {}
        StatementKind::Call(call) => visit(&call.target, NameRole::Symbol),
        StatementKind::BareCall(call) => {
            visit(&call.target, NameRole::Symbol);
            for argument in call.arguments.iter().flatten() {
                match argument {
                    CallArgument::Register(_) => {}
                    CallArgument::Immediate(expr) => expr_names(expr, visit),
                }
            }
            if let Some(outputs) = &call.outputs {
                output_names(outputs, visit);
            }
        }
        StatementKind::Conditional(conditional) => {
            for slot in conditional.condition.slots() {
                block_names(slot, visit);
            }
            block_names(&conditional.body, visit);
            if let Some(otherwise) = &conditional.otherwise {
                block_names(otherwise, visit);
            }
        }
        StatementKind::ConditionalJump(jump) => {
            for slot in jump.condition.slots() {
                block_names(slot, visit);
            }
            statement_names(&jump.jump, visit);
        }
        StatementKind::Loop(loop_statement) => {
            block_names(&loop_statement.body, visit);
            if let LoopCondition::Branch(chain) = &loop_statement.condition {
                for slot in chain.slots() {
                    block_names(slot, visit);
                }
            }
        }
        StatementKind::Goto(goto) => expr_names(&goto.target, visit),
        StatementKind::Break | StatementKind::Repeat | StatementKind::Continue => {}
        StatementKind::Nocross(body) => block_names(body, visit),
        StatementKind::Unroll(unroll) => block_names(&unroll.body, visit),
        StatementKind::Data(data) => data_names(data, visit),
        StatementKind::Align(align) => align_names(align, visit),
        StatementKind::Address(address) => expr_names(address, visit),
        StatementKind::Reserved(reserved) => reserved_names(reserved, visit),
        StatementKind::Preprocessor(directive) => preprocessor_names(directive, visit),
        StatementKind::Return(_) => {}
        StatementKind::Error => {}
    }
}

fn native_operand_names(operand: &NativeOperand, visit: &mut impl FnMut(&Name, NameRole)) {
    match operand {
        NativeOperand::Immediate(expr) => expr_names(expr, visit),
        NativeOperand::Register(_) => {}
        NativeOperand::Address(address) => expr_names(&address.value, visit),
        NativeOperand::Indirect(indirect) => expr_names(&indirect.value, visit),
        NativeOperand::LongIndirect(indirect) => expr_names(&indirect.value, visit),
        NativeOperand::BlockMove(move_operand) => {
            expr_names(&move_operand.source, visit);
            expr_names(&move_operand.destination, visit);
        }
    }
}

fn hla_names(hla: &HlaStatement, visit: &mut impl FnMut(&Name, NameRole)) {
    match hla {
        HlaStatement::Assign(assignment) => {
            for target in &assignment.targets {
                hla_operand_names(target, visit);
            }
            hla_operand_names(&assignment.source, visit);
        }
        HlaStatement::Exchange(_) => {}
        HlaStatement::Arithmetic(arithmetic) => hla_operand_names(&arithmetic.operand, visit),
        HlaStatement::Shift(shift) => hla_operand_names(&shift.target, visit),
        HlaStatement::Step(step) => hla_operand_names(&step.target, visit),
        HlaStatement::Flag(_) => {}
        HlaStatement::Stack(_) => {}
        HlaStatement::PushConstant { value, .. } => expr_names(value, visit),
        HlaStatement::Nop { count } => {
            if let Some(count) = count {
                expr_names(count, visit);
            }
        }
        HlaStatement::Break => {}
    }
}

fn hla_operand_names(operand: &HlaOperand, visit: &mut impl FnMut(&Name, NameRole)) {
    match &operand.kind {
        HlaOperandKind::Register(_) => {}
        HlaOperandKind::Immediate(expr) => expr_names(expr, visit),
        HlaOperandKind::Memory(address) => expr_names(&address.value, visit),
        HlaOperandKind::Indirect(indirect) => expr_names(&indirect.value, visit),
        HlaOperandKind::Error => {}
    }
}

fn expr_names(expr: &Expr, visit: &mut impl FnMut(&Name, NameRole)) {
    match &expr.kind {
        ExprKind::Number(_) => {}
        ExprKind::Symbol(name) | ExprKind::LocalSymbol(name) => visit(name, NameRole::Symbol),
        ExprKind::Unary(unary) => expr_names(&unary.operand, visit),
        ExprKind::Binary(binary) => {
            expr_names(&binary.left, visit);
            expr_names(&binary.right, visit);
        }
        ExprKind::Field(access) => {
            expr_names(&access.base, visit);
            visit(&access.field, NameRole::Field);
        }
        ExprKind::Subscript(subscript) => {
            expr_names(&subscript.base, visit);
            expr_names(&subscript.index, visit);
        }
        ExprKind::Metadata(query) => expr_names(&query.base, visit),
        ExprKind::TypedView(view) => expr_names(&view.base, visit),
        ExprKind::Eval(eval) => eval_names(eval, visit),
        ExprKind::Error => {}
    }
}

fn eval_names(expr: &EvalExpr, visit: &mut impl FnMut(&Name, NameRole)) {
    match &expr.kind {
        EvalExprKind::Number { .. }
        | EvalExprKind::Pi
        | EvalExprKind::Float(_)
        | EvalExprKind::String(_) => {}
        EvalExprKind::Symbol(name) => visit(name, NameRole::Evaluator),
        EvalExprKind::Call { name, arguments } => {
            visit(name, NameRole::Evaluator);
            for argument in arguments {
                eval_names(argument, visit);
            }
        }
        EvalExprKind::Unary { operand, .. } => eval_names(operand, visit),
        EvalExprKind::Binary { left, right, .. } => {
            eval_names(left, visit);
            eval_names(right, visit);
        }
        EvalExprKind::Ternary {
            condition,
            consequence,
            alternative,
        } => {
            eval_names(condition, visit);
            eval_names(consequence, visit);
            eval_names(alternative, visit);
        }
        EvalExprKind::Assignment { target, value, .. } => {
            // The evaluator's language has no declaration form: assigning a
            // name that does not exist is what introduces it, so the target
            // of an assignment is the binding occurrence.
            match &target.kind {
                EvalExprKind::Symbol(name) => visit(name, NameRole::Declaration),
                _ => eval_names(target, visit),
            }
            eval_names(value, visit);
        }
        EvalExprKind::Subscript { base, indices } => {
            eval_names(base, visit);
            for index in indices {
                eval_names(index, visit);
            }
        }
        EvalExprKind::Member { base, field } => {
            eval_names(base, visit);
            visit(field, NameRole::Evaluator);
        }
        EvalExprKind::MethodCall {
            base,
            method,
            arguments,
        } => {
            eval_names(base, visit);
            visit(method, NameRole::Evaluator);
            for argument in arguments {
                eval_names(argument, visit);
            }
        }
        EvalExprKind::List(items) | EvalExprKind::Block(items) => {
            for item in items {
                eval_names(item, visit);
            }
        }
        EvalExprKind::If {
            condition,
            consequence,
            alternative,
        } => {
            eval_names(condition, visit);
            eval_names(consequence, visit);
            if let Some(alternative) = alternative {
                eval_names(alternative, visit);
            }
        }
        EvalExprKind::While { condition, body } => {
            eval_names(condition, visit);
            eval_names(body, visit);
        }
        EvalExprKind::ReservedCall(call) => {
            for argument in &call.arguments {
                eval_names(argument, visit);
            }
        }
        EvalExprKind::Error => {}
    }
}

fn data_names(data: &DataBlock, visit: &mut impl FnMut(&Name, NameRole)) {
    if let Some(name) = &data.name {
        visit(name, NameRole::Declaration);
    }
    data_entry_names(&data.entries, visit);
}

fn data_entry_names(entries: &[DataEntry], visit: &mut impl FnMut(&Name, NameRole)) {
    for entry in entries {
        match &entry.kind {
            DataEntryKind::Segment(name) => visit(name, NameRole::Fixed),
            DataEntryKind::Align(align) => align_names(align, visit),
            DataEntryKind::Address(address) => expr_names(address, visit),
            DataEntryKind::Nocross => {}
            DataEntryKind::Charset(_) => {}
            DataEntryKind::Label(name) => visit(name, NameRole::Declaration),
            DataEntryKind::Typed { values, .. } => {
                for value in values {
                    data_value_names(value, visit);
                }
            }
            DataEntryKind::Value(value) => data_value_names(value, visit),
            DataEntryKind::Code { body, .. } => block_names(body, visit),
            DataEntryKind::NocrossGroup(inner) => data_entry_names(inner, visit),
            DataEntryKind::Evaluator(expr) => eval_names(expr, visit),
            DataEntryKind::Repeat { count, body } => {
                expr_names(count, visit);
                data_names(body, visit);
            }
            DataEntryKind::ForEval {
                variable,
                from,
                to,
                body,
            } => {
                visit(variable, NameRole::Declaration);
                expr_names(from, visit);
                expr_names(to, visit);
                expr_names(body, visit);
            }
            DataEntryKind::Preprocessor(directive) => preprocessor_names(directive, visit),
            DataEntryKind::Binary(_) => {}
            DataEntryKind::Reserved(reserved) => reserved_names(reserved, visit),
            DataEntryKind::Error => {}
        }
    }
}

fn data_value_names(value: &DataValue, visit: &mut impl FnMut(&Name, NameRole)) {
    match value {
        DataValue::Expr(expr) => expr_names(expr, visit),
        DataValue::String { .. } => {}
        DataValue::Undefined => {}
    }
}

fn align_names(align: &AlignDirective, visit: &mut impl FnMut(&Name, NameRole)) {
    expr_names(&align.boundary, visit);
    if let Some(offset) = &align.offset {
        expr_names(offset, visit);
    }
}

fn preprocessor_names(directive: &PreprocessorDirective, visit: &mut impl FnMut(&Name, NameRole)) {
    if let Some(condition) = &directive.condition {
        eval_names(condition, visit);
    }
}

fn reserved_names(reserved: &ReservedConstruct, visit: &mut impl FnMut(&Name, NameRole)) {
    if let Some(name) = &reserved.name {
        visit(name, NameRole::Declaration);
    }
}
