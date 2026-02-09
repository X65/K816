use crate::ast::{CodeBlock, Expr, File, Instruction, Item, Operand, Stmt, VarDecl};
use crate::diag::Diagnostic;
use crate::parser::parse_expression_fragment;
use crate::span::{SourceId, Span, Spanned};

pub fn expand_file(file: &File, source_id: SourceId) -> Result<File, Vec<Diagnostic>> {
    let mut diagnostics = Vec::new();
    let mut items = Vec::with_capacity(file.items.len());

    for item in &file.items {
        let expanded = expand_item(&item.node, item.span, source_id, &mut diagnostics);
        items.push(Spanned::new(expanded, item.span));
    }

    if diagnostics.is_empty() {
        Ok(File { items })
    } else {
        Err(diagnostics)
    }
}

fn expand_item(
    item: &Item,
    span: Span,
    source_id: SourceId,
    diagnostics: &mut Vec<Diagnostic>,
) -> Item {
    match item {
        Item::Var(var) => Item::Var(expand_var(var, span, source_id, diagnostics)),
        Item::CodeBlock(block) => Item::CodeBlock(expand_code_block(block, source_id, diagnostics)),
        Item::Statement(stmt) => Item::Statement(expand_stmt(stmt, span, source_id, diagnostics)),
        Item::Segment(segment) => Item::Segment(segment.clone()),
        Item::DataBlock(block) => Item::DataBlock(block.clone()),
    }
}

fn expand_code_block(
    block: &CodeBlock,
    source_id: SourceId,
    diagnostics: &mut Vec<Diagnostic>,
) -> CodeBlock {
    let mut body = Vec::with_capacity(block.body.len());
    for stmt in &block.body {
        body.push(Spanned::new(
            expand_stmt(&stmt.node, stmt.span, source_id, diagnostics),
            stmt.span,
        ));
    }

    CodeBlock {
        name: block.name.clone(),
        name_span: block.name_span,
        kind: block.kind,
        is_far: block.is_far,
        is_naked: block.is_naked,
        is_inline: block.is_inline,
        body,
    }
}

fn expand_stmt(
    stmt: &Stmt,
    span: Span,
    source_id: SourceId,
    diagnostics: &mut Vec<Diagnostic>,
) -> Stmt {
    match stmt {
        Stmt::Segment(segment) => Stmt::Segment(segment.clone()),
        Stmt::Var(var) => Stmt::Var(expand_var(var, span, source_id, diagnostics)),
        Stmt::Instruction(instruction) => Stmt::Instruction(expand_instruction(
            instruction,
            span,
            source_id,
            diagnostics,
        )),
        Stmt::Bytes(values) => {
            let expanded = values
                .iter()
                .map(|expr| expand_expr(expr, span, source_id, diagnostics))
                .collect();
            Stmt::Bytes(expanded)
        }
        Stmt::DataBlock(block) => Stmt::DataBlock(block.clone()),
        Stmt::Address(value) => Stmt::Address(*value),
        Stmt::Label(label) => Stmt::Label(label.clone()),
        Stmt::Call(call) => Stmt::Call(call.clone()),
        Stmt::Empty => Stmt::Empty,
    }
}

fn expand_var(
    var: &VarDecl,
    span: Span,
    source_id: SourceId,
    diagnostics: &mut Vec<Diagnostic>,
) -> VarDecl {
    VarDecl {
        name: var.name.clone(),
        array_len: var
            .array_len
            .as_ref()
            .map(|expr| expand_expr(expr, span, source_id, diagnostics)),
        initializer: var
            .initializer
            .as_ref()
            .map(|expr| expand_expr(expr, span, source_id, diagnostics)),
    }
}

fn expand_instruction(
    instruction: &Instruction,
    span: Span,
    source_id: SourceId,
    diagnostics: &mut Vec<Diagnostic>,
) -> Instruction {
    let operand = instruction.operand.as_ref().map(|operand| match operand {
        Operand::Immediate(expr) => {
            Operand::Immediate(expand_expr(expr, span, source_id, diagnostics))
        }
        Operand::Value { expr, force_far } => Operand::Value {
            expr: expand_expr(expr, span, source_id, diagnostics),
            force_far: *force_far,
        },
    });

    Instruction {
        mnemonic: instruction.mnemonic.clone(),
        operand,
    }
}

fn expand_expr(
    expr: &Expr,
    span: Span,
    source_id: SourceId,
    diagnostics: &mut Vec<Diagnostic>,
) -> Expr {
    match expr {
        Expr::EvalText(input) => {
            let expanded = match k816_eval::expand(input) {
                Ok(expanded) => expanded,
                Err(err) => {
                    diagnostics.push(
                        Diagnostic::error(span, format!("eval expansion failed: {err}"))
                            .with_help("check the expression inside [...]"),
                    );
                    return Expr::Number(0);
                }
            };

            match parse_expression_fragment(source_id, &expanded) {
                Ok(expr) => expr.node,
                Err(err) => {
                    diagnostics.push(
                        Diagnostic::error(
                            span,
                            format!("eval expansion is not valid syntax: {}", err.message),
                        )
                        .with_label(span, format!("expanded text: '{expanded}'")),
                    );
                    Expr::Number(0)
                }
            }
        }
        Expr::Number(_) | Expr::Ident(_) => expr.clone(),
    }
}
