/*****************************************************************************/
/*                                                                           */
/*                                 parser.rs                                 */
/*                                                                           */
/*             CST/AST snapshot and recovery tests for the parser            */
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

//! Exact CST and AST snapshots for the vertical slice, plus error recovery
//! (master plan Phase 3).

use k816::ast::{
    AddressEncoding, ArithmeticOp, AstFile, BinaryOp, BranchConditionKind, CallArgument,
    ComparisonKind, ContractOutput, EvalExprKind, ExprKind, ExternKind, FlagKind, FlagTestSpelling,
    FunctionKind, HlaOperandKind, HlaStatement, IndexRegister, IndexRegisterKind, ItemKind,
    LabelScope, LoopCondition, LoopSpelling, MetadataQueryKind, NativeOperand, RegisterKind,
    ReturnKind, ShiftOp, StackOp, StackSpelling, StatementKind, StepOp, StorageClassKind,
    TestableFlag, TypeWidthKind, UnaryOp, WidthAnnotationKind,
};
use k816::diagnostic::{Applicability, DiagnosticCode};
use k816::parser::parse_source;
use k816::source::{Provenance, SourceMap, Span};

const HLA_SLICE: &str = "var SCREEN = $0400

func work @a8 @i16 {
    a = #0
    a = #1
    SCREEN = a
    return
}
";

const NATIVE_SLICE: &str = "var SCREEN = $0400

func work @a8 @i16 {
    sep #$20
    rep #$10
    lda #0
    lda #1
    sta SCREEN
    rts
}
";

/// One of each literal form the lexical layer accepts (sub-plan B group 1).
const LEXICAL: &str = "var BIN = %1010
var CH = '\\n'

func work @a8 {
    a = #0b0101
    a = #'A'
    lda #$FF
}
";

/// Renders the AST in a compact, exact form for snapshot comparison.
fn dump_ast(file: &AstFile) -> String {
    let mut out = String::new();
    for item in &file.items {
        match &item.kind {
            ItemKind::Var(var) => {
                out.push_str("var ");
                if let Some(storage) = var.storage {
                    out.push_str(match storage.kind {
                        StorageClassKind::DirectPage => "dp ",
                        StorageClassKind::Absolute => "abs ",
                        StorageClassKind::Far => "far ",
                    });
                }
                out.push_str(&var.name.text);
                if let Some(width) = var.width {
                    out.push_str(match width.kind {
                        TypeWidthKind::Byte => ":byte",
                        TypeWidthKind::Word => ":word",
                        TypeWidthKind::Far => ":far",
                    });
                }
                if let Some(size) = &var.size {
                    out.push_str(&format!("[{}]", dump_expr(&size.kind)));
                }
                if let Some(count) = &var.count {
                    out.push_str(&format!(" * {}", dump_expr(&count.kind)));
                }
                if let Some(address) = &var.address {
                    out.push_str(&format!(" = {}", dump_expr(&address.kind)));
                }
                out.push_str(&format!(" @{}..{}\n", item.span.start, item.span.end));
            }
            ItemKind::Const(group) => {
                out.push_str("const");
                for entry in &group.entries {
                    out.push_str(&format!(" {}", entry.name.text));
                    if let Some(value) = &entry.value {
                        out.push_str(&format!(" = {}", dump_expr(&value.kind)));
                    }
                }
                out.push_str(&format!(" @{}..{}\n", item.span.start, item.span.end));
            }
            ItemKind::ModuleWidth(width) => {
                out.push_str(&format!(
                    "module {} @{}..{}\n",
                    dump_width(width.kind),
                    item.span.start,
                    item.span.end
                ));
            }
            ItemKind::Data(data) => {
                out.push_str(&format!(
                    "data {} ({} entries) @{}..{}\n",
                    data.name.as_ref().map(|n| n.text.as_str()).unwrap_or("-"),
                    data.entries.len(),
                    item.span.start,
                    item.span.end
                ));
            }
            ItemKind::EvalBlock(expression) => {
                out.push_str(&format!(
                    "eval {} @{}..{}\n",
                    dump_expr(&expression.kind),
                    item.span.start,
                    item.span.end
                ));
            }
            ItemKind::EvalFunc(function) => {
                let parameters: Vec<&str> = function
                    .parameters
                    .iter()
                    .map(|parameter| parameter.text.as_str())
                    .collect();
                out.push_str(&format!(
                    "evalfunc {}({}) {} @{}..{}\n",
                    function.name.text,
                    parameters.join(" "),
                    dump_eval(&function.body.kind),
                    item.span.start,
                    item.span.end
                ));
            }
            ItemKind::Preprocessor(directive) => {
                out.push_str(&format!(
                    "preprocessor {:?} @{}..{}\n",
                    directive.kind, item.span.start, item.span.end
                ));
            }
            ItemKind::Binary(binary) => {
                out.push_str(&format!(
                    "binary {} = {:?} @{}..{}\n",
                    binary.name.text, binary.file, item.span.start, item.span.end
                ));
            }
            ItemKind::Reserved(reserved) => {
                out.push_str(&format!(
                    "reserved {:?} @{}..{}\n",
                    reserved.kind, item.span.start, item.span.end
                ));
            }
            ItemKind::Segment(name) => {
                out.push_str(&format!(
                    "segment {} @{}..{}\n",
                    name.text, item.span.start, item.span.end
                ));
            }
            ItemKind::Func(func) => {
                if func.is_far {
                    out.push_str("far ");
                }
                out.push_str(match func.kind {
                    FunctionKind::Func => "func ",
                    FunctionKind::Naked => "naked ",
                    FunctionKind::Inline => "inline ",
                    FunctionKind::Main => "main ",
                });
                out.push_str(&func.name.text);
                for annotation in &func.width {
                    out.push_str(&format!(" {}", dump_width(annotation.kind)));
                }
                if func.contract.is_some() {
                    out.push_str(" (contract)");
                }
                out.push_str(&format!(" @{}..{}\n", item.span.start, item.span.end));
                for statement in &func.body.statements {
                    let rendered = match &statement.kind {
                        StatementKind::Native(native) => {
                            let operand = match &native.operand {
                                Some(NativeOperand::Immediate(expr)) => {
                                    format!(" #{}", dump_expr(&expr.kind))
                                }
                                Some(NativeOperand::Register(register)) => {
                                    format!(" {}", dump_register(register.kind))
                                }
                                Some(NativeOperand::Address(address)) => format!(
                                    " {}{}{}{}",
                                    dump_encoding(address.encoding),
                                    if address.offset_marker.is_some() {
                                        "&"
                                    } else {
                                        ""
                                    },
                                    dump_expr(&address.value.kind),
                                    dump_index(address.index)
                                ),
                                Some(NativeOperand::Indirect(indirect)) => format!(
                                    " ({}{}{}){}",
                                    dump_encoding(indirect.encoding),
                                    dump_expr(&indirect.value.kind),
                                    dump_index(indirect.inner_index),
                                    dump_index(indirect.outer_index)
                                ),
                                Some(NativeOperand::LongIndirect(long)) => format!(
                                    " [{}{}]{}",
                                    dump_encoding(long.encoding),
                                    dump_expr(&long.value.kind),
                                    dump_index(long.outer_index)
                                ),
                                Some(NativeOperand::BlockMove(moved)) => format!(
                                    " {},{}",
                                    dump_expr(&moved.source.kind),
                                    dump_expr(&moved.destination.kind)
                                ),
                                None => String::new(),
                            };
                            let repeat = match &native.repeat {
                                Some(repeat) => format!(" * {}", dump_expr(&repeat.kind)),
                                None => String::new(),
                            };
                            format!("native {}{operand}{repeat}", native.mnemonic.text)
                        }
                        StatementKind::Hla(HlaStatement::Assign(assign)) => {
                            let mut chain = String::from("hla");
                            for target in &assign.targets {
                                chain.push_str(&format!(" {} =", dump_hla_operand(&target.kind)));
                            }
                            chain.push_str(&format!(" {}", dump_hla_operand(&assign.source.kind)));
                            chain
                        }
                        StatementKind::Hla(HlaStatement::Exchange(exchange)) => format!(
                            "hla {}~{}",
                            dump_register(exchange.left.kind),
                            dump_register(exchange.right.kind)
                        ),
                        StatementKind::Hla(HlaStatement::Arithmetic(arithmetic)) => format!(
                            "hla {}{}{}",
                            dump_register(arithmetic.register.kind),
                            match arithmetic.op {
                                ArithmeticOp::Add => "+",
                                ArithmeticOp::Subtract => "-",
                                ArithmeticOp::And => "&",
                                ArithmeticOp::Or => "|",
                                ArithmeticOp::Xor => "^",
                                ArithmeticOp::Compare => "?",
                                ArithmeticOp::BitTest => "&?",
                            },
                            dump_hla_operand(&arithmetic.operand.kind)
                        ),
                        StatementKind::Hla(HlaStatement::Shift(shift)) => format!(
                            "hla {}{}",
                            dump_hla_operand(&shift.target.kind),
                            match shift.op {
                                ShiftOp::Left => "<<",
                                ShiftOp::Right => ">>",
                                ShiftOp::RotateLeft => "<<<",
                                ShiftOp::RotateRight => ">>>",
                            }
                        ),
                        StatementKind::Hla(HlaStatement::Step(step)) => format!(
                            "hla {}{}",
                            dump_hla_operand(&step.target.kind),
                            match step.op {
                                StepOp::Increment => "++",
                                StepOp::Decrement => "--",
                            }
                        ),
                        StatementKind::Hla(HlaStatement::Flag(flag)) => format!(
                            "hla {}{}",
                            match flag.flag {
                                FlagKind::Carry => "c",
                                FlagKind::Decimal => "d",
                                FlagKind::Interrupt => "i",
                                FlagKind::Overflow => "v",
                            },
                            if flag.set { "+" } else { "-" }
                        ),
                        StatementKind::Hla(HlaStatement::Stack(stack)) => match stack.spelling {
                            StackSpelling::Shorthand => format!(
                                "hla {}{}",
                                dump_register(stack.register.kind),
                                match stack.op {
                                    StackOp::Push => "!!",
                                    StackOp::Pull => "??",
                                }
                            ),
                            StackSpelling::Word => format!(
                                "hla {} {}",
                                match stack.op {
                                    StackOp::Push => "push",
                                    StackOp::Pull => "pull",
                                },
                                dump_register(stack.register.kind)
                            ),
                        },
                        StatementKind::Hla(HlaStatement::PushConstant { value, .. }) => {
                            format!("hla push {}", dump_expr(&value.kind))
                        }
                        StatementKind::Hla(HlaStatement::Nop { count }) => match count {
                            Some(count) => format!("hla * {}", dump_expr(&count.kind)),
                            None => "hla *".to_owned(),
                        },
                        StatementKind::Hla(HlaStatement::Break) => "hla %".to_owned(),
                        StatementKind::Label(label) => format!(
                            "{} {}",
                            match label.scope {
                                LabelScope::Global => "label",
                                LabelScope::Local => "local label",
                            },
                            label.name.text
                        ),
                        StatementKind::Return(ReturnKind::Subroutine) => "return".to_owned(),
                        StatementKind::Return(ReturnKind::Interrupt) => "return_i".to_owned(),
                        StatementKind::WidthBlock(_) => "width block".to_owned(),
                        StatementKind::ModeSet(width) => format!(
                            "mode set {}",
                            width
                                .iter()
                                .map(|annotation| dump_width(annotation.kind))
                                .collect::<Vec<_>>()
                                .join(" ")
                        ),
                        StatementKind::Call(call) => {
                            format!(
                                "call {}{}",
                                if call.is_far { "far " } else { "" },
                                call.target.text
                            )
                        }
                        StatementKind::BareCall(call) => {
                            let mut rendered = format!("call {}", call.target.text);
                            if let Some(arguments) = &call.arguments {
                                let arguments: Vec<String> = arguments
                                    .iter()
                                    .map(|argument| match argument {
                                        CallArgument::Register(register) => {
                                            dump_register(register.kind).to_owned()
                                        }
                                        CallArgument::Immediate(value) => {
                                            format!("#{}", dump_expr(&value.kind))
                                        }
                                    })
                                    .collect();
                                rendered.push_str(&format!(" {}", arguments.join(", ")));
                            }
                            if let Some(outputs) = &call.outputs {
                                let outputs: Vec<String> =
                                    outputs.iter().map(dump_contract_output).collect();
                                rendered.push_str(&format!(" -> {}", outputs.join(", ")));
                            }
                            rendered
                        }
                        StatementKind::Conditional(conditional) => format!(
                            "if {} ({} statements){}",
                            dump_condition(conditional.condition.kind),
                            conditional.body.statements.len(),
                            match &conditional.otherwise {
                                Some(otherwise) =>
                                    format!(" else ({} statements)", otherwise.statements.len()),
                                None => String::new(),
                            }
                        ),
                        StatementKind::ConditionalJump(jump) => {
                            format!("{} jump", dump_condition(jump.condition.kind))
                        }
                        StatementKind::Loop(loop_statement) => format!(
                            "{}loop{} ({} statements) {}",
                            match loop_statement.spelling {
                                LoopSpelling::Postfix => "",
                                LoopSpelling::Keyword => "keyword ",
                            },
                            if loop_statement.nocross {
                                " nocross"
                            } else {
                                ""
                            },
                            loop_statement.body.statements.len(),
                            match loop_statement.condition {
                                LoopCondition::Always => "always".to_owned(),
                                LoopCondition::Never => "never".to_owned(),
                                LoopCondition::Branch(condition) => dump_condition(condition.kind),
                            }
                        ),
                        StatementKind::Goto(goto) => format!(
                            "goto {}{}{}",
                            if goto.is_far { "far " } else { "" },
                            if goto.indirect { "indirect " } else { "" },
                            dump_expr(&goto.target.kind)
                        ),
                        StatementKind::Break => "break".to_owned(),
                        StatementKind::Repeat => "repeat".to_owned(),
                        StatementKind::Nocross(body) => {
                            format!("nocross ({} statements)", body.statements.len())
                        }
                        StatementKind::Data(data) => {
                            format!("data ({} entries)", data.entries.len())
                        }
                        StatementKind::Preprocessor(directive) => {
                            format!("preprocessor {:?}", directive.kind)
                        }
                        StatementKind::Align(align) => match &align.offset {
                            Some(offset) => format!(
                                "align {} + {}",
                                dump_expr(&align.boundary.kind),
                                dump_expr(&offset.kind)
                            ),
                            None => format!("align {}", dump_expr(&align.boundary.kind)),
                        },
                        StatementKind::Address(address) => {
                            format!("address {}", dump_expr(&address.kind))
                        }
                        StatementKind::Reserved(reserved) => {
                            format!("reserved {:?}", reserved.kind)
                        }
                        StatementKind::Error => "error".to_owned(),
                    };
                    out.push_str(&format!(
                        "  {rendered} @{}..{}\n",
                        statement.span.start, statement.span.end
                    ));
                }
            }
            ItemKind::Import(import) => {
                out.push_str(&format!("import \"{}\"\n", import.file.path));
            }
            ItemKind::Extern(declaration) => {
                out.push_str(&format!(
                    "extern {}\n",
                    k816::ast::extern_name(&declaration.kind).text
                ));
            }
            ItemKind::Error => {
                out.push_str(&format!("error @{}..{}\n", item.span.start, item.span.end));
            }
        }
    }
    out
}

fn dump_condition(kind: BranchConditionKind) -> String {
    match kind {
        BranchConditionKind::Comparison(comparison) => match comparison {
            ComparisonKind::Less => "<",
            ComparisonKind::GreaterEqual => ">=",
            ComparisonKind::Equal => "==",
            ComparisonKind::NotEqual => "!=",
            ComparisonKind::Negative => "<0",
            ComparisonKind::NotNegative => ">=0",
            ComparisonKind::OverflowSet => "<<=",
            ComparisonKind::OverflowClear => ">>=",
            ComparisonKind::Greater => ">",
            ComparisonKind::LessEqual => "<=",
        }
        .to_owned(),
        BranchConditionKind::Flag {
            flag,
            set,
            spelling,
        } => {
            let letter = match flag {
                TestableFlag::Carry => "c",
                TestableFlag::Negative => "n",
                TestableFlag::Zero => "z",
                TestableFlag::Overflow => "v",
            };
            match (spelling, set) {
                (FlagTestSpelling::Signed, true) => format!("{letter}+?"),
                (FlagTestSpelling::Signed, false) => format!("{letter}-?"),
                (FlagTestSpelling::Bare, true) => format!("{letter}?"),
                (FlagTestSpelling::Bare, false) => format!("!{letter}?"),
                (FlagTestSpelling::Letter, true) => letter.to_owned(),
                (FlagTestSpelling::Letter, false) => format!("!{letter}"),
            }
        }
    }
}

fn dump_width(kind: WidthAnnotationKind) -> &'static str {
    match kind {
        WidthAnnotationKind::A8 => "@a8",
        WidthAnnotationKind::A16 => "@a16",
        WidthAnnotationKind::I8 => "@i8",
        WidthAnnotationKind::I16 => "@i16",
    }
}

/// A `->` entry: a register, or a flag under its own name.
fn dump_contract_output(output: &ContractOutput) -> String {
    match output {
        ContractOutput::Register(register) => dump_register(register.kind).to_owned(),
        ContractOutput::Flag(name) => name.text.to_ascii_lowercase(),
    }
}

fn dump_register(kind: RegisterKind) -> &'static str {
    match kind {
        RegisterKind::A => "a",
        RegisterKind::B => "b",
        RegisterKind::Dpr => "dpr",
        RegisterKind::Dbr => "dbr",
        RegisterKind::S => "s",
        RegisterKind::X => "x",
        RegisterKind::Y => "y",
        RegisterKind::P => "p",
    }
}

fn dump_hla_operand(kind: &HlaOperandKind) -> String {
    match kind {
        HlaOperandKind::Register(register) => dump_register(register.kind).to_owned(),
        HlaOperandKind::Immediate(value) => format!("#{}", dump_expr(&value.kind)),
        HlaOperandKind::Memory(memory) => format!(
            "{}{}{}",
            dump_encoding(memory.encoding),
            dump_expr(&memory.value.kind),
            dump_index(memory.index)
        ),
        HlaOperandKind::Indirect(indirect) => format!(
            "({}{}){}",
            dump_expr(&indirect.value.kind),
            dump_index(indirect.inner_index),
            dump_index(indirect.outer_index)
        ),
        HlaOperandKind::Error => "<error>".to_owned(),
    }
}

fn dump_encoding(encoding: Option<AddressEncoding>) -> String {
    match encoding.map(|encoding| encoding.kind) {
        Some(StorageClassKind::DirectPage) => "dp ".to_owned(),
        Some(StorageClassKind::Absolute) => "abs ".to_owned(),
        Some(StorageClassKind::Far) => "far ".to_owned(),
        None => String::new(),
    }
}

fn dump_index(index: Option<IndexRegister>) -> String {
    match index.map(|index| index.kind) {
        Some(IndexRegisterKind::X) => ",x".to_owned(),
        Some(IndexRegisterKind::Y) => ",y".to_owned(),
        Some(IndexRegisterKind::Stack) => ",s".to_owned(),
        None => String::new(),
    }
}

/// Renders an expression fully parenthesized in prefix form, so operator
/// precedence and associativity are visible in the snapshot.
fn dump_expr(kind: &ExprKind) -> String {
    match kind {
        ExprKind::Number(value) => format!("{value}"),
        ExprKind::Symbol(name) | ExprKind::LocalSymbol(name) => name.text.clone(),
        ExprKind::Unary(unary) => {
            let operator = match unary.op {
                UnaryOp::Negate => "-",
                UnaryOp::BitNot => "~",
                UnaryOp::LowByte => "&<",
                UnaryOp::HighByte => "&>",
                UnaryOp::Address16 => "&&",
                UnaryOp::Address24 => "&&&",
            };
            format!("({operator} {})", dump_expr(&unary.operand.kind))
        }
        ExprKind::Binary(binary) => {
            let operator = match binary.op {
                BinaryOp::Mul => "*",
                BinaryOp::Add => "+",
                BinaryOp::Sub => "-",
                BinaryOp::Shl => "<<",
                BinaryOp::Shr => ">>",
                BinaryOp::BitAnd => "&",
                BinaryOp::BitXor => "^",
                BinaryOp::BitOr => "|",
            };
            format!(
                "({operator} {} {})",
                dump_expr(&binary.left.kind),
                dump_expr(&binary.right.kind)
            )
        }
        ExprKind::Field(access) => {
            format!("({}{})", dump_expr(&access.base.kind), access.field.text)
        }
        ExprKind::Subscript(subscript) => format!(
            "({}[{}])",
            dump_expr(&subscript.base.kind),
            dump_expr(&subscript.index.kind)
        ),
        ExprKind::Metadata(query) => format!(
            "({}:{})",
            dump_expr(&query.base.kind),
            match query.query {
                MetadataQueryKind::SizeOf => "sizeof",
                MetadataQueryKind::OffsetOf => "offsetof",
            }
        ),
        ExprKind::TypedView(view) => format!(
            "({}:{})",
            dump_expr(&view.base.kind),
            match view.width.kind {
                TypeWidthKind::Byte => "byte",
                TypeWidthKind::Word => "word",
                TypeWidthKind::Far => "far",
            }
        ),
        ExprKind::Eval(inner) => format!("[{}]", dump_eval(&inner.kind)),
        ExprKind::Error => "<error>".to_owned(),
    }
}

/// Renders an evaluator expression in the same prefix form.
fn dump_eval(kind: &EvalExprKind) -> String {
    match kind {
        EvalExprKind::Number { value, .. } => format!("{value}"),
        EvalExprKind::Float(text) => text.clone(),
        EvalExprKind::String(text) => format!("{text:?}"),
        EvalExprKind::Symbol(name) => name.text.clone(),
        EvalExprKind::Call { name, arguments } => {
            let arguments: Vec<String> = arguments.iter().map(|a| dump_eval(&a.kind)).collect();
            format!("{}({})", name.text, arguments.join(" "))
        }
        EvalExprKind::Unary {
            operator,
            operand,
            is_suffix,
        } => {
            if *is_suffix {
                format!("({} {})", dump_eval(&operand.kind), operator)
            } else {
                format!("({operator} {})", dump_eval(&operand.kind))
            }
        }
        EvalExprKind::Binary {
            operator,
            left,
            right,
        } => format!(
            "({operator} {} {})",
            dump_eval(&left.kind),
            dump_eval(&right.kind)
        ),
        EvalExprKind::Ternary {
            condition,
            consequence,
            alternative,
        } => format!(
            "(?: {} {} {})",
            dump_eval(&condition.kind),
            dump_eval(&consequence.kind),
            dump_eval(&alternative.kind)
        ),
        EvalExprKind::Assignment {
            operator,
            target,
            value,
        } => format!(
            "({operator} {} {})",
            dump_eval(&target.kind),
            dump_eval(&value.kind)
        ),
        EvalExprKind::Subscript { base, indices } => {
            let indices: Vec<String> = indices.iter().map(|i| dump_eval(&i.kind)).collect();
            format!("({}[{}])", dump_eval(&base.kind), indices.join(" "))
        }
        EvalExprKind::Member { base, field } => {
            format!("({}.{})", dump_eval(&base.kind), field.text)
        }
        EvalExprKind::MethodCall {
            base,
            method,
            arguments,
        } => {
            let arguments: Vec<String> = arguments.iter().map(|a| dump_eval(&a.kind)).collect();
            format!(
                "({}.{}({}))",
                dump_eval(&base.kind),
                method.text,
                arguments.join(" ")
            )
        }
        EvalExprKind::List(items) => {
            let items: Vec<String> = items.iter().map(|i| dump_eval(&i.kind)).collect();
            format!("(, {})", items.join(" "))
        }
        EvalExprKind::Block(statements) => {
            let statements: Vec<String> = statements.iter().map(|s| dump_eval(&s.kind)).collect();
            format!("(do {})", statements.join(" "))
        }
        EvalExprKind::If {
            condition,
            consequence,
            alternative,
        } => match alternative {
            Some(alternative) => format!(
                "(if {} {} {})",
                dump_eval(&condition.kind),
                dump_eval(&consequence.kind),
                dump_eval(&alternative.kind)
            ),
            None => format!(
                "(if {} {})",
                dump_eval(&condition.kind),
                dump_eval(&consequence.kind)
            ),
        },
        EvalExprKind::While { condition, body } => format!(
            "(while {} {})",
            dump_eval(&condition.kind),
            dump_eval(&body.kind)
        ),
        EvalExprKind::Pi => "pi".to_owned(),
        EvalExprKind::ReservedCall(call) => {
            let arguments: Vec<String> =
                call.arguments.iter().map(|a| dump_eval(&a.kind)).collect();
            format!("reserved:{:?}({})", call.name, arguments.join(" "))
        }
        EvalExprKind::Error => "<error>".to_owned(),
    }
}

/// Parses `var X = <expression>` and renders the expression in prefix form.
fn parse_expr(expression: &str) -> String {
    let mut map = SourceMap::new();
    let id = map.add_file("expr.k65", format!("var XX = {expression}\n"));
    let result = parse_source(map.file(id), None);
    assert_eq!(
        result.diagnostics,
        vec![],
        "`{expression}` did not parse cleanly"
    );
    let ItemKind::Var(var) = &result.file.items[0].kind else {
        panic!("expected a var declaration");
    };
    let address = var.address.as_ref().expect("expected an initializer");
    dump_expr(&address.kind)
}

#[test]
fn hla_slice_cst_snapshot_is_exact() {
    let mut map = SourceMap::new();
    let id = map.add_file("slice.k816", HLA_SLICE);
    let result = parse_source(map.file(id), None);
    assert_eq!(result.diagnostics, vec![]);
    assert_eq!(
        result.tree.dump(),
        "\
source_file 0..91
  var_declaration 0..18
    entry: var_entry 4..18
      name: identifier 4..10
      address: number 13..18
  func_declaration 20..90
    name: identifier 25..29
    width: width_annotation 30..33
    width: width_annotation 34..38
    body: block 39..90
      hla_assignment 45..51
        operand: register 45..46
        operand: immediate 49..51
          value: number 50..51
      hla_assignment 56..62
        operand: register 56..57
        operand: immediate 60..62
          value: number 61..62
      hla_assignment 67..77
        operand: address 67..73
          value: identifier 67..73
        operand: register 76..77
      return_statement 82..88
"
    );
}

/// Decision 16, and the two halves of it that pull in opposite directions.
///
/// The CST cannot tell these apart — both are one `address` with an
/// `encoding` — so the split lives in the AST builder and is pinned here.
#[test]
fn a_statement_leading_prefix_is_a_call_unless_it_is_one_of_the_three() {
    let source = "var HMOVE = $0400\nvar target = $0400\n\
                  func f @a8 @i8 {\n  wsync HMOVE = a\n  abs target = a\n  \
                  wsync target++\n}\n";
    let mut map = SourceMap::new();
    let id = map.add_file("split.k65", source);
    let result = parse_source(map.file(id), None);
    assert_eq!(result.diagnostics, vec![]);

    let ItemKind::Func(function) = &result.file.items[2].kind else {
        panic!("expected the function");
    };
    let kinds: Vec<String> = function
        .body
        .statements
        .iter()
        .map(|statement| match &statement.kind {
            StatementKind::BareCall(call) => format!("call {}", call.target.text),
            StatementKind::Hla(HlaStatement::Assign(assign)) => {
                format!("store {:?}", encoding_of(&assign.targets[0]))
            }
            StatementKind::Hla(HlaStatement::Step(step)) => {
                format!("step {:?}", encoding_of(&step.target))
            }
            other => format!("{other:?}"),
        })
        .collect();
    // `wsync` is not a prefix, so it is a call and the store keeps no
    // encoding; `abs` is one, so it stays the store's encoding and no call is
    // invented.
    assert_eq!(
        kinds,
        [
            "call wsync",
            "store None",
            "store Some(Absolute)",
            "call wsync",
            "step None",
        ]
    );
}

/// A flag statement ends at its sign, and K65 is newline-insensitive, so a
/// flag name followed by `+` or `-` and then anything an expression can
/// continue with parses as one long store instead.
///
/// The CST cannot tell these apart either — the flag names stay ordinary
/// identifiers so that `var o` keeps being a declaration — so the split
/// lives in the AST builder beside decision 16's, and is pinned here.
/// `mem+1 = a` is the case it must *not* fire on: that is decision 17's
/// store, and `mem` is not a flag.
#[test]
fn a_flag_statement_is_peeled_off_the_store_that_swallowed_it() {
    let source = "var mem = $80
func f @a8 @i8 {
  i+ d-
  s = x = $FF
                    v-
  mem = a
  mem+1 = a
}
";
    let mut map = SourceMap::new();
    let id = map.add_file("flags.k65", source);
    let result = parse_source(map.file(id), None);
    assert_eq!(result.diagnostics, vec![]);

    let ItemKind::Func(function) = &result.file.items[1].kind else {
        panic!("expected the function");
    };
    let kinds: Vec<String> = function
        .body
        .statements
        .iter()
        .map(|statement| match &statement.kind {
            StatementKind::Hla(HlaStatement::Flag(flag)) => {
                format!("{:?}{}", flag.flag, if flag.set { "+" } else { "-" })
            }
            StatementKind::Hla(HlaStatement::Assign(assign)) => {
                format!("store {} targets", assign.targets.len())
            }
            other => format!("{other:?}"),
        })
        .collect();
    assert_eq!(
        kinds,
        [
            "Interrupt+",
            "Decimal-",
            "store 2 targets",
            "Overflow-",
            "store 1 targets",
            "store 1 targets",
        ]
    );
}

/// The encoding an HLA memory operand carries, if any.
fn encoding_of(operand: &k816::ast::HlaOperand) -> Option<k816::ast::StorageClassKind> {
    match &operand.kind {
        k816::ast::HlaOperandKind::Memory(address) => {
            address.encoding.as_ref().map(|encoding| encoding.kind)
        }
        _ => None,
    }
}

#[test]
fn hla_slice_ast_snapshot_is_exact() {
    let mut map = SourceMap::new();
    let id = map.add_file("slice.k816", HLA_SLICE);
    let result = parse_source(map.file(id), None);
    assert_eq!(result.diagnostics, vec![]);
    assert_eq!(
        dump_ast(&result.file),
        "\
var SCREEN = 1024 @0..18
func work @a8 @i16 @20..90
  hla a = #0 @45..51
  hla a = #1 @56..62
  hla SCREEN = a @67..77
  return @82..88
"
    );
}

/// K816's spellings of a loop, a flag test and the stack reach the AST as the
/// statements their K65 spellings are, each marked with how it was written —
/// the one fact a dialect needs to refuse the other (`plans/14-readable-hla.md`
/// decisions 4, 5, 9 and 13). A list is one statement per item, in order.
#[test]
fn the_readable_spellings_reach_the_ast_marked_with_how_they_were_written() {
    let source = "\
func work {
    loop { x-- } while !=
    { x-- } !=
    nocross loop { y++ } always
    loop { } never
    c? { x++ }
    !z? goto done
    c-? { x++ }
    push a, $0012, x
    pull x, a
    a!!
loop:
done:
}
";
    let mut map = SourceMap::new();
    let id = map.add_file("spellings.kk", source);
    let result = parse_source(map.file(id), None);
    assert_eq!(result.diagnostics, vec![]);
    let dump = dump_ast(&result.file);
    let lines: Vec<&str> = dump
        .lines()
        .skip(1)
        .map(|line| line.trim().split(" @").next().unwrap_or_default())
        .collect();
    assert_eq!(
        lines,
        [
            "keyword loop (1 statements) !=",
            "loop (1 statements) !=",
            "keyword loop nocross (1 statements) always",
            "keyword loop (0 statements) never",
            "if c? (1 statements)",
            "!z? jump",
            "if c-? (1 statements)",
            "hla push a",
            "hla push 18",
            "hla push x",
            "hla pull x",
            "hla pull a",
            "hla a!!",
            "label loop",
            "label done",
        ]
    );
}

#[test]
fn native_slice_ast_snapshot_is_exact() {
    let mut map = SourceMap::new();
    let id = map.add_file("native.k816", NATIVE_SLICE);
    let result = parse_source(map.file(id), None);
    assert_eq!(result.diagnostics, vec![]);
    assert_eq!(
        dump_ast(&result.file),
        "\
var SCREEN = 1024 @0..18
func work @a8 @i16 @20..113
  native sep #32 @45..53
  native rep #16 @58..66
  native lda #0 @71..77
  native lda #1 @82..88
  native sta SCREEN @93..103
  native rts @108..111
"
    );
}

/// The bare mode set is the statement `doc/register-width-aware-syntax.md`
/// uses in its label entry width example, and it did not parse before.
#[test]
fn a_bare_mode_set_is_a_statement() {
    let source = "func main {\n  @a16\n.loop:\n  lda #1\n  @a8 @i16\n  nop\n}\n";
    let mut map = SourceMap::new();
    let id = map.add_file("modes.k816", source);
    let result = parse_source(map.file(id), None);
    assert_eq!(result.diagnostics, vec![]);
    assert_eq!(
        dump_ast(&result.file),
        "\
func main @0..53
  mode set @a16 @14..18
  local label .loop @19..25
  native lda #1 @28..34
  mode set @a8 @i16 @37..45
  native nop @48..51
"
    );
    // The two annotations of one run are one statement, not two.
    let ItemKind::Func(func) = &result.file.items[0].kind else {
        panic!("expected a function");
    };
    let StatementKind::ModeSet(width) = &func.body.statements[3].kind else {
        panic!(
            "expected a mode set, got {:?}",
            func.body.statements[3].kind
        );
    };
    assert_eq!(
        width.iter().map(|w| w.kind).collect::<Vec<_>>(),
        [WidthAnnotationKind::A8, WidthAnnotationKind::I16]
    );
}

/// Decision 14: a `{` after annotations always opens a width block, so a
/// moded loop is written `@a16 { { ... } != }` and a mode set that precedes a
/// width block takes the `;` separator.
#[test]
fn a_brace_after_annotations_always_opens_a_width_block() {
    let source =
        "func main {\n  @a16 { lda #1 }\n  @a16 { { lda #1 } != }\n  @a8 ;\n  @a16 { nop }\n}\n";
    let mut map = SourceMap::new();
    let id = map.add_file("modes.k816", source);
    let result = parse_source(map.file(id), None);
    assert_eq!(result.diagnostics, vec![]);
    let ItemKind::Func(func) = &result.file.items[0].kind else {
        panic!("expected a function");
    };
    let kinds: Vec<&str> = func
        .body
        .statements
        .iter()
        .map(|statement| match &statement.kind {
            StatementKind::WidthBlock(_) => "width block",
            StatementKind::ModeSet(_) => "mode set",
            other => panic!("unexpected statement {other:?}"),
        })
        .collect();
    assert_eq!(
        kinds,
        ["width block", "width block", "mode set", "width block"]
    );
    // The middle block's body is the postfix loop, which is the reading the
    // mode-set spelling would otherwise have stolen.
    let StatementKind::WidthBlock(moded_loop) = &func.body.statements[1].kind else {
        unreachable!("checked above");
    };
    assert!(matches!(
        moded_loop.body.statements[0].kind,
        StatementKind::Loop(_)
    ));
}

/// Statement identity is what lets the width solver and lowering name the
/// same statement instead of re-deriving it by position.
#[test]
fn statements_are_numbered_in_source_order_including_nested_blocks() {
    let source = "\
func main {
  nop
  @a16 {
    nop
    nop
  }
  nop
}

func other {
  nop
}
";
    let mut map = SourceMap::new();
    let id = map.add_file("ids.k816", source);
    let result = parse_source(map.file(id), None);
    assert_eq!(result.diagnostics, vec![]);

    let mut seen: Vec<(u32, u32)> = Vec::new();
    for item in &result.file.items {
        let ItemKind::Func(func) = &item.kind else {
            continue;
        };
        collect_ids(&func.body, &mut seen);
    }
    // Pre-order: the width block is numbered before the statements inside it,
    // and the numbering is a single sequence across the whole file.
    assert_eq!(
        seen.iter().map(|(id, _)| *id).collect::<Vec<_>>(),
        [0, 1, 2, 3, 4, 5]
    );
    assert_eq!(
        seen.iter().map(|(_, start)| *start).collect::<Vec<_>>(),
        // `nop`, `@a16 {`, the two nested `nop`s, the trailing `nop`, and
        // `other`'s `nop`, in the order they appear in the source.
        vec![14, 20, 31, 39, 49, 71]
    );
}

fn collect_ids(block: &k816::ast::Block, out: &mut Vec<(u32, u32)>) {
    for statement in &block.statements {
        assert!(
            statement.id.is_assigned(),
            "statement at {} was never numbered",
            statement.span.start
        );
        out.push((statement.id.index(), statement.span.start));
        if let StatementKind::WidthBlock(width) = &statement.kind {
            collect_ids(&width.body, out);
        }
    }
}

#[test]
fn lexical_cst_snapshot_is_exact() {
    let mut map = SourceMap::new();
    let id = map.add_file("lexical.k65", LEXICAL);
    let result = parse_source(map.file(id), None);
    assert_eq!(result.diagnostics, vec![]);
    assert_eq!(
        result.tree.dump(),
        "\
source_file 0..91
  var_declaration 0..15
    entry: var_entry 4..15
      name: identifier 4..7
      address: number 10..15
  var_declaration 16..29
    entry: var_entry 20..29
      name: identifier 20..22
      address: char_literal 25..29
  func_declaration 31..90
    name: identifier 36..40
    width: width_annotation 41..44
    body: block 45..90
      hla_assignment 51..62
        operand: register 51..52
        operand: immediate 55..62
          value: number 56..62
      hla_assignment 67..75
        operand: register 67..68
        operand: immediate 71..75
          value: char_literal 72..75
      native_instruction 80..88
        mnemonic: mnemonic 80..83
        operand: immediate 84..88
          value: number 85..88
"
    );
}

#[test]
fn lexical_ast_snapshot_is_exact() {
    let mut map = SourceMap::new();
    let id = map.add_file("lexical.k65", LEXICAL);
    let result = parse_source(map.file(id), None);
    assert_eq!(result.diagnostics, vec![]);
    assert_eq!(
        dump_ast(&result.file),
        "\
var BIN = 10 @0..15
var CH = 10 @16..29
func work @a8 @31..90
  hla a = #5 @51..62
  hla a = #65 @67..75
  native lda #255 @80..88
"
    );
}

#[test]
fn a_literal_too_big_for_32_bits_is_refused_where_it_is_written() {
    // The range check belongs to the lexer's decode, so it is a *parse*
    // diagnostic: nothing later ever sees a value that does not fit, which is
    // why no analysis test can reach this code.
    for source in [
        "var xx = $1000000000\n",
        "var xx = 4294967296\n",
        "var xx = 0b100000000000000000000000000000000\n",
        "func f @a16 {\n    lda #$FFFFFFFFF\n}\n",
    ] {
        let mut map = SourceMap::new();
        let id = map.add_file("range.k65", source);
        let result = parse_source(map.file(id), None);
        let diagnostic = result
            .diagnostics
            .iter()
            .find(|diagnostic| diagnostic.code == DiagnosticCode::NUMBER_RANGE)
            .unwrap_or_else(|| {
                panic!(
                    "`{source}` should be out of range: {:?}",
                    result.diagnostics
                )
            });
        assert!(
            diagnostic.help.is_some(),
            "`{source}` explains the limit: {diagnostic:?}"
        );
    }

    // The boundary itself fits: 32 bits is the limit, not one less.
    let mut map = SourceMap::new();
    let id = map.add_file("range.k65", "var xx = $FFFFFFFF\n");
    let result = parse_source(map.file(id), None);
    assert_eq!(result.diagnostics, vec![]);
}

#[test]
fn every_literal_form_decodes_to_its_value() {
    let source = "var DEC = 42
var DOLLAR = $2A
var HEX = 0x2A
var HEX_UPPER = 0X2A
var PCT = %101010
var BIN = 0b101010
var BIN_UPPER = 0B101010
var LETTER = 'A'
var NEWLINE = '\\n'
var NUL = '\\0'
var TAB = '\\t'
var BACKSLASH = '\\\\'
var APOSTROPHE = '\\''
var HEX_ESCAPE = '\\x41'
";
    let mut map = SourceMap::new();
    let id = map.add_file("literals.k65", source);
    let result = parse_source(map.file(id), None);
    assert_eq!(result.diagnostics, vec![]);

    let values: Vec<u32> = result
        .file
        .items
        .iter()
        .map(|item| {
            let ItemKind::Var(var) = &item.kind else {
                panic!("expected var declaration");
            };
            let address = var.address.as_ref().expect("expected an initializer");
            match address.kind {
                ExprKind::Number(value) => value,
                _ => panic!("`{}` did not decode to a number", var.name.text),
            }
        })
        .collect();
    assert_eq!(
        values,
        vec![
            42, 42, 42, 42, // decimal and the three hexadecimal spellings
            42, 42, 42, // the three binary spellings
            0x41, 0x0A, 0x00, 0x09, 0x5C, 0x27, 0x41, // character literals
        ]
    );
}

#[test]
fn binary_operators_follow_c_precedence() {
    // The table in syntax-reference.md "Binary operators", including the three
    // worked examples stated there.
    assert_eq!(parse_expr("A | B & C"), "(| A (& B C))");
    assert_eq!(parse_expr("A + B & C"), "(& (+ A B) C)");
    assert_eq!(parse_expr("A << 1 | C"), "(| (<< A 1) C)");
    assert_eq!(parse_expr("A ^ B | C"), "(| (^ A B) C)");
    assert_eq!(parse_expr("A * B + C * D"), "(+ (* A B) (* C D))");
    assert_eq!(parse_expr("A + B - C"), "(- (+ A B) C)");
    assert_eq!(parse_expr("A >> B >> C"), "(>> (>> A B) C)");
    assert_eq!(parse_expr("(A | B) & C"), "(& (| A B) C)");
}

#[test]
fn unary_operators_repeat_in_the_order_written() {
    // "Both repeat in the order written" — syntax-reference.md "Unary
    // operators".
    assert_eq!(parse_expr("-~FOO"), "(- (~ FOO))");
    assert_eq!(parse_expr("~-FOO"), "(~ (- FOO))");
    assert_eq!(parse_expr("-FOO * 2"), "(* (- FOO) 2)");
}

#[test]
fn address_byte_operators_take_the_whole_offset_expression() {
    // `&<ptr+1` is the low byte of `ptr+1`, not `(&<ptr)+1` —
    // syntax-reference.md "Address Byte Operators".
    assert_eq!(parse_expr("&<ptr+1"), "(&< (+ ptr 1))");
    assert_eq!(parse_expr("&>screen_1+0x100"), "(&> (+ screen_1 256))");
    assert_eq!(parse_expr("&&ptr"), "(&& ptr)");
    assert_eq!(parse_expr("&&&ptr"), "(&&& ptr)");
    // ...but tighter than the bitwise operators, matching the `&&` gotcha.
    assert_eq!(parse_expr("&<ptr & $F0"), "(& (&< ptr) 240)");
}

#[test]
fn double_ampersand_is_address_of_not_logical_and() {
    // The documented quirk: `FOO && BAR` is `FOO & (&&BAR)`. One source
    // token, two operations — logical AND does not exist in this layer.
    assert_eq!(parse_expr("FOO && BAR"), "(& FOO (&& BAR))");
    assert_eq!(parse_expr("FOO & (&&BAR)"), "(& FOO (&& BAR))");
    assert_eq!(parse_expr("FOO && BAR + 1"), "(& FOO (&& (+ BAR 1)))");
}

#[test]
fn evaluator_brackets_are_expression_atoms() {
    assert_eq!(parse_expr("[$FF]"), "[255]");
    assert_eq!(parse_expr("[LV_SIZE]"), "[LV_SIZE]");
    assert_eq!(parse_expr("[FLAG_A | FLAG_B]"), "[(| FLAG_A FLAG_B)]");
    assert_eq!(parse_expr("BASE + [SIZE]"), "(+ BASE [SIZE])");
}

#[test]
fn grouping_keeps_the_span_of_the_parentheses() {
    let mut map = SourceMap::new();
    let id = map.add_file("paren.k65", "var XX = (A + B)\n");
    let result = parse_source(map.file(id), None);
    let ItemKind::Var(var) = &result.file.items[0].kind else {
        panic!("expected a var declaration");
    };
    let address = var.address.as_ref().expect("expected an initializer");
    assert_eq!(map.span_text(address.span), "(A + B)");
    let ExprKind::Binary(binary) = &address.kind else {
        panic!("expected the grouped binary expression");
    };
    assert_eq!(map.span_text(binary.op_span), "+");
    assert_eq!(map.span_text(binary.left.span), "A");
}

#[test]
fn a_compound_address_folds_through_the_const_table() {
    // The grouped-expression shape the previous test pins down has to reach
    // a value, not just an AST: this is that shape end to end.
    let mut map = SourceMap::new();
    let id = map.add_file("fold.k65", "const BASE = $0400\nvar XX = BASE + 3\n");
    let parsed = parse_source(map.file(id), None);
    assert_eq!(parsed.diagnostics, vec![]);

    let (model, diagnostics) = k816::semantic::analyze_file(&parsed.file);
    assert_eq!(diagnostics, vec![]);
    let x = model.symbols.lookup("XX").expect("XX is declared");
    assert_eq!(
        model.symbols.symbol(x).kind,
        k816::symbol::SymbolKind::Var {
            address: Some(0x0403),
            size: k816::symbol::AddressSize::Absolute,
            width: None,
            element: 1,
            repeat: None,
            layout: None,
        }
    );
}

#[test]
fn declaration_ast_snapshot_is_exact() {
    let source = "segment fixed_lo
var dp counter
var abs slot = $0012
var w:word = $2000
var buffer[32] = $0100
var simple:byte * 32
var allocated
const AA = 0, BB, CC

func draw {
    .loop:
    hello:
    .LT+1 = a
    return
}
";
    let mut map = SourceMap::new();
    let id = map.add_file("declarations.k65", source);
    let result = parse_source(map.file(id), None);
    assert_eq!(result.diagnostics, vec![]);
    assert_eq!(
        dump_ast(&result.file),
        "\
segment fixed_lo @0..16
var dp counter @17..31
var abs slot = 18 @32..52
var w:word = 8192 @53..71
var buffer[32] = 256 @72..94
var simple:byte * 32 @95..115
var allocated @116..129
const AA = 0 BB CC @130..150
func draw @152..212
  local label .loop @168..174
  label hello @179..185
  hla (+ .LT 1) = a @190..199
  return @204..210
"
    );
}

#[test]
fn a_var_prefix_that_is_not_a_storage_class_is_reported() {
    // `var foo bar` cannot be two names; the prefix slot is checked in the
    // builder so the message names the three storage classes.
    let mut map = SourceMap::new();
    let id = map.add_file("bad.k65", "var foo bar = 1\n");
    let result = parse_source(map.file(id), None);
    assert_eq!(result.diagnostics.len(), 1, "{:?}", result.diagnostics);
    assert_eq!(result.diagnostics[0].code, DiagnosticCode::SYNTAX_ERROR);
    assert_eq!(map.span_text(result.diagnostics[0].primary.span), "foo");
}

#[test]
fn semicolons_leave_no_trace_in_the_ast() {
    let mut map = SourceMap::new();
    let dense = map.add_file("dense.k65", "var AA = 0; var BB = 1;;\n;\n");
    let sparse = map.add_file("sparse.k65", "var AA = 0\nvar BB = 1\n");
    let dense = parse_source(map.file(dense), None);
    let sparse = parse_source(map.file(sparse), None);
    assert_eq!(dense.diagnostics, vec![]);
    assert_eq!(dense.file.items.len(), sparse.file.items.len());
}

#[test]
fn local_labels_keep_their_leading_dot() {
    let mut map = SourceMap::new();
    let id = map.add_file("labels.k65", "func f {\n  .loop:\n  .LT+1 = a\n}\n");
    let result = parse_source(map.file(id), None);
    let ItemKind::Func(func) = &result.file.items[0].kind else {
        panic!("expected a func");
    };
    let StatementKind::Label(label) = &func.body.statements[0].kind else {
        panic!("expected a label");
    };
    assert_eq!(label.scope, LabelScope::Local);
    assert_eq!(label.name.text, ".loop");
    assert_eq!(map.span_text(label.name.span), ".loop");

    let StatementKind::Hla(HlaStatement::Assign(assign)) = &func.body.statements[1].kind else {
        panic!("expected an assignment");
    };
    let HlaOperandKind::Memory(memory) = &assign.targets[0].kind else {
        panic!("expected a memory target");
    };
    let ExprKind::Binary(binary) = &memory.value.kind else {
        panic!("expected a label+offset target");
    };
    assert!(matches!(binary.left.kind, ExprKind::LocalSymbol(_)));
}

#[test]
fn every_declaration_form_compiles() {
    // Allocation, the three storage classes, the typed widths, both
    // multipliers, a field list, and the layout that has no storage: the
    // whole declaration family, with nothing left naming a later sub-plan.
    for source in [
        "var counter\n",
        "var dp counter\n",
        "var abs counter\n",
        "var far rom = $C00000\n",
        "var w:word = $2000\n",
        "var dp pp:far\n",
        "var buffer[32] = $100\n",
        "var ss:byte * 32\n",
        "var grid:word[4] * 8\n",
        "var vec[ .lo :byte .hi :byte ]\n",
        "var dp regs:word[ .a .b ] = $10\n",
        "abstract var POINT[ .x :byte .y :byte ]\n",
        "abstract var NEST:word[ .head .body[ .lo :byte .hi :byte ] ]\n",
    ] {
        let mut map = SourceMap::new();
        let id = map.add_file("owner.k65", source);
        let parsed = parse_source(map.file(id), None);
        assert_eq!(parsed.diagnostics, vec![], "`{source}` did not parse");
        let (_, diagnostics) = k816::semantic::analyze_file(&parsed.file);
        assert_eq!(diagnostics, vec![], "`{source}` should compile");
    }
}

#[test]
fn accessors_and_metadata_queries_bind_at_atom_level() {
    // `#TASKS:sizeof + 1` is `(TASKS:sizeof) + 1` — syntax-reference.md
    // "Metadata Query Suffixes".
    assert_eq!(parse_expr("TASKS.sp"), "(TASKS.sp)");
    assert_eq!(parse_expr("COMP[2]"), "(COMP[2])");
    assert_eq!(parse_expr("COMP[2].two"), "((COMP[2]).two)");
    assert_eq!(parse_expr("COMP[2].str[3]"), "(((COMP[2]).str)[3])");
    assert_eq!(parse_expr("TASKS:sizeof + 1"), "(+ (TASKS:sizeof) 1)");
    assert_eq!(
        parse_expr("REGS.data.lo:offsetof"),
        "(((REGS.data).lo):offsetof)"
    );
    assert_eq!(parse_expr("ptr:byte"), "(ptr:byte)");
}

#[test]
fn bracketed_field_selection_matches_dotted_selection() {
    // The two spellings are documented as equivalent, so they reach the AST
    // as one shape even though the CST keeps them apart.
    assert_eq!(parse_expr("COMP[2][.two]"), parse_expr("COMP[2].two"));
}

#[test]
fn a_field_name_must_touch_its_base() {
    // Otherwise `X = FOO` followed by the local label reference `.LT+1` would
    // read as a single field access across the statement boundary.
    let mut map = SourceMap::new();
    let id = map.add_file("adjacent.k65", "func f {\n  FOO = a\n  .LT+1 = a\n}\n");
    let result = parse_source(map.file(id), None);
    assert_eq!(result.diagnostics, vec![]);
    let ItemKind::Func(func) = &result.file.items[0].kind else {
        panic!("expected a func");
    };
    assert_eq!(func.body.statements.len(), 2);
}

#[test]
fn subscript_declarations_reach_the_ast() {
    let source = "abstract var POINT:byte[ .x .y ]
var REGS[
    .status :byte
    .data[
        .lo :byte
        .hi :byte
    ]
] = $D000
";
    let mut map = SourceMap::new();
    let id = map.add_file("fields.k65", source);
    let result = parse_source(map.file(id), None);
    assert_eq!(result.diagnostics, vec![]);

    let ItemKind::Var(point) = &result.file.items[0].kind else {
        panic!("expected a var");
    };
    assert!(point.is_abstract);
    assert_eq!(
        point.width.expect("default width").kind,
        TypeWidthKind::Byte
    );
    assert_eq!(point.fields.len(), 2);
    assert_eq!(point.fields[0].name.text, ".x");
    assert!(point.fields[0].width.is_none());

    let ItemKind::Var(regs) = &result.file.items[1].kind else {
        panic!("expected a var");
    };
    assert!(!regs.is_abstract);
    assert_eq!(regs.fields.len(), 2);
    assert_eq!(regs.fields[1].name.text, ".data");
    assert_eq!(regs.fields[1].fields.len(), 2);
    assert_eq!(regs.fields[1].fields[1].name.text, ".hi");
    assert_eq!(map.span_text(regs.fields[1].fields[1].span), ".hi :byte");
}

#[test]
fn every_subscript_form_compiles() {
    let declarations =
        "abstract var POINT[ .x :byte ]\nvar TASKS[ .sp :word .str :byte[4] ] * 2 = $4000\n";
    for statement in [
        "lda #TASKS:sizeof",
        "lda #TASKS.sp",
        "lda #POINT:sizeof",
        "lda #POINT.x:offsetof",
        "lda TASKS.str[3]",
        "lda TASKS[1].str[2]",
        "lda TASKS[1][.str][2]",
        "a = TASKS.str[0]",
    ] {
        let source = format!("{declarations}func f @a8 @i8 {{ {statement} }}\n");
        let mut map = SourceMap::new();
        let id = map.add_file("owner.k65", &source);
        let parsed = parse_source(map.file(id), None);
        assert_eq!(parsed.diagnostics, vec![], "`{statement}` did not parse");
        let (_, diagnostics) = k816::semantic::analyze_file(&parsed.file);
        assert_eq!(diagnostics, vec![], "`{statement}` should compile");
    }
}

#[test]
fn classical_operand_shapes_reach_the_ast() {
    let source = "func main {
    nop
    asl a
    lda #1
    lda addr
    lda addr,x
    sta addr,y
    lda (ptr,x)
    lda (ptr),y
    jmp (vector)
    lda dp addr
    lda abs addr,x
    lda far base+3
    inx * 3
}
";
    let mut map = SourceMap::new();
    let id = map.add_file("native.k65", source);
    let result = parse_source(map.file(id), None);
    assert_eq!(result.diagnostics, vec![]);

    let ItemKind::Func(func) = &result.file.items[0].kind else {
        panic!("expected a func");
    };
    let rendered: Vec<String> = func
        .body
        .statements
        .iter()
        .map(|statement| {
            let StatementKind::Native(native) = &statement.kind else {
                panic!("expected a native instruction");
            };
            let operand = match &native.operand {
                Some(NativeOperand::Immediate(expr)) => format!(" #{}", dump_expr(&expr.kind)),
                Some(NativeOperand::Register(register)) => {
                    format!(" {}", dump_register(register.kind))
                }
                Some(NativeOperand::Address(address)) => format!(
                    " {}{}{}{}",
                    dump_encoding(address.encoding),
                    if address.offset_marker.is_some() {
                        "&"
                    } else {
                        ""
                    },
                    dump_expr(&address.value.kind),
                    dump_index(address.index)
                ),
                Some(NativeOperand::Indirect(indirect)) => format!(
                    " ({}{}{}){}",
                    dump_encoding(indirect.encoding),
                    dump_expr(&indirect.value.kind),
                    dump_index(indirect.inner_index),
                    dump_index(indirect.outer_index)
                ),
                Some(NativeOperand::LongIndirect(long)) => format!(
                    " [{}{}]{}",
                    dump_encoding(long.encoding),
                    dump_expr(&long.value.kind),
                    dump_index(long.outer_index)
                ),
                Some(NativeOperand::BlockMove(moved)) => format!(
                    " {},{}",
                    dump_expr(&moved.source.kind),
                    dump_expr(&moved.destination.kind)
                ),
                None => String::new(),
            };
            let repeat = match &native.repeat {
                Some(repeat) => format!(" * {}", dump_expr(&repeat.kind)),
                None => String::new(),
            };
            format!("{}{operand}{repeat}", native.mnemonic.text)
        })
        .collect();
    assert_eq!(
        rendered,
        vec![
            "nop",
            "asl a",
            "lda #1",
            "lda addr",
            "lda addr,x",
            "sta addr,y",
            "lda (ptr,x)",
            "lda (ptr),y",
            "jmp (vector)",
            "lda dp addr",
            "lda abs addr,x",
            "lda far (+ base 3)",
            "inx * 3",
        ]
    );
}

#[test]
fn an_operand_does_not_swallow_the_next_statement() {
    // `lda foo` then `bar = a` could also read as the direct-page prefix form
    // `lda foo bar` followed by `= a`; only the first goes on to parse.
    let mut map = SourceMap::new();
    let id = map.add_file("boundary.k65", "func f {\n  lda foo\n  bar = a\n}\n");
    let result = parse_source(map.file(id), None);
    assert_eq!(result.diagnostics, vec![]);
    let ItemKind::Func(func) = &result.file.items[0].kind else {
        panic!("expected a func");
    };
    assert_eq!(func.body.statements.len(), 2);
    let StatementKind::Native(native) = &func.body.statements[0].kind else {
        panic!("expected a native instruction");
    };
    let Some(NativeOperand::Address(address)) = &native.operand else {
        panic!("expected an address operand");
    };
    assert!(address.encoding.is_none());
    assert!(matches!(
        func.body.statements[1].kind,
        StatementKind::Hla(HlaStatement::Assign(_))
    ));
}

#[test]
fn every_operandless_mnemonic_the_grammar_accepts_is_in_the_isa_table() {
    // The grammar produces only real 65816 mnemonics, and the table now
    // carries all 92 of them — so an operandless instruction analyzes clean
    // rather than naming a sub-plan.
    for mnemonic in ["inx", "nop", "pha", "xce", "tcd", "rtl", "wai"] {
        let mut map = SourceMap::new();
        let id = map.add_file("op.k65", format!("func main @a8 {{\n    {mnemonic}\n}}\n"));
        let parsed = parse_source(map.file(id), None);
        assert_eq!(parsed.diagnostics, vec![], "`{mnemonic}` did not parse");
        let (_, diagnostics) = k816::semantic::analyze_file(&parsed.file);
        // `xce` and `rtl` leave widths a caller cannot rely on, which is
        // worth a warning and is not what this test is about.
        let errors: Vec<&k816::diagnostic::Diagnostic> =
            diagnostics.iter().filter(|d| d.is_error()).collect();
        assert_eq!(
            errors,
            Vec::<&k816::diagnostic::Diagnostic>::new(),
            "`{mnemonic}`"
        );
    }
}

#[test]
fn instruction_repetition_reaches_the_ast_and_compiles() {
    // `<mnemonic> * N` is a shorthand, not an operand shape, so the count
    // lands beside the instruction rather than inside it.
    let mut map = SourceMap::new();
    let id = map.add_file("repeat.k65", "func main @a8 { inx * 2 }\n");
    let parsed = parse_source(map.file(id), None);
    assert_eq!(parsed.diagnostics, vec![]);
    let ItemKind::Func(func) = &parsed.file.items[0].kind else {
        panic!("expected a func");
    };
    let StatementKind::Native(native) = &func.body.statements[0].kind else {
        panic!("expected a native instruction");
    };
    assert!(native.operand.is_none(), "the repeat is not an operand");
    assert!(native.repeat.is_some());

    let (_, diagnostics) = k816::semantic::analyze_file(&parsed.file);
    assert_eq!(diagnostics, vec![], "{diagnostics:?}");
}

#[test]
fn native_65816_operand_shapes_reach_the_ast() {
    let source = "func main {
    lda 4,s
    lda (4,s),y
    lda [ptr]
    lda [ptr],y
    lda &FIELD_HP, x
    lda abs &FIELD_HP, x
    mvn $01,$02
    xba
}
";
    let mut map = SourceMap::new();
    let id = map.add_file("native816.k65", source);
    let result = parse_source(map.file(id), None);
    assert_eq!(result.diagnostics, vec![]);

    let ItemKind::Func(func) = &result.file.items[0].kind else {
        panic!("expected a func");
    };
    let rendered: Vec<String> = func
        .body
        .statements
        .iter()
        .map(|statement| {
            let StatementKind::Native(native) = &statement.kind else {
                panic!("expected a native instruction");
            };
            let operand = match &native.operand {
                Some(NativeOperand::Address(address)) => format!(
                    " {}{}{}{}",
                    dump_encoding(address.encoding),
                    if address.offset_marker.is_some() {
                        "&"
                    } else {
                        ""
                    },
                    dump_expr(&address.value.kind),
                    dump_index(address.index)
                ),
                Some(NativeOperand::Indirect(indirect)) => format!(
                    " ({}{}){}",
                    dump_expr(&indirect.value.kind),
                    dump_index(indirect.inner_index),
                    dump_index(indirect.outer_index)
                ),
                Some(NativeOperand::LongIndirect(long)) => format!(
                    " [{}]{}",
                    dump_expr(&long.value.kind),
                    dump_index(long.outer_index)
                ),
                Some(NativeOperand::BlockMove(moved)) => format!(
                    " {},{}",
                    dump_expr(&moved.source.kind),
                    dump_expr(&moved.destination.kind)
                ),
                _ => String::new(),
            };
            format!("{}{operand}", native.mnemonic.text)
        })
        .collect();
    assert_eq!(
        rendered,
        vec![
            "lda 4,s",
            "lda (4,s),y",
            "lda [ptr]",
            "lda [ptr],y",
            "lda &FIELD_HP,x",
            "lda abs &FIELD_HP,x",
            "mvn 1,2",
            "xba",
        ]
    );
}

#[test]
fn brackets_after_a_mnemonic_are_indirection_not_evaluation() {
    // `lda [ptr]` dereferences; the same brackets in an HLA assignment are
    // the compile-time evaluator's, which is why the operand form is
    // raw-mnemonic-only.
    let mut map = SourceMap::new();
    let id = map.add_file("brackets.k65", "func f {\n  lda [ptr]\n  lda #[SIZE]\n}\n");
    let result = parse_source(map.file(id), None);
    assert_eq!(result.diagnostics, vec![]);
    let ItemKind::Func(func) = &result.file.items[0].kind else {
        panic!("expected a func");
    };
    let StatementKind::Native(long) = &func.body.statements[0].kind else {
        panic!("expected a native instruction");
    };
    assert!(matches!(long.operand, Some(NativeOperand::LongIndirect(_))));
    let StatementKind::Native(immediate) = &func.body.statements[1].kind else {
        panic!("expected a native instruction");
    };
    let Some(NativeOperand::Immediate(value)) = &immediate.operand else {
        panic!("expected an immediate");
    };
    assert!(matches!(value.kind, ExprKind::Eval(_)));
}

/// Parses one HLA statement and renders it in the `dump_ast` chain form.
fn parse_hla(statement: &str) -> String {
    let mut map = SourceMap::new();
    let id = map.add_file("hla.k65", format!("func f {{\n  {statement}\n}}\n"));
    let result = parse_source(map.file(id), None);
    assert_eq!(result.diagnostics, vec![], "`{statement}` did not parse");
    let ItemKind::Func(func) = &result.file.items[0].kind else {
        panic!("expected a func");
    };
    assert_eq!(func.body.statements.len(), 1, "`{statement}`");
    match &func.body.statements[0].kind {
        StatementKind::Hla(HlaStatement::Assign(assign)) => {
            let mut chain = String::new();
            for target in &assign.targets {
                chain.push_str(&format!("{} = ", dump_hla_operand(&target.kind)));
            }
            chain.push_str(&dump_hla_operand(&assign.source.kind));
            chain
        }
        StatementKind::Hla(HlaStatement::Exchange(exchange)) => format!(
            "{}~{}",
            dump_register(exchange.left.kind),
            dump_register(exchange.right.kind)
        ),
        StatementKind::Hla(_) => {
            // Every other HLA shorthand renders through `dump_ast`.
            let rendered = dump_ast(&result.file);
            rendered
                .lines()
                .nth(1)
                .expect("a statement line")
                .trim()
                .split(" @")
                .next()
                .expect("a rendered statement")
                .trim_start_matches("hla ")
                .to_owned()
        }
        other => panic!("`{statement}` is not an HLA statement: {other:?}"),
    }
}

#[test]
fn every_movement_shape_reaches_the_ast() {
    assert_eq!(parse_hla("a = #1"), "a = #1");
    assert_eq!(parse_hla("a = mem,x"), "a = mem,x");
    assert_eq!(parse_hla("a = (mem),y"), "a = (mem),y");
    assert_eq!(parse_hla("mem = a"), "mem = a");
    assert_eq!(parse_hla("mem,x = a"), "mem,x = a");
    assert_eq!(parse_hla("(mem),y = a"), "(mem),y = a");
    assert_eq!(parse_hla("mem = x"), "mem = x");
    assert_eq!(parse_hla("mem = 0"), "mem = 0");
    assert_eq!(parse_hla("b~a"), "b~a");
    assert_eq!(parse_hla("a~b"), "a~b");
}

#[test]
fn a_chain_flows_from_its_rightmost_operand() {
    // "The value flows from the rightmost source through each target
    // left-to-right" — syntax-reference.md "Assignment Chaining".
    assert_eq!(parse_hla("dst0 = dst1 = a = src"), "dst0 = dst1 = a = src");
    assert_eq!(parse_hla("x = a = dst0"), "x = a = dst0");
    assert_eq!(parse_hla("s = x = y"), "s = x = y");
    assert_eq!(
        parse_hla("UART_DATA = a = text,x"),
        "UART_DATA = a = text,x"
    );

    let mut map = SourceMap::new();
    let id = map.add_file("chain.k65", "func f {\n  dst0 = dst1 = a = src\n}\n");
    let result = parse_source(map.file(id), None);
    let ItemKind::Func(func) = &result.file.items[0].kind else {
        panic!("expected a func");
    };
    let StatementKind::Hla(HlaStatement::Assign(assign)) = &func.body.statements[0].kind else {
        panic!("expected an assignment");
    };
    assert_eq!(assign.targets.len(), 3);
    assert_eq!(map.span_text(assign.source.span), "src");
    assert_eq!(map.span_text(assign.targets[0].span), "dst0");
}

#[test]
fn registers_are_case_insensitive() {
    assert_eq!(parse_hla("A = X"), "a = x");
    assert_eq!(parse_hla("DPR = A"), "dpr = a");
    assert_eq!(parse_hla("DBR!!"), "dbr!!");
    assert_eq!(parse_hla("S = X = Y"), "s = x = y");
}

#[test]
fn the_slice_chain_shapes_still_compile_beside_the_family_that_grew_around_them() {
    // The two shapes sub-plan A compiles keep their semantics through the
    // generalization, and sub-plans E2 and E3 opened the rest of the family
    // around them: chains, transfers, and every shorthand now compile where
    // they used to name an owner.
    let mut map = SourceMap::new();
    let slice = map.add_file("slice.k816", HLA_SLICE);
    let parsed = parse_source(map.file(slice), None);
    let (_, diagnostics) = k816::semantic::analyze_file(&parsed.file);
    assert_eq!(diagnostics, vec![], "the slice must stay clean");

    for source in ["x = a\n", "mem,x = a\n", "a = mem\n", "b~a\n", "a+mem\n"] {
        let mut map = SourceMap::new();
        let id = map.add_file(
            "chain.k65",
            format!("var mem = $0400\nfunc main @a8 {{\n  {source}}}\n"),
        );
        let parsed = parse_source(map.file(id), None);
        assert_eq!(parsed.diagnostics, vec![], "`{source}` did not parse");
        let (_, diagnostics) = k816::semantic::analyze_file(&parsed.file);
        assert_eq!(diagnostics, vec![], "`{source}`: {diagnostics:?}");
    }
}

#[test]
fn every_shorthand_reaches_the_ast() {
    assert_eq!(parse_hla("a+mem"), "a+mem");
    assert_eq!(parse_hla("a-#1"), "a-#1");
    assert_eq!(parse_hla("a&mask"), "a&mask");
    assert_eq!(parse_hla("a|bits"), "a|bits");
    assert_eq!(parse_hla("a^bits"), "a^bits");
    assert_eq!(parse_hla("a?10"), "a?10");
    assert_eq!(parse_hla("x?#1"), "x?#1");
    assert_eq!(parse_hla("a&?READY"), "a&?READY");
    assert_eq!(parse_hla("a<<"), "a<<");
    assert_eq!(parse_hla("mem,x>>>"), "mem,x>>>");
    assert_eq!(parse_hla("x++"), "x++");
    assert_eq!(parse_hla("mem,x--"), "mem,x--");
    assert_eq!(parse_hla("c+"), "c+");
    assert_eq!(parse_hla("v-"), "v-");
    assert_eq!(parse_hla("a!!"), "a!!");
    assert_eq!(parse_hla("dpr??"), "dpr??");
    assert_eq!(parse_hla("dbr!!"), "dbr!!");
    assert_eq!(parse_hla("*"), "*");
    assert_eq!(parse_hla("* 5"), "* 5");
    assert_eq!(parse_hla("%"), "%");
}

#[test]
fn a_shift_statement_ends_at_its_operator() {
    // `mem<<` and `mem,x>>>` on consecutive lines are two shifts, not the
    // single shift `(mem << mem,x) >>>` — the shift target accepts atoms
    // only, so the operator cannot be swallowed into an operand.
    let mut map = SourceMap::new();
    let id = map.add_file(
        "shifts.k65",
        "func f {\n  mem<<\n  mem,x>>>\n  * 5\n  * COUNT\n}\n",
    );
    let result = parse_source(map.file(id), None);
    assert_eq!(result.diagnostics, vec![]);
    let ItemKind::Func(func) = &result.file.items[0].kind else {
        panic!("expected a func");
    };
    assert_eq!(func.body.statements.len(), 4);
}

#[test]
fn a_flag_letter_is_only_ever_a_flag() {
    // `c` and `d` named a register *and* a flag until decisions 29 and 30,
    // and only the token after the operator said which — the ambiguity that
    // made `i+ d-` parse as the arithmetic `i + d` (`postmortem.md` §G.1.16).
    // Both are flags now and nothing else.
    assert_eq!(parse_hla("c+"), "c+");
    assert_eq!(parse_hla("d-"), "d-");
    assert_eq!(parse_hla("dpr = a"), "dpr = a");
}

/// `c+mem` is `SEC` and then a call, which is what the original does too.
///
/// `Flag` is a terminal class distinct from `Ident` there
/// (`compiler.inc:253-260`), so `Instr -> Flag +` ends at the sign and `mem`
/// begins a new statement. K816 read it as one arithmetic statement for as
/// long as `c` was also a register; now it reads it the way K65 always did.
#[test]
fn a_flag_set_ends_at_its_sign() {
    let mut map = SourceMap::new();
    let id = map.add_file("flag.k65", "var mem = $80\nfunc f @a8 @i8 {\n  c+mem\n}\n");
    let result = parse_source(map.file(id), None);
    assert_eq!(result.diagnostics, vec![]);
    let ItemKind::Func(func) = &result.file.items[1].kind else {
        panic!("expected a func");
    };
    assert_eq!(func.body.statements.len(), 2, "`c+mem` is two statements");
    assert!(matches!(
        func.body.statements[0].kind,
        StatementKind::Hla(HlaStatement::Flag(_))
    ));
}

#[test]
fn a_flag_without_a_shorthand_is_reported() {
    // `a` is a register but not a flag with a set/clear form.
    let mut map = SourceMap::new();
    let id = map.add_file("flag.k65", "func f {\n  s+\n}\n");
    let result = parse_source(map.file(id), None);
    assert_eq!(result.diagnostics.len(), 1, "{:?}", result.diagnostics);
    assert_eq!(result.diagnostics[0].code, DiagnosticCode::SYNTAX_ERROR);
}

#[test]
fn a_binary_literal_is_not_the_break_shorthand() {
    assert_eq!(parse_hla("x = %0"), "x = 0");
    assert_eq!(parse_hla("%"), "%");
}

#[test]
fn every_code_section_form_reaches_the_ast() {
    let source = "func inc_x { }
naked dli { }
inline inc_y { }
far func long_range_sub { }
main { }
func add @a16 (a, x) -> @a8 a, y { }
";
    let mut map = SourceMap::new();
    let id = map.add_file("sections.k65", source);
    let result = parse_source(map.file(id), None);
    assert_eq!(result.diagnostics, vec![]);
    assert_eq!(
        dump_ast(&result.file),
        "\
func inc_x @0..14
naked dli @15..28
inline inc_y @29..45
far func long_range_sub @46..73
main main @74..82
func add @a16 (contract) @83..119
"
    );

    let ItemKind::Func(add) = &result.file.items[5].kind else {
        panic!("expected a func");
    };
    let contract = add.contract.as_ref().expect("a contract");
    let inputs = contract.inputs.as_ref().expect("an input list");
    assert_eq!(inputs.len(), 2);
    let outputs = contract.outputs.as_ref().expect("an output clause");
    assert_eq!(outputs.width.len(), 1);
    assert_eq!(outputs.outputs.len(), 2);
}

#[test]
fn an_operand_prefix_is_not_a_far_call() {
    // `far` is an aliased identifier, not a keyword, so `lda far addr` stays
    // an AbsoluteLong load rather than `lda` followed by a far call.
    let mut map = SourceMap::new();
    let id = map.add_file(
        "far.k65",
        "func main {\n  lda far addr\n  call far lib_init\n}\n",
    );
    let result = parse_source(map.file(id), None);
    assert_eq!(result.diagnostics, vec![]);
    let ItemKind::Func(func) = &result.file.items[0].kind else {
        panic!("expected a func");
    };
    let StatementKind::Native(native) = &func.body.statements[0].kind else {
        panic!("expected a native instruction");
    };
    let Some(NativeOperand::Address(address)) = &native.operand else {
        panic!("expected an address operand");
    };
    assert_eq!(
        address.encoding.expect("an encoding prefix").kind,
        StorageClassKind::Far
    );
    let StatementKind::Call(call) = &func.body.statements[1].kind else {
        panic!("expected a call");
    };
    assert!(call.is_far);
}

#[test]
fn every_section_form_and_contract_compiles() {
    // E5 landed the far family and E6 `naked`, `inline`, and `return_i`, so
    // no section kind names an owner any more. This test was
    // `section_forms_and_contracts_name_their_owner`; what is left of it is
    // the assertion that each form reaches analysis with nothing to say.
    for source in [
        "far func other @a8 { }\nfunc work @a8 { far other }\n",
        "far func other @a8 { }\nfunc work @a8 { call far other }\n",
        "naked f @a8 @i8 { }\n",
        "inline f @a8 @i8 { }\nfunc work @a8 @i8 { f }\n",
        "inline scale @a16 (a, #factor) -> a { lda #factor }\n\
         func work @a16 @i8 { scale a, #16 -> a }\n",
    ] {
        let (_, diagnostics) = analyze_section(source);
        assert_eq!(diagnostics, vec![], "`{source}`");
    }

    // `return_i` is `rti`, which restores `P` — and with it both width flags
    // — so control leaves at exactly the state that was pushed. From the
    // interrupted code's point of view the handler preserved its widths,
    // which is the idiom rather than a hazard, and the same answer the native
    // `rti` spelling gets.
    for source in [
        "naked handler @a8 @i8 { return_i }\n",
        "naked handler @a8 @i8 { rti }\n",
        // Even when the body changes a width: what leaves is what was
        // pushed, not what the body last set.
        "naked handler @a8 @i8 { rep #$20 return_i }\n",
    ] {
        let (_, diagnostics) = analyze_section(source);
        assert_eq!(diagnostics, vec![], "`{source}`");
    }
}

/// Parses and analyzes one section snippet, asserting only that it parses.
fn analyze_section(
    source: &str,
) -> (
    k816::semantic::SemanticModel,
    Vec<k816::diagnostic::Diagnostic>,
) {
    let mut map = SourceMap::new();
    let id = map.add_file("section.k65", source);
    let parsed = parse_source(map.file(id), None);
    assert_eq!(parsed.diagnostics, vec![], "`{source}` did not parse");
    k816::semantic::analyze_file(&parsed.file)
}

#[test]
fn control_flow_ast_snapshot_is_exact() {
    let source = "func main @a8 {
    >={ x++ }
    < goto done
    != repeat
    { x++ } !=
    { } always
    =={ a = #1 } else { a = #2 }
    goto (vec)
    nocross { y++ } <0
}
";
    let mut map = SourceMap::new();
    let id = map.add_file("control.k65", source);
    let result = parse_source(map.file(id), None);
    assert_eq!(result.diagnostics, vec![]);
    assert_eq!(
        dump_ast(&result.file),
        "\
func main @a8 @0..162
  if >= (1 statements) @20..29
  < jump @34..45
  != jump @50..59
  loop (1 statements) != @64..74
  loop (0 statements) always @79..89
  if == (1 statements) else (1 statements) @94..122
  goto indirect vec @127..137
  loop nocross (1 statements) <0 @142..160
"
    );
}

#[test]
fn a_condition_after_a_block_closes_that_block() {
    // The documented hazard: `{ x++ } !=` is a loop, and `!= { y++ }` is a
    // conditional. Consecutive blocks must not let the postfix condition of
    // one become the prefix condition of the next.
    let mut map = SourceMap::new();
    let id = map.add_file(
        "postfix.k65",
        "func f {\n  { x++ } !=\n  { y++ } ==\n  { } always\n}\n",
    );
    let result = parse_source(map.file(id), None);
    assert_eq!(result.diagnostics, vec![]);
    let ItemKind::Func(func) = &result.file.items[0].kind else {
        panic!("expected a func");
    };
    assert_eq!(func.body.statements.len(), 3);
    for statement in &func.body.statements {
        assert!(
            matches!(statement.kind, StatementKind::Loop(_)),
            "expected a loop, got {:?}",
            statement.kind
        );
    }
}

#[test]
fn control_flow_statements_compile_or_name_their_owner() {
    // E4 opened the branch family and E5 the jump family, so nothing in
    // control flow names an owner any more. `break` outside a loop and a
    // `goto` naming nothing are rejections of their own rather than
    // unimplemented constructs.
    for source in [
        "func main @a8 { >={ } }\n",
        "func main @a8 { { } always }\n",
        "func main @a8 { nocross { } }\n",
        "func main @a8 { { break } always }\n",
        "func main @a8 { { < repeat } always }\n",
        "func main @a8 { < goto done\ndone: }\n",
        "func main @a8 { goto done\ndone: }\n",
    ] {
        let (_, diagnostics) = analyze_control(source);
        assert_eq!(diagnostics, vec![], "`{source}`");
    }

    for source in [
        "func main @a8 { < goto done }\n",
        "func main @a8 { goto done }\n",
    ] {
        let (_, diagnostics) = analyze_control(source);
        assert_eq!(diagnostics.len(), 1, "`{source}`: {diagnostics:?}");
        assert_eq!(
            diagnostics[0].code,
            DiagnosticCode::UNDECLARED_SYMBOL,
            "`{source}`"
        );
    }

    for source in ["func main @a8 { break }\n", "func main @a8 { repeat }\n"] {
        let (_, diagnostics) = analyze_control(source);
        assert_eq!(diagnostics.len(), 1, "`{source}`: {diagnostics:?}");
        assert_eq!(
            diagnostics[0].code,
            DiagnosticCode::LOOP_CONTROL_OUTSIDE_LOOP,
            "`{source}`"
        );
    }
}

fn analyze_control(
    source: &str,
) -> (
    k816::semantic::SemanticModel,
    Vec<k816::diagnostic::Diagnostic>,
) {
    let mut map = SourceMap::new();
    let id = map.add_file("control.k65", source);
    let parsed = parse_source(map.file(id), None);
    assert_eq!(parsed.diagnostics, vec![], "`{source}` did not parse");
    k816::semantic::analyze_file(&parsed.file)
}

#[test]
fn exact_spans_map_back_to_source_text() {
    let mut map = SourceMap::new();
    let id = map.add_file("slice.k816", HLA_SLICE);
    let result = parse_source(map.file(id), None);

    let ItemKind::Var(var) = &result.file.items[0].kind else {
        panic!("expected var declaration");
    };
    assert_eq!(map.span_text(var.name.span), "SCREEN");
    let address = var.address.as_ref().expect("expected an initializer");
    assert_eq!(map.span_text(address.span), "$0400");
    assert_eq!(address.kind, ExprKind::Number(0x0400));
    assert_eq!(var.name.provenance, Provenance::Source);

    let ItemKind::Func(func) = &result.file.items[1].kind else {
        panic!("expected func declaration");
    };
    assert_eq!(map.span_text(func.name.span), "work");
    assert_eq!(func.width.len(), 2);
    assert_eq!(func.width[0].kind, WidthAnnotationKind::A8);
    assert_eq!(map.span_text(func.width[0].span), "@a8");
    assert_eq!(func.width[1].kind, WidthAnnotationKind::I16);
    assert_eq!(map.span_text(func.width[1].span), "@i16");

    let StatementKind::Hla(HlaStatement::Assign(assign)) = &func.body.statements[2].kind else {
        panic!("expected store statement");
    };
    assert_eq!(map.span_text(assign.targets[0].span), "SCREEN");
    assert_eq!(map.span_text(assign.source.span), "a");
}

#[test]
fn native_and_hla_parse_to_distinct_ast_forms() {
    let mut map = SourceMap::new();
    let hla = map.add_file("hla.k816", HLA_SLICE);
    let native = map.add_file("native.k816", NATIVE_SLICE);
    let hla = parse_source(map.file(hla), None);
    let native = parse_source(map.file(native), None);

    let ItemKind::Func(hla_func) = &hla.file.items[1].kind else {
        panic!("expected func");
    };
    let ItemKind::Func(native_func) = &native.file.items[1].kind else {
        panic!("expected func");
    };
    assert!(
        hla_func
            .body
            .statements
            .iter()
            .all(|s| matches!(s.kind, StatementKind::Hla(_) | StatementKind::Return(_)))
    );
    assert!(
        native_func
            .body
            .statements
            .iter()
            .all(|s| matches!(s.kind, StatementKind::Native(_)))
    );
}

#[test]
fn malformed_statement_yields_navigable_ast_after_first_error() {
    let mut map = SourceMap::new();
    let id = map.add_file(
        "bad.k816",
        "func main @a8 {\n    a = =\n    return\n}\n\nfunc after @a8 {\n    return\n}\n",
    );
    let result = parse_source(map.file(id), None);

    // The syntax error is reported...
    assert!(
        result
            .diagnostics
            .iter()
            .any(|d| d.code == DiagnosticCode::SYNTAX_ERROR),
        "expected a syntax diagnostic, got {:?}",
        result.diagnostics
    );

    // ...and the AST stays navigable: both functions exist, the malformed
    // statement is an owned error node, and the `return` after it survives.
    assert_eq!(result.file.items.len(), 2);
    let ItemKind::Func(func) = &result.file.items[0].kind else {
        panic!("expected first func");
    };
    assert!(
        func.body
            .statements
            .iter()
            .any(|s| matches!(s.kind, StatementKind::Error))
    );
    assert!(
        func.body
            .statements
            .iter()
            .any(|s| matches!(s.kind, StatementKind::Return(_)))
    );
    let ItemKind::Func(after) = &result.file.items[1].kind else {
        panic!("expected second func");
    };
    assert_eq!(after.name.text, "after");
}

#[test]
fn missing_brace_gets_a_machine_applicable_fix() {
    let mut map = SourceMap::new();
    let id = map.add_file("open.k816", "func main @a8 {\n    return\n");
    let result = parse_source(map.file(id), None);

    let missing = result
        .diagnostics
        .iter()
        .find(|d| d.code == DiagnosticCode::SYNTAX_MISSING)
        .expect("expected a missing-token diagnostic");
    assert_eq!(missing.title, "missing `}`");
    assert!(missing.primary.span.is_empty());

    let fix = missing.fixes.first().expect("expected a fix");
    assert_eq!(fix.applicability, Applicability::MachineApplicable);
    assert_eq!(fix.edits.len(), 1);
    assert_eq!(fix.edits[0].replacement, "}");
    assert!(fix.edits[0].span.is_empty());

    // The function is still present in the AST.
    assert!(matches!(result.file.items[0].kind, ItemKind::Func(_)));
}

#[test]
fn missing_name_is_marked_synthetic_with_origin_span() {
    let mut map = SourceMap::new();
    let id = map.add_file("noname.k816", "var = $0400\n");
    let result = parse_source(map.file(id), None);
    assert!(!result.diagnostics.is_empty());

    // Whatever recovery shape Tree-sitter picks, no synthetic name may claim
    // real source text, and every AST span must stay inside the file.
    let len = map.file(id).text().len() as u32;
    for item in &result.file.items {
        assert!(item.span.end <= len);
        if let ItemKind::Var(var) = &item.kind
            && var.name.provenance == Provenance::Synthetic
        {
            assert!(var.name.text.is_empty());
            assert!(var.name.span.is_empty());
        }
    }
}

#[test]
fn empty_file_parses_to_empty_ast() {
    let mut map = SourceMap::new();
    let id = map.add_file("empty.k816", "");
    let result = parse_source(map.file(id), None);
    assert_eq!(result.diagnostics, vec![]);
    assert_eq!(result.file.items, vec![]);
    let _ = Span::empty_at(id, 0);
}

#[test]
fn every_corpus_program_parses_without_syntax_diagnostics() {
    // The end-to-end gate: the stress test, the full-stack fixture, and the
    // four legacy K65 programs with their shared header.
    for path in [
        "examples/hello_uart.k65",
        "examples/fullstack/main.k816",
        "examples/k65/atari-xl/include/defs.k65",
        "examples/k65/atari-xl/dli-example/main.k65",
        "examples/k65/atari-xl/landscape/main.k65",
        "examples/k65/atari-xl/rainbow/main.k65",
        "examples/k65/atari-xl/starfield/main.k65",
    ] {
        let full = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join(path);
        let text = std::fs::read_to_string(&full).expect("read fixture");
        let mut map = SourceMap::new();
        let id = map.add_file(path, text);
        let result = parse_source(map.file(id), None);
        assert_eq!(result.diagnostics, vec![], "{path} did not parse cleanly");
        assert!(!result.file.items.is_empty(), "{path} produced no items");
    }
}

/// Every fixture the sub-plan names as a corpus program, plus the two slice
/// forms, for the tests that sweep over real source.
fn fixture_sources() -> Vec<(&'static str, String)> {
    [
        "examples/hello_uart.k65",
        "examples/fullstack/main.k816",
        "examples/k65/atari-xl/include/defs.k65",
        "examples/k65/atari-xl/dli-example/main.k65",
        "examples/k65/atari-xl/landscape/main.k65",
        "examples/k65/atari-xl/rainbow/main.k65",
        "examples/k65/atari-xl/starfield/main.k65",
    ]
    .into_iter()
    .map(|path| {
        let full = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join(path);
        (path, std::fs::read_to_string(full).expect("read fixture"))
    })
    .collect()
}

#[test]
fn truncating_a_fixture_anywhere_yields_a_usable_tree() {
    // Every prefix of a real program is a file someone is part-way through
    // typing. None of them may panic, produce a span outside the file, or
    // lose the declarations that were already complete.
    for (path, source) in fixture_sources() {
        for end in (0..source.len()).step_by(7) {
            if !source.is_char_boundary(end) {
                continue;
            }
            let truncated = &source[..end];
            let mut map = SourceMap::new();
            let id = map.add_file(path, truncated);
            let result = parse_source(map.file(id), None);

            let length = truncated.len() as u32;
            for item in &result.file.items {
                assert!(
                    item.span.start <= item.span.end && item.span.end <= length,
                    "{path} truncated at {end}: item span {:?} escapes the file",
                    item.span
                );
            }
            for diagnostic in &result.diagnostics {
                assert!(
                    diagnostic.primary.span.end <= length,
                    "{path} truncated at {end}: diagnostic span escapes the file"
                );
                // An unfinished file is not a compiler bug. `K816-E0900`
                // claims a grammar/builder mismatch, and every prefix of a
                // real program is something an editor sees between two
                // keystrokes — a `func` missing its closing brace leaves its
                // statements at the file root, and reporting *that* as an
                // internal error is a false alarm at the worst moment.
                assert_ne!(
                    diagnostic.code,
                    DiagnosticCode::INTERNAL,
                    "{path} truncated at {end}: {}\n{truncated}",
                    diagnostic.title
                );
            }
        }
    }
}

#[test]
fn typing_a_fixture_character_by_character_stays_stable() {
    // The same sweep from the other direction: a growing prefix must produce
    // the same tree as parsing that prefix outright, which is what an editor
    // sees between keystrokes.
    let (path, source) = fixture_sources()
        .into_iter()
        .find(|(path, _)| path.ends_with("hello_uart.k65"))
        .expect("the stress-test fixture");

    for end in (0..source.len()).step_by(11) {
        if !source.is_char_boundary(end) {
            continue;
        }
        let prefix = &source[..end];
        let mut map = SourceMap::new();
        let id = map.add_file(path, prefix);
        let first = parse_source(map.file(id), None);
        let second = parse_source(map.file(id), None);
        assert_eq!(
            first.tree.dump(),
            second.tree.dump(),
            "{path} at {end}: parsing is not deterministic"
        );
        assert_eq!(first.file, second.file);
    }
}

#[test]
fn a_syntax_error_names_what_was_expected_there() {
    // The expected-context table covers the whole grammar, so the message
    // depends on where the error is rather than defaulting to "valid syntax".
    for (source, expected) in [
        ("var = 1\n", "expected a declaration or a code section"),
        ("func f { >= }\n", "expected a statement"),
        ("data dd { repeat }\n", "expected a data entry"),
        ("var vv[ .x : ]\n", "expected a `.field` entry"),
    ] {
        let mut map = SourceMap::new();
        let id = map.add_file("bad.k65", source);
        let result = parse_source(map.file(id), None);
        let titles: Vec<&str> = result
            .diagnostics
            .iter()
            .map(|diagnostic| diagnostic.title.as_str())
            .collect();
        assert!(
            titles.contains(&expected),
            "`{source}`: expected {expected:?} among {titles:?}"
        );
    }
}

#[test]
fn a_keyword_less_far_call_is_one_call() {
    // `far NAME` is the documented alternative spelling. It must not degrade
    // into two bare calls, and it must not steal the `far` operand prefix.
    let mut map = SourceMap::new();
    let id = map.add_file(
        "far.k65",
        "func f {\n  far long_range_sub\n  inc_x\n  inc_y\n  lda far addr\n}\n",
    );
    let result = parse_source(map.file(id), None);
    assert_eq!(result.diagnostics, vec![]);
    let ItemKind::Func(func) = &result.file.items[0].kind else {
        panic!("expected a func");
    };
    assert_eq!(func.body.statements.len(), 4);

    let StatementKind::Call(call) = &func.body.statements[0].kind else {
        panic!(
            "expected a far call, got {:?}",
            func.body.statements[0].kind
        );
    };
    assert!(call.is_far);
    assert_eq!(call.target.text, "long_range_sub");
    assert!(matches!(
        func.body.statements[1].kind,
        StatementKind::BareCall(_)
    ));
    assert!(matches!(
        func.body.statements[2].kind,
        StatementKind::BareCall(_)
    ));
    let StatementKind::Native(native) = &func.body.statements[3].kind else {
        panic!("expected an instruction");
    };
    let Some(NativeOperand::Address(address)) = &native.operand else {
        panic!("expected an address operand");
    };
    assert_eq!(
        address.encoding.expect("an encoding prefix").kind,
        StorageClassKind::Far
    );
}

#[test]
fn a_call_site_contract_reaches_the_ast() {
    let mut map = SourceMap::new();
    let id = map.add_file(
        "contract.k65",
        "func f {\n  add a, x -> a, y\n  scale a, #16 -> a\n  reader -> a\n  inc_x\n}\n",
    );
    let result = parse_source(map.file(id), None);
    assert_eq!(result.diagnostics, vec![]);
    let ItemKind::Func(func) = &result.file.items[0].kind else {
        panic!("expected a func");
    };

    let calls: Vec<&k816::ast::BareCall> = func
        .body
        .statements
        .iter()
        .map(|statement| match &statement.kind {
            StatementKind::BareCall(call) => call,
            other => panic!("expected a bare call, got {other:?}"),
        })
        .collect();

    assert_eq!(calls[0].target.text, "add");
    assert_eq!(calls[0].arguments.as_ref().expect("arguments").len(), 2);
    assert_eq!(calls[0].outputs.as_ref().expect("outputs").len(), 2);

    let scale = calls[1].arguments.as_ref().expect("arguments");
    assert!(matches!(scale[0], CallArgument::Register(_)));
    assert!(matches!(scale[1], CallArgument::Immediate(_)));

    // A `->` clause with no arguments, and a call with neither.
    assert!(calls[2].arguments.is_none());
    assert_eq!(calls[2].outputs.as_ref().expect("outputs").len(), 1);
    assert!(calls[3].arguments.is_none());
    assert!(calls[3].outputs.is_none());
}

#[test]
fn a_nested_reserved_construct_reports_its_own_k65_code() {
    // A reserved construct keeps its `K816-K65-####` diagnostic wherever it
    // sits — inside a data block or nested in an evaluator expression — not
    // only when it is the whole item.
    for (source, expected) in [
        (
            "data dd {\n  [ 1 + color(0x101010) ]\n}\n",
            DiagnosticCode::K65_RESERVED_EVAL_FUNCTION,
        ),
        (
            "data dd {\n  [ max(color(0x101010), 1) ]\n}\n",
            DiagnosticCode::K65_RESERVED_EVAL_FUNCTION,
        ),
        (
            "data dd {\n  for x=0..255 eval [ rnd() & 255 ]\n}\n",
            DiagnosticCode::K65_RESERVED_EVAL_FUNCTION,
        ),
        (
            "data dd {\n  image sprites 0 0 8> 16v\n}\n",
            DiagnosticCode::K65_RESERVED_IMAGE,
        ),
        ("image S = \"s.bmp\"\n", DiagnosticCode::K65_RESERVED_IMAGE),
    ] {
        let mut map = SourceMap::new();
        let id = map.add_file("reserved.k65", source);
        let parsed = parse_source(map.file(id), None);
        assert_eq!(parsed.diagnostics, vec![], "`{source}` did not parse");
        let (_, diagnostics) = k816::semantic::analyze_file(&parsed.file);
        assert!(
            diagnostics.iter().any(|d| d.code == expected),
            "`{source}`: expected {} among {:?}",
            expected.as_str(),
            diagnostics
                .iter()
                .map(|d| d.code.as_str())
                .collect::<Vec<_>>()
        );
    }
}

#[test]
fn the_nocross_data_wrapper_is_the_same_block() {
    let mut map = SourceMap::new();
    let id = map.add_file(
        "nocross.k65",
        "nocross data DD { 1 2 3 }\ndata E { 4 }\nfunc f {\n  nocross data { 5 }\n}\n",
    );
    let result = parse_source(map.file(id), None);
    assert_eq!(result.diagnostics, vec![]);

    let ItemKind::Data(wrapped) = &result.file.items[0].kind else {
        panic!("expected a data block");
    };
    assert!(wrapped.nocross);
    assert_eq!(wrapped.entries.len(), 3);

    let ItemKind::Data(plain) = &result.file.items[1].kind else {
        panic!("expected a data block");
    };
    assert!(!plain.nocross);

    let ItemKind::Func(func) = &result.file.items[2].kind else {
        panic!("expected a func");
    };
    let StatementKind::Data(inline) = &func.body.statements[0].kind else {
        panic!("expected an inline data block");
    };
    assert!(inline.nocross);
}

#[test]
fn an_evaluator_entry_is_its_own_data_value() {
    // `&&ptr` followed by `[2+2]` is two entries — an address and an
    // evaluator-produced byte — not one address-of applied to `ptr[2+2]`.
    let mut map = SourceMap::new();
    let id = map.add_file("data.k65", "data dd {\n  &&ptr\n  [2+2]\n  &<ptr+1\n}\n");
    let result = parse_source(map.file(id), None);
    assert_eq!(result.diagnostics, vec![]);
    let ItemKind::Data(data) = &result.file.items[0].kind else {
        panic!("expected a data block");
    };
    assert_eq!(data.entries.len(), 3);

    let values: Vec<String> = data
        .entries
        .iter()
        .map(|entry| match &entry.kind {
            k816::ast::DataEntryKind::Value(k816::ast::DataValue::Expr(expr)) => {
                dump_expr(&expr.kind)
            }
            other => panic!("expected a value entry, got {other:?}"),
        })
        .collect();
    assert_eq!(values, vec!["(&& ptr)", "[(+ 2 2)]", "(&< (+ ptr 1))"]);
}

/// The two items sub-plan K adds build the shapes they declare.
///
/// Structural rather than a snapshot, because what matters here is the
/// *fields* — an extern's far-ness, its two contract halves, a variable's
/// storage class and typed width — and a rendered line would let one of them
/// go missing without changing the text.
#[test]
fn import_and_extern_items_build_the_declared_shapes() {
    let mut map = SourceMap::new();
    let file = map.add_file(
        "sub/dir/unit.k816",
        "import \"lib.k816\"\n\
         import \"../shared/defs.k65\"\n\
         extern func _puts\n\
         extern far func rom_entry\n\
         extern func setup @a8 @i16 -> @a16 @i8\n\
         extern var frame\n\
         extern var dp ptr\n\
         extern var far rom : far\n\
         extern const ROWS\n",
    );
    let parse = parse_source(map.file(file), None);
    assert_eq!(parse.diagnostics, Vec::new());

    let paths: Vec<(&str, String)> = parse
        .file
        .items
        .iter()
        .filter_map(|item| match &item.kind {
            ItemKind::Import(import) => Some((
                import.file.path.as_str(),
                import.file.resolved.to_string_lossy().into_owned(),
            )),
            _ => None,
        })
        .collect();
    // The path is kept as written *and* resolved against the importing
    // file's own directory, which is what makes `import "lib.k816"` mean the
    // sibling of the source rather than of the working directory.
    assert_eq!(
        paths,
        vec![
            ("lib.k816", "sub/dir/lib.k816".to_owned()),
            (
                "../shared/defs.k65",
                "sub/dir/../shared/defs.k65".to_owned()
            ),
        ]
    );

    let externs: Vec<&ExternKind> = parse
        .file
        .items
        .iter()
        .filter_map(|item| match &item.kind {
            ItemKind::Extern(declaration) => Some(&declaration.kind),
            _ => None,
        })
        .collect();
    assert_eq!(externs.len(), 7);

    match externs[0] {
        ExternKind::Func(function) => {
            assert_eq!(function.name.text, "_puts");
            assert!(!function.is_far);
            assert!(function.width.is_empty());
            assert!(function.exit.is_none());
        }
        other => panic!("expected an extern function, got {other:?}"),
    }
    match externs[1] {
        ExternKind::Func(function) => {
            assert_eq!(function.name.text, "rom_entry");
            assert!(function.is_far, "`extern far func` carries its far-ness");
        }
        other => panic!("expected an extern function, got {other:?}"),
    }
    match externs[2] {
        ExternKind::Func(function) => {
            let entry: Vec<WidthAnnotationKind> =
                function.width.iter().map(|width| width.kind).collect();
            let exit: Vec<WidthAnnotationKind> = function
                .exit
                .as_ref()
                .expect("a declared exit")
                .width
                .iter()
                .map(|width| width.kind)
                .collect();
            // Both halves, kept apart: the annotations before `->` are what
            // a caller must reach, the ones after are what it may believe.
            assert_eq!(
                entry,
                vec![WidthAnnotationKind::A8, WidthAnnotationKind::I16]
            );
            assert_eq!(
                exit,
                vec![WidthAnnotationKind::A16, WidthAnnotationKind::I8]
            );
        }
        other => panic!("expected an extern function, got {other:?}"),
    }
    let variables: Vec<(&str, Option<StorageClassKind>, Option<TypeWidthKind>)> = externs[3..6]
        .iter()
        .map(|kind| match kind {
            ExternKind::Var(variable) => (
                variable.name.text.as_str(),
                variable.storage.map(|storage| storage.kind),
                variable.width.map(|width| width.kind),
            ),
            other => panic!("expected an extern variable, got {other:?}"),
        })
        .collect();
    assert_eq!(
        variables,
        vec![
            ("frame", None, None),
            ("ptr", Some(StorageClassKind::DirectPage), None),
            ("rom", Some(StorageClassKind::Far), Some(TypeWidthKind::Far)),
        ]
    );
    match externs[6] {
        // No value, which is the whole difference from a `const`.
        ExternKind::Const(constant) => assert_eq!(constant.name.text, "ROWS"),
        other => panic!("expected an extern constant, got {other:?}"),
    }
}

/// `extern` and `import` are item keywords and nothing else.
///
/// Tree-sitter's keyword extraction makes a literal a keyword only where the
/// parser expects one, so a program that already uses either word as an
/// ordinary name keeps working. Nothing in the corpus does, but the language
/// promised it by not putting them in `reserved.global`, and a promise
/// nothing checks is not one.
#[test]
fn import_and_extern_are_still_usable_as_names() {
    let mut map = SourceMap::new();
    let file = map.add_file(
        "unit.k816",
        "var import = $0400\n\
         var extern = $0401\n\
         \n\
         func main @a8 @i8 {\n\
         \x20   a = import\n\
         \x20   extern = a\n\
         \x20   return\n\
         }\n",
    );
    let parse = parse_source(map.file(file), None);
    assert_eq!(parse.diagnostics, Vec::new());
}

// ---------------------------------------------------------------------------
// ca65 idioms, recognized where the grammar failed (`src/parser/ca65.rs`)
// ---------------------------------------------------------------------------
//
// A source carried over from ca65 by hand fails in a handful of recurring
// shapes, and the grammar's own account of each — "missing `:`", "expected a
// data entry" — is true and useless. These are the shapes, each reported
// with its own code, the K816 spelling, and a fix where one is token for
// token. The generic syntax diagnostic is *replaced* for that region, not
// added to.

/// The one diagnostic with `code` a source produces, and the map to read its
/// spans against.
fn ca65_diagnostic(
    source: &str,
    code: DiagnosticCode,
) -> (SourceMap, k816::diagnostic::Diagnostic) {
    let mut map = SourceMap::new();
    let id = map.add_file("carried.k816", source);
    let result = parse_source(map.file(id), None);
    let found = result
        .diagnostics
        .iter()
        .find(|d| d.code == code)
        .unwrap_or_else(|| {
            panic!(
                "no {} in {:?}",
                code.as_str(),
                result
                    .diagnostics
                    .iter()
                    .map(|d| (d.code.as_str(), d.title.as_str()))
                    .collect::<Vec<_>>()
            )
        })
        .clone();
    // The generic diagnostic for the same region is gone: a reader gets one
    // explanation, the better one.
    assert!(
        !result.diagnostics.iter().any(|d| {
            matches!(
                d.code,
                DiagnosticCode::SYNTAX_ERROR | DiagnosticCode::SYNTAX_MISSING
            ) && d.primary.span.start >= found.primary.span.start
                && d.primary.span.start <= found.primary.span.end
        }),
        "the generic syntax diagnostic survived beside {}",
        code.as_str()
    );
    (map, found)
}

#[test]
fn a_ca65_comment_is_named_and_offered_as_a_line_comment() {
    let (map, found) = ca65_diagnostic(
        "func main @a8 @i8 {\n    lda #1 ; load one\n}\n",
        DiagnosticCode::CA65_COMMENT,
    );
    assert!(map.span_text(found.primary.span).starts_with("; load one"));
    let fix = found.fixes.first().expect("a fix");
    assert_eq!(fix.applicability, Applicability::MaybeIncorrect);
    assert_eq!(map.span_text(fix.edits[0].span), ";");
    assert_eq!(fix.edits[0].replacement, "//");
}

#[test]
fn a_ca65_comment_that_parses_as_calls_is_reported_once_as_a_warning() {
    let mut map = SourceMap::new();
    let id = map.add_file(
        "prose.k816",
        "func main @a8 @i8 {\n    sei ; disable interrupts\n}\n",
    );
    let result = parse_source(map.file(id), None);
    let [found] = result.diagnostics.as_slice() else {
        panic!("one diagnostic, found {:?}", result.diagnostics);
    };
    assert_eq!(found.code, DiagnosticCode::CA65_COMMENT);
    assert_eq!(found.severity, k816::diagnostic::Severity::Warning);
    assert!(
        map.span_text(found.primary.span)
            .starts_with("; disable interrupts")
    );
    let fix = found.fixes.first().expect("a fix");
    assert_eq!(fix.applicability, Applicability::MaybeIncorrect);
    assert_eq!(map.span_text(fix.edits[0].span), ";");
    assert_eq!(fix.edits[0].replacement, "//");
}

#[test]
fn a_semicolon_separating_statements_is_not_a_comment() {
    for source in [
        "func main @a8 @i8 { nop ; nop }\n",
        // One word after the separator is a call, and may well be one.
        "func inc_x @a8 @i8 { x++ }\nfunc main @a8 @i8 { nop ; inc_x }\n",
        // Two calls with something else after them on the line are code.
        "func a_ @a8 @i8 { nop }\nfunc b_ @a8 @i8 { nop }\nfunc main @a8 @i8 { nop ; a_ b_ x++ }\n",
        // A trailing terminator, as the original's sources write it.
        "const MARK = 0x27 ;\nfunc main @a8 @i8 {\n    a = #MARK ;\n    return ;\n}\n",
    ] {
        let mut map = SourceMap::new();
        let id = map.add_file("sep.k816", source);
        let result = parse_source(map.file(id), None);
        assert_eq!(result.diagnostics, Vec::new(), "{source}");
    }
}

#[test]
fn a_ca65_directive_is_named_with_what_k816_writes() {
    // A statement directive with a token-for-token spelling: a fix.
    let (map, found) = ca65_diagnostic(
        "func main {\n    .a8\n    lda #1\n}\n",
        DiagnosticCode::CA65_DIRECTIVE,
    );
    assert_eq!(map.span_text(found.primary.span), ".a8");
    let fix = found.fixes.first().expect("a fix");
    assert_eq!(fix.applicability, Applicability::MachineApplicable);
    assert_eq!(fix.edits[0].replacement, "@a8");

    // A top-level directive whose K816 form is advice.
    let (map, found) = ca65_diagnostic(
        ".segment \"CODE\"\nfunc main @a8 @i8 {\n    nop\n}\n",
        DiagnosticCode::CA65_DIRECTIVE,
    );
    assert_eq!(map.span_text(found.primary.span), ".segment");
    assert!(
        found
            .help
            .as_deref()
            .is_some_and(|help| help.contains("segment CODE")),
        "{found:?}"
    );
    assert!(found.fixes.is_empty());

    // A data directive inside a block, which is not at the start of its line.
    let (map, found) = ca65_diagnostic("data t { .byte 1 2 }\n", DiagnosticCode::CA65_DIRECTIVE);
    assert_eq!(map.span_text(found.primary.span), ".byte");

    // A directive K816 does without: the line goes.
    let (_, found) = ca65_diagnostic(
        ".export main\nfunc main @a8 @i8 {\n    nop\n}\n",
        DiagnosticCode::CA65_DIRECTIVE,
    );
    let fix = found.fixes.first().expect("a fix");
    assert_eq!(fix.title, "delete the line");
    assert_eq!(fix.edits[0].replacement, "");

    // A directive the table does not know is still named, and pointed at
    // the guide.
    let (map, found) = ca65_diagnostic(".fopt compiler, \"x\"\n", DiagnosticCode::CA65_DIRECTIVE);
    assert_eq!(map.span_text(found.primary.span), ".fopt");
    assert_eq!(
        found.docs.as_ref().map(|docs| docs.0.as_str()),
        Some("doc/ca65-to-k816.md")
    );
}

#[test]
fn the_scope_operator_is_a_field_dot_or_nothing() {
    let (map, found) = ca65_diagnostic(
        "var RIA[ .op :byte ] = $FFC0\nfunc main @a8 @i8 {\n    sta RIA::op\n}\n",
        DiagnosticCode::CA65_SCOPE_OPERATOR,
    );
    assert_eq!(map.span_text(found.primary.span), "::");
    let fix = found.fixes.first().expect("a fix");
    assert_eq!(fix.applicability, Applicability::MachineApplicable);
    assert_eq!(fix.edits[0].replacement, ".");

    // As a global prefix there is nothing to write.
    let (map, found) = ca65_diagnostic(
        "func main @a8 @i8 {\n    jsr ::foo\n}\n",
        DiagnosticCode::CA65_SCOPE_OPERATOR,
    );
    assert_eq!(map.span_text(found.primary.span), "::");
    let fix = found.fixes.first().expect("a fix");
    assert_eq!(fix.title, "delete `::`");
    assert_eq!(fix.edits[0].replacement, "");
}

#[test]
fn address_byte_operators_are_spelled_with_an_ampersand() {
    let (map, found) = ca65_diagnostic(
        "var addr = $1234\nfunc main @a8 @i8 {\n    lda #<addr\n}\n",
        DiagnosticCode::CA65_ADDRESS_BYTE,
    );
    assert_eq!(map.span_text(found.primary.span), "#<");
    assert_eq!(found.fixes[0].edits[0].replacement, "#&<");

    let (map, found) = ca65_diagnostic(
        "var addr = $1234\nfunc main @a8 @i8 {\n    ldx #.hibyte(addr)\n}\n",
        DiagnosticCode::CA65_ADDRESS_BYTE,
    );
    assert_eq!(map.span_text(found.primary.span), ".hibyte(");
    assert_eq!(found.fixes[0].edits[0].replacement, "&>");

    // The bank byte has no operator: the help says what to write and no
    // fix claims otherwise.
    let (map, found) = ca65_diagnostic(
        "var far rom = $7E1234\nfunc main @a8 @i8 {\n    lda #^rom\n}\n",
        DiagnosticCode::CA65_ADDRESS_BYTE,
    );
    assert_eq!(map.span_text(found.primary.span), "#^");
    assert!(found.fixes.is_empty());
    assert!(
        found
            .help
            .as_deref()
            .is_some_and(|help| help.contains(">> 16"))
    );
}

#[test]
fn sizeof_is_a_suffix() {
    let (map, found) = ca65_diagnostic(
        "var RIA[ .op :byte ] = $FFC0\nfunc main @a8 @i8 {\n    lda #.sizeof(RIA)\n}\n",
        DiagnosticCode::CA65_SIZEOF,
    );
    assert_eq!(map.span_text(found.primary.span), ".sizeof(RIA)");
    assert_eq!(found.fixes[0].edits[0].replacement, "RIA:sizeof");
}

#[test]
fn a_cheap_local_is_renamed_at_every_use() {
    let source = "func main @a8 @i8 {\n@loop:\n    dex\n    bne @loop\n}\n";
    let (map, found) = ca65_diagnostic(source, DiagnosticCode::CA65_CHEAP_LOCAL);
    assert_eq!(map.span_text(found.primary.span), "@loop");
    let fix = found.fixes.first().expect("a fix");
    assert_eq!(fix.applicability, Applicability::MachineApplicable);
    assert_eq!(
        fix.edits.len(),
        2,
        "the definition and the branch: {:?}",
        fix.edits
    );

    // Applied, the program parses: the fix renames the use the caret was
    // not on, which is what makes it machine-applicable.
    let mut fixed = source.to_owned();
    let mut edits = fix.edits.clone();
    edits.sort_by_key(|edit| std::cmp::Reverse(edit.span.start));
    for edit in edits {
        fixed.replace_range(
            edit.span.start as usize..edit.span.end as usize,
            &edit.replacement,
        );
    }
    assert_eq!(
        fixed,
        "func main @a8 @i8 {\n.loop:\n    dex\n    bne .loop\n}\n"
    );
    let mut map = SourceMap::new();
    let id = map.add_file("fixed.k816", fixed);
    assert_eq!(parse_source(map.file(id), None).diagnostics, Vec::new());

    // A width annotation is not a cheap local.
    let mut map = SourceMap::new();
    let id = map.add_file("width.k816", "func main @a8 @i8 {\n    @a16 { nop }\n}\n");
    assert_eq!(parse_source(map.file(id), None).diagnostics, Vec::new());
}

#[test]
fn code_outside_a_section_is_told_to_open_one() {
    let (map, found) = ca65_diagnostic(
        "main:\n    nop\n    rts\n",
        DiagnosticCode::CA65_CODE_OUTSIDE_SECTION,
    );
    assert_eq!(map.span_text(found.primary.span), "main:");
    assert!(
        found
            .help
            .as_deref()
            .is_some_and(|help| help.contains("func main {")),
        "{found:?}"
    );
    assert!(found.fixes.is_empty());
}

/// Sub-plan Q: an `if` is built as the K65 statements it stands for — the
/// compare written in it first, then the bare conditional — and records
/// only how it was spelled, which is what the dialect reads
/// (`plans/16-if-statement.md` decisions 5 and 9).
///
/// The two statements must not claim the same text: the conditional's span
/// starts at its condition, after the compare's ends.
#[test]
fn an_if_is_built_as_the_statements_it_stands_for() {
    let source = "func f @a8 @i8 {\n    if a?#10 < { x++ } else if == { y++ }\n    \
                  if == break\n    loop { x++ } while x?#5 !=\n    >= { x++ }\n}\n";
    let mut map = SourceMap::new();
    let id = map.add_file("if.kk", source);
    let result = parse_source(map.file(id), None);
    assert_eq!(result.diagnostics, vec![]);
    let text = |span: Span| &source[span.start as usize..span.end as usize];
    let ItemKind::Func(function) = &result.file.items[0].kind else {
        panic!("expected a function");
    };
    let statements = &function.body.statements;
    assert_eq!(statements.len(), 5, "{statements:#?}");

    let StatementKind::Hla(HlaStatement::Arithmetic(_)) = &statements[0].kind else {
        panic!(
            "the compare is a statement of its own: {:?}",
            statements[0].kind
        );
    };
    assert_eq!(text(statements[0].span), "a?#10");

    let StatementKind::Conditional(conditional) = &statements[1].kind else {
        panic!("then the conditional: {:?}", statements[1].kind);
    };
    assert_eq!(conditional.keyword.map(text), Some("if"));
    assert!(conditional.chained, "`else if` is a chain");
    assert!(text(statements[1].span).starts_with("< { x++ }"));
    assert!(statements[0].span.end <= statements[1].span.start);
    let chain = conditional.otherwise.as_ref().expect("the else");
    assert_eq!(text(chain.span), "if == { y++ }");
    let [inner] = chain.statements.as_slice() else {
        panic!("a chain is the one `if` in a block: {:?}", chain.statements);
    };
    let StatementKind::Conditional(inner) = &inner.kind else {
        panic!("the chained if is a conditional");
    };
    assert_eq!(inner.keyword.map(text), Some("if"));
    assert!(!inner.chained && inner.otherwise.is_none());

    let StatementKind::ConditionalJump(jump) = &statements[2].kind else {
        panic!("an `if` before a jump: {:?}", statements[2].kind);
    };
    assert_eq!(jump.keyword.map(text), Some("if"));

    let StatementKind::Loop(loop_statement) = &statements[3].kind else {
        panic!("a loop: {:?}", statements[3].kind);
    };
    assert_eq!(loop_statement.test_compare.map(text), Some("x?#5"));
    let last = loop_statement.body.statements.last().expect("a body");
    assert!(
        matches!(last.kind, StatementKind::Hla(HlaStatement::Arithmetic(_))),
        "the `while` compare is the body's last statement"
    );
    assert_eq!(text(last.span), "x?#5");

    let StatementKind::Conditional(bare) = &statements[4].kind else {
        panic!("K65's bare conditional: {:?}", statements[4].kind);
    };
    assert_eq!(bare.keyword, None);
    assert!(!bare.chained);
}

/// `if <` with nothing after it is a valid prefix of `if < repeat`, so
/// Tree-sitter invents the `repeat`. The diagnostic says what is really
/// missing and offers no fix, and no `repeat` reaches the analysis to be
/// refused a second time as "not inside a loop" — a regression found while
/// building sub-plan Q, where the generic path did both.
#[test]
fn a_dangling_if_says_what_is_missing_and_invents_no_jump() {
    for body in ["if <", "if a?#10 <", "if < nop"] {
        let source = format!("func f @a8 @i8 {{\n    {body}\n}}\n");
        let mut map = SourceMap::new();
        let id = map.add_file("dangling.kk", source.as_str());
        let result = k816::compile_to_ca65(&map, id, &k816::Options::default());
        let codes: Vec<_> = result.diagnostics.iter().map(|d| d.code).collect();
        assert_eq!(
            codes,
            vec![DiagnosticCode::SYNTAX_MISSING],
            "`{body}`: {codes:?}"
        );
        let missing = &result.diagnostics[0];
        assert_eq!(
            missing.title, "missing a `{ ... }` block or a jump after the condition",
            "`{body}`"
        );
        assert!(
            missing.fixes.is_empty(),
            "`{body}` offers a fix: {:?}",
            missing.fixes
        );
    }
}

/// A malformed `if` body is phrased for the `if`, which is what
/// `expected_in`'s `if_statement` arm is for.
#[test]
fn a_malformed_if_is_phrased_for_the_if() {
    let mut map = SourceMap::new();
    let id = map.add_file("bad.kk", "func f @a8 @i8 {\n    if < 5 { x++ }\n}\n");
    let result = parse_source(map.file(id), None);
    let titles: Vec<_> = result
        .diagnostics
        .iter()
        .map(|d| d.title.as_str())
        .collect();
    assert_eq!(
        titles,
        ["expected a condition, then a `{ ... }` block or a jump"]
    );
}

/// Sub-plan R: after `if` or `while` a name is a flag test. The four flags
/// build the same test their `?` spelling does, with the spelling recorded;
/// `o` is `o?`'s refusal, and any other name is `K816-E0181`
/// (`plans/17-flag-tests.md` decision 3).
#[test]
fn a_flag_test_by_its_letter_names_one_of_the_four() {
    use k816::ast::{BranchConditionKind, FlagTestSpelling, TestableFlag};
    let source = "func f @a8 @i8 {\n    if z { x++ }\n    if !C goto .out\n.out:\n}\n";
    let mut map = SourceMap::new();
    let id = map.add_file("letter.kk", source);
    let result = parse_source(map.file(id), None);
    assert_eq!(result.diagnostics, vec![]);
    let ItemKind::Func(function) = &result.file.items[0].kind else {
        panic!("expected a function");
    };
    let conditions: Vec<_> = function
        .body
        .statements
        .iter()
        .filter_map(|statement| match &statement.kind {
            StatementKind::Conditional(conditional) => Some(conditional.condition.kind),
            StatementKind::ConditionalJump(jump) => Some(jump.condition.kind),
            _ => None,
        })
        .collect();
    assert_eq!(
        conditions,
        [
            BranchConditionKind::Flag {
                flag: TestableFlag::Zero,
                set: true,
                spelling: FlagTestSpelling::Letter,
            },
            BranchConditionKind::Flag {
                flag: TestableFlag::Carry,
                set: false,
                spelling: FlagTestSpelling::Letter,
            },
        ]
    );

    for (body, code) in [
        ("if done { x++ }", DiagnosticCode::FLAG_TEST_LETTER),
        ("if d { x++ }", DiagnosticCode::FLAG_TEST_LETTER),
        ("loop { x++ } while !i", DiagnosticCode::FLAG_TEST_LETTER),
        ("if o { x++ }", DiagnosticCode::K65_OVERFLOW_FLAG_SPELLING),
    ] {
        let mut map = SourceMap::new();
        let text = format!("func f @a8 @i8 {{\n    {body}\n}}\n");
        let id = map.add_file("letter.kk", text.as_str());
        let result = parse_source(map.file(id), None);
        let codes: Vec<_> = result.diagnostics.iter().map(|d| d.code).collect();
        assert_eq!(codes, vec![code], "`{body}`");
        // `o` is the overflow flag's other letter, and its fix writes `v`.
        if code == DiagnosticCode::K65_OVERFLOW_FLAG_SPELLING {
            let edit = &result.diagnostics[0].fixes[0].edits[0];
            let mut fixed = text.clone();
            fixed.replace_range(
                edit.span.start as usize..edit.span.end as usize,
                &edit.replacement,
            );
            assert!(fixed.contains("if v { x++ }"), "{fixed}");
        }
    }
}
