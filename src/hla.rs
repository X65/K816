/*****************************************************************************/
/*                                                                           */
/*                                  hla.rs                                   */
/*                                                                           */
/*         What an HLA statement means, as 65816 instructions             */
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

//! What an HLA statement means, as the instruction sequence it expands to.
//!
//! Analysis and lowering both need that answer, and a statement family whose
//! meaning is written out twice is the failure sub-plan E exists to end. So
//! the expansion lives here once: analysis reports what it turns up, lowering
//! emits what it returns, and neither of them owns the rule.
//!
//! Operands are not this module's business — they go through [`Resolver`]
//! exactly as a native instruction's do (sub-plan E, decision 1). What is
//! this module's business is *statement* knowledge: which mnemonic a spelling
//! selects, which register carries the value, and in which order the
//! instructions run.

use crate::ast::{
    ArithmeticOp, BranchConditionKind, ComparisonKind, Expr, ExprKind, FlagKind, HlaArithmetic,
    HlaAssignment, HlaExchange, HlaFlag, HlaOperand, HlaOperandKind, HlaShift, HlaStack,
    HlaStatement, HlaStep, RegisterKind, ShiftOp, StackOp, StepOp, TestableFlag,
};
use crate::diagnostic::{Applicability, Diagnostic, DiagnosticCode, Fix, Label, TextEdit};
use crate::isa::Mnemonic;
use crate::resolve::{Classified, Resolver};
use crate::source::Span;

/// The instruction sequence an HLA statement expands to, or `None` when it has
/// none — in which case the resolver carries the diagnostic that says so.
///
/// One entry point for the whole family, because analysis and lowering must
/// walk every statement the same way and there is nothing to gain from two
/// dispatches over the same enum.
pub fn statement(
    resolver: &mut Resolver,
    statement: &HlaStatement,
    site: Span,
) -> Option<Vec<Classified>> {
    match statement {
        HlaStatement::Assign(assign) => assignment(resolver, assign, site),
        HlaStatement::Exchange(exchange) => exchange_halves(resolver, exchange, site),
        HlaStatement::Arithmetic(arithmetic) => alu(resolver, arithmetic, site),
        HlaStatement::Shift(shift) => shift_or_rotate(resolver, shift, site),
        HlaStatement::Step(step) => step_by_one(resolver, step, site),
        HlaStatement::Flag(flag) => set_or_clear_flag(resolver, flag, site),
        HlaStatement::Stack(stack) => push_or_pull(resolver, stack, site),
        HlaStatement::Nop { count } => nops(resolver, count.as_ref(), site),
        HlaStatement::Break => breakpoint(resolver, site),
    }
}

/// The instruction sequence an assignment chain expands to.
///
/// A chain evaluates right to left: the rightmost operand is the source, and
/// each `=` to its left stores or transfers the value now held
/// (`doc/syntax-reference.md`, "Assignment Chaining"). The register that
/// carries it is the one the chain names; K816 never picks one the author did
/// not write, because an inserted load clobbers a register the author is
/// entitled to still be using.
fn assignment(
    resolver: &mut Resolver,
    assign: &HlaAssignment,
    site: Span,
) -> Option<Vec<Classified>> {
    let mut targets = assign.targets.as_slice();
    if targets.iter().all(|target| register_of(target).is_none())
        && register_of(&assign.source).is_none()
    {
        return without_a_register(resolver, assign, site);
    }

    // A retired register spelling reaches lowering as an *address*, because
    // an unknown identifier in an operand slot is one. Asked here, before any
    // of the rules below: each of them has a true answer about the operand it
    // looks at, and none of them is the answer to `c = s` (decision 33).
    if let Some(diagnostic) = std::iter::once(&assign.source)
        .chain(targets.iter())
        .find_map(retired_operand)
    {
        resolver.report(diagnostic);
        return None;
    }

    let mut steps = Vec::new();
    // Where the value is right now.
    let mut carrier = match register_of(&assign.source) {
        // `MEM = a`, `y = x = a`: it starts in a register.
        Some(register) => register,
        // `a = mem`, `MEM = a = #1`: it starts in memory or as a constant, so
        // the rightmost target is the register that takes it, and the load
        // consumes that target.
        None => {
            let (destination, rest) = targets.split_last().expect("a chain has a target");
            let register = register_of(destination).expect("a chain with no register is handled");
            let mnemonic = load(register).or_else(|| {
                resolver.report(no_load(register, destination));
                None
            })?;
            steps.push(operand_step(
                resolver,
                mnemonic,
                &assign.source,
                Role::Source,
                site,
            )?);
            targets = rest;
            register
        }
    };

    // The operand immediately to the right of the target being processed,
    // which is where a two-step route would be written.
    let mut right = &assign.source;
    for target in targets.iter().rev() {
        match register_of(target) {
            // `a = a`: the value is already there.
            Some(destination) if destination == carrier => {}
            Some(destination) => {
                let mnemonic = transfer(carrier, destination).or_else(|| {
                    resolver.report(no_transfer(carrier, destination, target, right));
                    None
                })?;
                steps.push(implied_step(resolver, mnemonic, site)?);
                carrier = destination;
            }
            None => {
                let mnemonic = store(carrier).or_else(|| {
                    resolver.report(no_store(carrier, target));
                    None
                })?;
                steps.push(operand_step(
                    resolver,
                    mnemonic,
                    target,
                    Role::Target,
                    site,
                )?);
            }
        }
        right = target;
    }
    Some(steps)
}

/// `a~b` and `b~a` — the one exchange the 65816 has.
fn exchange_halves(
    resolver: &mut Resolver,
    exchange: &HlaExchange,
    site: Span,
) -> Option<Vec<Classified>> {
    let halves = [exchange.left.kind, exchange.right.kind];
    if !halves.contains(&RegisterKind::A) || !halves.contains(&RegisterKind::B) {
        resolver.report(
            unavailable(
                format!(
                    "the 65816 cannot exchange `{}` with `{}`",
                    exchange.left.kind.k65(),
                    exchange.right.kind.k65()
                ),
                exchange.left.span.join(exchange.right.span),
                "no instruction swaps these two",
            )
            .with_help("`XBA` swaps the accumulator halves, written `a~b` or `b~a`"),
        );
        return None;
    }
    Some(vec![resolver.classify_implied(Mnemonic::Xba, site)?])
}

/// `a+mem`, `a-#1`, `a&mask`, the compares `a?imm` / `x?imm` / `y?imm`, and
/// the `BIT` form `a&?mem`.
///
/// Which mnemonic a spelling selects is `(register, operator)` and nothing
/// else; the operand goes through the classifier, so which *mode* it selects
/// is the same question `adc` asks.
fn alu(resolver: &mut Resolver, arithmetic: &HlaArithmetic, site: Span) -> Option<Vec<Classified>> {
    let register = arithmetic.register.kind;
    let mnemonic = match (register, arithmetic.op) {
        (RegisterKind::A, ArithmeticOp::Add) => Mnemonic::Adc,
        (RegisterKind::A, ArithmeticOp::Subtract) => Mnemonic::Sbc,
        (RegisterKind::A, ArithmeticOp::And) => Mnemonic::And,
        (RegisterKind::A, ArithmeticOp::Or) => Mnemonic::Ora,
        (RegisterKind::A, ArithmeticOp::Xor) => Mnemonic::Eor,
        (RegisterKind::A, ArithmeticOp::BitTest) => Mnemonic::Bit,
        (RegisterKind::A, ArithmeticOp::Compare) => Mnemonic::Cmp,
        (RegisterKind::X, ArithmeticOp::Compare) => Mnemonic::Cpx,
        (RegisterKind::Y, ArithmeticOp::Compare) => Mnemonic::Cpy,
        _ => {
            let operator = arithmetic.op.k65();
            let mut diagnostic = unavailable(
                format!(
                    "the 65816 has no `{operator}` instruction for `{}`",
                    register.k65()
                ),
                arithmetic.op_span,
                "this register has no arithmetic of its own",
            );
            diagnostic = if arithmetic.op == ArithmeticOp::Compare {
                diagnostic.with_help("only `a`, `x`, and `y` compare")
            } else {
                diagnostic.with_help(format!(
                    "only the accumulator has `{operator}`; load it first, e.g. `a = {}`",
                    register.k65()
                ))
            };
            resolver.report(diagnostic);
            return None;
        }
    };
    Some(vec![operand_step(
        resolver,
        mnemonic,
        &arithmetic.operand,
        Role::Source,
        site,
    )?])
}

/// `a<<`, `mem>>`, `mem,x>>>` — the shifts and rotates.
fn shift_or_rotate(
    resolver: &mut Resolver,
    shift: &HlaShift,
    site: Span,
) -> Option<Vec<Classified>> {
    let mnemonic = match shift.op {
        ShiftOp::Left => Mnemonic::Asl,
        ShiftOp::Right => Mnemonic::Lsr,
        ShiftOp::RotateLeft => Mnemonic::Rol,
        ShiftOp::RotateRight => Mnemonic::Ror,
    };
    // The accumulator shifts, and so does memory. Any other register would
    // have to go through one of them first.
    if let Some(register) = register_of(&shift.target)
        && register != RegisterKind::A
    {
        resolver.report(
            unavailable(
                format!("the 65816 cannot shift `{}`", register.k65()),
                shift.target.span,
                "no instruction shifts this register",
            )
            .with_help("only the accumulator and memory shift"),
        );
        return None;
    }
    Some(vec![operand_step(
        resolver,
        mnemonic,
        &shift.target,
        Role::Target,
        site,
    )?])
}

/// `x++`, `y--`, `mem++`, `mem,x--` — increment and decrement by one.
///
/// The index registers have instructions of their own; the accumulator and
/// memory share `INC`/`DEC`, which is the 65C02 addition for the accumulator
/// form.
fn step_by_one(resolver: &mut Resolver, step: &HlaStep, site: Span) -> Option<Vec<Classified>> {
    let increment = step.op == StepOp::Increment;
    if let Some(register) = register_of(&step.target) {
        let mnemonic = match (register, increment) {
            (RegisterKind::X, true) => Mnemonic::Inx,
            (RegisterKind::X, false) => Mnemonic::Dex,
            (RegisterKind::Y, true) => Mnemonic::Iny,
            (RegisterKind::Y, false) => Mnemonic::Dey,
            (RegisterKind::A, true) => Mnemonic::Inc,
            (RegisterKind::A, false) => Mnemonic::Dec,
            _ => {
                resolver.report(
                    unavailable(
                        format!(
                            "the 65816 cannot {} `{}`",
                            if increment { "increment" } else { "decrement" },
                            register.k65()
                        ),
                        step.op_span,
                        "no instruction steps this register",
                    )
                    .with_help("only `a`, `x`, `y`, and memory step by one"),
                );
                return None;
            }
        };
        // `a++` is the accumulator form of `INC`; the index registers have
        // implied instructions that take no operand at all.
        return Some(vec![if register == RegisterKind::A {
            operand_step(resolver, mnemonic, &step.target, Role::Target, site)?
        } else {
            resolver.classify_implied(mnemonic, site)?
        }]);
    }
    let mnemonic = if increment {
        Mnemonic::Inc
    } else {
        Mnemonic::Dec
    };
    Some(vec![operand_step(
        resolver,
        mnemonic,
        &step.target,
        Role::Target,
        site,
    )?])
}

/// `c+`, `c-`, `d+`, `d-`, `i+`, `i-`, `v-` — the flag shorthands.
///
/// `d` here is the decimal *flag*, not the direct page register `d` of the
/// stack shorthands: the suffix decides which one the letter means.
fn set_or_clear_flag(
    resolver: &mut Resolver,
    flag: &HlaFlag,
    site: Span,
) -> Option<Vec<Classified>> {
    let mnemonic = match (flag.flag, flag.set) {
        (FlagKind::Carry, true) => Mnemonic::Sec,
        (FlagKind::Carry, false) => Mnemonic::Clc,
        (FlagKind::Decimal, true) => Mnemonic::Sed,
        (FlagKind::Decimal, false) => Mnemonic::Cld,
        (FlagKind::Interrupt, true) => Mnemonic::Sei,
        (FlagKind::Interrupt, false) => Mnemonic::Cli,
        (FlagKind::Overflow, false) => Mnemonic::Clv,
        // The 65816 has `CLV` and no `SEV`: the overflow flag is set by the
        // hardware and by `BIT`, never by a flag instruction.
        (FlagKind::Overflow, true) => {
            resolver.report(
                unavailable(
                    "the 65816 cannot set the overflow flag".to_owned(),
                    flag.flag_span,
                    "there is no `SEV` instruction",
                )
                .with_help(
                    "only `v-` exists; the overflow flag is set by arithmetic or by `a&?mem`",
                ),
            );
            return None;
        }
    };
    Some(vec![resolver.classify_implied(mnemonic, site)?])
}

/// `a!!`, `x??`, `p!!`, `b!!`, `d??` — push and pull.
///
/// `dbr` and `dpr` are the data-bank and direct-page registers: `PHB` pushes
/// the first and `PHD` the second (`doc/syntax-reference.md`, "65816 Bank
/// Registers and Mode Switch"). They were spelled `b` and `d` until
/// `plans/06-compatibility.md` decisions 29 and 30 — `b` names the
/// accumulator's high half, which the stack cannot move on its own, and `d`
/// is the decimal flag.
fn push_or_pull(resolver: &mut Resolver, stack: &HlaStack, site: Span) -> Option<Vec<Classified>> {
    let push = stack.op == StackOp::Push;
    let register = stack.register.kind;
    let mnemonic = match (register, push) {
        (RegisterKind::A, true) => Mnemonic::Pha,
        (RegisterKind::A, false) => Mnemonic::Pla,
        (RegisterKind::X, true) => Mnemonic::Phx,
        (RegisterKind::X, false) => Mnemonic::Plx,
        (RegisterKind::Y, true) => Mnemonic::Phy,
        (RegisterKind::Y, false) => Mnemonic::Ply,
        (RegisterKind::P, true) => Mnemonic::Php,
        (RegisterKind::P, false) => Mnemonic::Plp,
        (RegisterKind::Dbr, true) => Mnemonic::Phb,
        (RegisterKind::Dbr, false) => Mnemonic::Plb,
        (RegisterKind::Dpr, true) => Mnemonic::Phd,
        (RegisterKind::Dpr, false) => Mnemonic::Pld,
        // `b` reaches here because it is still a register token, for `a~b`.
        // It is the accumulator's high half, which no stack instruction moves
        // on its own — and the register `b!!` used to push is `dbr`, so the
        // spelling gets its replacement named rather than a bare refusal.
        (RegisterKind::B, _) => {
            resolver.report(
                Diagnostic::error(
                    DiagnosticCode::RETIRED_REGISTER_SPELLING,
                    format!("`b` is not a stack target, so it cannot be {}ed", if push { "push" } else { "pull" }),
                    Label::new(stack.op_span, "`b` is the accumulator's high half"),
                )
                .with_help("to move the data bank, write `dbr!!` / `dbr??`")
                .with_note(
                    "`b` names one half of the accumulator and exists for `a~b` alone; \
                     `PHB` pushes the data bank, which is a different register",
                )
                .with_docs("doc/syntax-reference.md#registers"),
            );
            return None;
        }
        _ => {
            resolver.report(
                unavailable(
                    format!(
                        "the 65816 cannot {} `{}`",
                        if push { "push" } else { "pull" },
                        register.k65()
                    ),
                    stack.op_span,
                    "no instruction moves this register through the stack",
                )
                .with_help("`a`, `x`, `y`, `p`, `dbr`, and `dpr` have push and pull"),
            );
            return None;
        }
    };
    Some(vec![resolver.classify_implied(mnemonic, site)?])
}

/// `*` and `* N` — one `NOP`, or N of them.
fn nops(resolver: &mut Resolver, count: Option<&Expr>, site: Span) -> Option<Vec<Classified>> {
    let count = match count {
        None => 1,
        Some(expr) => resolver.constant(expr, "NOP counts")?,
    };
    Some(vec![
        resolver
            .classify_implied(Mnemonic::Nop, site)?
            .repeated(count),
    ])
}

/// `%` — the one-character breakpoint, which is `brk #0`.
fn breakpoint(resolver: &mut Resolver, site: Span) -> Option<Vec<Classified>> {
    // `BRK` has no implied form in the ISA table: the signature byte is part
    // of the instruction, and `%` is the spelling that leaves it zero.
    let zero = Expr {
        kind: ExprKind::Number(0),
        span: site,
    };
    Some(vec![resolver.classify_source(
        Mnemonic::Brk,
        crate::resolve::OperandSource::Immediate(&zero),
        site,
    )?])
}

/// A chain that names no register at all: either the zero store, or a step
/// the 65816 has no instruction for.
fn without_a_register(
    resolver: &mut Resolver,
    assign: &HlaAssignment,
    site: Span,
) -> Option<Vec<Classified>> {
    // The shortcut is a store of zero into a declared *name*. A bare address
    // literal does not take it — `$10 = 0` is not a spelling of `stz $10`
    // (`doc/syntax-reference.md`, "Zero-Store Shortcut") — and neither does
    // arithmetic over a name: `mem+1 = 0` is a place, but the documented rule
    // is a variable identifier, indexed or accessed, and not an expression
    // K816 happens to be able to address. Nor does a non-zero constant, which
    // has no instruction and must name the register that carries it.
    let named = assign
        .targets
        .iter()
        .all(|target| resolver.names_a_place(target));
    let zero = resolver
        .hla_value(&assign.source)
        .and_then(|value| resolver.fold(value))
        .and_then(|folded| folded.value())
        .is_some_and(|value| value.get() == 0);
    if !named || !zero {
        resolver.report(needs_a_register(assign));
        return None;
    }

    // `mem = 0` is an `stz`, with no accumulator traffic to undo
    // (`doc/syntax-reference.md`, "Zero-Store Shortcut"). Where `stz` has no
    // form for the target — it has neither `,y` nor a long one — the store
    // goes through the accumulator instead, and silently: the author wrote a
    // store of zero and gets one.
    let mut steps = Vec::new();
    let mut accumulator_holds_zero = false;
    for target in assign.targets.iter().rev() {
        if let Some(stz) = try_target_step(resolver, Mnemonic::Stz, target, site) {
            steps.push(stz);
            continue;
        }
        if !accumulator_holds_zero {
            steps.push(immediate_step(
                resolver,
                Mnemonic::Lda,
                &assign.source,
                site,
            )?);
            accumulator_holds_zero = true;
        }
        steps.push(operand_step(
            resolver,
            Mnemonic::Sta,
            target,
            Role::Target,
            site,
        )?);
    }
    Some(steps)
}

/// Which side of the `=` an operand sits on, which is what decides whether a
/// bare constant is a value to load or the address of a place to store into.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Role {
    Source,
    Target,
}

/// One step whose operand is written in the source.
fn operand_step(
    resolver: &mut Resolver,
    mnemonic: Mnemonic,
    operand: &HlaOperand,
    role: Role,
    site: Span,
) -> Option<Classified> {
    let source = match role {
        Role::Source => resolver.hla_source(operand)?,
        Role::Target => resolver.hla_place(operand)?,
    };
    resolver.classify_source(mnemonic, source, site)
}

/// The same, for a mnemonic that may legitimately have no form for this
/// operand and whose caller has a fallback.
fn try_target_step(
    resolver: &mut Resolver,
    mnemonic: Mnemonic,
    operand: &HlaOperand,
    site: Span,
) -> Option<Classified> {
    let source = resolver.hla_place(operand)?;
    resolver.try_classify_source(mnemonic, source, site)
}

/// One step that loads a constant the chain wrote without a `#`.
fn immediate_step(
    resolver: &mut Resolver,
    mnemonic: Mnemonic,
    operand: &HlaOperand,
    site: Span,
) -> Option<Classified> {
    let value = resolver.hla_value(operand)?;
    resolver.classify_source(
        mnemonic,
        crate::resolve::OperandSource::Immediate(value),
        site,
    )
}

/// One step with no operand at all: the transfers.
fn implied_step(resolver: &mut Resolver, mnemonic: Mnemonic, site: Span) -> Option<Classified> {
    resolver.classify_implied(mnemonic, site)
}

/// The register an operand names, or `None` when it names memory or a value.
fn register_of(operand: &HlaOperand) -> Option<RegisterKind> {
    match &operand.kind {
        HlaOperandKind::Register(register) => Some(register.kind),
        _ => None,
    }
}

/// The instruction that loads `register` from memory, for the three registers
/// that have one.
fn load(register: RegisterKind) -> Option<Mnemonic> {
    match register {
        RegisterKind::A => Some(Mnemonic::Lda),
        RegisterKind::X => Some(Mnemonic::Ldx),
        RegisterKind::Y => Some(Mnemonic::Ldy),
        _ => None,
    }
}

/// The instruction that stores `register` to memory.
fn store(register: RegisterKind) -> Option<Mnemonic> {
    match register {
        RegisterKind::A => Some(Mnemonic::Sta),
        RegisterKind::X => Some(Mnemonic::Stx),
        RegisterKind::Y => Some(Mnemonic::Sty),
        _ => None,
    }
}

/// Every register-to-register move the 65816 has, as data rather than as a
/// match arm per pair (sub-plan E, decision 3).
///
/// `doc/syntax-reference.md`'s "Complete Register Transfer Table" is the
/// source-level statement of this, and `doc/w65c816s.md` the hardware one.
/// `b~a` is deliberately absent: `XBA` exchanges the accumulator halves
/// rather than copying one into the other, so it is spelled `~` and is not a
/// chain step.
const TRANSFERS: &[(RegisterKind, RegisterKind, Mnemonic)] = &[
    (RegisterKind::A, RegisterKind::X, Mnemonic::Tax),
    (RegisterKind::A, RegisterKind::Y, Mnemonic::Tay),
    (RegisterKind::X, RegisterKind::A, Mnemonic::Txa),
    (RegisterKind::Y, RegisterKind::A, Mnemonic::Tya),
    (RegisterKind::X, RegisterKind::Y, Mnemonic::Txy),
    (RegisterKind::Y, RegisterKind::X, Mnemonic::Tyx),
    (RegisterKind::X, RegisterKind::S, Mnemonic::Txs),
    (RegisterKind::S, RegisterKind::X, Mnemonic::Tsx),
    // The four that move all sixteen bits whatever `M` says. They are spelled
    // with the accumulator's one name: `a` *is* the accumulator at both
    // widths, and what these move is the register, not the low byte
    // (`plans/06-compatibility.md` decision 29).
    (RegisterKind::A, RegisterKind::Dpr, Mnemonic::Tcd),
    (RegisterKind::Dpr, RegisterKind::A, Mnemonic::Tdc),
    (RegisterKind::A, RegisterKind::S, Mnemonic::Tcs),
    (RegisterKind::S, RegisterKind::A, Mnemonic::Tsc),
];

/// The instruction that moves `from` into `to`, if the CPU has one.
fn transfer(from: RegisterKind, to: RegisterKind) -> Option<Mnemonic> {
    TRANSFERS
        .iter()
        .find(|(source, destination, _)| *source == from && *destination == to)
        .map(|(_, _, mnemonic)| *mnemonic)
}

/// The register a two-step route would pass through, when a direct transfer
/// does not exist but a pair of them does: `s = y` becomes `s = x = y`.
fn two_step(from: RegisterKind, to: RegisterKind) -> Option<RegisterKind> {
    TRANSFERS
        .iter()
        .filter(|(source, _, _)| *source == from)
        .map(|(_, middle, _)| *middle)
        .find(|middle| transfer(*middle, to).is_some())
}

/// The retired-spelling refusal for an operand that is one, if it is.
///
/// `c` and `d` are ordinary identifiers now, so an operand written with
/// either arrives as an address naming nothing. `resolve` catches the general
/// case; this catches it one stage earlier, where the statement is rejected
/// for a different reason before the name is ever looked up.
fn retired_operand(target: &HlaOperand) -> Option<Diagnostic> {
    let HlaOperandKind::Memory(address) = &target.kind else {
        return None;
    };
    let crate::ast::ExprKind::Symbol(name) = &address.value.kind else {
        return None;
    };
    crate::compat::retired_register_spelling(name.span, &name.text)
}

/// A register memory cannot be loaded into.
///
/// Only `dpr` and `s` are in that position, and both are reachable from the
/// accumulator, whose transfer moves all sixteen bits of it.
fn no_load(register: RegisterKind, operand: &HlaOperand) -> Diagnostic {
    let diagnostic = no_instruction(
        format!("no instruction loads `{}`", register.k65()),
        Label::new(operand.span, "nothing loads this register from memory"),
        None,
    );
    if transfer(RegisterKind::A, register).is_none() {
        return diagnostic;
    }
    diagnostic.with_help(format!(
        "load the accumulator, then transfer it: `a = ...` and `{} = a`",
        register.k65()
    ))
}

/// A register whose value memory cannot be stored from.
fn no_store(register: RegisterKind, operand: &HlaOperand) -> Diagnostic {
    let diagnostic = no_instruction(
        format!("no instruction stores `{}`", register.k65()),
        Label::new(operand.span, "nothing stores this register to memory"),
        None,
    );
    if transfer(register, RegisterKind::A).is_none() {
        return diagnostic;
    }
    diagnostic.with_help(format!(
        "transfer it into the accumulator first: `a = {}` and then store `a`",
        register.k65()
    ))
}

fn no_transfer(
    from: RegisterKind,
    to: RegisterKind,
    target: &HlaOperand,
    right: &HlaOperand,
) -> Diagnostic {
    // `b` is the accumulator's high half and an `a~b` operand only: there is
    // no instruction that moves a value into or out of it on its own, because
    // `XBA` swaps the halves of one register rather than copying between two
    // (`plans/06-compatibility.md` decision 30). Named, so the author is not
    // left reading a generic refusal about a register that half-exists.
    if from == RegisterKind::B || to == RegisterKind::B {
        return Diagnostic::error(
            DiagnosticCode::RETIRED_REGISTER_SPELLING,
            "`b` is not a transfer operand",
            Label::new(target.span, "`b` is the accumulator's high half"),
        )
        .with_note(
            "`XBA` swaps the accumulator's two halves rather than copying one register \
             into another, so `b` exists for that one statement and nothing else",
        )
        .with_help("write `a~b` to swap the halves, or `dbr` for the data bank")
        .with_docs("doc/syntax-reference.md#registers");
    }
    let diagnostic = no_instruction(
        format!(
            "the 65816 cannot transfer `{}` to `{}`",
            from.k65(),
            to.k65()
        ),
        Label::new(target.span, "no instruction moves the value here"),
        None,
    );
    // The route is suggested, never inserted: an inserted transfer clobbers
    // the register it passes through, which is the damage `W0123` exists to
    // report. K816 does not author a bug it also warns about.
    let Some(middle) = two_step(from, to) else {
        return diagnostic;
    };
    diagnostic
        .with_help(format!(
            "route it through `{}`: write `{} = {} = {}`",
            middle.k65(),
            to.k65(),
            middle.k65(),
            from.k65()
        ))
        .with_fix(Fix {
            title: format!("transfer through `{}`", middle.k65()),
            applicability: Applicability::MaybeIncorrect,
            edits: vec![TextEdit::insert(right.span, format!("{} = ", middle.k65()))],
        })
}

fn needs_a_register(assign: &HlaAssignment) -> Diagnostic {
    let target = assign.targets.last().expect("a chain has a target");
    no_instruction(
        "this store names no register to carry the value".to_owned(),
        Label::new(
            assign.source.span,
            "nothing moves this into memory by itself",
        ),
        Some(Label::new(target.span, "stored here")),
    )
    .with_help("carry it in a register, e.g. `mem = a = value`")
    .with_note(
        "only a store of zero needs no register, and only when the target is a \
         declared name rather than a bare address",
    )
}

/// A shorthand the 65816 has no instruction for.
///
/// The same code as a chain step with no instruction: from where the author
/// sits, `s=y` and `v+` are one complaint — the spelling is legal K65 and the
/// CPU cannot do it — and the specifics belong in the labels rather than in a
/// second code.
fn unavailable(title: String, span: Span, label: &str) -> Diagnostic {
    Diagnostic::error(
        DiagnosticCode::NO_SUCH_INSTRUCTION,
        title,
        Label::new(span, label),
    )
    .with_docs("doc/syntax-reference.md#standard-6502-instructions")
}

fn no_instruction(title: String, primary: Label, secondary: Option<Label>) -> Diagnostic {
    let mut diagnostic = Diagnostic::error(DiagnosticCode::NO_SUCH_INSTRUCTION, title, primary)
        .with_docs("doc/syntax-reference.md#assignment-chaining");
    if let Some(secondary) = secondary {
        diagnostic = diagnostic.with_secondary(secondary);
    }
    diagnostic
}

// ---------------------------------------------------------------- branches

/// Which way a form reads the condition written on it.
///
/// The inversion is a property of the *form*, not of the condition
/// (`doc/syntax-reference.md`, "Branch Operator Mapping", and sub-plan E
/// decision 4).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BranchSense {
    /// `< goto label`, `{ … } <`, `< break` — branch when the condition
    /// holds.
    Take,
    /// `>= { … }` and `else` — the branch goes *past* the body, so it tests
    /// the complement of what is written: `>= { … }` runs its body when the
    /// carry is set, and the instruction that skips it is `BCC`.
    Skip,
}

/// The branch instructions a condition means, in the sense the form gives it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BranchPlan {
    /// One branch to the target.
    Single(Mnemonic),
    /// Two branches to the same target. Unsigned `<=` is "carry clear or
    /// equal" and the CPU has no single instruction for it.
    Pair(Mnemonic, Mnemonic),
    /// A branch over the branch that takes the target. Unsigned `>` is
    /// "carry set *and* not equal", so an equality test guards it.
    Guarded { over: Mnemonic, take: Mnemonic },
}

/// The branches `condition` lowers to.
///
/// `doc/k65/instructions.md` is the authority that settles the direction:
/// `BCC` lists `>={ … }`, `< goto label`, `{ … } <`, and `c+?{ … }` among its
/// spellings, so a prefix block names the condition its *body* runs under and
/// K816 emits the complement to skip it.
pub fn branch(condition: BranchConditionKind, sense: BranchSense) -> BranchPlan {
    let taken = match sense {
        BranchSense::Take => condition,
        BranchSense::Skip => negate(condition),
    };
    match taken {
        BranchConditionKind::Comparison(ComparisonKind::Greater) => BranchPlan::Guarded {
            over: Mnemonic::Beq,
            take: Mnemonic::Bcs,
        },
        BranchConditionKind::Comparison(ComparisonKind::LessEqual) => {
            BranchPlan::Pair(Mnemonic::Bcc, Mnemonic::Beq)
        }
        simple => BranchPlan::Single(single(simple)),
    }
}

/// The condition that holds exactly when `condition` does not.
fn negate(condition: BranchConditionKind) -> BranchConditionKind {
    use ComparisonKind as C;
    match condition {
        BranchConditionKind::Comparison(comparison) => {
            BranchConditionKind::Comparison(match comparison {
                C::Less => C::GreaterEqual,
                C::GreaterEqual => C::Less,
                C::Equal => C::NotEqual,
                C::NotEqual => C::Equal,
                C::Negative => C::NotNegative,
                C::NotNegative => C::Negative,
                C::OverflowSet => C::OverflowClear,
                C::OverflowClear => C::OverflowSet,
                C::Greater => C::LessEqual,
                C::LessEqual => C::Greater,
            })
        }
        BranchConditionKind::Flag { flag, set } => BranchConditionKind::Flag { flag, set: !set },
    }
}

/// The one branch that tests `condition`, for the eight that have one.
///
/// `>` and `<=` are not among them and are handled by their compound plans;
/// they are the forms `doc/syntax-reference.md` warns about the cost of.
fn single(condition: BranchConditionKind) -> Mnemonic {
    use ComparisonKind as C;
    match condition {
        BranchConditionKind::Comparison(C::Less) => Mnemonic::Bcc,
        BranchConditionKind::Comparison(C::GreaterEqual) => Mnemonic::Bcs,
        BranchConditionKind::Comparison(C::Equal) => Mnemonic::Beq,
        BranchConditionKind::Comparison(C::NotEqual) => Mnemonic::Bne,
        BranchConditionKind::Comparison(C::Negative) => Mnemonic::Bmi,
        BranchConditionKind::Comparison(C::NotNegative) => Mnemonic::Bpl,
        BranchConditionKind::Comparison(C::OverflowSet) => Mnemonic::Bvs,
        BranchConditionKind::Comparison(C::OverflowClear) => Mnemonic::Bvc,
        BranchConditionKind::Flag { flag, set } => match (flag, set) {
            (TestableFlag::Carry, true) => Mnemonic::Bcs,
            (TestableFlag::Carry, false) => Mnemonic::Bcc,
            (TestableFlag::Zero, true) => Mnemonic::Beq,
            (TestableFlag::Zero, false) => Mnemonic::Bne,
            (TestableFlag::Negative, true) => Mnemonic::Bmi,
            (TestableFlag::Negative, false) => Mnemonic::Bpl,
            (TestableFlag::Overflow, true) => Mnemonic::Bvs,
            (TestableFlag::Overflow, false) => Mnemonic::Bvc,
        },
        BranchConditionKind::Comparison(C::Greater | C::LessEqual) => {
            unreachable!("the compound comparisons have their own plans")
        }
    }
}
