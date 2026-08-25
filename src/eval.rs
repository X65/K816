/*****************************************************************************/
/*                                                                           */
/*                                  eval.rs                                  */
/*                                                                           */
/*                       K65's compile-time evaluator                        */
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

//! The compile-time evaluator: `[ … ]` in every position the grammar accepts
//! one, and the language inside it.
//!
//! K65's evaluator is a small dynamically-typed interpreter over numbers and
//! strings, with variables, statements, and a function library. The original
//! compiles each bracket to a stack machine and runs it
//! (`compiler.inc:341-362`); K816 walks the tree instead, which is invisible
//! from outside because the answers are what a program can see.
//!
//! **The numeric model is measured, not chosen.** The arithmetic is `f64`
//! and both ends are `f32`: a literal is parsed as `float`, and the result of
//! an evaluation is narrowed to `float` before it is rounded. So the one rule
//! at every integer boundary — an operand, a data entry, an initializer, an
//! `align`, a `for` bound — is [`to_integer`], and after it the context's own
//! width and range check applies unchanged. `plans/postmortem.md` §G.2.1 and
//! `tests/compat/k65/eval-integer-boundary/`.
//!
//! Three things the original's evaluator does *not* have, all measured:
//! there is no `abs`, `||` is not a token (`EvLogOr` and `EvLogAnd` are both
//! spelled `&&`), and `%` is a non-negative modulo. K816 has the first two as
//! extensions — refusing a spelling this document describes would buy nothing
//! — and matches the third exactly. §G.2.4.
//!
//! What is *not* here is what needs a section's bytes: `index`, a subscript,
//! and `size`. The original reads the assembled bytes of the section a data
//! label names, which K816 cannot do before lowering, and every one of those
//! forms is sub-plan G6's — the task that already owns section bytes for
//! `binary` and the `evaluator [ ]` transform.

use std::collections::{BTreeMap, HashMap};

use crate::ast::{EvalCallName, EvalExpr, EvalExprKind, Expr, ExprKind, Name, ReservedCall};
use crate::source::Span;
use crate::symbol::{SymbolId, SymbolKind, SymbolTable};

/// How many expression nodes one evaluation may visit.
///
/// The bound exists because `while` and `evalfunc` recursion are both real in
/// K816 and neither is bounded by the source's shape. The number is large
/// enough that no table generator will reach it — the widest loop in the
/// corpus runs 256 iterations of a dozen nodes — and small enough that a
/// runaway loop stops in well under a second.
pub const STEP_BUDGET: u64 = 2_000_000;

/// How deep `evalfunc` calls may nest.
///
/// The step budget does not bound this on its own, and the difference is not
/// academic: `evalfunc down(k) [ down(k) ]` spends one step per node and
/// exhausts the *interpreter's own* stack in a few thousand calls, long
/// before two million steps — an abort with no diagnostic instead of an
/// error. Two limits, because there are two resources.
///
/// The original has no working recursion at all
/// (`plans/postmortem.md` §G.2.3), so anything here is headroom rather than a
/// compatibility question.
///
/// The number is set by the smallest stack the compiler runs on rather than
/// by taste. A `cargo test` thread gets 2 MiB by default, and one level of
/// this interpreter costs a few kilobytes of it in a debug build — enough
/// that 256 aborts there while passing in the release binary. A limit that
/// holds only in the configuration you happened to measure is not a limit, so
/// it is sixty-four, which is deeper than a compile-time table generator has
/// reason to go and survives everywhere.
pub const MAX_CALL_DEPTH: usize = 64;

/// What `pi` is worth in the original: the literal `3.141593`, six decimals
/// and no more (`compiler.inc:438`).
///
/// Deliberately not `std::f32::consts::PI`, which is why the lint is turned
/// off here rather than the value corrected. The reference's own example
/// generates a 214-entry sine table with `sin(x/212*pi*2)`, and a table
/// generated from a more accurate constant is a table with different bytes.
#[allow(clippy::approx_constant)]
pub const K65_PI: f32 = 3.141_593;

/// Which evaluator bracket a name was first assigned in.
///
/// `doc/syntax-reference.md` makes a name write-once outside the block that
/// introduced it, so the binding has to remember where it came from. The
/// original does not enforce this — `[N = 1] [N = 2]` compiles there and `N`
/// is 2 — which makes the rule an `intentionally-changed` row rather than a
/// compatibility one.
pub type BlockId = u32;

/// The block a `for … eval` body counts as when it runs.
///
/// Every ordinary bracket gets a number from the pass that walks the file, in
/// source order. A loop body runs later and out of that sequence, so it is
/// given a number no bracket has: an assignment in one writes to the scratch
/// copy of the environment the loop runs against, which is discarded, and it
/// can never be the block that owns a name.
pub const LOOP_BLOCK: BlockId = BlockId::MAX;

// ---------------------------------------------------------------------------
// Values
// ---------------------------------------------------------------------------

/// A value in the evaluator's language.
///
/// Three types, exactly as the original has (`eval.h`): a number, a string,
/// and a *section* — what a data label evaluates to. A section is not a
/// number and never becomes one; the only things that consume one are the
/// section functions, which is why one reaching an integer boundary is an
/// error rather than a conversion.
#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Number(f64),
    Str(String),
    Section(SymbolId),
}

impl Value {
    /// The number this is, or `None` when it is not one.
    pub fn number(&self) -> Option<f64> {
        match self {
            Self::Number(value) => Some(*value),
            _ => None,
        }
    }

    /// Truth, as `EvalAny::IsTrue` defines it: a non-zero number, a non-empty
    /// string, and any section.
    pub fn is_true(&self) -> bool {
        match self {
            Self::Number(value) => *value != 0.0,
            Self::Str(text) => !text.is_empty(),
            Self::Section(_) => true,
        }
    }

    /// What this value contributes to a string, which is `%lf` for a number.
    ///
    /// The formatting is the original's and is observable: it is what `+`
    /// splices and what `print` and `error` carry, so `print(3)` says
    /// `3.000000` in both compilers.
    fn as_text(&self, symbols: &SymbolTable) -> String {
        match self {
            Self::Number(value) => format!("{value:.6}"),
            Self::Str(text) => text.clone(),
            Self::Section(id) => symbols.name(*id).to_owned(),
        }
    }

    /// A phrase naming this value's type, for a diagnostic.
    pub fn describe(&self) -> &'static str {
        match self {
            Self::Number(_) => "a number",
            Self::Str(_) => "a string",
            Self::Section(_) => "a data section",
        }
    }
}

/// The one integer boundary rule: `i64(floor(f32(v) + 0.5f32))`.
///
/// Measured rather than chosen. `Eval_Exec` returns a `float` and `doeval`
/// rounds it with `int(floor(x + .5f))` (`compiler_src.cpp:392`), so the
/// narrowing to single precision happens *before* the rounding — which is why
/// `[8388609]`, one past the last integer a `float` holds exactly, comes out
/// as 8388610. The rounding is half *up*, not away from zero and not to even:
/// `-2.5` is `-2`.
///
/// The context's own width and range check then applies to the result,
/// unchanged. The original wraps a value that does not fit and says nothing;
/// K816 keeps its check, which is decision 4's one deliberate difference.
pub fn to_integer(value: f64) -> i64 {
    let narrowed = value as f32 + 0.5f32;
    f64::from(narrowed).floor() as i64
}

/// The integer a *bitwise* operand becomes, which is not [`to_integer`].
///
/// The original's `EvalAny::ToInt` is `int(floor(x) + .5f)` (`eval.h:28`),
/// which truncates toward zero for every value except an exact negative
/// integer — where `floor(x) + 0.5` crosses back over zero and the result is
/// one *too high*: `(0 - 2) | 0` is `-1` there, and `(0 - 1) << 4` is `0`.
/// K816 truncates toward zero throughout, so it agrees with the original
/// everywhere the original is self-consistent and differs only on the defect.
/// The manifest carries that as `legacy-defective`.
fn to_bits(value: f64) -> i64 {
    value.trunc() as i64
}

// ---------------------------------------------------------------------------
// Environment and functions
// ---------------------------------------------------------------------------

#[derive(Clone, Debug)]
struct Binding {
    value: Value,
    block: BlockId,
}

/// The evaluator variables a unit has assigned so far, in source order.
///
/// One namespace with the compiler's own: a bracket reads a `const` by name
/// because the original's `getevalvar` resolves against the one symbol table
/// the compiler keeps (`compiler_src.cpp:288`), and an evaluator assignment
/// declares a name the K65 layer can then read as a constant. That is
/// measured, so K816 doing it is compatibility rather than an extension, and
/// it is why the legacy compiler's dual parse has nothing left to rescue.
#[derive(Clone, Debug, Default)]
pub struct Environment {
    values: BTreeMap<String, Binding>,
    /// Names assigned since the last [`Self::take_new_names`], in assignment
    /// order — what the driver declares as compiler constants.
    fresh: Vec<String>,
}

impl Environment {
    pub fn get(&self, name: &str) -> Option<&Value> {
        self.values.get(name).map(|binding| &binding.value)
    }

    /// The names assigned since this was last called, and their values.
    pub fn take_new_names(&mut self) -> Vec<(String, Value)> {
        std::mem::take(&mut self.fresh)
            .into_iter()
            .filter_map(|name| {
                let value = self.values.get(&name)?.value.clone();
                Some((name, value))
            })
            .collect()
    }
}

/// A user-defined `evalfunc`, keyed by name *and* argument count.
///
/// The original encodes the count in the function's internal name
/// (`compiler.inc:465`), so `f(a)` and `f(a, b)` are two functions and
/// neither shadows the other. K816 keeps that.
#[derive(Clone, Debug)]
pub struct UserFunction {
    pub parameters: Vec<String>,
    pub body: EvalExpr,
    pub declaration: Span,
}

/// Every `evalfunc` the unit declares.
#[derive(Clone, Debug, Default)]
pub struct Functions {
    defined: BTreeMap<(String, usize), UserFunction>,
}

impl Functions {
    /// Declares `function`, or hands back the declaration that already owns
    /// the name and arity.
    pub fn declare(&mut self, name: &str, function: UserFunction) -> Result<(), Span> {
        let key = (name.to_owned(), function.parameters.len());
        match self.defined.get(&key) {
            Some(existing) => Err(existing.declaration),
            None => {
                self.defined.insert(key, function);
                Ok(())
            }
        }
    }

    /// True when the library already answers to this name and arity, which
    /// the original refuses to let a user function shadow.
    pub fn is_builtin(name: &str, arity: usize) -> bool {
        builtin_arities(name).contains(&arity)
    }

    fn lookup(&self, name: &str, arity: usize) -> Option<&UserFunction> {
        self.defined.get(&(name.to_owned(), arity))
    }
}

/// The argument counts the library answers to for `name`.
fn builtin_arities(name: &str) -> &'static [usize] {
    match name {
        "floor" | "ceil" | "round" | "frac" | "sin" | "cos" | "sqrt" | "acos" | "asin" | "abs" => {
            &[1]
        }
        "max" | "min" | "pow" => &[2],
        "clamp" => &[3],
        _ => &[],
    }
}

// ---------------------------------------------------------------------------
// Errors and notes
// ---------------------------------------------------------------------------

/// A `print(msg)` that ran, which the caller reports as a note.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Note {
    pub span: Span,
    pub message: String,
}

/// Why an evaluation did not produce a value.
///
/// Every variant names what the caller should say. None of them is "invalid
/// syntax": the grammar accepted all of this, and what stops it is either a
/// question with no answer or a construct another task owns.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Error {
    /// A name nothing in scope gives a value.
    Name { span: Span, name: String },
    /// A name that exists and is not an evaluator value — a `var`, an
    /// `inline`, a code label.
    NotAValue {
        span: Span,
        name: String,
        what: &'static str,
    },
    /// An assignment to a name the evaluator does not own: a `const`, a
    /// `var`, a label.
    NotAssignable {
        span: Span,
        name: String,
        what: &'static str,
    },
    /// A name assigned in one evaluator block and again in another.
    Reassigned {
        span: Span,
        name: String,
        first: BlockId,
    },
    /// Arithmetic with no answer: division or remainder by zero.
    Undefined { span: Span, what: &'static str },
    /// A value of the wrong type where a number is needed.
    NotANumber {
        span: Span,
        found: &'static str,
        needed: &'static str,
    },
    /// A form K816 recognises and another task implements. `what` is a plural
    /// noun phrase; `owner` names the task in the diagnostic's note.
    Deferred { span: Span, what: &'static str },
    /// `error("…")` reached in a live evaluation.
    Raised { span: Span, message: String },
    /// The step budget ran out.
    Budget { span: Span },
    /// `evalfunc` calls nested deeper than [`MAX_CALL_DEPTH`].
    TooDeep { span: Span },
    /// One of the four evaluator functions K816 does not run, reached by a
    /// spelling analysis does not walk: `x.size` is `size(x)`, and only the
    /// parenthesised spelling is a `reserved_eval_call` node.
    Refused { span: Span, name: &'static str },
    /// A construct that has *already* reported itself, so the caller has
    /// nothing to add — the four reserved evaluator functions written as
    /// calls, and a node the parser already called a syntax error.
    Reported,
}

// ---------------------------------------------------------------------------
// The interpreter
// ---------------------------------------------------------------------------

/// Evaluates one bracket against an environment.
pub struct Interpreter<'a> {
    env: &'a mut Environment,
    functions: &'a Functions,
    symbols: &'a SymbolTable,
    block: BlockId,
    /// Call frames, innermost last. A frame is an `evalfunc`'s parameters or
    /// a `for … eval` loop variable, and it shadows the environment for reads
    /// and captures writes, so neither leaks.
    frames: Vec<BTreeMap<String, Value>>,
    steps: u64,
    notes: Vec<Note>,
}

impl<'a> Interpreter<'a> {
    pub fn new(
        env: &'a mut Environment,
        functions: &'a Functions,
        symbols: &'a SymbolTable,
        block: BlockId,
    ) -> Self {
        Self {
            env,
            functions,
            symbols,
            block,
            frames: Vec::new(),
            steps: STEP_BUDGET,
            notes: Vec::new(),
        }
    }

    /// Binds `name` for the whole evaluation, as `for … eval` binds its loop
    /// variable. The binding is a frame, so it neither reads nor writes the
    /// unit's evaluator variables.
    pub fn with_local(mut self, name: &str, value: Value) -> Self {
        self.frames.push(BTreeMap::from([(name.to_owned(), value)]));
        self
    }

    /// The `print` calls this evaluation made, in order.
    pub fn take_notes(&mut self) -> Vec<Note> {
        std::mem::take(&mut self.notes)
    }

    /// Evaluates one whole bracket.
    ///
    /// The step budget is charged per node, so whichever node happens to
    /// visit last is the one that runs out — and pointing a reader at that
    /// node says nothing, because it is not where the loop is. The report is
    /// re-spanned to the evaluation as a whole, which is the thing that did
    /// not finish.
    pub fn evaluate(&mut self, expr: &EvalExpr) -> Result<Value, Error> {
        match self.run(expr) {
            Err(Error::Budget { .. }) => Err(Error::Budget { span: expr.span }),
            other => other,
        }
    }

    fn run(&mut self, expr: &EvalExpr) -> Result<Value, Error> {
        self.step(expr.span)?;
        match &expr.kind {
            // A decimal literal is a `float` in the original, which is why
            // `[16777217 - 16777216]` is zero there: the two literals are one
            // number before the subtraction ever happens.
            EvalExprKind::Number { value, exact } => Ok(Value::Number(if *exact {
                f64::from(*value)
            } else {
                f64::from(*value as f32)
            })),
            // A literal is parsed as `float` in the original, so the value a
            // bracket starts from is single-precision even though everything
            // after it is double.
            EvalExprKind::Float(text) => Ok(Value::Number(f64::from(
                text.parse::<f32>().unwrap_or(f32::NAN),
            ))),
            EvalExprKind::String(text) => Ok(Value::Str(text.clone())),
            EvalExprKind::Pi => Ok(Value::Number(f64::from(K65_PI))),
            EvalExprKind::Symbol(name) => self.read(name),
            EvalExprKind::Unary {
                operator,
                operand,
                is_suffix,
            } => self.unary(expr.span, operator, operand, *is_suffix),
            EvalExprKind::Binary {
                operator,
                left,
                right,
            } => {
                // Both sides always, because the original's stack machine has
                // no short-circuit: `&&` and `||` are `LOGBINARY`, which pops
                // two values that were both computed.
                let left = self.run(left)?;
                let right = self.run(right)?;
                self.binary(expr.span, operator, left, right)
            }
            EvalExprKind::Ternary {
                condition,
                consequence,
                alternative,
            } => {
                // All three arms, for the same reason: `$1 $3 $5 ?:` computes
                // the two results and then selects.
                let condition = self.run(condition)?;
                let consequence = self.run(consequence)?;
                let alternative = self.run(alternative)?;
                Ok(if condition.is_true() {
                    consequence
                } else {
                    alternative
                })
            }
            EvalExprKind::Assignment {
                operator,
                target,
                value,
            } => self.assignment(expr.span, operator, target, value),
            // The value of a list is its last item, and of a statement run
            // its last statement: the original separates both with the pop
            // operator, so everything before the end is evaluated and
            // discarded.
            EvalExprKind::List(items) | EvalExprKind::Block(items) => {
                let mut value = Value::Number(0.0);
                for item in items {
                    value = self.run(item)?;
                }
                Ok(value)
            }
            EvalExprKind::If {
                condition,
                consequence,
                alternative,
            } => {
                if self.run(condition)?.is_true() {
                    self.run(consequence)
                } else {
                    match alternative {
                        Some(alternative) => self.run(alternative),
                        // An `if` with no `else` that does not fire is zero,
                        // because the original pushes `#0` before the test
                        // and pops it only on the taken path.
                        None => Ok(Value::Number(0.0)),
                    }
                }
            }
            EvalExprKind::While { condition, body } => {
                let mut value = Value::Number(0.0);
                while self.run(condition)?.is_true() {
                    self.step(expr.span)?;
                    value = self.run(body)?;
                }
                Ok(value)
            }
            EvalExprKind::Call { name, arguments } => {
                let values = self.arguments(arguments)?;
                self.call(expr.span, name, values)
            }
            // `x.f` is `f(x)` and `x.f(a)` is `f(x, a)` — sugar, measured, and
            // all a method call has ever been (`compiler.inc:448-449`).
            EvalExprKind::Member { base, field } => {
                let base = self.run(base)?;
                self.call(expr.span, field, vec![base])
            }
            EvalExprKind::MethodCall {
                base,
                method,
                arguments,
            } => {
                let mut values = vec![self.run(base)?];
                values.extend(self.arguments(arguments)?);
                self.call(expr.span, method, values)
            }
            EvalExprKind::Subscript { base, indices } => {
                // The base and the indices are evaluated first so that an
                // error in one of them is the answer, rather than the
                // deferral hiding it.
                self.run(base)?;
                for index in indices {
                    self.run(index)?;
                }
                Err(Error::Deferred {
                    span: expr.span,
                    what: if indices.len() > 1 {
                        "two-dimensional reads of a data section's bytes"
                    } else {
                        "reads of a data section's bytes"
                    },
                })
            }
            EvalExprKind::ReservedCall(call) => self.reserved(expr.span, call),
            // A syntax error was already reported for this node.
            EvalExprKind::Error => Err(Error::Reported),
        }
    }

    /// Charges one step, and refuses once the budget is gone.
    fn step(&mut self, span: Span) -> Result<(), Error> {
        match self.steps.checked_sub(1) {
            Some(left) => {
                self.steps = left;
                Ok(())
            }
            None => Err(Error::Budget { span }),
        }
    }

    fn arguments(&mut self, arguments: &[EvalExpr]) -> Result<Vec<Value>, Error> {
        arguments
            .iter()
            .map(|argument| self.run(argument))
            .collect()
    }

    // -- names ------------------------------------------------------------

    fn read(&mut self, name: &Name) -> Result<Value, Error> {
        for frame in self.frames.iter().rev() {
            if let Some(value) = frame.get(&name.text) {
                return Ok(value.clone());
            }
        }
        if let Some(value) = self.env.get(&name.text) {
            return Ok(value.clone());
        }
        let Some(id) = self.symbols.lookup(&name.text) else {
            return Err(Error::Name {
                span: name.span,
                name: name.text.clone(),
            });
        };
        match self.symbols.kind(id) {
            SymbolKind::Const { value } => Ok(Value::Number(f64::from(value))),
            // A data label is the section it names, which is what makes
            // `index(tab, 2)` mean anything at all. Nothing in G4 consumes
            // one, so in practice this value exists to produce a better
            // diagnostic than "undeclared" at the place it cannot be used.
            SymbolKind::DataLabel { .. } => Ok(Value::Section(id)),
            other => Err(Error::NotAValue {
                span: name.span,
                name: name.text.clone(),
                what: kind_phrase(&other),
            }),
        }
    }

    /// Writes `value` to `name`, enforcing who owns the name.
    fn assign(&mut self, span: Span, name: &Name, value: Value) -> Result<Value, Error> {
        for frame in self.frames.iter_mut().rev() {
            if let Some(slot) = frame.get_mut(&name.text) {
                *slot = value.clone();
                return Ok(value);
            }
        }
        if let Some(binding) = self.env.values.get_mut(&name.text) {
            // Write-once outside the block that introduced the name.
            if binding.block != self.block {
                return Err(Error::Reassigned {
                    span,
                    name: name.text.clone(),
                    first: binding.block,
                });
            }
            binding.value = value.clone();
            return Ok(value);
        }
        // A name the compiler already owns is not the evaluator's to write —
        // the original says so too, with "Can't assign value to const".
        if let Some(id) = self.symbols.lookup(&name.text) {
            return Err(Error::NotAssignable {
                span,
                name: name.text.clone(),
                what: kind_phrase(&self.symbols.kind(id)),
            });
        }
        self.env.values.insert(
            name.text.clone(),
            Binding {
                value: value.clone(),
                block: self.block,
            },
        );
        self.env.fresh.push(name.text.clone());
        Ok(value)
    }

    /// The name an assignment or an increment writes to.
    fn target<'e>(&mut self, expr: &'e EvalExpr) -> Result<&'e Name, Error> {
        match &expr.kind {
            EvalExprKind::Symbol(name) => Ok(name),
            EvalExprKind::Error => Err(Error::Reported),
            _ => Err(Error::NotANumber {
                span: expr.span,
                found: "an expression",
                needed: "a name to assign to",
            }),
        }
    }

    // -- operators --------------------------------------------------------

    fn unary(
        &mut self,
        span: Span,
        operator: &str,
        operand: &EvalExpr,
        is_suffix: bool,
    ) -> Result<Value, Error> {
        if operator == "++" || operator == "--" {
            let name = self.target(operand)?.clone();
            let old = self.read(&name)?;
            let number = self.number(span, &old, "an increment")?;
            let stepped = if operator == "++" {
                number + 1.0
            } else {
                number - 1.0
            };
            self.assign(span, &name, Value::Number(stepped))?;
            // `x++` is the value before the step and `++x` the value after,
            // which is what the original's `#1 + @=x #1 -` sequence spells.
            return Ok(Value::Number(if is_suffix { number } else { stepped }));
        }
        let value = self.run(operand)?;
        match operator {
            // Unary `+` is not an operation in the original: `EvUnary -> +
            // EvUnary` reduces to its operand and emits nothing, so `+"a"` is
            // the string.
            "+" => Ok(value),
            "-" => Ok(Value::Number(-self.number(span, &value, "a negation")?)),
            "~" => Ok(Value::Number(
                !to_bits(self.number(span, &value, "a bitwise complement")?) as f64,
            )),
            "!" => Ok(Value::Number(f64::from(!value.is_true()))),
            _ => Err(Error::Reported),
        }
    }

    fn binary(
        &mut self,
        span: Span,
        operator: &str,
        left: Value,
        right: Value,
    ) -> Result<Value, Error> {
        let logical = |value: bool| Ok(Value::Number(f64::from(value)));
        match operator {
            // `+` is the one operator that reaches strings: if either side is
            // one, both become text and the answer is the concatenation.
            "+" => match (&left, &right) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a + b)),
                (Value::Str(_), _) | (_, Value::Str(_)) => Ok(Value::Str(format!(
                    "{}{}",
                    left.as_text(self.symbols),
                    right.as_text(self.symbols)
                ))),
                _ => Err(self.not_a_number(span, &left, &right, "addition")),
            },
            "-" | "*" | "/" | "%" | "?>" | "?<" => {
                let a = self.number(span, &left, operator_phrase(operator))?;
                let b = self.number(span, &right, operator_phrase(operator))?;
                match operator {
                    "-" => Ok(Value::Number(a - b)),
                    "*" => Ok(Value::Number(a * b)),
                    "/" if b == 0.0 => Err(Error::Undefined {
                        span,
                        what: "division by zero",
                    }),
                    "/" => Ok(Value::Number(a / b)),
                    "%" if b == 0.0 => Err(Error::Undefined {
                        span,
                        what: "a remainder by zero",
                    }),
                    // Non-negative, measured: `fmod(fmod(a, b) + b, b)`, so
                    // `(0 - 7) % 3` is 2 and not -1.
                    "%" => Ok(Value::Number((a % b + b) % b)),
                    "?>" => Ok(Value::Number(if a > b { a } else { b })),
                    _ => Ok(Value::Number(if a < b { a } else { b })),
                }
            }
            "<<" | ">>" | "&" | "|" | "^" => {
                let a = to_bits(self.number(span, &left, operator_phrase(operator))?);
                let b = to_bits(self.number(span, &right, operator_phrase(operator))?);
                let shift = u32::try_from(b).unwrap_or(u32::MAX);
                let bits = match operator {
                    "<<" => a.checked_shl(shift).unwrap_or(0),
                    ">>" => a.checked_shr(shift).unwrap_or(if a < 0 { -1 } else { 0 }),
                    "&" => a & b,
                    "|" => a | b,
                    _ => a ^ b,
                };
                Ok(Value::Number(bits as f64))
            }
            // Equality compares within a type and is false across types, so
            // `1 == "1"` is 0 rather than an error.
            "==" => logical(self.equal(&left, &right)),
            "!=" => logical(!self.equal(&left, &right)),
            "<" | "<=" | ">" | ">=" => {
                let Some(less) = self.less(&left, &right) else {
                    return Err(self.not_a_number(span, &left, &right, "a comparison"));
                };
                let greater = self.less(&right, &left).unwrap_or(false);
                logical(match operator {
                    "<" => less,
                    "<=" => !greater,
                    ">" => greater,
                    _ => !less,
                })
            }
            "&&" => logical(left.is_true() && right.is_true()),
            // The original has no `||` — `EvLogOr` is spelled `&&` too, so the
            // token never appears there and a source containing one does not
            // compile. K816 keeps the documented spelling as an extension.
            "||" => logical(left.is_true() || right.is_true()),
            _ => Err(Error::Reported),
        }
    }

    fn equal(&self, left: &Value, right: &Value) -> bool {
        match (left, right) {
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::Str(a), Value::Str(b)) => a == b,
            (Value::Section(a), Value::Section(b)) => a == b,
            _ => false,
        }
    }

    /// `left < right`, or `None` when the two cannot be ordered.
    fn less(&self, left: &Value, right: &Value) -> Option<bool> {
        match (left, right) {
            (Value::Number(a), Value::Number(b)) => Some(a < b),
            (Value::Str(a), Value::Str(b)) => Some(a < b),
            _ => None,
        }
    }

    fn assignment(
        &mut self,
        span: Span,
        operator: &str,
        target: &EvalExpr,
        value: &EvalExpr,
    ) -> Result<Value, Error> {
        let name = self.target(target)?.clone();
        let value = self.run(value)?;
        let value = match operator {
            "=" => value,
            // `x op= e` is `x op e` assigned back, which is exactly how the
            // original expands it (`compiler.inc:376-385`).
            _ => {
                let current = self.read(&name)?;
                let operator = operator.strip_suffix('=').unwrap_or(operator);
                self.binary(span, operator, current, value)?
            }
        };
        self.assign(span, &name, value)
    }

    /// The number `value` is, or the error that says it is not one.
    fn number(&self, span: Span, value: &Value, needed: &'static str) -> Result<f64, Error> {
        value.number().ok_or(Error::NotANumber {
            span,
            found: value.describe(),
            needed,
        })
    }

    fn not_a_number(&self, span: Span, left: &Value, right: &Value, needed: &'static str) -> Error {
        let found = if left.number().is_none() { left } else { right };
        Error::NotANumber {
            span,
            found: found.describe(),
            needed,
        }
    }

    // -- calls ------------------------------------------------------------

    fn call(&mut self, span: Span, name: &Name, arguments: Vec<Value>) -> Result<Value, Error> {
        // Copied out of `self` first, and deliberately: `functions` is a
        // shared reference with the interpreter's own lifetime, so reading it
        // through a local does not borrow `self` and the body can be run
        // where it lies. Cloning it instead — which is what a naive
        // `self.functions.lookup(…)` forces — puts a copy of the whole
        // function tree on the stack at every level of a recursion.
        let functions = self.functions;
        if let Some(function) = functions.lookup(&name.text, arguments.len()) {
            if self.frames.len() >= MAX_CALL_DEPTH {
                return Err(Error::TooDeep { span });
            }
            let frame = function
                .parameters
                .iter()
                .cloned()
                .zip(arguments)
                .collect::<BTreeMap<_, _>>();
            self.frames.push(frame);
            let value = self.run(&function.body);
            self.frames.pop();
            return value;
        }
        // A section function reads bytes K816 does not have before lowering,
        // so it is named here and owned by G6 rather than mis-answered.
        if name.text == "index" {
            return Err(Error::Deferred {
                span,
                what: "reads of a data section's bytes",
            });
        }
        // The four refused functions, reached through `x.f` — which is
        // `f(x)`, and is not the node analysis walks for the call spelling.
        // Reporting them here rather than as an unknown name is what keeps
        // `source.size` and `size(source)` the same complaint.
        if let Some(refused) = ["size", "addbyte", "color", "rnd"]
            .into_iter()
            .find(|refused| *refused == name.text)
        {
            return Err(Error::Refused {
                span: name.span,
                name: refused,
            });
        }
        let numbers = arguments
            .iter()
            .map(|argument| self.number(span, argument, "a library function"))
            .collect::<Result<Vec<_>, _>>();
        let answer = match (name.text.as_str(), arguments.len()) {
            ("floor", 1) => numbers?[0].floor(),
            ("ceil", 1) => numbers?[0].ceil(),
            // The library's `round` is the boundary rule spelled as a
            // function: half up, so `round(-2.5)` is `-2`.
            ("round", 1) => (numbers?[0] + 0.5).floor(),
            ("frac", 1) => {
                let value = numbers?[0];
                value - value.floor()
            }
            ("sin", 1) => numbers?[0].sin(),
            ("cos", 1) => numbers?[0].cos(),
            ("asin", 1) => numbers?[0].asin(),
            ("acos", 1) => numbers?[0].acos(),
            ("sqrt", 1) => numbers?[0].sqrt(),
            // Not in the original's table at all: `abs(x)` ends the build
            // there. K816 has it because this document lists it, which makes
            // it a `K816-extension` row rather than a compatibility one.
            ("abs", 1) => numbers?[0].abs(),
            ("pow", 2) => {
                let numbers = numbers?;
                numbers[0].powf(numbers[1])
            }
            ("max", 2) | ("min", 2) => {
                let wants_greater = name.text == "max";
                let (a, b) = (&arguments[0], &arguments[1]);
                let Some(less) = self.less(a, b) else {
                    return Err(self.not_a_number(span, a, b, "a library function"));
                };
                return Ok(if less == wants_greater { b } else { a }.clone());
            }
            ("clamp", 3) => {
                let numbers = numbers?;
                numbers[0].max(numbers[1]).min(numbers[2])
            }
            _ => {
                return Err(Error::Name {
                    span: name.span,
                    name: name.text.clone(),
                });
            }
        };
        Ok(Value::Number(answer))
    }

    /// One of the six functions the grammar names as reserved.
    ///
    /// Two of them are implemented: `print` is a note carrying its message
    /// and `error` is an error carrying it, which is what the two were for
    /// (decision 6). The other four report themselves through
    /// `K816-K65-0002` when analysis walks the expression, so the interpreter
    /// meeting one has nothing to add.
    fn reserved(&mut self, span: Span, call: &ReservedCall) -> Result<Value, Error> {
        let arguments = self.arguments(&call.arguments)?;
        let message = || {
            arguments
                .iter()
                .map(|argument| argument.as_text(self.symbols))
                .collect::<Vec<_>>()
                .join(" ")
        };
        match call.name {
            EvalCallName::Print => {
                self.notes.push(Note {
                    span,
                    message: message(),
                });
                Ok(Value::Number(0.0))
            }
            EvalCallName::Error => Err(Error::Raised {
                span,
                message: message(),
            }),
            _ => Err(Error::Reported),
        }
    }
}

// ---------------------------------------------------------------------------
// Where the evaluator is invoked
// ---------------------------------------------------------------------------

/// One invocation of the evaluator, found by walking an item in source order.
///
/// Source order is the whole point. The original is a single pass, so
/// `[ FOUR = 4 ]` above a data block that reads `FOUR` works and the same two
/// lines swapped do not — and a K816 that evaluated brackets lazily, where
/// each value happens to be needed, would give the second program an answer.
pub enum Site<'a> {
    /// `[ … ]` — evaluated where it stands, for its value and its
    /// assignments.
    Bracket(&'a EvalExpr),
    /// A `for … eval` body, whose bounds are ordinary K65 expressions and are
    /// therefore not known during this pass. What is recorded here is the
    /// environment; the body itself runs once per iteration, later, against
    /// that recording.
    Loop { span: Span, body: &'a EvalExpr },
}

/// True when the parser had to recover somewhere inside this expression.
///
/// A recovered tree is not the program: `[( unbalanced]` reaches the builder
/// as a bracket around a name nobody wrote, and evaluating it would report
/// that name as undeclared on top of the syntax error that is the real news.
/// The house rule is that a construct which has already reported itself says
/// nothing more, and a bracket the parser could not read is one of those.
pub fn is_recovered(expr: &EvalExpr) -> bool {
    match &expr.kind {
        EvalExprKind::Error => true,
        EvalExprKind::Unary { operand, .. } => is_recovered(operand),
        EvalExprKind::Binary { left, right, .. } => is_recovered(left) || is_recovered(right),
        EvalExprKind::Ternary {
            condition,
            consequence,
            alternative,
        } => is_recovered(condition) || is_recovered(consequence) || is_recovered(alternative),
        EvalExprKind::Assignment { target, value, .. } => {
            is_recovered(target) || is_recovered(value)
        }
        EvalExprKind::Subscript { base, indices } => {
            is_recovered(base) || indices.iter().any(is_recovered)
        }
        EvalExprKind::Member { base, .. } => is_recovered(base),
        EvalExprKind::MethodCall {
            base, arguments, ..
        } => is_recovered(base) || arguments.iter().any(is_recovered),
        EvalExprKind::Call { arguments, .. } => arguments.iter().any(is_recovered),
        EvalExprKind::ReservedCall(call) => call.arguments.iter().any(is_recovered),
        EvalExprKind::List(items) | EvalExprKind::Block(items) => items.iter().any(is_recovered),
        EvalExprKind::If {
            condition,
            consequence,
            alternative,
        } => {
            is_recovered(condition)
                || is_recovered(consequence)
                || alternative.as_deref().is_some_and(is_recovered)
        }
        EvalExprKind::While { condition, body } => is_recovered(condition) || is_recovered(body),
        EvalExprKind::Number { .. }
        | EvalExprKind::Float(_)
        | EvalExprKind::String(_)
        | EvalExprKind::Symbol(_)
        | EvalExprKind::Pi => false,
    }
}

/// Every evaluator invocation `item` contains, in source order.
pub fn sites_of_item<'a>(item: &'a crate::ast::Item, out: &mut Vec<Site<'a>>) {
    use crate::ast::ItemKind;
    match &item.kind {
        ItemKind::EvalBlock(expr) => sites_of_expr(expr, out),
        ItemKind::Var(var) => {
            for expr in [&var.size, &var.count, &var.address].into_iter().flatten() {
                sites_of_expr(expr, out);
            }
            sites_of_fields(&var.fields, out);
        }
        ItemKind::Const(declaration) => {
            for entry in &declaration.entries {
                if let Some(value) = &entry.value {
                    sites_of_expr(value, out);
                }
            }
        }
        ItemKind::Data(block) => sites_of_data(block, out),
        ItemKind::Func(func) => sites_of_block(&func.body, out),
        // A `#if` condition is an evaluator expression and is deliberately
        // *not* run here. Which regions are live is sub-plan G5's, and a
        // condition inside a region G5 finds dead must not have been
        // evaluated already — its names may not exist, and its `print` would
        // have spoken for code that is not in the program. G5 evaluates them
        // through this same interpreter, in its own order.
        ItemKind::Preprocessor(_) => {}
        // An `evalfunc` body is not evaluated where it is written; it runs at
        // each call, in the caller's frame.
        ItemKind::EvalFunc(_)
        | ItemKind::Segment(_)
        | ItemKind::Binary(_)
        | ItemKind::Reserved(_)
        | ItemKind::ModuleWidth(_)
        | ItemKind::Error => {}
    }
}

fn sites_of_fields<'a>(fields: &'a [crate::ast::FieldDeclaration], out: &mut Vec<Site<'a>>) {
    for field in fields {
        if let Some(size) = &field.size {
            sites_of_expr(size, out);
        }
        sites_of_fields(&field.fields, out);
    }
}

fn sites_of_data<'a>(block: &'a crate::ast::DataBlock, out: &mut Vec<Site<'a>>) {
    use crate::ast::{DataEntryKind, DataValue};
    for entry in &block.entries {
        match &entry.kind {
            DataEntryKind::Align(align) => {
                sites_of_expr(&align.boundary, out);
                if let Some(offset) = &align.offset {
                    sites_of_expr(offset, out);
                }
            }
            DataEntryKind::Address(expr) => sites_of_expr(expr, out),
            DataEntryKind::Typed { values, .. } => {
                for value in values {
                    if let DataValue::Expr(expr) = value {
                        sites_of_expr(expr, out);
                    }
                }
            }
            DataEntryKind::Value(DataValue::Expr(expr)) => sites_of_expr(expr, out),
            DataEntryKind::Code { body, .. } => sites_of_block(body, out),
            DataEntryKind::Repeat { count, body } => {
                sites_of_expr(count, out);
                sites_of_data(body, out);
            }
            DataEntryKind::ForEval { from, to, body, .. } => {
                sites_of_expr(from, out);
                sites_of_expr(to, out);
                if let crate::ast::ExprKind::Eval(body) = &body.kind {
                    out.push(Site::Loop {
                        span: entry.span,
                        body,
                    });
                }
            }
            // Not run here; see `sites_of_item`.
            DataEntryKind::Preprocessor(_) => {}
            DataEntryKind::Segment(_)
            | DataEntryKind::Nocross
            | DataEntryKind::Charset(_)
            | DataEntryKind::Label(_)
            | DataEntryKind::Value(_)
            | DataEntryKind::Binary(_)
            | DataEntryKind::Reserved(_)
            | DataEntryKind::Error => {}
        }
    }
}

fn sites_of_block<'a>(block: &'a crate::ast::Block, out: &mut Vec<Site<'a>>) {
    for statement in &block.statements {
        sites_of_statement(statement, out);
    }
}

fn sites_of_statement<'a>(statement: &'a crate::ast::Statement, out: &mut Vec<Site<'a>>) {
    use crate::ast::StatementKind;
    {
        match &statement.kind {
            StatementKind::Native(native) => {
                if let Some(repeat) = &native.repeat {
                    sites_of_expr(repeat, out);
                }
                if let Some(operand) = &native.operand {
                    for expr in native_operand_expressions(operand) {
                        sites_of_expr(expr, out);
                    }
                }
            }
            StatementKind::Hla(hla) => {
                for expr in hla_expressions(hla) {
                    sites_of_expr(expr, out);
                }
            }
            StatementKind::WidthBlock(width) => sites_of_block(&width.body, out),
            StatementKind::Nocross(body) => sites_of_block(body, out),
            StatementKind::Loop(loop_statement) => sites_of_block(&loop_statement.body, out),
            StatementKind::Conditional(conditional) => {
                sites_of_block(&conditional.body, out);
                if let Some(otherwise) = &conditional.otherwise {
                    sites_of_block(otherwise, out);
                }
            }
            StatementKind::ConditionalJump(jump) => {
                sites_of_statement(&jump.jump, out);
            }
            StatementKind::Goto(goto) => sites_of_expr(&goto.target, out),
            StatementKind::Data(block) => sites_of_data(block, out),
            StatementKind::Align(align) => {
                sites_of_expr(&align.boundary, out);
                if let Some(offset) = &align.offset {
                    sites_of_expr(offset, out);
                }
            }
            StatementKind::Address(expr) => sites_of_expr(expr, out),
            _ => {}
        }
    }
}

/// The expressions one native operand carries.
fn native_operand_expressions(operand: &crate::ast::NativeOperand) -> Vec<&Expr> {
    use crate::ast::NativeOperand;
    match operand {
        NativeOperand::Immediate(expr) => vec![expr],
        NativeOperand::Address(address) => vec![&address.value],
        NativeOperand::Indirect(indirect) => vec![&indirect.value],
        NativeOperand::LongIndirect(indirect) => vec![&indirect.value],
        NativeOperand::BlockMove(moves) => vec![&moves.source, &moves.destination],
        NativeOperand::Register(_) => Vec::new(),
    }
}

/// The expressions one HLA statement carries.
fn hla_expressions(hla: &crate::ast::HlaStatement) -> Vec<&Expr> {
    use crate::ast::{HlaOperandKind, HlaStatement};
    let operands: Vec<&crate::ast::HlaOperand> = match hla {
        HlaStatement::Assign(assign) => assign
            .targets
            .iter()
            .chain(std::iter::once(&assign.source))
            .collect(),
        HlaStatement::Arithmetic(arithmetic) => vec![&arithmetic.operand],
        HlaStatement::Shift(shift) => vec![&shift.target],
        HlaStatement::Step(step) => vec![&step.target],
        // `* N` is the one HLA statement whose expression is not an operand:
        // the repeat count of a `NOP` run, and a bracket is legal there.
        HlaStatement::Nop { count } => {
            return count.iter().collect();
        }
        HlaStatement::Exchange(_)
        | HlaStatement::Flag(_)
        | HlaStatement::Stack(_)
        | HlaStatement::Break => Vec::new(),
    };
    let mut found = Vec::new();
    for operand in operands {
        match &operand.kind {
            HlaOperandKind::Immediate(expr) => found.push(expr),
            HlaOperandKind::Memory(address) => found.push(&address.value),
            HlaOperandKind::Indirect(indirect) => found.push(&indirect.value),
            HlaOperandKind::Register(_) | HlaOperandKind::Error => {}
        }
    }
    found
}

/// Every evaluator bracket inside one K65 expression, outermost first.
///
/// A bracket's interior is the evaluator's own language, so the walk stops at
/// one: a bracket inside a bracket is the evaluator's business, not this
/// pass's.
fn sites_of_expr<'a>(expr: &'a Expr, out: &mut Vec<Site<'a>>) {
    match &expr.kind {
        ExprKind::Eval(body) => out.push(Site::Bracket(body)),
        ExprKind::Unary(unary) => sites_of_expr(&unary.operand, out),
        ExprKind::Binary(binary) => {
            sites_of_expr(&binary.left, out);
            sites_of_expr(&binary.right, out);
        }
        ExprKind::Field(access) => sites_of_expr(&access.base, out),
        ExprKind::Subscript(subscript) => {
            sites_of_expr(&subscript.base, out);
            sites_of_expr(&subscript.index, out);
        }
        ExprKind::Metadata(query) => sites_of_expr(&query.base, out),
        ExprKind::TypedView(view) => sites_of_expr(&view.base, out),
        ExprKind::Number(_) | ExprKind::Symbol(_) | ExprKind::LocalSymbol(_) | ExprKind::Error => {}
    }
}

// ---------------------------------------------------------------------------
// What the pass leaves behind
// ---------------------------------------------------------------------------

/// The evaluator's answers, keyed by where they were asked.
///
/// Every stage after analysis reads this rather than re-running anything:
/// the evaluator has side effects — assignments, `print`, the step budget —
/// so running one bracket twice is one `print` too many, and lowering
/// re-resolves data blocks that analysis has already resolved once.
#[derive(Clone, Debug, Default)]
pub struct Outcome {
    // Keyed by span, and looked up only — never iterated — so a hash map's
    // order costs nothing and the key needs no ordering it does not have.
    values: HashMap<Span, Value>,
    loops: HashMap<Span, Environment>,
    functions: Functions,
}

impl Outcome {
    pub fn value(&self, span: Span) -> Option<&Value> {
        self.values.get(&span)
    }

    pub fn record(&mut self, span: Span, value: Value) {
        self.values.insert(span, value);
    }

    /// The environment a `for … eval` body runs against, one per iteration.
    pub fn loop_environment(&self, span: Span) -> Option<&Environment> {
        self.loops.get(&span)
    }

    pub fn record_loop(&mut self, span: Span, environment: Environment) {
        self.loops.insert(span, environment);
    }

    pub fn functions(&self) -> &Functions {
        &self.functions
    }

    pub fn functions_mut(&mut self) -> &mut Functions {
        &mut self.functions
    }
}

/// A phrase naming what a symbol is, for a diagnostic that has to say why a
/// name is not an evaluator value.
fn kind_phrase(kind: &SymbolKind) -> &'static str {
    match kind {
        SymbolKind::Const { .. } => "a `const`",
        SymbolKind::Var { .. } => "a variable",
        SymbolKind::AbstractVar { .. } => "an `abstract var`",
        SymbolKind::Label { .. } => "a code label",
        SymbolKind::DataLabel { .. } => "a data section",
        SymbolKind::Alias { .. } => "an `inline` parameter",
        SymbolKind::Inline => "an `inline` section",
        SymbolKind::Function { .. } => "a code section",
    }
}

/// What an operator needs its operands to be, for the same diagnostic.
fn operator_phrase(operator: &str) -> &'static str {
    match operator {
        "-" => "a subtraction",
        "*" => "a multiplication",
        "/" => "a division",
        "%" => "a remainder",
        "?>" | "?<" => "a selection",
        "<<" | ">>" => "a shift",
        _ => "a bitwise operation",
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse_source;
    use crate::source::SourceMap;

    /// Analyses `source` and hands back what its evaluator brackets produced,
    /// in source order, with the diagnostic codes it reported.
    fn evaluate(source: &str) -> (Vec<Value>, Vec<String>) {
        let mut map = SourceMap::new();
        let id = map.add_file("eval.k65", source);
        let parsed = parse_source(map.file(id), None);
        assert_eq!(parsed.diagnostics, vec![], "`{source}` did not parse");
        let (model, diagnostics) = crate::semantic::analyze_file(&parsed.file);

        // Source order, which is the whole contract: the driver walks items
        // in order, so re-walking them here recovers the same sequence.
        let mut values = Vec::new();
        for item in &parsed.file.items {
            let mut sites = Vec::new();
            sites_of_item(item, &mut sites);
            for site in sites {
                if let Site::Bracket(body) = site
                    && let Some(value) = model.eval.value(body.span)
                {
                    values.push(value.clone());
                }
            }
        }
        let codes = diagnostics
            .iter()
            .map(|diagnostic| diagnostic.code.as_str().to_owned())
            .collect();
        (values, codes)
    }

    /// The one number a single-bracket data block evaluates to.
    ///
    /// `K816-E0109` is tolerated because it is not the evaluator's: a byte
    /// entry checks its own range, and these cases are about what the
    /// arithmetic answered rather than about where the answer was put.
    fn number(body: &str) -> f64 {
        let (values, codes) = evaluate(&format!("data d {{\n  [ {body} ]\n}}\n"));
        let unexpected: Vec<&String> = codes.iter().filter(|code| *code != "K816-E0109").collect();
        assert!(unexpected.is_empty(), "`{body}` reported {unexpected:?}");
        match values.as_slice() {
            [Value::Number(value)] => *value,
            other => panic!("`{body}` produced {other:?}"),
        }
    }

    /// What a single-bracket data block reports, if anything.
    fn codes(body: &str) -> Vec<String> {
        evaluate(&format!("data d {{\n  [ {body} ]\n}}\n")).1
    }

    #[test]
    fn the_integer_boundary_rounds_half_up_through_single_precision() {
        // Half *up*, not away from zero and not to even.
        assert_eq!(to_integer(2.4), 2);
        assert_eq!(to_integer(2.5), 3);
        assert_eq!(to_integer(3.5), 4);
        assert_eq!(to_integer(-2.4), -2);
        assert_eq!(to_integer(-2.5), -2, "half up, so -2.5 is -2");
        assert_eq!(to_integer(-2.6), -3);
        // The narrowing to `f32` happens before the rounding, which is why
        // one past the last integer a `float` holds exactly moves.
        assert_eq!(to_integer(8388609.0), 8388610);
        assert_eq!(to_integer(8388608.0), 8388608);
    }

    #[test]
    fn arithmetic_is_double_between_single_precision_ends() {
        // A decimal literal is a `float`, so these two are one number.
        assert_eq!(number("16777217 - 16777216"), 0.0);
        // A `0x` literal is not, so these two are not.
        assert_eq!(number("0x1000001 - 0x1000000"), 1.0);
        // And the arithmetic between them is `f64`: single precision would
        // give zero here.
        assert!(number("(0.1 + 0.2 - 0.3) * 1000000000") < 0.0);
        assert_eq!(to_integer(number("(0.1 + 0.2 - 0.3) * 1000000000")), -7);
    }

    #[test]
    fn the_operator_table_answers_what_the_oracle_answers() {
        assert_eq!(number("1 + 2 * 3"), 7.0, "precedence is the grammar's");
        assert_eq!(number("7 / 2"), 3.5, "division is float, not integer");
        // Non-negative modulo: `fmod(fmod(a, b) + b, b)`.
        assert_eq!(number("7 % 3"), 1.0);
        assert_eq!(number("(0 - 7) % 3"), 2.0);
        assert_eq!(number("3 ?> 5"), 5.0, "`?>` selects the greater");
        assert_eq!(number("3 ?< 5"), 3.0, "`?<` selects the smaller");
        assert_eq!(number("1, 2, 3"), 3.0, "a list is its last item");
        assert_eq!(number("0 ? 11 : 22"), 22.0);
        // A bitwise operand truncates toward zero, which is *not* the integer
        // boundary rule: rounding here would make this -3.
        assert_eq!(number("(0 - 2.7) | 0"), -2.0);
        assert_eq!(number("~5"), -6.0);
    }

    #[test]
    fn a_statement_run_is_its_last_statement_and_a_block_may_reassign() {
        assert_eq!(number("1 2 3"), 3.0);
        assert_eq!(number("{ 1 2 3 }"), 3.0);
        // An `if` with no `else` that does not fire is zero, because the
        // original pushes `#0` before the test.
        assert_eq!(number("if 1 44"), 44.0);
        assert_eq!(number("if 0 44"), 0.0);
        assert_eq!(number("if 0 44 else 55"), 55.0);
        // A `while` is the last iteration's value, or zero if it never ran.
        assert_eq!(number("N = 0 while N < 5 { N = N + 1 } N"), 5.0);
        assert_eq!(number("while 0 { 9 }"), 0.0);
        // Within one bracket a name may be assigned as often as it likes.
        assert_eq!(number("M = 1, M = 2, M"), 2.0);
    }

    #[test]
    fn an_assignment_leaves_the_value_it_assigned() {
        assert_eq!(number("Q = 4"), 4.0);
        assert_eq!(number("Q = 4, Q += 3"), 7.0);
        assert_eq!(number("Q = 4, Q <<= 2, Q"), 16.0);
        // Prefix is the new value and suffix is the old one, which is what
        // the original's `#1 + @=x #1 -` sequence spells — except that its
        // prefix rules are broken, so only the suffix half is compatibility.
        assert_eq!(number("S = 2, ++S"), 3.0);
        assert_eq!(number("S = 2, S++"), 2.0);
        assert_eq!(number("S = 2, S++, S"), 3.0);
    }

    #[test]
    fn a_method_call_is_the_call_it_spells() {
        assert_eq!(number("(3).max(5)"), 5.0);
        assert_eq!(number("(2.75).frac() * 100"), 75.0);
        assert_eq!(number("max(3, 5)"), 5.0);
        // `pi` is the literal the original has, not the machine constant.
        assert_eq!(number("pi"), f64::from(K65_PI));
        assert_ne!(number("pi"), std::f64::consts::PI);
    }

    #[test]
    fn the_evaluator_reads_the_names_the_compiler_has() {
        let (values, codes) =
            evaluate("[ SHIFT = 2 ]\nconst FLAG = 0x30\ndata d {\n  [ FLAG ]\n  [ SHIFT ]\n}\n");
        assert_eq!(codes, Vec::<String>::new());
        assert_eq!(
            values,
            vec![Value::Number(2.0), Value::Number(48.0), Value::Number(2.0),],
            "a `const` and a name an earlier bracket assigned both resolve"
        );
        // And a name the evaluator assigned is a compiler constant below it.
        let (_, codes) = evaluate("[ FOUR = 4 ]\ndata d {\n  FOUR\n}\n");
        assert_eq!(codes, Vec::<String>::new());
    }

    #[test]
    fn what_the_evaluator_refuses_says_which_rule_stopped_it() {
        // A name nothing gives a value.
        assert_eq!(codes("nope"), vec!["K816-E0101"]);
        // A `var` is the one kind of name that does not resolve, which is
        // what the original says too.
        assert_eq!(
            evaluate("var slot\ndata d {\n  [ slot ]\n}\n").1,
            vec!["K816-E0148"]
        );
        // Arithmetic with no answer.
        assert_eq!(codes("1 / 0"), vec!["K816-E0108"]);
        assert_eq!(codes("1 % 0"), vec!["K816-E0108"]);
        // A string is a value inside an evaluation and never a value of one.
        assert_eq!(number("(\"ab\" == \"ab\")"), 1.0);
        assert_eq!(number("(\"ab\" < \"b\")"), 1.0);
        assert_eq!(codes("\"ab\""), vec!["K816-E0148"]);
        // A section read is G6's, and says so rather than guessing.
        assert_eq!(
            evaluate("data t {\n  1 2\n}\ndata d {\n  [ index(t, 1) ]\n}\n").1,
            vec!["K816-E0201"]
        );
        // The four refused functions, in both spellings.
        assert_eq!(codes("rnd()"), vec!["K816-K65-0002"]);
        assert_eq!(
            evaluate("data t {\n  1 2\n}\ndata d {\n  [ t.size ]\n}\n").1,
            vec!["K816-K65-0002"],
            "`x.size` is `size(x)`, and gets the same complaint"
        );
    }

    #[test]
    fn the_step_budget_is_what_bounds_a_loop_and_a_recursion() {
        assert_eq!(codes("Z = 0 while 1 { Z = Z + 1 }"), vec!["K816-E0147"]);
        assert_eq!(
            evaluate("evalfunc down(k) [ down(k) ]\ndata d {\n  [ down(1) ]\n}\n").1,
            vec!["K816-E0147"]
        );
        // A recursion that terminates is not bounded away: it is the
        // extension the original's broken one could never be.
        assert_eq!(
            evaluate("evalfunc fact(k) [ if k <= 1 1 else fact(k - 1) * k ]\ndata d {\n  [ fact(5) ]\n}\n").1,
            Vec::<String>::new()
        );
    }

    #[test]
    fn an_evaluator_name_is_write_once_outside_its_own_block() {
        assert_eq!(evaluate("[ N = 1 ]\n[ N = 2 ]\n").1, vec!["K816-E0149"]);
        // Reading it again is not assigning it.
        assert_eq!(
            evaluate("[ N = 1 ]\ndata d {\n  [ N ]\n}\n").1,
            Vec::<String>::new()
        );
        // Nor is a name the compiler already owns the evaluator's to write.
        assert_eq!(
            evaluate("const K = 1\ndata d {\n  [ K = 2 ]\n}\n").1,
            vec!["K816-E0149"]
        );
    }

    #[test]
    fn print_is_a_note_and_error_is_one() {
        assert_eq!(codes("print(\"ready\")"), vec!["K816-N0145"]);
        // A number splices into a string as `%lf`, which is what the original
        // prints, so the two compilers say the same words.
        let (_, spliced) = evaluate("data d {\n  [ print(\"n=\" + 8) ]\n  0\n}\n");
        assert_eq!(spliced, vec!["K816-N0145"]);
        assert_eq!(codes("error(\"no display\")"), vec!["K816-E0146"]);
    }

    #[test]
    fn a_bracket_the_parser_recovered_inside_is_not_evaluated() {
        // The syntax error is the news; a name recovery invented the shape of
        // is not. Parsed rather than analysed, because `evaluate` asserts a
        // clean parse.
        let mut map = SourceMap::new();
        let id = map.add_file("eval.k65", "data d {\n  [( unbalanced]\n}\n");
        let parsed = parse_source(map.file(id), None);
        assert_eq!(parsed.diagnostics.len(), 1, "one syntax error");
        let (_, diagnostics) = crate::semantic::analyze_file(&parsed.file);
        assert_eq!(
            diagnostics
                .iter()
                .map(|diagnostic| diagnostic.code.as_str())
                .collect::<Vec<_>>(),
            Vec::<&str>::new(),
            "and nothing else"
        );
    }
}
