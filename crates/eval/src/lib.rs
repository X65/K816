mod expr;
mod functions;

use std::collections::BTreeMap;
use std::fmt;

use thiserror::Error;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Number {
    Int(i64),
    Float(f64),
}

impl Number {
    pub const fn int(value: i64) -> Self {
        Self::Int(value)
    }

    pub const fn float(value: f64) -> Self {
        Self::Float(value)
    }

    pub fn as_f64(self) -> f64 {
        match self {
            Self::Int(value) => value as f64,
            Self::Float(value) => value,
        }
    }

    pub fn to_i64_exact(self) -> Option<i64> {
        match self {
            Self::Int(value) => Some(value),
            Self::Float(value) => {
                if !value.is_finite() || value.fract() != 0.0 {
                    return None;
                }
                if value < i64::MIN as f64 || value > i64::MAX as f64 {
                    return None;
                }
                Some(value as i64)
            }
        }
    }

    pub fn is_truthy(self) -> bool {
        match self {
            Self::Int(value) => value != 0,
            Self::Float(value) => value != 0.0,
        }
    }

    pub fn checked_add(self, rhs: Self) -> Result<Self, EvalError> {
        match (self, rhs) {
            (Self::Int(lhs), Self::Int(rhs)) => lhs
                .checked_add(rhs)
                .map(Self::Int)
                .ok_or(EvalError::Overflow),
            (lhs, rhs) => number_from_f64(lhs.as_f64() + rhs.as_f64()),
        }
    }

    pub fn checked_sub(self, rhs: Self) -> Result<Self, EvalError> {
        match (self, rhs) {
            (Self::Int(lhs), Self::Int(rhs)) => lhs
                .checked_sub(rhs)
                .map(Self::Int)
                .ok_or(EvalError::Overflow),
            (lhs, rhs) => number_from_f64(lhs.as_f64() - rhs.as_f64()),
        }
    }

    pub fn checked_mul(self, rhs: Self) -> Result<Self, EvalError> {
        match (self, rhs) {
            (Self::Int(lhs), Self::Int(rhs)) => lhs
                .checked_mul(rhs)
                .map(Self::Int)
                .ok_or(EvalError::Overflow),
            (lhs, rhs) => number_from_f64(lhs.as_f64() * rhs.as_f64()),
        }
    }

    pub fn checked_div(self, rhs: Self) -> Result<Self, EvalError> {
        match (self, rhs) {
            (Self::Int(_), Self::Int(0)) | (Self::Float(_), Self::Float(0.0)) => {
                Err(EvalError::DivisionByZero)
            }
            (Self::Float(_), Self::Int(0)) | (Self::Int(_), Self::Float(0.0)) => {
                Err(EvalError::DivisionByZero)
            }
            (Self::Int(lhs), Self::Int(rhs)) => {
                if lhs == i64::MIN && rhs == -1 {
                    return Err(EvalError::Overflow);
                }
                if lhs % rhs == 0 {
                    Ok(Self::Int(lhs / rhs))
                } else {
                    number_from_f64(lhs as f64 / rhs as f64)
                }
            }
            (lhs, rhs) => number_from_f64(lhs.as_f64() / rhs.as_f64()),
        }
    }

    pub fn checked_rem(self, rhs: Self) -> Result<Self, EvalError> {
        let lhs = self
            .to_i64_exact()
            .ok_or(EvalError::IntegerRequired { op: "%" })?;
        let rhs = rhs
            .to_i64_exact()
            .ok_or(EvalError::IntegerRequired { op: "%" })?;
        if rhs == 0 {
            return Err(EvalError::DivisionByZero);
        }
        lhs.checked_rem(rhs)
            .map(Self::Int)
            .ok_or(EvalError::Overflow)
    }

    pub fn checked_shl(self, rhs: Self) -> Result<Self, EvalError> {
        let lhs = self
            .to_i64_exact()
            .ok_or(EvalError::IntegerRequired { op: "<<" })?;
        let rhs = rhs
            .to_i64_exact()
            .ok_or(EvalError::IntegerRequired { op: "<<" })?;
        if rhs < 0 {
            return Err(EvalError::Overflow);
        }
        lhs.checked_shl(rhs as u32)
            .map(Self::Int)
            .ok_or(EvalError::Overflow)
    }

    pub fn checked_shr(self, rhs: Self) -> Result<Self, EvalError> {
        let lhs = self
            .to_i64_exact()
            .ok_or(EvalError::IntegerRequired { op: ">>" })?;
        let rhs = rhs
            .to_i64_exact()
            .ok_or(EvalError::IntegerRequired { op: ">>" })?;
        if rhs < 0 {
            return Err(EvalError::Overflow);
        }
        lhs.checked_shr(rhs as u32)
            .map(Self::Int)
            .ok_or(EvalError::Overflow)
    }

    pub fn bit_and(self, rhs: Self) -> Result<Self, EvalError> {
        let lhs = self
            .to_i64_exact()
            .ok_or(EvalError::IntegerRequired { op: "&" })?;
        let rhs = rhs
            .to_i64_exact()
            .ok_or(EvalError::IntegerRequired { op: "&" })?;
        Ok(Self::Int(lhs & rhs))
    }

    pub fn bit_xor(self, rhs: Self) -> Result<Self, EvalError> {
        let lhs = self
            .to_i64_exact()
            .ok_or(EvalError::IntegerRequired { op: "^" })?;
        let rhs = rhs
            .to_i64_exact()
            .ok_or(EvalError::IntegerRequired { op: "^" })?;
        Ok(Self::Int(lhs ^ rhs))
    }

    pub fn bit_or(self, rhs: Self) -> Result<Self, EvalError> {
        let lhs = self
            .to_i64_exact()
            .ok_or(EvalError::IntegerRequired { op: "|" })?;
        let rhs = rhs
            .to_i64_exact()
            .ok_or(EvalError::IntegerRequired { op: "|" })?;
        Ok(Self::Int(lhs | rhs))
    }

    pub fn bit_not(self) -> Result<Self, EvalError> {
        let value = self
            .to_i64_exact()
            .ok_or(EvalError::IntegerRequired { op: "~" })?;
        Ok(Self::Int(!value))
    }

    pub fn checked_neg(self) -> Result<Self, EvalError> {
        match self {
            Self::Int(value) => value
                .checked_neg()
                .map(Self::Int)
                .ok_or(EvalError::Overflow),
            Self::Float(value) => number_from_f64(-value),
        }
    }
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(value) => write!(f, "{value}"),
            Self::Float(value) => write!(f, "{value}"),
        }
    }
}

pub(crate) fn number_from_f64(value: f64) -> Result<Number, EvalError> {
    if !value.is_finite() {
        return Err(EvalError::Overflow);
    }
    if value.fract() == 0.0 && value >= i64::MIN as f64 && value <= i64::MAX as f64 {
        return Ok(Number::Int(value as i64));
    }
    Ok(Number::Float(value))
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct EvalContext {
    symbols: BTreeMap<String, Number>,
    arrays: BTreeMap<String, Vec<Number>>,
}

impl EvalContext {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get(&self, name: &str) -> Option<Number> {
        self.symbols.get(name).copied()
    }

    pub fn set(&mut self, name: impl Into<String>, value: Number) {
        self.symbols.insert(name.into(), value);
    }

    pub fn get_array(&self, name: &str) -> Option<&[Number]> {
        self.arrays.get(name).map(Vec::as_slice)
    }

    pub fn set_array(&mut self, name: impl Into<String>, values: Vec<Number>) {
        self.arrays.insert(name.into(), values);
    }

    pub fn iter(&self) -> impl Iterator<Item = (&str, Number)> {
        self.symbols
            .iter()
            .map(|(name, value)| (name.as_str(), *value))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct EvalOutcome {
    pub value: Number,
    pub assigned: Vec<(String, Number)>,
}

#[derive(Debug, Error, PartialEq, Eq)]
pub enum EvalError {
    #[error("unexpected token {token} at column {column}")]
    UnexpectedToken { column: usize, token: String },
    #[error("unexpected end of expression")]
    UnexpectedEof,
    #[error("invalid number literal '{literal}'")]
    InvalidNumber { literal: String },
    #[error("unknown identifier '{name}'")]
    UnknownIdentifier {
        name: String,
        start: usize,
        end: usize,
    },
    #[error("unknown function '{name}'")]
    UnknownFunction { name: String },
    #[error("function '{name}' expected {expected} arguments, got {got}")]
    BadArity {
        name: String,
        expected: usize,
        got: usize,
    },
    #[error("invalid assignment target")]
    InvalidAssignmentTarget,
    #[error("operator '{op}' requires exact integer operands")]
    IntegerRequired { op: &'static str },
    #[error("function '{name}' is deferred: {reason}")]
    DeferredFunction { name: String, reason: &'static str },
    #[error("array literal can only be assigned to an identifier or indexed")]
    ArrayLiteralInNumericContext,
    #[error("division by zero")]
    DivisionByZero,
    #[error("arithmetic overflow")]
    Overflow,
}

pub fn expand(input: &str) -> Result<String, EvalError> {
    let value = evaluate(input)?;
    Ok(value.to_string())
}

pub fn evaluate(input: &str) -> Result<Number, EvalError> {
    let mut context = EvalContext::default();
    expr::evaluate(input, &mut context).map(|outcome| outcome.value)
}

pub fn evaluate_with_context(
    input: &str,
    context: &mut EvalContext,
) -> Result<EvalOutcome, EvalError> {
    expr::evaluate(input, context)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn evaluates_basic_arithmetic() {
        assert_eq!(evaluate("1 + 2 * 3").expect("eval"), Number::Int(7));
    }

    #[test]
    fn evaluates_function_call() {
        assert_eq!(evaluate("max(2, 5)").expect("eval"), Number::Int(5));
    }
}
