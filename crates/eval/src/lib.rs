mod expr;
mod functions;

use thiserror::Error;

#[derive(Debug, Error, PartialEq, Eq)]
pub enum EvalError {
    #[error("unexpected token at column {column}")]
    UnexpectedToken { column: usize },
    #[error("unexpected end of expression")]
    UnexpectedEof,
    #[error("invalid number literal '{literal}'")]
    InvalidNumber { literal: String },
    #[error("unknown identifier '{name}'")]
    UnknownIdentifier { name: String },
    #[error("unknown function '{name}'")]
    UnknownFunction { name: String },
    #[error("function '{name}' expected {expected} arguments, got {got}")]
    BadArity {
        name: String,
        expected: usize,
        got: usize,
    },
    #[error("division by zero")]
    DivisionByZero,
    #[error("arithmetic overflow")]
    Overflow,
}

pub fn expand(input: &str) -> Result<String, EvalError> {
    let value = expr::evaluate(input)?;
    Ok(value.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn evaluates_basic_arithmetic() {
        assert_eq!(expand("1 + 2 * 3").expect("eval"), "7");
    }

    #[test]
    fn evaluates_function_call() {
        assert_eq!(expand("max(2, 5)").expect("eval"), "5");
    }
}
