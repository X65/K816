use crate::{EvalError, Number, number_from_f64};

pub fn invoke(name: &str, args: &[Number]) -> Result<Number, EvalError> {
    match name {
        "min" => expect_arity(name, args, 2).and_then(|_| min(args[0], args[1])),
        "max" => expect_arity(name, args, 2).and_then(|_| max(args[0], args[1])),
        "abs" => expect_arity(name, args, 1).and_then(|_| abs(args[0])),
        "sin" => expect_arity(name, args, 1).and_then(|_| number_from_f64(args[0].as_f64().sin())),
        "cos" => expect_arity(name, args, 1).and_then(|_| number_from_f64(args[0].as_f64().cos())),
        "asin" => {
            expect_arity(name, args, 1).and_then(|_| number_from_f64(args[0].as_f64().asin()))
        }
        "acos" => {
            expect_arity(name, args, 1).and_then(|_| number_from_f64(args[0].as_f64().acos()))
        }
        "sqrt" => {
            expect_arity(name, args, 1).and_then(|_| number_from_f64(args[0].as_f64().sqrt()))
        }
        "pow" => expect_arity(name, args, 2)
            .and_then(|_| number_from_f64(args[0].as_f64().powf(args[1].as_f64()))),
        "floor" => {
            expect_arity(name, args, 1).and_then(|_| number_from_f64(args[0].as_f64().floor()))
        }
        "ceil" => {
            expect_arity(name, args, 1).and_then(|_| number_from_f64(args[0].as_f64().ceil()))
        }
        "round" => {
            expect_arity(name, args, 1).and_then(|_| number_from_f64(args[0].as_f64().round()))
        }
        "frac" => {
            expect_arity(name, args, 1).and_then(|_| number_from_f64(args[0].as_f64().fract()))
        }
        "clamp" => expect_arity(name, args, 3).and_then(|_| clamp(args[0], args[1], args[2])),
        // Deferred functions for top-level evaluator semantics.
        "size" => deferred(
            name,
            "section size depends on emission state and is not supported here",
        ),
        "addbyte" => deferred(
            name,
            "section mutation has side effects and is not supported here",
        ),
        "print" => deferred(name, "compile-time side effects are not supported here"),
        "error" => deferred(name, "compile-time side effects are not supported here"),
        "color" => deferred(name, "palette conversion is not supported here"),
        "index" => deferred(name, "array/table indexing helpers are not supported here"),
        "rnd" => deferred(name, "randomness is disabled to keep deterministic output"),
        _ => Err(EvalError::UnknownFunction {
            name: name.to_string(),
        }),
    }
}

fn deferred(name: &str, reason: &'static str) -> Result<Number, EvalError> {
    Err(EvalError::DeferredFunction {
        name: name.to_string(),
        reason,
    })
}

fn min(lhs: Number, rhs: Number) -> Result<Number, EvalError> {
    if let (Some(lhs), Some(rhs)) = (lhs.to_i64_exact(), rhs.to_i64_exact()) {
        return Ok(Number::Int(lhs.min(rhs)));
    }
    number_from_f64(lhs.as_f64().min(rhs.as_f64()))
}

fn max(lhs: Number, rhs: Number) -> Result<Number, EvalError> {
    if let (Some(lhs), Some(rhs)) = (lhs.to_i64_exact(), rhs.to_i64_exact()) {
        return Ok(Number::Int(lhs.max(rhs)));
    }
    number_from_f64(lhs.as_f64().max(rhs.as_f64()))
}

fn abs(value: Number) -> Result<Number, EvalError> {
    match value {
        Number::Int(value) => value
            .checked_abs()
            .map(Number::Int)
            .ok_or(EvalError::Overflow),
        Number::Float(value) => number_from_f64(value.abs()),
    }
}

fn clamp(value: Number, min: Number, max: Number) -> Result<Number, EvalError> {
    if let (Some(value), Some(min), Some(max)) =
        (value.to_i64_exact(), min.to_i64_exact(), max.to_i64_exact())
    {
        return Ok(Number::Int(value.clamp(min, max)));
    }
    number_from_f64(value.as_f64().clamp(min.as_f64(), max.as_f64()))
}

fn expect_arity(name: &str, args: &[Number], expected: usize) -> Result<(), EvalError> {
    if args.len() == expected {
        Ok(())
    } else {
        Err(EvalError::BadArity {
            name: name.to_string(),
            expected,
            got: args.len(),
        })
    }
}
