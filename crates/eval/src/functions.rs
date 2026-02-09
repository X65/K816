use crate::EvalError;

pub fn invoke(name: &str, args: &[i64]) -> Result<i64, EvalError> {
    match name {
        "min" => expect_arity(name, args, 2).map(|_| args[0].min(args[1])),
        "max" => expect_arity(name, args, 2).map(|_| args[0].max(args[1])),
        "abs" => expect_arity(name, args, 1)
            .and_then(|_| args[0].checked_abs().ok_or(EvalError::Overflow)),
        _ => Err(EvalError::UnknownFunction {
            name: name.to_string(),
        }),
    }
}

fn expect_arity(name: &str, args: &[i64], expected: usize) -> Result<(), EvalError> {
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
