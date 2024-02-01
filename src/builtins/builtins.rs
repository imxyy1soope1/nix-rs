use crate::error::EvalError;
use crate::object::*;

pub fn builtin_fns() -> [(&'static str, bool, Object); 12] {
    [
        (
            "ceil",
            false,
            Object::BuiltinFunction(1, |a, e| match a.borrow_mut()[0].force_value(e) {
                Ok(Object::Int(int)) => Ok(Object::Int(int).into()),
                Ok(Object::Float(float)) => {
                    Ok(Object::Int(unsafe { float.ceil().to_int_unchecked() }).into())
                }
                Err(err) => Err(err),
                _ => Err(EvalError::new("invalid operation").into()),
            }),
        ),
        (
            "floor",
            false,
            Object::BuiltinFunction(1, |a, e| match a.borrow_mut()[0].force_value(e) {
                Ok(Object::Int(int)) => Ok(Object::Int(int).into()),
                Ok(Object::Float(float)) => {
                    Ok(Object::Int(unsafe { float.floor().to_int_unchecked() }).into())
                }
                Err(err) => Err(err),
                _ => Err(EvalError::new("invalid operation").into()),
            }),
        ),
        (
            "typeOf",
            false,
            Object::BuiltinFunction(1, |a, e| {
                use Object::*;
                match a.borrow_mut()[0].force_value(e) {
                    Ok(val) => Ok(Object::Str(
                        match val {
                            Int(_) => "int",
                            Float(_) => "float",
                            Bool(_) => "bool",
                            Null => "null",
                            Attrs(_) => "set",
                            List(_) => "list",
                            Function(..) => "lambda",
                            _ => unreachable!(),
                        }
                        .to_string(),
                    )
                    .into()),
                    err => err,
                }
            }),
        ),
        (
            "isNull",
            false,
            Object::BuiltinFunction(1, |a, e| {
                Ok(Object::Bool(matches!(a.borrow_mut()[0].force_value(e)?, Object::Null)).into())
            }),
        ),
        (
            "isFunction",
            false,
            Object::BuiltinFunction(1, |a, e| {
                Ok(Object::Bool(matches!(
                    a.borrow_mut()[0].force_value(e)?,
                    Object::Function(..)
                ))
                .into())
            }),
        ),
        (
            "isInt",
            false,
            Object::BuiltinFunction(1, |a, e| {
                Ok(
                    Object::Bool(matches!(a.borrow_mut()[0].force_value(e)?, Object::Int(..)))
                        .into(),
                )
            }),
        ),
        (
            "isFloat",
            false,
            Object::BuiltinFunction(1, |a, e| {
                Ok(Object::Bool(matches!(
                    a.borrow_mut()[0].force_value(e)?,
                    Object::Float(..)
                ))
                .into())
            }),
        ),
        (
            "isString",
            false,
            Object::BuiltinFunction(1, |a, e| {
                Ok(
                    Object::Bool(matches!(a.borrow_mut()[0].force_value(e)?, Object::Str(..)))
                        .into(),
                )
            }),
        ),
        (
            "isBool",
            false,
            Object::BuiltinFunction(1, |a, e| {
                Ok(Object::Bool(matches!(
                    a.borrow_mut()[0].force_value(e)?,
                    Object::Bool(..)
                ))
                .into())
            }),
        ),
        (
            "isPath",
            false,
            Object::BuiltinFunction(1, |a, e| {
                Ok(Object::Bool(matches!(
                    a.borrow_mut()[0].force_value(e)?,
                    Object::Path(..)
                ))
                .into())
            }),
        ),
        (
            "seq",
            false,
            Object::BuiltinFunction(2, |a, e| {
                match a.borrow_mut()[0].force_value(e) {
                    Ok(_) => Ok(()),
                    Err(err) => Err(err),
                }?;
                a.borrow_mut()[1].force_value(e)
            }),
        ),
        (
            "deepSeq",
            false,
            Object::BuiltinFunction(2, |a, e| {
                match a.borrow_mut()[0].force_value(e) {
                    Ok(_) => Ok(()),
                    Err(err) => Err(err),
                }?;
                a.borrow_mut()[1].force_value(e)
            }),
        ),
    ]
}
