use crate::error::EvalError;
use crate::object::*;

pub fn builtin_fns() -> [(&'static str, bool, Object); 12] {
    [
        (
            "ceil",
            false,
            Object::BuiltinFunction(1, |a| match a.borrow_mut()[0].force_value() {
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
            Object::BuiltinFunction(1, |a| match a.borrow_mut()[0].force_value() {
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
            Object::BuiltinFunction(1, |a| {
                use Object::*;
                match a.borrow_mut()[0].force_value() {
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
            Object::BuiltinFunction(1, |a| {
                Ok(Object::Bool(matches!(a.borrow_mut()[0].force_value()?, Object::Null)).into())
            }),
        ),
        (
            "isFunction",
            false,
            Object::BuiltinFunction(1, |a| {
                Ok(Object::Bool(matches!(
                    a.borrow_mut()[0].force_value()?,
                    Object::Function(..)
                ))
                .into())
            }),
        ),
        (
            "isInt",
            false,
            Object::BuiltinFunction(1, |a| {
                Ok(
                    Object::Bool(matches!(a.borrow_mut()[0].force_value()?, Object::Int(..)))
                        .into(),
                )
            }),
        ),
        (
            "isFloat",
            false,
            Object::BuiltinFunction(1, |a| {
                Ok(Object::Bool(matches!(
                    a.borrow_mut()[0].force_value()?,
                    Object::Float(..)
                ))
                .into())
            }),
        ),
        (
            "isString",
            false,
            Object::BuiltinFunction(1, |a| {
                Ok(
                    Object::Bool(matches!(a.borrow_mut()[0].force_value()?, Object::Str(..)))
                        .into(),
                )
            }),
        ),
        (
            "isBool",
            false,
            Object::BuiltinFunction(1, |a| {
                Ok(
                    Object::Bool(matches!(a.borrow_mut()[0].force_value()?, Object::Bool(..)))
                        .into(),
                )
            }),
        ),
        (
            "isPath",
            false,
            Object::BuiltinFunction(1, |a| {
                Ok(
                    Object::Bool(matches!(a.borrow_mut()[0].force_value()?, Object::Path(..)))
                        .into(),
                )
            }),
        ),
        (
            "seq",
            false,
            Object::BuiltinFunction(2, |a| {
                match a.borrow_mut()[0].force_value() {
                    Ok(_) => Ok(()),
                    Err(err) => Err(err),
                }?;
                a.borrow_mut()[1].force_value()
            }),
        ),
        (
            "deepSeq",
            false,
            Object::BuiltinFunction(2, |a| {
                match a.borrow_mut()[0].force_value() {
                    Ok(_) => Ok(()),
                    Err(err) => Err(err),
                }?;
                a.borrow_mut()[1].force_value()
            }),
        ),
    ]
}
