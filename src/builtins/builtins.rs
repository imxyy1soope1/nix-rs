use crate::ast::Node;
use crate::convany;
use crate::error::EvalError;
use crate::eval::EvalResult;
use crate::object::*;
use std::fmt::Display;
use std::rc::Rc;

/*
#[derive(Debug)]
pub struct BuiltinFunction {
    argscount: u8,
    func: fn(Vec<EvaledOr>) -> EvalResult,
}

#[derive(Debug)]
pub struct BuiltinFunctionApp {
    args: Vec<EvaledOr>,
    argsleft: u8,
    func: fn(Vec<EvaledOr>) -> EvalResult,
}

impl BuiltinFunctionApp {
    pub fn call(&self, arg: EvaledOr) -> EvalResult {
        let mut args = self.args.clone();
        args.push(arg);
        let a = self.argsleft - 1;
        if a == 0 {
            let f = self.func;
            f(args)
        } else {
            Ok(Rc::new(BuiltinFunctionApp {
                args,
                argsleft: a,
                func: self.func,
            }))
        }
    }
}

impl Object for BuiltinFunctionApp {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

impl Display for BuiltinFunctionApp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "«primop-app»")
    }
}

impl BuiltinFunction {
    pub fn new(argscount: u8, func: fn(Vec<EvaledOr>) -> EvalResult) -> BuiltinFunction {
        BuiltinFunction { argscount, func }
    }

    pub fn call(&self, arg: EvaledOr) -> EvalResult {
        let b = BuiltinFunctionApp {
            args: Vec::new(),
            argsleft: self.argscount,
            func: self.func,
        };
        b.call(arg)
    }
}

impl Object for BuiltinFunction {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

impl Display for BuiltinFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "«primop»")
    }
}
*/

pub fn builtin_fns() -> Vec<(&'static str, bool, Object)> {
    vec![
        (
            "ceil",
            false,
            Object::BuiltinFunction(1, |a| match a[0].force_value() {
                Ok(Object::Int(int)) => Node::Value(Box::new(Object::Int(*int))),
                Ok(Object::Float(float)) => {
                    Node::Value(Box::new(Object::Int(unsafe { float.ceil().to_int_unchecked() })))
                }
                Err(err) => Node::Error(err),
                _ => Node::Error(EvalError::new("invalid operation"))
            }),
        ),
        (
            "floor",
            false,
            Object::BuiltinFunction(1, |a| match a[0].force_value() {
                Ok(Object::Int(int)) => Node::Value(Box::new(Object::Int(*int))),
                Ok(Object::Float(float)) => {
                    Node::Value(Box::new(Object::Int(unsafe { float.floor().to_int_unchecked() })))
                }
                Err(err) => Node::Error(err),
                _ => Node::Error(EvalError::new("invalid operation"))
            }),
        ),
        (
            "typeOf",
            false,
            Object::BuiltinFunction(1, |a| {
                use Object::*;
                match a[0].force_value() {
                    Ok(val) => Node::Value(Box::new(Object::Str(match val {
                        Int(_) => "int",
                        Float(_) => "float",
                        Bool(_) => "bool",
                        Null => "null",
                        Attrs(_) => "set",
                        List(_) => "list",
                        Function(..) => "lambda",
                        _ => unreachable!()
                    }.to_string()))),
                    Err(err) => Node::Error(err),
                }
            }),
        ),
        (
            "isNull",
            false,
            Object::BuiltinFunction(1, |a| {
                Node::Value(Box::new(Object::Bool(if let Ok(Object::Null) = a[0].force_value() {
                    true
                } else {
                    false
                })))
            }),
        ),
        (
            "isFunction",
            false,
            Object::BuiltinFunction(1, |a| {
                Node::Value(Box::new(Object::Bool(if let Ok(Object::Function(..)) = a[0].force_value() {
                    true
                } else {
                    false
                })))
            })
        ),
        (
            "isInt",
            false,
            Object::BuiltinFunction(1, |a| {
                Node::Value(Box::new(Object::Bool(if let Ok(Object::Int(..)) = a[0].force_value() {
                    true
                } else {
                    false
                })))
            })
        ),
        (
            "isFloat",
            false,
            Object::BuiltinFunction(1, |a| {
                Node::Value(Box::new(Object::Bool(if let Ok(Object::Float(..)) = a[0].force_value() {
                    true
                } else {
                    false
                })))
            })
        ),
        (
            "isString",
            false,
            Object::BuiltinFunction(1, |a| {
                Node::Value(Box::new(Object::Bool(if let Ok(Object::Str(..)) = a[0].force_value() {
                    true
                } else {
                    false
                })))
            })
        ),
        (
            "isBool",
            false,
            Object::BuiltinFunction(1, |a| {
                Node::Value(Box::new(Object::Bool(if let Ok(Object::Bool(..)) = a[0].force_value() {
                    true
                } else {
                    false
                })))
            })
        ),
        (
            "isPath",
            false,
            Object::BuiltinFunction(1, |a| {
                Node::Value(Box::new(Object::Bool(if let Ok(Object::Path(..)) = a[0].force_value() {
                    true
                } else {
                    false
                })))
            })
        ),
        (
            "seq",
            false,
            Object::BuiltinFunction(2, |a| {
                match a[0].force_value() {
                    Ok(_) => (),
                    Err(err) => return Node::Error(err)
                }
                match a[1].force_value() {
                    Ok(val) => Node,
                    Err(err) => Node::Error(err)
                }
            }),
        ),
        (
            "deepSeq",
            false,
            Object::BuiltinFunction(2, |a| {
                a[0].eval()?;
                a[1].eval()
            }),
        ),
    ]
}
