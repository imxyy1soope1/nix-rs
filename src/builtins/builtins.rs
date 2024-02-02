use crate::convany;
use crate::error::EvalError;
use crate::eval::EvalResult;
use crate::object::*;
use std::fmt::Display;
use std::rc::Rc;

#[derive(Debug)]
pub struct PrimOp {
    argscount: u8,
    func: fn(Vec<EvaledOr>) -> EvalResult,
}

#[derive(Debug)]
pub struct PrimOpApp {
    args: Vec<EvaledOr>,
    argsleft: u8,
    func: fn(Vec<EvaledOr>) -> EvalResult,
}

impl PrimOpApp {
    pub fn call(&self, arg: EvaledOr) -> EvalResult {
        let mut args = self.args.clone();
        args.push(arg);
        let a = self.argsleft - 1;
        if a == 0 {
            let f = self.func;
            f(args)
        } else {
            Ok(Rc::new(PrimOpApp {
                args,
                argsleft: a,
                func: self.func,
            }))
        }
    }
}

impl Object for PrimOpApp {
    fn typename(&self) -> &'static str {
        "lambda"
    }

    fn try_into_primop_app(&self) -> Result<&PrimOpApp, Rc<dyn crate::error::NixRsError>> {
        Ok(self)
    }
}

impl Display for PrimOpApp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "«primop-app»")
    }
}

impl PrimOp {
    pub fn new(argscount: u8, func: fn(Vec<EvaledOr>) -> EvalResult) -> PrimOp {
        PrimOp { argscount, func }
    }

    pub fn call(&self, arg: EvaledOr) -> EvalResult {
        let b = PrimOpApp {
            args: Vec::new(),
            argsleft: self.argscount,
            func: self.func,
        };
        b.call(arg)
    }
}

impl Object for PrimOp {
    fn typename(&self) -> &'static str {
        "lambda"
    }

    fn try_into_primop(&self) -> Result<&PrimOp, Rc<dyn crate::error::NixRsError>> {
        Ok(self)
    }
}

impl Display for PrimOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "«primop»")
    }
}

pub fn builtin_fns() -> [(&'static str, bool, PrimOp); 12] {
    [
        (
            "ceil",
            false,
            PrimOp::new(1, |a| {
                let o = a[0].eval()?;
                Ok(if let Ok(int) = o.try_into_int() {
                    Rc::new(int)
                } else if let Ok(float) = o.try_into_float() {
                    Rc::new(float.ceil().round())
                } else {
                    return Err(EvalError::from(format!("value is a '{}' while a float was expected", o.typename())).into())
                })
            }),
        ),
        (
            "floor",
            false,
            PrimOp::new(1, |a| {
                let o = a[0].eval()?;
                Ok(if let Ok(int) = o.try_into_int() {
                    Rc::new(int)
                } else if let Ok(float) = o.try_into_float() {
                    Rc::new(float.floor().round())
                } else {
                    return Err(EvalError::from(format!("value is a '{}' while a float was expected", o.typename())).into())
                })
            }),
        ),
        (
            "typeOf",
            false,
            PrimOp::new(1, |a| {
                Ok(Rc::new(a[0].eval()?.typename().to_string()))
            }),
        ),
        (
            "isNull",
            false,
            PrimOp::new(1, |a| Ok(Rc::new(a[0].eval()?.is_null()))),
        ),
        (
            "isFunction",
            false,
            PrimOp::new(1, |a| Ok(Rc::new(a[0].eval()?.typename() == "lambda"))),
        ),
        (
            "isInt",
            false,
            PrimOp::new(1, |a| Ok(Rc::new(a[0].eval()?.try_into_int().is_ok()))),
        ),
        (
            "isFloat",
            false,
            PrimOp::new(1, |a| Ok(Rc::new(a[0].eval()?.try_into_float().is_ok()))),
        ),
        (
            "isString",
            false,
            PrimOp::new(1, |a| Ok(Rc::new(a[0].eval()?.try_into_string().is_ok()))),
        ),
        (
            "isBool",
            false,
            PrimOp::new(1, |a| Ok(Rc::new(a[0].eval()?.try_into_bool().is_ok()))),
        ),
        (
            "isPath",
            false,
            PrimOp::new(1, |a| Ok(Rc::new(a[0].eval()?.try_into_path().is_ok()))),
        ),
        (
            "seq",
            false,
            PrimOp::new(2, |a| {
                a[0].eval()?;
                a[1].eval()
            }),
        ),
        (
            "deepSeq",
            false,
            PrimOp::new(2, |a| {
                a[0].eval()?;
                a[1].eval()
            }),
        ),
    ]
}
