use crate::convany;
use crate::eval::EvalResult;
use crate::object::*;
use std::fmt::Display;
use std::rc::Rc;

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

pub fn builtin_fns() -> [(&'static str, bool, BuiltinFunction); 12] {
    [
        (
            "ceil",
            false,
            BuiltinFunction::new(1, |a| {
                Ok(if a[0].eval()?.as_any().is::<Float>() {
                    Rc::new(convany!(a[0].eval()?.as_any(), Float).ceil())
                } else {
                    Rc::new(*convany!(a[0].eval()?.as_any(), Int))
                })
            }),
        ),
        (
            "floor",
            false,
            BuiltinFunction::new(1, |a| {
                Ok(if a[0].eval()?.as_any().is::<Float>() {
                    Rc::new(convany!(a[0].eval()?.as_any(), Float).floor())
                } else {
                    Rc::new(*convany!(a[0].eval()?.as_any(), Int))
                })
            }),
        ),
        (
            "typeOf",
            false,
            BuiltinFunction::new(1, |a| {
                let a = a[0].eval()?;
                let a = a.as_any();
                macro_rules! is {
                    ($t:tt) => {
                        a.is::<$t>()
                    };
                }
                Ok(if is!(Int) {
                    Rc::new("int".to_string())
                } else if is!(Float) {
                    Rc::new("float".to_string())
                } else if is!(Str) {
                    Rc::new("string".to_string())
                } else if is!(Bool) {
                    Rc::new("bool".to_string())
                } else if is!(Null) {
                    Rc::new("null".to_string())
                } else if is!(Attrs) {
                    Rc::new("set".to_string())
                } else if is!(List) {
                    Rc::new("list".to_string())
                } else if is!(Lambda) {
                    Rc::new("lambda".to_string())
                } else {
                    unreachable!()
                })
            }),
        ),
        (
            "isNull",
            false,
            BuiltinFunction::new(1, |a| Ok(Rc::new(a[0].eval()?.as_any().is::<Null>()))),
        ),
        (
            "isFunction",
            false,
            BuiltinFunction::new(1, |a| Ok(Rc::new(a[0].eval()?.as_any().is::<Lambda>()))),
        ),
        (
            "isInt",
            false,
            BuiltinFunction::new(1, |a| Ok(Rc::new(a[0].eval()?.as_any().is::<Int>()))),
        ),
        (
            "isFloat",
            false,
            BuiltinFunction::new(1, |a| Ok(Rc::new(a[0].eval()?.as_any().is::<Float>()))),
        ),
        (
            "isString",
            false,
            BuiltinFunction::new(1, |a| Ok(Rc::new(a[0].eval()?.as_any().is::<Str>()))),
        ),
        (
            "isBool",
            false,
            BuiltinFunction::new(1, |a| Ok(Rc::new(a[0].eval()?.as_any().is::<Bool>()))),
        ),
        (
            "isPath",
            false,
            BuiltinFunction::new(1, |a| Ok(Rc::new(a[0].eval()?.as_any().is::<Path>()))),
        ),
        (
            "seq",
            false,
            BuiltinFunction::new(2, |a| {
                a[0].eval()?;
                a[1].eval()
            }),
        ),
        (
            "deepSeq",
            false,
            BuiltinFunction::new(2, |a| {
                a[0].eval()?;
                a[1].eval()
            }),
        ),
    ]
}
