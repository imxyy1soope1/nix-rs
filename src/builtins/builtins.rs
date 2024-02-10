use crate::convany;
use crate::error::ErrorCtx;
use crate::eval::EvalResult;
use crate::object::*;
use std::fmt::Display;

#[derive(Debug, Clone)]
pub struct PrimOp {
    arity: u8,
    ctx: ErrorCtx,
    func: fn(Vec<Object>, &ErrorCtx) -> EvalResult,
}

#[derive(Debug, Clone)]
pub struct PrimOpApp {
    args: Vec<Object>,
    arity: u8,
    ctx: ErrorCtx,
    func: fn(Vec<Object>, &ErrorCtx) -> EvalResult,
}

impl PrimOpApp {
    pub fn call(self, arg: Object) -> EvalResult {
        let mut args = self.args;
        args.push(arg);
        let a = self.arity - 1;
        if a == 0 {
            let f = self.func;
            f(args, &self.ctx)
        } else {
            Ok(Box::new(PrimOpApp {
                args,
                arity: a,
                ..self
            }))
        }
    }
}

impl PrimOp {
    pub fn new(arity: u8, ctx: ErrorCtx, func: fn(Vec<Object>, &ErrorCtx) -> EvalResult) -> PrimOp {
        PrimOp { arity, ctx, func }
    }

    pub fn call(&self, arg: Object, ctx: ErrorCtx) -> EvalResult {
        let b = PrimOpApp {
            args: Vec::with_capacity(self.arity as usize),
            arity: self.arity,
            func: self.func,
            ctx,
        };
        b.call(arg)
    }
}

pub fn builtin_fns() -> [(&'static str, bool, PrimOp); 12] {
    [
        ("ceil", false, PrimOp::new(1, |a| Ok())),
        (
            "floor",
            false,
            PrimOp::new(1, |a| {
                Ok(if a[0].as_any().is::<Float>() {
                    Box::new(convany!(a[0].as_any(), Float).floor())
                } else {
                    Box::new(*convany!(a[0].as_any(), Int))
                })
            }),
        ),
        (
            "typeOf",
            false,
            PrimOp::new(1, |a| {
                let a = a[0];
                let a = a.as_any();
                macro_rules! is {
                    ($t:tt) => {
                        a.is::<$t>()
                    };
                }
                Ok(if is!(Int) {
                    Box::new("int".to_string())
                } else if is!(Float) {
                    Box::new("float".to_string())
                } else if is!(Str) {
                    Box::new("string".to_string())
                } else if is!(Bool) {
                    Box::new("bool".to_string())
                } else if is!(Null) {
                    Box::new("null".to_string())
                } else if is!(Attrs) {
                    Box::new("set".to_string())
                } else if is!(List) {
                    Box::new("list".to_string())
                } else if is!(Lambda) {
                    Box::new("lambda".to_string())
                } else {
                    unreachable!()
                })
            }),
        ),
        (
            "isNull",
            false,
            PrimOp::new(1, |a| Ok(Box::new(a[0].as_any().is::<Null>()))),
        ),
        (
            "isFunction",
            false,
            PrimOp::new(1, |a| Ok(Box::new(a[0].as_any().is::<Lambda>()))),
        ),
        (
            "isInt",
            false,
            PrimOp::new(1, |a| Ok(Box::new(a[0].as_any().is::<Int>()))),
        ),
        (
            "isFloat",
            false,
            PrimOp::new(1, |a| Ok(Box::new(a[0].as_any().is::<Float>()))),
        ),
        (
            "isString",
            false,
            PrimOp::new(1, |a| Ok(Box::new(a[0].as_any().is::<Str>()))),
        ),
        (
            "isBool",
            false,
            PrimOp::new(1, |a| Ok(Box::new(a[0].as_any().is::<Bool>()))),
        ),
        (
            "isPath",
            false,
            PrimOp::new(1, |a| Ok(Box::new(a[0].as_any().is::<Path>()))),
        ),
        (
            "seq",
            false,
            PrimOp::new(2, |a| {
                a[0].force_value();
                a[1].force_value()
            }),
        ),
        (
            "deepSeq",
            false,
            PrimOp::new(2, |a| {
                a[0].force_value_deep();
                a[1].force_value()
            }),
        ),
    ]
}
