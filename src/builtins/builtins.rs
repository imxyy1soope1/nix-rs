use crate::convany;
use crate::error::ErrorCtx;
use crate::eval::EvalResult;
use crate::object::*;
use std::fmt::Display;

#[derive(Debug, Clone)]
pub struct PrimOp {
    arity: u8,
    func: fn(Vec<Object>, &ErrorCtx) -> EvalResult,
}

#[derive(Debug, Clone)]
pub struct PrimOpApp {
    args: Vec<Object>,
    arity: u8,
    func: fn(Vec<Object>, &ErrorCtx) -> EvalResult,
}

impl PrimOpApp {
    pub fn new(
        args: Vec<Object>,
        arity: u8,
        func: fn(Vec<Object>, &ErrorCtx) -> EvalResult,
    ) -> PrimOpApp {
        PrimOpApp { args, arity, func }
    }

    pub fn call(self, arg: Object, ctx: &ErrorCtx) -> EvalResult {
        let mut args = self.args;
        args.push(arg);
        let a = self.arity - 1;
        if a == 0 {
            let f = self.func;
            f(args, ctx)
        } else {
            Ok(Object::mk_prim_op_app(args, a, self.func))
        }
    }
}

impl PrimOp {
    pub fn new(arity: u8, func: fn(Vec<Object>, &ErrorCtx) -> EvalResult) -> PrimOp {
        PrimOp { arity, func }
    }

    pub fn call(&self, arg: Object, ctx: &ErrorCtx) -> EvalResult {
        let b = PrimOpApp {
            args: Vec::with_capacity(self.arity as usize),
            arity: self.arity,
            func: self.func,
        };
        b.call(arg, ctx)
    }
}

pub fn builtin_fns() -> [(&'static str, bool, PrimOp); 11] {
    [
        (
            "ceil",
            false,
            PrimOp::new(1, |args, ctx| {
                let float: f64 = args[0].try_into().map_err(|e| ctx.unwind(e))?;
                Ok(Object::mk_int(float.ceil() as i64))
            }),
        ),
        (
            "floor",
            false,
            PrimOp::new(1, |args, ctx| {
                let float: f64 = args[0].try_into().map_err(|e| ctx.unwind(e))?;
                Ok(Object::mk_int(float.floor() as i64))
            }),
        ),
        (
            "typeOf",
            false,
            PrimOp::new(1, |args, _ctx| {
                args[0].force_value()?;
                Ok(Object::mk_string(args[0].typename().to_string()))
            }),
        ),
        (
            "isNull",
            false,
            PrimOp::new(1, |args, _ctx| {
                args[0].force_value()?;
                Ok(Object::mk_bool(args[0].is_null()))
            })
        ),
        (
            "isFunction",
            false,
            PrimOp::new(1, |args, _ctx| {
                args[0].force_value()?;
                Ok(Object::mk_bool(args[0].is_function()))
            })
        ),
        (
            "isInt",
            false,
            PrimOp::new(1, |args, _ctx| {
                args[0].force_value()?;
                Ok(Object::mk_bool(args[0].is_int()))
            })
        ),
        (
            "isFloat",
            false,
            PrimOp::new(1, |args, _ctx| {
                args[0].force_value()?;
                Ok(Object::mk_bool(args[0].is_float()))
            })
        ),
        (
            "isString",
            false,
            PrimOp::new(1, |args, _ctx| {
                args[0].force_value()?;
                Ok(Object::mk_bool(args[0].is_str()))
            })
        ),
        (
            "isBool",
            false,
            PrimOp::new(1, |args, _ctx| {
                args[0].force_value()?;
                Ok(Object::mk_bool(args[0].is_bool()))
            })
        ),
        /* (
            "isPath",
            false,
            PrimOp::new(1, |args, _ctx| {
                args[0].force_value()?;
                Ok(Object::mk_bool(args[0].is_()))
            })
        ), */
        (
            "seq",
            false,
            PrimOp::new(2, |args, _ctx| {
                args[0].force_value()?;
                args[1].force_value();
                Ok(args[1])
            }),
        ),
        (
            "deepSeq",
            false,
            PrimOp::new(2, |args, _ctx| {
                args[0].force_value_deep()?;
                args[1].force_value();
                Ok(args[1])
            }),
        ),
    ]
}
