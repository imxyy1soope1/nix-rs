use crate::ast::{ArgSetExpr, Expression, IdentifierExpr, AttrsLiteralExpr, FunctionLiteralExpr, ListLiteralExpr};
use crate::builtins::{PrimOp, PrimOpApp};
use crate::convany;
use crate::error::*;
use crate::eval::{Environment, Env, EvalResult};

use std::any::Any;
use std::rc::Rc;
use std::cell::RefCell;
use std::fmt::{Debug, Display};

#[derive(Debug, Clone)]
enum _Object {
    Int(i64),
    Float(f64),
    Boll(bool),
    Null,
    Str(String),
    List(Vec<Object>),
    Lambda{func: FunctionLiteralExpr, env: Env},
    Attrs(Env),
    // PrimOp{arity:u8, ctx: ErrorCtx, func: fn(Vec<Object>, ctx: &ErrorCtx) -> Object},
    // PrimOpApp{arity:u8, args: ctx: ErrorCtx, func: fn(Vec<Object>, ctx: &ErrorCtx) -> Object},
    PrimOp(PrimOp),
    PrimOpApp(PrimOpApp),

    ClosureThunk(Env, Box<dyn Expression>),
    FunctionCallThunk{func: Object, arg: Object},
}

#[derive(Debug, Clone)]
pub struct Object {
    val: _Object
}

impl From<i64> for Object {
    fn from(value: i64) -> Self {
        Object { val: _Object::Int(value) }
    }
}

impl From<f64> for Object {
    fn from(value: f64) -> Self {
        Object { val: _Object::Float(value) }
    }
}

impl From<bool> for Object {
    fn from(value: bool) -> Self {
        Object { val: _Object::Bool(value) }
    }
}

impl From<String> for Object {
    fn from(value: String) -> Self {
        Object { val: _Object::Str(value) }
    }
}

impl From<Vec<Object>> for Object {
    fn from(value: Vec<Object>) -> Self {
        Object { val: _Object::List(value) }
    }
}

macro_rules! mkval {
    ($name:ident, $t:ty) => {
        pub fn $name(value: $t) -> Object {
            value.into()
        }
    };
}
impl Object {
    mkval!{mk_int, i64}
    mkval!{mk_float, f64}
    mkval!{mk_bool, bool}
    mkval!{mk_string, String}
    mkval!{mk_list, Vec<Object>}
    pub fn mk_null() -> Object {
        Object { val: _Object::Null }
    }
    pub fn mk_lambda(func: FunctionLiteralExpr, env: Env) -> Object {
        Object { val: _Object::Lambda { func, env } }
    }
    pub fn mk_attrs(env: Env) -> Object {
        Object { val: _Object::Attrs(env) }
    }
    pub fn mk_thunk(env: Env, expr: Box<dyn Expression>) -> Object {
        Object { val: _Object::ClosureThunk(env, expr) }
    }
    pub fn mk_call(func: Object, arg: Object) -> Object {
        Object { val: _Object::FunctionCallThunk { func, arg } }
    }
    pub fn mk_prim_op(arity: u8, ctx: ErrorCtx, func: fn(Vec<Object>, ctx: &ErrorCtx) -> Object) -> Object {
        Object { val: _Object::PrimOp(PrimOp::new(arity, ctx, func ) }
    }
    pub fn mk_prim_op_app(arity: u8, ctx: ErrorCtx, func: fn(Vec<Object>, ctx: &ErrorCtx) -> Object) -> Object {
        Object { val: _Object::PrimOpApp { arity, ctx, func } }
    }
}

/*
pub trait Object: Display + Debug {
    fn as_any(&self) -> &dyn Any;
    fn into_any(self: Box<Self>) -> Box<dyn Any>;
    fn force_value(&self)
}

pub type Int = i64;

impl Object for Int {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn into_any(self: Box<Self>) -> Box<dyn Any> {
        self
    }
}

pub type Float = f64;

impl Object for Float {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn into_any(self: Box<Self>) -> Box<dyn Any> {
        self
    }
}

pub type Bool = bool;

impl Object for bool {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn into_any(self: Box<Self>) -> Box<dyn Any> {
        self
    }
}

#[derive(Debug)]
pub struct Null {}

impl Object for Null {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn into_any(self: Box<Self>) -> Box<dyn Any> {
        self
    }
}

impl Display for Null {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "null")
    }
}

pub type Str = String;

impl Object for Str {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn into_any(self: Box<Self>) -> Box<dyn Any> {
        self
    }
}

pub struct InterpolateStr {}

impl InterpolateStr {
    pub fn new(value: String, replaces: Vec<(usize, Box<dyn Object>)>) -> Str {
        let mut offset = 0;
        let mut value = value;
        for (i, o) in replaces.into_iter() {
            let (p1, p2) = value.split_at(i + offset);
            let s = convany!(o.as_any(), Str).clone();
            value = format!("{p1}{s}{p2}");
            offset += s.len()
        }
        value
    }
}

#[derive(Debug, Clone)]
pub struct List {
    pub value: Vec<Box<dyn Object>>,
}

impl List {
    pub fn new(value: Vec<Box<dyn Object>>) -> List {
        List { value }
    }

    pub fn concat(&self, other: &dyn Object) -> List {
        let mut new = self.clone();
        new.value
            .extend(convany!(other.as_any(), List).value.clone());
        new
    }
}

impl Object for List {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn into_any(self: Box<Self>) -> Box<dyn Any> {
        self
    }
}

impl Display for List {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "[ ")?;
        for v in self.value.iter() {
            write!(f, "{} ", {
                if v.expr_is::<AttrsLiteralExpr>() {
                    "{ ... }".to_string()
                } else if v.expr_is::<ListLiteralExpr>() {
                    "[ ... ]".to_string()
                } else {
                    v.eval().unwrap().to_string()
                }
            })?;
        }
        write!(f, "]")
    }
}

#[derive(Debug)]
pub struct Lambda {
    arg: Rc<dyn Expression>,
    body: Rc<dyn Expression>,
    env: Rc<RefCell<Environment>>,
}

impl Lambda {
    pub fn new(
        arg: Rc<dyn Expression>,
        body: Rc<dyn Expression>,
        env: Rc<RefCell<Environment>>,
    ) -> Lambda {
        Lambda { arg, body, env }
    }

    pub fn call(&self, arg: Box<dyn Object>, ctx: ErrorCtx) -> EvalResult {
        let callenv = Rc::new(RefCell::new(Environment::new(Some(self.env.clone()))));
        if !self.arg.as_any().is::<ArgSetExpr>() {
            // IdentifierExpr
            callenv
                .borrow_mut()
                .set(
                    convany!(self.arg.as_any(), IdentifierExpr).ident.clone(),
                    Box<dyn Object>::evaled(arg.into()),
                )
                .unwrap();
            return self.body.eval(&callenv, &ctx);
        }
        for a in convany!(self.arg.as_any(), ArgSetExpr).args.iter() {
            let ident = a.0.clone();
            let e = {
                let t = convany!(arg.as_any(), Attrs).env.borrow().get(&ident);
                if let Ok(o) = t {
                    o
                } else {
                    drop(t); // to prevent multiple borrow of env (from `t` above)
                    println!("default {ident}");
                    Box<dyn Object>::expr(callenv.clone(), a.1.clone().unwrap(), ctx.clone())
                }
            };

            callenv.borrow_mut().set(ident, e).unwrap();
        }
        convany!(self.arg.as_any(), ArgSetExpr)
            .alias
            .clone()
            .map(|a| callenv.borrow_mut().set(a, Box<dyn Object>::evaled(arg.clone())));
        self.body.eval(&callenv, &ctx)
    }
}

impl Object for Lambda {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn into_any(self: Box<Self>) -> Box<dyn Any> {
        self
    }
}

impl Display for Lambda {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "«lambda»")
    }
}

#[derive(Debug, Clone)]
pub struct Attrs {
    pub env: Rc<RefCell<Environment>>,
}

impl Attrs {
    pub fn new(env: Rc<RefCell<Environment>>) -> Attrs {
        Attrs { env }
    }

    pub fn merge(&self, other: Rc<dyn Object>) -> Result<(), Rc<dyn NixRsError>> {
        let other = convany!(other.as_any(), Attrs);
        for (k, v) in other.env.borrow().iter() {
            let ret = self.env.borrow_mut().set(k.clone(), v.clone());
            if ret.is_err() {
                drop(ret);
                convany!(self.env.borrow().get(k).unwrap().eval()?.as_any(), Attrs)
                    .merge(v.eval()?)?;
            }
        }
        Ok(())
    }

    pub fn update(&self, other: &dyn Object) -> EvalResult {
        let new = self.clone();
        let other = convany!(other.as_any(), Attrs);
        for (k, v) in other.env.borrow().iter() {
            let ret = new.env.borrow_mut().set(k.clone(), v.clone());
            if ret.is_err() {
                drop(ret);
                let o = new.env.borrow().get(k).unwrap();
                let o = o.force_value()?;
                let o = o.as_any();
                let v = v.force_value()?;
                if o.is::<Attrs>() && v.as_any().is::<Attrs>() {
                    convany!(o, Attrs).update(v)?;
                } else {
                    new.env.borrow_mut().over(k.clone(), v);
                }
            }
        }
        Ok(Rc::new(new))
    }
}

impl Object for Attrs {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn into_any(self: Box<Self>) -> Box<dyn Any> {
        self
    }
}

impl Display for Attrs {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{{ ")?;
        for (k, v) in self.env.borrow().iter() {
            write!(f, "{k} = {} ", {
                if v.expr_is::<AttrsLiteralExpr>() {
                    "{ ... }".to_string()
                } else if v.expr_is::<ListLiteralExpr>() {
                    "[ ... ]".to_string()
                } else {
                    v.eval().unwrap().to_string()
                }
            })?;
        }
        write!(f, "}}")
    }
}

#[derive(Debug)]
pub struct Path {
    path: String,
}

impl Path {
    pub fn new(path: String) -> Path {
        Path { path }
    }
}
*/

pub fn objeq(
    obj1: &Object,
    obj2: &Object,
    ctx: &ErrorCtx,
) -> Result<bool, Box<dyn NixRsError>> {
    let a1 = obj1.as_any();
    let a2 = obj2.as_any();
    Ok(if a1.is::<Int>() {
        if a2.is::<Int>() {
            convany!(a1, Int) == convany!(a2, Int)
        } else if a2.is::<Float>() {
            *convany!(a1, Int) as Float == *convany!(a2, Float)
        } else {
            false
        }
    } else if a1.is::<Float>() {
        if a2.is::<Int>() {
            *convany!(a1, Float) == *convany!(a2, Int) as Float
        } else if a2.is::<Float>() {
            convany!(a1, Float) == convany!(a2, Float)
        } else {
            false
        }
    } else if a1.is::<Str>() {
        if a2.is::<Str>() {
            convany!(a1, Str) == convany!(a2, Str)
        } else {
            false
        }
    } else if a1.is::<List>() {
        if a2.is::<List>() {
            let mut tmp = convany!(a1, List).value.len() == convany!(a1, List).value.len();
            for (o1, o2) in std::iter::zip(
                convany!(a1, List)
                    .value
                    .iter()
                    .map(|o| -> EvalResult { o.eval() }),
                convany!(a2, List)
                    .value
                    .iter()
                    .map(|o| -> EvalResult { o.eval() }),
            ) {
                if !tmp {
                    break;
                }
                tmp = tmp && *convany!(objeq(o1?.as_ref(), o2?.as_ref(), ctx)?.as_any(), Bool);
            }
            tmp
        } else {
            false
        }
    } else if a1.is::<Attrs>() {
        if a2.is::<Attrs>() {
            let mut tmp =
                convany!(a1, Attrs).env.borrow().len() == convany!(a2, Attrs).env.borrow().len();
            for ((k1, v1), (k2, v2)) in std::iter::zip(
                convany!(a1, Attrs)
                    .env
                    .borrow()
                    .iter()
                    .map(|(k, v)| (k, v.eval())),
                convany!(a2, Attrs)
                    .env
                    .borrow()
                    .iter()
                    .map(|(k, v)| (k, v.eval())),
            ) {
                if !tmp {
                    break;
                }
                tmp = tmp && k1 == k2 && *convany!(objeq(v1?, v2?, ctx.clone())?.as_any(), Bool);
            }
            tmp
        } else {
            false
        }
    } else if a1.is::<Null>() {
        a2.is::<Null>()
    } else if a1.is::<Bool>() {
        if a2.is::<Bool>() {
            convany!(a1, Bool) == convany!(a2, Bool)
        } else {
            false
        }
    } else if a1.is::<Lambda>() {
        false
    } else {
        return Err(ctx.unwind(EvalError::new("unsupported type")));
    })
}

/* pub fn objneq(obj1: Rc<dyn Object>, obj2: Rc<dyn Object>, ctx: ErrorCtx) -> EvalResult {

} */

pub fn objlt(
    obj1: &dyn Object,
    obj2: &dyn Object,
    ctx: &ErrorCtx,
) -> Result<bool, Box<dyn NixRsError>> {
    let a1 = obj1.as_any();
    let a2 = obj2.as_any();
    Ok(if a1.is::<Int>() {
        if a2.is::<Int>() {
            convany!(a1, Int) < convany!(a2, Int)
        } else if a2.is::<Float>() {
            (*convany!(a1, Int) as Float) < *convany!(a2, Float)
        } else {
            false
        }
    } else if a1.is::<Float>() {
        if a2.is::<Int>() {
            *convany!(a1, Float) < *convany!(a2, Int) as Float
        } else if a2.is::<Float>() {
            convany!(a1, Float) < convany!(a2, Float)
        } else {
            false
        }
    } else if a1.is::<Str>() {
        if a2.is::<Str>() {
            convany!(a1, Str) < convany!(a2, Str)
        } else {
            false
        }
    } else if a1.is::<List>() {
        if a2.is::<List>() {
            let mut tmp = false;
            for (o1, o2) in std::iter::zip(
                convany!(a1, List).value.iter().map(|o| o.eval()),
                convany!(a2, List).value.iter().map(|o| o.eval()),
            ) {
                if tmp {
                    break;
                }
                tmp = tmp || objlt(o1?.as_ref(), o2?.as_ref(), ctx)?;
            }
            tmp || {
                let mut tmp = true;
                for (o1, o2) in std::iter::zip(
                    convany!(a1, List)
                        .value
                        .iter()
                        .map(|o| -> EvalResult { o.eval() }),
                    convany!(a2, List)
                        .value
                        .iter()
                        .map(|o| -> EvalResult { o.eval() }),
                ) {
                    if !tmp {
                        break;
                    }
                    tmp = tmp && *convany!(objeq(o1?, o2?, ctx.clone())?.as_any(), Bool);
                }
                tmp
            }
        } else {
            false
        }
    } else {
        return Err(ctx.unwind(EvalError::new("unsupported operation")));
    })
}
