/* use crate::ast::{
    ArgSetExpr, AttrsLiteralExpr, Expression, FunctionLiteralExpr, IdentifierExpr, ListLiteralExpr,
}; */
use crate::ast::Expression;
use crate::builtins::{PrimOp, PrimOpApp};
use crate::convany;
use crate::error::*;
use crate::eval::{Env, Environment, EvalResult};

use std::any::Any;
use std::cell::RefCell;
use std::fmt::{Debug, Display};
use std::rc::Rc;

#[derive(Debug, Clone)]
enum _Object {
    Int(i64),
    Float(f64),
    Bool(bool),
    Null,
    Str(String),
    List(Vec<Object>),
    Lambda {
        func: FunctionLiteralExpr,
        env: Env,
    },
    Attrs(Env),
    PrimOp(PrimOp),
    PrimOpApp(PrimOpApp),

    ClosureThunk(Env, ErrorCtx, Rc<dyn Expression>),
    FunctionCallThunk {
        func: Box<Object>,
        arg: Box<Object>,
        ctx: ErrorCtx,
    },
}

#[derive(Debug, Clone)]
pub struct Object {
    val: RefCell<_Object>,
}

impl From<i64> for Object {
    fn from(value: i64) -> Self {
        Object {
            val: RefCell::new(_Object::Int(value)),
        }
    }
}

impl From<f64> for Object {
    fn from(value: f64) -> Self {
        Object {
            val: RefCell::new(_Object::Float(value)),
        }
    }
}

impl From<bool> for Object {
    fn from(value: bool) -> Self {
        Object {
            val: RefCell::new(_Object::Bool(value)),
        }
    }
}

impl From<String> for Object {
    fn from(value: String) -> Self {
        Object {
            val: RefCell::new(_Object::Str(value)),
        }
    }
}

impl From<Vec<Object>> for Object {
    fn from(value: Vec<Object>) -> Self {
        Object {
            val: RefCell::new(_Object::List(value)),
        }
    }
}

macro_rules! mkval {
    ($name:ident, $t:ty) => {
        pub fn $name(value: $t) -> Object {
            value.into()
        }
    };
}
macro_rules! matchtuple {
    ($name:ident, $t:ident) => {
        pub fn $name(&self) -> bool {
            matches!(&*self.val.borrow(), _Object::$t(..))
        }
    };
}
macro_rules! matchstruct {
    ($name:ident, $t:ident) => {
        pub fn $name(&self) -> bool {
            matches!(&*self.val.borrow(), _Object::$t { .. })
        }
    };
}
impl Object {
    mkval! {mk_int, i64}
    mkval! {mk_float, f64}
    mkval! {mk_bool, bool}
    mkval! {mk_string, String}
    mkval! {mk_list, Vec<Object>}

    pub fn mk_null() -> Object {
        Object {
            val: RefCell::new(_Object::Null),
        }
    }

    pub fn mk_lambda(func: FunctionLiteralExpr, env: Env) -> Object {
        Object {
            val: RefCell::new(_Object::Lambda { func, env }),
        }
    }

    pub fn mk_attrs(env: Env) -> Object {
        Object {
            val: RefCell::new(_Object::Attrs(env)),
        }
    }

    pub fn mk_thunk(env: Env, ctx: ErrorCtx, expr: Rc<dyn Expression>) -> Object {
        Object {
            val: RefCell::new(_Object::ClosureThunk(env, ctx, expr)),
        }
    }

    pub fn mk_call(func: Object, arg: Object, ctx: ErrorCtx) -> Object {
        Object {
            val: RefCell::new(_Object::FunctionCallThunk {
                func: func.into(),
                arg: arg.into(),
                ctx,
            }),
        }
    }

    pub fn mk_prim_op(
        arity: u8,
        ctx: ErrorCtx,
        func: fn(Vec<Object>, ctx: &ErrorCtx) -> EvalResult,
    ) -> Object {
        Object {
            val: RefCell::new(_Object::PrimOp(PrimOp::new(arity, ctx, func))),
        }
    }
    /*
    pub fn mk_prim_op_app(
        arity: u8,
        ctx: ErrorCtx,
        func: fn(Vec<Object>, ctx: &ErrorCtx) -> EvalResult,
    ) -> Object {
        Object {
            val: RefCell::new(_Object::PrimOpApp(PrimOpApp::new(arity, ctx, func))),
        }
    } */

    matchtuple! {is_int, Int}
    matchtuple! {is_float, Float}
    matchtuple! {is_bool, Bool}
    matchtuple! {is_str, Str}
    matchtuple! {is_list, List}
    matchstruct! {is_lambda, Lambda}
    matchtuple! {is_attrs, Attrs}
    matchtuple! {is_prim_op, PrimOp}
    matchtuple! {is_prim_op_app, PrimOpApp}
    matchtuple! {is_thunk, ClosureThunk}
    matchstruct! {is_call, FunctionCallThunk}
    pub fn is_null(&self) -> bool {
        matches!(&*self.val.borrow(), _Object::Null)
    }

    pub fn typename(&self) -> &'static str {
        use _Object::*;
        match &*self.val.borrow() {
            Int(..) => "int",
            Float(..) => "float",
            Bool(..) => "bool",
            Null => "null",
            Str(..) => "string",
            List(..) => "list",
            Attrs(..) => "set",
            Lambda { .. } | PrimOp(..) | PrimOpApp(..) => "lambda",
            ClosureThunk(..) | FunctionCallThunk { .. } => "thunk",
        }
    }

    pub fn call(&self, arg: &Object, ctx: &ErrorCtx) -> EvalResult {

    }

    pub fn force_value(&self) -> Result<(), Box<dyn NixRsError>> {
        use _Object::*;
        match &*self.val.borrow() {
            ClosureThunk(env, ctx, expr) => *self.val.borrow_mut() = expr.eval(env, ctx)?.val.into_inner(),
            FunctionCallThunk { func, arg, ctx } => *self.val.borrow_mut() = func.call(arg.as_ref(), ctx)?.val.into_inner(),
            _ => return Ok(()),
        }
        self.force_value()
    }

    pub fn force_value_deep(&self) -> Result<(), Box<dyn NixRsError>> {
        use _Object::*;
        match &*self.val.borrow() {
            List(list) => {
                let _: Vec<_> = list.iter().map(|o| o.force_value_deep()).collect();
                return Ok(());
            }
            Attrs(env) => {
                let _: Vec<_> = env
                    .borrow()
                    .iter()
                    .map(|(_, o)| o.force_value_deep())
                    .collect();
                return Ok(());
            }
            ClosureThunk(env, ctx, expr) => *self.val.borrow_mut() = expr.eval(env, ctx)?.val.into_inner(),
            FunctionCallThunk { func, arg, ctx } => *self.val.borrow_mut() = func.call(arg.as_ref(), ctx)?.val.into_inner(),
            _ => return Ok(()),
        }
        self.force_value_deep()
    }
}

impl TryInto<i64> for Object {
    type Error = Box<dyn NixRsError>;
    fn try_into(self) -> Result<i64, Self::Error> {
        self.force_value();
        if let _Object::Int(val) = self.val.into_inner() {
            Ok(val)
        } else {
            Err(EvalError::new(format!(
                "value is {} {} while an int was expected",
                if matches!(&*self.val.borrow(), _Object::Int(..)) {
                    "an"
                } else {
                    "a"
                },
                self.typename()
            )))
        }
    }
}

impl TryInto<f64> for Object {
    type Error = Box<dyn NixRsError>;
    fn try_into(self) -> Result<f64, Self::Error> {
        self.force_value();
        let val = self.val.into_inner();
        if let _Object::Float(val) = val {
            Ok(val)
        } else if let _Object::Int(val) = val {
            Ok(val as f64)
        } else {
            Err(EvalError::new(format!(
                "value is a {} while a float was expected",
                self.typename()
            )))
        }
    }
}

impl TryInto<bool> for Object {
    type Error = Box<dyn NixRsError>;
    fn try_into(self) -> Result<bool, Self::Error> {
        self.force_value();
        if let _Object::Bool(val) = self.val.into_inner() {
            Ok(val)
        } else {
            Err(EvalError::new(format!(
                "value is {} {} while a bool was expected",
                if matches!(&*self.val.borrow(), _Object::Int(..)) {
                    "an"
                } else {
                    "a"
                },
                self.typename()
            )))
        }
    }
}

impl TryInto<String> for Object {
    type Error = Box<dyn NixRsError>;
    fn try_into(self) -> Result<String, Self::Error> {
        self.force_value();
        if let _Object::Str(val) = self.val.into_inner() {
            Ok(val)
        } else {
            Err(EvalError::new(format!(
                "value is {} {} while a string was expected",
                if matches!(&*self.val.borrow(), _Object::Int(..)) {
                    "an"
                } else {
                    "a"
                },
                self.typename()
            )))
        }
    }
}

impl TryInto<Vec<Object>> for Object {
    type Error = Box<dyn NixRsError>;
    fn try_into(self) -> Result<Vec<Object>, Self::Error> {
        self.force_value();
        if let _Object::List(val) = self.val.into_inner() {
            Ok(val)
        } else {
            Err(EvalError::new(format!(
                "value is {} {} while a list was expected",
                if matches!(&*self.val.borrow(), _Object::Int(..)) {
                    "an"
                } else {
                    "a"
                },
                self.typename()
            )))
        }
    }
}

impl TryInto<Env> for Object {
    type Error = Box<dyn NixRsError>;
    fn try_into(self) -> Result<Env, Self::Error> {
        self.force_value();
        if let _Object::Attrs(val) = self.val.into_inner() {
            Ok(val)
        } else {
            Err(EvalError::new(format!(
                "value is {} {} while a set was expected",
                if matches!(&*self.val.borrow(), _Object::Int(..)) {
                    "an"
                } else {
                    "a"
                },
                self.typename()
            )))
        }
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

/*
pub fn objlt(obj1: &Object, obj2: &Object, ctx: &ErrorCtx) -> Result<bool, Box<dyn NixRsError>> {
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
*/
