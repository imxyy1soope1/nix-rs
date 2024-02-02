use crate::{
    ast::{AttrsLiteralExpr, InterpolateExpr, InterpolateStringExpr, ListLiteralExpr}, builtins::{PrimOp, PrimOpApp}, eval::EvalResult
};
use std::{
    cell::RefCell,
    fmt::Debug,
    fmt::Display,
    rc::{Rc, Weak},
};

use crate::{
    ast::{ArgSetExpr, Expression, IdentifierExpr},
    convany,
    error::*,
    eval::Environment,
};

#[derive(Debug, Clone)]
pub enum _EvaledOr {
    Expr(Weak<RefCell<Environment>>, Rc<dyn Expression>, ErrorCtx),
    Evaled(Rc<dyn Object>),
    Error(Rc<dyn NixRsError>),
}

pub use _EvaledOr::*;

impl _EvaledOr {
    fn set(&mut self, new: Self) {
        *self = new
    }
    fn get(&self) -> EvalResult {
        match self {
            Evaled(r) => Ok(r.clone()),
            Error(e) => Err(e.clone()),
            _ => unreachable!(),
        }
    }
    fn eval(&mut self) -> EvalResult {
        if let Expr(env, e, ctx) = &*self {
            self.set(Evaled(e.eval(&env.upgrade().unwrap(), ctx)?))
        }
        self.get()
    }
}

#[derive(Debug, Clone)]
pub struct EvaledOr {
    val: RefCell<_EvaledOr>,
}

impl EvaledOr {
    pub fn evaled(r: Rc<dyn Object>) -> EvaledOr {
        EvaledOr {
            val: RefCell::new(_EvaledOr::Evaled(r)),
        }
    }

    pub fn expr(
        env: &Rc<RefCell<Environment>>,
        expr: Rc<dyn Expression>,
        ctx: ErrorCtx,
    ) -> EvaledOr {
        EvaledOr {
            val: RefCell::new(_EvaledOr::Expr(Rc::downgrade(env), expr, ctx)),
        }
    }

    pub fn eval(&self) -> EvalResult {
        self.val.borrow_mut().eval()
    }

    pub fn expr_is<T: 'static>(&self) -> bool {
        match &*self.val.borrow() {
            _EvaledOr::Error(_) | _EvaledOr::Evaled(_) => false,
            _EvaledOr::Expr(_, e, _) => e.as_any().is::<T>(),
        }
    }
}

pub trait Object: Display + Debug {
    fn typename(&self) -> &'static str;

    fn try_into_int(&self) -> Result<Int, Rc<dyn NixRsError>> {
        Err(EvalError::from(format!("can't convert {} to int", self.typename())).into())
    }

    fn try_into_float(&self) -> Result<Float, Rc<dyn NixRsError>> {
        Err(EvalError::from(format!("can't convert {} to float", self.typename())).into())
    }

    fn try_into_bool(&self) -> Result<Bool, Rc<dyn NixRsError>> {
        Err(EvalError::from(format!("can't convert {} to bool", self.typename())).into())
    }

    fn try_into_string(&self) -> Result<&Str, Rc<dyn NixRsError>> {
        Err(EvalError::from(format!("can't convert {} to string", self.typename())).into())
    }

    fn try_into_list(&self) -> Result<&List, Rc<dyn NixRsError>> {
        Err(EvalError::from(format!("can't convert {} to list", self.typename())).into())
    }

    fn try_into_lambda(&self) -> Result<&Lambda, Rc<dyn NixRsError>> {
        Err(EvalError::from(format!("can't convert {} to lambda", self.typename())).into())
    }

    fn try_into_primop(&self) -> Result<&PrimOp, Rc<dyn NixRsError>> {
        Err(EvalError::from(format!("can't convert {} to primop", self.typename())).into())
    }

    fn try_into_primop_app(&self) -> Result<&PrimOpApp, Rc<dyn NixRsError>> {
        Err(EvalError::from(format!("can't convert {} to primop-app", self.typename())).into())
    }

    fn try_into_attrs(&self) -> Result<&Attrs, Rc<dyn NixRsError>> {
        Err(EvalError::from(format!("can't convert {} to set", self.typename())).into())
    }

    fn try_into_path(&self) -> Result<&Path, Rc<dyn NixRsError>> {
        Err(EvalError::from(format!("can't convert {} to path", self.typename())).into())
    }

    fn is_null(&self) -> Bool {
        false
    }
}

pub type Int = i64;

impl Object for Int {
    fn typename(&self) -> &'static str {
        "int"
    }

    fn try_into_int(&self) -> Result<Int, Rc<dyn NixRsError>> {
        Ok(*self)
    }

    fn try_into_float(&self) -> Result<Float, Rc<dyn NixRsError>> {
        Ok(*self as Float)
    }
}

pub type Float = f64;

impl Object for Float {
    fn typename(&self) -> &'static str {
        "float"
    }

    fn try_into_float(&self) -> Result<Float, Rc<dyn NixRsError>> {
        Ok(*self)
    }
}

pub type Bool = bool;

impl Object for bool {
    fn typename(&self) -> &'static str {
        "bool"
    }

    fn try_into_bool(&self) -> Result<Bool, Rc<dyn NixRsError>> {
        Ok(*self)
    }
}

#[derive(Debug)]
pub struct Null {}

impl Object for Null {
    fn typename(&self) -> &'static str {
        "null"
    }

    fn is_null(&self) -> Bool {
        true
    }
}

impl Display for Null {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "null")
    }
}

pub type Str = String;

impl Object for Str {
    fn typename(&self) -> &'static str {
        "string"
    }

    fn try_into_string(&self) -> Result<&Str, Rc<dyn NixRsError>> {
        Ok(self)
    }
}

impl TryFrom<&InterpolateStringExpr> for Str {
    type Error = Rc<dyn NixRsError>;
    fn try_from(value: &InterpolateStringExpr) -> Result<Self, Self::Error> {
        let mut offset = 0;
        let mut string = value.literal.clone();
        for (i, o) in value.replaces.iter() {
            let (p1, p2) = string.split_at(i + offset);
            let s = convany!(o.as_any(), Str).clone();
            string = format!("{p1}{s}{p2}");
            offset += s.len()
        }
        Ok(string)
    }
}

#[derive(Debug, Clone)]
pub struct List {
    pub value: Vec<EvaledOr>,
}

impl List {
    pub fn new(value: Vec<EvaledOr>) -> List {
        List { value }
    }

    pub fn concat(&self, other: &List) -> Result<List, Rc<dyn NixRsError>> {
        let mut new = self.clone();
        new.value.extend(other.value.clone());
        Ok(new)
    }
}

impl Object for List {
    fn typename(&self) -> &'static str {
        "list"
    }

    fn try_into_list(&self) -> Result<&List, Rc<dyn NixRsError>> {
        Ok(self)
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
    env: Weak<RefCell<Environment>>,
}

impl Lambda {
    pub fn new(
        arg: Rc<dyn Expression>,
        body: Rc<dyn Expression>,
        env: Weak<RefCell<Environment>>,
    ) -> Lambda {
        Lambda { arg, body, env }
    }

    pub fn call(&self, arg: Rc<dyn Object>, ctx: &ErrorCtx) -> EvalResult {
        let callenv = Rc::new(RefCell::new(Environment::new(Some(
            self.env.upgrade().unwrap(),
        ))));
        if !self.arg.as_any().is::<ArgSetExpr>() {
            // IdentifierExpr
            callenv
                .borrow_mut()
                .set(
                    convany!(self.arg.as_any(), IdentifierExpr).ident.clone(),
                    EvaledOr::evaled(arg),
                )
                .unwrap();
            return self.body.eval(&callenv, ctx);
        }
        for a in convany!(self.arg.as_any(), ArgSetExpr).args.iter() {
            let ident = a.0.clone();
            let e = {
                let t = arg.try_into_attrs()?.env.borrow().get(&ident);
                if let Ok(o) = t {
                    o
                } else {
                    drop(t); // to prevent multiple borrow of env (from `t` above)
                    println!("default {ident}");
                    EvaledOr::expr(&callenv, a.1.clone().unwrap(), ctx.clone())
                }
            };

            callenv.borrow_mut().set(ident, e).unwrap();
        }
        convany!(self.arg.as_any(), ArgSetExpr)
            .alias
            .clone()
            .map(|a| callenv.borrow_mut().set(a, EvaledOr::evaled(arg.clone())));
        self.body.eval(&callenv, ctx)
    }
}

impl Object for Lambda {
    fn typename(&self) -> &'static str {
        "lambda"
    }

    fn try_into_lambda(&self) -> Result<&Lambda, Rc<dyn NixRsError>> {
        Ok(self)
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

    pub fn merge(&self, other: &Attrs) -> Result<(), Rc<dyn NixRsError>> {
        for (k, v) in other.env.borrow().iter() {
            let ret = self.env.borrow_mut().set(k.clone(), v.clone());
            if ret.is_err() {
                drop(ret);
                self.env
                    .borrow()
                    .get(k)
                    .unwrap()
                    .eval()?
                    .try_into_attrs()?
                    .merge(v.eval()?.try_into_attrs()?)?;
            }
        }
        Ok(())
    }

    pub fn update(&self, other: &dyn Object) -> Result<Attrs, Rc<dyn NixRsError>> {
        let new = Attrs::new(Rc::new(RefCell::clone(&*self.env)));
        let other = other.try_into_attrs()?;
        for (k, v) in other.env.borrow().iter() {
            let ret = new.env.borrow_mut().set(k.clone(), v.clone());
            if ret.is_err() {
                drop(ret);
                let o = new.env.borrow().get(k).unwrap();
                let o = o.eval()?;
                let o = o.try_into_attrs();
                let v = v.eval()?;
                if let Ok(v) = v.try_into_attrs() {
                    if let Ok(o) = o {
                        new.env
                            .borrow_mut()
                            .over(k.clone(), EvaledOr::evaled(Rc::new(o.update(v)?)));
                        continue;
                    }
                }
                new.env.borrow_mut().over(k.clone(), EvaledOr::evaled(v));
            }
        }
        Ok(new)
    }
}

impl Object for Attrs {
    fn typename(&self) -> &'static str {
        "set"
    }

    fn try_into_attrs(&self) -> Result<&Attrs, Rc<dyn NixRsError>> {
        Ok(self)
    }
}

impl Display for Attrs {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{{ ... }}")
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

pub fn objeq(
    obj1: Rc<dyn Object>,
    obj2: Rc<dyn Object>,
    ctx: &ErrorCtx,
) -> Result<bool, Rc<dyn NixRsError>> {
    Ok(if let Ok(l) = obj1.try_into_int() {
        if let Ok(r) = obj2.try_into_int() {
            l == r
        } else if let Ok(r) = obj2.try_into_float() {
            l as Float == r
        } else {
            false
        }
    } else if let Ok(l) = obj1.try_into_float() {
        if let Ok(r) = obj2.try_into_float() {
            l == r
        } else {
            false
        }
    } else if let Ok(l) = obj1.try_into_string() {
        if let Ok(r) = obj2.try_into_string() {
            l == r
        } else {
            false
        }
    } else if let Ok(l) = obj1.try_into_list() {
        if let Ok(r) = obj2.try_into_list() {
            let mut tmp = l.value.len() == r.value.len();
            for (o1, o2) in std::iter::zip(l.value.iter(), r.value.iter()) {
                if !tmp {
                    break;
                }
                tmp = tmp && objeq(o1.eval()?, o2.eval()?, ctx)?;
            }
            tmp
        } else {
            false
        }
    } else if let Ok(l) = obj1.try_into_attrs() {
        if let Ok(r) = obj2.try_into_attrs() {
            let mut tmp = l.env.borrow().len() == r.env.borrow().len();
            for ((k1, v1), (k2, v2)) in std::iter::zip(l.env.borrow().iter(), r.env.borrow().iter())
            {
                if !tmp {
                    break;
                }
                tmp = tmp && k1 == k2 && objeq(v1.eval()?, v2.eval()?, ctx)?;
            }
            tmp
        } else {
            false
        }
    } else if obj1.is_null() {
        obj2.is_null()
    } else if let Ok(l) = obj1.try_into_bool() {
        if let Ok(r) = obj2.try_into_bool() {
            l == r
        } else {
            false
        }
    } else if obj1.try_into_lambda().is_ok() {
        false
    } else {
        return Err(ctx.unwind(EvalError::from("unsupported type").into()));
    })
}

/* pub fn objneq(obj1: Rc<dyn Object>, obj2: Rc<dyn Object>, ctx: &ErrorCtx) -> EvalResult {

} */

pub fn objlt(
    obj1: Rc<dyn Object>,
    obj2: Rc<dyn Object>,
    ctx: &ErrorCtx,
) -> Result<bool, Rc<dyn NixRsError>> {
    Ok(if let Ok(l) = obj1.try_into_int() {
        if let Ok(r) = obj2.try_into_int() {
            l < r
        } else if let Ok(r) = obj2.try_into_float() {
            (l as Float) < r
        } else {
            false
        }
    } else if let Ok(l) = obj1.try_into_float() {
        if let Ok(r) = obj2.try_into_float() {
            l == r
        } else {
            false
        }
    } else if let Ok(l) = obj1.try_into_string() {
        if let Ok(r) = obj2.try_into_string() {
            l == r
        } else {
            false
        }
    } else if let Ok(l) = obj1.try_into_list() {
        if let Ok(r) = obj2.try_into_list() {
            let mut tmp = false;
            for (o1, o2) in std::iter::zip(l.value.iter(), r.value.iter()) {
                if tmp {
                    break;
                }
                tmp = tmp || objlt(o1.eval()?, o2.eval()?, ctx)?;
            }
            tmp || {
                let mut tmp = true;
                for (o1, o2) in std::iter::zip(l.value.iter(), r.value.iter()) {
                    if !tmp {
                        break;
                    }
                    tmp = tmp && objeq(o1.eval()?, o2.eval()?, ctx)?;
                }
                tmp
            }
        } else {
            false
        }
    } else {
        return Err(ctx.unwind(EvalError::from("unsupported operation").into()));
    })
}
