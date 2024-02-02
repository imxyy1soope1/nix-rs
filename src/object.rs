use crate::{
    ast::{AttrsLiteralExpr, ListLiteralExpr},
    eval::EvalResult,
};
use std::{any::Any, cell::RefCell, fmt::Debug, fmt::Display, rc::{Rc, Weak}};

use crate::{
    ast::{ArgSetExpr, Expression, IdentifierExpr},
    convany,
    error::*,
    eval::Environment,
};

#[derive(Debug)]
pub enum _EvaledOr {
    Expr(Rc<RefCell<Environment>>, Rc<dyn Expression>, ErrorCtx),
    Evaled(Box<dyn Object>),
    Error(Rc<dyn NixRsError>),
}

impl Clone for _EvaledOr {
    fn clone(&self) -> Self {
        match self {
            Expr(env, expr, ctx) => _EvaledOr::Expr(env.clone(), expr.clone(), ctx.clone()),
            Evaled(obj) => _EvaledOr::Evaled(obj.as_ref().objclone()),
            Error(err) => _EvaledOr::Error(err.clone())
        }
    }
}

pub use _EvaledOr::*;

impl _EvaledOr {
    fn set(&mut self, new: Self) {
        *self = new
    }
    fn get(&self) -> EvalResult {
        match self {
            Evaled(r) => Ok(r.as_ref().objclone()),
            Error(e) => Err(e.clone()),
            _ => unreachable!(),
        }
    }
    fn eval(&mut self) -> EvalResult {
        if let Expr(env, e, ctx) = &*self {
            self.set(Evaled(e.eval(env.clone(), ctx.clone())?))
        }
        self.get()
    }
}

#[derive(Debug, Clone)]
pub struct EvaledOr {
    val: RefCell<_EvaledOr>,
}

impl EvaledOr {
    pub fn evaled(r: Box<dyn Object>) -> EvaledOr {
        EvaledOr {
            val: RefCell::new(_EvaledOr::Evaled(r)),
        }
    }

    pub fn expr(
        env: Rc<RefCell<Environment>>,
        expr: Rc<dyn Expression>,
        ctx: ErrorCtx,
    ) -> EvaledOr {
        EvaledOr {
            val: RefCell::new(_EvaledOr::Expr(env, expr, ctx)),
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
    fn as_any(&self) -> &dyn Any;
    fn objclone(&self) -> Box<dyn Object>;
}

pub type Int = i64;

impl Object for Int {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn objclone(&self) -> Box<dyn Object> {
        Box::new(*self)
    }
}

pub type Float = f64;

impl Object for Float {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn objclone(&self) -> Box<dyn Object> {
        Box::new(*self)
    }
}

pub type Bool = bool;

impl Object for bool {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn objclone(&self) -> Box<dyn Object> {
        Box::new(*self)
    }
}

#[derive(Debug)]
pub struct Null {}

impl Object for Null {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn objclone(&self) -> Box<dyn Object> {
        Box::new(Null {})
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

    fn objclone(&self) -> Box<dyn Object> {
        Box::new(Clone::clone(self))
    }
}

pub struct InterpolateStr {}

impl InterpolateStr {
    pub fn new(value: String, replaces: Vec<(usize, Box<dyn Object>)>) -> Str {
        let mut offset = 0;
        let mut value = value;
        for (i, o) in replaces.into_iter() {
            let (p1, p2) = value.split_at(i + offset);
            let s = Clone::clone(convany!(o.as_any(), Str));
            value = format!("{p1}{s}{p2}");
            offset += s.len()
        }
        value
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

    pub fn concat(&self, other: &List) -> List {
        let mut new = Clone::clone(self);
        new.value
            .extend(other.value.clone());
        new
    }
}

impl Object for List {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn objclone(&self) -> Box<dyn Object> {
        Box::new(Clone::clone(self))
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

#[derive(Debug, Clone)]
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
        let callenv = Rc::new(RefCell::new(Environment::new(Some(Rc::downgrade(&self.env)))));
        if !self.arg.as_any().is::<ArgSetExpr>() {
            // IdentifierExpr
            callenv
                .borrow_mut()
                .set(
                    Clone::clone(&convany!(self.arg.as_any(), IdentifierExpr).ident),
                    EvaledOr::evaled(arg),
                )
                .unwrap();
            return self.body.eval(callenv, ctx);
        }
        for a in convany!(self.arg.as_any(), ArgSetExpr).args.iter() {
            let ident = Clone::clone(&a.0);
            let e = {
                let t = convany!(arg.as_any(), Attrs).env.borrow().get(&ident);
                if let Ok(o) = t {
                    o
                } else {
                    drop(t); // to prevent multiple borrow of env (from `t` above)
                    println!("default {ident}");
                    EvaledOr::expr(callenv.clone(), a.1.clone().unwrap(), ctx.clone())
                }
            };

            callenv.borrow_mut().set(ident, e).unwrap();
        }
        convany!(self.arg.as_any(), ArgSetExpr)
            .alias
            .clone()
            .map(|a| callenv.borrow_mut().set(a, EvaledOr::evaled(arg.objclone())));
        self.body.eval(callenv, ctx)
    }
}

impl Object for Lambda {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn objclone(&self) -> Box<dyn Object> {
        Box::new(Clone::clone(self))
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

    pub fn merge(&self, other: Box<dyn Object>) -> Result<(), Rc<dyn NixRsError>> {
        let other = convany!(other.as_any(), Attrs);
        for (k, v) in other.env.borrow().iter() {
            let ret = self.env.borrow_mut().set(Clone::clone(k), v.clone());
            if ret.is_err() {
                drop(ret);
                convany!(self.env.borrow().get(k).unwrap().eval()?.as_any(), Attrs)
                    .merge(v.eval()?)?;
            }
        }
        Ok(())
    }

    pub fn update(&self, other: &Attrs) -> EvalResult {
        let new = Clone::clone(self);
        let other = convany!(other.as_any(), Attrs);
        for (k, v) in other.env.borrow().iter() {
            let ret = new.env.borrow_mut().set(Clone::clone(k), v.clone());
            if ret.is_err() {
                drop(ret);
                let o = new.env.borrow().get(k).unwrap();
                let o = o.eval()?;
                let o = o.as_any();
                let v = v.eval()?;
                if o.is::<Attrs>() && v.as_any().is::<Attrs>() {
                    convany!(o, Attrs).update(v.as_any().downcast_ref::<Attrs>().unwrap())?;
                } else {
                    new.env.borrow_mut().over(Clone::clone(k), EvaledOr::evaled(v));
                }
            }
        }
        Ok(Box::new(new))
    }
}

impl Object for Attrs {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn objclone(&self) -> Box<dyn Object> {
        Box::new(Clone::clone(self))
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

pub fn objeq(
    obj1: Box<dyn Object>,
    obj2: Box<dyn Object>,
    ctx: ErrorCtx,
) -> Result<bool, Rc<dyn NixRsError>> {
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
                tmp = tmp && *convany!(objeq(o1?, o2?, ctx.clone())?.as_any(), Bool);
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
    obj1: Box<dyn Object>,
    obj2: Box<dyn Object>,
    ctx: ErrorCtx,
) -> Result<bool, Rc<dyn NixRsError>> {
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
                tmp = tmp || objlt(o1?, o2?, ctx.clone())?;
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
