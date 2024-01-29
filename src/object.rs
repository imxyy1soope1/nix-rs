use std::{any::Any, cell::RefCell, fmt::Debug, fmt::Display, rc::Rc};

use crate::{
    ast::{ArgSetExpr, Expression, IdentifierExpr},
    convany,
    eval::Environment,
};

#[derive(Debug, Clone)]
pub enum _EvaledOr {
    Expr(Rc<RefCell<Environment>>, Rc<dyn Expression>),
    Evaled(Rc<dyn Object>),
    Ref(Rc<dyn Object>),
}

pub use _EvaledOr::*;

impl _EvaledOr {
    fn set(&mut self, new: Self) {
        *self = new
    }
    fn get(&self) -> Rc<dyn Object> {
        match self {
            Evaled(r) | Ref(r) => r.clone(),
            _ => unreachable!(),
        }
    }
    fn eval(&mut self) -> Rc<dyn Object> {
        match &*self {
            Expr(env, e) => self.set(Evaled(e.eval((*env).clone()))),
            _ => (),
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
            val: RefCell::new(_EvaledOr::Ref(r)),
        }
    }

    pub fn expr(env: Rc<RefCell<Environment>>, expr: Rc<dyn Expression>) -> EvaledOr {
        EvaledOr {
            val: RefCell::new(_EvaledOr::Expr(env, expr)),
        }
    }

    pub fn eval(&self) -> Rc<dyn Object> {
        self.val.borrow_mut().eval()
    }
}

pub trait Object: Display + Debug {
    fn as_any(&self) -> &dyn Any;
}

pub type Int = i64;

impl Object for Int {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

pub type Float = f64;

impl Object for Float {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

pub type Bool = bool;

impl Object for bool {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Debug)]
pub struct Null {}

impl Object for Null {
    fn as_any(&self) -> &dyn Any {
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
}

pub struct InterpolateStr {
}

impl InterpolateStr {
    pub fn new(value: String, replaces: Vec<(usize, Rc<dyn Object>)>) -> Str {
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
    pub value: Vec<EvaledOr>,
}

impl List {
    pub fn new(value: Vec<EvaledOr>) -> List {
        List { value }
    }

    pub fn concat(&self, other: Rc<dyn Object>) -> List {
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
}

impl Display for List {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "[ ")?;
        for v in self.value.iter() {
            write!(f, "{} ", v.eval())?;
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

    pub fn call(&self, arg: Rc<dyn Object>) -> Rc<dyn Object> {
        let callenv = RefCell::new(Environment::new(Some(self.env.clone())));
        if !self.arg.as_any().is::<ArgSetExpr>() {
            // IdentifierExpr
            callenv
                .borrow_mut()
                .set(
                    convany!(self.arg.as_any(), IdentifierExpr).ident.clone(),
                    EvaledOr::evaled(arg),
                )
                .unwrap();
            return self.body.eval(Rc::new(callenv));
        }
        for a in convany!(self.arg.as_any(), ArgSetExpr).args.iter() {
            let ident = a.0.clone();
            let e = {
                let t = (*self.env.clone()).borrow_mut().get(&ident);
                if let Ok(o) = t {
                    o
                } else {
                    drop(t); // to prevent multiple borrow of env (from `t` above)
                    EvaledOr::expr(self.env.clone(), a.1.clone().unwrap())
                }
            };

            callenv.borrow_mut().set(ident, e).unwrap();
        }
        convany!(self.arg.as_any(), ArgSetExpr)
            .alias
            .clone()
            .map(|a| callenv.borrow_mut().set(a, EvaledOr::evaled(arg.clone())));
        self.body.eval(Rc::new(callenv))
    }
}

impl Object for Lambda {
    fn as_any(&self) -> &dyn Any {
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

    pub fn merge(&self, other: Rc<dyn Object>) {
        let other = convany!(other.as_any(), Attrs);
        for (k, v) in other.env.borrow().iter() {
            let ret = self.env.borrow_mut().set(k.clone(), v.clone());
            if ret.is_err() {
                drop(ret);
                convany!(self.env.borrow().get(k).unwrap().eval().as_any(), Attrs).merge(v.eval())
            }
        }
    }

    pub fn update(&self, other: Rc<dyn Object>) -> Attrs {
        let new = self.clone();
        let other = convany!(other.as_any(), Attrs);
        for (k, v) in other.env.borrow().iter() {
            let ret = new.env.borrow_mut().set(k.clone(), v.clone());
            if ret.is_err() {
                drop(ret);
                let o = new.env.borrow().get(k).unwrap();
                let o = o.eval();
                let o = o.as_any();
                let v = v.eval();
                if o.is::<Attrs>() && v.as_any().is::<Attrs>() {
                    convany!(o, Attrs).update(v);
                } else {
                    new.env.borrow_mut().over(k.clone(), EvaledOr::evaled(v));
                }
            }
        }
        new
    }
}

impl Object for Attrs {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Display for Attrs {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{{ ... }}")
    }
}

pub fn objeq(obj1: Rc<dyn Object>, obj2: Rc<dyn Object>) -> bool {
    let a1 = obj1.as_any();
    let a2 = obj2.as_any();
    if a1.is::<Int>() {
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
            std::iter::zip(
                convany!(a1, List).value.iter().map(|o| o.eval()),
                convany!(a2, List).value.iter().map(|o| o.eval()),
            )
            .all(|a| objeq(a.0, a.1))
        } else {
            false
        }
    } else if a1.is::<Attrs>() {
        if a2.is::<Attrs>() {
            std::iter::zip(
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
            )
            .any(|a| a.0 .0 == a.1 .0 && objeq(a.0 .1, a.1 .1))
                && convany!(a1, Attrs).env.borrow().len() == convany!(a2, Attrs).env.borrow().len()
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
        unimplemented!()
    }
}

pub fn objlt(obj1: Rc<dyn Object>, obj2: Rc<dyn Object>) -> bool {
    let a1 = obj1.as_any();
    let a2 = obj2.as_any();
    if a1.is::<Int>() {
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
            std::iter::zip(
                convany!(a1, List).value.iter().map(|o| o.eval()),
                convany!(a2, List).value.iter().map(|o| o.eval()),
            )
            .any(|a| objlt(a.0, a.1))
                || (objeq(obj1.clone(), obj2.clone())
                    && convany!(a1, List).value.len() < convany!(a2, List).value.len())
        } else {
            false
        }
    } else {
        unimplemented!()
    }
}
