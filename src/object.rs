use std::{any::Any, cell::RefCell, fmt::Debug, fmt::Display, rc::Rc, sync::Arc};

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
            Evaled(o) => o.clone(),
            Expr(env, e) => {
                self.set(Evaled(e.eval((&*env).clone())));
                self.eval()
            }
            Ref(r) => r.clone(),
        }
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

pub mod type_ids {
    use super::*;
    use std::any::TypeId;

    pub static INT: TypeId = TypeId::of::<Int>();
    pub static FLOAT: TypeId = TypeId::of::<Float>();
    pub static BOOL: TypeId = TypeId::of::<Bool>();
    pub static NULL: TypeId = TypeId::of::<Null>();
    pub static STRING: TypeId = TypeId::of::<Str>();
}

/* #[derive(Debug)]
pub struct Int {
    pub value: i64,
} */

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

lazy_static! {
    // pub static ref TRUE: Arc<Bool> = Arc::new(Bool::new(true));
    // pub static ref FALSE: Arc<Bool> = Arc::new(Bool::new(false));
    pub static ref NULL: Arc<Null> = Arc::new(Null {});
}

pub type Bool = bool;

impl Object for bool {
    fn as_any(&self) -> &dyn Any {
        return self;
    }
}

#[derive(Debug)]
pub struct Null {}

impl Null {
    pub fn null() -> &'static Null {
        &NULL
    }
}

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

#[derive(Debug)]
pub struct Str {
    pub value: String,
}

impl Str {
    pub fn new(value: String, replaces: Vec<(usize, Rc<dyn Object>)>) -> Str {
        let mut offset = 0;
        let mut value = value;
        for (i, o) in replaces.into_iter() {
            let (p1, p2) = value.split_at(i + offset);
            let s = convany!(o.as_any(), Str).value.clone();
            value = format!("{p1}{s}{p2}");
            offset += s.len()
        }
        Str { value }
    }
}

impl Object for Str {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Display for Str {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, r#""{}""#, self.value)
    }
}

#[derive(Debug)]
pub struct List {
    value: Vec<Rc<dyn Object>>,
}

impl List {
    pub fn new(value: Vec<Rc<dyn Object>>) -> List {
        List { value }
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
            write!(f, "{v} ")?;
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
        for a in self
            .arg
            .as_any()
            .downcast_ref::<ArgSetExpr>()
            .as_ref()
            .unwrap()
            .args
            .iter()
        {
            let ident = a.0.clone();
            let e = {
                let t = (&mut *(*self.env.clone()).borrow_mut()).get(&ident);
                if let Ok(o) = t {
                    EvaledOr::evaled(o)
                } else {
                    drop(t); // to prevent multiple borrow of env (from `t` above)
                    EvaledOr::evaled(a.1.clone().unwrap().eval(self.env.clone()))
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

#[derive(Debug)]
pub struct Attrs {
    pub env: Rc<RefCell<Environment>>,
}

impl Attrs {
    pub fn new(env: Rc<RefCell<Environment>>) -> Attrs {
        Attrs { env }
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
