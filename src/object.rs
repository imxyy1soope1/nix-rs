use crate::{ast::Node, eval::EvalResult};

use std::iter::zip;
use std::{cell::RefCell, fmt::Debug, fmt::Display, rc::Rc};

use crate::error::*;

use crate::{ast::*, Env};

pub type Int = i64;
pub type Float = f64;
pub type Bool = bool;

#[derive(Debug, PartialEq)]
pub enum Object {
    Int(Int),
    Float(Float),
    Bool(Bool),
    Null,
    Str(String),
    List(Vec<Node>),
    Attrs(Env),
    Path(String),
    SearchPath(String),

    Function(Expression, Expression, Env),
    BuiltinFunction(u8, fn(RefCell<Vec<Node>>, env: &Env) -> EvalResult),
    BuiltinFunctionApp(
        u8,
        RefCell<Vec<Node>>,
        fn(RefCell<Vec<Node>>, env: &Env) -> EvalResult,
    ),
}

impl Clone for Object {
    fn clone(&self) -> Self {
        use Object::*;
        match self {
            Attrs(env) => Attrs(Rc::new((**env).clone())),

            Int(int) => Int(int.clone()),
            Float(float) => Float(float.clone()),
            Bool(bool) => Bool(bool.clone()),
            Null => Null,
            Str(string) => Str(string.clone()),
            List(list) => List(list.clone()),
            Path(path) => Path(path.clone()),
            SearchPath(path) => SearchPath(path.clone()),

            Function(arg, body, env) => Function(arg.clone(), body.clone(), env.clone()),
            BuiltinFunction(argscount, f) => BuiltinFunction(argscount.clone(), f.clone()),
            BuiltinFunctionApp(argsleft, args, f) => {
                BuiltinFunctionApp(argsleft.clone(), args.clone(), f.clone())
            }
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Object::*;
        match self {
            Int(int) => write!(f, "{int}"),
            Float(float) => write!(f, "{float}"),
            Bool(val) => write!(f, "{val}"),
            Null => write!(f, "null"),
            Str(s) => write!(f, "{s}"),
            List(_list) => {
                /* write!(f, "[ ")?;
                for v in list.iter() {
                    write!(f, "{v} ")?;
                }
                write!(f, "]") */
                write!(f, "[ ... ]")
            }
            Function(..) => write!(f, "«lambda»"),
            Attrs(..) => write!(f, "{{ ... }}"),
            Path(path) => write!(f, "{path}"),
            SearchPath(path) => write!(f, "{path}"),
            BuiltinFunction(..) => write!(f, "«primop»"),
            BuiltinFunctionApp(..) => write!(f, "«primop-app»"),
        }
    }
}

impl Into<Node> for Object {
    fn into(self) -> Node {
        Node::Value(self.into())
    }
}

impl From<Int> for Object {
    fn from(value: Int) -> Self {
        Self::Int(value)
    }
}

impl From<Float> for Object {
    fn from(value: Float) -> Self {
        Self::Float(value)
    }
}

impl From<Bool> for Object {
    fn from(value: Bool) -> Self {
        Self::Bool(value)
    }
}

impl TryInto<Int> for Object {
    type Error = EvalError;
    fn try_into(self) -> Result<Int, Self::Error> {
        if let Object::Int(int) = self {
            Ok(int)
        } else {
            Err(format!("").into())
        }
    }
}

impl TryInto<Float> for Object {
    type Error = EvalError;
    fn try_into(self) -> Result<Float, Self::Error> {
        if let Object::Float(float) = self {
            Ok(float)
        } else {
            Err(format!("").into())
        }
    }
}

impl TryInto<String> for Object {
    type Error = EvalError;
    fn try_into(self) -> Result<String, Self::Error> {
        if let Object::Str(string) = self {
            Ok(string)
        } else {
            Err(format!("").into())
        }
    }
}

pub fn objeq(obj1: Object, obj2: Object, env: &Env) -> Result<bool, Box<dyn NixRsError>> {
    use Object::*;
    Ok(match obj1 {
        Int(l) => match obj2 {
            Int(r) => l == r,
            Float(r) => l as f64 == r,
            _ => false,
        },
        Float(l) => match obj2 {
            Int(r) => l == r as f64,
            Float(r) => l == r,
            _ => false,
        },
        Str(l) => match obj2 {
            Str(r) => l == r,
            _ => false,
        },
        List(l) => match obj2 {
            List(r) => {
                let mut tmp = l.len() == r.len();
                for (mut o1, mut o2) in zip(l.into_iter(), r.into_iter()) {
                    tmp = objeq(o1.force_value(env)?, o2.force_value(env)?, env)?;
                    if !tmp {
                        break;
                    }
                }
                tmp
            }
            _ => false,
        },
        Attrs(l) => match obj2 {
            Attrs(r) => {
                let mut tmp = l.borrow().len() == r.borrow().len();
                for (k1, v1) in l.borrow().iter() {
                    let mut v2 = r.borrow().get(k1).map_err(|e| e.into())?;
                    tmp = objeq(v1.value(env)?, v2.force_value(env)?, env)?;
                    if !tmp {
                        break;
                    }
                }
                tmp
            }
            _ => false,
        },
        Null => matches!(obj2, Null),
        Function(..) => false,
        Bool(l) => match obj2 {
            Bool(r) => l == r,
            _ => false,
        },
        _ => unimplemented!(),
    })
}

pub fn objlt(obj1: Object, obj2: Object, env: &Env) -> Result<bool, Box<dyn NixRsError>> {
    use Object::*;

    Ok(match obj1 {
        Int(l) => match obj2 {
            Int(r) => l < r,
            Float(r) => (l as f64) < r,
            _ => return Err(EvalError::from("unsupported operation").into()),
        },
        Float(l) => match obj2 {
            Int(r) => l < r as f64,
            Float(r) => l < r,
            _ => return Err(EvalError::from("unsupported operation").into()),
        },
        Str(l) => match obj2 {
            Str(r) => l < r,
            _ => return Err(EvalError::from("unsupported operation").into()),
        },
        List(mut l) => match obj2 {
            List(mut r) => {
                let mut tmp = false;
                let lenl = l.len();
                let lenr = r.len();
                for (o1, o2) in zip(l.iter_mut(), r.iter_mut()) {
                    tmp = objlt(o1.value(env)?, o2.value(env)?, env)?;
                    if tmp {
                        break;
                    }
                }
                tmp || (lenl == lenr
                    && objeq(
                        List(l),
                        List({
                            r.truncate(lenl);
                            r
                        }),
                        env,
                    )?)
            }
            _ => false,
        },
        _ => return Err(EvalError::from("unsupported operation").into()),
    })
}

/*
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
            write!(f, "{} ", {
                if v.expr_is::<AttrsLiteralExpr>() {
                    "...".to_string()
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

    pub fn call(&self, arg: Rc<dyn Object>, ctx: ErrorCtx) -> EvalResult {
        let callenv = Rc::new(RefCell::new(Environment::new(Some(self.env.clone()))));
        if !self.arg.as_any().is::<ArgSetExpr>() {
            // IdentifierExpr
            callenv
                .borrow_mut()
                .set(
                    convany!(self.arg.as_any(), IdentifierExpr).ident.clone(),
                    EvaledOr::evaled(arg),
                )
                .unwrap();
            return self.body.eval(callenv, ctx);
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
                    EvaledOr::expr(callenv.clone(), a.1.clone().unwrap(), ctx.clone())
                }
            };

            callenv.borrow_mut().set(ident, e).unwrap();
        }
        convany!(self.arg.as_any(), ArgSetExpr)
            .alias
            .clone()
            .map(|a| callenv.borrow_mut().set(a, EvaledOr::evaled(arg.clone())));
        self.body.eval(callenv, ctx)
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

    pub fn update(&self, other: Rc<dyn Object>) -> EvalResult {
        let new = self.clone();
        let other = convany!(other.as_any(), Attrs);
        for (k, v) in other.env.borrow().iter() {
            let ret = new.env.borrow_mut().set(k.clone(), v.clone());
            if ret.is_err() {
                drop(ret);
                let o = new.env.borrow().get(k).unwrap();
                let o = o.eval()?;
                let o = o.as_any();
                let v = v.eval()?;
                if o.is::<Attrs>() && v.as_any().is::<Attrs>() {
                    convany!(o, Attrs).update(v)?;
                } else {
                    new.env.borrow_mut().over(k.clone(), EvaledOr::evaled(v));
                }
            }
        }
        Ok(Rc::new(new))
    }
}
*/

pub fn update_env(left: &Env, right: &Env, env: &Env) -> Result<Env, Box<dyn NixRsError>> {
    let new = Rc::new(RefCell::new(left.borrow().clone()));
    for (k, v) in right.borrow_mut().iter() {
        let ret = new.borrow_mut().set(k.clone(), v.clone());
        if ret.is_err() {
            let mut o = new.borrow().get(k).unwrap();
            let o = o.force_value(env)?;
            let v = v.value(env)?;
            if let Object::Attrs(s) = o {
                if let Object::Attrs(other) = v {
                    update_env(&s, &other, env)?;
                } else {
                    new.borrow_mut().set_force(k.clone(), Node::Value(v.into()));
                }
            }
        }
    }
    Ok(new)
}

/*

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
    obj1: Rc<dyn Object>,
    obj2: Rc<dyn Object>,
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
*/
