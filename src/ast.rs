use crate::error::*;
use crate::eval::{Env, Environment, EvalResult};
use crate::object::*;
use crate::parser::ParseResult;
use crate::token::Token;

use std::rc::Rc;
use std::cell::RefCell;
use std::fmt::{Debug, Display};
use std::ops::{Add, Div, Mul, Sub};

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Expr(Box<Expression>),
    Value(Box<Object>),
}

impl Node {
    pub fn is_value(&self) -> bool {
        if let Node::Value(_) = self {
            true
        } else {
            false
        }
    }

    pub fn force_value(&mut self) -> EvalResult {
        match &*self {
            Node::Value(_) => (),
            Node::Expr(expr) => {
                *self = Node::Value(Box::new(expr.eval()?));
            }
        };
        if let Node::Value(v) = &*self {
            Ok((*v.as_ref()).clone())
        } else {
            unreachable!()
        }
    }

    pub fn value(&self) -> EvalResult {
        match self {
            Node::Value(value) => Ok((**value).clone()),
            Node::Expr(expr) => expr.eval(),
        }
    }

    pub fn get_attr(&self, key: &String) -> Result<Node, Box<dyn NixRsError>> {
        match self {
            Node::Expr(expr) => {
                if let &Expression::AttrsLiteral(ref env, _) = expr.as_ref() {
                    env.borrow_mut()
                        .get(key)
                        .map_err(|e| -> Box<dyn NixRsError> { Box::new(e) })
                } else {
                    Err(EvalError::from("expected a set").into())
                }
            }
            Node::Value(val) => {
                if let &Object::Attrs(ref env) = val.as_ref() {
                    env.borrow_mut()
                        .get(key)
                        .map_err(|e| -> Box<dyn NixRsError> { Box::new(e) })
                } else {
                    Err(EvalError::from("expected a set").into())
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Prefix(Token, Box<Expression>),
    Infix(Token, Box<Expression>, Box<Expression>),
    Ident(String, Env),
    IntLiteral(Int),
    FloatLiteral(Float),
    StringLiteral(String),
    InterpolateString(String, Vec<(usize, Expression)>),
    FunctionLiteral(Box<Expression>, Box<Expression>, Env),
    FunctionCall(Box<Expression>, Box<Expression>),
    If(Box<Expression>, Box<Expression>, Box<Expression>),
    Binding(Box<Expression>, Box<Expression>),
    AttrsLiteral(Env, bool),
    FormalSet(Vec<(String, Option<Expression>)>, Option<String>, bool, Env),
    ListLiteral(Vec<Expression>),
    Let(Env, Box<Expression>),
    With(String, Box<Expression>),
    Assert(Box<Expression>, Box<Expression>),
    Inherit(Vec<String>, Option<Box<Expression>>),
    Path(String, bool),
    SearchPath(Box<Expression>),
    Interpolate(Box<Expression>),
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Expression::*;
        match self {
            Prefix(token, right) => write!(f, "({token}{right})"),
            Infix(token, left, right) => write!(f, "({left} {token} {right})"),
            Ident(ident, _env) => write!(f, "{ident}"),
            IntLiteral(int) => write!(f, "{int}"),
            FloatLiteral(float) => write!(f, "{float}"),
            StringLiteral(string) => write!(f, r#""{string}""#),
            InterpolateString(string, _interpolates) => write!(f, r#""{string}""#),
            FunctionLiteral(arg, body, _env) => write!(f, "({arg}: {body})"),
            FunctionCall(func, arg) => write!(f, "({func} {arg})"),
            If(cond, consq, alter) => write!(f, "(if {cond} then {consq} else {alter})"),
            AttrsLiteral(bindings, rec) => {
                if *rec {
                    write!(f, "rec ")?;
                }
                write!(f, "{{ ")?;
                for (k, _v) in bindings.borrow().iter() {
                    write!(f, "{k} = ...; ",)?;
                }
                write!(f, "}}")
            }
            FormalSet(formals, alias, allow_more, _) => {
                write!(f, "{{ ")?;
                let mut first = true;
                for formal in formals.iter() {
                    if first {
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", formal.0)?;
                    if formal.1.is_some() {
                        write!(f, " ? {}", formal.1.as_ref().unwrap())?;
                    }
                }
                if *allow_more {
                    write!(f, ", ...")?;
                }
                write!(f, " }}")?;
                if alias.is_some() {
                    write!(f, " @ {}", alias.as_ref().unwrap())?;
                }
                Ok(())
            }
            ListLiteral(items) => {
                write!(f, "[ ")?;
                for item in items.iter() {
                    write!(f, "{item} ")?;
                }
                write!(f, "]")
            }
            Let(bindings, expr) => {
                write!(f, "(let ")?;
                for (k, _v) in bindings.borrow().iter() {
                    write!(f, "{k} = ...; ")?;
                }
                write!(f, "in {})", expr)
            }
            With(attrs, expr) => write!(f, "(with {attrs}; {expr})"),
            Assert(assertion, expr) => write!(f, "(assert {assertion}; {expr})"),
            Inherit(inherits, from) => {
                write!(f, "inherit")?;
                if from.is_some() {
                    write!(f, " ({})", from.as_ref().unwrap())?;
                }
                for inherit in inherits.iter() {
                    write!(f, " {inherit}")?;
                }
                Ok(())
            }
            Path(path, _relative) => write!(f, "{path}"),
            SearchPath(path) => write!(f, "<{path}>"),
            Interpolate(expr) => write!(f, "${{{expr}}}"),
            Binding(name, value) => write!(f, "{name} = {value}"),
        }
    }
}

impl Expression {
    pub fn eval(&self, env: &Env) -> EvalResult {
        use Expression::*;
        use Object::*;
        match self {
            IntLiteral(int) => Ok(Int(*int)),
            FloatLiteral(float) => Ok(Float(*float)),
            StringLiteral(string) => Ok(Str(string.clone())),
            InterpolateString(string, interpolates) => Ok(Str({
                let mut offset = 0;
                let mut value = string.clone();
                for (idx, r) in interpolates.iter().map(|(idx, r)| (idx, r.eval(env))) {
                    let (p1, p2) = value.split_at(idx + offset);
                    if let Str(s) = r? {
                        value = format!("{p1}{s}{p2}");
                        offset += s.len();
                    } else {
                        return Err(EvalError::from("expected a string").into());
                    }
                }
                value
            })),
            Interpolate(expr) => {
                if let Str(string) = expr.eval(env)? {
                    Ok(Str(string))
                } else {
                    Err(EvalError::from("expected a string").into())
                }
            }
            AttrsLiteral(env, _) => Ok(Attrs(env.clone())),
            ListLiteral(list) => Ok(List(
                list.iter()
                    .map(|expr| Node::Expr(expr.clone().into()))
                    .collect(),
            )),
            FunctionLiteral(arg, body, env) => {
                Ok(Function((**arg).clone(), (**body).clone(), env.clone()))
            }
            Expression::Path(..) => Ok(Null), // FIXME
            Expression::SearchPath(..) => Ok(Null),
            If(cond, consq, alter) => {
                if let Bool(val) = cond.eval(env)? {
                    if val {
                        consq.eval(env)
                    } else {
                        alter.eval(env)
                    }
                } else {
                    Err(EvalError::from("expected a bool").into())
                }
            }
            Let(env, expr) => expr.eval(env),
            With(attrs, expr) => expr.eval(if let Attrs(attrenv) = env.borrow().get(attrs).map_err(|e| e.into())?.force_value()? {&(update_env(env, &attrenv)?)} else {return Err(EvalError::from("expected a set").into())}),
            Assert(assertion, expr) => {
                if let Bool(val) = assertion.eval(env)? {
                    if val {
                        expr.eval(env)
                    } else {
                        expr.eval(env)
                    }
                } else {
                    Err(EvalError::from("expected a bool").into())
                }
            }
            Prefix(token, right) => eval_prefix(token, right.as_ref()),
            Infix(token, left, right) => eval_infix(token, left.as_ref(), right.as_ref()),
            Ident(ident, env) => {
                let t = env
                    .borrow()
                    .get(ident)
                    .map_err(|e| e.into())
                    .map(|mut n| n.force_value());
                t?
            }
            FunctionCall(func, arg) => {
                match func.eval(env)? {
                    Function(formal, body, env) => {
                        match formal {
                            Ident(ident, _) => {
                                let newenv = Rc::new(RefCell::new(Environment::new(Some(env.clone()))));
                                newenv.borrow_mut().set_force(ident, Node::Expr(arg.clone()));
                                body.eval(&newenv)
                            }
                            FormalSet(formals, alias, allow_more, _) => {
                                let argenv = if let AttrsLiteral(env, _) = arg.as_ref() {
                                    env
                                } else {
                                    return Err(EvalError::from("").into());
                                };
                                if formals.len() < argenv.borrow().len() && !allow_more {
                                    return Err(EvalError::from("unexpected argument").into());
                                }
                                for (ident, default) in formals {
                                    let e = {
                                        let t = argenv.borrow().get(&ident);
                                        if let Ok(o) = t {
                                            o
                                        } else {
                                            Node::Expr(default.unwrap().into())
                                        }
                                    };
                                    env.borrow_mut().set(ident, e).map_err(|e| e.into())?;
                                }
                                alias.map(|alias| {
                                    env.borrow_mut().set(alias, Node::Expr(arg.clone()))
                                });
                                body.eval()
                            }
                            _ => unreachable!(), // guarded in parser.rs
                        }
                    }
                    BuiltinFunction(argscount, f) => Ok(if argscount > 1 {
                        Object::BuiltinFunctionApp(
                            argscount - 1,
                            RefCell::new({
                                let mut t = Vec::with_capacity(argscount as usize);
                                t.push(Node::Expr(arg.clone()));
                                t
                            }),
                            f,
                        )
                    } else {
                        f(RefCell::new(vec![Node::Expr(arg.clone())]))?
                    }),
                    BuiltinFunctionApp(argscount, args, f) => {
                        args.borrow_mut().push(Node::Expr(arg.clone()));
                        Ok(if argscount > 1 {
                            Object::BuiltinFunctionApp(argscount - 1, args, f)
                        } else {
                            f(args)?
                        })
                    }
                    _ => Err(
                        EvalError::from("attempt to call something which is not a function").into(),
                    ),
                }
            }

            Binding(..) => Ok(Null),
            Inherit(..) => Ok(Null),
            FormalSet(..) => Ok(Null),
        }
    }
}

fn eval_prefix(token: &Token, right: &Expression) -> EvalResult {
    let val = right.eval()?;
    use Object::*;
    use Token::*;
    match token {
        MINUS => match val {
            Int(int) => Ok(Int(-int)),
            Float(float) => Ok(Float(-float)),
            _ => Err(EvalError::from("unsupported operation").into()),
        },
        BANG => {
            if let Bool(val) = val {
                Ok(Bool(!val))
            } else {
                Err(EvalError::from("unsupported operation").into())
            }
        }
        _ => Err(EvalError::from("unsupported operation").into()),
    }
}

fn eval_infix(token: &Token, left: &Expression, right: &Expression) -> EvalResult {
    use Object::*;
    use Token::*;

    let left_val = left.eval()?;
    if let Attrs(ref env) = left_val {
        if token == &DOT {
            return env
                .borrow()
                .get(&if let Expression::Ident(ident, _) = right {
                    ident.clone()
                } else if let Expression::Interpolate(_) = right {
                    if let Str(string) = right.eval()? {
                        string
                    } else {
                        unreachable!()
                    }
                } else if let Expression::InterpolateString(..) = right {
                    if let Str(string) = right.eval()? {
                        string
                    } else {
                        unreachable!()
                    }
                } else {
                    return Err(EvalError::from("unsupported operation").into());
                })
                .unwrap()
                .force_value();
        }
    }
    let right_val = right.eval()?;
    macro_rules! infix {
        ($t1:tt, $t2:tt, $op:expr) => {
            if let $t1(a) = left_val {
                if let $t2(b) = right_val {
                    Ok($op(a, b))
                } else {
                    Err(EvalError::from("unsupported operation").into())
                }
            } else {
                Err(EvalError::from("unsupported operation").into())
            }
        };
    }
    match token {
        PLUS => left_val + right_val,
        MINUS => left_val - right_val,
        MUL => left_val * right_val,
        SLASH => left_val / right_val,
        AND => infix!(Bool, Bool, |a, b| Bool(a && b)),
        OR => infix!(Bool, Bool, |a, b| Bool(a || b)),
        IMPL => infix!(Bool, Bool, |a: bool, b| Bool(!a || b)),
        UPDATE => {
            if let Attrs(ref l) = left_val {
                if let Attrs(ref r) = right_val {
                    Ok(Attrs(update_env(l, r)?))
                } else {
                    Err(EvalError::from("unsupported operation").into())
                }
            } else {
                Err(EvalError::from("unsupported operation").into())
            }
        }
        CONCAT => infix!(List, List, |mut a: Vec<Node>, mut b: Vec<Node>| List({
            a.append(&mut b);
            a
        })),
        EQ => Ok(Bool(objeq(left_val, right_val)?)),
        NEQ => Ok(Bool(!objeq(left_val, right_val)?)),
        LANGLE => Ok(Bool(objlt(left_val, right_val)?)),
        RANGLE => Ok(Bool(objlt(right_val, left_val)?)),
        LEQ => Ok(Bool(!objlt(right_val, left_val)?)),
        GEQ => Ok(Bool(!objlt(left_val, right_val)?)),
        _ => Err(EvalError::from_string(format!("unsupported operation: {}", token)).into()),
    }
}

impl Add for Object {
    type Output = EvalResult;
    fn add(self, rhs: Self) -> Self::Output {
        use Object::*;
        match self {
            Int(l) => match rhs {
                Int(r) => Ok(Int(l + r)),
                Float(r) => Ok(Float(l as f64 + r)),
                _ => Err(EvalError::from("unsupported operation").into()),
            },
            Float(l) => match rhs {
                Int(r) => Ok(Float(l + r as f64)),
                Float(r) => Ok(Float(l + r)),
                _ => Err(EvalError::from("unsupported operation").into()),
            },
            Str(l) => match rhs {
                Str(r) => Ok(Str(l + &r)),
                _ => Err(EvalError::from("unsupported operation").into()),
            },
            _ => Err(EvalError::from("unsupported operation").into()),
        }
    }
}

impl Sub for Object {
    type Output = EvalResult;
    fn sub(self, rhs: Self) -> Self::Output {
        use Object::*;
        match self {
            Int(l) => match rhs {
                Int(r) => Ok(Int(l - r)),
                Float(r) => Ok(Float(l as f64 - r)),
                _ => Err(EvalError::from("unsupported operation").into()),
            },
            Float(l) => match rhs {
                Int(r) => Ok(Float(l - r as f64)),
                Float(r) => Ok(Float(l - r)),
                _ => Err(EvalError::from("unsupported operation").into()),
            },
            _ => Err(EvalError::from("unsupported operation").into()),
        }
    }
}

impl Mul for Object {
    type Output = EvalResult;
    fn mul(self, rhs: Self) -> Self::Output {
        use Object::*;
        match self {
            Int(l) => match rhs {
                Int(r) => Ok(Int(l * r)),
                Float(r) => Ok(Float(l as f64 * r)),
                _ => Err(EvalError::from("unsupported operation").into()),
            },
            Float(l) => match rhs {
                Int(r) => Ok(Float(l * r as f64)),
                Float(r) => Ok(Float(l * r)),
                _ => Err(EvalError::from("unsupported operation").into()),
            },
            _ => Err(EvalError::from("unsupported operation").into()),
        }
    }
}

impl Div for Object {
    type Output = EvalResult;
    fn div(self, rhs: Self) -> Self::Output {
        use Object::*;
        match self {
            Int(l) => match rhs {
                Int(r) => Ok(Int(l / r)),
                Float(r) => Ok(Float(l as f64 / r)),
                _ => Err(EvalError::from("unsupported operation").into()),
            },
            Float(l) => match rhs {
                Int(r) => Ok(Float(l / r as f64)),
                Float(r) => Ok(Float(l / r)),
                _ => Err(EvalError::from("unsupported operation").into()),
            },
            _ => Err(EvalError::from("unsupported operation").into()),
        }
    }
}

impl Into<Node> for Expression {
    fn into(self) -> Node {
        Node::Expr(self.into())
    }
}

impl Into<Node> for Box<Expression> {
    fn into(self) -> Node {
        Node::Expr(self)
    }
}

impl Into<ParseResult> for Expression {
    fn into(self) -> ParseResult {
        Ok(self.into())
    }
}

/*
    fn eval(&self, env: Rc<RefCell<Environment>>, ctx: ErrorCtx) -> EvalResult {
        let ctx = ctx.with(EvalError::new("while evaluating function call"));
        let e = self.func.eval(env.clone(), ctx.clone())?;
        let fa = e.as_any();
        if fa.is::<BuiltinFunction>() {
            convany!(fa, BuiltinFunction).call(EvaledOr::expr(env, self.arg.clone(), ctx.clone()))
        } else if fa.is::<BuiltinFunctionApp>() {
            convany!(fa, BuiltinFunctionApp).call(EvaledOr::expr(
                env,
                self.arg.clone(),
                ctx.clone(),
            ))
        } else {
            fa.downcast_ref::<Lambda>()
                .unwrap()
                .call(self.arg.eval(env, ctx.clone())?, ctx)
        }
    }
}

impl Display for FunctionCallExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "({} {})", self.func, self.arg)
    }
}

#[derive(Debug)]
pub struct IfExpr {
    cond: Rc<dyn Expression>,
    consq: Rc<dyn Expression>,
    alter: Rc<dyn Expression>,
}

impl IfExpr {
    pub fn new(
        cond: Rc<dyn Expression>,
        consq: Rc<dyn Expression>,
        alter: Rc<dyn Expression>,
    ) -> IfExpr {
        IfExpr { cond, consq, alter }
    }
}

impl Expression for IfExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eval(&self, env: Rc<RefCell<Environment>>, ctx: ErrorCtx) -> EvalResult {
        let ctx = ctx.with(EvalError::new("while evaluating if expr"));
        let c = self.cond.eval(env.clone(), ctx.clone())?;
        let c = c.as_any();
        if c.is::<Bool>() {
            if *convany!(c, Bool) {
                self.consq.eval(env, ctx)
            } else {
                self.alter.eval(env, ctx)
            }
        } else {
            unimplemented!()
        }
    }
}

impl Display for IfExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "if {} then {} else {}",
            self.cond, self.consq, self.alter
        )
    }
}

#[derive(Debug)]
pub struct BindingExpr {
    pub name: Rc<dyn Expression>,
    pub value: Rc<dyn Expression>,
}

impl BindingExpr {
    pub fn new(name: Rc<dyn Expression>, value: Rc<dyn Expression>) -> BindingExpr {
        BindingExpr { name, value }
    }

    pub fn pair(
        &self,
        env: Rc<RefCell<Environment>>,
        ctx: ErrorCtx,
    ) -> Result<(String, EvaledOr), Rc<dyn NixRsError>> {
        let ctx = ctx.with(EvalError::new("while evaluating binding expr"));
        let a = self.name.as_any();
        Ok(if a.is::<IdentifierExpr>() {
            (
                a.downcast_ref::<IdentifierExpr>().unwrap().ident.clone(),
                EvaledOr::expr(env, self.value.clone(), ctx),
            )
        } else if a.is::<StringLiteralExpr>() {
            (
                convany!(a, StringLiteralExpr).literal.clone(),
                EvaledOr::expr(env, self.value.clone(), ctx),
            )
        } else if a.is::<InterpolateStringExpr>() {
            (
                convany!(self.name.eval(env.clone(), ctx.clone())?.as_any(), Str).clone(),
                EvaledOr::expr(env, self.value.clone(), ctx),
            )
        } else if a.is::<InfixExpr>() && convany!(a, InfixExpr).token == Token::DOT {
            BindingExpr::new(
                convany!(a, InfixExpr).left.clone(),
                Rc::new(AttrsLiteralExpr::new(
                    vec![Rc::new(BindingExpr::new(
                        convany!(a, InfixExpr).right.clone(),
                        self.value.clone(),
                    ))],
                    false,
                )),
            )
            .pair(env, ctx)?
        } else {
            return Err(ctx.unwind(EvalError::from_string(
                "invalid binding: ".to_owned() + &self.to_string(),
            )));
        })
    }
}

impl Expression for BindingExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eval(&self, _env: Rc<RefCell<Environment>>, _ctx: ErrorCtx) -> EvalResult {
        unimplemented!()
    }
}

impl Display for BindingExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{} = {}", self.name, self.value)
    }
}

#[derive(Debug)]
pub struct AttrsLiteralExpr {
    bindings: Vec<Rc<dyn Expression>>,
    rec: bool,
}

impl AttrsLiteralExpr {
    pub fn new(bindings: Vec<Rc<dyn Expression>>, rec: bool) -> AttrsLiteralExpr {
        AttrsLiteralExpr { bindings, rec }
    }
}

impl Expression for AttrsLiteralExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eval(&self, env: Rc<RefCell<Environment>>, ctx: ErrorCtx) -> EvalResult {
        let newenv = Rc::new(RefCell::new(Environment::new(Some(env.clone()))));
        for b in self.bindings.iter() {
            if b.as_any().is::<BindingExpr>() {
                let (name, value) = convany!(b.as_any(), BindingExpr).pair(
                    if self.rec {
                        newenv.clone()
                    } else {
                        env.clone()
                    },
                    ctx.with(EvalError::new("while evaluating attrs expr")),
                )?;
                let ret = newenv.borrow_mut().set(name.clone(), value.clone());
                if ret.is_err() {
                    drop(ret);
                    convany!(newenv.borrow().get(&name).unwrap().eval()?.as_any(), Attrs)
                        .merge(value.eval()?)?;
                }
            } else {
                // InheritExpr
                let inherit = b.as_any().downcast_ref::<InheritExpr>().unwrap();
                for (k, v) in inherit.apply(
                    env.clone(),
                    ctx.with(EvalError::new("while evaluating attrs expr")),
                )? {
                    newenv.borrow_mut().set(k, v).unwrap();
                }
            }
        }
        Ok(Rc::new(Attrs::new(newenv)))
    }
}

impl Display for AttrsLiteralExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if self.rec {
            write!(f, "rec ")?;
        }
        write!(f, "{{")?;
        for binding in self.bindings.iter() {
            write!(f, " {};", binding)?;
        }
        write!(f, " }}")
    }
}

#[derive(Debug)]
pub struct ArgSetExpr {
    pub args: Vec<(String, Option<Rc<dyn Expression>>)>,
    pub allow_more: bool,
    pub alias: Option<String>,
}

impl ArgSetExpr {
    pub fn new(
        args: Vec<(String, Option<Rc<dyn Expression>>)>,
        allow_more: bool,
        alias: Option<String>,
    ) -> ArgSetExpr {
        ArgSetExpr {
            args,
            allow_more,
            alias,
        }
    }
}

impl Expression for ArgSetExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eval(&self, _env: Rc<RefCell<Environment>>, _ctx: ErrorCtx) -> EvalResult {
        unimplemented!()
    }
}

impl Display for ArgSetExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{{ ")?;
        let mut first = true;
        for arg in self.args.iter() {
            if first {
                first = false;
            } else {
                write!(f, ", ")?;
            }
            write!(f, "{}", arg.0)?;
            if arg.1.is_some() {
                write!(f, " ? {}", arg.1.clone().unwrap())?;
            }
        }
        if self.allow_more {
            write!(f, ", ...")?;
        }
        write!(f, " }}")?;
        if self.alias.is_some() {
            write!(f, " @ {}", self.alias.as_ref().unwrap())?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct ListLiteralExpr {
    items: Vec<Rc<dyn Expression>>,
}

impl ListLiteralExpr {
    pub fn new(items: Vec<Rc<dyn Expression>>) -> ListLiteralExpr {
        ListLiteralExpr { items }
    }
}

impl Expression for ListLiteralExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eval(&self, env: Rc<RefCell<Environment>>, ctx: ErrorCtx) -> EvalResult {
        let ctx = ctx.with(EvalError::new("while evaluating list"));
        Ok(Rc::new(List::new(
            self.items
                .iter()
                .map(|i| EvaledOr::expr(env.clone(), i.clone(), ctx.clone()))
                .collect(),
        )))
    }
}

impl Display for ListLiteralExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "[ ")?;
        for item in self.items.iter() {
            write!(f, "{item} ")?;
        }
        write!(f, "]")
    }
}

#[derive(Debug)]
pub struct LetExpr {
    bindings: Vec<Rc<dyn Expression>>,
    expr: Rc<dyn Expression>,
}

impl LetExpr {
    pub fn new(bindings: Vec<Rc<dyn Expression>>, expr: Rc<dyn Expression>) -> LetExpr {
        LetExpr { bindings, expr }
    }
}

impl Expression for LetExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eval(&self, env: Rc<RefCell<Environment>>, ctx: ErrorCtx) -> EvalResult {
        let ctx = ctx.with(EvalError::new("while evaluating let expr"));
        let newenv = Rc::new(RefCell::new(Environment::new(Some(env.clone()))));
        for b in self.bindings.iter() {
            let b = b.as_any().downcast_ref::<BindingExpr>().unwrap();
            let env = newenv.clone();
            newenv
                .borrow_mut()
                .set(
                    convany!(b.name.as_any(), IdentifierExpr).ident.clone(),
                    EvaledOr::expr(env, b.value.clone(), ctx.clone()),
                )
                .unwrap();
        }
        self.expr.eval(newenv, ctx)
    }
}

impl Display for LetExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "(let ")?;
        for binding in self.bindings.iter() {
            write!(f, "{binding}; ")?;
        }
        write!(f, "in {})", self.expr)
    }
}

#[derive(Debug)]
pub struct WithExpr {
    attrs: Rc<dyn Expression>,
    expr: Rc<dyn Expression>,
}

impl WithExpr {
    pub fn new(attrs: Rc<dyn Expression>, expr: Rc<dyn Expression>) -> WithExpr {
        WithExpr { attrs, expr }
    }
}

impl Expression for WithExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eval(&self, env: Rc<RefCell<Environment>>, ctx: ErrorCtx) -> EvalResult {
        let e = self.attrs.eval(
            env.clone(),
            ctx.with(EvalError::new("while evaluating with expr")),
        )?;
        let newenv = convany!(e.as_any(), Attrs).env.clone();
        newenv.borrow_mut().father = Some(env);
        self.expr.eval(
            newenv,
            ctx.with(EvalError::new("while evaluating with expr")),
        )
    }
}

impl Display for WithExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "(with {}; {})", self.attrs, self.expr)
    }
}

#[derive(Debug)]
pub struct AssertExpr {
    assertion: Rc<dyn Expression>,
    expr: Rc<dyn Expression>,
}

impl AssertExpr {
    pub fn new(assertion: Rc<dyn Expression>, expr: Rc<dyn Expression>) -> AssertExpr {
        AssertExpr { assertion, expr }
    }
}

impl Expression for AssertExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eval(&self, env: Rc<RefCell<Environment>>, ctx: ErrorCtx) -> EvalResult {
        let ctx = ctx.with(EvalError::new("while evaluating assert expr"));
        let assertion = self.assertion.eval(env.clone(), ctx.clone())?;
        let assertion = assertion.as_any();
        if assertion.is::<Bool>() {
            if *convany!(assertion, Bool) {
                self.expr.eval(env, ctx)
            } else {
                Err(ctx.unwind(EvalError::from_string(format!(
                    "assertion {} failed",
                    self.assertion
                ))))
            }
        } else {
            Err(ctx.unwind(EvalError::from_string(format!(
                "expect {} to be Bool",
                self.assertion
            ))))
        }
    }
}

impl Display for AssertExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "(assert {}; {})", self.assertion, self.expr)
    }
}

#[derive(Debug)]
pub struct InheritExpr {
    inherits: Vec<Rc<dyn Expression>>,
    from: Option<Rc<dyn Expression>>,
}

impl InheritExpr {
    pub fn new(inherits: Vec<Rc<dyn Expression>>, from: Option<Rc<dyn Expression>>) -> InheritExpr {
        InheritExpr {
            inherits,
            from: from.map(Rc::from),
        }
    }

    fn apply(
        &self,
        env: Rc<RefCell<Environment>>,
        ctx: ErrorCtx,
    ) -> Result<Vec<(String, EvaledOr)>, Rc<dyn NixRsError>> {
        let mut ret = Vec::new();
        /* let env = self.from.clone().map_or(env.clone(), |f| {
        }); */
        let env = if let Some(f) = &self.from {
            env.clone()
                .borrow_mut()
                .get(&if f.as_any().is::<IdentifierExpr>() {
                    f.as_any()
                        .downcast_ref::<IdentifierExpr>()
                        .unwrap()
                        .ident
                        .clone()
                } else {
                    // StringLiteralExpr
                    f.eval(
                        env.clone(),
                        ctx.with(EvalError::new("while evaluating inherit expr")),
                    )?
                    .as_any()
                    .downcast_ref::<Str>()
                    .unwrap()
                    .clone()
                })
                .unwrap()
                .eval()?
                .as_any()
                .downcast_ref::<Attrs>()
                .unwrap()
                .env
                .clone()
        } else {
            env.clone()
        };
        for i in self.inherits.iter() {
            let string = if i.as_any().is::<IdentifierExpr>() {
                i.as_any()
                    .downcast_ref::<IdentifierExpr>()
                    .unwrap()
                    .ident
                    .clone()
            } else if i.as_any().is::<StringLiteralExpr>() {
                i.as_any()
                    .downcast_ref::<StringLiteralExpr>()
                    .unwrap()
                    .literal
                    .clone()
            } else {
                return Err(ctx.unwind(EvalError::new("unsupported type")));
            };
            ret.push((string.clone(), env.borrow().get(&string).unwrap()));
        }
        Ok(ret)
    }
}

impl Expression for InheritExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eval(&self, _env: Rc<RefCell<Environment>>, _ctx: ErrorCtx) -> EvalResult {
        unimplemented!()
    }
}

impl Display for InheritExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "inherit ")?;
        if self.from.is_some() {
            write!(f, "({}) ", self.from.as_ref().unwrap())?;
        }
        let mut first = true;
        for inherit in self.inherits.iter() {
            if first {
                first = false;
            } else {
                write!(f, " ")?;
            }
            write!(f, "{inherit}")?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct PathLiteralExpr {
    literal: String,
    relative: bool,
}

impl PathLiteralExpr {
    pub fn new(literal: String, relative: bool) -> PathLiteralExpr {
        PathLiteralExpr { literal, relative }
    }
}

impl Expression for PathLiteralExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eval(&self, _env: Rc<RefCell<Environment>>, _ctx: ErrorCtx) -> EvalResult {
        todo!()
    }
}

impl Display for PathLiteralExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.literal)
    }
}

#[derive(Debug)]
pub struct SearchPathExpr {
    path: Rc<dyn Expression>,
}

impl SearchPathExpr {
    pub fn new(path: Rc<dyn Expression>) -> SearchPathExpr {
        SearchPathExpr { path }
    }
}

impl Expression for SearchPathExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eval(&self, _env: Rc<RefCell<Environment>>, _ctx: ErrorCtx) -> EvalResult {
        todo!()
    }
}

impl Display for SearchPathExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "<{}>", self.path)
    }
}

#[derive(Debug)]
pub struct InterpolateExpr {
    pub ident: Rc<dyn Expression>,
}

impl InterpolateExpr {
    pub fn new(ident: Rc<dyn Expression>) -> InterpolateExpr {
        InterpolateExpr { ident }
    }
}

impl Expression for InterpolateExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eval(&self, _env: Rc<RefCell<Environment>>, _ctx: ErrorCtx) -> EvalResult {
        todo!()
    }
}

impl Display for InterpolateExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "${{{}}}", self.ident)
    }
}
*/
