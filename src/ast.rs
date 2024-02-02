use crate::builtins::{PrimOp, PrimOpApp};
use crate::convany;
use crate::error::*;
use crate::eval::{Environment, EvalResult};
use crate::object::*;
use crate::token::Token;

use std::any::Any;
use std::cell::RefCell;
use std::fmt::{format, Debug, Display};
use std::rc::Rc;

pub trait Expression: Display + Debug {
    fn as_any(&self) -> &dyn Any;
    fn eval(&self, env: &Rc<RefCell<Environment>>, ctx: &ErrorCtx) -> EvalResult;
}

#[derive(Debug)]
pub struct PrefixExpr {
    pub token: Token,
    pub right: Rc<dyn Expression>,
}

impl PrefixExpr {
    pub fn new(token: Token, right: Rc<dyn Expression>) -> PrefixExpr {
        PrefixExpr { token, right }
    }
}

impl Expression for PrefixExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eval(&self, env: &Rc<RefCell<Environment>>, ctx: &ErrorCtx) -> EvalResult {
        let val = self.right.eval(
            env,
            &ctx.with(EvalError::from("while evaluating prefix expr").into()),
        )?;
        // let a = val.as_any();
        use Token::*;
        match self.token {
            MINUS => {
                if let Ok(r) = val.try_into_int() {
                    Ok(Rc::new(-r))
                } else if let Ok(r) = val.try_into_float() {
                    Ok(Rc::new(-r))
                } else {
                    Err(ctx.unwind(EvalError::from("unsupported operation").into()))
                }
            }
            BANG => {
                if let Ok(r) = val.try_into_bool() {
                    Ok(Rc::new(!r))
                } else {
                    Err(ctx.unwind(EvalError::from("unsupported operation").into()))
                }
            }
            _ => Err(ctx.unwind(EvalError::from("unsupported operation").into())),
        }
    }
}

impl Display for PrefixExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "({}{})", self.token, self.right)
    }
}

#[derive(Debug)]
pub struct InfixExpr {
    pub token: Token,
    pub left: Rc<dyn Expression>,
    pub right: Rc<dyn Expression>,
}

impl InfixExpr {
    pub fn new(token: Token, left: Rc<dyn Expression>, right: Rc<dyn Expression>) -> InfixExpr {
        InfixExpr { token, left, right }
    }
}

impl Expression for InfixExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eval(&self, env: &Rc<RefCell<Environment>>, ctx: &ErrorCtx) -> EvalResult {
        let ctx = ctx.with(EvalError::from("while evaluating infix expr").into());
        let left = self.left.eval(env, &ctx)?;
        if let Ok(l) = left.try_into_attrs() {
            if self.token == Token::DOT {
                let ret = l
                    .env
                    .borrow_mut()
                    .get(&if self.right.as_any().is::<IdentifierExpr>() {
                        convany!(self.right.as_any(), IdentifierExpr).ident.clone()
                    } else if self.right.as_any().is::<StringLiteralExpr>()
                        || self.right.as_any().is::<InterpolateStringExpr>()
                    {
                        self.right.eval(env, &ctx)?.try_into_string()?.clone()
                    } else {
                        return Err(ctx.unwind(EvalError::from("unsupported operation").into()));
                    })
                    .unwrap();
                return ret.eval();
            }
        }
        let right = self.right.eval(env, &ctx)?;
        use Token::*;
        macro_rules! num {
            ($op:expr) => {
                if let Ok(l) = left.try_into_int() {
                    if let Ok(r) = right.try_into_int() {
                        Ok(Rc::new($op(l, r)))
                    } else if let Ok(r) = right.try_into_float() {
                        Ok(Rc::new($op(l as Float, r)))
                    } else {
                        Err(ctx.unwind(EvalError::from("unsupported operation").into()))
                    }
                } else if let Ok(l) = left.try_into_float() {
                    if let Ok(r) = right.try_into_float() {
                        Ok(Rc::new($op(l, r)))
                    } else {
                        Err(ctx.unwind(EvalError::from("unsupported operation").into()))
                    }
                } else {
                    Err(ctx.unwind(EvalError::from("unsupported operation").into()))
                }
            };
        }
        macro_rules! infix {
            ($into:tt, $op:expr) => {
                if let Ok(l) = left.$into() {
                    if let Ok(r) = right.$into() {
                        Ok(Rc::new($op(l, r)))
                    } else {
                        Err(ctx.unwind(EvalError::from("unsupported operation").into()))
                    }
                } else {
                    Err(ctx.unwind(EvalError::from("unsupported operation").into()))
                }
            };
            ($into:tt, $op:expr, $err:tt) => {
                if let Ok(l) = left.$into() {
                    if let Ok(r) = right.$into() {
                        Ok(Rc::new($op(l, r)?))
                    } else {
                        Err(ctx.unwind(EvalError::from("unsupported operation").into()))
                    }
                } else {
                    Err(ctx.unwind(EvalError::from("unsupported operation").into()))
                }
            };
        }
        match self.token {
            PLUS => num!(|a, b| a + b),
            MINUS => num!(|a, b| a - b),
            MUL => num!(|a, b| a * b),
            SLASH => num!(|a, b| a / b),
            AND => infix!(try_into_bool, |a, b| a && b),
            OR => infix!(try_into_bool, |a, b| a || b),
            IMPL => infix!(try_into_bool, |a: Bool, b| !a || b),
            UPDATE => infix!(try_into_attrs, |a: &Attrs, b| a.update(b), true),
            CONCAT => infix!(try_into_list, |a: &List, b| a.concat(b), true),
            EQ => Ok(Rc::new(objeq(left.clone(), right.clone(), &ctx)?)),
            NEQ => Ok(Rc::new(!objeq(left.clone(), right.clone(), &ctx)?)),
            LANGLE => Ok(Rc::new(objlt(left.clone(), right.clone(), &ctx)?)),
            RANGLE => Ok(Rc::new(objlt(right.clone(), left.clone(), &ctx)?)),
            LEQ => Ok(Rc::new(!objlt(right.clone(), left.clone(), &ctx)?)),
            GEQ => Ok(Rc::new(!objlt(left.clone(), right.clone(), &ctx)?)),
            _ => Err(ctx.unwind(EvalError::from(format!(
                "unsupported operation: {}",
                self.token
            )).into())),
        }
    }
}

impl Display for InfixExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "({} {} {})", self.left, self.token, self.right)
    }
}

#[derive(Debug)]
pub struct IdentifierExpr {
    pub ident: String,
}

impl IdentifierExpr {
    pub fn new(ident: String) -> IdentifierExpr {
        IdentifierExpr { ident }
    }
}

impl Expression for IdentifierExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eval(&self, env: &Rc<RefCell<Environment>>, _ctx: &ErrorCtx) -> EvalResult {
        env.borrow().get(&self.ident).unwrap().eval()
    }
}

impl Display for IdentifierExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.ident)
    }
}

#[derive(Debug)]
pub struct IntLiteralExpr {
    literal: i64,
}

impl IntLiteralExpr {
    pub fn new(s: String) -> IntLiteralExpr {
        IntLiteralExpr {
            literal: s.parse().unwrap(),
        }
    }
}

impl Expression for IntLiteralExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eval(&self, _env: &Rc<RefCell<Environment>>, _ctx: &ErrorCtx) -> EvalResult {
        Ok(Rc::new(self.literal))
    }
}

impl Display for IntLiteralExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.literal)
    }
}

#[derive(Debug)]
pub struct FloatLiteralExpr {
    literal: f64,
}

impl FloatLiteralExpr {
    pub fn new(s: String) -> FloatLiteralExpr {
        FloatLiteralExpr {
            literal: s.parse().unwrap(),
        }
    }
}

impl Expression for FloatLiteralExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eval(&self, _env: &Rc<RefCell<Environment>>, _ctx: &ErrorCtx) -> EvalResult {
        Ok(Rc::new(self.literal))
    }
}

impl Display for FloatLiteralExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.literal)
    }
}

#[derive(Debug)]
pub struct EllipsisLiteralExpr;

impl Expression for EllipsisLiteralExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eval(&self, _env: &Rc<RefCell<Environment>>, _ctx: &ErrorCtx) -> EvalResult {
        unimplemented!()
    }
}

impl Display for EllipsisLiteralExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "...")
    }
}

#[derive(Debug)]
pub struct StringLiteralExpr {
    pub literal: String,
}

impl StringLiteralExpr {
    pub fn new(s: String) -> StringLiteralExpr {
        StringLiteralExpr { literal: s.clone() }
    }
}

impl Expression for StringLiteralExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eval(&self, _env: &Rc<RefCell<Environment>>, _ctx: &ErrorCtx) -> EvalResult {
        Ok(Rc::new(self.literal.clone()))
    }
}

impl Display for StringLiteralExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, r#""{}""#, self.literal)
    }
}

#[derive(Debug)]
pub struct InterpolateStringExpr {
    pub literal: String,
    pub replaces: Vec<(usize, Rc<dyn Expression>)>,
}

impl InterpolateStringExpr {
    pub fn new(s: String, replaces: Vec<(usize, Rc<dyn Expression>)>) -> InterpolateStringExpr {
        InterpolateStringExpr {
            literal: s.clone(),
            replaces,
        }
    }
}

impl Expression for InterpolateStringExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eval(&self, env: &Rc<RefCell<Environment>>, ctx: &ErrorCtx) -> EvalResult {
        let ctx = ctx.with(EvalError::from("while evaluating string literal").into());
        let s: Str = self.try_into()?;
        Ok(Rc::new(s))
    }
}

impl Display for InterpolateStringExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, r#""{}""#, self.literal)
    }
}

#[derive(Debug)]
pub struct FunctionLiteralExpr {
    arg: Rc<dyn Expression>,
    body: Rc<dyn Expression>,
}

impl FunctionLiteralExpr {
    pub fn new(arg: Rc<dyn Expression>, body: Rc<dyn Expression>) -> FunctionLiteralExpr {
        FunctionLiteralExpr { arg, body }
    }
}

impl Expression for FunctionLiteralExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eval(&self, env: &Rc<RefCell<Environment>>, _ctx: &ErrorCtx) -> EvalResult {
        Ok(Rc::new(Lambda::new(
            self.arg.clone(),
            self.body.clone(),
            Rc::downgrade(env)
        )))
    }
}

impl Display for FunctionLiteralExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "({}: {})", self.arg, self.body)
    }
}

#[derive(Debug)]
pub struct FunctionCallExpr {
    func: Rc<dyn Expression>,
    arg: Rc<dyn Expression>,
}

impl FunctionCallExpr {
    pub fn new(func: Rc<dyn Expression>, arg: Rc<dyn Expression>) -> FunctionCallExpr {
        FunctionCallExpr { func, arg }
    }
}

impl Expression for FunctionCallExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eval(&self, env: &Rc<RefCell<Environment>>, ctx: &ErrorCtx) -> EvalResult {
        let ctx = ctx.with(EvalError::from("while evaluating function call").into());
        let e = self.func.eval(env, &ctx)?;
        if let Ok(func) = e.try_into_primop() {
            func.call(EvaledOr::expr(env, self.arg.clone(), ctx))
        } else if let Ok(func) = e.try_into_primop_app() {
            func.call(EvaledOr::expr(env, self.arg.clone(), ctx))
        } else if let Ok(func) = e.try_into_lambda() {
            func.call(self.arg.eval(env, &ctx)?, &ctx)
        } else {
            Err(EvalError::from(format!("attempt to call something which is not a function but a '{}'", e.typename())).into())
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

    fn eval(&self, env: &Rc<RefCell<Environment>>, ctx: &ErrorCtx) -> EvalResult {
        let ctx = ctx.with(EvalError::from("while evaluating if expr").into());
        let cond = self.cond.eval(env, &ctx)?;
        if let Ok(cond) = cond.try_into_bool() {
            if cond {
                self.consq.eval(env, &ctx)
            } else {
                self.alter.eval(env, &ctx)
            }
        } else {
            Err(ctx.unwind(EvalError::from(format!("value is a '{}' while a bool was expected", cond.typename())).into()))
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
        env: &Rc<RefCell<Environment>>,
        ctx: &ErrorCtx,
    ) -> Result<(String, EvaledOr), Rc<dyn NixRsError>> {
        let ctx = ctx.with(EvalError::from("while evaluating binding expr").into());
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
                Str::try_from(convany!(a, InterpolateStringExpr))?,
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
            .pair(env, &ctx)?
        } else {
            return Err(ctx.unwind(EvalError::from(
                "invalid binding: ".to_owned() + &self.to_string(),
            ).into()));
        })
    }
}

impl Expression for BindingExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eval(&self, _env: &Rc<RefCell<Environment>>, _ctx: &ErrorCtx) -> EvalResult {
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

    fn eval(&self, env: &Rc<RefCell<Environment>>, ctx: &ErrorCtx) -> EvalResult {
        let newenv = Rc::new(RefCell::new(Environment::new(Some(env.clone()))));
        for b in self.bindings.iter() {
            if b.as_any().is::<BindingExpr>() {
                let (name, value) = convany!(b.as_any(), BindingExpr).pair(
                    if self.rec {
                        &newenv
                    } else {
                        env
                    },
                    &ctx.with(EvalError::from("while evaluating attrs expr").into()),
                )?;
                let ret = newenv.borrow_mut().set(name.clone(), value.clone());
                if ret.is_err() {
                    drop(ret);
                    newenv.borrow().get(&name).unwrap().eval()?.try_into_attrs()?.merge(value.eval()?.try_into_attrs()?);
                }
            } else {
                // InheritExpr
                let inherit = b.as_any().downcast_ref::<InheritExpr>().unwrap();
                for (k, v) in inherit.apply(
                    env,
                    &ctx.with(EvalError::from("while evaluating attrs expr").into()),
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

    fn eval(&self, _env: &Rc<RefCell<Environment>>, _ctx: &ErrorCtx) -> EvalResult {
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

    fn eval(&self, env: &Rc<RefCell<Environment>>, ctx: &ErrorCtx) -> EvalResult {
        let ctx = ctx.with(EvalError::from("while evaluating list").into());
        Ok(Rc::new(List::new(
            self.items
                .iter()
                .map(|i| EvaledOr::expr(env, i.clone(), ctx.clone()))
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

    fn eval(&self, env: &Rc<RefCell<Environment>>, ctx: &ErrorCtx) -> EvalResult {
        let ctx = ctx.with(EvalError::from("while evaluating let expr").into());
        let newenv = Rc::new(RefCell::new(Environment::new(Some(env.clone()))));
        for b in self.bindings.iter() {
            let b = b.as_any().downcast_ref::<BindingExpr>().unwrap();
            let env = newenv.clone();
            newenv
                .borrow_mut()
                .set(
                    convany!(b.name.as_any(), IdentifierExpr).ident.clone(),
                    EvaledOr::expr(&env, b.value.clone(), ctx.clone()),
                )
                .unwrap();
        }
        self.expr.eval(&newenv, &ctx)
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

    fn eval(&self, env: &Rc<RefCell<Environment>>, ctx: &ErrorCtx) -> EvalResult {
        let e = self.attrs.eval(
            env,
            &ctx.with(EvalError::from("while evaluating with expr").into()),
        )?;
        let newenv = Rc::new(RefCell::clone(&*e.try_into_attrs()?.env));
        newenv.borrow_mut().father = Some(env.clone());
        self.expr.eval(
            &newenv,
            &ctx.with(EvalError::from("while evaluating with expr").into()),
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

    fn eval(&self, env: &Rc<RefCell<Environment>>, ctx: &ErrorCtx) -> EvalResult {
        let ctx = ctx.with(EvalError::from("while evaluating assert expr").into());
        let assertion = self.assertion.eval(env, &ctx)?;
        if let Ok(assertion) = assertion.try_into_bool() {
            if assertion {
                self.expr.eval(env, &ctx)
            } else {
                Err(ctx.unwind(EvalError::from(format!(
                    "assertion {} failed",
                    self.assertion
                )).into()))
            }
        } else {
            Err(ctx.unwind(EvalError::from(format!(
            "value is a '{}' while a bool is expected",
                self.assertion
            )).into()))
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
        env: &Rc<RefCell<Environment>>,
        ctx: &ErrorCtx,
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
                        env,
                        &ctx.with(EvalError::from("while evaluating inherit expr").into()),
                    )?.try_into_string()?.clone()
                })
                .unwrap()
                .eval()?
                .try_into_attrs()?
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
                return Err(ctx.unwind(EvalError::from("unsupported type").into()));
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

    fn eval(&self, _env: &Rc<RefCell<Environment>>, _ctx: &ErrorCtx) -> EvalResult {
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

    fn eval(&self, _env: &Rc<RefCell<Environment>>, _ctx: &ErrorCtx) -> EvalResult {
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

    fn eval(&self, _env: &Rc<RefCell<Environment>>, _ctx: &ErrorCtx) -> EvalResult {
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

    fn eval(&self, _env: &Rc<RefCell<Environment>>, _ctx: &ErrorCtx) -> EvalResult {
        todo!()
    }
}

impl Display for InterpolateExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "${{{}}}", self.ident)
    }
}
