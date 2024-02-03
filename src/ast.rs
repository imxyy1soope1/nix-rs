use crate::builtins::{BuiltinFunction, BuiltinFunctionApp};
use crate::convany;
use crate::error::*;
use crate::eval::{Environment, EvalResult};
use crate::object::*;
use crate::token::Token;

use std::any::Any;
use std::cell::RefCell;
use std::fmt::{Debug, Display};
use std::rc::Rc;

pub trait Expression: Display + Debug {
    fn as_any(&self) -> &dyn Any;
    fn eval(&self, env: &Rc<RefCell<Environment>>, ctx: &ErrorCtx) -> EvalResult;
}

#[derive(Debug)]
pub struct PrefixExpr {
    pub token: Token,
    pub right: Box<dyn Expression>,
}

impl PrefixExpr {
    pub fn new(token: Token, right: Box<dyn Expression>) -> PrefixExpr {
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
            &ctx.with(EvalError::new("while evaluating prefix expr")),
        )?;
        let a = val.as_any();
        use Token::*;
        match self.token {
            MINUS => {
                if a.is::<Int>() {
                    Ok(Box::new(-convany!(a, Int)))
                } else if a.is::<Float>() {
                    Ok(Box::new(-convany!(a, Float)))
                } else {
                    Err(ctx.unwind(EvalError::new("unsupported operation")))
                }
            }
            BANG => {
                if a.is::<Bool>() {
                    Ok(Box::new(!a.downcast_ref::<bool>().unwrap()))
                } else {
                    Err(ctx.unwind(EvalError::new("unsupported operation")))
                }
            }
            _ => Err(ctx.unwind(EvalError::new("unsupported operation"))),
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
    pub left: Box<dyn Expression>,
    pub right: Box<dyn Expression>,
}

impl InfixExpr {
    pub fn new(token: Token, left: Box<dyn Expression>, right: Box<dyn Expression>) -> InfixExpr {
        InfixExpr { token, left, right }
    }
}

impl Expression for InfixExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eval(&self, env: &Rc<RefCell<Environment>>, ctx: &ErrorCtx) -> EvalResult {
        let ctx = ctx.with(EvalError::new("while evaluating infix expr"));
        let le = self.left.eval(&env, &ctx)?;
        if le.as_any().is::<Attrs>() && self.token == Token::DOT {
            let ret = convany!(le.as_any(), Attrs)
                .env
                .borrow()
                .get_local(&if self.right.as_any().is::<IdentifierExpr>() {
                    convany!(self.right.as_any(), IdentifierExpr).ident.clone()
                } else if self.right.as_any().is::<StringLiteralExpr>()
                    || self.right.as_any().is::<InterpolateStringExpr>()
                {
                    let e = self.right.eval(&env, &ctx)?;
                    convany!(e.as_any(), Str).clone()
                } else {
                    return Err(ctx.unwind(EvalError::new("unsupported operation")));
                })
                .unwrap();
            return ret.eval();
        }
        let re = self.right.eval(&env, &ctx)?;
        let la = le.as_any();
        let ra = re.as_any();
        use Token::*;
        macro_rules! num {
            ($op:expr) => {
                if la.is::<Int>() {
                    if ra.is::<Int>() {
                        Ok(Box::new($op(convany!(la, Int), convany!(ra, Int))))
                    } else if ra.is::<Float>() {
                        Ok(Box::new($op(
                            *convany!(la, Int) as Float,
                            convany!(ra, Float),
                        )))
                    } else {
                        Err(ctx.unwind(EvalError::new("unsupported operation")))
                    }
                } else if la.is::<Float>() {
                    if ra.is::<Int>() {
                        Ok(Box::new($op(
                            convany!(la, Float),
                            *convany!(ra, Int) as Float,
                        )))
                    } else if ra.is::<Float>() {
                        Ok(Box::new($op(convany!(la, Float), convany!(ra, Float))))
                    } else {
                        Err(ctx.unwind(EvalError::new("unsupported operation")))
                    }
                } else {
                    Err(ctx.unwind(EvalError::new("unsupported operation")))
                }
            };
        }
        macro_rules! infix {
            ($t1:tt, $t2:tt, $op:expr) => {
                if la.is::<$t1>() && ra.is::<$t2>() {
                    Ok(Box::new($op(convany!(la, $t1), convany!(ra, $t2))))
                } else {
                    Err(ctx.unwind(EvalError::new("unsupported type")))
                }
            };
        }
        match self.token {
            PLUS => num!(|a, b| a + b),
            MINUS => num!(|a, b| a - b),
            MUL => num!(|a, b| a * b),
            SLASH => num!(|a, b| a / b),
            AND => infix!(Bool, Bool, |a: &Bool, b: &Bool| *a && *b),
            OR => infix!(Bool, Bool, |a: &Bool, b: &Bool| *a || *b),
            IMPL => infix!(Bool, Bool, |a: &Bool, b: &Bool| !*a || *b),
            UPDATE => {
                if la.is::<Attrs>() && ra.is::<Attrs>() {
                    Ok(convany!(la, Attrs).update(convany!(ra, Attrs))?)
                } else {
                    Err(ctx.unwind(EvalError::new("unsupported operation")))
                }
            }
            CONCAT => infix!(List, List, |a: &List, b| a.concat(b)),
            EQ => Ok(Box::new(objeq(le, re, ctx)?)),
            NEQ => Ok(Box::new(!(objeq(le, re, ctx)?))),
            LANGLE => Ok(Box::new(objlt(le, re, ctx)?)),
            RANGLE => Ok(Box::new(objlt(re, le, ctx)?)),
            LEQ => Ok(Box::new(!objlt(re, le, ctx)?)),
            GEQ => Ok(Box::new(!objlt(le, re, ctx)?)),
            _ => Err(ctx.unwind(EvalError::from_string(format!(
                "unsupported operation: {}",
                self.token
            )))),
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
        Ok(Box::new(self.literal))
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
        Ok(Box::new(self.literal))
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
        Ok(Box::new(self.literal.clone()))
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
    pub replaces: Vec<(usize, Box<dyn Expression>)>,
}

impl InterpolateStringExpr {
    pub fn new(s: String, replaces: Vec<(usize, Box<dyn Expression>)>) -> InterpolateStringExpr {
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
        let ctx = ctx.with(EvalError::new("while evaluating string literal"));
        let mut offset = 0;
        let mut value = self.literal.clone();
        for (i, o) in self.replaces.iter() {
            let (p1, p2) = value.split_at(i + offset);
            let s = convany!(o.eval(env, &ctx)?.as_any(), Str).clone();
            value = format!("{p1}{s}{p2}");
            offset += s.len()
        }
        Ok(Box::new(value))
    }
}

impl Display for InterpolateStringExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, r#""{}""#, self.literal)
    }
}

#[derive(Debug)]
pub struct FunctionLiteralExpr {
    arg: Box<dyn Expression>,
    body: Box<dyn Expression>,
}

impl FunctionLiteralExpr {
    pub fn new(arg: Box<dyn Expression>, body: Box<dyn Expression>) -> FunctionLiteralExpr {
        FunctionLiteralExpr { arg, body }
    }
}

impl Expression for FunctionLiteralExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eval(&self, env: &Rc<RefCell<Environment>>, _ctx: &ErrorCtx) -> EvalResult {
        Ok(Box::new(Lambda::new(
            self.arg.clone(),
            self.body.clone(),
            env.clone()
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
    pub fn new(func: Box<dyn Expression>, arg: Box<dyn Expression>) -> FunctionCallExpr {
        FunctionCallExpr { func, arg }
    }
}

impl Expression for FunctionCallExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eval(&self, env: &Rc<RefCell<Environment>>, ctx: &ErrorCtx) -> EvalResult {
        let ctx = ctx.with(EvalError::new("while evaluating function call"));
        let e = self.func.eval(&env, &ctx)?;
        let fa = e.as_any();
        if fa.is::<BuiltinFunction>() {
            convany!(fa, BuiltinFunction).call(EvaledOr::expr(env.clone(), self.arg.clone(), ctx.clone()))
        } else if fa.is::<BuiltinFunctionApp>() {
            convany!(fa, BuiltinFunctionApp).call(EvaledOr::expr(
                env.clone(),
                self.arg.clone(),
                ctx.clone(),
            ))
        } else {
            fa.downcast_ref::<Lambda>()
                .unwrap()
                .call(self.arg.eval(env, &ctx)?, &ctx)
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
    cond: Box<dyn Expression>,
    consq: Box<dyn Expression>,
    alter: Box<dyn Expression>,
}

impl IfExpr {
    pub fn new(
        cond: Box<dyn Expression>,
        consq: Box<dyn Expression>,
        alter: Box<dyn Expression>,
    ) -> IfExpr {
        IfExpr { cond, consq, alter }
    }
}

impl Expression for IfExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eval(&self, env: &Rc<RefCell<Environment>>, ctx: &ErrorCtx) -> EvalResult {
        let ctx = ctx.with(EvalError::new("while evaluating if expr"));
        let c = self.cond.eval(&env, &ctx)?;
        let c = c.as_any();
        if c.is::<Bool>() {
            if *convany!(c, Bool) {
                self.consq.eval(env, &ctx)
            } else {
                self.alter.eval(env, &ctx)
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
    pub name: Box<dyn Expression>,
    pub value: Box<dyn Expression>,
}

impl BindingExpr {
    pub fn new(name: Box<dyn Expression>, value: Box<dyn Expression>) -> BindingExpr {
        BindingExpr { name, value }
    }

    pub fn pair(
        &self,
        env: &Rc<RefCell<Environment>>,
        ctx: &ErrorCtx,
    ) -> Result<(String, EvaledOr), Rc<dyn NixRsError>> {
        let ctx = ctx.with(EvalError::new("while evaluating binding expr"));
        let a = self.name.as_any();
        Ok(if a.is::<IdentifierExpr>() {
            (
                a.downcast_ref::<IdentifierExpr>().unwrap().ident.clone(),
                EvaledOr::expr(env.clone(), self.value.clone(), ctx),
            )
        } else if a.is::<StringLiteralExpr>() {
            (
                convany!(a, StringLiteralExpr).literal.clone(),
                EvaledOr::expr(env.clone(), self.value.clone(), ctx),
            )
        } else if a.is::<InterpolateStringExpr>() {
            (
                convany!(self.name.eval(&env, &ctx)?.as_any(), Str).clone(),
                EvaledOr::expr(env.clone(), self.value.clone(), ctx),
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
    bindings: Vec<Box<dyn Expression>>,
    rec: bool,
}

impl AttrsLiteralExpr {
    pub fn new(bindings: Vec<Box<dyn Expression>>, rec: bool) -> AttrsLiteralExpr {
        AttrsLiteralExpr { bindings, rec }
    }
}

impl Expression for AttrsLiteralExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eval(&self, env: &Rc<RefCell<Environment>>, ctx: &ErrorCtx) -> EvalResult {
        let newenv = Rc::new(RefCell::new(Environment::new(Some(Rc::downgrade(&env)))));
        for b in self.bindings.iter() {
            if b.as_any().is::<BindingExpr>() {
                let (name, value) = convany!(b.as_any(), BindingExpr).pair(
                    if self.rec {
                        &newenv
                    } else {
                        &env
                    },
                    &ctx.with(EvalError::new("while evaluating attrs expr")),
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
                    &newenv,
                    &ctx.with(EvalError::new("while evaluating attrs expr")),
                )? {
                    newenv.borrow_mut().set(k, v).unwrap();
                }
            }
        }
        Ok(Box::new(Attrs::new(newenv)))
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
    pub args: Vec<(String, Option<Box<dyn Expression>>)>,
    pub allow_more: bool,
    pub alias: Option<String>,
}

impl ArgSetExpr {
    pub fn new(
        args: Vec<(String, Option<Box<dyn Expression>>)>,
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
    items: Vec<Box<dyn Expression>>,
}

impl ListLiteralExpr {
    pub fn new(items: Vec<Box<dyn Expression>>) -> ListLiteralExpr {
        ListLiteralExpr { items }
    }
}

impl Expression for ListLiteralExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eval(&self, env: &Rc<RefCell<Environment>>, ctx: &ErrorCtx) -> EvalResult {
        let ctx = ctx.with(EvalError::new("while evaluating list"));
        Ok(Box::new(List::new(
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

    fn eval(&self, env: &Rc<RefCell<Environment>>, ctx: &ErrorCtx) -> EvalResult {
        let ctx = ctx.with(EvalError::new("while evaluating let expr"));
        let newenv = Rc::new(RefCell::new(Environment::new(Some(Rc::downgrade(&env)))));
        for b in self.bindings.iter() {
            let b = b.as_any().downcast_ref::<BindingExpr>().unwrap();
            newenv
                .borrow_mut()
                .set(
                    convany!(b.name.as_any(), IdentifierExpr).ident.clone(),
                    EvaledOr::expr(newenv.clone(), b.value.clone(), ctx.clone()),
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
            &env,
            &ctx.with(EvalError::new("while evaluating with expr")),
        )?;
        let newenv = convany!(e.as_any(), Attrs).env.clone();
        newenv.borrow_mut().father = Some(Rc::downgrade(&env));
        self.expr.eval(
            &newenv,
            &ctx.with(EvalError::new("while evaluating with expr")),
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
        let ctx = ctx.with(EvalError::new("while evaluating assert expr"));
        let assertion = self.assertion.eval(&env, &ctx)?;
        let assertion = assertion.as_any();
        if assertion.is::<Bool>() {
            if *convany!(assertion, Bool) {
                self.expr.eval(env, &ctx)
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
        env: &Rc<RefCell<Environment>>,
        ctx: &ErrorCtx,
    ) -> Result<Vec<(String, EvaledOr)>, Rc<dyn NixRsError>> {
        let mut ret = Vec::new();
        let env = if let Some(f) = self.from.as_ref() {
            env
                .borrow()
                .get(&if f.as_any().is::<IdentifierExpr>() {
                    f.as_any()
                        .downcast_ref::<IdentifierExpr>()
                        .unwrap()
                        .ident
                        .clone()
                } else {
                    // StringLiteralExpr
                    f.eval(
                        &env,
                        &ctx.with(EvalError::new("while evaluating inherit expr")),
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
