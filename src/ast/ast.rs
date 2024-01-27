use crate::convany;

use super::types::*;
use crate::eval::Environment;
use crate::object::*;
use crate::token::Token;
use core::f64;
use std::any::Any;
use std::cell::RefCell;
use std::fmt::{Debug, Display};
use std::rc::Rc;

pub trait Expression: Display + Debug {
    fn as_any(&self) -> &dyn Any;
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Rc<dyn Object>;
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

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Rc<dyn Object> {
        let val = self.right.eval(env);
        let a = val.as_any();
        use Token::*;
        match self.token {
            MINUS => {
                if a.type_id() == type_ids::INT {
                    Rc::new(-convany!(a, Int))
                } else if a.type_id() == type_ids::FLOAT {
                    Rc::new(-convany!(a, Float))
                } else {
                    unimplemented!()
                }
            }
            BANG => {
                if a.type_id() == type_ids::BOOL {
                    Rc::new(!a.downcast_ref::<bool>().unwrap())
                } else {
                    panic!()
                }
            }
            _ => panic!(),
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
    pub fn new(token: Token, left: Box<dyn Expression>, right: Box<dyn Expression>) -> InfixExpr {
        InfixExpr {
            token,
            left: Rc::from(left),
            right: Rc::from(right),
        }
    }
}

impl Expression for InfixExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Rc<dyn Object> {
        let le = self.left.eval(env.clone());
        let re = self.right.eval(env);
        let la = le.as_any();
        let ra = re.as_any();
        use Token::*;
        match self.token {
            PLUS => {
                if la.type_id() == type_ids::INT {
                    if ra.type_id() == type_ids::INT {
                        Rc::new(convany!(la, Int) + convany!(ra, Int))
                    } else if ra.type_id() == type_ids::FLOAT {
                        Rc::new(*convany!(la, Int) as Float + convany!(ra, Float))
                    } else {
                        unimplemented!()
                    }
                } else if la.type_id() == type_ids::FLOAT {
                    if ra.type_id() == type_ids::INT {
                        Rc::new(convany!(la, Float) + *convany!(ra, Int) as Float)
                    } else if ra.type_id() == type_ids::FLOAT {
                        Rc::new(convany!(la, Float) + convany!(ra, Float))
                    } else {
                        unimplemented!()
                    }
                } else {
                    unimplemented!()
                }
            }
            MINUS => {
                if la.type_id() == type_ids::INT {
                    if ra.type_id() == type_ids::INT {
                        Rc::new(convany!(la, Int) - convany!(ra, Int))
                    } else if ra.type_id() == type_ids::FLOAT {
                        Rc::new(*convany!(la, Int) as Float - convany!(ra, Float))
                    } else {
                        unimplemented!()
                    }
                } else if la.type_id() == type_ids::FLOAT {
                    if ra.type_id() == type_ids::INT {
                        Rc::new(convany!(la, Float) - *convany!(ra, Int) as Float)
                    } else if ra.type_id() == type_ids::FLOAT {
                        Rc::new(convany!(la, Float) - convany!(ra, Float))
                    } else {
                        unimplemented!()
                    }
                } else {
                    unimplemented!()
                }
            }
            MUL => {
                if la.type_id() == type_ids::INT {
                    if ra.type_id() == type_ids::INT {
                        Rc::new(convany!(la, Int) * convany!(ra, Int))
                    } else if ra.type_id() == type_ids::FLOAT {
                        Rc::new(*convany!(la, Int) as Float * convany!(ra, Float))
                    } else {
                        unimplemented!()
                    }
                } else if la.type_id() == type_ids::FLOAT {
                    if ra.type_id() == type_ids::INT {
                        Rc::new(convany!(la, Float) * *convany!(ra, Int) as Float)
                    } else if ra.type_id() == type_ids::FLOAT {
                        Rc::new(convany!(la, Float) * convany!(ra, Float))
                    } else {
                        unimplemented!()
                    }
                } else {
                    unimplemented!()
                }
            }
            SLASH => {
                if la.type_id() == type_ids::INT {
                    if ra.type_id() == type_ids::INT {
                        Rc::new(convany!(la, Int) / convany!(ra, Int))
                    } else if ra.type_id() == type_ids::FLOAT {
                        Rc::new(*convany!(la, Int) as Float / convany!(ra, Float))
                    } else {
                        unimplemented!()
                    }
                } else if la.type_id() == type_ids::FLOAT {
                    if ra.type_id() == type_ids::INT {
                        Rc::new(convany!(la, Float) / *convany!(ra, Int) as Float)
                    } else if ra.type_id() == type_ids::FLOAT {
                        Rc::new(convany!(la, Float) / convany!(ra, Float))
                    } else {
                        unimplemented!()
                    }
                } else {
                    unimplemented!()
                }
            }
            AND => {
                if la.type_id() == type_ids::BOOL && ra.type_id() == type_ids::BOOL {
                    Rc::new(*convany!(la, Bool) && *convany!(ra, Bool))
                } else {
                    unimplemented!()
                }
            }
            OR => {
                if la.type_id() == type_ids::BOOL && ra.type_id() == type_ids::BOOL {
                    Rc::new(*convany!(la, Bool) || *convany!(ra, Bool))
                } else {
                    unimplemented!()
                }
            }
            _ => unimplemented!(),
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

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Rc<dyn Object> {
        env.borrow().get(&self.ident).unwrap()
    }
}

impl Display for IdentifierExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.ident)
    }
}

#[derive(Debug)]
pub struct IntLiteralExpr {
    literal: NixInt,
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

    fn eval(&self, _: Rc<RefCell<Environment>>) -> Rc<dyn Object> {
        Rc::new(self.literal)
    }
}

impl Display for IntLiteralExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.literal)
    }
}

#[derive(Debug)]
pub struct FloatLiteralExpr {
    literal: NixFloat,
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

    fn eval(&self, _: Rc<RefCell<Environment>>) -> Rc<dyn Object> {
        Rc::new(self.literal)
    }
}

impl Display for FloatLiteralExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.literal)
    }
}

#[derive(Debug)]
pub struct BoolLiteralExpr {
    literal: NixBool,
}

impl BoolLiteralExpr {
    pub fn new(v: bool) -> BoolLiteralExpr {
        BoolLiteralExpr { literal: v }
    }
}

impl Expression for BoolLiteralExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eval(&self, _: Rc<RefCell<Environment>>) -> Rc<dyn Object> {
        Rc::new(self.literal)
    }
}

impl Display for BoolLiteralExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.literal)
    }
}

#[derive(Debug)]
pub struct NullLiteralExpr;

impl Expression for NullLiteralExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Rc<dyn Object> {
        Rc::new(Null {})
    }
}

impl Display for NullLiteralExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "null")
    }
}

#[derive(Debug)]
pub struct EllipsisLiteralExpr;

impl Expression for EllipsisLiteralExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Rc<dyn Object> {
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
    pub literal: NixString,
    pub replaces: Vec<(usize, Box<dyn Expression>)>,
}

impl StringLiteralExpr {
    pub fn new(s: String, replaces: Vec<(usize, Box<dyn Expression>)>) -> StringLiteralExpr {
        StringLiteralExpr {
            literal: s.clone(),
            replaces,
        }
    }
}

impl Expression for StringLiteralExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Rc<dyn Object> {
        Rc::new(Str::new(
            self.literal.clone(),
            self.replaces
                .iter()
                .map(|r| (r.0, r.1.eval(env.clone())))
                .collect(),
        ))
    }
}

impl Display for StringLiteralExpr {
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
    pub fn new(arg: Box<dyn Expression>, body: Box<dyn Expression>) -> FunctionLiteralExpr {
        FunctionLiteralExpr {
            arg: Rc::from(arg),
            body: Rc::from(body),
        }
    }
}

impl Expression for FunctionLiteralExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Rc<dyn Object> {
        Rc::new(Lambda::new(
            self.arg.clone(),
            self.body.clone(),
            env.clone(),
        ))
    }
}

impl Display for FunctionLiteralExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "({}: {})", self.arg, self.body)
    }
}

#[derive(Debug)]
pub struct FunctionCallExpr {
    func: Box<dyn Expression>,
    arg: Box<dyn Expression>,
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

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Rc<dyn Object> {
        let e = self.func.eval(env.clone());
        let fa = e.as_any();
        fa.downcast_ref::<Lambda>()
            .unwrap()
            .call(self.arg.eval(env))
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

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Rc<dyn Object> {
        let c = self.cond.eval(env.clone());
        let c = c.as_any();
        if c.type_id() == type_ids::BOOL {
            if *convany!(c, Bool) {
                self.consq.eval(env)
            } else {
                self.alter.eval(env)
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
    pub value: Rc<dyn Expression>,
}

impl BindingExpr {
    pub fn new(name: Box<dyn Expression>, value: Box<dyn Expression>) -> BindingExpr {
        BindingExpr {
            name,
            value: Rc::from(value),
        }
    }

    pub fn string(&self, env: Rc<RefCell<Environment>>) -> String {
        let a = self.name.as_any();
        if a.is::<IdentifierExpr>() {
            a.downcast_ref::<IdentifierExpr>().unwrap().ident.clone()
        } else {
            // StringLiteralExpr
            self.name
                .eval(env)
                .as_any()
                .downcast_ref::<Str>()
                .unwrap()
                .value
                .clone()
        }
    }
}

impl Expression for BindingExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Rc<dyn Object> {
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

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Rc<dyn Object> {
        let newenv = Rc::new(RefCell::new(Environment::new(Some(env.clone()))));
        for b in self.bindings.iter() {
            if b.as_any().is::<BindingExpr>() {
                let b = b.as_any().downcast_ref::<BindingExpr>().unwrap();
                let env = if self.rec {
                    newenv.clone()
                } else {
                    env.clone()
                };
                newenv
                    .borrow_mut()
                    .set(b.string(env.clone()), EvaledOr::expr(env, b.value.clone()))
                    .unwrap();
            } else {
                // InheritExpr
                let inherit = b.as_any().downcast_ref::<InheritExpr>().unwrap();
                let env = inherit.from.clone().map_or(env.clone(), |f| {
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
                            f.eval(env.clone())
                                .as_any()
                                .downcast_ref::<Str>()
                                .unwrap()
                                .value
                                .clone()
                        })
                        .unwrap()
                        .as_any()
                        .downcast_ref::<Attrs>()
                        .unwrap()
                        .env
                        .clone()
                });
                for i in b
                    .as_any()
                    .downcast_ref::<InheritExpr>()
                    .unwrap()
                    .inherits
                    .iter()
                {
                    let string = if i.as_any().is::<IdentifierExpr>() {
                        b.as_any()
                            .downcast_ref::<IdentifierExpr>()
                            .unwrap()
                            .ident
                            .clone()
                    } else {
                        // StringLiteralExpr
                        b.eval(env.clone())
                            .as_any()
                            .downcast_ref::<Str>()
                            .unwrap()
                            .value
                            .clone()
                    }
                    .clone();
                    newenv
                        .borrow_mut()
                        .set(
                            string.clone(),
                            EvaledOr::evaled((*env.borrow_mut()).get(&string).unwrap()),
                        )
                        .unwrap();
                }
            }
        }
        Rc::new(Attrs::new(newenv))
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
    // pub args: Vec<Box<dyn Expression>>,
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

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Rc<dyn Object> {
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

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Rc<dyn Object> {
        Rc::new(List::new(
            self.items.iter().map(|i| i.eval(env.clone())).collect(),
        ))
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
    bindings: Vec<Box<dyn Expression>>,
    expr: Box<dyn Expression>,
}

impl LetExpr {
    pub fn new(bindings: Vec<Box<dyn Expression>>, expr: Box<dyn Expression>) -> LetExpr {
        LetExpr { bindings, expr }
    }
}

impl Expression for LetExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Rc<dyn Object> {
        let newenv = Rc::new(RefCell::new(Environment::new(Some(env.clone()))));
        for b in self.bindings.iter() {
            if b.as_any().is::<BindingExpr>() {
                let b = b.as_any().downcast_ref::<BindingExpr>().unwrap();
                let env = newenv.clone();
                newenv
                    .borrow_mut()
                    .set(b.string(env.clone()), EvaledOr::expr(env, b.value.clone()))
                    .unwrap();
            } else {
                // InheritExpr
                let inherit = b.as_any().downcast_ref::<InheritExpr>().unwrap();
                let env = inherit.from.clone().map_or(env.clone(), |f| {
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
                            f.eval(env.clone())
                                .as_any()
                                .downcast_ref::<Str>()
                                .unwrap()
                                .value
                                .clone()
                        })
                        .unwrap()
                        .as_any()
                        .downcast_ref::<Attrs>()
                        .unwrap()
                        .env
                        .clone()
                });
                for i in b
                    .as_any()
                    .downcast_ref::<InheritExpr>()
                    .unwrap()
                    .inherits
                    .iter()
                {
                    let string = if i.as_any().is::<IdentifierExpr>() {
                        b.as_any()
                            .downcast_ref::<IdentifierExpr>()
                            .unwrap()
                            .ident
                            .clone()
                    } else {
                        // StringLiteralExpr
                        b.eval(env.clone())
                            .as_any()
                            .downcast_ref::<Str>()
                            .unwrap()
                            .value
                            .clone()
                    }
                    .clone();
                    newenv
                        .borrow_mut()
                        .set(
                            string.clone(),
                            EvaledOr::evaled((*env.borrow_mut()).get(&string).unwrap()),
                        )
                        .unwrap();
                }
            }
        }
        self.expr.eval(newenv)
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
    attrs: Box<dyn Expression>,
    expr: Box<dyn Expression>,
}

impl WithExpr {
    pub fn new(attrs: Box<dyn Expression>, expr: Box<dyn Expression>) -> WithExpr {
        WithExpr { attrs, expr }
    }
}

impl Expression for WithExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Rc<dyn Object> {
        let e = self.attrs.eval(env.clone());
        let newenv = convany!(e.as_any(), Attrs).env.clone();
        newenv.borrow_mut().father = Some(env);
        self.expr.eval(newenv)
    }
}

impl Display for WithExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "(with {}; {})", self.attrs, self.expr)
    }
}

#[derive(Debug)]
pub struct AssertExpr {
    assertion: Box<dyn Expression>,
    expr: Box<dyn Expression>,
}

impl AssertExpr {
    pub fn new(assertion: Box<dyn Expression>, expr: Box<dyn Expression>) -> AssertExpr {
        AssertExpr { assertion, expr }
    }
}

impl Expression for AssertExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Rc<dyn Object> {
        let assertion = self.assertion.eval(env.clone());
        let assertion = assertion.as_any();
        if assertion.type_id() == type_ids::BOOL {
            if *convany!(assertion, Bool) {
                self.expr.eval(env)
            } else {
                // FIXME: error handling
                panic!("assert {} failed", self.assertion)
            }
        } else {
            panic!()
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
    inherits: Vec<Box<dyn Expression>>,
    from: Option<Rc<dyn Expression>>,
}

impl InheritExpr {
    pub fn new(
        inherits: Vec<Box<dyn Expression>>,
        from: Option<Box<dyn Expression>>,
    ) -> InheritExpr {
        InheritExpr {
            inherits,
            from: from.map_or(None, |f| Some(Rc::from(f))),
        }
    }
}

impl Expression for InheritExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Rc<dyn Object> {
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
    literal: NixPath,
    relative: bool,
}

impl PathLiteralExpr {
    pub fn new(literal: NixPath, relative: bool) -> PathLiteralExpr {
        PathLiteralExpr { literal, relative }
    }
}

impl Expression for PathLiteralExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Rc<dyn Object> {
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
    path: Box<dyn Expression>,
}

impl SearchPathExpr {
    pub fn new(path: Box<dyn Expression>) -> SearchPathExpr {
        SearchPathExpr { path }
    }
}

impl Expression for SearchPathExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Rc<dyn Object> {
        todo!()
    }
}

impl Display for SearchPathExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "<{}>", self.path)
    }
}

#[derive(Debug)]
pub struct ThunkExpr {
    pub ident: Box<dyn Expression>,
}

impl ThunkExpr {
    pub fn new(ident: Box<dyn Expression>) -> ThunkExpr {
        ThunkExpr { ident }
    }
}

impl Expression for ThunkExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Rc<dyn Object> {
        todo!()
    }
}

impl Display for ThunkExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "${{{}}}", self.ident)
    }
}
