use std::collections::HashMap;

use crate::ast::{self, Expr};

use super::symtable::*;
use super::env::IrEnv;

pub enum Ir {
    Var(Sym),
    Const(Const),
    Attrs(Attrs),
    RecAttrs(RecAttrs),
    List(List),
    BinOp(Box<BinOp>),
    If(Box<If>),
    Let(Box<Let>),
    LetRec(Box<LetRec>),
    With(Box<With>),
    Assert(Box<Assert>),
    Func(Box<Func>),
    Call(Box<Call>),
}

pub enum Const {
    Int(i64),
    Float(f64),
    String(String),
}

pub fn desugar(expr: Expr) -> (Ir, SymTable) {
    let mut state = SymTable::new();
    (expr.desugar(&mut state), state)
}

enum Env {
    Env(IrEnv),
    With
}

impl Env {
    fn env(& self) -> & IrEnv {
        match self {
            Env::Env(env) => env,
            _ => panic!()
        }
    }

    fn env_mut(&mut self) -> &mut IrEnv {
        match self {
            Env::Env(env) => env,
            _ => panic!()
        }
    }
}

struct OptimizeState {
    env_stack: Vec<Env>,
}

impl OptimizeState {
    fn new() -> OptimizeState {
        OptimizeState {
            env_stack: Vec::new(),
        }
    }

    fn new_env(&mut self) {
        self.env_stack.push(Env::Env(IrEnv::new()));
    }

    fn with(&mut self) {
        self.env_stack.push(Env::With);
    }

    fn pop_env(&mut self) {
        self.env_stack.pop().unwrap();
    }

    fn insert_stc(&mut self, sym: Sym, val: Ir) {
        if let Some(_) = self.env_stack.last_mut().unwrap().env_mut().stcs.insert(sym, val) {
            panic!()
        }
    }

    fn insert_dyn(&mut self, sym: Ir, val: Ir) {
        self.env_stack
            .last_mut()
            .unwrap()
            .env_mut()
            .dyns
            .push((sym, val));
    }

    fn lookup(&self, sym: Sym) -> Option<&Ir> {
        let mut env_stack = self.env_stack.iter();
        while let Some(Env::Env(IrEnv { stcs, .. })) = env_stack.next_back() {
            if let Some(idx) = stcs.get(&sym) {
                return Some(idx);
            }
        }
        None
    }
}

pub fn optimize(ir: Ir) -> Ir {
    ir.optimize(&mut OptimizeState::new())
}

macro_rules! into_ir {
    ($id:ident) => {
        impl From<$id> for Ir {
            fn from(value: $id) -> Self {
                Ir::$id(Box::new(value))
            }
        }
    };
    (not_boxed $id:ident) => {
        impl From<$id> for Ir {
            fn from(value: $id) -> Self {
                Ir::$id(value)
            }
        }
    };
}

macro_rules! ir {
    ($name:ident, $($attr:ident : $t:ty),*) => {
        pub struct $name {
            $(
                pub $attr: $t,
            )*
        }
        into_ir!($name);
    };
    (not_boxed $name:ident, $($attr:ident : $t:ty),*) => {
        pub struct $name {
            $(
                pub $attr: $t,
            )*
        }
        into_ir!(not_boxed $name);
    };
}

ir! {not_boxed Attrs, stcs: HashMap<Sym, Ir>, dyns: Vec<(Ir, Ir)>}
ir! {not_boxed RecAttrs, stcs: HashMap<Sym, Ir>, dyns: Vec<(Ir, Ir)>}
ir! {not_boxed List, items: Vec<Ir>}

ir! {BinOp, lhs: Ir, rhs: Ir, kind: BinOpKind}

pub enum BinOpKind {
    Add,
    Sub,
    Div,
    Mul,
    Eq,
    Neq,
    Lt,
    Gt,
    Leq,
    Geq,
    And,
    Or,
    Impl,
}

impl From<ast::ArithOp> for BinOpKind {
    fn from(op: ast::ArithOp) -> Self {
        match op {
            ast::ArithOp::Add => BinOpKind::Add,
            ast::ArithOp::Sub => BinOpKind::Sub,
            ast::ArithOp::Mul => BinOpKind::Mul,
            ast::ArithOp::Div => BinOpKind::Div,
        }
    }
}

impl From<ast::CmpOp> for BinOpKind {
    fn from(op: ast::CmpOp) -> Self {
        match op {
            ast::CmpOp::Eq => BinOpKind::Eq,
            ast::CmpOp::Neq => BinOpKind::Neq,
            ast::CmpOp::Lt => BinOpKind::Lt,
            ast::CmpOp::Gt => BinOpKind::Gt,
            ast::CmpOp::Leq => BinOpKind::Leq,
            ast::CmpOp::Geq => BinOpKind::Geq,
        }
    }
}

impl From<ast::BoolOp> for BinOpKind {
    fn from(op: ast::BoolOp) -> Self {
        match op {
            ast::BoolOp::And => BinOpKind::And,
            ast::BoolOp::Or => BinOpKind::Or,
            ast::BoolOp::Impl => BinOpKind::Impl,
        }
    }
}

ir! {If, cond: Ir, consq: Ir, alter: Ir}

ir! {Func, arg: Arg, body: Ir}

pub enum Arg {
    Arg(Sym),
    Formals {
        formals: Vec<(Sym, Option<Ir>)>,
        ellipsis: bool,
        alias: Option<Sym>,
    },
}

ir! {Call, func: Ir, arg: Ir}

ir! {LetRec, attrs: RecAttrs, expr: Ir}
ir! {Let, attrs: Attrs, expr: Ir}
ir! {With, attrs: Ir, expr: Ir}
ir! {Assert, assertion: Ir, expr: Ir}

trait Sugar {
    fn desugar(self, state: &mut SymTable) -> Ir;
}

trait Optimize {
    fn optimize(self, state: &mut OptimizeState) -> Ir;
}

impl Sugar for Expr {
    fn desugar(self, state: &mut SymTable) -> Ir {
        match self {
            Expr::Var(v) => Ir::Var(state.lookup(v.into())),
            Expr::Literal(ast::Literal::Int(i)) => Ir::Const(Const::Int(i)),
            Expr::Literal(ast::Literal::Float(f)) => Ir::Const(Const::Float(f)),
            Expr::Literal(ast::Literal::String(s)) => Ir::Const(Const::String(s)),
            Expr::Attrs(attrs) => attrs.desugar(state),
            Expr::List(list) => list.desugar(state),
            Expr::ArithBinOp(op) => op.desugar(state),
            Expr::CmpBinOp(op) => op.desugar(state),
            Expr::BoolBinOp(op) => op.desugar(state),
            Expr::If(if_) => If {
                cond: if_.cond.desugar(state),
                consq: if_.consq.desugar(state),
                alter: if_.alter.desugar(state),
            }
            .into(),
            Expr::Let(let_) => let_.desugar(state),
            Expr::With(with) => With {
                attrs: with.attrs.desugar(state),
                expr: with.expr.desugar(state),
            }
            .into(),
            Expr::Assert(assert_) => Assert {
                assertion: assert_.assertion.desugar(state),
                expr: assert_.expr.desugar(state),
            }
            .into(),
            Expr::Func(func) => func.desugar(state),
            Expr::Call(call) => Call {
                func: call.func.desugar(state),
                arg: call.arg.desugar(state),
            }
            .into(),
        }
    }
}

impl Optimize for Ir {
    fn optimize(self, state: &mut OptimizeState) -> Ir {
        use Ir::*;
        match self {
            Attrs(attrs) => attrs.optimize(state),
            RecAttrs(attrs) => attrs.optimize(state),
            BinOp(op) => op.optimize(state),
            If(if_) => if_.optimize(state),
            LetRec(let_) => let_.optimize(state),
            With(with) => with.optimize(state),
            Assert(assert) => assert.optimize(state),
            Func(func) => func.optimize(state),
            Call(call) => call.optimize(state),
            ir => ir,
        }
    }
}

impl Sugar for ast::Attrs {
    fn desugar(self, state: &mut SymTable) -> Ir {
        let stcs = self
            .stcs
            .into_iter()
            .map(|(ident, expr)| (state.lookup(ident.into()), expr.desugar(state)))
            .collect();
        let dyns = self
            .dyns
            .into_iter()
            .map(|(name, expr)| (name.desugar(state), expr.desugar(state)))
            .collect();
        if self.rec {
            RecAttrs { stcs, dyns }.into()
        } else {
            Attrs { stcs, dyns }.into()
        }
    }
}

impl Optimize for Attrs {
    fn optimize(self, state: &mut OptimizeState) -> Ir {
        self.into()
    }
}

impl Optimize for RecAttrs {
    fn optimize(self, state: &mut OptimizeState) -> Ir {
        self.into()
    }
}

impl Sugar for ast::List {
    fn desugar(self, state: &mut SymTable) -> Ir {
        let items = self
            .items
            .into_iter()
            .map(|expr| expr.desugar(state))
            .collect();
        List { items }.into()
    }
}

impl Optimize for List {
    fn optimize(self, state: &mut OptimizeState) -> Ir {
        self.into()
    }
}

impl<Op> Sugar for ast::BinOp<Op>
where
    BinOpKind: From<Op>,
    Op: Copy,
{
    fn desugar(self, state: &mut SymTable) -> Ir {
        BinOp {
            lhs: self.lhs.desugar(state),
            rhs: self.rhs.desugar(state),
            kind: BinOpKind::from(self.kind),
        }
        .into()
    }
}

impl Optimize for BinOp {
    fn optimize(self, state: &mut OptimizeState) -> Ir {
        self.into()
    }
}

impl Sugar for ast::Let {
    fn desugar(self, state: &mut SymTable) -> Ir {
        if !self.attrs.rec {
            panic!()
        }
        let stcs = self
            .attrs
            .stcs
            .into_iter()
            .map(|(ident, expr)| (state.lookup(ident.into()), expr.desugar(state)))
            .collect();
        let dyns = self
            .attrs
            .dyns
            .into_iter()
            .map(|(name, expr)| (name.desugar(state), expr.desugar(state)))
            .collect();
        LetRec {
            attrs: RecAttrs { stcs, dyns },
            expr: self.expr.desugar(state),
        }
        .into()
    }
}

impl Optimize for LetRec {
    fn optimize(self, state: &mut OptimizeState) -> Ir {
        self.into()
    }
}

impl Sugar for ast::Func {
    fn desugar(self, state: &mut SymTable) -> Ir {
        let arg = match self.arg {
            ast::Arg::Arg(name) => Arg::Arg(state.lookup(name.into())),
            ast::Arg::Formals {
                formals,
                ellipsis,
                alias,
            } => {
                let formals = formals
                    .into_iter()
                    .map(|(ident, default)| {
                        (
                            state.lookup(ident.into()),
                            default.map(|default| default.desugar(state)),
                        )
                    })
                    .collect();
                Arg::Formals {
                    formals,
                    alias: alias.map(|alias| state.lookup(alias.into())),
                    ellipsis,
                }
            }
        };
        Func {
            arg,
            body: self.body.desugar(state),
        }
        .into()
    }
}

impl Optimize for Func {
    fn optimize(self, state: &mut OptimizeState) -> Ir {
        self.into()
    }
}

impl Optimize for Call {
    fn optimize(self, state: &mut OptimizeState) -> Ir {
        self.into()
    }
}

impl Optimize for If {
    fn optimize(self, state: &mut OptimizeState) -> Ir {
        If {
            cond: self.cond.optimize(state),
            consq: self.consq.optimize(state),
            alter: self.alter.optimize(state),
        }.into()
    }
}

impl Optimize for With {
    fn optimize(self, state: &mut OptimizeState) -> Ir {
        if let Ir::Attrs(attrs) = self.attrs {
            let Ir::Attrs(attrs) = attrs.optimize(state) else { unreachable!() };
            Let { attrs, expr: self.expr.optimize(state) }.into()
        } else {
            With { attrs: self.attrs.optimize(state), expr: self.expr.optimize(state) }.into()
        }
    }
}

impl Optimize for Assert {
    fn optimize(self, state: &mut OptimizeState) -> Ir {
        self.into()
    }
}
