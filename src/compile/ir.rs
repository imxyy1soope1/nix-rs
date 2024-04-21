use std::collections::HashMap;

use rnix::ast::{self, Expr};

// use crate::ast::{self, Expr};
use crate::vm::program::{Idx, Const};

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

pub struct IrGenState {
    sym_table: SymTable,
    envs: Vec<Env>,
    thunks: Vec<(Ir, Vec<Sym>)>,
    consts: HashMap<Const, Idx>,
}

pub struct GenedIr {
    top_level: Ir,

}

impl IrGenState {
    fn new() -> IrGenState {
        IrGenState {
            sym_table: SymTable::new(),
            envs: Vec::new(),
            thunks: Vec::new(),
            consts: HashMap::new()
        }
    }

    fn new_env(&mut self) {
        self.envs.push(Env::Env(IrEnv::new()));
    }

    fn with(&mut self) {
        self.envs.push(Env::With);
    }

    fn pop_env(&mut self) {
        self.envs.pop().unwrap();
    }

    fn insert_stc(&mut self, sym: Sym, val: Ir) {
        if let Some(_) = self.envs.last_mut().unwrap().env_mut().stcs.insert(sym, val) {
            panic!()
        }
    }

    fn insert_dyn(&mut self, sym: Ir, val: Ir) {
        self.envs
            .last_mut()
            .unwrap()
            .env_mut()
            .dyns
            .push((sym, val));
    }

    fn lookup(&self, sym: Sym) -> Option<&Ir> {
        let mut envs = self.envs.iter();
        while let Some(Env::Env(IrEnv { stcs, .. })) = envs.next_back() {
            if let Some(idx) = stcs.get(&sym) {
                return Some(idx);
            }
        }
        None
    }

    fn sym_lookup(&mut self, name: String) -> Sym {
        self.sym_table.lookup(name)
    }
}

pub fn desugar(expr: Expr) -> (Ir, IrGenState) {
    let mut state = IrGenState::new();
    (expr.desugar(&mut state), state)
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
    fn desugar(self, state: &mut IrGenState) -> Ir;
}

impl Sugar for Expr {
    fn desugar(self, state: &mut IrGenState) -> Ir {
        match self {
            Expr::Var(v) => Ir::Var(state.sym_lookup(v.into())),
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

impl Sugar for ast::Attrs {
    fn desugar(self, state: &mut IrGenState) -> Ir {
        let stcs = self
            .stcs
            .into_iter()
            .map(|(ident, expr)| (state.sym_lookup(ident.into()), expr.desugar(state)))
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

impl Sugar for ast::List {
    fn desugar(self, state: &mut IrGenState) -> Ir {
        let items = self
            .items
            .into_iter()
            .map(|expr| expr.desugar(state))
            .collect();
        List { items }.into()
    }
}

impl<Op> Sugar for ast::BinOp<Op>
where
    BinOpKind: From<Op>,
    Op: Copy,
{
    fn desugar(self, state: &mut IrGenState) -> Ir {
        BinOp {
            lhs: self.lhs.desugar(state),
            rhs: self.rhs.desugar(state),
            kind: BinOpKind::from(self.kind),
        }
        .into()
    }
}


impl Sugar for ast::Let {
    fn desugar(self, state: &mut IrGenState) -> Ir {
        if !self.attrs.rec {
            panic!()
        }
        let stcs = self
            .attrs
            .stcs
            .into_iter()
            .map(|(ident, expr)| (state.sym_lookup(ident.into()), expr.desugar(state)))
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

impl Sugar for ast::Lambda {
    fn desugar(self, state: &mut IrGenState) -> Ir {
        let param = match self.param().unwrap() {
            ast::Param::IdentParam(ident) => ident.ident().unwrap(),
            ast::Param::Pattern(pattern) => pattern.
        }
        let arg = match self.arg {
            ast::Arg::Arg(name) => Arg::Arg(state.sym_lookup(name.into())),
            ast::Arg::Formals {
                formals,
                ellipsis,
                alias,
            } => {
                let formals = formals
                    .into_iter()
                    .map(|(ident, default)| {
                        (
                            state.sym_lookup(ident.into()),
                            default.map(|default| default.desugar(state)),
                        )
                    })
                    .collect();
                Arg::Formals {
                    formals,
                    alias: alias.map(|alias| state.sym_lookup(alias.into())),
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

