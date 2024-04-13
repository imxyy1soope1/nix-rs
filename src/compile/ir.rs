use crate::ast::{self, Expr};

use super::symtable::*;

pub enum Ir {
    Var(Sym),
    Const(Const),
    Attrs(Attrs),
    RecAttrs(RecAttrs),
    List(List),
    BinOp(Box<BinOp>),
    If(Box<If>),
    Let(Box<Let>),
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
    let mut table = SymTable::new();
    (expr.desugar(&mut table), table)
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

ir! {not_boxed Attrs, stcs: Vec<(Sym, Ir)>, dyns: Vec<(Ir, Ir)>}
ir! {not_boxed RecAttrs, stcs: Vec<(Sym, Ir)>, dyns: Vec<(Ir, Ir)>}
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

ir! {Let, attrs: RecAttrs, expr: Ir}
ir! {With, attrs: Ir, expr: Ir}
ir! {Assert, assertion: Ir, expr: Ir}

trait Sugar {
    fn desugar(self, _: &mut SymTable) -> Ir;
}

impl Sugar for Expr {
    fn desugar(self, table: &mut SymTable) -> Ir {
        match self {
            Expr::Var(v) => Ir::Var(table.lookup(v.into())),
            Expr::Literal(ast::Literal::Int(i)) => Ir::Const(Const::Int(i)),
            Expr::Literal(ast::Literal::Float(f)) => Ir::Const(Const::Float(f)),
            Expr::Literal(ast::Literal::String(s)) => Ir::Const(Const::String(s)),
            Expr::Attrs(attrs) => attrs.desugar(table),
            Expr::List(list) => list.desugar(table),
            Expr::ArithBinOp(op) => op.desugar(table),
            Expr::CmpBinOp(op) => op.desugar(table),
            Expr::BoolBinOp(op) => op.desugar(table),
            Expr::If(if_) => If {
                cond: if_.cond.desugar(table),
                consq: if_.consq.desugar(table),
                alter: if_.alter.desugar(table),
            }
            .into(),
            Expr::Let(let_) => let_.desugar(table),
            Expr::With(with) => With {
                attrs: with.attrs.desugar(table),
                expr: with.expr.desugar(table),
            }
            .into(),
            Expr::Assert(assert_) => Assert {
                assertion: assert_.assertion.desugar(table),
                expr: assert_.expr.desugar(table),
            }
            .into(),
            Expr::Func(func) => func.desugar(table),
            Expr::Call(call) => Call {
                func: call.func.desugar(table),
                arg: call.arg.desugar(table),
            }
            .into(),
        }
    }
}

impl Sugar for ast::Attrs {
    fn desugar(self, table: &mut SymTable) -> Ir {
        let stcs = self
            .stcs
            .into_iter()
            .map(|(ident, expr)| (table.lookup(ident.into()), expr.desugar(table)))
            .collect();
        let dyns = self
            .dyns
            .into_iter()
            .map(|(name, expr)| (name.desugar(table), expr.desugar(table)))
            .collect();
        if self.rec {
            RecAttrs { stcs, dyns }.into()
        } else {
            Attrs { stcs, dyns }.into()
        }
    }
}

impl Sugar for ast::List {
    fn desugar(self, table: &mut SymTable) -> Ir {
        let items = self
            .items
            .into_iter()
            .map(|expr| expr.desugar(table))
            .collect();
        List { items }.into()
    }
}

impl<Op> Sugar for ast::BinOp<Op>
where
    BinOpKind: From<Op>,
    Op: Copy,
{
    fn desugar(self, table: &mut SymTable) -> Ir {
        BinOp {
            lhs: self.lhs.desugar(table),
            rhs: self.rhs.desugar(table),
            kind: BinOpKind::from(self.kind),
        }
        .into()
    }
}

impl Sugar for ast::Let {
    fn desugar(self, table: &mut SymTable) -> Ir {
        if !self.attrs.rec {
            panic!()
        }
        let stcs = self
            .attrs
            .stcs
            .into_iter()
            .map(|(ident, expr)| (table.lookup(ident.into()), expr.desugar(table)))
            .collect();
        let dyns = self
            .attrs
            .dyns
            .into_iter()
            .map(|(name, expr)| (name.desugar(table), expr.desugar(table)))
            .collect();
        Let {
            attrs: RecAttrs { stcs, dyns }.into(),
            expr: self.expr.desugar(table),
        }
        .into()
    }
}

impl Sugar for ast::Func {
    fn desugar(self, table: &mut SymTable) -> Ir {
        let arg = match self.arg {
            ast::Arg::Arg(name) => Arg::Arg(table.lookup(name.into())),
            ast::Arg::Formals {
                formals,
                ellipsis,
                alias,
            } => {
                let formals = formals
                    .into_iter()
                    .map(|(ident, default)| {
                        (
                            table.lookup(ident.into()),
                            default.map(|default| default.desugar(table)),
                        )
                    })
                    .collect();
                Arg::Formals {
                    formals,
                    alias: alias.map(|alias| table.lookup(alias.into())),
                    ellipsis,
                }
            }
        };
        Func {
            arg,
            body: self.body.desugar(table),
        }
        .into()
    }
}
