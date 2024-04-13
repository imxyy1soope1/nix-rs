#![allow(dead_code)]

use std::error::Error;
use std::fmt::{Debug, Display, Formatter, Result as FmtResult};
use std::str::FromStr;

use macros::debug_fmt;

#[derive(PartialEq, Eq, Hash)]
pub struct Ident(String);

impl FromStr for Ident {
    type Err = Box<dyn Error>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Ident(s.to_owned()))
    }
}

impl From<Ident> for String {
    fn from(value: Ident) -> Self {
        value.0
    }
}

impl AsRef<str> for Ident {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        <String as Display>::fmt(&self.0, f)
    }
}

impl Debug for Ident {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        Display::fmt(self, f)
    }
}

pub enum Expr {
    Var(Ident),
    Literal(Literal),
    Attrs(Attrs),
    List(List),
    ArithBinOp(Box<ArithBinOp>),
    CmpBinOp(Box<CmpBinOp>),
    BoolBinOp(Box<BoolBinOp>),
    If(Box<If>),
    Let(Box<Let>),
    With(Box<With>),
    Assert(Box<Assert>),
    Func(Box<Func>),
    Call(Box<Call>),
}

macro_rules! into_expr {
    ($id:ident) => {
        impl From<$id> for Expr {
            fn from(value: $id) -> Self {
                Expr::$id(Box::new(value))
            }
        }
    };
    (not_boxed $id:ident) => {
        impl From<$id> for Expr {
            fn from(value: $id) -> Self {
                Expr::$id(value)
            }
        }
    };
}

impl Debug for Expr {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        use Expr::*;
        match self {
            Var(s) => f.write_str(s.as_ref()),
            Literal(x) => x.fmt(f),
            Attrs(x) => x.fmt(f),
            List(x) => x.fmt(f),
            ArithBinOp(x) => x.fmt(f),
            CmpBinOp(x) => x.fmt(f),
            BoolBinOp(x) => x.fmt(f),
            If(x) => x.fmt(f),
            Let(x) => x.fmt(f),
            With(x) => x.fmt(f),
            Assert(x) => x.fmt(f),
            Func(x) => x.fmt(f),
            Call(x) => x.fmt(f),
        }
    }
}

macro_rules! expr {
    ($name:ident, $($attr:ident : $t:ty),*, $fmt:literal) => {
        #[debug_fmt($fmt)]
        pub struct $name {
            $(
                pub $attr: $t,
            )*
        }
        into_expr!($name);
    };
    ($name:ident, $($attr:ident : $t:ty),*) => {
        pub struct $name {
            $(
                pub $attr: $t,
            )*
        }
        into_expr!($name);
    };
    (not_boxed $name:ident, $($attr:ident : $t:ty),*, $fmt:literal) => {
        #[debug_fmt($fmt)]
        pub struct $name {
            $(
                pub $attr: $t,
            )*
        }
        into_expr!(not_boxed $name);
    };
    (not_boxed $name:ident, $($attr:ident : $t:ty),*) => {
        pub struct $name {
            $(
                pub $attr: $t,
            )*
        }
        into_expr!(not_boxed $name);
    };
}

pub struct BinOp<T> {
    pub kind: T,
    pub lhs: Expr,
    pub rhs: Expr,
}

impl<T: Debug> Debug for BinOp<T> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "({:?} {:?} {:?})", self.kind, self.lhs, self.rhs)
    }
}

#[derive(Clone, Copy)]
pub enum ArithOp {
    Mul,
    Div,
    Add,
    Sub,
}

impl Debug for ArithOp {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        use self::ArithOp::*;
        f.write_str(match *self {
            Mul => "*",
            Div => "/",
            Add => "+",
            Sub => "-",
        })
    }
}

pub type ArithBinOp = BinOp<ArithOp>;

into_expr!(ArithBinOp);

#[derive(Clone, Copy)]
pub enum CmpOp {
    Eq,
    Neq,
    Lt,
    Gt,
    Leq,
    Geq,
}

impl Debug for CmpOp {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        use self::CmpOp::*;
        f.write_str(match *self {
            Eq => "==",
            Neq => "!=",
            Lt => "<",
            Gt => ">",
            Leq => "<=",
            Geq => ">=",
        })
    }
}

pub type CmpBinOp = BinOp<CmpOp>;

into_expr!(CmpBinOp);

#[derive(Clone, Copy)]
pub enum BoolOp {
    And,
    Or,
    Impl,
}

impl Debug for BoolOp {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        use self::BoolOp::*;
        f.write_str(match *self {
            And => "&&",
            Or => "||",
            Impl => "->",
        })
    }
}

pub type BoolBinOp = BinOp<BoolOp>;

into_expr!(BoolBinOp);

pub enum Arg {
    Arg(Ident),
    Formals {
        formals: Vec<(Ident, Option<Expr>)>,
        ellipsis: bool,
        alias: Option<Ident>,
    },
}

impl Debug for Arg {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            Arg::Arg(expr) => write!(f, "{expr}"),
            Arg::Formals {
                formals,
                ellipsis,
                alias,
            } => {
                write!(
                    f,
                    "{{ {}{}}}{}",
                    formals
                        .iter()
                        .map(|(formal, default)| format!(
                            "{formal}{}",
                            match default {
                                Some(expr) => format!(" ? {expr:?}"),
                                None => "".to_string(),
                            }
                        ))
                        .collect::<Vec<_>>()
                        .join(", "),
                    if *ellipsis {
                        if formals.is_empty() {
                            "... "
                        } else {
                            ", ... "
                        }
                    } else {
                        if formals.is_empty() {
                            ""
                        } else {
                            " "
                        }
                    },
                    match alias {
                        Some(alias) => format!(" ? {alias}"),
                        None => "".to_string(),
                    }
                )
            }
        }
    }
}

expr! {Func, arg: Arg, body: Expr, name: Option<String>, "({arg:?}: {body:?})"}
expr! {Call, func: Expr, arg: Expr, "({func:?} {arg:?})"}

pub enum Literal {
    Int(i64),
    Float(f64),
    String(String),
}

impl From<Literal> for Expr {
    fn from(val: Literal) -> Self {
        Expr::Literal(val)
    }
}

impl Debug for Literal {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        use Literal::*;
        match self {
            Int(x) => Debug::fmt(x, f),
            Float(x) => Debug::fmt(x, f),
            String(x) => Debug::fmt(x, f),
        }
    }
}

expr! {If, cond: Expr, consq: Expr, alter: Expr, "(if {cond:?} then {consq:?} else {alter:?})"}

expr! {not_boxed Attrs, stcs: Vec<(Ident, Expr)>, dyns: Vec<(Expr, Expr)>, rec: bool}

impl Display for Attrs {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        if self.rec {
            write!(f, "rec ")?;
        }
        write!(f, "{{")?;
        for binding in self.stcs.iter() {
            write!(f, "{:?} = {:?};", binding.0, binding.1)?;
        }
        for binding in self.dyns.iter() {
            write!(f, "{:?} = {:?};", binding.0, binding.1)?;
        }
        write!(f, " }}")
    }
}

expr! {not_boxed List, items: Vec<Expr>}

impl Display for List {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "[ ")?;
        for item in self.items.iter() {
            write!(f, "{item:?} ")?;
        }
        write!(f, "]")
    }
}

expr! {Let, attrs: Attrs, expr: Expr}

impl Debug for Let {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "(let ")?;
        for binding in self.attrs.stcs.iter() {
            write!(f, "{:?} = {:?}; ", binding.0, binding.1)?;
        }
        for binding in self.attrs.dyns.iter() {
            write!(f, "{:?} = {:?}; ", binding.0, binding.1)?;
        }
        write!(f, "in {:?})", self.expr)
    }
}

expr! {With, attrs: Expr, expr: Expr, "(with {attrs:?}; {expr:?})"}
expr! {Assert, assertion: Expr, expr: Expr, "(assert {assertion:?}; {expr:?})"}
