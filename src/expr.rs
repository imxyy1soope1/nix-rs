#![allow(dead_code)]

use std::any::Any;
use std::fmt::{Debug, Display};

use expr_macro::{display_fmt, Expression};

use crate::compile::Compile;

pub type Expr = Box<dyn Expression>;

pub(crate) trait Expression: Display + Debug + Compile + AsAny {
}

pub(crate) trait AsAny {
    fn as_any(&self) -> &dyn Any;
    fn as_any_mut(&mut self) -> &mut dyn Any;
    fn into_any(self: Box<Self>) -> Box<dyn Any>;
}

impl<T: 'static + Sized + Expression> AsAny for T {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }

    fn into_any(self: Box<Self>) -> Box<dyn Any> {
        self
    }
}

pub trait Downcast<T> {
    fn downcast_ref(&self) -> Option<&T>;
    fn downcast_mut(&mut self) -> Option<&mut T>;
    fn downcast(self) -> Option<Box<T>>;
}

impl<T: 'static> Downcast<T> for Box<dyn Expression> {
    fn downcast_ref(&self) -> Option<&T> {
        self.as_any().downcast_ref()
    }

    fn downcast_mut(&mut self) -> Option<&mut T> {
        self.as_any_mut().downcast_mut()
    }

    fn downcast(self) -> Option<Box<T>> {
        self.into_any().downcast().ok()
    }
}

#[derive(Debug, Expression)]
#[display_fmt("(!{})")]
pub struct OpNotExpr {
    pub(crate) right: Expr,
}

#[derive(Debug, Expression)]
#[display_fmt("(-{})")]
pub struct OpNegExpr {
    pub(crate) right: Expr,
}

macro_rules! bin_op {
    ($typename:ident, $s:expr) => {
        #[derive(Debug, Expression)]
        pub struct $typename {
            pub(crate) left: Expr,
            pub(crate) right: Expr,
        }

        impl Display for $typename {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, "({} {} {})", self.left, $s, self.right)
            }
        }
    };
}

bin_op! {OpAddExpr, "+"}
bin_op! {OpSubExpr, "-"}
bin_op! {OpMulExpr, "*"}
bin_op! {OpDivExpr, "/"}

bin_op! {OpAndExpr, "&&"}
bin_op! {OpOrExpr, "||"}
bin_op! {OpImplExpr, "->"}

bin_op! {OpEqExpr, "=="}
bin_op! {OpNeqExpr, "!="}
bin_op! {OpLtExpr, "<"}
bin_op! {OpGtExpr, ">"}
bin_op! {OpLeqExpr, "<="}
bin_op! {OpGeqExpr, ">="}

macro_rules! literal {
    ($name:ident, $ty:ty) => {
        #[derive(Debug, Expression)]
        #[display_fmt("{}")]
        pub struct $name {
            pub(crate) literal: $ty,
        }
    };
    ($name:ident, $ty:ty, $fmt:expr) => {
        #[derive(Debug, Expression)]
        #[display_fmt($fmt)]
        pub struct $name {
            pub(crate) literal: $ty,
        }
    };
}

literal! {IdentExpr, String}
literal! {IntExpr, i64}
literal! {FloatExpr, f64}
literal! {StringExpr, String, r#""{}""#}
literal! {PathExpr, String}
literal! {SearchPathExpr, String, "<{}>"}

#[derive(Debug)]
pub enum Arg {
    Arg(String),
    Formals {
        formals: Vec<(String, Option<Expr>)>,
        ellipsis: bool,
        alias: Option<String>,
    },
}

impl Display for Arg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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
                                Some(expr) => format!(" ? {expr}"),
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

macro_rules! expr {
    ($name:ident, $($attr:ident : $t:ty),*, $fmt:literal) => {
        #[derive(Debug, Expression)]
        #[display_fmt($fmt)]
        pub struct $name {
            $(
                pub(crate) $attr: $t,
            )*
        }
    };
    ($name:ident, $($attr:ident : $t:ty),*) => {
        #[derive(Debug, Expression)]
        pub struct $name {
            $(
                pub(crate) $attr: $t,
            )*
        }
    };
}

expr! {FuncExpr, arg: Arg, body: Expr, "({}: {})"}
expr! {CallExpr, func: Expr, args: Vec<Expr>}

impl Display for CallExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({} {})",
            self.func,
            self.args
                .iter()
                .map(|a| a.to_string())
                .collect::<Vec<_>>()
                .join(" ")
        )
    }
}

expr! {IfExpr, cond: Expr, consq: Expr, alter: Expr, "(if {} then {} else {})"}

expr! {AttrsExpr, bindings: Vec<(String, Expr)>, rec: bool}

impl Display for AttrsExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if self.rec {
            write!(f, "rec ")?;
        }
        write!(f, "{{")?;
        for binding in self.bindings.iter() {
            write!(f, "{} = {};", binding.0, binding.1)?;
        }
        write!(f, " }}")
    }
}

expr! {ListExpr, items: Vec<Expr>}

impl Display for ListExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "[ ")?;
        for item in self.items.iter() {
            write!(f, "{item} ")?;
        }
        write!(f, "]")
    }
}

expr! {LetExpr, attrs: AttrsExpr, expr: Expr}

impl Display for LetExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "(let ")?;
        for binding in self.attrs.bindings.iter() {
            write!(f, "{} = {}; ", binding.0, binding.1)?;
        }
        write!(f, "in {})", self.expr)
    }
}

expr! {WithExpr, attrs: Expr, expr: Expr, "(with {}; {})"}
expr! {AssertExpr, assertion: Expr, expr: Expr, "(assert {}; {})"}
