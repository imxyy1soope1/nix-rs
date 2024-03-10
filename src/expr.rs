use std::any::Any;
use std::fmt::{Debug, Display};
use std::rc::Rc;

use expr_macro::{Expression, display_fmt};

pub type Expr = Box<dyn Expression>;

pub trait Expression: Display + Debug {
    fn as_any(&self) -> &dyn Any;
    fn into_any(self: Box<Self>) -> Box<dyn Any>;
}

#[derive(Debug, Expression)]
#[display_fmt("(!{})")]
pub struct OpNotExpr {
    right: Expr,
}

#[derive(Debug, Expression)]
#[display_fmt("(-{})")]
pub struct OpNegExpr {
    right: Expr,
}

macro_rules! bin_op {
    ($typename:ident, $s:expr) => {
        #[derive(Debug, Expression)]
        pub struct $typename {
            left: Expr,
            right: Expr,
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
            literal: $ty
        }
    };
    ($name:ident, $ty:ty, $fmt:expr) => {
        #[derive(Debug, Expression)]
        #[display_fmt($fmt)]
        pub struct $name {
            literal: $ty
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

#[derive(Debug, Expression)]
#[display_fmt("({}: {})")]
pub struct FunctionLiteralExpr {
    arg: Arg,
    body: Rc<dyn Expression>,
}

#[derive(Debug, Expression)]
#[display_fmt("({} {})")]
pub struct FunctionCallExpr {
    func: Expr,
    arg: Expr,
}

#[derive(Debug, Expression)]
#[display_fmt("if {} then {} else {}")]
pub struct IfExpr {
    cond: Expr,
    consq: Expr,
    alter: Expr,
}

#[derive(Debug, Expression)]
pub struct AttrsLiteralExpr {
    bindings: Vec<Expr>,
    rec: bool,
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

#[derive(Debug, Expression)]
pub struct ListLiteralExpr {
    items: Vec<Expr>,
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

#[derive(Debug, Expression)]
pub struct LetExpr {
    bindings: Vec<Expr>,
    expr: Expr,
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

#[derive(Debug, Expression)]
#[display_fmt("(with {}; {})")]
pub struct WithExpr {
    attrs: Expr,
    expr: Expr,
}

#[derive(Debug, Expression)]
#[display_fmt("(assert {}; {})")]
pub struct AssertExpr {
    assertion: Expr,
    expr: Expr,
}

