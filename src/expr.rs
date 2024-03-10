use std::any::Any;
use std::fmt::{Debug, Display};
use std::rc::Rc;

use expr_macro::Expression;

pub type Expr = Box<dyn Expression>;

pub trait Expression: Display + Debug {
    fn as_any(&self) -> &dyn Any;
    fn into_any(self: Box<Self>) -> Box<dyn Any>;
}

#[derive(Debug, Expression)]
pub struct OpNotExpr {
    right: Expr,
}

impl Display for OpNotExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(!{})", self.right)
    }
}

#[derive(Debug, Expression)]
pub struct OpNegExpr {
    right: Expr,
}

impl Display for OpNegExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(-{})", self.right)
    }
}

macro_rules! bin_num_op {
    ($typename:ident, $s:expr, $op:tt) => {
        #[derive(Debug, Expression)]
        pub struct $typename {
            left: Expr,
            right: Expr,
        }

        impl Display for $typename {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, "({} {} {})", self.left, stringify!($op), self.right)
            }
        }
    };
}

bin_num_op! {OpAddExpr, "ADD (+)", +}
bin_num_op! {OpSubExpr, "SUB (-)", -}
bin_num_op! {OpMulExpr, "MUL (*)", *}
bin_num_op! {OpDivExpr, "DIV (/)", /}

macro_rules! bin_bool_op {
    ($typename:ident, $s:expr, $func:expr, $op:expr) => {
        #[derive(Debug, Expression)]
        pub struct $typename {
            left: Expr,
            right: Expr,
        }

        impl Display for $typename {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, "({} {} {})", self.left, $op, self.right)
            }
        }
    };
}

bin_bool_op! {OpAndExpr, "AND (&&)", |a, b| a && b, "&&"}
bin_bool_op! {OpOrExpr, "OR (||)", |a, b| a || b, "||"}
bin_bool_op! {OpImplExpr, "IMPL (->)", |a: bool, b| !a || b, "->"}

#[derive(Debug, Expression)]
pub struct OpEqExpr {
    left: Expr,
    right: Expr,
}

impl Display for OpEqExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "({} == {})", self.left, self.right)
    }
}

#[derive(Debug, Expression)]
pub struct OpNeqExpr {
    left: Expr,
    right: Expr,
}

impl Display for OpNeqExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "({} == {})", self.left, self.right)
    }
}

#[derive(Debug, Expression)]
pub struct OpLtExpr {
    left: Expr,
    right: Expr,
}

impl Display for OpLtExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "({} < {})", self.left, self.right)
    }
}

#[derive(Debug, Expression)]
pub struct OpGtExpr {
    left: Expr,
    right: Expr,
}

impl Display for OpGtExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "({} > {})", self.left, self.right)
    }
}

#[derive(Debug, Expression)]
pub struct OpLeqExpr {
    left: Expr,
    right: Expr,
}

impl Display for OpLeqExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "({} <= {})", self.left, self.right)
    }
}

#[derive(Debug, Expression)]
pub struct OpGeqExpr {
    left: Expr,
    right: Expr,
}

impl Display for OpGeqExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "({} >= {})", self.left, self.right)
    }
}

#[derive(Debug, Expression)]
pub struct IdentifierExpr {
    pub ident: String,
}

impl Display for IdentifierExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.ident)
    }
}

#[derive(Debug, Expression)]
pub struct IntLiteralExpr {
    literal: i64,
}

impl Display for IntLiteralExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.literal)
    }
}

#[derive(Debug, Expression)]
pub struct FloatLiteralExpr {
    literal: f64,
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

    fn into_any(self: Box<Self>) -> Box<dyn Any> {
        self
    }
}

impl Display for EllipsisLiteralExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "...")
    }
}

#[derive(Debug, Expression)]
pub struct StringLiteralExpr {
    pub literal: String,
}

impl Display for StringLiteralExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, r#""{}""#, self.literal)
    }
}

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
pub struct FunctionLiteralExpr {
    arg: Arg,
    body: Rc<dyn Expression>,
}

impl Display for FunctionLiteralExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "({}: {})", self.arg, self.body)
    }
}

#[derive(Debug, Expression)]
pub struct FunctionCallExpr {
    func: Expr,
    arg: Expr,
}

impl Display for FunctionCallExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "({} {})", self.func, self.arg)
    }
}

#[derive(Debug, Expression)]
pub struct IfExpr {
    cond: Expr,
    consq: Expr,
    alter: Expr,
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
pub struct WithExpr {
    attrs: Expr,
    expr: Expr,
}

impl Display for WithExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "(with {}; {})", self.attrs, self.expr)
    }
}

#[derive(Debug, Expression)]
pub struct AssertExpr {
    assertion: Expr,
    expr: Expr,
}

impl Display for AssertExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "(assert {}; {})", self.assertion, self.expr)
    }
}

#[derive(Debug, Expression)]
pub struct PathLiteralExpr {
    literal: String,
}

impl Display for PathLiteralExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.literal)
    }
}

#[derive(Debug, Expression)]
pub struct SearchPathExpr {
    path: Expr,
}

impl Display for SearchPathExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "<{}>", self.path)
    }
}

