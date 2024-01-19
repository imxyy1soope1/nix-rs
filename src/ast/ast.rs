use super::types::*;
use crate::token::Token;
use std::any::Any;
use std::fmt::Display;

pub trait Expression: Display + Any {}

pub struct PrefixExpr {
    token: Token,
    right: Box<dyn Expression>,
}

impl PrefixExpr {
    pub fn new(token: Token, right: Box<dyn Expression>) -> PrefixExpr {
        PrefixExpr { token, right }
    }
}

impl Expression for PrefixExpr {}

impl Display for PrefixExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}{})", self.token, self.right)
    }
}

pub struct InfixExpr {
    token: Token,
    left: Box<dyn Expression>,
    right: Box<dyn Expression>,
}

impl InfixExpr {
    pub fn new(token: Token, left: Box<dyn Expression>, right: Box<dyn Expression>) -> InfixExpr {
        InfixExpr { token, left, right }
    }
}

impl Expression for InfixExpr {}

impl Display for InfixExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {} {})", self.left, self.token, self.right)
    }
}

pub struct IdentifierExpr {
    ident: String,
}

impl IdentifierExpr {
    pub fn new(ident: &str) -> IdentifierExpr {
        IdentifierExpr {
            ident: ident.to_string(),
        }
    }
}

impl Expression for IdentifierExpr {}

impl Display for IdentifierExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ident)
    }
}

pub struct IntLiteralExpr {
    literal: NixInt,
}

impl IntLiteralExpr {
    pub fn new(s: &str) -> IntLiteralExpr {
        IntLiteralExpr {
            literal: s.parse().unwrap(),
        }
    }
}

impl Expression for IntLiteralExpr {}

impl Display for IntLiteralExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.literal)
    }
}

pub struct FloatLiteralExpr {
    literal: NixFloat,
}

impl FloatLiteralExpr {
    pub fn new(s: &str) -> FloatLiteralExpr {
        FloatLiteralExpr {
            literal: s.parse().unwrap(),
        }
    }
}

impl Expression for FloatLiteralExpr {}

impl Display for FloatLiteralExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.literal)
    }
}

pub struct BoolLiteralExpr {
    literal: NixBool,
}

impl BoolLiteralExpr {
    pub fn new(v: bool) -> BoolLiteralExpr {
        BoolLiteralExpr { literal: v }
    }
}

impl Expression for BoolLiteralExpr {}

impl Display for BoolLiteralExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.literal)
    }
}

pub struct NullLiteralExpr;

impl Expression for NullLiteralExpr {}

impl Display for NullLiteralExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "null")
    }
}

pub struct StringLiteralExpr {
    literal: NixString,
}

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

impl Expression for IfExpr {}

impl Display for IfExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "if {} then {} else {}",
            self.cond, self.consq, self.alter
        )
    }
}

impl StringLiteralExpr {
    pub fn new(s: &str) -> StringLiteralExpr {
        StringLiteralExpr {
            literal: s.to_string(),
        }
    }
}

impl Expression for StringLiteralExpr {}

impl Display for StringLiteralExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, r#""{}""#, self.literal)
    }
}

pub struct PathLiteralExpr {
    literal: NixPath,
}

impl Expression for PathLiteralExpr {}

impl Display for PathLiteralExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.literal)
    }
}
