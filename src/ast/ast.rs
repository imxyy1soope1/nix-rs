use super::types::*;
use crate::token::Token;
use std::any::Any;
use std::fmt::Display;

pub trait Expression: Display {
    fn as_any(&self) -> &dyn Any;
}

pub struct PrefixExpr {
    token: Token,
    right: Box<dyn Expression>,
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
}

impl Display for PrefixExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}{})", self.token, self.right)
    }
}

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
}

impl Display for InfixExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {} {})", self.left, self.token, self.right)
    }
}

pub struct IdentifierExpr {
    ident: String,
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
}

impl Display for IdentifierExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ident)
    }
}

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
}

impl Display for IntLiteralExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.literal)
    }
}

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
}

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

impl Expression for BoolLiteralExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Display for BoolLiteralExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.literal)
    }
}

pub struct NullLiteralExpr;

impl Expression for NullLiteralExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Display for NullLiteralExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "null")
    }
}

pub struct EllipsisLiteralExpr;

impl Expression for EllipsisLiteralExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Display for EllipsisLiteralExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "...")
    }
}

pub struct StringLiteralExpr {
    pub literal: NixString,
    pub replaces: Vec<(usize, Vec<Token>)>,
}

impl StringLiteralExpr {
    pub fn new(s: String, replaces: Vec<(usize, Vec<Token>)>) -> StringLiteralExpr {
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
}

impl Display for StringLiteralExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, r#""{}""#, self.literal)
    }
}

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
}

impl Display for FunctionLiteralExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}: {})", self.arg, self.body)
    }
}

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
}

impl Display for FunctionCallExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {})", self.func, self.arg)
    }
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

impl Expression for IfExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Display for IfExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "if {} then {} else {}",
            self.cond, self.consq, self.alter
        )
    }
}

pub struct BindingExpr {
    pub name: Box<dyn Expression>,
    pub value: Box<dyn Expression>,
}

impl BindingExpr {
    pub fn new(name: Box<dyn Expression>, value: Box<dyn Expression>) -> BindingExpr {
        BindingExpr { name, value }
    }
}

impl Expression for BindingExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Display for BindingExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} = {}", self.name, self.value)
    }
}

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
}

impl Display for AttrsLiteralExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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

pub struct ArgSetExpr {
    args: Vec<Box<dyn Expression>>,
    allow_more: bool,
    alias: Option<Box<dyn Expression>>,
}

impl ArgSetExpr {
    pub fn new(
        args: Vec<Box<dyn Expression>>,
        allow_more: bool,
        alias: Option<Box<dyn Expression>>,
    ) -> ArgSetExpr {
        assert!(args.iter().all(|a| a.as_any().is::<IdentifierExpr>()));
        assert!(alias.as_ref().map_or(true, |a| a.as_any().is::<IdentifierExpr>()));
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
}

impl Display for ArgSetExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{ ")?;
        let mut first = true;
        for arg in self.args.iter() {
            if first {
                first = false;
            } else {
                write!(f, ", ")?;
            }
            write!(f, "{arg}")?;
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

pub struct ListLiteralExpr {
    items: Vec<Box<dyn Expression>>,
}

impl ListLiteralExpr {
    pub fn new(items: Vec<Box<dyn Expression>>) -> ListLiteralExpr {
        assert!(!items.iter().any(|i| i.as_any().is::<BindingExpr>()));
        ListLiteralExpr { items }
    }
}

impl Expression for ListLiteralExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Display for ListLiteralExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[ ")?;
        for item in self.items.iter() {
            write!(f, "{item} ")?;
        }
        write!(f, "]")
    }
}

pub struct LetExpr {
    bindings: Vec<Box<dyn Expression>>,
    expr: Box<dyn Expression>,
}

impl LetExpr {
    pub fn new(bindings: Vec<Box<dyn Expression>>, expr: Box<dyn Expression>) -> LetExpr {
        assert!(bindings.iter().all(|b| b.as_any().is::<BindingExpr>()));
        assert!(!expr.as_any().is::<BindingExpr>());
        LetExpr { bindings, expr }
    }
}

impl Expression for LetExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Display for LetExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(let ")?;
        for binding in self.bindings.iter() {
            write!(f, "{binding}; ")?;
        }
        write!(f, "in {})", self.expr)
    }
}

pub struct WithExpr {
    attrs: Box<dyn Expression>,
    expr: Box<dyn Expression>,
}

impl WithExpr {
    pub fn new(attrs: Box<dyn Expression>, expr: Box<dyn Expression>) -> WithExpr {
        let a = attrs.as_any();
        assert!(!a.is::<BindingExpr>() && !a.is::<PrefixExpr>());
        WithExpr { attrs, expr }
    }
}

impl Expression for WithExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Display for WithExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(with {}; {})", self.attrs, self.expr)
    }
}

pub struct AssertExpr {
    assertion: Box<dyn Expression>,
    expr: Box<dyn Expression>,
}

impl AssertExpr {
    pub fn new(assertion: Box<dyn Expression>, expr: Box<dyn Expression>) -> AssertExpr {
        assert!(!assertion.as_any().is::<BindingExpr>() && !expr.as_any().is::<BindingExpr>());
        AssertExpr { assertion, expr }
    }
}

impl Expression for AssertExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Display for AssertExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(assert {}; {})", self.assertion, self.expr)
    }
}

pub struct InheritExpr {
    inherits: Vec<Box<dyn Expression>>,
    from: Option<Box<dyn Expression>>,
}

impl InheritExpr {
    pub fn new(
        inherits: Vec<Box<dyn Expression>>,
        from: Option<Box<dyn Expression>>,
    ) -> InheritExpr {
        assert!(inherits.iter().all(|e| {
            let a = e.as_any();
            a.is::<IdentifierExpr>() || a.is::<StringLiteralExpr>()
        }));
        InheritExpr { inherits, from }
    }
}

impl Expression for InheritExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Display for InheritExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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
}

impl Display for PathLiteralExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.literal)
    }
}

pub struct SearchPathExpr {
    path: Box<dyn Expression>,
}

impl SearchPathExpr {
    pub fn new(path: Box<dyn Expression>) -> SearchPathExpr {
        let a = path.as_any();
        assert!(a.is::<PathLiteralExpr>() && a.downcast_ref::<PathLiteralExpr>().unwrap().relative);
        SearchPathExpr { path }
    }
}

impl Expression for SearchPathExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Display for SearchPathExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{}>", self.path)
    }
}

pub struct ThunkExpr {
    pub ident: Box<dyn Expression>,
}

impl ThunkExpr {
    pub fn new(ident: Box<dyn Expression>) -> ThunkExpr {
        let a = ident.as_any();
        assert!(
            a.is::<IdentifierExpr>()
                || (a.is::<StringLiteralExpr>()
                    && a.downcast_ref::<StringLiteralExpr>()
                        .unwrap()
                        .replaces
                        .is_empty())
        );
        ThunkExpr { ident }
    }
}

impl Expression for ThunkExpr {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Display for ThunkExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "${{{}}}", self.ident)
    }
}
