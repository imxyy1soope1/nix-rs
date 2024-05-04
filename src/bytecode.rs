use std::hash::{Hasher, Hash};

use anyhow::Error;

pub type ThunkIdx = usize;
pub type ConstIdx = usize;
pub type SymIdx = usize;

#[derive(Debug, Clone)]
pub enum Const {
    Int(i64),
    Float(f64),
    String(String),
}

impl<'a> TryFrom<&'a Const> for &'a i64 {
    type Error = Error;

    fn try_from(value: &'a Const) -> Result<Self, Self::Error> {
        match value {
            Const::Int(int) => Ok(int),
            _ => panic!(),
        }
    }
}

impl<'a> TryFrom<&'a Const> for &'a f64 {
    type Error = Error;

    fn try_from(value: &'a Const) -> Result<Self, Self::Error> {
        match value {
            Const::Float(float) => Ok(float),
            _ => panic!(),
        }
    }
}

impl<'a> TryFrom<&'a Const> for &'a str {
    type Error = Error;

    fn try_from(value: &'a Const) -> Result<Self, Self::Error> {
        match value {
            Const::String(string) => Ok(string),
            _ => panic!(),
        }
    }
}

impl PartialEq for Const {
    fn eq(&self, other: &Self) -> bool {
        use Const::*;
        match self {
            Int(int) => other.try_into().map_or(false, |other| int.eq(other)),
            Float(float) => other.try_into().map_or(false, |other: &f64| float.to_bits().eq(&other.to_bits())),
            String(string) => other.try_into().map_or(false, |other: &str| string.eq(other))
        }
    }
}

impl Eq for Const {}

impl Hash for Const {
    fn hash<H: Hasher>(&self, state: &mut H) {
        use Const::*;
        match *self {
            Int(int) => int.hash(state),
            Float(float) => float.to_bits().hash(state),
            String(ref string) => string.hash(state),
        }
    }
}

pub enum OpCode {
    /// load a constant onto stack
    Const { idx: ConstIdx },
    /// load a thunk lazily onto stack
    Thunk { idx: ThunkIdx },
    /// load a thunk onto stack and force its value
    ThunkValue { idx: ThunkIdx },
    /// force TOS to value
    ForceValue,
    /// call a funcion thunk with `arity` numbers of parameters on the stack
    CallThunk { thunk: ThunkIdx, arity: usize },
    /// assert TOS is true
    Assert,
    /// if TOS is true, then load `consq` onto stack, else load `alter` onto stack
    IfElse { consq: ThunkIdx, alter: ThunkIdx },
    /// push an empty attribute set onto stack
    AttrSet,
    /// push an static attribute into the attribute set at TOS
    PushStaticAttr { name: ConstIdx, value: ThunkIdx },
    /// push an dynamic attribute into the attribute set at TOS
    PushDynAttr { name: ThunkIdx, value: ThunkIdx },
    /// push an empty list onto stack
    List,
    /// push an element into the list at TOS
    PushElem { elem: ThunkIdx },
    /// perform a binary operation
    BinOp { op: BinOp },
    /// perform a unary operation
    UnOp { op: UnOp },
    /// force a thunk, register the symbol, then push the symbol onto stack
    RegSym { sym: ThunkIdx },
    /// TODO: comment
    HasAttr,
    /// enter the environment of the attribute set at TOS
    EnterEnv,
    /// exit the envrironment
    ExitEnv,
    /// return a value
    Ret,
}

pub enum BinOp {
    Add,
    Mul,
    Div,
    And,
    Or,
}

pub enum UnOp {
    Neg,
    Not,
}

pub struct Program {
    pub codes: Box<[OpCode]>,
    pub consts: Box<[Const]>,
    pub syms: Box<[String]>,
}
