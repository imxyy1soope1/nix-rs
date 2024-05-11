use std::hash::{Hash, Hasher};

use anyhow::Error;

pub type ThunkIdx = usize;
pub type ConstIdx = usize;
pub type SymIdx = usize;
pub type CodeIdx = usize;

#[derive(Debug, Clone)]
pub enum Const {
    Int(i64),
    Float(f64),
    String(String),
}

impl From<i64> for Const {
    fn from(value: i64) -> Self {
        Const::Int(value)
    }
}

impl From<f64> for Const {
    fn from(value: f64) -> Self {
        Const::Float(value)
    }
}

impl From<String> for Const {
    fn from(value: String) -> Self {
        Const::String(value)
    }
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
            Float(float) => other
                .try_into()
                .map_or(false, |other: &f64| float.to_bits().eq(&other.to_bits())),
            String(string) => other
                .try_into()
                .map_or(false, |other: &str| string.eq(other)),
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

#[derive(Debug, Clone, Copy)]
pub enum OpCode {
    /// load a constant onto stack
    Const { idx: ConstIdx },
    /// create a thunk with codes[thunk_idx[idx]+1..thunk_idx[idx+1]]
    Thunk { idx: ThunkIdx },
    /// load a thunk lazily onto stack
    LoadThunk { idx: ThunkIdx },
    /// load a thunk onto stack and force its value
    LoadValue { idx: ThunkIdx },
    /// force TOS to value
    ForceValue,
    /// [ ... func, args @ .. ] call func with `arity` numbers of arg
    Call { arity: usize },
    /// assert TOS is true then consume it
    Assert,
    /// [ ... cond, consq, alter ] if (cond) is true, then force thunk (consq), else (alter)
    IfElse,
    /// push an empty attribute set onto stack
    AttrSet,
    /// [ ... set, value ] push the static kv pair (name, (value)) into (set)
    PushStaticAttr { name: ConstIdx },
    /// [ ... set, name, value ] push the dynamic kv pair ((name), (value)) in to (set)
    PushDynAttr,
    /// push an empty list onto stack
    List,
    /// [ ... list, elem ] push (elem) into (list)
    PushElem,
    /// [ ... a, b ] perform a binary operation ((a) `op` (b))
    BinOp { op: BinOp },
    /// [ ... a ] perform a unary operation (`op` (a))
    UnOp { op: UnOp },
    /// register a symbol with TOS
    RegSym,
    /// [ ... set, attr ] check whether (set) has (attr)
    HasAttr,
    /// enter the environment of the attribute set at TOS
    EnterEnv,
    /// exit the envrironment
    ExitEnv,
    /// return a value
    Ret,
}

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Add,
    Mul,
    Div,
    And,
    Or,
}

#[derive(Debug, Clone, Copy)]
pub enum UnOp {
    Neg,
    Not,
}

pub struct Program {
    pub codes: Box<[OpCode]>,
    pub consts: Box<[Const]>,
    pub syms: Box<[String]>,
    pub thunk_idxs: Box<[CodeIdx]>,
}

pub type Frame = Box<[OpCode]>;
