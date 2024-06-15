use std::hash::{Hash, Hasher};

use anyhow::Error;

pub type ThunkIdx = usize;
pub type ConstIdx = usize;
pub type SymIdx = usize;
pub type OpCodes = Box<[OpCode]>;
pub type Consts = Box<[Const]>;
pub type Symbols = Box<[String]>;

#[derive(Debug, Clone)]
pub enum Const {
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
}

impl From<bool> for Const {
    fn from(value: bool) -> Self {
        Const::Bool(value)
    }
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

impl<'a> TryFrom<&'a Const> for &'a bool {
    type Error = Error;

    fn try_from(value: &'a Const) -> Result<Self, Self::Error> {
        match value {
            Const::Bool(b) => Ok(b),
            _ => panic!(),
        }
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
            Bool(b) => other.try_into().map_or(false, |other| b.eq(other)),
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
            Bool(b) => b.hash(state),
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
    /// load a dynamic var onto stack
    Var { sym: SymIdx },
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
    /// jump forward
    Jmp { step: usize },
    /// [ ... cond ] if (cond) is true, then jump forward
    JmpIfTrue { step: usize },
    /// [ ... cond ] if (cond) is false, then jump forward
    JmpIfFalse { step: usize },
    /// push an empty attribute set onto stack
    AttrSet,
    /// [ ... set, value ] push the static kv pair (name, (value)) into (set)
    PushStaticAttr { name: SymIdx },
    /// [ ... set, name, value ] push the dynamic kv pair ((name), (value)) in to (set)
    PushDynamicAttr,
    /// push an empty list onto stack
    List,
    /// push list with capacity onto stack
    ListWithCap { cap: usize },
    /// [ ... list, elem ] push (elem) into (list)
    PushElem,
    /// [ ... a, b ] perform a binary operation ((a) `op` (b))
    BinOp { op: BinOp },
    /// [ ... a ] perform a unary operation (`op` (a))
    UnOp { op: UnOp },
    /// push a symbol onto stack
    Sym { sym: SymIdx },
    /// register a symbol with TOS
    RegSym,
    /// TODO:
    HasAttr { arity: usize },
    /// TODO:
    SelectAttr { arity: usize },
    /// TODO:
    SelectWithDefault { arity: usize },
    /// enter the environment of the attribute set at TOS
    EnterEnv,
    /// exit the envrironment
    ExitEnv,
    /// return a value
    Ret,
    /// no-op
    NoOp,
}

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Add,
    Mul,
    Div,
    And,
    Or,
    Eq,
    Lt,
    Con,
    Upd,
}

#[derive(Debug, Clone, Copy)]
pub enum UnOp {
    Neg,
    Not,
}

pub struct Program {
    pub top_level: OpCodes,
    pub thunks: Box<[OpCodes]>,
    pub consts: Consts,
    pub symbols: Symbols,
}

pub type Frame = Box<[OpCode]>;
