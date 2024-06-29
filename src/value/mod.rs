use std::ops::Deref;
use std::sync::Arc;
use std::fmt::{Debug, Display, Formatter, Result as FmtResult};

use derive_more::{Constructor, IsVariant, Unwrap};
use rpds::{HashTrieMapSync, VectorSync};
use ecow::EcoString;

use crate::bytecode::{Args, OpCodes, Const as ByteCodeConst};

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Symbol(Arc<str>);

impl From<Arc<str>> for Symbol {
    fn from(value: Arc<str>) -> Self {
        Symbol(value)
    }
}

impl From<String> for Symbol {
    fn from(value: String) -> Self {
        Symbol(Arc::from(value.into_boxed_str()))
    }
}

impl Deref for Symbol {
    type Target = Arc<str>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug, Clone, Hash)]
pub struct Func {
    pub args: Args,
    pub opcodes: OpCodes,
}

impl PartialEq for Func {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}

#[derive(IsVariant, Unwrap, Clone, Debug, PartialEq)]
pub enum Const {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(EcoString),
    Func(Arc<Func>),
}

impl Display for Const {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "Const::")?;
        match self {
            Const::Int(int) => write!(f, "Int@{}", int),
            Const::Float(float) => write!(f, "Float@{}", float),
            Const::Bool(bool) => write!(f, "Bool@{}", bool),
            Const::String(string) => write!(f, r#"String@"{}""#, string.as_ref()),
            Const::Func(func) => write!(f, "Func@{:?}", func.as_ref() as *const Func),
        }
    }
}

impl From<ByteCodeConst> for Const {
    fn from(value: ByteCodeConst) -> Self {
        use ByteCodeConst::*;
        match value {
            Int(int) => Const::Int(int),
            Float(float) => Const::Float(float),
            Bool(bool) => Const::Bool(bool),
            String(string) => Const::String(EcoString::from(string)),
            Func(func) => Const::Func(Arc::new(func))
        }
    }
}

#[derive(Constructor, Clone, Debug, PartialEq)]
pub struct AttrSet {
    data: HashTrieMapSync<Symbol, Value>,
}

#[derive(Constructor, Clone, Debug, PartialEq)]
pub struct List {
    data: VectorSync<Value>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Catchable {

}

#[derive(IsVariant, Unwrap, Clone, Debug, PartialEq)]
pub enum Value {
    Const(Const),
    AttrSet(AttrSet),
    List(List),
    Catchable(Catchable),
    Thunk,
}
