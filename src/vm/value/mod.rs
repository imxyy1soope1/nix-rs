use std::fmt::{Debug, Display, Formatter, Result as FmtResult};

use derive_more::{Constructor, IsVariant, Unwrap};

use crate::bytecode::Func;

use rpds::{HashTrieMapSync, Vector};

#[derive(IsVariant, Unwrap)]
pub enum Const<'vm> {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(&'vm str),
    Func(&'vm Func),
}

impl<'vm> Display for Const<'vm> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "Const::")?;
        match self {
            Const::Int(int) => write!(f, "Int@{}", int),
            Const::Float(float) => write!(f, "Float@{}", float),
            Const::Bool(bool) => write!(f, "Bool@{}", bool),
            Const::String(string) => write!(f, r#"String@"{}""#, *string),
            Const::Func(func) => write!(f, "Func@{:?}", *func as *const Func),
        }
    }
}

pub struct Symbol<'vm>(&'vm str);

impl<'vm> Debug for Symbol<'vm> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        Display::fmt(self, f)
    }
}

impl<'vm> Display for Symbol<'vm> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "Symbol@{}", self.0)
    }
}

#[derive(Constructor)]
pub struct AttrSet<'vm> {
    data: HashTrieMapSync<Symbol<'vm>, Value<'vm>>,
}

pub struct OwnedAttrSet {
    data: HashTrieMapSync<String, OwnedValue>
}

#[derive(Constructor)]
pub struct List<'vm> {
    data: Vector<Value<'vm>>,
}

pub struct OwnedList {
    data: Vector<OwnedValue>
}

/* #[derive(Constructor)]
pub struct Thunk<'vm> {
} */

#[derive(IsVariant, Unwrap)]
pub enum Value<'vm> {
    Const(Const<'vm>),
    AttrSet(AttrSet<'vm>),
    List(List<'vm>),
}

#[derive(IsVariant, Unwrap)]
pub enum OwnedValue {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Func(Func),
    AttrSet(OwnedAttrSet),
    List(OwnedList)
}
