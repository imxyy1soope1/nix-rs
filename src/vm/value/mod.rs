use std::fmt::{Debug, Display, Formatter, Result as FmtResult};

use derive_more::{Constructor, IsVariant, Unwrap};
use rpds::{HashTrieMapSync, Vector};

use crate::bytecode::{Func, SymIdx};
use super::vm::VM;

pub trait ToOwnedValue {
    fn to_owned_value(self, vm: &VM) -> OwnedValue;
}


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

#[derive(Constructor)]
pub struct AttrSet {
    data: HashTrieMapSync<SymIdx, Value>,
}

impl ToOwnedValue for AttrSet {
    fn to_owned_value(self, vm: &VM) -> OwnedValue {
        todo!()
    }
}

pub struct OwnedAttrSet {
    data: HashTrieMapSync<String, OwnedValue>
}

#[derive(Constructor)]
pub struct List {
    data: Vector<Value>,
}

impl ToOwnedValue for List {
    fn to_owned_value(self, vm: &VM) -> OwnedValue {
        todo!()
    }
}

pub struct OwnedList {
    data: Vector<OwnedValue>
}

#[derive(Constructor)]
pub struct Thunk {
}

pub struct Catchable {

}

#[derive(IsVariant, Unwrap)]
pub enum Value {
    // Const(Const<'vm>),
    AttrSet(AttrSet),
    List(List),
    Catchable(Catchable),
}

impl ToOwnedValue for Value {
    fn to_owned_value(self, vm: &VM) -> OwnedValue {
        match self {
            Value::AttrSet(attrs) => attrs.to_owned_value(vm),
            Value::List(list) => list.to_owned_value(vm),
            Value::Catchable(catchable) => OwnedValue::Catchable(catchable)
        }
    }
}

#[derive(IsVariant, Unwrap)]
pub enum OwnedValue {
    // Int(i64),
    // Float(f64),
    // Bool(bool),
    // String(String),
    // Func(Func),
    AttrSet(OwnedAttrSet),
    List(OwnedList),
    Catchable(Catchable)
}
