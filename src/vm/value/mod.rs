use std::fmt::{Debug, Display, Formatter, Result as FmtResult};

use derive_more::{Constructor, IsVariant, Unwrap};
use rpds::{HashTrieMapSync, Vector};

use crate::bytecode::{Func, SymIdx, Const};
use super::vm::VM;

pub trait ToOwnedValue {
    fn to_owned_value(self, vm: &VM) -> OwnedValue;
}

/*
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
} */

#[derive(Constructor, Clone)]
pub struct AttrSet {
    data: HashTrieMapSync<SymIdx, Value>,
}

impl ToOwnedValue for AttrSet {
    fn to_owned_value(self, vm: &VM) -> OwnedValue {
        todo!()
    }
}

#[derive(Debug, PartialEq)]
pub struct OwnedAttrSet {
    data: HashTrieMapSync<String, OwnedValue>
}

#[derive(Constructor, Clone)]
pub struct List {
    data: Vector<Value>,
}

impl ToOwnedValue for List {
    fn to_owned_value(self, vm: &VM) -> OwnedValue {
        todo!()
    }
}

#[derive(Debug, PartialEq)]
pub struct OwnedList {
    data: Vector<OwnedValue>
}

#[derive(Constructor)]
pub struct Thunk {
}

#[derive(Clone, Debug, PartialEq)]
pub struct Catchable {

}

#[derive(IsVariant, Unwrap, Clone)]
pub enum Value {
    // Const(Const<'vm>),
    Const(usize),
    AttrSet(AttrSet),
    List(List),
    Catchable(Catchable),
}

impl ToOwnedValue for Value {
    fn to_owned_value(self, vm: &VM) -> OwnedValue {
        match self {
            Value::AttrSet(attrs) => attrs.to_owned_value(vm),
            Value::List(list) => list.to_owned_value(vm),
            Value::Catchable(catchable) => OwnedValue::Catchable(catchable),
            Value::Const(cnst) => OwnedValue::Const(vm.get_const(cnst)),
        }
    }
}

#[derive(IsVariant, Unwrap, Debug, PartialEq)]
pub enum OwnedValue {
    // Int(i64),
    // Float(f64),
    // Bool(bool),
    // String(String),
    // Func(Func),
    Const(Const),
    AttrSet(OwnedAttrSet),
    List(OwnedList),
    Catchable(Catchable)
}
