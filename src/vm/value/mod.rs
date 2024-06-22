use derive_more::{Constructor, IsVariant, Unwrap};
use rpds::{HashTrieMapSync, Vector};

use crate::value::*;
use super::vm::VM;

pub trait ToValue {
    fn to_value(self, vm: &VM) -> Value;
}

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub struct Symbol(usize);

#[derive(Constructor, Clone)]
pub struct AttrSet {
    data: HashTrieMapSync<Symbol, VmValue>,
}

impl ToValue for AttrSet {
    fn to_value(self, vm: &VM) -> Value {
        todo!()
    }
}

#[derive(Debug, PartialEq)]
pub struct OwnedAttrSet {
    data: HashTrieMapSync<String, Value>
}

#[derive(Constructor, Clone)]
pub struct List {
    data: Vector<VmValue>,
}

impl ToValue for List {
    fn to_value(self, vm: &VM) -> Value {
        todo!()
    }
}

#[derive(Debug, PartialEq)]
pub struct OwnedList {
    data: Vector<Value>
}

#[derive(Constructor)]
pub struct Thunk {
}

#[derive(IsVariant, Unwrap, Clone)]
pub enum VmValue {
    // Const(Const<'vm>),
    Const(usize),
    AttrSet(AttrSet),
    List(List),
    Catchable(crate::value::Catchable),
}

impl VmValue {
    pub fn not(self) -> VmValue {
        todo!()
    }

    pub fn neg(self) -> VmValue {
        todo!()
    }

    pub fn add(self, other: Self) -> VmValue {
        todo!()
    }

    pub fn mul(self, other: Self) -> VmValue {
        todo!()
    }

    pub fn div(self, other: Self) -> VmValue {
        todo!()
    }
}

impl ToValue for VmValue {
    fn to_value(self, vm: &VM) -> Value {
        match self {
            VmValue::AttrSet(attrs) => attrs.to_value(vm),
            VmValue::List(list) => list.to_value(vm),
            VmValue::Catchable(catchable) => Value::Catchable(catchable),
            VmValue::Const(cnst) => Value::Const(vm.get_const(cnst)),
        }
    }
}
