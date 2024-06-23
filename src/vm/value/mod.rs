use std::sync::Arc;

use derive_more::{IsVariant, Unwrap};

use crate::value::*;

use super::vm::VM;

mod thunk;
mod attrset;
mod list;

pub use thunk::VmThunk;
pub use attrset::AttrSet;
pub use list::List;

pub trait ToValue {
    fn to_value(self, vm: &VM) -> Value;
}

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub struct Symbol(usize);

#[derive(IsVariant, Unwrap, Clone)]
pub enum VmValue {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(Arc<String>),
    Func(Arc<Func>),
    AttrSet(AttrSet),
    List(List),
    Catchable(crate::value::Catchable),
}

impl VmValue {
    pub fn not(self) -> VmValue {
        match self {
            VmValue::Bool(bool) => VmValue::Bool(!bool),
            _ => unimplemented!()
        }
    }

    pub fn neg(self) -> VmValue {
        match self {
            VmValue::Int(int) => VmValue::Int(-int),
            VmValue::Float(float) => VmValue::Float(-float),
            _ => unimplemented!()
        }
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
            
        }
    }
}
