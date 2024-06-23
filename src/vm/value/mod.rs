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

#[derive(IsVariant, Unwrap, Clone, PartialEq)]
pub enum VmValue {
    Const(Const),
    AttrSet(AttrSet),
    List(List),
    Catchable(crate::value::Catchable),
}

impl VmValue {
    pub fn not(self) -> VmValue {
        use Const::*;
        match self {
            VmValue::Const(Bool(bool)) => VmValue::Const(Bool(!bool)),
            _ => unimplemented!()
        }
    }

    pub fn and(self, other: VmValue) -> VmValue {
        use Const::*;
        match (self, other) {
            (VmValue::Const(Bool(a)), VmValue::Const(Bool(b))) => VmValue::Const(Bool(a && b)),
            _ => unimplemented!()
        }
    }

    pub fn or(self, other: VmValue) -> VmValue {
        use Const::*;
        match (self, other) {
            (VmValue::Const(Bool(a)), VmValue::Const(Bool(b))) => VmValue::Const(Bool(a || b)),
            _ => unimplemented!()
        }
    }

    pub fn eq(self, other: VmValue) -> VmValue {
        use Const::Bool;
        VmValue::Const(Bool(self == other))
    }

    pub fn neg(self) -> VmValue {
        use Const::*;
        match self {
            VmValue::Const(Int(int)) => VmValue::Const(Int(-int)),
            VmValue::Const(Float(float)) => VmValue::Const(Float(-float)),
            _ => unimplemented!()
        }
    }

    pub fn add(self, other: VmValue) -> VmValue {
        use Const::*;
        match (self, other) {
            (VmValue::Const(Int(a)), VmValue::Const(Int(b))) => VmValue::Const(Int(a + b)),
            (VmValue::Const(Int(a)), VmValue::Const(Float(b))) => VmValue::Const(Float(a as f64 + b)),
            (VmValue::Const(Float(a)), VmValue::Const(Int(b))) => VmValue::Const(Float(a + b as f64)),
            (VmValue::Const(Float(a)), VmValue::Const(Float(b))) => VmValue::Const(Float(a + b)),
            _ => unimplemented!()
        }
    }

    pub fn mul(self, other: VmValue) -> VmValue {
        use Const::*;
        match (self, other) {
            (VmValue::Const(Int(a)), VmValue::Const(Int(b))) => VmValue::Const(Int(a * b)),
            (VmValue::Const(Int(a)), VmValue::Const(Float(b))) => VmValue::Const(Float(a as f64 * b)),
            (VmValue::Const(Float(a)), VmValue::Const(Int(b))) => VmValue::Const(Float(a * b as f64)),
            (VmValue::Const(Float(a)), VmValue::Const(Float(b))) => VmValue::Const(Float(a * b)),
            _ => unimplemented!()
        }
    }

    pub fn div(self, other: VmValue) -> VmValue {
        use Const::*;
        match (self, other) {
            (_, VmValue::Const(Int(0))) => unimplemented!(),
            (_, VmValue::Const(Float(0.))) => unimplemented!(),
            (VmValue::Const(Int(a)), VmValue::Const(Int(b))) => VmValue::Const(Int(a / b)),
            (VmValue::Const(Int(a)), VmValue::Const(Float(b))) => VmValue::Const(Float(a as f64 / b)),
            (VmValue::Const(Float(a)), VmValue::Const(Int(b))) => VmValue::Const(Float(a / b as f64)),
            (VmValue::Const(Float(a)), VmValue::Const(Float(b))) => VmValue::Const(Float(a / b)),
            _ => unimplemented!()
        }
    }

    pub fn push(&mut self, elem: VmValue) {
        if let VmValue::List(list) = self {
            list.push(elem);
        } else {
            unreachable!();
        }
    }
}

impl ToValue for VmValue {
    fn to_value(self, vm: &VM) -> Value {
        match self {
            VmValue::AttrSet(attrs) => attrs.to_value(vm),
            VmValue::List(list) => list.to_value(vm),
            VmValue::Catchable(catchable) => Value::Catchable(catchable),
            VmValue::Const(cnst) => Value::Const(cnst),
        }
    }
}
