use derive_more::{Constructor, IsVariant, Unwrap};

use crate::value::*;

use super::vm::VM;

mod attrset;
mod list;
mod thunk;

pub use attrset::AttrSet;
pub use list::List;
pub use thunk::VmThunk;

pub trait ToValue {
    fn to_value(self, vm: &VM) -> Value;
}

#[derive(PartialEq, Eq, Hash, Clone, Copy, Constructor)]
pub struct Symbol(usize);

#[derive(IsVariant, Unwrap, Clone, PartialEq)]
pub enum VmValue {
    Const(Const),
    AttrSet(AttrSet),
    List(List),
    Catchable(crate::value::Catchable),
}

use VmValue::Const as VmConst;
impl VmValue {
    pub fn not(self) -> VmValue {
        use Const::*;
        match self {
            VmConst(Bool(bool)) => VmConst(Bool(!bool)),
            _ => todo!(),
        }
    }

    pub fn and(self, other: VmValue) -> VmValue {
        use Const::*;
        match (self, other) {
            (VmConst(Bool(a)), VmConst(Bool(b))) => VmConst(Bool(a && b)),
            _ => todo!(),
        }
    }

    pub fn or(self, other: VmValue) -> VmValue {
        use Const::*;
        match (self, other) {
            (VmConst(Bool(a)), VmConst(Bool(b))) => VmConst(Bool(a || b)),
            _ => todo!(),
        }
    }

    pub fn eq(self, other: VmValue) -> VmValue {
        use Const::Bool;
        VmConst(Bool(self == other))
    }

    pub fn neg(self) -> VmValue {
        use Const::*;
        match self {
            VmConst(Int(int)) => VmConst(Int(-int)),
            VmConst(Float(float)) => VmConst(Float(-float)),
            _ => todo!(),
        }
    }

    pub fn add(self, other: VmValue) -> VmValue {
        use Const::*;
        match (self, other) {
            (VmConst(Int(a)), VmConst(Int(b))) => VmConst(Int(a + b)),
            (VmConst(Int(a)), VmConst(Float(b))) => VmConst(Float(a as f64 + b)),
            (VmConst(Float(a)), VmConst(Int(b))) => VmConst(Float(a + b as f64)),
            (VmConst(Float(a)), VmConst(Float(b))) => VmConst(Float(a + b)),
            (VmConst(String(a)), VmConst(String(b))) => {
                let mut string = a.clone();
                string.push_str(b.as_str());
                VmConst(String(string))
            }
            _ => todo!(),
        }
    }

    pub fn mul(self, other: VmValue) -> VmValue {
        use Const::*;
        match (self, other) {
            (VmConst(Int(a)), VmConst(Int(b))) => VmConst(Int(a * b)),
            (VmConst(Int(a)), VmConst(Float(b))) => VmConst(Float(a as f64 * b)),
            (VmConst(Float(a)), VmConst(Int(b))) => VmConst(Float(a * b as f64)),
            (VmConst(Float(a)), VmConst(Float(b))) => VmConst(Float(a * b)),
            _ => todo!(),
        }
    }

    pub fn div(self, other: VmValue) -> VmValue {
        use Const::*;
        match (self, other) {
            (_, VmConst(Int(0))) => todo!(),
            (_, VmConst(Float(0.))) => todo!(),
            (VmConst(Int(a)), VmConst(Int(b))) => VmConst(Int(a / b)),
            (VmConst(Int(a)), VmConst(Float(b))) => VmConst(Float(a as f64 / b)),
            (VmConst(Float(a)), VmConst(Int(b))) => VmConst(Float(a / b as f64)),
            (VmConst(Float(a)), VmConst(Float(b))) => VmConst(Float(a / b)),
            _ => todo!(),
        }
    }

    pub fn push(&mut self, elem: VmValue) {
        if let VmValue::List(list) = self {
            list.push(elem);
        } else {
            todo!()
        }
    }

    pub fn concat(self, other: VmValue) -> VmValue {
        if let (VmValue::List(a), VmValue::List(b)) = (self, other) {
            VmValue::List(a.concat(b))
        } else {
            todo!()
        }
    }

    pub fn push_attr(&mut self, sym: Symbol, val: VmValue) {
        if let VmValue::AttrSet(attrs) = self {
            attrs.push_attr(sym, val)
        } else {
            todo!()
        }
    }

    pub fn select(&mut self, sym: Symbol) {
        if let VmValue::AttrSet(attrs) = self {
            let val = attrs.select(sym);
            *self = val;
        } else {
            todo!()
        }
    }

    pub fn select_with_default(&mut self, sym: Symbol, default: VmValue) {
        if let VmValue::AttrSet(attrs) = self {
            let val = attrs.select_with_default(sym, default);
            *self = val;
        } else {
            todo!()
        }
    }

    pub fn has_attr(&mut self, sym: Symbol) {
        if let VmValue::AttrSet(attrs) = self {
            let val = VmConst(Const::Bool(attrs.has_attr(sym)));
            *self = val;
        } else {
            *self = VmConst(Const::Bool(false));
        }
    }

    pub fn coerce_to_string(&mut self) {
        if let VmConst(Const::String(_)) = self {
            ()
        } else {
            todo!()
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
