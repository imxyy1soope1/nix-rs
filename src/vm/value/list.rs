use rpds::Vector;
use derive_more::Constructor;

use crate::value::Value;

use super::super::vm::VM;
use super::{ToValue, VmValue};

#[derive(Constructor, Clone)]
pub struct List {
    data: Vector<VmValue>,
}

impl ToValue for List {
    fn to_value(self, vm: &VM) -> Value {
        todo!()
    }
}

