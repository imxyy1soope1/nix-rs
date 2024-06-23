use rpds::Vector;
use derive_more::Constructor;

use crate::value::{Value, self};

use super::super::vm::VM;
use super::{ToValue, VmValue};

#[derive(Constructor, Clone, PartialEq)]
pub struct List {
    data: Vector<VmValue>,
}

impl List {
    pub fn push(&mut self, elem: VmValue) {
        self.data.push_back_mut(elem);
    }
}

impl ToValue for List {
    fn to_value(self, vm: &VM) -> Value {
        Value::List(value::List::new(self.data.iter().map(|value| value.clone().to_value(vm)).collect()))
    }
}

