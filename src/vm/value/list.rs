use derive_more::Constructor;
use rpds::VectorSync;

use crate::value::{self, Value};

use super::super::vm::VM;
use super::{ToValue, VmValue};

#[derive(Constructor, Clone, PartialEq)]
pub struct List {
    data: VectorSync<VmValue>,
}

impl List {
    pub fn push(&mut self, elem: VmValue) {
        self.data.push_back_mut(elem);
    }

    pub fn concat(mut self, other: List) -> List {
        for elem in other.data.iter() {
            self.data.push_back_mut(elem.clone());
        }
        self
    }
}

impl ToValue for List {
    fn to_value(self, vm: &VM) -> Value {
        Value::List(value::List::new(
            self.data
                .iter()
                .map(|value| value.clone().to_value(vm))
                .collect(),
        ))
    }
}
