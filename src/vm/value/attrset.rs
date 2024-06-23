use rpds::HashTrieMapSync;
use derive_more::Constructor;

use crate::value::{Value, self};

use super::super::vm::VM;
use super::{Symbol, VmValue, ToValue};

#[derive(Constructor, Clone, PartialEq)]
pub struct AttrSet {
    data: HashTrieMapSync<Symbol, VmValue>,
}

impl ToValue for AttrSet {
    fn to_value(self, vm: &VM) -> Value {
        Value::AttrSet(value::AttrSet::new(self.data.iter().map(|(sym, value)| (vm.get_sym(sym.0).unwrap(), value.clone().to_value(vm))).collect()))
    }
}

