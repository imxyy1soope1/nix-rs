use derive_more::Constructor;
use rpds::HashTrieMapSync;

use crate::value::{self, Value};

use super::super::vm::VM;
use super::{Symbol, ToValue, VmValue};

#[derive(Constructor, Clone, PartialEq)]
pub struct AttrSet {
    data: HashTrieMapSync<Symbol, VmValue>,
}

impl AttrSet {
    pub fn push_attr(&mut self, sym: Symbol, val: VmValue) {
        self.data.insert_mut(sym, val);
    }

    pub fn select(&self, sym: Symbol) -> Option<VmValue> {
        self.data.get(&sym).cloned()
    }

    pub fn has_attr(&self, sym: Symbol) -> bool {
        self.data.get(&sym).is_some()
    }

    pub fn to_data(self) -> HashTrieMapSync<Symbol, VmValue> {
        self.data
    }
}

impl ToValue for AttrSet {
    fn to_value(self, vm: &VM) -> Value {
        Value::AttrSet(value::AttrSet::new(
            self.data
                .iter()
                .map(|(sym, value)| (vm.symbols.get(sym.0).unwrap(), value.clone().to_value(vm)))
                .collect(),
        ))
    }
}
