use rpds::HashTrieMapSync;
use derive_more::Constructor;

use crate::value::Value;

use super::super::vm::VM;
use super::{Symbol, VmValue, ToValue};

#[derive(Constructor, Clone)]
pub struct AttrSet {
    data: HashTrieMapSync<Symbol, VmValue>,
}

impl ToValue for AttrSet {
    fn to_value(self, vm: &VM) -> Value {
        todo!()
    }
}

