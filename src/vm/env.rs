use std::ptr::NonNull;

use rpds::HashTrieMapSync;

use super::value::{Symbol, VmValue};

pub struct Env {
    last: NonNull<Env>,
    map: HashTrieMapSync<Symbol, VmValue>,
}

impl Env {
    pub fn empty() -> Env {
        Env {
            last: NonNull::dangling(),
            map: HashTrieMapSync::new_sync(),
        }
    }

    pub fn lookup(&self, symbol: Symbol) -> VmValue {
        if let Some(value) = self.map.get(&symbol) {
            value.clone()
        } else {
            let last = unsafe { self.last.as_ref() };
            last.lookup(symbol)
        }
    }

    pub fn insert(&mut self, symbol: Symbol, value: VmValue) {
        self.map.insert_mut(symbol, value);
    }

    pub fn enter(&mut self, map: HashTrieMapSync<Symbol, VmValue>) {
        let last = std::mem::replace(
            self,
            Env {
                last: NonNull::dangling(),
                map,
            },
        );
        self.last = NonNull::new(Box::leak(Box::new(last)) as *mut Env).unwrap();
    }

    pub fn leave(&mut self) {
        let last = unsafe { self.last.as_ref() };
        self.last = last.last;
        self.map = last.map.clone();
    }
}
