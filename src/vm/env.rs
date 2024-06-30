use rpds::HashTrieMapSync;

use super::value::{VmValue, Symbol};

pub struct Env {
    map: HashTrieMapSync<Symbol, VmValue>
}

impl Env {
    pub fn new() -> Env {
        Env {
            map: HashTrieMapSync::new_sync()
        }
    }

    pub fn insert(&mut self, symbol: Symbol, value: VmValue) {
        self.map.insert_mut(symbol, value);
    }
}
