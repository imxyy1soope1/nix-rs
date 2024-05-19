use std::collections::HashMap;

use crate::bytecode::SymIdx;

// FIXME: don't store syms twice to make it more memory efficient?

pub struct SymTable {
    syms: Vec<String>,
    syms_table: HashMap<String, SymIdx>,
}

impl SymTable {
    pub fn new() -> SymTable {
        SymTable {
            syms: Vec::new(),
            syms_table: HashMap::new(),
        }
    }

    pub fn lookup(&mut self, name: String) -> SymIdx {
        if let Some(sym) = self.syms_table.get(&name) {
            *sym
        } else {
            let sym = self.syms.len();
            self.syms.push(name.clone());
            self.syms_table.insert(name, sym);
            sym
        }
    }
}
