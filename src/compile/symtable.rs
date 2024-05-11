use std::collections::BTreeMap;
use std::collections::HashMap;

pub type Sym = usize;

// FIXME: don't store syms twice to make it more memory efficient?

pub struct SymTable {
    syms: Vec<String>,
    syms_table: HashMap<String, Sym>,
}

impl SymTable {
    pub fn new() -> SymTable {
        SymTable {
            syms: Vec::new(),
            syms_table: HashMap::new(),
        }
    }

    pub fn lookup(&mut self, name: String) -> Sym {
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
