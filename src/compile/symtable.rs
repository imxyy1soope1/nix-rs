use std::collections::HashMap;
use std::collections::BTreeMap;

pub type Sym = usize;

pub struct SymTable {
    syms: Vec<String>,
    syms_table: HashMap<*const String, Sym>
}

impl SymTable {
    pub fn new() -> SymTable {
        SymTable {
            syms: Vec::new(),
            syms_table: HashMap::new()
        }
    }

    pub fn lookup(&mut self, name: String) -> Sym {
        if let Some(sym) = self.syms_table.get(&(&name as *const String)) {
            *sym
        } else {
            let sym = self.syms.len();
            self.syms_table.insert(&name as *const String, sym);
            sym
        }
    }

    pub fn into_syms(self) -> Box<[String]> {
        let len = self.syms.len();
        let mut syms = vec![String::new(); len];
        for (cnst, idx) in self.syms {
            unsafe { *syms.get_unchecked_mut(idx) = cnst }
        }
        syms.into_boxed_slice()
    }
}
