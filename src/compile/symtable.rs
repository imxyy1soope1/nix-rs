use std::collections::HashMap;

pub type Sym = usize;

pub struct SymTable {
    syms: HashMap<String, Sym>,
}

impl SymTable {
    pub fn new() -> SymTable {
        SymTable {
            syms: HashMap::new(),
        }
    }

    pub fn lookup(&mut self, name: String) -> Sym {
        if let Some(sym) = self.syms.get(&name) {
            *sym
        } else {
            let sym = self.syms.len();
            self.syms.insert(name, sym);
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
