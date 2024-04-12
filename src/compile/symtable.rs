use std::collections::HashMap;

use crate::closure::Closure;

pub(crate) enum SymType {
    Int = 0,
    Float,
    String,
    Path,
    SearchPath,
    List,
    Set,
    Lambda,

    Arg,
}

pub(crate) struct Symbol {
    pub(crate) idx: usize,
    pub(crate) ty: SymType,
}

pub(crate) struct SymTable {
    table: HashMap<String, Symbol>,
    closures: Vec<Closure>,
}

impl SymTable {
    pub(crate) fn new() -> SymTable {
        SymTable {
            table: HashMap::new(),
            closures: Vec::new(),
        }
    }
}
