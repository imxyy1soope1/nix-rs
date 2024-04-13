use std::collections::HashMap;

use crate::vm::program::Frame;

use super::ir::Ir;
use super::symtable::Sym;

pub struct Env {
    pub stcs: HashMap<Sym, Frame>,
    pub dyns: Vec<(Ir, Frame)>,
}

impl Env {
    pub fn new() -> Env {
        Env {
            stcs: HashMap::new(),
            dyns: Vec::new(),
        }
    }
}
