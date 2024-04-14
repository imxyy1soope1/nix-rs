use std::collections::HashMap;

use crate::vm::program::{Frame, Idx};

use super::ir::Ir;
use super::symtable::Sym;

pub struct Env {
    pub stcs: HashMap<Sym, Idx>,
    pub dyns: Vec<(Frame, Idx)>,
}

impl Env {
    pub fn new() -> Env {
        Env {
            stcs: HashMap::new(),
            dyns: Vec::new(),
        }
    }
}
