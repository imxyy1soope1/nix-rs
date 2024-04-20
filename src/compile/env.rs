use std::collections::HashMap;

use crate::vm::program::{Frame, Idx};

use super::symtable::Sym;
use super::ir::Ir;

pub struct IrEnv {
    pub stcs: HashMap<Sym, Ir>,
    pub dyns: Vec<(Ir, Ir)>,
}

impl IrEnv {
    pub fn new() -> IrEnv {
        IrEnv { stcs: HashMap::new(), dyns: Vec::new() }
    }
}

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
