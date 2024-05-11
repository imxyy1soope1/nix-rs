use std::collections::HashMap;

use crate::bytecode::{Frame, ThunkIdx};

use super::ir::Ir;
use super::symtable::Sym;

pub struct IrEnv {
    pub stcs: HashMap<Sym, Box<dyn Ir>>,
    pub dyns: Vec<(Box<dyn Ir>, Box<dyn Ir>)>,
}

impl IrEnv {
    pub fn new() -> IrEnv {
        IrEnv {
            stcs: HashMap::new(),
            dyns: Vec::new(),
        }
    }
}

pub struct Env {
    pub stcs: HashMap<Sym, ThunkIdx>,
    pub dyns: Vec<(Frame, ThunkIdx)>,
}

impl Env {
    pub fn new() -> Env {
        Env {
            stcs: HashMap::new(),
            dyns: Vec::new(),
        }
    }
}
