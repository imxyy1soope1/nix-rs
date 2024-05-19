use std::collections::HashMap;

use crate::bytecode::{Frame, SymIdx, ThunkIdx};

use super::ir::Ir;

pub struct IrEnv {
    pub stcs: HashMap<SymIdx, Ir>,
    pub dyns: Vec<(Ir, Ir)>,
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
    pub stcs: HashMap<SymIdx, ThunkIdx>,
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
