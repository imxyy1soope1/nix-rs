use std::collections::HashMap;

use crate::bytecode::SymIdx;

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
