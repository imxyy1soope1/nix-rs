use std::mem::MaybeUninit;
use std::collections::HashMap;
use std::sync::{Arc, Weak, RwLock};
use std::cell::RefCell;

use anyhow::{anyhow, Result};

use crate::bytecode::*;
use crate::slice::*;

use super::data::*;
use super::value::*;

pub fn run(prog: Program) -> Result<OwnedValue> {
    let vm = VM::new(prog.consts, prog.symbols, prog.thunks);
    Ok(vm.eval(prog.top_level)?.to_owned_value(&vm))
}

#[doc(hidden)]
struct VmRef(*const VM);

pub struct VM {
    consts: Consts,
    symbols: Symbols,
    thunks: VmThunks,
    symbols_map: HashMap<String, SymIdx>,
    dynamic_symbols: Vec<String>
}


impl VM {
    fn new(consts: Consts, symbols: Symbols, thunks: Thunks) -> Self {
        let symbols_map = symbols.clone().into_iter().enumerate().map(|(idx, sym)| (sym, idx)).collect();
        let thunks = VmThunks;
        VM {
            consts,
            symbols,
            thunks,
            symbols_map,
            dynamic_symbols: Vec::new()
        }
    }

    pub fn eval(&self, opcodes: OpCodes) -> Result<Value> {
        todo!()
    }
}
