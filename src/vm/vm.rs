use std::mem::MaybeUninit;
use std::collections::HashMap;
use std::sync::{Arc, Weak, RwLock};
use std::cell::RefCell;

use anyhow::{anyhow, Result};

use crate::bytecode::*;
use crate::bytecode::Thunk as ByteCodeThunk;
use crate::slice::*;

use super::data::*;
use super::value::*;

pub fn run(prog: Program) -> Result<OwnedValue> {
    let vm = VM::new(prog.consts, prog.symbols, prog.thunks);
    Ok(vm.eval(prog.top_level)?.to_owned_value(&vm))
}

pub struct VM {
    consts: Consts,
    thunks: Slice<Arc<VmThunk>>,
    symbols: Symbols,
    symbols_map: HashMap<String, SymIdx>,
    dynamic_symbols: Vec<String>
}


impl VM {
    fn new(consts: Consts, symbols: Symbols, thunks: Thunks) -> Self {
        let symbols_map = symbols.clone().into_iter().enumerate().map(|(idx, sym)| (sym, idx)).collect();
        let thunks = thunks.into_iter().map(|ByteCodeThunk { opcodes }| Arc::new(VmThunk::new(opcodes))).collect();
        VM {
            consts,
            thunks,
            symbols,
            symbols_map,
            dynamic_symbols: Vec::new()
        }
    }

    pub fn eval(&self, opcodes: OpCodes) -> Result<Value> {
        todo!()
    }
}
