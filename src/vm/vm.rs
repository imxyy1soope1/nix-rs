use std::mem::{transmute, MaybeUninit};
use std::collections::HashMap;
use std::sync::{Arc, Weak, RwLock};
use std::cell::RefCell;

use anyhow::{anyhow, Result};

use crate::bytecode::*;
use crate::slice::*;

use super::data::*;
use super::value::*;

pub fn run() -> Result<OwnedValue> {
    todo!()
}

pub struct VmData {
    consts: Consts,
    symbols: Symbols,
}

pub struct VM<'data> {
    thunks: VmThunks<'data>,
    symbols_map: HashMap<String, SymIdx>,
    dynamic_symbols: Vec<String>
}

impl VmData {
    pub fn new(consts: Consts, symbols: Symbols) -> Self {
        VmData { consts, symbols }
    }
}

impl<'data> VM<'data> {
    pub fn new(data: &'data VmData, thunks: Thunks) -> Self {
        let mut temp: Slice<Arc<RefCell<MaybeUninit<VmThunk<'data>>>>> = (0..thunks.len()).map(|_| Arc::new(RefCell::new(MaybeUninit::uninit()))).collect();
        thunks.into_iter().enumerate().for_each(|(idx, Thunk { deps, opcodes })| {
            unsafe {
                let deps = deps.into_iter().map(|idx| transmute(temp.get(idx).unwrap().clone())).collect();
                temp.get_mut(idx).unwrap().borrow_mut().write(VmThunk::new(data, deps, opcodes));
            }
        });
        VM {
            thunks: unsafe { transmute(temp) },
            symbols_map: data.symbols.iter().cloned().enumerate().map(|(idx, sym)| (sym, idx)).collect(),
            dynamic_symbols: Vec::new()
        }
    }
}
