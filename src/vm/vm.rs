use std::collections::HashMap;
use std::sync::Arc;

use rayon::{ThreadPool, ThreadPoolBuilder};
use anyhow::{anyhow, Result};

use crate::bytecode::*;
use crate::bytecode::Thunk as ByteCodeThunk;
use crate::slice::*;

use super::data::*;
use super::value::*;

pub async fn run(prog: Program) -> Result<OwnedValue> {
    let vm = VM::new(prog.consts, prog.symbols, prog.thunks);
    Ok(vm.eval(prog.top_level).await?.to_owned_value(&vm))
}

pub struct VM {
    consts: Consts,
    thunks: Slice<Arc<VmThunk>>,
    symbols: Symbols,
    symbols_map: HashMap<String, SymIdx>,
    dynamic_symbols: Vec<String>,
    pool: ThreadPool
}


impl VM {
    fn new(consts: Consts, symbols: Symbols, thunks: Thunks) -> Self {
        let symbols_map = symbols.clone().into_iter().enumerate().map(|(idx, sym)| (sym, idx)).collect();
        let thunks = thunks.into_iter().map(|ByteCodeThunk { opcodes }| Arc::new(VmThunk::new(opcodes))).collect();
        let pool = ThreadPoolBuilder::new().num_threads(num_cpus::get()).spawn_handler(|thread| {
            tokio::task::spawn_blocking(|| thread.run());
            Ok(())
        }).build().unwrap();
        VM {
            consts,
            thunks,
            symbols,
            symbols_map,
            dynamic_symbols: Vec::new(),
            pool
        }
    }

    pub async fn eval(&self, opcodes: OpCodes) -> Result<Value> {

    }
}
