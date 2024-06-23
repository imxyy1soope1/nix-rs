use std::collections::HashMap;
use std::sync::Arc;

use anyhow::{anyhow, Result};

use crate::bytecode::*;
use crate::bytecode::Thunk as ByteCodeThunk;
use crate::slice::*;
use crate::value::{Value, Const as ValueConst};

use super::stack::{Stack, STACK_SIZE};
use super::value::*;

pub fn run(prog: Program) -> Result<Value> {
    let vm = VM::new(prog.consts, prog.symbols, prog.thunks);
    Ok(vm.eval(prog.top_level)?.to_value(&vm))
}

pub struct VM {
    consts: Slice<ValueConst>,
    thunks: Slice<Arc<VmThunk>>,
    symbols: Symbols,
    symbols_map: HashMap<String, SymIdx>,
    dynamic_symbols: Vec<String>,
    // pool: ThreadPool
}


impl VM {
    fn new(consts: Consts, symbols: Symbols, thunks: Thunks) -> Self {
        let symbols_map = symbols.clone().into_iter().enumerate().map(|(idx, sym)| (sym, idx)).collect();
        let thunks = thunks.into_iter().map(|ByteCodeThunk { opcodes }| Arc::new(VmThunk::new(opcodes))).collect();
        /* let pool = ThreadPoolBuilder::new().num_threads(num_cpus::get()).spawn_handler(|thread| {
            tokio::task::spawn_blocking(|| thread.run());
            Ok(())
        }).build().unwrap(); */
        let consts = consts.into_iter().map(Into::into).collect();
        VM {
            consts,
            thunks,
            symbols,
            symbols_map,
            dynamic_symbols: Vec::new(),
            // pool
        }
    }

    pub fn get_const(&self, idx: usize) -> Result<ValueConst> {
        self.consts.get(idx).cloned().ok_or(anyhow!(""))
    }

    pub fn eval(&self, opcodes: OpCodes) -> Result<VmValue> {
        let mut stack = Stack::<STACK_SIZE>::new();
        let mut iter = opcodes.into_iter();
        while let Some(opcode) = iter.next() {
            let jmp = self.single_op(opcode, &mut stack)?;
            for _ in 0..jmp {
                iter.next().unwrap();
            }
        }
        assert_eq!(stack.len(), 1);
        stack.pop()
    }

    #[inline]
    fn single_op<const CAP: usize>(&self, opcode: OpCode, stack: &mut Stack<CAP>) -> Result<usize> {
        match opcode {
            OpCode::Const { idx } => stack.push(VmValue::Const(self.get_const(idx)?))?,
            OpCode::Jmp { step } => return Ok(step),
            OpCode::UnOp { op } => {
                let value = stack.pop()?;
                match op {
                    UnOp::Neg => stack.push(value.neg()),
                    UnOp::Not => stack.push(value.not())
                }?
            }
            _ => unimplemented!()
        }
        Ok(0)
    }
}
