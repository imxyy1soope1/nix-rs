use anyhow::{anyhow, Result};

use crate::bytecode::*;

use super::data::*;
use super::value::*;

pub struct VM {
    consts: Consts,
    symbols: Symbols,
    thunks: Thunks,
    top_level: OpCodes,
}

impl VM {
    pub fn new(prog: Program) -> VM {
        VM {
            consts: prog.consts,
            symbols: prog.symbols,
            thunks: prog.thunks,
            top_level: prog.top_level
        }
    }
}

impl VM {
    /* pub fn run(mut self) -> Result<Value> {
        // self.iter = Some(Box::new(self.prog.codes.iter().map(NonNull::from)));
        let thunk_idxs = &self.prog.thunk_idxs;
        // let mut iter: Box<dyn Iterator<Item = &OpCode>> = Box::new(codes.iter());

        loop {
            let (ip, end) = self.callstack.last_mut().unwrap();
            let code = self.prog.codes[*ip];
            *ip += 1;
            if *ip == *end {
                self.callstack.pop();
            }
            match code {
                OpCode::Const { idx } => self.stack.push(StackElem::Const(idx)),
                OpCode::Thunk { idx } => self.stack.push(Value::ThunkCode {
                    start: thunk_idxs[idx] + 1,
                    end: thunk_idxs[idx + 1],
                }),
                OpCode::LoadThunk { idx } => self.stack.push(Value::ThunkCode {
                    start: thunk_idxs[idx],
                    end: thunk_idxs[idx + 1],
                }),
                OpCode::LoadValue { idx } => {
                    self.callstack.push((thunk_idxs[idx], thunk_idxs[idx + 1]));
                }
                OpCode::ForceValue => {
                    let thunk = try_pop!(self)?;
                    match thunk {
                        Value::ThunkCode { start, end } => self.callstack.push((start, end)),
                        value => self.stack.push(value),
                    }
                }

                OpCode::Ret => {
                    self.callstack.pop();
                    if self.callstack.is_empty() {
                        break try_pop!(self);
                    }
                }
                _ => unimplemented!(),
            }
        }
    } */
}
