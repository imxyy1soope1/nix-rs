use anyhow::{anyhow, Result};

use crate::bytecode::*;

use super::value::*;
use super::data::*;

pub struct VM {
    consts: Consts,
    symbols: Symbols
}

impl VM {
    pub fn new(consts: Consts, symbols: Symbols) -> VM {
        VM {
            consts, symbols
        }
    }
}

impl VM {
    pub fn lookup_symbol(&self, index: usize) -> Option<&str> {
        self.symbols.get(index).map(|s| s.as_str())
    }

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
