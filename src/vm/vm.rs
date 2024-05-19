use std::collections::HashMap;

use anyhow::{anyhow, Result};

use crate::bytecode::*;

use super::data::*;

pub struct VM {
    prog: Program,
    syms_table: HashMap<String, SymIdx>,
    callstack: Vec<(usize, usize)>,
    stack: Vec<Value>,
}

impl VM {
    pub fn new(prog: Program) -> VM {
        let syms = prog.syms.clone().into_vec();
        VM {
            callstack: vec![(0, prog.codes.len())],
            prog,
            syms_table: syms
                .into_iter()
                .enumerate()
                .map(|(idx, sym)| (sym, idx))
                .collect::<HashMap<String, SymIdx>>(),
            stack: Vec::new(),
        }
    }
}

macro_rules! try_pop {
    ($self:ident) => {
        $self.stack.pop().ok_or(anyhow!("stack empty"))
    };
}

impl VM {
    pub fn run(mut self) -> Result<Value> {
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
                OpCode::Const { idx } => self.stack.push(Value::Const { idx }),
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
    }
}
