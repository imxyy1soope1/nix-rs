use std::collections::HashMap;

use anyhow::{Result, anyhow};

use crate::bytecode::*;

use super::data::*;

pub struct VM {
    codes: Vec<OpCode>,
    consts: Box<[Const]>,
    syms: Vec<String>,
    syms_table: HashMap<String, SymIdx>,
    callstack: Vec<usize>,
    stack: Vec<Value>,
}

impl VM {
    pub fn new(prog: Program) -> VM {
        let syms = prog.syms.into_vec();
        VM {
            codes: prog.codes.into_vec(),
            consts: prog.consts,
            syms: syms.clone(),
            syms_table: syms.into_iter().enumerate().map(|(idx, sym)| (sym, idx)).collect::<HashMap<String, SymIdx>>(),
            callstack: Vec::new(),
            stack: Vec::new(),
        }
    }
}

impl VM {


    pub fn run(mut self) -> Result<Value> {
        let codes = self.codes;
        let mut iter: Box<dyn Iterator<Item = &OpCode>> = Box::new(codes.iter());

        loop {
            let code = iter.next().ok_or(anyhow!("incomplete bytecode"))?;
            match *code {
                OpCode::Const { idx } => self.stack.push(Value::Const(idx)),
                OpCode::Thunk { idx } => self.stack.push(Value::ThunkCode(idx)),
                OpCode::ThunkValue { idx } => {
                    let thunk = codes[idx..].iter();
                    iter = Box::new(thunk.take_while(|code| !matches!(code, OpCode::Ret)).chain(iter));
                },
                OpCode::ForceValue => {
                    
                }

                OpCode::Ret => break self.stack.pop().ok_or(anyhow!("stack empty")),
                _ => unimplemented!()
            }
        }
    }

    /* fn try_pop(&mut self) -> Result<Value> {
        self.stack.pop().ok_or(anyhow!("stack empty"))
    } */
}
