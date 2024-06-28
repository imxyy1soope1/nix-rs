use std::collections::HashMap;
use std::sync::Arc;

use anyhow::{anyhow, Result};
use rpds::{Vector, HashTrieMap};

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
    symbols: Slice<Arc<String>>,
    symbols_map: HashMap<Arc<String>, SymIdx>,
    dynamic_symbols: Vec<Arc<String>>,
    // pool: ThreadPool
}


impl VM {
    fn new(consts: Consts, symbols: Symbols, thunks: Thunks) -> Self {
        let symbols = symbols.into_iter().map(|sym| sym.into()).collect::<Slice<Arc<String>>>();
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

    pub fn get_sym(&self, idx: usize) -> Result<Arc<String>> {
        if idx < self.symbols.len() {
            self.symbols.get(idx).cloned().ok_or_else(|| unreachable!())
        } else {
            self.dynamic_symbols.get(idx).cloned().ok_or(anyhow!("internal error"))
        }
    }

    pub fn reg_sym(&mut self, sym: String) -> usize {
        let sym = Arc::new(sym);
        let idx = self.symbols_map.len();
        self.symbols_map.insert(sym.clone(), idx);
        self.dynamic_symbols.push(sym);
        idx
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
            OpCode::NoOp => (),
            OpCode::Const { idx } => stack.push(VmValue::Const(self.get_const(idx)?))?,
            OpCode::Jmp { step } => return Ok(step),
            OpCode::JmpIfTrue { step } => {
                if let VmValue::Const(ValueConst::Bool(true)) = stack.pop()? {
                    return Ok(step);
                }
            },
            OpCode::JmpIfFalse { step } => {
                if let VmValue::Const(ValueConst::Bool(false)) = stack.pop()? {
                    return Ok(step);
                }
            },
            OpCode::UnOp { op } => {
                use UnOp::*;
                let value = stack.pop()?;
                stack.push(match op {
                    Neg => value.neg(),
                    Not => value.not()
                })?;
            }
            OpCode::BinOp { op } => {
                use BinOp::*;
                let rhs = stack.pop()?;
                let lhs = stack.pop()?;
                stack.push(match op {
                    Add => lhs.add(rhs),
                    Mul => lhs.mul(rhs),
                    Div => lhs.div(rhs),
                    And => lhs.and(rhs),
                    Or => lhs.or(rhs),
                    Eq => lhs.eq(rhs),
                    _ => todo!()
                })?;
            }
            OpCode::List => {
                stack.push(VmValue::List(List::new(Vector::new_sync())))?;
            }
            OpCode::PushElem => {
                let elem = stack.pop()?;
                stack.tos_mut()?.push(elem);
            }
            OpCode::AttrSet => {
                stack.push(VmValue::AttrSet(AttrSet::new(HashTrieMap::new_sync())))?;
            }
            OpCode::PushStaticAttr { name } => {
                let val = stack.pop()?;
                stack.tos_mut()?.push_attr(Symbol::new(name), val);
            }
            OpCode::PushDynamicAttr => {
                let val = stack.pop()?;
                let mut sym = stack.pop()?;
                sym.coerce_to_string();
                let sym = self.reg_sym(sym.unwrap_const().unwrap_string().into());
                stack.tos_mut()?.push_attr(Symbol::new(sym), val);
            }
            _ => todo!()
        }
        Ok(0)
    }
}
