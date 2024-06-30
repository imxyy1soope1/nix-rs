use std::cell::RefCell;
use std::collections::HashMap;
use std::sync::{Arc, RwLock};

use anyhow::{anyhow, Result};
use rpds::{HashTrieMap, Vector};

use crate::bytecode::{self, *};
use crate::slice::*;
use crate::value::{self, Value};

use super::stack::{Stack, STACK_SIZE};
use super::value::*;

pub fn run(prog: Program) -> Result<Value> {
    let vm = VM::new(prog.consts, prog.symbols, prog.thunks);
    Ok(vm.eval(prog.top_level)?.to_value(&vm))
}

pub struct Symbols {
    static_symbols: Slice<value::Symbol>,
    dynamic_symbols: RefCell<Vec<value::Symbol>>,
    map: HashMap<value::Symbol, SymIdx>,
    lock: RwLock<()>,
}

impl Symbols {
    fn new(static_symbols: bytecode::Symbols) -> Symbols {
        let static_symbols: Slice<value::Symbol> =
            static_symbols.into_iter().map(|sym| sym.into()).collect();
        let map = static_symbols
            .iter()
            .cloned()
            .enumerate()
            .map(|(idx, sym)| (sym, idx))
            .collect();
        Symbols {
            static_symbols,
            map,
            dynamic_symbols: RefCell::new(Vec::new()),
            lock: RwLock::new(()),
        }
    }

    pub fn get(&self, idx: SymIdx) -> Result<value::Symbol> {
        let _guard = self.lock.read().unwrap();
        if idx < self.static_symbols.len() {
            self.static_symbols
                .get(idx)
                .cloned()
                .ok_or_else(|| unreachable!())
        } else {
            self.dynamic_symbols
                .borrow()
                .get(idx)
                .cloned()
                .ok_or(anyhow!("internal error"))
        }
    }

    pub fn reg(&self, sym: impl Into<value::Symbol>) -> Symbol {
        let _guard = self.lock.write().unwrap();
        self.dynamic_symbols.borrow_mut().push(sym.into());
        Symbol::new(self.static_symbols.len() + self.dynamic_symbols.borrow().len() - 1)
    }
}

pub struct VM {
    consts: Slice<value::Const>,
    thunks: Slice<Arc<VmThunk>>,
    pub symbols: Symbols,
    // pool: ThreadPool
}

impl VM {
    fn new(consts: Consts, symbols: bytecode::Symbols, thunks: Thunks) -> Self {
        let symbols = Symbols::new(symbols);
        let thunks = thunks
            .into_iter()
            .map(|bytecode::Thunk { opcodes }| Arc::new(VmThunk::new(opcodes)))
            .collect();
        /* let pool = ThreadPoolBuilder::new().num_threads(num_cpus::get()).spawn_handler(|thread| {
            tokio::task::spawn_blocking(|| thread.run());
            Ok(())
        }).build().unwrap(); */
        let consts = consts.into_iter().map(Into::into).collect();
        VM {
            consts,
            thunks,
            symbols,
            // pool
        }
    }

    pub fn get_const(&self, idx: usize) -> Result<value::Const> {
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
            OpCode::NoOp => (),
            OpCode::Const { idx } => stack.push(VmValue::Const(self.get_const(idx)?))?,
            OpCode::Jmp { step } => return Ok(step),
            OpCode::JmpIfTrue { step } => {
                if let VmValue::Const(value::Const::Bool(true)) = stack.pop()? {
                    return Ok(step);
                }
            }
            OpCode::JmpIfFalse { step } => {
                if let VmValue::Const(value::Const::Bool(false)) = stack.pop()? {
                    return Ok(step);
                }
            }
            OpCode::UnOp { op } => {
                use UnOp::*;
                let value = stack.pop()?;
                stack.push(match op {
                    Neg => value.neg(),
                    Not => value.not(),
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
                    Con => lhs.concat(rhs),
                    _ => todo!(),
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
                let sym = self
                    .symbols
                    .reg(sym.unwrap_const().unwrap_string().to_string());
                stack.tos_mut()?.push_attr(sym, val);
            }
            OpCode::Select { sym } => {
                stack.tos_mut()?.select(Symbol::new(sym));
            }
            OpCode::SelectWithDefault { sym } => {
                let default = stack.pop()?;
                stack
                    .tos_mut()?
                    .select_with_default(Symbol::new(sym), default.clone());
            }
            OpCode::SelectOrFalse { sym } => {
                stack.tos_mut()?.select_with_default(
                    Symbol::new(sym),
                    VmValue::Const(value::Const::Bool(false)),
                );
            }
            OpCode::SelectDynamic => {
                let sym = self.reg_sym_tos(stack)?;
                stack.tos_mut()?.select(sym);
            }
            OpCode::SelectDynamicWithDefault => {
                let sym = self.reg_sym_tos(stack)?;
                let default = stack.pop()?;
                stack.tos_mut()?.select_with_default(sym, default.clone());
            }
            OpCode::SelectDynamicOrFalse => {
                let sym = self.reg_sym_tos(stack)?;
                stack
                    .tos_mut()?
                    .select_with_default(sym, VmValue::Const(value::Const::Bool(false)));
            }
            OpCode::HasAttr { sym } => {
                stack.tos_mut()?.has_attr(Symbol::new(sym));
            }
            OpCode::HasDynamicAttr => {
                let sym = self.reg_sym_tos(stack)?;
                stack.tos_mut()?.has_attr(sym);
            }
            _ => todo!(),
        }
        Ok(0)
    }

    fn reg_sym_tos<const CAP: usize>(&self, stack: &mut Stack<CAP>) -> Result<Symbol> {
        let mut val = stack.pop()?;
        val.coerce_to_string();
        let sym = self
            .symbols
            .reg(val.unwrap_const().unwrap_string().to_string());
        Ok(sym)
    }
}
