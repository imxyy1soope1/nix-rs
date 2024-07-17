use std::cell::RefCell;
use std::collections::HashMap;
use std::sync::RwLock;

use anyhow::{anyhow, Result};
use rpds::{HashTrieMap, Vector};

use crate::bytecode::{self, *};
use crate::slice::*;
use crate::value::{self, Value};

use super::env::Env;
use super::stack::{Stack, STACK_SIZE};
use super::value::{self as vmValue, *};
use super::vmthunk::*;

pub fn run(prog: Program) -> Result<Value> {
    let vm = VM::new(prog.consts, prog.symbols, prog.thunks);
    Ok(vm.eval(prog.top_level, &mut Env::empty())?.to_value(&vm))
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
        let sym = sym.into();
        if let Some(idx) = self.map.get(&sym) {
            Symbol::new(*idx)
        } else {
            self.dynamic_symbols.borrow_mut().push(sym);
            Symbol::new(self.static_symbols.len() + self.dynamic_symbols.borrow().len() - 1)
        }
    }
}

pub struct VM {
    consts: Slice<value::Const>,
    thunks: Slice<VmThunk>,
    pub symbols: Symbols,
}

impl VM {
    fn new(consts: Consts, symbols: bytecode::Symbols, thunks: Thunks) -> Self {
        let consts = consts.into_iter().map(Into::into).collect();
        let symbols = Symbols::new(symbols);
        let thunks = thunks
            .into_iter()
            .map(|bytecode::Thunk { opcodes }| VmThunk::new(opcodes))
            .collect();
        VM {
            consts,
            thunks,
            symbols,
        }
    }

    pub fn get_const(&self, idx: usize) -> Result<value::Const> {
        self.consts.get(idx).cloned().ok_or(anyhow!(""))
    }

    pub fn get_thunk_value(&self, idx: usize, env: &mut Env) -> Result<VmValue> {
        self.thunks.get(idx).unwrap().force(self, env)
    }

    pub fn eval(&self, opcodes: OpCodes, env: &mut Env) -> Result<VmValue> {
        let mut stack = Stack::<STACK_SIZE>::new();
        let mut iter = opcodes.into_iter();
        while let Some(opcode) = iter.next() {
            let jmp = self.single_op(opcode, &mut stack, env)?;
            for _ in 0..jmp {
                iter.next().unwrap();
            }
        }
        assert_eq!(stack.len(), 1);
        stack.pop()
    }

    #[inline]
    fn single_op<const CAP: usize>(
        &self,
        opcode: OpCode,
        stack: &mut Stack<CAP>,
        env: &mut Env,
    ) -> Result<usize> {
        match opcode {
            OpCode::NoOp => (),
            OpCode::Const { idx } => stack.push(VmValue::Const(self.get_const(idx)?))?,
            OpCode::LoadThunk { idx } => stack.push(VmValue::Thunk(vmValue::Thunk::new(idx)))?,
            OpCode::LoadValue { idx } => {
                stack.push(self.get_thunk_value(idx, env)?)?;
            }
            OpCode::ForceValue => {
                stack.tos_mut()?.force(self, env)?;
            }
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
                    Upd => lhs.update(rhs),
                    _ => todo!(),
                })?;
            }
            OpCode::ConcatString => {
                let rhs = stack.pop()?;
                stack.tos_mut()?.concat_string(rhs);
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
                let sym = self.reg_sym_tos(stack)?;
                stack.tos_mut()?.push_attr(sym, val);
            }
            OpCode::Select { sym } => {
                stack.tos_mut()?.select(Symbol::new(sym)).force(self, env)?;
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
            OpCode::Var { sym } => {
                stack.push(env.lookup(Symbol::new(sym)))?;
            }
            OpCode::EnterEnv => {
                env.enter(stack.pop()?.unwrap_attr_set().to_data());
            }
            OpCode::LeaveEnv => {
                env.leave();
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

