use std::sync::RwLock;

use derive_more::{IsVariant, Unwrap};
use anyhow::{Result, anyhow};

use crate::bytecode::OpCodes;

use super::value::*;
use super::vm::VM;

pub enum StackElem<'vm> {
    Thunk(VmThunk),
    Const(Const<'vm>),
    Value(Value)
}

pub struct VmThunk(RwLock<_VmThunk>);

#[derive(IsVariant, Unwrap)]
enum _VmThunk {
    Code(OpCodes),
    SuspendedFrom(*const VmThunk),
    Value(Value),
}

impl VmThunk {
    pub fn new(opcodes: OpCodes) -> VmThunk {
        VmThunk(RwLock::new(_VmThunk::Code(opcodes)))
    }

    pub fn force(&mut self, vm: &VM) -> Result<Value> {
        match &*self.0.read().unwrap() {
            _VmThunk::Value(value) => return Ok(value.clone()),
            _VmThunk::SuspendedFrom(from) => return Err(anyhow!("already suspended from {from:p} (infinite recursion encountered)")),
            _VmThunk::Code(_) => ()
        }
        let guard = &mut *self.0.write().unwrap();
        let opcodes = std::mem::replace(&mut *self.0.write().unwrap(), _VmThunk::SuspendedFrom(self as *const VmThunk)).unwrap_code();
        let value = vm.eval(opcodes).unwrap();
        let _ = std::mem::replace(guard, _VmThunk::Value(value));
        if let _VmThunk::Value(value) = guard {
            Ok(value.clone())
        } else {
            unreachable!()
        }
    }
}

