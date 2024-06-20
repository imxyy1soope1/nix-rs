use std::sync::RwLock;

use derive_more::IsVariant;
use anyhow::{Result, anyhow};

use crate::bytecode::{OpCodes, ThunkIdx, Thunks, Thunk};

use super::value::*;
use super::vm::VM;

pub enum StackElem<'vm> {
    Thunk(VmThunk),
    Const(Const<'vm>),
    Value(Value)
}

pub struct VmThunk(RwLock<_VmThunk>);

#[derive(IsVariant)]
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
        if let _VmThunk::Value(ref value) = &*self.0.read().unwrap() {
            return Ok(value.clone())
        }
        let guard = &mut *self.0.write().unwrap();
        #[allow(unused)]
        let _VmThunk::Code(opcodes) = std::mem::replace(&mut *self.0.write().unwrap(), _VmThunk::SuspendedFrom(self as *const VmThunk)) else {
            return Err(anyhow!("infinite recursion occured"))
        };
        let value = vm.eval(opcodes).unwrap();
        let _ = std::mem::replace(guard, _VmThunk::Value(value));
        if let _VmThunk::Value(value) = guard {
            Ok(value.clone())
        } else {
            unreachable!()
        }
    }
}

