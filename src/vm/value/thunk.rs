use std::sync::RwLock;

use anyhow::{anyhow, Result};
use derive_more::{IsVariant, Unwrap};

use crate::bytecode::OpCodes;

use super::super::vm::VM;
use super::VmValue;

pub struct VmThunk(RwLock<_VmThunk>);

#[derive(IsVariant, Unwrap)]
enum _VmThunk {
    Code(OpCodes),
    SuspendedFrom(*const VmThunk),
    Value(VmValue),
}

impl VmThunk {
    pub fn new(opcodes: OpCodes) -> VmThunk {
        VmThunk(RwLock::new(_VmThunk::Code(opcodes)))
    }

    pub fn force(&mut self, vm: &VM) -> Result<VmValue> {
        todo!();
        /* match &*self.0.read().unwrap() {
            _VmThunk::Value(value) => return Ok(value.clone()),
            _VmThunk::SuspendedFrom(from) => {
                return Err(anyhow!(
                    "already suspended from {from:p} (infinite recursion encountered)"
                ))
            }
            _VmThunk::Code(_) => (),
        }
        let guard = &mut *self.0.write().unwrap();
        let opcodes = std::mem::replace(
            &mut *self.0.write().unwrap(),
            _VmThunk::SuspendedFrom(self as *const VmThunk),
        )
        .unwrap_code();
        let value = vm.eval(opcodes).unwrap();
        let _ = std::mem::replace(guard, _VmThunk::Value(value));
        if let _VmThunk::Value(value) = guard {
            Ok(value.clone())
        } else {
            unreachable!()
        } */
    }

    pub fn value(&self) -> Option<VmValue> {
        match &*self.0.read().unwrap() {
            _VmThunk::Value(value) => Some(value.clone()),
            _ => None,
        }
    }
}
