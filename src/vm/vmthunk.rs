use std::cell::RefCell;
use std::sync::RwLock;

use anyhow::{anyhow, Result};
use derive_more::{IsVariant, Unwrap};

use crate::bytecode::OpCodes;

use super::vm::VM;
use super::env::Env;
use super::value::VmValue;

pub struct VmThunk {
    thunk: RefCell<_VmThunk>,
    lock: RwLock<()>
}

#[derive(IsVariant, Unwrap)]
enum _VmThunk {
    Code(OpCodes),
    SuspendedFrom(*const VmThunk),
    Value(VmValue),
}

impl VmThunk {
    pub fn new(opcodes: OpCodes) -> VmThunk {
        VmThunk {
            thunk: RefCell::new(_VmThunk::Code(opcodes)),
            lock: RwLock::new(())
        }
    }

    pub fn force(&self, vm: &VM, env: &mut Env) -> Result<VmValue> {
        {
            let _guard = self.lock.read().unwrap();
            match &*self.thunk.borrow() {
                _VmThunk::Value(value) => return Ok(value.clone()),
                _VmThunk::SuspendedFrom(from) => {
                    return Err(anyhow!(
                    "already suspended from {from:p} (infinite recursion encountered)"
                ))
                }
                _VmThunk::Code(_) => (),
            }
        }
        {
            let _guard = self.lock.write().unwrap();
            let opcodes = std::mem::replace(
                &mut *self.thunk.borrow_mut(),
                _VmThunk::SuspendedFrom(self as *const VmThunk),
            ).unwrap_code();
            let value = vm.eval(opcodes, env).unwrap();
            let _ = std::mem::replace(&mut *self.thunk.borrow_mut(), _VmThunk::Value(value.clone()));
            Ok(value)
        }
    }

    pub fn value(&self) -> Option<VmValue> {
        let _guard = self.lock.read();
        match &*self.thunk.borrow() {
            _VmThunk::Value(value) => Some(value.clone()),
            _ => None,
        }
    }
}
