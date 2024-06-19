use std::mem::MaybeUninit;
use std::pin::Pin;
use std::sync::{Weak, Arc, RwLock};
use std::cell::RefCell;

use derive_more::{IsVariant, Unwrap};

use crate::bytecode::{OpCodes, ThunkIdx, Thunks, Thunk};
use crate::slice::Slice;

use super::value::*;
use super::vm::VM;

pub enum StackElem<'vm> {
    Thunk(VmThunk),
    Const(Const<'vm>),
    Value(Value)
}

pub struct VmThunks(Slice<Weak<RefCell<RwLock<VmThunk>>>>);

impl VmThunks {
    pub fn new(thunks: Thunks) -> VmThunks {
        let mut temp: Slice<Arc<RefCell<MaybeUninit<RwLock<_>>>>> = (0..thunks.len()).map(|_| Arc::new(None).collect();
        thunks.into_iter().enumerate().for_each(|(idx, Thunk { deps, opcodes })| {
            temp.get(idx).unwrap().borrow_mut().write(RwLock::new(VmThunk::new(opcodes)))
        })
        unsafe {
            VmThunks(std::mem::transmute(temp.iter().map(|thunk| Arc::downgrade(thunk)).collect()))
        }
    }
}

#[derive(IsVariant, Unwrap)]
pub enum VmThunk {
    Code{ deps: Slice<Arc<VmThunk>>, opcodes: OpCodes },
    Suspended,
    Value(Value),
}

impl VmThunk {
    pub fn new(deps: Slice<Arc<VmThunk>>, opcodes: OpCodes) -> VmThunk {
        VmThunk::Code { deps, opcodes }
    }

    pub fn force<'a>(&'a mut self, vm: &VM) -> &'a Value {
        if let VmThunk::Value(value) = self {
            return value
        }
        let code = std::mem::replace(self, VmThunk::Suspended).unwrap_code();
        let value = vm.eval(code).unwrap();
        let _ = std::mem::replace(self, VmThunk::Value(value));
        if let VmThunk::Value(value) = self {
            value
        } else {
            unreachable!()
        }
    }
}

