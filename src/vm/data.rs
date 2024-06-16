use std::sync::{Weak, Arc, RwLock};
use std::cell::RefCell;

use crate::bytecode::{OpCodes, ThunkIdx};
use crate::slice::Slice;

use super::value::*;
use super::vm::VmData;

pub enum StackElem<'vm> {
    Thunk(VmThunk<'vm>),
    Const(Const<'vm>),
}

pub type VmThunks<'vm> = Slice<Weak<RefCell<VmThunk<'vm>>>>;
pub struct VmThunk<'vm>(RwLock<_Thunk<'vm>>);

impl<'data> VmThunk<'data> {
    pub fn new(vm: &'data VmData, deps: Slice<Arc<VmThunk<'data>>>, opcodes: OpCodes) -> VmThunk<'data> {
        VmThunk(RwLock::new(_Thunk::Code { deps, opcodes }))
    }
}

enum _Thunk<'vm> {
    Code { deps: Slice<Arc<VmThunk<'vm>>>, opcodes: OpCodes },
    Value(Value<'vm>),
}
