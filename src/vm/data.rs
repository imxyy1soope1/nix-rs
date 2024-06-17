use std::pin::Pin;
use std::ptr::NonNull;
use std::sync::{Weak, Arc, RwLock};
use std::cell::RefCell;

use crate::bytecode::{OpCodes, ThunkIdx};
use crate::slice::Slice;

use super::value::*;
use super::vm::{VM, VmData};

pub enum StackElem<'vm> {
    Thunk(VmThunk<'vm>),
    Const(Const<'vm>),
}

pub struct VmRef<'data>(NonNull<VM<'data>>);

pub struct VmThunks<'vm>(Pin<Slice<RefCell<RwLock<VmThunk<'vm>>>>>);

type Deps = Slice<ThunkIdx>;

enum VmThunk<'data> {
    Code(OpCodes),
    Suspended,
    Value(Value<'data>),
}

impl<'data> VmThunk<'data> {
    pub fn new(vm: &'data VmData, opcodes: OpCodes) -> VmThunk<'data> {
        VmThunk::Code(opcodes)
    }

    pub fn force<'a>(&'a mut self, vm: &VM) -> &'a Value<'data> {
        let thunk =  *self;
        *self = VmThunk::Suspended;
        match self {
            VmThunk::Code(code) => vm
        }
    }
}

