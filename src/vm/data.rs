use std::cell::RefCell;
use std::sync::Arc;

use crate::bytecode::OpCodes;
use crate::slice::Slice;

use super::value::*;

pub enum StackElem<'vm> {
    Thunk(VmThunk<'vm>),
    Const(Const<'vm>),
}

pub type VmThunks<'vm> = Slice<VmThunk<'vm>>;
pub struct VmThunk<'vm>(RefCell<_Thunk<'vm>>);

enum _Thunk<'vm> {
    Code(OpCodes),
    Value(Value<'vm>),
}
