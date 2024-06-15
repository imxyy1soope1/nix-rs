use std::cell::RefCell;
use std::sync::Arc;

use crate::bytecode::{ConstIdx, OpCodes};

use super::value::*;

pub enum StackElem<'vm> {
    Thunk(Thunk<'vm>),
    Const(Const<'vm>),
}

pub struct Thunk<'vm>(RefCell<_Thunk<'vm>>);

enum _Thunk<'vm> {
    Code(OpCodes),
    Value(Value<'vm>),
}
