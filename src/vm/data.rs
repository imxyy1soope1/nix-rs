use std::cell::RefCell;
use std::sync::Arc;

use crate::bytecode::{ConstIdx, OpCodes};
use crate::value::Value;

pub enum StackElem {
    Thunk(Arc<Thunk>),
    Const(ConstIdx)
}

pub struct Thunk(RefCell<_Thunk>);

enum _Thunk {
    Code(OpCodes),
    Value(Value)
}
