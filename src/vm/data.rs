use crate::bytecode::{ConstIdx, ThunkIdx};

pub type CodeIdx = usize;

pub enum Value {
    ThunkCode(ThunkIdx),
    ThunkValue(ThunkIdx),
    Const(ConstIdx),
}
