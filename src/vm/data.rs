use crate::bytecode::{CodeIdx, ConstIdx, ThunkIdx};

pub enum Value {
    /// code[start..end]
    ThunkCode {
        start: CodeIdx,
        end: CodeIdx,
    },
    ThunkValue {
        idx: ThunkIdx,
    },
    Const {
        idx: ConstIdx,
    },
}
