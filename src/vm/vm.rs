use crate::bytecode::*;

pub struct VM {
    code: Box<dyn Iterator<Item = OpCode>>,
}

impl VM {
    pub fn new(code: Into<dyn Iterator>) -> VM {
        VM {
            code: code.into()
        }
    }
}
