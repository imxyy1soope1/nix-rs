pub enum OpCode {

}

pub(crate) struct Instruction {
    code: OpCode,
    args: Box<[u8]>,
}

impl Instruction {
    pub(crate) fn new(code: OpCode, args: Box<[u8]>) -> Instruction {
        Instruction { code, args }
    }
}
