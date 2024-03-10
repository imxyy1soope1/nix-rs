#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum Code {
    Load = 0,
    Pop,

    Neg,
    Add,
    Mul,
    Div,

    Not,
    And,
    Or,

    Eq,
    Lt,

    Call,
    Ret,
}

pub struct Instruction {
    code: Code,
    args: Vec<u8>,
}

impl Instruction {
    pub fn new(code: Code, args: Vec<u8>) -> Instruction {
        Instruction { code, args }
    }

    pub fn dump(&self) -> Vec<u8> {
        match self.code {
            Code::Load => unsafe {
                vec![
                    Code::Load as u8,
                    *self.args.get_unchecked(0),
                    *self.args.get_unchecked(1),
                    *self.args.get_unchecked(2),
                    *self.args.get_unchecked(3),
                ]
            },
            Code::Ret => unsafe {
                vec![
                    Code::Ret as u8,
                    *self.args.get_unchecked(0),
                    *self.args.get_unchecked(1),
                    *self.args.get_unchecked(2),
                    *self.args.get_unchecked(3),
                ]
            },
            code => vec![code as u8],
        }
    }
}
