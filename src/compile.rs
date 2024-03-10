use crate::bytecode::*;
use crate::expr::*;

pub trait Compile {
    fn compile(&self) -> Vec<Instruction>;
}

impl Compile for OpNotExpr {
    fn compile(&self) -> Vec<Instruction> {
        let mut inst = self.right.compile();
        inst.push(Instruction::new(Code::Not, vec![]));
        inst
    }
}

impl Compile for OpNegExpr {
    fn compile(&self) -> Vec<Instruction> {
        let mut inst = self.right.compile();
        inst.push(Instruction::new(Code::Neg, vec![]));
        inst
    }
}

impl Compile for OpAddExpr {
    fn compile(&self) -> Vec<Instruction> {
        let mut inst = self.left.compile();
        inst.append(&mut self.right.compile());
        inst.push(Instruction::new(Code::Add, vec![]));
        inst
    }
}

impl Compile for OpSubExpr {
    fn compile(&self) -> Vec<Instruction> {
        let mut inst = self.left.compile();
        inst.append(&mut self.right.compile());
        inst.push(Instruction::new(Code::Neg, vec![]));
        inst.push(Instruction::new(Code::Add, vec![]));
        inst
    }
}

impl Compile for OpMulExpr {
    fn compile(&self) -> Vec<Instruction> {
        let mut inst = self.left.compile();
        inst.append(&mut self.right.compile());
        inst.push(Instruction::new(Code::Mul, vec![]));
        inst
    }
}

impl Compile for OpDivExpr {
    fn compile(&self) -> Vec<Instruction> {
        let mut inst = self.left.compile();
        inst.append(&mut self.right.compile());
        inst.push(Instruction::new(Code::Div, vec![]));
        inst
    }
}

impl Compile for OpAndExpr {
    fn compile(&self) -> Vec<Instruction> {
        let mut inst = self.left.compile();
        inst.append(&mut self.right.compile());
        inst.push(Instruction::new(Code::And, vec![]));
        inst
    }
}

impl Compile for OpOrExpr {
    fn compile(&self) -> Vec<Instruction> {
        let mut inst = self.left.compile();
        inst.append(&mut self.right.compile());
        inst.push(Instruction::new(Code::Or, vec![]));
        inst
    }
}

impl Compile for OpImplExpr {
    fn compile(&self) -> Vec<Instruction> {
        let mut inst = self.left.compile();
        inst.push(Instruction::new(Code::Not, vec![]));
        inst.append(&mut self.right.compile());
        inst.push(Instruction::new(Code::Or, vec![]));
        inst
    }
}

impl Compile for OpEqExpr {
    fn compile(&self) -> Vec<Instruction> {
        let mut inst = self.left.compile();
        inst.append(&mut self.right.compile());
        inst.push(Instruction::new(Code::Eq, vec![]));
        inst
    }
}

impl Compile for OpNeqExpr {
    fn compile(&self) -> Vec<Instruction> {
        let mut inst = self.left.compile();
        inst.append(&mut self.right.compile());
        inst.push(Instruction::new(Code::Eq, vec![]));
        inst.push(Instruction::new(Code::Not, vec![]));
        inst
    }
}

impl Compile for OpLtExpr {
    fn compile(&self) -> Vec<Instruction> {
        let mut inst = self.left.compile();
        inst.append(&mut self.right.compile());
        inst.push(Instruction::new(Code::Lt, vec![]));
        inst
    }
}

impl Compile for OpGtExpr {
    fn compile(&self) -> Vec<Instruction> {
        let mut inst = self.right.compile();
        inst.append(&mut self.left.compile());
        inst.push(Instruction::new(Code::Lt, vec![]));
        inst
    }
}

impl Compile for OpLeqExpr {
    fn compile(&self) -> Vec<Instruction> {
        let mut inst = self.right.compile();
        inst.append(&mut self.left.compile());
        inst.push(Instruction::new(Code::Lt, vec![]));
        inst.push(Instruction::new(Code::Not, vec![]));
        inst
    }
}

impl Compile for OpGeqExpr {
    fn compile(&self) -> Vec<Instruction> {
        let mut inst = self.left.compile();
        inst.append(&mut self.right.compile());
        inst.push(Instruction::new(Code::Lt, vec![]));
        inst.push(Instruction::new(Code::Not, vec![]));
        inst
    }
}

impl Compile for IdentExpr {
    fn compile(&self) -> Vec<Instruction> {
        unimplemented!()
    }
}

impl Compile for IntExpr {
    fn compile(&self) -> Vec<Instruction> {
        unimplemented!()
    }
}

impl Compile for FloatExpr {
    fn compile(&self) -> Vec<Instruction> {
        unimplemented!()
    }
}

impl Compile for StringExpr {
    fn compile(&self) -> Vec<Instruction> {
        unimplemented!()
    }
}

impl Compile for PathExpr {
    fn compile(&self) -> Vec<Instruction> {
        unimplemented!()
    }
}

impl Compile for SearchPathExpr {
    fn compile(&self) -> Vec<Instruction> {
        unimplemented!()
    }
}

impl Compile for FuncExpr {
    fn compile(&self) -> Vec<Instruction> {
        unimplemented!()
    }
}

impl Compile for CallExpr {
    fn compile(&self) -> Vec<Instruction> {
        unimplemented!()
    }
}

impl Compile for IfExpr {
    fn compile(&self) -> Vec<Instruction> {
        unimplemented!()
    }
}

impl Compile for AttrsExpr {
    fn compile(&self) -> Vec<Instruction> {
        unimplemented!()
    }
}

impl Compile for ListExpr {
    fn compile(&self) -> Vec<Instruction> {
        unimplemented!()
    }
}

impl Compile for LetExpr {
    fn compile(&self) -> Vec<Instruction> {
        unimplemented!()
    }
}

impl Compile for WithExpr {
    fn compile(&self) -> Vec<Instruction> {
        unimplemented!()
    }
}

impl Compile for AssertExpr {
    fn compile(&self) -> Vec<Instruction> {
        unimplemented!()
    }
}
