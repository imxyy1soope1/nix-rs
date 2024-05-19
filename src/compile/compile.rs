use crate::bytecode::*;

use super::ir;

pub struct CompileState {
    opcodes: Vec<OpCode>,
}

impl CompileState {
    fn new() -> Self {
        Self {
            opcodes: Vec::new(),
        }
    }

    fn push(&mut self, code: OpCode) {
        self.opcodes.push(code)
    }
}

pub trait Compile {
    fn compile(self, state: &mut CompileState) -> usize;
}

impl Compile for ir::Ir {
    fn compile(self, state: &mut CompileState) -> usize {
        todo!()
    }
}

impl Compile for ir::Const {
    fn compile(self, state: &mut CompileState) -> usize {
        state.push(OpCode::Const { idx: self.idx });
        1
    }
}

impl Compile for ir::Var {
    fn compile(self, state: &mut CompileState) -> usize {
        state.push(OpCode::Var { sym: self.sym });
        1
    }
}

impl Compile for ir::Thunk {
    fn compile(self, state: &mut CompileState) -> usize {
        state.push(OpCode::LoadThunk { idx: self.idx });
        1
    }
}

impl Compile for ir::Attrs {
    fn compile(self, state: &mut CompileState) -> usize {
        todo!()
    }
}

impl Compile for ir::List {
    fn compile(self, state: &mut CompileState) -> usize {
        todo!()
    }
}

impl Compile for ir::UnOp {
    fn compile(self, state: &mut CompileState) -> usize {
        let rhs = self.rhs.compile(state);
        use ir::UnOpKind::*;
        let op = match self.kind {
            Neg => UnOp::Neg,
            Not => UnOp::Not,
        };
        state.push(OpCode::UnOp { op });
        rhs + 1
    }
}

impl Compile for ir::BinOp {
    fn compile(self, state: &mut CompileState) -> usize {
        use ir::BinOpKind::*;
        match self.kind {
            Add => {
                let lhs = self.lhs.compile(state);
                let rhs = self.rhs.compile(state);
                state.push(OpCode::BinOp { op: BinOp::Add });
                lhs + rhs + 1
            }
            Mul => {
                let lhs = self.lhs.compile(state);
                let rhs = self.rhs.compile(state);
                state.push(OpCode::BinOp { op: BinOp::Mul });
                lhs + rhs + 1
            }
            Div => {
                let lhs = self.lhs.compile(state);
                let rhs = self.rhs.compile(state);
                state.push(OpCode::BinOp { op: BinOp::Div });
                lhs + rhs + 1
            }
            And => {
                let lhs = self.lhs.compile(state);
                let rhs = self.rhs.compile(state);
                state.push(OpCode::BinOp { op: BinOp::And });
                lhs + rhs + 1
            }
            Or => {
                let lhs = self.lhs.compile(state);
                let rhs = self.rhs.compile(state);
                state.push(OpCode::BinOp { op: BinOp::Or });
                lhs + rhs + 1
            }
            Eq => {
                let lhs = self.lhs.compile(state);
                let rhs = self.rhs.compile(state);
                state.push(OpCode::BinOp { op: BinOp::Eq });
                lhs + rhs + 1
            }
            Lt => {
                let lhs = self.lhs.compile(state);
                let rhs = self.rhs.compile(state);
                state.push(OpCode::BinOp { op: BinOp::Lt });
                lhs + rhs + 1
            }
            Con => {
                let lhs = self.lhs.compile(state);
                let rhs = self.rhs.compile(state);
                state.push(OpCode::BinOp { op: BinOp::Con });
                lhs + rhs + 1
            }
            Upd => {
                let lhs = self.lhs.compile(state);
                let rhs = self.rhs.compile(state);
                state.push(OpCode::BinOp { op: BinOp::Upd });
                lhs + rhs + 1
            }

            Sub => {
                let lhs = self.lhs.compile(state);
                let rhs = self.rhs.compile(state);
                state.push(OpCode::UnOp { op: UnOp::Neg });
                state.push(OpCode::BinOp { op: BinOp::Add });
                lhs + rhs + 2
            }
            Impl => {
                let lhs = self.lhs.compile(state);
                state.push(OpCode::UnOp { op: UnOp::Not });
                let rhs = self.rhs.compile(state);
                state.push(OpCode::BinOp { op: BinOp::Or });
                lhs + rhs + 2
            }
            Neq => {
                let lhs = self.lhs.compile(state);
                let rhs = self.rhs.compile(state);
                state.push(OpCode::BinOp { op: BinOp::Eq });
                state.push(OpCode::UnOp { op: UnOp::Not });
                lhs + rhs + 2
            }
            Gt => {
                let rhs = self.rhs.compile(state);
                let lhs = self.lhs.compile(state);
                state.push(OpCode::BinOp { op: BinOp::Lt });
                lhs + rhs + 1
            }
            Leq => {
                let rhs = self.rhs.compile(state);
                let lhs = self.lhs.compile(state);
                state.push(OpCode::BinOp { op: BinOp::Lt });
                state.push(OpCode::UnOp { op: UnOp::Not });
                lhs + rhs + 2
            }
            Geq => {
                let lhs = self.lhs.compile(state);
                let rhs = self.rhs.compile(state);
                state.push(OpCode::BinOp { op: BinOp::Lt });
                state.push(OpCode::UnOp { op: UnOp::Not });
                lhs + rhs + 2
            }
        }
    }
}

impl Compile for ir::HasAttr {
    fn compile(self, state: &mut CompileState) -> usize {
        let mut length = self.lhs.compile(state);
        let arity = self.rhs.len();
        for attr in self.rhs {
            match attr {
                ir::Attr::Ident(sym) => state.push(OpCode::Sym { sym }),
                ir::Attr::Dynamic(dynamic) => {
                    length += dynamic.compile(state) + 1;
                    state.push(OpCode::RegSym);
                }
                ir::Attr::Str(string) => {
                    length += string.compile(state) + 1;
                    state.push(OpCode::RegSym);
                }
            }
        }
        state.push(OpCode::HasAttr { arity });
        length + 1
    }
}

impl Compile for ir::Select {
    fn compile(self, state: &mut CompileState) -> usize {
        let mut length = self.expr.compile(state);
        let arity = self.attrpath.len();
        for attr in self.attrpath {
            match attr {
                ir::Attr::Ident(sym) => state.push(OpCode::Sym { sym }),
                ir::Attr::Dynamic(dynamic) => {
                    length += dynamic.compile(state) + 1;
                    state.push(OpCode::RegSym);
                }
                ir::Attr::Str(string) => {
                    string.compile(state);
                    state.push(OpCode::RegSym);
                }
            }
        }
        match self.default {
            Some(default) => {
                length += default.compile(state);
                state.push(OpCode::SelectWithDefault { arity });
            }
            None => state.push(OpCode::SelectAttr { arity }),
        }
        length + 1
    }
}

impl Compile for ir::ConcatStrings {
    fn compile(self, state: &mut CompileState) -> usize {
        todo!()
    }
}

impl Compile for ir::If {
    fn compile(self, state: &mut CompileState) -> usize {
        todo!()
    }
}

impl Compile for ir::Let {
    fn compile(self, state: &mut CompileState) -> usize {
        todo!()
    }
}

impl Compile for ir::With {
    fn compile(self, state: &mut CompileState) -> usize {
        todo!()
    }
}

impl Compile for ir::Assert {
    fn compile(self, state: &mut CompileState) -> usize {
        let assertion = self.assertion.compile(state);
        state.push(OpCode::Assert);
        let expr = self.expr.compile(state);
        assertion + expr + 1
    }
}

impl Compile for ir::Func {
    fn compile(self, state: &mut CompileState) -> usize {
        todo!()
    }
}

impl Compile for ir::Path {
    fn compile(self, state: &mut CompileState) -> usize {
        todo!()
    }
}

impl Compile for ir::Call {
    fn compile(self, state: &mut CompileState) -> usize {
        todo!()
    }
}
