use crate::bytecode::*;

use super::ir;

pub struct CompileState {
    opcodes: Vec<OpCode>,
}

pub fn compile(downgraded: ir::Downgraded) -> Program {
    Program {
        top_level: CompileState::new().compile(downgraded.top_level),
        thunks: downgraded
            .thunks
            .into_iter()
            .map(|thunk| Thunk {
                opcodes: CompileState::new().compile(thunk),
            })
            .collect(),
        consts: downgraded.consts,
        symbols: downgraded.symbols,
    }
}

impl CompileState {
    fn new() -> Self {
        Self {
            opcodes: Vec::new(),
        }
    }

    fn compile(mut self, ir: ir::Ir) -> OpCodes {
        ir.compile(&mut self);
        self.opcodes()
    }

    fn push(&mut self, code: OpCode) {
        self.opcodes.push(code);
    }

    fn idx(&self) -> usize {
        self.opcodes.len()
    }

    fn modify(&mut self, idx: usize, code: OpCode) {
        self.opcodes[idx] = code;
    }

    fn last(&self) -> Option<OpCode> {
        self.opcodes.last().copied()
    }

    fn pop(&mut self) -> Option<OpCode> {
        self.opcodes.pop()
    }

    fn opcodes(self) -> OpCodes {
        self.opcodes.into()
    }
}

pub trait Compile {
    fn compile(self, state: &mut CompileState);
}

pub trait CompileWithLength {
    fn compile_with_length(self, state: &mut CompileState) -> usize;
}

impl<T: Compile> CompileWithLength for T {
    fn compile_with_length(self, state: &mut CompileState) -> usize {
        let start = state.idx();
        self.compile(state);
        let end = state.idx();
        end - start
    }
}

impl Compile for ir::Const {
    fn compile(self, state: &mut CompileState) {
        state.push(OpCode::Const { idx: self.idx });
    }
}

impl Compile for ir::Var {
    fn compile(self, state: &mut CompileState) {
        state.push(OpCode::Var { sym: self.sym });
    }
}

impl Compile for ir::Thunk {
    fn compile(self, state: &mut CompileState) {
        state.push(OpCode::LoadThunk { idx: self.idx });
    }
}

impl Compile for ir::Attrs {
    fn compile(self, state: &mut CompileState) {
        state.push(OpCode::AttrSet);
        for stc in self.stcs {
            stc.1.compile(state);
            state.push(OpCode::PushStaticAttr { name: stc.0 });
        }
        for dynamic in self.dyns {
            dynamic.0.compile(state);
            dynamic.1.compile(state);
            state.push(OpCode::PushDynamicAttr)
        }
    }
}

impl Compile for ir::StaticAttrs {
    fn compile(self, state: &mut CompileState) {
        state.push(OpCode::AttrSet);
        for stc in self.stcs {
            stc.1.compile(state);
            state.push(OpCode::PushStaticAttr { name: stc.0 });
        }
    }
}

impl Compile for ir::DynamicAttrs {
    fn compile(self, state: &mut CompileState) {
        state.push(OpCode::AttrSet);
        for dynamic in self.dyns {
            dynamic.0.compile(state);
            dynamic.1.compile(state);
            state.push(OpCode::PushDynamicAttr)
        }
    }
}

impl Compile for ir::RecAttrs {
    fn compile(self, state: &mut CompileState) {
        state.push(OpCode::AttrSet);
        for dynamic in self.dyns.clone() {
            dynamic.0.compile(state);
            dynamic.1.compile(state);
            state.push(OpCode::PushDynamicAttr)
        }
        state.push(OpCode::EnterEnv);
        state.push(OpCode::AttrSet);
        for stc in self.stcs {
            stc.1.compile(state);
            state.push(OpCode::PushStaticAttr { name: stc.0 });
        }
        for dynamic in self.dyns {
            dynamic.0.compile(state);
            dynamic.1.compile(state);
            state.push(OpCode::PushDynamicAttr)
        }
        state.push(OpCode::LeaveEnv);
    }
}

impl Compile for ir::List {
    fn compile(self, state: &mut CompileState) {
        state.push(OpCode::List);
        for item in self.items {
            item.compile(state);
            state.push(OpCode::PushElem);
        }
    }
}

impl Compile for ir::UnOp {
    fn compile(self, state: &mut CompileState) {
        self.rhs.compile(state);
        use ir::UnOpKind::*;
        let op = match self.kind {
            Neg => UnOp::Neg,
            Not => UnOp::Not,
        };
        state.push(OpCode::UnOp { op });
    }
}

impl Compile for ir::BinOp {
    fn compile(self, state: &mut CompileState) {
        use ir::BinOpKind::*;
        match self.kind {
            Add => {
                self.lhs.compile(state);
                self.rhs.compile(state);
                state.push(OpCode::BinOp { op: BinOp::Add });
            }
            Mul => {
                self.lhs.compile(state);
                self.rhs.compile(state);
                state.push(OpCode::BinOp { op: BinOp::Mul });
            }
            Div => {
                self.lhs.compile(state);
                self.rhs.compile(state);
                state.push(OpCode::BinOp { op: BinOp::Div });
            }
            And => {
                self.lhs.compile(state);
                self.rhs.compile(state);
                state.push(OpCode::BinOp { op: BinOp::And });
            }
            Or => {
                self.lhs.compile(state);
                self.rhs.compile(state);
                state.push(OpCode::BinOp { op: BinOp::Or });
            }
            Eq => {
                self.lhs.compile(state);
                self.rhs.compile(state);
                state.push(OpCode::BinOp { op: BinOp::Eq });
            }
            Lt => {
                self.lhs.compile(state);
                self.rhs.compile(state);
                state.push(OpCode::BinOp { op: BinOp::Lt });
            }
            Con => {
                self.lhs.compile(state);
                self.rhs.compile(state);
                state.push(OpCode::BinOp { op: BinOp::Con });
            }
            Upd => {
                self.lhs.compile(state);
                self.rhs.compile(state);
                state.push(OpCode::BinOp { op: BinOp::Upd });
            }

            Sub => {
                self.lhs.compile(state);
                self.rhs.compile(state);
                state.push(OpCode::UnOp { op: UnOp::Neg });
                state.push(OpCode::BinOp { op: BinOp::Add });
            }
            Impl => {
                self.lhs.compile(state);
                state.push(OpCode::UnOp { op: UnOp::Not });
                self.rhs.compile(state);
                state.push(OpCode::BinOp { op: BinOp::Or });
            }
            Neq => {
                self.lhs.compile(state);
                self.rhs.compile(state);
                state.push(OpCode::BinOp { op: BinOp::Eq });
                state.push(OpCode::UnOp { op: UnOp::Not });
            }
            Gt => {
                self.rhs.compile(state);
                self.lhs.compile(state);
                state.push(OpCode::BinOp { op: BinOp::Lt });
            }
            Leq => {
                self.rhs.compile(state);
                self.lhs.compile(state);
                state.push(OpCode::BinOp { op: BinOp::Lt });
                state.push(OpCode::UnOp { op: UnOp::Not });
            }
            Geq => {
                self.lhs.compile(state);
                self.rhs.compile(state);
                state.push(OpCode::BinOp { op: BinOp::Lt });
                state.push(OpCode::UnOp { op: UnOp::Not });
            }
        }
    }
}

impl Compile for ir::HasAttr {
    fn compile(self, state: &mut CompileState) {
        self.lhs.compile(state);
        // let arity = self.rhs.len();
        for attr in self.rhs {
            match attr {
                ir::Attr::Ident(sym) => {
                    state.push(OpCode::SelectOrFalse { sym });
                }
                ir::Attr::Dynamic(dynamic) => {
                    dynamic.compile(state);
                    state.push(OpCode::SelectDynamicOrFalse);
                }
                ir::Attr::Str(string) => {
                    string.compile(state);
                    state.push(OpCode::SelectDynamicOrFalse);
                }
            }
        }
        match state.pop().unwrap() {
            OpCode::SelectOrFalse { sym } => state.push(OpCode::HasAttr { sym }),
            OpCode::SelectDynamicOrFalse => state.push(OpCode::HasDynamicAttr),
            _ => unreachable!(),
        }
        // state.push(OpCode::HasAttr { arity });
    }
}

impl Compile for ir::Select {
    fn compile(self, state: &mut CompileState) {
        self.expr.compile(state);
        for attr in self.attrpath {
            match attr {
                ir::Attr::Ident(sym) => state.push(OpCode::SelectOrFalse { sym }),
                ir::Attr::Dynamic(dynamic) => {
                    dynamic.compile(state);
                    state.push(OpCode::SelectDynamicOrFalse);
                }
                ir::Attr::Str(string) => {
                    string.compile(state);
                    state.push(OpCode::SelectDynamicOrFalse);
                }
            }
        }
        match self.default {
            Some(default) => {
                let last = state.pop().unwrap();
                default.compile(state);
                match last {
                    OpCode::SelectOrFalse { sym } => state.push(OpCode::SelectWithDefault { sym }),
                    OpCode::SelectDynamicOrFalse => state.push(OpCode::SelectDynamicWithDefault),
                    _ => unreachable!(),
                }
            }
            None => match state.pop().unwrap() {
                OpCode::SelectOrFalse { sym } => state.push(OpCode::Select { sym }),
                OpCode::SelectDynamicOrFalse => state.push(OpCode::SelectDynamic),
                _ => unreachable!(),
            },
        }
    }
}

impl Compile for ir::ConcatStrings {
    fn compile(self, state: &mut CompileState) {
        let mut iter = self.parts.into_iter();
        iter.next().unwrap().compile(state);
        for item in iter {
            item.compile(state);
            state.push(OpCode::ConcatString);
        }
    }
}

impl Compile for ir::If {
    fn compile(self, state: &mut CompileState) {
        self.cond.compile(state);

        let idx_jmp_if_false = state.idx();
        // place holder
        state.push(OpCode::NoOp);

        let consq_length = self.consq.compile_with_length(state);

        let idx_jmp = state.idx();
        // place holder
        state.push(OpCode::NoOp);

        let alter_length = self.alter.compile_with_length(state);

        state.modify(idx_jmp_if_false, OpCode::JmpIfFalse { step: consq_length });
        state.modify(idx_jmp, OpCode::Jmp { step: alter_length });
    }
}

impl Compile for ir::Let {
    fn compile(self, state: &mut CompileState) {
        self.attrs.compile(state);
        state.push(OpCode::EnterEnv);
        self.expr.compile(state);
        state.push(OpCode::LeaveEnv);
    }
}

impl Compile for ir::With {
    fn compile(self, state: &mut CompileState) {
        self.namespace.compile(state);
        state.push(OpCode::EnterEnv);
        self.expr.compile(state);
        state.push(OpCode::LeaveEnv);
    }
}

impl Compile for ir::Assert {
    fn compile(self, state: &mut CompileState) {
        self.assertion.compile(state);
        state.push(OpCode::Assert);
        self.expr.compile(state);
    }
}

impl Compile for ir::Func {
    fn compile(self, state: &mut CompileState) {
        todo!()
    }
}

impl Compile for ir::Call {
    fn compile(self, state: &mut CompileState) {
        todo!()
    }
}

impl Compile for ir::Path {
    fn compile(self, state: &mut CompileState) {
        todo!()
    }
}
