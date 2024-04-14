use std::collections::HashMap;

use crate::ast::Expr;
use crate::vm::program::*;

use super::env::Env;
use super::ir::{self, Ir};
use super::symtable::*;

pub fn compile(expr: Expr) -> CompiledProgram {
    let (ir, table) = ir::desugar(expr);
    let mut state = CompileState::new();
    let compiled = ir.compile(&mut state);
    state.frames.push(compiled.into());
    let len = state.consts.len();
    let mut consts = vec![Const::Int(0); len];
    for (cnst, idx) in state.consts {
        unsafe { *consts.get_unchecked_mut(idx) = cnst }
    }
    CompiledProgram {
        consts: consts.into(),
        frames: state.frames.into(),
        syms: table.into_syms(),
    }
}

trait Compile {
    fn compile(self, state: &mut CompileState) -> Vec<Instruction>;
}

struct CompileState {
    env_stack: Vec<Env>,
    envs: HashMap<Idx, Env>,
    consts: HashMap<Const, Idx>,
    frames: Vec<Frame>,
}

impl CompileState {
    fn new() -> CompileState {
        CompileState {
            env_stack: Vec::new(),
            envs: HashMap::new(),
            consts: HashMap::new(),
            frames: Vec::new(),
        }
    }

    fn new_env(&mut self) {
        self.env_stack.push(Env::new())
    }

    fn pop_env(&mut self) -> Env {
        self.env_stack.pop().unwrap()
    }

    fn insert_stc(&mut self, sym: Sym, frame: Vec<Instruction>) {
        let idx = self.new_frame(frame);
        if let Some(_) = self.env_stack.last_mut().unwrap().stcs.insert(sym, idx) {
            panic!()
        }
    }

    fn alloc_stcs<'a>(&'a mut self, syms: &[Sym]) -> (usize, usize) {
        let len = self.frames.len();
        self.frames.resize_with(len + syms.len(), Default::default);
        for (sym, idx) in std::iter::zip(syms, len..len + syms.len()) {
            if let Some(_) = self.env_stack.last_mut().unwrap().stcs.insert(*sym, idx) {
                panic!()
            }
        }
        (len, syms.len())
    }

    fn insert_dyn(&mut self, sym: impl Into<Frame>, frame: impl Into<Frame>) {
        let idx = self.new_frame(frame);
        self.env_stack
            .last_mut()
            .unwrap()
            .dyns
            .push((sym.into(), idx));
    }

    fn lookup(&self, sym: Sym) -> Option<Idx> {
        let mut env_stack = self.env_stack.iter();
        while let Some(Env { stcs, .. }) = env_stack.next_back() {
            if let Some(idx) = stcs.get(&sym) {
                return Some(*idx);
            }
        }
        None
    }

    fn new_const(&mut self, cnst: Const) -> Idx {
        if let Some(idx) = self.consts.get(&cnst) {
            *idx
        } else {
            let idx = self.consts.len();
            self.consts.insert(cnst, idx);
            idx
        }
    }

    fn new_frame(&mut self, frame: impl Into<Frame>) -> Idx {
        let idx = self.frames.len();
        self.frames.push(frame.into());
        idx
    }
}

#[derive(Debug, PartialEq)]
pub struct CompiledProgram {
    pub consts: Box<[Const]>,
    pub frames: Box<[Frame]>,
    pub syms: Box<[String]>,
}

impl Compile for Ir {
    fn compile(self, state: &mut CompileState) -> Vec<Instruction> {
        use Instruction::*;
        match self {
            Ir::Var(sym) => {
                if let Some(idx) = state.lookup(sym) {
                    vec![Load(idx)]
                } else {
                    vec![DynLoad(sym)]
                }
            }
            Ir::Const(cnst) => {
                let idx = state.new_const(cnst.into());
                vec![Const(idx)]
            }
            Ir::Attrs(attrs) => attrs.compile(state),
            Ir::RecAttrs(attrs) => attrs.compile(state),
            Ir::List(list) => list.compile(state),
            Ir::BinOp(op) => op.compile(state),
            Ir::If(if_) => if_.compile(state),
            Ir::Let(let_) => let_.compile(state),
            Ir::With(with) => with.compile(state),
            Ir::Assert(assert_) => assert_.compile(state),
            Ir::Func(func) => func.compile(state),
            Ir::Call(call) => call.compile(state),
        }
    }
}

impl Compile for ir::Attrs {
    fn compile(self, state: &mut CompileState) -> Vec<Instruction> {
        let mut frame = Vec::with_capacity(self.stcs.len() + self.dyns.len() + 1);
        frame.push(Instruction::Attrs);
        let mut stcs = self.stcs;
        stcs.sort_by_key(|(sym, _)| *sym);
        for (sym, item) in stcs {
            let compiled = item.compile(state);
            let idx = state.new_frame(compiled);
            frame.push(Instruction::StcAttr { sym, idx });
        }
        for (sym, item) in self.dyns {
            let compiled_sym = sym.compile(state);
            let sym = state.new_frame(compiled_sym);
            let compiled_item = item.compile(state);
            let idx = state.new_frame(compiled_item);
            frame.push(Instruction::DynAttr { sym, idx });
        }
        frame
    }
}

impl Compile for ir::RecAttrs {
    fn compile(self, state: &mut CompileState) -> Vec<Instruction> {
        state.new_env();
        let mut frame = Vec::with_capacity(self.stcs.len() + self.dyns.len() + 1);
        frame.push(Instruction::Attrs);
        let mut stcs = self.stcs;
        stcs.sort_by_key(|(sym, _)| *sym);
        let range = state.alloc_stcs(&stcs.iter().map(|(sym, _)| *sym).collect::<Vec<_>>());
        for ((sym, item), idx) in std::iter::zip(stcs, range.0..range.1) {
            let compiled = item.compile(state);
            *state.frames.get_mut(idx).unwrap() = compiled.into();
            frame.push(Instruction::StcAttr { sym, idx });
        }
        for (sym, item) in self.dyns {
            let compiled_sym = sym.compile(state);
            let sym = state.new_frame(compiled_sym);
            let compiled_item = item.compile(state);
            let idx = state.new_frame(compiled_item);
            frame.push(Instruction::DynAttr { sym, idx });
        }
        state.pop_env();
        frame
    }
}

impl Compile for ir::List {
    fn compile(self, state: &mut CompileState) -> Vec<Instruction> {
        let mut frame = Vec::with_capacity(self.items.len() + 1);
        frame.push(Instruction::List);
        for item in self.items {
            let compiled = item.compile(state);
            let idx = state.new_frame(compiled);
            frame.push(Instruction::ListElem(idx));
        }
        frame
    }
}

impl Compile for ir::BinOp {
    fn compile(self, state: &mut CompileState) -> Vec<Instruction> {
        let mut frame = self.lhs.compile(state);
        frame.append(&mut self.rhs.compile(state));
        use ir::BinOpKind::*;
        frame.push(Instruction::Op(match self.kind {
            Add => Op::Add,
            Sub => Op::Sub,
            Mul => Op::Mul,
            Div => Op::Div,
            Eq => Op::Eq,
            Neq => Op::Neq,
            Lt => Op::Lt,
            Gt => Op::Gt,
            Leq => Op::Leq,
            Geq => Op::Geq,
            And => Op::And,
            Or => Op::Or,
            Impl => Op::Impl,
        }));
        frame
    }
}

impl Compile for ir::If {
    fn compile(self, state: &mut CompileState) -> Vec<Instruction> {
        let mut cond = self.cond.compile(state);
        let consq = self.consq.compile(state);
        let alter = self.alter.compile(state);
        let (consq, alter) = (state.new_frame(consq), state.new_frame(alter));
        cond.push(Instruction::If { consq, alter });
        cond
    }
}

impl Compile for ir::Let {
    fn compile(self, state: &mut CompileState) -> Vec<Instruction> {
        state.new_env();
        let mut frame = Vec::with_capacity(self.attrs.dyns.len() + 5);
        frame.push(Instruction::Attrs);
        let mut stcs = self.attrs.stcs;
        stcs.sort_by_key(|(sym, _)| *sym);
        let range = state.alloc_stcs(&stcs.iter().map(|(sym, _)| *sym).collect::<Vec<_>>());
        for ((_, item), idx) in std::iter::zip(stcs, range.0..range.1) {
            let compiled = item.compile(state);
            *state.frames.get_mut(idx).unwrap() = compiled.into();
        }
        for (sym, item) in self.attrs.dyns {
            let compiled_sym = sym.compile(state);
            let sym = state.new_frame(compiled_sym);
            let compiled_item = item.compile(state);
            let idx = state.new_frame(compiled_item);
            frame.push(Instruction::DynAttr { sym, idx });
        }
        frame.push(Instruction::EnterEnv);
        frame.append(&mut self.expr.compile(state));
        frame.push(Instruction::ExitEnv);
        state.pop_env();
        frame
    }
}

impl Compile for ir::With {
    fn compile(self, state: &mut CompileState) -> Vec<Instruction> {
        let mut frame = self.attrs.compile(state);
        frame.push(Instruction::EnterEnv);
        frame.append(&mut self.expr.compile(state));
        frame.push(Instruction::ExitEnv);
        frame
    }
}

impl Compile for ir::Assert {
    fn compile(self, state: &mut CompileState) -> Vec<Instruction> {
        let mut frame = self.assertion.compile(state);
        frame.push(Instruction::Assert);
        frame.append(&mut self.expr.compile(state));
        frame
    }
}

impl Compile for ir::Func {
    fn compile(self, state: &mut CompileState) -> Vec<Instruction> {
        vec![]
    }
}

impl Compile for ir::Call {
    fn compile(self, state: &mut CompileState) -> Vec<Instruction> {
        vec![]
    }
}
