use std::collections::HashMap;

use rnix::ast::Expr;

use crate::vm::program::*;

use super::env::Env as _Env;
use super::ir::{self, Ir};
use super::symtable::*;

pub fn compile(expr: Expr) -> CompiledProgram {
    let (ir, table) = ir::downgrade(expr);
    let mut state = CompileState::new();
    let compiled = ir.compile(&mut state);
    let len = state.consts.len();
    let mut consts = Box::<[Const]>::new_uninit_slice(len);
    for (cnst, idx) in state.consts {
        unsafe { *consts.get_unchecked_mut(idx) = std::mem::MaybeUninit::new(cnst) }
    }
    let consts = unsafe { consts.assume_init() };
    CompiledProgram {
        top_level: compiled.into(),
        consts: consts.into(),
        thunks: state.thunks.into(),
        syms: table.into_syms(),
    }
}

trait Compile {
    fn compile(self, state: &mut CompileState) -> Vec<Instruction>;
}

enum Env {
    Env(_Env),
    With,
}

impl Env {
    fn env(&self) -> &_Env {
        match self {
            Env::Env(env) => env,
            _ => panic!(),
        }
    }

    fn env_mut(&mut self) -> &mut _Env {
        match self {
            Env::Env(env) => env,
            _ => panic!(),
        }
    }
}

struct CompileState {
    envs: Vec<Env>,
    consts: HashMap<Const, Idx>,
    thunks: Vec<Frame>,
}

impl CompileState {
    fn new() -> CompileState {
        CompileState {
            envs: Vec::new(),
            consts: HashMap::new(),
            thunks: Vec::new(),
        }
    }

    fn new_env(&mut self) {
        self.envs.push(Env::Env(_Env::new()));
    }

    fn with(&mut self) {
        self.envs.push(Env::With);
    }

    fn pop_env(&mut self) {
        self.envs.pop().unwrap();
    }

    fn insert_stc(&mut self, sym: Sym, thunk: Vec<Instruction>) {
        let idx = self.new_thunk(thunk);
        if let Some(_) = self
            .envs
            .last_mut()
            .unwrap()
            .env_mut()
            .stcs
            .insert(sym, idx)
        {
            panic!()
        }
    }

    fn alloc_stcs<'a>(&'a mut self, syms: &[Sym]) -> (usize, usize) {
        let len = self.thunks.len();
        self.thunks.resize_with(len + syms.len(), Default::default);
        for (sym, idx) in std::iter::zip(syms, len..len + syms.len()) {
            if let Some(_) = self
                .envs
                .last_mut()
                .unwrap()
                .env_mut()
                .stcs
                .insert(*sym, idx)
            {
                panic!()
            }
        }
        (len, syms.len())
    }

    fn insert_dyn(&mut self, sym: impl Into<Frame>, thunk: impl Into<Frame>) {
        let idx = self.new_thunk(thunk);
        self.envs
            .last_mut()
            .unwrap()
            .env_mut()
            .dyns
            .push((sym.into(), idx));
    }

    fn lookup(&self, sym: Sym) -> Option<Idx> {
        let mut envs = self.envs.iter();
        while let Some(Env::Env(_Env { stcs, .. })) = envs.next_back() {
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

    fn new_thunk(&mut self, thunk: impl Into<Frame>) -> Idx {
        let idx = self.thunks.len();
        self.thunks.push(thunk.into());
        idx
    }
}

#[derive(Debug, PartialEq)]
pub struct CompiledProgram {
    pub top_level: Frame,
    pub consts: Box<[Const]>,
    pub thunks: Box<[Frame]>,
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
            Ir::LetRec(let_) => let_.compile(state),
            Ir::With(with) => with.compile(state),
            Ir::Assert(assert_) => assert_.compile(state),
            Ir::Func(func) => func.compile(state),
            Ir::Call(call) => call.compile(state),
        }
    }
}

impl Compile for ir::Attrs {
    fn compile(self, state: &mut CompileState) -> Vec<Instruction> {
        let mut thunk = Vec::with_capacity(self.stcs.len() + self.dyns.len() + 1);
        thunk.push(Instruction::Attrs);
        // let mut stcs = self.stcs;
        // stcs.sort_by_key(|(sym, _)| *sym);
        for (sym, item) in self.stcs {
            let compiled = item.compile(state);
            let idx = state.new_thunk(compiled);
            thunk.push(Instruction::StcAttr { sym, idx });
        }
        for (sym, item) in self.dyns {
            let compiled_sym = sym.compile(state);
            let sym = state.new_thunk(compiled_sym);
            let compiled_item = item.compile(state);
            let idx = state.new_thunk(compiled_item);
            thunk.push(Instruction::DynAttr { sym, idx });
        }
        thunk
    }
}

impl Compile for ir::RecAttrs {
    fn compile(self, state: &mut CompileState) -> Vec<Instruction> {
        state.new_env();
        let mut thunk = Vec::with_capacity(self.stcs.len() + self.dyns.len() + 1);
        thunk.push(Instruction::Attrs);
        // let mut stcs = self.stcs;
        // stcs.sort_by_key(|(sym, _)| *sym);
        let range = state.alloc_stcs(&self.stcs.iter().map(|(sym, _)| *sym).collect::<Vec<_>>());
        for ((sym, item), idx) in std::iter::zip(self.stcs, range.0..range.1) {
            let compiled = item.compile(state);
            *state.thunks.get_mut(idx).unwrap() = compiled.into();
            thunk.push(Instruction::StcAttr { sym, idx });
        }
        for (sym, item) in self.dyns {
            let compiled_sym = sym.compile(state);
            let sym = state.new_thunk(compiled_sym);
            let compiled_item = item.compile(state);
            let idx = state.new_thunk(compiled_item);
            thunk.push(Instruction::DynAttr { sym, idx });
        }
        state.pop_env();
        thunk
    }
}

impl Compile for ir::List {
    fn compile(self, state: &mut CompileState) -> Vec<Instruction> {
        let mut thunk = Vec::with_capacity(self.items.len() + 1);
        thunk.push(Instruction::List);
        for item in self.items {
            let compiled = item.compile(state);
            let idx = state.new_thunk(compiled);
            thunk.push(Instruction::ListElem(idx));
        }
        thunk
    }
}

impl Compile for ir::BinOp {
    fn compile(self, state: &mut CompileState) -> Vec<Instruction> {
        let mut thunk = self.lhs.compile(state);
        thunk.append(&mut self.rhs.compile(state));
        use ir::BinOpKind::*;
        thunk.push(Instruction::Op(match self.kind {
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
        thunk
    }
}

impl Compile for ir::If {
    fn compile(self, state: &mut CompileState) -> Vec<Instruction> {
        let mut cond = self.cond.compile(state);
        let consq = self.consq.compile(state);
        let alter = self.alter.compile(state);
        let (consq, alter) = (state.new_thunk(consq), state.new_thunk(alter));
        cond.push(Instruction::If { consq, alter });
        cond
    }
}

impl Compile for ir::Let {
    fn compile(self, state: &mut CompileState) -> Vec<Instruction> {
        state.new_env();
        let mut thunk = Vec::with_capacity(self.attrs.dyns.len() + 5);
        thunk.push(Instruction::Attrs);
        for (sym, item) in self.attrs.stcs {
            let compiled = item.compile(state);
            let idx = state.new_thunk(compiled);
            thunk.push(Instruction::StcAttr { sym, idx });
        }
        for (sym, item) in self.attrs.dyns {
            let compiled_sym = sym.compile(state);
            let sym = state.new_thunk(compiled_sym);
            let compiled_item = item.compile(state);
            let idx = state.new_thunk(compiled_item);
            thunk.push(Instruction::DynAttr { sym, idx });
        }
        thunk.push(Instruction::EnterEnv);
        thunk.append(&mut self.expr.compile(state));
        thunk.push(Instruction::ExitEnv);
        state.pop_env();
        thunk
    }
}

impl Compile for ir::LetRec {
    fn compile(self, state: &mut CompileState) -> Vec<Instruction> {
        state.new_env();
        let mut thunk = Vec::with_capacity(self.attrs.dyns.len() + 5);
        thunk.push(Instruction::Attrs);
        // let mut stcs = self.attrs.stcs;
        // stcs.sort_by_key(|(sym, _)| *sym);
        let range = state.alloc_stcs(
            &self
                .attrs
                .stcs
                .iter()
                .map(|(sym, _)| *sym)
                .collect::<Vec<_>>(),
        );
        for ((_, item), idx) in std::iter::zip(self.attrs.stcs, range.0..range.1) {
            let compiled = item.compile(state);
            *state.thunks.get_mut(idx).unwrap() = compiled.into();
        }
        for (sym, item) in self.attrs.dyns {
            let compiled_sym = sym.compile(state);
            let sym = state.new_thunk(compiled_sym);
            let compiled_item = item.compile(state);
            let idx = state.new_thunk(compiled_item);
            thunk.push(Instruction::DynAttr { sym, idx });
        }
        thunk.push(Instruction::EnterEnv);
        thunk.append(&mut self.expr.compile(state));
        thunk.push(Instruction::ExitEnv);
        state.pop_env();
        thunk
    }
}

impl Compile for ir::With {
    fn compile(self, state: &mut CompileState) -> Vec<Instruction> {
        let mut thunk = self.attrs.compile(state);
        state.with();
        thunk.push(Instruction::EnterEnv);
        thunk.append(&mut self.expr.compile(state));
        thunk.push(Instruction::ExitEnv);
        state.pop_env();
        thunk
    }
}

impl Compile for ir::Assert {
    fn compile(self, state: &mut CompileState) -> Vec<Instruction> {
        let mut thunk = self.assertion.compile(state);
        thunk.push(Instruction::Assert);
        thunk.append(&mut self.expr.compile(state));
        thunk
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
