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
    state.frames.push(compiled);
    let len = state.consts.len();
    let mut consts = vec![Const::Int(0); len];
    for (cnst, idx) in state.consts {
        unsafe { *consts.get_unchecked_mut(idx) = cnst }
    }
    CompiledProgram {
        consts: consts.into_boxed_slice(),
        frames: state.frames.into_boxed_slice(),
        syms: table.into_syms(),
    }
}

trait Compile {
    fn compile(self, state: &mut CompileState) -> Frame;
}

macro_rules! boxvec {
    () => (
        vec![].into_boxed_slice()
    );
    ( $($item:expr), + $(,)? ) => (
        vec![$( $item, )*].into_boxed_slice()
    );
}

struct CompileState {
    envs: Vec<Env>,
    consts: HashMap<Const, Idx>,
    frames: Vec<Frame>,
}

impl CompileState {
    fn new() -> CompileState {
        CompileState {
            envs: Vec::new(),
            consts: HashMap::new(),
            frames: Vec::new(),
        }
    }

    fn new_env(&mut self) {
        self.envs.push(Env::new())
    }

    fn pop_env(&mut self) -> Env {
        self.envs.pop().unwrap()
    }

    fn insert_stc(&mut self, sym: Sym, frame: Frame) {
        let idx = self.envs.len() - 1;
        unsafe {
            if let Some(_) = self.envs.get_unchecked_mut(idx).stcs.insert(sym, frame) {
                panic!()
            }
        }
    }

    fn insert_dyn(&mut self, sym: Ir, frame: Frame) {
        let idx = self.envs.len() - 1;
        unsafe { self.envs.get_unchecked_mut(idx).dyns.push((sym, frame)) }
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

    fn new_frame(&mut self, frame: Frame) -> Idx {
        let idx = self.frames.len();
        self.frames.push(frame);
        idx
    }
}

pub struct CompiledProgram {
    pub consts: Box<[Const]>,
    pub frames: Box<[Frame]>,
    pub syms: Box<[String]>,
}

impl Compile for Ir {
    fn compile(self, state: &mut CompileState) -> Frame {
        use Instruction::*;
        match self {
            Ir::Var(sym) => boxvec![Load(sym)],
            Ir::Const(cnst) => {
                let idx = state.new_const(cnst.into());
                boxvec![Const(idx)]
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
    fn compile(self, state: &mut CompileState) -> Frame {
        boxvec![]
    }
}

impl Compile for ir::RecAttrs {
    fn compile(self, state: &mut CompileState) -> Frame {
        boxvec![]
    }
}

impl Compile for ir::List {
    fn compile(self, state: &mut CompileState) -> Frame {
        boxvec![]
    }
}

impl Compile for ir::BinOp {
    fn compile(self, state: &mut CompileState) -> Frame {
        boxvec![]
    }
}

impl Compile for ir::If {
    fn compile(self, state: &mut CompileState) -> Frame {
        boxvec![]
    }
}

impl Compile for ir::Let {
    fn compile(self, state: &mut CompileState) -> Frame {
        boxvec![]
    }
}

impl Compile for ir::With {
    fn compile(self, state: &mut CompileState) -> Frame {
        boxvec![]
    }
}

impl Compile for ir::Assert {
    fn compile(self, state: &mut CompileState) -> Frame {
        boxvec![]
    }
}

impl Compile for ir::Func {
    fn compile(self, state: &mut CompileState) -> Frame {
        boxvec![]
    }
}

impl Compile for ir::Call {
    fn compile(self, state: &mut CompileState) -> Frame {
        boxvec![]
    }
}
