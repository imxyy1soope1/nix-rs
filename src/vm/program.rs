use std::error::Error;
use std::fmt;
use std::hash::{Hash, Hasher};

#[derive(PartialEq, Debug)]
pub enum Arg {
    Arg(Idx),
    Formals {
        formals: Vec<(Idx, Option<Frame>)>,
        ellipsis: bool,
        alias: Option<Idx>,
    },
}

pub type Idx = usize;
pub type Frame = Box<[Instruction]>;

#[derive(PartialEq, Debug)]
pub enum Instruction {
    Const(Idx),
    Load(Idx),
    DynLoad(Idx),
    Attrs,
    StcAttr { sym: Idx, idx: Idx },
    DynAttr { sym: Idx, idx: Idx },
    List,
    ListElem(Idx),
    Op(Op),
    If { consq: Idx, alter: Idx },
    Assert,
    EnterEnv,
    ExitEnv,
    Closure { arg: Arg, frame: Frame },
    Call,
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Neq,
    Lt,
    Gt,
    Leq,
    Geq,
    And,
    Or,
    Impl,
    Not,
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Op::*;
        f.write_str(match *self {
            Add => "add",
            Sub => "sub",
            Mul => "mul",
            Div => "div",
            Eq => "eq",
            Neq => "neq",
            Lt => "lt",
            Gt => "gt",
            Leq => "leq",
            Geq => "geq",
            And => "and",
            Or => "or",
            Impl => "impl",
            Not => "not",
        })
    }
}

impl fmt::Debug for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

pub struct Program {}
