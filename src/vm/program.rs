use std::error::Error;
use std::fmt;
use std::hash::{Hash, Hasher};

use crate::compile::ir;
use crate::compile::CompiledProgram;

#[derive(Debug, Clone)]
pub enum Const {
    Int(i64),
    Float(f64),
    String(String),
}

/* impl From<ir::Const> for Const {
    fn from(value: ir::Const) -> Self {
        match value {
            ir::Const::Int(x) => Const::Int(x),
            ir::Const::Float(x) => Const::Float(x),
            ir::Const::String(x) => Const::String(x),
        }
    }
} */

impl<'a> TryFrom<&'a Const> for &'a i64 {
    type Error = Box<dyn Error>;

    fn try_from(value: &'a Const) -> Result<Self, Self::Error> {
        match value {
            Const::Int(int) => Ok(int),
            _ => panic!(),
        }
    }
}

impl<'a> TryFrom<&'a Const> for &'a f64 {
    type Error = Box<dyn Error>;

    fn try_from(value: &'a Const) -> Result<Self, Self::Error> {
        match value {
            Const::Float(float) => Ok(float),
            _ => panic!(),
        }
    }
}

impl<'a> TryFrom<&'a Const> for &'a str {
    type Error = Box<dyn Error>;

    fn try_from(value: &'a Const) -> Result<Self, Self::Error> {
        match value {
            Const::String(string) => Ok(string),
            _ => panic!(),
        }
    }
}

impl PartialEq for Const {
    fn eq(&self, other: &Self) -> bool {
        use Const::*;
        match *self {
            Int(int) => int.eq(other.try_into().unwrap()),
            Float(float) => float
                .to_bits()
                .eq(&(TryInto::<&f64>::try_into(other).unwrap().to_bits())),
            String(ref string) => string.eq(TryInto::<&str>::try_into(other).unwrap()),
        }
    }
}

impl Eq for Const {}

impl Hash for Const {
    fn hash<H: Hasher>(&self, state: &mut H) {
        use Const::*;
        match *self {
            Int(int) => int.hash(state),
            Float(float) => float.to_bits().hash(state),
            String(ref string) => string.hash(state),
        }
    }
}

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

pub struct Program {

}
