use std::fmt::{Display, Formatter, Result as FmtResult};

use derive_more::{IsVariant, Unwrap, Constructor};

use super::vm::VM;
use rpds::{HashTrieMapSync, Vector};

#[derive(IsVariant, Unwrap)]
pub enum Const<'vm> {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(&'vm str),
}

pub struct Symbol<'vm>(&'vm VM, usize);

impl<'vm> Display for Symbol<'vm> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{}", self.0.lookup_symbol(self.1).unwrap())
    }
}

#[derive(Constructor)]
pub struct AttrSet<'vm> {
    data: HashTrieMapSync<Symbol<'vm>, Value<'vm>>
}

#[derive(Constructor)]
pub struct List<'vm> {
    data: Vector<Value<'vm>>
}

#[derive(IsVariant, Unwrap)]
pub enum Value<'vm> {
    Const(Const<'vm>),
    AttrSet(AttrSet<'vm>),
    List(List<'vm>),
    
}
