use crate::ast::*;

pub struct Eval {
    root: Box<dyn Expression>,
}

impl Eval {
    pub fn new(root: Box<dyn Expression>) -> Eval {
        Eval { root }
    }
}
