use crate::{ast::*, object::*};

struct Eval {
    root: Node
}

struct Node {
    expr: Box<dyn Expression>,
    evaled: Option<Box<dyn Object>>,
}

impl Eval {
    pub fn new(expr: Box<dyn Expression>) -> Eval {
        let mut e = Eval {
            root: Node {
                expr,
                evaled: None,
            },
        };

        e.build();

        e
    }

    fn build(&mut self) {}
}
