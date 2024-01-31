use crate::error::{ErrorCtx, NixRsError};
use crate::{ast::*, object::*};

pub type EvalResult = core::result::Result<Object, Box<dyn NixRsError>>;

pub struct Eval {
    root: Node,
}

impl Eval {
    pub fn new(expr: Expression) -> Eval {
        Eval {
            root: Node::Expr(Box::new(expr)),
        }
    }

    pub fn eval(&mut self) -> EvalResult {
        self.root.force_value()
    }
}
