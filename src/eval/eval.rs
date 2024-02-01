use crate::error::NixRsError;
use crate::{ast::*, object::*, Env};

pub type EvalResult = core::result::Result<Object, Box<dyn NixRsError>>;

pub struct Eval {
    root: Node,
    env: Env,
}

impl Eval {
    pub fn new(expr: Expression, env: Env) -> Eval {
        Eval {
            root: Node::Expr(Box::new(expr)),
            env,
        }
    }

    pub fn eval(&mut self) -> EvalResult {
        self.root.force_value(&self.env)
    }
}
