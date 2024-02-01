use crate::error::NixRsError;
use crate::{ast::*, object::*, Env};

pub type EvalResult = core::result::Result<Object, Box<dyn NixRsError>>;

pub struct Eval {
    root: ObjectOr
}

impl Eval {
    pub fn new(expr: Expression, env: Env) -> Eval {
        Eval {
            root: ObjectOr::expr(expr, env),
        }
    }

    pub fn eval(&mut self) -> EvalResult {
        self.root.get()
    }
}
