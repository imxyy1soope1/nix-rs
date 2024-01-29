use super::env::Environment;
use crate::builtins::new_builtins_env;
use crate::error::{ErrorCtx, NixRsError};
use crate::{ast::*, object::*};
use std::cell::RefCell;
use std::rc::Rc;

pub type EvalResult = core::result::Result<Rc<dyn Object>, Rc<dyn NixRsError>>;

pub struct Eval {
    root: EvaledOr,
}

impl Eval {
    pub fn new(expr: Rc<dyn Expression>) -> Eval {
        Eval {
            root: EvaledOr::expr(
                Rc::new(RefCell::new(Environment::new(Some(new_builtins_env())))),
                expr,
                ErrorCtx::new(),
            ),
        }
    }

    pub fn with_env(env: Rc<RefCell<Environment>>, expr: Rc<dyn Expression>) -> Eval {
        Eval {
            root: EvaledOr::expr(env, expr, ErrorCtx::new()),
        }
    }

    pub fn eval(&mut self) -> EvalResult {
        self.root.eval()
    }
}
