use super::env::Environment;
use crate::builtins::new_builtins_env;
use crate::error::{ErrorCtx, NixRsError};
use crate::{ast::*, object::*, Env};
use std::cell::RefCell;
use std::rc::Rc;

pub type EvalResult = core::result::Result<Rc<dyn Object>, Rc<dyn NixRsError>>;

pub struct Eval {
    root: EvaledOr,
    rootenv: Option<Env>,
}

impl Eval {
    pub fn new(expr: Rc<dyn Expression>) -> Eval {
        let env = Rc::new(RefCell::new(Environment::new(Some(new_builtins_env()))));
        Eval {
            root: EvaledOr::expr(
                &env,
                expr,
                ErrorCtx::new(),
            ),
            rootenv: Some(env)
        }
    }

    pub fn with_env(env: &Rc<RefCell<Environment>>, expr: Rc<dyn Expression>) -> Eval {
        Eval {
            root: EvaledOr::expr(env, expr, ErrorCtx::new()),
            rootenv: None
        }
    }

    pub fn eval(&mut self) -> EvalResult {
        self.root.eval()
    }
}
