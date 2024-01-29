use super::env::Environment;
use crate::builtins::new_builtins_env;
use crate::{ast::*, object::*};
use std::cell::RefCell;
use std::rc::Rc;

pub struct Eval {
    root: EvaledOr,
}

impl Eval {
    pub fn new(expr: Rc<dyn Expression>) -> Eval {
        Eval {
            root: EvaledOr::expr(
                Rc::new(RefCell::new(Environment::new(Some(new_builtins_env())))),
                Rc::from(expr),
            ),
        }
    }

    pub fn with_env(env: Rc<RefCell<Environment>>, expr: Rc<dyn Expression>) -> Eval {
        Eval {
            root: EvaledOr::expr(env, Rc::from(expr)),
        }
    }

    pub fn eval(&mut self) -> Rc<dyn Object> {
        self.root.eval()
    }
}
