use super::env::{Env, Environment};
use crate::builtins::new_builtins_env;
use crate::error::{ErrorCtx, NixRsError};
use crate::{ast::*, object::*};
use std::cell::RefCell;
use std::rc::Rc;

pub type EvalResult = core::result::Result<Object, Box<dyn NixRsError>>;

pub struct Eval {
    root: Node,
}

impl Eval {
    pub fn new(expr: Expression) -> Eval {
        Eval {
            root: Node::Expr(
                Rc::new(RefCell::new(Environment::new(Some(new_builtins_env())))),
                Box::new(expr),
            ),
        }
    }

    pub fn with_env(env: Env, expr: Expression) -> Eval {
        Eval {
            root: Node::Expr(env, Box::new(expr)),
        }
    }

    pub fn eval(&mut self) -> EvalResult {
        self.root.force_value()
    }
}
