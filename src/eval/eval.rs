use super::env::*;
use crate::builtins::new_builtins_env;
use crate::error::{ErrorCtx, NixRsError};
use crate::{ast::*, object::*};
use std::rc::Rc;

pub type EvalResult = core::result::Result<Object, Box<dyn NixRsError>>;

pub struct EvalState {
    env: Env,
    ctx: ErrorCtx,
}

impl EvalState {
    pub fn new() -> EvalState {
        EvalState {
            env: new_builtins_env(),
            ctx: ErrorCtx::new(),
        }
    }

    pub fn with_env(env: Env) -> EvalState {
        EvalState {
            env,
            ctx: ErrorCtx::new(),
        }
    }
}

pub struct Eval {
    state: EvalState,
    root: Box<dyn Expression>,
}

impl Eval {
    pub fn new(expr: Box<dyn Expression>) -> Eval {
        Eval {
            state: EvalState::new(),
            root: expr,
        }
    }

    pub fn with_env(env: Env, expr: Rc<dyn Expression>) -> Eval {
        Eval {
            state: EvalState::with_env(env),
            root: expr,
        }
    }

    pub fn eval(&mut self) -> EvalResult {
        self.root.eval(&self.state.env, &self.state.ctx)
    }
}
