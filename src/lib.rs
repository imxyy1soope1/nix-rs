#![feature(const_type_id)]

mod ast;
mod builtins;
mod error;
mod eval;
mod lexer;
pub mod object;
mod parser;
mod token;

use eval::{Environment, Eval, EvalResult};
use std::{cell::RefCell, rc::Rc};

pub use ast::Expression;
pub use ast::BindingExpr;
use builtins::new_builtins_env;
pub use eval::Env;
pub use lexer::Lexer;
pub use object::Object;
pub use parser::Parser;
pub use token::Token;

pub fn eval(s: String) -> EvalResult {
    Eval::new(Parser::new(Box::new(Lexer::build(&s))).parse()).eval()
}

pub fn new_env() -> Env {
    Rc::new(RefCell::new(Environment::new(Some(new_builtins_env()))))
}

pub fn eval_with_env(e: Env, s: String) -> EvalResult {
    Eval::with_env(e, Parser::new(Box::new(Lexer::build(&s))).parse()).eval()
}

#[macro_export]
macro_rules! convany {
    ( $x:expr, $t:tt ) => {
        $x.downcast_ref::<$t>().unwrap()
    };
}
