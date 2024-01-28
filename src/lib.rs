#![feature(const_type_id)]

mod ast;
mod builtins;
mod error;
mod eval;
mod lexer;
mod object;
mod parser;
mod token;

use eval::{Environment, Eval};
use std::{cell::RefCell, rc::Rc};

use builtins::new_builtins_env;
pub use eval::Env;
use lexer::Lexer;
use object::Object;
use parser::Parser;

pub fn eval(s: String) -> Rc<dyn object::Object> {
    Eval::new(Parser::new(Box::new(Lexer::build(&s))).parse()).eval()
}

pub fn new_env() -> Env {
    Rc::new(RefCell::new(Environment::new(Some(new_builtins_env()))))
}

pub fn eval_with_env(e: Env, s: String) -> Rc<dyn Object> {
    Eval::with_env(e, Parser::new(Box::new(Lexer::build(&s))).parse()).eval()
}

#[macro_use]
extern crate lazy_static;

#[macro_export]
macro_rules! convany {
    ( $x:expr, $t:tt ) => {
        $x.downcast_ref::<$t>().unwrap()
    };
}
