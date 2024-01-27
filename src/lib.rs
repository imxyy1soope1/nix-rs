#![feature(const_type_id)]

mod ast;
mod error;
mod eval;
mod lexer;
mod object;
mod parser;
mod token;

use eval::{Environment, Eval};
use std::{cell::RefCell, rc::Rc};

pub use eval::Env;
use lexer::Lexer;
use object::Object;
use parser::Parser;

pub fn eval(s: String) -> Rc<dyn object::Object> {
    eval::Eval::new(parser::Parser::new(Box::new(lexer::Lexer::build(&s))).parse()).eval()
}

pub fn new_env() -> Env {
    Rc::new(RefCell::new(Environment::new(None)))
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
