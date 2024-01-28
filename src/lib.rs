mod ast;
mod builtins;
mod error;
mod eval;
mod lexer;
mod parser;
mod token;
pub mod object;

use eval::{Environment, Eval};
use std::{cell::RefCell, rc::Rc};

use builtins::new_builtins_env;
pub use eval::Env;
pub use lexer::Lexer;
pub use parser::Parser;
pub use token::Token;
pub use ast::Expression;
pub use object::Object;

pub fn eval(s: String) -> Rc<dyn object::Object> {
    Eval::new(Parser::new(Box::new(Lexer::build(&s))).parse()).eval()
}

pub fn new_env() -> Env {
    Rc::new(RefCell::new(Environment::new(Some(new_builtins_env()))))
}

pub fn eval_with_env(e: Env, s: String) -> Rc<dyn Object> {
    Eval::with_env(e, Parser::new(Box::new(Lexer::build(&s))).parse()).eval()
}

#[macro_export]
macro_rules! convany {
    ( $x:expr, $t:tt ) => {
        $x.downcast_ref::<$t>().unwrap()
    };
}
