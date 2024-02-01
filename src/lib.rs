mod ast;
mod builtins;
mod error;
mod eval;
mod lexer;
pub mod object;
mod parser;
mod token;

use error::NixRsError;
use eval::{Environment, Eval, EvalResult};
use std::{cell::RefCell, rc::Rc};

pub use ast::Expression;
pub use ast::Node as AstNode;
use builtins::new_builtins_env;
pub use eval::Env;
pub use lexer::Lexer;
pub use object::Object;
pub use parser::Parser;
pub use token::Token;

pub fn eval(s: String) -> EvalResult {
    let (expr, env) = Parser::new(Box::new(Lexer::build(&s))).parse()?;
    Ok(Eval::new(expr, env).eval()?)
}

pub fn new_env() -> Env {
    let builtins = new_builtins_env();
    Rc::new(RefCell::new(Environment::new(Some(builtins))))
}

pub fn eval_with_env(e: Env, s: String) -> Result<Object, Rc<dyn NixRsError>> {
    Ok(Eval::new(
        Parser::new(Box::new(Lexer::build(&s))).parse_with_env(&e)?,
        e,
    )
    .eval()?)
}

#[macro_export]
macro_rules! convany {
    ( $x:expr, $t:tt ) => {
        $x.downcast_ref::<$t>().unwrap()
    };
}
