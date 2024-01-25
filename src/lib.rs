mod ast;
mod eval;
mod lexer;
mod object;
mod parser;
mod token;

#[macro_use]
extern crate lazy_static;

#[macro_export]
macro_rules! convany {
    ( $x:expr, $t:tt ) => {
        $x.downcast_ref::<$t>().unwrap()
    };
}
