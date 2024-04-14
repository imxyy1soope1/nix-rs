mod compile;
mod env;
pub mod ir;
mod symtable;

pub use compile::{compile, CompiledProgram};
