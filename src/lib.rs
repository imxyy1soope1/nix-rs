#![allow(dead_code)]
#![feature(new_uninit)]

mod ast;
mod bytecode;
mod compile;
mod vm;

#[cfg(test)]
mod test;
