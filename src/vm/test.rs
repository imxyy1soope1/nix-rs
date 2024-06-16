use rnix::Root;

use crate::compile::compile;

use super::vm::VM;

fn test_expr(expr: &str) {
    let prog = compile(expr).unwrap();
    let vm = VM::new(prog);
}

#[test]
fn test_literal() {}
