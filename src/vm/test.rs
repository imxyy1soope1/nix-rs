use rnix::Root;

use crate::compile::compile;

use super::vm::{VmData, VM};

fn test_expr(expr: &str) {
    let prog = compile(expr).unwrap();
    let data = VmData::new(prog.consts, prog.symbols);
    let vm = VM::new(&data, prog.thunks);
}

#[test]
fn test_literal() {
    test_expr("1");
}
