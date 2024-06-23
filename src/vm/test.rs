use crate::compile::compile;
use crate::value::*;

use super::vm::run;

fn test_expr(expr: &str, expected: Value) {
    let prog = compile(expr).unwrap();
    assert_eq!(run(prog).unwrap(), expected);
}

#[test]
fn test_arith() {
    test_expr("1", Value::Const(Const::Int(1)));
    test_expr("1.", Value::Const(Const::Float(1.)));
    test_expr("-1", Value::Const(Const::Int(-1)));
    test_expr("-1.", Value::Const(Const::Float(-1.)));
}
