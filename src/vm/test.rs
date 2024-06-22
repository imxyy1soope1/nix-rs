use crate::compile::compile;
use crate::bytecode::Const;

use super::vm::run;
use super::value::OwnedValue;

fn test_expr(expr: &str, expected: OwnedValue) {
    let prog = compile(expr).unwrap();
    assert_eq!(run(prog).unwrap(), expected);
}

#[test]
fn test_literal() {
    test_expr("1", OwnedValue::Const(Const::Int(1)));
    test_expr("1.", OwnedValue::Const(Const::Float(1.)));
}
