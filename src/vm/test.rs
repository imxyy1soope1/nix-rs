use rpds::vector;

use crate::compile::compile;
use crate::value::*;

use super::vm::run;

#[inline]
fn test_expr(expr: &str, expected: Value) {
    let prog = compile(expr).unwrap();
    assert_eq!(run(prog).unwrap(), expected);
}

macro_rules! int {
    ($e:expr) => (Value::Const(Const::Int($e)))
}

macro_rules! float {
    ($e:expr) => (Value::Const(Const::Float($e as f64)))
}

macro_rules! boolean {
    ($e:expr) => (Value::Const(Const::Bool($e)))
}

macro_rules! list {
    () => (
        Value::List(List::new(vector![]))
    );
    ($($x:tt)*) => (
        Value::List(List::new(vector![$($x)*]))
    );
}

#[test]
fn test_arith() {
    test_expr("1", int!(1));
    test_expr("1.", float!(1));
    test_expr("-1", int!(-1));
    test_expr("-1.", float!(-1));
    test_expr("1 + 1", int!(2));
    test_expr("1 + 1.", float!(2));
    test_expr("1. + 1", float!(2));
    test_expr("1. + 1.", float!(2));
    test_expr("1 - 1", int!(0));
    test_expr("1 - 1.", float!(0));
    test_expr("1. - 1", float!(0));
    test_expr("1. - 1.", float!(0));
    test_expr("1 * 1", int!(1));
    test_expr("1 * 1.", float!(1));
    test_expr("1. * 1", float!(1));
    test_expr("1. * 1.", float!(1));
    test_expr("1 / 1", int!(1));
    test_expr("1 / 1.", float!(1));
    test_expr("1. / 1", float!(1));
    test_expr("1. / 1.", float!(1));
}

#[test]
fn test_bool() {
    test_expr("true", boolean!(true));
    test_expr("false", boolean!(false));
    test_expr("!false", boolean!(true));
    test_expr("true && false", boolean!(false));
    test_expr("true || false", boolean!(true));
    test_expr("true -> false", boolean!(false));
}

#[test]
fn test_list() {
    test_expr("[ 1 2 3 true ]", list![int!(1), int!(2), int!(3), boolean!(true)]);
}
