use ecow::EcoString;
use rpds::{ht_map_sync, vector_sync};

use crate::compile::compile;
use crate::value::*;

use super::vm::run;

#[inline]
fn test_expr(expr: &str, expected: Value) {
    let prog = compile(expr).unwrap();
    dbg!(&prog);
    assert_eq!(run(prog).unwrap(), expected);
}

macro_rules! int {
    ($e:expr) => {
        Value::Const(Const::Int($e))
    };
}

macro_rules! float {
    ($e:expr) => {
        Value::Const(Const::Float($e as f64))
    };
}

macro_rules! boolean {
    ($e:expr) => {
        Value::Const(Const::Bool($e))
    };
}

macro_rules! string {
    ($e:expr) => {
        Value::Const(Const::String(EcoString::from($e)))
    };
}

macro_rules! symbol {
    ($e:expr) => {
        Symbol::from($e.to_string())
    };
}

macro_rules! list {
    ($($x:tt)*) => (
        Value::List(List::new(vector_sync![$($x)*]))
    );
}

macro_rules! attrs {
    ($($x:tt)*) => (
        Value::AttrSet(AttrSet::new(ht_map_sync!{$($x)*}))
    )
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
fn test_string() {
    test_expr(r#""test""#, string!("test"));
    test_expr(r#""hello" + " world""#, string!("hello world"));
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
    test_expr(
        "[ 1 2 3 true ]",
        list![int!(1), int!(2), int!(3), boolean!(true)],
    );
    test_expr(
        "[ 1 2 ] ++ [ 3 4 ]",
        list![int!(1), int!(2), int!(3), int!(4)],
    );
}

#[test]
fn test_attrs() {
    test_expr(
        "{ a = 1; }",
        attrs! {
            symbol!("a") => int!(1)
        },
    );
    test_expr("{ a = 1; }.a", int!(1));
    test_expr("{ a = 1; }.b or 1", int!(1));
    test_expr(
        "{ a = { a = 1; }; }.a",
        attrs! {
            symbol!("a") => int!(1)
        },
    );
    test_expr("{ a.b = 1; }.a.b", int!(1));
    test_expr(
        "{ a.b = 1; a.c = 2; }",
        attrs! { symbol!("a") => attrs!{ symbol!("b") => int!(1), symbol!("c") => int!(2) } },
    );
    test_expr("{ a.b = 1; } ? a.b", boolean!(true));
    test_expr(
        "{ a.b = 1; } // { a.c = 2 }",
        attrs! { symbol!("a") => attrs!{ symbol!("b") => int!(1), symbol!("c") => int!(2) } },
    );
}

#[test]
fn test_if() {
    test_expr("if true || false then 1 else 2", int!(1));
}

#[test]
fn test_with() {
    test_expr(r#"with { a = 1; }; a"#, int!(1));
}

#[test]
fn test_let() {
    test_expr(r#"let a = 1; in a"#, int!(1));
    test_expr(r#"let a = { a = 1; }; b = "a"; in a.${b}"#, int!(1));
    test_expr(
        r#"let b = "c"; in { a.b = 1; } // { a."a${b}" = 2 }"}"#,
        attrs! { symbol!("a") => attrs!{ symbol!("b") => int!(1), symbol!("ac") => int!(2) } },
    );
}
