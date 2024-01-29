use nix_rs::convany;
use nix_rs::eval;
use nix_rs::object::*;

macro_rules! test_type {
    ( $input:expr, $type:tt ) => {
        let e = eval($input.to_string());
        println!("{e}");
        assert!(e.as_any().is::<$type>());
    };
}

macro_rules! test_eq {
    ( $input:expr, $type:tt, $val:expr ) => {
        let e = eval($input.to_string());
        assert_eq!(e.as_any().downcast_ref::<$type>().unwrap().value, $val);
    };
}

macro_rules! test_raw {
    ( $input:expr, $type:tt, $val:expr ) => {
        let e = eval($input.to_string());
        assert_eq!(convany!(e.as_any(), $type), &$val);
    };
}

#[test]
fn test_int() {
    test_raw!("1", Int, 1);
}

#[test]
fn test_float() {
    test_raw!("1.", Float, 1.0f64);
    test_raw!(".1", Float, 0.1f64);
    test_raw!("1.1", Float, 1.1f64);
}

#[test]
fn test_prefix() {
    test_raw!("-1", Int, -1);
    test_raw!("-1.0", Float, -1.0f64);
    test_raw!("!true", Bool, false);
}

#[test]
fn test_infix() {
    test_raw!("1 + 1", Int, 2);
    test_raw!("1.0 + 1", Float, 2f64);
    test_raw!("true && false", Bool, false);
    test_raw!("true || false", Bool, true);
}

#[test]
fn test_literal() {
    test_raw!(r#""test""#, Str, "test");
    test_raw!(r#"let test = "a"; in "${test}""#, Str, "a");
    test_raw!("true", Bool, true);
    test_raw!("false", Bool, false);
    test_type!("null", Null);
}

#[test]
fn test_attrs() {
    test_type!(r#"{a = 1; b = "test";}"#, Attrs);
}

#[test]
fn test_function() {
    test_raw!("(a: a + 1) 1", Int, 2);
    test_raw!("(a: b: a + b) 1 2", Int, 3);
    test_raw!("(let b = 1; in {a?b}: a) {}", Int, 1);
    test_type!("let f = args@{a?42, ...}: [a args]; in f {b = 1;}", List);
    // test_raw!("({b?1, c?b}: b + c) {b=2;}", Int, 4);
}

#[test]
fn test_let() {
    test_raw!("let a = 1; b = a + 1; in b", Int, 2);
}

#[test]
fn test_with() {
    test_raw!("with { a = 1; }; a + 1", Int, 2);
}
