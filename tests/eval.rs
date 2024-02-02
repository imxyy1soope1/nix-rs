use nix_rs::eval;
use nix_rs::object::*;

macro_rules! test_type {
    ( $input:expr, $type_id:expr ) => {
        let e = eval($input.to_string()).unwrap();
        println!("{e}");
        assert_eq!(e.objtype(), $type_id);
    };
}

macro_rules! test_raw {
    ( $input:expr, $conv:tt, $val:expr ) => {
        let e = eval($input.to_string()).unwrap();
        assert_eq!(e.$conv().unwrap(), $val);
    };
}

#[test]
fn test_int() {
    test_raw!("1", try_into_int, 1);
}

#[test]
fn test_float() {
    test_raw!("1.", try_into_float, 1.0f64);
    test_raw!(".1", try_into_float, 0.1f64);
    test_raw!("1.1", try_into_float, 1.1f64);
}

#[test]
fn test_prefix() {
    test_raw!("-1", try_into_int, -1);
    test_raw!("-1.0", try_into_float, -1.0f64);
    test_raw!("!true", try_into_bool, false);
}

#[test]
fn test_infix() {
    test_raw!("1 + 1", try_into_int, 2);
    test_raw!("1.0 + 1", try_into_float, 2f64);
    test_raw!("true && false", try_into_bool, false);
    test_raw!("true || false", try_into_bool, true);
}

#[test]
fn test_literal() {
    test_raw!(r#""test""#, try_into_string, "test");
    test_raw!(r#"let test = "a"; in "${test}""#, try_into_string, "a");
    test_raw!("true", try_into_bool, true);
    test_raw!("false", try_into_bool, false);
    test_type!("null", ObjectType::Null);
}

#[test]
fn test_attrs() {
    test_type!(r#"{a = 1; b = "test";}"#, ObjectType::Attrs);
}

#[test]
fn test_function() {
    test_raw!("(a: a + 1) 1", try_into_int, 2);
    test_raw!("(a: b: a + b) 1 2", try_into_int, 3);
    test_raw!("(let b = 1; in {a?b}: a) {}", try_into_int, 1);
    test_type!(
        "let f = args@{a?42, ...}: [a args]; in f {b = 1;}",
        ObjectType::List
    );
    test_raw!("({b?1, c?b}: b + c) {b=2;}", try_into_int, 4);
    test_raw!("({c?b, b?1}: b + c) {b=2;}", try_into_int, 4);
}

#[test]
fn test_let() {
    test_raw!("let a = 1; b = a + 1; in b", try_into_int, 2);
}

#[test]
fn test_with() {
    test_raw!("with { a = 1; }; a + 1", try_into_int, 2);
}
