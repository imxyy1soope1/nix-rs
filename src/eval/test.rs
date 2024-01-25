#[cfg(test)]
mod test {
    use core::panic;

    use super::super::Eval;
    use crate::lexer::Lexer;
    use crate::object::*;
    use crate::parser::Parser;

    macro_rules! test_type {
        ( $input:expr, $type:tt ) => {
            let mut e = Eval::new(Parser::new(Box::new(Lexer::build($input))).parse());
            let e = e.eval();
            println!("{e}");
            assert!(e.as_any().is::<$type>());
        };
    }

    macro_rules! test_eq {
        ( $input:expr, $type:tt, $val:expr ) => {
            let mut e = Eval::new(Parser::new(Box::new(Lexer::build($input))).parse());
            let e = e.eval();
            assert_eq!(e.as_any().downcast_ref::<$type>().unwrap().value, $val);
        };
    }

    #[test]
    fn test_int() {
        test_eq!("1", Int, 1);
    }

    #[test]
    fn test_float() {
        test_eq!("1.", Float, 1.0f64);
        test_eq!(".1", Float, 0.1f64);
        test_eq!("1.1", Float, 1.1f64);
    }

    #[test]
    fn test_prefix() {
        test_eq!("-1", Int, -1);
        test_eq!("-1.0", Float, -1.0f64);
        test_eq!("!true", Bool, false);
    }

    #[test]
    fn test_infix() {
        test_eq!("1 + 1", Int, 2);
        test_eq!("1.0 + 1", Float, 2f64);
        test_eq!("true && false", Bool, false);
        test_eq!("true || false", Bool, true);
    }

    #[test]
    fn test_literal() {
        test_eq!(r#""test""#, Str, "test");
        test_eq!(r#"let test = "a"; in "${test}""#, Str, "a");
        test_eq!("true", Bool, true);
        test_eq!("false", Bool, false);
        test_type!("null", Null);
    }

    #[test]
    fn test_attrs() {
        test_type!(r#"{a = 1; b = "test";}"#, Attrs);
    }

    #[test]
    fn test_function() {
        test_eq!("(a: a + 1) 1", Int, 2);
        test_eq!("(a: b: a + b) 1 2", Int, 3);
        test_eq!("(let b = 1; in {a?b}: a) {}", Int, 1);
        test_type!("let f = args@{a?42, ...}: [a args]; in f {b = 1;}", List);
        // test_eq!("({b?1, c?b}: b + c) {b=2;}", Int, 4);
    }

    #[test]
    fn test_let() {
        test_eq!("let a = 1; b = a + 1; in b", Int, 2);
    }

    #[test]
    fn test_with() {
        test_eq!("with { a = 1; }; a + 1", Int, 2);
    }
}
