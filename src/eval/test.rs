#[cfg(test)]
mod test {
    use super::super::Eval;
    use crate::lexer::Lexer;
    use crate::object::*;
    use crate::parser::Parser;

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
    }

    #[test]
    fn test_literal() {
        test_eq!(r#""test""#, Str, "test");
        // test_eq!(r#""""#)
    }
}
