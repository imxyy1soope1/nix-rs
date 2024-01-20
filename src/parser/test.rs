#[cfg(test)]
mod test {
    use super::super::Parser;
    use crate::lexer::Lexer;
    use crate::token::Token::*;

    fn _test_parse(input: &str, expect: &str) {
        let mut parser = Parser::from_lexer(Lexer::from_str(input));
        assert_eq!(parser.parse().to_string(), expect)
    }

    #[test]
    fn test_parse_ident() {
        _test_parse("five", "five");
    }

    #[test]
    fn test_parse_int() {
        _test_parse("5", "5");
    }

    #[test]
    fn test_parse_bool() {
        _test_parse("true", "true");
        _test_parse("false", "false");
    }

    #[test]
    fn test_parse_null() {
        _test_parse("null", "null")
    }

    #[test]
    fn test_parse_string() {
        _test_parse(r#""test""#, r#""test""#)
    }

    #[test]
    fn test_parse_prefix() {
        _test_parse("-5", "(-5)");
        _test_parse("!true", "(!true)");
    }

    #[test]
    fn test_parse_infix() {
        _test_parse("5 + 5", "(5 + 5)");
        _test_parse("5 - 5", "(5 - 5)");
        _test_parse("5 * 5", "(5 * 5)");
        _test_parse("5 * 5 + 5", "((5 * 5) + 5)");
        // _test_parse("5 / 5", "(5 / 5)");
        _test_parse("true && false", "(true && false)");
        _test_parse("true || false", "(true || false)");
        _test_parse("true -> false", "(true -> false)");
        _test_parse("a.b ? c", "((a . b) ? c)");
    }

    #[test]
    fn test_parse_group() {
        // _test_parse("(5 + 5) * 5", "(5 + 5) * 5");
        _test_parse("5 * (5 + 5)", "(5 * (5 + 5))");
    }

    #[test]
    fn test_parse_if() {
        _test_parse(
            "if true then true else false",
            "if true then true else false",
        );
    }

    #[test]
    fn test_parse_binding() {
        _test_parse(r#"test="test""#, r#"test = "test""#);
    }

    #[test]
    fn test_parse_attrs() {
        _test_parse(
            r#"{test="test";"123"="asdf";}"#,
            r#"{ test = "test"; "123" = "asdf"; }"#,
        );
        _test_parse(
            "{a, b, ...}: { c = a + b; }",
            "({ a, b, ... }: { c = (a + b); })",
        );
    }

    #[test]
    fn test_parse_list() {
        _test_parse(r#"[1 2 "1" "3"]"#, r#"[ 1 2 "1" "3" ]"#);
        _test_parse("[(a+b) (b+c)]", "[ (a + b) (b + c) ]")
    }

    #[test]
    fn test_parse_let() {
        _test_parse(
            "let a = 1; b = a + 1; in a + b",
            "let a = 1; b = (a + 1); in (a + b)",
        );
    }

    #[test]
    fn test_parse_with() {
        _test_parse("with {a = 1;}; a + 1", "with { a = 1; }; (a + 1)");
        _test_parse("with pkgs; [hello]", "with pkgs; [ hello ]");
    }

    #[test]
    fn test_parse_rec() {
        _test_parse("rec { a = 1; b = a + 1; }", "rec { a = 1; b = (a + 1); }");
        _test_parse(
            "with rec { a = 1; b = a + 1; }; a + b",
            "with rec { a = 1; b = (a + 1); }; (a + b)",
        );
    }

    #[test]
    fn test_parse_inherit() {
        _test_parse(
            "let super = { a = 1; b = 2; }; in { inherit (super) a b; }",
            "let super = { a = 1; b = 2; }; in { inherit (super) a b; }",
        )
    }

    #[test]
    fn test_call() {
        _test_parse("(a: b: a + b) 1 2", "(((a: (b: (a + b))) 1) 2)");
    }

    // #[test]
    fn test_parse() {
        /*
         ```nix
         let five = 5;
           time_two = num: num * 2;
         in {
           ten = time_two five;
           ast1 = assert ten == 10;
           ast2 = assert ten != 9;
           attr = { inherit ten; };
           pkgs = with (import <nixpkgs>); [
               hello
           ];
           f = 1.0;
           il = 1.0.1;
         ```
        }*/
        let input = [
            LET,
            IDENT("five".to_string()),
            ASSIGN,
            INT("5".to_string()),
            SEMI,
            IDENT("time_two".to_string()),
            ASSIGN,
            IDENT("num".to_string()),
            COLON,
            IDENT("num".to_string()),
            MUL,
            INT("2".to_string()),
            SEMI,
            IN,
            LBRACE,
            IDENT("ten".to_string()),
            ASSIGN,
            IDENT("time_two".to_string()),
            IDENT("five".to_string()),
            SEMI,
            IDENT("ast1".to_string()),
            ASSIGN,
            ASSERT,
            IDENT("ten".to_string()),
            EQ,
            INT("10".to_string()),
            SEMI,
            IDENT("ast2".to_string()),
            ASSIGN,
            ASSERT,
            IDENT("ten".to_string()),
            NEQ,
            INT("9".to_string()),
            SEMI,
            IDENT("attr".to_string()),
            ASSIGN,
            LBRACE,
            INHERIT,
            IDENT("ten".to_string()),
            SEMI,
            RBRACE,
            SEMI,
            IDENT("pkgs".to_string()),
            ASSIGN,
            WITH,
            LPAREN,
            IDENT("import".to_string()),
            LANGLE,
            IDENT("nixpkgs".to_string()),
            RANGLE,
            RPAREN,
            SEMI,
            LBRACKET,
            IDENT("hello".to_string()),
            RBRACKET,
            SEMI,
            IDENT("f".to_string()),
            ASSIGN,
            FLOAT("1.0".to_string()),
            SEMI,
            IDENT("il".to_string()),
            ASSIGN,
            FLOAT("1.0".to_string()),
            DOT,
            INT("1".to_string()),
            SEMI,
            RBRACE,
            EOF,
        ];

        // let expect =
    }
}
