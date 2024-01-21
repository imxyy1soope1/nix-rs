#[cfg(test)]
mod test {
    use super::super::Parser;
    use crate::lexer::Lexer;

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
            "{a, b, ...} @ d: { c = a + b; }",
            "({ a, b, ... } @ d: { c = (a + b); })",
        );
        _test_parse(
            "d @ {a, b, ...}: { c = a + b; }",
            "({ a, b, ... } @ d: { c = (a + b); })",
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
            "(let a = 1; b = (a + 1); in (a + b))",
        );
    }

    #[test]
    fn test_parse_with() {
        _test_parse("with {a = 1;}; a + 1", "(with { a = 1; }; (a + 1))");
        _test_parse("with pkgs; [hello]", "(with pkgs; [ hello ])");
    }

    #[test]
    fn test_parse_assert() {
        _test_parse("assert true; 1", "(assert true; 1)");
    }

    #[test]
    fn test_parse_rec() {
        _test_parse("rec { a = 1; b = a + 1; }", "rec { a = 1; b = (a + 1); }");
        _test_parse(
            "with rec { a = 1; b = a + 1; }; a + b",
            "(with rec { a = 1; b = (a + 1); }; (a + b))",
        );
    }

    #[test]
    fn test_parse_inherit() {
        _test_parse(
            "let super = { a = 1; b = 2; }; in { inherit (super) a b; }",
            "(let super = { a = 1; b = 2; }; in { inherit (super) a b; })",
        )
    }

    #[test]
    fn test_parse_path() {
        _test_parse("./.", "./.");
        _test_parse("./test", "./test");
        _test_parse("/test/../.", "/test/../.");
        _test_parse("<.>", "<./.>");
        _test_parse("<nixpkgs>", "<./nixpkgs>");
    }

    #[test]
    fn test_call() {
        _test_parse("(a: b: a + b) 1 2", "(((a: (b: (a + b))) 1) 2)");
        _test_parse(
            "(a: b: a + b) ((a: a + 1) 2) 3",
            "(((a: (b: (a + b))) ((a: (a + 1)) 2)) 3)",
        )
    }

    #[test]
    fn test_parse() {
        let input = r#"let five = 5;
            time_two = num: num * 2;
        in rec {
            ten = time_two five;
            ast1 = assert ten == 10; true;
            ast2 = assert ten != 9; true;
            attr = { inherit ten; };
            pkgs = with (import null); [
                hello
            ];
            f = 1.0;
            s = "test";
            # comments
            b = (true && false) -> /* long comments */(true || false);
            b2 = attr ? ten;
            l = ["1" "2" 1 2];
            a.b.c = 1;
        }"#;

        let expect = r#"(let five = 5; time_two = (num: (num * 2)); in rec { ten = (time_two five); ast1 = (assert (ten == 10); true); ast2 = (assert (ten != 9); true); attr = { inherit ten; }; pkgs = (with (import null); [ hello ]); f = 1; s = "test"; b = ((true && false) -> (true || false)); b2 = (attr ? ten); l = [ "1" "2" 1 2 ]; ((a . b) . c) = 1; })"#;

        _test_parse(input, expect);
    }
}
