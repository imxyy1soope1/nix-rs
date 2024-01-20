#[cfg(test)]
mod test {
    use super::super::lexer::Lexer;
    use crate::token::Token::*;
    use std::iter::zip;

    #[test]
    fn test_lexer() {
        let input = r#"let five = 5;
            time_two = num: num * 2;
        in rec {
            ten = time_two five;
            ast1 = assert ten == 10;
            ast2 = assert ten != 9;
            attr = { inherit ten; };
            pkgs = with (import <nixpkgs>); [
                hello
            ];
            f = 1.0;
            il = 1.0.1;
            s = "test";
            # comments
            b = (true && false) -> (true || false);
            b2 = attr ? ten;
        }"#;
        let expect = [
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
            REC,
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
            IDENT("s".to_string()),
            ASSIGN,
            STRING("test".to_string()),
            SEMI,
            IDENT("b".to_string()),
            ASSIGN,
            LPAREN,
            TRUE,
            AND,
            FALSE,
            RPAREN,
            IMPL,
            LPAREN,
            TRUE,
            OR,
            FALSE,
            RPAREN,
            SEMI,
            IDENT("b2".to_string()),
            ASSIGN,
            IDENT("attr".to_string()),
            QUEST,
            IDENT("ten".to_string()),
            SEMI,
            RBRACE,
            EOF,
        ];

        let l = Lexer::from_str(input);

        for (actual, exp) in zip(l, expect) {
            assert_eq!(actual, exp);
        }
    }
}