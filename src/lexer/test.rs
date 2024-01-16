#[cfg(test)]
mod test {
    use super::super::lexer::Lexer;
    use crate::token::Token::*;
    use std::iter::zip;

    #[test]
    fn test_next_token() {
        let input = r"let five = 5;
            time_two = num: num * 2;
        in {
            ten = time_two five;
            ast1 = assert ten == 10;
            ast2 = assert ten != 9;
            attr = { inherit ten; };
            pkgs = with (import <nixpkgs>); [
                hello
            ];
        }";
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
            NE,
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
            IMPORT,
            LT,
            IDENT("nixpkgs".to_string()),
            GT,
            RPAREN,
            SEMI,
            LBRACKET,
            IDENT("hello".to_string()),
            RBRACKET,
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
