use crate::token::Token;
use std::str;
// use unicode_segmentation::{Graphemes, UnicodeSegmentation};

#[allow(unused)]
pub struct Lexer {
    input: String,
    chars: Vec<char>,
    pos: usize,
    next_pos: usize,
    cur_ch: Option<char>,
    next_ch: Option<char>,
}

#[inline]
fn is_letter(ch: char) -> bool {
    'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_'
}

#[inline]
fn is_white(ch: char) -> bool {
    ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r'
}

impl Lexer {
    pub fn from_str(s: &str) -> Lexer {
        assert!(!s.is_empty(), "s must not be empty!");
        let input = s.to_string();
        let mut l = Lexer {
            input,
            chars: s.chars().collect(),
            pos: 0,
            next_pos: 0,
            cur_ch: None,
            next_ch: None,
        };
        l.read_char();
        l
    }

    fn _read_wrap(&mut self) -> Option<char> {
        if self.next_pos < self.input.len() {
            Some(self.chars[self.next_pos])
        } else {
            None
        }
    }

    fn read_char(&mut self) {
        self.cur_ch = self._read_wrap();
        self.pos = self.next_pos;
        self.next_pos += 1;
        self.next_ch = self._read_wrap();
    }

    fn read_ident(&mut self) -> Token {
        use crate::token::Token::*;
        let pos = self.pos;
        while is_letter(self.next_ch.unwrap_or_default())
            || self.next_ch.unwrap_or_default().is_numeric()
                && !is_white(self.next_ch.unwrap_or_default())
        {
            self.read_char();
        }

        match &self.input[pos..=self.pos] {
            "if" => IF,
            "then" => THEN,
            "else" => ELSE,
            "let" => LET,
            "in" => IN,
            "with" => WITH,
            "import" => IMPORT,
            "assert" => ASSERT,
            "inherit" => INHERIT,
            ident => IDENT(ident.to_string()),
        }
    }

    fn read_number(&mut self) -> Token {
        let pos = self.pos;
        while self.next_ch.unwrap_or_default().is_digit(10) {
            self.read_char();
        }

        Token::INT(self.input[pos..=self.pos].to_string())
    }

    fn skip_white(&mut self) {
        let mut ch = self.cur_ch.unwrap_or_default();
        while is_white(ch) {
            self.read_char();
            ch = self.cur_ch.unwrap_or_default();
        }
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        use crate::token::Token::*;

        self.skip_white();

        let res = Some(match self.cur_ch.unwrap_or_default() {
            '=' => {
                if self.next_ch.unwrap_or_default() != '=' {
                    ASSIGN
                } else {
                    self.read_char();
                    EQ
                }
            }
            '+' => PLUS,
            '-' => MINUS,
            '*' => MUL,
            '/' => SLASH,
            '!' => {
                if self.next_ch.unwrap_or_default() != '=' {
                    BANG
                } else {
                    self.read_char();
                    NE
                }
            }

            '<' => LT,
            '>' => GT,

            ',' => COMMA,
            ';' => SEMI,
            ':' => COLON,
            '.' => DOT,

            '(' => LPAREN,
            ')' => RPAREN,
            '[' => LBRACKET,
            ']' => RBRACKET,
            '{' => LBRACE,
            '}' => RBRACE,

            '\0' => EOF,

            ch => {
                if ch.is_ascii() && is_letter(self.chars[self.pos]) {
                    self.read_ident()
                } else if self.chars[self.pos].is_digit(10) {
                    self.read_number()
                } else {
                    ILLEGAL
                }
            }
        });

        self.read_char();

        res
    }
}

#[cfg(test)]
mod test {
    use super::*;
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
