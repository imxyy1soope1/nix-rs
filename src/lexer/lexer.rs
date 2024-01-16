use crate::token::Token;
use std::str;

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
