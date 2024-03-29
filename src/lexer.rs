use crate::token::Token;
use std::fmt::Display;

/* pub enum SourcePath {
    None,
    String,
    Stdin,
    Path(Box<Path>),
}

impl Display for SourcePath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::None => write!(f, "«none»"),
            Self::String => write!(f, "«string»"),
            Self::Stdin => write!(f, "«stdin»"),
            Self::Path(path) => write!(f, "")
        }
    }
} */

#[derive(Clone, Copy)]
pub struct Pos {
    line: i64,
    ch: i64,
}

impl Display for Pos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.ch)
    }
}

impl Pos {
    fn new() -> Pos {
        Pos { line: 1, ch: 0 }
    }

    fn nextline(&mut self) {
        self.line += 1;
    }

    fn nextchar(&mut self) {
        self.ch += 1;
    }

    fn clearchar(&mut self) {
        self.ch = 0;
    }
}

#[allow(unused)]
pub struct Lexer {
    input: String,
    chars: Vec<char>,
    hpos: Pos,
    pos: usize,
    next_pos: usize,
    cur_ch: Option<char>,
    next_ch: Option<char>,
    finished: bool,
}

#[inline]
fn is_letter(ch: char) -> bool {
    ch.is_ascii_lowercase() || ch.is_ascii_uppercase() || ch == '_' || ch == '-'
}

#[inline]
fn is_white(ch: char) -> bool {
    ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r'
}

fn escape_string(s: String) -> (String, Vec<(usize, Vec<Token>)>) {
    if s == "\\" {
        panic!();
    } else if let 0..=1 = s.len() {
        return (s, Vec::new());
    }

    let mut buf = String::new();
    let mut chars = s.chars().peekable();
    let mut c = Some('\0');
    let mut replaces: Vec<(usize, Vec<Token>)> = Vec::new();
    let mut i = 0;
    let mut skip = true;
    'outer: while c.is_some() {
        if !skip {
            i += 1;
        } else {
            skip = false;
        }
        c = chars.next();
        if c.is_none() {
            break;
        }
        buf.push(match c.unwrap() {
            '\\' => {
                c = chars.next();
                i += 1;
                match c.unwrap_or_default() {
                    'n' => '\n',
                    't' => '\t',
                    'r' => '\r',
                    '$' => '$',
                    '\\' => '\\',
                    '\0' => panic!("string not closed"),
                    _ => panic!(),
                }
            }
            '$' => {
                c = chars.next();
                match c.unwrap_or_default() {
                    '{' => {
                        let mut l = Lexer::build(&String::from_iter(chars.clone()));
                        let mut tokens: Vec<Token> = Vec::new();
                        loop {
                            match l.next().unwrap() {
                                Token::RBRACE => {
                                    tokens.push(Token::EOF);
                                    replaces.push((i, tokens));
                                    while c.unwrap() != '}' {
                                        c = chars.next();
                                    }
                                    skip = true;
                                    continue 'outer;
                                }
                                Token::EOF => panic!("unclosed DOLLAR_CURLY"),
                                v => tokens.push(v),
                            }
                        }
                    }
                    c => c,
                }
            }
            '\r' => {
                c = chars.peek().copied();
                match c.unwrap_or_default() {
                    '\n' => {
                        c = chars.next();
                        i += 1;
                        '\n'
                    }
                    _ => continue,
                }
            }
            c => c,
        })
    }

    (buf, replaces)
}

impl Lexer {
    pub fn build(s: &str) -> Lexer {
        assert!(!s.trim().is_empty(), "s must not be empty!");
        let input = s.trim().to_string();
        let mut l = Lexer {
            input,
            chars: s.chars().collect(),
            pos: 0,
            hpos: Pos::new(),
            next_pos: 0,
            cur_ch: None,
            next_ch: None,
            finished: false,
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
        if self.cur_ch.unwrap_or_default() == '\n' {
            self.hpos.nextline();
            self.hpos.clearchar();
        } else if self.cur_ch.is_some() {
            self.hpos.nextchar();
        }
    }

    fn peek(&self) -> Option<char> {
        if self.next_ch.is_some() {
            Some(self.chars[self.next_pos + 1])
        } else {
            None
        }
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
            "assert" => ASSERT,
            "with" => WITH,
            "let" => LET,
            "in" => IN,
            "rec" => REC,
            "inherit" => INHERIT,
            "or" => ORKW,

            ident => IDENT(ident.to_string()),
        }
    }

    fn read_number(&mut self) -> Token {
        let pos = self.pos;
        let mut is_float = false;
        while self.next_ch.unwrap_or_default().is_ascii_digit()
            || (if self.next_ch.unwrap_or_default() == '.' && !is_float {
                is_float = true;
                true
            } else {
                false
            })
        {
            self.read_char();
        }

        if is_float {
            Token::FLOAT(self.input[pos..=self.pos].to_string())
        } else {
            Token::INT(self.input[pos..=self.pos].to_string())
        }
    }

    fn read_float(&mut self) -> Token {
        let pos = self.pos;
        while self.next_ch.unwrap_or_default().is_ascii_digit() {
            self.read_char();
        }

        Token::FLOAT(self.input[pos..=self.pos].to_string())
    }

    fn read_string(&mut self) -> Token {
        // FIXME
        self.read_char();
        let pos = self.pos;
        while self.cur_ch.map_or(false, |c| c != '"') {
            self.read_char();
        }

        let (s, r) = escape_string(self.input[pos..self.pos].to_string());
        Token::STRING(s, r)
    }

    fn read_lines(&mut self) -> Token {
        // FIXME
        unimplemented!();
        /*
        self.read_char();
        self.read_char();
        let pos = self.pos;
        while self.next_ch.map_or(false, |c| c != '\'') || self.cur_ch.map_or(false, |c| c != '\'') {
            self.read_char();
        }

        Token::ILLEGAL */
    }

    fn skip_comment(&mut self) {
        while self.cur_ch.unwrap_or_default() != '\n' {
            self.read_char();
        }
        self.read_char();
    }

    fn skip_long_comment(&mut self) {
        while self.cur_ch.unwrap_or_default() != '*' || self.next_ch.unwrap_or_default() != '/' {
            self.read_char();
        }
        self.read_char();
        self.read_char();
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
        if self.finished {
            return None;
        }

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
            '+' => {
                if self.next_ch.unwrap_or_default() != '+' {
                    PLUS
                } else {
                    self.read_char();
                    CONCAT
                }
            }
            '-' => {
                if self.next_ch.unwrap_or_default() != '>' {
                    MINUS
                } else {
                    self.read_char();
                    IMPL
                }
            }
            '*' => MUL,
            '/' => match self.next_ch.unwrap_or_default() {
                '/' => {
                    self.read_char();
                    UPDATE
                }
                '*' => {
                    self.skip_long_comment();
                    return self.next();
                }
                _ => SLASH,
            },
            '!' => {
                if self.next_ch.unwrap_or_default() != '=' {
                    BANG
                } else {
                    self.read_char();
                    NEQ
                }
            }
            '&' => {
                if self.next_ch.unwrap_or_default() != '&' {
                    ILLEGAL
                } else {
                    self.read_char();
                    AND
                }
            }
            '|' => {
                if self.next_ch.unwrap_or_default() != '|' {
                    ILLEGAL
                } else {
                    self.read_char();
                    OR
                }
            }

            '<' => {
                if self.next_ch.unwrap_or_default() != '=' {
                    LANGLE
                } else {
                    self.read_char();
                    LEQ
                }
            }
            '>' => {
                if self.next_ch.unwrap_or_default() != '=' {
                    RANGLE
                } else {
                    self.read_char();
                    GEQ
                }
            }

            ',' => COMMA,
            ';' => SEMI,
            ':' => COLON,
            '.' => {
                if self.next_ch.unwrap_or_default() == '.' {
                    if self.peek().unwrap_or_default() == '.' {
                        self.read_char();
                        self.read_char();
                        ELLIPSIS
                    } else {
                        self.read_char();
                        PARENT
                    }
                } else if self.next_ch.unwrap_or_default().is_ascii_digit() {
                    println!("{}", self.next_ch.unwrap_or_default());
                    self.read_float()
                } else {
                    DOT
                }
            }
            '~' => WAVY,
            '?' => QUEST,
            '@' => AT,

            '(' => LPAREN,
            ')' => RPAREN,
            '[' => LBRACKET,
            ']' => RBRACKET,
            '{' => LBRACE,
            '}' => RBRACE,
            '$' => {
                if self.next_ch.unwrap_or_default() == '{' {
                    self.read_char();
                    DOLLARCURLY
                } else {
                    ILLEGAL
                }
            }

            '\0' => {
                self.finished = true;
                EOF
            }

            '"' => self.read_string(),
            /* '\'' => {
                if self.next_ch.unwrap_or_default() == '\'' {
                    self.read_lines()
                } else {
                    ILLEGAL
                }
            } */
            '#' => {
                self.skip_comment();
                return self.next();
            }

            ch => {
                if is_letter(ch) {
                    self.read_ident()
                } else if ch.is_ascii_digit() {
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
