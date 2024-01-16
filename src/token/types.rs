use std::fmt;

#[allow(unused)]
#[derive(Debug, PartialEq)]
pub enum Token {
    ILLEGAL,
    EOF,

    IDENT(String),
    INT(String),

    ASSIGN,
    PLUS,
    MINUS,
    MUL,
    SLASH,
    BANG,

    LT,
    GT,
    EQ,
    NE,

    COMMA,
    SEMI,
    COLON,
    DOT,

    LPAREN,
    RPAREN,
    LBRACKET,
    RBRACKET,
    LBRACE,
    RBRACE,

    IF,
    THEN,
    ELSE,
    LET,
    IN,
    WITH,
    IMPORT,
    ASSERT,
    INHERIT,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Token::*;
        match self {
            IDENT(literal) => write!(f, "{literal}"),
            INT(literal) => write!(f, "{literal}"),

            ASSIGN => write!(f, "="),
            PLUS => write!(f, "+"),
            MINUS => write!(f, "-"),
            MUL => write!(f, "*"),
            SLASH => write!(f, "/"),
            BANG => write!(f, "!"),

            LT => write!(f, "<"),
            GT => write!(f, ">"),
            EQ => write!(f, "=="),
            NE => write!(f, "!="),

            COMMA => write!(f, ","),
            SEMI => write!(f, ";"),
            COLON => write!(f, ":"),
            DOT => write!(f, "."),

            LPAREN => write!(f, ")"),
            RPAREN => write!(f, "("),
            LBRACKET => write!(f, "["),
            RBRACKET => write!(f, "]"),
            LBRACE => write!(f, "{{"),
            RBRACE => write!(f, "}}"),

            IF => write!(f, "if"),
            THEN => write!(f, "then"),
            ELSE => write!(f, "else"),
            LET => write!(f, "let"),
            IN => write!(f, "in"),
            WITH => write!(f, "with"),
            IMPORT => write!(f, "import"),
            ASSERT => write!(f, "assert"),
            INHERIT => write!(f, "inherit"),

            ILLEGAL => write!(f, "ILLEGAL"),
            EOF => write!(f, "EOF"),
        }
    }
}
