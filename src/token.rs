use std::fmt;

#[allow(unused)]
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Token {
    ILLEGAL,
    EOF,

    IDENT(String),
    INT(String),
    FLOAT(String),
    STRING(String),

    ASSIGN,
    PLUS,
    MINUS,
    MUL,
    SLASH,
    BANG,
    AND,
    OR,
    IMPL,
    UPDATE,
    CONCAT,

    EQ,
    NEQ,
    LEQ,
    GEQ,

    COMMA,
    SEMI,
    COLON,
    DOT,
    ELLIPSIS,

    LPAREN,
    RPAREN,
    LBRACKET,
    RBRACKET,
    LBRACE,
    RBRACE,
    LANGLE,
    RANGLE,

    TRUE,
    FALSE,

    NULL,

    IF,
    THEN,
    ELSE,
    ASSERT,
    WITH,
    LET,
    IN,
    REC,
    INHERIT,
    ORKW,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Token::*;
        match self {
            ILLEGAL => write!(f, "ILLEGAL"),
            EOF => write!(f, "EOF"),

            IDENT(literal) => write!(f, "{literal}"),
            INT(literal) => write!(f, "{literal}"),
            FLOAT(literal) => write!(f, "{literal}"),
            STRING(literal) => write!(f, r#""{literal}""#),

            ASSIGN => write!(f, "="),
            PLUS => write!(f, "+"),
            MINUS => write!(f, "-"),
            MUL => write!(f, "*"),
            SLASH => write!(f, "/"),
            BANG => write!(f, "!"),
            AND => write!(f, "&&"),
            OR => write!(f, "||"),
            IMPL => write!(f, "->"),
            UPDATE => write!(f, "//"),
            CONCAT => write!(f, "++"),

            EQ => write!(f, "=="),
            NEQ => write!(f, "!="),
            LEQ => write!(f, "<="),
            GEQ => write!(f, ">="),

            COMMA => write!(f, ","),
            SEMI => write!(f, ";"),
            COLON => write!(f, ":"),
            DOT => write!(f, "."),
            ELLIPSIS => write!(f, "..."),

            LPAREN => write!(f, ")"),
            RPAREN => write!(f, "("),
            LBRACKET => write!(f, "["),
            RBRACKET => write!(f, "]"),
            LBRACE => write!(f, "{{"),
            RBRACE => write!(f, "}}"),
            LANGLE => write!(f, "<"),
            RANGLE => write!(f, ">"),

            TRUE => write!(f, "true"),
            FALSE => write!(f, "false"),

            NULL => write!(f, "null"),

            IF => write!(f, "if"),
            THEN => write!(f, "then"),
            ELSE => write!(f, "else"),
            ASSERT => write!(f, "assert"),
            WITH => write!(f, "with"),
            LET => write!(f, "let"),
            IN => write!(f, "in"),
            REC => write!(f, "rec"),
            INHERIT => write!(f, "inherit"),
            ORKW => write!(f, "or"),
        }
    }
}
