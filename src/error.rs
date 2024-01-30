use std::error::Error;
use std::fmt::Display;
use std::string::ParseError;

pub trait NixRsError: Error {
    // fn pos(&self) -> Pos;
}

#[derive(Debug)]
pub struct ParserError {
    msg: String,
}

impl ParserError {
    pub fn new(msg: &str) -> Box<dyn NixRsError> {
        Box::new(ParserError {
            msg: msg.to_string(),
        })
    }

    pub fn from_string(msg: String) -> Box<dyn NixRsError> {
        Box::new(ParserError { msg })
    }
}

impl Error for ParserError{}

impl NixRsError for ParserError {
    // fn pos
}

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.msg)
    }
}

#[derive(Debug)]
pub struct EvalError {
    msg: String,
}

impl EvalError {
    pub fn new(msg: &str) -> Box<dyn NixRsError> {
        Box::new(EvalError {
            msg: msg.to_string(),
        })
    }

    pub fn from_string(msg: String) -> Box<dyn NixRsError> {
        Box::new(EvalError { msg })
    }
}

impl Error for EvalError {}

impl NixRsError for EvalError {
    // fn pos
}

impl Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.msg)
    }
}

#[derive(Debug)]
struct Stack<'a, 'b: 'a> {
    this: Option<Box<dyn NixRsError>>,
    prev: Option<&'a Stack<'b, 'b>>,
}

#[derive(Debug)]
pub struct ErrorCtx<'a> {
    stack: Stack<'a, 'a>,
}

impl Default for ErrorCtx<'_> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> ErrorCtx<'a> {
    pub fn new() -> ErrorCtx<'a> {
        ErrorCtx {
            stack: Stack {
                this: None,
                prev: None,
            },
        }
    }

    pub fn with(&self, err: Box<dyn NixRsError>) -> ErrorCtx {
        ErrorCtx {
            stack: Stack {
                this: Some(err),
                prev: Some(&self.stack),
            },
        }
    }

    pub fn unwind(&self, err: Box<dyn NixRsError>) -> Box<dyn NixRsError> {
        let mut e = String::new();
        e.push_str(&err.to_string());
        e.push('\n');
        let mut this = &self.stack;
        while this.this.is_some() {
            e.push_str(&this.this.unwrap().to_string());
            e.push('\n');
            this = this.prev.unwrap();
        }
        EvalError::from_string(e)
    }
}
