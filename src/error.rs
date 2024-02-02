use std::error::Error;
use std::fmt::Display;
use std::rc::Rc;

pub trait NixRsError: Error {
    // fn pos(&self) -> Pos;
}

#[derive(Debug)]
pub struct EvalError {
    msg: String,
}

impl NixRsError for EvalError {
    // fn pos
}

impl Error for EvalError {}

impl Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.msg)
    }
}

impl From<&str> for EvalError {
    fn from(value: &str) -> Self {
        EvalError {
            msg: value.to_string(),
        }
    }
}

impl From<String> for EvalError {
    fn from(value: String) -> Self {
        EvalError { msg: value }
    }
}

impl From<EvalError> for Rc<dyn NixRsError> {
    fn from(value: EvalError) -> Self {
        Rc::new(value)
    }
}

#[derive(Debug)]
struct Stack {
    this: Option<Rc<dyn NixRsError>>,
    prev: Option<Rc<Stack>>,
}

#[derive(Clone, Debug)]
pub struct ErrorCtx {
    stack: Rc<Stack>,
}

impl Default for ErrorCtx {
    fn default() -> Self {
        Self::new()
    }
}

impl ErrorCtx {
    pub fn new() -> ErrorCtx {
        ErrorCtx {
            stack: Rc::new(Stack {
                this: None,
                prev: None,
            }),
        }
    }

    pub fn with(&self, err: Rc<dyn NixRsError>) -> ErrorCtx {
        ErrorCtx {
            stack: Rc::new(Stack {
                this: Some(err),
                prev: Some(self.stack.clone()),
            }),
        }
    }

    pub fn unwind(&self, err: Rc<dyn NixRsError>) -> Rc<dyn NixRsError> {
        let mut e = String::new();
        e.push_str(&err.to_string());
        e.push('\n');
        let mut this = self.stack.clone();
        while this.this.is_some() {
            e.push_str(&this.this.clone().unwrap().to_string());
            e.push('\n');
            this = this.prev.clone().unwrap();
        }
        EvalError::from(e).into()
    }
}
