use crate::ast::Node;
use crate::builtins::new_builtins_env;
use crate::error::NixRsError;
use std::cell::RefCell;
use std::collections::hash_map::{HashMap, Iter};
use std::error::Error;
use std::fmt::Display;
use std::rc::Rc;

pub type Env = Rc<RefCell<Environment>>;

#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    pub env: HashMap<String, Node>,
    pub father: Option<Env>,
}

#[derive(Debug)]
pub struct EnvironmentError {
    text: String,
}

impl From<String> for EnvironmentError {
    fn from(value: String) -> Self {
        EnvironmentError { text: value }
    }
}

impl From<&str> for EnvironmentError {
    fn from(value: &str) -> Self {
        EnvironmentError {
            text: value.to_owned(),
        }
    }
}

impl Into<Box<dyn NixRsError>> for EnvironmentError {
    fn into(self) -> Box<dyn NixRsError> {
        Box::new(self)
    }
}

impl EnvironmentError {
    pub fn new(text: String) -> EnvironmentError {
        EnvironmentError { text }
    }
}

impl Display for EnvironmentError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.text)
    }
}

impl NixRsError for EnvironmentError {}

impl Error for EnvironmentError {}

impl Environment {
    pub fn new(father: Option<Env>) -> Environment {
        Environment {
            env: HashMap::new(),
            father,
        }
    }

    pub fn with_builtins() -> Environment {
        Environment {
            env: HashMap::new(),
            father: Some(new_builtins_env()),
        }
    }

    pub fn get(&self, sym: &String) -> Result<Node, EnvironmentError> {
        if let Some(val) = self.env.get(sym) {
            Ok(val.clone())
        } else if let Some(father) = &self.father {
            father.borrow().get(sym)
        } else {
            Err(EnvironmentError::new(format!(
                "undefined variable '{}'",
                sym
            )))
        }
    }

    pub fn exsits(&self, sym: &String) -> bool {
        if self.env.contains_key(sym) {
            true
        } else if let Some(father) = &self.father {
            father.borrow().exsits(sym)
        } else {
            false
        }
    }

    pub fn exsits_local(&self, sym: &String) -> bool {
        self.env.contains_key(sym)
    }

    pub fn set(&mut self, sym: String, obj: Node) -> Result<(), EnvironmentError> {
        if self.exsits_local(&sym) {
            Err(EnvironmentError::new(format!(
                "{sym} exsits in environment!"
            )))
        } else {
            self.env.insert(sym, obj);
            Ok(())
        }
    }

    pub fn set_force(&mut self, sym: String, obj: Node) {
        self.env.insert(sym, obj);
    }

    pub fn iter(&self) -> Iter<String, Node> {
        self.env.iter()
    }

    pub fn len(&self) -> usize {
        self.env.len()
    }
}
