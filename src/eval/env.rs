use crate::ast::Node;
use crate::error::NixRsError;
use std::cell::RefCell;
use std::collections::hash_map::{HashMap, Iter};
use std::error::Error;
use std::fmt::Display;

#[derive(Debug)]
pub struct Environment<'a, 'b: 'a> {
    pub env: HashMap<String, Node<'a>>,
    pub father: Option<&'a RefCell<Environment<'b, 'b>>>,
}

#[derive(Debug)]
pub struct EnvironmentError {
    text: String,
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

impl<'a, 'b: 'a> Environment<'a, 'b> {
    pub fn new(father: Option<&RefCell<Environment>>) -> Environment<'a, 'b> {
        Environment {
            env: HashMap::new(),
            father,
        }
    }



    pub fn get(&self, sym: &String) -> Result<&Node, EnvironmentError> {
        if let Some(val) = self.env.get(sym) {
            Ok(val)
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
        self.env.contains_key(sym)
    }

    pub fn set(&mut self, sym: String, obj: Node) -> Result<(), EnvironmentError> {
        if self.exsits(&sym) {
            Err(EnvironmentError::new(format!(
                "{sym} exsits in environment!"
            )))
        } else {
            self.env.insert(sym, obj);
            Ok(())
        }
    }

    pub fn over(&mut self, sym: String, obj: Node) {
        self.env.insert(sym, obj);
    }

    pub fn iter(&self) -> Iter<String, Node> {
        self.env.iter()
    }

    pub fn len(&self) -> usize {
        self.env.len()
    }
}
