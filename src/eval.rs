use crate::ast::*;
use crate::object::Object;
use std::collections::HashMap;
use std::error::Error;
use std::fmt::Display;

pub struct Eval {
    root: Box<dyn Expression>,
}

impl Eval {
    pub fn new(root: Box<dyn Expression>) -> Eval {
        Eval { root }
    }
}

pub struct Environment {
    env: HashMap<String, Box<dyn Object>>,
}

#[derive(Debug)]
struct EnvironmentError {
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

impl Error for EnvironmentError {}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            env: HashMap::new(),
        }
    }

    pub fn get(&self, sym: String) -> Result<&Box<dyn Object>, EnvironmentError> {
        let obj = self.env.get(&sym);
        if obj.is_some() {
            Ok(obj.unwrap())
        } else {
            Err(EnvironmentError::new(format!(
                "undefined variable '{}'",
                sym
            )))
        }
    }

    pub fn exsits(&self, sym: String) -> bool {
        self.env.contains_key(&sym)
    }

    pub fn set(&self, sym: String, obj: Box<dyn Object>) -> Result<(), EnvironmentError> {
        if self.exsits(sym) {
            Err(EnvironmentError::new(format!(
                "{sym} exsits in environment!"
            )))
        } else {
            self.env.insert(sym, obj);
            Ok(())
        }
    }

    pub fn over(&self, sym: String, obj: Box<dyn Object>) -> Result<(), EnvironmentError> {
        if self.exsits(sym) {
            self.env.insert(sym, obj);
            Ok(())
        } else {
            Err(EnvironmentError::new(format!(
                "{sym} does not exsit in environment!"
            )))
        }
    }

    pub fn set_force(&self, sym: String, obj: Box<dyn Object>) {
        self.env.insert(sym, obj);
    }
}
