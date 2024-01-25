use crate::object::{EvaledOr, Object};
use std::cell::RefCell;
use std::collections::HashMap;
use std::error::Error;
use std::fmt::Display;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Environment {
    pub env: HashMap<String, EvaledOr>,
    pub father: Option<Rc<RefCell<Environment>>>,
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

impl Error for EnvironmentError {}

impl Environment {
    pub fn new(father: Option<Rc<RefCell<Environment>>>) -> Environment {
        Environment {
            env: HashMap::new(),
            father,
        }
    }

    pub fn get(&self, sym: &String) -> Result<Rc<dyn Object>, EnvironmentError> {
        if let Some(val) = self.env.get(sym) {
            Ok(val.eval())
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

    pub fn set(&mut self, sym: String, obj: EvaledOr) -> Result<(), EnvironmentError> {
        if self.exsits(&sym) {
            Err(EnvironmentError::new(format!(
                "{sym} exsits in environment!"
            )))
        } else {
            self.env.insert(sym, obj);
            Ok(())
        }
    }

    pub fn over(&mut self, sym: String, obj: EvaledOr) {
        self.env.insert(sym, obj);
    }
}
