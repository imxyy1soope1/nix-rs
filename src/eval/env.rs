use crate::error::NixRsError;
use crate::object::Object;

use std::cell::RefCell;
use std::collections::hash_map::{HashMap, Iter};
use std::error::Error;
use std::fmt::Display;
use std::rc::Rc;

pub type Env = Rc<RefCell<Environment>>;

#[derive(Debug, Clone)]
pub struct Environment {
    pub env: HashMap<String, Object>,
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

impl NixRsError for EnvironmentError {}

impl Error for EnvironmentError {}

impl Environment {
    pub fn new(father: Option<Rc<RefCell<Environment>>>) -> Environment {
        Environment {
            env: HashMap::new(),
            father,
        }
    }

    pub fn get(&self, sym: &String) -> Result<Object, EnvironmentError> {
        if let Some(val) = self.env.get(sym) {
            Ok(val.clone())
        } else if let Some(father) = &self.father {
            father.borrow().get(sym)
        } else {
            Err(EnvironmentError::new(format!("undefined variable '{sym}'")))
        }
    }

    pub fn get_local(&self, sym: &String) -> Result<Object, EnvironmentError> {
        if let Some(val) = self.env.get(sym) {
            Ok(val.clone())
        } else {
            Err(EnvironmentError::new(format!("undefined variable '{sym}'")))
        }
    }

    pub fn exsits(&self, sym: &String) -> bool {
        self.env.contains_key(sym)
    }

    pub fn set(&mut self, sym: String, obj: Object) -> Result<(), EnvironmentError> {
        if self.exsits(&sym) {
            Err(EnvironmentError::new(format!(
                "{sym} exsits in environment!"
            )))
        } else {
            self.env.insert(sym, obj);
            Ok(())
        }
    }

    pub fn over(&mut self, sym: String, obj: Object) {
        self.env.insert(sym, obj);
    }

    pub fn iter(&self) -> Iter<String, Object> {
        self.env.iter()
    }

    pub fn len(&self) -> usize {
        self.env.len()
    }
}
