use crate::object::Object;
use std::collections::HashMap;
use std::error::Error;
use std::fmt::Display;

pub struct Environment<'a> {
    env: HashMap<String, Box<dyn Object>>,
    father: Option<&'a Environment<'a>>,
    objects: Vec<Box<dyn Object>>,
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

impl<'a> Environment<'a> {
    pub fn new(father: Option<&'a Environment<'a>>) -> Environment<'a> {
        Environment {
            env: HashMap::new(),
            father,
            objects: Vec::new(),
        }
    }

    pub fn new_obj(&'a mut self, obj: Box<dyn Object>) -> &'a dyn Object {
        self.objects.push(obj);
        self.objects[self.objects.len() - 1].as_ref()
    }

    pub fn get(&self, sym: &String) -> Result<&Box<dyn Object>, EnvironmentError> {
        let obj = self.env.get(sym);
        if obj.is_some() {
            Ok(obj.unwrap())
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

    pub fn set(&mut self, sym: String, obj: Box<dyn Object>) -> Result<(), EnvironmentError> {
        if self.exsits(&sym) {
            Err(EnvironmentError::new(format!(
                "{sym} exsits in environment!"
            )))
        } else {
            self.env.insert(sym, obj);
            Ok(())
        }
    }

    pub fn over(&mut self, sym: String, obj: Box<dyn Object>) -> Result<(), EnvironmentError> {
        if self.exsits(&sym) {
            self.env.insert(sym, obj);
            Ok(())
        } else {
            Err(EnvironmentError::new(format!(
                "{sym} does not exsit in environment!"
            )))
        }
    }

    pub fn set_force(&mut self, sym: String, obj: Box<dyn Object>) {
        self.env.insert(sym, obj);
    }
}
