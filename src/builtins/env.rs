use super::builtins::builtin_fns;
use crate::ast::Node;
use crate::eval::{Env, Environment};
use crate::object::*;
use std::cell::RefCell;
use std::rc::Rc;

pub fn new_builtins_env() -> Rc<RefCell<Environment>> {
    let env = Rc::new(RefCell::new(Environment::new(None)));
    macro_rules! set {
        ($s:expr, $e:expr) => {
            env.borrow_mut().set($s, $e).unwrap()
        };
    }

    set!(
        String::from("true"),
        Node::Value(Box::new(Object::Bool(true)))
    );
    set!(
        String::from("false"),
        Node::Value(Box::new(Object::Bool(false)))
    );
    set!(String::from("null"), Node::Value(Box::new(Object::Null)));

    let builtinsenv = Rc::new(RefCell::new(Environment::new(Some(env.clone()))));
    macro_rules! bset {
        ($s:expr, $e:expr) => {
            builtinsenv.borrow_mut().set($s, $e).unwrap()
        };
    }

    for b in builtin_fns().into_iter() {
        let v = b.2;
        if b.1 {
            set!(b.0.to_string(), Node::Value(Box::new(v.clone())));
        } else {
            set!("__".to_string() + b.0, Node::Value(Box::new(v.clone())));
        }
        bset!(b.0.to_string(), Node::Value(Box::new(v)));
    }
    set!(
        String::from("builtins"),
        Node::Value(Box::new(Object::Attrs(builtinsenv)))
    );

    env
}
