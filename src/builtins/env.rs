use super::builtins::builtin_fns;
use crate::eval::{Env, Environment};
use crate::object::*;
use std::cell::RefCell;
use std::rc::Rc;

pub fn new_builtins_env() -> Env {
    let env = Rc::new(RefCell::new(Environment::new(None)));
    macro_rules! set {
        ($s:expr, $e:expr) => {
            env.borrow_mut().set($s, $e).unwrap()
        };
    }

    set!(String::from("true"), EvaledOr::evaled(Box::new(true)));
    set!(String::from("false"), EvaledOr::evaled(Box::new(false)));
    set!(String::from("null"), EvaledOr::evaled(Box::new(Null {})));

    let builtinsenv = Rc::new(RefCell::new(Environment::new(Some(Rc::downgrade(&env)))));
    macro_rules! bset {
        ($s:expr, $e:expr) => {
            builtinsenv.borrow_mut().set($s, $e).unwrap()
        };
    }
    set!(
        String::from("builtins"),
        EvaledOr::evaled(Box::new(Attrs::new(builtinsenv.clone())))
    );

    for b in builtin_fns().into_iter() {
        let v = Box::new(b.2);
        if b.1 {
            set!(b.0.to_string(), EvaledOr::evaled(v.clone()));
        } else {
            set!("__".to_string() + b.0, EvaledOr::evaled(v.clone()));
        }
        bset!(b.0.to_string(), EvaledOr::evaled(v));
    }

    env
}
