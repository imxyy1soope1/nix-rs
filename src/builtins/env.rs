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

    set!(String::from("true"), Object::mk_bool(true));
    set!(String::from("false"), Object::mk_bool(false));
    set!(String::from("null"), Object::mk_null());

    let builtinsenv = Rc::new(RefCell::new(Environment::new(Some(env.clone()))));
    macro_rules! bset {
        ($s:expr, $e:expr) => {
            builtinsenv.borrow_mut().set($s, $e).unwrap()
        };
    }
    set!(
        String::from("builtins"),
        Object::mk_attrs(builtinsenv.clone())
    );

    for b in builtin_fns().into_iter() {
        let v = b.2;
        if b.1 {
            set!(b.0.to_string(), v.clone().into());
        } else {
            set!("__".to_string() + b.0, v.clone().into());
        }
        bset!(b.0.to_string(), v.into());
    }

    env
}
