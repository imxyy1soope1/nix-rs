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

    set!(String::from("true"), EvaledOr::evaled(Rc::new(true)));
    set!(String::from("false"), EvaledOr::evaled(Rc::new(false)));
    set!(String::from("null"), EvaledOr::evaled(Rc::new(Null {})));

    let builtinsenv = Rc::new(RefCell::new(Environment::new(Some(env.clone()))));
    macro_rules! bset {
        ($s:expr, $e:expr) => {
            builtinsenv.borrow_mut().set($s, $e).unwrap()
        };
    }
    set!(
        String::from("builtins"),
        EvaledOr::evaled(Rc::new(Attrs::new(builtinsenv.clone())))
    );

    for b in builtin_fns().into_iter() {
        bset!(b.0, EvaledOr::evaled(Rc::new(b.1)));
    }

    env
}
