use super::builtins::builtin_fns;
use crate::eval::Environment;
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
        ObjectOr::obj(Object::Bool(true))
    );
    set!(
        String::from("false"),
        ObjectOr::obj(Object::Bool(false))
    );
    set!(String::from("null"), ObjectOr::obj(Object::Null));

    let builtinsenv = Rc::new(RefCell::new(Environment::new(Some(env.clone()))));
    macro_rules! bset {
        ($s:expr, $e:expr) => {
            builtinsenv.borrow_mut().set($s, $e).unwrap()
        };
    }

    for b in builtin_fns().into_iter() {
        let v = b.2;
        if b.1 {
            set!(b.0.to_string(), ObjectOr::obj(v.clone()));
        } else {
            set!("__".to_string() + b.0, ObjectOr::obj(v.clone()));
        }
        bset!(b.0.to_string(), ObjectOr::obj(v));
    }
    set!(
        String::from("builtins"),
        ObjectOr::obj(Object::Attrs(builtinsenv))
    );

    env
}
