use super::env::Environment;
use crate::{ast::*, convany, object::*};
use std::cell::RefCell;
use std::rc::Rc;

pub struct Eval {
    root: EvaledOr,
}

impl Eval {
    pub fn new(expr: Box<dyn Expression>) -> Eval {
        let e = Eval {
            root: EvaledOr::expr(
                Rc::new(RefCell::new(Environment::new(None))),
                Rc::from(expr),
            ),
        };

        e
    }

    pub fn eval(&mut self) -> Rc<dyn Object> {
        self.root.eval()
    }
}
