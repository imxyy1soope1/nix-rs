use super::env::Environment;
use crate::{ast::*, object::*};
use std::cell::RefCell;
use std::rc::Rc;

pub struct Eval<'a> {
    root: Node<'a>,
}

struct Node<'a> {
    expr: Box<dyn Expression>,
    evaled: Option<&'a dyn Object>,
    env: Rc<RefCell<Environment<'a>>>,
}

impl<'a> Eval<'a> {
    pub fn new(expr: Box<dyn Expression>) -> Eval<'a> {
        let mut e = Eval {
            root: Node {
                expr,
                evaled: None,
                env: Rc::new(RefCell::new(Environment::new(None))),
            },
        };

        e
    }

    pub fn eval(&mut self) -> &dyn Object {
        self.root.eval()
    }
}

impl Node<'_> {
    fn eval(&mut self) -> &dyn Object {
        *self.evaled.get_or_insert({
            let a = self.expr.as_any();
            if a.is::<IdentifierExpr>() {
                let t = &*self.env.clone();
                let t = &*(*t).borrow() as *const Environment;
                unsafe { &(*t) }
                    .get(&a.downcast_ref::<IdentifierExpr>().unwrap().ident)
                    .unwrap()
                    .as_ref()
            } else {
                let t = &*self.env.clone();
                let t: &mut Environment<'_> = unsafe { &mut *t.as_ptr() };
                t.new_obj(self.expr.eval(self.env.clone()))
            }
        })
    }
}
