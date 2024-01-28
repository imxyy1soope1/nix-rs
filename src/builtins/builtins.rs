use crate::convany;
use crate::object::*;
use std::fmt::Display;
use std::rc::Rc;

#[derive(Debug)]
pub struct BuiltinFunction {
    argscount: u8,
    func: fn(Vec<Rc<dyn Object>>) -> Rc<dyn Object>,
}

#[derive(Debug)]
pub struct BuiltinFunctionApp {
    args: Vec<Rc<dyn Object>>,
    argsleft: u8,
    func: fn(Vec<Rc<dyn Object>>) -> Rc<dyn Object>,
}

impl BuiltinFunctionApp {
    pub fn call(&self, arg: Rc<dyn Object>) -> Rc<dyn Object> {
        let mut args = self.args.clone();
        args.push(arg);
        let a = self.argsleft - 1;
        if a == 0 {
            let f = self.func;
            f(args)
        } else {
            Rc::new(BuiltinFunctionApp {
                args,
                argsleft: a,
                func: self.func,
            })
        }
    }
}

impl Object for BuiltinFunctionApp {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

impl Display for BuiltinFunctionApp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "«primop-app»")
    }
}

impl BuiltinFunction {
    pub fn new(argscount: u8, func: fn(Vec<Rc<dyn Object>>) -> Rc<dyn Object>) -> BuiltinFunction {
        BuiltinFunction { argscount, func }
    }

    pub fn call(&self, arg: Rc<dyn Object>) -> Rc<dyn Object> {
        let b = BuiltinFunctionApp {
            args: Vec::new(),
            argsleft: self.argscount,
            func: self.func,
        };
        b.call(arg)
    }
}

impl Object for BuiltinFunction {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}

impl Display for BuiltinFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "«primop»")
    }
}

pub fn builtin_fns() -> Vec<(String, BuiltinFunction)> {
    vec![
        (
            "ceil".to_string(),
            BuiltinFunction::new(1, |a| Rc::new(convany!(a[0].as_any(), Float).ceil())),
        ),
        (
            "floor".to_string(),
            BuiltinFunction::new(1, |a| Rc::new(convany!(a[0].as_any(), Float).floor())),
        ),
    ]
}
