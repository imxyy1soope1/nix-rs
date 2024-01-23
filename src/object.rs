use std::{any::Any, fmt::Display};

pub trait Object: Display {
    fn as_any(&self) -> &dyn Any;
}

pub mod type_ids {
    use super::*;
    use std::any::TypeId;

    lazy_static! {
        pub static ref INT: TypeId = Int::new(0).type_id();
        pub static ref FLOAT: TypeId = Float::new(0f64).type_id();
        pub static ref BOOL: TypeId = TRUE.type_id();
        pub static ref NULL: TypeId = super::NULL.type_id();
        pub static ref STRING: TypeId = Str::new("".to_string()).type_id();
    }

}

pub struct Int {
    pub value: i64,
}

impl Int {
    pub fn new(value: i64) -> Int {
        Int { value }
    }
}

impl Object for Int {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Display for Int {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

pub struct Float {
    pub value: f64,
}

impl Float {
    pub fn new(value: f64) -> Float {
        Float { value }
    }
}

impl Object for Float {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Display for Float {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

lazy_static! {
    pub static ref TRUE: Bool = Bool::new(true);
    pub static ref FALSE: Bool = Bool::new(false);
    pub static ref NULL: Null = Null {};
}

pub struct Bool {
    pub value: bool,
}

impl Bool {
    fn new(value: bool) -> Bool {
        Bool { value }
    }
    pub fn bang(&self) -> Bool {
        if self.value {
            TRUE
        } else {
            FALSE
        }
    }
    pub fn from_bool(value: bool) -> &'static Bool {
        if value {
            &TRUE
        } else {
            &FALSE
        }
    }
}

impl Object for Bool {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Display for Bool {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

pub struct Null {}

impl Object for Null {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Display for Null {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "null")
    }
}

pub struct Str {
    value: String
}

impl Str {
    pub fn new(value: String) -> Str {
        Str { value }
    }
}

impl Object for Str {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Display for Str {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, r#""{}""#, self.value)
    }
}

pub struct List {
    value: Vec<Box<dyn Object>>
}

impl List {
    pub fn new(value: Vec<Box<dyn Object>>) -> List {
        List { value }
    }
}

impl Object for List {
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Display for List {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[ ")?;
        for v in self.value.iter() {
            write!(f, "{v} ")?;
        }
        write!(f, "]")
    }
}
