// TODO: Contextful String

use ecow::EcoString;
use rpds::List;

pub struct StringContext {
    context: List<()>
}

impl StringContext {
    pub fn new() -> StringContext {
        StringContext{
            context: List::new()
        }
    }
}

pub struct ContextfulString {
    string: EcoString,
    context: StringContext
}

impl ContextfulString {
    pub fn new(string: EcoString) -> ContextfulString {
        ContextfulString {
            string,
            context: StringContext::new()
        }
    }
}
