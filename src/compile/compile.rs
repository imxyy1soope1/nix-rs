use crate::closure::*;

macro_rules! cur_closure {
    ($state:ident) => {
        unsafe { $state.closures.last_mut().unwrap_unchecked() }
    };
}

pub(crate) struct CompileState {
    closures: Vec<Closure>,
}

pub(crate) trait Compile {
    fn compile(self, state: &mut CompileState);
}

