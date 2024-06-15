use std::mem::{MaybeUninit, size_of};

use anyhow::{anyhow, Result};

use super::data::StackElem;

pub const STACK_SIZE: usize = 8 * 1024 / size_of::<StackElem>();

pub struct Stack {
    items: Box<[MaybeUninit<StackElem>]>,
    top: usize,
}

impl Stack {
    pub fn new(size: usize) -> Stack {
        Stack {
            items: (0..size).map(|_| MaybeUninit::uninit()).collect(),
            top: 0
        }
    }

    pub fn push(&mut self, item: StackElem) -> Result<()> {
        self.items.get_mut(self.top).map_or(Err(anyhow!("stack overflow")), |ok| Ok(ok))?.write(item);
        Ok(())
    }
}

impl Default for Stack {
    fn default() -> Self {
        Self::new(STACK_SIZE)
    }
}
