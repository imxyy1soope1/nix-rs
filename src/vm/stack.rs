use std::mem::{size_of, MaybeUninit};

use anyhow::{anyhow, Result};

use super::data::StackElem;

pub const STACK_SIZE: usize = 8 * 1024 / size_of::<StackElem>();

pub struct Stack<'vm, const CAP: usize> {
    items: Box<[MaybeUninit<StackElem<'vm>>; CAP]>,
    top: usize,
}

impl<'vm, const CAP: usize> Stack<'vm, CAP> {
    pub fn new() -> Self {
        Stack {
            items: (0..CAP)
                .map(|_| MaybeUninit::uninit())
                .collect::<Box<_>>()
                .try_into()
                .unwrap(),
            top: 0,
        }
    }

    pub fn push(&mut self, item: StackElem<'vm>) -> Result<()> {
        self.items
            .get_mut(self.top)
            .map_or(Err(anyhow!("stack overflow")), |ok| Ok(ok))?
            .write(item);
        self.top += 1;
        Ok(())
    }

    pub fn pop(&mut self) -> Result<StackElem<'vm>> {
        self.top -= 1;
        let item = self
            .items
            .get_mut(self.top)
            .map_or(Err(anyhow!("stack empty")), |ok| Ok(ok))?;
        unsafe { Ok(std::mem::replace(item, MaybeUninit::uninit()).assume_init()) }
    }
}

impl<'vm, const CAP: usize> Drop for Stack<'vm, CAP> {
    fn drop(&mut self) {
        self.items.as_mut_slice()[0..self.top]
            .iter_mut()
            .map(|item| unsafe { item.assume_init_drop() })
            .for_each(drop)
    }
}
