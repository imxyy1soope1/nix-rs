use std::mem::{transmute, size_of, MaybeUninit};
use std::ops::Deref;

use anyhow::{anyhow, Result};

use super::value::VmValue;

pub const STACK_SIZE: usize = 8 * 1024 / size_of::<VmValue>();

pub struct Stack<const CAP: usize> {
    items: Box<[MaybeUninit<VmValue>; CAP]>,
    top: usize,
}

impl<const CAP: usize> Stack<CAP> {
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

    pub fn push(&mut self, item: VmValue) -> Result<()> {
        self.items
            .get_mut(self.top)
            .map_or(Err(anyhow!("stack overflow")), |ok| Ok(ok))?
            .write(item);
        self.top += 1;
        Ok(())
    }

    pub fn pop(&mut self) -> Result<VmValue> {
        self.top -= 1;
        let item = self
            .items
            .get_mut(self.top)
            .map_or(Err(anyhow!("stack empty")), |ok| Ok(ok))?;
        unsafe { Ok(std::mem::replace(item, MaybeUninit::uninit()).assume_init()) }
    }
}

impl<const CAP: usize> Deref for Stack<CAP> {
    type Target = [VmValue];
    fn deref(&self) -> &Self::Target {
        unsafe {
            transmute(&self.items[0..self.top])
        }
    }
}

impl<const CAP: usize> Drop for Stack<CAP> {
    fn drop(&mut self) {
        self.items.as_mut_slice()[0..self.top]
            .iter_mut()
            .map(|item| unsafe { item.assume_init_drop() })
            .for_each(drop)
    }
}
