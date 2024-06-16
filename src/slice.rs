use std::mem::{transmute, replace};
use std::ops::Deref;

#[derive(Debug)]
struct MaybeUninit<T: Sized + Clone>(std::mem::MaybeUninit<T>);

impl<T: Sized + Clone> Deref for MaybeUninit<T> {
    type Target = std::mem::MaybeUninit<T>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: Sized + Clone> Clone for MaybeUninit<T> {
    fn clone(&self) -> Self {
        todo!()
    }
}

#[derive(Debug, Clone)]
#[repr(transparent)]
pub struct Slice<T: Sized + Clone>(Box<[MaybeUninit<T>]>);

impl<T: Sized + Clone, S: Into<Box<[T]>>> From<S> for Slice<T> {
    fn from(value: S) -> Self {
        unsafe {
            transmute(value.into())
        }
    }
}

impl<T: Sized + Clone> Drop for Slice<T> {
    fn drop(&mut self) {
        for i in 0..self.len() {
            unsafe {
                self.0.get_mut(i).unwrap().assume_init_drop()
            }
        }
    }
}

impl<T: Sized + Clone> Deref for Slice<T> {
    type Target = [T];
    fn deref(&self) -> &Self::Target {
        unsafe {
            transmute(self.0.deref())
        }
    }
}

pub struct IntoIter<T: Sized + Clone>(Slice<T>, usize);

impl<T: Sized + Clone> Iterator for IntoIter<T> {
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        if self.1 == self.0.len() {
            None
        } else {
            let item = unsafe {
                replace(self.0.0.get_mut(self.1).unwrap(), MaybeUninit::uninit()).assume_init()
            };
            self.1 = self.1 + 1;
            Some(item)
        }
    }
}

impl<T: Sized + Clone> IntoIterator for Slice<T> {
    type Item = T;
    type IntoIter = IntoIter<T>;
    fn into_iter(self) -> Self::IntoIter {
        IntoIter(self, 0)
    }
}
