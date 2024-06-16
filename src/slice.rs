use std::hash::{Hash, Hasher};
use std::mem::{transmute, replace, MaybeUninit};
use std::ops::{Deref, DerefMut};

#[derive(Debug)]
#[repr(transparent)]
pub struct Slice<T: Sized>(Box<[T]>);

impl<T: Sized + Clone> Clone for Slice<T> {
    fn clone(&self) -> Self {
        Slice(self.0.clone())
    }
}

impl<T: Sized + Hash> Hash for Slice<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}

impl<T: Sized> FromIterator<T> for Slice<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        Slice(iter.into_iter().collect())
    }
}

impl<T: ?Sized + Clone, S: Into<Box<[T]>>> From<S> for Slice<T> {
    fn from(value: S) -> Self {
        Self(value.into())
    }
}

impl<T: ?Sized + Clone> Deref for Slice<T> {
    type Target = [T];
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: ?Sized + Clone> DerefMut for Slice<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

pub struct IntoIter<T: ?Sized + Clone>(Box<[MaybeUninit<T>]>, usize);

impl<T: ?Sized + Clone> Iterator for IntoIter<T> {
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        if self.1 == self.0.len() {
            None
        } else {
            let item = unsafe {
                replace(self.0.get_mut(self.1).unwrap(), MaybeUninit::uninit()).assume_init()
            };
            self.1 = self.1 + 1;
            Some(item)
        }
    }
}

impl<T: ?Sized + Clone> IntoIterator for Slice<T> {
    type Item = T;
    type IntoIter = IntoIter<T>;
    fn into_iter(self) -> Self::IntoIter {
        IntoIter(unsafe { transmute(self.0) }, 0)
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn test_slice() {
        use super::Slice;

        let slice: Box<[i32]> = Box::new([1, 2, 3]);
        let slice = Slice::from(slice);

        assert_eq!(slice.iter().cloned().collect::<Vec<_>>(), vec![1, 2, 3]);
        assert_eq!(slice.into_iter().collect::<Vec<_>>(), vec![1, 2, 3]);
    }
}
