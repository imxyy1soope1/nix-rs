#![allow(dead_code)]

mod bytecode;
mod compile;
mod downcast;
mod vm;
mod slice;

#[cfg(test)]
mod test {
    #[test]
    fn test_slice() {
        use super::slice::Slice;

        let slice: Box<[i32]> = Box::new([1, 2, 3]);
        let slice = Slice::from(slice);

        assert_eq!(slice.into_iter().collect::<Vec<_>>(), vec![1, 2, 3])
    }
}
