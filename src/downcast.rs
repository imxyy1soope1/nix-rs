/*
use std::any::Any;

trait AsAny<'a> {
    fn as_any(&'a self) -> &'a dyn Any;
    fn as_any_mut(&'a mut self) -> &'a mut dyn Any;
}

trait IntoAny {
    fn into_any(self) -> Box<dyn Any>;
}

pub(crate) trait DowncastRef<'a, T: 'a + ?Sized> {
    fn downcast_ref(&'a self) -> Option<&'a T>;
    fn downcast_mut(&'a mut self) -> Option<&'a mut T>;
}

pub(crate) trait DowncastInto<T: ?Sized> {
    fn downcast(self) -> Option<Box<T>>;
}

impl<'a, T: 'static + 'a> AsAny<'a> for T {
    fn as_any(&'a self) -> &'a dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

impl<T: 'static> IntoAny for Box<T> {
    fn into_any(self) -> Box<dyn Any> {
        self
    }
}

impl<'a, T: 'static + Sized, U: 'static + ?Sized> DowncastRef<'a, T> for U {
    fn downcast_ref(&'a self) -> Option<&'a T> {
        self.as_any().downcast_ref()
    }

    fn downcast_mut(&'a mut self) -> Option<&'a mut T> {
        self.as_any_mut().downcast_mut()
    }
}

impl<T: 'static + Sized, U: 'static> DowncastInto<T> for Box<U> {
    fn downcast(self) -> Option<Box<T>> {
        IntoAny::into_any(self).downcast().ok()
    }
}
*/
