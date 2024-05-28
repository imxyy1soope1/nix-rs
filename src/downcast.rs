pub trait Downcast<T: Sized>
where Self: Sized
{
    fn downcast_ref(&self) -> Option<&T>;
    fn downcast_mut(&mut self) -> Option<&mut T>;
    fn downcast(self) -> Result<T, Self>;
}
