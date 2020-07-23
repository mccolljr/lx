use std::cell::{
    Ref,
    RefCell,
    RefMut,
};
use std::fmt::{
    Debug,
    Display,
    Formatter,
    Result as FmtResult,
};
use std::marker::Unsize;
use std::ops::CoerceUnsized;
use std::rc::Rc;
#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub struct RcCell<T: ?Sized>(Rc<RefCell<T>>);

impl<T: ?Sized + Unsize<U>, U: ?Sized> CoerceUnsized<RcCell<U>> for RcCell<T> {}

impl<T> RcCell<T> {
    pub fn new(src: T) -> RcCell<T> { RcCell(Rc::from(RefCell::new(src))) }
}

impl<T: ?Sized> RcCell<T> {
    pub fn borrow(&self) -> Ref<T> { self.0.borrow() }

    pub fn borrow_mut(&self) -> RefMut<T> { self.0.borrow_mut() }

    pub fn ptr_eq(&self, other: &Self) -> bool {
        std::ptr::eq(Rc::as_ptr(&self.0), Rc::as_ptr(&other.0))
    }
}

impl<T: Clone> RcCell<T> {
    #[allow(dead_code)]
    pub fn clone_inner(&self) -> Self { RcCell::new(self.borrow().clone()) }
}

impl<T: ?Sized> Debug for RcCell<T> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "{:?}", Rc::as_ptr(&self.0))
    }
}

impl<T: ?Sized> Clone for RcCell<T> {
    fn clone(&self) -> Self { RcCell(Rc::clone(&self.0)) }
}

impl<T: ?Sized + Display> Display for RcCell<T> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "{}", self.0.borrow())
    }
}
