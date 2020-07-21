use super::value::Value;

use std::cell::RefCell;
use std::fmt::{
    Debug,
    Formatter,
    Result as FmtResult,
};
use std::rc::Rc;

#[derive(Clone)]
pub struct Iter {
    next: Rc<RefCell<dyn FnMut() -> Option<Value>>>,
}

impl Debug for Iter {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f.debug_struct("Iter")
            .field(
                "next",
                &(&*self.next as *const RefCell<dyn FnMut() -> Option<Value>>),
            )
            .finish()
    }
}

impl PartialEq<Iter> for Iter {
    fn eq(&self, other: &Iter) -> bool {
        std::ptr::eq(self.next.as_ref(), other.next.as_ref())
    }
}

impl Eq for Iter {}

impl Iter {
    pub fn new(next: Rc<RefCell<dyn FnMut() -> Option<Value>>>) -> Self {
        Iter { next }
    }
}

impl Iterator for Iter {
    type Item = Value;

    fn next(&mut self) -> Option<Value> { (self.next.borrow_mut())() }
}
