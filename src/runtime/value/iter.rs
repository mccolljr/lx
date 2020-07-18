use super::value::Value;

use crate::error::{
    Error,
    RuntimeError,
};

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

impl Iter {
    pub fn new(next: Rc<RefCell<dyn FnMut() -> Option<Value>>>) -> Self {
        Iter { next }
    }

    pub fn next(&self) -> Option<Value> { (self.next.borrow_mut())() }
}
