use super::value::Value;

use crate::error::Error;
use crate::mem::rccell::RcCell;

use std::fmt::{
    Debug,
    Formatter,
    Result as FmtResult,
};

#[derive(Clone)]
pub struct Iter {
    next: RcCell<dyn FnMut() -> Result<Option<Value>, Error>>,
}

impl Debug for Iter {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f.debug_struct("Iter").field("next", &self.next).finish()
    }
}

impl PartialEq<Iter> for Iter {
    fn eq(&self, other: &Iter) -> bool { self.next.ptr_eq(&other.next) }
}

impl Eq for Iter {}

impl Iter {
    pub fn new<F: FnMut() -> Result<Option<Value>, Error> + 'static>(
        next: F,
    ) -> Self {
        Iter {
            next: RcCell::new(next),
        }
    }
}

impl Iterator for Iter {
    type Item = Value;

    fn next(&mut self) -> Option<Value> {
        (self.next.borrow_mut())().map_or(None, |opt| opt)
    }
}
