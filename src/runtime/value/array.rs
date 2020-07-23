use super::iter::Iter;
use super::value::Value;

use std::cell::RefCell;
use std::collections::VecDeque;
use std::fmt::{
    Debug,
    Formatter,
    Result as FmtResult,
};
use std::iter::FromIterator;
use std::rc::Rc;

#[derive(Clone, PartialEq)]
pub struct Array(Rc<RefCell<VecDeque<Value>>>);

impl Debug for Array {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f.debug_tuple("Array").field(&self.0.borrow()).finish()
    }
}

impl From<VecDeque<Value>> for Array {
    fn from(src: VecDeque<Value>) -> Self { Self(Rc::new(RefCell::new(src))) }
}

impl From<&[Value]> for Array {
    fn from(src: &[Value]) -> Self {
        Self(Rc::new(RefCell::new(VecDeque::from_iter(
            src.iter().map(|v| v.clone()),
        ))))
    }
}

impl Array {
    pub fn new() -> Self { Array(Rc::new(RefCell::new(VecDeque::new()))) }

    pub fn len(&self) -> usize { self.0.borrow().len() }

    pub fn push_back(&self, val: Value) { self.0.borrow_mut().push_back(val) }

    pub fn push_front(&self, val: Value) { self.0.borrow_mut().push_front(val) }

    pub fn index_get(&self, index: usize) -> Value {
        self.0.borrow().get(index).map_or(Value::Null, Clone::clone)
    }

    pub fn index_set(&self, index: usize, val: Value) {
        let size = self.0.borrow().len();
        if index >= size {
            for _ in 0..=(index - size) {
                self.0.borrow_mut().push_back(Value::Null);
            }
        }
        self.0.borrow_mut()[index] = val;
    }

    pub fn concat(&self, other: &Self) -> Self {
        return Array(Rc::new(RefCell::new(VecDeque::from_iter(
            self.0
                .borrow()
                .iter()
                .chain(other.0.borrow().iter())
                .map(Clone::clone),
        ))));
    }

    pub fn value_iter(&self) -> Iter {
        let size = self.len();
        let src = self.clone();
        let mut i: usize = 0;
        Iter::new(Rc::new(RefCell::new(move || {
            if i >= size {
                return None;
            }
            let next_val = src.index_get(i);
            i += 1;
            Some(next_val)
        })))
    }
}
