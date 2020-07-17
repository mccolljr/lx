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
        self.0
            .borrow()
            .get(index)
            .map_or(Value::Null, |v| v.clone())
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
                .map(|v| v.clone()),
        ))));
    }

    pub fn value_iter(&self) -> impl Iterator<Item = Value> {
        ArrayIter {
            src:  self.clone(),
            size: self.len(),
            i:    0,
        }
    }
}

struct ArrayIter {
    src:  Array,
    size: usize,
    i:    usize,
}

impl Iterator for ArrayIter {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        if self.i >= self.size {
            return None;
        }
        let next_val = self.src.index_get(self.i);
        self.i += 1;
        Some(next_val)
    }
}
