use super::value::Value;
use std::{
    cell::RefCell,
    collections::VecDeque,
    fmt::{
        Debug,
        Formatter,
        Result as FmtResult,
    },
    iter::FromIterator,
    rc::Rc,
};

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

impl Array {
    pub fn new() -> Self { Array(Rc::new(RefCell::new(VecDeque::new()))) }

    pub fn len(&self) -> usize { self.0.borrow().len() }

    pub fn push_back(&self, val: Value) { self.0.borrow_mut().push_back(val) }

    pub fn index_get(&self, index: usize) -> Value {
        self.0
            .borrow()
            .get(index)
            .map_or(Value::Null, |v| v.clone())
    }

    pub fn index_set(&self, index: usize, val: Value) {
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
}

impl IntoIterator for Array {
    type IntoIter = std::collections::vec_deque::IntoIter<Value>;
    type Item = Value;

    fn into_iter(self) -> Self::IntoIter {
        (*self.0).clone().into_inner().into_iter()
    }
}