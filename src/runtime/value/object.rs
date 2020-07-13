use super::value::Value;
use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::{
        Debug,
        Formatter,
        Result as FmtResult,
    },
    rc::Rc,
};

#[derive(Clone, PartialEq)]
pub struct Object(Rc<RefCell<HashMap<String, Value>>>);

impl Debug for Object {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f.debug_tuple("Object").field(&self.0.borrow()).finish()
    }
}

impl From<HashMap<String, Value>> for Object {
    fn from(src: HashMap<String, Value>) -> Self {
        Self(Rc::new(RefCell::new(src)))
    }
}

impl Object {
    pub fn new() -> Self { Object(Rc::new(RefCell::new(HashMap::new()))) }

    pub fn len(&self) -> usize { self.0.borrow().len() }

    pub fn index_get(&self, index: &String) -> Value {
        self.0
            .borrow()
            .get(index)
            .map_or(Value::Null, |v| v.clone())
    }

    pub fn index_set(&self, index: String, val: Value) {
        self.0.borrow_mut().insert(index, val);
    }
}

impl IntoIterator for Object {
    type IntoIter = std::collections::hash_map::IntoIter<String, Value>;
    type Item = (String, Value);

    fn into_iter(self) -> Self::IntoIter {
        ((*self.0).clone()).into_inner().into_iter()
    }
}
