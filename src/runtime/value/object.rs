use super::iter::Iter;
use super::value::Value;

use crate::mem::rccell::RcCell;

use std::collections::HashMap;
use std::fmt::{
    Debug,
    Formatter,
    Result as FmtResult,
};

#[derive(Clone, PartialEq)]
pub struct Object(RcCell<HashMap<String, Value>>);

impl Debug for Object {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f.debug_tuple("Object").field(&self.0.borrow()).finish()
    }
}

impl From<HashMap<String, Value>> for Object {
    fn from(src: HashMap<String, Value>) -> Self { Self(RcCell::new(src)) }
}

impl Object {
    pub fn new() -> Self { Self::from(HashMap::new()) }

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

    pub fn has_method(&self, name: &String) -> bool {
        if let Some(v) = self.0.borrow().get(name) {
            return match v {
                Value::Func { .. } => true,
                Value::NativeFunc { .. } => true,
                _ => false,
            };
        }
        false
    }

    pub fn value_iter(&self) -> Iter {
        let mut items = self
            .rust_iter()
            .map(|(k, v)| Value::from(&[Value::Str(k), v][..]));
        Iter::new(move || Ok(items.next()))
    }

    pub fn rust_iter(&self) -> impl Iterator<Item = (String, Value)> {
        let mut keys: Vec<String> =
            self.0.borrow().keys().map(|k| k.clone()).collect();
        keys.sort();
        ObjectIter {
            src: self.clone(),
            keys,
            i: 0,
        }
    }
}

struct ObjectIter {
    src:  Object,
    keys: Vec<String>,
    i:    usize,
}

impl Iterator for ObjectIter {
    type Item = (String, Value);

    fn next(&mut self) -> Option<Self::Item> {
        if self.i >= self.keys.len() {
            return None;
        }
        let key = &self.keys[self.i];
        let next_val = self.src.index_get(key);
        self.i += 1;
        Some((key.clone(), next_val))
    }
}
