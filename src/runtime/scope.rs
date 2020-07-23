use crate::runtime::value::Value;

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{
    Display,
    Formatter,
    Result as FmtResult,
};
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Scope {
    pub parent: Option<Rc<Scope>>,
    entries:    Rc<RefCell<HashMap<String, Value>>>,
}

impl Display for Scope {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f.debug_map()
            .entries(
                self.entries
                    .borrow()
                    .iter()
                    .map(|(k, v)| return (k, v.to_string())),
            )
            .finish()
    }
}

impl Scope {
    pub fn new(parent: Option<Rc<Scope>>) -> Scope {
        Scope {
            parent,
            entries: Rc::new(RefCell::new(HashMap::new())),
        }
    }

    pub fn names(&self) -> Vec<String> {
        self.entries.borrow().keys().map(Clone::clone).collect()
    }

    pub fn declare(&self, key: String, val: Value) {
        self.entries.borrow_mut().insert(key, val);
    }

    pub fn set(&self, key: String, val: Value) {
        if self.has_local(&key) {
            self.entries.borrow_mut().insert(key, val);
            return;
        }
        let mut maybe_parent = self.parent.clone();
        while maybe_parent.is_some() {
            let parent = maybe_parent.unwrap();
            if parent.has_local(&key) {
                parent.entries.borrow_mut().insert(key, val);
                return;
            }
            maybe_parent = parent.parent.clone();
        }
        self.declare(key, val);
    }

    pub fn get(&self, key: &String) -> Value {
        if let Some(result) = self.entries.borrow().get(key) {
            return result.clone();
        }
        if let Some(parent) = self.parent.as_ref() {
            return parent.get(key);
        }
        Value::Null
    }

    pub fn extend(root: Rc<Scope>) -> Scope { Scope::new(Some(root)) }

    fn has_local(&self, key: &String) -> bool {
        self.entries.borrow().contains_key(key)
    }
}
