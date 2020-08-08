use crate::source::Pos;

use std::cell::RefCell;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Scope {
    parent:     Option<Rc<Scope>>,
    decls:      RefCell<BTreeMap<String, Pos>>,
    type_decls: RefCell<BTreeMap<String, Pos>>,
    captures:   RefCell<BTreeSet<String>>,
}

impl Scope {
    pub fn global(decls: Vec<String>) -> Rc<Self> {
        let s = Scope::create(None);
        decls.into_iter().for_each(|name| {
            s.declare(name, Pos::mark(0))
                .expect_none("redeclared global");
        });
        s
    }

    pub fn create(parent: Option<Rc<Scope>>) -> Rc<Self> {
        Rc::from(Scope {
            parent,
            decls: RefCell::new(BTreeMap::new()),
            type_decls: RefCell::new(BTreeMap::new()),
            captures: RefCell::new(BTreeSet::new()),
        })
    }

    pub fn parent(&self) -> Option<&Rc<Scope>> { return self.parent.as_ref() }

    pub fn is_closure(&self) -> bool {
        return !self.captures.borrow().is_empty();
    }

    pub fn get_local_decl(&self, name: &String) -> Option<Pos> {
        self.decls.borrow().get(name).map(|pos| *pos)
    }

    pub fn get_local_type_decl(&self, name: &String) -> Option<Pos> {
        self.type_decls.borrow().get(name).map(|pos| *pos)
    }

    pub fn declare(&self, name: String, at: Pos) -> Option<Pos> {
        if let Some(original) = self.get_local_decl(&name) {
            return Some(original);
        }
        self.decls.borrow_mut().insert(name.clone(), at);
        None
    }

    pub fn declare_type(&self, name: String, at: Pos) -> Option<Pos> {
        if let Some(original) = self.get_local_type_decl(&name) {
            return Some(original);
        }
        self.type_decls.borrow_mut().insert(name.clone(), at);
        None
    }

    pub fn utilize(&self, name: &String) -> bool {
        if self.get_local_decl(name).is_some() {
            // found in local scope
            return true;
        }

        if let Some(parent) = &self.parent {
            // if the name is found several scopes up, we need to capture it in
            // all of the intermediate scopes, too.
            if !parent.utilize(name) {
                return false;
            }
            // if we get here, it was found in or above the parent scope,
            // and we need to capture the name
            self.captures.borrow_mut().insert(name.clone());
            return true;
        }

        return false;
    }

    pub fn utilize_type(&self, name: &String) -> bool {
        if self.get_local_type_decl(name).is_some() {
            // found in local scope
            return true;
        }

        if let Some(parent) = &self.parent {
            if parent.utilize_type(name) {
                return true;
            }
        }

        return false;
    }
}
