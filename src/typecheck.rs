#![allow(dead_code, unused_variables)]

use crate::ast::*;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

struct Null(Box<TypeSpec>);
struct Object(HashMap<String, TypeSpec>);
struct Map(Box<TypeSpec>);
struct Tuple(Vec<TypeSpec>);
struct Array(Box<TypeSpec>);
struct Union(Vec<TypeSpec>);
struct ArgType {
    t:        Box<TypeSpec>,
    variadic: bool,
}
struct Func(Vec<ArgType>, Box<TypeSpec>);

enum TypeSpec {
    Any,
    Object(Object),
    Map(Map),
    Tuple(Tuple),
    Array(Array),
    Union(Union),
    Func(Func),
}

struct Scope {
    parent: Option<Rc<Scope>>,
    decls:  RefCell<HashMap<String, TypeSpec>>,
    types:  RefCell<HashMap<String, TypeSpec>>,
}

impl Scope {
    fn new() -> Rc<Self> {
        Rc::new(Scope {
            parent: None,
            decls:  RefCell::new(HashMap::new()),
            types:  RefCell::new(HashMap::new()),
        })
    }

    fn extend(self: &Rc<Self>) -> Rc<Self> {
        Rc::new(Scope {
            parent: Some(Rc::clone(self)),
            decls:  RefCell::new(HashMap::new()),
            types:  RefCell::new(HashMap::new()),
        })
    }

    fn declare_type(self: &Rc<Self>, name: String, spec: TypeSpec) {
        if self.types.borrow().get(&name).is_some() {
            unreachable!("type checker encountered redeclared type");
        }
        self.types.borrow_mut().insert(name, spec);
    }

    fn declare_var(self: &Rc<Self>, name: String, spec: TypeSpec) {
        if self.decls.borrow().get(&name).is_some() {
            unreachable!("type checker encountered redeclared variable");
        }
        self.decls.borrow_mut().insert(name, spec);
    }
}

struct Checker {
    scope: Rc<Scope>,
}

impl Checker {
    fn new() -> Self {
        Checker {
            scope: Scope::new(),
        }
    }

    fn check_expr(&mut self, expr: &Expr) -> TypeSpec { TypeSpec::Any }

    fn collect_return_types(
        &mut self,
        types: &mut Vec<TypeSpec>,
        stmts: &Vec<Stmt>,
    ) {
        for stmt in stmts.iter() {
            match stmt {
                Stmt::If { head, tail } => {
                    for block in head {
                        self.collect_return_types(types, &block.body);
                    }
                    if tail.is_some() {
                        self.collect_return_types(
                            types,
                            &tail.as_ref().unwrap().body,
                        );
                    }
                }
                Stmt::Try {
                    body,
                    catch,
                    finally,
                } => {
                    self.collect_return_types(types, &body.body);
                    if catch.is_some() {
                        self.collect_return_types(
                            types,
                            &catch.as_ref().unwrap().body,
                        );
                    }
                    if finally.is_some() {
                        self.collect_return_types(
                            types,
                            &finally.as_ref().unwrap().body,
                        );
                    }
                }
                Stmt::While { body, .. } => {
                    self.collect_return_types(types, &body)
                }
                Stmt::ForIn { body, .. } => {
                    self.collect_return_types(types, &body)
                }
                Stmt::Return { expr, .. } => todo!(),
                _ => {
                    // other statement types can't contain relevant returns
                }
            }
        }
    }
}
