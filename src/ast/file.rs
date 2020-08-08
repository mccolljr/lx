use super::scope::Scope;
use super::stmt::Stmt;

use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct File {
    pub path:  String,
    pub stmts: Vec<Stmt>,
    pub scope: Rc<Scope>,
}
