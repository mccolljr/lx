use super::scope::Scope;
use super::stmt::Stmt;

use crate::source::Code;

use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct File {
    pub path:  String,
    pub src:   Code,
    pub stmts: Vec<Stmt>,
    pub scope: Rc<Scope>,
}
