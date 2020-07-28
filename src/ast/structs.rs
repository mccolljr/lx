use super::expr::Expr;
use super::stmt::Stmt;

use crate::source::Pos;
use crate::token::TokenType;

use std::fmt::{
    Display,
    Formatter,
    Result as FmtResult,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident {
    pub pos:  Pos,
    pub name: String,
}

impl Display for Ident {
    fn fmt(&self, f: &mut Formatter) -> FmtResult { write!(f, "{}", self.name) }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfBlock {
    pub kw_typ: TokenType,
    pub kw_pos: Pos,
    pub cond:   Box<Expr>,
    pub obrace: Pos,
    pub body:   Vec<Stmt>,
    pub cbrace: Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ElseBlock {
    pub kwelse: Pos,
    pub obrace: Pos,
    pub body:   Vec<Stmt>,
    pub cbrace: Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TryBlock {
    pub kwtry:  Pos,
    pub obrace: Pos,
    pub body:   Vec<Stmt>,
    pub cbrace: Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CatchBlock {
    pub kwcatch: Pos,
    pub name:    Ident,
    pub obrace:  Pos,
    pub body:    Vec<Stmt>,
    pub cbrace:  Pos,
}
#[derive(Debug, Clone, PartialEq)]
pub struct FinallyBlock {
    pub kwfinally: Pos,
    pub obrace:    Pos,
    pub body:      Vec<Stmt>,
    pub cbrace:    Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub enum VDeclTarget {
    Ident(Ident),
    ArrDestruct(Vec<String>),
    ObjDestruct(Vec<ObjDestructItem>),
}

impl Display for VDeclTarget {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            VDeclTarget::Ident(ident) => write!(f, "{}", ident),
            VDeclTarget::ArrDestruct(items) => {
                write!(f, "[")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", item)?;
                }
                write!(f, "]")
            }
            VDeclTarget::ObjDestruct(items) => {
                write!(f, "{{")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", item)?;
                }
                write!(f, "}}")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ObjDestructItem {
    Name(Ident),
    NameMap(String, Ident),
}

impl Display for ObjDestructItem {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            ObjDestructItem::Name(ident) => write!(f, "{}", ident.name),
            ObjDestructItem::NameMap(key, ident) => {
                write!(f, "{:?}: {}", key, ident.name)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnArg {
    pub pos:  Pos,
    pub name: String,
}

impl Display for FnArg {
    fn fmt(&self, f: &mut Formatter) -> FmtResult { write!(f, "{}", self.name) }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ObjField {
    pub key:   ObjKey,
    pub colon: Pos,
    pub val:   Box<Expr>,
}

impl Display for ObjField {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "{}: {}", self.key, self.val)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ObjKey {
    Static(String, Pos),
    Dynamic(Box<Expr>),
}

impl Display for ObjKey {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            ObjKey::Static(name, _) => write!(f, "{:?}", name),
            ObjKey::Dynamic(expr) => write!(f, "({})", expr),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignTarget {
    Ident(Ident),
    Index(Box<Expr>, Box<Expr>),
    Select(Box<Expr>, Ident),
}

impl Display for AssignTarget {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            AssignTarget::Ident(ident) => write!(f, "{}", ident),
            AssignTarget::Index(target, elt) => {
                write!(f, "{}[{}]", target, elt)
            }
            AssignTarget::Select(target, elt) => {
                write!(f, "{}.{}", target, elt)
            }
        }
    }
}
