use super::node::Node;
use super::stmt::Stmt;
use super::structs::{
    AssignTarget,
    FnArg,
    Ident,
    ObjLitField,
    TypeAnnotation,
};

use crate::source::Pos;
use crate::token::TokenType;

use std::fmt::{
    Display,
    Formatter,
    Result as FmtResult,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    LitNull {
        pos: Pos,
    },
    LitInt {
        pos: Pos,
        val: i64,
    },
    LitFlt {
        pos: Pos,
        val: f64,
    },
    LitBool {
        pos: Pos,
        val: bool,
    },
    LitStr {
        pos: Pos,
        val: String,
    },
    LitArr {
        osquare:  Pos,
        elements: Vec<Expr>,
        csquare:  Pos,
    },
    LitObj {
        obrace: Pos,
        fields: Vec<ObjLitField>,
        cbrace: Pos,
    },
    LitFunc {
        kwfn:       Pos,
        oparen:     Pos,
        args:       Vec<FnArg>,
        cparen:     Pos,
        ret_typ:    Option<TypeAnnotation>,
        obrace:     Pos,
        body:       Vec<Stmt>,
        cbrace:     Pos,
        is_closure: bool,
    },
    Ident(Ident),
    Paren {
        oparen: Pos,
        expr:   Box<Expr>,
        cparen: Pos,
    },
    Unary {
        op_pos: Pos,
        op_typ: TokenType,
        expr:   Box<Expr>,
    },
    Binary {
        lhs:    Box<Expr>,
        op_pos: Pos,
        op_typ: TokenType,
        rhs:    Box<Expr>,
    },
    Ternary {
        cond:     Box<Expr>,
        question: Pos,
        pass:     Box<Expr>,
        colon:    Pos,
        fail:     Box<Expr>,
    },
    Call {
        expr:   Box<Expr>,
        oparen: Pos,
        args:   Vec<Expr>,
        cparen: Pos,
    },
    Index {
        expr:    Box<Expr>,
        osquare: Pos,
        index:   Box<Expr>,
        csquare: Pos,
    },
    Selector {
        expr:     Box<Expr>,
        dot:      Pos,
        selector: Ident,
    },
    Import {
        kwimport: Pos,
        oparen:   Pos,
        name:     String,
        cparen:   Pos,
    },
    Typeof {
        kwtypeof: Pos,
        expr:     Box<Expr>,
    },
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Expr::LitNull { .. } => {
                write!(f, "null")?;
            }
            Expr::LitInt { val, .. } => {
                write!(f, "{}", val)?;
            }
            Expr::LitFlt { val, .. } => {
                write!(f, "{}", val)?;
            }
            Expr::LitBool { val, .. } => {
                write!(f, "{}", val)?;
            }
            Expr::LitStr { val, .. } => {
                write!(f, "{:?}", val)?;
            }
            Expr::LitArr { elements, .. } => {
                write!(f, "[")?;
                for (i, v) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                        write!(f, "{}", v)?;
                    }
                }
                write!(f, "]")?;
            }
            Expr::LitObj { fields, .. } => {
                write!(f, "{{")?;
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", field)?;
                }
                write!(f, "}}")?;
            }
            Expr::LitFunc { args, body, .. } => {
                write!(f, "fn (")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg.name)?;
                }
                write!(f, ") {{")?;
                for stmt in body.iter() {
                    write!(f, "\n\t{}", stmt)?;
                }
                write!(f, "\n}}")?;
            }
            Expr::Call { expr, args, .. } => {
                write!(f, "({}(", expr)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, "))")?;
            }
            Expr::Ident(ident) => {
                write!(f, "{}", ident.name)?;
            }
            Expr::Paren { expr, .. } => {
                write!(f, "({})", expr)?;
            }
            Expr::Unary { op_typ, expr, .. } => {
                write!(f, "({}{})", op_typ, expr)?;
            }
            Expr::Binary {
                lhs, op_typ, rhs, ..
            } => {
                write!(f, "({} {} {})", lhs, op_typ, rhs)?;
            }
            Expr::Ternary {
                cond, pass, fail, ..
            } => {
                write!(f, "({} ? {} : {})", cond, pass, fail)?;
            }
            Expr::Index { expr, index, .. } => {
                write!(f, "({}[{}])", expr, index)?;
            }
            Expr::Selector { expr, selector, .. } => {
                write!(f, "({}.{})", expr, selector.name)?;
            }
            Expr::Import { name, .. } => {
                write!(f, "import({:?})", name)?;
            }
            Expr::Typeof { expr, .. } => {
                write!(f, "(typeof {})", expr)?;
            }
        }
        Ok(())
    }
}

impl Expr {
    pub fn get_assign_target(&self) -> Option<(AssignTarget, Pos)> {
        match self {
            Expr::Ident(ident) => {
                Some((AssignTarget::Ident(ident.clone()), self.pos()))
            }
            Expr::Index { expr, index, .. } => {
                Some((
                    AssignTarget::Index(expr.clone(), index.clone()),
                    self.pos(),
                ))
            }
            Expr::Selector { expr, selector, .. } => {
                Some((
                    AssignTarget::Select(expr.clone(), selector.clone()),
                    self.pos(),
                ))
            }
            _ => None,
        }
    }
}
