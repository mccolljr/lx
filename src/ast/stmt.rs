use super::expr::Expr;
use super::structs::{
    AssignTarget,
    CatchBlock,
    ElseBlock,
    FinallyBlock,
    FnArg,
    Ident,
    IfBlock,
    LetDeclTarget,
    TryBlock,
    TypeAnnotation,
};
use super::types::Type;

use crate::source::Pos;

use std::fmt::{
    Display,
    Formatter,
    Result as FmtResult,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    LetDecl {
        kwlet:      Pos,
        target:     LetDeclTarget,
        target_pos: Pos,
        annotation: Option<TypeAnnotation>,
        assign:     Pos,
        expr:       Box<Expr>,
        semi:       Pos,
    },
    FnDecl {
        kwfn:       Pos,
        name:       Ident,
        oparen:     Pos,
        args:       Vec<FnArg>,
        cparen:     Pos,
        ret_typ:    Option<TypeAnnotation>,
        obrace:     Pos,
        body:       Vec<Stmt>,
        cbrace:     Pos,
        is_closure: bool,
    },
    TypeDecl {
        kwtype: Pos,
        name:   Ident,
        assign: Pos,
        typ:    Box<Type>,
        semi:   Pos,
    },
    Assignment {
        target_pos: Pos,
        target:     AssignTarget,
        assign:     Pos,
        rhs:        Box<Expr>,
        semi:       Pos,
    },
    If {
        head: Vec<IfBlock>,
        tail: Option<ElseBlock>,
    },
    While {
        kwwhile: Pos,
        cond:    Box<Expr>,
        obrace:  Pos,
        body:    Vec<Stmt>,
        cbrace:  Pos,
    },
    ForIn {
        kwfor:  Pos,
        ident:  Ident,
        kwin:   Pos,
        expr:   Box<Expr>,
        obrace: Pos,
        body:   Vec<Stmt>,
        cbrace: Pos,
    },
    Expr {
        expr: Box<Expr>,
        semi: Pos,
    },
    Return {
        kwreturn: Pos,
        expr:     Box<Expr>,
        semi:     Pos,
    },
    Yield {
        kwyield: Pos,
        expr:    Box<Expr>,
        semi:    Pos,
    },
    Throw {
        kwthrow: Pos,
        error:   Box<Expr>,
        semi:    Pos,
    },
    Break {
        kwbreak: Pos,
        semi:    Pos,
    },
    Try {
        body:    TryBlock,
        catch:   Option<CatchBlock>,
        finally: Option<FinallyBlock>,
    },
}

impl Display for Stmt {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Stmt::LetDecl { target, expr, .. } => {
                write!(f, "let {} = {};", target, expr)
            }
            Stmt::FnDecl {
                name, args, body, ..
            } => {
                write!(f, "fn {} (", name.name)?;
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
                write!(f, "\n}}")
            }
            Stmt::TypeDecl { name, typ, .. } => {
                write!(f, "type {} = {};", name, typ)
            }
            Stmt::Assignment { target, rhs, .. } => {
                write!(f, "{} = {};", target, rhs)
            }
            Stmt::If { head, tail } => {
                for (i, block) in head.iter().enumerate() {
                    write!(
                        f,
                        "{}{} {} {{",
                        if i > 0 { " " } else { "" },
                        block.kw_typ,
                        block.cond
                    )?;
                    for stmt in block.body.iter() {
                        write!(f, "\n\t{}", stmt)?;
                    }
                    write!(f, "\n}}")?;
                }
                if tail.is_some() {
                    let block = tail.as_ref().unwrap();
                    write!(f, " else {{")?;
                    for stmt in block.body.iter() {
                        write!(f, "\n\t{}", stmt)?;
                    }
                    write!(f, "\n}}")?;
                }
                Ok(())
            }
            Stmt::While { cond, body, .. } => {
                write!(f, "while {} {{", cond)?;
                for stmt in body.iter() {
                    write!(f, "\n\t{}", stmt)?;
                }
                write!(f, "\n}}")
            }
            Stmt::ForIn {
                ident, expr, body, ..
            } => {
                write!(f, "for {} in {} {{", ident, expr)?;
                for stmt in body.iter() {
                    write!(f, "\n\t{}", stmt)?;
                }
                write!(f, "\n}}")
            }
            Stmt::Expr { expr, .. } => write!(f, "{};", expr),
            Stmt::Return { expr, .. } => write!(f, "return {};", expr),
            Stmt::Yield { expr, .. } => write!(f, "yield {};", expr),
            Stmt::Throw { error, .. } => write!(f, "throw {};", error),
            Stmt::Break { .. } => write!(f, "break;"),
            Stmt::Try {
                body,
                catch,
                finally,
            } => {
                write!(f, "try {{")?;
                for stmt in body.body.iter() {
                    write!(f, "\n\t{}", stmt)?;
                }
                if catch.is_some() {
                    let c = catch.as_ref().unwrap();
                    write!(f, "}} catch {} {{", c.name)?;
                    for stmt in c.body.iter() {
                        write!(f, "\n\t{}", stmt)?;
                    }
                }
                if finally.is_some() {
                    let fin = finally.as_ref().unwrap();
                    write!(f, "}} finally {{")?;
                    for stmt in fin.body.iter() {
                        write!(f, "\n\t{}", stmt)?;
                    }
                }
                write!(f, "}}")
            }
        }
    }
}
