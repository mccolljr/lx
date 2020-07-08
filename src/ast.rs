use super::{
    source::Pos,
    token::TokenType,
};
use std::fmt::{
    Display,
    Formatter,
    Result as FmtResult,
};

pub trait Node {
    fn pos(&self) -> Pos;
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
pub struct FnArg {
    pub pos:  Pos,
    pub name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ObjField {
    pub key:   Box<Expr>,
    pub colon: Pos,
    pub val:   Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Let {
        kwlet:      Pos,
        ident_pos:  Pos,
        ident_name: String,
        assign:     Pos,
        expr:       Box<Expr>,
        semi:       Pos,
    },
    FnDef {
        kwfn:       Pos,
        ident_pos:  Pos,
        ident_name: String,
        oparen:     Pos,
        args:       Vec<FnArg>,
        cparen:     Pos,
        obrace:     Pos,
        body:       Vec<Stmt>,
        cbrace:     Pos,
    },
    Assignment {
        lhs:    Box<Expr>,
        assign: Pos,
        rhs:    Box<Expr>,
        semi:   Pos,
    },
    If {
        head: Vec<IfBlock>,
        tail: Option<ElseBlock>,
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
    Throw {
        kwthrow: Pos,
        error:   Box<Expr>,
        semi:    Pos,
    },
}

impl Display for Stmt {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Stmt::Let {
                ident_name, expr, ..
            } => write!(f, "let {} = {};", ident_name, expr),
            Stmt::Assignment { lhs, rhs, .. } => {
                write!(f, "{} = {};", lhs, rhs)
            }
            Stmt::FnDef {
                ident_name,
                args,
                body,
                ..
            } => {
                write!(f, "fn {} (", ident_name)?;
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
            Stmt::Expr { expr, .. } => write!(f, "{};", expr),
            Stmt::Return { expr, .. } => write!(f, "return {};", expr),
            Stmt::Throw { error, .. } => write!(f, "throw {};", error),
        }
    }
}

impl Node for Stmt {
    fn pos(&self) -> Pos {
        match self {
            Stmt::Let { kwlet, semi, .. } => {
                let start = kwlet.offset;
                let end = semi.offset + semi.length;
                Pos::span(start, end - start)
            }
            Stmt::FnDef { kwfn, cbrace, .. } => {
                let start = kwfn.offset;
                let end = cbrace.offset + cbrace.length;
                Pos::span(start, end - start)
            }
            Stmt::Assignment { lhs, semi, .. } => {
                let start = lhs.pos().offset;
                let end = semi.offset + semi.length;
                Pos::span(start, end - start)
            }
            Stmt::If { head, tail } => {
                if head.len() < 1 {
                    panic!("empty if");
                }
                let start = head[0].kw_pos.offset;
                let endpos = if tail.is_some() {
                    tail.as_ref().unwrap().cbrace
                } else {
                    head[head.len() - 1].cbrace
                };
                let end = endpos.offset + endpos.length;
                Pos::span(start, end - start)
            }
            Stmt::Expr { expr, semi } => {
                let start = expr.pos().offset;
                let end = semi.offset + semi.length;
                Pos::span(start, end - start)
            }
            Stmt::Return { kwreturn, semi, .. } => {
                let start = kwreturn.offset;
                let end = semi.offset + semi.length;
                Pos::span(start, end - start)
            }
            Stmt::Throw { kwthrow, semi, .. } => {
                let start = kwthrow.offset;
                let end = semi.offset + semi.length;
                Pos::span(start, end - start)
            }
        }
    }
}

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
        fields: Vec<ObjField>,
        cbrace: Pos,
    },
    LitFunc {
        kwfn:   Pos,
        oparen: Pos,
        args:   Vec<FnArg>,
        cparen: Pos,
        obrace: Pos,
        body:   Vec<Stmt>,
        cbrace: Pos,
    },
    Ident {
        pos:  Pos,
        name: String,
    },
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
        elt_name: String,
        elt_pos:  Pos,
    },
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Expr::LitNull { .. } => write!(f, "null"),
            Expr::LitInt { val, .. } => write!(f, "{}", val),
            Expr::LitFlt { val, .. } => write!(f, "{}", val),
            Expr::LitBool { val, .. } => write!(f, "{}", val),
            Expr::LitStr { val, .. } => write!(f, "{:?}", val),
            Expr::LitArr { elements, .. } => {
                write!(f, "[")?;
                for (i, v) in elements.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                        write!(f, "{}", v)?;
                    }
                }
                write!(f, "]")
            }
            Expr::LitObj { fields, .. } => {
                write!(f, "{{")?;
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", field.key, field.val)?;
                }
                write!(f, "}}")
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
                write!(f, "\n}}")
            }
            Expr::Call { expr, args, .. } => {
                write!(f, "{}(", expr)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
            Expr::Ident { name, .. } => write!(f, "{}", name),
            Expr::Paren { expr, .. } => write!(f, "({})", expr),
            Expr::Unary { op_typ, expr, .. } => write!(f, "{}{}", op_typ, expr),
            Expr::Binary {
                lhs, op_typ, rhs, ..
            } => write!(f, "({} {} {})", lhs, op_typ, rhs),
            Expr::Ternary {
                cond, pass, fail, ..
            } => write!(f, "({} ? {} : {})", cond, pass, fail),
            Expr::Index { expr, index, .. } => write!(f, "{}[{}]", expr, index),
            Expr::Selector { expr, elt_name, .. } => {
                write!(f, "{}.{}", expr, elt_name)
            }
        }
    }
}

impl Node for Expr {
    fn pos(&self) -> Pos {
        match self {
            Expr::LitNull { pos } => *pos,
            Expr::LitInt { pos, .. } => *pos,
            Expr::LitFlt { pos, .. } => *pos,
            Expr::LitBool { pos, .. } => *pos,
            Expr::LitStr { pos, .. } => *pos,
            Expr::LitArr {
                osquare, csquare, ..
            } => {
                let start = osquare.offset;
                let end = csquare.offset + csquare.length;
                Pos::span(start, end - start)
            }
            Expr::LitObj { obrace, cbrace, .. } => {
                let start = obrace.offset;
                let end = cbrace.offset + cbrace.length;
                Pos::span(start, end - start)
            }
            Expr::LitFunc { kwfn, cbrace, .. } => {
                let start = kwfn.offset;
                let end = cbrace.offset + cbrace.length;
                Pos::span(start, end - start)
            }
            Expr::Ident { pos, .. } => *pos,
            Expr::Call { expr, cparen, .. } => {
                let start = expr.pos().offset;
                let end = cparen.offset + cparen.length;
                Pos::span(start, end - start)
            }
            Expr::Paren { oparen, cparen, .. } => {
                let start = oparen.offset;
                let end = cparen.offset + cparen.length;
                Pos::span(start, end - start)
            }
            Expr::Unary { op_pos, expr, .. } => {
                let start = op_pos.offset;
                let xpos = expr.pos();
                let end = xpos.offset + xpos.length;
                Pos::span(start, end - start)
            }
            Expr::Binary { lhs, rhs, .. } => {
                let start = lhs.pos().offset;
                let rhpos = rhs.pos();
                let end = rhpos.offset + rhpos.length;
                Pos::span(start, end - start)
            }
            Expr::Ternary { cond, fail, .. } => {
                let start = cond.pos().offset;
                let endpos = fail.pos();
                let end = endpos.offset + endpos.length;
                Pos::span(start, end - start)
            }
            Expr::Index { expr, csquare, .. } => {
                let start = expr.pos().offset;
                let end = csquare.offset + csquare.length;
                Pos::span(start, end - start)
            }
            Expr::Selector { expr, elt_pos, .. } => {
                let start = expr.pos().offset;
                let end = elt_pos.offset + elt_pos.length;
                Pos::span(start, end - start)
            }
        }
    }
}

impl Expr {
    pub fn is_assignable(&self) -> bool {
        match self {
            Expr::Ident { .. } | Expr::Index { .. } | Expr::Selector { .. } => {
                true
            }
            _ => false,
        }
    }

    pub fn is_const_lit(&self) -> bool {
        match self {
            Expr::LitNull { .. } => true,
            Expr::LitInt { .. } => true,
            Expr::LitFlt { .. } => true,
            Expr::LitStr { .. } => true,
            Expr::LitBool { .. } => true,
            Expr::LitArr { elements, .. } => {
                return elements.iter().all(|elt| elt.is_const_lit());
            }
            Expr::LitObj { fields, .. } => {
                return fields.iter().all(|field| {
                    return match field.key.as_ref() {
                        Expr::LitStr { .. } => true,
                        Expr::Ident { .. } => true,
                        _ => false,
                    } && field.val.is_const_lit();
                })
            }
            Expr::Paren { expr, .. } => expr.is_const_lit(),
            Expr::Unary { expr, .. } => expr.is_const_lit(),
            Expr::Binary { lhs, rhs, .. } => {
                return lhs.is_const_lit() && rhs.is_const_lit();
            }
            Expr::Ternary {
                cond, pass, fail, ..
            } => {
                return cond.is_const_lit()
                    && pass.is_const_lit()
                    && fail.is_const_lit();
            }
            Expr::LitFunc { .. } => false,
            Expr::Ident { .. } => false,
            Expr::Call { .. } => false,
            Expr::Index { .. } => false,
            Expr::Selector { .. } => false,
        }
    }
}
