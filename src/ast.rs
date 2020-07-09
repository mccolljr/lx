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
    pub key:   ObjKey,
    pub colon: Pos,
    pub val:   Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ObjKey {
    Static(String, Pos),
    Dynamic(Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignTarget {
    Ident(String),
    Index(Box<Expr>, Box<Expr>),
    Select(Box<Expr>, String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum LetTarget {
    Ident(String),
    ArrDestruct(Vec<String>),
    ObjDestruct(Vec<ObjDestructItem>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ObjDestructItem {
    Name(String),
    NameMap(String, String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Let {
        kwlet:      Pos,
        target:     LetTarget,
        target_pos: Pos,
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
        is_closure: bool,
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
            Stmt::Let { target, expr, .. } => {
                match target {
                    LetTarget::Ident(name) => {
                        write!(f, "let {} = {};", name, expr)
                    }
                    LetTarget::ArrDestruct(names) => {
                        write!(f, "let [")?;
                        for (i, name) in names.iter().enumerate() {
                            if i > 0 {
                                write!(f, ", ")?;
                            }
                            write!(f, "{}", name)?;
                        }
                        write!(f, "] = {};", expr)
                    }
                    LetTarget::ObjDestruct(items) => {
                        write!(f, "let {{")?;
                        for (i, item) in items.iter().enumerate() {
                            if i > 0 {
                                write!(f, ", ")?;
                            }
                            match item {
                                ObjDestructItem::Name(name) => {
                                    write!(f, "{}", name)?
                                }
                                ObjDestructItem::NameMap(key, name) => {
                                    write!(f, "{:?}: {}", key, name)?
                                }
                            }
                        }
                        write!(f, "}} = {};", expr)
                    }
                }
            }
            Stmt::Assignment { target, rhs, .. } => {
                match target {
                    AssignTarget::Ident(name) => {
                        write!(f, "{} = {};", name, rhs)
                    }
                    AssignTarget::Index(item, index) => {
                        write!(f, "{}[{}] = {};", item, index, rhs)
                    }
                    AssignTarget::Select(item, name) => {
                        write!(f, "{}.{} = {};", item, name, rhs)
                    }
                }
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
            Stmt::Assignment {
                target_pos, semi, ..
            } => {
                let start = target_pos.offset;
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
        kwfn:       Pos,
        oparen:     Pos,
        args:       Vec<FnArg>,
        cparen:     Pos,
        obrace:     Pos,
        body:       Vec<Stmt>,
        cbrace:     Pos,
        is_closure: bool,
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
                    match &field.key {
                        ObjKey::Static(name, ..) => {
                            write!(f, "{:?}: {}", name, field.val)?;
                        }
                        ObjKey::Dynamic(expr) => {
                            match expr.as_ref() {
                                Expr::Paren { .. } => {
                                    write!(f, "{}: {}", expr, field.val)?
                                }
                                _ => write!(f, "({}): {}", expr, field.val)?,
                            }
                        }
                    }
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
    pub fn get_assign_target(&self) -> Option<(AssignTarget, Pos)> {
        match self {
            Expr::Ident { name, .. } => {
                Some((AssignTarget::Ident(name.clone()), self.pos()))
            }
            Expr::Index { expr, index, .. } => {
                Some((
                    AssignTarget::Index(expr.clone(), index.clone()),
                    self.pos(),
                ))
            }
            Expr::Selector { expr, elt_name, .. } => {
                Some((
                    AssignTarget::Select(expr.clone(), elt_name.clone()),
                    self.pos(),
                ))
            }
            _ => None,
        }
    }
}
