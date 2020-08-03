use super::expr::Expr;
use super::stmt::Stmt;
use super::structs::Ident;
use super::types::Type;

use crate::source::Pos;
pub trait Node {
    fn pos(&self) -> Pos;
}

impl Node for Type {
    fn pos(&self) -> Pos {
        match self {
            Type::Any { kwany } => *kwany,
            Type::Int { kwint } => *kwint,
            Type::Float { kwfloat } => *kwfloat,
            Type::Bool { kwbool } => *kwbool,
            Type::Str { kwstr } => *kwstr,
            Type::Null { kwnull } => *kwnull,
            Type::Named { ident } => ident.pos,
            Type::Nullable { question, element } => {
                let start = question.offset;
                let endpos = element.pos();
                let end = endpos.offset + endpos.length;
                Pos::span(start, end - start)
            }
            Type::Object { obrace, cbrace, .. } => {
                let start = obrace.offset;
                let end = cbrace.offset + cbrace.length;
                Pos::span(start, end - start)
            }
            Type::Map { kwmap, csquare, .. } => {
                let start = kwmap.offset;
                let end = csquare.offset + csquare.length;
                Pos::span(start, end - start)
            }
            Type::Tuple {
                osquare, csquare, ..
            } => {
                let start = osquare.offset;
                let end = csquare.offset + csquare.length;
                Pos::span(start, end - start)
            }
            Type::Array {
                kwarray, csquare, ..
            } => {
                let start = kwarray.offset;
                let end = csquare.offset + csquare.length;
                Pos::span(start, end - start)
            }
            Type::Union { alts } => {
                let start =
                    alts.first().expect("empty union type").pos().offset;
                let endpos = alts.last().unwrap().pos();
                let end = endpos.offset + endpos.length;
                Pos::span(start, end - start)
            }
            Type::Func { kwfn, ret, .. } => {
                let start = kwfn.offset;
                let endpos = ret.pos();
                let end = endpos.offset + endpos.length;
                Pos::span(start, end - start)
            }
        }
    }
}

impl Node for Stmt {
    fn pos(&self) -> Pos {
        match self {
            Stmt::LetDecl { kwlet, semi, .. } => {
                let start = kwlet.offset;
                let end = semi.offset + semi.length;
                Pos::span(start, end - start)
            }
            Stmt::FnDecl { kwfn, cbrace, .. } => {
                let start = kwfn.offset;
                let end = cbrace.offset + cbrace.length;
                Pos::span(start, end - start)
            }
            Stmt::TypeDecl { kwtype, semi, .. } => {
                let start = kwtype.offset;
                let end = semi.offset + semi.length;
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
            Stmt::While {
                kwwhile, cbrace, ..
            } => {
                let start = kwwhile.offset;
                let end = cbrace.offset + cbrace.length;
                Pos::span(start, end - start)
            }
            Stmt::ForIn { kwfor, cbrace, .. } => {
                let start = kwfor.offset;
                let end = cbrace.offset + cbrace.length;
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
            Stmt::Yield { kwyield, semi, .. } => {
                let start = kwyield.offset;
                let end = semi.offset + semi.length;
                Pos::span(start, end - start)
            }
            Stmt::Throw { kwthrow, semi, .. } => {
                let start = kwthrow.offset;
                let end = semi.offset + semi.length;
                Pos::span(start, end - start)
            }
            Stmt::Break { kwbreak, semi } => {
                let start = kwbreak.offset;
                let end = semi.offset + semi.length;
                Pos::span(start, end - start)
            }
            Stmt::Try {
                body,
                catch,
                finally,
            } => {
                let start = body.kwtry.offset;
                let end_pos = if finally.is_some() {
                    finally.as_ref().unwrap().cbrace
                } else {
                    catch.as_ref().unwrap().cbrace
                };
                let end = end_pos.offset + end_pos.length;
                Pos::span(start, end - start)
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
            Expr::Ident(Ident { pos, .. }) => *pos,
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
            Expr::Selector { expr, selector, .. } => {
                let start = expr.pos().offset;
                let end = selector.pos.offset + selector.pos.length;
                Pos::span(start, end - start)
            }
            Expr::Import {
                kwimport, cparen, ..
            } => {
                let start = kwimport.offset;
                let end = cparen.offset + cparen.length;
                Pos::span(start, end - start)
            }
            Expr::Typeof { kwtypeof, expr } => {
                let start = kwtypeof.offset;
                let endpos = expr.pos();
                let end = endpos.offset + endpos.length;
                Pos::span(start, end - start)
            }
        }
    }
}
