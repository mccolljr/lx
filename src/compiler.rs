use super::ast::{Expr, Stmt};
use super::errors::Error;
use super::parser::Parser;
use super::runtime::inst::Inst;
use super::runtime::value::Value;
use super::source::Code;
use super::token::TokenType;
use std::collections::VecDeque;
use std::rc::Rc;

fn compile_stmt(insts: &mut Vec<Inst>, stmt: &Stmt) {
    match stmt {
        Stmt::Expr { expr, .. } => {
            compile_expr(insts, expr);
            insts.push(Inst::PopStack());
        }
        Stmt::Let {
            ident_name, expr, ..
        } => {
            compile_expr(insts, expr);
            insts.push(Inst::DeclareNamed(ident_name.clone()));
        }
        Stmt::Assignment { lhs, rhs, .. } => {
            match lhs.as_ref() {
                Expr::Ident { name, .. } => {
                    compile_expr(insts, rhs);
                    insts.push(Inst::StoreNamed(name.clone()));
                }
                Expr::Index { expr, index, .. } => {
                    compile_expr(insts, expr);
                    compile_expr(insts, index);
                    compile_expr(insts, rhs);
                    insts.push(Inst::IndexSet);
                }
                _ => unreachable!(),
            };
        }
        Stmt::If { head, tail } => {
            let mut escape_indices: Vec<usize> = Vec::with_capacity(head.len() + 1);
            for block in head {
                compile_expr(insts, block.cond.as_ref());
                let branch_idx = insts.len();
                insts.push(Inst::Noop);
                let start_idx = insts.len();
                for stmt in block.body.iter() {
                    compile_stmt(insts, stmt);
                }
                escape_indices.push(insts.len());
                insts.push(Inst::Noop);
                let after_idx = insts.len();
                insts[branch_idx] = Inst::Branch(start_idx, after_idx);
            }
            if let Some(else_block) = tail {
                for stmt in else_block.body.iter() {
                    compile_stmt(insts, stmt);
                }
            }
            let end_idx = insts.len();
            for idx in escape_indices {
                insts[idx] = Inst::Goto(end_idx);
            }
        }
        Stmt::Return { expr, .. } => {
            compile_expr(insts, expr);
            insts.push(Inst::Return);
        }
        Stmt::Throw { error, .. } => {
            compile_expr(insts, error);
            insts.push(Inst::Throw);
        }
    }
}

fn compile_expr(insts: &mut Vec<Inst>, expr: &Expr) {
    match expr {
        Expr::LitNull { .. } => insts.push(Inst::PushStack(Value::Null)),
        Expr::LitInt { val, .. } => insts.push(Inst::PushStack(Value::Int(*val))),
        Expr::LitFlt { val, .. } => insts.push(Inst::PushStack(Value::Flt(*val))),
        Expr::LitBool { val, .. } => insts.push(Inst::PushStack(Value::Bool(*val))),
        Expr::LitStr { val, .. } => insts.push(Inst::PushStack(Value::Str(val.clone()))),
        Expr::LitFunc { args, body, .. } => {
            let mut fn_insts = Vec::<Inst>::new();
            // TODO handle excess args, etc
            fn_insts.push(Inst::PopStack());
            let argct = args.len();
            for i in 1..=argct {
                fn_insts.push(Inst::DeclareNamed(args[argct - i].name.clone()));
            }
            for stmt in body {
                compile_stmt(&mut fn_insts, stmt);
            }
            insts.push(Inst::MakeClosure(argct, Rc::from(fn_insts)));
        }
        Expr::LitArr { elements, .. } => {
            insts.push(Inst::PushStack(Value::from(VecDeque::new())));
            for e in elements {
                compile_expr(insts, e);
                insts.push(Inst::BinaryOp(TokenType::OpFeed))
            }
        }
        Expr::LitObj { fields, .. } => {
            for field in fields.iter() {
                match field.key.as_ref() {
                    Expr::Ident { name, .. } => {
                        insts.push(Inst::PushStack(Value::from(name.clone())))
                    }
                    Expr::LitStr { val, .. } => {
                        insts.push(Inst::PushStack(Value::from(val.clone())))
                    }
                    Expr::Paren { expr, .. } => {
                        compile_expr(insts, expr);
                    }
                    _ => unreachable!(),
                }
                compile_expr(insts, &field.val);
            }
            insts.push(Inst::PushStack(Value::from(fields.len() as i64)));
            insts.push(Inst::BuildObject);
        }
        Expr::Call { expr, args, .. } => {
            insts.push(Inst::InitCall);
            for arg in args.iter() {
                compile_expr(insts, arg);
                insts.push(Inst::AddCallArg);
            }
            compile_expr(insts, expr);
            insts.push(Inst::FinishCall);
        }
        Expr::Ident { name, .. } => insts.push(Inst::LoadNamed(name.clone())),
        Expr::Paren { expr, .. } => compile_expr(insts, expr),
        Expr::Binary {
            lhs, rhs, op_typ, ..
        } => {
            compile_expr(insts, lhs);
            compile_expr(insts, rhs);
            insts.push(Inst::BinaryOp(*op_typ));
        }
        Expr::Unary { expr, op_typ, .. } => {
            compile_expr(insts, expr);
            insts.push(Inst::UnaryOp(*op_typ));
        }
        Expr::Ternary {
            cond, pass, fail, ..
        } => {
            compile_expr(insts, cond);
            let branch_idx = insts.len();
            insts.push(Inst::Noop);
            let start_pass_idx = insts.len();
            compile_expr(insts, pass);
            let end_pass_idx = insts.len();
            insts.push(Inst::Noop);
            let start_fail_idx = end_pass_idx + 1;
            compile_expr(insts, fail);
            let after_idx = insts.len();
            insts[branch_idx] = Inst::Branch(start_pass_idx, start_fail_idx);
            insts[end_pass_idx] = Inst::Goto(after_idx);
        }
        Expr::Index { expr, index, .. } => {
            compile_expr(insts, expr);
            compile_expr(insts, index);
            insts.push(Inst::Index);
        }
        Expr::Selector { expr, element, .. } => match element.as_ref() {
            Expr::Ident { name, .. } => {
                compile_expr(insts, expr);
                insts.push(Inst::PushStack(Value::from(name.clone())));
                insts.push(Inst::Index);
            }
            _ => unreachable!(),
        },
    }
}

pub fn compile(src: &str) -> Result<Vec<Inst>, Error> {
    let mut parser = Parser::new(&Code::from(src));
    let stmts = parser.parse_stmt_list(vec![TokenType::EOF])?;
    let mut insts = Vec::<Inst>::new();
    for stmt in stmts {
        compile_stmt(&mut insts, &stmt);
    }
    Ok(insts)
}
