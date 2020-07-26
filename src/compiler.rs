use crate::ast::{
    AssignTarget,
    Expr,
    FnArg,
    LetTarget,
    ObjKey,
    Stmt,
};
use crate::ast_rewrite::simplify_expr;
use crate::error::Error;
use crate::parser::Parser;
use crate::runtime::frame::{
    CatchContext,
    FinallyContext,
};
use crate::runtime::inst::Inst;
use crate::runtime::value::Value;
use crate::source::Code;

use std::rc::Rc;

fn compile_stmt(insts: &mut Vec<Inst>, stmt: Stmt) {
    match stmt {
        Stmt::Expr { expr, .. } => {
            compile_expr(insts, simplify_expr(*expr));
            insts.push(Inst::StackPop);
        }
        Stmt::Let { target, expr, .. } => {
            compile_expr(insts, simplify_expr(*expr));
            match target {
                LetTarget::Ident(ident) => {
                    insts.push(Inst::ScopeDefine(ident.name));
                }
                LetTarget::ArrDestruct(names) => {
                    insts.push(Inst::DestructureArray(Rc::from(names)));
                }
                LetTarget::ObjDestruct(items) => {
                    insts.push(Inst::DestructureObject(Rc::from(items)));
                }
            }
        }
        Stmt::FnDef {
            name,
            args,
            body,
            is_closure,
            ..
        } => {
            compile_func(
                insts,
                args,
                body,
                Some(name.name.clone()),
                is_closure,
            );
            insts.push(Inst::ScopeDefine(name.name));
        }
        Stmt::Assignment { target, rhs, .. } => {
            match target {
                AssignTarget::Ident(ident) => {
                    compile_expr(insts, simplify_expr(*rhs));
                    insts.push(Inst::ScopeStore(ident.name));
                }
                AssignTarget::Index(expr, index) => {
                    compile_expr(insts, simplify_expr(*expr));
                    compile_expr(insts, simplify_expr(*index));
                    compile_expr(insts, simplify_expr(*rhs));
                    insts.push(Inst::OperationIndexSet);
                }
                AssignTarget::Select(expr, selector) => {
                    compile_expr(insts, simplify_expr(*expr));
                    insts.push(Inst::StackPush(Value::from(selector.name)));
                    compile_expr(insts, simplify_expr(*rhs));
                    insts.push(Inst::OperationIndexSet);
                }
            };
        }
        Stmt::If { head, tail } => {
            let mut escape_indices: Vec<usize> =
                Vec::with_capacity(head.len() + 1);
            for block in head {
                compile_expr(insts, simplify_expr(*block.cond));
                let branch_idx = insts.len();
                insts.push(Inst::Illegal);
                let start_idx = insts.len();
                insts.push(compile_subframe(block.body));
                escape_indices.push(insts.len());
                insts.push(Inst::Illegal);
                let after_idx = insts.len();
                insts[branch_idx] =
                    Inst::BranchConditional(start_idx, after_idx);
            }
            if let Some(else_block) = tail {
                insts.push(compile_subframe(else_block.body));
            }
            let end_idx = insts.len();
            for idx in escape_indices {
                insts[idx] = Inst::BranchGoto(end_idx);
            }
        }
        Stmt::While { cond, body, .. } => {
            let cond_start_idx = insts.len();
            compile_expr(insts, simplify_expr(*cond));
            let cond_branch_idx = insts.len();
            insts.push(Inst::Illegal);
            let body_frame_idx = insts.len();
            insts.push(Inst::Illegal);
            insts.push(Inst::BranchGoto(cond_start_idx));
            let end_idx = insts.len();
            insts[body_frame_idx] = compile_while_loop(body, end_idx);
            insts[cond_branch_idx] =
                Inst::BranchConditional(body_frame_idx, end_idx);
        }
        Stmt::ForIn {
            ident, expr, body, ..
        } => {
            compile_expr(insts, simplify_expr(*expr));
            insts.push(Inst::BuildIter);
            let body_frame_idx = insts.len();
            insts.push(Inst::Illegal);
            let end_idx = insts.len();
            insts[body_frame_idx] = compile_for_loop(ident.name, body, end_idx);
        }
        Stmt::Return { expr, .. } => {
            compile_expr(insts, simplify_expr(*expr));
            insts.push(Inst::ControlReturn);
        }
        Stmt::Yield { expr, .. } => {
            compile_expr(insts, simplify_expr(*expr));
            insts.push(Inst::ControlYield);
        }
        Stmt::Throw { error, .. } => {
            compile_expr(insts, simplify_expr(*error));
            insts.push(Inst::ControlThrow);
        }
        Stmt::Break { .. } => insts.push(Inst::ControlBreak),
        Stmt::Try {
            body,
            catch,
            finally,
        } => {
            let mut try_insts = Vec::<Inst>::new();
            for stmt in body.body {
                compile_stmt(&mut try_insts, stmt);
            }
            let catch_ctx = if catch.is_some() {
                let c = catch.unwrap();
                let mut catch_insts = Vec::<Inst>::new();
                for stmt in c.body {
                    compile_stmt(&mut catch_insts, stmt);
                }
                Some(CatchContext(c.name.name, Rc::from(catch_insts)))
            } else {
                None
            };
            let finally_context = if finally.is_some() {
                let f = finally.unwrap();
                let mut finally_insts = Vec::<Inst>::new();
                for stmt in f.body {
                    compile_stmt(&mut finally_insts, stmt);
                }
                Some(FinallyContext(Rc::from(finally_insts)))
            } else {
                None
            };
            insts.push(Inst::RunTryFrame {
                insts:   Rc::from(try_insts),
                catch:   catch_ctx,
                finally: finally_context,
            })
        }
    }
}

fn compile_expr(insts: &mut Vec<Inst>, expr: Expr) {
    match expr {
        Expr::LitNull { .. } => insts.push(Inst::StackPush(Value::Null)),
        Expr::LitInt { val, .. } => {
            insts.push(Inst::StackPush(Value::Int(val)))
        }
        Expr::LitFlt { val, .. } => {
            insts.push(Inst::StackPush(Value::Flt(val)))
        }
        Expr::LitBool { val, .. } => {
            insts.push(Inst::StackPush(Value::Bool(val)))
        }
        Expr::LitStr { val, .. } => {
            insts.push(Inst::StackPush(Value::Str(val)))
        }
        Expr::LitFunc {
            args,
            body,
            is_closure,
            ..
        } => compile_func(insts, args, body, None, is_closure),
        Expr::LitArr { elements, .. } => {
            let elt_count = elements.len();
            for e in elements.into_iter().rev() {
                compile_expr(insts, e);
            }
            insts.push(Inst::StackPush(Value::from(elt_count as i64)));
            insts.push(Inst::BuildArray);
        }
        Expr::LitObj { fields, .. } => {
            let fieldct = fields.len();
            for field in fields {
                match field.key {
                    ObjKey::Static(val, ..) => {
                        insts.push(Inst::StackPush(Value::from(val)))
                    }
                    ObjKey::Dynamic(expr) => {
                        compile_expr(insts, *expr);
                    }
                }
                compile_expr(insts, *field.val);
            }
            insts.push(Inst::StackPush(Value::from(fieldct as i64)));
            insts.push(Inst::BuildObject);
        }
        Expr::Call { expr, args, .. } => {
            insts.push(Inst::CallBegin);
            for arg in args {
                compile_expr(insts, arg);
                insts.push(Inst::CallAppend);
            }
            compile_expr(insts, *expr);
            insts.push(Inst::CallEnd);
        }
        Expr::Ident(ident) => insts.push(Inst::ScopeLoad(ident.name)),
        Expr::Paren { expr, .. } => compile_expr(insts, *expr),
        Expr::Binary {
            lhs, rhs, op_typ, ..
        } => {
            compile_expr(insts, *lhs);
            compile_expr(insts, *rhs);
            insts.push(Inst::OperationBinary(op_typ));
        }
        Expr::Unary { expr, op_typ, .. } => {
            compile_expr(insts, *expr);
            insts.push(Inst::OperationUnary(op_typ));
        }
        Expr::Ternary {
            cond, pass, fail, ..
        } => {
            compile_expr(insts, *cond);
            let branch_idx = insts.len();
            insts.push(Inst::Illegal);
            let start_pass_idx = insts.len();
            compile_expr(insts, *pass);
            let end_pass_idx = insts.len();
            insts.push(Inst::Illegal);
            let start_fail_idx = end_pass_idx + 1;
            compile_expr(insts, *fail);
            let after_idx = insts.len();
            insts[branch_idx] =
                Inst::BranchConditional(start_pass_idx, start_fail_idx);
            insts[end_pass_idx] = Inst::BranchGoto(after_idx);
        }
        Expr::Index { expr, index, .. } => {
            compile_expr(insts, *expr);
            compile_expr(insts, *index);
            insts.push(Inst::OperationIndexGet);
        }
        Expr::Selector { expr, selector, .. } => {
            compile_expr(insts, *expr);
            insts.push(Inst::StackPush(Value::from(selector.name)));
            insts.push(Inst::OperationIndexGet);
        }
        Expr::Import { name, .. } => {
            insts.push(Inst::SysImport(name));
        }
        Expr::Typeof { expr, .. } => {
            compile_expr(insts, *expr);
            insts.push(Inst::SysTypeof);
        }
    }
}

fn compile_func(
    insts: &mut Vec<Inst>,
    args: Vec<FnArg>,
    body: Vec<Stmt>,
    name: Option<String>,
    is_closure: bool,
) {
    let mut fn_insts = Vec::<Inst>::new();
    for stmt in body {
        compile_stmt(&mut fn_insts, stmt);
    }
    insts.push(Inst::BuildFunc {
        args: args.into_iter().map(|a| a.name.clone()).collect(),
        insts: Rc::from(fn_insts),
        name: name.map(|v| Rc::from(v.as_ref())),
        is_closure,
    });
}

fn compile_subframe(stmts: Vec<Stmt>) -> Inst {
    let mut sub_insts = Vec::<Inst>::with_capacity(stmts.len());
    for stmt in stmts {
        compile_stmt(&mut sub_insts, stmt);
    }
    return Inst::RunFrame {
        insts: Rc::from(sub_insts),
    };
}

fn compile_while_loop(stmts: Vec<Stmt>, on_break: usize) -> Inst {
    let mut sub_insts = Vec::<Inst>::with_capacity(stmts.len());
    for stmt in stmts {
        compile_stmt(&mut sub_insts, stmt);
    }
    return Inst::RunLoopFrame {
        insts: Rc::from(sub_insts),
        on_break,
    };
}

fn compile_for_loop(var: String, stmts: Vec<Stmt>, on_break: usize) -> Inst {
    let mut insts = Vec::<Inst>::with_capacity(stmts.len());
    let branch_idx = insts.len();
    insts.push(Inst::Illegal);
    let body_idx = insts.len();
    for stmt in stmts {
        compile_stmt(&mut insts, stmt);
    }
    insts.push(Inst::BranchGoto(branch_idx));
    let end_idx = insts.len();
    insts[branch_idx] = Inst::BranchIter(var.clone(), body_idx, end_idx);
    return Inst::RunIterFrame {
        insts: Rc::from(insts),
        on_break,
    };
}

pub fn compile(src: Code, globals: Vec<String>) -> Result<Vec<Inst>, Error> {
    let stmts = Parser::parse_file(&src, true, globals)?;
    let mut insts = Vec::<Inst>::new();
    for stmt in stmts {
        compile_stmt(&mut insts, stmt);
    }
    Ok(insts)
}
