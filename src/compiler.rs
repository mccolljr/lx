use super::{
    ast::{
        AssignTarget,
        Expr,
        FnArg,
        LetTarget,
        ObjKey,
        Stmt,
    },
    ast_rewrite::simplify_expr,
    errors::Error,
    parser::Parser,
    runtime::{
        inst::Inst,
        value::Value,
    },
    source::Code,
    token::TokenType,
};
use std::rc::Rc;

fn compile_stmt(insts: &mut Vec<Inst>, stmt: Stmt) {
    match stmt {
        Stmt::Expr { expr, .. } => {
            compile_expr(insts, simplify_expr(*expr));
            insts.push(Inst::PopStack());
        }
        Stmt::Let { target, expr, .. } => {
            compile_expr(insts, simplify_expr(*expr));
            match target {
                LetTarget::Ident(name) => {
                    insts.push(Inst::DeclareNamed(name));
                }
                LetTarget::ArrDestruct(names) => {
                    insts.push(Inst::ArrDestruct(Rc::from(names)));
                }
                LetTarget::ObjDestruct(items) => {
                    insts.push(Inst::ObjDestruct(Rc::from(items)));
                }
            }
        }
        Stmt::FnDef {
            ident_name,
            args,
            body,
            is_closure,
            ..
        } => {
            compile_func(
                insts,
                args,
                body,
                Some(ident_name.clone()),
                is_closure,
            );
            insts.push(Inst::DeclareNamed(ident_name.clone()));
        }
        Stmt::Assignment { target, rhs, .. } => {
            match target {
                AssignTarget::Ident(name) => {
                    compile_expr(insts, simplify_expr(*rhs));
                    insts.push(Inst::StoreNamed(name.clone()));
                }
                AssignTarget::Index(expr, index) => {
                    compile_expr(insts, simplify_expr(*expr));
                    compile_expr(insts, simplify_expr(*index));
                    compile_expr(insts, simplify_expr(*rhs));
                    insts.push(Inst::IndexSet);
                }
                AssignTarget::Select(expr, elt_name) => {
                    compile_expr(insts, simplify_expr(*expr));
                    insts.push(Inst::PushStack(Value::from(elt_name)));
                    compile_expr(insts, simplify_expr(*rhs));
                    insts.push(Inst::IndexSet);
                }
            };
        }
        Stmt::If { head, tail } => {
            let mut escape_indices: Vec<usize> =
                Vec::with_capacity(head.len() + 1);
            for block in head {
                compile_expr(insts, simplify_expr(*block.cond));
                let branch_idx = insts.len();
                insts.push(Inst::Noop);
                let start_idx = insts.len();
                for stmt in block.body {
                    compile_stmt(insts, stmt);
                }
                escape_indices.push(insts.len());
                insts.push(Inst::Noop);
                let after_idx = insts.len();
                insts[branch_idx] = Inst::Branch(start_idx, after_idx);
            }
            if let Some(else_block) = tail {
                for stmt in else_block.body {
                    compile_stmt(insts, stmt);
                }
            }
            let end_idx = insts.len();
            for idx in escape_indices {
                insts[idx] = Inst::Goto(end_idx);
            }
        }
        Stmt::Return { expr, .. } => {
            compile_expr(insts, simplify_expr(*expr));
            insts.push(Inst::Return);
        }
        Stmt::Throw { error, .. } => {
            compile_expr(insts, simplify_expr(*error));
            insts.push(Inst::Throw);
        }
    }
}

fn compile_expr(insts: &mut Vec<Inst>, expr: Expr) {
    match expr {
        Expr::LitNull { .. } => insts.push(Inst::PushStack(Value::Null)),
        Expr::LitInt { val, .. } => {
            insts.push(Inst::PushStack(Value::Int(val)))
        }
        Expr::LitFlt { val, .. } => {
            insts.push(Inst::PushStack(Value::Flt(val)))
        }
        Expr::LitBool { val, .. } => {
            insts.push(Inst::PushStack(Value::Bool(val)))
        }
        Expr::LitStr { val, .. } => {
            insts.push(Inst::PushStack(Value::Str(val)))
        }
        Expr::LitFunc {
            args,
            body,
            is_closure,
            ..
        } => compile_func(insts, args, body, None, is_closure),
        Expr::LitArr { elements, .. } => {
            let elt_count = elements.len();
            for e in elements {
                compile_expr(insts, e);
            }
            insts.push(Inst::PushStack(Value::from(elt_count as i64)));
            insts.push(Inst::BuildArray);
        }
        Expr::LitObj { fields, .. } => {
            let fieldct = fields.len();
            for field in fields {
                match field.key {
                    ObjKey::Static(val, ..) => {
                        insts.push(Inst::PushStack(Value::from(val)))
                    }
                    ObjKey::Dynamic(expr) => {
                        compile_expr(insts, *expr);
                    }
                }
                compile_expr(insts, *field.val);
            }
            insts.push(Inst::PushStack(Value::from(fieldct as i64)));
            insts.push(Inst::BuildObject);
        }
        Expr::Call { expr, args, .. } => {
            insts.push(Inst::InitCall);
            for arg in args {
                compile_expr(insts, arg);
                insts.push(Inst::AddCallArg);
            }
            compile_expr(insts, *expr);
            insts.push(Inst::FinishCall);
        }
        Expr::Ident { name, .. } => insts.push(Inst::LoadNamed(name.clone())),
        Expr::Paren { expr, .. } => compile_expr(insts, *expr),
        Expr::Binary {
            lhs, rhs, op_typ, ..
        } => {
            compile_expr(insts, *lhs);
            compile_expr(insts, *rhs);
            insts.push(Inst::BinaryOp(op_typ));
        }
        Expr::Unary { expr, op_typ, .. } => {
            compile_expr(insts, *expr);
            insts.push(Inst::UnaryOp(op_typ));
        }
        Expr::Ternary {
            cond, pass, fail, ..
        } => {
            compile_expr(insts, *cond);
            let branch_idx = insts.len();
            insts.push(Inst::Noop);
            let start_pass_idx = insts.len();
            compile_expr(insts, *pass);
            let end_pass_idx = insts.len();
            insts.push(Inst::Noop);
            let start_fail_idx = end_pass_idx + 1;
            compile_expr(insts, *fail);
            let after_idx = insts.len();
            insts[branch_idx] = Inst::Branch(start_pass_idx, start_fail_idx);
            insts[end_pass_idx] = Inst::Goto(after_idx);
        }
        Expr::Index { expr, index, .. } => {
            compile_expr(insts, *expr);
            compile_expr(insts, *index);
            insts.push(Inst::Index);
        }
        Expr::Selector { expr, elt_name, .. } => {
            compile_expr(insts, *expr);
            insts.push(Inst::PushStack(Value::from(elt_name)));
            insts.push(Inst::Index);
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
    insts.push(Inst::MakeFunc {
        args: args.into_iter().map(|a| a.name.clone()).collect(),
        insts: Rc::from(fn_insts),
        name,
        is_closure,
    });
}

pub fn compile(src: &str) -> Result<Vec<Inst>, Error> {
    let mut parser = Parser::new(&Code::from(src), true);
    let stmts = parser.parse_stmt_list(vec![TokenType::EOF])?;
    let mut insts = Vec::<Inst>::new();
    for stmt in stmts {
        compile_stmt(&mut insts, stmt);
    }
    Ok(insts)
}
