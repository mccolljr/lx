use super::{
    ast::{
        Expr,
        FnArg,
        Stmt,
    },
    errors::Error,
    parser::Parser,
    runtime::{
        error::Error as RuntimeError,
        inst::Inst,
        value::Value,
    },
    source::Code,
    token::TokenType,
};
use std::{
    collections::{
        HashMap,
        VecDeque,
    },
    rc::Rc,
};

fn const_eval(expr: Expr) -> Result<Value, RuntimeError> {
    match expr {
        Expr::LitNull { .. } => Ok(Value::Null),
        Expr::LitInt { val, .. } => Ok(Value::Int(val)),
        Expr::LitFlt { val, .. } => Ok(Value::Flt(val)),
        Expr::LitBool { val, .. } => Ok(Value::Bool(val)),
        Expr::LitStr { val, .. } => Ok(Value::Str(val)),
        Expr::LitArr { elements, .. } => {
            let mut result =
                Value::from(VecDeque::with_capacity(elements.len()));
            for elt in elements {
                result = result.op_feed(&const_eval(elt)?)?;
            }
            Ok(result)
        }
        Expr::LitObj { fields, .. } => {
            let result = Value::from(HashMap::new());
            for field in fields {
                match *field.key {
                    Expr::LitStr { val, .. } => {
                        result.op_index_set(
                            &Value::Str(val),
                            &const_eval(*field.val)?,
                        )?;
                    }
                    Expr::Ident { name, .. } => {
                        result.op_index_set(
                            &Value::Str(name),
                            &const_eval(*field.val)?,
                        )?;
                    }
                    _ => {
                        return Err(RuntimeError::RuntimeError(
                            "invalid constant evaluation".into(),
                        ));
                    }
                }
            }
            Ok(result)
        }
        Expr::Paren { expr, .. } => const_eval(*expr),
        Expr::Binary {
            lhs: lhs_expr,
            rhs: rhs_expr,
            op_typ,
            ..
        } => {
            let lhs = const_eval(*lhs_expr)?;
            let rhs = const_eval(*rhs_expr)?;
            Value::op_binary(&lhs, &rhs, op_typ)
        }
        Expr::Unary { expr, op_typ, .. } => {
            let target = const_eval(*expr)?;
            Value::op_unary(&target, op_typ)
        }
        Expr::Ternary {
            cond, pass, fail, ..
        } => {
            if const_eval(*cond)?.truthy() {
                const_eval(*pass)
            } else {
                const_eval(*fail)
            }
        }
        _ => {
            Err(RuntimeError::RuntimeError(
                "invalid constant evaluation".into(),
            ))
        }
    }
}

fn compile_stmt(insts: &mut Vec<Inst>, stmt: Stmt) {
    match stmt {
        Stmt::Expr { expr, .. } => {
            if expr.is_const_lit() {
                // Don't compile const literal expr statements,
                // since they don't do anything at runtime.
                return;
            }
            compile_expr(insts, *expr);
            insts.push(Inst::PopStack());
        }
        Stmt::Let {
            ident_name, expr, ..
        } => {
            compile_expr(insts, *expr);
            insts.push(Inst::DeclareNamed(ident_name.clone()));
        }
        Stmt::FnDef {
            ident_name,
            args,
            body,
            ..
        } => {
            compile_func(insts, args, body, Some(ident_name.clone()));
            insts.push(Inst::DeclareNamed(ident_name.clone()));
        }
        Stmt::Assignment { lhs, rhs, .. } => {
            match *lhs {
                Expr::Ident { name, .. } => {
                    compile_expr(insts, *rhs);
                    insts.push(Inst::StoreNamed(name.clone()));
                }
                Expr::Index { expr, index, .. } => {
                    compile_expr(insts, *expr);
                    compile_expr(insts, *index);
                    compile_expr(insts, *rhs);
                    insts.push(Inst::IndexSet);
                }
                Expr::Selector { expr, elt_name, .. } => {
                    compile_expr(insts, *expr);
                    insts.push(Inst::PushStack(Value::from(elt_name)));
                    compile_expr(insts, *rhs);
                    insts.push(Inst::IndexSet);
                }
                _ => unreachable!(),
            };
        }
        Stmt::If { head, tail } => {
            let mut escape_indices: Vec<usize> =
                Vec::with_capacity(head.len() + 1);
            for block in head {
                compile_expr(insts, *block.cond);
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
            compile_expr(insts, *expr);
            insts.push(Inst::Return);
        }
        Stmt::Throw { error, .. } => {
            compile_expr(insts, *error);
            insts.push(Inst::Throw);
        }
    }
}

fn compile_expr(insts: &mut Vec<Inst>, expr: Expr) {
    if expr.is_const_lit() {
        insts.push(Inst::PushStack(const_eval(expr).unwrap()));
        return;
    }
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
            insts.push(Inst::PushStack(Value::Str(val.clone())))
        }
        Expr::LitFunc { args, body, .. } => {
            compile_func(insts, args, body, None)
        }
        Expr::LitArr { elements, .. } => {
            insts.push(Inst::PushStack(Value::from(VecDeque::new())));
            for e in elements {
                compile_expr(insts, e);
                insts.push(Inst::BinaryOp(TokenType::OpFeed))
            }
        }
        Expr::LitObj { fields, .. } => {
            let fieldct = fields.len();
            for field in fields {
                match *field.key {
                    Expr::Ident { name, .. } => {
                        insts.push(Inst::PushStack(Value::from(name.clone())))
                    }
                    Expr::LitStr { val, .. } => {
                        insts.push(Inst::PushStack(Value::from(val.clone())))
                    }
                    Expr::Paren { expr, .. } => {
                        compile_expr(insts, *expr);
                    }
                    _ => unreachable!(),
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
) {
    let mut fn_insts = Vec::<Inst>::new();
    for stmt in body {
        compile_stmt(&mut fn_insts, stmt);
    }
    insts.push(Inst::MakeClosure(
        args.into_iter().map(|a| a.name.clone()).collect(),
        Rc::from(fn_insts),
        name,
    ));
}

pub fn compile(src: &str) -> Result<Vec<Inst>, Error> {
    let mut parser = Parser::new(&Code::from(src));
    let stmts = parser.parse_stmt_list(vec![TokenType::EOF])?;
    let mut insts = Vec::<Inst>::new();
    for stmt in stmts {
        compile_stmt(&mut insts, stmt);
    }
    Ok(insts)
}
