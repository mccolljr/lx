use super::super::token::TokenType;
use super::error::Error;
use super::inst::Inst;
use super::scope::Scope;
use super::value::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug)]
pub struct VM {
    stack: RefCell<Vec<Value>>,
    root_scope: Rc<Scope>,
    insts: Rc<[Inst]>,
}

impl VM {
    pub fn new(prog: Vec<Inst>) -> Self {
        VM {
            stack: RefCell::new(Vec::with_capacity(2048)),
            root_scope: Rc::from(Scope::new(None)),
            insts: Rc::from(prog),
        }
    }

    pub fn with_global(self, key: impl Into<String>, val: Value) -> Self {
        self.root_scope.set(key.into(), val);
        self
    }

    fn pop_stack(&self) -> Result<Value, Error> {
        let mut stack = self.stack.borrow_mut();
        stack.pop().map_or(Err(Error::StackUnderflow), |v| Ok(v))
    }

    fn push_stack(&self, val: Value) -> Result<(), Error> {
        let mut stack = self.stack.borrow_mut();
        stack.push(val);
        Ok(())
    }

    fn truncate_stack(&self, sp: usize) {
        let mut stack = self.stack.borrow_mut();
        stack.truncate(sp);
    }

    fn run_frame(&self, insts: Rc<[Inst]>, scope: Rc<Scope>, ret_sp: usize) -> Result<(), Error> {
        let mut ip: usize = 0;
        let mut ret_val = Value::Null;
        loop {
            if ip >= insts.len() {
                break;
            }
            match insts[ip].clone() {
                Inst::Noop => {}
                Inst::PopStack() => {
                    self.pop_stack()?;
                }
                Inst::PushStack(ref v) => {
                    self.push_stack(v.clone())?;
                }
                Inst::LoadNamed(ref name) => {
                    self.push_stack(scope.get(name))?;
                }
                Inst::StoreNamed(ref name) => {
                    scope.set(name.clone(), self.pop_stack()?);
                }
                Inst::DeclareNamed(ref name) => {
                    scope.declare(name.clone(), self.pop_stack()?);
                }
                Inst::BinaryOp(typ) => {
                    let rhs = self.pop_stack()?;
                    let lhs = self.pop_stack()?;
                    self.push_stack(match typ {
                        TokenType::OpAdd => lhs.op_add(&rhs)?,
                        TokenType::OpSub => lhs.op_sub(&rhs)?,
                        TokenType::OpMul => lhs.op_mul(&rhs)?,
                        TokenType::OpDiv => lhs.op_div(&rhs)?,
                        TokenType::OpRem => lhs.op_rem(&rhs)?,
                        TokenType::OpEq => lhs.op_eq(&rhs)?,
                        TokenType::OpNeq => lhs.op_neq(&rhs)?,
                        TokenType::OpGeq => lhs.op_geq(&rhs)?,
                        TokenType::OpLeq => lhs.op_leq(&rhs)?,
                        TokenType::OpGt => lhs.op_gt(&rhs)?,
                        TokenType::OpLt => lhs.op_lt(&rhs)?,
                        TokenType::OpFeed => lhs.op_feed(&rhs)?,
                        _ => unreachable!(),
                    })?;
                }
                Inst::UnaryOp(typ) => {
                    let target = self.pop_stack()?;
                    self.push_stack(match typ {
                        TokenType::OpSub => target.op_sub_unary()?,
                        TokenType::Bang => target.op_not_unary()?,
                        TokenType::OpFeed => target.op_feed_unary()?,
                        _ => unreachable!(),
                    })?;
                }
                Inst::Branch(pass_ip, fail_ip) => {
                    // TODO guard against invalid jumps
                    if self.pop_stack()?.truthy() {
                        ip = pass_ip;
                    } else {
                        ip = fail_ip;
                    }
                    continue;
                }
                Inst::Goto(new_ip) => {
                    ip = new_ip;
                    continue;
                }
                Inst::MakeClosure(fn_argct, fn_insts) => {
                    self.push_stack(Value::Func {
                        argct: fn_argct,
                        insts: fn_insts.clone(),
                        outer: Rc::new(Scope::extend(Rc::clone(&scope))),
                    })?;
                }
                Inst::InitCall => {
                    self.push_stack(Value::Int(0))?;
                }
                Inst::AddCallArg => {
                    let arg = self.pop_stack()?;
                    let argct = self.pop_stack()?;
                    self.push_stack(arg)?;
                    self.push_stack(argct.op_add(&Value::Int(1))?)?;
                }
                Inst::FinishCall => {
                    let target = self.pop_stack()?;
                    match target {
                        Value::Func { insts, outer, .. } => {
                            self.run_frame(
                                insts,
                                Rc::new(Scope::extend(outer)),
                                self.get_return_sp()?,
                            )?;
                        }
                        Value::NativeFunc { f, .. } => {
                            if let Value::Int(argct) = self.pop_stack()? {
                                if argct < 0 {
                                    return Err(Error::MalformedStackPanic);
                                }
                                let mut args = Vec::<Value>::with_capacity(argct as usize);
                                for _ in 0..argct {
                                    args.push(self.pop_stack()?);
                                }
                                args.reverse();
                                self.push_stack(f(args)?)?;
                            } else {
                                return Err(Error::MalformedStackPanic);
                            }
                        }
                        _ => {
                            return Err(Error::InvalidOperation(format!(
                                "can't call {:?}",
                                target
                            )));
                        }
                    }
                }
                Inst::Return => {
                    ret_val = self.pop_stack()?;
                    break;
                }
                Inst::BuildObject => {
                    if let Value::Int(field_count) = self.pop_stack()? {
                        let mut obj = HashMap::<String, Value>::with_capacity(field_count as usize);
                        for _ in 0..field_count {
                            let val = self.pop_stack()?;
                            let key = self.pop_stack()?;
                            obj.insert(key.to_string(), val.clone());
                        }
                        self.push_stack(Value::from(obj))?;
                    } else {
                        return Err(Error::MalformedStackPanic);
                    }
                }
                Inst::Index => {
                    let index = self.pop_stack()?;
                    let target = self.pop_stack()?;
                    self.push_stack(target.op_index(&index)?)?;
                }
                Inst::IndexSet => {
                    let value = self.pop_stack()?;
                    let index = self.pop_stack()?;
                    let target = self.pop_stack()?;
                    target.op_index_set(&index, &value)?;
                }
                Inst::Throw => {
                    return Err(Error::RuntimeError(self.pop_stack()?.to_string()));
                }
            }
            ip += 1;
        }
        self.truncate_stack(ret_sp);
        self.push_stack(ret_val)?;
        Ok(())
    }

    pub fn run(&self) -> Result<(), Error> {
        self.run_frame(self.insts.clone(), self.root_scope.clone(), 0)
    }

    fn get_return_sp(&self) -> Result<usize, Error> {
        let stack = self.stack.borrow();
        let size = stack.len();
        let last = stack.last();
        if let Some(Value::Int(argct)) = last {
            Ok(size - (*argct as usize) - 1)
        } else {
            Err(Error::MalformedStackPanic)
        }
    }
}
