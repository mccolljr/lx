use super::{
    error::Error,
    inst::Inst,
    scope::Scope,
    value::{
        Func,
        Value,
    },
};
use std::{
    cell::RefCell,
    collections::HashMap,
    rc::Rc,
};

#[derive(Debug)]
pub struct VM {
    state: Rc<VMState>,
}

#[derive(Debug)]
pub struct VMState {
    stack:      RefCell<Vec<Value>>,
    root_scope: Rc<Scope>,
    insts:      Rc<[Inst]>,
}

impl VM {
    pub fn new(prog: Vec<Inst>) -> Self {
        VM {
            state: Rc::new(VMState {
                stack:      RefCell::new(Vec::with_capacity(2048)),
                root_scope: Rc::from(Scope::new(None)),
                insts:      Rc::from(prog),
            }),
        }
    }

    pub fn with_global(
        self,
        key: impl Into<String>,
        val: impl Into<Value>,
    ) -> Self {
        self.state.root_scope.set(key.into(), val.into());
        self
    }

    pub fn run(&self) -> Result<(), Error> {
        self.state.run_frame(
            Rc::clone(&self.state.insts),
            Rc::clone(&self.state.root_scope),
        )
    }
}

impl VMState {
    pub(crate) fn pop_stack(self: &Rc<Self>) -> Result<Value, Error> {
        let mut stack = self.stack.borrow_mut();
        if let Some(popped) = stack.pop() {
            Ok(popped)
        } else {
            Err(Error::StackUnderflow)
        }
    }

    pub(crate) fn push_stack(self: &Rc<Self>, val: Value) -> Result<(), Error> {
        let mut stack = self.stack.borrow_mut();
        stack.push(val);
        Ok(())
    }

    pub(crate) fn truncate_stack(self: &Rc<Self>, sp: usize) {
        let mut stack = self.stack.borrow_mut();
        stack.truncate(sp);
    }

    #[inline(always)]
    pub(crate) fn run_frame(
        self: &Rc<Self>,
        insts: Rc<[Inst]>,
        scope: Rc<Scope>,
    ) -> Result<(), Error> {
        let mut ip: usize = 0;
        let mut ret_val = Value::Null;
        let ret_sp = self.stack.borrow().len();
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
                    self.push_stack(Value::op_binary(&lhs, &rhs, typ)?)?;
                }
                Inst::UnaryOp(typ) => {
                    let target = self.pop_stack()?;
                    self.push_stack(Value::op_unary(&target, typ)?)?;
                }
                Inst::Branch(pass_ip, fail_ip) => {
                    // TODO: protect against invalid or dangerous jumps
                    if self.pop_stack()?.truthy() {
                        ip = pass_ip;
                    } else {
                        ip = fail_ip;
                    }
                    continue;
                }
                Inst::Goto(new_ip) => {
                    // TODO: protect against invalid or dangerous jumps
                    ip = new_ip;
                    continue;
                }
                Inst::MakeClosure(fn_args, fn_insts, maybe_name) => {
                    let name = maybe_name
                        .map_or("anonymous".into(), |name| name.clone());
                    self.push_stack(Value::Func(Func::new(
                        &name,
                        Rc::clone(self),
                        Rc::from(fn_args),
                        Rc::clone(&fn_insts),
                        Rc::new(Scope::extend(Rc::clone(&scope))),
                    )))?;
                }
                Inst::InitCall => {
                    self.push_stack(Value::Int(0))?;
                }
                Inst::AddCallArg => {
                    let arg = self.pop_stack()?;
                    if let Value::Int(argct) = self.pop_stack()? {
                        self.push_stack(arg)?;
                        self.push_stack(Value::Int(argct + 1))?;
                    } else {
                        return Err(Error::MalformedStackPanic);
                    };
                }
                Inst::FinishCall => {
                    let target = self.pop_stack()?;
                    if let Value::Int(argct) = self.pop_stack()? {
                        if argct < 0 {
                            return Err(Error::MalformedStackPanic);
                        }
                        let mut args =
                            Vec::<Value>::with_capacity(argct as usize);
                        for _ in 0..argct {
                            args.push(self.pop_stack()?);
                        }
                        args.reverse();
                        self.push_stack(target.call(args)?)?;
                    } else {
                        return Err(Error::MalformedStackPanic);
                    }
                }
                Inst::Return => {
                    ret_val = self.pop_stack()?;
                    break;
                }
                Inst::BuildObject => {
                    if let Value::Int(field_count) = self.pop_stack()? {
                        let mut obj = HashMap::<String, Value>::with_capacity(
                            field_count as usize,
                        );
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
                    return Err(Error::RuntimeError(
                        self.pop_stack()?.to_string(),
                    ));
                }
            }
            ip += 1;
        }
        self.truncate_stack(ret_sp);
        self.push_stack(ret_val)?;
        Ok(())
    }
}
