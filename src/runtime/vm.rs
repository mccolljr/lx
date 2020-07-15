use super::{
    super::ast::ObjDestructItem,
    inst::Inst,
    scope::Scope,
    value::{
        Array,
        Func,
        Object,
        Value,
    },
};
use crate::error::{
    Error,
    Panic,
    RuntimeError,
};
use std::{
    cell::RefCell,
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

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum FrameStatus {
    Broke,    // break statement encountered
    Returned, // return statement encountered
    Ended,    // last instruction was executed with no break or return
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
        )?;
        Ok(())
    }
}

impl VMState {
    pub(crate) fn pop_stack(self: &Rc<Self>) -> Result<Value, Error> {
        let mut stack = self.stack.borrow_mut();
        if let Some(popped) = stack.pop() {
            Ok(popped)
        } else {
            Err(Panic::StackUnderflow.into())
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
    ) -> Result<FrameStatus, Error> {
        let mut ip: usize = 0;
        let mut ret_val = Value::Null;
        let mut exit_status = FrameStatus::Ended;
        let ret_sp = self.stack.borrow().len();
        'frame: loop {
            if ip >= insts.len() {
                break;
            }
            match insts[ip].clone() {
                Inst::Illegal => return Err(Panic::IllegalInstruction.into()),
                Inst::Noop => {}
                Inst::PopStack() => {
                    self.pop_stack()?;
                }
                Inst::PushStack(v) => {
                    self.push_stack(v)?;
                }
                Inst::LoadNamed(name) => {
                    self.push_stack(scope.get(&name))?;
                }
                Inst::StoreNamed(name) => {
                    scope.set(name, self.pop_stack()?);
                }
                Inst::DeclareNamed(name) => {
                    scope.declare(name, self.pop_stack()?);
                }
                Inst::ArrDestruct(names) => {
                    let source = self.pop_stack()?;
                    if let Value::Array(arr) = source {
                        for (i, name) in names.iter().enumerate() {
                            scope.declare(name.clone(), arr.index_get(i));
                        }
                    } else {
                        return Err(RuntimeError::InvalidOperation(format!(
                            "can't destructure {:?} as an array",
                            source
                        ))
                        .into());
                    }
                }
                Inst::ObjDestruct(items) => {
                    let source = self.pop_stack()?;
                    if let Value::Object(obj) = source {
                        for item in items.iter() {
                            match item {
                                ObjDestructItem::Name(ident) => {
                                    scope.declare(
                                        ident.name.clone(),
                                        obj.index_get(&ident.name),
                                    );
                                }
                                ObjDestructItem::NameMap(key, ident) => {
                                    scope.declare(
                                        ident.name.clone(),
                                        obj.index_get(key),
                                    );
                                }
                            }
                        }
                    } else {
                        return Err(RuntimeError::InvalidOperation(format!(
                            "can't destructure {:?} as an object",
                            source
                        ))
                        .into());
                    }
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
                    continue 'frame;
                }
                Inst::Goto(new_ip) => {
                    // TODO: protect against invalid or dangerous jumps
                    ip = new_ip;
                    continue 'frame;
                }
                Inst::MakeFunc {
                    args: fn_args,
                    insts: fn_insts,
                    name: maybe_name,
                    is_closure,
                } => {
                    let name = maybe_name
                        .map_or("anonymous".into(), |name| name.clone());
                    self.push_stack(Value::Func(Func::new(
                        &name,
                        Rc::clone(self),
                        Rc::from(fn_args),
                        Rc::clone(&fn_insts),
                        if is_closure {
                            Some(Rc::clone(&scope))
                        } else {
                            None
                        },
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
                        return Err(Panic::MalformedStack(
                            "invalid argct when adding arg",
                        )
                        .into());
                    };
                }
                Inst::FinishCall => {
                    let target = self.pop_stack()?;
                    if let Value::Int(argct) = self.pop_stack()? {
                        if argct < 0 {
                            return Err(Panic::MalformedStack(
                                "negative argct when finishing call",
                            )
                            .into());
                        }
                        let mut args =
                            Vec::<Value>::with_capacity(argct as usize);
                        for _ in 0..argct {
                            args.push(self.pop_stack()?);
                        }
                        args.reverse();
                        self.push_stack(target.call(args)?)?;
                    } else {
                        return Err(Panic::MalformedStack(
                            "non-integer argct when finishing call",
                        )
                        .into());
                    }
                }
                Inst::Return => {
                    ret_val = self.pop_stack()?;
                    exit_status = FrameStatus::Returned;
                    break 'frame;
                }
                Inst::BuildObject => {
                    match self.pop_stack()? {
                        Value::Int(field_count) if field_count >= 0 => {
                            let obj = Object::new();
                            for _ in 0..field_count {
                                let val = self.pop_stack()?;
                                let key = self.pop_stack()?;
                                obj.index_set(key.to_string(), val);
                            }
                            self.push_stack(Value::Object(obj))?;
                        }
                        _ => {
                            return Err(Panic::MalformedStack(
                                "invalid field count when building object",
                            )
                            .into())
                        }
                    }
                }
                Inst::BuildArray => {
                    match self.pop_stack()? {
                        Value::Int(element_count) if element_count >= 0 => {
                            let arr = Array::new();
                            for _ in 0..element_count {
                                arr.push_back(self.pop_stack()?);
                            }
                            self.push_stack(Value::Array(arr))?;
                        }
                        _ => {
                            return Err(Panic::MalformedStack(
                                "invalid element count when building array",
                            )
                            .into())
                        }
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
                    return Err(RuntimeError::Generic(
                        self.pop_stack()?.to_string(),
                    )
                    .into());
                }
                Inst::Break => {
                    exit_status = FrameStatus::Broke;
                    break 'frame;
                }
                Inst::Subframe { insts, on_break } => {
                    match self.run_frame(
                        insts,
                        Rc::new(Scope::extend(Rc::clone(&scope))),
                    )? {
                        FrameStatus::Returned => {
                            ret_val = self.pop_stack()?;
                            exit_status = FrameStatus::Returned;
                            break 'frame;
                        }
                        FrameStatus::Broke => {
                            self.pop_stack()?;
                            if on_break.is_some() {
                                ip = on_break.unwrap();
                                continue 'frame;
                            } else {
                                exit_status = FrameStatus::Broke;
                                break 'frame;
                            }
                        }
                        FrameStatus::Ended => {
                            self.pop_stack()?;
                        }
                    }
                }
                Inst::Iterate {
                    var,
                    insts,
                    on_break,
                } => {
                    let iterator = self.pop_stack()?.iter()?;
                    let scope = &Rc::new(Scope::extend(Rc::clone(&scope)));
                    for v in iterator {
                        scope.declare(var.clone(), v);
                        match self.run_frame(insts.clone(), Rc::clone(scope))? {
                            FrameStatus::Returned => {
                                ret_val = self.pop_stack()?;
                                exit_status = FrameStatus::Returned;
                                break 'frame;
                            }
                            FrameStatus::Broke => {
                                self.pop_stack()?;
                                ip = on_break;
                                continue 'frame;
                            }
                            FrameStatus::Ended => {
                                self.pop_stack()?;
                            }
                        }
                    }
                }
            }
            ip += 1;
        }
        self.truncate_stack(ret_sp);
        self.push_stack(ret_val)?;
        Ok(exit_status)
    }
}
