use crate::ast::ObjDestructItem;
use crate::compiler::compile;
use crate::error::{
    Error,
    Panic,
    RuntimeError,
};
use crate::runtime::builtin::{
    builtins,
    extend_builtins,
};
use crate::runtime::inst::Inst;
use crate::runtime::scope::Scope;
use crate::runtime::value::{
    Array,
    Func,
    Object,
    Value,
};

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug)]
pub struct VM {
    state: Rc<VMState>,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum FrameStatus {
    Broke,    // break statement encountered
    Returned, // return statement encountered
    Yielded,  // yield statement encountered
    Ended,    // last instruction was executed with no break or return
}

#[derive(Debug, Clone)]
pub struct FrameOutput {
    pub status: FrameStatus,
    pub retval: Option<Value>,
}

impl VM {
    pub fn eval(
        src: impl Into<String>,
        globals: Option<HashMap<String, Value>>,
    ) -> Result<(), Error> {
        let global_values = if globals.is_some() {
            extend_builtins(globals.unwrap())
        } else {
            builtins()
        };

        let insts = Rc::from(compile(
            src,
            global_values.keys().map(String::clone).collect(),
        )?);
        let root_scope = Rc::from(Scope::new(None));
        for (k, v) in global_values {
            root_scope.declare(k, v);
        }
        let v = VM {
            state: Rc::new(VMState {
                stack: RefCell::new(Vec::with_capacity(2048)),
                root_scope,
                insts,
                imports: RefCell::new(HashMap::new()),
            }),
        };
        v.run()
    }

    fn run(&self) -> Result<(), Error> {
        self.state.run_frame(
            Rc::clone(&self.state.insts),
            Rc::new(Scope::new(Some(Rc::clone(&self.state.root_scope)))),
            0,
        )?;
        Ok(())
    }
}

#[derive(Debug)]
pub struct VMState {
    stack:      RefCell<Vec<Value>>,
    root_scope: Rc<Scope>,
    insts:      Rc<[Inst]>,
    imports:    RefCell<HashMap<String, Object>>,
}

impl VMState {
    pub(crate) fn pop_stack(self: &Rc<Self>) -> Value {
        let mut stack = self.stack.borrow_mut();
        if let Some(popped) = stack.pop() {
            popped
        } else {
            panic!(Panic::StackUnderflow);
        }
    }

    pub(crate) fn push_stack(self: &Rc<Self>, val: Value) {
        let mut stack = self.stack.borrow_mut();
        stack.push(val);
    }

    pub(crate) fn truncate_stack(self: &Rc<Self>, sp: usize) {
        let mut stack = self.stack.borrow_mut();
        stack.truncate(sp);
    }

    pub(crate) fn import(
        self: &Rc<Self>,
        path: String,
    ) -> Result<Value, Error> {
        if let Some(existing) = self.imports.borrow().get(&path) {
            return Ok(Value::Object(existing.clone()));
        }
        let src = std::fs::read_to_string(&path).expect("unable to import");
        let insts: Rc<[Inst]> =
            Rc::from(compile(src, self.root_scope.names())?);
        let import_scope =
            Rc::new(Scope::new(Some(Rc::clone(&self.root_scope))));
        match self.run_frame(insts, import_scope.clone(), 0)?.status {
            FrameStatus::Ended => { /* all good */ }
            _ => panic!("PANIC: UNEXPECTED IMPORT FRAME STATUS"),
        }
        let import = Object::new();
        for name in import_scope.names() {
            let v = import_scope.get(&name);
            import.index_set(name, v);
        }
        self.imports.borrow_mut().insert(path, import.clone());
        Ok(Value::from(import.clone()))
    }

    #[inline(always)]
    pub(crate) fn run_frame(
        self: &Rc<Self>,
        insts: Rc<[Inst]>,
        scope: Rc<Scope>,
        start_ip: usize,
    ) -> Result<FrameOutput, Error> {
        let mut ip: usize = start_ip;
        let mut output = FrameOutput {
            status: FrameStatus::Ended,
            retval: None,
        };
        let ret_sp = self.stack.borrow().len();
        'frame: loop {
            if ip >= insts.len() {
                break;
            }
            match insts[ip].clone() {
                Inst::Illegal => panic!(Panic::IllegalInstruction),
                Inst::Import(name) => {
                    self.push_stack(self.import(name)?);
                }
                Inst::StackPop => {
                    self.pop_stack();
                }
                Inst::StackPush(v) => {
                    self.push_stack(v);
                }
                Inst::ScopeLoad(name) => {
                    self.push_stack(scope.get(&name));
                }
                Inst::ScopeStore(name) => {
                    scope.set(name, self.pop_stack());
                }
                Inst::ScopeDefine(name) => {
                    scope.declare(name, self.pop_stack());
                }
                Inst::DestructureArray(names) => {
                    let source = self.pop_stack();
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
                Inst::DestructureObject(items) => {
                    let source = self.pop_stack();
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
                Inst::OperationBinary(typ) => {
                    let rhs = self.pop_stack();
                    let lhs = self.pop_stack();
                    self.push_stack(Value::op_binary(&lhs, &rhs, typ)?);
                }
                Inst::OperationUnary(typ) => {
                    let target = self.pop_stack();
                    self.push_stack(Value::op_unary(&target, typ)?);
                }
                Inst::BranchIter(name, pass_ip, fail_ip) => {
                    let iter = self.pop_stack();
                    match iter {
                        Value::Iter(mut i) => {
                            if let Some(v) = i.next() {
                                scope.declare(name, v);
                                self.push_stack(Value::from(i));
                                ip = pass_ip;
                            } else {
                                ip = fail_ip;
                            }
                            continue 'frame;
                        }
                        _ => {
                            return Err(RuntimeError::InvalidOperation(
                                format!("can't iterate over {:?}", iter),
                            )
                            .into())
                        }
                    }
                }
                Inst::BranchConditional(pass_ip, fail_ip) => {
                    // TODO: protect against invalid or dangerous jumps
                    if self.pop_stack().truthy() {
                        ip = pass_ip;
                    } else {
                        ip = fail_ip;
                    }
                    continue 'frame;
                }
                Inst::BranchGoto(new_ip) => {
                    // TODO: protect against invalid or dangerous jumps
                    ip = new_ip;
                    continue 'frame;
                }
                Inst::BuildFunc {
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
                    )));
                }
                Inst::CallBegin => {
                    self.push_stack(Value::Int(0));
                }
                Inst::CallAppend => {
                    let arg = self.pop_stack();
                    if let Value::Int(argct) = self.pop_stack() {
                        self.push_stack(arg);
                        self.push_stack(Value::Int(argct + 1));
                    } else {
                        panic!(Panic::MalformedStack(
                            "invalid argct when adding arg",
                        ));
                    };
                }
                Inst::CallEnd => {
                    let target = self.pop_stack();
                    if let Value::Int(argct) = self.pop_stack() {
                        if argct < 0 {
                            panic!(Panic::MalformedStack(
                                "negative argct when finishing call",
                            ));
                        }
                        let mut args =
                            Vec::<Value>::with_capacity(argct as usize);
                        for _ in 0..argct {
                            args.push(self.pop_stack());
                        }
                        args.reverse();
                        self.push_stack(target.call(args)?);
                    } else {
                        panic!(Panic::MalformedStack(
                            "non-integer argct when finishing call",
                        ));
                    }
                }
                Inst::ControlReturn => {
                    output.retval = Some(self.pop_stack());
                    output.status = FrameStatus::Returned;
                    break 'frame;
                }
                Inst::ControlYield => {
                    output.retval = Some(self.pop_stack());
                    output.status = FrameStatus::Yielded;
                    break 'frame;
                }
                Inst::BuildObject => {
                    match self.pop_stack() {
                        Value::Int(field_count) if field_count >= 0 => {
                            let obj = Object::new();
                            for _ in 0..field_count {
                                let val = self.pop_stack();
                                let key = self.pop_stack();
                                obj.index_set(key.to_string(), val);
                            }
                            self.push_stack(Value::Object(obj));
                        }
                        _ => {
                            panic!(Panic::MalformedStack(
                                "invalid field count when building object",
                            ))
                        }
                    }
                }
                Inst::BuildArray => {
                    match self.pop_stack() {
                        Value::Int(element_count) if element_count >= 0 => {
                            let arr = Array::new();
                            for _ in 0..element_count {
                                arr.push_back(self.pop_stack());
                            }
                            self.push_stack(Value::Array(arr));
                        }
                        _ => {
                            panic!(Panic::MalformedStack(
                                "invalid element count when building array",
                            ))
                        }
                    }
                }
                Inst::BuildIter => {
                    self.push_stack(Value::from(self.pop_stack().iter()?));
                }
                Inst::OperationIndexGet => {
                    let index = self.pop_stack();
                    let target = self.pop_stack();
                    self.push_stack(target.op_index(&index)?);
                }
                Inst::OperationIndexSet => {
                    let value = self.pop_stack();
                    let index = self.pop_stack();
                    let target = self.pop_stack();
                    target.op_index_set(&index, &value)?;
                }
                Inst::ControlThrow => {
                    return Err(RuntimeError::Generic(
                        self.pop_stack().to_string(),
                    )
                    .into());
                }
                Inst::ControlBreak => {
                    output.status = FrameStatus::Broke;
                    break 'frame;
                }
                Inst::RunFrame { insts } => {
                    match self.run_frame(
                        insts,
                        Rc::new(Scope::extend(Rc::clone(&scope))),
                        0,
                    )? {
                        FrameOutput { status, retval }
                            if [
                                FrameStatus::Returned,
                                FrameStatus::Yielded,
                            ]
                            .contains(&status) =>
                        {
                            output.retval = retval;
                            output.status = status;
                            break 'frame;
                        }
                        FrameOutput {
                            status: FrameStatus::Broke,
                            ..
                        } => {
                            output.retval = None;
                            output.status = FrameStatus::Broke;
                            break 'frame;
                        }
                        _ => {}
                    }
                }
                Inst::RunLoopFrame { insts, on_break } => {
                    match self.run_frame(
                        insts,
                        Rc::new(Scope::extend(Rc::clone(&scope))),
                        0,
                    )? {
                        FrameOutput { status, retval }
                            if [
                                FrameStatus::Returned,
                                FrameStatus::Yielded,
                            ]
                            .contains(&status) =>
                        {
                            output.retval = retval;
                            output.status = status;
                            break 'frame;
                        }
                        FrameOutput {
                            status: FrameStatus::Broke,
                            ..
                        } => {
                            ip = on_break;
                            continue 'frame;
                        }
                        _ => {}
                    }
                }
                Inst::RunIterFrame { insts, on_break } => {
                    match self.run_frame(
                        insts,
                        Rc::new(Scope::extend(Rc::clone(&scope))),
                        0,
                    )? {
                        FrameOutput { status, retval }
                            if [
                                FrameStatus::Returned,
                                FrameStatus::Yielded,
                            ]
                            .contains(&status) =>
                        {
                            output.retval = retval;
                            output.status = status;
                            break 'frame;
                        }
                        FrameOutput { status, .. }
                            if [FrameStatus::Broke, FrameStatus::Ended]
                                .contains(&status) =>
                        {
                            if status == FrameStatus::Broke {
                                self.pop_stack(); // remove iter from stack
                            }
                            ip = on_break;
                            continue 'frame;
                        }
                        _ => {}
                    }
                }
            }
            ip += 1;
        }
        self.truncate_stack(ret_sp);
        Ok(output)
    }
}
