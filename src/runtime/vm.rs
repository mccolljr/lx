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
use crate::runtime::frame::{
    CatchContext,
    FinallyContext,
    Frame,
    FrameStatus,
};
use crate::runtime::inst::Inst;
use crate::runtime::scope::Scope;
use crate::runtime::value::{
    Array,
    Func,
    Object,
    Value,
};
use crate::source::Code;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug)]
pub struct VM {
    state: Rc<VMState>,
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

        let code = Code::from(src.into());
        let insts = Rc::from(compile(
            code,
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
        match self.state.run_frame(Frame::new(
            Rc::clone(&self.state.insts),
            Rc::new(Scope::new(Some(Rc::clone(&self.state.root_scope)))),
            0,
        )) {
            FrameStatus::Excepted(err) => Err(err),
            _ => Ok(()),
        }
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
        let insts: Rc<[Inst]> = Rc::from(compile(
            Code::from(
                std::fs::read_to_string(&path).expect("unable to import"),
            ),
            self.root_scope.names(),
        )?);
        let import_scope =
            Rc::new(Scope::new(Some(Rc::clone(&self.root_scope))));
        match self.run_frame(Frame::new(insts, import_scope.clone(), 0)) {
            FrameStatus::Excepted(err) => return Err(err),
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
    pub(crate) fn run_frame(self: &Rc<Self>, frame: Frame) -> FrameStatus {
        let Frame {
            insts,
            scope,
            start_ip,
            catch,
            finally,
        } = frame;
        let ret_sp = self.stack.borrow().len();
        let mut ip: usize = start_ip;
        let mut result = (|| -> FrameStatus {
            'frame: loop {
                if ip >= insts.len() {
                    return FrameStatus::Ended;
                }
                match insts[ip].clone() {
                    Inst::Illegal => panic!(Panic::IllegalInstruction),
                    Inst::SysImport(name) => {
                        self.push_stack(self.import(name)?);
                    }
                    Inst::SysTypeof => {
                        self.push_stack(Value::from(
                            self.pop_stack().type_of(),
                        ));
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
                            return FrameStatus::Excepted(
                                RuntimeError::InvalidOperation(format!(
                                    "can't destructure {:?} as an array",
                                    source
                                ))
                                .into(),
                            );
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
                            return FrameStatus::Excepted(
                                RuntimeError::InvalidOperation(format!(
                                    "can't destructure {:?} as an object",
                                    source
                                ))
                                .into(),
                            );
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
                                return FrameStatus::Excepted(
                                    RuntimeError::InvalidOperation(format!(
                                        "can't iterate over {:?}",
                                        iter
                                    ))
                                    .into(),
                                )
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
                        return FrameStatus::Returned(self.pop_stack());
                    }
                    Inst::ControlYield => {
                        return FrameStatus::Yielded(self.pop_stack());
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
                        return FrameStatus::Excepted(
                            RuntimeError::Generic(self.pop_stack().to_string())
                                .into(),
                        );
                    }
                    Inst::ControlBreak => {
                        return FrameStatus::Broke;
                    }
                    Inst::RunFrame { insts } => {
                        match self.run_frame(Frame::new(
                            insts,
                            Rc::new(Scope::extend(Rc::clone(&scope))),
                            0,
                        ))? {
                            FrameStatus::Returned(v) => {
                                return FrameStatus::Returned(v)
                            }
                            FrameStatus::Yielded(v) => {
                                return FrameStatus::Yielded(v)
                            }
                            FrameStatus::Broke => return FrameStatus::Broke,
                            _ => {}
                        }
                    }
                    Inst::RunLoopFrame { insts, on_break } => {
                        match self.run_frame(Frame::new(
                            insts,
                            Rc::new(Scope::extend(Rc::clone(&scope))),
                            0,
                        ))? {
                            FrameStatus::Returned(v) => {
                                return FrameStatus::Returned(v)
                            }
                            FrameStatus::Yielded(v) => {
                                return FrameStatus::Yielded(v)
                            }
                            FrameStatus::Broke => {
                                ip = on_break;
                                continue 'frame;
                            }
                            _ => {}
                        }
                    }
                    Inst::RunIterFrame { insts, on_break } => {
                        match self.run_frame(Frame::new(
                            insts,
                            Rc::new(Scope::extend(Rc::clone(&scope))),
                            0,
                        ))? {
                            FrameStatus::Returned(v) => {
                                return FrameStatus::Returned(v)
                            }
                            FrameStatus::Yielded(v) => {
                                return FrameStatus::Yielded(v)
                            }
                            FrameStatus::Broke => {
                                self.pop_stack(); // iterator is still on stack, remove it
                                ip = on_break;
                                continue 'frame;
                            }
                            FrameStatus::Ended => {
                                ip = on_break;
                                continue 'frame;
                            }
                            _ => {}
                        }
                    }
                    Inst::RunTryFrame {
                        insts,
                        catch,
                        finally,
                    } => {
                        match self.run_frame(Frame::new_with_catch(
                            insts,
                            Rc::new(Scope::extend(Rc::clone(&scope))),
                            0,
                            catch,
                            finally,
                        ))? {
                            FrameStatus::Returned(v) => {
                                return FrameStatus::Returned(v)
                            }
                            FrameStatus::Yielded(v) => {
                                return FrameStatus::Yielded(v)
                            }
                            FrameStatus::Broke => return FrameStatus::Broke,
                            _ => {}
                        }
                    }
                }
                ip += 1;
            }
        })();
        self.truncate_stack(ret_sp);
        if catch.is_some() {
            if let FrameStatus::Excepted(err) = result {
                let CatchContext(name, insts) = catch.unwrap();
                let scope = Rc::new(Scope::extend(Rc::clone(&scope)));
                scope.declare(name, Value::from(format!("{}", err)));
                result = self.run_frame(Frame::new(insts, scope, 0));
            }
        }
        if finally.is_some() {
            let FinallyContext(insts) = finally.unwrap();
            match self.run_frame(Frame::new(
                insts,
                Rc::new(Scope::extend(Rc::clone(&scope))),
                0,
            )) {
                FrameStatus::Returned(v) => return FrameStatus::Returned(v),
                FrameStatus::Yielded(v) => return FrameStatus::Yielded(v),
                FrameStatus::Broke => return FrameStatus::Broke,
                FrameStatus::Excepted(err) => {
                    return FrameStatus::Excepted(err)
                }
                FrameStatus::Ended => result,
            }
        } else {
            result
        }
    }
}
