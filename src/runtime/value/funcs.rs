use super::value::Value;

use crate::error::{
    Error,
    Panic,
    RuntimeError,
};
use crate::runtime::inst::Inst;
use crate::runtime::scope::Scope;
use crate::runtime::vm::{
    FrameStatus,
    VMState,
};

use std::cell::Cell;
use std::fmt::{
    Debug,
    Formatter,
    Result as FmtResult,
};
use std::rc::Rc;

#[derive(Clone)]
pub struct Func {
    vm:      Rc<VMState>,
    name:    Rc<str>,
    args:    Rc<[String]>,
    insts:   Rc<[Inst]>,
    closure: Option<Rc<Scope>>,
}

impl Debug for Func {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f.debug_struct("Func")
            .field("vm", &(self.vm.as_ref() as *const VMState))
            .field("name", &self.name)
            .field("args", &self.args)
            .field("insts", &(self.insts.as_ref() as *const [Inst]))
            .field(
                "closure",
                &self
                    .closure
                    .clone()
                    .map_or(0 as *const Scope, |c| c.as_ref() as *const Scope),
            )
            .finish()
    }
}

impl PartialEq<Func> for Func {
    fn eq(&self, other: &Func) -> bool {
        return std::ptr::eq(self.vm.as_ref(), other.vm.as_ref())
            && std::ptr::eq(self.args.as_ref(), other.args.as_ref())
            && std::ptr::eq(self.insts.as_ref(), other.insts.as_ref())
            && match (&self.closure, &other.closure) {
                (Some(sc), Some(oc)) => std::ptr::eq(sc.as_ref(), oc.as_ref()),
                (None, None) => true,
                _ => false,
            }
            && self.name == other.name;
    }
}

impl Eq for Func {}

impl Func {
    pub fn new(
        name: impl AsRef<str>,
        vm: Rc<VMState>,
        args: Rc<[String]>,
        insts: Rc<[Inst]>,
        closure: Option<Rc<Scope>>,
    ) -> Self {
        Func {
            name: Rc::from(name.as_ref()),
            vm,
            args,
            insts,
            closure,
        }
    }

    pub fn call(&self, args: Vec<Value>) -> Result<Value, Error> {
        if args.len() != self.args.len() {
            return Err(RuntimeError::InvalidArguments(format!(
                "wrong number of arguments: expected {}, got {}",
                self.args.len(),
                args.len()
            ))
            .into());
        }
        let scope = match &self.closure {
            Some(c) => Rc::new(Scope::extend(Rc::clone(&c))),
            None => Rc::new(Scope::new(None)),
        };
        for (i, arg) in args.into_iter().enumerate() {
            scope.declare(self.args[i].clone(), arg);
        }
        match self.vm.run_frame(Rc::clone(&self.insts), scope, 0)? {
            FrameStatus::Returned => self.vm.pop_stack(),
            _ => Err(Panic::IllegalInstruction.into()),
        }
    }
}

#[derive(Clone)]
pub struct NativeFunc {
    name:  Rc<str>,
    fnptr: fn(Vec<Value>) -> Result<Value, Error>,
}

impl Debug for NativeFunc {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f.debug_struct("NativeFunc")
            .field("name", &self.name)
            .field("fnptr", &self.fnptr)
            .finish()
    }
}

impl PartialEq<NativeFunc> for NativeFunc {
    fn eq(&self, other: &NativeFunc) -> bool {
        return self.fnptr == other.fnptr && self.name == other.name;
    }
}

impl Eq for NativeFunc {}

impl NativeFunc {
    pub fn new(
        name: impl AsRef<str>,
        fnptr: fn(Vec<Value>) -> Result<Value, Error>,
    ) -> Self {
        NativeFunc {
            name: Rc::from(name.as_ref()),
            fnptr,
        }
    }

    pub fn call(&self, args: Vec<Value>) -> Result<Value, Error> {
        (self.fnptr)(args)
    }
}
