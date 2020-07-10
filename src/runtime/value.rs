use super::{
    super::token::TokenType,
    error::Error,
    inst::Inst,
    scope::Scope,
    vm::VMState,
};
use std::{
    cell::RefCell,
    collections::{
        HashMap,
        VecDeque,
    },
    fmt::{
        Debug,
        Formatter,
        Result as FmtResult,
    },
    iter::FromIterator,
    rc::Rc,
};

#[derive(Clone)]
pub enum Value {
    Null,
    Int(i64),
    Flt(f64),
    Str(String),
    Bool(bool),
    Object(Object),
    Array(Array),
    Func(Func),
    NativeFunc(NativeFunc),
}

impl Debug for Value {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Self::Null => f.write_str("Null"),
            Self::Int(v) => f.debug_tuple("Int").field(v).finish(),
            Self::Flt(v) => f.debug_tuple("Flt").field(v).finish(),
            Self::Str(v) => f.debug_tuple("Str").field(v).finish(),
            Self::Bool(v) => f.debug_tuple("Bool").field(v).finish(),
            Self::Array(v) => write!(f, "{:?}", v),
            Self::Object(v) => write!(f, "{:?}", v),
            Self::Func(v) => write!(f, "{:?}", v),
            Self::NativeFunc(v) => write!(f, "{:?}", v),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Null, Value::Null) => true,
            (Value::Int(si), Value::Int(oi)) => si == oi,
            (Value::Flt(sf), Value::Flt(of)) => {
                if sf.is_finite() && of.is_finite() {
                    sf == of
                } else if sf.is_nan() && of.is_nan() {
                    true
                } else if sf.is_infinite() && of.is_infinite() {
                    sf.is_sign_negative() == of.is_sign_negative()
                } else {
                    false
                }
            }
            (Value::Str(ss), Value::Str(os)) => ss == os,
            (Value::Bool(sb), Value::Bool(ob)) => sb == ob,
            (Value::Object(so), Value::Object(oo)) => so == oo,
            (Value::Array(sa), Value::Array(oa)) => sa == oa,
            _ => false,
        }
    }
}

impl From<String> for Value {
    fn from(src: String) -> Self { Value::Str(src) }
}

impl From<&str> for Value {
    fn from(src: &str) -> Self { Value::Str(src.to_owned()) }
}

impl From<i64> for Value {
    fn from(src: i64) -> Self { Value::Int(src) }
}

impl From<f64> for Value {
    fn from(src: f64) -> Self { Value::Flt(src) }
}

impl From<bool> for Value {
    fn from(src: bool) -> Self { Value::Bool(src) }
}

impl From<VecDeque<Value>> for Value {
    fn from(src: VecDeque<Value>) -> Self {
        Value::Array(Array(Rc::new(RefCell::new(src))))
    }
}

impl From<HashMap<String, Value>> for Value {
    fn from(src: HashMap<String, Value>) -> Self {
        Value::Object(Object(Rc::new(RefCell::new(src))))
    }
}

impl From<Func> for Value {
    fn from(src: Func) -> Value { Value::Func(src) }
}

impl From<NativeFunc> for Value {
    fn from(src: NativeFunc) -> Value { Value::NativeFunc(src) }
}

impl Value {
    pub fn op_index(&self, index: &Value) -> Result<Value, Error> {
        use Value::*;
        match (self, index) {
            (Array(arr), Int(i)) => Ok(arr.index_get(*i as usize)),
            (Array(arr), Flt(f)) => Ok(arr.index_get(*f as i64 as usize)),
            (Object(obj), key) => Ok(obj.index_get(&key.to_string())),
            _ => {
                Err(Error::InvalidOperation(format!(
                    "can't index {:?} with {:?}",
                    self, index
                )))
            }
        }
    }

    pub fn op_index_set(
        &self,
        index: &Value,
        val: &Value,
    ) -> Result<(), Error> {
        use Value::*;
        match (self, index) {
            (Array(arr), Int(idx)) => {
                // TODO: sparse arrays or array set out of range
                arr.index_set(*idx as usize, val.clone());
                Ok(())
            }
            (Array(arr), Flt(idx)) => {
                // TODO: sparse arrays or array set out of range
                arr.index_set(*idx as i64 as usize, val.clone());
                Ok(())
            }
            (Object(obj), key) => {
                obj.index_set(key.to_string(), val.clone());
                Ok(())
            }
            _ => {
                Err(Error::InvalidOperation(format!(
                    "can't index {:?} with {:?}",
                    self, index
                )))
            }
        }
    }

    pub fn op_add(&self, rhs: &Value) -> Result<Value, Error> {
        use Value::*;
        match (self, rhs) {
            (Int(left), Int(right)) => {
                left.checked_add(*right).map_or(
                    Err(Error::InvalidOperation("integer overflow".into())),
                    |i| Ok(Int(i)),
                )
            }
            (Flt(left), Flt(right)) => Ok(Flt(left + right)),
            (Int(left), Flt(right)) => Ok(Flt(*left as f64 + right)),
            (Flt(left), Int(right)) => Ok(Flt(left + *right as f64)),
            (Str(left), Str(right)) => Ok(Str(left.clone() + right.as_ref())),
            (Array(left), Array(right)) => Ok(Value::Array(left.concat(right))),
            _ => {
                Err(Error::InvalidOperation(format!(
                    "can't add {:?} to {:?}",
                    self, rhs
                )))
            }
        }
    }

    pub fn op_sub(&self, rhs: &Value) -> Result<Value, Error> {
        use Value::*;
        match (self, rhs) {
            (Int(left), Int(right)) => {
                left.checked_sub(*right).map_or(
                    Err(Error::InvalidOperation("integer underflow".into())),
                    |i| Ok(Int(i)),
                )
            }
            (Flt(left), Flt(right)) => Ok(Flt(left - right)),
            (Int(left), Flt(right)) => Ok(Flt(*left as f64 - right)),
            (Flt(left), Int(right)) => Ok(Flt(left - *right as f64)),
            _ => {
                Err(Error::InvalidOperation(format!(
                    "can't subtract {:?} from {:?}",
                    self, rhs
                )))
            }
        }
    }

    pub fn op_mul(&self, rhs: &Value) -> Result<Value, Error> {
        use Value::*;
        match (self, rhs) {
            (Int(left), Int(right)) => {
                left.checked_mul(*right).map_or(
                    Err(Error::InvalidOperation("integer overflow".into())),
                    |i| Ok(Int(i)),
                )
            }
            (Flt(left), Flt(right)) => Ok(Flt(left * right)),
            (Int(left), Flt(right)) => Ok(Flt(*left as f64 * right)),
            (Flt(left), Int(right)) => Ok(Flt(left * *right as f64)),
            _ => {
                Err(Error::InvalidOperation(format!(
                    "can't multiply {:?} by {:?}",
                    self, rhs
                )))
            }
        }
    }

    pub fn op_div(&self, rhs: &Value) -> Result<Value, Error> {
        use Value::*;
        match (self, rhs) {
            (Int(left), Int(right)) => {
                left.checked_div(*right).map_or(
                    Err(Error::InvalidOperation(
                        "invalid integer division".into(),
                    )),
                    |i| Ok(Int(i)),
                )
            }
            (Flt(left), Flt(right)) => Ok(Flt(left / right)),
            (Int(left), Flt(right)) => Ok(Flt(*left as f64 / right)),
            (Flt(left), Int(right)) => Ok(Flt(left / *right as f64)),
            _ => {
                Err(Error::InvalidOperation(format!(
                    "can't divide {:?} by {:?}",
                    self, rhs
                )))
            }
        }
    }

    pub fn op_rem(&self, rhs: &Value) -> Result<Value, Error> {
        use Value::*;
        match (self, rhs) {
            (Int(left), Int(right)) => Ok(Int(left % right)),
            (Flt(left), Flt(right)) => Ok(Flt(left % right)),
            (Int(left), Flt(right)) => Ok(Flt(*left as f64 % right)),
            (Flt(left), Int(right)) => Ok(Flt(left % *right as f64)),
            _ => {
                Err(Error::InvalidOperation(format!(
                    "can't divide {:?} by {:?} for a remainder",
                    self, rhs
                )))
            }
        }
    }

    pub fn op_eq(&self, rhs: &Value) -> Result<Value, Error> {
        use Value::*;
        Ok(Bool(self == rhs))
    }

    pub fn op_neq(&self, rhs: &Value) -> Result<Value, Error> {
        use Value::*;
        Ok(Bool(self != rhs))
    }

    pub fn op_gt(&self, rhs: &Value) -> Result<Value, Error> {
        use Value::*;
        match (self, rhs) {
            (Int(left), Int(right)) => Ok(Bool(left > right)),
            (Flt(left), Flt(right)) => Ok(Bool(left > right)),
            (Int(left), Flt(right)) => Ok(Bool(*left as f64 > *right)),
            (Flt(left), Int(right)) => Ok(Bool(*left > *right as f64)),
            _ => {
                Err(Error::InvalidOperation(format!(
                    "can't compare {:?} to {:?} (>)",
                    self, rhs,
                )))
            }
        }
    }

    pub fn op_lt(&self, rhs: &Value) -> Result<Value, Error> {
        use Value::*;
        match (self, rhs) {
            (Int(left), Int(right)) => Ok(Bool(left < right)),
            (Flt(left), Flt(right)) => Ok(Bool(left < right)),
            (Int(left), Flt(right)) => Ok(Bool((*left as f64) < *right)),
            (Flt(left), Int(right)) => Ok(Bool(*left < *right as f64)),
            _ => {
                Err(Error::InvalidOperation(format!(
                    "can't compare {:?} to {:?} (<)",
                    self, rhs,
                )))
            }
        }
    }

    pub fn op_geq(&self, rhs: &Value) -> Result<Value, Error> {
        use Value::*;
        match (self, rhs) {
            (Int(left), Int(right)) => Ok(Bool(left >= right)),
            (Flt(left), Flt(right)) => Ok(Bool(left >= right)),
            (Int(left), Flt(right)) => Ok(Bool(*left as f64 >= *right)),
            (Flt(left), Int(right)) => Ok(Bool(*left >= *right as f64)),
            _ => {
                Err(Error::InvalidOperation(format!(
                    "can't compare {:?} to {:?} (>=)",
                    self, rhs,
                )))
            }
        }
    }

    pub fn op_leq(&self, rhs: &Value) -> Result<Value, Error> {
        use Value::*;
        match (self, rhs) {
            (Int(left), Int(right)) => Ok(Bool(left <= right)),
            (Flt(left), Flt(right)) => Ok(Bool(left <= right)),
            (Int(left), Flt(right)) => Ok(Bool((*left as f64) <= *right)),
            (Flt(left), Int(right)) => Ok(Bool(*left <= *right as f64)),
            _ => {
                Err(Error::InvalidOperation(format!(
                    "can't compare {:?} to {:?} (<=)",
                    self, rhs,
                )))
            }
        }
    }

    pub fn op_sub_unary(&self) -> Result<Value, Error> {
        use Value::*;
        match self {
            Int(v) => Ok(Int(-1 * v)),
            Flt(v) => Ok(Flt(-1.0 * v)),
            _ => {
                Err(Error::InvalidOperation(format!(
                    "can't make {:?} negative",
                    self,
                )))
            }
        }
    }

    pub fn op_not_unary(&self) -> Result<Value, Error> {
        use Value::*;
        Ok(Bool(!self.truthy()))
    }

    pub fn truthy(&self) -> bool {
        use Value::*;
        match self {
            Null => false,
            Bool(v) => *v,
            Int(v) => *v != 0,
            Flt(v) => *v != 0.0,
            Str(v) => v.len() != 0,
            Array(v) => v.len() != 0,
            Object(v) => v.len() != 0,
            Func { .. } => true,
            NativeFunc { .. } => true,
        }
    }

    pub fn call(&self, args: Vec<Value>) -> Result<Value, Error> {
        match self {
            Value::Func(f) => f.call(args),
            Value::NativeFunc(f) => f.call(args),
            _ => {
                return Err(Error::InvalidOperation(format!(
                    "can't call {:?}",
                    self
                )));
            }
        }
    }

    pub fn to_string(&self) -> String {
        use Value::*;
        match self {
            Null => "null".into(),
            Bool(v) => format!("{}", v),
            Int(v) => format!("{}", v),
            Flt(v) => format!("{}", v),
            Str(v) => v.clone(),
            Array(_) => "<array>".into(),
            Object(_) => "<object>".into(),
            Func { .. } => "<func>".into(),
            NativeFunc { .. } => "<native func>".into(),
        }
    }

    pub(crate) fn op_binary(
        lhs: &Value,
        rhs: &Value,
        op: TokenType,
    ) -> Result<Value, Error> {
        match op {
            TokenType::OpAdd => lhs.op_add(&rhs),
            TokenType::OpSub => lhs.op_sub(&rhs),
            TokenType::OpMul => lhs.op_mul(&rhs),
            TokenType::OpDiv => lhs.op_div(&rhs),
            TokenType::OpRem => lhs.op_rem(&rhs),
            TokenType::OpEq => lhs.op_eq(&rhs),
            TokenType::OpNeq => lhs.op_neq(&rhs),
            TokenType::OpGeq => lhs.op_geq(&rhs),
            TokenType::OpLeq => lhs.op_leq(&rhs),
            TokenType::OpGt => lhs.op_gt(&rhs),
            TokenType::OpLt => lhs.op_lt(&rhs),
            _ => unreachable!(),
        }
    }

    pub(crate) fn op_unary(
        target: &Value,
        op: TokenType,
    ) -> Result<Value, Error> {
        match op {
            TokenType::OpSub => target.op_sub_unary(),
            TokenType::Bang => target.op_not_unary(),
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct Object(Rc<RefCell<HashMap<String, Value>>>);

impl Debug for Object {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f.debug_tuple("Object").field(&self.0.borrow()).finish()
    }
}

impl Object {
    pub fn new() -> Self { Object(Rc::new(RefCell::new(HashMap::new()))) }

    pub fn len(&self) -> usize { self.0.borrow().len() }

    pub fn index_get(&self, index: &String) -> Value {
        self.0
            .borrow()
            .get(index)
            .map_or(Value::Null, |v| v.clone())
    }

    pub fn index_set(&self, index: String, val: Value) {
        self.0.borrow_mut().insert(index, val);
    }
}

impl IntoIterator for Object {
    type IntoIter = std::collections::hash_map::IntoIter<String, Value>;
    type Item = (String, Value);

    fn into_iter(self) -> Self::IntoIter {
        ((*self.0).clone()).into_inner().into_iter()
    }
}

#[derive(Clone, PartialEq)]
pub struct Array(Rc<RefCell<VecDeque<Value>>>);

impl Debug for Array {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f.debug_tuple("Array").field(&self.0.borrow()).finish()
    }
}

impl Array {
    pub fn new() -> Self { Array(Rc::new(RefCell::new(VecDeque::new()))) }

    pub fn len(&self) -> usize { self.0.borrow().len() }

    pub fn pop_front(&self) -> Value {
        self.0.borrow_mut().pop_front().map_or(Value::Null, |v| v)
    }

    // pub fn pop_back(&self) -> Value {
    //     self.0.borrow_mut().pop_back().map_or(Value::Null, |v| v)
    // }

    // pub fn push_front(&self, val: Value) {
    // self.0.borrow_mut().push_front(val) }

    pub fn push_back(&self, val: Value) { self.0.borrow_mut().push_back(val) }

    pub fn index_get(&self, index: usize) -> Value {
        self.0
            .borrow()
            .get(index)
            .map_or(Value::Null, |v| v.clone())
    }

    pub fn index_set(&self, index: usize, val: Value) {
        self.0.borrow_mut()[index] = val;
    }

    pub fn concat(&self, other: &Self) -> Self {
        return Array(Rc::new(RefCell::new(VecDeque::from_iter(
            self.0
                .borrow()
                .iter()
                .chain(other.0.borrow().iter())
                .map(|v| v.clone()),
        ))));
    }
}

impl IntoIterator for Array {
    type IntoIter = std::collections::vec_deque::IntoIter<Value>;
    type Item = Value;

    fn into_iter(self) -> Self::IntoIter {
        (*self.0).clone().into_inner().into_iter()
    }
}

#[derive(Clone)]
pub struct Func {
    vm:      Rc<VMState>,
    name:    String,
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
        name: impl Into<String>,
        vm: Rc<VMState>,
        args: Rc<[String]>,
        insts: Rc<[Inst]>,
        closure: Option<Rc<Scope>>,
    ) -> Self {
        Func {
            name: name.into(),
            vm,
            args,
            insts,
            closure,
        }
    }

    // pub fn name(&self) -> &str { self.name.as_ref() }

    pub fn call(&self, args: Vec<Value>) -> Result<Value, Error> {
        if args.len() != self.args.len() {
            return Err(Error::ArgumentError(format!(
                "wrong number of arguments: expected {}, got {}",
                self.args.len(),
                args.len()
            )));
        }
        let scope = match &self.closure {
            Some(c) => Rc::new(Scope::extend(Rc::clone(&c))),
            None => Rc::new(Scope::new(None)),
        };
        for (i, arg) in args.into_iter().enumerate() {
            scope.declare(self.args[i].clone(), arg);
        }
        self.vm.run_frame(Rc::clone(&self.insts), scope)?;
        self.vm.pop_stack()
    }
}

#[derive(Clone)]
pub struct NativeFunc {
    name:  String,
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
        name: impl Into<String>,
        fnptr: fn(Vec<Value>) -> Result<Value, Error>,
    ) -> Self {
        NativeFunc {
            name: name.into(),
            fnptr,
        }
    }

    // pub fn name(&self) -> &str { self.name.as_ref() }

    pub fn call(&self, args: Vec<Value>) -> Result<Value, Error> {
        (self.fnptr)(args)
    }
}
