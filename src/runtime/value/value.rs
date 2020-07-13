use super::{
    super::{
        super::token::TokenType,
        error::Error,
    },
    array::Array,
    funcs::{
        Func,
        NativeFunc,
    },
    object::Object,
};

#[derive(Clone, Debug)]
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

impl From<Array> for Value {
    fn from(src: Array) -> Self { Value::Array(src) }
}

impl From<Object> for Value {
    fn from(src: Object) -> Self { Value::Object(src) }
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