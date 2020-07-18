use super::array::Array;
use super::funcs::{
    Func,
    NativeFunc,
};
use super::iter::Iter;
use super::object::Object;

use crate::error::{
    Error,
    RuntimeError,
};
use crate::token::TokenType;

use std::convert::TryFrom;

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
    Iter(Iter),
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

impl From<&[Value]> for Value {
    fn from(src: &[Value]) -> Self { Value::Array(Array::from(src)) }
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

impl From<Iter> for Value {
    fn from(src: Iter) -> Value { Value::Iter(src) }
}

impl TryFrom<Value> for i64 {
    type Error = Error;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Int(v) => Ok(v),
            Value::Flt(v) => Ok(v as i64),
            _ => {
                Err(RuntimeError::InvalidType(format!(
                    "cannot convert {:?} to an integer",
                    value
                ))
                .into())
            }
        }
    }
}

impl TryFrom<Value> for f64 {
    type Error = Error;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Int(v) => Ok(v as f64),
            Value::Flt(v) => Ok(v),
            _ => {
                Err(RuntimeError::InvalidType(format!(
                    "cannot convert {:?} to a float",
                    value
                ))
                .into())
            }
        }
    }
}

impl TryFrom<Value> for bool {
    type Error = Error;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Bool(v) => Ok(v),
            _ => {
                Err(RuntimeError::InvalidType(format!(
                    "cannot convert {:?} to a bool",
                    value
                ))
                .into())
            }
        }
    }
}

impl TryFrom<Value> for String {
    type Error = Error;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Str(v) => Ok(v),
            _ => {
                Err(RuntimeError::InvalidType(format!(
                    "cannot convert {:?} to a string",
                    value
                ))
                .into())
            }
        }
    }
}

impl Value {
    pub fn op_index(&self, index: &Value) -> Result<Value, Error> {
        use Value::*;
        match (self, index) {
            (Array(arr), Int(i)) => Ok(arr.index_get(*i as usize)),
            (Array(arr), Flt(f)) => Ok(arr.index_get(*f as i64 as usize)),
            (Object(obj), key) => Ok(obj.index_get(&key.to_string())),
            _ => {
                Err(RuntimeError::InvalidOperation(format!(
                    "can't index {:?} with {:?}",
                    self, index
                ))
                .into())
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
                Err(RuntimeError::InvalidOperation(format!(
                    "can't index {:?} with {:?}",
                    self, index
                ))
                .into())
            }
        }
    }

    pub fn op_add(&self, rhs: &Value) -> Result<Value, Error> {
        use Value::*;
        match (self, rhs) {
            (Int(left), Int(right)) => {
                left.checked_add(*right).map_or(
                    Err(RuntimeError::InvalidOperation(format!(
                        "{} + {} overflows integer",
                        left, right
                    ))
                    .into()),
                    |result| Ok(Int(result)),
                )
            }
            (Flt(left), Flt(right)) => Ok(Flt(left + right)),
            (Int(left), Flt(right)) => Ok(Flt(*left as f64 + right)),
            (Flt(left), Int(right)) => Ok(Flt(left + *right as f64)),
            (Str(left), Str(right)) => Ok(Str(left.clone() + right.as_ref())),
            (Array(left), Array(right)) => Ok(Value::Array(left.concat(right))),
            (Object(left), right) if left.has_method(&"__add__".into()) => {
                left.index_get(&"__add__".into()).call(vec![right.clone()])
            }
            _ => {
                Err(RuntimeError::InvalidOperation(format!(
                    "can't add {:?} to {:?}",
                    rhs, self
                ))
                .into())
            }
        }
    }

    pub fn op_sub(&self, rhs: &Value) -> Result<Value, Error> {
        use Value::*;
        match (self, rhs) {
            (Int(left), Int(right)) => {
                left.checked_sub(*right).map_or(
                    Err(RuntimeError::InvalidOperation(format!(
                        "{} - {} underflows integer",
                        left, right
                    ))
                    .into()),
                    |result| Ok(Int(result)),
                )
            }
            (Flt(left), Flt(right)) => Ok(Flt(left - right)),
            (Int(left), Flt(right)) => Ok(Flt(*left as f64 - right)),
            (Flt(left), Int(right)) => Ok(Flt(left - *right as f64)),
            (Object(left), right) if left.has_method(&"__sub__".into()) => {
                left.index_get(&"__sub__".into()).call(vec![right.clone()])
            }
            _ => {
                Err(RuntimeError::InvalidOperation(format!(
                    "can't subtract {:?} from {:?}",
                    rhs, self
                ))
                .into())
            }
        }
    }

    pub fn op_mul(&self, rhs: &Value) -> Result<Value, Error> {
        use Value::*;
        match (self, rhs) {
            (Int(left), Int(right)) => {
                left.checked_mul(*right).map_or(
                    Err(RuntimeError::InvalidOperation(format!(
                        "{} * {} overflows integer",
                        left, right
                    ))
                    .into()),
                    |result| Ok(Int(result)),
                )
            }
            (Flt(left), Flt(right)) => Ok(Flt(left * right)),
            (Int(left), Flt(right)) => Ok(Flt(*left as f64 * right)),
            (Flt(left), Int(right)) => Ok(Flt(left * *right as f64)),
            (Object(left), right) if left.has_method(&"__mul__".into()) => {
                left.index_get(&"__mul__".into()).call(vec![right.clone()])
            }
            _ => {
                Err(RuntimeError::InvalidOperation(format!(
                    "can't multiply {:?} by {:?}",
                    self, rhs
                ))
                .into())
            }
        }
    }

    pub fn op_div(&self, rhs: &Value) -> Result<Value, Error> {
        use Value::*;
        match (self, rhs) {
            (Int(left), Int(right)) => {
                left.checked_div(*right).map_or(
                    Err(RuntimeError::InvalidOperation(
                        if *right == 0 {
                            format!("division by 0: {} / {}", left, right)
                        } else {
                            format!(
                                "division results in integer overflow: {} / {}",
                                left, right
                            )
                        },
                    )
                    .into()),
                    |i| Ok(Int(i)),
                )
            }
            (Flt(left), Flt(right)) => Ok(Flt(left / right)),
            (Int(left), Flt(right)) => Ok(Flt(*left as f64 / right)),
            (Flt(left), Int(right)) => Ok(Flt(left / *right as f64)),
            (Object(left), right) if left.has_method(&"__div__".into()) => {
                left.index_get(&"__div__".into()).call(vec![right.clone()])
            }
            _ => {
                Err(RuntimeError::InvalidOperation(format!(
                    "can't divide {:?} by {:?}",
                    self, rhs
                ))
                .into())
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
            (Object(left), right) if left.has_method(&"__rem__".into()) => {
                left.index_get(&"__rem__".into()).call(vec![right.clone()])
            }
            _ => {
                Err(RuntimeError::InvalidOperation(format!(
                    "can't divide {:?} by {:?} for a remainder",
                    self, rhs
                ))
                .into())
            }
        }
    }

    pub fn op_eq(&self, rhs: &Value) -> Result<Value, Error> {
        use Value::*;
        match (self, rhs) {
            (Object(left), right) if left.has_method(&"__eq__".into()) => {
                left.index_get(&"__eq__".into()).call(vec![right.clone()])
            }
            _ => Ok(Bool(self == rhs)),
        }
    }

    pub fn op_neq(&self, rhs: &Value) -> Result<Value, Error> {
        use Value::*;
        match (self, rhs) {
            (Object(left), right) if left.has_method(&"__neq__".into()) => {
                left.index_get(&"__neq__".into()).call(vec![right.clone()])
            }
            _ => Ok(Bool(self != rhs)),
        }
    }

    pub fn op_gt(&self, rhs: &Value) -> Result<Value, Error> {
        use Value::*;
        match (self, rhs) {
            (Int(left), Int(right)) => Ok(Bool(left > right)),
            (Flt(left), Flt(right)) => Ok(Bool(left > right)),
            (Int(left), Flt(right)) => Ok(Bool(*left as f64 > *right)),
            (Flt(left), Int(right)) => Ok(Bool(*left > *right as f64)),
            (Object(left), right) if left.has_method(&"__gt__".into()) => {
                left.index_get(&"__gt__".into()).call(vec![right.clone()])
            }
            _ => {
                Err(RuntimeError::InvalidOperation(format!(
                    "can't compare {:?} to {:?} (>)",
                    self, rhs,
                ))
                .into())
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
            (Object(left), right) if left.has_method(&"__lt__".into()) => {
                left.index_get(&"__lt__".into()).call(vec![right.clone()])
            }
            _ => {
                Err(RuntimeError::InvalidOperation(format!(
                    "can't compare {:?} to {:?} (<)",
                    self, rhs,
                ))
                .into())
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
            (Object(left), right) if left.has_method(&"__geq__".into()) => {
                left.index_get(&"__geq__".into()).call(vec![right.clone()])
            }
            _ => {
                Err(RuntimeError::InvalidOperation(format!(
                    "can't compare {:?} to {:?} (>=)",
                    self, rhs,
                ))
                .into())
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
            (Object(left), right) if left.has_method(&"__leq__".into()) => {
                left.index_get(&"__leq__".into()).call(vec![right.clone()])
            }
            _ => {
                Err(RuntimeError::InvalidOperation(format!(
                    "can't compare {:?} to {:?} (<=)",
                    self, rhs,
                ))
                .into())
            }
        }
    }

    pub fn op_sub_unary(&self) -> Result<Value, Error> {
        use Value::*;
        match self {
            Int(v) => Ok(Int(-1 * v)),
            Flt(v) => Ok(Flt(-1.0 * v)),
            Object(o) if o.has_method(&"__sub_unary__".into()) => {
                o.index_get(&"__sub_unary__".into()).call(vec![])
            }
            _ => {
                Err(RuntimeError::InvalidOperation(format!(
                    "can't make {:?} negative",
                    self,
                ))
                .into())
            }
        }
    }

    pub fn op_not_unary(&self) -> Result<Value, Error> {
        use Value::*;
        Ok(Bool(!self.truthy()))
    }

    pub fn iter(&self) -> Result<Iter, Error> {
        use Value::*;
        match self {
            Object(v) => Ok(v.value_iter()),
            Array(v) => Ok(v.value_iter()),
            Iter(v) => Ok(v.clone()),
            _ => {
                Err(RuntimeError::InvalidOperation(format!(
                    "can't iterate over {:?}",
                    self,
                ))
                .into())
            }
        }
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
            Func(..) => true,
            NativeFunc(..) => true,
            Iter(..) => true,
        }
    }

    pub fn call(&self, args: Vec<Value>) -> Result<Value, Error> {
        match self {
            Value::Func(f) => f.call(args),
            Value::NativeFunc(f) => f.call(args),
            Value::Object(o) if o.has_method(&"__call__".into()) => {
                o.index_get(&"__call__".into()).call(args)
            }
            _ => {
                return Err(RuntimeError::InvalidOperation(format!(
                    "can't call {:?}",
                    self
                ))
                .into());
            }
        }
    }

    pub fn type_of(&self) -> String {
        use Value::*;
        (match self {
            Null => "null",
            Bool(..) => "bool",
            Int(..) => "int",
            Flt(..) => "float",
            Str(..) => "string",
            Array(..) => "array",
            Object(..) => "object",
            Func(..) | NativeFunc(..) => "func",
            Iter(..) => "iter",
        })
        .into()
    }

    pub fn to_string(&self) -> String {
        use Value::*;
        match self {
            Null => "null".into(),
            Bool(v) => format!("{}", v),
            Int(v) => format!("{}", v),
            Flt(v) => format!("{}", v),
            Str(v) => v.clone(),
            Array(..) => "<array>".into(),
            Object(..) => "<object>".into(),
            Func(..) => "<func>".into(),
            NativeFunc(..) => "<native func>".into(),
            Iter(..) => "<iter>".into(),
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
