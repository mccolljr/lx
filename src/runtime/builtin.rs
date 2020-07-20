use crate::error::{
    Error,
    RuntimeError,
};
use crate::runtime::value::{
    Array,
    Iter,
    NativeFunc,
    Value,
};

use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::rc::Rc;

macro_rules! assert_args {
    ($args:expr => exactly $exact:expr) => {{
        let argct = $args.len();
        if argct != $exact {
            return Err(RuntimeError::InvalidArguments(format!(
                "expected {} argument(s), but found {}",
                $exact, argct
            ))
            .into());
        }
    }};
    ($args:expr => at least $minimum:expr) => {{
        let argct = $args.len();
        if argct < $minimum {
            return Err(RuntimeError::InvalidArguments(format!(
                "expected at least {} argument(s), but found {}",
                $minimum, argct
            ))
            .into());
        }
    }};
}

fn type_of(args: Vec<Value>) -> Result<Value, Error> {
    assert_args!(args => exactly 1);
    Ok(Value::from(args[0].type_of()))
}

fn print(args: Vec<Value>) -> Result<Value, Error> {
    for (i, arg) in args.into_iter().enumerate() {
        if i > 0 {
            print!(" ");
        }
        print!("{}", arg.to_string());
    }
    println!("");
    Ok(Value::Null)
}

fn debug(args: Vec<Value>) -> Result<Value, Error> {
    for (i, arg) in args.into_iter().enumerate() {
        if i > 0 {
            print!(" ")
        }
        print!("{:?}", arg);
    }
    println!("");
    Ok(Value::Null)
}

fn range(args: Vec<Value>) -> Result<Value, Error> {
    assert_args!(args => exactly 2);
    let start = i64::try_from(args[0].clone())?;
    let end = i64::try_from(args[1].clone())?;
    if end < start {
        let mut i = end;
        return Ok(Value::from(Iter::new(Rc::new(RefCell::new(move || {
            if i < start {
                return None;
            }
            let next_val = Some(Value::from(i));
            i -= 1;
            next_val
        })))));
    }

    let mut i = start;
    return Ok(Value::from(Iter::new(Rc::new(RefCell::new(move || {
        if i > end {
            return None;
        }
        let next_val = Some(Value::from(i));
        i += 1;
        next_val
    })))));
}

fn len(args: Vec<Value>) -> Result<Value, Error> {
    assert_args!(args => exactly 1);
    match &args[0] {
        Value::Array(arr) => Ok(Value::from(arr.len() as i64)),
        Value::Object(obj) => Ok(Value::from(obj.len() as i64)),
        unexpected => {
            Err(RuntimeError::InvalidArguments(format!(
                "expected Object or Array, but found {:?}",
                unexpected
            ))
            .into())
        }
    }
}

fn filter(args: Vec<Value>) -> Result<Value, Error> {
    assert_args!(args => exactly 2);
    let iter = args[0].clone();
    let f = args[1].clone();
    let result = Array::new();
    match iter {
        Value::Array(arr) => {
            for v in arr.value_iter() {
                if f.call(vec![v.clone()])?.truthy() {
                    result.push_back(v);
                }
            }
            Ok(Value::from(result))
        }
        unexpected => {
            Err(RuntimeError::InvalidArguments(format!(
                "expected Array, but found {:?}",
                unexpected
            ))
            .into())
        }
    }
}

fn map(args: Vec<Value>) -> Result<Value, Error> {
    assert_args!(args => exactly 2);
    let iter = args[0].clone();
    let f = args[1].clone();
    let result = Array::new();
    match iter {
        Value::Array(arr) => {
            for v in arr.value_iter() {
                result.push_back(f.call(vec![v.clone()])?);
            }
            Ok(Value::from(result))
        }
        unexpected => {
            Err(RuntimeError::InvalidArguments(format!(
                "expected Array, but found {:?}",
                unexpected
            ))
            .into())
        }
    }
}

macro_rules! declare_builtin {
    ($map:expr, $name:ident) => {
        $map.insert(
            stringify!($name).into(),
            NativeFunc::new(stringify!($name), $name).into(),
        );
    };
}

pub fn builtins() -> HashMap<String, Value> {
    let mut h: HashMap<String, Value> = HashMap::new();
    declare_builtin!(h, type_of);
    declare_builtin!(h, print);
    declare_builtin!(h, debug);
    declare_builtin!(h, range);
    declare_builtin!(h, len);
    declare_builtin!(h, filter);
    declare_builtin!(h, map);
    h
}

pub fn extend_builtins(
    additional: HashMap<String, Value>,
) -> HashMap<String, Value> {
    let mut h = builtins();
    for (k, v) in additional {
        h.insert(k, v);
    }
    h
}
