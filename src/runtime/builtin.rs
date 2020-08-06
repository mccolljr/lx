use regex::{
    Captures,
    Regex,
};

use crate::ast::{
    Type,
    TypeAnnotation,
};
use crate::error::{
    Error,
    RuntimeError,
};
use crate::runtime::value::{
    Iter,
    NativeFunc,
    Value,
};

use std::collections::HashMap;
use std::convert::TryFrom;

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

fn printf(args: Vec<Value>) -> Result<Value, Error> {
    println!("{}", format(args)?.to_string());
    Ok(Value::Null)
}

fn range(args: Vec<Value>) -> Result<Value, Error> {
    assert_args!(args => exactly 2);
    let start = i64::try_from(args[0].clone())?;
    let end = i64::try_from(args[1].clone())?;
    if end < start {
        let mut i = end;
        return Ok(Value::from(Iter::new(move || {
            if i < start {
                return Ok(None);
            }
            let next_val = Ok(Some(Value::from(i)));
            i -= 1;
            next_val
        })));
    }

    let mut i = start;
    return Ok(Value::from(Iter::new(move || {
        if i > end {
            return Ok(None);
        }
        let next_val = Ok(Some(Value::from(i)));
        i += 1;
        next_val
    })));
}

fn len(mut args: Vec<Value>) -> Result<Value, Error> {
    assert_args!(args => exactly 1);
    match args.pop().unwrap() {
        Value::Array(arr) => Ok(Value::from(arr.len() as i64)),
        Value::Object(obj) => Ok(Value::from(obj.len() as i64)),
        unexpected => {
            Err(RuntimeError::InvalidArguments(format!(
                "expected object or array, but found {}",
                unexpected.type_of()
            ))
            .into())
        }
    }
}

fn format(mut args: Vec<Value>) -> Result<Value, Error> {
    assert_args!(args => at least 1);
    let mut rest = args.drain(1..).collect::<Vec<Value>>();
    let first = args.pop().unwrap();
    if let Value::Str(s) = first {
        rest.reverse();
        return Ok(Value::from(
            Regex::new(r"\{\??\}")
                .unwrap()
                .replace_all(s.as_ref(), |c: &Captures| {
                    rest.pop().map_or("".into(), |v| {
                        if c.get(0).unwrap().as_str().contains("?") {
                            format!("{:?}", v)
                        } else {
                            v.to_string()
                        }
                    })
                })
                .as_ref(),
        ));
    }
    Err(RuntimeError::InvalidArguments(format!(
        "expected string, but found {}",
        first.type_of()
    ))
    .into())
}

fn generate(mut args: Vec<Value>) -> Result<Value, Error> {
    assert_args!(args => exactly 1);
    let f = args.pop().unwrap();
    Ok(Value::Iter(Iter::new(move || {
        match f.call(vec![])? {
            Value::Null => Ok(None),
            val => Ok(Some(val)),
        }
    })))
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
    declare_builtin!(h, print);
    declare_builtin!(h, printf);
    declare_builtin!(h, range);
    declare_builtin!(h, len);
    declare_builtin!(h, format);
    declare_builtin!(h, generate);
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
