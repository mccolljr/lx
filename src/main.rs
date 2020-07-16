#[macro_use]
extern crate quick_error;

mod ast;
mod ast_rewrite;
mod compiler;
mod error;
mod lexer;
mod parser;
mod runtime;
mod source;
mod token;

macro_rules! globals {
    ($($key:ident => $val:expr),*) => {
        {
            let mut m = std::collections::HashMap::<String, crate::runtime::value::Value>::new();
            $(
                m.insert(String::from(stringify!($key)), Value::from($val));
            )*
            m
        }
    };
}

const CODE: &'static str = "
    let arr = [1,2,3,4];
    arr = map(
        map(arr, fn(v) { return v*2; }),
        fn(v) { return v*-1.33; }
    );
    debug(arr, len(arr));
";

fn main() {
    use error::RuntimeError;
    use runtime::value::{
        Array,
        NativeFunc,
        Value,
    };
    use runtime::vm::VM;

    let result = VM::eval(CODE, globals! {
        debug => NativeFunc::new("debug", |args| {
            for (i, v) in args.iter().enumerate() {
                if i > 0 {
                    print!(" ");
                }
                print!("{:?}", v);
            }
            print!("\n");
            Ok(Value::Null)
        }),
        print => NativeFunc::new("print", |args| {
            for (i, v) in args.iter().enumerate() {
                if i > 0 {
                    print!(" ");
                }
                print!("{}", v.to_string());
            }
            print!("\n");
            Ok(Value::Null)
        }),
        len => NativeFunc::new("len", |args| {
            if args.len() != 1 {
                return Err(RuntimeError::InvalidArguments(
                    format!("expected 1 argument, got {}", args.len())
                ).into());
            }
            let v = &args[0];
            match v {
                Value::Array(arr) => Ok(Value::from(arr.len() as i64)),
                Value::Object(obj) => Ok(Value::from(obj.len() as i64)),
                _ => Err(RuntimeError::InvalidArguments(
                    format!("expected object or array, got {}", v.to_string())
                ).into())
            }
        }),
        map => NativeFunc::new("map", |args| {
            if args.len() != 2 {
                return Err(RuntimeError::InvalidArguments(
                    format!("expected 2 arguments, got {}", args.len())
                ).into());
            }
            let iter = &args[0];
            let f = &args[1];
            match iter {
                Value::Array(arr) => {
                    let result = Array::new();
                    for (i, v) in arr.value_iter().enumerate() {
                        result.index_set(i,f.call(vec![v])?);
                    }
                    Ok(Value::from(result))
                },
                _ => Err(RuntimeError::InvalidArguments(
                    format!("expected array, got {}", iter.to_string())
                ).into())
            }
        })
    });
    match result {
        Ok(_) => println!("OK!"),
        Err(e) => println!("ERROR! {}", e),
    }
}
