mod ast;
mod compiler;
mod errors;
mod lexer;
mod parser;
mod runtime;
mod source;
mod token;

fn main() {
    use compiler::compile;
    use runtime::{
        error::Error,
        value::Value,
        vm::VM,
    };

    let vm = VM::new(
        compile(
            "
            let fib = (fn() {
                let cache = { '0': 0, '1': 1 };
                let _fib = fn(n) {
                    if n % 1 != 0 {
                        throw 'can only call fib with integer';
                    }
                    let result = cache[n];
                    if result == null {
                        result = _fib(n-2) + _fib(n-1);
                        cache[n] = result;
                    }
                    return result;
                };
                return _fib;
            })();
            print(do(fn() { return 10; }, fib, fib));
            ",
        )
        .expect("compilation error"),
    )
    .with_global("print", Value::NativeFunc {
        name:  "print".into(),
        fnptr: |args| {
            for (i, v) in args.iter().enumerate() {
                if i > 0 {
                    print!(" ");
                }
                print!("{}", v.to_string());
            }
            print!("\n");
            Ok(Value::Null)
        },
    })
    .with_global("do", Value::NativeFunc {
        name:  "do".into(),
        fnptr: |args| {
            let mut prev: Option<Value> = None;
            for arg in args {
                if let Some(v) = prev {
                    prev = Some(arg.call(vec![v])?);
                } else {
                    prev = Some(arg.call(vec![])?);
                }
            }
            Ok(prev.map_or(Value::Null, |v| v))
        },
    });
    let result = vm.run();
    match result {
        Ok(_) => println!("OK!\n\n{:?}", vm),
        Err(e) => println!("ERROR! {:?}\n\n{:?}", e, vm),
    }
}
