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
    use runtime::error::Error;
    use runtime::value::Value;
    use runtime::vm::VM;

    let vm = VM::new(
        compile(
            "
            let fib = (fn () {
                let cache = {'0': 0, '1': 1};
                let _fib = fn(n) {
                    if n < 0 {
                        throw 'n cannot be negative';
                    }
                    if n % 1 != 0 {
                        throw 'n must be an integer';
                    }
                    let result = cache[n];
                    if result == null {
                        result = _fib(n-2) + _fib(n-1);
                        cache[n] = result;
                    }
                    return result;
                };
                return _fib;
            }());
            print('fib(90):', fib(90));
            print('fib(45):', fib(45));
            print('abs(-1):', abs(-1));
            ",
        )
        .unwrap(),
    )
    .with_global(
        "print",
        Value::NativeFunc {
            name: "print".into(),
            f: |args| {
                for (i, v) in args.iter().enumerate() {
                    if i > 0 {
                        print!(" ");
                    }
                    print!("{}", v.to_string());
                }
                print!("\n");
                Ok(Value::Null)
            },
        },
    )
    .with_global(
        "abs",
        Value::NativeFunc {
            name: "abs".into(),
            f: |args| {
                if args.len() != 1 {
                    return Err(Error::RuntimeError(format!(
                        "wrong number of arguments: expected 1, got {}",
                        args.len()
                    )));
                }
                let arg = &args[0];
                match arg {
                    Value::Int(v) => Ok(Value::Int(v.abs())),
                    _ => Err(Error::RuntimeError(format!("argument must be an integer"))),
                }
            },
        },
    );
    let result = vm.run();
    match result {
        Ok(_) => println!("OK!"),
        Err(e) => println!("ERROR! {:?}\n\n{:?}", e, vm),
    }
}
