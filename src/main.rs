mod ast;
mod ast_rewrite;
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
        value::{
            NativeFunc,
            Value,
        },
        vm::VM,
    };

    let vm = VM::new(
        compile(
            "
            let fib = fn() {
                let cache = {'0': 0, '1': 1};
                let _fib = fn (n) {
                    if n < 0 {
                        throw 'n cannot be negative';
                    }
                    if n % 1 != 0 {
                        throw 'n cannot be a float';
                    }
                    let result = cache[n];
                    if result == null {
                        result = _fib(n-1) + _fib(n-2);
                        cache[n] = result; 
                    }
                    return result;
                };
                return _fib;
            } ();
            print(fib(100));
            ",
        )
        .expect("compilation error"),
    )
    .with_global(
        "print",
        NativeFunc::new("print", |args| {
            for (i, v) in args.iter().enumerate() {
                if i > 0 {
                    print!(" ");
                }
                print!("{:?}", v);
            }
            print!("\n");
            Ok(Value::Null)
        }),
    )
    .with_global(
        "do",
        NativeFunc::new("do", |args: Vec<Value>| {
            let mut prev: Option<Value> = None;
            for arg in args {
                if let Some(v) = prev {
                    prev = Some(arg.call(vec![v])?);
                } else {
                    prev = Some(arg.call(vec![])?);
                }
            }
            Ok(prev.map_or(Value::Null, |v| v))
        }),
    );
    let result = vm.run();
    match result {
        Ok(_) => println!("OK!\n\n{:#?}", vm),
        Err(e) => println!("ERROR! {:?}\n\n{:#?}", e, vm),
    }
}
