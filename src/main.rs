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
            let a = 1 + 2 + 3 + 4 + 5 + 6 + 7;
            let b = 'a' + 'b';
            print(a, b);
            ",
        )
        .expect("compilation error"),
    )
    .with_global(
        "print",
        Value::NativeFunc {
            name: "print".into(),
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
        },
    )
    .with_global(
        "abs",
        Value::NativeFunc {
            name: "abs".into(),
            fnptr: |args| {
                if args.len() != 1 {
                    return Err(Error::ArgumentError(format!(
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
        Ok(_) => println!("OK! {:?}", vm),
        Err(e) => println!("ERROR! {:?}\n\n{:?}", e, vm),
    }
}
