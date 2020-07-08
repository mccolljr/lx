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
        value::{
            NativeFunc,
            Value,
        },
        vm::VM,
    };

    let vm = VM::new(
        compile(
            "
            fn counter() {
                let this = {
                    count: 0,
                    incr: fn() {
                        this.count = this.count+1;
                    },
                    decr: fn() {
                        this.count = this.count-1;
                    }
                };
                return this;
            }

            let x = counter();
            let i = x.incr;
            let d = x.decr;
            i();
            print(x.count);
            i();
            print(x.count);
            i();
            print(x.count);
            d();
            print(x.count);
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
                print!("{}", v.to_string());
            }
            print!("\n");
            Ok(Value::Null)
        }),
    )
    .with_global(
        "do",
        NativeFunc::new("do", |args| {
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
        Ok(_) => println!("OK!\n\n{:?}", vm),
        Err(e) => println!("ERROR! {:?}\n\n{:?}", e, vm),
    }
}
