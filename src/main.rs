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
            let x = 0;
            while true {
                x = x + 1;
                let y = 0;
                while true {
                    y = y + 1;
                    print(y);
                    if y >= x {
                        break;
                    }
                }
                print(x);
                if x >= 10 {
                    break;
                }
            }
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
        Ok(_) => println!("{:#?}\n\nOK!", vm),
        Err(e) => println!("{:#?}\n\nERROR! {:?}", vm, e),
    }
}
