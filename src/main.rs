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
            fn make_arr() {
                return [1,2,3];
            }
            let [ a, b, c ] = make_arr();
            print(a,b,c);
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
