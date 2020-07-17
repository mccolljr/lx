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

fn main() {
    use runtime::vm::VM;
    let args: Vec<String> = std::env::args().collect();

    let script = String::from_utf8(
        std::fs::read(&args[1]).expect("unable to read script file"),
    )
    .expect("file not in utf8");

    let result = VM::eval(script, None);
    match result {
        Ok(_) => println!("OK!"),
        Err(e) => println!("ERROR! {}", e),
    }
}
