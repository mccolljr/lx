#![feature(unsize, coerce_unsized, try_trait, option_expect_none)]

#[macro_use]
extern crate quick_error;
extern crate regex;

mod ast;
mod compiler;
mod error;
mod lexer;
mod mem;
mod parser;
mod runtime;
mod source;
mod token;

#[cfg(test)]
mod tests;

#[cfg(test)]
#[macro_use]
extern crate insta;

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
        Err(e) => {
            println!("ERROR! {}", e);
            std::process::exit(100);
        }
    }
}
