#![feature(
    unsize,
    coerce_unsized,
    try_trait,
    option_expect_none,
    map_first_last
)]

extern crate bincode;
extern crate itertools;
extern crate path_clean;
extern crate pathdiff;
extern crate quick_error;
extern crate regex;
extern crate serde;

mod ast;
mod check;
mod compiler;
mod context;
mod error;
mod lexer;
mod mem;
mod parser;
mod runtime;
mod source;
mod token;
mod typing;

#[cfg(test)]
mod tests;

#[cfg(test)]
extern crate insta;

fn main() {
    use runtime::vm::VM;

    let args: Vec<String> = std::env::args().collect();
    let result = VM::execute(args[1].as_ref(), None);
    match result {
        Ok(_) => println!("OK!"),
        Err(e) => {
            println!("ERROR! {}", e);
            std::process::exit(100);
        }
    }
}
