use crate::source::Pos;

quick_error! {
    #[derive(Debug)]
    pub enum SyntaxError {
        InvalidCharacter{at: Pos, ch: char} {
            display("at {:?}: invalid character '{}'", at, ch)
        }
        UnexpectedCharacter{at: Pos, ch: char, context: &'static str} {
            display("at {:?}: unexpected character '{}' in {}", at, ch, context)
        }
        UnterminatedStringLiteral{at: Pos} {
            display("unterminated string literal at {:?}", at)
        }
        InvalidEscapeSequence{at: Pos, ch: char} {
            display("invalid escape sequence '\\{}' at {:?}", ch, at)
        }
        Expected{at: Pos, wanted: String, found: String} {
            display("expected {}, but found {} at {:?}", wanted, found, at)
        }
        InvalidAssignment{at: Pos, found: String} {
            display("unable to assign to {} at {:?}", found, at)
        }
        Redeclaration{at: Pos, original: Pos, name: String} {
            display("redeclaration of '{}' at {:?} (original declaration is at {:?})", name, at, original)
        }
        Undeclared{at: Pos, name: String} {
            display("use of undeclared variable '{}' at {:?}", name, at)
        }
        NotAllowed{at: Pos, what: String} {
            display("{} at {:?}", what, at)
        }
    }
}

quick_error! {
    #[derive(Debug)]
    pub enum RuntimeError {
        InvalidOperation(reason: String) {
            display("invalid operation: {}", reason)
        }
        InvalidArguments(reason: String) {
            display("invalid arguments: {}", reason)
        }
        InvalidType(reason: String) {
            display("invalid type: {}", reason)
        }
        Generic(reason: String) {
            display("runtime error: {}", reason)
        }
    }
}

quick_error! {
    #[derive(Debug)]
    pub enum Panic {
        StackUnderflow {
            display("PANIC: STACK UNDERFLOW")
        }
        StackOverflow {
            display("PANIC: STACK OVERFLOW")
        }
        MalformedStack(context: &'static str) {
            display("PANIC: MALFORMED STACK: {}", context)
        }
        IllegalInstruction {
            display("PANIC: ILLEGAL INSTRUCTION")
        }
    }
}

quick_error! {
    #[derive(Debug)]
    pub enum Error {
        Syntax(err: SyntaxError) {
            from(src: SyntaxError) -> (src)
            cause(err)
            display("{}", err)
        }
        Runtime(err: RuntimeError) {
            from(src: RuntimeError) -> (src)
            cause(err)
            display("{}", err)
        }
        Panic(err: Panic) {
            from(src: Panic) -> (src)
            cause(err)
            display("{}", err)
        }
    }
}
