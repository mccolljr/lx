use crate::source::{
    Code,
    Pos,
};

quick_error! {
    #[derive(Debug, Clone)]
    pub enum SyntaxError {
        InvalidCharacter{code: Code, at: Pos, ch: char} {
            display("at {}: invalid character '{}'", code.describe(*at), ch)
        }
        UnexpectedCharacter{code: Code, at: Pos, ch: char, context: &'static str} {
            display("at {}: unexpected character '{}' in {}", code.describe(*at), ch, context)
        }
        UnterminatedStringLiteral{code: Code, at: Pos} {
            display("unterminated string literal starting at {}", code.describe(*at))
        }
        InvalidEscapeSequence{code: Code, at: Pos, ch: char} {
            display("invalid escape sequence '\\{}' at {}", ch, code.describe(*at))
        }
        Expected{code: Code, at: Pos, wanted: String, found: String} {
            display("expected {}, but found {} at {}", wanted, found, code.describe(*at))
        }
        InvalidAssignment{code: Code, at: Pos, found: String} {
            display("unable to assign to {} at {}", found, code.describe(*at))
        }
        Redeclaration{code: Code, at: Pos, original: Pos, name: String} {
            display("redeclaration of '{}' at {} (original declaration is at {})",
                    name, code.describe(*at), code.describe(*original))
        }
        Undeclared{code: Code, at: Pos, name: String} {
            display("use of undeclared variable '{}' at {}", name, code.describe(*at))
        }
        NotAllowed{code: Code, at: Pos, what: String} {
            display("{} at {}", what, code.describe(*at))
        }
        UnterminatedBlockComment{code: Code, at: Pos} {
            display("unterminated block comment at {}", code.describe(*at))
        }
    }
}

quick_error! {
    #[derive(Debug, Clone)]
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
    #[derive(Debug, Clone)]
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
        IllegalFuncFrameStatus {
            display("PANIC: ILLEGAL FUNC FRAME STATUS")
        }
    }
}

quick_error! {
    #[derive(Debug, Clone)]
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
    }
}
