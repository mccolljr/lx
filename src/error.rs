use quick_error::quick_error;

use crate::source::{
    Code,
    Pos,
};
use crate::token::TokenType;
use crate::typing::Typing;

quick_error! {
    #[derive(Debug, Clone)]
    pub enum SyntaxError {
        InvalidCharacter{code: Code, at: Pos, ch: char} {
            display("{}: invalid character '{}'", code.describe(*at), ch)
        }
        UnexpectedCharacter{code: Code, at: Pos, ch: char, context: &'static str} {
            display("{}: unexpected character '{}' in {}", code.describe(*at), ch, context)
        }
        UnterminatedStringLiteral{code: Code, at: Pos} {
            display("{}: unterminated string literal starting", code.describe(*at))
        }
        InvalidEscapeSequence{code: Code, at: Pos, ch: char} {
            display("{}: invalid escape sequence '\\{}'", code.describe(*at), ch)
        }
        Expected{code: Code, at: Pos, wanted: String, found: String} {
            display("{}: expected {}, but found {}", code.describe(*at), wanted, found)
        }
        InvalidAssignment{code: Code, at: Pos, found: String} {
            display("unable to assign to {} at {}", found, code.describe(*at))
        }
        Redeclaration{code: Code, at: Pos, original: Pos, name: String} {
            display("{}: redeclaration of '{}' (original declaration is at {})",
                code.describe(*at), name, code.describe(*original))
        }
        Undeclared{code: Code, at: Pos, name: String} {
            display("{}: use of undeclared variable '{}'", code.describe(*at), name)
        }
        TypeRedeclaration{code: Code, at: Pos, original: Pos, name: String} {
            display("{}: redeclaration of type '{}' (original declaration is at {})",
                code.describe(*at), name, code.describe(*original))
        }
        TypeUndeclared{code: Code, at: Pos, name: String} {
            display("{}: use of undeclared type '{}'", code.describe(*at), name)
        }
        NotAllowed{code: Code, at: Pos, what: String} {
            display("{}: {}", code.describe(*at), what)
        }
        UnterminatedBlockComment{code: Code, at: Pos} {
            display("{}: unterminated block comment", code.describe(*at))
        }
        UnresolvedImport{path: String} {
            display("unresolved import: {}", path)
        }
        CircularImport{path: String} {
            display("circular import: {}", path)
        }
    }
}

quick_error! {
    #[derive(Debug, Clone)]
    pub enum TypeError {
        InvalidAssign(source: Typing, target: Typing) {
            display("cannot assign {} to {}", source, target)
        }
        InvalidBinaryOp(lhs: Typing, rhs: Typing, op: TokenType) {
            display("invalid operation: {} {} {}", lhs, op, rhs)
        }
        InvalidUnaryOp(item: Typing, op: TokenType) {
            display("invalid operation: {}{}", op, item)
        }
        InvalidIndexOp(target: Typing, index: Typing) {
            display("cannot index {} with {}", target, index)
        }
        InvalidIndexAtAll(target: Typing) {
            display("cannot index {}", target)
        }
        InvalidObjectField(key: String, target: Typing) {
            display("field {} is not present in {}", key, target)
        }
        InvalidTupleField(key: usize, target: Typing) {
            display("field {} is not present in {}", key, target)
        }
        InvalidArrayDestruct(item: Typing) {
            display("cannot destructure {} as an array", item)
        }
        InvalidObjectDestruct(item: Typing) {
            display("cannot destructure {} as an object", item)
        }
        InvalidArgumentType(have: Typing, want: Typing) {
            display("cannot use {} as {} in function call", have, want)
        }
        InvalidCallArgct(want: usize, have: usize) {
            display("expected {} arguments in function call, have {}", want, have)
        }
        InvalidCallAtAll(target: Typing) {
            display("cannot call {}", target)
        }
        InvalidIteration(target: Typing) {
            display("cannot iterate over {}", target)
        }
        InvalidAsRefinement(target: Typing, astype: Typing) {
            display("cannot treat {} as {}", target, astype)
        }
        Other(s: &'static str) {
            display("{}", s)
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
        Type(err: TypeError) {
            from(src: TypeError) -> (src)
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
