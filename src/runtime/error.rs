#[derive(Debug)]
pub enum Error {
    InvalidOperation(String),
    RuntimeError(String),
    ArgumentError(String),
    StackUnderflow,
    MalformedStackPanic,
}
